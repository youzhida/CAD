const DEFAULT_OPTIONS = {
  darkThreshold: 245,
  planCloseRadius: 2,
  barrierRadius: 0,
  planPadding: 8,
  minPlanWidthRatio: 0.25,
  minPlanHeightRatio: 0.18,
  minFurnitureAreaRatio: 0.00005,
  maxFurnitureAreaRatio: 0.02,
  minFurnitureFillRatio: 0.05,
  minStructuralDarkAreaRatio: 0.006,
  furnitureNonStructuralBias: 0.25,
  maxFurnitureAspectRatio: 40,
  furnitureSealRadius: 1,
  houseInsetRadius: 1,
  edgePocketMarginRatio: 0.16,
  edgePocketMarginMin: 160
};

function clamp(value, min, max) {
  return Math.max(min, Math.min(max, value));
}

function rgbaToDarkMask(rgba, width, height, threshold) {
  const total = width * height;
  const out = new Uint8Array(total);
  for (let i = 0, j = 0; i < total; i += 1, j += 4) {
    const alpha = rgba[j + 3];
    if (!alpha) {
      continue;
    }
    const gray = (rgba[j] + rgba[j + 1] + rgba[j + 2]) / 3;
    if (gray < threshold) {
      out[i] = 1;
    }
  }
  return out;
}

function dilate(mask, width, height, radius) {
  if (radius <= 0) {
    return mask.slice();
  }

  const out = new Uint8Array(mask.length);

  for (let y = 0; y < height; y += 1) {
    const minY = Math.max(0, y - radius);
    const maxY = Math.min(height - 1, y + radius);
    for (let x = 0; x < width; x += 1) {
      let hit = 0;
      const minX = Math.max(0, x - radius);
      const maxX = Math.min(width - 1, x + radius);
      for (let yy = minY; yy <= maxY && !hit; yy += 1) {
        const row = yy * width;
        for (let xx = minX; xx <= maxX; xx += 1) {
          if (mask[row + xx]) {
            hit = 1;
            break;
          }
        }
      }
      out[y * width + x] = hit;
    }
  }

  return out;
}

function erode(mask, width, height, radius) {
  if (radius <= 0) {
    return mask.slice();
  }

  const out = new Uint8Array(mask.length);

  for (let y = 0; y < height; y += 1) {
    const minY = Math.max(0, y - radius);
    const maxY = Math.min(height - 1, y + radius);
    for (let x = 0; x < width; x += 1) {
      let keep = 1;
      const minX = Math.max(0, x - radius);
      const maxX = Math.min(width - 1, x + radius);
      for (let yy = minY; yy <= maxY && keep; yy += 1) {
        const row = yy * width;
        for (let xx = minX; xx <= maxX; xx += 1) {
          if (!mask[row + xx]) {
            keep = 0;
            break;
          }
        }
      }
      out[y * width + x] = keep;
    }
  }

  return out;
}

function closeMask(mask, width, height, radius) {
  return erode(dilate(mask, width, height, radius), width, height, radius);
}

function labelComponents(mask, width, height) {
  const total = width * height;
  const labels = new Int32Array(total);
  const stats = [];
  const queue = new Int32Array(total);
  let componentId = 0;

  for (let start = 0; start < total; start += 1) {
    if (!mask[start] || labels[start]) {
      continue;
    }

    componentId += 1;
    let head = 0;
    let tail = 0;
    queue[tail] = start;
    tail += 1;
    labels[start] = componentId;

    let area = 0;
    let minX = width;
    let minY = height;
    let maxX = -1;
    let maxY = -1;

    while (head < tail) {
      const idx = queue[head];
      head += 1;
      const x = idx % width;
      const y = (idx / width) | 0;

      area += 1;
      if (x < minX) minX = x;
      if (y < minY) minY = y;
      if (x > maxX) maxX = x;
      if (y > maxY) maxY = y;

      for (let ny = Math.max(0, y - 1); ny <= Math.min(height - 1, y + 1); ny += 1) {
        const row = ny * width;
        for (let nx = Math.max(0, x - 1); nx <= Math.min(width - 1, x + 1); nx += 1) {
          const next = row + nx;
          if (!mask[next] || labels[next]) {
            continue;
          }
          labels[next] = componentId;
          queue[tail] = next;
          tail += 1;
        }
      }
    }

    stats.push({
      id: componentId,
      area,
      x: minX,
      y: minY,
      width: maxX - minX + 1,
      height: maxY - minY + 1
    });
  }

  return { labels, stats };
}

function componentTouchesBorder(stat, width, height, margin = 2) {
  return (
    stat.x <= margin ||
    stat.y <= margin ||
    stat.x + stat.width >= width - margin ||
    stat.y + stat.height >= height - margin
  );
}

function copyCrop(mask, width, bbox) {
  const crop = new Uint8Array(bbox.width * bbox.height);
  for (let y = 0; y < bbox.height; y += 1) {
    const srcRow = (bbox.y + y) * width + bbox.x;
    const dstRow = y * bbox.width;
    crop.set(mask.subarray(srcRow, srcRow + bbox.width), dstRow);
  }
  return crop;
}

function pasteCrop(targetMask, targetWidth, cropMask, bbox) {
  for (let y = 0; y < bbox.height; y += 1) {
    const srcRow = y * bbox.width;
    const dstRow = (bbox.y + y) * targetWidth + bbox.x;
    targetMask.set(cropMask.subarray(srcRow, srcRow + bbox.width), dstRow);
  }
}

function invertMask(mask) {
  const out = new Uint8Array(mask.length);
  for (let i = 0; i < mask.length; i += 1) {
    out[i] = mask[i] ? 0 : 1;
  }
  return out;
}

function maskAnd(a, b) {
  const out = new Uint8Array(a.length);
  for (let i = 0; i < a.length; i += 1) {
    out[i] = a[i] && b[i] ? 1 : 0;
  }
  return out;
}

function countMask(mask) {
  let total = 0;
  for (let i = 0; i < mask.length; i += 1) {
    total += mask[i];
  }
  return total;
}

function findMaskBounds(mask, width, height) {
  let minX = width;
  let minY = height;
  let maxX = -1;
  let maxY = -1;

  for (let y = 0; y < height; y += 1) {
    const row = y * width;
    for (let x = 0; x < width; x += 1) {
      if (!mask[row + x]) {
        continue;
      }
      if (x < minX) minX = x;
      if (y < minY) minY = y;
      if (x > maxX) maxX = x;
      if (y > maxY) maxY = y;
    }
  }

  if (maxX < minX || maxY < minY) {
    return {
      x: 0,
      y: 0,
      width,
      height
    };
  }

  return {
    x: minX,
    y: minY,
    width: maxX - minX + 1,
    height: maxY - minY + 1
  };
}

function floodFillFromEdges(openMask, width, height) {
  const visited = new Uint8Array(openMask.length);
  const queue = new Int32Array(openMask.length);
  let head = 0;
  let tail = 0;

  const tryPush = (idx) => {
    if (!openMask[idx] || visited[idx]) {
      return;
    }
    visited[idx] = 1;
    queue[tail] = idx;
    tail += 1;
  };

  for (let x = 0; x < width; x += 1) {
    tryPush(x);
    tryPush((height - 1) * width + x);
  }

  for (let y = 0; y < height; y += 1) {
    tryPush(y * width);
    tryPush(y * width + width - 1);
  }

  while (head < tail) {
    const idx = queue[head];
    head += 1;
    const x = idx % width;
    const y = (idx / width) | 0;

    if (x > 0) {
      tryPush(idx - 1);
    }
    if (x < width - 1) {
      tryPush(idx + 1);
    }
    if (y > 0) {
      tryPush(idx - width);
    }
    if (y < height - 1) {
      tryPush(idx + width);
    }
  }

  return visited;
}

function findMainPlanBBox(mask, width, height, options) {
  const closed = closeMask(mask, width, height, options.planCloseRadius);
  const { stats } = labelComponents(closed, width, height);
  let best = null;
  let bestArea = -1;

  for (const stat of stats) {
    if (componentTouchesBorder(stat, width, height, 2)) {
      continue;
    }
    if (stat.width < width * options.minPlanWidthRatio) {
      continue;
    }
    if (stat.height < height * options.minPlanHeightRatio) {
      continue;
    }
    if (stat.area > bestArea) {
      best = stat;
      bestArea = stat.area;
    }
  }

  if (!best) {
    return {
      x: 0,
      y: 0,
      width,
      height
    };
  }

  return {
    x: clamp(best.x - options.planPadding, 0, width - 1),
    y: clamp(best.y - options.planPadding, 0, height - 1),
    width: clamp(best.width + options.planPadding * 2, 1, width),
    height: clamp(best.height + options.planPadding * 2, 1, height)
  };
}

function upscaleMaskNearest(mask, srcWidth, srcHeight, dstWidth, dstHeight) {
  const out = new Uint8Array(dstWidth * dstHeight);
  for (let y = 0; y < dstHeight; y += 1) {
    const srcY = Math.min(srcHeight - 1, Math.floor((y * srcHeight) / dstHeight));
    const srcRow = srcY * srcWidth;
    const dstRow = y * dstWidth;
    for (let x = 0; x < dstWidth; x += 1) {
      const srcX = Math.min(srcWidth - 1, Math.floor((x * srcWidth) / dstWidth));
      out[dstRow + x] = mask[srcRow + srcX];
    }
  }
  return out;
}

function collectWhiteDarkAdjacency(whiteLabels, darkLabels, width, height) {
  const out = new Map();

  for (let y = 0; y < height; y += 1) {
    for (let x = 0; x < width; x += 1) {
      const idx = y * width + x;
      const whiteId = whiteLabels[idx];
      if (!whiteId) {
        continue;
      }

      let entry = out.get(whiteId);
      if (!entry) {
        entry = new Map();
        out.set(whiteId, entry);
      }

      for (let ny = Math.max(0, y - 1); ny <= Math.min(height - 1, y + 1); ny += 1) {
        const row = ny * width;
        for (let nx = Math.max(0, x - 1); nx <= Math.min(width - 1, x + 1); nx += 1) {
          if (nx === x && ny === y) {
            continue;
          }
          const darkId = darkLabels[row + nx];
          if (!darkId) {
            continue;
          }
          entry.set(darkId, (entry.get(darkId) || 0) + 1);
        }
      }
    }
  }

  return out;
}

function buildMasksFromRgba(rgba, width, height, userOptions = {}) {
  const options = { ...DEFAULT_OPTIONS, ...userOptions };
  const darkMask = rgbaToDarkMask(rgba, width, height, options.darkThreshold);
  const planBBox = findMainPlanBBox(darkMask, width, height, options);

  const cropDark = copyCrop(darkMask, width, planBBox);
  const barrierCrop = dilate(cropDark, planBBox.width, planBBox.height, options.barrierRadius);
  const openCrop = invertMask(barrierCrop);
  const outsideCrop = floodFillFromEdges(openCrop, planBBox.width, planBBox.height);
  const houseCrop = closeMask(
    invertMask(outsideCrop),
    planBBox.width,
    planBBox.height,
    1
  );

  const roomCandidateCrop = new Uint8Array(openCrop.length);
  for (let i = 0; i < openCrop.length; i += 1) {
    roomCandidateCrop[i] = openCrop[i] && !outsideCrop[i] ? 1 : 0;
  }

  const { stats: roomStats } = labelComponents(roomCandidateCrop, planBBox.width, planBBox.height);

  const houseMask = new Uint8Array(width * height);
  const roomMask = new Uint8Array(width * height);
  pasteCrop(houseMask, width, houseCrop, planBBox);
  pasteCrop(roomMask, width, roomCandidateCrop, planBBox);

  return {
    houseMask,
    roomMask,
    debug: {
      width,
      height,
      planBBox,
      roomCount: roomStats.length,
      roomCandidateCount: roomStats.length,
      furnitureCutCount: 0
    }
  };
}

function buildFurnitureCutMaskFromRgba(rgba, width, height, houseMask, userOptions = {}) {
  const options = { ...DEFAULT_OPTIONS, ...userOptions };
  const darkMask = rgbaToDarkMask(rgba, width, height, options.darkThreshold);
  const sealedDarkMask = closeMask(
    darkMask,
    width,
    height,
    options.furnitureSealRadius
  );
  const originalOpenMask = invertMask(darkMask);
  const sealedOpenMask = invertMask(sealedDarkMask);
  const houseInteriorMask = houseMask
    ? erode(houseMask, width, height, options.houseInsetRadius)
    : (() => {
        const mask = new Uint8Array(width * height);
        mask.fill(1);
        return mask;
      })();
  const houseBounds = findMaskBounds(houseInteriorMask, width, height);
  const edgePocketMargin = Math.max(
    options.edgePocketMarginMin,
    Math.round(Math.min(houseBounds.width, houseBounds.height) * options.edgePocketMarginRatio)
  );
  const candidateMask = houseMask ? maskAnd(sealedOpenMask, houseInteriorMask) : sealedOpenMask;
  const { labels: candidateLabels, stats: candidateStats } = labelComponents(
    candidateMask,
    width,
    height
  );
  const { labels: darkLabels, stats: darkStats } = labelComponents(
    sealedDarkMask,
    width,
    height
  );

  const effectiveArea = Math.max(1, countMask(houseInteriorMask));
  const minFurnitureArea = Math.max(
    600,
    Math.round(effectiveArea * options.minFurnitureAreaRatio)
  );
  const maxFurnitureArea = Math.max(
    minFurnitureArea,
    Math.round(effectiveArea * options.maxFurnitureAreaRatio)
  );
  const minStructuralDarkArea = Math.max(
    80,
    Math.round(effectiveArea * options.minStructuralDarkAreaRatio)
  );
  const structuralDarkIds = new Set(
    darkStats.filter((stat) => stat.area >= minStructuralDarkArea).map((stat) => stat.id)
  );
  const adjacencyMap = collectWhiteDarkAdjacency(candidateLabels, darkLabels, width, height);
  const furnitureIds = [];
  const selectedStats = [];

  for (const stat of candidateStats) {
    if (componentTouchesBorder(stat, width, height, 1)) {
      continue;
    }

    if (stat.area < minFurnitureArea || stat.area > maxFurnitureArea) {
      continue;
    }

    const aspect =
      Math.max(stat.width, stat.height) / Math.max(1, Math.min(stat.width, stat.height));
    if (aspect > options.maxFurnitureAspectRatio) {
      continue;
    }

    const fillRatio = stat.area / Math.max(1, stat.width * stat.height);
    if (fillRatio < options.minFurnitureFillRatio) {
      continue;
    }

    const adjacency = adjacencyMap.get(stat.id);
    if (!adjacency) {
      continue;
    }

    let structuralAdj = 0;
    let nonStructuralAdj = 0;
    let structuralAdjCount = 0;
    let nonStructuralAdjCount = 0;
    for (const [darkId, count] of adjacency.entries()) {
      if (structuralDarkIds.has(darkId)) {
        structuralAdj += count;
        structuralAdjCount += 1;
      } else {
        nonStructuralAdj += count;
        nonStructuralAdjCount += 1;
      }
    }

    const nearHouseEdge =
      stat.x <= houseBounds.x + edgePocketMargin ||
      stat.y <= houseBounds.y + edgePocketMargin ||
      stat.x + stat.width >= houseBounds.x + houseBounds.width - edgePocketMargin ||
      stat.y + stat.height >= houseBounds.y + houseBounds.height - edgePocketMargin;

    if (nonStructuralAdj > structuralAdj * options.furnitureNonStructuralBias) {
      // Skip broad floor slabs along the house boundary. They often become
      // sealed candidate pockets near beds / open rooms, but should remain
      // gray in the reference layer 2.
      if (
        nearHouseEdge &&
        structuralAdjCount === 1 &&
        nonStructuralAdjCount === 1 &&
        stat.area >= 50000 &&
        fillRatio >= 0.5
      ) {
        continue;
      }
      // Skip narrow dotted / hatched edge panels. They are usually service
      // strips or decorative blocks, not furniture white holes.
      if (
        nearHouseEdge &&
        structuralAdjCount <= 1 &&
        nonStructuralAdjCount >= 20 &&
        stat.width <= 110 &&
        fillRatio <= 0.75
      ) {
        continue;
      }
      // Skip medium empty edge pockets that hug the exterior boundary with
      // almost no object adjacency. These are often room-floor voids rather
      // than furniture interiors.
      if (
        nearHouseEdge &&
        structuralAdjCount === 0 &&
        nonStructuralAdjCount <= 2 &&
        stat.area >= 5000 &&
        stat.area <= 12000 &&
        fillRatio >= 0.5 &&
        aspect <= 2.5
      ) {
        continue;
      }
      // Skip compact solid pockets with almost no object adjacency. These are
      // usually small accent fills instead of furniture interiors.
      if (
        structuralAdjCount === 0 &&
        nonStructuralAdjCount <= 2 &&
        stat.area <= 7000 &&
        fillRatio >= 0.85 &&
        aspect <= 2
      ) {
        continue;
      }
      // Skip tall thin panels that behave like wall/fixture slits rather than
      // furniture cut-outs.
      if (
        structuralAdjCount <= 1 &&
        nonStructuralAdjCount <= 2 &&
        stat.width <= 40 &&
        stat.height >= 120 &&
        fillRatio >= 0.8
      ) {
        continue;
      }
      // Skip elongated edge pockets that remain room-floor voids after
      // sealing, especially in small service areas on the right side.
      if (
        structuralAdjCount === 0 &&
        nonStructuralAdjCount === 1 &&
        stat.area >= 9000 &&
        fillRatio <= 0.55 &&
        aspect >= 2
      ) {
        continue;
      }
      // Skip room-like slabs that only lean on one structural edge and barely
      // touch non-structural lines. These are common false positives for
      // bathrooms / hatched zones in the reference PSD.
      if (
        structuralAdjCount === 1 &&
        nonStructuralAdjCount <= 1 &&
        fillRatio >= 0.65
      ) {
        continue;
      }
      // Skip dense pockets that do not anchor to any structural edge but touch
      // many tiny non-structural fragments. These are usually text / hatch
      // neighborhoods rather than real furniture holes.
      if (
        structuralAdjCount === 0 &&
        nonStructuralAdjCount >= 20 &&
        fillRatio >= 0.75
      ) {
        continue;
      }
      furnitureIds.push(stat.id);
      selectedStats.push({
        id: stat.id,
        area: stat.area,
        x: stat.x,
        y: stat.y,
        width: stat.width,
        height: stat.height,
        fillRatio,
        aspect,
        structuralAdj,
        nonStructuralAdj,
        structuralAdjCount,
        nonStructuralAdjCount
      });
    }
  }

  const selectedIds = new Set(furnitureIds);
  const furnitureMask = new Uint8Array(width * height);
  for (let i = 0; i < candidateLabels.length; i += 1) {
    if (!selectedIds.has(candidateLabels[i])) {
      continue;
    }
    if (!originalOpenMask[i]) {
      continue;
    }
    if (!houseInteriorMask[i]) {
      continue;
    }
    furnitureMask[i] = 1;
  }

  return {
    furnitureMask,
    debug: {
      furnitureCount: furnitureIds.length,
      candidateCount: candidateStats.length,
      minFurnitureArea,
      maxFurnitureArea,
      minStructuralDarkArea,
      selectedStats
    }
  };
}

module.exports = {
  DEFAULT_OPTIONS,
  buildMasksFromRgba,
  buildFurnitureCutMaskFromRgba,
  closeMask,
  upscaleMaskNearest
};
