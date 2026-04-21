const DEFAULT_OPTIONS = {
  darkThreshold: 245,
  bboxCloseRadius: 2,
  barrierRadius: 1,
  backgroundSeedInset: 40,
  minRoomAreaRatio: 0.0005,
  maxAspectRatio: 12,
  smallHoleAreaRatio: 0.00006,
  bboxPadding: 8
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
  const size = radius * 2 + 1;

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
      height: maxY - minY + 1,
      centerX: (minX + maxX) / 2,
      centerY: (minY + maxY) / 2
    });
  }

  return { labels, stats };
}

function findLargestHouseBBox(mask, width, height, padding) {
  const closed = closeMask(mask, width, height, DEFAULT_OPTIONS.bboxCloseRadius);
  const { stats } = labelComponents(closed, width, height);
  const pageCenterX = width / 2;
  let best = null;
  let bestScore = -1;

  for (const stat of stats) {
    if (stat.width < width * 0.15 || stat.height < height * 0.12) {
      continue;
    }
    const centerBias = 1 - Math.abs(stat.centerX - pageCenterX) / Math.max(1, pageCenterX);
    const score = stat.area * (0.7 + 0.3 * centerBias);
    if (score > bestScore) {
      bestScore = score;
      best = stat;
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
    x: clamp(best.x - padding, 0, width - 1),
    y: clamp(best.y - padding, 0, height - 1),
    width: clamp(best.width + padding * 2, 1, width),
    height: clamp(best.height + padding * 2, 1, height)
  };
}

function findOpenSeed(openMask, width, height, inset) {
  const maxY = Math.min(height - 1, inset + 240);
  const maxX = Math.min(width - 1, inset + 240);
  for (let y = inset; y <= maxY; y += 1) {
    for (let x = inset; x <= maxX; x += 1) {
      if (openMask[y * width + x]) {
        return y * width + x;
      }
    }
  }

  for (let y = 0; y < height; y += 4) {
    for (let x = 0; x < width; x += 4) {
      if (openMask[y * width + x]) {
        return y * width + x;
      }
    }
  }

  return -1;
}

function floodFill(openMask, width, height, seed) {
  const visited = new Uint8Array(openMask.length);
  if (seed < 0 || !openMask[seed]) {
    return visited;
  }

  const queue = new Int32Array(openMask.length);
  let head = 0;
  let tail = 0;
  queue[tail] = seed;
  tail += 1;
  visited[seed] = 1;

  while (head < tail) {
    const idx = queue[head];
    head += 1;
    const x = idx % width;
    const y = (idx / width) | 0;

    if (x > 0) {
      const left = idx - 1;
      if (openMask[left] && !visited[left]) {
        visited[left] = 1;
        queue[tail] = left;
        tail += 1;
      }
    }

    if (x < width - 1) {
      const right = idx + 1;
      if (openMask[right] && !visited[right]) {
        visited[right] = 1;
        queue[tail] = right;
        tail += 1;
      }
    }

    if (y > 0) {
      const up = idx - width;
      if (openMask[up] && !visited[up]) {
        visited[up] = 1;
        queue[tail] = up;
        tail += 1;
      }
    }

    if (y < height - 1) {
      const down = idx + width;
      if (openMask[down] && !visited[down]) {
        visited[down] = 1;
        queue[tail] = down;
        tail += 1;
      }
    }
  }

  return visited;
}

function componentTouchesHouse(stat, houseBBox) {
  const insideX = stat.centerX >= houseBBox.x && stat.centerX <= houseBBox.x + houseBBox.width;
  const insideY = stat.centerY >= houseBBox.y && stat.centerY <= houseBBox.y + houseBBox.height;
  return insideX && insideY;
}

function fillComponentsById(targetMask, labels, allowedIds) {
  const allow = new Set(allowedIds);
  for (let i = 0; i < labels.length; i += 1) {
    if (allow.has(labels[i])) {
      targetMask[i] = 1;
    }
  }
}

function fillSmallHoles(roomMask, barrierMask, width, height, houseBBox, maxHoleArea) {
  const x1 = houseBBox.x;
  const y1 = houseBBox.y;
  const x2 = Math.min(width, houseBBox.x + houseBBox.width);
  const y2 = Math.min(height, houseBBox.y + houseBBox.height);
  const cropWidth = x2 - x1;
  const cropHeight = y2 - y1;
  const inv = new Uint8Array(cropWidth * cropHeight);

  for (let y = 0; y < cropHeight; y += 1) {
    const srcRow = (y1 + y) * width;
    const dstRow = y * cropWidth;
    for (let x = 0; x < cropWidth; x += 1) {
      const src = srcRow + x1 + x;
      inv[dstRow + x] = roomMask[src] || barrierMask[src] ? 0 : 1;
    }
  }

  const visited = new Uint8Array(inv.length);
  const queue = new Int32Array(inv.length);

  const floodFrom = (seed) => {
    let head = 0;
    let tail = 0;
    queue[tail] = seed;
    tail += 1;
    visited[seed] = 1;
    while (head < tail) {
      const idx = queue[head];
      head += 1;
      const x = idx % cropWidth;
      const y = (idx / cropWidth) | 0;

      if (x > 0) {
        const left = idx - 1;
        if (inv[left] && !visited[left]) {
          visited[left] = 1;
          queue[tail] = left;
          tail += 1;
        }
      }
      if (x < cropWidth - 1) {
        const right = idx + 1;
        if (inv[right] && !visited[right]) {
          visited[right] = 1;
          queue[tail] = right;
          tail += 1;
        }
      }
      if (y > 0) {
        const up = idx - cropWidth;
        if (inv[up] && !visited[up]) {
          visited[up] = 1;
          queue[tail] = up;
          tail += 1;
        }
      }
      if (y < cropHeight - 1) {
        const down = idx + cropWidth;
        if (inv[down] && !visited[down]) {
          visited[down] = 1;
          queue[tail] = down;
          tail += 1;
        }
      }
    }
  };

  for (let x = 0; x < cropWidth; x += 1) {
    const top = x;
    const bottom = (cropHeight - 1) * cropWidth + x;
    if (inv[top] && !visited[top]) floodFrom(top);
    if (inv[bottom] && !visited[bottom]) floodFrom(bottom);
  }

  for (let y = 0; y < cropHeight; y += 1) {
    const left = y * cropWidth;
    const right = left + cropWidth - 1;
    if (inv[left] && !visited[left]) floodFrom(left);
    if (inv[right] && !visited[right]) floodFrom(right);
  }

  const holeMask = new Uint8Array(inv.length);
  for (let i = 0; i < inv.length; i += 1) {
    if (inv[i] && !visited[i]) {
      holeMask[i] = 1;
    }
  }

  const { labels, stats } = labelComponents(holeMask, cropWidth, cropHeight);
  const allowed = stats.filter((stat) => stat.area <= maxHoleArea).map((stat) => stat.id);
  if (!allowed.length) {
    return;
  }

  const allow = new Set(allowed);
  for (let i = 0; i < labels.length; i += 1) {
    if (!allow.has(labels[i])) {
      continue;
    }
    const x = i % cropWidth;
    const y = (i / cropWidth) | 0;
    roomMask[(y1 + y) * width + x1 + x] = 1;
  }
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

function buildMasksFromRgba(rgba, width, height, userOptions = {}) {
  const options = { ...DEFAULT_OPTIONS, ...userOptions };
  const darkMask = rgbaToDarkMask(rgba, width, height, options.darkThreshold);
  const houseBBox = findLargestHouseBBox(darkMask, width, height, options.bboxPadding);
  const barrierMask = dilate(darkMask, width, height, options.barrierRadius);

  const openMask = new Uint8Array(barrierMask.length);
  for (let i = 0; i < barrierMask.length; i += 1) {
    openMask[i] = barrierMask[i] ? 0 : 1;
  }

  const backgroundSeed = findOpenSeed(openMask, width, height, options.backgroundSeedInset);
  const backgroundMask = floodFill(openMask, width, height, backgroundSeed);

  const candidateMask = new Uint8Array(openMask.length);
  for (let i = 0; i < openMask.length; i += 1) {
    candidateMask[i] = openMask[i] && !backgroundMask[i] ? 1 : 0;
  }

  const { labels, stats } = labelComponents(candidateMask, width, height);
  const minRoomArea = Math.max(150, Math.round(width * height * options.minRoomAreaRatio));
  const maxHoleArea = Math.max(40, Math.round(width * height * options.smallHoleAreaRatio));

  const roomIds = [];
  for (const stat of stats) {
    if (stat.area < minRoomArea) {
      continue;
    }
    const aspect = Math.max(stat.width, stat.height) / Math.max(1, Math.min(stat.width, stat.height));
    if (aspect > options.maxAspectRatio) {
      continue;
    }
    if (!componentTouchesHouse(stat, houseBBox)) {
      continue;
    }
    roomIds.push(stat.id);
  }

  const roomMask = new Uint8Array(openMask.length);
  fillComponentsById(roomMask, labels, roomIds);
  fillSmallHoles(roomMask, barrierMask, width, height, houseBBox, maxHoleArea);

  return {
    backgroundMask,
    roomMask,
    debug: {
      width,
      height,
      houseBBox,
      backgroundSeed,
      roomCount: roomIds.length,
      minRoomArea,
      maxHoleArea
    }
  };
}

module.exports = {
  DEFAULT_OPTIONS,
  buildMasksFromRgba,
  upscaleMaskNearest
};
