const photoshop = require("photoshop");
const { app, core, imaging } = photoshop;
const { buildMasksFromRgba, upscaleMaskNearest } = require("./region-engine");

const RUN_BUTTON_ID = "run-button";
const STATUS_ID = "status";
const LAYER2_NAME = "图层 2";
const LAYER7_NAME = "图层 7";
const DEFAULT_BG_COLOR = [195, 195, 195, 255];
const DEFAULT_ROOM_COLOR = [226, 226, 225, 255];
const ANALYSIS_HEIGHT = 1200;
const PALETTE_SAMPLE_HEIGHT = 600;

function setStatus(message) {
  const status = document.getElementById(STATUS_ID);
  if (status) {
    status.textContent = message;
  }
}

function flattenLayers(layers, out = []) {
  for (const layer of layers) {
    out.push(layer);
    if (layer.layers && layer.layers.length) {
      flattenLayers(layer.layers, out);
    }
  }
  return out;
}

function getAllLayers(doc) {
  return flattenLayers([...doc.layers]);
}

function findLayerByName(doc, name) {
  return getAllLayers(doc).find((layer) => layer.name === name) || null;
}

function looksLikeSmartPdf(layer) {
  const kind = String(layer.kind || "").toLowerCase();
  const name = String(layer.name || "").toLowerCase();
  return (
    kind.includes("smart") ||
    name.includes(".pdf") ||
    name.includes("pdf") ||
    name.includes("保利天悦")
  );
}

function findPdfSmartLayer(doc) {
  const allLayers = getAllLayers(doc);
  return allLayers.find(looksLikeSmartPdf) || null;
}

function normalizeUnitValue(value) {
  if (value && typeof value === "object") {
    if ("_value" in value) {
      return Number(value._value);
    }
    if ("value" in value) {
      return Number(value.value);
    }
  }
  return Number(value);
}

function normalizeBounds(bounds) {
  if (!bounds) {
    return null;
  }
  return {
    left: Math.round(normalizeUnitValue(bounds.left)),
    top: Math.round(normalizeUnitValue(bounds.top)),
    right: Math.round(normalizeUnitValue(bounds.right)),
    bottom: Math.round(normalizeUnitValue(bounds.bottom))
  };
}

async function getLayerPixels(doc, layer, sourceBounds, targetHeight) {
  const imageObj = await imaging.getPixels({
    documentID: doc.id,
    layerID: layer.id,
    sourceBounds,
    targetSize: targetHeight ? { height: targetHeight } : undefined,
    colorSpace: "RGB",
    componentSize: 8
  });

  const data = await imageObj.imageData.getData();
  const width = imageObj.imageData.width;
  const height = imageObj.imageData.height;
  imageObj.imageData.dispose();

  return {
    data: new Uint8Array(data),
    width,
    height
  };
}

function pickPalette(rgba) {
  const counts = new Map();
  for (let i = 0; i < rgba.length; i += 4) {
    const alpha = rgba[i + 3];
    if (alpha < 8) {
      continue;
    }
    const key = `${rgba[i]},${rgba[i + 1]},${rgba[i + 2]}`;
    counts.set(key, (counts.get(key) || 0) + 1);
  }

  const entries = [...counts.entries()].sort((a, b) => b[1] - a[1]);
  if (!entries.length) {
    return {
      backgroundColor: DEFAULT_BG_COLOR,
      roomColor: DEFAULT_ROOM_COLOR
    };
  }

  const parseColor = (key) => key.split(",").map((part) => Number(part));
  const backgroundColor = [...parseColor(entries[0][0]), 255];
  let roomColor = DEFAULT_ROOM_COLOR;

  for (let i = 1; i < entries.length; i += 1) {
    const rgb = parseColor(entries[i][0]);
    const distance =
      Math.abs(rgb[0] - backgroundColor[0]) +
      Math.abs(rgb[1] - backgroundColor[1]) +
      Math.abs(rgb[2] - backgroundColor[2]);
    if (distance >= 18) {
      roomColor = [...rgb, 255];
      break;
    }
  }

  return {
    backgroundColor,
    roomColor
  };
}

async function pickPaletteFromLayer(doc, layer, sourceBounds) {
  try {
    const paletteSample = await getLayerPixels(
      doc,
      layer,
      sourceBounds,
      Math.min(PALETTE_SAMPLE_HEIGHT, Math.max(1, sourceBounds.bottom - sourceBounds.top))
    );
    return pickPalette(paletteSample.data);
  } catch (error) {
    return {
      backgroundColor: DEFAULT_BG_COLOR,
      roomColor: DEFAULT_ROOM_COLOR
    };
  }
}

function paintLayerBuffer(width, height, backgroundMask, roomMask, backgroundColor, roomColor, includeBackground) {
  const out = new Uint8Array(width * height * 4);
  for (let i = 0, j = 0; i < backgroundMask.length; i += 1, j += 4) {
    if (includeBackground && backgroundMask[i]) {
      out[j] = backgroundColor[0];
      out[j + 1] = backgroundColor[1];
      out[j + 2] = backgroundColor[2];
      out[j + 3] = backgroundColor[3];
    }
    if (roomMask[i]) {
      out[j] = roomColor[0];
      out[j + 1] = roomColor[1];
      out[j + 2] = roomColor[2];
      out[j + 3] = roomColor[3];
    }
  }
  return out;
}

async function putRgbaIntoLayer(doc, layer, bounds, rgbaBuffer, width, height, commandName) {
  const imageData = await imaging.createImageDataFromBuffer(rgbaBuffer, {
    width,
    height,
    components: 4,
    colorSpace: "RGB"
  });

  await imaging.putPixels({
    documentID: doc.id,
    layerID: layer.id,
    imageData,
    replace: true,
    targetBounds: {
      left: bounds.left,
      top: bounds.top
    },
    commandName
  });

  imageData.dispose();
}

async function ensurePixelLayer(doc, name) {
  const existing = findLayerByName(doc, name);
  if (existing) {
    return { layer: existing, created: false };
  }

  const layer = await doc.createLayer({ name });
  return { layer, created: true };
}

async function runFloorFill() {
  const doc = app.activeDocument;
  if (!doc) {
    throw new Error("当前没有打开的 PSD 文档。");
  }

  const smartLayer = findPdfSmartLayer(doc);
  if (!smartLayer) {
    throw new Error("没找到 PDF 智能对象线稿层。");
  }

  const smartBounds = normalizeBounds(smartLayer.boundsNoEffects || smartLayer.bounds);
  if (!smartBounds) {
    throw new Error("没读到线稿层边界。");
  }

  const cropWidth = Math.max(1, smartBounds.right - smartBounds.left);
  const cropHeight = Math.max(1, smartBounds.bottom - smartBounds.top);

  const smartPixels = await getLayerPixels(
    doc,
    smartLayer,
    smartBounds,
    Math.min(ANALYSIS_HEIGHT, cropHeight)
  );

  const layer2Info = await ensurePixelLayer(doc, LAYER2_NAME);
  const layer7Info = await ensurePixelLayer(doc, LAYER7_NAME);
  const layer2 = layer2Info.layer;
  const layer7 = layer7Info.layer;

  layer2.allLocked = false;
  layer2.visible = true;
  layer2.fillOpacity = 100;

  layer7.allLocked = false;
  layer7.visible = true;
  layer7.fillOpacity = 0;

  const { backgroundColor, roomColor } = await pickPaletteFromLayer(doc, layer2, smartBounds);

  const masks = buildMasksFromRgba(smartPixels.data, smartPixels.width, smartPixels.height);
  const backgroundMaskFull = upscaleMaskNearest(
    masks.backgroundMask,
    smartPixels.width,
    smartPixels.height,
    cropWidth,
    cropHeight
  );
  const roomMaskFull = upscaleMaskNearest(
    masks.roomMask,
    smartPixels.width,
    smartPixels.height,
    cropWidth,
    cropHeight
  );

  const layer2Buffer = paintLayerBuffer(
    cropWidth,
    cropHeight,
    backgroundMaskFull,
    roomMaskFull,
    backgroundColor,
    roomColor,
    true
  );
  await putRgbaIntoLayer(doc, layer2, smartBounds, layer2Buffer, cropWidth, cropHeight, "生成图层2");

  const layer7Buffer = paintLayerBuffer(
    cropWidth,
    cropHeight,
    backgroundMaskFull,
    roomMaskFull,
    backgroundColor,
    roomColor,
    false
  );
  await putRgbaIntoLayer(doc, layer7, smartBounds, layer7Buffer, cropWidth, cropHeight, "生成图层7");

  return {
    smartLayerName: smartLayer.name,
    smartBounds,
    backgroundColor,
    roomColor,
    roomCount: masks.debug.roomCount,
    layer7Created: layer7Info.created
  };
}

async function handleRunClick() {
  const button = document.getElementById(RUN_BUTTON_ID);
  button.disabled = true;
  setStatus("开始分析线稿并生成图层...");

  try {
    const result = await core.executeAsModal(
      async () => runFloorFill(),
      { commandName: "一键生成图层2和图层7" }
    );

    const warnings = [];
    if (result.layer7Created) {
      warnings.push("注意：这次新建了图层7，现有内阴影样式需要你在模板里先存好一版。");
    }

    setStatus(
      [
        "已完成。",
        `线稿层：${result.smartLayerName}`,
        `线稿边界：${result.smartBounds.left}, ${result.smartBounds.top}, ${result.smartBounds.right}, ${result.smartBounds.bottom}`,
        `背景灰：${result.backgroundColor.slice(0, 3).join(", ")}`,
        `房间灰：${result.roomColor.slice(0, 3).join(", ")}`,
        `候选房间区数量：${result.roomCount}`,
        ...warnings
      ].join("\n")
    );
  } catch (error) {
    setStatus(`执行失败：${error && error.message ? error.message : String(error)}`);
  } finally {
    button.disabled = false;
  }
}

document.addEventListener("DOMContentLoaded", () => {
  const button = document.getElementById(RUN_BUTTON_ID);
  if (button) {
    button.addEventListener("click", handleRunClick);
  }
});
