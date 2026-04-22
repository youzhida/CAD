const photoshop = require("photoshop");
const { app, core, imaging, action, constants } = photoshop;
const {
  buildMasksFromRgba,
  buildFurnitureCutMaskFromRgba,
  closeMask,
  upscaleMaskNearest
} = require("./region-engine");

const RUN_BUTTON_ID = "run-button";
const STATUS_ID = "status";
const LAYER2_NAME = "图层 2";
const LAYER7_NAME = "图层 7";
const WHITE_BASE_NAME = "PDF白底";
const DEFAULT_BG_COLOR = [195, 195, 195, 255];
const DEFAULT_ROOM_COLOR = [226, 226, 225, 255];
const SOLID_WHITE = [255, 255, 255, 255];
const ANALYSIS_HEIGHT = 1800;
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

function getDocumentBounds(doc) {
  const width = Math.round(normalizeUnitValue(doc.width));
  const height = Math.round(normalizeUnitValue(doc.height));
  return {
    left: 0,
    top: 0,
    right: width,
    bottom: height
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

function createSolidBuffer(width, height, color) {
  const out = new Uint8Array(width * height * 4);
  for (let i = 0; i < out.length; i += 4) {
    out[i] = color[0];
    out[i + 1] = color[1];
    out[i + 2] = color[2];
    out[i + 3] = color[3];
  }
  return out;
}

function createTransparentBuffer(width, height) {
  return new Uint8Array(width * height * 4);
}

function applyMaskToBuffer(
  buffer,
  bufferWidth,
  mask,
  maskWidth,
  maskHeight,
  offsetX,
  offsetY,
  color
) {
  for (let y = 0; y < maskHeight; y += 1) {
    const maskRow = y * maskWidth;
    const bufferRow = (offsetY + y) * bufferWidth;
    for (let x = 0; x < maskWidth; x += 1) {
      if (!mask[maskRow + x]) {
        continue;
      }
      const outIdx = (bufferRow + offsetX + x) * 4;
      buffer[outIdx] = color[0];
      buffer[outIdx + 1] = color[1];
      buffer[outIdx + 2] = color[2];
      buffer[outIdx + 3] = color[3];
    }
  }
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

async function ensureWhiteBaseLayer(doc, smartLayer, bounds) {
  const info = await ensurePixelLayer(doc, WHITE_BASE_NAME);
  const layer = info.layer;

  layer.allLocked = false;
  layer.visible = true;
  layer.fillOpacity = 100;
  layer.opacity = 100;

  const width = Math.max(1, bounds.right - bounds.left);
  const height = Math.max(1, bounds.bottom - bounds.top);
  const whiteBuffer = createSolidBuffer(width, height, SOLID_WHITE);
  await putRgbaIntoLayer(doc, layer, bounds, whiteBuffer, width, height, "更新PDF白底");
  await layer.move(smartLayer, constants.ElementPlacement.PLACEAFTER);

  return info;
}

async function syncWorkingLayerOrder(smartLayer, whiteBaseLayer, layer2, layer7) {
  await whiteBaseLayer.move(smartLayer, constants.ElementPlacement.PLACEAFTER);
  await layer2.move(whiteBaseLayer, constants.ElementPlacement.PLACEBEFORE);
  await layer7.move(layer2, constants.ElementPlacement.PLACEBEFORE);
  await smartLayer.move(layer7, constants.ElementPlacement.PLACEBEFORE);
}

async function applyDefaultInnerShadow(layer) {
  await action.batchPlay(
    [
      {
        _obj: "set",
        _target: [
          {
            _ref: "property",
            _property: "layerEffects"
          },
          {
            _ref: "layer",
            _id: layer.id
          }
        ],
        to: {
          _obj: "layerEffects",
          scale: {
            _unit: "percentUnit",
            _value: 100
          },
          innerShadow: {
            _obj: "innerShadow",
            enabled: true,
            present: true,
            showInDialog: true,
            mode: {
              _enum: "blendMode",
              _value: "multiply"
            },
            color: {
              _obj: "RGBColor",
              red: 0,
              green: 0,
              blue: 0
            },
            opacity: {
              _unit: "percentUnit",
              _value: 75
            },
            useGlobalAngle: false,
            localLightingAngle: {
              _unit: "angleUnit",
              _value: 90
            },
            distance: {
              _unit: "pixelsUnit",
              _value: 8
            },
            chokeMatte: {
              _unit: "pixelsUnit",
              _value: 5
            },
            blur: {
              _unit: "pixelsUnit",
              _value: 8
            },
            noise: {
              _unit: "percentUnit",
              _value: 0
            },
            antiAlias: false,
            transferSpec: {
              _obj: "shapeCurveType",
              name: "Linear",
              curve: [
                {
                  _obj: "paint",
                  horizontal: 0,
                  vertical: 0
                },
                {
                  _obj: "paint",
                  horizontal: 255,
                  vertical: 255
                }
              ]
            }
          }
        },
        _options: {
          dialogOptions: "dontDisplay"
        }
      }
    ],
    {
      synchronousExecution: true,
      modalBehavior: "execute"
    }
  );
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

  const outputBounds = getDocumentBounds(doc);
  const outputWidth = Math.max(1, outputBounds.right - outputBounds.left);
  const outputHeight = Math.max(1, outputBounds.bottom - outputBounds.top);
  const cropWidth = Math.max(1, smartBounds.right - smartBounds.left);
  const cropHeight = Math.max(1, smartBounds.bottom - smartBounds.top);

  const smartPixels = await getLayerPixels(
    doc,
    smartLayer,
    smartBounds,
    Math.min(ANALYSIS_HEIGHT, cropHeight)
  );
  const smartPixelsFull = await getLayerPixels(doc, smartLayer, smartBounds);

  const whiteBaseInfo = await ensureWhiteBaseLayer(doc, smartLayer, outputBounds);
  const layer2Info = await ensurePixelLayer(doc, LAYER2_NAME);
  const layer7Info = await ensurePixelLayer(doc, LAYER7_NAME);
  const layer2 = layer2Info.layer;
  const layer7 = layer7Info.layer;

  layer2.allLocked = false;
  layer2.visible = true;
  layer2.fillOpacity = 100;
  layer2.opacity = 100;

  layer7.allLocked = false;
  layer7.visible = true;
  layer7.fillOpacity = 0;
  layer7.opacity = 100;

  await syncWorkingLayerOrder(smartLayer, whiteBaseInfo.layer, layer2, layer7);

  const { backgroundColor, roomColor } = await pickPaletteFromLayer(doc, layer2, outputBounds);
  const masks = buildMasksFromRgba(smartPixels.data, smartPixels.width, smartPixels.height);
  const houseMaskFull = upscaleMaskNearest(
    masks.houseMask,
    smartPixels.width,
    smartPixels.height,
    cropWidth,
    cropHeight
  );
  let roomMaskFull = upscaleMaskNearest(
    masks.roomMask,
    smartPixels.width,
    smartPixels.height,
    cropWidth,
    cropHeight
  );
  roomMaskFull = closeMask(roomMaskFull, cropWidth, cropHeight, 1);
  const furnitureCut = buildFurnitureCutMaskFromRgba(
    smartPixelsFull.data,
    smartPixelsFull.width,
    smartPixelsFull.height,
    houseMaskFull
  );
  for (let i = 0; i < roomMaskFull.length; i += 1) {
    if (furnitureCut.furnitureMask[i]) {
      roomMaskFull[i] = 0;
    }
  }

  const layer2Buffer = createSolidBuffer(outputWidth, outputHeight, backgroundColor);
  applyMaskToBuffer(
    layer2Buffer,
    outputWidth,
    houseMaskFull,
    cropWidth,
    cropHeight,
    smartBounds.left,
    smartBounds.top,
    SOLID_WHITE
  );
  applyMaskToBuffer(
    layer2Buffer,
    outputWidth,
    roomMaskFull,
    cropWidth,
    cropHeight,
    smartBounds.left,
    smartBounds.top,
    roomColor
  );
  await putRgbaIntoLayer(
    doc,
    layer2,
    outputBounds,
    layer2Buffer,
    outputWidth,
    outputHeight,
    "生成图层2"
  );

  const layer7Buffer = createTransparentBuffer(outputWidth, outputHeight);
  applyMaskToBuffer(
    layer7Buffer,
    outputWidth,
    roomMaskFull,
    cropWidth,
    cropHeight,
    smartBounds.left,
    smartBounds.top,
    roomColor
  );
  await putRgbaIntoLayer(
    doc,
    layer7,
    outputBounds,
    layer7Buffer,
    outputWidth,
    outputHeight,
    "生成图层7"
  );

  await applyDefaultInnerShadow(layer7);

  return {
    smartLayerName: smartLayer.name,
    smartBounds,
    planBounds: masks.debug.planBBox,
    backgroundColor,
    roomColor,
    roomCount: masks.debug.roomCount,
    roomCandidateCount: masks.debug.roomCandidateCount,
    furnitureCutCount: furnitureCut.debug.furnitureCount,
    furnitureCandidateCount: furnitureCut.debug.candidateCount,
    minFurnitureArea: furnitureCut.debug.minFurnitureArea,
    maxFurnitureArea: furnitureCut.debug.maxFurnitureArea,
    whiteBaseCreated: whiteBaseInfo.created,
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

    const notes = [];
    if (result.whiteBaseCreated) {
      notes.push("已自动创建 PDF 白底。");
    }
    notes.push(
      result.layer7Created
        ? "已自动创建图层7并同步内阴影。"
        : "已同步图层7内阴影。"
    );

    setStatus(
      [
        "已完成。",
        `线稿层：${result.smartLayerName}`,
        `线稿边界：${result.smartBounds.left}, ${result.smartBounds.top}, ${result.smartBounds.right}, ${result.smartBounds.bottom}`,
        `主户型分析框：${result.planBounds.x}, ${result.planBounds.y}, ${result.planBounds.width}, ${result.planBounds.height}`,
        `背景灰：${result.backgroundColor.slice(0, 3).join(", ")}`,
        `房间灰：${result.roomColor.slice(0, 3).join(", ")}`,
        `灰区候选片段：${result.roomCandidateCount}`,
        `最终灰区片段：${result.roomCount}`,
        `家具候选片段：${result.furnitureCandidateCount}`,
        `家具扣白数：${result.furnitureCutCount}`,
        `最小家具面积：${result.minFurnitureArea}`,
        `最大家具面积：${result.maxFurnitureArea}`,
        ...notes
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
