import { callbackMapper } from "presto-ui";
const JBridge = window.JBridge

export const removeRippleCircle = function (config) {
  if (JBridge.removeRippleCircle) {
    JBridge.removeRippleCircle(JSON.stringify(config))
  }
}
export const addRippleCircle = function (config) {
  if (JBridge.addRippleCircle) {
    JBridge.addRippleCircle(JSON.stringify(config))
  }
}
export const animateRippleCircle = function (config) {
  if (JBridge.animateRippleCircle) {
    JBridge.animateRippleCircle(JSON.stringify(config))
  }
}
export const updateRippleCirclePosition = function (config) {
  if (JBridge.updateRippleCirclePosition) {
    JBridge.updateRippleCirclePosition(JSON.stringify(config))
  }
}
export const addGroundOverlay = function (config) {
  if (JBridge.addGroundOverlay) {
    JBridge.addGroundOverlay(JSON.stringify(config))
  }
}
export const removeGroundOverlay = function (config) {
  if (JBridge.removeGroundOverlay) {
    JBridge.removeGroundOverlay(JSON.stringify(config))
  }
}
export const updateGroundOverlay = function (config) {
  if (JBridge.updateGroundOverlay) {
    JBridge.updateGroundOverlay(JSON.stringify(config))
  }
}

export const isOverlayPresent = function (id) {
  if (JBridge.isOverlayPresent) {
    return JBridge.isOverlayPresent(id)
  } else return false;
}

export const isCirclePresent = function (id) {
  if (JBridge.isCirclePresent) {
    return JBridge.isCirclePresent(id)
  } else return false;
}
export const clearMap = function () {
  if (JBridge.clearMap) {
    return JBridge.clearMap()
  } else return false;
}
export const upsertMarkerLabel = function (config) {
  if (JBridge.upsertMarkerLabel) {
    JBridge.upsertMarkerLabel(JSON.stringify(config))
  }
}