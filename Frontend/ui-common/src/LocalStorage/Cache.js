let cacheMap = window.cacheMap || {};

export const getFromCache = function (key, nothing, just) {
  if (typeof cacheMap[key] !== "undefined") {
    return just(cacheMap[key]);
  } else {
    return nothing;
  }
}

export const setInCache = function (key, value) {
  cacheMap[key] = value;
  return value;
}
export const clearCache = function (key) {
  if (key != "") {
    delete cacheMap[key];
  } else {
    cacheMap = {};
  }
}