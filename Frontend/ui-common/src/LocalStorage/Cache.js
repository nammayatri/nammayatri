
export const getFromCache = function (key, nothing, just) {
  if (typeof window.cacheMap[key] !== "undefined") {
    return just(window.cacheMap[key]);
  } else {
    return nothing;
  }
}

export const setInCache = function (key, value) {
  window.cacheMap[key] = value;
  return value;
}
export const clearCache = function (key) {
  if (key != "") {
    delete window.cacheMap[key];
  } else {
    window.cacheMap = {};
  }
}
