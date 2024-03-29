let cacheMap = {};

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
export const clearCache = function () {
  cacheMap = {};
}