
export const getFromWindow = function (key,nothing,just) {
  if (typeof window[key] !== "undefined") {
    return just(window[key]);
  } else {
    return nothing;
  }
}

export const getFromWindowString = getFromWindow;

export const getAnyFromWindow = getFromWindow;

export const setInWindow = function (key,value) {
  window[key] = value;
  return value;
}

export const setAnyInWindow = setInWindow;

// JSON UTILS
export function parseJSON(param) {
  try {
    return JSON.parse(param);
  } catch (e) {
    return param;
  }
}

export const stringifyJSON = function (obj) {
  let result;
  try {
    result = JSON.stringify(obj);
  } catch (errr) {
    result = ""
  }
  return result;
}