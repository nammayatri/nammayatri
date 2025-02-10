const JBridge = window.JBridge;
const isDebug =  JSON.parse(JBridge.getDeviceInfo()).package_name.includes(".debug") || JSON.parse(JBridge.getDeviceInfo()).package_name.includes(".staging")
export const getFromWindow = function (key,nothing,just) {
  if (typeof window[key] !== "undefined") {
    return just(window[key]);
  } else {
    return nothing;
  }
}

export const getFromWindowString = getFromWindow;

export const getAnyFromWindow = getFromWindow;

export const removeFromWindow = function (key) {
  delete window[key];
}

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

export const toastWithLog = function (str) {
  const JOSFlags = window.JOS.getJOSflags()
  if (JOSFlags.isCUGUser || isDebug) {
    if (window.__OS == "IOS") {
      // window.JBridge.toast(str); //remove once toast is fixed in iOS.
    } else {
      window.JBridge.toast(str);
    }
  }
  console.error(str);
};

export const unsafeSetForeign = function (key,obj,value) {
  if (typeof obj === "string") {
    obj = {}
  }
  obj[key] = value;
  return obj;
};

export const unsafeGet = function (key,obj) {
  return obj[key];
};

export const unsafeHas = function (key,obj) {
  return obj[key] !== undefined && obj[key] !== null;
};
