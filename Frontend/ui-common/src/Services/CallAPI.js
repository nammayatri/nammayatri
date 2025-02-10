import {
  callbackMapper as PrestoCallbackMapper
} from "presto-ui";
export const callbackMapper = PrestoCallbackMapper.map


export const addBenchMark = function (key, isEnded) {
  window.benchMark = window.benchMark || {};
  window.benchMark[key] = window.benchMark[key] || {};
  window.benchMark[key].renders = window.benchMark[key].renders || [];
  if (window.benchMark[key].startTime && isEnded) {
    let diff = Date.now() - window.benchMark[key].startTime;
    window.benchMark[key].renders.push(diff);
  } else {
    window.benchMark[key].startTime = Date.now();
  }
}

const getEncodedData = function (data) {
  if (window.__OS == "IOS") {
    return btoa(unescape(encodeURIComponent(data)));
  }
  return data;
}

export const atobImpl = function (value) {
  try {
    return window.atob(value);
  } catch (e) {
    return value;
  }
}

export const callApi = function () {
  return window.JBridge.callAPI(arguments[0], encodeURI(arguments[1]), getEncodedData(arguments[2]), getEncodedData(arguments[3]), arguments[4], arguments[5], arguments[6])
}

export const callAPIWithOptions = function () {
  if (typeof window.JBridge.callAPIWithOptions == "function") {
    return window.JBridge.callAPIWithOptions(arguments[0], encodeURI(arguments[1]), getEncodedData(arguments[2]), getEncodedData(arguments[3]), arguments[4], arguments[5], arguments[6], arguments[7]);
  } else {
    return window.JBridge.callAPI(arguments[0], encodeURI(arguments[1]), getEncodedData(arguments[2]), getEncodedData(arguments[3]), arguments[4], arguments[5], arguments[7]);
  }
}