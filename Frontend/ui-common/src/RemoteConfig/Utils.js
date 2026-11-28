
export const fetchRemoteConfigString = function (key) {
  if (window.JBridge.fetchRemoteConfigString){
    return window.JBridge.fetchRemoteConfigString(key);
  }
  return {};
}

export const isWhiteListed = function (key) {
  return function (arr) {
    return arr.includes(key);
  }
}

export const fetchRemoteConfig = fetchRemoteConfigString;