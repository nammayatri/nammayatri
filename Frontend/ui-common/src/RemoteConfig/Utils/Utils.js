
export const fetchRemoteConfigString = function (key) {
  if (window.JBridge.fetchRemoteConfigString){
    return window.JBridge.fetchRemoteConfigString(key);
  }
  return "";
}