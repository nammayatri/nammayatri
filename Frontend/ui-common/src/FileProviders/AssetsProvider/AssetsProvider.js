const JBridge = window.JBridge;
const JOS = window.JOS;
import { callbackMapper } from "presto-ui";
export const getFromTopWindow = function (key) {
  return top[key];
}

export const getCUGUser = function () {
  const JOSFlags = JOS.getJOSflags();
  if (JOSFlags && JOSFlags.isCUGUser) {
    return JOSFlags.isCUGUser;
  } else {
    return false;
  }
}

export const isUseLocalAssets = function () {
  if (window.__payload ) {
    return window.__payload.use_local_assets;
  } else {
    return false;
  }
}

export const renewFile = function(filePath,_location,cb) {
  JBridge.renewFile(_location,filePath,callbackMapper.map(function(result) {
    cb(result)();
  }));
}