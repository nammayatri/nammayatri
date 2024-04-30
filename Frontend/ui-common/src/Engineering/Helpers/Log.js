let debugMode = null;

export const log = function (a) {
  return function (b) {
    console.log(a, " : ", b);
  };
};

export const loggerEnabled = function () {
  return debugMode != null && debugMode != undefined 
          ? debugMode
          : (() => {
              const sessionInfo = JSON.parse(JBridge.getDeviceInfo())
              debugMode = (sessionInfo.package_name.includes(".debug") || sessionInfo.package_name.includes(".staging"))
              return debugMode;
            })();
}

export const logDebug = function (level) {
  return function (key) {
    return function (value) {
      switch (level) {
        case 'info' : console.info(key, " : ", value); break;
        case "error" : console.error(key, " : ", value); break;
        case "warning" : console.warn(key, " : ", value); break;
        default: console.log(key, " : ", value); break;;
      }
    }
  };
};

export const getWindowKey = function (key) {
  return window[key] ? window[key] : "";
}