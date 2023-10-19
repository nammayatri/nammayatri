const JBridge = window.JBridge;

export const toggleLoaderIOS = function(flag){
  console.log("inside toggle loader")
  return JBridge.toggleLoader(flag);
}

export const loaderTextIOS = function(mainTxt, subTxt){
  console.log("inside loader Text IOS")
  return JBridge.loaderText(mainTxt,subTxt);
}

export const getFromWindow = function (key) {
  if (typeof window[key] !== "undefined") {
    return window[key];
  }
}

export const saveToLocalStoreImpl = function(key) {
  return function (state) {
    window.JBridge.setKeysInSharedPrefs(key, state);
    return function () {
    };
  };
}

export const fetchFromLocalStoreImpl = function(key) {
  return function (just) {
    return function (nothing) {
      return function () {
        const state = JBridge.getFromSharedPrefs(key);
        if (state != "__failed" && state != "(null)") {
          return just(state);
        }
        return nothing;
      };
    };
  };
}
export const reboot = window.JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"reboot"}))()

export const showSplash = window.JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"show_splash"}))()