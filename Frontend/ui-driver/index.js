/* eslint-disable no-loop-func */
console.log("APP_PERF INDEX_BUNDLE_START : ", new Date().getTime());

window.timeStamps = window.timeStamps || {}

const date = Date.now();
window.timeStamps["indexBundleEval"] = window.timeStamps["indexBundleEval"] || {}
window.timeStamps["indexBundleEval"]["start"] = date;
window.timeStamps["onCreateToIndexBundle"] = window.timeStamps["onCreateToIndexBundle"] || {}
window.timeStamps["onCreateToIndexBundle"]["end"] = date;
window.timeStamps["initiateToIndexBundle"] = window.timeStamps["initiateToIndexBundle"] || {}
window.timeStamps["initiateToIndexBundle"]["end"] = date;
window.timeStamps["assetDownloaderToIndexBundle"] = window.timeStamps["assetDownloaderToIndexBundle"] || {}
window.timeStamps["assetDownloaderToIndexBundle"]["end"] = date;

window.version = window.version || {};
window.version["app"] = __VERSION__;
let previousDateObject = new Date();
const refreshThreshold = 300;
console.warn("Hello World");
const JBridge = window.JBridge;
const JOS = window.JOS;
window.session_id = JBridge.getSessionId();

console.log("APP_PERF ON JOS READY  END: ", new Date().getTime());

const bundleLoadTime = Date.now();
window.flowTimeStampObject = {};
window.whitelistedNotification = new Set(["DRIVER_ASSIGNMENT", "CANCELLED_PRODUCT", "DRIVER_REACHED", "REALLOCATE_PRODUCT", "TRIP_STARTED", "EDIT_LOCATION", "USER_FAVOURITE_DRIVER", "FROM_METRO_COINS", "TO_METRO_COINS"]);

window.fetchCachedSessionInfo = (key) => {
  window.cacheMap = window.cacheMap || {};
  if (Object.prototype.hasOwnProperty.call(window.cacheMap, "sessionInfo") && Object.prototype.hasOwnProperty.call(window.cacheMap, key)) {
    return window.cacheMap.sessionInfo[key];
  }
  if (window.JBridge.getSessionInfo) {
    const sessionInfo = JSON.parse(window.JBridge.getSessionInfo());
    window.cacheMap["sessionInfo"] = sessionInfo;
    return sessionInfo[key];
  }
}

window.manualEventsName = ["onBackPressedEvent", "onNetworkChange", "onResume", "onPause", "onKeyboardHeightChange", "RestartAutoScroll"];


const logger = function () {
  let oldConsoleLog = null;
  const pub = {};

  pub.enableLogger = function enableLogger() {
    if (oldConsoleLog === null)
      return;

    window["console"]["log"] = oldConsoleLog;
  };

  pub.disableLogger = function disableLogger() {
    oldConsoleLog = console.log;
    window["console"]["log"] = function () {};
  };

  return pub;
}();
console.log("APP_PERF INDEX_LOGGER_END : ", new Date().getTime());

window.__FN_INDEX = window.__FN_INDEX || 0;
window.__PROXY_FN = window.__PROXY_FN || {};
console.log("APP_PERF INDEX_BUNDLE_OS_END : ", new Date().getTime());

let purescript

function getPureScript() {
  if (purescript === null || purescript === undefined) {
    window.timeStamps["pureScriptRead"] = window.timeStamps["pureScriptRead"] || {}
    window.timeStamps["pureScriptRead"]["start"] = Date.now();
    purescript = require("./output/Main");
    window.timeStamps["pureScriptRead"] = window.timeStamps["pureScriptRead"] || {}
    window.timeStamps["pureScriptRead"]["end"] = Date.now();
  }
  return purescript;
}

// JOS.emitEvent("java")("onEvent")(JSON.stringify({ action: "DUI_READY", event: "initiate",service : JOS.self }))()();

function callInitiateResult() {
  const payload = {
    event: "initiate_result",
    service: "in.juspay.becknui",
    payload: {
      action: "initiate",
      status: "SUCCESS"
    },
    error: false,
    errorMessage: "",
    errorCode: ""
  }
  const jpConsumingBackpress = {
    event: "jp_consuming_backpress",
    payload: {
      jp_consuming_backpress: true
    }
  }
  console.log("APP_PERF INDEX_BUNDLE_INITIATE_RESULT : ", new Date().getTime());
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), null)
}

function refreshFlow() {
  const dontCallRefresh = (window.JBridge.getKeysInSharedPref("DONT_CALL_REFRESH") == "true");
  if (dontCallRefresh) {
    window.JBridge.setKeysInSharedPrefs("DONT_CALL_REFRESH", "false");
    return;
  }
  const currentDate = new Date();
  const diff = Math.abs(previousDateObject - currentDate) / 1000;
  const token = window.JBridge.getKeysInSharedPref("REGISTERATION_TOKEN");
  const shouldRefresh = window.JBridge.getKeysInSharedPref("CALL_REFRESH");
  if (((diff > refreshThreshold) && (token != "__failed")) || shouldRefresh == "true") {
    if (window.storeCallBackMessageUpdated) {
      window.__PROXY_FN[window.storeCallBackMessageUpdated] = undefined;
    }
    if (JBridge.removeCallBackOpenChatScreen) {
      JBridge.removeCallBackOpenChatScreen();
    }
    window.chatMessages = undefined;
    window.JBridge.setKeysInSharedPrefs("CALL_REFRESH", "false");
    // purescript.onConnectivityEvent("REFRESH")();
    for(const key in window.onResumeListenersMap) {
      window.onResumeListenersMap[key].call()
    }
  }
}


function makeEvent(_type, _data) {
  return {
    type: _type,
    data: _data
  };
}

function checkForReferral(viewParam, eventType) {
  if (viewParam.slice(0, 8) == "referrer") {
    const referralData = viewParam.substring(viewParam.indexOf("=") + 1);
    getPureScript().onNewIntent(makeEvent(eventType, referralData))();
    return true;
  }
  return false;
}

console.log("APP_PERF INDEX_BUNDLE_END_ON_MERCHANT : ", new Date().getTime());

window.onMerchantEvent = function (_event, payload) {
  console.log(payload);
  const JOSFlags = window.JOS.getJOSflags();
  const clientPayload = JSON.parse(payload);
  window.__payload = clientPayload;
  const clientId = clientPayload.payload.clientId;
  const appName = clientPayload.payload.appName;
  window.loadDynamicModule = clientPayload.payload.loadDynamicModule;
  window.appName = appName;
  window.isCUGUser = JOSFlags.isCUGUser;
  if (_event == "initiate") {
    console.log("APP_PERF INDEX_BUNDLE_INITIATE_START : ", new Date().getTime());
    if (clientId == "yatriprovider") {
      window.merchantID = "NAMMAYATRI"
    } else if (clientId == "jatrisaathiprovider" || clientId == "jatrisaathidriver" || clientId == "yatrisathiprovider") {
      window.merchantID = "YATRISATHI"
    } else if (clientId.includes("provider")) {
      let merchant = clientId.replace("mobility", "")
      merchant = merchant.replace("provider", "");
      window.merchantID = merchant.toUpperCase();
    } else {
      window.merchantID = "NAMMAYATRI";
    }
    try {
      if (
        clientPayload.payload &&
        Object.prototype.hasOwnProperty.call(clientPayload.payload, "onCreateTimeStamp") &&
        Object.prototype.hasOwnProperty.call(clientPayload.payload, "initiateTimeStamp") &&
        typeof window.events.onCreateToInitiateDuration === "undefined" &&
        typeof window.events.initAppToInitiateDuration === "undefined"
      ) {
        const onCreateTimeStamp = clientPayload.payload.onCreateTimeStamp;
        const initiateTimeStamp = clientPayload.payload.initiateTimeStamp;
        window.flowTimeStampObject["onCreateToBundle"] = window.prevTimeStamp - onCreateTimeStamp;
        window.flowTimeStampObject["nativeIntiateToBundle"] = window.prevTimeStamp - initiateTimeStamp;
        window.events.onCreateToInitiateDuration =
          new Date().getTime() - clientPayload.payload.onCreateTimeStamp;
        window.events.initAppToInitiateDuration =
          new Date().getTime() - clientPayload.payload.initiateTimeStamp;
        window.events.onCreateToHomeScreenRenderDuration =
          new Date(clientPayload.payload.onCreateTimeStamp);
        window.events.initAppToHomeScreenRenderDuration =
          new Date(clientPayload.payload.initiateTimeStamp);
        window.timeStamps["onCreateToIndexBundle"] = window.timeStamps["onCreateToIndexBundle"] || {}
        window.timeStamps["onCreateToIndexBundle"]["start"] = onCreateTimeStamp;
        window.timeStamps["initiateToIndexBundle"] = window.timeStamps["initiateToIndexBundle"] || {}
        window.timeStamps["initiateToIndexBundle"]["start"] = initiateTimeStamp;
        window.timeStamps["onCreateToHomeScreenRender"] = window.timeStamps["onCreateToHomeScreenRender"] || {}
        window.timeStamps["onCreateToHomeScreenRender"]["start"] = onCreateTimeStamp;
        window.timeStamps["initAppToHomeScreenRender"] = window.timeStamps["initAppToHomeScreenRender"] || {}
        window.timeStamps["initAppToHomeScreenRender"]["start"] = initiateTimeStamp;
        window.timeStamps["onCreateToAssetDownloader"] = window.timeStamps["onCreateToAssetDownloader"] || {}
        window.timeStamps["onCreateToAssetDownloader"]["start"] = onCreateTimeStamp;
      }
      window.flowTimeStampObject["bundleLoadTime"] = bundleLoadTime - window.prevTimeStamp;
      window.flowTimeStampObject["bundleToInitiate"] = Date.now() - bundleLoadTime;
      window.prevTimeStamp = Date.now();
    } catch (err) {
      console.log(err)
    }
    callInitiateResult();
    setTimeout(() => {
      getPureScript()
    },0) 
  } else if (_event == "process") {
    console.log("APP_PERF INDEX_PROCESS_CALLED : ", new Date().getTime());
    console.warn("Process called");
    const parsedPayload = JSON.parse(payload);
    window.__payload = parsedPayload;
    try {
      if (
        parsedPayload.payload &&
        Object.prototype.hasOwnProperty.call(parsedPayload.payload, "initiateTimeStamp") &&
        typeof window.events.initAppToProcessDuration === "undefined" &&
        typeof window.events.onCreateToProcessDuration === "undefined"
      ) {
        window.events.onCreateToProcessDuration =
          new Date().getTime() - parsedPayload.payload.onCreateTimeStamp;
        window.events.initAppToProcessDuration =
          new Date().getTime() - parsedPayload.payload.initiateTimeStamp;
      }
    } catch (err) {
      console.log(err)
    }
    window.__payload = parsedPayload;
    console.log("window Payload: ", window.__payload);
    const jpConsumingBackpress = {
      event: "jp_consuming_backpress",
      payload: {
        jp_consuming_backpress: true
      }
    }
    JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
    if (parsedPayload.payload.notificationData && parsedPayload.payload.notificationData.notification_type == "NEW_MESSAGE" && parsedPayload.payload.notificationData.entity_ids) {
      getPureScript().main(makeEvent("NEW_MESSAGE", parsedPayload.payload.notificationData.entity_ids))();
    } else if (parsedPayload.payload.notificationData && parsedPayload.payload.notificationData.notification_type == "PAYMENT_MODE_MANUAL") {
      getPureScript().main(makeEvent("PAYMENT_MODE_MANUAL", ""))();
    } else if (parsedPayload.payload.viewParam) {
      if (!checkForReferral(parsedPayload.payload.viewParam, "REFERRAL_NEW_INTENT")) {
        getPureScript().onNewIntent(makeEvent("DEEP_VIEW", parsedPayload.payload.viewParam))();
      }
    } else if (parsedPayload.payload.view_param) {
      if (!checkForReferral(parsedPayload.payload.view_param, parsedPayload.payload.onNewIntent ? "REFERRAL" : "REFERRAL_NEW_INTENT")) {
        const deepLinkType = parsedPayload.payload.onNewIntent ? "DEEP_VIEW_NEW_INTENT" : "DEEP_VIEW";
        const param = parsedPayload.payload.viewParam ? parsedPayload.payload.viewParam : parsedPayload.payload.view_param;
        getPureScript().onNewIntent(makeEvent(deepLinkType, param))();
      }
    } else if (parsedPayload.payload.viewParamNewIntent) {
      if (!checkForReferral(parsedPayload.payload.viewParamNewIntent, "REFERRAL_NEW_INTENT")) {
        getPureScript().onNewIntent(makeEvent("DEEP_VIEW_NEW_INTENT", parsedPayload.payload.viewParamNewIntent))();
      }
    } else if (parsedPayload.payload.action == "process_hv_resp" && parsedPayload.payload.callback && parsedPayload.payload.hv_response) {
      console.log(parsedPayload.payload.callback);
      window.callUICallback(parsedPayload.payload.callback, parsedPayload.payload.hv_response);
    } else if (parsedPayload.payload.action == "gl_process" && parsedPayload.payload.callback && parsedPayload.payload.value) {
      window.callUICallback(parsedPayload.payload.callback, parsedPayload.payload.value);
    } else {
      getPureScript().main(makeEvent("", ""))();
    }
  } else {
    console.error("unknown event: ", event);
  }
}
console.log("APP_PERF INDEX_BUNDLE_END_ON_MERCHANT : ", new Date().getTime());

window.callUICallback = function () {
  const getCurrTime = () => (new Date()).getTime()
  const args = (arguments.length === 1 ? [arguments[0]] : Array.apply(null,
    arguments));
  const fName = args[0]
  const functionArgs = args.slice(1)
  let currTime;
  let timeDiff;
  if (fName) {
    if (window.__THROTTELED_ACTIONS && window.__THROTTELED_ACTIONS.indexOf(fName) == -1) {
      window.__PROXY_FN[fName].apply(null, functionArgs);
    } else if (window.__LAST_FN_CALLED && (fName == window.__LAST_FN_CALLED.fName)) {
      currTime = getCurrTime();
      timeDiff = currTime - window.__LAST_FN_CALLED.timeStamp;

      if (timeDiff >= 100) {
        window.__PROXY_FN[fName].apply(null, functionArgs);
        window.__LAST_FN_CALLED.timeStamp = currTime;
      } else {
        console.warn("function throtteled", fName);
        console.warn("time diff", timeDiff);
      }
    } else {
      window.__PROXY_FN[fName].apply(null, functionArgs);
      window.__LAST_FN_CALLED = {
        timeStamp: (new Date()).getTime(),
        fName: fName
      }
    }
  } else {
    console.error("got empty callback", fName, functionArgs)
  }
};

window.onResumeListeners = [];
window.onResumeListenersMap = {};
window.internetListeners = {};

window.onPause = function () {
  console.error("onEvent onPause");
  if (JBridge.pauseMediaPlayer) {
    JBridge.pauseMediaPlayer();
  }
}

window.onResume = function () {
  console.error("onEvent onResume");
  if (window.onResumeListeners && Array.isArray(window.onResumeListeners)) {
    for (let i = 0; i < window.onResumeListeners.length; i++) {
      try {
        window.onResumeListeners[i].call();
      } catch (err) {
        console.error(err);
      }
    }
  }
  if (window.scrollAction) {
    window.scrollAction();
  }
}

window.onActivityResult = function () {
  console.log(arguments)
}

window.onBackPressed = function () {
  if (window.eventListeners && window.eventListeners["onBackPressed"] && window.enableBackpress) {
    window.eventListeners["onBackPressed"]()();
  }
}

window.callPopUp = function (type, entityPayload) {
  if ((type == "LOCATION_DISABLED") || (type == "INTERNET_ACTION")) {
    getPureScript().onConnectivityEvent(type)();
  } else if (type == "NEW_RIDE_AVAILABLE") {
    getPureScript().mainAllocationPop(type)(entityPayload)();
  } else {
    getPureScript().main(makeEvent("", ""))();
  }
}

window.activityResultListeners = {}
window.eventListeners = {}

window.listenForActivityResult = function (requestCode, callback) {
  window.activityResultListeners[requestCode] = callback;
}
window.onActivityResult = function (requestCode, resultCode, bundle) {
  if (window.activityResultListeners[requestCode]) {
    window.activityResultListeners[requestCode](resultCode, bundle);
    window.activityResultListeners[requestCode] = undefined;
  }
}

window["onEvent'"] = function (_event, args) {
  console.log(_event, args);
  if (_event == "onBackPressed") {
    if (JBridge.onBackPressedPP && JBridge.onBackPressedPP()) {
      console.log("Backpress Consumed by PP")
    } else {
      getPureScript().onEvent(_event)();
    }
  } else if (_event == "onLocationChanged") {
    if (JBridge.isLocationEnabled && !JBridge.isLocationEnabled()) {
      getPureScript().onConnectivityEvent("LOCATION_DISABLED")();
    }
  } else if (_event == "onInternetChanged") {
    getPureScript().onConnectivityEvent("INTERNET_ACTION")();
  } else if (_event == "onPause") {
    previousDateObject = new Date();
    window.onPause();
  } else if (_event == "onResume") {
    window.onResume();
    refreshFlow();
  } else if (_event == "onBundleUpdated") {
    getPureScript().onBundleUpdatedEvent(JSON.parse(args))();
  } else if (_event == "onTimeChanged") {
    if (window.dateCallback != undefined) {
      window.dateCallback();
    } else {
      getPureScript().onConnectivityEvent("CHECK_NETWORK_TIME")();
    }
  } else if ((_event == "onKeyboardOpen" || _event == "onKeyboardClose") && window.keyBoardCallback) {
    window.keyBoardCallback(_event);
  } else if (_event === "onReloadApp") {
    getPureScript().onEvent(_event)();
  }
}

window["onEvent"] = function (jsonPayload, args, callback) { // onEvent from hyperPay
  console.log("onEvent Payload", jsonPayload);
  const payload = JSON.parse(jsonPayload)
  switch (payload.event) {
    case "initiate_result":
      window.isPPInitiated = true;
      break;
    case "process_result":
      if (window.processCallBack) window.processCallBack(0)(jsonPayload)();
      break;
    case "log_stream":
      const logs = require("./output/Engineering.Helpers.LogEvent/index.js");
      logs.handleLogStream(payload.payload);
      break;
    default:
      console.log("Unknown Event");
  }
}

const sessionInfo = JSON.parse(JBridge.getDeviceInfo())
const enableLogs = JBridge.fetchRemoteConfigBool && JBridge.fetchRemoteConfigBool("enable_logs")

const JOSFlags = window.JOS.getJOSflags()
if (sessionInfo.package_name.includes(".debug") || sessionInfo.package_name.includes(".staging") || enableLogs || JOSFlags.isCUGUser) {
  logger.enableLogger();
  window.Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_true;", "null");
} else {
  logger.disableLogger();
  window.Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_false;", "null");
}

JBridge.runInJuspayBrowser("onEvent", JSON.stringify({
  action: "DUI_READY",
  event: "initiate",
  service: JOS.self
}), "");

eval(window.JBridge.loadFileInDUI("v1-tracker.jsa"));


window.JOS.tracker = window.getTrackerModule.Main.initTracker()
window.tracker =  window.JOS.tracker


if (window.eventQueue) {
  while (window.eventQueue.length) {
    const args = window.eventQueue.pop();
    window.onMerchantEvent.apply(null, args)
  }
}

window.timeStamps["indexBundleEval"] = window.timeStamps["indexBundleEval"] || {}
window.timeStamps["indexBundleEval"]["end"] = Date.now();
// console.log("Start : ", new Date().getTime());
// eval(window._jbridge.loadFileInDUI("index_bundle.js"));
// console.log("End : ", new Date().getTime());