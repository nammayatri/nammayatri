console.log("APP_PERF INDEX_BUNDLE_START : ", new Date().getTime(), Object.assign({},window.__payload));
import "core-js";
import "presto-ui";
import "regenerator-runtime/runtime";

window.events = {};
try {
  if (typeof window.assetDownloadDuration === "number") {
    window.events.assetDownloadDuration = Date.now() - window.assetDownloadDuration;
  }
} catch (err) {}
const bundleLoadTime = Date.now();
window.flowTimeStampObject = {};
window.whitelistedNotification = ["DRIVER_ASSIGNMENT", "CANCELLED_PRODUCT", "DRIVER_REACHED", "REALLOCATE_PRODUCT", "TRIP_STARTED", "EDIT_LOCATION"];

console.log("APP_PERF INDEX_FIREBASE_LOG_PARAMS_START : ", new Date().getTime());
// Will enable to debug anrs
// const blackListFunctions = ["getFromSharedPrefs", "getKeysInSharedPref", "setInSharedPrefs", "addToLogList", "requestPendingLogs", "sessioniseLogs", "setKeysInSharedPrefs", "getLayoutBounds"]
// if (window.JBridge.firebaseLogEventWithParams){  
//   Object.getOwnPropertyNames(window.JBridge).filter((fnName) => {
//     return blackListFunctions.indexOf(fnName) == -1
//   }).forEach(fnName => {
//       window.JBridgeProxy = window.JBridgeProxy || {};
//       window.JBridgeProxy[fnName] = window.JBridge[fnName];
//       window.JBridge[fnName] = function () {
//         let params = Object.values(arguments).join(", ");
//         if (fnName === "callAPI") {
//           params = arguments[1].split("/").splice(6).join("/");
//         }
//         let shouldLog = true;
//         if (window.appConfig) {
//         shouldLog = window.appConfig.logFunctionCalls ? window.appConfig.logFunctionCalls : shouldLog;
//         }
//         if (shouldLog) {
//         window.JBridgeProxy.firebaseLogEventWithParams("ny_fn_" + fnName,"params",JSON.stringify(params));
//         }
//         console.log("fnName ->", fnName);
//         const result = window.JBridgeProxy[fnName](...arguments);
//         return result;
//       };
//     });
// }
console.log("APP_PERF INDEX_FIREBASE_LOG_PARAMS_END : ", new Date().getTime());

function guid() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
  }
  return s4() + s4() + "-" + s4() + "-" + s4() + "-" +
    s4() + "-" + s4() + s4() + s4();
}

window.fetchCachedSessionInfo = (key) => {
  window.cacheMap = window.cacheMap || {};
  if (Object.prototype.hasOwnProperty.call(window.cacheMap,"sessionInfo") && Object.prototype.hasOwnProperty.call(window.cacheMap,key)) {
    return window.cacheMap.sessionInfo[key];
  }
  if(window.JBridge.getSessionInfo){
    const sessionInfo = JSON.parse(window.JBridge.getSessionInfo());
    window.cacheMap["sessionInfo"] = sessionInfo;
    return sessionInfo[key];
  }
}

window.session_id = guid();
window.version = window.version || {};
window.version["app"] = __VERSION__;
let previousDateObject = new Date();
const refreshThreshold = 300;
console.warn("Hello World");
const JBridge = window.JBridge;
const JOS = window.JOS;


window.isObject = function (object) {
  return (typeof object == "object");
}
window.manualEventsName = ["onBackPressedEvent", "onNetworkChange", "onResume", "onPause", "onKeyboardHeightChange", "RestartAutoScroll"];

setInterval(function () { JBridge.submitAllLogs(); }, 10000);

const isUndefined = function (val) {
  return (typeof val == "undefined");
}

const logger = function()
{
  let oldConsoleLog = null;
  const pub = {};

  pub.enableLogger =  function enableLogger()
  {
    if(oldConsoleLog === null)
      return;

    window["console"]["log"] = oldConsoleLog;
  };

  pub.disableLogger = function disableLogger()
  {
    oldConsoleLog = console.log;
    window["console"]["log"] = function() {};
  };

  return pub;
}();
window.storeMerchantId = function (payload) {
  const clientPaylod = payload ? payload : window.__payload;
  let clientId = clientPaylod.payload.clientId;
  if (clientId.includes("_ios")) {
    clientId = clientId.replace("_ios", "");
  }
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
}

console.log("APP_PERF INDEX_LOGGER_END : ", new Date().getTime());

function setManualEvents(eventName, callbackFunction) {
  window[eventName] = (!isUndefined(window[eventName])) ? window[eventName] : {};
  if (!isUndefined(window.__dui_screen)) {
    window[eventName][window.__dui_screen] = callbackFunction;
    if ((!isUndefined(window.__currScreenName.value0)) && (window.__dui_screen != window.__currScreenName.value0)) {
      console.warn("window.__currScreenName is varying from window.__currScreenName");
    }
  } else {
    console.error("Please set value to __dui_screen --shouldn't come here");
  }
}

window.setManualEvents = setManualEvents;
window.__FN_INDEX = 0;
window.__PROXY_FN = {};

if (!window.__OS) {
  const getOS = function () { //taken from getOS() in presto-ui
    const userAgent = navigator.userAgent;
    if (!userAgent) return console.error(new Error("UserAgent is null"));
    if (userAgent.indexOf("Android") != -1 && userAgent.indexOf("Version") != -1) return "ANDROID";
    if (userAgent.indexOf("iPhone") != -1 && userAgent.indexOf("Version") == -1) return "IOS";
    return "WEB";
  }
  window.__OS = getOS();
}
console.log("APP_PERF INDEX_BUNDLE_OS_END : ", new Date().getTime());

const purescript = require("./output/Main");

function callInitiateResult () {
  const payload = {
    event: "initiate_result"
    , service: "in.juspay.becknui"
    , payload: { action : "initiate", status: "SUCCESS" , fromDriver : true}
    , error: false
    , errorMessage: ""
    , errorCode: ""
  }
  const jpConsumingBackpress = {
    event: "jp_consuming_backpress",
    payload: { jp_consuming_backpress: true }
  }
  console.log("APP_PERF INDEX_BUNDLE_INITIATE_RESULT : ", new Date().getTime());
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
  JOS.emitEvent(JOS.parent)("onEvent")(JSON.stringify(payload))()();
}

function refreshFlow(){
  const currentDate = new Date();
  const diff = Math.abs(previousDateObject - currentDate) / 1000;
  const token = window.JBridge.getKeysInSharedPref("REGISTERATION_TOKEN");
  const shouldRefresh = window.JBridge.getKeysInSharedPref("CALL_REFRESH");
  if (((diff > refreshThreshold) && (token != "__failed")) || shouldRefresh == "true") {
    if(window.storeCallBackMessageUpdated){
      window.__PROXY_FN[window.storeCallBackMessageUpdated] = undefined;
    }
    if(JBridge.removeCallBackOpenChatScreen) {
      JBridge.removeCallBackOpenChatScreen();
    }
    window.chatMessages = undefined;
    window.JBridge.setKeysInSharedPrefs("CALL_REFRESH", "false");
    purescript.onConnectivityEvent("REFRESH")();
  }
}


function makeEvent(_type, _data) {
  return { type : _type, data : _data };
}

function checkForReferral(viewParam, eventType) {
  if (viewParam.slice(0,8) == "referrer") {
    let referralData = viewParam.substring(viewParam.indexOf('=')+1);
    purescript.onNewIntent(makeEvent(eventType, referralData))();
    return true;
  }
  return false;
}


window.onMerchantEvent = function (_event, payload) {
  console.log(payload);
  const JOSFlags = window.JOS.getJOSflags();
  const clientPaylod = JSON.parse(payload);
  const appName = clientPaylod.payload.appName;
  window.appName = appName;
  window.isCUGUser = JOSFlags.isCUGUser;
  if (_event == "initiate") {
    window.storeMerchantId(clientPaylod)
    console.log("APP_PERF INDEX_BUNDLE_INITIATE_START : ", new Date().getTime());
    try {
      if (
        clientPaylod.payload &&
        Object.prototype.hasOwnProperty.call(clientPaylod.payload,"onCreateTimeStamp") &&
        Object.prototype.hasOwnProperty.call(clientPaylod.payload,"initiateTimeStamp") &&
        typeof window.events.onCreateToInitiateDuration === "undefined" &&
        typeof window.events.initAppToInitiateDuration === "undefined"
      ) {
        const onCreateTimeStamp = clientPaylod.payload.onCreateTimeStamp;
        const initiateTimeStamp = clientPaylod.payload.initiateTimeStamp;
        window.flowTimeStampObject["onCreateToBundle"] = window.prevTimeStamp - onCreateTimeStamp;
        window.flowTimeStampObject["nativeIntiateToBundle"] = window.prevTimeStamp - initiateTimeStamp;
        window.events.onCreateToInitiateDuration =
          new Date().getTime() - clientPaylod.payload.onCreateTimeStamp;
        window.events.initAppToInitiateDuration =
          new Date().getTime() - clientPaylod.payload.initiateTimeStamp;
        window.events.onCreateToHomeScreenRenderDuration =
          new Date(clientPaylod.payload.onCreateTimeStamp);
        window.events.initAppToHomeScreenRenderDuration = 
          new Date(clientPaylod.payload.initiateTimeStamp);
      }
      window.flowTimeStampObject["bundleLoadTime"] = bundleLoadTime - window.prevTimeStamp;
      window.flowTimeStampObject["bundleToInitiate"] = Date.now() - bundleLoadTime;
      window.prevTimeStamp = Date.now();
    } catch (err) {
      console.error(err)
    }
    callInitiateResult();
  } else if (_event == "process") {
    console.log("APP_PERF INDEX_PROCESS_CALLED : ", new Date().getTime());
    window.__payload.sdkVersion = "2.0.1"
    console.warn("Process called");
    const parsedPayload = JSON.parse(payload);
    try {
      if (
        parsedPayload.payload &&
        parsedPayload.payload.hasOwnProperty("initiateTimeStamp") &&
        typeof window.events.initAppToProcessDuration === "undefined" &&
        typeof window.events.onCreateToProcessDuration === "undefined"
      ) {
        window.events.onCreateToProcessDuration =
          new Date().getTime() - parsedPayload.payload.onCreateTimeStamp;
        window.events.initAppToProcessDuration =
          new Date().getTime() - parsedPayload.payload.initiateTimeStamp;
      }
    } catch (err) {}
    if (parsedPayload && parsedPayload.payload && parsedPayload.payload.action == "callDriverAlert" && parsedPayload.payload.id && parsedPayload.payload.popType) {
      // purescript.alertNotification(parsedPayload.payload.id)();
      console.log("alert notification called");
    }else if (parsedPayload && parsedPayload.payload && parsedPayload.payload.action == "showPopup" && parsedPayload.payload.id && parsedPayload.payload.popType){
      window.callPopUp(parsedPayload.payload.popType, parsedPayload.payload.entityPayload);
    }
    else {
      if (window.__payload.payload.fragmentViewGroups) {
        const fvg = window.__payload.payload.fragmentViewGroups;
        window.__payload = parsedPayload;
        window.__payload.payload.fragmentViewGroups = fvg
      } else {
        window.__payload = parsedPayload;
      }
      
      console.log("window Payload: ", window.__payload);
      const jpConsumingBackpress = {
        event: "jp_consuming_backpress",
        payload: { jp_consuming_backpress: true }
      }
      JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
      if (parsedPayload.payload.notificationData && parsedPayload.payload.notificationData.notification_type == "NEW_MESSAGE" && parsedPayload.payload.notificationData.entity_ids) {
        purescript.main(makeEvent("NEW_MESSAGE", parsedPayload.payload.notificationData.entity_ids))(parsedPayload.payload.driverInfoResponse)(true)();
      }else if (parsedPayload.payload.notificationData && parsedPayload.payload.notificationData.notification_type == "PAYMENT_MODE_MANUAL") {
        purescript.main(makeEvent("PAYMENT_MODE_MANUAL", ""))(parsedPayload.payload.driverInfoResponse)(true)();
      } else if (parsedPayload.payload.viewParam){
        if (!checkForReferral(parsedPayload.payload.viewParam, "REFERRAL_NEW_INTENT")) {
          purescript.onNewIntent(makeEvent("DEEP_VIEW", parsedPayload.payload.viewParam))();
        }
      } else if (parsedPayload.payload.view_param){
        if (!checkForReferral(parsedPayload.payload.view_param, parsedPayload.payload.onNewIntent ? "REFERRAL" : "REFERRAL_NEW_INTENT")) {
          const deepLinkType = parsedPayload.payload.onNewIntent ? "DEEP_VIEW_NEW_INTENT" : "DEEP_VIEW";
          const param = parsedPayload.payload.viewParam ? parsedPayload.payload.viewParam : parsedPayload.payload.view_param;
          purescript.onNewIntent(makeEvent(deepLinkType, param))();
        }
      } else if (parsedPayload.payload.viewParamNewIntent){
        if (!checkForReferral(parsedPayload.payload.viewParamNewIntent, "REFERRAL_NEW_INTENT")) {
          purescript.onNewIntent(makeEvent("DEEP_VIEW_NEW_INTENT", parsedPayload.payload.viewParamNewIntent))();
        }
      } else if (parsedPayload.payload.action == "process_hv_resp" && parsedPayload.payload.callback && parsedPayload.payload.hv_response) {
        console.log(parsedPayload.payload.callback);
        window.callUICallback(parsedPayload.payload.callback, parsedPayload.payload.hv_response);
      } else {
        purescript.main(makeEvent("", ""))(parsedPayload.payload.driverInfoResponse)(!parsedPayload.payload.onNewIntent)();
      }
    }
  } else if (_event == "update"){
    window.__payload = JSON.parse(payload)
  } else {
    console.error("unknown event: ", _event, payload);
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
};

window.onResumeListeners = [];

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

window.callPopUp = function(type, entityPayload){
  if ((type == "LOCATION_DISABLED") || ( type == "INTERNET_ACTION" )){
    purescript.onConnectivityEvent(type)();
  } else if(type == "NEW_RIDE_AVAILABLE"){
    purescript.mainAllocationPop(type)(entityPayload)();}
  else{
    purescript.main(makeEvent("", ""))();
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
      purescript.onEvent(_event)();
    }
  } else if (_event == "onLocationChanged") {
    if (JBridge.isLocationEnabled && !JBridge.isLocationEnabled()) {
      purescript.onConnectivityEvent("LOCATION_DISABLED")();
    }
  } else if (_event == "onInternetChanged") {
    purescript.onConnectivityEvent("INTERNET_ACTION")();
  } else if (_event == "onPause") {
    previousDateObject = new Date();
    window.onPause();
  } else if (_event == "onResume") {
    window.onResume();
    refreshFlow();
  } else if (_event == "onBundleUpdated") {
    purescript.onBundleUpdatedEvent(JSON.parse(args))();
  } else if (_event == "onTimeChanged") {
    if (window.dateCallback != undefined) {
      window.dateCallback();
    } else {
      purescript.onConnectivityEvent("CHECK_NETWORK_TIME")();
    }
  } else if ((_event == "onKeyboardOpen" || _event == "onKeyboardClose") && window.keyBoardCallback) {
    window.keyBoardCallback(_event);
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

// window.onFileUpdated = function () {
//   console.log("arguments -=>", arguments)
//   //JBridge.toast("GOT UPDATE");
//   if (arguments[0].includes("v1-index_bundle.jsa")) {
//     //JBridge.toast("BUNDLE UPDATE AVAILABLE");
//     JBridge.runInJuspayBrowser("onEvent", JSON.stringify({
//       event: "reboot",
//       service: "in.juspay.becknui",
//       payload: {},
//       error: false,
//       errorMessage: "",
//       errorCode: ""
//     }), "");
//     console.log("arguments after -=>", arguments)
//   }

// }



if (typeof window.JOS != "undefined") {
  window.JOS.addEventListener("onEvent'")();
  window.JOS.addEventListener("onEvent")(); // adding onEvent listener for hyperPay
  window.JOS.addEventListener("onMerchantEvent")();
  window.JOS.addEventListener("onActivityResult")();
  // window.JOS.addEventListener("onFileUpdated")();
  console.error("Calling action DUI_READY");
  JOS.emitEvent("java")("onEvent")(JSON.stringify({ action: "DUI_READY", event: "initiate",service : JOS.self }))()();
} else {
  console.error("JOS not present")
}

// const sessionInfo = JSON.parse(JBridge.getDeviceInfo())

// if (sessionInfo.package_name.includes("debug")) {
//   logger.enableLogger();
// } else {
//   logger.disableLogger();
// }

const sessionInfo = JSON.parse(JBridge.getDeviceInfo())
const enableLogs = JBridge.fetchRemoteConfigBool && JBridge.fetchRemoteConfigBool("enable_logs")

const JOSFlags = window.JOS.getJOSflags()
if (sessionInfo.package_name.includes(".debug") || sessionInfo.package_name.includes(".staging") || enableLogs || JOSFlags.isCUGUser || JOSFlags.isDevQa.isDevQa) {
  logger.enableLogger();
  Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_true;", "null");
} else {
  logger.disableLogger();
  Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_false;", "null");
}

console.log("APP_PERF INDEX_BUNDLE_END : ", new Date().getTime());