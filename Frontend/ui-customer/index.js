console.log("APP_PERF INDEX_BUNDLE_START : ", new Date().getTime());
import "core-js";
import "presto-ui";
import "regenerator-runtime/runtime";

window.events = {};
try {
  if (typeof window.assetDownloadDuration === "number") {
    window.events.assetDownloadDuration =
      Date.now() - window.assetDownloadDuration;
  }
} catch (err) {}

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

const blackListFunctions = new Set(["getFromSharedPrefs", "getKeysInSharedPref", "setInSharedPrefs", "requestPendingLogs", "sessioniseLogs", "setKeysInSharedPrefs", "getLayoutBounds", "addToLogList"])

if (window.JBridge.firebaseLogEventWithParams && window.__OS != "IOS"){  
  Object.getOwnPropertyNames(window.JBridge).filter((fnName) => {
    return !blackListFunctions.has(fnName);
  }).forEach(fnName => {
      window.JBridgeProxy = window.JBridgeProxy || {};
      window.JBridgeProxy[fnName] = window.JBridge[fnName];
      window.JBridge[fnName] = function () {
        let params = Object.values(arguments).join(", ");
        if (fnName === "callAPI") {
          params = arguments[1].split("/").splice(6).join("/");
        }
        let shouldLog = true;
        if (window.decodeAppConfig) {
          shouldLog = window.decodeAppConfig.logFunctionCalls ? window.decodeAppConfig.logFunctionCalls : shouldLog;
        }
        if (shouldLog) {
          window.JBridgeProxy.firebaseLogEventWithParams("ny_fn_" + fnName,"params",JSON.stringify(params));
        }
        try{
        const result = window.JBridgeProxy[fnName](...arguments);
        return result;
        }
        catch(e){
          console.error("Error in index.js" + e);
          return;
        }
      };
    });
}

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
// JBridge.setSessionId(window.session_id);
console.warn("Hello World MASTER ONE");
let previousDateObject = new Date();
const refreshThreshold = 30;
const homeScreenRT = 300;
const JBridge = window.JBridge;
const JOS = window.JOS;
const Android = window.Android;

const eventObject = {
  type : ""
  , data : ""
}

function makeEvent(_type, _data) {
  return { type : _type, data : _data };
}

window.isObject = function (object) {
  return (typeof object == "object");
}
window.manualEventsName = ["onBackPressedEvent", "onNetworkChange", "onResume", "onPause", "onKeyboardHeightChange", "onKeyboardClose", "onKeyboardOpen", "RestartAutoScroll"];
window.whitelistedNotification = new Set(["DRIVER_ASSIGNMENT", "CANCELLED_PRODUCT", "TRIP_FINISHED", "TRIP_STARTED", "REALLOCATE_PRODUCT", "FOLLOW_RIDE", "SOS_TRIGGERED", "SOS_RESOLVED", "SOS_MOCK_DRILL", "SHARE_RIDE", "SAFETY_ALERT_DEVIATION", "SOS_MOCK_DRILL_NOTIFY", "STOP_REACHED"]);
// setInterval(function () { JBridge.submitAllLogs(); }, 10000);

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
window.__PROXY_FN = top.__PROXY_FN || {};

const purescript = require("./output/Main");
// if (window.__OS == "WEB") {
//   purescript.main();
// }

function callInitiateResult () {
  const payload = {
    event: "initiate_result"
    , service: "in.juspay.becknui"
    , payload: { action : "initiate", status: "SUCCESS" }
    , error: false
    , errorMessage: ""
    , errorCode: ""
  }
  console.log("APP_PERF INDEX_BUNDLE_INITIATE_RESULT : ", new Date().getTime());
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), null)
}

function shouldRefresh() {
  const currentDate = new Date();
  const diff = Math.abs(previousDateObject - currentDate) / 1000;
  const token = (window.JBridge.getKeysInSharedPref("REGISTERATION_TOKEN"));
  const currentState = (window.JBridge.getKeysInSharedPref("LOCAL_STAGE"));
  const refreshTh = (currentState == "RideStarted" || currentState == "RideAccepted") ? refreshThreshold : homeScreenRT;
  return ((diff > refreshTh) && 
      (token != "__failed") && 
      (token != "(null)") &&
      checkInternet()
    )
}

window.onMerchantEvent = function (_event, globalPayload) {
  console.log(globalPayload);
  window.__payload = JSON.parse(globalPayload);
  const clientPaylod = window.__payload.payload;
  const appName = clientPaylod.appName;
  if (clientPaylod.notification_type == "TRIGGER_FCM" && window.__OS == "IOS") {
    clientPaylod["chatMessageData"] = JSON.parse(clientPaylod?.fullNotificationBody?.data?.entity_data);
  }
  window.appName = appName;
  if (_event == "initiate") {
    console.log(
      "APP_PERF INDEX_BUNDLE_INITIATE_START : ",
      new Date().getTime()
    );
    let clientId = clientPaylod.clientId;
    if (clientId.includes("_ios"))
    {
      clientId = clientId.replace("_ios","");
    }
    if (!clientId.startsWith("mobility") && clientId.includes("consumer")) {
      let merchant = clientId.replace("mobility","")
      merchant = merchant.replace("consumer","")
      window.merchantID = merchant.toUpperCase();
    } else {
      let merchant = clientId.replace("mobility","")
      merchant = merchant.replace("consumer","")
      merchant = merchant.toUpperCase();
      window.merchantID = "MOBILITY_" + merchant.charAt(0) + merchant.charAt(merchant.length - 1);
    }
    if (window.merchantID == "YATRI") {
      window.merchantID = "NAMMAYATRI";
    }
    console.log("MID",window.merchantID);
    try {
      if (
        clientPaylod.hasOwnProperty("onCreateTimeStamp") &&
        clientPaylod.hasOwnProperty("initiateTimeStamp") &&
        typeof window.events.onCreateToInitiateDuration === "undefined" &&
        typeof window.events.initAppToInitiateDuration === "undefined"
      ) {
        window.events.onCreateToInitiateDuration =
          new Date().getTime() - clientPaylod.onCreateTimeStamp;
        window.events.initAppToInitiateDuration =
          new Date().getTime() - clientPaylod.initiateTimeStamp;
        window.events.onCreateToHomeScreenRenderDuration = new Date(
          clientPaylod.onCreateTimeStamp
        );
        window.events.initAppToHomeScreenRenderDuration = new Date(
          clientPaylod.initiateTimeStamp
        );
      }
    } catch (err) {}
    callInitiateResult();
  } else if (_event == "process") {
    console.log("APP_PERF INDEX_PROCESS_CALLED : ", new Date().getTime());
    console.warn("Process called");
    const jpConsumingBackpress = {
      event: "jp_consuming_backpress",
      payload: { jp_consuming_backpress: true }
    }
    JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
    try {
      const clientPaylod = window.__payload.payload;
      if (
        clientPaylod &&
        clientPaylod.hasOwnProperty("initiateTimeStamp") &&
        typeof window.events.initAppToProcessDuration === "undefined" &&
        typeof window.events.onCreateToProcessDuration === "undefined"
      ) {
        window.events.onCreateToProcessDuration =
          new Date().getTime() - clientPaylod.onCreateTimeStamp;
        window.events.initAppToProcessDuration =
          new Date().getTime() - clientPaylod.initiateTimeStamp;
      }
    } catch (err) {
      console.error(err);
    }
    if (clientPaylod.action == "notification" && clientPaylod.notification_content && clientPaylod.notification_content.type) {
      window.callNotificationCallBack(clientPaylod.notification_content.type);
    } else if ((clientPaylod.action == "OpenChatScreen" || (window.__OS == "IOS" && clientPaylod.notification_type == "CHAT_MESSAGE")) && window.openChatScreen && !shouldRefresh()) {
      window.openChatScreen();
    } else {
      console.log("client Payload: ", clientPaylod);
      if(clientPaylod.notification_type == "TRIGGER_FCM"){
        purescript.main(makeEvent("", ""))(!clientPaylod.onNewIntent)();
      }else if(clientPaylod.notification_type == "SOS_MOCK_DRILL" || clientPaylod.notificationData && clientPaylod.notificationData.notification_type == "SOS_MOCK_DRILL"){
        purescript.mockFollowRideEvent(makeEvent("SOS_MOCK_DRILL", ""))();
      }else if(clientPaylod.notification_type == "SOS_MOCK_DRILL_NOTIFY" || clientPaylod.notificationData && clientPaylod.notificationData.notification_type == "SOS_MOCK_DRILL_NOTIFY"){
        purescript.mockFollowRideEvent(makeEvent("SOS_MOCK_DRILL_NOTIFY", ""))();
      }else if(clientPaylod.notification_type == "CHAT_MESSAGE" || (clientPaylod.notificationData && clientPaylod.notificationData.notification_type == "CHAT_MESSAGE")){
        purescript.main(makeEvent("CHAT_MESSAGE", ""))(!clientPaylod.onNewIntent)();
      }else if (clientPaylod.viewParamNewIntent && clientPaylod.viewParamNewIntent.slice(0, 8) == "referrer") {
        purescript.onNewIntent(makeEvent("REFERRAL", clientPaylod.viewParamNewIntent.slice(9)))();
      }else if (clientPaylod.viewParam && clientPaylod.viewParam.slice(0, 8) == "referrer") {
        purescript.onNewIntent(makeEvent("REFERRAL_NEW_INTENT", clientPaylod.viewParam.slice(9)))();
      }else if(clientPaylod.notification_type == "SAFETY_ALERT_DEVIATION" || (clientPaylod.notificationData && clientPaylod.notificationData.notification_type == "SAFETY_ALERT_DEVIATION")){
        purescript.main(makeEvent("SAFETY_ALERT_DEVIATION", ""))(!clientPaylod.onNewIntent)();
      }else if (!clientPaylod.notification_type || !clientPaylod.onNewIntent) {
        purescript.main(makeEvent("", ""))(!clientPaylod.onNewIntent)();
      }
    }
  }
}

window.callUICallback = function () {
  const args = (arguments.length === 1 ? [arguments[0]] : Array.apply(null, arguments));
  const fName = args[0]
  const functionArgs = args.slice(1)

  try {
    window.__PROXY_FN[fName].call(null, ...functionArgs);
  } catch (err) {
    console.error(err)
  }
}

window.onResumeListeners = [];
window.internetListeners = {};
window.onPauseListeners = [];

window.onPause = function () {
  console.error("onEvent onPause");
  if (window.onPauseListeners && Array.isArray(window.onPauseListeners)) {
    for (let i = 0; i < window.onPauseListeners.length;i++) {
      window.onPauseListeners[i].call();
    }
  }
  if(JBridge.unregisterShakeListener) {
    JBridge.unregisterShakeListener();
  }
}
function checkInternet() {
  return window.__OS === "IOS" ? JBridge.isNetworkAvailable() === "1" : JBridge.isInternetAvailable();
}

window.onResume = function () {
  console.error("onEvent onResume");
  if (window.onResumeListeners && Array.isArray(window.onResumeListeners)) {
    for (let i = 0; i < window.onResumeListeners.length;i++) {
      window.onResumeListeners[i].call();
    }
    if(window.scrollAction) {
      window.scrollAction();
    }
    if(JBridge.registerShakeListener) {
      JBridge.registerShakeListener();
    }
  }
}

window.onActivityResult = function () {
  console.log(arguments)
}
// window.__trackAPICalls = __trackAPICalls;

window.onBackPressed = function () {
  if (window.eventListeners && window.eventListeners["onBackPressed"] && window.enableBackpress) {
    window.eventListeners["onBackPressed"]()();
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

function refreshFlow(){
  if (shouldRefresh()){
    if(window.storeCallBackMessageUpdated){
      window.__PROXY_FN[window.storeCallBackMessageUpdated] = undefined;
    }
    window.chatMessages = undefined;
    purescript.onConnectivityEvent("REFRESH")();
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
  } else if (_event == "onPause") {
    previousDateObject = new Date();
    window.onPause();
  } else if (_event == "onResume") {
    window.onResume();
    refreshFlow();
  } else if (_event == "onLocationChanged" && !(window.receiverFlag)) {
    purescript.onConnectivityEvent("LOCATION_DISABLED")();
  } else if (_event == "onInternetChanged") {
    if(window.noInternetAction) window.noInternetAction();
    else purescript.onConnectivityEvent("INTERNET_ACTION")();
  }else if(_event == "onBundleUpdated"){
    purescript.onBundleUpdatedEvent(JSON.parse(args))();
  } else if ((_event == "onKeyboardOpen" || _event == "onKeyboardClose") && window.keyBoardCallback) {
    window.keyBoardCallback(_event);
  } else if (_event == "onLocationFetch") {
    console.log("Location Fetch Event", args);
    window["current_location"] = JSON.parse(args);
  } else {
    purescript.onEvent(_event)();
  }
};

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

console.error("Calling action DUI_READY");
if (window.JBridge.emptyCallbackQueue)
  window.JBridge.emptyCallbackQueue()
JBridge.runInJuspayBrowser("onEvent", JSON.stringify({
  action: "DUI_READY",
  event: "initiate",
  service: JOS.self
}), "");

eval(window.JBridge.loadFileInDUI("v1-tracker.jsa"));


window.JOS.tracker = window.getTrackerModule.Main.initTracker()
window.tracker =  window.JOS.tracker

const sessionInfo = JSON.parse(JBridge.getDeviceInfo())
const enableLogs = JBridge.fetchRemoteConfigBool && JBridge.fetchRemoteConfigBool("enable_logs")
const JOSFlags = window.JOS.getJOSflags()
if (sessionInfo.package_name.includes(".debug") || sessionInfo.package_name.includes(".staging") || enableLogs || JOSFlags.isCUGUser) {
  logger.enableLogger();
  Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_true;", "null");
} else {
  logger.disableLogger();
  Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_false;", "null");
}

if (window.eventQueue) {
  while (window.eventQueue.length) {
    const args = window.eventQueue.pop();
    window.onMerchantEvent.apply(null, args)
  }
}

console.log("APP_PERF INDEX_BUNDLE_END : ", new Date().getTime());
