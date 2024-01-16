import "core-js";
import "presto-ui";
import "regenerator-runtime/runtime";

const bundleLoadTime = Date.now();
window.flowTimeStampObject = {};
const blackListFunctions = ["getFromSharedPrefs", "getKeysInSharedPref", "setInSharedPrefs", "addToLogList", "requestPendingLogs", "sessioniseLogs", "setKeysInSharedPrefs", "getLayoutBounds"]

if (window.JBridge.firebaseLogEventWithParams){  
  Object.getOwnPropertyNames(window.JBridge).filter((fnName) => {
    return blackListFunctions.indexOf(fnName) == -1
  }).forEach(fnName => {
    window.JBridgeProxy = window.JBridgeProxy || {};
    window.JBridgeProxy[fnName] = window.JBridge[fnName];
    window.JBridge[fnName] = function () {
      let params = Object.values(arguments).join(", ");
      if (fnName === "callAPI") {
        params = arguments[1].split("/").splice(6).join("/");
      }
      let shouldLog = true;
      if (window.appConfig) {
        shouldLog = window.appConfig.logFunctionCalls ? window.appConfig.logFunctionCalls : shouldLog;
      }
      if (shouldLog) {
        window.JBridgeProxy.firebaseLogEventWithParams("ny_fn_" + fnName,"params",JSON.stringify(params));
      }
      const result = window.JBridgeProxy[fnName](...arguments);
      return result;
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


function loadConfig() {
  const config = require("./output/ConfigProvider/index.js");
  config.loadAppConfig("");
}


window.session_id = guid();
window.version = window.version || {};
window.version["app"] = __VERSION__;
let previousDateObject = new Date();
const refreshThreshold = 300;
console.warn("Hello World");
const JBridge = window.JBridge;
const JOS = window.JOS;
loadConfig();

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

const purescript = require("./output/Main");


function callInitiateResult () {
  const payload = {
    event: "initiate_result"
    , service: "in.juspay.becknui"
    , payload: { status: "SUCCESS" }
    , error: false
    , errorMessage: ""
    , errorCode: ""
  }
  const jpConsumingBackpress = {
    event: "jp_consuming_backpress",
    payload: { jp_consuming_backpress: true }
  }
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), null)
}

function refreshFlow(){
  const currentDate = new Date();
  const diff = Math.abs(previousDateObject - currentDate) / 1000;
  const token = window.JBridge.getKeysInSharedPref("REGISTERATION_TOKEN");
  if ((diff > refreshThreshold) && (token != "__failed")){
    if(window.storeCallBackMessageUpdated){
      window.__PROXY_FN[window.storeCallBackMessageUpdated] = undefined;
    }
    if(JBridge.removeCallBackOpenChatScreen) {
      JBridge.removeCallBackOpenChatScreen();
    }
    window.chatMessages = undefined;
    purescript.onConnectivityEvent("REFRESH")();
  }
}


function makeEvent(_type, _data) {
  return { type : _type, data : _data };
}


window.onMerchantEvent = function (_event, payload) {
  console.log(payload);
  const clientPaylod = JSON.parse(payload);
  const clientId = clientPaylod.payload.clientId
  if (_event == "initiate") {
    if (clientId == "yatriprovider") {
      window.merchantID = "YATRI"
    } else if(clientId == "jatrisaathiprovider" || clientId == "jatrisaathidriver" || clientId == "yatrisathiprovider"){
      window.merchantID = "YATRISATHI"
    }else if (clientId.includes("provider")){
      let merchant = clientId.replace("mobility","")
      merchant = merchant.replace("provider","");
      window.merchantID = merchant.toUpperCase();
    } else {
      // window.merchantID = clientPaylod.payload.clientId.toUpperCase();
      window.merchantID = "NAMMAYATRI";
    }
    if (clientPaylod.payload && clientPaylod.payload.hasOwnProperty('onCreateTimeStamp') && clientPaylod.payload.hasOwnProperty('initiateTimeStamp'))
    {
      const onCreateTimeStamp = clientPaylod.payload.onCreateTimeStamp;
      const initiateTimeStamp = clientPaylod.payload.initiateTimeStamp;
      window.flowTimeStampObject["onCreateToBundle"] = window.prevTimeStamp - onCreateTimeStamp;
      window.flowTimeStampObject["nativeIntiateToBundle"] = window.prevTimeStamp - initiateTimeStamp;
    }
    window.flowTimeStampObject["bundleLoadTime"] = bundleLoadTime - window.prevTimeStamp;
    window.flowTimeStampObject["bundleToInitiate"] = Date.now() - bundleLoadTime;
    window.prevTimeStamp = Date.now();
    callInitiateResult();
  } else if (_event == "process") {
    window.__payload.sdkVersion = "2.0.1"
    console.warn("Process called");
    const parsedPayload = JSON.parse(payload);
    if (parsedPayload && parsedPayload.payload && parsedPayload.payload.action == "callDriverAlert" && parsedPayload.payload.id && parsedPayload.payload.popType) {
      // purescript.alertNotification(parsedPayload.payload.id)();
      console.log("alert notification called");
    }else if (parsedPayload && parsedPayload.payload && parsedPayload.payload.action == "showPopup" && parsedPayload.payload.id && parsedPayload.payload.popType){
      window.callPopUp(parsedPayload.payload.popType, parsedPayload.payload.entityPayload);
    }
    else {
      window.__payload = parsedPayload;
      console.log("window Payload: ", window.__payload);
      const jpConsumingBackpress = {
        event: "jp_consuming_backpress",
        payload: { jp_consuming_backpress: true }
      }
      JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
      if (parsedPayload.payload.notificationData && parsedPayload.payload.notificationData.notification_type == "NEW_MESSAGE" && parsedPayload.payload.notificationData.entity_ids) {
        purescript.main(makeEvent("NEW_MESSAGE", parsedPayload.payload.notificationData.entity_ids))();
      }else if (parsedPayload.payload.notificationData && parsedPayload.payload.notificationData.notification_type == "PAYMENT_MODE_MANUAL") {
        purescript.main(makeEvent("PAYMENT_MODE_MANUAL", ""))();
      } else if (parsedPayload.payload.viewParam){
        purescript.onNewIntent(makeEvent("DEEP_VIEW", parsedPayload.payload.viewParam))();
      } else if (parsedPayload.payload.view_param){
        const deepLinkType = parsedPayload.payload.onNewIntent ? "DEEP_VIEW_NEW_INTENT" : "DEEP_VIEW";
        const param = parsedPayload.payload.viewParam ? parsedPayload.payload.viewParam : parsedPayload.payload.view_param;
        purescript.onNewIntent(makeEvent(deepLinkType, param))();
      } else if (parsedPayload.payload.viewParamNewIntent){
        purescript.onNewIntent(makeEvent("DEEP_VIEW_NEW_INTENT", parsedPayload.payload.viewParamNewIntent))();
      } else{
        purescript.main(makeEvent("", ""))();
      }
    }
  } else {
    console.error("unknown event: ", event);
  }
}

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




if (typeof window.JOS != "undefined") {
  window.JOS.addEventListener("onEvent'")();
  window.JOS.addEventListener("onEvent")(); // adding onEvent listener for hyperPay
  window.JOS.addEventListener("onMerchantEvent")();
  window.JOS.addEventListener("onActivityResult")();
  console.error("Calling action DUI_READY");
  JOS.emitEvent("java")("onEvent")(JSON.stringify({ action: "DUI_READY", event: "initiate",service : JOS.self }))()();
} else {
  console.error("JOS not present")
}

const sessionInfo = JSON.parse(JBridge.getDeviceInfo())

if (sessionInfo.package_name.includes("debug")) {
  logger.enableLogger();
} else {
  logger.disableLogger();
}