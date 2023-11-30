import "core-js";
import "presto-ui";
import "regenerator-runtime/runtime";


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
// JBridge.setSessionId(window.session_id);
console.warn("Hello World MASTER ONE");
let previousDateObject = new Date();
const refreshThreshold = 30;
const JBridge = window.JBridge;
const JOS = window.JOS;
const Android = window.Android;

const eventObject = {
  type : ""
  , data : ""
}


window.isObject = function (object) {
  return (typeof object == "object");
}
window.manualEventsName = ["onBackPressedEvent", "onNetworkChange", "onResume", "onPause", "onKeyboardHeightChange"];
window.whitelistedNotification = ["DRIVER_ASSIGNMENT", "CANCELLED_PRODUCT", "TRIP_FINISHED", "TRIP_STARTED"];

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
// if (window.__OS == "WEB") {
//   purescript.main();
// }

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

window.onMerchantEvent = function (_event, GlobalPayload) {
  console.log(GlobalPayload);
  const clientPaylod = JSON.parse(GlobalPayload).payload;
  if (_event == "initiate") {
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
    console.log(window.merchantID);
    callInitiateResult();
  } else if (_event == "process") {
    console.warn("Process called");
    window.__payload.sdkVersion = "2.0.1"
    if (clientPaylod.action == "notification") {
      if (clientPaylod.notification_content && clientPaylod.notification_content.type) {
        if (window.whitelistedNotification.includes(clientPaylod.notification_content.type)){
          window.callNotificationCallBack(clientPaylod.notification_content.type);
        }
      }
    } else if (clientPaylod.action == "OpenChatScreen") { 
      if (window.openChatScreen) {
        window.openChatScreen();
      }
    }
    else {
      eventObject["type"] = "";
      eventObject["data"] = "";
      if(clientPaylod.notificationData && clientPaylod.notificationData.notification_type == "CHAT_MESSAGE"){
        eventObject["type"] = "CHAT_MESSAGE";
      }
      window.__payload = JSON.parse(GlobalPayload);
      console.log("window Payload: ", window.__payload);
      purescript.main(eventObject)();
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

window.onPause = function () {
  console.error("onEvent onPause");
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
  const currentDate = new Date();
  const diff = Math.abs(previousDateObject - currentDate) / 1000;
  const token = (window.JBridge.getKeysInSharedPref("REGISTERATION_TOKEN"));
  const currentState = (window.JBridge.getKeysInSharedPref("LOCAL_STAGE"));
  if ((diff > refreshThreshold) && 
      (token != "__failed") && 
      (token != "(null)") &&
      ((currentState == "RideStarted") || currentState == "RideAccepted")){
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
    purescript.onConnectivityEvent("INTERNET_ACTION")();
  }else if(_event == "onBundleUpdated"){
    purescript.onBundleUpdatedEvent(JSON.parse(args))();
  } else if ((_event == "onKeyboardOpen" || _event == "onKeyboardClose") && window.keyBoardCallback) {
    window.keyBoardCallback(_event);
  } else {
    purescript.onEvent(_event)();
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
  window.JOS.addEventListener("onEvent")();
  window.JOS.addEventListener("onMerchantEvent")();
  window.JOS.addEventListener("onActivityResult")();
  console.error("Calling action DUI_READY");
  JOS.emitEvent("java")("onEvent")(JSON.stringify({ action: "DUI_READY", service : JOS.self }))()();
} else {
  console.error("JOS not present")
}

const sessionInfo = JSON.parse(JBridge.getDeviceInfo())
if(sessionInfo.package_name.includes(".debug") || sessionInfo.package_name.includes(".staging")){
  logger.enableLogger();
}else{
  logger.disableLogger();
  Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_false;","null");
}