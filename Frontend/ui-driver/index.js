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
  const config = require("./output/Helpers.FileProvider.Utils/index.js");
  config.loadAppConfig("");
}


window.session_id = guid();
window.version = __VERSION__;
let previousDateObject = new Date();
const refreshThreshold = 300;
console.warn("Hello World");
const JBridge = window.JBridge;
const JOS = window.JOS;
loadConfig();

window.isObject = function (object) {
  return (typeof object == "object");
}
window.manualEventsName = ["onBackPressedEvent", "onNetworkChange", "onResume", "onPause", "onKeyboardHeightChange"];

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
    purescript.onEvent(_event)();
  } else if (_event == "onLocationChanged") {
    purescript.onConnectivityEvent("LOCATION_DISABLED")();
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
  if ((JSON.parse(jsonPayload)).event == "initiate_result"){
    window.isPPInitiated = true;
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