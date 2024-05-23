import * as purescript from "./output/Main";

const JBridge = window.JBridge
const JOS = window.JOS

window.__PROXY_FN = {}
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
  purescript.initiateDriverMapp();
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
  JOS.emitEvent(JOS.parent)("onEvent")(JSON.stringify(payload))()();
}

export const getSearchRequestId = function () {
  if (window.__payload.payload && window.__payload.payload["rideData"]) {
    if (window.__payload.payload["rideData"]["entity_ids"]) return window.__payload.payload["rideData"]["entity_ids"];
    else {
      const data = JSON.parse(window.__payload.payload["rideData"]["entity_data"])
      return data["searchRequestId"] ? data["searchRequestId"] : ""; 
    }
  }
  return ""; 
}
export const getNotificationType = function () {
  if (window.__payload.payload && window.__payload.payload["rideData"]) {
    if (window.__payload.payload["rideData"]["notification_type"]) return window.__payload.payload["rideData"]["notification_type"];
  }
  return ""; 
}
  
function checkAndStart() {
  window.retry = window.retry || 0;
  window.retry++;
  if (window.JOS.parent == "in.yatri.provider" || window.isDriverAppInitiated) {
    purescript.main()();
  } else {
    setTimeout(() => {checkAndStart()},5);
  }
}
  
window.onMerchantEvent = function (_event, payload) {
  console.warn("onMerchantEvent RideRequest",_event, Object.assign({},JSON.parse(payload)));
  console.error("onMerchantEvent RideRequest",Object.assign({},JSON.parse(payload))["payload"]["rideData"]["notification_type"]);
  const clientPaylod = JSON.parse(payload);
  const appName = clientPaylod.payload.appName;
  window.appName = appName;
  window.__payload = clientPaylod
  if (_event == "initiate") {
    callInitiateResult();
    window.callDecline = function(id) {
      purescript.callDecline(id)()
    }
  } else if (_event == "process") {

    if (window.notificationCallBack && window.rideRequestCallBack) {
      purescript.checkAndPushRideRequest(window.rideRequestCallBack)(window.notificationCallBack)(getNotificationType())(getSearchRequestId())();
    } else {
      checkAndStart();
    }
  }
}

window.onResumeListeners = [];

window.onPause = function () {
  console.error("onEvent onPause");
}

window.onResume = function () {
  console.error("onEvent onResume");
}

window["onEvent'"] = function (_event, args) {
}

window["onEvent"] = function (_event, args) {
  console.log("onEvent ->" , args);
  // function getPayload(action) {
  //   const innerPayload = Object.assign({}, window.__payload.payload);
  //   const initiatePayload = Object.assign({}, window.__payload);
  //   if (action) innerPayload["action"] = action;
  //   initiatePayload["payload"] = innerPayload;
  //   initiatePayload["service"] = "in.yatri.provider";
  //   return initiatePayload;
  // }  
  // if (JOS && top.mapps["in.yatri.provider"]) {
  //   JOS.emitEvent("in.yatri.provider")("onMerchantEvent")("process")(JSON.stringify(getPayload()))();
  // } else {
  //   console.error("JOS Not Found")
  // }
  window.isDriverAppInitiated = true
}





if (typeof window.JOS != "undefined") {
  window.JOS.addEventListener("onEvent'")();
  window.JOS.addEventListener("onEvent")();
  window.JOS.addEventListener("onMerchantEvent")();
  console.error("Calling action DUI_READY");
  JOS.emitEvent(JOS.parent)("onEvent")(JSON.stringify({
    action: "DUI_READY",
    event: "initiate",
    service: JOS.self
  }))()();
} else {
  console.error("JOS not present")
}