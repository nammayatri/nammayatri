import * as purescript from "./output/Main";

const JBridge = window.JBridge
const JOS = window.JOS

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
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
  JOS.emitEvent(JOS.parent)("onEvent")(JSON.stringify(payload))()();
}

  
  
window.onMerchantEvent = function (_event, payload) {
  console.log(payload);
  const clientPaylod = JSON.parse(payload);
  const appName = clientPaylod.payload.appName;
  window.appName = appName;
  window.__payload = clientPaylod
  if (_event == "initiate") {
    callInitiateResult();
  } else if (_event == "process") {
    purescript.main()();
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
  purescript.onEvent(_event)(args)();
}

window["onEvent"] = function (args) {
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