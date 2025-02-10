
const { JOS, JBridge } = window;
const microapps = ["in.juspay.hyperpay", "in.juspay.ec", "in.juspay.upiintent"];

function waitTillSeviceLoad (cb,serives,statusChecker) {
  const checkPP = function () {
    console.log("waitTillSeviceLoad");
    statusChecker(cb,serives);
  }
  setTimeout(checkPP,10);
}

function ppInitiateStatus() {
  if (JBridge.ppInitiateStatus) {
    let _status = JBridge.ppInitiateStatus();
    if (window.__OS === "IOS") {
      _status = _status === "1" ? true : false;
    }
    return _status;
  }
  return false;
}

function getInitiatPayload () {
  const innerPayload = Object.assign({},window.__payload.payload);
  const initiatePayload = Object.assign({},window.__payload);
  innerPayload["action"] = "initiate";
  innerPayload["merchantId"] = "nammayatri";
  if (innerPayload.clientId.includes("_ios")) {
    innerPayload["clientId"] = innerPayload.clientId.replace("_ios","");
  }
  initiatePayload["payload"] = innerPayload;
  initiatePayload["service"] = "in.juspay.hyperpay";
  return initiatePayload;
}

function checkPPLoadStatus(services) {
  let result = false;
  services.forEach(key => {
    if (top.mapps[key]) {
      if (top.mapps[key].contentWindow["onMerchantEvent"]) {
        result = true;
      } else {
        result = false;
      }
    }
  })
  return result;
}

export const  killPP = function (services) {
  services.forEach((key) => {
    if (key != JOS.self) {
      const currJOS = top.JOSHolder[key];
      try{currJOS.finish(1)(JSON.stringify({result:"success"}))();}
      catch (err){
        console.log(err);
      }
      delete top.JOSHolder[key];
    }
  });
}

export const initiatePP = function () {
  if (ppInitiateStatus()) {
    window.isPPInitiated = true;
    return;
  }
  try {
    if (JBridge.initiatePP) {
      JBridge.initiatePP(JSON.stringify(getInitiatPayload()));
    }
  } catch (err) {
    console.error("Hyperpay initiate Request not sent : ", err);
  }
}

export const initiatePPSingleInstance = function () {
  if (!JBridge.initiatePP) {
    if (JOS) {
      try {
        const cb = function (code) {
          return function (_response) {
            return function () {
              const response = JSON.parse(_response);
              console.log("%cHyperpay Terminate Response ", "background:darkblue;color:white;font-size:13px;padding:2px", code, response);
            }
          }
        }
        JOS.startApp("in.juspay.hyperpay")(getInitiatPayload())(cb)();
      } catch (err) {
        console.error("Hyperpay initiate Request not sent : ", err);
      }
    } else {
      console.log("%cHyperpay initiate Result - Already Initiated", "background:darkblue;color:white;font-size:13px;padding:2px", window.__payload);
    }
  }
}

function callUpiProcess(process,service) {
  if (checkPPLoadStatus(service)) {
    process();
  } else {
    waitTillSeviceLoad(process,service,callUpiProcess);
  }
}

export const getAvailableUpiApps = function (resultCb) {
  const cb = function (code) {
    return function (_response) {
      return function () {
        console.log("%cUPIINTENT Terminate Response ", "background:darkblue;color:white;font-size:13px;padding:2px", code, _response);
      }
    }
  }
  if (JOS) {
    try {
      const result = function (code) {
        return function (_response) {
          return function () {
            console.log("%cUPIINTENT initiate Result ", "background:darkblue;color:white;font-size:13px;padding:2px", _response);
            try {
              const resultPayload = JSON.parse(_response)
              resultCb(resultPayload.payload.response.available_apps)();
              killPP(["in.juspay.upiintent"]);
            } catch (err) {
              console.log("%cUPIINTENT initiate Result error", "background:darkblue;color:white;font-size:13px;padding:2px", err, code);
            }
          }
        }
      }
      const payload = {
        "UPI_PAYMENT_METHOD": "NA",
        "client_id": "",
        "environment": "",
        "get_available_apps": "true",
        "get_mandate_apps": "true",
        "merchant_id": "",
        "order_id": "",
      }
      const outerPayload = {
        payload : payload,
        requestId : window.__payload.requestId,
        service : "in.juspay.upiintent"
      };
      console.log("%cUPIINTENT initiate Result - Initiated", "background:darkblue;color:white;font-size:13px;padding:2px", payload); 
      JOS.startApp("in.juspay.upiintent")(payload)(cb)();
      const process = function() {
        // window.JOS.emitEvent("in.juspay.upiintent")("onMerchantEvent")(["process",JSON.stringify(outerPayload)])(result)();
      }
      callUpiProcess(process,["in.juspay.upiintent"]);
    } catch (err) {
      console.error("UPIINTENT initiate Request not sent : ", err);
    }
  } else {
    console.log("%cUPIINTENT initiate Result - Already Initiated", "background:darkblue;color:white;font-size:13px;padding:2px", window.__payload);
  }
}

export const consumeBP = function (unit){
  const jpConsumingBackpress = {
    event: "jp_consuming_backpress",
    payload: { jp_consuming_backpress: true }
  }
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
}

export const checkPPInitiateStatus = function (cb,services = microapps) {
  if (ppInitiateStatus() && window.isPPInitiated || (window.isPPInitiated && checkPPLoadStatus(services))) {
    cb()();
  } else {
    waitTillSeviceLoad(cb,services,checkPPInitiateStatus);
  }
}

export const startPP = function (payload) {
  return function (sc) {
    return function () {
      const cb = function (code) {
        return function (_response) {
          return function () {
            const response = JSON.parse(_response);
            console.log("%cHyperpay Response ","background:darkblue;color:white;font-size:13px;padding:2px", response);                                                        
            sc(response.payload.status)();
            if ((window.__OS).toUpperCase() == "ANDROID") {
              const ppServices = Object.keys(top.JOSHolder).filter((key) => { return key != JOS.self });
              killPP(ppServices);
            }
            consumeBP();
          }
        }
      }
      if (JOS) {  
        try {
          payload = JSON.parse(payload);                    
          console.log("%cHyperpay Request ", "background:darkblue;color:white;font-size:13px;padding:2px", payload);
          if (JBridge.processPP) {
            window.processCallBack = cb;
            console.log("%cHyperpay Callback ", "background:darkblue;color:white;font-size:13px;padding:2px", cb);
            console.log("inside process call", JSON.stringify(payload));
            JBridge.processPP(JSON.stringify(payload));
          } else {
            if (JOS.isMAppPresent("in.juspay.hyperpay")()){
              console.log("inside process call");
              JOS.emitEvent("in.juspay.hyperpay")("onMerchantEvent")(["process",JSON.stringify(payload)])(cb)();
            } else {
              sc("FAIL")();
            }
          }
        } catch (err) {
          console.error("Hyperpay Request not sent : ", err);
        }
      }else{
        sc("FAIL")();
      }
    }
  }
}