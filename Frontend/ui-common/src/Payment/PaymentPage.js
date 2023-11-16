
const { JOS, JBridge } = window;
const microapps = ["in.juspay.hyperpay", "in.juspay.ec", "in.juspay.upiintent"];

function waitTillSeviceLoad (cb,serives,statusChecker) {
  const checkPP = function () {
    console.log("waitTillSeviceLoad");
    statusChecker(cb,serives);
  }
  setTimeout(checkPP,10);
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
  const cb = function (code) {
    return function (_response) {
      return function () {
        const response = JSON.parse(_response);
        console.log("%cHyperpay Terminate Response ", "background:darkblue;color:white;font-size:13px;padding:2px", code, response);
      }
    }
  }
  if (JOS) {
    try {
      const innerPayload = Object.assign({},window.__payload.payload);
      const initiatePayload = Object.assign({},window.__payload);
      innerPayload["action"] = "initiate";
      initiatePayload["payload"] = innerPayload;
      JOS.startApp("in.juspay.hyperpay")(initiatePayload)(cb)();
    } catch (err) {
      console.error("Hyperpay initiate Request not sent : ", err);
    }
  } else {
    console.log("%cHyperpay initiate Result - Already Initiated", "background:darkblue;color:white;font-size:13px;padding:2px", window.__payload);
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
        window.JOS.emitEvent("in.juspay.upiintent")("onMerchantEvent")(["process",JSON.stringify(outerPayload)])(result)();
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
  if (window.isPPInitiated && checkPPLoadStatus(services)) {
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
            sc(response.payload.payload.status.value0)();
          }
        }
      }
      if (JOS) {      
        try {
          payload = JSON.parse(payload);                    
          console.log("%cHyperpay Request ", "background:darkblue;color:white;font-size:13px;padding:2px", payload);

          if (JOS.isMAppPresent("in.juspay.hyperpay")()){
            console.log("inside process call");
            JOS.emitEvent("in.juspay.hyperpay")("onMerchantEvent")(["process",JSON.stringify(payload)])(cb)();
          } else {
            sc("FAIL")();
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