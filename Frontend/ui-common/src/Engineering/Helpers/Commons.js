const axios = require("axios");
const moment = require("moment");
const callbackMapper = require('presto-ui').callbackMapper;

exports.getOs = function () {
  if (window.__OS) {
    return window.__OS;
  }
  return "ANDROID";
};

const trackAPICalls = function(statusCode,url, apiStartTime){
  if(typeof window.__trackAPICalls == "function"){
    try{
      window.__trackAPICalls(url,apiStartTime,Date.now(),statusCode?statusCode:"0");
    } catch (error){
      console.error("Error while invoking " + window.__trackAPICalls + " function in Tracker.js \n",error.toString());
    }
  } else {
    console.error(window.__trackAPICalls+" is not a function in Tracker.js");
  }
}

const getEncodedData = function(data) {
  if (window.__OS == "IOS"){
    return  btoa(unescape(encodeURIComponent(data)));
  }
  return data;
}

const makeRequest = function (headersRaw, method, url, payload, success) {
  var apiStartTIme = Date.now();
  var successResponse = {};
  var headers = {};
  headers["Cache-Control"] = "no-cache";
  // headers["x-jp-merchant-id"] = __payload.merchant_id;
  // headers["x-jp-session-id"] = window.session_id;
  var isSSLPinnedURL = false;

  for (var i = 0; i < headersRaw.length; i++) {
    headers[headersRaw[i].field] = headersRaw[i].value;
  }
  isSSLPinnedURL = headers["x-pinned"] == "true"
  var callback = callbackMapper.map(function () {
    console.log("RESPONSE =---=-=-=-=-=-=---???", arguments)
    if (arguments && arguments.length >= 5) {
      try{
      // console.log("decodeURIComponent =---=-=-=-=-=-=---???", (decodeURIComponent(escape(window.atob(arguments[1]))) ))
      // console.log("decodeURIComponent json =---=-=-=-=-=-=---???", JSON.parse((decodeURIComponent(escape(window.atob(arguments[1])))) ))
      var decoderesp = (decodeURIComponent(escape(window.atob(arguments[1]))) )
      var decoderespp =  JSON.parse(decoderesp)
      var status_value
      if(decoderespp.hasOwnProperty('status')){
        status_value = decoderespp.status
      }
      else
      {
        status_value = "success"
      }
      console.log("status_value =---=-=-=-=-=-=---???", status_value)
      var responseHeaders = JSON.parse(atob(arguments[4]) || "{}");
      successResponse = {
        status: status_value , //arguments[0],
        responseHeaders: responseHeaders, 
        response: Object.assign(JSON.parse(decodeURIComponent(escape(window.atob(arguments[1]))) || "{}"), { "headers" : JSON.parse(atob(arguments[4]) || "{}")}),
        code: parseInt(arguments[2])
      };}
      catch(e)
      {
        successResponse = {
          status: arguments[0],
          responseHeaders: {}, 
          response : Object.assign(({ response : {errorMessage : decodeURIComponent(escape(window.atob(arguments[1]))) ,error:true, userMessage: "" }} || "{}"), { "headers" : JSON.parse(atob(arguments[4]) || "{}")}),
          code: parseInt(arguments[2])
        
        };
      }
      trackAPICalls(successResponse.code, url, apiStartTIme);
      var resp = successResponse.response;
      resp["code"] = successResponse["code"];
      resp["status"] = successResponse["status"];
      if(successResponse["code"] != 200 && resp.action == "NACK" ){
        successResponse["response"] = (successResponse["response"]||{error:true, userMessage: "", errorMessage: resp.errorMessage.fromErrorMsg});
        console.log("Condition 1 =---=-=-=-=-=-=---???", resp.errorMessage.fromErrorMsg)
      }
      else if (successResponse["code"] != 200 && resp.errorMessage != null){
        successResponse["response"] = (successResponse["response"]||{error:true, userMessage: resp.errorMessage, errorMessage: resp.errorCode});
        console.log("Condition 2 =---=-=-=-=-=-=---???", resp.errorCode)
      }
      else if (successResponse["code"] != 200 && resp.errorMessage == null){
        successResponse["response"] = (successResponse["response"]||{error:true, userMessage: "", errorMessage: resp.errorCode});
        console.log("Condition 3 =---=-=-=-=-=-=---???", resp.errorCode)
      }
      else{
        successResponse["response"] = (successResponse["response"]||{error:true, userMessage: "", errorMessage: ""});
        console.log("Condition 4 =---=-=-=-=-=-=---???", "")
      }
      console.log(successResponse);
      console.log(resp);
      successResponse["response"] = JSON.stringify(successResponse["response"]);
      success(successResponse)();
      // success(JSON.stringify(resp))();
    }else if(arguments[2] == "-2"){
      // trackAPICalls(0,url,apiStartTIme);
      // success(JSON.stringify({
      var dummyErrorResp = {
        status: "ssl handshake failure",
        responseHeaders: {}, 
        response: {
          error: true,
          errorMessage: "Monitored Network",
          userMessage: "Monitored Network"
        },
        code: -2
      // }))();
      };
      dummyErrorResp.response = JSON.stringify(dummyErrorResp.response);
      success(dummyErrorResp)();
    }else {
      // trackAPICalls(0,url,apiStartTIme);
      // success(JSON.stringify({
      var dummyErrorResp = {
        status: "failure",
        responseHeaders: {}, 
        response: {
          error: true,
          errorMessage: arguments[2].toString(),
          userMessage: "Unknown error"
        },
        code: -1
      // }))();
      }
      dummyErrorResp.response = JSON.stringify(dummyErrorResp.response);
      success(dummyErrorResp)();
    }
  });
  console.log("REQUEST =>",payload)
  isSSLPinnedURL = false; // TODO: Change it to true later
  JBridge.callAPI(method, url, getEncodedData(payload), getEncodedData(JSON.stringify(headers)), false, isSSLPinnedURL, callback);
}

exports["showUI'"] = function (sc, screen) {
  return function () {
    var screenJSON = JSON.parse(screen);
    var screenName = screenJSON.tag;
    screenJSON.screen = screenName;
    if (screenName == "InitScreen") {
      screenJSON.screen = "INIT_UI";
    }
    window.__duiShowScreen(sc, screenJSON);
  };
};

exports["getNewIDWithTag"] = function(tag){
  window.__usedIDS = window.__usedIDS || []
  window.__usedIDS[tag] = window.__usedIDS[tag] || "" + window.createPrestoElement().__id;
  return window.__usedIDS[tag];
}

exports["callAPI'"] = function () {
  return function (success) {
    return function (request) {
      return function () {
        makeRequest(request.headers, request.method, request.url, request.payload, success);
      };
    };
  };
}

exports["window'"] = function(key) {
  return function(just) {
    return function(nothing) {
      return function() {
        if(typeof window !== "undefined" && typeof window[key] !== "undefined") {
          return just(window[key]);
        } else {
          return nothing;
        }
      }
    }
  }
}

exports["setWindowVariable'"] = function(key) {
    return function(value) {
      return function() {
        if(typeof window !== "undefined") {
           window[key] =value;
        }
        return;
      }
    }
}

exports ["callSahay"] = function (request) {
    return function (_error, success) {
      window.__HANDLERLSP = function (response) {
        var reqJson = JSON.parse(request);
        var resJson = JSON.parse(response);

        if (reqJson.event != resJson.event) {
          console.error("callSahay", "Got response for different event", "Expected", reqJson.event, "Received", resJson.event);
        }

        console.warn("callSahay", "Got response", response);
        success(response);
      }

      console.warn("callSahay", "Sending payload", request);
      JBridge.processWithSdk(request);

      // return function() {};
    };
  };

exports["setScreen'"] = function(screen) {
  return function() {
    setTimeout(function() {
      if(window.idToBeRemoved) {
          Android.runInUI("set_VIEW=ctx->findViewById:i_" + window.idToBeRemoved + ";get_VIEW->removeAllViews;", null);
          Android.runInUI("set_VIEW=ctx->findViewById:i_" + window.idToBeRemoved + ";set_PARENT=get_VIEW->getParent;get_PARENT->removeView:get_VIEW;", null);
          window.idToBeRemoved = null;
      }
    }, 1200);
    window.__dui_screen = screen
    if(typeof window.pageId == "undefined"){
        window.pageid = -1;
    }
    ++window.pageId
  }
}

exports["screenWidth"] = function(){
  return screen.width;
}

exports["screenHeight"] = function(){
  return screen.height;
}

exports["safeMarginTop'"] = function () {
  try {
    if (parent.__DEVICE_DETAILS && parent.__DEVICE_DETAILS.safe_area_frame) {
      return parent.__DEVICE_DETAILS.safe_area_frame.y
    }
  } catch(e){

  }

  return 0;
}

exports["safeMarginBottom'"] = function () {
  try{
    var d = parent.__DEVICE_DETAILS;
    if (!d || !d.safe_area_frame) {
      return 0;
    }
    return (d.screen_height - d.safe_area_frame.height - d.safe_area_frame.y);
  } catch(e){
    return 0;
  }
}

exports["bundleVersion"] = function(){
  return window.version;
}

exports ["setText'"] = function (id) {
  return function (text) {
      return function (){
          setText(id, text, text.length);
      }
  }
} 

function setText(id, text, pos) {
  if (__OS === "ANDROID") {
      var cmd = "set_view=ctx->findViewById:i_" + id + ";";
      cmd += "get_view->setText:cs_" + text + ";";
      cmd += "get_view->setSelection:i_" + pos + ";";
      Android.runInUI(cmd, null);
  } else {
      Android.runInUI({id: id, text: text});
      Android.runInUI({id: id, cursorPosition: pos});
  }
}

exports["countDown"] = function (countDownTime) {
  return function (id) {
    return function (cb) {
      return function (action) {
        return function () {
          var callback = callbackMapper.map(function () {
            var countDown = countDownTime;
            var timerIID = instantGetTimer(function () {
              countDown -= 1;
              if (countDown < 0) {
                cb(action(0)(id)("EXPIRED")(timerIID))();
              } else {
                cb(action(countDown)(id)("INPROGRESS")(timerIID))();
              }
            }, 1000);
          });
          window.callUICallback(callback);
        }
      }
    }
  }
}

function instantGetTimer (fn , delay) {
  fn();
  window.timerId = setInterval( fn, delay );
  return window.timerId;
}

exports ["clearTimer"] = function (a)
{
  clearInterval(parseInt(a));
};

exports["getExpiryTime"] = function (str1) {
    return function (reverse) {
      try {
      var expiry = new Date(str1);
      var date = new Date();
      var result =  moment(date).utc().format();
      var current = new Date(result);
      var diff = (expiry.getTime() - current.getTime())/ 1000;
      if (reverse)
        {
          diff = (current.getTime() - expiry.getTime())/ 1000;
        }
      diff = (Math.round(diff));
      if (diff >= 0)
          return (diff);
        else
          return 0;
      } catch (err) {
        console.log("error in getExpiryTime " + err);
      }
    };
};