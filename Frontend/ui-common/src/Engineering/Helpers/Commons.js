import axios from "axios";
import { callbackMapper } from 'presto-ui';

const { JBridge, Android } = window;

export const getOs = function () {
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

export const showUIImpl = function (sc, screen) {
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

export const getNewIDWithTag = function(tag){
  window.__usedID = window.__usedID || {}
  window.__usedID[tag] = window.__usedID[tag] || "" + window.createPrestoElement().__id;
  return window.__usedID[tag];
}

export const callAPIImpl = function () {
  return function (success) {
    return function (request) {
      return function () {
        makeRequest(request.headers, request.method, request.url, request.payload, success);
      };
    };
  };
}

export const getWindowVariable = function(key) {
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

export const setWindowVariableImpl = function(key) {
    return function(value) {
      return function() {
        if(typeof window !== "undefined") {
           window[key] =value;
        }
        return;
      }
    }
}

export const callSahay = function (request) {
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
      window.JBridge.processWithSdk(request);

      // return function() {};
    };
  };

export const setScreenImpl = function(screen) {
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

export const screenWidth = function(){
  return screen.width;
}

export const screenHeight = function(){
  return screen.height;
}

export const safeMarginTopImpl = function () {
  try {
    if (parent.__DEVICE_DETAILS && parent.__DEVICE_DETAILS.safe_area_frame) {
      return parent.__DEVICE_DETAILS.safe_area_frame.y
    }
  } catch(e){

  }

  return 0;
}

export const safeMarginBottomImpl = function () {
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

export const bundleVersion = function(){
  return window.version;
}

export const setText = function (id) {
  return function (text) {
          setTextImpl(id, text, text.length);
  }
}

function setTextImpl(id, text, pos) {
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

export const countDown = function (countDownTime) {
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

export const clearTimer = function (a)
{
  clearInterval(parseInt(a));
};

export const getExpiryTime = function (str1) {
    return function (reverse) {
      try {
      var expiry = new Date(str1);
      var current = new Date();
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

export const getCurrentTimeStamp = function () {
  return new Date()
};

export const getCurrentUTC = function (str) {
  var result = new Date().toISOString();
  console.log(result);
  return result;
};

export const convertUTCtoISC = function (str) {
  return function (format) {
    var localTime = new Date(str);
    const language = JBridge.getFromSharedPrefs("LANGUAGE_KEY");
    localTime = formatDates(localTime, format, getFormattedLanguage(language));
    return localTime;
  };
};

export const getDateFromObj = function (obj){
  let date = new Date(`${obj.month} ${obj.date}, ${obj.year}`);
  var dd = String(date.getDate()).padStart(2, '0');
  var mm = String(date.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = date.getFullYear();
  return  yyyy + '-' + mm + '-' + dd;
}

function getFormattedLanguage(language){
  if (language == "EN_US") return "en-us";
  else if (language == "HI_IN") return "hi-in";
  else if (language == "KN_IN") return "kn-in";
  else if (language == "TA_IN") return "ta-in";
  else if (language == "BN_IN") return "bn-in";
  else if (language == "ML_IN") return "ml-in";
  else return "en-us";
}

export const getFormattedDate = function (str) {
  var date = new Date(str);
  const language = JBridge.getFromSharedPrefs("LANGUAGE_KEY");
  return formatDates(new Date(date),"MMMM Do, YYYY", getFormattedLanguage(language));
}

export const getPastDays = function (count) {
  try {
    let result = [];
    const language = JBridge.getFromSharedPrefs("LANGUAGE_KEY");
    for (var i = 0; i < count; i++) {
      let d = new Date();
      d.setDate(d.getDate() - i);
      let obj = { utcDate: d.toISOString(), date: d.getDate(), month: d.toLocaleString(getFormattedLanguage(language), { month: 'short' }), year: d.getFullYear() };
      result.push(obj);
    }
    console.log(language, getFormattedLanguage(language))
    console.log(result);
    return result.reverse();
  } catch (e) {
    console.log("error in getPastDays", e);
  }
};

export const getPastWeeks = function (count) {
  try {
    let result = []
    var currentDate = new Date();
    while (currentDate.getDay() != 0) {
        currentDate.setDate(currentDate.getDate() - 1);
    }
    const language = JBridge.getFromSharedPrefs("LANGUAGE_KEY");
    currentDate.setDate(currentDate.getDate() + 7);
    for (var i = 0; i < count; i++) {
      let dStart = new Date(currentDate);
      let dEnd = new Date(currentDate);
      dStart.setDate(dStart.getDate() - 7 * (i + 1));
      dEnd.setDate(dEnd.getDate() - (7 * i + 1));
      let obj = { utcStartDate: dStart.toISOString(), startDate: dStart.getDate(), utcEndDate: dEnd.toISOString(), endDate: dEnd.getDate(),
                  startMonth: dStart.toLocaleString(getFormattedLanguage(language), { month: 'short' }), endMonth: dEnd.toLocaleString(getFormattedLanguage(language), { month: 'short' }) }
      result.push(obj)
    }
    console.log(result);
    return result.reverse();
  } catch (e) {
    console.log("error in getPastWeeks", e);
  }
};

// ---------------------------------- moment ---------------------------------------------

function formatDates(date, format, language) {
  const mappings = {
    'h': () => {
      var hours = date.getHours();
      hours = hours % 12 || 12;
      return `${hours}`;
    },
    'hh': () => {
      var hours = ('0' + date.getHours()).slice(-2);
      hours = hours % 12 || 12;
      return `${hours}`;
    },
    'HH': () => {
      const hours = ('0' + date.getHours()).slice(-2);
      return `${hours}`;
    },
    'a': () => {
      const hours = date.getHours();
      const ampm = hours < 12 ? 'am' : 'pm';
      return `${ampm}`;
    },
    'A': () => {
      const hours = date.getHours();
      const ampm = hours < 12 ? 'AM' : 'PM';
      return `${ampm}`;
    },
    'mm': () => {
      const minutes = ('0' + date.getMinutes()).slice(-2);
      return `${minutes}`;
    },
    'ss': () => {
      const seconds = ('0' + date.getSeconds()).slice(-2);
      return `${seconds}`;
    },
    'DD': () => {
      const day = ('0' + date.getDate()).slice(-2);
      return `${day}`;
    },
    'MM': () => {
      const month = ('0' + (date.getMonth() + 1)).slice(-2);
      return `${month}`;
    },
    'YYYY': () => {
      const year = date.getFullYear();
      return `${year}`;
    },
    'D': () => {
      const day = date.getDate();
      return `${day}`;
    },
    'Do': () => {
      const day = date.getDate();
      let daySuffix;
      if (day === 1 || day === 21 || day === 31) {
        daySuffix = 'st';
      } else if (day === 2 || day === 22) {
        daySuffix = 'nd';
      } else if (day === 3 || day === 23) {
        daySuffix = 'rd';
      } else {
        daySuffix = 'th';
      }
      return `${day}${daySuffix}`;
    },
    'MMM': () => {
      const month = date.toLocaleDateString(language, { month: 'short' });
      return `${month}`;
    },
    'MMMM': () => {
      const month = date.toLocaleDateString(language, { month: 'long' });
      return `${month}`;
    },
    'ddd': () => {
      const weekdays = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
      const weekday = weekdays[date.getDay()];
      return `${weekday}`;
    },
    'llll': () => {
      const weekdays = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
      const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
      const weekday = weekdays[date.getDay()];
      const day = date.getDate();
      const month = months[date.getMonth()];
      const year = date.getFullYear();
      var hours = date.getHours();
      hours = hours % 12 || 12;
      const ampm = hours < 12 ? 'AM' : 'PM';
      const minutes = ('0' + date.getMinutes()).slice(-2);
      return `${weekday}, ${month} ${day}, ${year} ${hours}:${minutes} ${ampm}`;
    }
  }

  var reg = /(:| |\/|-|,)/g;
  var arr = format.split(reg);
  var result = '';
  for (const a of arr) {
    var maps = mappings[a];
    if (maps) {
      result += maps();
    } else {
      result += a;
    }
  }
  return result;
}

export function formatCurrencyWithCommas(amount) {
  amount = parseFloat(amount.replace(/[^0-9.-]+/g, ''));
  if (isNaN(amount)) {
    return "";
  }
  return amount.toLocaleString('en-IN');
}

export function camelCaseToSentenceCase(string){
  var result = string.replaceAll(/([A-Z])/g, ' $1');
  return (result.substring(0, 1).toUpperCase() + result.substring(1).toLowerCase());
}