import { callbackMapper as PrestoCallbackMapper } from "presto-ui";

const { JBridge, Android } = window;

function getLanguageLocale (){
  if (!window.languageKey) {
    const locale = JBridge.getKeysInSharedPref("LANGUAGE_KEY");
    window.languageKey = locale;
    return locale;
  } 
  return window.languageKey;
}

const idMap = {};

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

export const getNewIDWithTag = function(tag){
  window.__usedID = window.__usedID || {}
  window.__usedID[tag] = window.__usedID[tag] || "" + window.createPrestoElement().__id;
  return window.__usedID[tag];
}

export const callAPI = function () {
  return window.JBridge.callAPI(arguments[0], encodeURI(arguments[1]), getEncodedData(arguments[2]), getEncodedData(arguments[3]), arguments[4], arguments[5], arguments[6] )
}

export const callAPIWithOptions = function () {
  if (typeof window.JBridge.callAPIWithOptions == "function") {
    return window.JBridge.callAPIWithOptions(arguments[0], encodeURI(arguments[1]), getEncodedData(arguments[2]), getEncodedData(arguments[3]), arguments[4], arguments[5], arguments[6], arguments[7]);
  } else {
    return window.JBridge.callAPI(arguments[0], encodeURI(arguments[1]), getEncodedData(arguments[2]), getEncodedData(arguments[3]), arguments[4], arguments[5], arguments[7]);
  }
}

export const callbackMapper = PrestoCallbackMapper.map

export const atobImpl = function (value) {
  try {
    return window.atob(value);
  } catch (e) {
    return value;
  }
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
        
    }
  }
}

export const callSahay = function (request) {
  return function (_error, success) {
    window.__HANDLERLSP = function (response) {
      const reqJson = JSON.parse(request);
      const resJson = JSON.parse(response);

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

export const screenWidth = function(){
  return screen.width;
}

export const screenHeight = function(){
  return screen.height;
}

export const getDeviceHeight = function(){
  try{
    if(window.__OS == "IOS" && JBridge.getDeviceHeight) return parseInt(JBridge.getDeviceHeight());
    return JSON.parse(JBridge.getSessionInfo()).screen_height
  }
  catch(e){
    console.log("error in getDeviceHeight", e);
    return -1;
  }
}

export const getScreenPpi = function(){
  try{
    return Math.round(JSON.parse(JBridge.getSessionInfo()).screen_ppi);
  }
  catch(e){
    console.log("error in getScreenPpi", e);
    return -1;
  }
}


export const safeMarginTopImpl = function () {
  try {
    if (parent.__DEVICE_DETAILS && parent.__DEVICE_DETAILS.safe_area_frame) {
      return parent.__DEVICE_DETAILS.safe_area_frame.y
    }
  } catch(e){
    console.log("error in safeMarginTopImpl", e);
  }

  return 0;
}

export const safeMarginBottomImpl = function () {
  try{
    const d = parent.__DEVICE_DETAILS;
    if (!d || !d.safe_area_frame) {
      return 0;
    }
    return (d.screen_height - d.safe_area_frame.height - d.safe_area_frame.y);
  } catch(e){
    return 0;
  } 
}

export const getVersionByKey = function(key){
  return window.version[key] || "";
}

function setTextImpl(id, text, pos) {
  if (window.__OS === "ANDROID") {
    let cmd = "set_view=ctx->findViewById:i_" + id + ";";
    cmd += "get_view->setText:cs_" + text + ";";
    cmd += "get_view->setSelection:i_" + pos + ";";
    Android.runInUI(cmd, null);
  } else {
    Android.runInUI({id: id, text: text});
    Android.runInUI({id: id, cursorPosition: pos});
  }
}

export const setText = function (id) {
  return function (text) {
    setTextImpl(id, text, text.length);
  }
}

export const getExpiryTime = function (str1) {
  return function (reverse) {
    try {
      const expiry = new Date(str1);
      const current = new Date();
      let diff = (expiry.getTime() - current.getTime())/ 1000;
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
  return new Date().toISOString();
};

export const getDateFromObj = function (obj){
  const date = new Date(`${obj.month} ${obj.date}, ${obj.year}`);
  const dd = String(date.getDate()).padStart(2, "0");
  const mm = String(date.getMonth() + 1).padStart(2, "0"); //January is 0!
  const yyyy = date.getFullYear();
  return  yyyy + "-" + mm + "-" + dd;
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

export const getPastDays = function (count) {
  try {
    const result = [];
    const language = getLanguageLocale();
    for (let i = 0; i < count; i++) {
      const d = new Date();
      d.setDate(d.getDate() - i);
      const obj = { utcDate: d.toISOString(), date: d.getDate(), month: d.toLocaleString(getFormattedLanguage(language), { month: "short" }), year: d.getFullYear() };
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
    const result = []
    const currentDate = new Date();
    while (currentDate.getDay() != 0) {
      currentDate.setDate(currentDate.getDate() - 1);
    }
    const language = getLanguageLocale();
    currentDate.setDate(currentDate.getDate() + 7);
    for (let i = 0; i < count; i++) {
      const dStart = new Date(currentDate);
      const dEnd = new Date(currentDate);
      dStart.setDate(dStart.getDate() - 7 * (i + 1));
      dEnd.setDate(dEnd.getDate() - (7 * i + 1));
      const obj = { utcStartDate: dStart.toISOString(), startDate: dStart.getDate(), utcEndDate: dEnd.toISOString(), endDate: dEnd.getDate(),
        startMonth: dStart.toLocaleString(getFormattedLanguage(language), { month: "short" }), endMonth: dEnd.toLocaleString(getFormattedLanguage(language), { month: "short" }) }
      result.push(obj)
    }
    console.log(result);
    return result.reverse();
  } catch (e) {
    console.log("error in getPastWeeks", e);
  }
};

export const getDayName = function (dateString) {
  const date = new Date(dateString);
  const daysOfWeek = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
  const dayOfWeek = date.getDay();
  return daysOfWeek[dayOfWeek];
}

export const getFutureDate = function (startDate) {
  return function (noOfDays) {
    const date = new Date(startDate);
    date.setDate(date.getDate() + noOfDays);
    const year = date.getFullYear();
    const month = String(date.getMonth() + 1).padStart(2, "0");
    const day = String(date.getDate()).padStart(2, "0");

    return `${year}-${month}-${day}`;
  };
};

// ---------------------------------- moment ---------------------------------------------

function formatDates(date, format, language) {
  const mappings = {
    "h": () => {
      let hours = date.getHours();
      hours = hours % 12 || 12;
      return `${hours}`;
    },
    "hh": () => {
      let hours = ("0" + date.getHours()).slice(-2);
      hours = hours % 12 || 12;
      return `${hours}`;
    },
    "HH": () => {
      const hours = ("0" + date.getHours()).slice(-2);
      return `${hours}`;
    },
    "a": () => {
      const hours = date.getHours();
      const ampm = hours < 12 ? "am" : "pm";
      return `${ampm}`;
    },
    "A": () => {
      const hours = date.getHours();
      const ampm = hours < 12 ? "AM" : "PM";
      return `${ampm}`;
    },
    "mm": () => {
      const minutes = ("0" + date.getMinutes()).slice(-2);
      return `${minutes}`;
    },
    "ss": () => {
      const seconds = ("0" + date.getSeconds()).slice(-2);
      return `${seconds}`;
    },
    "DD": () => {
      const day = ("0" + date.getDate()).slice(-2);
      return `${day}`;
    },
    "MM": () => {
      const month = ("0" + (date.getMonth() + 1)).slice(-2);
      return `${month}`;
    },
    "YYYY": () => {
      const year = date.getFullYear();
      return `${year}`;
    },
    "YY": () => {
      const year = ("" + date.getFullYear()).slice(-2);
      return `${year}`;
    },
    "D": () => {
      const day = date.getDate();
      return `${day}`;
    },
    "Do": () => {
      const day = date.getDate();
      let daySuffix;
      if (day === 1 || day === 21 || day === 31) {
        daySuffix = "st";
      } else if (day === 2 || day === 22) {
        daySuffix = "nd";
      } else if (day === 3 || day === 23) {
        daySuffix = "rd";
      } else {
        daySuffix = "th";
      }
      return `${day}${daySuffix}`;
    },
    "MMM": () => {
      const month = date.toLocaleDateString(language, { month: "short" });
      return `${month}`;
    },
    "MMMM": () => {
      const month = date.toLocaleDateString(language, { month: "long" });
      return `${month}`;
    },
    "ddd": () => {
      const weekdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
      const weekday = weekdays[date.getDay()];
      return `${weekday}`;
    },
    "dddFull": () => {
      const weekdays = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
      const weekday = weekdays[date.getDay()];
      return `${weekday}`;
    },
    "llll": () => {
      const weekdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
      const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
      const weekday = weekdays[date.getDay()];
      const day = date.getDate();
      const month = months[date.getMonth()];
      const year = date.getFullYear();
      let hours = date.getHours();
      hours = hours % 12 || 12;
      const ampm = hours < 12 ? "AM" : "PM";
      const minutes = ("0" + date.getMinutes()).slice(-2);
      return `${weekday}, ${month} ${day}, ${year} ${hours}:${minutes} ${ampm}`;
    }
  }

  const reg = /(:| |\/|-|,)/g;
  const arr = format.split(reg);
  let result = "";
  for (const a of arr) {
    const maps = mappings[a];
    if (maps) {
      result += maps();
    } else {
      result += a;
    }
  }
  return result;
}

export const formatCurrencyWithCommas = function (amount) {
  amount = parseFloat(amount.replace(/[^0-9.-]+/g, ""));
  if (isNaN(amount)) {
    return "";
  }
  return amount.toLocaleString("en-IN");
}

export const camelCaseToSentenceCase = function(string) {
  const result = string.replace(/([A-Z])/g, " $1");
  return (result.substring(0, 1).toUpperCase() + result.substring(1).toLowerCase());
}

export const convertUTCtoISC = function (str) {
  return function (format) {
    let localTime = new Date(str);
    const language = getLanguageLocale();
    localTime = formatDates(localTime, format, getFormattedLanguage(language));
    return localTime;
  };
};

export const convertUTCTimeToISTTimeinHHMMSS = function (utcTime) {

  const utcDate = new Date(`1970-01-01T${utcTime}Z`);
  return (String(utcDate.getHours()).padStart(2, "0") + ":" + String(utcDate.getMinutes()).padStart(2, "0") + ":" + String(utcDate.getSeconds()).padStart(2, "0"));
};

export const getFormattedDate = function (str) {
  const date = new Date(str);
  const language = getLanguageLocale();
  return formatDates(new Date(date),"MMMM Do, YYYY", getFormattedLanguage(language));
}

export const getVideoID = function (url) {
  try {
    if (url === "") {
      return "";
    }
    let ID = "";
    const updatedURL = url.replace(/(>|<)/gi, "").split(/(vi\/|v=|\/v\/|youtu\.be\/|\/embed\/|\/shorts\/)/);
    if (updatedURL[2] !== undefined) {
      ID = updatedURL[2].split(/[^0-9a-z_-]/i);
      ID = ID[0];
    }
    else {
      if (updatedURL[1] == /shorts/) {
        ID = updatedURL[2];
      }else {
        ID = updatedURL;
      }
    }
    return ID;
  }catch (e) {
    console.log("error in getVideoID " + e);
  }
}

export const getImageUrl = function (url) {
  try {
    const videoId = getVideoID(url);
    return ("https://img.youtube.com/vi/" + videoId + "/maxresdefault.jpg");
  }catch (e) {
    console.log("error in getImageUrl " + e);
  }
};

export const setEventTimestamp = function(string){
  return function(){
    if (!window.flowTimeStampObject[string]){
      window.flowTimeStampObject[string] = Date.now() - window.prevTimeStamp;
      window.prevTimeStamp = Date.now();
    }
  }
}

export const getTimeStampObject = function(){
  return function(){
    const keyValuePairArray = Object.keys(window.flowTimeStampObject).map(function(key) {
      return {
        key: key,
        value: window.flowTimeStampObject[key]
      };
    });
    return keyValuePairArray;
  }
}

function getRandom(max) {
  return Math.floor(Math.random() * max) + 1; 
}

export const updateIdMap = function (key) {
  console.log ("updateIdMap")
  idMap[key] = {id : getRandom(10000), shouldPush: true};
  return idMap[key];
};

export const updatePushInIdMap = function (key, flag) {
  console.log ("updatePushInIdMap", idMap, flag);
  if (idMap[key]) {
    idMap[key]["shouldPush"] = flag;
  }
}

export const getValueFromIdMap = function (key) {
  let val = idMap[key]; 
  console.log ("getValueFromIdMap", idMap);
  if (!val) {
    idMap[key] = {id : getRandom(10000), shouldPush: true};
    val = idMap[key];
  }
  return val;
};

export const isTrue = function (a) {
  const bool = true;
  return a.toString() === bool.toString();
}