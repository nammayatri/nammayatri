import {
  callbackMapper as PrestoCallbackMapper
} from "presto-ui";

const JBridge = window.JBridge;
const JOS = window.JOS;

export const getOs = function () {
  if (window.__OS) {
    return window.__OS;
  }
  return "ANDROID";
};

const getEncodedData = function (data) {
  if (window.__OS == "IOS") {
    return btoa(unescape(encodeURIComponent(data)));
  }
  return data;
}

export const getNewIDWithTag = function (tag) {
  window.__usedID = window.__usedID || {}
  window.__usedID[tag] = window.__usedID[tag] || "" + window.createPrestoElement().__id;
  return window.__usedID[tag];
}

export const callAPI = function () {
  return window.JBridge.callAPI(arguments[0], encodeURI(arguments[1]), getEncodedData(arguments[2]), getEncodedData(arguments[3]), arguments[4], arguments[5], arguments[6])
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

export const getWindowVariable = function (key) {
  return function (just) {
    return function (nothing) {
      return function () {
        if (typeof window !== "undefined" && typeof window[key] !== "undefined") {
          return just(window[key]);
        } else {
          return nothing;
        }
      }
    }
  }
}


export const screenWidth = function () {
  return screen.width;
}

export const screenHeight = function () {
  return screen.height;
}

export const getDeviceHeight = function () {
  try {
    if (window.__OS == "IOS" && JBridge.getDeviceHeight) return parseInt(JBridge.getDeviceHeight());
    return JSON.parse(JBridge.getSessionInfo()).screen_height
  } catch (e) {
    console.log("error in getDeviceHeight", e);
    return -1;
  }
}

export const getScreenPpi = function () {
  try {
    return Math.round(JSON.parse(JBridge.getSessionInfo()).screen_ppi);
  } catch (e) {
    console.log("error in getScreenPpi", e);
    return -1;
  }
}


export const safeMarginTopImpl = function () {
  try {
    if (parent.__DEVICE_DETAILS && parent.__DEVICE_DETAILS.safe_area_frame) {
      return parent.__DEVICE_DETAILS.safe_area_frame.y
    }
  } catch (e) {
    console.log("error in safeMarginTopImpl", e);
  }

  return 0;
}

export const safeMarginBottomImpl = function () {
  try {
    const d = parent.__DEVICE_DETAILS;
    if (!d || !d.safe_area_frame) {
      return 0;
    }
    return (d.screen_height - d.safe_area_frame.height - d.safe_area_frame.y);
  } catch (e) {
    return 0;
  }
}
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
      const month = date.toLocaleDateString(language, {
        month: "short"
      });
      return `${month}`;
    },
    "MMMM": () => {
      const month = date.toLocaleDateString(language, {
        month: "long"
      });
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

export const getPermissionStatusImpl = function (permission) {
  return function () {
    window.JBridge.checkPermission(permission);
  };
};

export const requestPermissionImpl = function (err, success, permissions) {
  return function () {
    const callback = callbackMapper.map(function (params) {
      console.warn("Permissions", params)
      success(JSON.stringify(params))();
    });
    console.log(permissions, typeof permissions)
    window.JBridge.requestPermission(permissions, "2", callback);
  };
};

export const getKeyInSharedPrefKeys = function (key) {
  return JBridge.getFromSharedPrefs(key);
};

export const setKeyInSharedPref = function (key, value) {
  return JBridge.setInSharedPrefs(key, value);
};

function getPayload(action) {
  const innerPayload = Object.assign({}, window.__payload.payload);
  const initiatePayload = Object.assign({}, window.__payload);
  if (action) innerPayload["action"] = action;
  initiatePayload["payload"] = innerPayload;
  initiatePayload["service"] = "in.yatri.provider";
  return initiatePayload;
}

export const bootDriverInParallel = function () {
  console.log("hello -? ")
  if (JOS) {
    JOS.startApp("in.yatri.provider")(getPayload("initiate"))(null)();
  } else {
    console.error("JOS Not Found")
  }
}

export const openDriverApp = function () {
  if (JOS) {
    JOS.emitEvent("in.yatri.provider")("onMerchantEvent")("process")(JSON.stringify(getPayload()))();
  } else {
    console.error("JOS Not Found")
  }
}

export const waitTillDriverAppBoot = function (cb) {
  if (window.isDriverAppInitiated || Object.prototype.hasOwnProperty.call(window.parent.mapps, "in.yatri.provider")) {
    cb()();
  } else {
    console.error("Waiting for Driver App to Boot")
    setTimeout(() => {waitTillDriverAppBoot(cb)},5);
  }
}

export const getRandomID = function(max) {
  const id = Math.floor(Math.random() * max) + 1;
  return id.toString(); 
}

export const emitEvent = function (mapp, eventType, payload) {
  console.log("payload", payload);
  JOS.emitEvent(mapp)(eventType)(JSON.stringify(payload))()()
};