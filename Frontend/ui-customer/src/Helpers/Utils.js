/*

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

*/

import { callbackMapper } from "presto-ui";

const JBridge = window.JBridge;
const notificationCallBacks = {};
let tracking_id = 0;
export const getNewTrackingId = function (unit) {
  tracking_id += 1;
  return JSON.stringify(tracking_id);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
  return JBridge.getFromSharedPrefs(key);
};

export const validateInputPattern = function (input, pattern) {
  const reg = new RegExp(pattern, "g");
  const result = reg.test(input);
  console.log("validateInputPattern " + result + " Values :- " + input + " Pattern :- " + pattern);
  return (result);
}

export const getLocationName = function (cb) {
  return function (lat) {
    return function (lng) {
      return function (defaultText) {
        return function (action) {
          return function () {
            const callback = callbackMapper.map(function (resultLat, resultLon, result) {
              const decodedString = decodeURIComponent(result).replace(/\+/g, " ");
              cb(action(parseFloat(resultLat))(parseFloat(resultLon))(decodedString))();
            });
            return window.JBridge.getLocationName(lat, lng, defaultText, callback);
          }
        }
      }
    }
  }
}

export const getCurrentDate = function (string) {
  let today = new Date();
  const dd = String(today.getDate()).padStart(2, "0");
  const mm = String(today.getMonth() + 1).padStart(2, "0"); //January is 0!
  const yyyy = today.getFullYear();
  today = dd + "/" + mm + "/" + yyyy;
  return today;
}

export const getCurrentDatev2 = function (string) {
  let today = new Date();
  const dd = String(today.getDate()).padStart(2, "0");
  const mm = String(today.getMonth() + 1).padStart(2, "0"); //January is 0!
  const yyyy = today.getFullYear();
  today = yyyy + "-" + mm + "-" + dd;
  return today;
}

export const getNextDate = function (unit) {
  const currentDate = new Date();
  const isLastDayOfMonth = (currentDate.getDate() === new Date(currentDate.getFullYear(), currentDate.getMonth() + 1, 0).getDate());
  if (isLastDayOfMonth) {
    currentDate.setDate(1);
    currentDate.setMonth(currentDate.getMonth() + 1);
  } else {
    currentDate.setDate(currentDate.getDate() + 1);
  }
  const dd = String(currentDate.getDate()).padStart(2, "0");
  const mm = String(currentDate.getMonth() + 1).padStart(2, "0"); //January is 0!
  const yyyy = currentDate.getFullYear();
  if(unit === "yyyy-mm-dd")
    return yyyy + "-" + mm + "-" + dd;
  return dd + "/" + mm + "/" + yyyy;
}

export const getNextDateV2 = function (unit) {
  const currentDate = new Date();
  const dd = String(currentDate.getDate()).padStart(2, "0");
  const mm = String(currentDate.getMonth() + 1).padStart(2, "0"); //January is 0!
  const yyyy = currentDate.getFullYear();
  return yyyy + "-" + mm + "-" + dd;
}

export const validateEmail = function (email) {
  const re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
  return re.test(String(email).toLowerCase());
}

export const factoryResetApp = function (str) {
  console.log("HERE IN RESET ===--->>")
  window.JBridge.factoryResetApp()
}

export const secondsToHms = function (d) {
  d = Number(d);
  const h = Math.floor(d / 3600);
  const m = Math.floor(d % 3600 / 60);

  const mDisplay = m > 0 ? m + (m == 1 ? " min " : " mins ") : "--";
  const hDisplay = h > 0 ? h + (h == 1 ? " hr" : " hrs") : "";
  if(hDisplay != "" && mDisplay == "--") return hDisplay
  else return hDisplay + ((hDisplay != "") ? ", " : "") + mDisplay;
}

export const toIST = function (date) {
  const today = new Date(date);
  const istOffset = 5.5 * 60 * 60 * 1000;
  const istTime = new Date(today.getTime() + istOffset);
  return istTime;
}


export const getISTDay = function (date) {
  const today = toIST(date);
  return today.getDay();
}


export const getISTMonth = function (date) {
  const today = toIST(date);
  return today.getMonth();
}

export const getISTFullYear = function (date) {
  const today = toIST(date);
  return today.getFullYear();
}

export const getISTDate = function (date) {
  const today = toIST(date);
  return today.getDate();
}

export const getISTHours = function (date) {
  const today = toIST(date);
  return today.getHours();
}

export const getISTMinutes = function (date) {
  const today = toIST(date);
  return today.getMinutes();
}

export const getISTSeconds = function (date) {
  const today = toIST(date);
  return today.getSeconds();
}


export const getUTCDay = function (date) {
  const today = new Date(date)
  return today.getUTCDay();
}

export const getUTCMonth = function (date) {
  const today = new Date(date)
  return today.getUTCMonth();
}

export const getUTCFullYear = function (date) {
  const today = new Date(date)
  return today.getUTCFullYear();
}

export const getUTCDate = function (date) {
  const today = new Date(date)
  return today.getUTCDate();
}

export const getUTCHours = function (date) {
  const today = new Date(date)
  return today.getUTCHours();
}

export const getUTCMinutes = function (date) {
  const today = new Date(date)
  return today.getUTCMinutes();
}

export const getUTCSeconds = function (date) {
  const today = new Date(date)
  return today.getUTCSeconds();
}

export const getTime = function (unit) {
  return Date.now();
}

export const requestKeyboardShow = function (id) {
  return function () {
    const delayInMilliseconds = 100;
    setTimeout(function () {
      window.JBridge.requestKeyboardShow(id);
    }, delayInMilliseconds);
  }
}

export const storeCallBackCustomer = function (cb) {
  return function (action) {
    return function(screenName){
      return function(just){
        return function(nothing){
          return function () {
            try {
              notificationCallBacks[screenName] = function(notificationType,notificationBody){
                const parsedNotificationBody = JSON.parse(notificationBody || {});
                const currentTimeUTC = new Date().toISOString();
                const rideTimeIfAvailable = (parsedNotificationBody.rideTime) ? just (parsedNotificationBody.rideTime) : just (currentTimeUTC);
                const bookingIdIfAvailable = (parsedNotificationBody.bookingId) ? just (parsedNotificationBody.bookingId) : nothing;
                cb(action(notificationType)({rideTime : rideTimeIfAvailable, bookingId : bookingIdIfAvailable}))();
              }
              const callback = callbackMapper.map(function (notificationType, notificationBody) {
                console.log("notificationType ->", notificationType);
                console.log("notificationBody ->", notificationBody);
                window.notificationType = notificationType;
                if (window.whitelistedNotification.has(notificationType)) {
                  Object.keys(notificationCallBacks).forEach((key) => {
                    notificationCallBacks[key](notificationType,notificationBody);
                  })
                  if (notificationBody) {
                    window.notificationBody = JSON.parse(notificationBody);
                  }
                }
              });
              const notificationCallBack = function (notificationType,notificationBody) {
                console.log("notificationType ->", notificationType);
                if (window.whitelistedNotification.has(notificationType)) {
                  Object.keys(notificationCallBacks).forEach((key) => {
                    notificationCallBacks[key](notificationType,notificationBody);
                  })
                }
              };
              window.callNotificationCallBack = notificationCallBack;
              console.log("In storeCallBackCustomer ---------- + " + action);
              JBridge.storeCallBackCustomer(callback);
            }
            catch (error) {
              console.log("Error occurred in storeCallBackCustomer ------", error);
            }
          }
        }
      }
    }
  }
}

export const storeCallBackContacts = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (contact) {
          const json = JSON.parse(contact.toString().replace(/\s/g, " "));
          if (window.__OS == "IOS") {
            json.forEach((object) => {
              let base64regex = /^([0-9a-zA-Z+/]{4})*(([0-9a-zA-Z+/]{2}==)|([0-9a-zA-Z+/]{3}=))?$/;
              if (base64regex.test(object.name)) {
                object.name = new TextDecoder('utf-8').decode(Uint8Array.from(atob(object.name), c => c.charCodeAt(0)));
              }
            });
          }
          console.log("storeCallBackContacts js " , json);
          cb(action(json))();
        });

        console.log("In storeCallBackContacts ---------- + " , action);
        return window.JBridge.storeCallBackContacts(callback);
      } catch (err) {
        console.log("storeCallBackContacts error " + err);
      }
    }
  }
}

export const parseNewContacts = function (contact) {
  return JSON.parse(contact);
}


export const makePascalCase = function (str) {
  let changeToUpperCase = str[0].toUpperCase();
  for (let i = 1; i < str.length; i++) {
    if (str[i - 1] == " " || str[i - 1] == ",") {
      changeToUpperCase += str[i].toUpperCase();
    } else {
      changeToUpperCase += str[i].toLowerCase();
    }
  }
  return changeToUpperCase;
}

export const decodeError = function (er) {
  return function (key) {
    try {
      const errorPayload = JSON.parse(er)[key];
      if (errorPayload === null)
        return "";
      return errorPayload.toString();
    } catch (e) {
      console.log(e);
      return "";
    }
  }
};

export const didReceiverMessage = function() {
  try {
    return window.didReceiverMessage || false;
  } catch (error) {
    console.log("Error in didReceiverMessage " + error);
    return false;
  }
}


export const setRefreshing = function (id) {
  return function (bool) {
    if (window.__OS == "ANDROID") {
      const cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setRefreshing:b_" + bool + ";"
      window.Android.runInUI(cmd, null)
    }
  }
}

export const setEnabled = function (id) {
  return function (bool) {
    if (window.__OS == "ANDROID") {
      const cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setEnabled:b_" + bool + ";"
      window.Android.runInUI(cmd, null)
    }
  }
}

export const fetchFromLocalStoreImpl = function (key) {
  return function (just) {
    return function (nothing) {
      return function () {
        const state = JBridge.getFromSharedPrefs(key);
        if (state != "__failed" && state != "(null)") {
          return just(state);
        }
        return nothing;
      };
    };
  };
}

export const fetchFromLocalStoreTempImpl = function (key) {
  return function (just) {
    return function (nothing) {
      return function () {
        const state = JBridge.getFromSharedPrefs(key);
        const newState = JSON.parse(state);
        const predictionArray = newState.predictionArray;
        try {
          for (let i = 0; i < predictionArray.length; i++) {
            if (!Object.prototype.hasOwnProperty.call(predictionArray[i], "fullAddress")) {
              predictionArray[i].fullAddress = {};
            }
          }
        }
        catch (e) {
          console.log(e);
        }

        newState["predictionArray"] = predictionArray;
        if (state != "__failed" && state != "(null)") {
          return just(JSON.stringify(newState));
        }
        return nothing;
      };
    };
  };
}

export const saveToLocalStoreImpl = function (key) {
  return function (state) {
    console.log("==------>>>>>> SAVE SCREEN");
    console.log(key);
    console.log(state);
    window.JBridge.setKeysInSharedPrefs(key, state);
    return function () {
      console.log("==------>>>>>> SAVED SCREEN");
    };
  };
}

export const seperateByWhiteSpaces = function (string) {
  return string.replace(/\s+/g, " ").trim();
};

export const shuffle = function (array) {
  const shuffled = array
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
  return shuffled
}

function between(x, min, max) {
  return x >= min && x <= max;
}

export const withinTimeRange = function (startTime) {
  return function (endTime) {
    return function (timeStr) {
      try {
        return startTime < endTime ? between(timeStr, startTime, endTime) : between(timeStr, startTime, "23:59:59") || between(timeStr, "00:00:01", endTime);
      } catch (err) {
        return false;
      }
    }
  }
}

export const isWeekend = function (dateString) {
  const date = new Date(dateString);
  const dayOfWeek = date.getDay();
  return dayOfWeek === 0 || dayOfWeek === 6; // 0 is Sunday, 6 is Saturday
}

export const adjustViewWithKeyboard = function (flag) {
  return function () {
    if (window.JBridge.adjustViewWithKeyboard) {
      window.JBridge.adjustViewWithKeyboard(flag)
    }
  };
};

export const fetchAndUpdateCurrentLocation = function (cb) {
  return function (action) {
    return function (fallbackAction) {
      return function () {
        if (window.JBridge.fetchAndUpdateCurrentLocation) {
          const callback = callbackMapper.map(function (lat, lng) {
            cb(action(lat)(lng))();
          });
          return window.JBridge.fetchAndUpdateCurrentLocation(callback);
        } else {
          const fallBackCallback = callbackMapper.map(function () {
            cb(fallbackAction)();
          });
          window.callUICallback(fallBackCallback);
        }
      };
    };
  };
};
export const contactPermission = function () {
  if (window.JBridge.contactPermission) {
    return window.JBridge.contactPermission();
  }
}

export const performHapticFeedback = function () {
  if (window.JBridge.performHapticFeedback) {
    return window.JBridge.performHapticFeedback();
  }
}


export const drawPolygon = function (geoJson) {
  return function (locationName) {
    return function () {
      if (JBridge.drawPolygon) {
        JBridge.drawPolygon(geoJson, locationName);
      }
    }
  }
}

export const removeLabelFromMarker = (zoomLevel) => {
  if (JBridge.removeLabelFromMarker) {
    try {
      return JBridge.removeLabelFromMarker(zoomLevel);
    } catch (err) {
      return JBridge.removeLabelFromMarker();
    }
  }
}

export const strLenWithSpecificCharacters = function (input) {
  return function (pattern) {
    const regex = new RegExp(pattern, "g");
    const matches = input.match(regex);
    return matches ? matches.length : 0;
  }
}

export const getMobileNumber = function (signatureAuthData, maskedNumber) {
  try {
    const re = /^[6-9][)]?[-\s.]?[0-9]{3}[-\s.]?[0-9]{4,6}$/;
    const mobileNumber = JSON.parse(signatureAuthData).mobileNumber;
    if (re.test(mobileNumber)) {
      return mobileNumber;
    } else {
      return maskedNumber.replace("...", "****");
    }
  } catch (err) {
    console.log("Decode mobileNumber from SignatureAuthData Error => " + err);
  }
}

export const extractKeyByRegex = (regex, text) => {
  const matches = text.match(regex);
  return matches ? matches[0] : "";
}

export const _generateQRCode = function (data, id, size, margin, sc) {
  if (typeof JBridge.generateQRCode === "function") {
    try {
      const cb = callbackMapper.map(function (_status) {
        console.log("QR status:: ", _status);
        sc(_status)();
      });
      JBridge.generateQRCode(data, id, size, margin, cb);
    } catch (e) {
      console.warn(e);
      sc("FAILURE")();
    }
  }
  else {
    sc("FAILURE")();
  }
}

export const parseSourceHashArray = function (str) {
  return JSON.parse(str);
}
export const getDeviceDefaultDensity = function (){
  return window.fetchCachedSessionInfo ? window.fetchCachedSessionInfo("screen_ppi") : JSON.parse(window.JBridge.getSessionInfo())["screen_ppi"];
}
export const getDefaultPixels = function (){
  if(window.JBridge.getDefaultPixels)return parseFloat(window.JBridge.getDefaultPixels());
  else return getDeviceDefaultDensity();
}

export const getPixels = function (){
  if(window.__OS == "IOS"){
    return parseFloat(window.JBridge.getPixel());
  } 
  if (window.parent.devicePixelRatio) {
    return window.parent.devicePixelRatio;
  } else {
    return window.JBridge.getPixels();
  }
}

export const getDateAfterNDaysv2 = function (n) {
  const today = new Date();
  const dateAfterNDays = new Date(today);
  dateAfterNDays.setDate(today.getDate() + n);
  const year = dateAfterNDays.getFullYear();
  const month = String(dateAfterNDays.getMonth() + 1).padStart(2, "0");
  const day = String(dateAfterNDays.getDate()).padStart(2, "0");
  return `${year}-${month}-${day}`;
}

export const incrOrDecrTimeFrom = function (inputTime, minutesToAddOrSubtract, isIncrement) {
  const [hours, minutes, seconds] = inputTime.split(":").map(Number);
  const date = new Date();
  date.setHours(hours);
  date.setMinutes(minutes);
  date.setSeconds(seconds);
    
  if (isIncrement) date.setMinutes(date.getMinutes() + minutesToAddOrSubtract);
  else date.setMinutes(date.getMinutes() - minutesToAddOrSubtract);
  const newTime = `${String(date.getHours()).padStart(2, "0")}:${String(date.getMinutes()).padStart(2, "0")}:${String(date.getSeconds()).padStart(2, "0")}`;
  return newTime;
}

export const getMockFollowerName = function() {
  let currentMockName = "User";
  if (window.notificationBody) {
    const msg = window.notificationBody.msg;
    currentMockName = msg.split(" ")[0];
  } else if (window.__payload && window.__payload.payload && window.__payload.payload.fullNotificationBody && window.__payload.payload.fullNotificationBody.msg) {
    const msg = window.__payload.payload.fullNotificationBody.msg;
    currentMockName = msg.split(" ")[0];
  }
  return currentMockName;
}


export const decodeErrorCode = function (a) {
  try {
    const errorCodee = JSON.parse(a).errorCode;
    return  errorCodee;
  } catch (e) {
    console.log(e);
    return " ";
  }
};

export const isHybridApp = ()=>{
  try {
    return (window.__payload.payload.isHybrid === true);
  } catch (unhandled){
    return false;
  }
}

export const decodeErrorMessage = function (a) {
  try {
    const errorMessagee = JSON.parse(a).errorMessage;
    if(errorMessagee === null)
    {
      return "";
    }
    return  errorMessagee;
  } catch (e) {
    console.log(e);
    return " ";
  }
};


export const releaseBackpress = function (unit) {
  const jpConsumingBackpress = {
    event: "jp_consuming_backpress",
    payload: { jp_consuming_backpress: false }
  }
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
}

export const isItSameDay = (date) => {
  if (date == "__failed" || date == "(null)") return false;
  const dateObj = new Date(parseInt(date));
  const today = new Date();
  return today.getDate() == dateObj.getDate() && (today.getMonth() == dateObj.getMonth()) && (today.getFullYear() == dateObj.getFullYear())
}

export const launchAppSettings = function (unit) {
  return JBridge.openApp(null,"package:" + JSON.parse(JBridge.getSessionInfo()).package_name,"android.settings.APPLICATION_DETAILS_SETTINGS",268435456,2000);
};

