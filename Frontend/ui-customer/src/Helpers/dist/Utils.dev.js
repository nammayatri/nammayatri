"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.logEvent = exports.storeOnResumeCallback = exports.goBackPrevWebPage = exports.initialWebViewSetUp = exports.contactPermission = exports.fetchAndUpdateCurrentLocation = exports.adjustViewWithKeyboard = exports.withinTimeRange = exports.shuffle = exports.seperateByWhiteSpaces = exports.saveToLocalStoreImpl = exports.fetchFromLocalStoreTempImpl = exports.fetchFromLocalStoreImpl = exports.debounceFunction = exports.updateInputString = exports.getCurrentUTC = exports.getExpiryTime = exports.convertUTCtoISC = exports.setEnabled = exports.setRefreshing = exports.clearWaitingTimer = exports.waitingCountdownTimer = exports.toString = exports.decodeErrorMessage = exports.decodeErrorCode = exports.makePascalCase = exports.parseNewContacts = exports.storeCallBackContacts = exports.storeCallBackCustomer = exports.storeCallBackLocateOnMap = exports.setTextImpl = exports.requestKeyboardShow = exports.getTime = exports.getUTCDay = exports.secondsToHms = exports.dateCompare = exports.factoryResetApp = exports.addTimeToDate = exports.validateEmail = exports.getCurrentDate = exports.hideSplash = exports.getLocationName = exports.validateInputPattern = exports.getKeyInSharedPrefKeysConfigEff = exports.getNewTrackingId = void 0;

var _prestoUi = require("presto-ui");

var _moment = _interopRequireDefault(require("moment"));

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { "default": obj }; }

var tracking_id = 0;

var getNewTrackingId = function getNewTrackingId(unit) {
  tracking_id += 1;
  return JSON.stringify(tracking_id);
};

exports.getNewTrackingId = getNewTrackingId;

var getKeyInSharedPrefKeysConfigEff = function getKeyInSharedPrefKeysConfigEff(key) {
  return window.JBridge.getKeysInSharedPrefs(key);
};

exports.getKeyInSharedPrefKeysConfigEff = getKeyInSharedPrefKeysConfigEff;

var validateInputPattern = function validateInputPattern(input, pattern) {
  var reg = new RegExp(pattern, 'g');
  var result = reg.test(input);
  console.log("validateInputPattern " + result + " Values :- " + input + " Pattern :- " + pattern);
  return result;
};

exports.validateInputPattern = validateInputPattern;

var getLocationName = function getLocationName(cb) {
  return function (lat) {
    return function (lng) {
      return function (defaultText) {
        return function (action) {
          return function () {
            var callback = _prestoUi.callbackMapper.map(function (lat, lon, result) {
              var decodedString = decodeURIComponent(result).replace(/\+/g, ' ');
              cb(action(lat)(lon)(decodedString))();
            });

            return window.JBridge.getLocationName(lat, lng, defaultText, callback);
          };
        };
      };
    };
  };
};

exports.getLocationName = getLocationName;
var hideSplash = window.JOS.emitEvent("java")("onEvent")(JSON.stringify({
  event: "hide_splash"
}))();
exports.hideSplash = hideSplash;

var getCurrentDate = function getCurrentDate(string) {
  var today = new Date();
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!

  var yyyy = today.getFullYear();
  today = dd + '/' + mm + '/' + yyyy;
  return today;
};

exports.getCurrentDate = getCurrentDate;

var validateEmail = function validateEmail(email) {
  var re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
  return re.test(String(email).toLowerCase());
};

exports.validateEmail = validateEmail;

var addTimeToDate = function addTimeToDate(date) {
  return function (num) {
    return function (timeunit) {
      return (0, _moment["default"])(date).add(num, timeunit).toDate();
    };
  };
};

exports.addTimeToDate = addTimeToDate;

var factoryResetApp = function factoryResetApp(str) {
  console.log("HERE IN RESET ===--->>");
  window.JBridge.factoryResetApp();
};

exports.factoryResetApp = factoryResetApp;

var dateCompare = function dateCompare(currentDate) {
  return function (validDate) {
    console.log((0, _moment["default"])(currentDate).isBefore(validDate), currentDate, validDate);

    if (!(0, _moment["default"])(currentDate).isBefore(validDate)) {
      window.JBridge.toast("Invalid Date/Time");
    }

    return (0, _moment["default"])(currentDate).isBefore(validDate);
  };
};

exports.dateCompare = dateCompare;

var secondsToHms = function secondsToHms(d) {
  d = Number(d);
  var h = Math.floor(d / 3600);
  var m = Math.floor(d % 3600 / 60); // var s = Math.floor(d % 3600 % 60);

  var hDisplay = h > 0 ? h + (h == 1 ? " hr, " : " hrs, ") : "";
  var mDisplay = m > 0 ? m + (m == 1 ? " min " : " mins ") : ""; // var sDisplay = s > 0 ? s + (s == 1 ? " second" : " seconds") : "";

  return hDisplay + mDisplay; //+ sDisplay;
};

exports.secondsToHms = secondsToHms;

var getUTCDay = function getUTCDay(date) {
  return date.getUTCDay();
};

exports.getUTCDay = getUTCDay;

var getTime = function getTime(unit) {
  return Date.now();
};

exports.getTime = getTime;

var requestKeyboardShow = function requestKeyboardShow(id) {
  return function () {
    var delayInMilliseconds = 100;
    setTimeout(function () {
      window.JBridge.requestKeyboardShow(id);
    }, delayInMilliseconds);
  };
};

exports.requestKeyboardShow = requestKeyboardShow;

function setText(id, text, pos) {
  if (__OS === "ANDROID") {
    var cmd = "set_view=ctx->findViewById:i_" + id + ";";
    cmd += "get_view->setText:cs_" + text + ";";
    cmd += "get_view->setSelection:i_" + pos + ";";
    window.Android.runInUI(cmd, null);
  } else {
    window.Android.runInUI({
      id: id,
      text: text
    });
    window.Android.runInUI({
      id: id,
      cursorPosition: pos
    });
  }
}

var setTextImpl = function setTextImpl(id) {
  return function (text) {
    return function () {
      setText(id, text, text.length);
    };
  };
};

exports.setTextImpl = setTextImpl;

var storeCallBackLocateOnMap = function storeCallBackLocateOnMap(cb) {
  try {
    return function (action) {
      return function () {
        var callback = _prestoUi.callbackMapper.map(function (key, lat, lon) {
          console.log("in show storeCallBackLocateOnMap", action);
          window.x = cb;
          window.y = action;
          cb(action(key)(lat)(lon))();
        });

        console.log("In storeCallBackLocateOnMap ---------- + " + action);
        window.JBridge.storeCallBackLocateOnMap(callback);
      };
    };
  } catch (error) {
    console.log("Error occurred in storeCallBackLocateOnMap ------", error);
  }
};

exports.storeCallBackLocateOnMap = storeCallBackLocateOnMap;

var storeCallBackCustomer = function storeCallBackCustomer(cb) {
  try {
    return function (action) {
      return function () {
        var callback = _prestoUi.callbackMapper.map(function (notificationType) {
          cb(action(notificationType))();
        });

        console.log("In storeCallBackCustomer ---------- + " + action);
        window.JBridge.storeCallBackCustomer(callback);
      };
    };
  } catch (error) {
    console.log("Error occurred in storeCallBackCustomer ------", error);
  }
};

exports.storeCallBackCustomer = storeCallBackCustomer;

var storeCallBackContacts = function storeCallBackContacts(cb) {
  return function (action) {
    return function () {
      try {
        var callback = _prestoUi.callbackMapper.map(function (contact) {
          json = JSON.parse(contact);
          console.log("storeCallBackContacts js " + json);
          cb(action(json))();
        });

        console.log("In storeCallBackContacts ---------- + " + action);
        window.JBridge.storeCallBackContacts(callback);
      } catch (err) {
        console.log("storeCallBackContacts error " + err);
      }
    };
  };
};

exports.storeCallBackContacts = storeCallBackContacts;

var parseNewContacts = function parseNewContacts(String) {
  return JSON.parse(String);
};

exports.parseNewContacts = parseNewContacts;

var makePascalCase = function makePascalCase(str) {
  var changeToUpperCase = str[0].toUpperCase();

  for (var i = 1; i < str.length; i++) {
    if (str[i - 1] == " " || str[i - 1] == ",") {
      changeToUpperCase += str[i].toUpperCase();
    } else {
      changeToUpperCase += str[i].toLowerCase();
    }
  }

  return changeToUpperCase;
};

exports.makePascalCase = makePascalCase;

var decodeErrorCode = function decodeErrorCode(a) {
  try {
    var errorCodee = JSON.parse(a).errorCode;
    return errorCodee;
  } catch (e) {
    console.log(e);
    return " ";
  }
};

exports.decodeErrorCode = decodeErrorCode;

var decodeErrorMessage = function decodeErrorMessage(a) {
  try {
    var errorMessagee = JSON.parse(a).errorMessage;

    if (errorMessagee == null) {
      return "";
    }

    return errorMessagee;
  } catch (e) {
    console.log(e);
    return " ";
  }
};

exports.decodeErrorMessage = decodeErrorMessage;

var toString = function toString(attr) {
  return JSON.stringify(attr);
};

exports.toString = toString;
var driverWaitingTimerId = null;

var waitingCountdownTimer = function waitingCountdownTimer(startingTime) {
  return function (cb) {
    return function (action) {
      return function () {
        if (__OS == "IOS") {
          if (window.JBridge.startCountUpTimer) {
            var callbackIOS = _prestoUi.callbackMapper.map(function (timerId, sec) {
              var minutes = getTwoDigitsNumber(Math.floor(sec / 60));
              var seconds = getTwoDigitsNumber(sec - minutes * 60);
              var timeInMinutesFormat = minutes + " : " + seconds;
              cb(action(timerId)(timeInMinutesFormat)(sec))();
            });

            window.JBridge.startCountUpTimer(startingTime.toString(), callbackIOS);
          }
        } else {
          var callback = _prestoUi.callbackMapper.map(function () {
            var sec = startingTime;
            if (driverWaitingTimerId) clearInterval(driverWaitingTimerId);
            driverWaitingTimerId = setInterval(convertInMinutesFromat, 1000);

            function convertInMinutesFromat() {
              sec++;
              var minutes = getTwoDigitsNumber(Math.floor(sec / 60));
              var seconds = getTwoDigitsNumber(sec - minutes * 60);
              var timeInMinutesFormat = minutes + " : " + seconds;
              cb(action(driverWaitingTimerId)(timeInMinutesFormat)(sec))();
            }
          });

          window.callUICallback(callback);
        }
      };
    };
  };
};

exports.waitingCountdownTimer = waitingCountdownTimer;

var clearWaitingTimer = function clearWaitingTimer(id) {
  if (__OS == "IOS" && id == "countUpTimerId") {
    if (window.JBridge.clearCountUpTimer) {
      window.JBridge.clearCountUpTimer();
    }
  } else {
    clearInterval(parseInt(id));
  }
};

exports.clearWaitingTimer = clearWaitingTimer;

function getTwoDigitsNumber(number) {
  return number >= 10 ? number : "0" + number.toString();
}

var setRefreshing = function setRefreshing(id) {
  return function (bool) {
    if (window.__OS == "ANDROID") {
      var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setRefreshing:b_" + bool + ";";
      window.Android.runInUI(cmd, null);
    }
  };
};

exports.setRefreshing = setRefreshing;

var setEnabled = function setEnabled(id) {
  return function (bool) {
    if (window.__OS == "ANDROID") {
      var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setEnabled:b_" + bool + ";";
      window.Android.runInUI(cmd, null);
    }
  };
};

exports.setEnabled = setEnabled;

var convertUTCtoISC = function convertUTCtoISC(str) {
  return function (format) {
    var localTime1 = _moment["default"].utc(str).toDate();

    localTime1 = (0, _moment["default"])(localTime1).format(format);
    return localTime1;
  };
};

exports.convertUTCtoISC = convertUTCtoISC;

var getExpiryTime = function getExpiryTime(str1) {
  return function (str2) {
    return function (forLostAndFound) {
      var expiry = new Date(str1);
      var d = new Date();
      var result = (0, _moment["default"])(d).utc().format();
      var current = new Date(result);
      console.log(current + " , " + expiry + "STR");
      var diff = (expiry.getTime() - current.getTime()) / 1000;

      if (forLostAndFound) {
        diff = (current.getTime() - expiry.getTime()) / 1000;
      }

      diff = Math.round(diff);
      console.log("STR ->>>> expiry time " + diff);
      if (diff >= 0) return diff;else return 0;
    };
  };
};

exports.getExpiryTime = getExpiryTime;

var getCurrentUTC = function getCurrentUTC(str) {
  var d = new Date();
  var result = (0, _moment["default"])(d).utc().format();
  console.log(result);
  return result;
}; // exports ["debounceFunction"] = function (delay) { NEED TO HANDLE DEBOUNCING IN LOCATEONMAP
//   return function (cb){
//     return function (action){
//       return function(){
//         console.logs("debounceFunctiondebounceFunction");
//         var callback = callbackMapper.map(function () {
//           if (timerIdDebounce) clearTimeout(timerIdDebounce);
//           timerIdDebounce = setTimeout(() => {
//             timerIdDebounce = "MAKEAPICALL";
//             cb(action (inputForDebounce))();
//           },delay);
//         });
//         window.callUICallback(callback);
//       }
//     }
//   }
// }


exports.getCurrentUTC = getCurrentUTC;
var inputForDebounce;

var updateInputString = function updateInputString(a) {
  console.log("UPDATED STRING NOW" + a);
  inputForDebounce = a;
};

exports.updateInputString = updateInputString;
var timerIdDebounce = null;

var debounceFunction = function debounceFunction(delay) {
  return function (cb) {
    return function (action) {
      return function () {
        console.log("CALLED :- ");

        var callback = _prestoUi.callbackMapper.map(function () {
          if (timerIdDebounce) clearTimeout(timerIdDebounce);
          timerIdDebounce = setTimeout(function () {
            timerIdDebounce = "MAKEAPICALL";
            cb(action(inputForDebounce))();
          }, delay);
        });

        window.callUICallback(callback);
      };
    };
  };
};

exports.debounceFunction = debounceFunction;

var fetchFromLocalStoreImpl = function fetchFromLocalStoreImpl(key) {
  return function (just) {
    return function (nothing) {
      return function () {
        var state = window.JBridge.getKeysInSharedPrefs(key);

        if (state != "__failed" && state != "(null)") {
          return just(state);
        }

        return nothing;
      };
    };
  };
};

exports.fetchFromLocalStoreImpl = fetchFromLocalStoreImpl;

var fetchFromLocalStoreTempImpl = function fetchFromLocalStoreTempImpl(key) {
  return function (just) {
    return function (nothing) {
      return function () {
        var state = window.JBridge.getKeysInSharedPrefs(key);
        var newState = JSON.parse(state);
        var predictionArray = newState.predictionArray;

        try {
          for (var i = 0; i < predictionArray.length; i++) {
            if (!predictionArray[i].hasOwnProperty("fullAddress")) {
              predictionArray[i].fullAddress = {};
            }
          }
        } catch (e) {
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
};

exports.fetchFromLocalStoreTempImpl = fetchFromLocalStoreTempImpl;

var saveToLocalStoreImpl = function saveToLocalStoreImpl(key) {
  return function (state) {
    console.log("==------>>>>>> SAVE SCREEN");
    console.log(key);
    console.log(state);
    window.JBridge.setKeysInSharedPrefs(key, state);
    return function () {
      console.log("==------>>>>>> SAVED SCREEN");
    };
  };
};

exports.saveToLocalStoreImpl = saveToLocalStoreImpl;

var seperateByWhiteSpaces = function seperateByWhiteSpaces(string) {
  return string.replace(/\s+/g, ' ').trim();
}; // function uuidv4() {
//   return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
//     (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
//   );
// }
// exports["generateSessionToken"] = function (str){
//     var token = uuidv4();
//     return token;
// }


exports.seperateByWhiteSpaces = seperateByWhiteSpaces;

var shuffle = function shuffle(array) {
  var shuffled = array.map(function (value) {
    return {
      value: value,
      sort: Math.random()
    };
  }).sort(function (a, b) {
    return a.sort - b.sort;
  }).map(function (_ref) {
    var value = _ref.value;
    return value;
  });
  return shuffled;
};

exports.shuffle = shuffle;

var withinTimeRange = function withinTimeRange(startTime) {
  return function (endTime) {
    try {
      var currentTimeString = (0, _moment["default"])(new Date()).format("HH:mm:ss");
      return startTime < endTime ? between(currentTimeString, startTime, endTime) : between(currentTimeString, startTime, "23:59:59") || between(currentTimeString, "00:00:01", endTime);
    } catch (err) {
      return false;
    }
  };
};

exports.withinTimeRange = withinTimeRange;

function between(x, min, max) {
  return x >= min && x <= max;
}

var adjustViewWithKeyboard = function adjustViewWithKeyboard(flag) {
  return function () {
    if (window.JBridge.adjustViewWithKeyboard) {
      window.JBridge.adjustViewWithKeyboard(flag);
    }
  };
};

exports.adjustViewWithKeyboard = adjustViewWithKeyboard;

var fetchAndUpdateCurrentLocation = function fetchAndUpdateCurrentLocation(cb) {
  return function (action) {
    return function (fallbackAction) {
      return function () {
        if (window.JBridge.fetchAndUpdateCurrentLocation) {
          var callback = _prestoUi.callbackMapper.map(function (lat, lng) {
            cb(action(lat)(lng))();
          });

          return window.JBridge.fetchAndUpdateCurrentLocation(callback);
        } else {
          // fallback for previous release
          var fallBackCallback = _prestoUi.callbackMapper.map(function () {
            cb(fallbackAction)();
          });

          window.callUICallback(fallBackCallback);
        }
      };
    };
  };
};

exports.fetchAndUpdateCurrentLocation = fetchAndUpdateCurrentLocation;

var contactPermission = function contactPermission() {
  if (window.JBridge.contactPermission) {
    return window.JBridge.contactPermission();
  }
};

exports.contactPermission = contactPermission;

var initialWebViewSetUp = function initialWebViewSetUp(cb) {
  return function (id) {
    return function (action) {
      return function () {
        try {
          var callback = _prestoUi.callbackMapper.map(function (val) {
            cb(action(val))();
          });

          return window.JBridge.initialWebViewSetUp(callback, id);
        } catch (err) {
          console.log("initialWebViewSetUp error " + err);
        }
      };
    };
  };
};

exports.initialWebViewSetUp = initialWebViewSetUp;

var goBackPrevWebPage = function goBackPrevWebPage(id) {
  try {
    if (window.JBridge.goBackPrevWebPage) {
      return window.JBridge.goBackPrevWebPage(id);
    }
  } catch (err) {
    console.log("goBackPrevWebPage error " + err);
  }
};

exports.goBackPrevWebPage = goBackPrevWebPage;

var storeOnResumeCallback = function storeOnResumeCallback(cb) {
  return function (action) {
    return function () {
      try {
        var callback = _prestoUi.callbackMapper.map(function () {
          cb(action)();
        });

        window.JBridge.storeOnResumeCallback(callback);
      } catch (error) {
        console.log("Error occurred in storeOnResumeCallback ------", error);
      }
    };
  };
};

exports.storeOnResumeCallback = storeOnResumeCallback;

var logEvent = function logEvent(string) {
  return function () {
    if (!window.timeFunc[string]) {
      console.log("log the " + string);
      window.timeFunc[string] = Date.now();
    }

    console.log("timestamp " + string + " : " + Date.now());
  };
};

exports.logEvent = logEvent;