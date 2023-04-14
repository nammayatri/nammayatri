import { callbackMapper } from 'presto-ui';
import moment from 'moment';

var tracking_id = 0;
export const getNewTrackingId = function (unit) {
  tracking_id += 1;
  return JSON.stringify(tracking_id);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
    return window.JBridge.getKeysInSharedPrefs(key);
  };

export const validateInputPattern = function (input, pattern){
    const reg = new RegExp(pattern,'g');
    var result = reg.test(input);
    console.log("validateInputPattern " + result + " Values :- " + input + " Pattern :- " + pattern);
    return (result);
}

export const getLocationName = function(cb){
    return function (lat) {
        return function (lng){
            return function (defaultText) {
                return function (action) {
                    return function(){
                        var callback = callbackMapper.map(function (lat,lon,result){
                            var decodedString = decodeURIComponent(result).replace(/\+/g, ' ');
                            cb(action(lat)(lon)(decodedString))();
                        });
                        return window.JBridge.getLocationName(lat, lng, defaultText, callback);
                    }
                }
            }
        }
    }
}
export const hideSplash = window.JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"hide_splash"}))()


export const getCurrentDate = function (string) {
  var today = new Date();
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = today.getFullYear();

  today = dd + '/' + mm + '/' + yyyy;
  return today;
}

export const validateEmail = function (email){
    const re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
    return re.test(String(email).toLowerCase());
}

export const addTimeToDate = function(date){
    return function(num){
        return function(timeunit){
            return moment(date).add(num,timeunit).toDate();
        }
    }
}

export const factoryResetApp = function (str) {
    console.log("HERE IN RESET ===--->>")
    window.JBridge.factoryResetApp()
}

export const dateCompare = function (currentDate) {
    return function (validDate) {
        console.log (moment(currentDate).isBefore(validDate), currentDate, validDate);
        if(!moment(currentDate).isBefore(validDate)){
            window.JBridge.toast("Invalid Date/Time");
        }
        return moment(currentDate).isBefore(validDate);
    }
}

export const secondsToHms = function (d) {
    d = Number(d);
    var h = Math.floor(d / 3600);
    var m = Math.floor(d % 3600 / 60);
    // var s = Math.floor(d % 3600 % 60);

    var hDisplay = h > 0 ? h + (h == 1 ? " hr, " : " hrs, ") : "";
    var mDisplay = m > 0 ? m + (m == 1 ? " min " : " mins ") : "";
    // var sDisplay = s > 0 ? s + (s == 1 ? " second" : " seconds") : "";
    return hDisplay + mDisplay; //+ sDisplay;
}

export const getUTCDay = function (date){
    return date.getUTCDay();
}

export const getTime = function (unit){
    return Date.now();
}

export const requestKeyboardShow = function(id) {
    return function() {
        var delayInMilliseconds = 100;
        setTimeout(function() {
            window.JBridge.requestKeyboardShow(id);
        }, delayInMilliseconds);
    }
  }

function setText(id, text, pos) {
        if (__OS === "ANDROID") {
            var cmd = "set_view=ctx->findViewById:i_" + id + ";";
            cmd += "get_view->setText:cs_" + text + ";";
            cmd += "get_view->setSelection:i_" + pos + ";";
            window.Android.runInUI(cmd, null);
        } else {
            window.Android.runInUI({id: id, text: text});
            window.Android.runInUI({id: id, cursorPosition: pos});
        }
    }

export const setTextImpl = function (id) {
    return function (text) {
        return function (){
            setText(id, text, text.length);
        }
    }
}

export const storeCallBackLocateOnMap = function (cb) {
  try {
  return function (action) {
      return function () {
        var callback = callbackMapper.map(function (key, lat, lon) {
          console.log("in show storeCallBackLocateOnMap",action);
          window.x = cb;
          window.y = action;
          cb(action (key) (lat) (lon))();
        });
          console.log("In storeCallBackLocateOnMap ---------- + " + action);
          window.JBridge.storeCallBackLocateOnMap(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackLocateOnMap ------", error);
  }
}

export const storeCallBackCustomer = function (cb) {
    try {
    return function (action) {
        return function () {
            var callback = callbackMapper.map(function (notificationType) {
                cb(action (notificationType))();
            });
            console.log("In storeCallBackCustomer ---------- + " + action);
            window.JBridge.storeCallBackCustomer(callback);
        }
    }}
    catch (error){
        console.log("Error occurred in storeCallBackCustomer ------", error);
    }
}

export const storeCallBackContacts = function (cb) {
  return function (action) {
    return function () {
      try {
        var callback = callbackMapper.map(function (contact) {
          json = JSON.parse(contact);
          console.log("storeCallBackContacts js " + json);
          cb(action(json))();
        });

        console.log("In storeCallBackContacts ---------- + " + action);
        window.JBridge.storeCallBackContacts(callback);
      } catch (err) {
        console.log("storeCallBackContacts error " + err);
      }
    }
  }
}

export parseNewContacts = function (String) {
    return JSON.parse(String);
}


export const makePascalCase = function (str){
    var changeToUpperCase = str[0].toUpperCase();
    for(var i = 1; i < str.length; i++){
        if(str[i-1] == " " || str[i-1] == ","){
            changeToUpperCase += str[i].toUpperCase();
        }else{
            changeToUpperCase += str[i].toLowerCase();
        }
    }
    return changeToUpperCase;
}

export const decodeErrorCode = function (a) {
    try {
      var errorCodee = JSON.parse(a).errorCode;
      return  errorCodee;
    } catch (e) {
      console.log(e);
      return " ";
    }
  };

export const decodeErrorMessage = function (a) {
try {
    var errorMessagee = JSON.parse(a).errorMessage;
    if(errorMessagee == null)
    {
    return "";
    }
    return  errorMessagee;
} catch (e) {
    console.log(e);
    return " ";
}
};

export const toString = function (attr) {
return JSON.stringify(attr);
};

var driverWaitingTimerId = null;
export const waitingCountdownTimer = function (startingTime) {
  return function (cb) {
    return function (action) {
      return function () {
        if (__OS == "IOS") {
          if (window.JBridge.startCountUpTimer) {
            var callbackIOS = callbackMapper.map(function (timerId, sec) {
              var minutes = getTwoDigitsNumber(Math.floor(sec / 60));
              var seconds = getTwoDigitsNumber(sec - minutes * 60);
              var timeInMinutesFormat = minutes + " : " + seconds;
              cb(action(timerId)(timeInMinutesFormat)(sec))();
            });
            window.JBridge.startCountUpTimer(startingTime.toString(), callbackIOS);
          }
        } else {
          var callback = callbackMapper.map(function () {
            var sec = startingTime;
            if (driverWaitingTimerId) clearInterval(driverWaitingTimerId);
            driverWaitingTimerId = setInterval(
              convertInMinutesFromat,
              1000
            );
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

export clearWaitingTimer = function (id){
  if(__OS == "IOS" && id=="countUpTimerId") {
    if (window.JBridge.clearCountUpTimer) {
      window.JBridge.clearCountUpTimer();
    }
  } else {
    clearInterval(parseInt(id));
  }
}

function getTwoDigitsNumber(number) {
  return number >= 10 ? number : "0"+number.toString();
}

export const setRefreshing = function (id){
    return function (bool){
      if (window.__OS == "ANDROID") {
        var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setRefreshing:b_" + bool + ";"
        window.Android.runInUI(cmd,null)
      }
    }
  }

export const setEnabled = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setEnabled:b_" + bool + ";"
      window.Android.runInUI(cmd,null)
    }
  }
}

export const convertUTCtoISC = function (str) {
  return function (format) {
    var localTime1 = moment.utc(str).toDate();
    localTime1 = moment(localTime1).format(format);
    return localTime1;
  };
};


export const getExpiryTime = function (str1) {
  return function (str2){
    return function (forLostAndFound) {
      var expiry = new Date(str1);
      var d = new Date();
      var result =  moment(d).utc().format();
      var current = new Date(result);
      console.log(current + " , " + expiry + "STR");
      var diff = (expiry.getTime() - current.getTime())/ 1000;
      if (forLostAndFound)
        {
          diff = (current.getTime() - expiry.getTime())/ 1000;
        }
      diff = (Math.round(diff));
      console.log("STR ->>>> expiry time "+diff);
      if (diff >= 0)
          return (diff);
        else
          return 0;
    };
  };
};


export const getCurrentUTC = function (str) {
  var d = new Date();
  var result =  moment(d).utc().format();
  console.log(result);
  return result;
};

  // exports ["debounceFunction"] = function (delay) { NEED TO HANDLE DEBOUNCING IN LOCATEONMAP
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

var inputForDebounce;
export const updateInputString = function (a){
  console.log("UPDATED STRING NOW" + a);
  inputForDebounce = a;
}

var timerIdDebounce = null;
export const debounceFunction = function (delay) {
  return function (cb){
    return function (action){
      return function(){
        console.log("CALLED :- ");
        var callback = callbackMapper.map(function () {
          if (timerIdDebounce) clearTimeout(timerIdDebounce);
          timerIdDebounce = setTimeout(() => {
            timerIdDebounce = "MAKEAPICALL";
            cb(action (inputForDebounce))();
          },delay);
        });
        window.callUICallback(callback);
      }
    }
  }
}

export const fetchFromLocalStoreImpl = function(key) {
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
}

export const fetchFromLocalStoreTempImpl = function(key) {
  return function (just) {
      return function (nothing) {
        return function () {
          var state = window.JBridge.getKeysInSharedPrefs(key);
          var newState = JSON.parse(state);
          var predictionArray = newState.predictionArray;
          try {
                for(var i = 0; i < predictionArray.length; i++) {
                  if (!predictionArray[i].hasOwnProperty("fullAddress"))
                    {
                      predictionArray[i].fullAddress = {};
                    }
                }
            }
          catch(e) {
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

export const saveToLocalStoreImpl = function(key) {
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

export const seperateByWhiteSpaces = function(string) {
    return string.replace(/\s+/g, ' ').trim();
};

// function uuidv4() {
//   return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
//     (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
//   );
// }

// exports["generateSessionToken"] = function (str){
//     var token = uuidv4();
//     return token;
// }

export const shuffle = function (array) {
  var shuffled = array
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
  return shuffled
}

export const withinTimeRange = function (startTime) {
  return function (endTime) {
    try {
      var currentTimeString = moment(new Date()).format("HH:mm:ss");
      return startTime < endTime ? between(currentTimeString, startTime, endTime) : between(currentTimeString, startTime, "23:59:59") || between(currentTimeString, "00:00:01", endTime);
    }catch (err){
      return false;
    }
  }
}
function between(x, min, max) {
  return x >= min && x <= max;
}

export const adjustViewWithKeyboard = function(flag) {
  return function() {
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
          var callback = callbackMapper.map(function (lat, lng) {
            cb(action(lat)(lng))();
          });
          return window.JBridge.fetchAndUpdateCurrentLocation(callback);
        } else {  // fallback for previous release
          var fallBackCallback = callbackMapper.map(function(){
            cb(fallbackAction)();
          });
          window.callUICallback(fallBackCallback);
        }
      };
    };
  };
};
export const contactPermission = function () {
  if(window.JBridge.contactPermission){
    return window.JBridge.contactPermission();
  }
}

export const initialWebViewSetUp = function (cb) {
  return function (id) {
      return function (action) {
        return function () {
          try {
            var callback = callbackMapper.map(function (val) {
              cb(action(val))();
            });

            return window.JBridge.initialWebViewSetUp(callback,id);
          } catch (err) {
            console.log("initialWebViewSetUp error " + err);
          }
        };
      };
  };
};

export const goBackPrevWebPage = function (id) {
  try {
    if (window.JBridge.goBackPrevWebPage){
      return window.JBridge.goBackPrevWebPage(id);
    }
  } catch (err) {
    console.log("goBackPrevWebPage error " + err);
  }
}

export const storeOnResumeCallback = function (cb) {
  return function (action) {
    return function () {
      try {
        var callback = callbackMapper.map(function () {
          cb(action)();
        });
        window.JBridge.storeOnResumeCallback(callback);
      }
      catch (error) {
        console.log("Error occurred in storeOnResumeCallback ------", error);
      }
    }
  }
}
