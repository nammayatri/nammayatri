
const callbackMapper = require('presto-ui').callbackMapper;
const moment = require("moment");

var timerIdDebounce = null;
var driverWaitingTimerId = null;
var zoneOtpExpiryTimerId = null;
var inputForDebounce;
var timerIdForTimeout;
var tracking_id = 0;

exports.getNewTrackingId = function (unit) {
  tracking_id += 1;
  return JSON.stringify(tracking_id);
};

exports.getKeyInSharedPrefKeysConfigEff = function (key) {
    return JBridge.getKeysInSharedPrefs(key);
  };

exports["validateInputPattern"] = function (input, pattern){
    const reg = new RegExp(pattern,'g');
    var result = reg.test(input);
    console.log("validateInputPattern " + result + " Values :- " + input + " Pattern :- " + pattern);
    return (result);
}

exports["getLocationName"] = function(cb){
    return function (lat) {
        return function (lng){
            return function (defaultText) {
                return function (action) {
                    return function(){
                        var callback = callbackMapper.map(function (lat,lon,result){
                            var decodedString = decodeURIComponent(result).replace(/\+/g, ' ');
                            cb(action(lat)(lon)(decodedString))();
                        });
                        return JBridge.getLocationName(lat, lng, defaultText, callback);
                    }
                }
            }
        }
    }
}
exports.hideSplash =JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"hide_splash"}))()


exports ["getCurrentDate"] = function (string) {
  var today = new Date();
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = today.getFullYear();

  today = dd + '/' + mm + '/' + yyyy;
  return today;
}

exports["validateEmail"] = function (email){
    const re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
    return re.test(String(email).toLowerCase());
}

exports["addTimeToDate"] = function(date){
    return function(num){
        return function(timeunit){
            return moment(date).add(num,timeunit).toDate();
        }
    }
}

exports["factoryResetApp"] = function (str) {
    console.log("HERE IN RESET ===--->>")
    JBridge.factoryResetApp()
}

exports["dateCompare"] = function (currentDate) {
    return function (validDate) {
        console.log (moment(currentDate).isBefore(validDate), currentDate, validDate);
        if(!moment(currentDate).isBefore(validDate)){
            JBridge.toast("Invalid Date/Time");
        }
        return moment(currentDate).isBefore(validDate);
    }
}

exports["secondsToHms"] =  function (d) {
    d = Number(d);
    var h = Math.floor(d / 3600);
    var m = Math.floor(d % 3600 / 60);
    // var s = Math.floor(d % 3600 % 60);

    var hDisplay = h > 0 ? h + (h == 1 ? " hr, " : " hrs, ") : "";
    var mDisplay = m > 0 ? m + (m == 1 ? " min " : " mins ") : "";
    // var sDisplay = s > 0 ? s + (s == 1 ? " second" : " seconds") : "";
    return hDisplay + mDisplay; //+ sDisplay;
}

exports["getUTCDay"] = function (date){
    return date.getUTCDay();
}

exports["getTime"] = function (unit){
    return Date.now();
}

exports.requestKeyboardShow = function(id) {
    return function() {
        var delayInMilliseconds = 100;
        setTimeout(function() {
            JBridge.requestKeyboardShow(id);
        }, delayInMilliseconds);
    }
  }


exports ["setText'"] = function (id) {
    return function (text) {
        return function (){
            setText(id, text, text.length);
        }
    }
}

exports["storeCallBackLocateOnMap"] = function (cb) {
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
          JBridge.storeCallBackLocateOnMap(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackLocateOnMap ------", error);
  }
}

exports["storeCallBackCustomer"] = function (cb) {
    try {
    return function (action) {
        return function () {
            var callback = callbackMapper.map(function (notificationType) {
                cb(action (notificationType))();
            });
            console.log("In storeCallBackCustomer ---------- + " + action);
            JBridge.storeCallBackCustomer(callback);
        }
    }}
    catch (error){
        console.log("Error occurred in storeCallBackCustomer ------", error);
    }
}

exports["storeCallBackContacts"] = function (cb) {
  return function (action) {
    return function () {
      try {
        var callback = callbackMapper.map(function (contact) {
          json = JSON.parse(contact);
          console.log("storeCallBackContacts js " + json);
          cb(action(json))();
        });

        console.log("In storeCallBackContacts ---------- + " + action);
        JBridge.storeCallBackContacts(callback);
      } catch (err) {
        console.log("storeCallBackContacts error " + err);
      }
    }
  }
}

exports["parseNewContacts"] = function (String) {
    return JSON.parse(String);
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

exports["makePascalCase"] = function (str){
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

exports["decodeErrorCode"] = function (a) {
    try {
      var errorCodee = JSON.parse(a).errorCode;
      return  errorCodee;
    } catch (e) {
      console.log(e);
      return " ";
    }
  };

exports["decodeErrorMessage"] = function (a) {
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

exports.toString = function (attr) {
return JSON.stringify(attr);
};

exports["waitingCountdownTimer"] = function (startingTime) {
  return function (cb) {
    return function (action) {
      return function () {
        if (__OS == "IOS") {
          if (JBridge.startCountUpTimer) {
            var callbackIOS = callbackMapper.map(function (timerId, sec) {
              var minutes = getTwoDigitsNumber(Math.floor(sec / 60));
              var seconds = getTwoDigitsNumber(sec - minutes * 60);
              var timeInMinutesFormat = minutes + " : " + seconds;
              cb(action(timerId)(timeInMinutesFormat)(sec))();
            });
            JBridge.startCountUpTimer(startingTime.toString(), callbackIOS);
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

exports["zoneOtpExpiryTimer"] = function (startingTime) {
  return function(endingTime) {
    return function (cb) {
      return function (action) {
        return function () {
          if (startingTime >= endingTime){
            cb(action(zoneOtpExpiryTimerId)("")(0))();
          } else {
            var callback = callbackMapper.map(function () {
              var sec = endingTime - startingTime;
              if (zoneOtpExpiryTimerId) clearInterval(zoneOtpExpiryTimerId);
              zoneOtpExpiryTimerId = setInterval(
                convertInMinutesFromat,
                1000
              );
              function convertInMinutesFromat() {
                sec--;
                var minutes = getTwoDigitsNumber(Math.floor(sec / 60));
                var seconds = getTwoDigitsNumber(sec - minutes * 60);
                var timeInMinutesFormat = minutes + " : " + seconds;
                cb(action(zoneOtpExpiryTimerId)(timeInMinutesFormat)(sec))();
              }
            });
          }
          window.callUICallback(callback);
        };
      };
    };
  }
};

exports["clearWaitingTimer"] = function (id){
  if(__OS == "IOS" && id=="countUpTimerId") {
    if (JBridge.clearCountUpTimer) {
      JBridge.clearCountUpTimer();
    }
  } else {
    clearInterval(parseInt(id));
  }
}

function getTwoDigitsNumber(number) {
  return number >= 10 ? number : "0"+number.toString();
}

exports ["setRefreshing"] = function (id){
    return function (bool){
      if (window.__OS == "ANDROID") {
        cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setRefreshing:b_" + bool + ";"
        Android.runInUI(cmd,null)
      }
    }
  }

exports ["setEnabled"] = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setEnabled:b_" + bool + ";"
      Android.runInUI(cmd,null)
    }
  }
}

exports["convertUTCtoISC"] = function (str) {
  return function (format) {
    var localTime1 = moment.utc(str).toDate();
    localTime1 = moment(localTime1).format(format);
    return localTime1;
  };
};


exports["getExpiryTime"] = function (str1) {
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


exports["getCurrentUTC"] = function (str) {
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

exports ["debounceFunction"] = function (delay) {
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


exports ["updateInputString"] = function (a){
  console.log("UPDATED STRING NOW" + a);
  inputForDebounce = a;
}

exports["fetchFromLocalStore'"] = function(key) {
    return function (just) {
        return function (nothing) {
          return function () {
            var state = JBridge.getKeysInSharedPrefs(key);
            if (state != "__failed" && state != "(null)") {
              return just(state);
            }
            return nothing;
          };
        };
      };
}

exports["fetchFromLocalStoreTemp'"] = function(key) {
  return function (just) {
      return function (nothing) {
        return function () {
          var state = JBridge.getKeysInSharedPrefs(key);
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

exports["saveToLocalStore'"] = function(key) {
    return function (state) {
        console.log("==------>>>>>> SAVE SCREEN");
        console.log(key);
        console.log(state);
        JBridge.setKeysInSharedPrefs(key, state);
        return function () {
          console.log("==------>>>>>> SAVED SCREEN");
        };
      };
}

exports["seperateByWhiteSpaces"] = function(string) {
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

exports ["shuffle"] = function (array) {
  var shuffled = array
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
  return shuffled
}

exports ["withinTimeRange"] = function (startTime) {
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

exports["parseFloat"] = function (number) {
  return function (decimalDigit) {
      return String(parseFloat(number).toFixed(decimalDigit));
  }
}

exports["adjustViewWithKeyboard"] = function(flag) {
  return function() {
    if (JBridge.adjustViewWithKeyboard) {
      JBridge.adjustViewWithKeyboard(flag)
    }
  };
};

exports["fetchAndUpdateCurrentLocation"] = function (cb) {
  return function (action) {
    return function (fallbackAction) {
      return function () {
        if (JBridge.fetchAndUpdateCurrentLocation) {
          var callback = callbackMapper.map(function (lat, lng) {
            cb(action(lat)(lng))();
          });
          return JBridge.fetchAndUpdateCurrentLocation(callback);
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
exports ["contactPermission"] = function () {
  if(JBridge.contactPermission){
    return JBridge.contactPermission();
  }
}

exports["initialWebViewSetUp"] = function (cb) {
  return function (id) {
      return function (action) {
        return function () {
          try {
            var callback = callbackMapper.map(function (val) {
              cb(action(val))();
            });

            return JBridge.initialWebViewSetUp(callback,id);
          } catch (err) {
            console.log("initialWebViewSetUp error " + err);
          }
        };
      };
  };
};

exports["goBackPrevWebPage"] = function (id) {
  try {
    if (JBridge.goBackPrevWebPage){
      return JBridge.goBackPrevWebPage(id);
    }
  } catch (err) {
    console.log("goBackPrevWebPage error " + err);
  }
}

exports["storeOnResumeCallback"] = function (cb) {
  return function (action) {
    return function () {
      try {
        var callback = callbackMapper.map(function () {
          cb(action)();
        });
        JBridge.storeOnResumeCallback(callback);
      }
      catch (error) {
        console.log("Error occurred in storeOnResumeCallback ------", error);
      }
    }
  }
}

exports["getMerchantId"] = function(id) {
  return window.merchantID;
}
