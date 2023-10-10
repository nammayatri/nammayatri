import { callbackMapper } from 'presto-ui';

const { JOS, JBridge } = window;
const hindiStrings = require("./../../src/Strings/HI.js");
const kannadaStrings = require("./../../src/Strings/KN.js");
const englishStrings = require("./../../src/Strings/EN.js");
const bengaliStrings = require("./../../src/Strings/BN.js");
const malayalamStrings = require("./../../src/Strings/ML.js");
const tamilStrings = require("./../../src/Strings/TA.js");
const frenchStrings = require("./../../src/Strings/FR.js");

var timerIdDebounce;
var inputForDebounce;
var timerIdForTimeout;
var allTimerIID = [];
var uniqueId = 0;

export const generateUniqueId = function (unit) {
  uniqueId += 1;
  return JSON.stringify(uniqueId);
};

export const getOs = function () {
  if (window.__OS) {
    return window.__OS;
  }
  return "ANDROID";
};

export const getTimer = function (valType) {
  return function (startTime) {
  return function (inputVal) {
    return function (cb){
      return function (action) {
          return function(){
              var callback = callbackMapper.map(function () {

                console.log(valType+"<===>"+startTime+"<===>"+inputVal)

                var t = new Date().getTime()

                console.log("ttt-->>"+t)
                var countDownDate = (new Date(inputVal).getTime())-5000;

                console.log("countDownDate-->>"+countDownDate)

                if((Math.floor(((countDownDate - t) % (1000 * 60)) / 1000)) > 25000)
                {
                  countDownDate = (new Date().getTime())+ 20000;
                }

                instantGetTimer (function() {

                  // Get today's date and time
                  var now = new Date().getTime();

                  // Find the distance between now and the count down date
                  var distance = countDownDate - now;

                  // Time calculations for days, hours, minutes and seconds
                  var days = Math.floor(distance / (1000 * 60 * 60 * 24));
                  var hours = Math.floor((distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
                  var minutes = Math.floor((distance % (1000 * 60 * 60)) / (1000 * 60));
                  var seconds = Math.floor((distance % (1000 * 60)) / 1000);

                  // Display the result in the element with id="demo"
                  // document.getElementById("demo").innerHTML = days + "d " + hours + "h "
                  // + minutes + "m " + seconds + "s ";
                  // If the count down is finished, write some text
                 // console.log("Distance -->>")
                 // console.log(distance)
                 // console.log("-------------")
                  if (valType == "STOP"){
                    clearInterval(window.timerId);
                  }

                  if (distance <= 0) {
                    clearInterval(window.timerId);

                    console.log("EXPIRED");
                    cb(action("EXPIRED"))();
                    // return "EXPIRED";
                  }else{
                    if(days==0){
                      if(hours == 0){
                        if(minutes==0){
                          var timer = (seconds+1) + "s "
                        }else{
                          var timer = minutes + "m " + seconds + "s "
                        }
                      }else{
                        var timer = hours + "h "+ minutes + "m " + seconds + "s "
                      }
                    }else{
                      var timer = days + "d " + hours + "h "+ minutes + "m " + seconds + "s "
                    }

                  console.log(timer);
                  // return timer;
                  cb(action(timer))();
                }
              }, 1000);
              console.log("timerId",window.timerId);
            });
            // return JBridge.timerCallback(callback);
            window.callUICallback(callback);
        }

      }
    }
  }
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
                //clearInterval(window.timerId);
                cb(action(0)(id)("EXPIRED")(timerIID))();
              } else {
                cb(action(countDown)(id)("INPROGRESS")(timerIID))();
              }
            }, 1000);
            // let timerId = setInterval(function () {
            //     countDown -= 1;
            //     if (countDown < 0) {
            //       cb(action(0)(id)("EXPIRED"))();
            //     } else {
            //       cb(action(countDown)(id)("INPROGRESS"))();
            //     }
            //   }, 1000);
            // setTimeout(() => { clearInterval(timerId); }, countDownTime*1000 + 1000);
          });
          window.callUICallback(callback);
        }
      }
    }
  }
}


export const clampNumber = function (number) {
  return function(originalMax) {
    return function(newMax) {
      const originalMin = 0;
      const newMin = 0;
      const originalRange = originalMax - originalMin;
      const newRange = newMax - newMin;
      
      const percentage = (number - originalMin) / originalRange;
      
      return Math.floor(newMin + percentage * newRange);
    }
  }
}

export const get15sTimer = function (cb){
      return function (action) {
          return function(){
              var callback = callbackMapper.map(function () {
                var seconds = 15;
                instantGetTimer (function() {
                  seconds = seconds - 1;
                  if (seconds <= 0) {
                    clearInterval(window.timerId);
                    console.log("EXPIRED");
                    cb(action("EXPIRED"))();
                  }else{
                    var timer = seconds + "s "
                    console.log(timer);
                    cb(action(timer))();
                }
              }, 1000);
              console.log("timerId",window.timerId);
            });
            window.callUICallback(callback);
        }

      }
    }

export const get5sTimer = function (cb){
      return function (action) {
          return function(){
              var callback = callbackMapper.map(function () {
                var time = 5;
                sayLetter(time);

                function sayLetter(seconds){
                 if (seconds >= 0)
                {
                     console.log(seconds+ "seconds")
                 setTimeout(() => {
                      if (seconds <= 0) {
                       clearInterval(window.timerId);
                      console.log("EXPIRED");
                         cb(action("EXPIRED"))();
                     }else{
                         var timer = seconds + "s "
                       console.log(timer);
                         cb(action(timer))();
                         sayLetter(seconds-1);
                       }
                     }, 1000);
                }
                }
                console.log("timerId",window.timerId);
            });
            window.callUICallback(callback);
        }
      }
    }

export const parseNumber = function (num) {
    num = num.toString();
    var lastThree = num.substring(num.length-3);
    var otherNumbers = num.substring(0,num.length-3);
    if(otherNumbers != '')
        lastThree = ',' + lastThree;
    var res = otherNumbers.replace(/\B(?=(\d{2})+(?!\d))/g, ",") + lastThree;
    return res;
}


export const get10sTimer = function (cb){
  return function (action) {
      return function(){
          var callback = callbackMapper.map(function () {
            var time = 10;
            sayLetter(time);

            function sayLetter(seconds){
             if (seconds >= 0)
            {
                console.log(seconds+ "seconds")
                setTimeout(() => {
                  if (seconds <= 0) {
                   clearInterval(window.timerId);
                  console.log("EXPIRED");
                     cb(action("EXPIRED"))();
                 }else{
                     var timer = seconds + "s "
                   console.log(timer);
                     cb(action(timer))();
                     sayLetter(seconds-1);
                   }
                 }, 1000);
            }
            }
            console.log("timerId",window.timerId);
        });
        window.callUICallback(callback);
    }
  }
}

export const startTimer = function(input) {
  return function (isCountDown) {
    return function (cb) {
      return function (action) {
        return function () {
          var callback = callbackMapper.map(function () {
            var time = input;
            time = time + (isCountDown ? -1 : 1);
            startCountDown(time);
            console.log("inside startTimer");
            function startCountDown(seconds) {
              if (seconds >= 0) {
                timerIdForTimeout = setTimeout(() => {
                  if (seconds <= 0) {
                    clearTimeout(timerIdForTimeout);
                    console.log("EXPIRED");
                    console.log(seconds + "seconds");
                    cb(action("EXPIRED"))();
                  } else {
                    var timer = seconds + "s ";
                    console.log(timer + "seconds");
                    cb(action(timer))();
                    startCountDown(seconds + (isCountDown ? -1 : 1));
                  }
                }, 1000);
              }
            }
            console.log("timerId : " + timerIdForTimeout);
          });
          window.callUICallback(callback);
        }
      }
    }
  }
}


export const clearTimer = function (a)
{ if(timerIdForTimeout){
    clearTimeout(timerIdForTimeout);
  }
  if(window.timerId){
    clearInterval(window.timerId);
  }
};

export const clearPopUpTimer = function (a) {
  clearInterval(parseInt(a));
};

function instantGetTimer (fn , delay) {
  fn();
  window.timerId = setInterval( fn, delay );
  allTimerIID.push(window.timerId);
  return window.timerId;
}

export const clearAllTimer = function(a) {
  console.log("allTimerIID");
  console.log(allTimerIID);
  while(allTimerIID.length > 0){
    clearInterval(parseInt(allTimerIID.pop()));
  }
}
export const setRefreshing = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setRefreshing:b_" + bool + ";"
      Android.runInUI(cmd,null)
    }
  }
}

export const setEnabled = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setEnabled:b_" + bool + ";"
      Android.runInUI(cmd,null)
    }
  }
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

export const convertKmToM = function (dist) {
  try{
    var distance = parseInt(dist);
    if (distance<1000)
    {
      return ((distance.toString()) + "m");
    }
    else
    {
      var disKm = distance/1000;
      return (((disKm.toFixed(1)).toString()) + "km");
    }
  }catch(e){
    console.log(e);
    console.log("error in convertKmToM----------------------------------");
  }
};

export const differenceBetweenTwoUTC = function (str1) {
  return function (str2) {
    var curr1 = new Date(str1);
    var departure = new Date(str2);
    console.log(departure + " , " + curr1 + "STR");
    var diff =(curr1.getTime() - departure.getTime())/ 1000;
    diff = (Math.round(diff));
    return diff
  };
};


export const storeCallBackForNotification = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (notificationType) {
            cb(action(notificationType))();
          });
          window.onResumeListeners = [];
          JBridge.storeCallBackForNotification(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackForNotification ------", error);
  }
}

export const getcurrentdate = function (string) {
  var today = new Date();
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = today.getFullYear();

  today = yyyy + '-' + mm + '-' + dd;
  return today;
}

export const getDatebyCount = function (count) {
  var today = new Date();
  today.setDate(today.getDate() - count);
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = today.getFullYear();

  today = yyyy + '-' + mm + '-' + dd;
  return today;
}

export const hideSplash = JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"hide_splash"}))()

export const currentPosition = function (str) {
  return function() {
    JBridge.currentPosition(str);
  }
}

export const toString = function (attr) {
  return JSON.stringify(attr);
};

export const getTime = function (unit){
  return Date.now();
}

export const toInt = function (val) {
  return parseInt(val);
}

export const secondsLeft = function (time){
  var validity = new Date(time).getTime();
  var now = new Date().getTime();
  if (validity <= now){
    return parseInt(1);
  }else{
    return parseInt((validity - now)/1000);
  }
}

export const objectToAllocationType = function (stringifyObject) {
  var json = JSON.parse(stringifyObject);
  console.log(json);
  return json;
}

export const storeCallBackTime = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (time, lat, lng) {
              cb(action(time)(lat)(lng))();
          });
          JBridge.storeCallBackTime(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackTime ------", error);
  }
}

export const launchAppSettings = function (unit) {
  return JBridge.launchAppSettings();
};

export const shuffle = function (array) {
  var shuffled = array
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
  return shuffled
}

export const setYoutubePlayer = function (json) {
  return function (viewId) {
    return function (videoStatus) {
        if (JBridge.setYoutubePlayer) {
          try {
            console.log("Inside setYoutubePlayer ------------");
            return JBridge.setYoutubePlayer(JSON.stringify(json), viewId, videoStatus);
          } catch (err) {
            console.log("error in setYoutubePlayer");
          }
        }
    };
  };
};

export const getTimeStampString = function (utcTime){
  var createdDate = new Date(utcTime);
  var currentDate = new Date();
  var diff = (currentDate.getTime() - createdDate.getTime())/ 1000;
  var seconds = (Math.round(diff));
  if (seconds <0) return "";
  var d = Math.floor(seconds / (3600*24));
  var h = Math.floor(seconds % (3600*24) / 3600);
  var m = Math.floor(seconds % 3600 / 60);
  var s = Math.floor(seconds % 60);
  if      (d > 0) return d + (d == 1 ? " day " : " days")
  else if (h > 0) return h + (h == 1 ? " hour " : " hours")
  else if (m > 0) return m + (m == 1 ? " minute " : " minutes")
  else            return s + (s == 1 ? " second" : " seconds")
}

export const clearFocus = function (id) {
    return JBridge.clearFocus(id)
}

export const addMediaPlayer = function (id) {
  return function(source) {
    return function () {
      JBridge.addMediaPlayer(id,source);
    }
  };
};

export const saveAudioFile = function (source) {
    return function() {
        return JBridge.saveAudioFile(source);
    }
}

export const uploadMultiPartData = function (path) {
    return function (url) {
        return function(fileType) {
            return function() {
                return JBridge.uploadMultiPartData(path, url, fileType);
            }
        }
    }
}

export const startAudioRecording = function (id) {
    return function() {
        return JBridge.startAudioRecording();
    }
};

export const stopAudioRecording = function (id) {
    return function() {
        return JBridge.stopAudioRecording();
    }
}

export const renderBase64ImageFile = function (base64Image) {
    return function(id) {
        return function (fitCenter) {
          return function (imgScaleType){
            return function () {
              try{
                return JBridge.renderBase64ImageFile(base64Image, id, fitCenter, imgScaleType);
              }catch (err){
                return JBridge.renderBase64ImageFile(base64Image, id, fitCenter);
              }
            }
          }  
        }
    }
}

export const removeMediaPlayer = function (id) {
  return function () {
    JBridge.removeMediaPlayer();
  };
};

export const getVideoID = function (url) {
  try {
    var ID = '';
    var updatedURL = url.replace(/(>|<)/gi, '').split(/(vi\/|v=|\/v\/|youtu\.be\/|\/embed\/|\/shorts\/)/);
    if (updatedURL[2] !== undefined) {
      ID = updatedURL[2].split(/[^0-9a-z_\-]/i);
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
    let videoId = getVideoID(url);
    return ("https://img.youtube.com/vi/" + videoId + "/maxresdefault.jpg");
  }catch (e) {
    console.log("error in getImageUrl " + e);
  }
};

export const getRideLabelConfig = function (just, nothing, key, zoneType){
  const rideLabel = rideLabelConfig() 
  if (zoneType in rideLabel ){
    var zoneOb = rideLabel[zoneType];
    if(key in zoneOb){
      var prop = zoneOb[key]
      return just(prop);
    }
    console.error("No value found for key "+ key);
  }
  console.error("No value found for key "+ zoneType);
  return nothing;
}

function rideLabelConfig(){
  return  {
    SureMetro_Pickup : {
      "backgroundColor" : "#2194FF",
      "text" : "Metro Pickup",
      "secondaryText" : "",
      "imageUrl" : "ic_metro_white,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_metro_white.png",
      "cancelText" : "ZONE_CANCEL_TEXT_PICKUP",
      "cancelConfirmImage" : "ic_cancelride_metro_pickup,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_cancelride_metro_pickup.png"
    },
    SureMetro_Drop : {
      "backgroundColor" : "#2194FF",
      "text" : "Metro Drop",
      "secondaryText" : "",
      "imageUrl" : "ic_metro_white,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_metro_white.png",
      "cancelText" : "ZONE_CANCEL_TEXT_DROP",
      "cancelConfirmImage" : "ic_cancelride_metro_drop,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_cancelride_metro_drop.png"
    },
    Accessibility : {
      "backgroundColor" : "#9747FF",
      "text" : getStringFromCommon("ASSISTANCE_REQUIRED"),
      "secondaryText" : getStringFromCommon("LEARN_MORE"), //"Learn More",
      "imageUrl" : "ny_ic_wheelchair,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_wheelchair.png",
      "cancelText" : "FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES",
      "cancelConfirmImage" : "ic_cancelride_metro_drop,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_cancelride_metro_drop.png"
    },
    Purple_Ride : {
      "backgroundColor" : "#9747FF",
      "text" : getStringFromCommon("PURPLE_RIDE"),
      "secondaryText" : "", //"Learn More",
      "imageUrl" : "ny_ic_wheelchair,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_wheelchair.png",
      "cancelText" : "FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES",
      "cancelConfirmImage" : "ic_cancelride_metro_drop,https://assets.juspay.in/beckn/nammayatri/driver/images/ic_cancelride_metro_drop.png"
    }
    //More rideLabelConfigs can be added
  }
}

export const getPeriod = function(date) {
  var currentDate = new Date();
  var pastDate = new Date(date);
  var diff = Math.floor(currentDate.getTime() - pastDate.getTime());
  var days = Math.floor(diff/(1000 * 60 * 60 * 24));
  var months = Math.floor(days/31);
  var years = Math.floor(months/12);
  var period = years > 0 ? years : months > 0 ? months : days > 0 ? days : 0
  var periodType = years > 0 ? "Yrs" : months > 0 ? "Months" : days > 0 ? "Days" : "new"
  return {period : Math.abs(period)
  , periodType : periodType}
  }

export const getMerchantId = function(id) {
  return window.merchantID;
}

export const startPP = function (payload) {
	return function (sc) {
		return function () {
			var cb = function (code) {
				return function (_response) {
					return function () {
            var response = JSON.parse(_response);
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

// TODO NEED TO REFACTOR 
export const renewFile = function(filePath,location,cb) {
  JBridge.renewFile(location,filePath,callbackMapper.map(function(result) {
    cb(result)();
  }));
}

export const preFetch = function() {
  let configPackage = {};
  try {
    if (top.configPackage) configPackage = Object.assign({},top.configPackage)
    else configPackage = JSON.parse(JBridge.loadFileInDUI("config.json"));
  }catch (err){
    window.JBridge.firebaseLogEventWithParams("pre_fetch_failed", "config_read_failed",err);
    }

  let JOSFlags = JOS.getJOSflags();
  let assets = {};
  let bundles = {};
  if (JOSFlags && JOSFlags.isCUGUser) {
    assets = Object.assign(assets,configPackage.new.assets)
    bundles = Object.assign(bundles,configPackage.new.package)
  } else {
    assets = Object.assign(assets,configPackage.live.assets)
    bundles = Object.assign(bundles,configPackage.live.package)
  }
  return getDownloadObject(assets,bundles,configPackage.app_list,configPackage.dependencies);
}

function getDownloadObject(assets,bundles,apps,dependencies) {
  let files = [];
  apps.forEach((app) => {

    if (!bundles[app]) return;
    let assetsBlock = assets[app];
    let root = getRoot(dependencies[app]);
    let assetsKey = Object.keys(assetsBlock);
    let assetsList = [];



    assetsKey.forEach((key) => {if (typeof assetsBlock[key] == "string") assetsList.push(assetsBlock[key])})

    files.push(constructDownloadObject(root,bundles[app]));

    assetsList.forEach((url) => {
      files.push(constructDownloadObject(root,url));
    });
  });
  return files;
}

function constructDownloadObject (root,url) {
  let download = {};
  Object.assign(download,({
    "filePath" : root.concat(getFileName(url)),
    "location" : url
  }));
  return download;
}

function getRoot(rootObject) {
  return rootObject ? rootObject.default.root : "";
}

function getFileName(url) {
  const reg = /(v1-[^/]*\.((zip)|(jsa)))|[^/]*\.(html|js)/;
  const out = reg.exec(url);
  return out ? out[0] : fileurl;
}

export const initiatePP = function () {
  var cb = function (code) {
    return function (_response) {
      return function () {
        var response = JSON.parse(_response);
        console.log("%cHyperpay Terminate Response ", "background:darkblue;color:white;font-size:13px;padding:2px", code, response);
      }
    }
  }
  if (JOS) {
    try {
      var innerPayload = Object.assign({},window.__payload.payload);
      var initiatePayload = Object.assign({},window.__payload);
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
export const getAvailableUpiApps = function (resultCb) {
  var cb = function (code) {
    return function (_response) {
      return function () {
        console.log("%cUPIINTENT Terminate Response ", "background:darkblue;color:white;font-size:13px;padding:2px", code, _response);
      }
    }
  }
  if (JOS) {
    try {
      var result = function (code) {
        return function (_response) {
          return function () {
            console.log("%cUPIINTENT initiate Result ", "background:darkblue;color:white;font-size:13px;padding:2px", _response);
            try {
              let resultPayload = JSON.parse(_response)
              resultCb(resultPayload.payload.response.available_apps)();
              killPP(["in.juspay.upiintent"]);
            } catch (err) {
              console.log("%cUPIINTENT initiate Result error", "background:darkblue;color:white;font-size:13px;padding:2px", err, code);
            }
          }
        }
      }
      var payload = {
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
      let process = function() {
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
  var jpConsumingBackpress = {
    event: "jp_consuming_backpress",
    payload: { jp_consuming_backpress: true }
  }
  JBridge.runInJuspayBrowser("onEvent", JSON.stringify(jpConsumingBackpress), "");
}

export const isYesterday = function (dateString){
  try {
    const yesterday = new Date()
    yesterday.setDate(yesterday.getDate() - 1);
    var date = new Date(dateString);
    return (yesterday.toDateString() == date.toDateString());
  }catch(error){
    console.error(error);
  }
  return false;
}

export const isDateGreaterThan = function(dateString){
  try {
    const currDate = new Date();
    var futureDate = new Date(dateString);
    return currDate > futureDate;
  }catch(error){
    console.error("error", error);
  }
  return false;
}

export const getPopupObject = function (just, nothing, key){
  try {
    var val = JBridge.getFromSharedPrefs(key);
    if (val == "__failed") {
      return nothing;
    } 
    console.log("zxc console key ", val);
    console.log("zxc console parsed key ", JSON.parse(val));
    return just(JSON.parse(val));
  } catch( e ){
    console.warn(e);
  }
  return nothing;
}

export const checkPPInitiateStatus = function (cb,services = microapps) {
  if (window.isPPInitiated && checkPPLoadStatus(services)) {
    cb()();
  } else {
    waitTillSeviceLoad(cb,services,checkPPInitiateStatus);
  }
}

export const  killPP = function (services) {
    services.forEach((key) => {
      if (key != JOS.self) {
        var currJOS = top.JOSHolder[key];
        try{currJOS.finish(1)(JSON.stringify({result:"success"}))();}
        catch (err){
          console.log(err);
        }
        delete top.JOSHolder[key];
      }
    });
}

export const getDateAfterNDays = function (n) {
  const today = new Date();
  const dateAfterNDays = new Date(today);
  dateAfterNDays.setDate(today.getDate() + n);
  const year = dateAfterNDays.getFullYear();
  const month = String(dateAfterNDays.getMonth() + 1).padStart(2, '0');
  const day = String(dateAfterNDays.getDate()).padStart(2, '0');
  const result = `${day}/${month}/${year}`;
  return `${day}/${month}/${year}`;
}

function getStringFromCommon(key) {
  var selectedLanguage = JBridge.getKeysInSharedPref("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return hindiStrings.getStringValue(key);
    case "KN_IN":
      return kannadaStrings.getStringValue(key);
    case "BN_IN":
      return bengaliStrings.getStringValue(key);
    case "ML_IN":
      return malayalamStrings.getStringValue(key);
    case "TA_IN":
      return tamilStrings.getStringValue(key);
    case "FR_FR":
      return frenchStrings.getStringValue(key);
    default:
      return englishStrings.getStringValue(key);
  }
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

function waitTillSeviceLoad (cb,serives,statusChecker) {
  let checkPP = function () {
    console.log("waitTillSeviceLoad");
    statusChecker(cb,serives);
  }
  setTimeout(checkPP,10);
}

function callUpiProcess(process,service) {
  if (checkPPLoadStatus(service)) {
    process();
  } else {
    waitTillSeviceLoad(process,service,callUpiProcess);
  }
}

let microapps = ["in.juspay.hyperpay", "in.juspay.ec", "in.juspay.upiintent"];


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
    console.warn(e);
    sc("FAILURE")();
  }
}

export const downloadQR = function (id){
  if (window.JBridge.downloadLayoutAsImage)
    return window.JBridge.downloadLayoutAsImage(id);
}
