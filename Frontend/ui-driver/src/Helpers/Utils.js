const callbackMapper = require("presto-ui").callbackMapper;
const moment = require("moment");

// exports["readFile'"] = function (filePath) {
//   return function () {
//     return JBridge.loadFileInDUI(filePath);
//   };
// };

// exports["showLoader'"] = function (str) {
//   return JBridge.showLoader(str);
// };
var timerIdDebounce;
var inputForDebounce;
var timerIdForTimeout;
var allTimerIID = [];
var uniqueId = 0;

exports.generateUniqueId = function (unit) {
  uniqueId += 1;
  return JSON.stringify(uniqueId);
};

exports.getOs = function () {
  if (window.__OS) {
    return window.__OS;
  }
  return "ANDROID";
};

exports["getTimer"] = function (valType) {
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


exports["get15sTimer"] = function (cb){
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

exports["get5sTimer"] = function (cb){
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

exports ["debounceFunction"] = function (delay) {
  return function (cb){
    return function (action){
      return function(){
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
  inputForDebounce = a;
}


exports["get10sTimer"] = function (cb){
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

exports["startTimer"] = function(input){  
  return function(cb){
    return function (action){
      return function(){
        var callback = callbackMapper.map(function(){
          var time = input;
          time = time-1;
          startCountDown(time);
          console.log("inside startTimer");
          function startCountDown(seconds){
            if (seconds >= 0){
              timerIdForTimeout = setTimeout(()=> {
                if(seconds <=0){
                  clearTimeout(timerIdForTimeout);
                  console.log("EXPIRED");
                  console.log(seconds + "seconds");
                  cb(action("EXPIRED"))();
                }else{
                  var timer = seconds + "s ";
                  console.log(timer + "seconds") ;
                  cb(action(timer))();
                  startCountDown(seconds-1);
                }
              },1000);
            }
          }
          console.log("timerId : " + timerIdForTimeout);
        });
        window.callUICallback(callback);
      }
    }
  }
}


exports ["clearTimer"] = function (a)
{ if(timerIdForTimeout){
    clearTimeout(timerIdForTimeout);
  }
  if(window.timerId){
    clearInterval(window.timerId);
  }
};

exports["clearPopUpTimer"] = function (a) {
  clearInterval(parseInt(a));
};

function instantGetTimer (fn , delay) {
  fn();
  window.timerId = setInterval( fn, delay );
  allTimerIID.push(window.timerId);
  return window.timerId;
}

exports["clearAllTimer"] = function(a) {
  console.log("allTimerIID");
  console.log(allTimerIID);
  while(allTimerIID.length > 0){
    clearInterval(parseInt(allTimerIID.pop()));
  }
}
// exports["decodeDriverInfo"] = function (a) {
//   try {
//     var driverName = JSON.parse(a).tracker.trip.driver.persona.descriptor
//       .first_name;
//     var vehicleNo = JSON.parse(a).tracker.trip.vehicle.registration.number;
//     return driverName + ", " + vehicleNo;
//   } catch (e) {
//     return "(Driver not yet assigned)";
//   }

//   // return JSON.parse(JSON.parse(a).driverInfo).fullName;
// };

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

exports["decodeDriverInfo"] = function (a) {
  try {
    var driverName = JSON.parse(a).tracker.trip.driver.persona.descriptor
      .first_name;
    var vehicleNo = JSON.parse(a).tracker.trip.vehicle.registration.number;
    return driverName + ", " + vehicleNo;
  } catch (e) {
    return "(Driver not yet assigned)";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

exports["decodeDriver"] = function (a) {
  try {
    var vehicleVariant = JSON.parse(a).variant;
    var vehicleNo = JSON.parse(a).registrationNo;
    var vehicleColor = JSON.parse(a).color;
    var vehicleModel = JSON.parse(a).model;
    return vehicleVariant + " - " + vehicleNo + ", " + vehicleColor + " " + vehicleModel ;
  } catch (e) {
    return "";
  }
};


exports["decodeAgencyInfo"] = function (a) {
  try {
    var agencyName = JSON.parse(a).provider.name;
    var agencyNumbers = JSON.parse(a).provider.phones;
    var agencyNumber = agencyNumbers[0];     //tracker.trip.driver.persona.descriptor.first_name;
    //var vehicleNo = JSON.parse(a).tracker.trip.vehicle.registration.number;
    return  agencyName+ ", " + agencyNumber;
  } catch (e) {
    console.log(e);
    return "auto Agency, 890";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

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

exports["decodeErrorPayload"] = function (a) {
  try {
    var errPayload = JSON.parse(a).errorPayload;
    if(errPayload == null){
      return "";
    }else{
      return errPayload[0].expectation;
    }
  }catch(e){
    console.log(e);
    return "";
  }
}

exports["decodeAgencyName"] = function (a) {
  try {
    var agencyName = JSON.parse(a).provider.name;
    return  agencyName;
  } catch (e) {
    console.log(e);
    return "auto Agency";
  }
};

exports["decodeAgencyPhoneNo"] = function (a) {
  try {
    var agencyNumbers = JSON.parse(a).provider.phones;
    var agencyNumber = agencyNumbers[0];    
    return agencyNumber;
  } catch (e) {
    console.log(e);
    return "9876543210";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

exports["completedRides"] = function (a) {
  try {
    var rides1 = JSON.parse(a).provider.info;
    var rides = JSON.parse(rides1).completed;
    if(rides == null)
    {
      return 0;
    }  
    return rides;
  } catch (e) {
    console.log(e);
    return 0;
  }
};

exports["totalAgencies"] = function (a) {
  try {
    var rides = JSON.parse(a).total;
    if(rides == null)
    {
      return 0;
    }   
    return rides;
  } catch (e) {
    console.log(e);
    return 0;
  }
};

exports["declinedAgencies"] = function (a) {
  try {
    var rides = JSON.parse(a).declined;
    if(rides == null)
    {
      return 0;
    }  
    return rides;
  } catch (e) {
    console.log(e);
    return 0;
  }
};

exports["acceptedAgencies"] = function (a) {
  try {
    var rides = JSON.parse(a).accepted;  
    if(rides == null)
    {
      return 0;
    }
    return rides;
  } catch (e) {
    console.log(e);
    return 0;
  }
};

exports["hasDriver"] = function (a) {
  try {
    var info = JSON.parse(a);
    if (info.tracker.trip.driver == null)
      {
        return false;
      }  
    return true;
  } catch (e) {
    console.log(e);
    return false;
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

exports["driverInfo"] = function (a) {
  try {
    var driverName = JSON.parse(a).tracker.trip.driver.name;
    var driverNumbers = JSON.parse(a).tracker.trip.driver.phones;
    var driverNumber = driverNumbers[0];
    return driverName + ", "+ driverNumber;
  } catch (e) {
    // console.log(e);
    return "(Driver not yet assigned)";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

exports["driverRating"] = function (a) {
  try {
    var info = JSON.parse(a);
    if (info.tracker.trip.driver.rating == null)
      {
        return "__";
      }  
    return JSON.parse(a).tracker.trip.driver.rating.value;
  } catch (e) {
    console.log(e);
    return "__";
  }
};

exports["otp"] = function (a) {
  try {
    var otp = JSON.parse(a).tracker.trip.id;
    return otp ;
  } catch (e) {
    return "0000";
  }

};

exports["vehicleInfo"] = function (a) {
  try {
    var vehicleName = JSON.parse(a).tracker.trip.vehicle.variant;
    var vehicleNumber = JSON.parse(a).tracker.trip.vehicle.registrationNumber;
    //var driverNumber = driverNumbers[0];
    return vehicleName + ", "+ vehicleNumber;
  } catch (e) {
    console.log(e);
    return "(Driver not yet assigned)";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};


exports["vehicleNumber"] = function (a) {
  try {
    var vehicleNumber = JSON.parse(a).registrationNo;
    return vehicleNumber;
  } catch (e) {
    console.log(e);
    return "";
  }

};

exports["vehicleColorAndModal"] = function (a) {
  try {
    var vehicleColor = JSON.parse(a).tracker.trip.vehicle.color;
    var vehicleModal = JSON.parse(a).tracker.trip.vehicle.model;
    //var driverNumber = driverNumbers[0];
    return vehicleColor + " " + vehicleModal;
  } catch (e) {
    console.log(e);
    return "";
  }
};

exports["vehicleType"] = function (a) {
  try {
    var category = JSON.parse(a).category;
    if(category == "AUTO")
    {
      return "Auto";
    }
    else
    {
    var vehicleType = JSON.parse(a).variant;
    return vehicleType;
    }
  } catch (e) {
    console.log(e);
    return "";
  }

};

exports["modelNumber"] = function (a) {
  try {
    var modelNumber = JSON.parse(a).model;
    return modelNumber;
  } catch (e) {
    console.log(e);
    return "";
  }

};

exports["vehicleId"] = function (a) {
  try {
    var vehicleId = JSON.parse(a).id;
    return vehicleId;
  } catch (e) {
    console.log(e);
    return "";
  }

};


exports["decodeAddress"] = function (a){
  try  {
    var building = JSON.parse(a).door + ", " + JSON.parse(a).building + ", " + JSON.parse(a).street + ", " + JSON.parse(a).locality + ", " + JSON.parse(a).city + ", " + JSON.parse(a).state + ", " + JSON.parse(a).country;
    var building4 = JSON.parse(a).building + ", " + JSON.parse(a).street + ", " + JSON.parse(a).locality + ", " + JSON.parse(a).city + ", " + JSON.parse(a).state + ", " + JSON.parse(a).country;
    var building3 = JSON.parse(a).street + ", " + JSON.parse(a).locality + ", " + JSON.parse(a).city + ", " + JSON.parse(a).state + ", " + JSON.parse(a).country;
    var building1 = JSON.parse(a).locality + ", " + JSON.parse(a).city + ", " + JSON.parse(a).state + ", " + JSON.parse(a).country;
    var building2 = JSON.parse(a).city + ", " + JSON.parse(a).state + ", " + JSON.parse(a).country;
    if (JSON.parse(a).street == "" && JSON.parse(a).locality == " " && JSON.parse(a).door == "" && JSON.parse(a).building == " ") {
      return building2;
    }
    else if (JSON.parse(a).door == "" && JSON.parse(a).building == " " && JSON.parse(a).street == ""){
      return building1;
    }
    else if (JSON.parse(a).door == "" && JSON.parse(a).building == " "){
      return building3;
    }
    else if (JSON.parse(a).door == "") {
      return building4;
    }
    else {
      return building;
    }

  } catch (e) {
    return "";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

exports["decodeDriverInfoTransporter"] = function (a) {
  try {
    var driverInfo = JSON.parse(a).driverInfo;
    var driverFirstName = JSON.parse(driverInfo).firstName;
    var driverLastName = JSON.parse(driverInfo).lastName;
    var driverName = driverFirstName + " " + driverLastName;
    var vehicleInfo = JSON.parse(a).vehicleInfo;
    var vehicleNo = JSON.parse(vehicleInfo).registrationNo;
    console.log(driverName);
    console.log(vehicleNo);
    //console.log(JSON.parse(a).driverInfo.firstName);
    //console.log(JSON.parse(a).driverInfo.lastName);
    // .registration.number;
    return driverName + ", " + vehicleNo;
  } catch (e) {
    console.log(JSON.parse(a));
    console.log(e);
    return "(Driver not yet assigned)";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

exports["decodeDriverIdTransporter"] = function (a) {
  try {
    var driverInfo = JSON.parse(a).driverInfo;
    var driverId = JSON.parse(driverInfo).id;
    return driverId;
  } catch (e) {
    console.log(JSON.parse(a));
    console.log(e);
    return "";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

exports["decodeVehicleIdTransporter"] = function (a) {
  try {
    var vehicleInfo = JSON.parse(a).vehicleInfo;
    var VehicleId = JSON.parse(vehicleInfo).id;
    // var driverLastName =  JSON.parse(driverInfo).lastName;
    // var driverName = driverFirstName+driverLastName;
    // var vehicleInfo = JSON.parse(a).vehicleInfo;
    // var vehicleNo = JSON.parse(vehicleInfo).registrationNo;
    // console.log(driverName);
    // console.log(vehicleNo);
    //console.log(JSON.parse(a).driverInfo.firstName);
    //console.log(JSON.parse(a).driverInfo.lastName);
    // .registration.number;
    return VehicleId;
  } catch (e) {
    console.log(JSON.parse(a));
    console.log(e);
    return "";
  }

  // return JSON.parse(JSON.parse(a).driverInfo).fullName;
};

// exports["decodeAgencyInfo"] = function (a) {
//   try {
//     // var driverName =  JSON.parse(a).tracker.trip.driver.persona.descriptor.full_name;
//     // var vehicleNo = JSON.parse(a).tracker.trip.vehicle.registration.number;
//     var AgencyName = JSON.parse(a).provider.name;
//     return AgencyName;
//   } catch (e) {
//     return "";
//   }
// // };
// exports["isLocationPermissionEnabled"] = function (unit) {
//   return function () {
//     return JBridge.isLocationPermissionEnabled();
//   };
// };

exports["decodeDriverNo"] = function (a) {
  try {
    var driverNo = JSON.parse(a).tracker.trip.driver.persona.descriptor.contact
      .mobile.number;
    return driverNo;
  } catch (e) {
    return "";
  }
};

exports["decodeDriverNoTransporter"] = function (a) {
  try {
    var driverInfo = JSON.parse(a).driverInfo;
    var driverNo = JSON.parse(driverInfo).mobileNumber;
    //driver.persona.descriptor.contact.mobile.number;
    return driverNo;
  } catch (e) {
    return "";
  }
};

// exports["showQrCode'"] = function (id) {
//   return function (str) {
//     return function (cb) {
//       JBridge.showQrCode(id, str);
//     };
//   };
// };

// exports["dateISO"] = function (str) {
//   var d = new Date();
//   var update = moment.utc(d).toDate();
//   return update.toISOString().split(".")[0];
// };

exports["getCurrentUTC"] = function (str) {
  var d = new Date();
  // return moment(d).utc().format("YYYY-MM-DDTHH:mm:ss");
  return moment(d).utc().format();
};

exports["getCurrMinUTC"] = function (mins) {
  var d = new Date();
  d.setMinutes(d.getMinutes() + mins);
  // return moment(d).utc().format("YYYY-MM-DDTHH:mm:ss");
  return moment(d).utc().format();
};


exports["getNextUTC"] = function (mins) {
  return function (formatStr) {
    var d = new Date();
    d.setMinutes(d.getMinutes() + mins);
    return moment(d).format(formatStr);
  };
};

exports["convertISTtoUTC"] = function (str) {
  var n = new Date(str);
  // return moment(n).utc().format("YYYY-MM-DDTHH:mm:ss");
  return moment(n).utc().format();
};

exports["convertUTCtoISC"] = function (str) {
  return function (format) {
    var localTime1 = moment.utc(str).toDate();
    localTime1 = moment(localTime1).format(format);
    return localTime1;
  };
}; 

exports["isUTCTimeValid"] = function (str1) {
  return function (str2) {
    var curr1 = new Date(str1);
    var departure = new Date(str2); 
    console.log(departure + " , " + curr1 + "STR");
    var diff =(curr1.getTime() - departure.getTime())/ 1000;
    diff /= 60;
    diff = (Math.round(diff));
    if(diff <= 0){
      return false;
    }else{
      return true;
    }
  };
};

exports["differenceBetweenTwoUTC"] = function (str1) {
  return function (str2) {
    var curr1 = new Date(str1);
    var departure = new Date(str2); 
    console.log(departure + " , " + curr1 + "STR");
    var diff =(curr1.getTime() - departure.getTime())/ 1000;
    diff = (Math.round(diff));
    return diff
  };
};

exports["saveScreenState'"] = function (screenName) {
  return function (state) {
    return function () {
      return window.sessionStorage.setItem(screenName, state);
    };
  };
};

exports["fetchScreenState'"] = function (screenName) {
  return function (just) {
    return function (nothing) {
      return function () {
        var state = window.sessionStorage.getItem(screenName);
        if (state) {
          return just(state);
        }
        return nothing;
      };
    };
  };
};

// exports["scanQrCode"] = function (requestCode) {
//   return function (cb) {
//     return function (action) {
//       return function () {
//         window.activityResultListeners[requestCode] = function (
//           resultCode,
//           bundle
//         ) {
//           var result = "";
//           try {
//             bundle = JSON.parse(bundle);
//             result = bundle["SCAN_RESULT"];
//           } catch (error) {
//             console.error("Couldn't parse bundle on scanQrCode.");
//           }
//           cb(action(result))();
//         };
//         return JBridge.scanQrCode();
//       };
//     };
//   };
// };

// exports["timePicker"] = function (cb) {
//   return function (action) {
//     return function () {
//       var callback = callbackMapper.map(function (hour, min) {
//         cb(action(hour)(min))();
//       });
//       return JBridge.timePicker(callback);
//     };
//   };
// };

// exports["datePicker"] = function (cb) {
//   return function (action) {
//     return function () {
//       var callback = callbackMapper.map(function (year, month, date) {
//         cb(action(year)(month)(date))();
//       });
//       return JBridge.datePicker(callback);
//     };
//   };
// };

// exports["setFCMToken"] = function (cb) {
//   JBridge.setFCMToken();
// };

// exports["dateTimePicker"] = function (cb) {
//   return function (action) {
//     return function () {
//       var callback = callbackMapper.map(function (epochTime) {
//         cb(action(epochTime))();
//       });
//       return JBridge.dateTimePicker(callback);
//     };
//   };
// };

exports["epochToDate"] = function (epochTime) {
  console.log("Date ==----==" + epochTime);
  console.log("Date ==----==" + moment(epochTime).toDate());
  return moment(epochTime).toDate();
};

exports["epochToDateString"] = function (epochTime) {
  console.log("Date ==------==" + epochTime);
  console.log("Date ==----==" + moment(epochTime).toDate());
  return moment(epochTime).toDate().toString();
};

exports["stringToDate"] = function (str) {
  return moment(str).toDate();
};

exports["formatDate"] = function (date) {
  return function (format) {
    var localTime1 = moment.utc(date).toDate();
    localTime1 = moment(localTime1).format(format);

    return localTime1;
  };
};
exports["formatDateFix"] = function (date) {
  return function (format) {
    var localTime1 = moment.utc(date).toDate();
    localTime1 = moment(localTime1).format(format);
    return localTime1;
  };
};

exports["storeCallBackForNotification"] = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (notificationType) {
            cb(action(notificationType))();
          });
          JBridge.storeCallBackForNotification(callback);
      }    
  }}
  catch (error){
      console.log("Error occurred in storeCallBackForNotification ------", error);
  }
}

// exports["getNearbyPlaces"] = function (cb) {
//   return function (action) {
//     return function () {
//       window.activityResultListeners["545"] = function (resultCode, bundle) {
//         console.log("BUNDLE", bundle);
//         var place = JSON.parse(bundle);
//         cb(action(place))();
//       };
//       JBridge.getNearbyPlaces();
//     };
//   };
// };

// exports["getNearbyPlaces'"] = function (cb) {
//   return function (action) {
//     return function (just) {
//       return function (nothing) {
//         return function () {
//           window.activityResultListeners["545"] = function (
//             resultCode,
//             bundle
//           ) {
//             var place = JSON.parse(bundle);
//             place.lat = place.lat ? just(place.lat) : nothing;
//             place.lng = place.lng ? just(place.lng) : nothing;
//             JBridge.hideKeyboardOnNavigation(true);
//             cb(action(place))();
//           };
//           JBridge.getNearbyPlaces();
//         };
//       };
//     };
//   };
// };

// exports["showSnackBar"] = function (str) {
//   return function (actionTitle) {
//     return function (cb) {
//       return function (action) {
//         return function () {
//           var callback = callbackMapper.map(function () {
//             cb(action)();
//           });
//           JBridge.showSnackBar(str, actionTitle, callback);
//         };
//       };
//     };
//   };
// };

// exports["isNetworkAvailable"] = function (unit) {
//   return JBridge.isNetworkAvailable();
// };

// exports["goToUrl"] = function (str) {
//   return function (unit) {
//     return JBridge.goToUrl(str);
//   };
// };

// exports["openUrlInApp"] = function (str) {
//   return function (unit) {
//     return JBridge.openUrlInApp(str);
//   };
// };

exports["getuuid"] = function (dummy) {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
  }
  return (
    s4() +
    s4() +
    "-" +
    s4() +
    "-" +
    s4() +
    "-" +
    s4() +
    "-" +
    s4() +
    s4() +
    s4()
  );
};

// exports["addMarker'"] = function (title) {
//   return function (lat) {
//     return function (lng) {
//       return function () {
//         console.log("I AM HERE ------------------");
//         JBridge.upsertMarker(title, lat, lng);
//         return true;
//       };
//     };
//   };
// };


// exports["removeMarker"] = function (title) {
//     try{
//       return function () {
//         console.log("I AM HERE to remove marker------------------");
//         JBridge.removeMarker(title);
//         return true;
//       };
//   }
//   catch(e)
//   { 
//     console.log(e);
//     console.log("error in removeMarker----------------------------------");
//   }
// };

exports["convertKmToM"] = function (dist) {
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

exports["convertMToKm"] = function (dist) {
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

exports["calculateETA"] = function (dist) {
  try{
    var distance = parseInt(dist);
    distance = distance/1000;
    var hours =  distance / 25;
    var minutes = hours*60; 
    if ( minutes < 1) 
    {
      return ("1 min");
    }
    else 
    {
      return (((minutes.toFixed(0)).toString()) + " mins");
    }
  }catch(e){ 
    console.log(e);
    console.log("error in calculateETA----------------------------------");
  }
}




// exports["parseAddress"] = function (json) {
//   return JSON.parse(json);
// };

// exports["drawRoute"] = function (style) {
//   return function (trackColor) {
//     return function (data) {
//       return function () {
//         console.log("I AM HERE ------------------ IN DRAW ROUTE");
//         // if(i == 1){
//         // sampleData1
//         return JBridge.drawRoute(JSON.stringify(data), style, trackColor);
//         // }else{
//         //   // sampleData2
//         //   JBridge.drawRoute(JSON.stringify(sampleData2), style, trackColor);
//         // }
//       };
//     };
//   };
// };


// exports["drawActualRoute"] = function (style) {
//   try{
//   return function (trackColor) {
//     return function (data) {
//       return function () {
//         console.log("I AM HERE ------------------ IN DRAW ActualRoute   --------------- ROUTE");
//         // if(i == 1){
//         // sampleData1
//         return JBridge.drawActualRoute(JSON.stringify(data), style, trackColor);
//         // }else{
//         //   // sampleData2
//         //   JBridge.drawRoute(JSON.stringify(sampleData2), style, trackColor);
//         // }
//       };
//     };
//   };
// }
// catch(e)
//   {
//     console.log(e);
//     console.log("error ----------------------------------"); 
//   }
// };


// exports["showAndDrawRoute"] = function(title){
//   try {
//   return function (style) {
//     return function (trackColor) {
//       return function (data) {
//         return function () {
//           console.log("I AM HERE ------------------ IN DRAW ROUTE");
//           // if(i == 1){
//           // sampleData1
//           JBridge.showAndDrawRoute(title,JSON.stringify(data), style, trackColor);
//           // }else{
//           //   // sampleData2
//           //   JBridge.drawRoute(JSON.stringify(sampleData2), style, trackColor);
//           // }

//           return true;
//         };
//       };
//     };
//   };
//   }
//   catch(e)
//   {
//     console.log(e);
//     console.log("error ----------------------------------"); 
//   }

// };


// exports["addMarkers"] = function (data) {
//   JBridge.addMarkers(JSON.stringify(data));
// };

// exports["removePolyLine"] = function (str) {
//   JBridge.removePolyLine(str);
// };

// exports["requestLocation"] = function (str) {
//   return function () {
//     JBridge.requestLocation();
//   };
// };

// exports["showMap'"] = function (id) {
//   return function (type) {
//     return function (cb) {
//       return function (action) {
//         return function () {
//           var callback = callbackMapper.map(function () {
//             console.log("in show map",action);
//             window.x = cb;
//             window.y = action;
//             cb(action)();
//           });
//           JBridge.showMap(id, type, callback);
//           return true;
//         };
//       };
//     };
//   };
// };

// exports["isLocationPermissionEnabled"] = function (unit) {
//   return function () {
//     return JBridge.isLocationPermissionEnabled();
//   }
// }

// exports["getCurrentLatLong'"] = function () {
//   var location = JBridge.getCurrentLatLong();
//   if (location) {
//     var parsedData = JSON.parse(location);
//     if (parsedData.lat && parsedData.long) {
//       return location;
//     } else {
//       return '{"lat":"","long":""}';
//     }
//   }
// };
// exports["isLocationEnabled"] = function (unit) {
//   return function () {
//     return JBridge.isLocationEnabled();
//   };
// };

// exports["getCurrentPosition"] = function (cb) {
//   return function (action) {
//     return function () {
//       var callback = callbackMapper.map(function (lat, lng) {
//         cb(action(lat)(lng))();
//       });
//       return JBridge.getCurrentPosition(callback);
//     };
//   };
// };

// exports["openNavigation"] = function (slat) {
//   return function (slong) {
//     return function (dlat) {
//       return function (dlong) {
//         return JBridge.openNavigation(slat, slong, dlat, dlong);
//       };
//     };
//   };
// };

// exports["animateCamera"] = function (lat) {
//   return function (lng) {
//     return function (zoom) {
//       return function () {
//         JBridge.animateCamera(lat, lng, zoom);
//       };
//     };
//   };
// };


// exports["moveCamera"] = function (lat1) {
//   return function (lng1) {
//     return function (lat2) {
//       return function (lng2){
//         return function () {
//           JBridge.moveCamera(lat1, lng1, lat2,lng2);
//         };
//       };
//     };
//   };
// };

// exports["hideLoader'"] = function () {
//   return JBridge.hideLoader();
// };

// exports["closeApp"] = function (str) {
//   JBridge.closeApp();
// };

// exports["minimizeApp"] = function (str) {
//   JBridge.minimizeApp();
// };
// exports["toast"] = function (str) {
//   JBridge.toast(str);
// };

// exports["requestReadAndSendSmsPermission"] = function (permission) {
//   JBridge.requestReadAndSendSmsPermission(permission);
// };

// exports["hideKeyboardOnNavigation"] = function (permission) {
//   JBridge.hideKeyboardOnNavigation(permission);
// };

// exports["showAuthScreen"] = function (title) {
//   return function (value) {
//     JBridge.showAuthScreen(title, value);
//   };
// };

exports ["getcurrentdate"] = function (string) {
  var today = new Date();
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = today.getFullYear();

  today = yyyy + '-' + mm + '-' + dd;
  return today;
}

exports["eval'"] = function (string) {
  return function () {
    try {
      eval(string);
    } catch (err) {
      console.error(err);
    }
    return;
  };
};

exports.getISOTime = function () {
  return new Date().toISOString();
};

// exports["getPan"] = function (gstin) {
//   return gstin.slice(2, -3);
// };

exports["getMobileNo"] = function (mobileNo) {
  return mobileNo.slice(0, 2) + "xxxxxx" + mobileNo.slice(8, 10);
};

// exports["setF"] = function (src) {
//   return function (key) {
//     return function (val) {
//       try {
//         src[key] = val;
//         return src;
//       } catch (err) {
//         console.error(err);
//         return src;
//       }
//     };
//   };
// };

// exports.onEvent = function (payload) {
//   return function () {
//     window.temp = payload;
//     JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), "");
//   };
// };

// $(function(){
//   setInterval(function(){
//     var divUtc = $('#divUTC');
//     var divUtc1 = $('#divUTC1');
//     var divLocal = $('#divLocal');
//     var divLocal1 = $('#divLocal1');
//     //put UTC time into divUTC
//     divUtc1.text(moment.utc("2020-05-20T15:29:43").format('YYYY-MM-DD HH:mm:ss'));
//     divUtc.text(moment.utc().format('YYYY-MM-DD HH:mm:ss'));

//     //get text from divUTC and conver to local timezone
//     var localTime  = moment.utc(divUtc.text()).toDate();
//     localTime = moment(localTime).format('YYYY-MM-DD HH:mm:ss');
//     divLocal.text(localTime);

//         //get text from divUTC and conver to local timezone
//     var localTime1  = moment.utc(divUtc1.text()).toDate();
//     localTime1 = moment(localTime1).format('YYYY-MM-DD HH:mm:ss');
//     divLocal1.text(localTime1);

//   },1000);
// });
exports["getAge"] = function (dateFutureS) {
  var localTime1 = moment.utc(dateFutureS).toDate();
  // localTime1 = moment(localTime1).format('YYYY-MM-DD HH:mm:ss');
  // console.log("LOCAL-TIME-"+localTime1)

  var dateFuture = new Date(localTime1);
  // console.log("FUTURE-TIME-"+localTime1)
  var dateNow = new Date();
  // console.log("NOW-TIME-"+localTime1)
  var diffInMilliSeconds = Math.abs(dateFuture - dateNow) / 1000;

  // calculate days
  const days = Math.floor(diffInMilliSeconds / 86400);
  diffInMilliSeconds -= days * 86400;
  // console.log('calculated days', days);

  // calculate hours
  const hours = Math.floor(diffInMilliSeconds / 3600) % 24;
  diffInMilliSeconds -= hours * 3600;
  // console.log('calculated hours', hours);

  // calculate minutes
  const minutes = Math.floor(diffInMilliSeconds / 60) % 60;
  diffInMilliSeconds -= minutes * 60;
  // console.log('minutes', minutes);

  var difference = "";
  if (days > 0)
    difference += days === 1 ? days + " day ago" : days + " days ago";

  if (days === 0 && hours > 0)
    difference +=
      hours === 0 || hours === 1 ? hours + " hr ago" : hours + " hrs ago";

  if (days === 0 && hours === 0)
    difference +=
      minutes === 0 || hours === 1
        ? minutes + " min ago"
        : minutes + " min ago";

  if (days === 0 && hours === 0 && minutes == 0)
    difference += days === 0 || hours === 0 || minutes == 0 ? "" : "now";
  // console.log(difference)
  return difference;
};

// exports._onEventWithCB = function (payload) {
//   return function (succ) {
//     return function (err) {
//       return function () {
//         var cb = callbackMapper.map(function (res) {
//           try {
//             var result = JSON.parse(res);
//             if (result.status == "onResponse") {
//               succ(result.payload)();
//             } else {
//               err(result.payload)();
//             }
//           } catch (err) {
//             console.error(err);
//             succ(res)();
//           }
//         });
//         JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), cb);
//       };
//     };
//   };
// };

exports["exitApp'"] = function (code) {
  return function (payload) {
    if (JOS) {
      return JOS.finish(code)(payload)();
    }
  };
};

// exports.getSessionInfo = JSON.parse(JBridge.getDeviceInfo());

exports.base64Encode = function (key) {
  return btoa(key + ":");
};
// exports.getKeyInSharedPrefKeys = function (key) {
//   return JBridge.getKeysInSharedPrefs(key);
// };

// exports["setKeyInSharedPrefKeys'"] = function (key) {
//   return function (value) {
//     JBridge.setKeysInSharedPrefs(key, value);
//   };
// };

// exports.setKeyInSharedPrefKeyss = function (key) {
//   return function (value) {
//     return JBridge.setKeysInSharedPrefs(key, value);
//   };
// };

// exports.removeKeysInSharedPrefs = function (key) {
//   return JBridge.removeKeysInSharedPrefs(key);
// };
// exports.createNonce = function (length) {
//   var text = "";
//   var possible =
//     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
//   for (var i = 0; i < length; i++) {
//     text += possible.charAt(Math.floor(Math.random() * possible.length));
//   }
//   return text;
// };

// exports["startGodel'"] = function (payload) {
//   return function (left) {
//     return function (right) {
//       return function () {
//         console.log(payload)
//         var cb = function (code) {
//           return function (resp) {
//             return function () {
//               JBridge.setSessionAttribute("inject_acs_into_iframes", "false");
//               console.log("from godel:", code, resp);
//               if (code == 0) {
//                 left("User Aborted Transaction")();
//               } else {
//                 right(resp)();
//               }
//             }
//           }
//         };
//         JBridge.setSessionAttribute("inject_acs_into_iframes", "true");
//         JOS.startApp("in.juspay.godel")(payload)(cb)();
//       }
//     }
//   }
// };

exports["subscribeEvent'"] = function (event) {
  return function (fn) {
    return function () {
      window.eventListeners = window.eventListeners || {};
      window.eventListeners[event] = fn;
    };
  };
};

exports["subscribeEventCB'"] = function (event) {
  return function (fn) {
    return function () {
      window.eventListeners = window.eventListeners || {};
      window.eventListeners[event] = fn;
    };
  };
};

// exports["registerEvent'"] = function (event) {
//   return function (fn) {
//     return function () {
//       window.eventListeners = window.eventListeners || {};
//       if (Array.isArray(window.eventListeners[event])) {
//         window.eventListeners[event].push(fn);
//       } else {
//         window.eventListeners[event] = [];
//         window.eventListeners[event].push(fn);
//       }
//     };
//   };
// };

// exports.getExitFlows = function () {
//   return window.exitFlows || [];
// };

exports["_getTime"] = function () {
  return new Date().getTime().toString();
};

exports["parseURL'"] = function (str) {
  return function () {
    var parser = document.createElement("a");
    parser.href = str;
    var queryParams = parser.search.replace("?", "").split("&");
    var ans = {};
    for (var i in queryParams) {
      if (typeof queryParams[i] == "string") {
        var pair = queryParams[i].split("=");
        ans[pair[0]] = pair[1];
      }
    }
    return JSON.stringify(ans);
  };
};

// exports["ecBaseUrl"] = (function () {
//   if (window.__payload && window.__payload.environment) {
//     return window.__payload.environment == "prod"
//       ? "https://api.juspay.in"
//       : "https://sandbox.juspay.in";
//   } else {
//     return "https://api.juspay.in";
//   }
// })();

// exports["getEnvironment"] = function () {
//   return window.__payload.environment;
// };

exports["addToExitFlows'"] = function (f) {
  return function () {
    window.exitFlows = window.exitFlows || [];
    window.exitFlows.push(f);
  };
};

// exports["_log"] = function (msg) {
//   return function () {
//     console.log(msg);
//   };
// };

// exports["getSinValue"] = function (x) {
//   var value = Math.sin(degrees_to_radians(x)) + 0.0;
//   var value2 = value.toFixed(2);
//   if (value2 == "1") return 1.0;
//   if (value2 == "0") return 0.0;
//   return parseFloat(value2);
// };

// exports["getCosValue"] = function (y) {
//   var value = Math.cos(degrees_to_radians(y)) + 0.0;
//   var value2 = value.toFixed(2);
//   if (value2 == "1") return 1.0;
//   if (value2 == "0") return 0.0;
//   return parseFloat(value2);
// };

function degrees_to_radians(degrees) {
  var pi = Math.PI;
  return degrees * (pi / 180);
}

exports["_getSessionId"] = function () {
  return window.session_id;
};

// exports["loadSVGImage"] = function (imageId) {
//   return function (url) {
//     return function (localAsset) {
//       return function () {
//         return JBridge.loadSVGImage(imageId, url, localAsset);
//       };
//     };
//   };
// };

// exports["loadGIFImage"] = function (imageId) {
//   return function (url) {
//     return function (localAsset) {
//       return function () {
//         return JBridge.loadGIFImage(imageId, url, localAsset);
//       };
//     };
//   };
// };

exports["isMobile"] = (function () {
  if (window.__OS !== "WEB") {
    return true;
  }

  if (window.matchMedia("only screen and (max-width: 992px)").matches) {
    return true;
  }

  return false;
})();

// exports["toggleLoader'"] = function (showLoader) {
//   return function () {
//     return JBridge.toggleLoader(showLoader);
//   };
// };

// exports["loaderText'"] = function (mainTxt) {
//   return function (subTxt) {
//     return function () {
//       return JBridge.loaderText(mainTxt, subTxt);
//     };
//   };
// };





exports["calculateDist"] = function(lat1)
{
  return function(long1){
      return function(lat2){
        return function(long2){
          const R = 6371e3; // metres
          const φ1 = lat1 * Math.PI/180; // φ, λ in radians
          const φ2 = lat2 * Math.PI/180;
          const Δφ = (lat2-lat1) * Math.PI/180;
          const Δλ = (long2-long1) * Math.PI/180;
          
          const a = Math.sin(Δφ/2) * Math.sin(Δφ/2) +
                    Math.cos(φ1) * Math.cos(φ2) *
                    Math.sin(Δλ/2) * Math.sin(Δλ/2);
          const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
          
          const d = R * c; // in metres
          if(d<=100) return true;
          else return false; 
        };
      };
  };
};


// exports["showDialer"] = function (str) {
//   JBridge.showDialer(str);
// };

// var sampleData1 = {
//   hints: {
//     "visited_nodes.average": "74.0",
//     "visited_nodes.sum": "74",
//   },
//   info: {
//     copyrights: ["GraphHopper", "OpenStreetMap contributors"],
//     took: 7,
//   },
//   paths: [
//     {
//       distance: 24563.212,
//       weight: 1428.00993,
//       time: 1059512,
//       transfers: 0,
//       points_encoded: false,
//       bbox: [72.877643, 19.023512, 73.0407, 19.075966],
//       points: {
//         type: "LineString",
//         coordinates: [
//           [72.877643, 19.075966],
//           [72.87911, 19.075068],
//           [72.879416, 19.074915],
//           [72.88015, 19.074502],
//           [72.880958, 19.073957],
//           [72.881251, 19.073816],
//           [72.881453, 19.073754],
//           [72.881871, 19.073674],
//           [72.882519, 19.073604],
//           [72.88288, 19.073549],
//           [72.883159, 19.073457],
//           [72.88347, 19.073297],
//           [72.88394, 19.072959],
//           [72.884237, 19.07267],
//           [72.884978, 19.072014],
//           [72.885147, 19.071849],
//           [72.886699, 19.070028],
//           [72.886941, 19.069703],
//           [72.888704, 19.067652],
//           [72.889022, 19.067326],
//           [72.88983, 19.066573],
//           [72.89006, 19.066401],
//           [72.890285, 19.066278],
//           [72.890794, 19.066104],
//           [72.891794, 19.065848],
//           [72.893053, 19.065503],
//           [72.893344, 19.06548],
//           [72.895342, 19.064944],
//           [72.895729, 19.064792],
//           [72.896375, 19.064642],
//           [72.896829, 19.06458],
//           [72.897103, 19.064585],
//           [72.897362, 19.064619],
//           [72.897579, 19.064683],
//           [72.897772, 19.064775],
//           [72.898796, 19.065465],
//           [72.900235, 19.066345],
//           [72.901554, 19.067209],
//           [72.902147, 19.067545],
//           [72.903608, 19.068546],
//           [72.903856, 19.068642],
//           [72.907184, 19.070855],
//           [72.907376, 19.070703],
//           [72.907589, 19.070501],
//           [72.907708, 19.070437],
//           [72.908091, 19.070336],
//           [72.908552, 19.070079],
//           [72.908916, 19.069785],
//           [72.909363, 19.069284],
//           [72.909655, 19.068747],
//           [72.909793, 19.068335],
//           [72.909977, 19.066979],
//           [72.910058, 19.066487],
//           [72.910209, 19.066047],
//           [72.910436, 19.065622],
//           [72.910797, 19.065149],
//           [72.911534, 19.064486],
//           [72.911842, 19.064226],
//           [72.912525, 19.063821],
//           [72.913333, 19.063541],
//           [72.914195, 19.063314],
//           [72.914645, 19.063204],
//           [72.915053, 19.063251],
//           [72.915275, 19.063202],
//           [72.916052, 19.062975],
//           [72.916686, 19.062734],
//           [72.917665, 19.062301],
//           [72.919737, 19.061423],
//           [72.92283, 19.059741],
//           [72.92782, 19.05721],
//           [72.92862, 19.056824],
//           [72.928901, 19.056715],
//           [72.92925, 19.056639],
//           [72.930892, 19.056457],
//           [72.93175, 19.056257],
//           [72.932294, 19.056086],
//           [72.93288, 19.055768],
//           [72.933991, 19.054933],
//           [72.934151, 19.0548],
//           [72.93452, 19.054434],
//           [72.935125, 19.053712],
//           [72.935593, 19.053113],
//           [72.936704, 19.051567],
//           [72.93684, 19.051485],
//           [72.936928, 19.051464],
//           [72.937127, 19.051484],
//           [72.940065, 19.052529],
//           [72.940788, 19.052696],
//           [72.95228, 19.05683],
//           [72.956902, 19.058478],
//           [72.957424, 19.058636],
//           [72.957908, 19.058747],
//           [72.958303, 19.058814],
//           [72.95959, 19.059078],
//           [72.961102, 19.059447],
//           [72.978222, 19.063468],
//           [72.979087, 19.063752],
//           [72.980283, 19.064315],
//           [72.98153, 19.064894],
//           [72.981805, 19.065084],
//           [72.982115, 19.065342],
//           [72.985576, 19.068775],
//           [72.985802, 19.06897],
//           [72.986064, 19.069147],
//           [72.986364, 19.069299],
//           [72.986639, 19.069404],
//           [72.986935, 19.069492],
//           [72.987203, 19.069546],
//           [72.98753, 19.069566],
//           [72.98954, 19.069404],
//           [72.992012, 19.069194],
//         ],
//       },
//       instructions: [
//         {
//           distance: 3771.715,
//           heading: 123.5,
//           sign: 0,
//           interval: [0, 41],
//           text: "Continue",
//           time: 259857,
//           street_name: "",
//         },
//         {
//           distance: 1205.176,
//           sign: 2,
//           interval: [41, 60],
//           text: "Turn right onto Ghatkopar - Mankhurd Link Road, GMLR",
//           time: 45189,
//           street_name: "Ghatkopar - Mankhurd Link Road, GMLR",
//         },
//         {
//           distance: 2778.545,
//           sign: 0,
//           interval: [60, 82],
//           text: "Continue onto Jeejabai Bhosale Marg, GMLR",
//           time: 101392,
//           street_name: "Jeejabai Bhosale Marg, GMLR",
//         },
//         {
//           distance: 4812.655,
//           sign: -7,
//           interval: [82, 97],
//           text: "Keep left",
//           time: 156381,
//           street_name: "",
//         },
//         {
//           annotation_text: "toll road",
//           distance: 9849.698,
//           sign: 0,
//           annotation_importance: 1,
//           interval: [97, 182],
//           text: "Continue onto Sion-Panvel Highway, SPH",
//           time: 369347,
//           street_name: "Sion-Panvel Highway, SPH",
//         },
//         {
//           distance: 349.322,
//           sign: 7,
//           interval: [182, 186],
//           text: "Keep right onto TBR",
//           time: 34932,
//           street_name: "TBR",
//         },
//         {
//           exit_number: 4,
//           distance: 1796.101,
//           sign: 6,
//           exited: true,
//           turn_angle: 0.83,
//           interval: [186, 221],
//           text: "At roundabout, take exit 4 onto TBR",
//           time: 92414,
//           street_name: "TBR",
//         },
//         {
//           distance: 0.0,
//           sign: 4,
//           last_heading: 352.1169425755987,
//           interval: [221, 221],
//           text: "Arrive at destination",
//           time: 0,
//           street_name: "",
//         },
//       ],
//       legs: [],
//       details: {},
//       ascend: 135.71599973738194,
//       descend: 124.16100014746189,
//       snapped_waypoints: {
//         type: "LineString",
//         coordinates: [
//           [72.877643, 19.075966],
//           [73.029661, 19.033048],
//         ],
//       },
//     },
//   ],
// };

// var sampleData2 = {
//   hints: {
//     "visited_nodes.average": "74.0",
//     "visited_nodes.sum": "74",
//   },
//   info: {
//     copyrights: ["GraphHopper", "OpenStreetMap contributors"],
//     took: 7,
//   },
//   paths: [
//     {
//       distance: 24563.212,
//       weight: 1428.00993,
//       time: 1059512,
//       transfers: 0,
//       points_encoded: false,
//       bbox: [72.877643, 19.023512, 73.0407, 19.075966],
//       points: {
//         type: "LineString",
//         coordinates: [
//           [72.992767, 19.069116],
//           [72.993513, 19.06901],
//           [73.000632, 19.068429],
//           [73.001239, 19.068425],
//           [73.002746, 19.068287],
//           [73.005332, 19.068112],
//           [73.006008, 19.06801],
//           [73.007022, 19.067981],
//           [73.008783, 19.067871],
//           [73.011576, 19.067638],
//           [73.01212, 19.06761],
//           [73.015956, 19.067283],
//           [73.018031, 19.067127],
//           [73.019456, 19.067002],
//           [73.019691, 19.066942],
//           [73.019881, 19.066829],
//           [73.020005, 19.066664],
//           [73.020078, 19.066414],
//           [73.020007, 19.065452],
//           [73.019981, 19.065335],
//           [73.019916, 19.065175],
//           [73.01982, 19.065026],
//           [73.019611, 19.064795],
//           [73.01952, 19.064635],
//           [73.019417, 19.064236],
//           [73.019377, 19.063902],
//           [73.019346, 19.063176],
//           [73.019339, 19.062239],
//           [73.019304, 19.061719],
//           [73.019311, 19.060417],
//           [73.019371, 19.058517],
//           [73.019416, 19.057641],
//           [73.019512, 19.053685],
//           [73.019555, 19.053389],
//           [73.019627, 19.053098],
//           [73.019729, 19.052814],
//           [73.019858, 19.052542],
//           [73.020015, 19.052282],
//           [73.020197, 19.052038],
//           [73.021653, 19.050454],
//           [73.021888, 19.050144],
//           [73.026584, 19.044756],
//           [73.02697, 19.044396],
//           [73.02798, 19.043295],
//           [73.028155, 19.043057],
//           [73.028304, 19.042803],
//           [73.028427, 19.042537],
//           [73.028521, 19.042261],
//           [73.028586, 19.041978],
//           [73.029004, 19.038534],
//           [73.029027, 19.038109],
//           [73.030109, 19.030719],
//           [73.030147, 19.030517],
//           [73.030228, 19.030327],
//           [73.030347, 19.030155],
//           [73.030499, 19.030013],
//           [73.030589, 19.029949],
//           [73.030678, 19.029903],
//           [73.03088, 19.029838],
//           [73.03109, 19.029803],
//           [73.031303, 19.029814],
//           [73.03151, 19.029853],
//           [73.032323, 19.030062],
//           [73.032624, 19.030096],
//           [73.032776, 19.030092],
//           [73.033074, 19.030042],
//           [73.033217, 19.029997],
//           [73.033488, 19.029868],
//           [73.033727, 19.02969],
//           [73.034248, 19.029133],
//           [73.034819, 19.028576],
//           [73.037907, 19.025862],
//           [73.038079, 19.025642],
//           [73.040202, 19.023916],
//           [73.040288, 19.02389],
//           [73.04043, 19.023877],
//           [73.040547, 19.02392],
//           [73.040646, 19.023887],
//           [73.0407, 19.023803],
//           [73.040684, 19.023704],
//           [73.040596, 19.023579],
//           [73.040506, 19.023527],
//           [73.040431, 19.023512],
//           [73.040393, 19.023519],
//           [73.040328, 19.023557],
//           [73.040291, 19.023621],
//           [73.040299, 19.023709],
//           [73.040155, 19.02386],
//           [73.038029, 19.025588],
//           [73.037783, 19.025738],
//           [73.034695, 19.02844],
//           [73.034108, 19.029011],
//           [73.033593, 19.029562],
//           [73.033385, 19.029716],
//           [73.033149, 19.029829],
//           [73.033022, 19.029869],
//           [73.032761, 19.029913],
//           [73.032628, 19.029916],
//           [73.032364, 19.029887],
//           [73.031558, 19.02968],
//           [73.031321, 19.029635],
//           [73.031198, 19.02963],
//           [73.031069, 19.029642],
//           [73.030844, 19.029701],
//           [73.030622, 19.029802],
//           [73.030436, 19.029934],
//           [73.030266, 19.030093],
//           [73.030136, 19.030281],
//           [73.030047, 19.030489],
//           [73.030004, 19.030705],
//           [73.029661, 19.033048],
//         ],
//       },
//       instructions: [
//         {
//           distance: 3771.715,
//           heading: 123.5,
//           sign: 0,
//           interval: [0, 41],
//           text: "Continue",
//           time: 259857,
//           street_name: "",
//         },
//         {
//           distance: 1205.176,
//           sign: 2,
//           interval: [41, 60],
//           text: "Turn right onto Ghatkopar - Mankhurd Link Road, GMLR",
//           time: 45189,
//           street_name: "Ghatkopar - Mankhurd Link Road, GMLR",
//         },
//         {
//           distance: 2778.545,
//           sign: 0,
//           interval: [60, 82],
//           text: "Continue onto Jeejabai Bhosale Marg, GMLR",
//           time: 101392,
//           street_name: "Jeejabai Bhosale Marg, GMLR",
//         },
//         {
//           distance: 4812.655,
//           sign: -7,
//           interval: [82, 97],
//           text: "Keep left",
//           time: 156381,
//           street_name: "",
//         },
//         {
//           annotation_text: "toll road",
//           distance: 9849.698,
//           sign: 0,
//           annotation_importance: 1,
//           interval: [97, 182],
//           text: "Continue onto Sion-Panvel Highway, SPH",
//           time: 369347,
//           street_name: "Sion-Panvel Highway, SPH",
//         },
//         {
//           distance: 349.322,
//           sign: 7,
//           interval: [182, 186],
//           text: "Keep right onto TBR",
//           time: 34932,
//           street_name: "TBR",
//         },
//         {
//           exit_number: 4,
//           distance: 1796.101,
//           sign: 6,
//           exited: true,
//           turn_angle: 0.83,
//           interval: [186, 221],
//           text: "At roundabout, take exit 4 onto TBR",
//           time: 92414,
//           street_name: "TBR",
//         },
//         {
//           distance: 0.0,
//           sign: 4,
//           last_heading: 352.1169425755987,
//           interval: [221, 221],
//           text: "Arrive at destination",
//           time: 0,
//           street_name: "",
//         },
//       ],
//       legs: [],
//       details: {},
//       ascend: 135.71599973738194,
//       descend: 124.16100014746189,
//       snapped_waypoints: {
//         type: "LineString",
//         coordinates: [
//           [72.877643, 19.075966],
//           [73.029661, 19.033048],
//         ],
//       },
//     },
//   ],
// };

// exports["getNearbyPlaces"] = function (cb) {
//   return function (action) {
//     return function () {
//       window.activityResultListeners["545"] = function (resultCode, bundle) {
//         console.log("BUNDLE", bundle);
//         var place = JSON.parse(bundle);
//         cb(action(place))();
//       };
//       JBridge.getNearbyPlaces();
//     };
//   };
// };

exports.hideSplash =JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"hide_splash"}))()

// exports["startLocationPollingAPI"] = function (caseId) {
//   return function (baseUrl) {
//     return function (title) {
//       return function (content) {
//         return function (data) {
//           return function(interval){
//             return function () {
//               JBridge.startLocationPollingAPI(caseId, baseUrl, title, content, data, interval);
//             }
//           }
//         }
//       }
//     }
//   }
// }

// exports["stopLocationPollingAPI"] = function () {
//   JBridge.stopLocationPollingAPI()
// }

// exports["removePolyLineById"]= function(id){
//   return function(){
//     JBridge.removePolyLineById(id);
//   }
// }

// exports["removeAllPolylines"] = function(){
//   JBridge.removeAllPolylines();
// }

exports["currentPosition"] = function (str) {
  return function() {
    JBridge.currentPosition(str);
  }
}

exports.toString = function (attr) {
  return JSON.stringify(attr);
};

exports["getTime"] = function (unit){
  return Date.now();
}

exports.toInt = function (val) {
  return parseInt(val);
}

exports["secondsLeft"] = function (time){
  var validity = new Date(time).getTime();
  var now = new Date().getTime();
  if (validity <= now){
    return parseInt(1);
  }else{
    return parseInt((validity - now)/1000);
  }
}

exports["objectToAllocationType"] = function (stringifyObject) {
  var json = JSON.parse(stringifyObject);
  console.log(json);
  return json;
}

exports["parseFloat"] = function (number) {
  return function (decimalDigit) {
      return String(parseFloat(number).toFixed(decimalDigit));
  }
}

exports["storeCallBackTime"] = function (cb) {
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

exports ["setText'"] = function (id) {
  return function (text) {
      return function (){
          setText(id, text, text.length);
      }
  }
} 

exports["launchAppSettings"] = function (unit) {
  return JBridge.launchAppSettings();
};

exports ["shuffle"] = function (array) {
  var shuffled = array
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
  return shuffled
}

exports["setYoutubePlayer"] = function (json) {
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

exports["getTimeStampString"] = function (utcTime){
  var createdDate = new Date(utcTime);
  var result =  moment(new Date()).utc().format();
  var currentDate = new Date(result);
  var diff = (currentDate.getTime() - createdDate.getTime())/ 1000;
  seconds = (Math.round(diff));
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

exports ["addMediaPlayer"] = function (id) {
  return function(source) {
    return function () {
      JBridge.addMediaPlayer(id,source);
    }
  };
};

exports ["removeMediaPlayer"] = function (id) {
  return function () {
    JBridge.removeMediaPlayer();
  };
};

exports["getVideoID"] = function (url) {
  return getVideoId(url);
};

function getVideoId (url) {
  try{
    var ID = '';
    url = url.replace(/(>|<)/gi, '').split(/(vi\/|v=|\/v\/|youtu\.be\/|\/embed\/|\/shorts\/)/);
    if (url[2] !== undefined) {
      ID = url[2].split(/[^0-9a-z_\-]/i);
      ID = ID[0];
    }
    else {
      if (url[1] == /shorts/) {
        ID = url[2];
      }else {
        ID = url;
      }
    }
    return ID;
  }catch (e) {
    console.log("error in getVideoID " + e);
  }
}

exports["getImageUrl"] = function (url) {
  try {
    let videoId = getVideoId(url);
    return ("https://img.youtube.com/vi/" + videoId + "/maxresdefault.jpg");
  }catch (e) {
    console.log("error in getImageUrl " + e);
  }
};