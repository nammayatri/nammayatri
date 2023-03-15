const callbackMapper = require('presto-ui').callbackMapper;
const btnLoaderState = new Map();

// exports._keyStoreEntryPresent = function(alias) {
//   return function() {
//     return JBridge.keyStoreEntryPresent(alias);
//   }
// }

// exports._createKeyStoreEntry = function(keyAlias) {
//   return function(certExpiryDate) {
//     return function(success) {
//       return function(error) {
//         return function() {
//           var successCb = callbackMapper.map(function() {
//             success();
//           })

//           var errorCb = callbackMapper.map(function(e) {
//             error(e)();
//           })

//           JBridge.createKeyStoreEntry(keyAlias, certExpiryDate, successCb, errorCb);
//         }
//       }
//     }
//   }
// }

// exports._deleteKeyStoreEntry = function(keyAlias) {
//   return function() {
//     JBridge.removeKeyFromKeyStore(keyAlias);
//   }
// }

// exports._keyStoreEncrypt = function(keyAlias) {
//   return function(data) {
//     return function(success) {
//       return function(error) {
//         return function() {
//           var successCb = callbackMapper.map(function(cipher) {
//             success(cipher)();
//           })

//           var errorCb = callbackMapper.map(function(e) {
//             error(e)();
//           })

//           JBridge.keyStoreEncrypt(keyAlias, data, successCb, errorCb);
//         }
//       }
//     }
//   }
// }

// exports._keyStoreDecrypt = function(keyAlias) {
//   return function(cipher) {
//     return function(success) {
//       return function(error) {
//         return function() {
//           var successCb = callbackMapper.map(function(data) {
//             success(data)();
//           })

//           var errorCb = callbackMapper.map(function(e) {
//             error(e)();
//           })

//           JBridge.keyStoreDecrypt(keyAlias, cipher, successCb, errorCb);
//         }
//       }
//     }
//   }
// }

// exports._keyStoreSign = function(keyAlias) {
//   return function(data) {
//     return function(success) {
//       return function(error) {
//         return function() {
//           var successCb = callbackMapper.map(function(sign) {
//             success(sign)();
//           })

//           var errorCb = callbackMapper.map(function(e) {
//             error(e)();
//           })

//           JBridge.keyStoreSign(keyAlias, data, successCb, errorCb);
//         }
//       }
//     }
//   }
// }

// exports._getKeyStorePublicKey = function(keyAlias) {
//   return function(success) {
//     return function(error) {
//       return function () {
//         var successCb = callbackMapper.map(function(pubKey) {
//           success(pubKey)();
//         })

//         var errorCb = callbackMapper.map(function(e) {
//           error(e)();
//         })

//         JBridge.getKeyStorePublicKey(keyAlias, successCb, errorCb);
//       };
//     }
//   }
// }

// exports._sendSafetyNetRequest = function(nonce) {
//   return function(apiKey) {
//     return function(success) {
//       return function(error) {
//         return function() {
//           var successCb = callbackMapper.map(function(jws) {
//             success(jws)();
//           })

//           var errorCb = callbackMapper.map(function(e) {
//             error(e)();
//           })
//           var stat = JBridge.sendSafetyNetRequest(nonce, apiKey, successCb, errorCb);
//           if(stat.toString() == "false") {
//             error("SendSafetyNetRequest Failed")(); // Unable to initiate Google pay servies
//           }
//         }
//       }
//     }
//   }
// }

// exports._jweEncrypt = function(plainText) {
//   return function(headers) {
//     return function(keyPath) {
//       return function(left) {
//         return function(right) {
//           return function() {
//             try {
//               var result = JSON.parse(JBridge.jweEncrypt(plainText, headers, keyPath));
//               return (result.error)? left(result.payload) : right(result.payload);
//             } catch(err) {
//               console.error(err);
//               return left("Error during encryption");
//             }
//           }
//         }
//       }
//     }
//   }
// }

// exports._jweDecrypt = function(cipher) {
//   return function(alias) {
//     return function(left) {
//       return function(right) {
//         return function() {
//           try {
//             var result = JSON.parse(JBridge.jweDecrypt(cipher, alias));
//             return (result.error)? left(result.payload.payload) : right(result.payload.payload);
//           } catch(err) {
//             console.error(err);
//             return left("Error during decryption");
//           }
//         }
//       }
//     }
//   }
// }


// exports._jwsSign = function(data) {
//   return function(headers) {
//     return function(alias) {
//       return function(left) {
//         return function(right) {
//           return function() {
//             try {
//               var result = JSON.parse(JBridge.jwsSign(data, headers, alias));
//               return (result.error)? left(result.payload) : right(result.payload);
//             } catch(err) {
//               return left("Error during signing")
//             }
//           }
//         }
//       }
//     }
//   }
// }

// exports._jwsVerify = function(data) {
//   return function(key) {
//     return function() {
//       return JBridge.jwsVerify(data, key);
//     }
//   }
// }

// exports._rsaEncryption = function(data) {
//   return function(algo) {
//     return function(key) {
//       return function(left) {
//         return function(right) {
//           return function() {
//             try {
//               var result = JSON.parse(JBridge.rsaEncryption(data, algo, key))
//               return result.error? left(result.payload) : right(result.payload);
//             } catch(err) {
//               return left("Error during encryption");
//             }
//           }
//         }
//       }
//     }
//   }
// }

exports["requestLocationPermissionDriver"] = function (cb){
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (isLocationPermissionEnabled) {
        cb(action(isLocationPermissionEnabled))();
      });
      return JBridge.requestLocationPermissionDriver(callback);
    };
  };
};

exports._addCertificates = function(str) {
  return function() {
    JBridge.addCertificates(str);
  }
}

exports._isPlayStoreInstalled = function () {
  return JBridge.isPlayStoreInstalled();
}

// exports._safefyNetShutdownSafeBrowsing = function(cb) {
//   return function() {
//       JBridge.shutdownSafeBrowsing();
//       cb("onPause")();
//   }
// }

// exports._safefyNetInitSafeBrowsing = function(cb) {
//   return function() {
//       JBridge.initSafeBrowsing();
//       cb("onResume")();
//   }
// }

exports._isNetworkAvailable = function () {
  return function () {
    return JBridge.isNetworkAvailable();
  }
}

exports._renewFile = function (file) {
  return function () {
    console.log("renew files")
    return JBridge.renewFile(file)
  }
}

exports["closeApp"] = function (str) {
  JBridge.closeApp();
};

exports["readFile'"] = function (filePath) {
  return function () {
    return JBridge.loadFileInDUI(filePath);
  };
};

exports["showLoader'"] = function (str) {
  return JBridge.showLoader(str);
};

exports["isLocationPermissionEnabled"] = function (unit) {
  return function () {
    if (window.__OS == "IOS")
    {
      if (JBridge.isLocationAuthenticationStatusDetermined() == "1") 
      {return true} 
      else {return false };
    }
    return JBridge.isLocationPermissionEnabled();
    
  };
};

exports["getPackageName"] = function () {
  if (window.__OS == "IOS") {
    var sessionDetails = JSON.parse(JBridge.getSessionDetails())
    if (sessionDetails && sessionDetails.packageName) {
      return sessionDetails.packageName;
    } 
    return "";
  } else {
    return JBridge.getPackageName();
  }
};

exports["getVersionCode"] = function () {
  if (window.__OS == "IOS") {
    var sessionDetails = JSON.parse(JBridge.getSessionDetails())
    if (sessionDetails && sessionDetails.appVersion) {
      return parseInt(sessionDetails.appVersion);
    } 
    return 0;
  } else {
    return JBridge.getVersionCode();
  }
};

exports["getVersionName"] = function () {
  if (window.__OS == "IOS") {
    var sessionDetails = JSON.parse(JBridge.getSessionDetails())
    if (sessionDetails && sessionDetails.app_version) {
      return sessionDetails.app_version;
    } 
    return "0";
  } else {
    return JBridge.getVersionName();
  }
};

exports["getManufacturerName"] = function (unit) {
  return JBridge.getManufacturerName();
};

exports["getAndroidVersion"] = function(unit){
  if(window.__OS == "IOS")
  {return 0;}
  else{ 
  return JBridge.getAndroidVersion();}
};

exports["showQrCode'"] = function (id) {
  return function (str) {
    return function (cb) {
      JBridge.showQrCode(id, str);
    };
  };
};

exports["scanQrCode"] = function (requestCode) {
  return function (cb) {
    return function (action) {
      return function () {
        window.activityResultListeners[requestCode] = function (
          resultCode,
          bundle
        ) {
          var result = "";
          try {
            bundle = JSON.parse(bundle);
            result = bundle["SCAN_RESULT"];
          } catch (error) {
            console.error("Couldn't parse bundle on scanQrCode.");
          }
          cb(action(result))();
        };
        return JBridge.scanQrCode();
      };
    };
  };
};

exports["timePicker"] = function (cb) {
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (hour, min) {
        cb(action(hour)(min))();
      });
      return JBridge.timePicker(callback);
    };
  };
};

exports["datePicker"] = function (label)
{return function (cb) 
  {return function (action)
    {return function () {
      var callback = callbackMapper.map(function (year, month, date) {
        cb(action(year)(month)(date))();
      });
      return JBridge.datePicker(callback,label);
      };
    };
  };
};

exports["setFCMToken"] = function (cb) {
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (id) {
        cb(action(id))();
      });
      return JBridge.setFCMToken(callback);
    };
  };
  // JBridge.setFCMToken();
};

exports["dateTimePicker"] = function (cb) {
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (epochTime) {
        cb(action(epochTime))();
      });
      return JBridge.dateTimePicker(callback);
    };
  };
};

exports ["disableActionEditText"] = function (str) {
  return JBridge.disableActionEditText(str);
};

exports["getNearbyPlaces"] = function (cb) {
  return function (action) {
    return function () {
      window.activityResultListeners["545"] = function (resultCode, bundle) {
        console.log("BUNDLE", bundle);
        var place = JSON.parse(bundle);
        cb(action(place))();
      };
      JBridge.getNearbyPlaces();
    };
  };
};

exports["getNearbyPlaces'"] = function (cb) {
  return function (action) {
    return function (just) {
      return function (nothing) {
        return function () {
          window.activityResultListeners["545"] = function (
            resultCode,
            bundle
          ) {
            var place = JSON.parse(bundle);
            place.lat = place.lat ? just(place.lat) : nothing;
            place.lng = place.lng ? just(place.lng) : nothing;
            JBridge.hideKeyboardOnNavigation(true);
            cb(action(place))();
          };
          JBridge.getNearbyPlaces();
        };
      };
    };
  };
};


exports["isNetworkAvailable"] = function (unit) {
  return JBridge.isNetworkAvailable();
};

exports["openUrlInApp"] = function (str) {
  return function (unit) {
    return JBridge.openUrlInApp(str);
  };
};

exports["openUrlInMailApp"] = function (str) {
  return function (unit) {
    return JBridge.openUrlInMailApp(str);
  };
};

exports["addMarker'"] = function (title) {
  return function (lat) {
    return function (lng) {
      return function (markerSize) {
        return function (anchorV) {
          return function (anchorV1) {
            return function () {
              console.log("I AM HERE ------------------");
              JBridge.upsertMarker(title, lat, lng, markerSize, anchorV, anchorV1);
              return true;
            };
          }
        }
      }
    };
  };
};


exports["removeMarker"] = function (title) {
    try{
      return function () {
        console.log("I AM HERE to remove marker------------------");
        JBridge.removeMarker(title);
        return true;
      };
  }
  catch(e)
  { 
    console.log(e);
    console.log("error in removeMarker----------------------------------");
  }
};

exports["parseAddress"] = function (json) {
  return JSON.parse(json);
};

exports["drawRoute"] = function (data) {
  return function (style) {
    return function (trackColor) {
      return function (isActual) {
        return function (sourceMarker) {
          return function (destMarker) {
            return function (polylineWidth) {
              return function (type) {
                return function (sourceName) {
                  return function (destinationName){
                    return function () {
                      console.log("I AM HERE ------------------ IN DRAW ROUTE");
                      return JBridge.drawRoute(JSON.stringify(data), style, trackColor, isActual, sourceMarker, destMarker, polylineWidth,type, sourceName, destinationName);
                      };
                  }
                }
              };
            };
          };
        };
      };
    };
  };
};

exports["updateRoute"] = function (data) {
  return function (destMarker) {
    return function (eta) {
      return function (){
          if (JBridge.updateRoute) {
            var json = JSON.stringify(data);
            try {
              console.log("I AM HERE ------------------ IN UPDATE ROUTE");
              var extension = "";
              return JBridge.updateRoute(json,destMarker,eta,extension);
            } catch (err) {
                console.log("Catch error" + err);
                /*
                * This Function is deprecated on 12 Jan - 2023 
                * Remove this function once it is not begin used.
                */
                return JBridge.updateRoute(json,destMarker,eta);
            }
          }
        };
      };
    };
  };


exports["isCoordOnPath"] = function (data) {
  return function (lat) {
    return function (lon) {
      return function (speed) {
        return function () {
            if (JBridge.isCoordOnPath) {
              var json = JSON.stringify(data);
              try {
                console.log("I AM HERE ------------------ IN CHECK ROUTE");
                var res = JBridge.isCoordOnPath(json,lat,lon, speed);
                return JSON.parse(res);
              } catch (err) {
                console.log("Catch error" + err);
                /*
                * This Function is deprecated on 12 Jan - 2023 
                * Remove this function once it is not begin used.
                */
                return JBridge.isCoordOnPath(json,lat,lon);
              }
            }
          };
        };
      };
    };
  };


exports["addMarkers"] = function (data) {
  JBridge.addMarkers(JSON.stringify(data));
};

exports["removePolyLine"] = function (str) {
  JBridge.removePolyLine(str);
};

exports["requestLocation"] = function (str) {
  JBridge.requestLocation();
};

exports["initiateLocationServiceClient"] = function (){
  if (window.__OS == "IOS"){
    return true;
  }else{
    JBridge.initiateLocationServiceClient();
  }
}

exports["checkOverlayPermission"] = function (str) {
  return function () {
    JBridge.checkOverlayPermission();
  };
};

exports["reallocateMapFragment"] = function (str) {
  return function () {
    JBridge.reallocateMapFragment(str);
  };
};

exports["requestAutoStartPermission"] = function (str) {
  return function () {
    JBridge.requestAutoStartPermission();
  };
};

exports["showMap'"] = function (id) {
  return function (isEnableCurrentLocation) {
    return function (type) {
      return function (zoom) {
        return function (cb) {
          return function (action) {
            return function () {
              var callback = callbackMapper.map(function (key, lat, lon) {
                console.log("in show map",action);
                window.x = cb;
                window.y = action;
                cb(action (key) (lat) (lon))();
              });
              JBridge.showMap(id, isEnableCurrentLocation, type, zoom, callback);
              return true;
            };
          };
        }
      };
    };
  };
};

exports["getCurrentLatLong"] = function () {
  if (JBridge.getCurrentLatLong) {
      var parsedData = JSON.parse(JBridge.getCurrentLatLong());
      if (parsedData.lat && parsedData.lng) {
        return parsedData;
      } else { // fallBack for previous release
        return {
          "lat" : parsedData.lat
        , "lng" : parsedData.long
        }
      }
  }
};

exports["isLocationEnabled"] = function (unit) {
  return function () {
    if (window.__OS == "IOS")
    {
      if(JBridge.isLocationEnabled() == "1")
      return true;
      else return false;
    }
    return JBridge.isLocationEnabled();
  };
};

exports["getCurrentPosition"] = function (cb) {
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (lat, lng) {
        cb(action(lat)(lng))();
      });
      return JBridge.getCurrentPosition(callback);
    };
  };
};

exports["openNavigation"] = function (slat) {
  return function (slong) {
    return function (dlat) {
      return function (dlong) {
        return JBridge.openNavigation(slat, slong, dlat, dlong);
      };
    };
  };
};

exports["animateCamera"] = function (lat) {
  return function (lng) {
    return function (zoom) {
      return function () {
        JBridge.animateCamera(lat, lng, zoom);
      };
    };
  };
};


exports["moveCamera"] = function (lat1) {
  return function (lng1) {
    return function (lat2) {
      return function (lng2){
        return function () {
          JBridge.moveCamera(lat1, lng1, lat2,lng2);
        };
      };
    };
  };
};

// exports["hideLoader'"] = function () {
//   return JBridge.hideLoader();
// };

exports["minimizeApp"] = function (str) {
  JBridge.minimizeApp();
};
exports["toast"] = function (str) {
  JBridge.toast(str);
};

exports["firebaseLogEventWithParams"] = function (event) {
  return function (paramKey) {
    return function (paramValue) {
      return function () {
        if(JBridge.firebaseLogEventWithParams){
            JBridge.firebaseLogEventWithParams(event, paramKey,paramValue);
        }
    };
  };
 };
};

exports["firebaseLogEventWithTwoParams"] = function (event) {
  return function (paramKey1) {
    return function (paramValue1) {
      return function (paramKey2) {
        return function (paramValue2) {
          return function () {
            if (JBridge.firebaseLogEventWithTwoParams) {
                JBridge.firebaseLogEventWithTwoParams(event, paramKey1,paramValue1,paramKey2,paramValue2);
            }
          };
        };
      };
    };
  };
};

exports["firebaseLogEvent"] = function (str) {
  if (JBridge.firebaseLogEvent){
      JBridge.firebaseLogEvent(str);
  }
};

exports["hideKeyboardOnNavigation"] = function (permission) {
  JBridge.hideKeyboardOnNavigation(permission);
};

exports.onEvent = function (payload) {
  return function () {
    window.temp = payload;
    JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), "");
  };
};

exports._onEventWithCB = function (payload) {
  return function (succ) {
    return function (err) {
      return function () {
        var cb = callbackMapper.map(function (res) {
          try {
            var result = JSON.parse(res);
            if (result.status == "onResponse") {
              succ(result.payload)();
            } else {
              err(result.payload)();
            }
          } catch (err) {
            console.error(err);
            succ(res)();
          }
        });
        JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), cb);
      };
    };
  };
};

// exports.getSessionInfo = JSON.parse(JBridge.getDeviceInfo());

exports.getKeyInSharedPrefKeys = function (key) {
  return JBridge.getKeysInSharedPrefs(key);
};

exports.getKeyInNativeSharedPrefKeys = function (key) {
  if (window.__OS == "IOS") {
    return JBridge.getKeysInSharedPrefs(key);
  } else { 
    return JBridge.getKeyInNativeSharedPrefKeys(key);
  }
};

exports["setKeyInSharedPrefKeys'"] = function (key) {
  return function (value) {
    JBridge.setKeysInSharedPrefs(key, value);
  };
};

exports["setEnvInNativeSharedPrefKeys'"] = function (key) {
  return function (value) {
    if (window.__OS == "IOS") {
      JBridge.setKeysInSharedPrefs(key, value);
    } else { 
      JBridge.setEnvInNativeSharedPrefKeys(key, value);
    }
  };
};

// exports.setKeyInSharedPrefKeyss = function (key) {
//   return function (value) {
//     return JBridge.setKeysInSharedPrefs(key, value);
//   };
// };

exports.removeKeysInSharedPrefs = function (key) {
  if (window.__OS == "IOS") {
    return JBridge.removeFromSharedPrefs(key);
  } else { 
    return JBridge.removeKeysInSharedPrefs(key);
  }
};

exports.removeKeysInNativeSharedPrefs = function (key) {
  if (window.__OS == "IOS") {
    JBridge.removeFromSharedPrefs(key);
  } else { 
    JBridge.removeKeysInNativeSharedPrefs(key);
  }
};

exports["toggleLoader'"] = function (showLoader) {
  return function () {
    return JBridge.toggleLoader(showLoader);
  };
};

exports["loaderText'"] = function (mainTxt) {
  return function (subTxt) {
    return function () {
      return JBridge.loaderText(mainTxt, subTxt);
    };
  };
};


exports["showDialer"] = function (str) {
  JBridge.showDialer(str);
};

exports["startLocationPollingAPI"] = function () {
                  return JBridge.startLocationPollingAPI();
}

exports["generatePDF"] = function (state) {
  return function (type) {
    var data = JSON.stringify(state)
    JBridge.generatePDF(data, type);
    return true;
  };
};

exports["stopLocationPollingAPI"] = function () {
  JBridge.stopLocationPollingAPI()
}

exports["removeAllPolylines"] = function(str){
  JBridge.removeAllPolylines(str);
}

exports["currentPosition"] = function (str) {
    JBridge.currentPosition(str);
}

exports["firebaseScreenNameLog"] = function (str) {
  if(JBridge.firebaseScreenNameLog){
      JBridge.firebaseScreenNameLog(str);
  }
};
exports["firebaseUserID"] = function (str) {
  if(JBridge.firebaseUserID) {
      JBridge.firebaseUserID(str);
  }
};

exports["storeCallBackDriverLocationPermission"] = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (isLocationPermissionGranted) {
              cb(action (isLocationPermissionGranted))();
          });
          console.log("In storeCallBackDriverLocationPermission ---------- + " + action);
          JBridge.storeCallBackDriverLocationPermission(callback);
      }    
  }}
  catch (error){
      console.log("Error occurred in storeCallBackDriverLocationPermission ------", error);
  }
}

exports["storeCallBackInternetAction"] = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (isNetworkOn) {
              cb(action (isNetworkOn))();
          });
          console.log("In storeCallBackInternetAction ---------- + " + action);
          JBridge.storeCallBackInternetAction(callback);
      }    
  }}
  catch (error){
      console.log("Error occurred in storeCallBackInternetAction ------", error);
  }
}

exports["storeCallBackImageUpload"] = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (imgStr, imageName) {
              cb(action (imgStr)(imageName))();
          });
          JBridge.storeCallBackImageUpload(callback);
      }    
  }}
  catch (error){
      console.log("Error occurred in storeCallBackImageUpload ------", error);
  }
}

exports["storeCallBackOverlayPermission"] = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (isOverlayPermission) {
              cb(action (isOverlayPermission))();
          });
          console.log("In storeCallBackOverlapPermission ---------- + " + action);
          JBridge.storeCallBackOverlayPermission(callback);
      }    
  }}
  catch (error){
      console.log("Error occurred in storeCallBackOverlapPermission ------", error);
  }
}

exports["storeCallBackBatteryUsagePermission"] = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (isPermissionEnabled) {
              cb(action (isPermissionEnabled))();
          });
          console.log("In storeCallBackBatteryUsagePermission ---------- + " + action);
          JBridge.storeCallBackBatteryUsagePermission(callback);
      }    
  }}
  catch (error){
      console.log("Error occurred in storeCallBackBatteryUsagePermission ------", error);
  }
}

exports["isBatteryPermissionEnabled"] = function (unit) {
  return function () {
    return JBridge.isBatteryPermissionEnabled();
  };
};

exports["getAAID"] = function (str) {
  console.log("HERE IN getAAID ===--->>")
  return JBridge.getAAID();
}

exports["isInternetAvailable"] = function (unit) {
  return function () {
    if (window.__OS == "IOS")
      {if (JBridge.isNetworkAvailable() == "1") return true;
    else return false;}
    else return JBridge.isInternetAvailable();
  };
};

exports["factoryResetApp"] = function (str) {
  console.log("HERE IN RESET ===--->>")
  JBridge.factoryResetApp()
}

exports["uploadFile"] = function (unit) {
  return function () {
    return JBridge.uploadFile();
  };
};

exports["previewImage"] = function (base64Image) {
  return function () {
    return JBridge.previewImage(base64Image);
  }
}

exports["renderBase64Image"] = function (image) {
  return function (id) {
      return JBridge.renderBase64Image(image, id);
  };
};

exports["isOverlayPermissionEnabled"] = function (unit) {
  return function () {
    return JBridge.isOverlayPermissionEnabled();
  };
};

exports["requestBatteryPermission"] = function (str) {
  return function () {
    JBridge.requestBatteryPermission();
  };
};
exports["copyToClipboard"] = function(str) {
     JBridge.copyToClipboard(str);
}

exports["requestKeyboardShow"] = function(id) {
  JBridge.requestKeyboardShow(id);
}

exports["locateOnMap"] = function(str){
  return function (lat){
    return function (lon){
      JBridge.locateOnMap(str, lat, lon);
    }
  }
}

exports["exitLocateOnMap"] = function(str){
  JBridge.exitLocateOnMap(str);
}

exports["shareTextMessage"] = function(str){
  return function (message){
    if(JBridge.shareTextMessage){
      JBridge.shareTextMessage(str, message);
    }
  }
}

exports["shareImageMessage"] = function(message){
  return function (imageName){
    if(JBridge.shareTextMessage){
      JBridge.shareImageMessage(message,imageName);
    }
  }
}

exports.openWhatsAppSupport = function (contactNumber) {
  return function () {
    console.log("contactNumber" + contactNumber)
    return JBridge.openWhatsAppSupport(contactNumber)
  }
}

exports["mapSnapShot"] = function (id) {
  return function (coordinates) {
    return function (routeType) {
      return function (actualRoute) {
        return function (cb) {
          return function (action) {
            return function () {
              var callback = callbackMapper.map(function (encImage) {
                console.log("in show map", action);
                window.x = cb;
                window.y = action;
                cb(action(encImage))();
              });
              JBridge.mapSnapShot(id, JSON.stringify(coordinates), routeType, actualRoute, callback);
              return true;
            };
          };
        };
      };
    };
  };
};


exports["setStoreCallBackPopUp"] = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (stringifyPayload) {
              cb(action (stringifyPayload))();
          });
          JBridge.setStoreCallBackPopUp(callback);
      }    
  }}
  catch (error){
      console.log("Error occurred in setStoreCallBackPopUp", error);
  }
}

exports["deletePopUpCallBack"] = function (dummy) {
  console.log("jbridge deletepopupcallback before");
  JBridge.deletePopUpCallBack(dummy);
  return true;
}

exports ["startLottieProcess"] = function (rawJson) {
  return function (lottieId) {
    return function (loop) {
      return function (speed) {
        return function (scaleType){
          if (JBridge.startLottieProcess) {
            try {
              return JBridge.startLottieProcess(rawJson,lottieId, loop, speed, scaleType);
            } catch (err) {
              /*
              * This Function is deprecated on 12 Jan - 2023 
              * Remove this function once it is not begin used.
              */
              return JBridge.startLottieProcess(rawJson,lottieId, loop, speed);
            }
          }
        };
      };
    };
  };
};

exports["generateSessionToken"] = function (type) {
  if (window.__OS == "IOS"){
    return "d4faebc6-2f98-44a0-957e-20cb4646c013";
  }
  return JBridge.generateSessionToken(type);
}

exports["enableMyLocation"] = function(isEnableCurrentLocation){
  JBridge.enableMyLocation(isEnableCurrentLocation);
}

exports["toggleBtnLoader"] = function(id){
  return function (val) {
      if (val == true) {
        btnLoaderState.set(id,true);
      } else {
        btnLoaderState.clear();
      }
  };
};

exports["getBtnLoader"] = function(val){
    return (btnLoaderState.get(val) == true)? true : false;
};

exports["launchInAppRatingPopup"] = function (unit) {
  if (JBridge.launchInAppRatingPopup) {
    JBridge.launchInAppRatingPopup();
  }
};

exports ["startTimerWithTime"] = function (time) {
  return function (qouteId) {
    return function (interval) {
      return function (cb) {
        return function (action) {
          var callback = callbackMapper.map(function (seconds,quoteID,status,timerID) {
            cb(action(seconds)(quoteID)(status)(timerID))();
          });
          if (JBridge.startCountDownTimerWithTime)  {
            return JBridge.startCountDownTimerWithTime(time, interval, qouteId, callback);}
        }
      }
    }
  }
}
