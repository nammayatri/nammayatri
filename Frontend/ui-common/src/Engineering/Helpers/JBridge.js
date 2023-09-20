import { callbackMapper } from 'presto-ui';
const btnLoaderState = new Map();
const { JBridge } = window;
var mainFiber = null;
let suggestions = require("../Engineering.Helpers.Suggestions")
var timer;
let locationPollingTimer;
const locationUpdateServiceName = "in.juspay.mobility.app.LocationUpdateService";

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

export const requestLocationPermissionDriver = function (cb){
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (isLocationPermissionEnabled) {
        cb(action(isLocationPermissionEnabled))();
      });
      return window.JBridge.requestLocationPermissionDriver(callback);
    };
  };
};

export const _addCertificates = function(str) {
  return function() {
    window.JBridge.addCertificates(str);
  }
}

export const _isPlayStoreInstalled = function () {
  return window.JBridge.isPlayStoreInstalled();
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

export const _isNetworkAvailable = function () {
  return function () {
    return window.JBridge.isNetworkAvailable();
  }
}

export const _renewFile = function (file) {
  return function () {
    console.log("renew files")
    return window.JBridge.renewFile(file)
  }
}

export const closeApp = function (str) {
  window.JBridge.closeApp();
};

export const readFileImpl = function (filePath) {
  return function () {
    return window.JBridge.loadFileInDUI(filePath);
  };
};

export const showLoaderImpl = function (str) {
  return window.JBridge.showLoader(str);
};

export const isLocationPermissionEnabled = function (unit) {
  return function () {
    if (window.__OS == "IOS")
    {
      if (window.JBridge.isLocationAuthenticationStatusDetermined() == "1")
      {return true}
      else {return false };
    }
    return window.JBridge.isLocationPermissionEnabled();

  };
};

export const isMicrophonePermissionEnabled = function (unit) {
    return window.JBridge.isMicrophonePermissionEnabled();
};

export const getPackageName = function () {
  if (window.__OS == "IOS") {
    var sessionDetails = JSON.parse(window.JBridge.getSessionDetails())
    if (sessionDetails && sessionDetails.packageName) {
      return sessionDetails.packageName;
    }
    return "";
  } else {
    return window.JBridge.getPackageName();
  }
};

export const getVersionCode = function () {
  if (window.__OS == "IOS") {
    var sessionDetails = JSON.parse(window.JBridge.getSessionDetails())
    if (sessionDetails && sessionDetails.appVersion) {
      return parseInt(sessionDetails.appVersion);
    }
    return 0;
  } else {
    return window.JBridge.getVersionCode();
  }
};

export const getVersionName = function () {
  if (window.__OS == "IOS") {
    var sessionDetails = JSON.parse(window.JBridge.getSessionDetails())
    if (sessionDetails && sessionDetails.app_version) {
      return sessionDetails.app_version;
    }
    return "0";
  } else {
    return window.JBridge.getVersionName();
  }
};

export const getManufacturerName = function (unit) {
  return window.JBridge.getManufacturerName();
};

export const getAndroidVersion = function(unit){
  if(window.__OS == "IOS")
  {return 0;}
  else{
  return window.JBridge.getAndroidVersion();}
};

export const showQrCodeImpl = function (id) {
  return function (str) {
    return function (cb) {
      window.JBridge.showQrCode(id, str);
    };
  };
};

export const scanQrCode = function (requestCode) {
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

export const timePicker = function (cb) {
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (hour, min) {
        cb(action(hour)(min))();
      });
      return window.JBridge.timePicker(callback);
    };
  };
};

export const datePicker = function (label)
{return function (cb)
  {return function (action)
    {return function () {
      var callback = callbackMapper.map(function (resp, year, month, date) {
        cb(action(resp)(year)(month)(date))();
      });
      return window.JBridge.datePicker(callback,label);
      };
    };
  };
};

export const setFCMToken = function (cb) {
  return function (action) {
    return function () {
      if (window.JBridge.setFCMToken) {
        var callback = callbackMapper.map(function (id) {
          cb(action(id))();
        });
        return window.JBridge.setFCMToken(callback);
      }
    };
  };
  // JBridge.setFCMToken();
};

export const setFCMTokenWithTimeOut = function (timeOut) {
  return function (cb){
    return function (action) {
      return function () {
        if (window.JBridge.setFCMToken) {
          var timer;
          var callback = callbackMapper.map(function (id) {
            clearTimeout(timer);
            cb(action(id))();
          });
          var timeOutCallBack = function ()
          {
              cb(action("NOT_FOUND"))();
          };
          timer = setTimeout(timeOutCallBack,timeOut);
          return window.JBridge.setFCMToken(callback);
        }
     };
   };
  // JBridge.setFCMToken();
  };
};

export const dateTimePicker = function (cb) {
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (epochTime) {
        cb(action(epochTime))();
      });
      return window.JBridge.dateTimePicker(callback);
    };
  };
};

export const disableActionEditText = function (str) {
  return window.JBridge.disableActionEditText(str);
};

export const getNearbyPlaces = function (cb) {
  return function (action) {
    return function () {
      window.activityResultListeners["545"] = function (resultCode, bundle) {
        console.log("BUNDLE", bundle);
        var place = JSON.parse(bundle);
        cb(action(place))();
      };
      window.JBridge.getNearbyPlaces();
    };
  };
};

export const getNearbyPlaces2 = function (cb) {
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


export const isNetworkAvailable = function (unit) {
  return window.JBridge.isNetworkAvailable();
};

export const openUrlInApp = function (str) {
  return function (unit) {
    return window.JBridge.openUrlInApp(str);
  };
};

export const openUrlInMailApp = function (str) {
  return function (unit) {
    if (window.JBridge.openUrlInMailApp) {
      return window.JBridge.openUrlInMailApp(str);
    }
    return;
  };
};

export const addMarkerImpl = function (title) {
  return function (lat) {
    return function (lng) {
      return function (markerSize) {
        return function (anchorV) {
          return function (anchorV1) {
            return function () {
              console.log("I AM HERE ------------------");
              window.JBridge.upsertMarker(title, lat, lng, markerSize, anchorV, anchorV1);
              return true;
            };
          }
        }
      }
    };
  };
};


export const removeMarker = function (title) {
  try {
    window.JBridge.removeMarker(title);
    return true;
  } catch (e) {
    console.log(e);
    console.log("error in removeMarker----------------------------------", e);
  }
};

export const parseAddress = function (json) {
  return JSON.parse(json);
};

export const drawRoute = function (data) {
  return function (style) {
    return function (trackColor) {
      return function (isActual) {
        return function (sourceMarker) {
          return function (destMarker) {
            return function (polylineWidth) {
              return function (type) {
                return function (sourceName) {
                  return function (destinationName) {
                    return function (specialLocation) {
                      return function () {
                        console.log("I AM HERE ------------------ IN DRAW ROUTE");
                        try {
                          return window.JBridge.drawRoute(JSON.stringify(data), style, trackColor, isActual, sourceMarker, destMarker, polylineWidth, type, sourceName, destinationName, JSON.stringify(specialLocation));
                        } catch (err) {
                          console.log("Catch error" + err);
                          return window.JBridge.drawRoute(JSON.stringify(data), style, trackColor, isActual, sourceMarker, destMarker, polylineWidth, type, sourceName, destinationName);
                        }
                      };
                    }
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

export const updateRouteMarker = function (data) {
  return function () {
    if (window.JBridge.updateRouteMarker) {
      return window.JBridge.updateRouteMarker(JSON.stringify(data));
    }
  }
}

export const updateRoute = function (data) {
  return function (destMarker) {
    return function (eta) {
      return function (srcMarker){
        return function (specialLocation) {
          return function () {
            if (window.JBridge.updateRoute) {
              var json = JSON.stringify(data);
              try{
                console.log("I AM HERE ------------------ IN UPDATE ROUTE");
                return window.JBridge.updateRoute(json, destMarker, eta, srcMarker, JSON.stringify(specialLocation));
              }catch (err){
                console.log("Catch error" + err);
                return window.JBridge.updateRoute(json, destMarker, eta, JSON.stringify(specialLocation));
              }
            }
          }
        };
      };
    };
  };
};

export const storeCallBackMessageUpdated = function (cb) {
  return function (chatChannelID) {
    return function (chatUserId) {
      return function (action) {
        return function () {
          var callback = callbackMapper.map(function (message, sentBy, timeStamp, messagesSize) {
            if (messagesSize == undefined) {
              messagesSize = "-1"
            }
            let decodedMessage = suggestions.getMessageFromKey(message)(getKeyInSharedPrefKeys("LANGUAGE_KEY"))
            let messageObj = { "message": decodedMessage, "sentBy": sentBy, "timeStamp": timeStamp, type: "Text", delay: 0 }
            window.chatMessages = window.chatMessages || [];
            window.chatMessages.push(messageObj);
            if (window.chatMessages.length - 1 == messagesSize || messagesSize === "-1") {
              cb(action(message)(sentBy)(timeStamp)(messagesSize))();
            }
          });
          window.storeCallBackMessageUpdated = callback;
          if (JBridge.storeCallBackMessageUpdated) {
            JBridge.storeCallBackMessageUpdated(chatChannelID, chatUserId, callback);
          }
        };
      };
    };
  };
};

export const getChatMessages = function(string) {
    return [].concat(window.chatMessages !== undefined ? window.chatMessages : []);
}

export const dateCallback = function (cb, action) {
    var callback = function (){
      cb(action)();
    }
    window.dateCallback = callback;
  };

export const unregisterDateAndTime = function () {
    window.dateCallback = undefined;
};

export const storeCallBackOpenChatScreen = function (cb) {
    return function(action) {
        return function (){
            var callback = callbackMapper.map(function(){
              cb(action)();
            });
            var openChatScreen = function(){
              cb(action)();
            };
            window.openChatScreen = openChatScreen;
            if(window.JBridge.storeCallBackOpenChatScreen) {
              window.JBridge.storeCallBackOpenChatScreen(callback);
            }
        };
      };
  };

export const scrollOnResume = function (cb) {
  return function (action) {
    try {
      var callback = function () {
        cb(action)();
      }
      var scroll = function () {
        if(getKeyInSharedPrefKeys("LOCAL_STAGE") === "ChatWithCustomer" || getKeyInSharedPrefKeys("LOCAL_STAGE") === "ChatWithDriver") {
           setTimeout(callback, 500);
        }
      }
      window.scrollAction = scroll;
    } catch (e) {
      console.error("Error in scrollOnResume : " + e);
    }
  }
}

export const startChatListenerService = function() {
  if (JBridge.startChatListenerService) {
    JBridge.startChatListenerService();
  }
}

export const stopChatListenerService = function () {
  if (JBridge.stopChatListenerService) {
    window.chatMessages = undefined;
    JBridge.stopChatListenerService();
  }
}

export const sendMessage = function (message) {
  if (JBridge.sendMessage) {
    if(timer) clearTimeout(timer);
    const fn = function () {
      return JBridge.sendMessage(message);
    }
    timer = setTimeout(fn, 200);
  }
};

export const scrollToEnd = function (id) {
  return function (bottom) {
    if (JBridge.scrollToEnd) {
      JBridge.scrollToEnd(id, bottom);
    }else if (JBridge.scrollToBottom && bottom) {
      JBridge.scrollToBottom(id);
    }
  }
}

export const saveSuggestions = function(key){
  return function (suggestions) {
    try {
      let convertedJSON = {};
      if (!Array.isArray(suggestions)) {
        return;
      } else {
        suggestions.map(item => convertedJSON[item.key] = item.value);
      }
      window.suggestions = convertedJSON;
      JBridge.setKeysInSharedPrefs(key,JSON.stringify(convertedJSON));
    } catch (error) {
      console.error("Error in saveSuggestions " + error);
    }
  }
}

export const saveSuggestionDefs = function(key){
  return function (suggestionDefs) {
    try {
      let convertedJSON = {};
      if (!Array.isArray(suggestionDefs)) {
        return;
      } else {
        suggestionDefs.map(item => convertedJSON[item.key] = item.value);
      }

      window.suggestionsDefs = convertedJSON;
      JBridge.setKeysInSharedPrefs(key,JSON.stringify(convertedJSON));
    } catch (error) {
      console.error("Error in saveSuggestionDefs " + error);
    }
  }
}

export const getSuggestionsfromLocal = function(key) {
    try {
      if(!window.suggestions) {
        window.suggestions = JSON.parse(getKeyInSharedPrefKeys("SUGGESTIONS"));
      }
      let suggestions = window.suggestions;
      let keys = suggestions[key];
      if(keys) {
        return keys;
      };
      return [];
    } catch (error) {
      console.error("Error in getSuggestionsfromKey " + error);
      return ["error"];
    }
  }

export const getSuggestionfromKey = function (key) {
  return function (language) {
    try {
      if (!window.suggestionsDefs) {
        window.suggestionsDefs = JSON.parse(JBridge.getFromSharedPrefs("SUGGESTIONS_DEFINITIONS"));
      }
      let suggestionsDefs = window.suggestionDefs;
      let val = suggestionsDefs[key];
      let suggestion = "";
      if (val) {
        switch(language) {
          case "EN_US" :
            suggestion = val[en_us];
            break;
          case "HI_IN" :
            suggestion = val[hi_in];
            break;
          case "KN_IN" :
            suggestion = val[kn_in];
            break;
          case "BN_IN" :
            suggestion = val[bn_in];
            break;
          case "ML_IN" :
            suggestion = val[ml_in];
            break;
          case "TA_IN" :
            suggestion = val[ta_in];
            break;
          default :
            suggestion = val[en_us];
            break;
        }
        return suggestion;
      } else
        return key;
    } catch (error) {
      console.error("Error in getSuggestionfromKey : " + error);
      return "";
    }
}
};

export const addMediaFile = function (viewID) {
  return function (source) {
      return function (actionButtonID) {
          return function (playIcon) {
              return function (pauseIcon) {
                  return function (timerID) {
                      return function () {
                          JBridge.addMediaFile(viewID, source, actionButtonID, playIcon, pauseIcon, timerID);
                      }
                  }
              }
          }
      }
  }
}

export const isCoordOnPath = function (data) {
  return function (lat) {
    return function (lon) {
      return function (speed) {
        return function () {
            if (window.JBridge.isCoordOnPath) {
              var json = JSON.stringify(data);
              try {
                console.log("I AM HERE ------------------ IN CHECK ROUTE");
                var res = window.JBridge.isCoordOnPath(json,lat,lon, speed);
                return JSON.parse(res);
              } catch (err) {
                console.log("Catch error" + err);
                /*
                * This Function is deprecated on 12 Jan - 2023
                * Remove this function once it is not begin used.
                */
                return window.JBridge.isCoordOnPath(json,lat,lon);
              }
            }
          };
        };
      };
    };
  };


export const addMarkers = function (data) {
  window.JBridge.addMarkers(JSON.stringify(data));
};

export const removePolyLine = function (str) {
  window.JBridge.removePolyLine(str);
};

export const requestLocation = (unit) => () => {
  window.JBridge.requestLocation();
};

export const initiateLocationServiceClient = function (){
  if (window.__OS == "IOS"){
    return true;
  }else{
    window.JBridge.initiateLocationServiceClient();
  }
}

export const checkOverlayPermission = function (str) {
  return function () {
    window.JBridge.checkOverlayPermission();
  };
};

export const reallocateMapFragment = function (str) {
  return function () {
    window.JBridge.reallocateMapFragment(str);
  };
};

export const requestAutoStartPermission = function (str) {
  return function () {
    window.JBridge.requestAutoStartPermission();
  };
};

export const showMapImpl = function (id) {
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
              window.JBridge.showMap(id, isEnableCurrentLocation, type, zoom, callback);
              return true;
            };
          };
        }
      };
    };
  };
};

export const getCurrentLatLong = function () {
  if (window.JBridge.getCurrentLatLong) {
      var parsedData = JSON.parse(window.JBridge.getCurrentLatLong());
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


export const isLocationEnabled = function (unit) {
  return function () {
    if (window.__OS == "IOS")
    {
      if(window.JBridge.isLocationEnabled() == "1")
      return true;
      else return false;
    }
    return window.JBridge.isLocationEnabled();
  };
};

export const isMockLocation = function (cb) {
  return function (action) {
    return function () {
      if (window.JBridge.isMockLocation) {
        console.log("IsMockLocationIsMockLocation");
        var callback = callbackMapper.map(function (lng) {
          cb(action(lng))();
        });
        return window.JBridge.isMockLocation(callback);
      }
    };
  };
};

export const getCurrentPosition = function (cb) {
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (lat, lng) {
        cb(action(lat)(lng))();
      });
      return window.JBridge.getCurrentPosition(callback);
    };
  };
};

export const getCurrentPositionWithTimeout = function (cb){
    return function (action){
      return function (delay){
        return function () {
          var callbackFallback = function (){
            cb(action("0.0")("0.0"))();
          };
          var timer = setTimeout(callbackFallback, delay);
          var callback = callbackMapper.map(function (lat, lng) {
            clearTimeout(timer);
            cb(action(lat)(lng))();
          });
          window.JBridge.getCurrentPosition(callback);
        }
      }
    }
  }

export const openNavigation = function (slat) {
  return function (slong) {
    return function (dlat) {
      return function (dlong) {
        return function (mode) {
          if (window.appConfig && window.appConfig.navigationAppConfig && window.JBridge.openNavigationWithQuery) {
            if(window.__OS == "IOS"){
              let query = mode == "WALK" ? window.appConfig.navigationAppConfig.ios.walkQuery : window.appConfig.navigationAppConfig.ios.query;
              return window.JBridge.openNavigationWithQuery(dlat, dlong, query);
            }else{
              let query = mode == "WALK" ? window.appConfig.navigationAppConfig.android.walkQuery : window.appConfig.navigationAppConfig.android.query;
              let packageName = window.appConfig.navigationAppConfig.android.packageName;
              return window.JBridge.openNavigationWithQuery(dlat, dlong, query, packageName);
            }
          } else {
            // deprecated 
            return window.JBridge.openNavigation(slat, slong, dlat, dlong);
          }
        };
      };
    };
  };
};

export const animateCamera = function (lat) {
  return function (lng) {
    return function (zoom) {
      return function (zoomType) {
        return function () {
          try {
            window.JBridge.animateCamera(lat, lng, zoom, zoomType);
          } catch (err) {
            window.JBridge.animateCamera(lat, lng, zoom);
          }
        };
      };
    };
  };
};


export const moveCamera = function (lat1) {
  return function (lng1) {
    return function (lat2) {
      return function (lng2){
        return function () {
          window.JBridge.moveCamera(lat1, lng1, lat2,lng2);
        };
      };
    };
  };
};

// exports["hideLoader'"] = function () {
//   return JBridge.hideLoader();
// };

export const minimizeApp = function (str) {
  window.JBridge.minimizeApp();
};
export const toast = function (str) {
  if (window.__OS == "IOS")
    window.JBridge.toast(str); //remove once toast is fixed in iOS.
    else
      if(window.JBridge.toaster)
        window.JBridge.toaster(str);
      else
        window.JBridge.toast(str);
};

export const firebaseLogEventWithParams = function (event) {
  return function (paramKey) {
    return function (paramValue) {
      return function () {
        if(window.JBridge.firebaseLogEventWithParams){
            window.JBridge.firebaseLogEventWithParams(event, paramKey,paramValue);
        }
    };
  };
 };
};

export const firebaseLogEventWithTwoParams = function (event) {
  return function (paramKey1) {
    return function (paramValue1) {
      return function (paramKey2) {
        return function (paramValue2) {
          return function () {
            if (window.JBridge.firebaseLogEventWithTwoParams) {
                window.JBridge.firebaseLogEventWithTwoParams(event, paramKey1,paramValue1,paramKey2,paramValue2);
            }
          };
        };
      };
    };
  };
};

export const firebaseLogEvent = function (str) {
  return function () {
    if (window.JBridge.firebaseLogEvent){
        window.JBridge.firebaseLogEvent(str);
    }
  }
};

export const metaLogEvent = function (event) {
  if(window.JBridge.metaLogEvent) {
    window.JBridge.metaLogEvent(event);
  }
}

export const metaLogEventWithParams = function (event) {
  return function (paramKey) {
    return function (paramValue) {
      return function () {
        if(window.JBridge.metaLogEventWithParams){
            window.JBridge.metaLogEventWithParams(event, paramKey,paramValue);
        }
    };
  };
 };
};

export const metaLogEventWithTwoParams = function (event) {
  return function (paramKey1) {
    return function (paramValue1) {
      return function (paramKey2) {
        return function (paramValue2) {
          return function () {
            if (window.JBridge.metaLogEventWithTwoParams) {
                window.JBridge.metaLogEventWithTwoParams(event, paramKey1,paramValue1,paramKey2,paramValue2);
            }
          };
        };
      };
    };
  };
};

export const hideKeyboardOnNavigation = function (permission) {
  if (permission)
    window.JBridge.hideKeyboardOnNavigation(permission);
};

export const onEvent = function (payload) {
  return function () {
    window.temp = payload;
    window.JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), "");
  };
};

export const _onEventWithCB = function (payload) {
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
        window.JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), cb);
      };
    };
  };
};

// exports.getSessionInfo = JSON.parse(JBridge.getDeviceInfo());

export const getKeyInSharedPrefKeys = function (key) {
    return JBridge.getFromSharedPrefs(key);
};

export const getKeyInNativeSharedPrefKeys = function (key) {
    return JBridge.getFromSharedPrefs(key);
};

export const setKeyInSharedPrefKeysImpl = function (key) {
  return function (value) {
    return JBridge.setInSharedPrefs(key, value);
  };
};

export const setEnvInNativeSharedPrefKeysImpl = function (key) {
  return function (value) {
    return JBridge.setInSharedPrefs(key, value);
  };
};

// exports.setKeyInSharedPrefKeyss = function (key) {
//   return function (value) {
//     return JBridge.setKeysInSharedPrefs(key, value);
//   };
// };

export const removeKeysInSharedPrefs = function (key) {
    return JBridge.removeDataFromSharedPrefs(key);
};

export const removeKeysInNativeSharedPrefs = function (key) {
  return JBridge.removeDataFromSharedPrefs(key);
};

export const toggleLoaderImpl = function (showLoader) {
  return function () {
    return window.JBridge.toggleLoader(showLoader);
  };
};

export const loaderTextImpl = function (mainTxt) {
  return function (subTxt) {
    return function () {
      return window.JBridge.loaderText(mainTxt, subTxt);
    };
  };
};


export const showDialer = function (str) {
  return function (call) {
    try {
      window.JBridge.showDialer(str, call);
    } catch (error) {
      window.JBridge.showDialer(str);
    }
  }
};

export const startLocationPollingAPI = function () {
  if (locationPollingTimer) {
    clearTimeout(locationPollingTimer);
    locationPollingTimer = undefined;
  }
  return window.JBridge.startLocationPollingAPI();
}

export const generatePDF = function (state) {
  return function (type) {
    var data = JSON.stringify(state)
    window.JBridge.generatePDF(data, type);
    return true;
  };
};

export const stopLocationPollingAPI = function () {
  if (locationPollingTimer) return;
  if (JBridge.isServiceRunning){
    if (JBridge.isServiceRunning(locationUpdateServiceName)) {
      stopLocationService();
    }
  } else {
    stopLocationService();
  }
}

function stopLocationService () {
  let cb = function() {
    window.JBridge.stopLocationPollingAPI();
  }
  locationPollingTimer = setTimeout(cb,5000);
}

export const removeAllPolylines = function(str){
  window.JBridge.removeAllPolylines(str);
}

export const currentPosition = function (str) {
    window.JBridge.currentPosition(str);
}

export const firebaseScreenNameLog = function (str) {
  if(window.JBridge.firebaseScreenNameLog){
      window.JBridge.firebaseScreenNameLog(str);
  }
};
export const firebaseUserID = function (str) {
  if(window.JBridge.firebaseUserID) {
      window.JBridge.firebaseUserID(str);
  }
};

export const storeCallBackDriverLocationPermission = function (cb) {
  try {
  return function (action) {
      return function () {
          var locationCallBack = function () {
            var isPermissionEnabled = isLocationPermissionEnabled()() && isLocationEnabled()()
            cb(action (isPermissionEnabled))();
          };
          var callback = callbackMapper.map(function (isLocationPermissionGranted) {
            cb(action (isLocationPermissionGranted))();
          });
          if (window.onResumeListeners){
            window.onResumeListeners.push(locationCallBack);
          };
          window.JBridge.storeCallBackDriverLocationPermission(callback);
          console.log("In storeCallBackDriverLocationPermission ---------- + " + action);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackDriverLocationPermission ------", error);
  }
}

export const storeCallBackInternetAction = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (isNetworkOn) {
              cb(action (isNetworkOn))();
          });
          console.log("In storeCallBackInternetAction ---------- + " + action);
          window.JBridge.storeCallBackInternetAction(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackInternetAction ------", error);
  }
}

export const storeCallBackImageUpload = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (imgStr, imageName, imagePath) {
              cb(action (imgStr)(imageName)(imagePath))();
          });
          window.JBridge.storeCallBackImageUpload(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackImageUpload ------", error);
  }
}

export const storeCallBackOverlayPermission = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (isOverlayPermission) {
              cb(action (isOverlayPermission))();
          });
          var overlayCallBack = function () {
            var isPermissionEnabled = JBridge.isOverlayPermissionEnabled()
            cb(action (isPermissionEnabled))();
          }
          if (window.onResumeListeners){
            window.onResumeListeners.push(overlayCallBack);
          }
          console.log("In storeCallBackOverlayPermission ---------- + " + action);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackOverlayPermission ------", error);
  }
}

export const storeCallBackBatteryUsagePermission = function (cb) {
  try {
  return function (action) {
      return function () {
          var callback = callbackMapper.map(function (isPermissionEnabled) {
              cb(action (isPermissionEnabled))();
          });
          var batteryCallBack = function () {
            var isPermissionEnabled = JBridge.isBatteryPermissionEnabled()
            cb(action (isPermissionEnabled))();
          }
          if (window.onResumeListeners){
            window.onResumeListeners.push(batteryCallBack);
          }
          console.log("In storeCallBackBatteryUsagePermission ---------- + " + action);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackBatteryUsagePermission ------", error);
  }
}

export const isBatteryPermissionEnabled = function (unit) {
  return function () {
    return window.JBridge.isBatteryPermissionEnabled();
  };
};

export const getAAID = function (str) {
  console.log("HERE IN getAAID ===--->>")
  return window.JBridge.getAAID();
}

export const isInternetAvailable = function (unit) {
  return function () {
    if (window.__OS == "IOS")
      {if (window.JBridge.isNetworkAvailable() == "1") return true;
    else return false;}
    else return JBridge.isInternetAvailable();
  };
};

export const factoryResetApp = function (str) {
  console.log("HERE IN RESET ===--->>")
  JBridge.factoryResetApp()
}

export const uploadFile = function (unit) {
  return function () {
    return JBridge.uploadFile();
  };
};

export const previewImage = function (base64Image) {
  return function () {
    return JBridge.previewImage(base64Image);
  }
}

export const renderBase64Image = function (image) {
  return function (id) {
    return function (fitCenter) {
      return function (imgScaleType){
        try {
          if(JBridge.renderBase64Image){
            return JBridge.renderBase64Image(image, id, fitCenter, imgScaleType);
          }
        }catch(err1){
          try {
            if (JBridge.renderBase64Image) {
              return JBridge.renderBase64Image(image, id, fitCenter);
            }
          } catch (err2) {
          /*
          * This function is deprecated on 22 May - 2023
          * Added only for Backward Compability
          * Remove this function once it is not begin used.
          */
            return JBridge.renderBase64Image(image, id);
          }
        }
      }
    };
  };
};

export const isOverlayPermissionEnabled = function (unit) {
  return function () {
    return JBridge.isOverlayPermissionEnabled();
  };
};

export const setScaleType = function (id) {
  return function (url) {
    return function (scaleType) {
      if(JBridge.setScaleType){
        return JBridge.setScaleType(id, url, scaleType);
      }
    }
  };
};

export const requestBatteryPermission = function (str) {
  return function () {
    JBridge.requestBatteryPermission();
  };
};
export const copyToClipboard = function(str) {
     JBridge.copyToClipboard(str);
}

export const requestKeyboardShow = function(id) {
    JBridge.requestKeyboardShow(id);
}

export const showKeyboard = function(id){
    JBridge.showKeyboard(id); // imeOptions is set to IME_ACTION_SEARCH and IME_ACTION_DONE
}

export const locateOnMap = function (str, lat, lon, geoJson, coodinates) {
  try {
    return JBridge.locateOnMap(str, lat, lon, geoJson, JSON.stringify(coodinates));
  } catch (err) {
    return JBridge.locateOnMap(str, lat, lon);
  }
};

export const exitLocateOnMap = function(str){
  JBridge.exitLocateOnMap(str);
}

export const shareTextMessage = function(str){
  return function (message){
    if(JBridge.shareTextMessage){
      JBridge.shareTextMessage(str, message);
    }
  }
}

export const shareImageMessage = function(message){
  return function (imageName){
    if(JBridge.shareTextMessage){
      JBridge.shareImageMessage(message,imageName);
    }
  }
}

export const showInAppNotification = function(title){
  return function(message){
    return function(onTapAction){
      return function(action1Text){
        return function(action2Text){
          return function(action1Image){
            return function(action2Image){
              return function(channelId){
                return function(duration){
                  return window.JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"in_app_notification" , title:title ,message:message ,onTapAction:onTapAction, action1Text:action1Text,action2Text:action2Text , action1Image : action1Image ,action2Image :action2Image , channelId:channelId , durationInMilliSeconds:duration}))()
                }
              }
            }
          }
        }
      }
    }
  }
}

export const openWhatsAppSupport = function (contactNumber) {
  return function () {
    console.log("contactNumber" + contactNumber)
    return JBridge.openWhatsAppSupport(contactNumber)
  }
}

export const mapSnapShot = function (id) {
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


export const setStoreCallBackPopUp = function (cb) {
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

export const deletePopUpCallBack = function (dummy) {
  console.log("jbridge deletepopupcallback before");
  JBridge.deletePopUpCallBack(dummy);
  return true;
}

export const startLottieProcess = function (configObj) {
  var rawJson = configObj.rawJson;
  var lottieName = "";
  var fileName = rawJson.substr(rawJson.lastIndexOf("/") + 1);
  if (JBridge.isFilePresentDeep) {
    lottieName = JBridge.isFilePresentDeep(fileName) ? (fileName.slice(0, fileName.lastIndexOf("."))) : rawJson;
  } else {
    lottieName = isFilePresent(fileName) ? (fileName.slice(0, fileName.lastIndexOf("."))) : rawJson;
  }
  try {
    if (window.__OS == "IOS") {
      return JBridge.startLottieProcess(lottieName, configObj.lottieId, configObj.repeat, configObj.speed, configObj.scaleType, JSON.stringify(configObj));
    }
    configObj.rawJson = lottieName;
    return JBridge.startLottieProcess(JSON.stringify(configObj));
  } catch (err) {
    return JBridge.startLottieProcess(lottieName, configObj.lottieId, configObj.repeat, configObj.speed, configObj.scaleType);
  }
};

function isFilePresent(fileName) {
   if (window.__OS == "IOS") {
      return JBridge.isFilePresent(fileName) == "0" ? false : true;
   } else {
      return JBridge.isFilePresent(fileName);
   }
}

export const methodArgumentCount = function (functionName) {
  try {
    return window.JBridge.methodArgumentCount(functionName);
  } catch (error) {
    console.log("error inside argumentCount : " + error)
    return 0;
  }
}

export const generateSessionToken = function (type) {
  if (window.__OS == "IOS"){
    return "d4faebc6-2f98-44a0-957e-20cb4646c013";
  }
  return JBridge.generateSessionToken(type);
}

export const enableMyLocation = function(isEnableCurrentLocation){
  JBridge.enableMyLocation(isEnableCurrentLocation);
}

export const toggleBtnLoader = function(id){
  return function (val) {
      if (val == true) {
        btnLoaderState.set(id,true);
      } else {
        btnLoaderState.clear();
      }
  };
};

export const getBtnLoader = function(val){
    return (btnLoaderState.get(val) == true)? true : false;
};

export const storeMainFiberOb = function(fiber){
  mainFiber = fiber;
};

export const getMainFiber = function (just, nothing) {
  if (mainFiber == null) {
    return nothing;
  } else {
    var localFiber = mainFiber;
    mainFiber = null;
    return just(localFiber);
  }
}

export const launchInAppRatingPopup = function (unit) {
  if (JBridge.launchInAppRatingPopup) {
    JBridge.launchInAppRatingPopup();
  }
};
export const getExtendedPath = function (path) {
  if (JBridge.getExtendedPath) {
    var extendedPath = JBridge.getExtendedPath(JSON.stringify(path));
    return JSON.parse(extendedPath);
  }else {
    return path;
  }
};

export const startTimerWithTime = function (time) {
  return function (qouteId) {
    return function (interval) {
      return function (cb) {
        return function (action) {
          return function () {
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
}


export const generateSessionId = function () {
  try {
    var dt = new Date().getTime();
    var uuid = 'xxxxxxxx-xxxx-xxxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = (dt + Math.random()*16)%16 | 0;
        dt = Math.floor(dt/16);
        return (c=='x' ? r :(r&0x3|0x8)).toString(16);
    });
    return uuid;
  } catch (err) {
    return Math.random().toString(16);
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

            return JBridge.initialWebViewSetUp(callback,id);
          } catch (err) {
            console.log("initialWebViewSetUp error " + err);
          }
        };
      };
  };
};

export const goBackPrevWebPage = function (id) {
  try {
    if (JBridge.goBackPrevWebPage){
      return JBridge.goBackPrevWebPage(id);
    }
  } catch (err) {
    console.log("goBackPrevWebPage error " + err);
  }
}

export const detectPhoneNumbers = function (cb) {
  return function (action) {
    return function () {
      var callback = callbackMapper.map(function (id) {
        cb(action(id))();
      });
      if(JBridge.detectPhoneNumbers){
        return window.JBridge.detectPhoneNumbers(callback);
      }
    };
  };
};

export const setCleverTapUserData = function (key) {
  return function (value) {
        if(window.JBridge.setCleverTapUserData){
            window.JBridge.setCleverTapUserData(key, value);
        }
  };
 };

 export const setCleverTapUserProp = function (key) {
   return function (value) {
         if(window.JBridge.setCleverTapUserProp){
             window.JBridge.setCleverTapUserProp(key,value);
         }
   };
  };

export const cleverTapCustomEvent = function(event){
    if (window.JBridge.cleverTapCustomEvent){
      return JBridge.cleverTapCustomEvent(event);
    }
}


export const cleverTapCustomEventWithParams = function (event) {
  return function (paramKey) {
    return function (paramValue) {
      if(window.JBridge.cleverTapCustomEventWithParams){
          window.JBridge.cleverTapCustomEventWithParams(event, paramKey,paramValue);
      }
  };
 };
};

export const cleverTapSetLocation = function () {
  if(window.JBridge.cleverTapSetLocation ){
      return window.JBridge.cleverTapSetLocation();
    }
}

export const launchDateSettings = function (res) {
  if(JBridge.launchDateSettings){
    return JBridge.launchDateSettings();
  }
};

function getTwoDigitsNumber(number)
{
  return number >= 10 ? number : "0"+number.toString();
}

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

export const cleverTapEvent = function(event){
  return function (param){
    if (window.JBridge.cleverTapEvent){
        return JBridge.cleverTapEvent(event, JSON.stringify(param));
    }
  }
}

export const emitJOSEvent = function (mapp,eventType,payload) {
  console.log("payload" , payload);
  JOS.emitEvent(mapp)(eventType)(JSON.stringify(payload))()()
};

export const hideLoader = function () {
  JOS.emitEvent("java")("onEvent")(JSON.stringify({event : "hide_loader"}))()()
};

export const getLayoutBounds = function (id) {
  if (JBridge.getLayoutBounds) {
    var bounds = JSON.parse(JBridge.getLayoutBounds(id));
    return bounds;
  };
  return {"height" : 0, "width" : 0}
};



export const  horizontalScrollToPos = function (id, childId, focus) {
  if (window.JBridge.horizontalScrollToPos){
    window.JBridge.horizontalScrollToPos(id,childId, focus);
  }
}

// focus values --
  // left - 17
  // right - 66
  // top - 33
  // down - 130

export const withinTimeRange = function (startTime) {
  return function (endTime) {
    return function(timeStr){
      try {
        return startTime < endTime ? between(timeStr, startTime, endTime) : between(timeStr, startTime, "23:59:59") || between(timeStr, "00:00:01", endTime);
     }catch (err){
        return false;
      }
    }
  }
}

function between(x, min, max) {
  return x >= min && x <= max;
}

export const askNotificationPermission = function () {
  if (window.JBridge.requestNotificationPermission){
    return window.JBridge.requestNotificationPermission();
  }
}

export const scrollViewFocus = function (parentID) {
  return function (index) {
    try {
      if (window.__OS == "ANDROID") {
        let cmd = "set_scrollView=ctx->findViewById:i_" + parentID + ";"
        cmd += "set_childView=get_scrollView->getChildAt:i_0;"
        cmd += "set_focusChildView=get_childView->getChildAt:i_" + JSON.stringify(index) + ";"
        cmd += "get_btm=get_focusChildView->getTop;"
        cmd += "get_scrollView->smoothScrollTo:i_0,get_btm;"
        setTimeout(function () {
          window.Android.runInUI(cmd, null);
        }, 200)
        return true;
      } else {
        if (window.JBridge.scrollViewFocus) {
          window.JBridge.scrollViewFocus(parentID, JSON.stringify(index));
          return true;
        }
      }
    } catch (err) {
      console.log("error in scrollViewFocus : " + err);
    }
    return false;
  }
}

export const scrollDirectionListner = function (parentID) {

    if (JBridge.scrollDirectionListner){
      console.log("calling scrollDirectionListner")
      return JBridge.scrollDirectionListner(parentID);
    } else{
      console.log("error")
    }
}