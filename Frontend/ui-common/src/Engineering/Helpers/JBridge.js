import { callbackMapper } from "presto-ui";

let timerIdForTimeout;
const btnLoaderState = new Map();
const {
  JBridge
} = window;
let mainFiber = null;
let timer;
let locationPollingTimer;
const locationUpdateServiceName = "in.juspay.mobility.app.LocationUpdateService";
let timerIdDebounce = null;
let inputForDebounce;
const JOS = window.JOS;

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

export const requestLocationPermissionDriver = function (cb) {
  return function (action) {
    return function () {
      const callback = callbackMapper.map(function (isLocationPermissionEnabled) {
        cb(action(isLocationPermissionEnabled))();
      });
      return window.JBridge.requestLocationPermissionDriver(callback);
    };
  };
};

export const _addCertificates = function (str) {
  return function () {
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
    if (window.__OS == "IOS") {
      if (window.JBridge.isLocationAuthenticationStatusDetermined() == "1") {
        return true
      } else {
        return false
      }
    }
    return window.JBridge.isLocationPermissionEnabled();

  };
};

export const getKeyInSharedPrefKeys = function (key) {
  return JBridge.getFromSharedPrefs(key);
};

export const checkAndAskNotificationPermission = function (shouldAlwaysAsk) {
  const lastAppOpened = getKeyInSharedPrefKeys("LAST_APP_OPENED");
  const appOpenCount = getKeyInSharedPrefKeys("APP_OPEN_COUNT");
  let check = true;
  if (lastAppOpened != "__failed") {
    if (lastAppOpened != new Date().toLocaleDateString()) {
      if (parseInt(appOpenCount) >= 2) {
        check = true;
      } else {
        check = false;
        JBridge.setKeysInSharedPrefs("LAST_APP_OPENED", new Date().toLocaleDateString());
        JBridge.setKeysInSharedPrefs("APP_OPEN_COUNT", String(parseInt(appOpenCount) + 1));
      }
    } else {
      check = false;
    }
  } else {
    JBridge.setKeysInSharedPrefs("LAST_APP_OPENED", new Date().toLocaleDateString());
    JBridge.setKeysInSharedPrefs("APP_OPEN_COUNT", "0");
  }
  
  if ((check || shouldAlwaysAsk) && window.__OS == "ANDROID" && window.JBridge.checkAndAskNotificationPermission) {
    return window.JBridge.checkAndAskNotificationPermission();
  }
};

export const getLocationPermissionStatus = function (unit) {
  try {
    if (window.__OS == "IOS") {
      const resp = window.JBridge.getLocationPermissionStatus();
      if (resp == "3" || resp == "4" || resp == "5") return "ENABLED"
      else if (resp == "0") return "DISABLED"
      else return "DENIED"
    } else {
      return window.JBridge.getLocationPermissionStatus();
    }
  } catch (e) {
    return "ENABLED";
  }
}

export const isMicrophonePermissionEnabled = function (unit) {
  return window.JBridge.isMicrophonePermissionEnabled();
};

export const getPackageName = function () {
  if (window.__OS == "IOS") {
    const sessionDetails = JSON.parse(window.JBridge.getSessionDetails())
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
    const sessionDetails = JSON.parse(window.JBridge.getSessionDetails())
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
    const sessionDetails = JSON.parse(window.JBridge.getSessionDetails())
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

export const getAndroidVersion = function (unit) {
  if (window.__OS == "IOS") {
    return 0;
  } else {
    const sessionData = window.JBridge.getSessionInfo();
    const parsedSessionData = JSON.parse(sessionData);
    return parsedSessionData["os_version"];
  }
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
          let result = "";
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
      const callback = callbackMapper.map(function (hour, min, resp) {
        cb(action(hour)(min)(resp))();
      });
      return window.JBridge.timePicker(callback);
    };
  };
};

export const datePicker = function (label) {
  return function (cb) {
    return function (action) {
      return function () {
        const callback = callbackMapper.map(function (resp, year, month, date) {
          cb(action(resp)(year)(month)(date))();
        });
        return window.JBridge.datePicker(callback, label);
      };
    };
  };
};

export const setFCMToken = function (cb) {
  return function (action) {
    return function () {
      if (window.JBridge.setFCMToken) {
        const callback = callbackMapper.map(function (id) {
          cb(action(id))();
        });
        return window.JBridge.setFCMToken(callback);
      }
    };
  };
  // JBridge.setFCMToken();
};

export const setFCMTokenWithTimeOut = function (timeOut, cb) {
  if (window.JBridge.setFCMToken) {
    const timeOutCallBack = function () {
      cb("NOT_FOUND")();
    };
    const fcmTimer = setTimeout(timeOutCallBack, timeOut);
    const callback = callbackMapper.map(function (token) {
      clearTimeout(fcmTimer);
      cb(token)();
    });
    return window.JBridge.setFCMToken(callback);
  }
}

export const dateTimePicker = function (cb) {
  return function (action) {
    return function () {
      const callback = callbackMapper.map(function (epochTime) {
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
        const place = JSON.parse(bundle);
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
            const place = JSON.parse(bundle);
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

const drawRoute = function (data, style, trackColor, isActual, sourceMarker, destMarker, polylineWidth, type, sourceName, destinationName, mapRouteConfig) {
  console.log("I AM HERE ------------------ IN DRAW ROUTE");
  try {
    return window.JBridge.drawRoute(JSON.stringify(data), style, trackColor, isActual, sourceMarker, destMarker, polylineWidth, type, sourceName, destinationName, JSON.stringify(mapRouteConfig));
  } catch (err) {
    /*
        * This Function is deprecated on 10 Jul- 2023
        * Remove this function once it is not begin used.
        */
    return window.JBridge.drawRoute(JSON.stringify(data), style, trackColor, isActual, sourceMarker, destMarker, polylineWidth, type, sourceName, destinationName);
  }
}
                   

export const updateRouteMarker = function (data) {
  return function () {
    if (window.JBridge.updateRouteMarker) {
      return window.JBridge.updateRouteMarker(JSON.stringify(data));
    }
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


export const updateRoute = (configObj) => {
  if (window.JBridge.updateRoute) {
    try {
      if (window.__OS == "IOS" || (window.__OS == "ANDROID" && (methodArgumentCount("updateRoute") == 1)))
        return window.JBridge.updateRoute(JSON.stringify(configObj));
      else
        return window.JBridge.updateRoute(JSON.stringify(configObj.json), configObj.destMarker, configObj.eta, configObj.srcMarker, JSON.stringify(configObj.specialLocation), configObj.zoomLevel);
    } catch (e) {
      return window.JBridge.updateRoute(JSON.stringify(configObj.json), configObj.destMarker, configObj.eta, configObj.srcMarker, JSON.stringify(configObj.specialLocation));
    }
  }
};

export const storeCallBackMessageUpdated = function (cb) {
  return function (chatChannelID) {
    return function (chatUserId) {
      return function (action) {
        return function () {
          const callback = callbackMapper.map(function (message, sentBy, timeStamp, messagesSize) {
            if (messagesSize == undefined) {
              messagesSize = "-1"
            }
            const messageObj = {
              "message": message,
              "sentBy": sentBy,
              "timeStamp": timeStamp,
              type: "Text",
              delay: 0
            }
            window.chatMessages = window.chatMessages || [];
            if(sentBy == "Driver") window.didDriverMessage = true;
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


export const drawRouteV2 = function (drawRouteConfig){
  return function() {
    try{
      console.log("Inside drawRouteV2")
      const { locations, style, routeColor, isActual, startMarker, endMarker, routeWidth, routeType, startMarkerLabel, endMarkerLabel, mapRouteConfig } = drawRouteConfig.routes.normalRoute;
      if (window.JBridge.drawRouteV2){
        return window.JBridge.drawRouteV2(JSON.stringify(drawRouteConfig));
      } else {
        return drawRoute(locations, style, routeColor, isActual, startMarker, endMarker, routeWidth, routeType, startMarkerLabel, endMarkerLabel, mapRouteConfig);
      }
    } catch (err) {
      console.log("error in drawRouteV2----------------------------------", err);
    }
  };
};


export const storeKeyBoardCallback = function (cb, action) {
  const keyBoardCallback = function (state) {
    cb(action(state))();
  }
  window.keyBoardCallback = keyBoardCallback;
};

export const getChatMessages = function (string) {
  return [].concat(window.chatMessages !== undefined ? window.chatMessages : []);
}

export const clearChatMessages = function () {
  window.chatMessages = undefined;
  window.didDriverMessage = undefined;
}

export const dateCallback = function (cb, action) {
  const callback = function () {
    cb(action)();
  }
  window.dateCallback = callback;
};

export const unregisterDateAndTime = function () {
  window.dateCallback = undefined;
};

export const storeCallBackOpenChatScreen = function (cb) {
  return function (action) {
    return function () {
      const callback = callbackMapper.map(function () {
        cb(action)();
      });
      const openChatScreen = function () {
        cb(action)();
      };
      window.openChatScreen = openChatScreen;
      if (window.JBridge.storeCallBackOpenChatScreen) {
        window.JBridge.storeCallBackOpenChatScreen(callback);
      }
    };
  };
};

export const scrollOnResume = function (cb) {
  return function (action) {
    try {
      const callback = function () {
        cb(action)();
      }
      const scrollTime = function () {
        if (getKeyInSharedPrefKeys("LOCAL_STAGE") === "ChatWithCustomer" || getKeyInSharedPrefKeys("LOCAL_STAGE") === "ChatWithDriver") {
          setTimeout(callback, 500);
        }
      }
      window.scrollAction = scrollTime;
    } catch (e) {
      console.error("Error in scrollOnResume : " + e);
    }
  }
}

export const startChatListenerService = function () {
  if (JBridge.startChatListenerService) {
    JBridge.startChatListenerService();
  }
}

export const stopChatListenerService = function () {
  if (JBridge.stopChatListenerService) {
    window.chatMessages = undefined;
    window.didDriverMessage = undefined;
    JBridge.stopChatListenerService();
  }
}

export const sendMessage = function (message) {
  if (JBridge.sendMessage) {
    if (timer) clearTimeout(timer);
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
    } else if (JBridge.scrollToBottom && bottom) {
      JBridge.scrollToBottom(id);
    }
  }
}

export const saveSuggestions = function (key) {
  return function (suggestions) {
    try {
      let configSuggestions = "";
      if(JBridge.fetchRemoteConfigString) {
        configSuggestions = JBridge.fetchRemoteConfigString("chat_suggestions");
      }
      if (configSuggestions == "") {
        const convertedJSON = {};
        if (!Array.isArray(suggestions)) {
          return;
        } else {
          suggestions.forEach(item => {
            convertedJSON[item.key] = item.value
          });
        }
        configSuggestions = JSON.stringify(convertedJSON);
      }
      JBridge.setKeysInSharedPrefs(key, configSuggestions);
    } catch (error) {
      console.error("Error in saveSuggestions " + error);
    }
  }
}

export const saveSuggestionDefs = function (key) {
  return function (suggestionDefs) {
    try {
      let configSuggestionDefs = "";
      if(JBridge.fetchRemoteConfigString) {
        configSuggestionDefs = JBridge.fetchRemoteConfigString("chat_suggestions_defs");
      }
      if(configSuggestionDefs == "") {
        const convertedJSON = {};
        if (!Array.isArray(suggestionDefs)) {
          return;
        } else {
          suggestionDefs.forEach(item => {
            convertedJSON[item.key] = item.value
          });
        }
        configSuggestionDefs = JSON.stringify(convertedJSON);
      }
      JBridge.setKeysInSharedPrefs(key, configSuggestionDefs);
    } catch (error) {
      console.error("Error in saveSuggestionDefs " + error);
    }
  }
}

export const getSuggestionsfromLocal = function (key) {
  try {
    const suggestions = JSON.parse(JBridge.fetchRemoteConfigString("chat_suggestions"));
    const keys = suggestions[key];
    if (keys) {
      return keys;
    }
    return [];
  } catch (error) {
    console.error("Error in getSuggestionsfromKey " + error);
    return ["error"];
  }
}

export const getSuggestionfromKey = function (key) {
  return function (language) {
    try {
      const suggestionDefs = JSON.parse(JBridge.fetchRemoteConfigString("chat_suggestions_defs"));
      const val = suggestionDefs[key];
      let suggestion = "";
      if (val) {
        switch (language) {
          case "EN_US":
            suggestion = val["en_us"];
            break;
          case "HI_IN":
            suggestion = val["hi_in"];
            break;
          case "KN_IN":
            suggestion = val["kn_in"];
            break;
          case "BN_IN":
            suggestion = val["bn_in"];
            break;
          case "ML_IN":
            suggestion = val["ml_in"];
            break;
          case "TA_IN":
            suggestion = val["ta_in"];
            break;
          case "TE_IN": 
            suggestion = val["te_in"];
            break;
          default:
            suggestion = val["en_us"];
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

export const clearFocus = function (id){
  if(window.JBridge.clearFocus){
    return JBridge.clearFocus(id)
  }
}


export const removeMediaPlayer = function (id) {
  if (window.JBridge.removeMediaPlayer){
    JBridge.removeMediaPlayer();
  }
};

// Deprecated 5-Jan-2024 - Remove this function once it is not begin used.
// use displayBase64Image instead
export const renderBase64ImageFile = function (base64Image, id, fitCenter, imgScaleType){
  try{
    return JBridge.renderBase64ImageFile(base64Image, id, fitCenter, imgScaleType);
  }catch (err){
    return JBridge.renderBase64ImageFile(base64Image, id, fitCenter);
  }
}

export const uploadMultiPartData = function (path, url, fileType) {
  if (window.JBridge.uploadMultiPartData){
    JBridge.uploadMultiPartData(path, url, fileType);
  }
}

export const startAudioRecording = function (id) {
  if (window.JBridge.startAudioRecording){
    if (window.__OS == "IOS") {
      return JBridge.startAudioRecording() == "0" ? false : true;
    } else {
      return JBridge.startAudioRecording();
    }
  }
};

export const stopAudioRecording = function (id) {
  if (window.JBridge.stopAudioRecording){
    return JBridge.stopAudioRecording();
  }
}

export const saveAudioFile = function (source) {
  if (window.JBridge.saveAudioFile){
    return JBridge.saveAudioFile(source);
  }
}


export const differenceBetweenTwoUTC = function (date1, date2) {
  const diffInSeconds = Math.round((new Date(date1) - new Date(date2)) / 1000);
  return diffInSeconds;
}

export const isCoordOnPath = function (data) {
  return function (lat) {
    return function (lon) {
      return function (speed) {
        return function () {
          if (window.JBridge.isCoordOnPath) {
            const json = JSON.stringify(data);
            try {
              console.log("I AM HERE ------------------ IN CHECK ROUTE");
              const res = window.JBridge.isCoordOnPath(json, lat, lon, speed);
              return JSON.parse(res);
            } catch (err) {
              console.log("Catch error" + err);
              /*
               * This Function is deprecated on 12 Jan - 2023
               * Remove this function once it is not begin used.
               */
              return window.JBridge.isCoordOnPath(json, lat, lon);
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

export const initiateLocationServiceClient = function () {
  if (window.__OS == "IOS") {
    return true;
  } else {
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
              const callback = callbackMapper.map(function (key, lat, lon) {
                console.log("in show map", action);
                window.x = cb;
                window.y = action;
                cb(action(key)(lat)(lon))();
              });
              const mapConfig = window && window.appConfig && window.appConfig.mapConfig ? JSON.stringify(window.appConfig.mapConfig) : "{}";
              try {
                window.JBridge.showMap(id, isEnableCurrentLocation, type, zoom, callback, mapConfig);
              } catch (err) {
                window.JBridge.showMap(id, isEnableCurrentLocation, type, zoom, callback);
              }
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
    const parsedData = JSON.parse(window.JBridge.getCurrentLatLong());
    if (parsedData.lat && parsedData.lng) {
      return parsedData;
    } else { // fallBack for previous release
      return {
        "lat": parsedData.lat,
        "lng": parsedData.lng
      }
    }
  }
};


export const isLocationEnabled = function (unit) {
  return function () {
    if (window.__OS == "IOS") {
      if (window.JBridge.isLocationEnabled() == "1")
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
        const callback = callbackMapper.map(function (lng) {
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
      const callback = callbackMapper.map(function (lat, lng) {
        cb(action(lat)(lng))();
      });
      return window.JBridge.getCurrentPosition(callback);
    };
  };
};

export const getCurrentPositionWithTimeoutImpl = function (cb, action, delay, shouldFallBack) {
  const callbackFallback = function () {
    cb(action("0.0")("0.0")(new Date().toISOString()))();
  };
  const currentLocationTimer = setTimeout(callbackFallback, delay);
  const callback = callbackMapper.map(function (lat, lng, ts) {
    clearTimeout(currentLocationTimer);
    cb(action(lat)(lng)(ts))();
  });
  try {
    window.JBridge.getCurrentPosition(callback, shouldFallBack);
  } catch (err) {
    window.JBridge.getCurrentPosition(callback);
  }
}

export const translateStringWithTimeout = function (cb) {
  return function (action) {
    return function (delay) {
      return function (value) {
        return function () {
          // if(window.JBridge.translateString){ TODO:: Need to perform testing and handle edge cases. Pushing the code for the release for now
          //   var callbackFallback = function (){
          //     cb(action(value))();
          //   };
          //   var timer = setTimeout(callbackFallback, delay);
          //   var callback = callbackMapper.map(function (value) {
          //     clearTimeout(timer);
          //     cb(action(value))();
          //   });
          //   window.JBridge.translateString(callback, value);
          // } else{
          //   cb(action(value))();
          // }
          cb(action(value))();
        }
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
            if (window.__OS == "IOS") {
              const query = mode == "WALK" ? window.appConfig.navigationAppConfig.ios.walkQuery : window.appConfig.navigationAppConfig.ios.query;
              return window.JBridge.openNavigationWithQuery(dlat, dlong, query);
            } else {
              const query = mode == "WALK" ? window.appConfig.navigationAppConfig.android.walkQuery : window.appConfig.navigationAppConfig.android.query;
              const packageName = window.appConfig.navigationAppConfig.android.packageName;
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
      return function (lng2) {
        return function () {
          window.JBridge.moveCamera(lat1, lng1, lat2, lng2);
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
  else if (window.JBridge.toaster)
    window.JBridge.toaster(str);
  else
    window.JBridge.toast(str);
};

export const firebaseLogEventWithParams = function (_event) {
  return function (paramKey) {
    return function (paramValue) {
      return function () {
        if (window.JBridge.firebaseLogEventWithParams) {
          window.JBridge.firebaseLogEventWithParams(_event, paramKey, paramValue);
        }
      };
    };
  };
};

export const firebaseLogEventWithTwoParams = function (_event) {
  return function (paramKey1) {
    return function (paramValue1) {
      return function (paramKey2) {
        return function (paramValue2) {
          return function () {
            if (window.JBridge.firebaseLogEventWithTwoParams) {
              window.JBridge.firebaseLogEventWithTwoParams(_event, paramKey1, paramValue1, paramKey2, paramValue2);
            }
          };
        };
      };
    };
  };
};

export const firebaseLogEvent = function (str) {
  return function () {
    if (window.JBridge.firebaseLogEvent) {
      window.JBridge.firebaseLogEvent(str);
    }
  }
};

export const metaLogEvent = function (_event) {
  if (window.JBridge.metaLogEvent) {
    window.JBridge.metaLogEvent(_event);
  }
}

export const metaLogEventWithParams = function (_event) {
  return function (paramKey) {
    return function (paramValue) {
      return function () {
        if (window.JBridge.metaLogEventWithParams) {
          window.JBridge.metaLogEventWithParams(_event, paramKey, paramValue);
        }
      };
    };
  };
};

export const metaLogEventWithTwoParams = function (_event) {
  return function (paramKey1) {
    return function (paramValue1) {
      return function (paramKey2) {
        return function (paramValue2) {
          return function () {
            if (window.JBridge.metaLogEventWithTwoParams) {
              window.JBridge.metaLogEventWithTwoParams(_event, paramKey1, paramValue1, paramKey2, paramValue2);
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
        const cb = callbackMapper.map(function (res) {
          try {
            const result = JSON.parse(res);
            if (result.status == "onResponse") {
              succ(result.payload)();
            } else {
              err(result.payload)();
            }
          } catch (e) {
            console.error(e);
            succ(res)();
          }
        });
        window.JBridge.runInJuspayBrowser("onEvent", JSON.stringify(payload), cb);
      };
    };
  };
};

// exports.getSessionInfo = JSON.parse(JBridge.getDeviceInfo());

export const getKeyInNativeSharedPrefKeys = function (key) {
  return JBridge.getFromSharedPrefs(key);
};

export const setKeyInSharedPrefKeysImpl = function (key) {
  return function (value) {
    return JBridge.setInSharedPrefs(key, value);
  };
};

export const setKeyInSharedPref = function (key, value) {
  return JBridge.setInSharedPrefs(key, value);
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
    const data = JSON.stringify(state)
    window.JBridge.generatePDF(data, type);
    return true;
  };
};

function stopLocationService() {
  const cb = function () {
    window.JBridge.stopLocationPollingAPI();
  }
  locationPollingTimer = setTimeout(cb, 5000);
}


export const stopLocationPollingAPI = function () {
  if (locationPollingTimer) return;
  if (JBridge.isServiceRunning) {
    if (JBridge.isServiceRunning(locationUpdateServiceName)) {
      stopLocationService();
    }
  } else {
    stopLocationService();
  }
}
export const removeAllPolylines = function (str) {
  window.JBridge.removeAllPolylines(str);
}

export const removeAllPolylinesAndMarkers = function (array, unit) {
  const stringifiedArray = JSON.stringify(array)
  window.JBridge.removeAllPolylines(stringifiedArray);
  return unit;
}

export const currentPosition = function (str) {
  window.JBridge.currentPosition(str);
}

export const firebaseScreenNameLog = function (str) {
  if (window.JBridge.firebaseScreenNameLog) {
    window.JBridge.firebaseScreenNameLog(str);
  }
};
export const firebaseUserID = function (str) {
  if (window.JBridge.firebaseUserID) {
    window.JBridge.firebaseUserID(str);
  }
};

export const storeCallBackDriverLocationPermission = function (cb) {
  return function (action) {
    return function () {
      try {
        if (window.onResumeListeners) {
          const locationCallBack = function () {
            const isPermissionEnabled = isLocationPermissionEnabled()() && isLocationEnabled()()
            cb(action(isPermissionEnabled))();
          };
          window.onResumeListeners.push(locationCallBack);
        }
        if (window.__OS == "ANDROID") {
          const callback = callbackMapper.map(function (isLocationPermissionGranted) {
            cb(action(isLocationPermissionGranted))();
          });
          window.JBridge.storeCallBackDriverLocationPermission(callback);
        }
        console.log("In storeCallBackDriverLocationPermission ---------- + " + action);
      } catch (error) {
        console.log("Error occurred in storeCallBackDriverLocationPermission ------", error);
      }
    }
  }
}

export const storeOnResumeCallback = function (cb, action) {
  try {
    const callback = function () {
      cb(action)();
    }
    console.log ("onResumeListeners",callback);
    if (window.onResumeListeners) {
      window.onResumeListeners.push(callback);
    }
  } catch (error) {
    console.log("Error occurred in storeOnResumeCallback ------", error);
  }
}

export const storeCallBackInternetAction = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (isNetworkOn) {
          cb(action(isNetworkOn))();
        });
        console.log("In storeCallBackInternetAction ---------- + " + action);
        window.JBridge.storeCallBackInternetAction(callback);
      } catch (error) {
        console.log("Error occurred in storeCallBackInternetAction ------", error);
      }
    }
  }
}

export const storeCallBackImageUpload = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (imgStr, imageName, imagePath) {
          cb(action(imgStr)(imageName)(imagePath))();
        });
        window.JBridge.storeCallBackImageUpload(callback);
      } catch (error) {
        console.log("Error occurred in storeCallBackImageUpload ------", error);
      }
    }
  }
}

export const storeCallBackUploadMultiPartData = function (cb, action) {
  try {
    const callback = callbackMapper.map(function (fileType, fileId) {
      cb(action (fileType)(fileId))();
    });
    window.JBridge.storeCallBackUploadMultiPartData(callback);
  }catch (error){
    console.log("Error occurred in storeCallBackUploadMultiPartData ------", error);
  }
}

export const storeCallBackOverlayPermission = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (isOverlayPermission) {
          cb(action(isOverlayPermission))();
        });
        const overlayCallBack = function () {
          const isPermissionEnabled = JBridge.isOverlayPermissionEnabled()
          cb(action(isPermissionEnabled))();
        }
        if (window.onResumeListeners) {
          window.onResumeListeners.push(overlayCallBack);
        }
        console.log("In storeCallBackOverlayPermission ---------- + " + action);
      } catch (error) {
        console.log("Error occurred in storeCallBackOverlayPermission ------", error);
      }
    }
  }
}

export const storeCallBackNotificationPermission = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (isPermissionEnabled) {
          cb(action(isPermissionEnabled))();
        });
        const notificationCallback = function () {
          const isPermissionEnabled = JBridge.isNotificationPermissionEnabled()
          cb(action(isPermissionEnabled))();
        }
        if (window.onResumeListeners) {
          window.onResumeListeners.push(notificationCallback);
        }
        console.log("In storeCallBackNotificationPermission ---------- + " + action);
      } catch (error) {
        console.log("Error occurred in storeCallBackNotificationPermission ------", error);
      }
    }
  }
}

export const storeCallBackBatteryUsagePermission = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (isPermissionEnabled) {
          cb(action(isPermissionEnabled))();
        });
        const batteryCallBack = function () {
          const isPermissionEnabled = JBridge.isBatteryPermissionEnabled()
          cb(action(isPermissionEnabled))();
        }
        if (window.onResumeListeners) {
          window.onResumeListeners.push(batteryCallBack);
        }
        console.log("In storeCallBackBatteryUsagePermission ---------- + " + action);
      } catch (error) {
        console.log("Error occurred in storeCallBackBatteryUsagePermission ------", error);
      }
    }
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
    if (window.__OS == "IOS") {
      if (window.JBridge.isNetworkAvailable() == "1") return true;
      else return false;
    } else return JBridge.isInternetAvailable();
  };
};

export const restartApp = function () {
  return function() {
    console.log("HERE IN RESET ===--->>")
    if (JBridge.restartApp){
      JBridge.restartApp();
    } else {
      JBridge.factoryResetApp();
    }
  }
}

// Deprecated
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

// Deprecated 5-Jan-2024 - Remove this function once it is not begin used.
// use displayBase64Image instead
export const renderBase64Image = function (image) {
  return function (id) {
    return function (fitCenter) {
      return function (imgScaleType) {
        try {
          if (JBridge.renderBase64Image) {
            return JBridge.renderBase64Image(image, id, fitCenter, imgScaleType);
          }
        } catch (err1) {
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
      if (JBridge.setScaleType) {
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
export const copyToClipboard = function (str) {
  JBridge.copyToClipboard(str);
}

export const requestKeyboardShow = function (id) {
  JBridge.requestKeyboardShow(id);
}

export const showKeyboard = function (id) {
  JBridge.showKeyboard(id); // imeOptions is set to IME_ACTION_SEARCH and IME_ACTION_DONE
}

export const locateOnMap = (configObj) => {
  try {
    if ( window.__OS == "IOS" || (window.__OS == "ANDROID" && methodArgumentCount("locateOnMap") == 1))
      return JBridge.locateOnMap(JSON.stringify(configObj));
    else
      return JBridge.locateOnMap(configObj.goToCurrentLocation, configObj.lat, configObj.lon, JSON.stringify(configObj.geoJson), JSON.stringify(configObj.points));
  } catch (err) {
    try{
      return JBridge.locateOnMap(configObj.goToCurrentLocation, configObj.lat, configObj.lon);
    } catch (e) {
      return JBridge.locateOnMap(configObj.goToCurrentLocation, configObj.lat, configObj.lon, configObj.zoomLevel);
    }
  }
};

export const exitLocateOnMap = function (str) {
  JBridge.exitLocateOnMap(str);
}

export const shareTextMessage = function (str) {
  return function (message) {
    if (JBridge.shareTextMessage) {
      JBridge.shareTextMessage(str, message);
    }
  }
}

export const shareImageMessage = function (message) {
  return function (data) {
    try {
      if (JBridge.shareImageMessage && methodArgumentCount("shareImageMessage") == 3) {
        JBridge.shareImageMessage(message, "", JSON.stringify(data));
      } else {
        if (JBridge.shareTextMessage) {
          JBridge.shareTextMessage("", message);
        }
      }
    } catch (e) {
      if (JBridge.shareTextMessage) {
        JBridge.shareTextMessage("", message);
      }
    }
  }
}

export const showInAppNotification = function (title) {
  return function (message) {
    return function (onTapAction) {
      return function (action1Text) {
        return function (action2Text) {
          return function (action1Image) {
            return function (action2Image) {
              return function (channelId) {
                return function (duration) {
                  return window.JOS.emitEvent("java")("onEvent")(JSON.stringify({
                    event: "in_app_notification",
                    title: title,
                    message: message,
                    onTapAction: onTapAction,
                    action1Text: action1Text,
                    action2Text: action2Text,
                    action1Image: action1Image,
                    action2Image: action2Image,
                    channelId: channelId,
                    durationInMilliSeconds: duration
                  }))()
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
              const callback = callbackMapper.map(function (encImage) {
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
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (stringifyPayload) {
          cb(action(stringifyPayload))();
        });
        JBridge.setStoreCallBackPopUp(callback);
      } catch (error) {
        console.log("Error occurred in setStoreCallBackPopUp", error);
      }
    }
  }
}

export const deletePopUpCallBack = function (dummy) {
  console.log("jbridge deletepopupcallback before");
  JBridge.deletePopUpCallBack(dummy);
  return true;
}

function isFilePresent(fileName) {
  if (window.__OS == "IOS") {
    return JBridge.isFilePresent(fileName) == "0" ? false : true;
  } else {
    return JBridge.isFilePresent(fileName);
  }
}

export const startLottieProcess = function (configObj) {
  const rawJson = configObj.rawJson;
  let lottieName = "";
  const fileName = rawJson.substr(rawJson.lastIndexOf("/") + 1);
  if (!configObj.forceToUseRemote) {
    if (JBridge.isFilePresentDeep) {
      lottieName = JBridge.isFilePresentDeep(fileName) ? (fileName.slice(0, fileName.lastIndexOf("."))) : rawJson;
    } else {
      lottieName = isFilePresent(fileName) ? (fileName.slice(0, fileName.lastIndexOf("."))) : rawJson;
    }
  } else {
    lottieName = rawJson;
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
}

export const generateSessionToken = function (type) {
  if (window.__OS == "IOS") {
    return "d4faebc6-2f98-44a0-957e-20cb4646c013";
  }
  return JBridge.generateSessionToken(type);
}

export const enableMyLocation = function (isEnableCurrentLocation) {
  JBridge.enableMyLocation(isEnableCurrentLocation);
}

export const toggleBtnLoader = function (id) {
  return function (val) {
    if (val == true) {
      btnLoaderState.set(id, true);
    } else {
      if (id == "") btnLoaderState.clear();
      else btnLoaderState.set(id, false)
    }
  };
};

export const getBtnLoader = function (val) {
  return (btnLoaderState.get(val) == true) ? true : false;
};

export const storeMainFiberOb = function (fiber) {
  mainFiber = fiber;
};

export const getMainFiber = function (just, nothing) {
  if (mainFiber === null) {
    return nothing;
  } else {
    const localFiber = mainFiber;
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
    const extendedPath = JBridge.getExtendedPath(JSON.stringify(path));
    return JSON.parse(extendedPath);
  } else {
    return path;
  }
};


export const setMapPaddingImpl = function (left, topPadding, right, bottom) {
  if (JBridge.setMapPadding) {
    JBridge.setMapPadding(left, topPadding, right, bottom);
  }
}

export const generateSessionId = function () {
  try {
    let dt = new Date().getTime();
    const uuid = "xxxxxxxx-xxxx-xxxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, function (c) {
      const r = (dt + Math.random() * 16) % 16 | 0;
      dt = Math.floor(dt / 16);
      return (c == "x" ? r : (r & 0x3 | 0x8)).toString(16);
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
          const callback = callbackMapper.map(function (val) {
            cb(action(val))();
          });

          return JBridge.initialWebViewSetUp(callback, id);
        } catch (err) {
          console.log("initialWebViewSetUp error " + err);
        }
      };
    };
  };
};

export const goBackPrevWebPage = function (id) {
  try {
    if (JBridge.goBackPrevWebPage) {
      return JBridge.goBackPrevWebPage(id);
    }
  } catch (err) {
    console.log("goBackPrevWebPage error " + err);
  }
}

export const detectPhoneNumbers = function (cb) {
  return function (action) {
    return function () {
      const callback = callbackMapper.map(function (id) {
        cb(action(id))();
      });
      if (JBridge.detectPhoneNumbers) {
        return window.JBridge.detectPhoneNumbers(callback);
      }
    };
  };
};

export const setCleverTapUserData = function (key) {
  return function (value) {
    return function () {
      if (window.JBridge.setCleverTapUserData) {
        window.JBridge.setCleverTapUserData(key, value);
      }
    }
  };
};

export const setCleverTapUserProp = function (arr) {
  try {
    if (window.JBridge.setCleverTapUserMultipleProp) {
      return JBridge.setCleverTapUserMultipleProp(JSON.stringify(arr));
    } else {
      if (window.JBridge.setCleverTapUserProp) {
        for (let i = 0; i < arr.length; i++) {
          const jsonObject = arr[i];
          const key = jsonObject.key;
          const value = jsonObject.value;
          window.JBridge.setCleverTapUserProp(key, value);
        }
      }
    }
  } catch (err) {
    console.log("setCleverTapUserProp error " + err);
  }
}

export const cleverTapCustomEvent = function (_event) {
  if (window.JBridge.cleverTapCustomEvent) {
    return JBridge.cleverTapCustomEvent(_event);
  }
}


export const cleverTapCustomEventWithParams = function (_event) {
  return function (paramKey) {
    return function (paramValue) {
      if (window.JBridge.cleverTapCustomEventWithParams) {
        window.JBridge.cleverTapCustomEventWithParams(_event, paramKey, paramValue);
      }
    };
  };
};

export const cleverTapSetLocation = function () {
  if (window.JBridge.cleverTapSetLocation) {
    return window.JBridge.cleverTapSetLocation();
  }
}

export const extractReferrerUrl = function () {
  if(JBridge.extractReferrerUrl){
    return window.JBridge.extractReferrerUrl();
  }
}

export const launchDateSettings = function (res) {
  if (JBridge.launchDateSettings) {
    return JBridge.launchDateSettings();
  }
};

export const cleverTapEvent = function (_event) {
  return function (param) {
    if (window.JBridge.cleverTapEvent) {
      return JBridge.cleverTapEvent(_event, JSON.stringify(param));
    }
  }
}

export const emitJOSEvent = function (mapp, eventType, payload) {
  console.log("payload", payload);
  JOS.emitEvent(mapp)(eventType)(JSON.stringify(payload))()()
};

export const getLocationNameV2 = function (lat, lon) { 
  try {
    if (JBridge.getLocationNameSDK) {
      return JBridge.getLocationNameSDK(lat, lon);
    }else{
      return "NO_LOCATION_FOUND";
    }
  } catch (error) {
    return "NO_LOCATION_FOUND";
  }
}

function isJSONString(str) {
  try {
    JSON.parse(str);
    return true;
  } catch (error) {
    return false;
  }
}

export const getLatLonFromAddress = function (address) {
  const defaultCoordinate = {
    "latitude": 0.0,
    "longitude": 0.0
  }
  try {
    if (JBridge.getCoordinateFromAddress) {
      const result = (JBridge.getCoordinateFromAddress(address));
      return result != "NO_COORDINATE_FOUND" && isJSONString(result) ? JSON.parse(result) : defaultCoordinate;
    }else{
      return defaultCoordinate;
    }
  } catch (error) {
    return defaultCoordinate;
  }
};

export const hideLoader = function () {
  JOS.emitEvent("java")("onEvent")(JSON.stringify({
    event: "hide_loader"
  }))()()
};

export const getLayoutBounds = function (id) {
  if (JBridge.getLayoutBounds) {
    const bounds = JSON.parse(JBridge.getLayoutBounds(id));
    return bounds;
  }
  return {
    "height": 0,
    "width": 0
  }
};

export const listDownloadedTranslationModels = function (cb) {
  return function (delay){
    return function () {
      const fallbackCB = function () {
        cb([])();
      };
      const modelListTimer = setTimeout(fallbackCB, delay);
      const modelsCB = callbackMapper.map(function (modelList) {
        clearTimeout(modelListTimer);
        cb(modelList)();
      }); 
      if (JBridge.listDownloadedTranslationModels) {
        JBridge.listDownloadedTranslationModels(modelsCB);
      }
    }
  }
}

export const horizontalScrollToPos = function (id, childId, scrollFocus) {
  if (window.JBridge.horizontalScrollToPos) {
    window.JBridge.horizontalScrollToPos(id, childId, scrollFocus);
  }
}

// focus values --
// left - 17
// right - 66
// top - 33
// down - 130


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

export const askNotificationPermission = function () {
  if (window.JBridge.requestNotificationPermission) {
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

export const setYoutubePlayer = function (json, viewId, videoStatus) {
  if (JBridge.setYoutubePlayer) {
    try {
      console.log("Inside setYoutubePlayer ------------");
      return JBridge.setYoutubePlayer(JSON.stringify(json), viewId, videoStatus);
    } catch (err) {
      console.log("error in setYoutubePlayer");
    }
  }
};

export const pauseYoutubeVideo = function (unit){
  if (JBridge.pauseYoutubeVideo) {
    return JBridge.pauseYoutubeVideo();
  }
}

export const downloadMLTranslationModel = function (language) {
  if(JBridge.triggerDownloadForML){
    return JBridge.triggerDownloadForML(language);
  }
}

export const supportsInbuildYoutubePlayer = function () {
  return JBridge.setYoutubePlayer ;
}

export const addCarouselWithVideoExists = function () {
  return JBridge.addCarouselWithVideo;
}

export const addCarouselImpl = function (carouselModalJson, id) {
  const carouselJson = JSON.stringify(carouselModalJson);
  const data = JSON.parse(carouselJson);
  const originalArray = data.carouselData;
  if(JBridge.addCarouselWithVideo){
    return JBridge.addCarouselWithVideo(carouselJson, id);
  }
  else if(JBridge.addCarousel){
    const modifiedArray = originalArray.map(item => ({ image : item.imageConfig.image , title : item.titleConfig.text , description : item.descriptionConfig.text }));
    return JBridge.addCarousel(JSON.stringify(modifiedArray), id);
  }
};

export const storeCallBackLocateOnMap = function (cb) {
  return function (action) {
    return function () {
      try {
        const callback = callbackMapper.map(function (key, lat, lon) {
          if (timerIdDebounce) {
            clearTimeout(timerIdDebounce);
          }
          timerIdDebounce = setTimeout(() => {
            cb(action(key)(lat)(lon))();
          }, 100);
        });
        window.JBridge.storeCallBackLocateOnMap(callback);
      } catch (error) {
        console.log("Error occurred ", error);
      }
    }
  }
}

export const debounceFunction = function (delay) {
  return function (cb) {
    return function (action) {
      return function (isSource) {
        return function () {
          const callback = callbackMapper.map(function () {
            if (timerIdDebounce) clearTimeout(timerIdDebounce);
            timerIdDebounce = setTimeout(() => {
              timerIdDebounce = "MAKEAPICALL";
              cb(action(inputForDebounce)(isSource))();
            }, delay);
          });
          window.callUICallback(callback);
        }
      }
    }
  }
}

export const updateInputString = function (a) {
  console.log("UPDATED STRING " + a);
  inputForDebounce = a;
}
export const isNetworkTimeEnabled = function () {
  if (JBridge.isNetworkTimeEnabled) {
    return JBridge.isNetworkTimeEnabled();
  }
  return true;
}

export const renderCameraProfilePicture = function (id) {
  return function () {
    if (JBridge.renderCameraProfilePicture) {
      return JBridge.renderCameraProfilePicture(id);
    }
  };
};

export const isNotificationPermissionEnabled = function () {
  return function() {
    if (window.JBridge.isNotificationPermissionEnabled) {
      return window.JBridge.isNotificationPermissionEnabled();
    } else {
      return false;
    }
  }
}

export const displayBase64Image = (configObj) => {
  try {
    console.log("displayBase64Image success");
    return JBridge.displayBase64Image(JSON.stringify(configObj));
  } catch (err) {
    try{
      console.log("displayBase64Image error " + err);
      // Deprecated on 4th Jan 2024
      return JBridge.renderBase64ImageFile(configObj.source, configObj.id, false, configObj.scaleType);
    }catch(err2){
      console.log("displayBase64Image error " + err2);
    }
  }
} 

export const datePickerImpl = function (cb , action, delay){
  const callback = callbackMapper.map(function (str, year, month, date) {
    cb(action(str)(year)(month)(date))();
  })
  window.JBridge.datePicker(callback, "");
}

export const timePickerImpl = function (cb , action, delay){
  const callback = callbackMapper.map(function (hour, min, str) {
    cb(action(hour)(min)(str))();
  })
  window.JBridge.timePicker(callback);
}

export const renderSliderImpl = (cb, action, config) => {
  
  const callback = callbackMapper.map(function (val) {
    cb(action(parseInt(val)))();
  });
  const { id, sliderConversionRate, sliderMinValue, sliderMaxValue, sliderDefaultValue, toolTipId, enableToolTip, progressColor, thumbColor, bgColor, bgAlpha } = config;
  const configg =  { id, sliderConversionRate, sliderMinValue, sliderMaxValue, sliderDefaultValue, toolTipId, enableToolTip, progressColor, thumbColor, bgColor, bgAlpha, callback };

  window.JBridge.renderSlider(JSON.stringify(configg));
};
