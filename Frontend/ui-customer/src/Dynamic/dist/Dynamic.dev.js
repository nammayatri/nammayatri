"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.dynamicFlow2 = void 0;

function _typeof(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

function _getRequireWildcardCache() { if (typeof WeakMap !== "function") return null; var cache = new WeakMap(); _getRequireWildcardCache = function _getRequireWildcardCache() { return cache; }; return cache; }

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } if (obj === null || _typeof(obj) !== "object" && typeof obj !== "function") { return { "default": obj }; } var cache = _getRequireWildcardCache(); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj["default"] = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// import * as Flow1 from './../Flows.Flow1/index.js';
// export function dflow (onError, onSuccess) {
//     // import (/* webpackChunkName: "DFlow" */"./../DFlow")
//     //     .then(module => {
//     //         console.log("inside dFlow then");
//     //         onSuccess(module.dummyFlow)})
//     //     .catch(e => {
//     //         console.log("inside dFlow catch");
//     //         console.error("error in dflow",e)
//     //     });
//     // return function (cancelError, onCancelerError, onCancelerSuccess) {
//     //   onCancelerSuccess();
//     // };
//     // const headID = document.getElementsByTagName("head")[0];
//     // const newScript = document.createElement("script");
//     // newScript.type = "text/javascript";
//     var headID = document.getElementsByTagName("head")[0];
//     var newScript = document.createElement('script');
//     newScript.type = 'text/javascript';
//     // newScript.id = 'DFlow';
//     newScript.onload = function() {
//       console.log("Trying onload")
//       import("./../../output/DFlow/index.js")
//         .then(module => {
//           console.log("This log proves that dynamic split is working");
//           onSuccess(module.dummyFlow);
//         })
//         .catch(e => {
//           console.log("Proxy ",e);
//           onError("Error",e);
//         });
//     }
//     // newScript.innerHTML = window.DUIGatekeeper.loadFileInDUI("0.index_bundle.js");
//     // // var script = window.DUIGatekeeper.loadFileInDUI("/dist/0.index_bundle.js");
//     // console.log("innerHTML", JSON.stringify(script));
//     // console.log("innerHTML", newScript);
//     // headID.appendChild(newScript);
//     // newScript.onload();
//   };
var dynamicFlow2 = function dynamicFlow2(func) {
  return function (onError, onSuccess) {
    var headID = document.getElementsByTagName("head")[0];
    var newScript = document.createElement("script");
    newScript.type = "text/javascript";
    var url = "http://" + "192.168.1.34" + ":" + "8081";

    newScript.onload = function () {
      console.log("dynamic import file");
      Promise.resolve().then(function () {
        return _interopRequireWildcard(require("./../Flow"));
      }).then(function (module) {
        console.log("onSuccess import");
        onSuccess(module[func]);
      })["catch"](function (e) {
        return console.error("error in dynamicFlow2", e);
      });
      return function (cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };

    newScript.src = url + "/dist/0.index_bundle.js"; // newScript.innerHTML = window.DUIGatekeeper.loadFileInDUI("0.index_bundle.js");

    console.log("inner script", newScript.innerHTML);
    headID.appendChild(newScript); // newScript.onload();
    // callImport();
  };
}; // export const dynamicHomeScreenView = function(func) {
//     return function(onError, onSuccess) {
//     const headID = document.getElementsByTagName("head")[0];
//     const newScript = document.createElement("script");
//     newScript.type = "text/javascript";
//     var url = "http://" + "192.168.1.36" + ":" + "8081";
//     newScript.onload = function() {
//       console.log("dynamic import file");
//       import("./../../output/Screens.HomeScreen.View").then(module => {
//                                             console.log("onSuccess import");
//                                             onSuccess(module[func]);
//                                         }
//                                     ).catch(e => console.error("error in dynamicHomeScreenView",e));
//       return function (cancelError, onCancelerError, onCancelerSuccess) {
//       onCancelerSuccess();
//       };
//     };
//     newScript.src = url + '/dist/1.index_bundle.js';
//     console.log("dynamic url", url + '/dist/1.index_bundle.js');
//     // newScript.innerHTML = window.DUIGatekeeper.loadFileInDUI("0.index_bundle.js");
//     console.log("inner script", newScript.innerHTML);
//     headID.appendChild(newScript);
//     newScript.onload();
// };
// };
// export const dynamicFlow2 = function(fnName)
// {
//   return function(onError,onSuccess){
//     console.log("Hereee");
//     // const src = window.webpackManifest["chunkName"];
//     const headID = document.getElementsByTagName("head")[0];
//     const newScript = document.createElement("script");
//     newScript.type = "text/javascript";
//     // newScript.id = "mystique";
//     // const bundleLoadStart = Date.now();
//     top.JBridge.toast("about to loadFileInDui ");
//     newScript.innerHTML = top.JBridge.loadFileInDUI("0.index_bundle.js");
//     const bundleLoadEnd = Date.now();
//     const obj ={}
//     headID.appendChild(newScript);
//     // top.JBridge.loadFileInDUI("payments/in.juspay.hyperpay" + src);
//     import("dummyImport")
//       .then(objj => {
//         if(window.JBridge && window.JBridge.toast)
//           window.JBridge.toast("This log proves that dynamic split is working fine");
//         console.log("This log proves that dynamic split is working");
//         onSuccess(objj[fnName])
//       })
//       .catch(e => {
//         console.log("Proxy ",e);
//         // send High priority logs here
//         onError("Error",e);
//       });
//   }
// }


exports.dynamicFlow2 = dynamicFlow2;