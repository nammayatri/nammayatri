"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.dynamicImport = void 0;

function _typeof(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

function _getRequireWildcardCache() { if (typeof WeakMap !== "function") return null; var cache = new WeakMap(); _getRequireWildcardCache = function _getRequireWildcardCache() { return cache; }; return cache; }

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } if (obj === null || _typeof(obj) !== "object" && typeof obj !== "function") { return { "default": obj }; } var cache = _getRequireWildcardCache(); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj["default"] = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

// export const dynamicImport = function (func) {
//     return function (onError, onSuccess) {
//         const headID = document.getElementsByTagName("head")[0];
//         const newScript = document.createElement("script");
//         newScript.type = "text/javascript";
//         var url = "http://" + "192.168.1.36" + ":" + "8081";
//         newScript.onload = function () {
//             console.log("dynamic import file customerUtils " + func);
//             // import("./../../output/Screens.CustomerUtils.Flow/index.js").then(module => {
//             import("./../Screens.CustomerUtils.Flow").then(module => {
//                 console.log("onSuccess import " + func);
//                 onSuccess(module[func]);
//             }
//             ).catch(e => console.error("error in dynamic import CustomerUtils", e));
//             return function (cancelError, onCancelerError, onCancelerSuccess) {
//                 onCancelerSuccess();
//             };
//         };
//         newScript.src = url + '/dist/2.index_bundle.js';
//         console.log("inner script", newScript.innerHTML);
//         headID.appendChild(newScript);
//         // newScript.onload();
//     };
// };
var dynamicImport = function dynamicImport(func) {
  return function (onError, onSuccess) {
    var headID = document.getElementsByTagName("head")[0];
    var newScript = document.createElement("script");
    newScript.type = "text/javascript";
    var url = "http://" + "192.168.1.36" + ":" + "8081";

    newScript.onload = function () {
      console.log("dynamic import file customerUtils " + func);
      Promise.resolve().then(function () {
        return _interopRequireWildcard(require("./../Screens.CustomerUtils.Flow"));
      }).then(function (module) {
        console.log("onSuccess import " + func);
        onSuccess(module[func]);
      })["catch"](function (e) {
        return console.error("error in dynamicFlow2", e);
      });
      return function (cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };

    newScript.src = url + '/dist/2.index_bundle.js'; // newScript.innerHTML = window.DUIGatekeeper.loadFileInDUI("0.index_bundle.js");

    console.log("inner script", newScript.innerHTML);
    headID.appendChild(newScript); // newScript.onload();
    // callImport();
  };
};

exports.dynamicImport = dynamicImport;