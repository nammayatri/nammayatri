"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.dynamicImport = void 0;

function _typeof(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

function _getRequireWildcardCache() { if (typeof WeakMap !== "function") return null; var cache = new WeakMap(); _getRequireWildcardCache = function _getRequireWildcardCache() { return cache; }; return cache; }

function _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } if (obj === null || _typeof(obj) !== "object" && typeof obj !== "function") { return { "default": obj }; } var cache = _getRequireWildcardCache(); if (cache && cache.has(obj)) { return cache.get(obj); } var newObj = {}; var hasPropertyDescriptor = Object.defineProperty && Object.getOwnPropertyDescriptor; for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) { var desc = hasPropertyDescriptor ? Object.getOwnPropertyDescriptor(obj, key) : null; if (desc && (desc.get || desc.set)) { Object.defineProperty(newObj, key, desc); } else { newObj[key] = obj[key]; } } } newObj["default"] = obj; if (cache) { cache.set(obj, newObj); } return newObj; }

var dynamicImport = function dynamicImport(func) {
  return function (onError, onSuccess) {
    var headID = document.getElementsByTagName("head")[0];
    var newScript = document.createElement("script");
    newScript.type = "text/javascript";
    var url = "http://" + "192.168.1.34" + ":" + "8081";

    newScript.onload = function () {
      console.log("dynamic import file");
      Promise.resolve().then(function () {
        return _interopRequireWildcard(require("./../../output/Screens.OnBoardingFlow.Flow"));
      }).then(function (module) {
        console.log("onSuccess import EnterMobileNumberScreen");
        onSuccess(module[func]);
      })["catch"](function (e) {
        return console.error("error in dHandler", e);
      });
      return function (cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };

    newScript.src = url + "/dist/onBoarding.index_bundle.js";
    console.log("inner script", newScript.innerHTML);
    headID.appendChild(newScript);
  };
};

exports.dynamicImport = dynamicImport;