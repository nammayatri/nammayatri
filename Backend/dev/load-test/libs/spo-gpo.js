(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.spoGpopolyfill = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
var S = 'setPrototypeOf';
var G = 'getPrototypeOf';
var O = Object;
var OP = O.prototype;
var M = 'Cannot convert undefined or null to object';
var gpo = O[G];
var spo = O[S];

if (!spo) {
  if ({ __proto__: null } instanceof O) {
    var has = function(o, k) {
      return OP.hasOwnProperty.call(o, k);
    };
    var _gpo = gpo;
    gpo = function gpo(o) {
      if (o == undefined) throw TypeError(M);
      o = O(o);
      if (has(o, '__proto__')) return o.__proto__;
      if (_gpo) return _gpo(o);
      if (typeof o.constructor === 'function' && o instanceof o.constructor) {
        return o.constructor.prototype;
      }
      return o instanceof O ? OP : null;
    };
    spo = function mixinProperties(o, proto) {
      O.defineProperty(o, '__proto__', {
        value: proto,
        enumerable: false,
        writable: true,
        configurable: true
      });
      var keys = O.getOwnPropertyNames(proto);
      for (var i = 0; i < keys.length; i++) {
        var key = keys[i];
        if (!has(o, key)) {
          var descriptor = O.getOwnPropertyDescriptor(proto, key);
          if (descriptor.configurable) {
            O.defineProperty(o, key, descriptor);
          }
        }
      }
      return o;
    };
  } else {
    spo = function setProtoOf(o, proto) {
      o.__proto__ = proto;
      return o;
    };
  }
}

if (!gpo) {
  gpo = function(o) {
    if (o == undefined) throw TypeError(M);
    o = O(o);
    return o.__proto__;
  };
}

exports[G] = gpo;
exports[S] = spo;
exports.polyfill = function() {
  O[G] = gpo;
  O[S] = spo;
};

},{}],"spo-gpo/polyfill":[function(require,module,exports){
require('./index').polyfill();

},{"./index":1}]},{},[])("spo-gpo/polyfill")
});
