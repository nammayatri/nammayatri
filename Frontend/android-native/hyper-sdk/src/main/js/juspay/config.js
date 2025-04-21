window.config_version = "2.0.325";
/* DO NOT TO ABOVE LINE*/

/*
 * JUSPAY CONFIDENTIAL
 * __________________
 *
 * [2012] - [2015] JusPay Technologies Pvt Ltd
 * All Rights Reserved.
 *
 * NOTICE:  All information contained herein is, and remains
 * the property of JusPay Technologies Pvt Ltd. The intellectual
 * and technical concepts contained
 * herein are proprietary to JusPay Technologies Pvt Ltd
 * and may be covered by Indian Patents Office and Foreign Patents,
 * patents in process, and are protected by trade secret or copyright law.
 * Dissemination of this information or reproduction of this material
 * is strictly forbidden unless prior written permission is obtained
 * from JusPay Technologies Pvt Ltd.
 */

window.version = window.version || {};
window.version["in.juspay.hyperos"] = window.version["in.juspay.hyperos"] || {};
window.version["in.juspay.hyperos"]["config"] = window.config_version;
window.testUpdate = false;

var sdkVersion = "rc.00"
var sdkRootVersion = "2.0.0";

try {
    sdkVersion = JBridge.getResourceByName("godel_build_version");
    sdkRootVersion = JBridge.getResourceByName("godel_version");
} catch(e) {

}

if (JBridge && JBridge.setWhiteListedEvents) {
    JBridge.setWhiteListedEvents(['initiate_result', 'process_result', 'hide_loader', 'show_loader', 'log_stream', 'user_event', 'paymentAttempt']);
}

try {
    window.webkit.messageHandlers.IOS.postMessage(JSON.stringify({
        methodName: "runInUI",
        parameters: [{
                invokeOn: "UIDevice",
                methodName: "currentDevice",
                return: "CURRENT_DEVICE"
            },
            {
                return : "DEVICE_NAME",
                invokeOn: "this",
                methodName: "name",
                fromStore: "true",
                storeKey: "CURRENT_DEVICE"
            },
            {
                return : "deviceNameScript",
                invokeOn: "NSString",
                methodName: "stringWithFormat:",
                values: [{ "name": "window.DEVICE_NAME = '", type: "s" }]
            },
            {
                return : "deviceNameScript",
                invokeOn: "this",
                methodName: "stringByAppendingString:",
                fromStore: "true",
                storeKey: "deviceNameScript",
                values: [{ "name": "DEVICE_NAME", computed: "true" }]
            },
            {
                return : "deviceNameScript",
                invokeOn: "this",
                methodName: "stringByAppendingString:",
                fromStore: "true",
                storeKey: "deviceNameScript",
                values: [{ "name": "';", type: "s" }]
            },
            {
                return : "SELF",
                invokeOn: "self",
                methodName: "self"
            },
            {
                invokeOn: "this",
                methodName: "loadJS:",
                fromStore: "true",
                storeKey: "SELF",
                values: [{ "name": "deviceNameScript", computed: "true" }]
            }
        ]
    }));

} catch (e) {
// Ignored
}

//----------------------------- Javascript Polyfill -------------------------------//
try{
    var bundle_payload_var = JSON.parse(JBridge.getSessionAttribute("bundleParams","{}"));
    var clientIdVar = "";
    if (bundle_payload_var && bundle_payload_var.payload){
        var backupClientIdVar = bundle_payload_var.payload.clientId;
        if(backupClientIdVar && backupClientIdVar != null && backupClientIdVar != "") {
            var clientIdVar = backupClientIdVar;
        }
        var merchantIdVar = (clientIdVar && clientIdVar != "null") ? clientIdVar.split("_")[0] : "";
        if(merchantIdVar == "olacabs" && (bundle_payload_var.payload.customerId == "15106054" || bundle_payload_var.payload.customerId == "33211348" )){ // rajarajan's and venkatesh customerId for ola -- bundle_payload_var.payload.customerId == "17909377" || bundle_payload_var.payload.customerId == "29418125"
            Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_true;",null);
        }
    }
}catch(e){
}
if (!Array.prototype.includes) {
  Object.defineProperty(Array.prototype, 'includes', {
    value: function(searchElement, fromIndex) {

      if (this == null) {
        throw new TypeError('"this" is null or not defined');
      }

      // 1. Let O be ? ToObject(this value).
      var o = Object(this);

      // 2. Let len be ? ToLength(? Get(O, "length")).
      var len = o.length >>> 0;

      // 3. If len is 0, return false.
      if (len === 0) {
        return false;
      }

      // 4. Let n be ? ToInteger(fromIndex).
      //    (If fromIndex is undefined, this step produces the value 0.)
      var n = fromIndex | 0;

      // 5. If n ≥ 0, then
      //  a. Let k be n.
      // 6. Else n < 0,
      //  a. Let k be len + n.
      //  b. If k < 0, let k be 0.
      var k = Math.max(n >= 0 ? n : len - Math.abs(n), 0);

      function sameValueZero(x, y) {
        return x === y || (typeof x === 'number' && typeof y === 'number' && isNaN(x) && isNaN(y));
      }

      // 7. Repeat, while k < len
      while (k < len) {
        // a. Let elementK be the result of ? Get(O, ! ToString(k)).
        // b. If SameValueZero(searchElement, elementK) is true, return true.
        if (sameValueZero(o[k], searchElement)) {
          return true;
        }
        // c. Increase k by 1.
        k++;
      }

      // 8. Return false
      return false;
    }
  });
}

if (!Array.prototype.findIndex) {
  Object.defineProperty(Array.prototype, 'findIndex', {
    value: function(predicate) {
     // 1. Let O be ? ToObject(this value).
      if (this == null) {
        throw new TypeError('"this" is null or not defined');
      }

      var o = Object(this);

      // 2. Let len be ? ToLength(? Get(O, "length")).
      var len = o.length >>> 0;

      // 3. If IsCallable(predicate) is false, throw a TypeError exception.
      if (typeof predicate !== 'function') {
        throw new TypeError('predicate must be a function');
      }

      // 4. If thisArg was supplied, let T be thisArg; else let T be undefined.
      var thisArg = arguments[1];

      // 5. Let k be 0.
      var k = 0;

      // 6. Repeat, while k < len
      while (k < len) {
        // a. Let Pk be ! ToString(k).
        // b. Let kValue be ? Get(O, Pk).
        // c. Let testResult be ToBoolean(? Call(predicate, T, « kValue, k, O »)).
        // d. If testResult is true, return k.
        var kValue = o[k];
        if (predicate.call(thisArg, kValue, k, o)) {
          return k;
        }
        // e. Increase k by 1.
        k++;
      }

      // 7. Return -1.
      return -1;
    },
    configurable: true,
    writable: true
  });
}

//Polyfill URL constructor
try {
    (function(t){var e=function(){try{return!!Symbol.iterator}catch(e){return false}};var r=e();var n=function(t){var e={next:function(){var e=t.shift();return{done:e===void 0,value:e}}};if(r){e[Symbol.iterator]=function(){return e}}return e};var i=function(e){return encodeURIComponent(e).replace(/%20/g,"+")};var o=function(e){return decodeURIComponent(String(e).replace(/\+/g," "))};var a=function(){var a=function(e){Object.defineProperty(this,"_entries",{writable:true,value:{}});var t=typeof e;if(t==="undefined"){}else if(t==="string"){if(e!==""){this._fromString(e)}}else if(e instanceof a){var r=this;e.forEach(function(e,t){r.append(t,e)})}else if(e!==null&&t==="object"){if(Object.prototype.toString.call(e)==="[object Array]"){for(var n=0;n<e.length;n++){var i=e[n];if(Object.prototype.toString.call(i)==="[object Array]"||i.length!==2){this.append(i[0],i[1])}else{throw new TypeError("Expected [string, any] as entry at index "+n+" of URLSearchParams's input")}}}else{for(var o in e){if(e.hasOwnProperty(o)){this.append(o,e[o])}}}}else{throw new TypeError("Unsupported input's type for URLSearchParams")}};var e=a.prototype;e.append=function(e,t){if(e in this._entries){this._entries[e].push(String(t))}else{this._entries[e]=[String(t)]}};e.delete=function(e){delete this._entries[e]};e.get=function(e){return e in this._entries?this._entries[e][0]:null};e.getAll=function(e){return e in this._entries?this._entries[e].slice(0):[]};e.has=function(e){return e in this._entries};e.set=function(e,t){this._entries[e]=[String(t)]};e.forEach=function(e,t){var r;for(var n in this._entries){if(this._entries.hasOwnProperty(n)){r=this._entries[n];for(var i=0;i<r.length;i++){e.call(t,r[i],n,this)}}}};e.keys=function(){var r=[];this.forEach(function(e,t){r.push(t)});return n(r)};e.values=function(){var t=[];this.forEach(function(e){t.push(e)});return n(t)};e.entries=function(){var r=[];this.forEach(function(e,t){r.push([t,e])});return n(r)};if(r){e[Symbol.iterator]=e.entries}e.toString=function(){var r=[];this.forEach(function(e,t){r.push(i(t)+"="+i(e))});return r.join("&")};t.URLSearchParams=a};if(!("URLSearchParams"in t)||new t.URLSearchParams("?a=1").toString()!=="a=1"){a()}var s=t.URLSearchParams.prototype;if(typeof s.sort!=="function"){s.sort=function(){var r=this;var n=[];this.forEach(function(e,t){n.push([t,e]);if(!r._entries){r.delete(t)}});n.sort(function(e,t){if(e[0]<t[0]){return-1}else if(e[0]>t[0]){return+1}else{return 0}});if(r._entries){r._entries={}}for(var e=0;e<n.length;e++){this.append(n[e][0],n[e][1])}}}if(typeof s._fromString!=="function"){Object.defineProperty(s,"_fromString",{enumerable:false,configurable:false,writable:false,value:function(e){if(this._entries){this._entries={}}else{var r=[];this.forEach(function(e,t){r.push(t)});for(var t=0;t<r.length;t++){this.delete(r[t])}}e=e.replace(/^\?/,"");var n=e.split("&");var i;for(var t=0;t<n.length;t++){i=n[t].split("=");this.append(o(i[0]),i.length>1?o(i[1]):"")}}})}})(typeof global!=="undefined"?global:typeof window!=="undefined"?window:typeof self!=="undefined"?self:this);(function(h){var e=function(){try{var e=new h.URL("b","http://a");e.pathname="c%20d";return e.href==="http://a/c%20d"&&e.searchParams}catch(e){return false}};var t=function(){var t=h.URL;var e=function(e,t){if(typeof e!=="string")e=String(e);var r=document,n;if(t&&(h.location===void 0||t!==h.location.href)){r=document.implementation.createHTMLDocument("");n=r.createElement("base");n.href=t;r.head.appendChild(n);try{if(n.href.indexOf(t)!==0)throw new Error(n.href)}catch(e){throw new Error("URL unable to set base "+t+" due to "+e)}}var i=r.createElement("a");i.href=e;if(n){r.body.appendChild(i);i.href=i.href}if(i.protocol===":"||!/:/.test(i.href))
    {throw new TypeError("Invalid URL")}Object.defineProperty(this,"_anchorElement",{value:i});var o=new h.URLSearchParams(this.search);var a=true;var s=true;var c=this;["append","delete","set"].forEach(function(e){var t=o[e];o[e]=function(){t.apply(o,arguments);if(a){s=false;c.search=o.toString();s=true}}});Object.defineProperty(this,"searchParams",{value:o,enumerable:true});var f=void 0;Object.defineProperty(this,"_updateSearchParams",{enumerable:false,configurable:false,writable:false,value:function(){if(this.search!==f){f=this.search;if(s){a=false;this.searchParams._fromString(this.search);a=true}}}})};var r=e.prototype;var n=function(t){Object.defineProperty(r,t,{get:function(){return this._anchorElement[t]},set:function(e){this._anchorElement[t]=e},enumerable:true})};["hash","host","hostname","port","protocol"].forEach(function(e){n(e)});Object.defineProperty(r,"search",{get:function(){return this._anchorElement["search"]},set:function(e){this._anchorElement["search"]=e;this._updateSearchParams()},enumerable:true});Object.defineProperties(r,{toString:{get:function(){var e=this;return function(){return e.href}}},href:{get:function(){return this._anchorElement.href.replace(/\?$/,"")},set:function(e){this._anchorElement.href=e;this._updateSearchParams()},enumerable:true},pathname:{get:function(){return this._anchorElement.pathname.replace(/(^\/?)/,"/")},set:function(e){this._anchorElement.pathname=e},enumerable:true},origin:{get:function(){var e={"http:":80,"https:":443,"ftp:":21}[this._anchorElement.protocol];var t=this._anchorElement.port!=e&&this._anchorElement.port!=="";return this._anchorElement.protocol+"//"+this._anchorElement.hostname+(t?":"+this._anchorElement.port:"")},enumerable:true},password:{get:function(){return""},set:function(e){},enumerable:true},username:{get:function(){return""},set:function(e){},enumerable:true}});e.createObjectURL=function(e){return t.createObjectURL.apply(t,arguments)};e.revokeObjectURL=function(e){return t.revokeObjectURL.apply(t,arguments)};h.URL=e};if(!e()){t()}if(h.location!==void 0&&!("origin"in h.location)){var r=function(){return h.location.protocol+"//"+h.location.hostname+(h.location.port?":"+h.location.port:"")};try{Object.defineProperty(h.location,"origin",{get:r,enumerable:true})}catch(e){setInterval(function(){h.location.origin=r()},100)}}})(typeof global!=="undefined"?global:typeof window!=="undefined"?window:typeof self!=="undefined"?self:this);
} catch(e) {
    console.error(e);
}

//----------------------------- --------------------- -------------------------------//


function getOS() {
    if (window.__OS && typeof window.__OS == "string"){
        return window.__OS
    }
    var userAgent = navigator.userAgent;
    if (!userAgent) return console.error(new Error("UserAgent is null"));
    if (userAgent.indexOf("Android") != -1 && userAgent.indexOf("Version") != -1) return "ANDROID";
    if ((userAgent.indexOf("iPhone") != -1 || userAgent.indexOf("iPad") != -1 || userAgent.indexOf("Macintosh") != -1) && userAgent.indexOf("Version") == -1) return "IOS";
    return "WEB";
}

try {
    var juspayDeviceId = "";
    var variables = JBridge.getConfigVariables();
    console.log("config: " + variables);
    if(getOS() == 'ANDROID') {
        eval(variables);
    } else {
        window.eval(variables);
    }

    try {
        var bundle_payload = JSON.parse(JBridge.getSessionAttribute("bundleParams","{}"));
        try {
            var weirdPayload = JSON.parse(bundle_payload.client_id);
            var weirdClientId = weirdPayload.payload.clientId;
        }
        catch(err) {
            var weirdClientId = "";
        }

        if (bundle_payload && bundle_payload.payload){
            var backupClientId = bundle_payload.payload.clientId || bundle_payload.payload.client_id || weirdClientId;
            if(( !clientId || clientId == "null" || clientId == "") && backupClientId && backupClientId != null && backupClientId != "") {
                clientId = backupClientId;
            }
        }
    } catch(e) {
        console.error("CLIENT_ID POLYFILL TRIGGERED EXCEPTION :: ", e);
    }

    var merchantId = (clientId && clientId != "null") ? clientId.split("_")[0] : "";
} catch(err) {
    console.error(err)
    var merchantId = "";
}

//Merchant specific maps clientId mapps
if (merchantId && merchantId.indexOf("swiggy") != -1) {
    // convert com.swiggy to swiggy
    merchantId = "swiggy"
}

var godel_remotes_version = "";
try {
    godel_remotes_version = JBridge.getResourceByName("godel_remotes_version");
} catch (err) {}

var isWrapper = false;
try {
    var wrapper_version = JBridge.getResourceByName("wrapper_version");
    var wrapper_build_version = JBridge.getResourceByName("wrapper_build_version");
    isWrapper = wrapper_version !== "" && wrapper_build_version !== "";
} catch (err) {
    console.log("This is not a wrapper build");
}

var beckn_version = "";
try{
    beckn_version = JBridge.getResourceByName("beckn_version");
}catch(err){
    console.log("beckn remote version is not available");
}

var vies_remotes_version = "";
try{
    vies_remotes_version = JBridge.getResourceByName("vies_remotes_version");
}catch(err){
    console.error("vies remote version is not available");
}

var bucket_path = "https://assets.juspay.in";
try{
    var dev = JBridge.getResourceByName("development");
    if (dev) {
        bucket_path = "https://dpj0m1myaqz8f.cloudfront.net";
    }
}catch(err){
    console.log("This is not a development build");
}


var checkout_remotes_version = "";
try {
    checkout_remotes_version = JBridge.getResourceByName("checkout_remotes_version");
} catch(err) {
    checkout_remotes_version = godel_remotes_version;
}

var upi_remotes_version = "";
try {
    upi_remotes_version = JBridge.getResourceByName("upi_remotes_version");
} catch(err) {
    upi_remotes_version = godel_remotes_version;
}

var ec_remotes_version = "";
try {
    ec_remotes_version = JBridge.getResourceByName("ec_remotes_version");
} catch(err) {
    ec_remotes_version = godel_remotes_version;
}

// PolyFill
var godel_version = "";
try {
    godel_version = JBridge.getResourceByName("godel_version");
} catch(err) {}

if(getOS() == "ANDROID" && (godel_version == "1.0.4" || godel_version == "1.0.4.1" || godel_version == "1.0.4.1_2")) {
    godel_remotes_version = "1.0rc2"
}

var OS = "";
if(getOS() == "IOS") {
    OS = "/IOS";
}

var newOS = "common"
switch(getOS()) {
    case "ANDROID" :
        newOS = "android"
        break;
    case "IOS":
        newOS = "ios"
        break;
    case "WEB":
        newOS = "web"
        break;
    default :
        console.error("NO OS AVAILABLE")
}

var dreamplugBetaUsers = [
    "8568d67a13b022435334db188e9dbdbe9676d7c665581989fd7f5854d165afd6",
    "e9fca297d2f05b95b0c701188012ed7e8798af26ea7eb07bc2153bd498514317",
    "6e5ed80461e1f7bd88c918d877b032a739eb3f77f620164d5aa8adb14d3d4bcb",
    "0706ea2986b65f0e2ae6fd3471d5b589da5c4d92601c251a7b431abe09314d6f",
    "2facff58598c76eb43d931b456f93420dd5e3dab146384e4f2f6726b007e3320",
    "2ab5d9207782f236779f20ece92301518867ef8306b4bcd63658122cf33cdfbe",
    "2da822e5a7ea742ed76988f74102003e1e18f60097e1b065e24a1931190cbe41",
    "beaf260b96c8ea0d3d9279e3c7d6101698122661108860c0759e080c8edfc7f9",
    "09e25a2558bad6dc298b206bb740598c424c9b56a1d979bfb88898394db10e59",
    "e06c3bc1990485a1bea4cb4c1ab677e8e1e4b17449bde1f0f326bc6519a57e3f",
    "d078d2b6f9343b0fe64d4247980a8c4baecb154a7a9fae55e4fec4ba7710d861",
    "8c291913067979fa43b15bd29dd6cf78aaa5b143a17214793e7d6dd1ca3b350c",
    "883d060fd3c2d3aeadf3b7d3313fede25831c5f179fc8a2ddb37d6030f9be0d6",
    "75c6e2d9a06afdd180c7cb97a5738e7e5fb4c69633f86fcde41606cf3d284af0",
    "e41a4db8dc8fc0e53c72327c0147f22168e642753373061c6dea88c4956424c8",
    "bfaee89f64da874069bb305c4b4309e41fb2818181a8d9d1f92d85d8fe06e794",
    "ac2dcf2c4da794cc9a50edca2656d225244d748c2fc87e5d93b25c1335a778af" //[TEST] george varghese Juspay
]

var juspayBetaUsers = [
    "fcd45a95943ee82bab08d7942897ebafbde554c9f6a3fb5166edf9ca73094b6a", // naman
    "1a8d7c0d4ccd3d02e367c9bcba2ff2dea3b8bd183e04a22e1899df7e62f8d260", // harshith
    "76b32fca608adc29dffb0978cf62820f34d05ba03ce74020a269c1130556e0f3", // sri harsha
    "6ccf2feb8d58a5a1a059286a97abd2afc3c5dadf8e406ee987b0480f708cb43d", // parasuram
    "1a8d7c0d4ccd3d02e367c9bcba2ff2dea3b8bd183e04a22e1899df7e62f8d260", // harshith
    "85f5c9974e2f9544d94ffa7c5d1ab908bee365bc8272977e19a3c3f36ccf369a", // juspaytest
    "1d813a7669edc6a97db02b70e299faf08c69b57b13e1dd44649e06f3b2567d96", // juspaytest
    "00d3e4ae1c014ce434c70f139a51eec7c8588800f1c784280443f8e34a986b92", // juspaytest
    "db43eda0367ddc59a7f0031b71653908a0c8b7b776a98ab76dce415a7c7ec3dc", // ishan
    "0855e1eba0f2fec5607daf387cb2b6f0aa2178f6ecc9c8944ef10fb1e1cc9ff3", // Krunal
    "262bc7460b20f08d2ddf6cc0c01827bb025c79eca3431b7baa934c437263e7b0", // Kaushal
    "b5f804a56ef195860c66d5d2e00173425e464b4b7c491c42e5f366afe6774510" //oneplus 6T testing device
]

var defaultIndex = function (remoteVersion, merchants) {
    return {
        merchants: merchants || [],
        remoteVersion : remoteVersion || "2.0rc1",
        shouldGetRemotesVerison: true,
        osBased: true,
        newPath: false
    }
}

var defaultConfig = function (merchants, useNew, remoteVersion) {
    return {
        merchants: merchants || [],
        remoteVersion : remoteVersion || "2.0.0",
        shouldGetRemotesVerison: useNew || false,
        osBased: useNew || false,
        newPath: useNew,
    }
}

try {

    //Remove this after Unification bundle is stabilized
    var isNewTOISdk = function() {
        var rcSplit = [];
        try {
            rcSplit = sdkVersion.split("rc.");
        }
        catch(err) {
            console.warn("SDK version not found");
            return false;
        }
        var rootVersionSplit = sdkRootVersion.split(".");
        var isRootVersionValid = (rootVersionSplit[0] == "2") && (rootVersionSplit[1] == "0") && (parseInt(rootVersionSplit[2]) > 3)
        return (isRootVersionValid || (sdkRootVersion == "2.0.3" && (parseInt(rcSplit[1]) >= 41)))
    }

    var upiintentSpecificMerchants = function(useBeta) {
        var merchants = ["curefit", "cars24"];
        if (merchantId == 'TOI' && isNewTOISdk()) {
          merchants.push(merchantId.toLowerCase());
        }
        return merchants;
    }

    var ecSpecificMerchants = function(useBeta) {
        var merchants = ["gaana", "firstcry", "olacabs", "slice"];
        var nonSwiggySdkAndroid = ["2.0.0", "2.0.1", "2.0.2", "2.0.3"]
        if(getOS() == "ANDROID" && nonSwiggySdkAndroid.indexOf(sdkRootVersion) == -1) {
          merchants.push("swiggy")
        }
        return merchants;
    }

    var flyerSpecificMerchants = function(useBeta) {
        var merchants = [];
        return merchants;
    }

    var merchantConfig = function(useBeta) {
        var config = {
        "in.juspay.dotp": {
            "index_bundle": defaultIndex(checkout_remotes_version, ["bms", "onecard", "TUL", "tul", "timesprime", "cars24"]),
            "config": defaultConfig(["onecard"])
        },
        "in.juspay.ec": {
            "index_bundle": defaultIndex(ec_remotes_version, ecSpecificMerchants(useBeta)), // adding meesho till beta moves to release
            "config": defaultConfig([])
        },
        "in.juspay.upiintent": {
            "index_bundle": defaultIndex(ec_remotes_version, upiintentSpecificMerchants(useBeta)),
            "config": defaultConfig([])
        },
        "in.juspay.vies": {
            "index_bundle": defaultIndex(vies_remotes_version, ["zee5"]),
            "config": defaultConfig(["zee5"])
        },
        "in.juspay.flyer": {
            "index_bundle": defaultIndex("2.0rc1", flyerSpecificMerchants(useBeta)),
        },
        "in.juspay.hyperpay" : {
            "configuration": defaultConfig([merchantId], true)
        },
        }
      return config;
    }
} catch(e) {
    console.log("CRITICAL :: TOBE FIXED" , e)
}

var getPath = function (fileConfig, service, useBeta, fileName) {
    var beta = useBeta ? "beta" : "release";
    if (fileConfig.newPath) {
        var remoteVersion = fileConfig.shouldGetRemotesVerison ? fileConfig.remoteVersion : "config"
        var merchantName = fileConfig.merchants.indexOf(merchantId.toLowerCase()) != -1 ? merchantId.toLowerCase() : "common"
        var osName = fileConfig.osBased ? newOS : "common"
        // Sample path :: hyper/bundles/release/in.juspay.hyperpay/IOS/2.0rc1/common/stagger/v1-index_bundle.jsa
        return bucket_path + "/hyper/bundles/" + osName + "/" + beta + "/" + service + "/" + remoteVersion + "/" + merchantName + "/stable/v1-" + fileName + ".zip"
    } else {
        var remoteVersion = fileConfig.shouldGetRemotesVerison ? "/" + fileConfig.remoteVersion : ""
        var merchantName = fileConfig.merchants.indexOf(merchantId.toLowerCase()) != -1 ? "." + merchantId.toLowerCase() : ""
        var osName = fileConfig.osBased ? OS : ""
        // Sample path :: juspay/payments/in.juspay.hyperpay.merchantId/release/2.0rc1/index_bundle.js
        return bucket_path + "/juspay/payments/" + service + merchantName + "/" + beta + osName + remoteVersion + "/v1-" + fileName + ".zip"
    }
}

var makeAssets = function (service, useBeta) {
    var assets = {}
    var merConf = merchantConfig(useBeta)[service]
    for (var file in merConf) {
        assets[file] = getPath(merConf[file], service, useBeta, file);
    }
    return assets;
}

const indexBundleForOkc = function () {
    try {
        var godel_version = JBridge.getResourceByName("godel_version");
        var gv1 = parseInt(godel_version.split(".")[0]);
        var gv2 = parseInt(godel_version.split(".")[1]);
        var gv3 = parseInt(godel_version.split(".")[2]);
        if ((gv1 >= 2 && gv2 >= 0 && gv3 >= 4) && (merchantId == "okcredit")) {
                return "/juspay/payments/in.juspay.hyperpay.okcredit";
        }
    } catch (e) {
        console.error("Couldn't fetch sdk version " + e);
    }
    return "/juspay/payments/in.juspay.hyperpay";
};

var getHyperPayConfig = function(useBeta) {
    if(merchantId == "olacabs" && (sdkRootVersion == "2.0.0" || sdkRootVersion == "2.0.1")) {
        return {
            "src": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + OS  + "/" + checkout_remotes_version + "/base.zip",
            "version" : "1.0rc10",
            "iconUrl" : "",
            "assets" : {
                "config": bucket_path + "/juspay/payments/in.hyper.pay." + merchantId + "/" + (useBeta || juspayBetaUsers.includes(juspayDeviceId) ? "beta" : "release") + "/v1-config.zip",
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta || juspayBetaUsers.includes(juspayDeviceId) ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "olacabs") {
        return {
            "src": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + OS  + "/" + checkout_remotes_version + "/base.zip",
            "version" : "1.0rc10",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.olanew/" + (useBeta || juspayBetaUsers.includes(juspayDeviceId) ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.olacabs" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.olacabs" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.olacabs" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "bigbasket" || merchantId == "bb") {
        return {
            "src": "",
            "version": "2.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.bigbasket" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.bigbasket" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.bigbasket" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.bigbasket" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "bboutdoor") {
        return {
            "src": "",
            "version": "2.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.bboutdoor" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.bboutdoor" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.bboutdoor" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip",
                "polyfill": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-polyfill.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if(merchantId == "udaan") {
         return {
              "src": "",
              "version" : "2.0rc1",
              "iconUrl" : "",
              "assets" : {
                  "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/release" : ".udaan/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                  "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.udaan" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                  "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.udaan" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip",
                  "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.udaan" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
              },
              "root": "payments/in.juspay.hyperpay/",
              "entry": "base.html",
              "canOpen": ["in.juspay.ec"]
         }
    } else if(merchantId == "curefit") {
             return {
                  "src": "",
                  "version" : "1.0rc11",
                  "iconUrl" : "",
                  "assets" : {
                        "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.curefit" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                        "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.curefit" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                        "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.curefit" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
                  },
                  "root": "payments/in.juspay.hyperpay/",
                  "entry": "base.html",
                  "canOpen": ["in.juspay.ec", "in.juspay.hyperapi"]
             }
    } else if (merchantId == "idea") {
        return {
              "src": "",
              "version" : "",
              "iconUrl" : "",
              "assets" : {
                    "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                    "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.idea" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                    "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.idea" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                    "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.idea" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip"
                },
              "root": "payments/in.juspay.hyperpay/",
              "entry": "base.html",
              "canOpen": ["in.juspay.ec"]
          }
    } else if (merchantId == "tatasky") {
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.tatasky" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.tatasky" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip",
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
          }
    } else if (merchantId == "tatabinge") {
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.tatabinge" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.tatabinge" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
          }
    } else if (merchantId == "jiosaavn") {
         return {
               "src": "",
               "version" : "",
               "iconUrl" : "",
               "assets" : {
                     "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                     "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.jiosaavn" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                     "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.jiosaavn" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                     "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.jiosaavn" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                     "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.jiosaavn" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
               },
               "root": "payments/in.juspay.hyperpay/",
               "entry": "base.html",
               "canOpen": ["in.juspay.ec"]
           }
    } else if (merchantId == "urbanclap") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.urbanclap" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.urbanclap" + "/" + (useBeta ? "release" : "release") + OS + "/v1-strings.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.urbanclap" + "/" + (useBeta ? "release" : "release") + OS + "/v1-icons.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.urbanclap" + "/" + (useBeta ? "release" : "release") + "/v1-configuration.zip"
        }
        // Live with merchant specific
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        return {
            "src": "",
            "version" : "2.0rc1",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "zee5") {
        var rcSplit = [];
        try {
            rcSplit = sdkVersion.split("rc.");
        }
        catch(err) {
            console.warn("SDK version not found");
        }
        //Remove this after Unification bundle is stabilized
        var indexBundlePath = (sdkRootVersion === "2.0.3") && (parseInt(rcSplit[1]) >= 30) ?
                                bucket_path + "/hyper/bundles/" + newOS + (useBeta ? "/beta" : "/release") + "/in.juspay.hyperpay/" + checkout_remotes_version + "/zee5/stable/v1-index_bundle.zip" :
                                bucket_path + "/juspay/payments/in.juspay.hyperpay.zee5/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip";
        try {
            var endVersion = sdkRootVersion.split(".")[2];
            if(endVersion >= 4) {
                var indexBundlePath = bucket_path + "/hyper/bundles/" + newOS + (useBeta ? "/beta" : "/release") + "/in.juspay.hyperpay/" + checkout_remotes_version + "/zee1/stable/v1-index_bundle.zip";
            }
        } catch (e) {
            //Ignored
        }

        return {
            "src": "",
            "version" : "2.0rc1",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": indexBundlePath,
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.zee5" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.zee5" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.zee5" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "okcredit") {
        var assets = {
            "index_bundle": bucket_path + indexBundleForOkc() + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.okcredit" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.okcredit" + "/" + (useBeta ? "release" : "release") + OS + "/v1-strings.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.okcredit" + "/" + (useBeta ? "release" : "release") + OS + "/v1-icons.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.okcredit" + "/" + (useBeta ? "release" : "release") + "/v1-configuration.zip"
        }
        // Live with merchant specific
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        // if (sdkRootVersion === "2.0.3" && sdkVersion == "rc.86") {
        //     assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".okc/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        // }
        return {
            "src": "",
            "version" : "2.0rc1",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "vodaidea" || merchantId == "vodafone") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodaidea" + "/" + (useBeta ? "release" : "release") + OS + "/v1-configuration.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodaidea" + "/" + (useBeta ? "release" : "release") + OS + "/v1-icons.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodaidea" + "/" + (useBeta ? "release" : "release") + OS + "/v1-strings.zip"
        }
        // Live with merchant specific
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".vodaidea/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        return {
            "src": "",
            "version": "2.0rc1",
            "iconUrl": "",
            "assets": assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "onecard") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.onecard" + "/" + (useBeta ? "release" : "release") + OS + "/v1-configuration.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.onecard" + "/" + (useBeta ? "release" : "release") + OS + "/v1-icons.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.onecard" + "/" + (useBeta ? "release" : "release") + OS + "/v1-strings.zip",
            "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.onecard" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
        }
        // Live with merchant specific
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        return {
            "src": "",
            "version": "",
            "iconUrl": "",
            "assets": assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "croma") {
        return {
              "src": "",
              "version" : "",
              "iconUrl" : "",
              "assets" : {
                    "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                    "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.croma" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                    "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.croma" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip"
                },
              "root": "payments/in.juspay.hyperpay/",
              "entry": "base.html",
              "canOpen": ["in.juspay.ec"]
          }
    } else if (merchantId == "truefan") {
        var isUnifiedSDK = false;
        try {
            if (window.__OS && window.__OS == "IOS") {
                //iOS SDK check
                var iosSDKVersion = JBridge.getResourceByName("CFBundleVersion");
                var iosSDKVersionSplit = iosSDKVersion.split(".");

                isUnifiedSDK = parseInt(iosSDKVersionSplit[0]) >= 2 && (parseInt(iosSDKVersionSplit[1]) >= 1 || (parseInt(iosSDKVersionSplit[1]) == 0 && parseInt(iosSDKVersionSplit[2]) >= 92)); // iOS SDK version >= 2.0.92
            }
            else {
                //Android SDK check
                var rcSplit = [];
                rcSplit = sdkVersion.split("rc.");

                var rootSplit = [];
                rootSplit = sdkRootVersion.split(".");

                isUnifiedSDK = parseInt(rootSplit[0]) >= 2 && (parseInt(rootSplit[1]) >= 1 || (parseInt(rootSplit[1]) == 0 && parseInt(rootSplit[2]) >= 3)) && (parseInt(rcSplit[1]) >= 84); // Android SDK version >= 2.0.3-rc.84
            }
        }
        catch(err) {
            console.warn("SDK version not found");
        }

        var indexBundlePath = isUnifiedSDK && !useBeta ?
                                bucket_path + "/hyper/bundles/" + newOS + "/release" + "/in.juspay.hyperpay/" + "2.0.0" + "/truefan/stable/v1-index_bundle.zip" :
                                bucket_path + "/juspay/payments/in.juspay.hyperpay.truefan/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip";

        return {
              "src": "",
              "version" : "",
              "iconUrl" : "",
              "assets" : {
                    "index_bundle": indexBundlePath,
                    "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.truefan" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                    "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "release" : "release") + OS + "/v1-configuration.zip",
                    "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.truefan" + "/" + (useBeta ? "release" : "release") + OS + "/v1-strings.zip",
                    "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.truefan" + "/" + (useBeta ? "release" : "release") + "/v1-icons.zip",
                },
              "root": "payments/in.juspay.hyperpay/",
              "entry": "base.html",
              "canOpen": ["in.juspay.ec"]
          }
    } else if (merchantId == "TOI") {
        //Remove this after Unification bundle is stabilized
        var indexBundlePath = isNewTOISdk() ?
                                bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".toi/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip" : //latest
                                bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".legacy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"; //old
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": indexBundlePath,
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.toi" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.toi" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.toi" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.toi" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
     } else if (merchantId == "qmin") {
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/release" : ".qmin/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.qmin" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.qmin" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.qmin" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
     } else if (merchantId == "astroyogi") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".astroyogi/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.astroyogi" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.astroyogi" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.astroyogi" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.astroyogi" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
        }
        // Live with merchant specific
        // if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
        //     assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".astroyogi/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        // }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
     } else if (merchantId == "trulymadly") {
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".trulymadly/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.trulymadly" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.trulymadly" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.trulymadly" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
     } else if (merchantId.toLowerCase() == "timesprime") {
        var assets = {
            "index_bundle": useBeta
                                ? bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + "beta" + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
                                :  bucket_path + "/hyper/bundles/" + newOS + "/release" + "/in.juspay.hyperpay/" + "2.0.0" + "/timesprime/stable/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.timesprime" + "/" + (useBeta ? "release" : "release") + "/v1-configuration.zip",
            "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.timesprime" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.timesprime" + "/" + (useBeta ? "release" : "release") + "/v1-icons.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.timesprime" + "/" + (useBeta ? "release" : "release") + "/v1-strings.zip"
        }
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") != "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId.toLowerCase() == "acko") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.acko" + "/" + (useBeta ? "release" : "release") + "/v1-configuration.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.acko" + "/" + (useBeta ? "release" : "release") + "/v1-strings.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.acko" + "/" + (useBeta ? "release" : "release") + "/v1-icons.zip",
        }
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".ackostable/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId.toLowerCase().includes("citymall")) {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.citymall" + "/" + (useBeta ? "release" : "release") + "/v1-configuration.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.citymall" + "/" + (useBeta ? "release" : "release") + "/v1-strings.zip",
        }
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".citymall/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip";
        }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "dineout") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".dineout/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.dineout" + "/" + (useBeta ? "release" : "release") + "/v1-icons.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.dineout" + "/" + (useBeta ? "release" : "release") + "/v1-strings.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.dineout" + "/" + (useBeta ? "release" : "release") + OS + "/v1-configuration.zip"
        }
        // if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
        //     assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".dineout/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip";
        // }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "geddit") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.geddit" + "/" + (useBeta ? "release" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.geddit" + "/" + (useBeta ? "release" : "release") + "/v1-strings.zip",
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "cars24") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".cars24/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.cars24/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.cars24" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.cars24" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
        }
        // if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") != "" ) {
        //     assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        // }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "omi") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".omi/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.omi" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.omi" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.omi" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "gyandairy") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".gyandairy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.gyandairy" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.gyandairy" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.gyandairy" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "healthifyme") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/release" : ".healthifyme/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.healthifyme" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.healthifyme" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.healthifyme" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "sachargamingpp") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.sachargamingpp" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.sachargamingpp" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "pocketfm") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.pocketfm" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.pocketfm" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.pocketfm" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "vodafonetv") {
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {

                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".vodafonetv/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodafonetv" + "/" + ("release") + OS + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodafonetv" + "/" + ("release") + OS + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodafonetv" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    }  else if (merchantId == "vodafonemusic") {
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {

                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodafonemusic" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodafonemusic" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodafonemusic" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "stockgro") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "release" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "release" : "release") + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "tagmango") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".tagmango/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.tagmango/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.tagmango" + "/" + ("release") + "/v1-strings.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.tagmango" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
        }
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") != "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId.toLowerCase() === "a23games") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? ".a23games/beta" : ".a23games/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.a23games" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.a23games" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.a23games" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "meesho") {
        var merch = "meesho"
        try {
            var endVersion = sdkRootVersion.split(".")[2];
            if(endVersion >= 4) {
                merch = "meesho1"
            }
        } catch (e) {}
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merch + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.meesho" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.meesho" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "shoppersstop") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/release" : ".shoppersstop/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.shoppersstop" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.shoppersstop" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "lenskart") {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.lenskart" + "/" + (useBeta ? "release" : "release") + "/v1-configuration.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.lenskart" + "/" + (useBeta ? "release" : "release") + "/v1-strings.zip"
        }
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") != "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".lenstable/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId.indexOf("classicrummy") != -1) {
        var assets = {
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.classicrummy" + "/" + ("release") + "/v1-configuration.zip",
            "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.classicrummy" + "/" + ("release") + "/v1-strings.zip",
            "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.classicrummy" + "/" + ("release") + OS + "/v1-icons.zip",
        }
        if( JBridge.loadFileInDUI("payments/in.juspay.hyperpay/v1-configuration.jsa") == "" ) {
            assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".classicrummy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "classplus") {
        return {
            "src": "",
            "version": "",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.classplus" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.classplus" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.classplus" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "frontrow" ) {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? ".frontrow/beta" : ".frontrow/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.frontrow" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.frontrow" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.frontrow" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "onsurity") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".gyandairy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.onsurity" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.onsurity" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "physics" ) {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : ".gyandairy/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.physics" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.physics" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.physics" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if(merchantId.toLowerCase() == "mamaearth"){
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.mamaearth" + (useBeta ? "/beta" : "/beta") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "cloverventures" ) {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? "/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay.cloverventures" + "/" + (useBeta ? "beta" : "release") + "/v1-configuration.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.cloverventures" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.cloverventures" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId === "slice") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "configuration": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + OS + "/v1-configuration.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else {
        var assets = makeAssets("in.juspay.hyperpay", useBeta)
        assets.index_bundle = bucket_path + "/juspay/payments/in.juspay.hyperpay/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip";
        assets.strings = bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip";
        assets.icons = bucket_path + "/juspay/payments/in.juspay.hyperpay." + merchantId + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip";
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : assets,
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    }
}

var getHyperManageConfig = function(useBeta) {
    return {
        "src": "",
        "version" : "2.0rc1",
        "iconUrl" : "",
        "assets" : {},
        "root": "payments/in.juspay.hyperpay/",
        "entry": "base.html",
        "canOpen": []
    }
}

var getUPIAssets = function(useBeta) {
    if(clientId == "goibibo_android") {
        return {
            "config": "",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.upi.goibibo/" + (useBeta ? "beta" : "release") + OS + "/" + upi_remotes_version + "/v1-index_bundle.zip"
        }
    } else {
        return {}
    }
}

var getMerchantForSdkConfig = function (merchantId) {
    var merchantName;
    switch (merchantId) {
        default:
            merchantName = "common";
    }
    return merchantName;
}

var getHyperOSAssets = function(useBeta, service) {
    if(useBeta || juspayBetaUsers.includes(juspayDeviceId) || dreamplugBetaUsers.includes(juspayDeviceId)) {
        var assets = {
            "config": bucket_path + "/juspay/payments/2.0/beta/v1-config.zip",
            "manifest": bucket_path + "/juspay/payments" + OS + "/beta/manifest.json",
        };
        if (isWrapper) {
            assets["polyfill"] = bucket_path + "/juspay/payments/2.0/beta/v1-polyfill.zip";
        }
        if (service == "net.openkochi.yatri" || service == "net.openkochi.yatripartner")
        {
            var merchantName = getMerchantForSdkConfig(merchantId);
            assets["sdk_config"] = bucket_path + "/juspay/beckn/" + service + (useBeta ? "/beta/" : "/release/") + merchantName + "/sdk_config.json";
        }
        return assets;
    } else {
        var assets = {
            "config": bucket_path + "/juspay/payments/2.0/release/v1-config.zip",
            "manifest": bucket_path + "/juspay/payments" + OS + "/release/manifest.json"
        };
        if (isWrapper) {
            assets["polyfill"] = bucket_path + "/juspay/payments/2.0/release/v1-polyfill.zip";
        }
        if (service == "net.openkochi.yatri" || service == "net.openkochi.yatripartner")
        {
            var merchantName = getMerchantForSdkConfig(merchantId);
            assets["sdk_config"] = bucket_path + "/juspay/beckn/" + service + (useBeta ? "/beta/" : "/release/") + merchantName + "/sdk_config.json";
        }
        return assets;
    }
}

var getHyperOSPlaceHolderAssets = function(useBeta) {
    var assets = {
        "tracker" : bucket_path + "/juspay/payments/2.0/" + (useBeta ? "beta" : "release") + "/v1-tracker.zip"
    };
    if (isWrapper) {
        assets["polyfill"] = bucket_path + "/juspay/payments/2.0/" + (useBeta ? "beta" : "release") + "/v1-polyfill.zip";
    }

    var viesCardMerchants = ["olacabs_sandbox_android", "bms_android", "dreamplug_android", "mmt_android" , "olacabs_main_android"] ;
    if (viesCardMerchants.includes(clientId.toLowerCase())){
        assets["FiraMono-Medium"]  = bucket_path + "/hyper/fonts/FiraMono-Medium.ttf";
        assets["Lato-Regular"] = bucket_path + "/hyper/fonts/Lato-Regular.ttf";
    }

    return assets;
}

var getInAppUPIMerchant = function() {
    if (merchantId.includes("slice")) {
        return ".slice";
    }
    return "";
}

var getMerchantIdHyperUpi = function() {
    if (merchantId.includes("slice")) {
        return ".slice";
    }
    if (merchantId.includes("okcredit")) {
        return ".okcredit";
    }
    if (merchantId.includes("groww")) {
        return ".groww";
    }
    if (merchantId.includes("swiggy")) {
        return ".swiggy";
    }
    return "";
}

var getInAppUPIAssets = function(useBeta) {
    var assets = {
        "config": bucket_path + "/juspay/payments/in.juspay.inappupi/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
        "index_bundle":  bucket_path + "/juspay/payments/in.juspay.inappupi" + getInAppUPIMerchant() + "/" + (useBeta ? "beta" : "release") + OS + "/" + "2.0rc1" + "/v1-index_bundle.zip"
    }
    if (isWrapper) {
        assets["polyfill"] = bucket_path + "/juspay/payments/in.juspay.inappupi/" + (useBeta ? "beta" : "release") + "/v1-polyfill.zip"
    }
    return assets;
}

var getHyperUPIAssets = function(useBeta) {
    var assets = {
        "config": bucket_path + "/juspay/payments/in.juspay.hyperupi/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
        "index_bundle":  bucket_path + "/juspay/payments/in.juspay.hyperupi" + getMerchantIdHyperUpi() + "/" + (useBeta ? "beta" : "release") + OS + "/" + "2.0rc1" + "/v1-index_bundle.zip"
    }
    if (isWrapper) {
        assets["polyfill"] = bucket_path + "/juspay/payments/in.juspay.inappupi/" + (useBeta ? "beta" : "release") + "/v1-polyfill.zip"
    }
    return assets;
}

var getECAssets = function (useBeta) {
    // TODO refactor to support optional files
    var assets = makeAssets("in.juspay.ec" , useBeta)
    if (merchantId == "okcredit") {
        assets["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.ec.slice" + "/" + (useBeta ? "beta" : "release") + OS + "/" + "2.0rc1" + "/v1-index_bundle.zip"
    }
    if (isWrapper) {
        assets["polyfill"] = bucket_path + "/juspay/payments/in.juspay.ec/" + (useBeta ? "beta" : "release") + "/v1-polyfill.zip"
    }
    return assets;
}

var getMerchantIdGodel = function(){
    if(merchantId == "cars24")
        return ".cars24"
    if(merchantId == "tatapay")
        return ".tatapay"
    return "";
}

var getGodelAssets = function(useBeta) {
    var addMerchantId = getMerchantIdGodel();
    if (getOS() == "ANDROID") {
        return {
            "config": "https://d3e0hckk6jr53z.cloudfront.net/godel/v1-config.zip",
            "acs_js_source" : bucket_path + "/juspay/payments/in.juspay.godel/" + (useBeta ? "beta" : "release") + "/1.0rc2/v1-acs.zip",
            "boot_loader_js_source": bucket_path + "/juspay/payments/in.juspay.godel/" + (useBeta ? "beta" : "release") + "/" + godel_remotes_version + "/v1-boot_loader.zip",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.godel" + addMerchantId + "/" + ((useBeta || juspayBetaUsers.includes(juspayDeviceId)) ? "beta" : "release") + "/" + godel_remotes_version + "/v1-index_bundle.zip",
            "certificates": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/certificates_v1.zip"
        }
    } else {
        return {
            "config": "https://d3e0hckk6jr53z.cloudfront.net/godel/v1-config.jsa",
            "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/v1-acs.jsa"
        }
    }
}

var getGpayAssets = function (useBeta) {
    if(getOS() == "ANDROID") {
        return {
           "config": bucket_path + "/juspay/payments/in.juspay.gpay/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
           "index_bundle": bucket_path + "/juspay/payments/in.juspay.gpay/" + (useBeta ? "beta" : "release") + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
       }
    } else {
        return {}
    }
}

var getAppList = function(useBeta, service) {
    var viFlyerObj = makeAssets("in.juspay.flyer", useBeta);
    if (merchantId == "vodaidea" || (merchantId == "vodafone" && getOS() == "IOS")) {
        viFlyerObj["index_bundle"] = bucket_path + "/juspay/payments/in.juspay.flyer/release" + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip";
    }
    return {
    "in.juspay.hyperos": {
        "src": "",
        "version": "1.0rc1",
        "iconUrl" : "",
        "assets" : getHyperOSAssets(useBeta, service),
        "root": "/",
        "entry": "",
        "canOpen": []
    },
    "in.juspay.hyperos.placeholder": {
        "src": "",
        "version": "1.0rc1",
        "iconUrl" : "",
        "assets" : getHyperOSPlaceHolderAssets(useBeta),
        "root": "",
        "entry": "",
        "canOpen": []
    },
    "in.juspay.godel": {
        "src": "",
        "version" : "1.0rc1",
        "iconUrl" : "",
        "assets" : getGodelAssets(useBeta),
        "root": "payments/in.juspay.godel/",
        "entry": "base.html",
        "canOpen": []
    },
    "in.juspay.godel.placeholder": {
        "src": "",
        "version" : "1.0rc1",
        "iconUrl" : "",
        "assets" : getGodelAssets(useBeta),
        "root": "payments/in.juspay.godel/",
        "entry": "",
        "canOpen": []
    },
    "in.juspay.gatekeeper": {
        "src": "",
        "version" : "1.0rc1",
        "iconUrl" : "",
        "assets" : {},
        "root": "payments/in.juspay.gatekeeper/",
        "entry": "base.html",
        "canOpen": []
    },
    "in.juspay.gpay": {
        "src": "",
        "version" : "1.0rc1",
        "iconUrl" : "",
        "assets" : getGpayAssets(useBeta),
        "root": "payments/in.juspay.gpay/",
        "entry": "base.html",
        "canOpen": []
    },
    "in.hyper.pay": getHyperPayConfig(useBeta),
    "in.juspay.hyperpay": getHyperPayConfig(useBeta),
    "in.juspay.hypermanage": getHyperManageConfig(useBeta),
    "in.juspay.upimanage": {
       "src": "",
       "version" : "1.0rc1",
       "iconUrl" : "",
       "assets" : {},
       "root": "payments/in.hyper.pay/",
       "entry": "base.html",
       "canOpen": []
    },
    "in.juspay.upiintent": {
        "src": "",
        "version": "0.2rc1",
        "iconUrl": "",
        "assets": makeAssets("in.juspay.upiintent", useBeta),
        "root": "payments/in.juspay.upiintent/",
        "entry": "base.html",
        "canOpen": []
    },
    "in.juspay.hyperapi": {
        "src": "",
        "version": "1.0rc4",
        "iconUrl": "",
        "assets": {},
        "root": "payments/in.juspay.hyperapi/",
        "entry": "base.html",
        "canOpen": ["in.juspay.ec"]
    },
    "in.juspay.inappupi": {
        "src": "",
        "version": "1.0rc1",
        "iconUrl": "",
        "assets": getInAppUPIAssets(useBeta),
        "root": "payments/in.juspay.inappupi/",
        "entry": "base.html",
        "canOpen": []
    },
    "in.juspay.hyperupi": {
        "src": "",
        "version": "1.0rc1",
        "iconUrl": "",
        "assets": getHyperUPIAssets(useBeta),
        "root": "payments/in.juspay.hyperupi/",
        "entry": "base.html",
        "canOpen": ["in.juspay.inappupi"]
    },
    "in.juspay.ec": {
        "src": "",
        "version" : "1.0rc4",
        "iconUrl": "",
        "assets": getECAssets(useBeta),
        "root": "payments/in.juspay.ec/",
        "entry": "base.html",
        "canOpen": ["in.juspay.vies", "in.juspay.dotp", "in.juspay.upiintent", "in.juspay.godel.placeholder", "in.juspay.hyperos.placeholder", "in.juspay.flyer" , "in.juspay.escrow", "in.juspay.hyperupi"]
    },
    "in.juspay.upi": {
        "src": "",
        "version" : "1.0rc1",
        "iconUrl": "",
        "assets": getUPIAssets(useBeta),
        "root": "payments/in.juspay.upi/",
        "entry": "base.html",
        "canOpen": []
    },
    "in.juspay.vies": {
        "src": "",
        "version" : "1.0rc1",
        "iconUrl": "",
        "assets": makeAssets("in.juspay.vies", useBeta),
        "root": "payments/in.juspay.vies/",
        "entry": "base.html",
        "canOpen": []
    },
    "in.juspay.dotp": {
        "src": "",
        "version" : "1.0rc1",
        "iconUrl": "",
        "assets": makeAssets("in.juspay.dotp", useBeta),
        "root": "payments/in.juspay.dotp/",
        "entry": "base.html",
        "canOpen": []
    },"in.juspay.becknuser":{
        "src": "",
        "version": "1.0rc1",
        "iconUrl" : "",
        "assets" : {
            "index_bundle" : bucket_path + "/juspay/beckn/in.juspay.becknuser/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
        },
        "root": "",
        "entry": "becknbase.html",
        "canOpen": []
     },"net.openkochi.yatri":{
        "src": "",
        "version": "1.0rc1",
        "iconUrl" : "",
        "assets" : {
            "index_bundle" : bucket_path + "/juspay/beckn/net.openkochi.yatri/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
        },
        "root": "",
        "entry": "becknbase.html",
        "canOpen": []
     },"in.juspay.becknui":{
        "src": "",
        "version": "1.0rc1",
        "iconUrl" : "",
        "assets" : {
            "index_bundle" : bucket_path + "/juspay/beckn/in.juspay.becknui/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
        },
        "root": "",
        "entry": "becknbase.html",
        "canOpen": []
     },"in.juspay.beckn.transporter":{
        "src": "",
        "version": "1.0rc1",
        "iconUrl" : "",
        "assets" : {
            "index_bundle" : bucket_path + "/juspay/beckn/in.juspay.beckn.transporter/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
        },
        "root": "",
        "entry": "becknbase.html",
        "canOpen": []
     },"net.openkochi.yatripartner":{
        "src": "",
        "version": "1.0rc1",
        "iconUrl" : "",
        "assets" : {
            "index_bundle" : bucket_path + "/juspay/beckn/net.openkochi.yatripartner/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
        },
        "root": "",
        "entry": "becknbase.html",
        "canOpen": []
     },
     "in.juspay.sahay": {
         "src": "",
         "version": "1.0rc1",
         "iconUrl": "",
         "assets": {
            "index_bundle" : bucket_path + "/juspay/in.juspay.sahay/" + (useBeta ? "beta" : "release") + "/1.0rc1/v1-index_bundle.zip"
         },
         "root": "",
         "entry": "sahaybase.html",
         "canOpen": []
     },
     "in.juspay.arya": {
         "src": "",
         "version": "1.0rc1",
         "iconUrl": "",
         "assets": {
            "index_bundle" : bucket_path + "/juspay/in.juspay.arya/" + (useBeta ? "beta" : "release") + "/1.0rc1/v1-index_bundle.zip"
         },
         "root": "",
         "entry": "aryabase.html",
         "canOpen": []
     },
    "in.juspay.escrow": {
        "src": "",
        "version" : "1.0rc1",
        "iconUrl": "",
        "assets": {
            "index_bundle": bucket_path + "/juspay/payments" + "/in.juspay.escrow" + "/" + (useBeta ? "beta" : "release") + "/2.0rc1/v1-index_bundle.zip"
        },
        "root": "payments/in.juspay.escrow/",
        "entry": "base.html",
        "canOpen": []
    },
    "in.juspay.flyer": {
        "src": bucket_path + "/hyper/bundles/" + newOS + (useBeta ? "/beta" : "/release") + "/in.juspay.flyer/common/common/stable/base.html",
        "version": "1.0rc1",
        "iconUrl": "",
        "assets": viFlyerObj,
        "root": "payments/in.juspay.flyer/",
        "entry": "base.html",
        "canOpen": []
    }
};}

var getRollOut = function() {
    return {
        "v1-index_bundle" : 0
    }
}

var getStaggeredReleaseConfig = function() {
   return {
       "in.juspay.hyperpay": {
           "release": {
               "2.0rc1": getRollOut()
           }
       }
   }
}

var updateBlockedHashes = function(){
// Please maintain only last 5 hashes of each mapp to so that the size will not get increased
    var blockHash = {
    "in.juspay.vies" : []
    , "in.juspay.ec": []
    }
    JBridge.addDataToSharedPrefs("jp_external_blocked_hashes",JSON.stringify(blockHash));
    return ;
}

try{
    updateBlockedHashes();
}catch(e){
    console.error("error while calling updateBlockedHashes",e)
}

window.getConfig = function(payload) {
    var useBeta = false;
    var service = false;
    if (typeof payload == "object" && typeof payload.hasOwnProperty == "function") {
        if (payload.hasOwnProperty("betaAssets")) {
            useBeta = payload.betaAssets;
        }
        if (payload.hasOwnProperty("service")){
            service = payload.service;
        }
    }
    try {
        window.__payload = window.__payload || JSON.parse(JBridge.getSessionAttribute("bundleParams"))
    } catch (e) {
        //Ignored
    }

    try {
        if(merchantId == "jiosaavn") {
            jiosaavnVersions0 = [ "rc.6", "rc.14", "rc.16", "rc.17", "rc.25", "rc.26", "rc.27", "rc.49", "rc.74", "rc.85", "rc.99"]
            jiosaavnVersions1 = ["rc.15", "rc.23", "rc.32", "rc.35", "rc.72"]
            jiosaavnVersions2 = ["rc.14", "rc.64"]
            jiosaavnVersions3 = ["rc.14", "rc.39", "rc.42"]
            if (jiosaavnVersions0.indexOf(sdkVersion) != -1 && sdkRootVersion == "2.0.0") {
                useBeta = false;
            } else if (jiosaavnVersions1.indexOf(sdkVersion) != -1 && sdkRootVersion == "2.0.1") {
                useBeta = false;
            } else if (jiosaavnVersions2.indexOf(sdkVersion) != -1 && sdkRootVersion == "2.0.2") {
                useBeta = false;
            } else if (jiosaavnVersions3.indexOf(sdkVersion) != -1 && sdkRootVersion == "2.0.3") {
                useBeta = false;
            }
        }
        if ( getOS() == 'ANDROID' && typeof JBridge.findApps == "function" && (JBridge.findApps("juspay://pay/"+merchantId).indexOf("in.juspay.webview") != -1 )) {
            Android.runInUI("android.webkit.WebView->setWebContentsDebuggingEnabled:b_true;",null);
        }
        if ( getOS() == 'ANDROID' && typeof JBridge.findApps == "function" && (JBridge.findApps("juspay://pay/"+merchantId).indexOf("in.juspay.devtools") != -1  || JBridge.findApps("juspay://pay/"+merchantId).indexOf("in.juspay.forceBeta") != -1) ) {
            useBeta = true
            JBridge.setInSharedPrefs("forceBeta", "true");
        } else if ( getOS() == 'ANDROID' && typeof JBridge.findApps == "function" && window.__payload && window.__payload.pre_fetch != "true") {
            JBridge.setInSharedPrefs("forceBeta", "false");
        } else if ( window.DEVICE_NAME == "Elephant555" ) {
            useBeta = true
        }
        if (JBridge.getFromSharedPrefs("forceBeta") === "true") {
            useBeta = true;
        }
    } catch (e) {
        useBeta = false;
        //  Ignored
    }

    return {
        apps: getAppList(useBeta, service),
        controlPanel: getStaggeredReleaseConfig()
    };
}

try{
    var godel_build_version = "rc.01"
    var godel_version = "2.0.0"
    try {
        godel_build_version = JBridge.getResourceByName("godel_build_version");
        godel_version = JBridge.getResourceByName("godel_version");
    } catch (e) {
        Android.runInUI("set_PM=ctx->getPackageManager;set_cn=android.content.ComponentName->new:ctx_ctx,s_in.juspay.hypersdk.core.CustomtabActivity;get_PM->setComponentEnabledSetting:get_cn,i_2,i_1;",null)
    }
    if( godel_version == "2.0.0" || godel_version == "2.0.1" || (godel_version == "2.0.2" && parseInt(godel_build_version.split(".")[1]) < 15)) {
        Android.runInUI("set_PM=ctx->getPackageManager;set_cn=android.content.ComponentName->new:ctx_ctx,s_in.juspay.hypersdk.core.CustomtabActivity;get_PM->setComponentEnabledSetting:get_cn,i_2,i_1;",null)
    }
    else {
        Android.runInUI("set_PM=ctx->getPackageManager;set_cn=android.content.ComponentName->new:ctx_ctx,s_in.juspay.hypersdk.core.CustomtabActivity;get_PM->setComponentEnabledSetting:get_cn,i_1,i_1;",null)
    }
} catch (e) {
    Android.runInUI("set_PM=ctx->getPackageManager;set_cn=android.content.ComponentName->new:ctx_ctx,s_in.juspay.hypersdk.core.CustomtabActivity;get_PM->setComponentEnabledSetting:get_cn,i_2,i_1;",null)
}

var payload = {
    fileName: "v1-config.jsa"
}
JBridge.runInJuspayBrowser("onBundleLoaded", JSON.stringify(payload), null);


//Zee5 specific logic
try {

    if (sdkRootVersion === "2.0.2" && (sdkVersion === "rc.44" || sdkVersion === "rc.40" || sdkVersion === "rc.27")) {
        var downloadStatus = {
            "index_bundle" : false,
            "config" : false
        }

        var indexUrl = window.getConfig(__payload).apps["in.juspay.dotp"].assets.index_bundle;
        var configUrl = window.getConfig(__payload).apps["in.juspay.dotp"].assets.config;

        window.__BOOT_LOADER = window.__BOOT_LOADER || {};
        window.__BOOT_LOADER['abc'] = (function() {
            console.log(arguments);

            if (arguments[1] === indexUrl) {
                this.downloadStatus.index_bundle = true;
            }

            if (arguments[1] === configUrl) {
                this.downloadStatus.config = true;
            }

            if (this.downloadStatus.config && this.downloadStatus.index_bundle) {
                console.log("reload dotp");
                if (window.mapps && window.mapps["in.juspay.dotp"] && window.mapps["in.juspay.dotp"].contentWindow && window.mapps["in.juspay.dotp"].contentWindow.location && window.mapps["in.juspay.dotp"].contentWindow.location.reload) {
                    console.log("Relaoding Iframe");
                    window.mapps["in.juspay.dotp"].contentWindow.location.reload();
                }
            }
        }).bind({downloadStatus});
        var x = JBridge.getFromSharedPrefs("isFirstTimeLoadZee5");

        if (x !== "TRUE") {
            JBridge.setInSharedPrefs("isFirstTimeLoadZee5", "TRUE");

            console.log("Renewing index_bundle and config");
            JBridge.renewFile(indexUrl, "payments/in.juspay.dotp/v1-index_bundle.zip", "abc");
            JBridge.renewFile(configUrl, "payments/in.juspay.dotp/v1-config.zip",  "abc");
        }
    }
}
catch (err) {
    console.log("Zee5 Polyfill failed", err);
}

// Function for determining whether or not to use encrypted SharedPrefs
var useEncryptedSharedPrefs = function () {

    // Disable on iOS because of incomplete native changes in SDK >= 2.0.36
    if (window.__OS && window.__OS == "IOS") {
        var sdkVersion = JBridge.getResourceByName("CFBundleVersion");
        sdkVersion = sdkVersion.split(".");
        if (parseInt(sdkVersion[0]) >= 2 && ((parseInt(sdkVersion[1]) == 0 && parseInt(sdkVersion[2]) >= 36) || parseInt(sdkVersion[1]) >= 1 && (parseInt(sdkVersion[2]) == 0))) { // iOS SDK version >= 2.0.36 && <= 2.1.0
            return false;
        }
    }

    // Defaults to false; make it true once SDK changes have been rolled out
    return false;
}

window.JOSflags = {
    useEncryptedSharedPrefs: useEncryptedSharedPrefs()
}
