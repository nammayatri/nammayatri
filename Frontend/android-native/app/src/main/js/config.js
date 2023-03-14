window.config_version = "2.0.167";
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

var sdkVersion = "rc.00"
var sdkRootVerison = "2.0.0";

try {
    sdkVersion = JBridge.getResourceByName("godel_build_version");
    sdkRootVerison = JBridge.getResourceByName("godel_version");
} catch(e) {

}


//----------------------------- Javascript Polyfill -------------------------------//

if (!Array.prototype.includes) {
    Object.defineProperty(Array.prototype, 'includes', {
        value: function (searchElement, fromIndex) {

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
        value: function (predicate) {
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
    (function (t) {
        var e = function () {
            try {
                return !!Symbol.iterator
            } catch (e) {
                return false
            }
        };
        var r = e();
        var n = function (t) {
            var e = {
                next: function () {
                    var e = t.shift();
                    return {
                        done: e === void 0,
                        value: e
                    }
                }
            };
            if (r) {
                e[Symbol.iterator] = function () {
                    return e
                }
            }
            return e
        };
        var i = function (e) {
            return encodeURIComponent(e).replace(/%20/g, "+")
        };
        var o = function (e) {
            return decodeURIComponent(String(e).replace(/\+/g, " "))
        };
        var a = function () {
            var a = function (e) {
                Object.defineProperty(this, "_entries", {
                    writable: true,
                    value: {}
                });
                var t = typeof e;
                if (t === "undefined") {} else if (t === "string") {
                    if (e !== "") {
                        this._fromString(e)
                    }
                } else if (e instanceof a) {
                    var r = this;
                    e.forEach(function (e, t) {
                        r.append(t, e)
                    })
                } else if (e !== null && t === "object") {
                    if (Object.prototype.toString.call(e) === "[object Array]") {
                        for (var n = 0; n < e.length; n++) {
                            var i = e[n];
                            if (Object.prototype.toString.call(i) === "[object Array]" || i.length !== 2) {
                                this.append(i[0], i[1])
                            } else {
                                throw new TypeError("Expected [string, any] as entry at index " + n + " of URLSearchParams's input")
                            }
                        }
                    } else {
                        for (var o in e) {
                            if (e.hasOwnProperty(o)) {
                                this.append(o, e[o])
                            }
                        }
                    }
                } else {
                    throw new TypeError("Unsupported input's type for URLSearchParams")
                }
            };
            var e = a.prototype;
            e.append = function (e, t) {
                if (e in this._entries) {
                    this._entries[e].push(String(t))
                } else {
                    this._entries[e] = [String(t)]
                }
            };
            e.delete = function (e) {
                delete this._entries[e]
            };
            e.get = function (e) {
                return e in this._entries ? this._entries[e][0] : null
            };
            e.getAll = function (e) {
                return e in this._entries ? this._entries[e].slice(0) : []
            };
            e.has = function (e) {
                return e in this._entries
            };
            e.set = function (e, t) {
                this._entries[e] = [String(t)]
            };
            e.forEach = function (e, t) {
                var r;
                for (var n in this._entries) {
                    if (this._entries.hasOwnProperty(n)) {
                        r = this._entries[n];
                        for (var i = 0; i < r.length; i++) {
                            e.call(t, r[i], n, this)
                        }
                    }
                }
            };
            e.keys = function () {
                var r = [];
                this.forEach(function (e, t) {
                    r.push(t)
                });
                return n(r)
            };
            e.values = function () {
                var t = [];
                this.forEach(function (e) {
                    t.push(e)
                });
                return n(t)
            };
            e.entries = function () {
                var r = [];
                this.forEach(function (e, t) {
                    r.push([t, e])
                });
                return n(r)
            };
            if (r) {
                e[Symbol.iterator] = e.entries
            }
            e.toString = function () {
                var r = [];
                this.forEach(function (e, t) {
                    r.push(i(t) + "=" + i(e))
                });
                return r.join("&")
            };
            t.URLSearchParams = a
        };
        if (!("URLSearchParams" in t) || new t.URLSearchParams("?a=1").toString() !== "a=1") {
            a()
        }
        var s = t.URLSearchParams.prototype;
        if (typeof s.sort !== "function") {
            s.sort = function () {
                var r = this;
                var n = [];
                this.forEach(function (e, t) {
                    n.push([t, e]);
                    if (!r._entries) {
                        r.delete(t)
                    }
                });
                n.sort(function (e, t) {
                    if (e[0] < t[0]) {
                        return -1
                    } else if (e[0] > t[0]) {
                        return +1
                    } else {
                        return 0
                    }
                });
                if (r._entries) {
                    r._entries = {}
                }
                for (var e = 0; e < n.length; e++) {
                    this.append(n[e][0], n[e][1])
                }
            }
        }
        if (typeof s._fromString !== "function") {
            Object.defineProperty(s, "_fromString", {
                enumerable: false,
                configurable: false,
                writable: false,
                value: function (e) {
                    if (this._entries) {
                        this._entries = {}
                    } else {
                        var r = [];
                        this.forEach(function (e, t) {
                            r.push(t)
                        });
                        for (var t = 0; t < r.length; t++) {
                            this.delete(r[t])
                        }
                    }
                    e = e.replace(/^\?/, "");
                    var n = e.split("&");
                    var i;
                    for (var t = 0; t < n.length; t++) {
                        i = n[t].split("=");
                        this.append(o(i[0]), i.length > 1 ? o(i[1]) : "")
                    }
                }
            })
        }
    })(typeof global !== "undefined" ? global : typeof window !== "undefined" ? window : typeof self !== "undefined" ? self : this);
    (function (h) {
        var e = function () {
            try {
                var e = new h.URL("b", "http://a");
                e.pathname = "c%20d";
                return e.href === "http://a/c%20d" && e.searchParams
            } catch (e) {
                return false
            }
        };
        var t = function () {
            var t = h.URL;
            var e = function (e, t) {
                if (typeof e !== "string") e = String(e);
                var r = document,
                    n;
                if (t && (h.location === void 0 || t !== h.location.href)) {
                    r = document.implementation.createHTMLDocument("");
                    n = r.createElement("base");
                    n.href = t;
                    r.head.appendChild(n);
                    try {
                        if (n.href.indexOf(t) !== 0) throw new Error(n.href)
                    } catch (e) {
                        throw new Error("URL unable to set base " + t + " due to " + e)
                    }
                }
                var i = r.createElement("a");
                i.href = e;
                if (n) {
                    r.body.appendChild(i);
                    i.href = i.href
                }
                if (i.protocol === ":" || !/:/.test(i.href)) {
                    throw new TypeError("Invalid URL")
                }
                Object.defineProperty(this, "_anchorElement", {
                    value: i
                });
                var o = new h.URLSearchParams(this.search);
                var a = true;
                var s = true;
                var c = this;
                ["append", "delete", "set"].forEach(function (e) {
                    var t = o[e];
                    o[e] = function () {
                        t.apply(o, arguments);
                        if (a) {
                            s = false;
                            c.search = o.toString();
                            s = true
                        }
                    }
                });
                Object.defineProperty(this, "searchParams", {
                    value: o,
                    enumerable: true
                });
                var f = void 0;
                Object.defineProperty(this, "_updateSearchParams", {
                    enumerable: false,
                    configurable: false,
                    writable: false,
                    value: function () {
                        if (this.search !== f) {
                            f = this.search;
                            if (s) {
                                a = false;
                                this.searchParams._fromString(this.search);
                                a = true
                            }
                        }
                    }
                })
            };
            var r = e.prototype;
            var n = function (t) {
                Object.defineProperty(r, t, {
                    get: function () {
                        return this._anchorElement[t]
                    },
                    set: function (e) {
                        this._anchorElement[t] = e
                    },
                    enumerable: true
                })
            };
            ["hash", "host", "hostname", "port", "protocol"].forEach(function (e) {
                n(e)
            });
            Object.defineProperty(r, "search", {
                get: function () {
                    return this._anchorElement["search"]
                },
                set: function (e) {
                    this._anchorElement["search"] = e;
                    this._updateSearchParams()
                },
                enumerable: true
            });
            Object.defineProperties(r, {
                toString: {
                    get: function () {
                        var e = this;
                        return function () {
                            return e.href
                        }
                    }
                },
                href: {
                    get: function () {
                        return this._anchorElement.href.replace(/\?$/, "")
                    },
                    set: function (e) {
                        this._anchorElement.href = e;
                        this._updateSearchParams()
                    },
                    enumerable: true
                },
                pathname: {
                    get: function () {
                        return this._anchorElement.pathname.replace(/(^\/?)/, "/")
                    },
                    set: function (e) {
                        this._anchorElement.pathname = e
                    },
                    enumerable: true
                },
                origin: {
                    get: function () {
                        var e = {
                            "http:": 80,
                            "https:": 443,
                            "ftp:": 21
                        } [this._anchorElement.protocol];
                        var t = this._anchorElement.port != e && this._anchorElement.port !== "";
                        return this._anchorElement.protocol + "//" + this._anchorElement.hostname + (t ? ":" + this._anchorElement.port : "")
                    },
                    enumerable: true
                },
                password: {
                    get: function () {
                        return ""
                    },
                    set: function (e) {},
                    enumerable: true
                },
                username: {
                    get: function () {
                        return ""
                    },
                    set: function (e) {},
                    enumerable: true
                }
            });
            e.createObjectURL = function (e) {
                return t.createObjectURL.apply(t, arguments)
            };
            e.revokeObjectURL = function (e) {
                return t.revokeObjectURL.apply(t, arguments)
            };
            h.URL = e
        };
        if (!e()) {
            t()
        }
        if (h.location !== void 0 && !("origin" in h.location)) {
            var r = function () {
                return h.location.protocol + "//" + h.location.hostname + (h.location.port ? ":" + h.location.port : "")
            };
            try {
                Object.defineProperty(h.location, "origin", {
                    get: r,
                    enumerable: true
                })
            } catch (e) {
                setInterval(function () {
                    h.location.origin = r()
                }, 100)
            }
        }
    })(typeof global !== "undefined" ? global : typeof window !== "undefined" ? window : typeof self !== "undefined" ? self : this);
} catch (e) {
    console.error(e);
}

//----------------------------- --------------------- --------------------------------//

var defaultIndex = function (remoteVersion, merchants, useNew) {
    return {
        merchants: merchants || [],
        remoteVersion : remoteVersion || "2.0rc1",
        shouldGetRemotesVerison: true,
        osBased: true,
        newPath: useNew
    }
}

var defaultConfig = function (merchants, useNew) {
    return {
        merchants: merchants || [],
        shouldGetRemotesVerison: false,
        osBased: false,
        newPath: useNew,
    }
}

var defaultConfigJson = function (merchants) {
    return {
        merchants: merchants || [],
        shouldGetRemotesVerison: false,
        osBased: false,
        newPath: useNew,
        extension : ".json"
    }
}

var merchantConfig = function() {
    return {
        "in.juspay.dotp": {
            "index_bundle": defaultIndex(checkout_remotes_version, ["bms", "onecard", "TUL", "tul", "zee5"]),
            "config": defaultConfig(["onecard", "zee5"])
        },
        "in.juspay.ec": {
            "index_bundle": defaultIndex(ec_remotes_version, ["zoomin", "zee5", "grofers", "curefit", "dineout"]),
            "config": defaultConfig(["zee5"])
        },
        "in.juspay.upiintent": {
            "index_bundle": defaultIndex(ec_remotes_version, ["zee5"]),
            "config": defaultConfig(["zee5"])
        },
        "in.juspay.vies": {
            "index_bundle": defaultIndex(vies_remotes_version, ["zee5"]),
            "config": defaultConfig(["zee5"])
        },
        "in.juspay.hyperpay" : {
            "configuration":defaultConfig(["configurator"], true)
        }
    }
}

var getPath = function (fileConfig, service, useBeta, fileName) {
    var beta = useBeta ? "beta" : "release";
    if (fileConfig.newPath) {
        var remoteVersion = fileConfig.shouldGetRemotesVerison ? fileConfig.remoteVersion : "config"
        var merchantName = fileConfig.merchants.indexOf(merchantId) != -1 ? merchantId.toLowerCase() : "common"
        var osName = fileConfig.osBased ? newOS : "common"
        var fN = fileConfig.extension ? fileName + fileConfig.extension : "v1-" + fileName + ".zip"
        // Sample path :: /hyper/bundles/android/release/in.juspay.hyperpay/config/idea/stagger/config.json
        return bucket_path + "/hyper/bundles/" + osName + "/" + beta + "/" + service + "/" + remoteVersion + "/" + merchantName + "/stable/" + fN
    } else {
        var remoteVersion = fileConfig.shouldGetRemotesVerison ? "/" + fileConfig.remoteVersion : ""
        var merchantName = fileConfig.merchants.indexOf(merchantId) != -1 ? "." + merchantId.toLowerCase() : ""
        var osName = fileConfig.osBased ? OS : ""
        var fN = fileConfig.extension ? fileName + fileConfig.extension : "v1-" + fileName + ".zip"
        // Sample path :: juspay/payments/in.juspay.hyperpay.merchantId/release/2.0rc1/index_bundle.js
        return bucket_path + "/juspay/payments/" + service + merchantName + "/" + beta + osName + remoteVersion + "/" + fN
    }
}

var makeAssets = function (service, useBeta) {
    var assets = {}
    for (var file in merchantConfig()[service]) {
        assets[file] = getPath(merchantConfig()[service][file], service, useBeta, file);
    }
    return assets;
}

//----------------------------- --------------------- -------------------------------//

var okcGodelBuildVersion1 = [77,70];
var okcGodelBuildVersion2 = [17,33,41,51, 76, 79, 86];
var okcGodelBuildVersion3 = [02];
var swiggyGodelBuildVersion2 = [61, 74, 78];


const indexBundleForOkc = function () {
    try {
        var godel_version = JBridge.getResourceByName("godel_version");
        var godel_build_version = JBridge.getResourceByName(
            "godel_build_version"
        ).replace("rc.", "");
        var gv = parseInt(godel_version.split(".")[2]);
        if (gv == 1) {
            if (okcGodelBuildVersion1.includes(parseInt(godel_build_version))) {
                return "/juspay/payments/in.juspay.hyperpay.okcredit";
            }
            return "/juspay/payments/in.juspay.hyperpay";
        } else if (gv == 2) {
            if (okcGodelBuildVersion2.includes(parseInt(godel_build_version))) {
                return "/juspay/payments/in.juspay.hyperpay.okcredit";
            }
            return "/juspay/payments/in.juspay.hyperpay";
        } else if (gv == 3) {
            if (okcGodelBuildVersion3.includes(parseInt(godel_build_version))) {
                return "/juspay/payments/in.juspay.hyperpay.okcredit";
            }
            return "/juspay/payments/in.juspay.hyperpay";
        }
    } catch (e) {
        console.error("Couldn't fetch sdk version " + e);
    }
    return "/juspay/payments/in.juspay.hyperpay";
};

const getECAssetsNewVer = function (useBeta) {
    try {
        var godel_version = JBridge.getResourceByName("godel_version");
        var godel_build_version = JBridge.getResourceByName(
            "godel_build_version"
        ).replace("rc.", "");
        var godel_build_version_alpha = JBridge.getResourceByName(
            "godel_build_version"
        ).replace("alpha.", "");
        var gv = parseInt(godel_version.split(".")[2]);
        if (gv == 1 && (merchantId == "okcredit")) {
            if (okcGodelBuildVersion1.includes(parseInt(godel_build_version))) {
                return getECAssetsHyperUPI(useBeta);
            }
        } else if (gv == 2 && (merchantId == "okcredit")) {
            if (okcGodelBuildVersion2.includes(parseInt(godel_build_version))) {
                return getECAssetsHyperUPI(useBeta);
            }
        } else if (gv == 3 && (merchantId == "okcredit")) {
            if (okcGodelBuildVersion3.includes(parseInt(godel_build_version))) {
                return getECAssetsHyperUPI(useBeta);
            }
        } else if (gv == 1 && (merchantId.includes("swiggy"))) {
            if (swiggyGodelBuildVersion2.includes(parseInt(godel_build_version_alpha))) {
                return getECAssetsHyperUPI(useBeta);
            }
        } else if (window.__OS && window.__OS == "IOS" && (merchantId.includes("swiggy"))) {
            var sdkVersion = JBridge.getResourceByName("CFBundleVersion");
            sdkVersion = sdkVersion.split(".");
            if (parseInt(sdkVersion[0]) >= 2 && (parseInt(sdkVersion[1]) >= 1 || (parseInt(sdkVersion[1]) == 0 && parseInt(sdkVersion[2]) >= 36))) { // iOS SDK version >= 2.0.36
                return getECAssetsHyperUPI(useBeta);
            }
        }
    } catch (e) {
        console.error("Couldn't fetch sdk version " + e);
    }
    return getECAssets(useBeta);
};

const getECCanOpen = function () {
    try {
        var godel_version = JBridge.getResourceByName("godel_version");
        var godel_build_version_alpha = JBridge.getResourceByName(
            "godel_build_version"
        ).replace("alpha.", "");
        var gv = parseInt(godel_version.split(".")[2]);
        if (gv == 1 && (merchantId.includes("swiggy"))) {
            if (swiggyGodelBuildVersion2.includes(parseInt(godel_build_version_alpha))) {
                return ["in.juspay.vies", "in.juspay.dotp", "in.juspay.upiintent", "in.juspay.godel.placeholder", "in.juspay.hyperos.placeholder", "in.juspay.escrow", "in.juspay.hyperupi"]
            }
        } else if (window.__OS && window.__OS == "IOS" && (merchantId.includes("swiggy"))) {
            var sdkVersion = JBridge.getResourceByName("CFBundleVersion");
            sdkVersion = sdkVersion.split(".");
            if (parseInt(sdkVersion[0]) >= 2 && (parseInt(sdkVersion[1]) >= 1 || (parseInt(sdkVersion[1]) == 0 && parseInt(sdkVersion[2]) >= 36))) { // iOS SDK version >= 2.0.36
                return ["in.juspay.vies", "in.juspay.dotp", "in.juspay.upiintent", "in.juspay.godel.placeholder", "in.juspay.hyperos.placeholder", "in.juspay.escrow", "in.juspay.hyperupi"]
            }
        }
    } catch (e) {
        console.error("Couldn't fetch sdk version " + e);
    }
    return ["in.juspay.vies", "in.juspay.dotp", "in.juspay.upiintent", "in.juspay.godel.placeholder", "in.juspay.hyperos.placeholder", "in.juspay.escrow", "in.juspay.flyer"]
};

const getHyperUPIAssets = function (useBeta) {
    try {
        if (merchantId == "dreamplug") {
            return getHyperUPIAssetsDreamplug(useBeta)
        }

        var godel_version = JBridge.getResourceByName("godel_version");
        var godel_build_version = JBridge.getResourceByName(
            "godel_build_version"
        ).replace("rc.", "");
        var godel_build_version_alpha = JBridge.getResourceByName(
            "godel_build_version"
        ).replace("alpha.", "");
        var gv = parseInt(godel_version.split(".")[2]);
        if (gv == 1 && (merchantId == "okcredit")) {
            if (okcGodelBuildVersion1.includes(parseInt(godel_build_version))) {
                return getHyperUPIAssetsPath(useBeta);
            }
        } else if (gv == 2 && (merchantId == "okcredit")) {
            if (okcGodelBuildVersion2.includes(parseInt(godel_build_version))) {
                return getHyperUPIAssetsPath(useBeta);
            }
        }  else if (gv == 3 && (merchantId == "okcredit")) {
            if (okcGodelBuildVersion3.includes(parseInt(godel_build_version))) {
                return getHyperUPIAssetsPath(useBeta);
            }
        } else if (gv == 1 && (merchantId.includes("swiggy"))) {
            if (swiggyGodelBuildVersion2.includes(parseInt(godel_build_version_alpha))) {
                return getHyperUPIAssetsPath(useBeta);
            }
        } else if (window.__OS && window.__OS == "IOS" && (merchantId.includes("swiggy"))) {
            var sdkVersion = JBridge.getResourceByName("CFBundleVersion");
            sdkVersion = sdkVersion.split(".");
            if (parseInt(sdkVersion[0]) >= 2 && (parseInt(sdkVersion[1]) >= 1 || (parseInt(sdkVersion[1]) == 0 && parseInt(sdkVersion[2]) >= 36))) { // iOS SDK version >= 2.0.36
                return getHyperUPIAssetsPath(useBeta);
            }
        }
    } catch (e) {
        console.error("Couldn't fetch sdk version " + e);
    }
    return getHyperUPIAssetsGeneric(useBeta);
};


var getHyperUPIAssetsPath = function (useBeta) {
    var assets = {
        "config": bucket_path + "/juspay/payments/in.juspay.hyperupi/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
        "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperupi." + merchantId + "/" + (useBeta ? "beta" : "release") + OS  + "/" + "2.0rc1" + "/v1-index_bundle.zip"
    }
    if (isWrapper) {
        assets["polyfill"] = bucket_path + "/juspay/payments/in.juspay.hyperupi/" + (useBeta ? "beta" : "release") + "/v1-polyfill.zip"
    }
    return assets;
}


var getHyperUPIAssetsGeneric = function (useBeta) {
    var assets = {
        // "config": bucket_path + "/juspay/payments/in.juspay.hyperupi/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
        "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperupi" + "/" + (useBeta ? "beta" : "release") + "/" + "2.0rc1" + "/v1-index_bundle.zip"
    }
    if (isWrapper) {
        assets["polyfill"] = bucket_path + "/juspay/payments/in.juspay.hyperupi/" + (useBeta ? "beta" : "release") + "/v1-polyfill.zip"
    }
    return assets;
}

var getHyperUPIAssetsDreamplug = function (useBeta) {
    var assets = {
        "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperupi/dreamplug/" + (useBeta ? "beta" : "release") + "/" + "2.0rc1" + "/v1-index_bundle.zip"
    }
    return assets;
}


function getOS() {
    if (window.__OS && typeof window.__OS == "string") {
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
    if (getOS() == 'ANDROID') {
        eval(variables);
    } else {
        window.eval(variables);
    }

    try {
        var bundle_payload = JSON.parse(JBridge.getSessionAttribute("bundleParams", "{}"));
        if (bundle_payload && bundle_payload.payload) {
            var backupClientId = bundle_payload.payload.clientId || bundle_payload.payload.client_id;
            if ((!clientId || clientId == "null" || clientId == "") && backupClientId && backupClientId != null && backupClientId != "") {
                clientId = backupClientId;
            }
        }
    } catch (e) {
        console.error("CLIENT_ID POLYFILL TRIGGERED EXCEPTION :: ", e);
    }

    var merchantId = (clientId && clientId != "null") ? clientId.split("_")[0] : "";
} catch (err) {
    console.error(err)
    var merchantId = "";
}

var godel_remotes_version = JBridge.getResourceByName("godel_remotes_version");

var isWrapper = false;
try {
    var wrapper_version = JBridge.getResourceByName("wrapper_version");
    var wrapper_build_version = JBridge.getResourceByName("wrapper_build_version");
    isWrapper = wrapper_version !== "" && wrapper_build_version !== "";
} catch (err) {
    console.log("This is not a wrapper build");
}

var beckn_version = "";
try {
    beckn_version = JBridge.getResourceByName("beckn_version");
} catch (err) {
    console.log("beckn remote version is not available");
}

var vies_remotes_version = "";
try {
    vies_remotes_version = JBridge.getResourceByName("vies_remotes_version");
} catch (err) {
    console.error("vies remote version is not available");
}

var bucket_path = "https://assets.juspay.in";
var useNew = false;
try {
    var dev = JBridge.getResourceByName("development");
    if (dev) {
        bucket_path = "https://dpj0m1myaqz8f.cloudfront.net";
        useNew = true
    }
} catch (err) {
    console.log("This is not a development build");
}


var checkout_remotes_version = "";
try {
    checkout_remotes_version = JBridge.getResourceByName("checkout_remotes_version");
} catch (err) {
    checkout_remotes_version = godel_remotes_version;
}

var upi_remotes_version = "";
try {
    upi_remotes_version = JBridge.getResourceByName("upi_remotes_version");
} catch (err) {
    upi_remotes_version = godel_remotes_version;
}

var ec_remotes_version = "";
try {
    ec_remotes_version = JBridge.getResourceByName("ec_remotes_version");
} catch (err) {
    ec_remotes_version = godel_remotes_version;
}

// PolyFill
var godel_version = JBridge.getResourceByName("godel_version");
if (getOS() == "ANDROID" && (godel_version == "1.0.4" || godel_version == "1.0.4.1" || godel_version == "1.0.4.1_2")) {
    godel_remotes_version = "1.0rc2"
}

var OS = "";
if (getOS() == "IOS") {
    OS = "/IOS";
}

var newOS = "common"
switch(getOS()) {
    case "ANDROID" :
        newOS = "android"
        break;
    case "IOS":
        newOS = "IOS"
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


var getHyperPayConfig = function (useBeta) {
    if(merchantId == "olacabs" && (sdkRootVerison == "2.0.0" || sdkRootVerison == "2.0.1")) {
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
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay/" + (useBeta || juspayBetaUsers.includes(juspayDeviceId) ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
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
    } else if (merchantId == "udaan") {
        return {
            "src": "",
            "version": "2.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? ".udaan" : "") + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.udaan" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.udaan" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "curefit") {
        return {
            "src": "",
            "version": "",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.curefit" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec", "in.juspay.hyperapi"]
        }
    } else if (merchantId == "idea") {
        return {
            "src": "",
            "version": "",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.idea" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.idea" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "jiosaavn") {
         return {
               "src": "",
               "version": "",
               "iconUrl": "",
               "assets": {
                     "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                     "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.jiosaavn" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                     "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.jiosaavn" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                     "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.jiosaavn" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
                },
               "root": "payments/in.juspay.hyperpay/",
               "entry": "base.html",
               "canOpen": ["in.juspay.ec"]
           }
    } else if (merchantId == "urbanclap") {
        return {
            "src": "",
            "version": "2.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? ".urbanclap" : "") + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.urbanclap" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.urbanclap" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "zee5") {
        return {
            "src": "",
            "version": "2.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? ".zee5/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.zee5" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.zee5" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.zee5" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "okcredit") {
        return {
            "src": "",
            "version": "2.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + indexBundleForOkc() + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.okcredit" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.okcredit" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec", "in.juspay.hyperupi"]
        }
    } else if (merchantId == "vodaidea" || merchantId == "vodafone") {
        return {
            "src": "",
            "version": "",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodaidea" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.vodaidea" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip"
            },
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
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.trulymadly" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.trulymadly" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.trulymadly" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "acko") {
        return {
            "src": "",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.acko" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.acko" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "onecard") {
        return {
            "src": "",
            "version": "",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.onecard" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.onecard" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-icons.zip"
            },
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
                    "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                    "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.croma" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip"
                },
              "root": "payments/in.juspay.hyperpay/",
              "entry": "base.html",
              "canOpen": ["in.juspay.ec"]
          }
    } else if (merchantId == "truefan") {
        return {
              "src": "",
              "version" : "",
              "iconUrl" : "",
              "assets" : {
                    "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                    "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.truefan" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip",
                    "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.truefan" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
                },
              "root": "payments/in.juspay.hyperpay/",
              "entry": "base.html",
              "canOpen": ["in.juspay.ec"]
          }
    } else if (merchantId == "TOI") {
        return {
              "src": "",
              "version" : "",
              "iconUrl" : "",
              "assets" : {
                    "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                    "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.toi" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-ui_config.zip"
                },
              "root": "payments/in.juspay.hyperpay/",
              "entry": "base.html",
              "canOpen": ["in.juspay.ec"]
          }
    } else if (merchantId == "qmin") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (useBeta ? ".qmin/beta" : "/release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.qmin" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.qmin" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    }
    else if (merchantId == "spicexpress") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.spicexpress" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.spicexpress" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    }
    else if (merchantId.toLowerCase() == "timesprime") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.timesprime" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
                "icons": bucket_path + "/juspay/payments/in.juspay.hyperpay.timesprime" + "/" + (useBeta ? "beta" : "release") + "/v1-icons.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.timesprime" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    }
    else if (merchantId.toLowerCase() == "acko") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.acko" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    }
    else if (merchantId == "astroyogi") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.astroyogi" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.astroyogi" + "/" + (useBeta ? "beta" : "release") + OS + "/v1-strings.zip"

            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "tatasky") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.tatasky" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.tatasky" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "dineout") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.dineout" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "ui_config": bucket_path + "/juspay/payments/in.juspay.hyperpay.dineout" + "/" + (useBeta ? "beta" : "release") + "/v1-ui_config.zip",
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "configurator") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : makeAssets("in.juspay.hyperpay", useBeta),
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "kiranakart") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.kiranakart" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else if (merchantId == "meesho") {
        return {
            "src":"",
            "version" : "",
            "iconUrl" : "",
            "assets" : {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay.meesho" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
                "strings": bucket_path + "/juspay/payments/in.juspay.hyperpay.meesho" + "/" + (useBeta ? "beta" : "release") + "/v1-strings.zip",
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    } else {
        return {
            "src": "",
            "version": "",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "base.html",
            "canOpen": ["in.juspay.ec"]
        }
    }
}

var getHyperManageConfig = function (useBeta) {
    return {
        "src": "",
        "version": "2.0rc1",
        "iconUrl": "",
        "assets": {},
        "root": "payments/in.juspay.hyperpay/",
        "entry": "base.html",
        "canOpen": []
    }
}

var getUPIAssets = function (useBeta) {
    if (clientId == "goibibo_android") {
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
        if (service == "in.juspay.nammayatri" || service == "in.juspay.nammayatripartner")
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
        if (service == "in.juspay.nammayatri" || service == "in.juspay.nammayatripartner")
        {
            var merchantName = getMerchantForSdkConfig(merchantId);
            assets["sdk_config"] = bucket_path + "/juspay/beckn/" + service + (useBeta ? "/beta/" : "/release/") + merchantName + "/sdk_config.json";
        }
        return assets;
    }
}

var getHyperOSPlaceHolderAssets = function (useBeta) {
    var assets = {
        "tracker": bucket_path + "/juspay/payments/2.0/" + (useBeta ? "beta" : "release") + "/v1-tracker.zip"
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

var getUPIIntentAssets = function (useBeta) {
    var merchantPath = (
        function(){
            switch(merchantId){
                case "curefit":
                    return ".curefit/";
                default: return "/";
            }
        }
    )();

    return {
        "config": bucket_path + "/juspay/payments/in.juspay.upiintent/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
        "index_bundle": bucket_path + "/juspay/payments/in.juspay.upiintent" + merchantPath  + (useBeta ? "beta" : "release") + OS + "/" + ec_remotes_version + "/v1-index_bundle.zip"
    }
}

var getInAppUPIAssets = function (useBeta) {
    return {
        "index_bundle":  bucket_path + "/juspay/payments/in.juspay.inappupi/" + (useBeta ? "beta" : "release") + OS + "/" + "2.0rc1" + "/v1-index_bundle.zip"
    }
}

var getMerchantIdEC = function() {
    if (merchantId == "TUL") {
        return ".tul";
    }
    if (merchantId == "ajio") {
        return ".ajio";
    }
    if (merchantId == "grofers") {
        return ".grofers";
    }
    if (merchantId == "curefit"){
        return ".curefit";
    }
    if (merchantId == "dineout"){
        return ".dineout";
    }
    return "";
}

var getECAssets = function (useBeta) {
    var assets = {
        "config": bucket_path + "/juspay/payments/in.juspay.ec/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
        "index_bundle": bucket_path + "/juspay/payments/in.juspay.ec" + getMerchantIdEC() + "/" + (useBeta ? "beta" : "release") + OS + "/" + ec_remotes_version + "/v1-index_bundle.zip"
    }
    if (isWrapper) {
        assets["polyfill"] = bucket_path + "/juspay/payments/in.juspay.ec/" + (useBeta ? "beta" : "release") + "/v1-polyfill.zip"
    }
    return assets;
}

var getECAssetsHyperUPI = function (useBeta) {
    var assets = {
        "config": bucket_path + "/juspay/payments/in.juspay.ec/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
        "index_bundle": bucket_path + "/juspay/payments/in.juspay.ec." + merchantId + getMerchantIdEC() + "/" + (useBeta ? "beta" : "release") + OS + "/" + ec_remotes_version + "/v1-index_bundle.zip"
    }
    if (isWrapper) {
        assets["polyfill"] = bucket_path + "/juspay/payments/in.juspay.ec/" + (useBeta ? "beta" : "release") + "/v1-polyfill.zip"
    }
    return assets;
}

var getMerchantIdGodel = function () {
    if (merchantId == "ajio")
        return ".ajio"
    if(merchantId == "onecard")
        return ".onecard"
    if(merchantId == "gameskraft")
        return ".gameskraft"
    return "";
}

var getGodelAssets = function (useBeta) {
    var addMerchantId = getMerchantIdGodel();
    if (getOS() == "ANDROID") {
        return {
            "config": "https://d3e0hckk6jr53z.cloudfront.net/godel/v1-config.zip",
            "acs_js_source": bucket_path + "/juspay/payments/in.juspay.godel/" + (useBeta ? "beta" : "release") + "/1.0rc2/v1-acs.zip",
            "boot_loader_js_source": bucket_path + "/juspay/payments/in.juspay.godel/" + (useBeta ? "beta" : "release") + "/" + godel_remotes_version + "/v1-boot_loader.zip",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.godel" + addMerchantId + "/" + ((useBeta || juspayBetaUsers.includes(juspayDeviceId)) ? "beta" : "release") + "/" + godel_remotes_version + "/v1-index_bundle.zip",
            "certificates": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/certificates_v1.zip"
        }
    } else {
        return {
            "config": "https://d3e0hckk6jr53z.cloudfront.net/godel/v1-config.jsa",
            "acs_js_source": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/v1-acs.jsa"
        }
    }
}

var getMerchantIdVies = function () {
    return "";
}

var getViesAssets = function (useBeta) {
    var addMerchantId = getMerchantIdVies();
    if (getOS() == "ANDROID" || getOS() == "IOS") {
        return {
            "config": bucket_path + "/juspay/payments/in.juspay.vies/" + (useBeta || juspayBetaUsers.includes(juspayDeviceId) ? "beta" : "release") + "/v1-config.zip",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.vies" + addMerchantId + "/" + (useBeta || juspayBetaUsers.includes(juspayDeviceId) ? "beta" : "release") + OS + "/" + vies_remotes_version + "/v1-index_bundle.zip"
        }
    } else {
        return {}
    }
}

var getGpayAssets = function (useBeta) {
    if (getOS() == "ANDROID") {
        return {
            "config": bucket_path + "/juspay/payments/in.juspay.gpay/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.gpay/" + (useBeta ? "beta" : "release") + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
    } else {
        return {}
    }
}

var getDOTPAssets = function (useBeta) {
    if (merchantId == "bms") {
        return {
            "config": bucket_path + "/juspay/payments/in.juspay.dotp/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.dotp.bms/" + (useBeta ? "beta" : "release") + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
    } else if (merchantId == "onecard") {
        return {
            "config": bucket_path + "/juspay/payments/in.juspay.dotp.onecard/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.dotp.onecard/" + (useBeta ? "beta" : "release") + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
    } else if (merchantId == "TUL" || merchantId == "tul") {
        return {
            "config": bucket_path + "/juspay/payments/in.juspay.dotp/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.dotp.tul/" + (useBeta ? "beta" : "release") + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
    } else {
        return {
            "config": bucket_path + "/juspay/payments/in.juspay.dotp/" + (useBeta ? "beta" : "release") + "/v1-config.zip",
            "index_bundle": bucket_path + "/juspay/payments/in.juspay.dotp/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip"
        }
    }
}

var getAppList = function (useBeta, service) {
    return {
        "in.juspay.hyperos": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getHyperOSAssets(useBeta, service),
            "root": "/",
            "entry": "",
            "canOpen": []
        },
        "in.juspay.hyperos.placeholder": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getHyperOSPlaceHolderAssets(useBeta),
            "root": "",
            "entry": "",
            "canOpen": []
        },
        "in.juspay.hyperpay.placeholder": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/payments/in.juspay.hyperpay" + (merchantId == "olacabs" ? ".olacabs" : "") + "/" + (useBeta ? "beta" : "release") + OS + "/" + checkout_remotes_version + "/v1-index_bundle.zip",
            },
            "root": "payments/in.juspay.hyperpay/",
            "entry": "",
            "canOpen": []
        },
        "in.juspay.godel": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getGodelAssets(useBeta),
            "root": "payments/in.juspay.godel/",
            "entry": "base.html",
            "canOpen": []
        },
        "in.juspay.godel.placeholder": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getGodelAssets(useBeta),
            "root": "payments/in.juspay.godel/",
            "entry": "",
            "canOpen": []
        },
        "in.juspay.gatekeeper": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {},
            "root": "payments/in.juspay.gatekeeper/",
            "entry": "base.html",
            "canOpen": []
        },
        "in.juspay.gpay": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getGpayAssets(useBeta),
            "root": "payments/in.juspay.gpay/",
            "entry": "base.html",
            "canOpen": []
        },
        "in.hyper.pay": getHyperPayConfig(useBeta),
        "in.juspay.hyperpay": getHyperPayConfig(useBeta),
        "in.juspay.hypermanage": getHyperManageConfig(useBeta),
        "in.juspay.upimanage": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {},
            "root": "payments/in.hyper.pay/",
            "entry": "base.html",
            "canOpen": []
        },
        "in.juspay.upiintent": {
            "src": "",
            "version": "0.2rc1",
            "iconUrl": "",
            "assets": getUPIIntentAssets(useBeta),
            "root": "payments/in.juspay.upiintent/",
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
        "in.juspay.inappupi": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getInAppUPIAssets(useBeta),
            "root": "payments/in.juspay.inappupi/",
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
        "in.juspay.ec": {
            "src": "",
            "version": "1.0rc4",
            "iconUrl": "",
            "assets": getECAssetsNewVer(useBeta), //getECAssets(useBeta),
            "root": "payments/in.juspay.ec/",
            "entry": "base.html",
            "canOpen": getECCanOpen()
        },
        "in.juspay.upi": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getUPIAssets(useBeta),
            "root": "payments/in.juspay.upi/",
            "entry": "base.html",
            "canOpen": []
        },
        "in.juspay.vies": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getViesAssets(useBeta),
            "root": "payments/in.juspay.vies/",
            "entry": "base.html",
            "canOpen": []
        },
        "in.juspay.dotp": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": getDOTPAssets(useBeta),
            "root": "payments/in.juspay.dotp/",
            "entry": "base.html",
            "canOpen": []
        },
        "net.openkochi.yatri": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/beckn/net.openkochi.yatri/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
            },
            "root": "",
            "entry": "becknbase.html",
            "canOpen": []
        },
        "in.juspay.becknuser": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/beckn/in.juspay.becknuser/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
            },
            "root": "",
            "entry": "becknbase.html",
            "canOpen": []
        },
        "in.juspay.becknui": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/beckn/in.juspay.becknui/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
            },
            "root": "",
            "entry": "becknbase.html",
            "canOpen": []
        },
        "net.openkochi.yatripartner": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/beckn/net.openkochi.yatripartner/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
            },
            "root": "",
            "entry": "becknbase.html",
            "canOpen": []
        },
        "in.juspay.nammayatripartner": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/beckn/in.juspay.nammayatripartner/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
            },
            "root": "",
            "entry": "becknbase.html",
            "canOpen": []
        },
        "in.juspay.nammayatri": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/beckn/in.juspay.nammayatri/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
            },
            "root": "",
            "entry": "becknbase.html",
            "canOpen": []
        },

        "in.juspay.mobility.transporter": {
            "src": "",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle": bucket_path + "/juspay/beckn/in.juspay.mobility.transporter/" + (useBeta ? "beta/" : "release/") + beckn_version + "/v1-index_bundle.zip"
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
               "index_bundle": bucket_path + "/hyper/bundles/" + (getOS() == "IOS" ? "IOS" : "android") + "/" + (useBeta ? "beta" : "release") + "/in.juspay.escrow" + "/2.0rc1/common/stable/v1-index_bundle.zip"
           },
           "root": "payments/in.juspay.escrow/",
           "entry": "base.html",
           "canOpen": []
        },
        "in.juspay.flyer": {
            "src": bucket_path + "/hyper/bundles/" + (useBeta ? "beta" : "release") + "/" + newOS + "/in.juspay.flyer/common/common/stable/base.html",
            "version": "1.0rc1",
            "iconUrl": "",
            "assets": {
                "index_bundle" : bucket_path + "/juspay/payments/in.juspay.flyer/" + (useBeta ? "beta" : "release") + OS + "/2.0rc1/v1-index_bundle.zip"
            },
            "root": "payments/in.juspay.flyer/",
            "entry": "base.html",
            "canOpen": []
        }
    };
}

var getRollOut = function () {
    if (merchantId == "idea") {
        return {
            "v1-index_bundle": 0
        }
    }
    return {
        "v1-index_bundle": 0
    }
}

var getStaggeredReleaseConfig = function () {
    return {
        "in.juspay.hyperpay": {
            "beta": {
                "2.0rc1": getRollOut()
            }
        }
    }
}

var updateBlockedHashes = function () {
    // Please maintain only last 5 hashes of each mapp to so that the size will not get increased
    var blockHash = {
        "in.juspay.vies": [],
        "in.juspay.ec": []
    }
    JBridge.addDataToSharedPrefs("jp_external_blocked_hashes", JSON.stringify(blockHash));
    return;
}

try {
    updateBlockedHashes();
} catch (e) {
    console.error("error while calling updateBlockedHashes", e)
}

window.getConfig = function (payload) {
    var useBeta = false;
    var service = false;
    if (typeof payload == "object" && typeof payload.hasOwnProperty == "function" && payload.hasOwnProperty("betaAssets")) {
        useBeta = payload.betaAssets
    }
    if (payload.hasOwnProperty("service")){
        service = payload.service;
    }
    try {
        if (merchantId == "jiosaavn") {
            sdkVersion = JBridge.getResourceByName("godel_build_version");
            sdkRootVerison = JBridge.getResourceByName("godel_version");
            jiosaavnVersions = ["rc.6", "rc.14", "rc.16", "rc.17", "rc.25", "rc.26", "rc.27", "rc.49", "rc.74"]
            jiosaavnVersions1 = ["rc.23"]
            if (jiosaavnVersions.indexOf(sdkVersion) != -1 && sdkRootVerison == "2.0.0") {
                useBeta = false;
            } else if (jiosaavnVersions1.indexOf(sdkVersion) != -1 && sdkRootVerison == "2.0.1") {
                useBeta = false;
            }
        }
        if ( getOS() == 'ANDROID' && typeof JBridge.findApps == "function" && JBridge.findApps("juspay://pay/"+merchantId).indexOf("in.juspay.devtools") != -1 ) {
            useBeta = true
        }
    } catch (e){
        useBeta = false;
        //  Ignored
    }

    return {
        apps: getAppList(useBeta, service),
        controlPanel: getStaggeredReleaseConfig()
    };
}

try {
    var godel_build_version = "rc.01"
    var godel_version = "2.0.0"

    try {
        godel_build_version = JBridge.getResourceByName("godel_build_version");
        godel_version = JBridge.getResourceByName("godel_version");
    } catch (e) {
        Android.runInUI("set_PM=ctx->getPackageManager;set_cn=android.content.ComponentName->new:ctx_ctx,s_in.juspay.hypersdk.core.CustomtabActivity;get_PM->setComponentEnabledSetting:get_cn,i_2,i_1;",null)
    }

    if (godel_version == "2.0.0" || godel_version == "2.0.1" || (godel_version == "2.0.2" && parseInt(godel_build_version.split(".")[1]) < 15)) {
        Android.runInUI("set_PM=ctx->getPackageManager;set_cn=android.content.ComponentName->new:ctx_ctx,s_in.juspay.hypersdk.core.CustomtabActivity;get_PM->setComponentEnabledSetting:get_cn,i_2,i_1;",null)
    } else {
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
    var sdkVersion = JBridge.getResourceByName("godel_build_version");
    var sdkRootVerison = JBridge.getResourceByName("godel_version");

    if (sdkRootVerison === "2.0.2" && (sdkVersion === "rc.44" || sdkVersion === "rc.40")) {
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
