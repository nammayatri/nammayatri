var version = "1.0.832";
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

try {
    window.version = version;
} catch(err) {

}

var ALL = "true";
var conditionForLogs = [];

var addToFirst = function(first, second) {
    for(var key in second) {
        first[key] = second[key];
    }
    return first;
}

var match = function(params, returnOnFirstMatch) {
    var config = {};
    console.log("params: " + JSON.stringify(params));
    for(var i in params) {
        var entry = params[i];
        var condition = entry.c;
        var result = entry.r;
        console.log("Condition: " + condition+", Result: "+JSON.stringify(result));
        try {
            if(eval(condition)) {
                console.log("condition is true for : " + JSON.stringify(entry));
                conditionForLogs.push(entry);
                config = addToFirst(config, result);
                if(returnOnFirstMatch) {
                    return config;
                }
            }
        } catch(e) {
            console.error("Error when evaluating condition: " + condition + ". Exception: " + e);
        }
    }
    console.log("Returning config ")
    return config;
}

var matchAll = function(params) {
    return match(params, false);
}

var matchFirst = function(params) {
    return match(params, true);
}

//compare 2 sem-version strings
var compareVersions = function(v1, v2) {
    var v1Parts = v1.split(".").map(function(n){ return parseInt(n) });
    var v2Parts = v2.split(".").map(function(n){ return parseInt(n) });

    var compareAt = function(i) {
        var a = v1Parts[i] == undefined ? 0 : v1Parts[i];
        var b = v2Parts[i] == undefined ? 0 : v2Parts[i];

        if(a > b) { return 1 }
        else if (a < b) { return -1 }
        else if (v1Parts.length > i+1 || v2Parts.length > i+1) { return compareAt(i + 1) }
        else { return 0 }
    }

    return compareAt(0);
}

//This app's package name. Can be "" if an exception happens
var PACKAGE_NAME = (function() {
    try {
        var packageName = JSON.parse(JBridge.getSessionInfo()).package_name;
        return packageName || "";
    } catch(e) {
        return "";
    }
})();

// Check if a given version is between lower and upper (inclusive)
var versionBetween = function(version, lower, upper) {
    try {
        var isNewerThanLower = lower ? compareVersions(version, lower) >= 0 : true;
        var isOlderThanUpper = upper ? compareVersions(version, upper) <= 0 : true;
        return isNewerThanLower && isOlderThanUpper;
    } catch(e) {
        return false;
    }
}

// Check if app version is between 2 version strings (inclusive)
// Pass `null` as `lower` for no lower limit (similarly for upper).
// Also returns false if any exception happens
var appVersionBetween = function(lower, upper) {
    try {
        var appVersion = JSON.parse(JBridge.getSessionInfo()).app_version;
        return versionBetween(appVersion, lower, upper);
    } catch(e) {
        return false;
    }
}

// Similar to appVersionBetween but for checking godel-core version instead
// Join godel version and build version with a '.' and pass (eg: 1.0.7-22 = 1.0.7.22)
var godelVersionBetween = function(lower, upper) {
    try {
        var godelVersion = JBridge.getResourceByName("godel_version");
        var buildVersion = JBridge.getResourceByName("godel_build_version");
        return versionBetween(godelVersion + "." + buildVersion, lower, upper);
    } catch(e) {
        return false;
    }
}

var statusRules = [
    {
        "matches": {
           "sender": ["SBICRD","ATMSBI", "NPCIJP"],
           "message": "made on"
        },
        "status": "Success",
        "bank": "SBI",
        "otp_timeout": 60
    },
    {
        "matches": {
        "sender": ["SBINB", "NPCIJP"],
        "message": "Thank you for using"
        },
        "status": "Success",
        "bank": "SBI",
        "otp_timeout": 60
    },
    {
        "matches": {
          "sender": ["SBICRD","ATMSBI", "NPCIJP"],
          "message": "debited"
        },
        "status": "Success",
        "bank": "SBI",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBICRD","ATMSBI", "NPCIJP"],
            "message": "spent"
        },
        "status": "Success",
        "bank": "SBI",
        "otp_timeout": 60
    },
    {
        "matches": {
           "sender": ["SBICRD","ATMSBI", "NPCIJP"],
           "message": "has been used"
        },
        "status": "Success",
        "bank": "SBI",
        "otp_timeout": 60
    },
    {
        "matches": {
           "sender": ["SBICRD","ATMSBI", "NPCIJP"],
           "message": "declined"
        },
        "status": "Failure",
        "bank": "SBI",
        "otp_timeout": 60
    },
    {
        "matches": {
           "sender": ["SBICRD","ATMSBI", "NPCIJP"],
           "message": "could not be processed"
        },
        "status": "Failure",
        "bank": "SBI",
        "otp_timeout": 60
    },
    {
        "matches": {
           "sender": ["SBICRD","ATMSBI","NPCIJP"],
           "message": "blocked"
        },
        "status": "Failure",
        "bank": "SBI",
        "otp_timeout": 60
    },
    {
        "matches": {
           "sender": ["HDFCBK","NPCIJP"],
           "message": "Thank you for using"
        },
        "status": "Success",
        "bank": "HDFC",
        "otp_timeout": 60
    },
    {
        "matches": {
           "sender": ["HDFCBK", "NPCIJP"],
           "message": "debited"
        },
        "status": "Success",
        "bank": "HDFC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["HDFCBK","TXNALE", "NPCIJP"],
            "message": "declined"
        },
        "status": "Failure",
        "bank": "HDFC",
        "otp_timeout": 60
    },
    {
        "matches": {
         "sender": ["AXISBK", "NPCIJP"],
         "message": "debited"
        },
        "status": "Success",
        "bank": "AXIS",
        "otp_timeout": 60
    }
    ,
    {
        "matches": {
          "sender": ["AXISBK","NPCIJP"],
          "message": "spent"
        },
        "status": "Success",
        "bank": "AXIS",
        "otp_timeout": 60
    },
    {
        "matches": {
          "sender": ["AXISBK", "NPCIJP"],
          "message": "blocked"
        },
        "status": "Failure",
        "bank": "AXIS",
        "otp_timeout": 60
    },
    {
        "matches": {
        "sender": ["AXISBK", "NPCIJP"],
        "message": "declined"
        },
        "status": "Failure",
        "bank": "AXIS",
        "otp_timeout": 60
    },
    {
        "matches": {
         "sender": ["CITIBK", "NPCIJP"],
         "message": "spent"
        },
        "status": "Success",
        "bank": "CITI",
        "otp_timeout": 60
    },
    {
        "matches": {
           "sender": ["CITIBK", "NPCIJP"],
           "message": "has been used"
        },
        "status": "Success",
        "bank": "CITI",
        "otp_timeout": 60
    },
    {
         "matches": {
             "sender": ["CITIBK", "NPCIJP"],
             "message": "debited"
         },
         "status": "Success",
         "bank": "CITI",
         "otp_timeout": 60
    },
    {
        "matches": {
         "sender": ["CITIBK", "NPCIJP"],
         "message": "declined"
        },
        "status": "Failure",
        "bank": "CITI",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KOTAKB","NPCIJP"],
            "message": "made on"
        },
        "status": "Success",
        "bank": "KOTAK",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KOTAKB","NPCIJP"],
            "message": "could not be processed"
        },
        "status": "Failure",
        "bank": "KOTAK",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB","NPCIJP"],
            "message": "debited"
        },
        "status": "Success",
        "bank": "ICICI",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB","NPCIJP"],
            "message": "declined"
        },
        "status": "Failure",
        "bank": "ICICI",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IDFCFB", "NPCIJP"],
            "message": "OTP"
        },
        "otp": "\\d{6}",
        "bank": "IDFCNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IDFCBK","NPCIJP"],
            "message": "You have made"
        },
        "status": "Success",
        "bank": "IDFCDC",
        "otp_timeout": 60
    }
]


var otpRules = [
    {
        "matches": {
            "sender": ["PNBSMS","PNBACS", "NPCIJP"],
            "message": "Your One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "PNBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IDFCBK", "NPCIJP"],
            "message": "is your OTP"
        },
        "otp": "\\d{6}",
        "bank": "IDFCDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IDFCBK", "NPCIJP"],
            "message": "IS THE OTP"
        },
        "otp": "\\d{6}",
        "bank": "IDFCDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KOTAKB", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "Kotak",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["AIRBNK", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "AIRTEL",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["AIRBNK", "AIRVOT", "ARWINC" ,"NPCIJP"],
            "message": "is your OTP"
        },
        "otp": "\\d{6}",
        "bank": "AIRTEL",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["RM-020001", "NPCIJP"],
            "message": "is the One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "Kotak",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["HDFCBK", "NPCIJP"],
            "message": "One time password"
        },
        "otp": "\\d{6}",
        "bank": "HDFC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["HDFCBK", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "HDFC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["HDFCBK", "NPCIJP"],
            "message": "OTP FOR"
        },
        "otp": "\\d{6}",
        "bank": "HDFC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["HDFCBK", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "HDFCNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["NPCIJP"],
            "message": "One time password is"
        },
        "otp": "\\d{6}",
        "bank": "NPCIJP",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["HDFCBK", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "HDFCNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["NPCIJP"],
            "message": "is your OTP"
        },
        "otp": "\\d{4}",
        "bank": "PAYLATER",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["FROMSC", "SCBANK", "NPCIJP"],
            "message": "Your One-Time Password"
        },
        "otp": "\\d{6}",
        "bank": "SCB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "ICICIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "is OTP for"
        },
        "otp": "\\d{6}",
        "bank": "ICICIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "5676766", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "ICICICC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "5676766", "NPCIJP"],
            "message": "IS OTP FOR"
        },
        "otp": "\\d{6}",
        "bank": "ICICICC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": ".* OTP is (\\d{6})",
        "bank": "ICICIDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "OTP IS"
        },
        "otp": ".* OTP IS (\\d{6})",
        "bank": "ICICIDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "5676766", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "ICICICC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "OTP IS"
        },
        "otp": ".* OTP IS (\\d{6})",
        "bank": "ICICICC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "is OTP for"
        },
        "otp": "\\d{6}",
        "bank": "ICICI",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "5676766", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "ICICI",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "OTP IS"
        },
        "otp": ".* OTP IS (\\d{6})",
        "bank": "ICICI",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "5676766", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": ".* OTP is (\\d{6})",
        "bank": "ICICI",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "5676766", "NPCIJP"],
            "message": "IS OTP FOR"
        },
        "otp": "\\d{6}",
        "bank": "ICICI",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CITIBK", "NPCIJP", "9632106388"],
            "message": "Onetime password"
        },
        "otp": "\\d{6}",
        "bank": "CITI",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IDBIBK", "NPCIJP"],
            "message": "The Secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "IDBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UCOBNK", "NPCIJP"],
            "message": "OTP for UCO"
        },
        "otp": "\\d{6}",
        "bank": "UCODC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBIOTP", "NPCIJP"],
            "message": "is your One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["MAHABK", "NPCIJP"],
            "message": "The Secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "BOMDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SLCEIT", "NPCIJP"],
            "message": "is your OTP"
        },
        "otp": "\\d{6}",
        "bank": "SLICESBM",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CUBSMS", "NPCIJP"],
            "message": "The Secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "CUBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IDBIBK", "NPCIJP"],
            "message": "OTP FOR"
        },
        "otp": "\\d{6}",
        "bank": "IDBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["YESBNK", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "YESDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["YESBNK", "NPCIJP"],
            "message": "OTP FOR"
        },
        "otp": ".*is (\\d{6})",
        "bank": "YESDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["YESBNK", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "YESCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["YESBNK", "NPCIJP"],
            "message": "is the otp"
        },
        "otp": "\\d{6}",
        "bank": "YESCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["YESBNK", "NPCIJP"],
            "message": "Password"
        },
        "otp": "\\d{6}",
        "bank": "YESNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["TATACD", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "SBICC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBICRD", "NPCIJP"],
            "message": "OTP for trxn"
        },
        "otp": "\\d{6}",
        "bank": "SBICC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBIACS", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBIACS", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CITIBK", "NPCIJP"],
            "message": "is the Onetime password"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["DBSBNK", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "DBSDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBICRD", "NPCIJP"],
            "message": "IS THE OTP"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBIACS", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "SBICC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBIACS", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "SBINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["FROMSC", "SCBANK", "NPCIJP"],
            "message": "One-Time Password"
        },
        "otp": "\\d{6}",
        "bank": "SCBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["TATACD", "NPCIJP"],
            "message": "OTP for trxn"
        },
        "otp": "\\d{6}",
        "bank": "TATACD",
        "otp_timeout": 60
    },
//    {
//        "matches": {
//            "sender": [".*"],
//            "message": "OTP"
//        },
//        "otp": "\\d{8}",
//        "bank": "SBHNB",
//        "otp_timeout": 60
//    },
//    {
//        "matches": {
//            "sender": [".*"],
//            "message": "OTP"
//        },
//        "otp": "\\d{8}",
//        "bank": "SBBJNB",
//        "otp_timeout": 60
//    },
//    {
//        "matches": {
//            "sender": [".*"],
//            "message": "OTP"
//        },
//        "otp": "\\d{8}",
//        "bank": "SBPNB",
//        "otp_timeout": 60
//    },

    {
        "matches": {
            "sender": ["SBIINB", "NPCIJP"],
            "message": "OTP"
        },
        "otp": "\\d{8}",
        "bank": "SBINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "ICICINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "ICICINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "is OTP"
        },
        "otp": "\\d{6}",
        "bank": "ICICINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "ICICINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "IS THE OTP"
        },
        "otp": "\\d{6}",
        "bank": "ICICINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CUBANK", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{8}",
        "bank": "CUBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CUBANK", "NPCIJP"],
            "message": "OTP FOR"
        },
        "otp": "\\d{6}",
        "bank": "CUBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "ICICIQC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["HSBCIN", "NPCIJP"],
            "message": "OTP"
        },
        "otp": "\\d{6}",
        "bank": "HSBCCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["HSBCIN", "NPCIJP"],
            "message": "Onetime password"
        },
        "otp": "\\d{6}",
        "bank": "HSBCCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["AXISBK", "NPCIJP"],
            "message": "your NETSECURE code"
        },
        "otp": "\\d{6}",
        "bank": "AXISNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["AXISBK", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{6}",
        "bank": "AXISNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["AXISBK", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{6}",
        "bank": "AXIS",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["AXISBK", "NPCIJP"],
            "message": "ONE TIME PASSWORD for"
        },
        "otp": "\\d{6}",
        "bank": "AXIS",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CANBNK", "NPCIJP"],
            "message": "OTP"
        },
        "otp": "\\d{5}|\\d{4}",
        "bank": "CANARANB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CANBNK", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "CANARADC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ALBANK", "NPCIJP"],
            "message": "please enter OTP"
        },
        "otp": "\\d{6}",
        "bank": "ALLBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["MYAMEX", "NPCIJP", "59039093", "50350301", "5676735", "51457", "57273"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "AMEXCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["HDFCBK", "NPCIJP"],
            "message": ".*HDFC Bank Card \\d{4} is (\\d{6}).*"
        },
        "otp": "\\d{6}",
        "bank": "HDFCIVR",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "HDFCIVR",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KOTAKB", "NPCIJP","INGVYS"],
            "message": "One Time Authorization Code\\(OTAC\\) for online transaction"
        },
        "otp": "\\d{6}",
        "bank": "INGVYS",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["RBLBNK", "NPCIJP"],
            "message": "One Time"
        },
        "otp": "\\d{6}",
        "bank": "RBLDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["RBLBNK", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "RBLDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["RBLCRD", "RBLBNK", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "RBLCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["TATACD", "NPCIJP"],
            "message": "OTP for trxn"
        },
        "otp": "\\d{6}",
        "bank": "TATACD",
        "otp_timeout": 60
    },
    {   "matches": {
            "sender" : ["9820992273","KOTAKB","NPCIJP"],
            "message": "Dynamic Access Code for CRN"
        },
        "otp": "\\d{6}",
        "bank": "KOTAKNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IOBCHN", "NPCIJP"],
            "message": "OTP"
        },
        "otp": "\\d{6}",
        "bank": "IOBNB",
        "otp_timeout": 60
    },
    // for 24x7
    {
        "matches": {
            "sender": ["IOBOTP", "NPCIJP"],
            "message": "to Reset Password"
        },
        "otp": "\\d{4}",
        "bank": "IOBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IOBBNK", "NPCIJP"],
            "message": "YOUR OTP IS"
        },
        "otp": "\\d{6}",
        "bank": "IOBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["OBCBNK","NPCIJP"],
            "message": "Kindly use One Time Password-OTP"
        },
        "otp": "\\d{6}",
        "bank": "OBCDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["OBCBNK","NPCIJP"],
            "message": "is OTP for"
        },
        "otp": "\\d{6}",
        "bank": "OBCDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["INDBNK","NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{6}",
        "bank": "INDNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOBSMS", "BOBOTP", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{8}",
        "bank": "BOBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UCOBNK", "NPCIJP"],
            "message": "OTP FOR UCO"
        },
        "otp": "\\d{6}",
        "bank": "UCODC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UCOBNK", "NPCIJP"],
            "message": "OneTimePassword"
        },
        "otp": "\\d{6}",
        "bank": "UCONB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["LVBANK", "NPCIJP"],
            "message": "Onetime Password"
        },
        "otp": "\\d{6}",
        "bank": "LVBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["LVBANK", "LVBSMS", "NPCIJP"],
            "message": "Your One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "LVBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["LVBANK", "LVBSMS", "NPCIJP"],
            "message": "YOUR ONE TIME PASSWORD"
        },
        "otp": "\\d{6}",
        "bank": "LVBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CORPBK", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "CORPCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CORPBK", "NPCIJP"],
            "message": "One Time Password for"
        },
        "otp": "\\d{6}",
        "bank": "CORPDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CORPBK", "NPCIJP"],
            "message": "is the otp"
        },
        "otp": "\\d{6}",
        "bank": "CORPDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["YESBNK", "NPCIJP"],
            "message": "Transaction Password is"
        },
        "otp": "\\d{6}",
        "bank": "YESNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBPAYU", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBICRD", "NPCIJP"],
            "message": "is OTP"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBPAYU", "SBIOTP", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBPAYU", "SBIOTP", "SBIBDP", "NPCIJP"],
            "message": "is your OTP"
        },
        "otp": "\\d{6}",
        "bank": "SBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SBIOTP", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": ".*is (\\d{6})",
        "bank": "SBIDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOIIND", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{7}",
        "bank": "BOINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOIIND", "NPCIJP"],
            "message": "Your OTP for Card"
        },
        "otp": "\\d{7}",
        "bank": "BOIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOIIND", "NPCIJP"],
            "message": "Your OTP is"
        },
        "otp": "\\d{7}",
        "bank": "BOIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOIIND", "NPCIJP"],
            "message": "OTP"
        },
        "otp": ".* OTP (\\d{7})",
        "bank": "BOIDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["MYUDIO", "NPCIJP"],
            "message": "Onetime password"
        },
        "otp": "\\d{4}",
        "bank": "UDIOFY",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CORPBK", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "CORPNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UBICCR", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "UBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UBICCR", "NPCIJP"],
            "message": "Your OTP is"
        },
        "otp": "\\d{6}",
        "bank": "UBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UBICCR", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{6}",
        "bank": "UBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UBICCR", "UBIACQ", "NPCIJP"],
            "message": "The Secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "UBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UBICCR", "UBIACQ", "NPCIJP"],
            "message": "the secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "UBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UBICCR", "UBIACQ", "NPCIJP"],
            "message": "THE SECRET OTP"
        },
        "otp": "\\d{6}",
        "bank": "UBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KOTAKB", "KOTAK", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "KOTAKCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KBANKT", "KOTAKB", "KOTAK", "NPCIJP"],
            "message": "IS THE ONE TIME PASSWORD"
        },
        "otp": "\\d{6}",
        "bank": "KOTAKCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CENTBK", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": ".*is (\\d{6})",
        "bank": "CENTDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CBIBNK", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": ".*is (\\d{6})",
        "bank": "CENTDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CENTBK", "NPCIJP"],
            "message": "IS THE OTP FOR"
        },
        "otp": "\\d{6}",
        "bank": "CENTDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SYBOTP", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": "\\d{6}",
        "bank": "SYNDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SYNRUP", "NPCIJP"],
            "message": "Your One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "SYNDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SYNRUP", "NPCIJP"],
            "message": "Your One Time Password"
        },
        "otp": "\\d{4}",
        "bank": "SYNDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["APGECM", "NPCIJP"],
            "message": "OTP FOR"
        },
        "otp": "\\d{4}",
        "bank": "SYNDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["APGECM", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{4}",
        "bank": "SYNDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CBIBNK", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": ".*is (\\d{6})",
        "bank": "CBIDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CENTBK", "NPCIJP"],
            "message": "The Secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "CBIDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["MAHABK", "NPCIJP"],
            "message": "OTP FOR"
        },
        "otp": ".*is (\\d{6})",
        "bank": "BOMDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SIBSMS", "NPCIJP"],
            "message": "YOUR ONE TIME PASSWORD"
        },
        "otp": "\\d{6}",
        "bank": "SIBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["DENABK", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": "\\d{6}",
        "bank": "DENADC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["FROMSC", "SCBANK", "NPCIJP"],
            "message": "one time password"
        },
        "otp": "\\d{6}",
        "bank": "SCB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["FROMSC", "SCBANK", "NPCIJP"],
            "message": "ONE-TIME PASSWORD"
        },
        "otp": "\\d{6}",
        "bank": "SCB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["FROMSC", "SCBANK", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": "\\d{6}",
        "bank": "SCB",
        "otp_timeout": 60
    },
    {
       "matches": {
           "sender": ["FROMSC", "SCBANK", "NPCIJP"],
           "message": "is your one time password"
       },
       "otp": ".*(\\d{6})",
       "bank": "SCB",
       "group":1,
       "otp_timeout": 60
    },
    {
       "matches": {
           "sender": ["FROMSC", "SCBANK", "NPCIJP"],
           "message": "IS YOUR ONE TIME PASSWORD"
       },
       "otp": ".*(\\d{6})",
       "bank": "SCB",
       "group":1,
       "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOIIND", "NPCIJP"],
            "message": "OTP FOR"
        },
        "otp": ".*is (\\d{7})",
        "bank": "BOICC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["INDUSB", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": ".*is (\\d{6})",
        "bank": "INDUSCC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["INDUSB", "NPCIJP"],
            "message": "is the One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "INDUSCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["INDUSB", "NPCIJP"],
            "message": "THIS OTP IS"
        },
        "otp": "\\d{5}",
        "bank": "INDUSCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["INDUSB", "NPCIJP"],
            "message": "THIS OTP IS"
        },
        "otp": "\\d{6}",
        "bank": "INDUSCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["INDUSB", "NPCIJP"],
            "message": "This OTP is"
        },
        "otp": "\\d{6}",
        "bank": "INDUSCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UBIBNK", "NPCIJP"],
            "message": "OTP FOR"
        },
        "otp": ".*is (\\d{6})",
        "bank": "UNBIDC",
        "group":1,
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["UBIBNK", "NPCIJP"],
            "message": "OTP IS"
        },
        "otp": "\\d{6}",
        "bank": "UNBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KOTAKB", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "KOTAKDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KOTAKB", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "KOTAKDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CUBSMS", "NPCIJP"],
            "message": "Your OTP for"
        },
        "otp": "\\d{6}",
        "bank": "CUBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CUBLTD", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{8}",
        "bank": "CUBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
        "sender": ["VIJBNK", "NPCIJP"],
        "message": "One Time Password for"
        },
        "otp": "\\d{6}",
        "bank": "VIJAYADC",
        "otp_timeout": 60
    },
    {
        "matches": {
        "sender": ["VIJBNK", "NPCIJP"],
        "message": "IS THE OTP FOR"
        },
        "otp": "\\d{6}",
        "bank": "VIJAYADC",
        "otp_timeout": 60
    },
    {
        "matches": {
        "sender": ["VIJBNK", "NPCIJP"],
        "message": "is the OTP for"
        },
        "otp": "\\d{6}",
        "bank": "VIJAYADC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["TMBANK", "NPCIJP"],
            "message": "ONE TIME PASSWORD"
        },
        "otp": "[A-Z]{3}\\d{5}",
        "bank": "TMBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["DBSBNK", "NPCIJP"],
            "message": "One time password is"
        },
        "otp": ".*is (\\S{6})",
        "bank": "DBSDC",
        "group":1,
        "otp_timeout": 60
    },
    {
       "matches": {
           "sender": ["INDBNK", "NPCIJP"],
           "message": "One Time Password for"
       },
       "otp": ".*is (\\d{6})",
       "bank": "INDDC",
       "group":1,
       "otp_timeout": 60
    },
    {
       "matches": {
           "sender": ["INDBNK", "NPCIJP"],
           "message": "OTP for"
       },
       "otp": ".* is .*(\\d{6})",
       "bank": "INDDC",
       "group":1,
       "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KTKBNK", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "KTBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KVBANK", "NPCIJP"],
            "message": "is OTP"
        },
        "otp": "\\d{6}",
        "bank": "KVBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KVBANK", "NPCIJP"],
            "message": "Your One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "KVBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["FEDBNK", "NPCIJP"],
            "message": "is your otp"
        },
        "otp": "\\d{6}",
        "bank": "FEDDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOBOTP", "NPCIJP"],
            "message": "is the one time password"
        },
        "otp": "\\d{6}",
        "bank": "BOBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOBOTP", "NPCIJP"],
            "message": "is the OTP for transaction"
        },
        "otp": "\\d{6}",
        "bank": "BOBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["BOBCRD", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "BOBCC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IOBBNK", "NPCIJP"],
            "message": "your otp is"
        },
        "otp": "\\d{6}",
        "bank": "IOBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IOBBNK", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": ".*is (\\d{6})",
        "bank": "IOBDC",
        "group":1,
        "otp_timeout": 60
    },
    {
         "matches": {
             "sender": ["SBIINB", "NPCIJP"],
             "message": "OTP to know INB username"
         },
         "otp": "\\d{8}",
         "bank": "SBINB",
         "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IDBIBK", "NPCIJP"],
            "message": "OTP IS"
        },
        "otp": "\\d{6}",
        "bank": "IDBIDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["SIBSMS", "NPCIJP"],
            "message": "OTP IS"
        },
        "otp": "\\d{6}",
        "bank": "SIBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ICICIB", "NPCIJP"],
            "message": "The Unique number is"
        },
        "otp": "\\d{6}",
        "bank": "ICICINB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ANDHBK", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "ANDHDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ANDHBK", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{6}",
        "bank": "ANDHDC",
        "otp_timeout": 60
    },
    {
       "matches": {
           "sender": ["ANKBNK", "UNIONB", "NPCIJP"],
           "message": "OTP for"
       },
       "otp": ".* is .*(\\d{6})",
       "bank": "ANDHDC",
       "group":1,
       "otp_timeout": 60
    },
    {
       "matches": {
           "sender": ["ANKBNK", "UNIONB", "NPCIJP"],
           "message": "OTP FOR"
       },
       "otp": ".* IS .*(\\d{6})",
       "bank": "ANDHDC",
       "group":1,
       "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ALBANK", "NPCIJP"],
            "message": "enter OTP"
        },
        "otp": "\\d{6}",
        "bank": "ALLBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["ALBANK", "NPCIJP"],
            "message": "The Secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "ALLBDC",
        "otp_timeout": 60
    },

    {
        "matches": {
            "sender": ["JKCARD", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{6}",
        "bank": "JKDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["KOTAKB","NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "KOTAKNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["KOTAKB","NPCIJP"],
            "message": "is your OTP"
        },
        "otp": "\\d{6}",
        "bank": "KOTAKNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender" : ["KOTAKB","NPCIJP"],
            "message": "IS YOUR OTP"
        },
        "otp": "\\d{6}",
        "bank": "KOTAKNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["PNBSMS","PNBACS", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "PNBNB",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["PNBSMS","PNBACS", "NPCIJP"],
            "message": "Your Secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "PNBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IPAYTM", "PAYTM", "NPCIJP"],
            "message": "OTP is"
        },
        "otp": "\\d{6}",
        "bank": "PAYTM",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IPAYTM", "PAYTM", "NPCIJP"],
            "message": "is your One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "PAYTM",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["IPAYTM", "PAYTM", "PAYTMB","NPCIJP"],
            "message": "The Secret OTP"
        },
        "otp": "\\d{6}",
        "bank": "PAYTM",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["KTKBNK","NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{6}",
        "bank": "KTBDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["AxisBk", "NPCIJP"],
            "message": "is the OTP"
        },
        "otp": "\\d{6}",
        "bank": "AXISDC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["CanBnk", "NPCIJP"],
            "message": "OTP for"
        },
        "otp": "\\d{6}",
        "bank": "CANARADC",
        "otp_timeout": 60
    },
    {
        "matches": {
            "sender": ["MAHABK", "NPCIJP"],
            "message": "is OTP for"
        },
        "otp": "\\d{6}",
        "bank": "BOMDC",
        "otp_timeout": 60
    },
    {
       "matches": {
           "sender": ["IRCTCI","NPCIJP"],
           "message": "Password is :"
       },
       "otp": ".*Password is : ([a-zA-Z0-9]\\w{8})",
       "bank": "IRCTC_LOGIN",
       "group":1,
       "otp_timeout": 120
    },
    {
        "matches": {
            "sender": ["IndBnk", "NPCIJP"],
            "message": "One Time Password"
        },
        "otp": "\\d{6}",
        "bank": "INDDC",
        "otp_timeout": 60
    }
];

reload_network_based_config = {
    network2G:{reloadProgressInterval:1000*1000,reloadProgressMaxTime:1000*1000},
    network3G:{reloadProgressInterval:1000*1000,reloadProgressMaxTime:1000*1000},
    networkAll:{reloadProgressInterval:1000*1000,reloadProgressMaxTime:1000*1000},
    page_load_percentage:30
};

sms_template_config = {
    sms_fly:true,
    sender_pattern:".*",
    message_pattern:".*(OTP|ONE TIME).*"
};

pg_server_redirect = {
    enabled: 0,
    redirectTaskMaxTime: 10000,
    redirectConnectionTimeout: 8000,
    redirectSoTimeout: 8000,
    redirectServerUrl: "https://redir1.juspay.in/gateway/sc",
    forwardUrls: [{url:"https://www\.billdesk\.com/pgidsk/pgmerc/AMAZONRedirect\.jsp",postData:".*State\+Bank\+Of\+India.*"},
           {url:"https://api\.zaakpay\.com/transactD.*", postData: ".*netbanking.*"}
    ]
};

pg_custom_branding_dialog = {
    default_timer_threshold: 100,
    timer_threshold : [{"billdesk":100}]
};

var blackListedIpAddresses = [];

var isIpBlackListed = function(ip){
    var blackListed = false;
    for(i in blackListedIpAddresses){
        if(ip.startsWith(blackListedIpAddresses[i])){
            blackListed = true;
            break;
        }
    }
    return blackListed;
}

var waitingDialogConfig = {
    progress_dialog_enabled:true,
    turn_off_between_transaction:false,
    dialog_timeout_max_secs: (godelVersion.indexOf('0.6')==0)?20:10000,
    dialog_timeout_if_no_progress_max_secs: (godelVersion.indexOf('0.6')==0)?10:10000,
    exclude_domains:"snapdeal|redbus|vodafone|paytm|freecharge|jio|jioconnect|indiamart|firstcry|craftsvilla",
    include_url: [
        "https:\\/\\/secure.axisbank.com\\/acs-web-axis\\/EnrollWeb\\/AxisBank\\/server\\/OtpServer\\?perform\\=commonVerifyOtp",
        "https:\\/\\/secure.axisbank.com\\/acs-web-axis\\/EnrollWeb\\/AxisBank\\/server\\/AccessControlServer\\?perform\\=USER_AUTH&CANCEL"
    ],
    exclude_url: [
//        "https:\\/\\/api.juspay.in"
    ],
    custom_dialog_domains : [],
    blacklist_dialog_domains: [],
    background_opacity: 1,
    exclude_premature_hide: [
        "shopping.icicibank.com\\/corp\\/(BANKAWAY)|(AuthenticationController)" //Don't hide before MPIN check
    ],
    messages: [
        { message: "Processing your payment", duration: -1 }
    ]
};

var configLoseConditions = {
}

var logPushConfig = {
    "interval_start" : 3000,
    "interval_batch" : 5000
}

var cacheableUrls = {"CACHEABLE_TTL": 30 * 24 * 60 * 60 * 1000,
    "log_not_cached": false,
    "should_submit_cookies": false,
    "urls":[]};

var cacheableUrlsV1 = {"CACHEABLE_TTL": 30 * 24 * 60 * 60 * 1000,
    "log_not_cached": false,
    "should_submit_cookies": false,
    "urls":[]};

var cacheableUrlsV2 = {"CACHEABLE_TTL": 30 * 24 * 60 * 60 * 1000,
    "log_not_cached": false,
    "should_submit_cookies": false,
    "urls":["https://shopping.icicibank.com/corp/PR5/L001/consumer/images/gridCard.jpg",
        "https://shopping.icicibank.com/corp/PR5/L001/consumer/images/new_logo_inner.png?mtime=1409805162000",
        //HDFC NB
        "https://netbanking.hdfcbank.com/gif/continue.gif",
        "https://netbanking.hdfcbank.com/gif/vseal_new1.gif",
        "https://netbanking.hdfcbank.com/netbanking/gif/redsq_hdfc.gif",
        "https://netbanking.hdfcbank.com/netbanking/gif/hdfc_bank.gif",
        "https://netbanking.hdfcbank.com/gif/redsq_hdfc.gif",
        "https://netbanking.hdfcbank.com/gif/hdfc_bank.gif",
        "https://netbanking.hdfcbank.com/gif/logo_left_top_new1.jpg",
        //SBINB
        "https://merchant.onlinesbi.com/sbijava/images/sbi_logo.gif",
        "https://merchant.onlinesbi.com/sbijava/images/app_loading.gif",
        "https://merchant.onlinesbi.com/sbijava/images/logout_icon.gif",
        "https://merchant.onlinesbi.com/sbijava/images/header_2_06.jpg",
        "https://merchant.onlinesbi.com/sbijava/images/verisignNew.jpg",
        "https://merchant.onlinesbi.com/sbijava/images/header_2_02.jpg",
        "https://merchant.onlinesbi.com/sbijava/images/help_icon.jpg",
        //AXISNB
        "https://www.axisbank.co.in/BankAway/web/L001/images/feba_axis_logo.png",
        "https://retail.axisbank.co.in/wps/AxisSMRetailLogin/css/images/axis_logo.png"
        ]};

var excludeUrlPatterns = [".*/favicon.ico.*",
     //SBI NB Garbage URL causing page load delay
     ".*monstat.com/sbi.png.*/merchant.onlinesbi.com/merchant/merchantprelogin.*",
     //HDFC NB
     ".*netbanking.hdfcbank.com/gif/home-top-n2_new.gif.*",
     ".*netbanking.hdfcbank.com/gif/header1_new.jpg",
     //CCA start
     ".*www.ccavenue.com/images/loading.gif.*",
     //CCA return
     ".*www.ccavenue.com/mwap/images/loader_circular.gif.*",
     //MMT
     ".*compay.makemytrip.com/payment-ui-service/responsive/common/images/mmt_logo.gif.*",
     ".*compay.makemytrip.com/payment-ui-service/responsive/common/images/ajax-loader.gif.*",
     // MIGS PG
     ".*migs.mastercard.com.au/.*/style/.*",
     ".*migs.mastercard.com.au/.*/images/.*",

     // Kotak NB js
     ".*/webtrends.kotak.com/dcs4jpcyf00000wwumq0arq17_9m6b/wtid.js",
     ".*/webtrends.kotak.com/dcs4jpcyf00000wwumq0arq17_9m6b/dcs.gif?.*",

     //AXIS NB js

     //1st page js
     ".*/www.axisbank.co.in/BankAway/web/L001/script/rc4script.js.*",
     ".*/www.axisbank.co.in/BankAway/web/L001/script/base64.js.*",
     //removable
     ".*/retail.axisbank.co.in/.*/ria/visualeffects.*",
     ".*/retail.axisbank.co.in/.*/Moonraft/fonts/iconic_stroke.ttf.*",

     //SCBNB images
     ".*/marketing.online.standardchartered.com/scb/AP02_IN/en/pre-didyouknow11.jpg.*",
     ".*/ibank.standardchartered.co.in/nfs/ibank/theme/default/images/login_breeze_right.jpg.*",
     ".*/ibank.standardchartered.co.in/nfs/ibank/theme/default/images/ico_important.gif.*",
     ".*/ibank.standardchartered.co.in/nfs/ibank/theme/default/images/ico_or.gif.*",
     ".*/ibank.standardchartered.co.in/nfs/ibank/theme/default/images/notice_img.jpg.*",
     ".*/ibank.standardchartered.co.in/nfs/ibank/theme/default/images/arrow_green.gif.*",
     ".*/ibank.standardchartered.co.in/adHelper.html?height=75&cacheb=0.19544174098096778&frameName=pre-didyouknow11.*",
     ".*/marketing.online.standardchartered.com/scb/AP02_IN/en/pre-didyouknow11.*",

     //removable common files
     ".*/retail.axisbank.co.in/.*/common/NgroupletActionHandler.js.*",
     ".*/retail.axisbank.co.in/.*/common/Nfacebook-utils.js.*",
     ".*/retail.axisbank.co.in/.*/common/Nmessages.js.*",
     ".*/retail.axisbank.co.in/.*/common/NmessageFunctions.js.*",
     ".*/retail.axisbank.co.in/.*/common/NGeoTagging.js.*",
     ".*/retail.axisbank.co.in/.*/common/Ninteractivity.js.*",
     ".*/retail.axisbank.co.in/.*/common/NtypeSysValidation.js.*",
     ".*/retail.axisbank.co.in/.*/common/NpageCustom.js.*",
     ".*/retail.axisbank.co.in/.*/common/NHWMails.js.*",
     ".*/retail.axisbank.co.in/.*/common/Nmashup.js.*",
     ".*/retail.axisbank.co.in/.*/common/Njson2.js.*",
     ".*/retail.axisbank.co.in/.*/common/NAnalyticsEngine.js.*",
     ".*/retail.axisbank.co.in/.*/common/NEBGCD.js.*",
     ".*/retail.axisbank.co.in/.*/common/NListingDetails.js.*",

     //AXISNB images
     ".*/retail.axisbank.co.in/wps/WsrpProxyPortlet/.*/widget-loading.gif.*",
     ".*/retail.axisbank.co.in/wps/WsrpProxyPortlet/.*/images/bg-axis.jpg.*",
     ".*/retail.axisbank.co.in/wps/AxisSMRetailLogin/.*/images/bg-axis.jpg.*",
     ".*/retail.axisbank.co.in/wps/WsrpProxyPortlet/.*/images/tesselation.png.*",
     ".*/retail.axisbank.co.in/wps/WsrpProxyPortlet/.*/images/moonraft/arrow_right.png.*",
     ".*/www.axisbank.co.in/BankAway/web/L001/images/feba_bg.jpg.*",
     ".*/www.axisbank.co.in/BankAway/web/L001/images/feba_bg-axis.jpg.*",
     ".*/www.axisbank.co.in/BankAway/web/L001/images/feba_form-bg.png.*",
     ".*/www.axisbank.co.in/BankAway/web/L001/images/feba_lock.png.*",
     ".*/retail.axisbank.co.in/wps/AxisSMRetailLogin/css/images/Axis%20Bank%20Logo.png.*",
     ".*/retail.axisbank.co.in/wps/WsrpProxyPortlet/.*/consumer/images/left-tri.png.*",
     ".*/retail.axisbank.co.in/wps/WsrpProxyPortlet/.*/consumer/images/right-tri.png.*",

     //SSL error URLs
     //CanaraNB
     ".*/page-source.com/resizeimage.ashx\\?ig=netbanking.canarabank.in&sz=15401",
     //SIB
     ".*/page-source.com/resizeimage.ashx\\?ig=sibernet.southindianbank.com&sz=20401",
     //BOBNB
     ".*/page-source.com/resizeimage.ashx\\?ig=www.bobibanking.com&sz=19401"
     ];

var airtel_specific_exclude_urls = [
//    //ICICINB
//    ".*shopping.icicibank.com/.*/ria/visualeffects.*",
//    ".*shopping.icicibank.com/.*/common/Ndsprocessor.js.*",
//    ".*shopping.icicibank.com/.*/common/Nmessages.js.*",
//    ".*shopping.icicibank.com/.*/common/NmessageFunctions.js.*",
//    ".*shopping.icicibank.com/.*/common/Nxregexp.js.*",
//    ".*shopping.icicibank.com/.*/common/Nxregexp-unicode.js.*",
//    ".*shopping.icicibank.com/.*/common/NtypeSysValidation.js.*",
//    ".*shopping.icicibank.com/.*/common/NgroupletActionHandler.js.*",
//    ".*shopping.icicibank.com/.*/common/NEBGCD.js.*",
//    ".*shopping.icicibank.com/.*/common/NCxpsGDHelper.js.*",
//    ".*shopping.icicibank.com/.*/common/NListingDetails.js.*",
//    ".*shopping.icicibank.com/.*/common/NAnalyticsEngine.js.*",
//    ".*shopping.icicibank.com/.*/common/Nmashup.js.*",
//    ".*shopping.icicibank.com/.*/common/NGeoTagging.js.*",
//    ".*shopping.icicibank.com/.*/ajaxfeatures/jquery.corner.js.*",
//    ".*shopping.icicibank.com/.*/adaptiveauthentication/arcot/FlashDetect.js.*",
//    ".*shopping.icicibank.com/.*/adaptiveauthentication/arcot/FDeploy.js.*",
//    ".*shopping.icicibank.com/.*/ajaxfeatures/jquery.jcarousel.js.*",
//    ".*shopping.icicibank.com/.*/ajaxfeatures/jquery.autocomplete.js.*",
//    ".*shopping.icicibank.com/.*/ajaxfeatures/jquery.stylish-select.js.*",
//    ".*shopping.icicibank.com/.*/ajaxfeatures/jquery.simplemodal-1.4.1.js.*",
//    ".*shopping.icicibank.com/.*/ajaxfeatures/jquery.blockUI.js.*",
//    ".*shopping.icicibank.com/.*/ajaxfeatures/richTextEditor.js.*",
//    ".*shopping.icicibank.com/.*/ajaxfeatures/NFEBAWidgets.js.*",
//    ".*shopping.icicibank.com/.*/adaptiveauthentication/arcot/deployJava.js.*",
//    ".*shopping.icicibank.com/.*/adaptiveauthentication/arcot/swfobject.js.*",
//    ".*shopping.icicibank.com/.*/adaptiveauthentication/arcot/iciciAdaptiveCommon.js.*",
//
//    //ICICINB image exclusion
//    ".*shopping.icicibank.com/.*/images/widget-loading.gif.*",

     //HDFCNB js exclusion
     ".*/netbanking.hdfcbank.com/jsdir/virtualkeyboard.js.*",

     //HDFCNB images
     ".*/netbanking.hdfcbank.com/.*/log-right.gif.*",
     ".*/netbanking.hdfcbank.com/.*/log-bottom.gif.*",
     ".*/netbanking.hdfcbank.com/.*/log-corner.gif.*",
     ".*/netbanking.hdfcbank.com/.*/phrase_curve_top.gif.*",
     ".*/netbanking.hdfcbank.com/.*/phrase_curve_bott.gif.*",
     ".*/netbanking.hdfcbank.com/.*/header1_new1.jpg.*",

    //SBINB js exclusion
     ".*/merchant.onlinesbi.com/sbijava/js/virtualkb.js.*",

     //SBINB images
     ".*/merchant.onlinesbi.com/.*/images/right_link_curve.gif.*",
     ".*/merchant.onlinesbi.com/.*/images/changelimit_button_bg.jpg.*",
     ".*/merchant.onlinesbi.com/.*/images/kb_bg.gif.*",
     ".*/merchant.onlinesbi.com/.*/images/acctlist_separator_top.jpg.*",
     ".*/merchant.onlinesbi.com/.*/images/main_curve_bg.jpg.*",
     ".*/merchant.onlinesbi.com/.*/images/acctlist_separator_btm.jpg.*"
     ];

var reloadableUrls = {"urls":[".*netsafe\.hdfcbank\.com.*",".*acs\.onlinesbi\.com.*",".*"]};
var nonReloadableUrls = {"urls":[
    ".*pc\.amxvpos\.com.*",
    ".*indusnet\.indusind\.com\/corp\/BANKAWAY.*",
    ".*www\.statebankofindia\.com.*",
    "mailto.*"
]};

var rupay_specific_domains= [
    "ias.fisglobal.com",
    "rupay-pnb.enstage-sas.com",
    "rupay.enstage-sas.com",
    "online.billdesk.com",
    "www.billdesk.com",
    "paysecure.yalamanchili.in",
    "starconnectcbs.bankofindia.com",
    "tab.syndicatebank.in",
    "www.payments.vijayabankonline.in"
]

var bank_js_urls = [
    "https:\\/\\/.*.icicibank.com\\/ACSWeb\\/js\\/aes\\.js", //icici
    "https:\/\/s3.ap-south-1.amazonaws.com\/in-mum-wibmo-acs\/static\/common\/js\/cipher\.js", //Canara Debit Card
//    "https:\/\/acs2.onlinesbi.com\/bdacs\/sbi\/visa\/images\/jquery\.min\.js", // SBI Debit Card
    "https:\/\/secure4.arcot.com\/acspage\/GenericOTP_SBI_Visa\/js\/main\.js", // SBI Credit Card
    "https:\/\/acs7.enstage-sas.com\/acs-web-v2\/jsp\/enTrack\/enTrack\.js", // Kotak Credit Card
    "https:\/\/.*.enstage-sas.com\/acs-web-v2\/jsp\/enTrack\/enTrack\.js", // Kotak Debit Card
    "https:\/\/secure.axisbank.com\/acs-web-axis\/jsp\/enTrack\/enTrack\.js", // Axis
    "https:\/\/cardsecurity.standardchartered.com\/acspage\/OTP_Auth_SCB\/js\/master\.js" //SCB
]

var fragmentConfig = {
    "acs_text": "CLICK BUTTONS BELOW FOR SECURE PAYMENTS",
    "otp_text_0": "Waiting to detect Bank OTP SMS",
    "otp_text_1": "Waiting to detect Bank OTP SMS",
    "otp_text_2": "Waiting to detect Bank OTP SMS",
    "otp_delay": "10000",
    "sms_polling_interval":"10000",
    "otp_counter_delay":"15000",
    "otp_wait_more_time":"30000",
    "otp_auto_submit_time":"5000",
    "sms_polling_start_time":"0",
    "sms_back_read_session_time":"0",
    "auto_submit_otp_life":"604800", //1 week
    "manual_otp_help": "Securely Enter OTP Below",
    "waiting_text_connect": "Securely connecting with your Bank",
    "waiting_text_otp": "Waiting to detect Bank OTP SMS",
    "retry_dialog_title" : "Could not read OTP",
    "otp_do_not_close" : "Please do not close",
    "waiting_do_not_close" : "Please do not close",
    "waiting_text_pass": "Processing your authentication option",
    "waiting_text_success": "Processing your payment",
    "waiting_text_default": "We Are Preparing Options For You",
    "waiting_dialog_processing" : "Processing Your Request",
    "waiting_dialog_still_processing" : "Still processing ...",
    "nb_customer_id_remember" : "Remember Customer ID",
    "waiting_fragment" : "Waiting...",
    "reached_otp_state" : "Waiting for OTP...",
    "reached_acs_state" : "Select Authentication Option",
    "reached_send_sms_state" : "Awaiting approval to send sms...",
    "received_otp" : "Awaiting Approval...",
    "reached_manual_otp_state" : "Please enter OTP...",
    "sms_text": "Send SMS to ",
    "nb_cust_fragment_color": "#282828",
    "nb_default_fragment_color": "#282828",
    "pass_fragment_color": "#282828",
    "overlay_top_color": "#00000000",
    "overlay_bottom_color": "#00000000",
    "waiting_fragment_opaque": "true",
    "highlight_show_password_backspace":"false",
    "highlight_show_password_always":"false",
    "highlight_navigation_buttons_always":"false",
    // 0.9 onwards
    "otp_approve_button_text": "APPROVE",
    "long_press_to_stop_auto_submission":"true",
    "stop_auto_submission_on_fragment_touch":"true"
};

var assetConfig = function() {return matchFirst(
    {
        "0.4_acs_js":{
            c:"godelRemotesVersion == '0.4'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.4/acs.jsa"
            }
        },
        "0.4rc1_acs_js":{
            c:"godelRemotesVersion == '0.4rc1'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.4rc1/acs.jsa"
            }
        },
        "0.4rc3_acs_js":{
            c:"godelRemotesVersion == '0.4rc3'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.4rc3/acs.jsa"
            }
        },
        "0.4rc4_acs_js":{
            c:"godelRemotesVersion == '0.4rc4'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.4rc4/acs.jsa"
            }
        },
        "0.5rc0_acs_js":{
            c:"godelRemotesVersion == '0.5rc0'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.5rc0/acs.jsa",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.5rc0/uber.jsa",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.5rc0/uber_html.jsa"
            }
        },
        "0.5rc1_acs_js":{
            c:"godelRemotesVersion == '0.5rc1'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.5rc1/acs.jsa",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.5rc1/uber.jsa",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.5rc1/uber_html.jsa"
            }
        },
        "0.5rc3_acs_js":{
            c:"godelRemotesVersion == '0.5rc3'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.5rc3/acs.jsa",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.5rc3/uber.jsa",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.5rc3/uber_html.jsa"
            }
        },
        "0.5rc5_acs_js":{
            c:"godelRemotesVersion == '0.5rc5'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.5rc5/acs.zip",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.5rc5/uber.zip",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.5rc5/uber_html.zip"
            }
        },
        "0.6rc1_acs_js":{
            c:"godelRemotesVersion == '0.6rc1'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc1/acs.zip",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc1/uber.zip",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.6rc1/uber_html.zip"
            }
        },
        "0.6rc9_v1_acs_js":{
            c:"godelRemotesVersion == '0.6rc9' && (godelVersion.indexOf('0.6') == 0 && godelVersion.split('.').length >= 3 && godelVersion.split('.')[2] >= 25)",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/v1-acs.zip",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/v1-uber.zip",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/v1-uber_html.zip",
                "dropout_js_source": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/v1-dropout.zip"
            }
        },
        "0.6rc9_acs_js":{
            c:"godelRemotesVersion == '0.6rc9'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/acs.zip",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/uber.zip",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/uber_html.zip",
                "dropout_js_source": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/dropout.zip"
            }
        },
        "0.6rc10_acs_js":{
            c:"godelRemotesVersion == '0.6rc10'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc10/acs.zip",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc10/uber.zip",
                "uber_html_source": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc10/uber_html.zip"
            }
        },
        "0.6rc11_acs_js":{
            c:"godelRemotesVersion == '0.6rc11'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc11/acs.zip",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.6rc11/uber.zip",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.6rc11/uber_html.zip",
                "dropout_js_source": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc11/dropout.zip"
            }
        },
        "0.0_int_acs_js":{
            c:"godelRemotesVersion == '0.0_int'",
            r:{
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.0_int/acs.zip",
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.0_int/uber.zip",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.0_int/uber_html.zip"
            }
        },
        "0.7rc1_acs_js":{
            c:"godelRemotesVersion == '0.7rc1'",
            r:{
                "uber_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.7rc1/uber.zip",
                "acs_js_source" : "https://d3e0hckk6jr53z.cloudfront.net/0.7rc1/acs.zip",
                "uber_html_source":"https://d3e0hckk6jr53z.cloudfront.net/0.7rc1/uber_html.zip",
                "jpl_source":"https://d3e0hckk6jr53z.cloudfront.net/0.7rc1/jpl.zip"

            }
        },
        "0.8rc1_acs_js":{
            c:"godelRemotesVersion == '0.8rc1'",
            r:{
                "uber_js_source" : "https://s3-ap-southeast-1.amazonaws.com/godel-remote-assets/0.8rc1/uber.zip",
                "acs_js_source" : "https://s3-ap-southeast-1.amazonaws.com/godel-remote-assets/0.8rc1/acs.zip",
                "uber_html_source":"https://s3-ap-southeast-1.amazonaws.com/godel-remote-assets/0.8rc1/uber_html.zip",
                "nnb_dui_bundle_source": "https://s3-ap-southeast-1.amazonaws.com/godel-remote-assets/0.8rc1/nnb_dui_bundle.zip",
                "nnb_dui_loader_source": "https://s3-ap-southeast-1.amazonaws.com/godel-remote-assets/0.8rc1/nnb_dui_loader.zip"
            }
        },
        "0.9rc1_acs_js":{
            c:"godelRemotesVersion == '0.9rc1'",
            r:{
                "acs_js_source" : "https://d3n85rao6710xg.cloudfront.net/godel-core/release/0.9rc1/v1-acs.zip",
                "dui_js_source" : "https://d3n85rao6710xg.cloudfront.net/godel-core/release/0.9rc1/v1-index_bundle.zip",
                "boot_loader_js_source": "https://d3n85rao6710xg.cloudfront.net/godel-core/release/0.9rc1/v1-boot_loader.zip"
            }
        },
        "0.9rc2_acs_js":{
            c:"godelRemotesVersion == '0.9rc2'",
            r:{
                "acs_js_source" : "https://d3n85rao6710xg.cloudfront.net/godel-core/release/0.9rc2/v1-acs.zip",
                "dui_js_source" : "https://d3n85rao6710xg.cloudfront.net/godel-core/release/0.9rc2/v1-index_bundle.zip",
                "boot_loader_js_source": "https://d3n85rao6710xg.cloudfront.net/godel-core/release/0.9rc2/v1-boot_loader.zip"
            }
        }
    }
)};

var certificatesLocation = function() {return matchFirst(
    {
        "from_0.6.18_to_0.6.21":{
            c:"godelVersion.indexOf('0.6') >= 0 && godelVersion.split('.').length >= 3 && parseInt(godelVersion.split('.')[2]) >= 18 && parseInt(godelVersion.split('.')[2]) <= 21",
            r:{
                "certificates_source": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/certificates.zip"
            }
        },
        "from_0.6.22":{
            c:"(godelVersion.split('.').length >= 3 && ((godelVersion.indexOf('0.6') >= 0 && parseInt(godelVersion.split('.')[2]) >= 22) || (parseInt(godelVersion.split('.')[1]) >= 9))) || godelVersion == '0.6.16'",
            r:{
                "certificates_source": "https://d3e0hckk6jr53z.cloudfront.net/0.6rc9/certificates_v1.zip"
            }
        }
    }
)};

var permissionUberConfig = {
    uberType : "dialog",
    shouldShowOnLoad : true,
    uberScreenHeight : "12%",
    uberScreenWidth : -2
};

var progressBarConfig = {
    color: "#000000"
};

var trustManagerConfig = {
    "defaultDomainRegex": [".*"]
}

var weblabConfig = function() {return matchAll(
    [
        {
            c:ALL,
            r:{
                // Godel features
                "ANDROID_VERSION_23":1,"location":0,"dynamicConfig":0,"shouldUseMemory":4,"minCacheSizeAvailability":4000000,"maxCacheSize":4000000,"zoomEnabled":0,"displayZoom":0,
                "setNumberOfExtraParameterAllowed":"10","trust_manager":1,"godelTrackerExitDelayMills":30*1000,"log_typing_behaviour":0,"cacheImageV2":0,"log_scroll":0,
                "hide_press_and_hold_button":0,"canStoreCustomerId":1,"sendSmsEvent":1,"HTML_DUMP_CHECK":0,"progress_bar":0,"should_use_fragment_config":0.5,"GLOBAL_MPIN":0,
                "ICICINB_MPIN":0,"KOTAKNB_MPIN":0,"AXISNB_MPIN:":0,"sms_consent":1,"sms_consent_denied_forever_threshold":1,"sms_consent_max_deny":2,"sms_consent_cool_off_period":2592000000,
                // Bank Support
                "pi_CORPNB":0,"pi_INDNB":1,"pi_YESDC":0,"pi_CENTDC":0,"pi_ICICIDC":0,"pi_BOMDC":0,"pi_BOICC":0,"pi_INDUSCC":0,"pi_UNBIDC":0,"pi_INDDC":0,"pi_AMEXCC":1,"pi_PNBDC":1,"pi_DBSDC":0,"pi_CBIDC":0,
                "pi_CUBNB":0.5,"pi_IDFCDC":0.5,"RUPAY_PAYTM":0.5,"RUPAY_KTBDC":0.5,"RUPAY_CANARADC":0.5,"RUPAY_AXISDC":0.5,"pi_IRCTC_LOGIN":1,"pi_SCB":0,
                // Uber
                "SBINB_RESET_UNAME_HELP":0,"HDFCNB_RESET_IPIN":0,"HDFCNB_RECOVER_CUSTID":0,"ICICINB_RESET_CREDENTIALS":0,"KOTAKNB_RESET_CREDENTIALS":0,"SBINB_RESET_CREDENTIALS":0,
                "SBINB_PASSWORD_BACKPRESS":0,"SBINB_RESET_PASSWORD":0,"REDIRECT_DEBIT":0,"CANARANB_RESET_CREDENTIALS":0,"IOBNB_RESET_CREDENTIALS":0,
                "SHOW_READ_PHONE_STATE_PERMISSION":1,"UPDATE_WEBVIEW":0,"WEBVIEW_ALERT":1,
                // MOBUI, Auto Clicks, Auto Choose
                "MOBUI_IOBNB":0,"MOBUI_INDNB":0,"MOBUI_PNBNB":0,"MOBUI_CANARANB":0,"MOBUI_BOINB":0,"MOBUI_HTMLDUMP":0,"MOBUI_ICICIDCPIN":0,"MOBUI_HDFC":0,"MOBUI_CITI":0.5,"MOBUI_ICICIDC":0.5,
                "isAutoAuthEnabledHdfc":1,"isAutoAuthEnabledIciciDC":1,"isAutoAuthEnabledAxis":1,
                "GLOBAL_AUTO_CLICK":1,"GLOBAL_MOBUI":1,"AUTO_CHOOSE_ICICI":0,"AUTO_CLICK_PNBNB":0,"FEEDBACK_UBER":0,"ICICIDC_AUTO_CLICK":0,"HDFC_AUTO_RESEND_OTP":0,"ICICICC_AUTO_CLICK":0,"AUTO_CLICK_ICICI":0,"AUTO_CLICK_ICICICC":0
            }
        },
        {
            c:"godelRemotesVersion == '0.4rc4'",
            r:{"GODEL":1, "otpRecordTimeout":1000*60*60*24*30}
        },
        {
            c:"clientId == 'juspay_browser_android'",
            r:{"ANDROID_VERSION_23": 1}
        },
        {
            c:"clientId == 'bms_android_debug'",
            r:{"GODEL":1,"pi_AXISNB":1, "pi_CITI":1, "pi_CITINB":1, "otpRecordTimeout":1000*60*60*24*30,"SBINB_RESET_CREDENTIALS":1, "MOBUI_SBINB": 0, "AUTOCLICK_SBINB": 0, "auto_submit_otp_enabled":0.75}
        },
        {
            c:"clientId == 'snapdeal_android'",
            r:{"GODEL":1,"uber":0,"uber_SBINB":0,"SBINB_RESET_PASSWORD":0,"SBINB_PASSWORD_BACKPRESS":0, "otpRecordTimeout":1000*60*60*24*30, "hardwareAcceleration":1}
        },
        {
            c:"clientId == 'snapdeal'",
            r:{"GODEL":1, "otpRecordTimeout":1000*60*60*24*30, "hardwareAcceleration":1}
        },
        {
            c:"godelRemotesVersion == '0.5rc1'",
            r:{"GODEL":1, "pi_AXISNB":1, "pi_CITI":1, "pi_CITINB":1, "otpRecordTimeout":1000*60*60*24*30}
        },
        {
            c:"godelRemotesVersion == '0.5rc3'",
            r:{"GODEL":1, "pi_AXISNB":1, "pi_CITI":1, "pi_CITINB":1, "otpRecordTimeout":1000*60*60*24*30}
        },
        {
            c:"clientId == 'mobikwik_android' && godelVersion=='0.6.4.2'",
            r:{"GODEL":0,"otpRecordTimeout":1000*60*60*24*15,"redirectEnabledValue":0,"reloadDialog":1}
        },
        {
            c:"clientId == 'redbus_android'",
            r:{"GODEL":1,"on_screen_display_enabled":1,"pi_ICICINB":0,"pi_BOIDC":0}
        },
        {
            c:"clientId == 'ShaadiAndroidApp'",
            r:{"GODEL":0}
        },
        {
            c:"clientId == 'indiamart_android'",
            r:{"GODEL":0}
        },
        {
            c:"clientId == 'swiggy_android'",
            r:{"GODEL":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"clientId == 'cleartrip_android'",
            r:{"GODEL":0,"otpRecordTimeout":1000*60*60*24*30}
        },
        {
            c:"godelRemotesVersion == '0.5rc5'",
            r:{"GODEL":1, "pi_HDFC":1, "ANDROID_VERSION_17":1, "SBIDC":1, "otpRecordTimeout":1000*60*60*24*30}
        },
        {
            c:"clientId == 'fcbrowser_android'",
            r:{"GODEL":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"clientId == 'foodpanda_android'",
            r:{"GODEL":1,"pi_HDFC":1,"pi_HDFCNB":1,"ANDROID_VERSION_23":0.5}
        },
        {
            c:"clientId == 'mp_flipkart'",
            r:{"GODEL":1,"ANDROID_VERSION_23":1,"fragment_nb_customer_id":1,"MOBUI_HTMLDUMP":0}
        },
        {
            c:"clientId == 'com.jabong.android'",
             r:{"GODEL":1,"pi_SBINB":1,"pi_HDFC":1, "pi_ICICIDC":1, "pi_ICICICC":1,"uber":0,"uber_SBINB":0,"SBINB_RESET_PASSWORD":0,"SBINB_PASSWORD_BACKPRESS":0}
        },
        {
            c:"clientId == 'yatra_android'",
            r:{"GODEL":1,"pi_AXISNB":1,"pi_KOTAKNB":1,"pi_HDFC":1,"pi_SBICC":1,"pi_SBIDC":1,"uber":1,"uber_SBINB":0,"SBINB_RESET_PASSWORD":0,"SBINB_PASSWORD_BACKPRESS":0, "auto_submit_otp_enabled":0,"AUTOCLICK_SBINB": 0}
        },
        {
            c:"clientId == 'rummycircle_android'",
            r:{"pi_CANARANB":0}
        },
        {
            c:"clientId == 'rr_v2_android'",
            r:{"GODEL":1,"pi_AXISNB":1,"pi_KOTAKNB":1,"pi_HDFC":1,"pi_SBICC":1,"pi_SBIDC":1,"pl_enabled":1}
        },
        {
            c:"godelVersion == '0.6.3' && clientId == 'mmt_android'",
            r:{"GODEL":0}
        },
        {
            c:"godelVersion == '0.6.6'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"godelVersion == '0.6.7.2'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"godelVersion == '0.6.7'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"godelVersion == '0.6.7.1.2'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"godelVersion == '0.6.7.1.3'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"godelVersion == '0.6.7_sd'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"godelVersion == '0.6.7_sd1'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"godelVersion > '0.6.9'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1}
        },
        {
            c:"godelVersion == '0.6.8.7' && (clientId == 'yatra_android' || clientId == 'swiggy_android')",
            r:{"HDFCNB_RESET_IPIN":1,"HDFCNB_RECOVER_CUSTID":1,"ICICINB_RESET_CREDENTIALS":0, "KOTAKNB_RESET_CREDENTIALS":1, "CANARANB_RESET_CREDENTIALS":1,"IOBNB_RESET_CREDENTIALS":1}
        },
        {
            c:"godelVersion == '0.6.15'",
            r:{"LANDSCAPE_MOBUI":1, "pi_CANARANB":0,
                "MOBUI_YESNB":1,"MOBUI_SBINB":1,"MOBUI_SBTNB":1,"MOBUI_SBHNB":1,"MOBUI_SBPNB":1,"MOBUI_SBMNB":1,"MOBUI_SBBJNB":1,"MOBUI_HDFCNB":1,"MOBUI_AXISNB":1,"MOBUI_IOBNB":0,"MOBUI_INDNB":0,"MOBUI_KVBNB":1,"MOBUI_SBIDC":1,
                "AUTO_CLICK_AXISNB":1,"AUTO_CLICK_CANARANB":1,"AUTO_CLICK_CORPNB":1,"AUTO_CLICK_SBINB":1,"AUTO_CLICK_BOINB":1,"AUTO_CLICK_ICICINB":1,"AUTO_CLICK_HDFCNB":1,"WEBVIEW_ALERT":1,
                "AUTO_CLICK_KOTAKNB":1,"AUTO_CLICK_PNBNB":1,"AUTO_CLICK_ICICIDC":1,"AUTO_CLICK_PNBDC":1,"AUTO_CLICK_UCODC":1,"AUTO_CLICK_INDDC":1,"AUTO_CLICK_KTBDC":1,"AUTO_CLICK_ICICI":1,"AUTO_CLICK_ICICICC":1,"AUTO_CLICK_CORPCC":1,"GLOBAL_AUTO_CHOOSE":1,"AUTO_CHOOSE_HDFC":1,"MOBUI_PNBNB":0,"MOBUI_CORPNB":1,"MOBUI_CANARANB":0,"MOBUI_HDFC":0,"MOBUI_BOINB":1}
        },
        {
            c:"godelVersion == '0.6.17'",
            r:{"WEBVIEW_ALERT":0}
        },
        {
            c:"godelVersion == '0.6.18'",
            r:{"WEBVIEW_ALERT":0}
        },
        {
            c:"godelVersion == '0.6.22'",
            r:{"WEBVIEW_ALERT":0}
        },
        {
            c:"godelVersion == '0.6.21.1'",
            r:{"pi_CENTDC":1, "pi_BOMDC":1, "pi_BOICC":1, "pi_INDUSCC":1, "pi_UNBIDC":1, "pi_INDDC":1, "pi_AMEXCC":1, "pi_CORPNB":1}
        },
        {
            c:"clientId == 'tul_android'",
             r:{"GODEL":1,"ANDROID_VERSION_23":1,"reloadDialog":1}
        },
        {
            c:"clientId == 'hike_android'",
             r:{"GODEL":1,"ANDROID_VERSION_23":1,"reloadDialog":1,"pi_YESNB":1}
        },
        {
            c:"clientId == 'juspay_recharge_android'",
            r:{"GODEL":1,"pi_SBIDC":1,"pi_SBICC":1,"ANDROID_VERSION_23":1,"reloadDialog":1,"otpRecordTimeout":1000*60*60*24*30,"pl_enabled":1,
            "global_exception_handling":0, "HDFCNB_RESET_IPIN":1,"HDFCNB_RECOVER_CUSTID":1,"ICICINB_RESET_CREDENTIALS":0, "KOTAKNB_RESET_CREDENTIALS":1, "hide_press_and_hold_button":0, "CANARANB_RESET_CREDENTIALS":1,"IOBNB_RESET_CREDENTIALS":1,"on_screen_display_enabled":1,"feature_dynamic_sms_permission":1}
        },
        {
            c:"clientId == 'Myntra'",
            r:{"GODEL":1,"pi_SBIDC":1,"pi_SBICC":1,"ANDROID_VERSION_23":1,"ANDROID_VERSION_14":0,"ANDROID_VERSION_15":0,"ANDROID_VERSION_16":0,"ANDROID_VERSION_17":0,"ANDROID_VERSION_18":0,"reloadDialog":1,"uber_SBINB":0,"SBINB_RESET_PASSWORD":0,"SBINB_PASSWORD_BACKPRESS":0,"otpRecordTimeout":1000*60*60*24*30}
        },
        {
            c:"clientId == 'firstcry_android'",
            r:{"auto_submit_otp_enabled" :0,"hardwareAcceleration":1}
        },
        {
            c:"clientId == 'voonikapp_android'",
            r:{"GODEL":1,"ANDROID_VERSION_23":1,"reloadDialog":1,"otpRecordTimeout":1000*60*60*24*30,"pl_enabled":0}
        },
        {
            c:"clientId == 'juggernaut_android'",
            r:{"ANDROID_VERSION_23":1}
        },
        {
            c:"clientId == 'myairtel_android'",
            r:{"GODEL":1,"ANDROID_VERSION_23":1,"pi_HDFC":1,"pi_FEDDC":1,"pi_SBICC":1,"pi_AMEXCC":1,"pi_CITI":1,"fragment_nb_customer_id":1,"fragment_password":1,"reloadDialog":0,"fragment_acs":0,"fragment_nb_default":1,"fragment_password_generic":1,"fragment_waiting":0,"HDFCNB_RESET_IPIN":0,"HDFCNB_RECOVER_CUSTID":0,"ICICINB_RESET_CREDENTIALS":0, "KOTAKNB_RESET_CREDENTIALS":0, "hide_press_and_hold_button":1, "CANARANB_RESET_CREDENTIALS":0,"IOBNB_RESET_CREDENTIALS":0, "cacheImageV2":1,
              "MOBUI_CANARANB":0,"isAutoAuthEnabledHdfc":0,"isAutoAuthEnabledAxis":0,"isAutoAuthEnabledIciciDC":0,"GLOBAL_AUTO_CLICK":0,"log_scroll":0,"log_typing_behaviour":0,"uber":1,"GLOBAL_AUTO_CHOOSE":0,"FEEDBACK_UBER":1}
        },
        {
            c:"clientId == 'myairtel_android' && (godelVersion.indexOf('0.6') == 0 && godelVersion.split('.').length >= 3 && godelVersion.split('.')[2] >= 17)",
            r:{"UPDATE_WEBVIEW":0}
        },
        {
            c:"clientId == 'mpesa_android'",
            r:{"pig_NB":0.5}
        },
        {
            c:"clientId == 'vodafone'",
            r:{"auto_submit_otp_enabled":0}
        },
        {
            c:"clientId == 'freshmenu_ios'",
            r:{"GODEL":1}
        },
        {
            c:"clientId == 'redbus_iOS'",
            r:{"pi_ICICINB":0}
        },
        {
            c:"clientId == 'olacabs_ios'",
            r:{"pi_HDFCNB":0}
        },
        {
            c:"clientId == 'byg_ios'",
            r:{"GODEL":0}
        },
        {
            c:"clientId == 'Grofers_ios'",
            r:{"GODEL":0}
        },
        {
            c:"clientId == 'grofers_android'",
            r:{"pi_ICICINB":0}
        },
        {
            c:"clientId == 'byg'",
            r:{"GODEL":0}
        },
        {
            c:"clientId == 'yatra_android'",
            r:{"GODEL":0}
        },
        {
            c:"godelVersion == '0.0_int'",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1,"GODEL":1}
        },
        {
            c:"godelRemotesVersion == '0.6rc9'",
            r:{"init_uber_on_start":1, "reloadDialog":1,"log_scroll":1,"log_typing_behaviour":0,"GLOBAL_RUPAY":0}
        },
        {
            c:"godelRemotesVersion == '0.9rc1'",
            r:{"GLOBAL_RUPAY":0}
        },
        {
            c:"clientId == 'mmt_android'",
            r:{ "global_exception_handling" :1, "on_screen_display_enabled":1,"auto_submit_otp_enabled" :1, "reloadDialog":1,"MOBUI_HTMLDUMP":0 }
        },
        {
            c:"parseFloat(os_version) < 4.4",
            r:{"GLOBAL_MOBUI":0}
        },
        {
            c:"(clientId.toLowerCase().indexOf('_ios') >= 0 && parseFloat(godelVersion) >= 0.7) || (godelVersion.indexOf('0.6') >= 0 && ((godelVersion.split('.').length == 3 && (parseInt(godelVersion.split('.')[2]) >= 9)) || (godelVersion.split('.').length >= 4 && (parseInt(godelVersion.split('.')[2]) > 9 || (parseInt(godelVersion.split('.')[2]) == 9 && parseInt(godelVersion.split('.')[3]) >= 3))))) ||(godelVersion.indexOf('0.9') == 0) || (godelVersion.indexOf('1.0') == 0) || (godelVersion.indexOf('1.1') == 0) || (godelVersion.indexOf('2.0') == 0)",
            r:{"reloadDialog":1,"ANDROID_VERSION_23":1, "pi_INDDC":1, "pi_ICICIDC":1, "pi_UNBIDC":1, "pi_INDUSCC":1, "pi_BOMDC":1, "pi_YESDC":1, "pi_CENTDC":1,"pi_DBSDC":1,"pi_CBIDC":1,"pi_SCB":1}
        },
        {
            c:"clientId.toLowerCase().indexOf('_ios') >= 0",
            r:{"isAutoAuthEnabledIciciDC":0, "MOBUI_HDFC":0, "MOBUI_SBIDC":0, "pi_YESNB": 0,"pi_ICICIDC":0.1,"pi_ICICI":0.1,"pi_ICICICC":0.1,"pi_ICICINB":0, "pi_AXIS":0.1, "pi_KOTAKCC":0.1, "fragment_otp":0, "auto_submit_otp_enabled":0}
        },
        {
            c:"clientId.toLowerCase().indexOf('_ios') >= 0 && (godelVersion.split('.')[0] >= 1 || godelVersion.split('.')[1] >= 8 || (godelVersion.split('.').length == 3 && godelVersion.split('.')[1] >= 7 && godelVersion.split('.')[2] >= 11))",
            r:{"fragment_otp": 1}
        },
        {
    		c:"typeof iOSVersion != 'undefined' && iOSVersion.split('.')[0] >= 13",
    		r:{"fragment_otp": 0}
        },
        {
            c:"clientId == 'myjio_ios'",
            r:{"reloadDialog":0}
        },
        {
            c:"clientId == 'dreamplug_ios' || clientId == 'dreamplug_live_ios'",
            r:{"fragment_otp": 0}
        },
        {
            c:"clientId == 'bigbasket_ios' || clientId == 'bb_staging_ios'",
            r:{"fragment_otp": 0}
        },
        {
            c:"clientId == 'SPI_CINEMAS_ANDROID'",
            r:{"GLOBAL_MOBUI" :1}
        },
        {
            c:"(godelVersion.indexOf('0.9') == 0 && godelDuiVersion ==\"0.9rc1_1\")",
            r:{"pi_ICICIDC":1,"pi_DBSDC":0}
        },
        {
            c:"(godelVersion == '0.6.26.0')",
            r:{"GLOBAL_RUPAY":1}
        },
        {
            c:"clientId == 'uber_android'",
            r:{"MOBUI_HTMLDUMP":0}
        },
        {
            c:"clientId == 'oxigen_android'",
            r:{"GODEL":0}
        },
        {
            c:"clientId == 'goibibo_android'",
            r:{"auto_submit_otp_enabled":0, "pi_ICICIDC":1}
        },
        {
            c:  "juspayDeviceId == 'f10429783405849f56e81942f57a3916a09c91023858ff89d0a7bf0c2d354b06' || " +
                "juspayDeviceId == '561d502b2b4f704d65fe4d2427bd6dc0'",
            r:{"GODEL":0}
        },
        {
            c:"clientId == 'dreamplug_android' && juspayDeviceId == 'efdc4c74b471a45f7c738a2d1cbf44efe127917d21b74fc49d0581d01a0ed961'",
            r:{"pi_SBINB":0}
        },
        {
            c:"clientId == 'hungerbox_tablet_android'",
            r:{"fragment_otp":0}
        },
        {
            c:  "juspayDeviceId == 'fb94f90e98aed6ff5df8a043eedf8fd4291514ba1305d404a27e98f349380361' || " +
                "juspayDeviceId == 'a6db6f659acd3025d17c9ee0c46dc8c904addefa3dc6112cbf23ee75dcc7ab18' || " +
                "juspayDeviceId == 'bfb9df00334fe825eb2da7947948f54d205e00b5a405a1ee8532196a1ff69914'",
            r:{"pi_IRCTC_LOGIN":1}
        },
        {
            c:"clientId == 'ajio_android'",
            r:{"sms_consent":1}
        },
        {
            c:"clientId == 'onecard_android'",
            r:{"sms_consent":1}
        },
        {
            c: (function() {
                if (clientId == 'dreamplug_android' && appVersionBetween('1.0.8.7')) {
                    try {
                        var bundleParams = JSON.parse(JBridge.getSessionAttribute("bundleParams", "{}"));
                        var enable_ICICINB_MPIN = (bundleParams.enable_ICICINB_MPIN || "").toString();

                        if (enable_ICICINB_MPIN == "true" || enable_ICICINB_MPIN == "false") {
                            return enable_ICICINB_MPIN;
                        }
                    } catch(e) {
                        console.error(e);
                    }

                    return true;
                }

                return false;
            })(),
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: (function(){
                return (clientId == 'olacabs_android' || clientId == 'olacabs_sandbox_android')
                    && godelVersionBetween('1.0.6.35');
            })(),
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: "clientId == 'bbinstant_android'",
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: (function(){ // 1mg wasn't passing client_id. Checking package name instead
                return (PACKAGE_NAME == 'com.aranoah.healthkart.plus') && appVersionBetween('10.3.0');
            })(),
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: (function(){ //ixigo uses the same client_id across multiple apps. Check package name instead
                return (PACKAGE_NAME == 'com.ixigo.train.ixitrain') && appVersionBetween('4.3.5.9');
            })(),
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: (function() {
                return (PACKAGE_NAME == 'com.india.foodpanda.android') && appVersionBetween('2.9.0');
            })(),
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: "clientId == 'idea_android' && juspayDeviceId == '96df0543862109dc9ca33db24d2f0f66fe9f387d6bb71f8e8ac1814f6606e57a'",
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: "clientId == 'idea_android'",
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: "clientId == 'urbanclap_android'",
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: "godelVersion.indexOf('2.0') == 0",
            r: {"GLOBAL_MPIN":0, "ICICINB_MPIN":0}
        },
        {
            c: (function() {
                if (clientId == 'dreamplug_android' && godelVersion.indexOf('2.0') == 0) {
                    try {
//                        var bundleParams = JSON.parse(JBridge.getSessionAttribute("bundleParams", "{}"));
                        var enable_ICICINB_MPIN = (window.__payload.enable_ICICINB_MPIN).toString();

                        if (enable_ICICINB_MPIN == "true" || enable_ICICINB_MPIN == "false") {
                            return enable_ICICINB_MPIN;
                        }
                    } catch(e) {
                        console.error(e);
                    }

                    return true;
                }

                return false;
            })(),
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c: (function() {
                if (clientId == 'mmt_android' && godelRemotesVersion == '2.0rc1') {
                    try {
                        var enable_ICICINB_MPIN = (window.__payload.enable_ICICINB_MPIN).toString();

                        if (enable_ICICINB_MPIN == "true" || enable_ICICINB_MPIN == "false") {
                            return enable_ICICINB_MPIN;
                        }
                    } catch(e) {
                        console.error(e);
                    }

                    return true;
                }

                return false;
            })(),
            r:{"GLOBAL_MPIN":1, "ICICINB_MPIN":1}
        },
        {
            c:  "juspayDeviceId == 'f8d09dfbf457211e6f88e062fd5c22761450c0db2065a6510d0ef1b4a4560793' || " +
                "juspayDeviceId == '229c67cc2f5f7aa02878a4ae58ad4e31449cae30124647d38ed34a0f80b3cfc1' || " +
                "juspayDeviceId == '0ee2ba7c677acf6a8171401664ee457b792b336892797c915e917c65640a387a' || " +
                "juspayDeviceId == '372879fdf074a079130f9a71a35ecc30bbb0020a60efda2619aa4513de06c10d' || " +
                "juspayDeviceId == '96df0543862109dc9ca33db24d2f0f66fe9f387d6bb71f8e8ac1814f6606e57a' || " +
                "juspayDeviceId == '8b929ed99556955ae7c99605d0071340538b00125a0404e182d5069d1fc47597' || " +
                "juspayDeviceId == 'd8e3d0ecc19009f8f96687f350351333e1dedc488c445e4723197fd56bb5eb57' || " +
                "juspayDeviceId == 'dd487dfb62d9979580e9c50f3eaaaa3cb559b40dc520f8056f5d6c60d7d2df7d'",
            r:{"GLOBAL_MPIN":1,"ICICINB_MPIN":1}
        },
        {
            c: (function() {
                try {
                    return (JSON.parse(JBridge.getSessionInfo()).os == "ios").toString();
                } catch(e) {
                    return "false"
                }
            })(),
            r:{"GLOBAL_MPIN":0}
        },
        {
            c: godelVersion.indexOf('2.0') == 0,
            r: { "sms_consent": 1 }
        },
        {
            c:"clientId.indexOf('swiggy') != -1",
            r:{"sms_consent":0}
        },
        {
            c: "clientId == 'vodaidea_android'",
            r: {"feature_dynamic_sms_permission":0, "sms_consent":0}
        },
        {
            c: "juspayAndroidId == '069347966ad34659c184e4997872c574053ca69e24473c7f2069ade2e2aa5905'", // BB customer with rupay cyber source issue
            r: {"GODEL":0}
        }
    ]
)};


var globalConfigs = function() {return matchAll(
    {
        "default_global_configs":{
            c:ALL,
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000,
                "ON_EXCEPTION_GODEL_OFF_STICKINESS":24*60*60*1000
            }
        },
        "juspay_recharge":{
            c:"clientId == 'juspay_recharge'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "juspay_recharge_android":{
            c:"clientId == 'juspay_recharge_android'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "snapdeal_android":{
            c:"clientId == 'snapdeal_android'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "cleartrip_android":{
            c:"clientId == 'cleartrip_android'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "redbus_android":{
            c:"clientId == 'redbus_android'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "bms_android_debug":{
            c:"clientId == 'bms_android_debug'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "bms_android":{
            c:"clientId == 'bms_android'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "fcbrowser_android":{
            c:"clientId == 'fcbrowser_android'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "godel_integ_demo_fl":{
            c:"clientId == 'godel_integ_demo_fl'",
            r:{
                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        },
        "mobikwik_android":{
            c:"clientId == 'mobikwik_android'",
            r:{

                "REMOTE_ASSET_TTL_MILLISECONDS" : 30 * 1000,
                "CONFIG_TTL_MILLISECONDS" : 30 * 1000,
                "WEBLAB_PROBABLISTIC_STICKINESS_MILLISECONDS": 15 * 60 * 1000
            }
        }
    }
)};

var log_level_config = {
    log_level:0,
    white_list:[{"label":"GODEL_SWITCHING_OFF"},{"label":"config_event"}],
    black_list:[{"label":"not populating otp"}]
};

var logConfig = function() {return matchAll(
    {
        "default_logging_rules":{
            c:ALL,
            r:{
                "dev_options_enabled": {
                    "value":1,
                    "logHash":false
                }
            }
        },
        "juspay_recharge_android":{
            c:"clientId == 'juspay_recharge_android'",
            r:{
                "account_info": {
                    "value":1,
                    "logHash":true
                }
            }
        }
    }
)};
var paylater_params = {
    "max_attempts":5,
    "base_url":"https://paylater.juspay.in",
    "init_path":"/api/v1/orders",
    "checkout_path":"/api/v1/orders/:orderId/checkout",
    "jpl_flow_path":"/api/v1/orders",
    "batch_logs":1,
    "show_on_bp":0,
    "timeout":{
        "2G":{
            "init":10000,
            "checkout":7000
        },
        "3G":{
            "init":10000,
            "checkout":7000
        },
        "WIFI":{
            "init":10000,
            "checkout":7000
        },
        "OTHER":{
            "init":10000,
            "checkout":7000
        }
    },
    "exclude_urls":[
        ".*paylater-beta.juspay.in.*",
        ".*paylater.juspay.in.*"
    ]
}

var keyboardConfig = {
    "keyboards_enabled" : true,
    "keyboard_hide" : true,
    "generic_keyboard": true,
    "phone_keyboard" : true,
    "password_keyboard" : true,
    "numeric_keyboard" : true
}

//To use this, change js_alert_ovjects according example
// "js_alert_object": [{url:"rgex of url", alertText:"regex of text"}]
var suppressJsAlert= {
    "js_alert_object": []
}

var movementConfig={
    "min_locationt_time": 15000,
    "min_distance":50
}

//Example: {featureName:"fragment_config.otp_counter_delay", featureValue:"9999", valueType: "int"}
var piConfig={
    "HDFC": [],
    "ICICI": []
}

var webviewVersionsToOverride = ["50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90"];

var canExcuseSSLError = function() {
    var webviewVersion = "";
    try {
        var userAgent = navigator.userAgent;
        if(userAgent.indexOf("Chrome") != -1)
            webviewVersion = userAgent.substr(userAgent.indexOf("Chrome") + 7,2);
    } catch(error) {
        console.error("got this at ssl excuse check, ", error);
    }
    for(val in webviewVersionsToOverride) {
        if(webviewVersionsToOverride[val] == webviewVersion)
            return true;
    }
    return false;
}

var getMPINConfig = function() { return matchAll(
    {
        "default_mpin_rules":{
            c:ALL,
            r: {
                "open_app_in_new_process": 0,
                "ICICINB": {
                    "packageName": "com.csam.icici.bank.imobile",
                    "activityName": "com.csam.icici.bank.imobile.IMOBILE",
                    "mpinBroker": "in.juspay.mpin.services.JuspayIPC",
                    "supportedVersion": 165
                },
                "KOTAKNB": {
                    "packageName": "com.msf.kbankuat.mobile",
                    "activityName": "in.juspay.mpin.PayActivity",
                    "mpinBroker": "in.juspay.mpin.services.JuspayIPC",
                    "pollingUrl": "https://mb.uat.kotak.com/api/v1/api/v1/status",
                    "supportedVersion" : 61
                },
                "AXISNB": {
                    "packageName": "in.juspay.mpindemo",
                    "activityName": "in.juspay.mpindemo.DemoMPINActivity",
                    "mpinBroker": "in.juspay.mpin.services.JuspayIPC"
                }
            }
        }
    }

);}

var getDrawFromStatusBar = function() {
    if((clientId == 'bms_android' || clientId == 'idea_android' || clientId == 'vodaidea_android' || clientId == 'vodafone_android' || clientId == 'curefit_android') && godelVersion.indexOf('2.0') == 0)
    {
        return true;
    }
    return false;
}

var getConfig = function() {
    var configList = [
        { "version": version },
        { "otp_rules": otpRules },
        { "status_rules": statusRules},
        { "remotes": assetConfig() },
        { "weblab": weblabConfig() },
        { "fragment_config": fragmentConfig },
        { "pg_server_redirect": pg_server_redirect },
        { "pg_custom_branding_dialog": pg_custom_branding_dialog },
        { "sms_template_config": sms_template_config },
        { "reloadable_config": reloadableUrls },
        { "non_reloadable_config": nonReloadableUrls},
        { "reload_network_based_config": reload_network_based_config },
        { "cacheable_config": cacheableUrls },
        { "cacheable_config_v1": cacheableUrlsV1 },
        { "cacheable_config_v2": cacheableUrlsV2 },
        { "logging_config":logConfig()},
        { "logging_level_config":log_level_config},
        { "paylater_params":paylater_params},
        { "exclude_url_patterns":excludeUrlPatterns},
        { "waiting_dialog_config":waitingDialogConfig},
        { "keyboard_config":keyboardConfig},
        { "log_push_config":logPushConfig},
        { "suppress_js_alert":suppressJsAlert},
        {"movement_config":movementConfig},
        {"config_lose_conditions": configLoseConditions},
        {"pi_config":piConfig},
        { "can_excuse_ssl_error": canExcuseSSLError()},
        { "certificates_location": certificatesLocation()},
        { "permission_uber_config": permissionUberConfig},
        { "progress_bar_config": progressBarConfig},
        { "trust_manager_config": trustManagerConfig},
        { "mpin_config": getMPINConfig() },
        { "rupay_specific_domains": rupay_specific_domains},
        { "bank_js_urls": bank_js_urls},
        { "draw_from_status_bar": getDrawFromStatusBar()},
        globalConfigs()
    ];
    var config = {};
    for(var i = 0; i < configList.length; i++) {
        addToFirst(config, configList[i]);
    }

    return config;
}

var getConfigString = function() {
    var configString = getConfig();
    try{
        configString["pi_config"]["AXISNB"] = [{featureName:"weblab.auto_submit_otp_enabled", featureValue:"0", valueType:"int"}];
        configString["pi_config"]["ICICIDC"] = [{featureName:"weblab.auto_submit_otp_enabled", featureValue:"0", valueType:"int"}];
        if(configString["fragment_config"]) {
            if(clientId == 'oxigen_android') {
                configString["fragment_config"]["waiting_dialog_processing"] = 'Breathe OXIGEN while\nwe process your request';
                configString["fragment_config"]["waiting_dialog_still_processing"] = 'Breathe OXIGEN while\nwe process your request';
            }
        }
//        if(clientId =='fcbrowser_android'){
//            configString["exclude_url_patterns"] =  configString["exclude_url_patterns"].concat(freeCharge_specific_exclude_urls);
//        }
        if(clientId == 'mmt_android') {
             configString["waiting_dialog_config"]["custom_dialog_domains"] = ["makemytrip","migs.mastercard.com.au", "securepayments.fssnet.co.in", "www.ccavenue.com", "payseal.icicibank.com", "pguat.paytm.com", "secure.payu.in",
                            "mpi.onlinesbi.com", "securepg.fssnet.co.in", "www.billdesk.com", "www.wibmo.com", "ps.gcsip.com"];
            for(var i=0;i<configString["otp_rules"].length;i++){
                configString["otp_rules"][i]["otp_timeout"] = 30;
                configString["fragment_config"]["otp_counter_delay"] = "0";
                var bank = configString["otp_rules"][i]["bank"];
                if(bank =='AXISNB' || bank =='KOTAKDC' || bank =='KOTAKCC' || bank =='KOTAK' || bank =='DENADC' || bank =='YESNB' || bank =='BOINB'){
                    configString["otp_rules"][i]["otp_timeout"] = 45;
                }
                if(bank =='AXIS') {
                    configString["otp_rules"][i]["otp_timeout"] = 60;
                }
                if(bank =='IRCTC_LOGIN') {
                    configString["otp_rules"][i]["otp_timeout"] = 120;
                }
            }
        }
        if(clientId == 'myairtel_android') {
             configString["waiting_dialog_config"]["custom_dialog_domains"] = ["airtel","bsbportal"];
             configString["fragment_config"]["otp_auto_submit_time"] = "3000";
             configString["exclude_url_patterns"] =  configString["exclude_url_patterns"].concat(airtel_specific_exclude_urls);
        }

        if(clientId == 'mmt_android' || clientId == 'juspay_recharge_android') {
             configString["fragment_config"]["otp_auto_submit_time"] = "10000";
             configString["pi_config"]["IRCTC_LOGIN"] = [{featureName:"weblab.fragment_otp", featureValue:"0", valueType:"int"}];
             configString["pi_config"]["IRCTC_LOGIN"] =
             [{featureName:"fragment_config.otp_waiting_to_detect", featureValue:"Waiting <b>%ss</b> to detect password", valueType:"string"},
             {featureName:"fragment_config.otp_submitting_x_seconds", featureValue:"Entering password in <b>%ss</b>", valueType:"string"},
             {featureName:"fragment_config.otp_heading_text", featureValue:"Waiting for IRCTC Password", valueType:"string"},
             {featureName:"fragment_config.otp_status_text", featureValue:"Could not detect IRCTC Password", valueType:"string"},
             {featureName:"fragment_config.regenerate_otp_text", featureValue:"Regenerate Password", valueType:"string"},
             {featureName:"fragment_config.otp_approve_button_text", featureValue:"APPROVE PASSWORD", valueType:"string"},
             {featureName:"fragment_config.otp_received_message", featureValue:"PASSWORD RECEIVED", valueType:"string"},
             {featureName:"fragment_config.otp_sub_message", featureValue:"Note: Password will be auto-submitted", valueType:"string"}];
        }
        if(clientId == 'jiomoney_android') {
             configString["fragment_config"]["otp_auto_submit_time"] = "3000";
        }
//        if(clientId == 'Myntra') {
//             configString["waiting_dialog_config"]["custom_dialog_domains"] = ["myntra"];
//             configString["logging_level_config"]["log_level"] = 2;
//        }
        if(clientId == 'Myntra' || clientId == 'mmt_android' || clientId == 'myairtel_android' || clientId == 'yatra_android' || clientId == 'swiggy_android'
            || clientId == 'fcbrowser_android' || clientId == 'bms_android_debug' || clientId == 'freshmenu_android' || clientId == 'oxigen_android'){
             configString["fragment_config"]["sms_polling_interval"] = '2000';
             configString["weblab"]["sms_polling_interval"] = '2000';
        }
        if(clientId == 'yatra_android'){
            configString["fragment_config"]["otp_auto_submit_time"] = "7000";
        }
        if(clientId == 'bms_android_debug'){
            configString["pi_config"]["AMEXCC"] = [{featureName:"weblab.auto_submit_otp_enabled", featureValue:"0", valueType:"int"}];
            configString["fragment_config"]["otp_auto_submit_time"] = "3000";
        }
        if(clientId == 'PhonePe_android'){
            configString["fragment_config"]["otp_auto_submit_time"] = "0001";
        }
        if(clientId == 'myairtel_android'){
            configString["fragment_config"]["otp_auto_submit_time"] = "0001";
        }
        if(clientId == 'dream11_android'){
            configString["fragment_config"]["otp_auto_submit_time"] = "0001";
        }
        if(clientId == 'zomato_order_android'){
            configString["fragment_config"]["otp_auto_submit_time"] = "0001";
        }

        if(clientId == 'mp_flipkart') {
            configString["pi_config"]["SBIDC"] = [{featureName:"weblab.auto_submit_otp_enabled", featureValue:"0", valueType:"int"}];
            configString["fragment_config"]["otp_counter_delay"] = "5";
            for(var i=0;i<configString["otp_rules"].length;i++){
                var bank = configString["otp_rules"][i]["bank"];
                if(bank == 'CITI') {
                    configString["otp_rules"][i]["otp_timeout"] = 30;
                }
            }

        }
        if(clientId == 'freshmenu_android' || clientId == 'myairtel_android') {
            for(var i=0;i<configString["otp_rules"].length;i++){
                var bank = configString["otp_rules"][i]["bank"];
                if(bank == 'HDFC' || bank == 'ICICICC' || bank == 'ICICIDC') {
                    configString["otp_rules"][i]["otp_timeout"] = 30;
                }
                if(bank == 'SBICC') {
                    configString["otp_rules"][i]["otp_timeout"] = 45;
                }
            }
        }
        if(clientId == 'firstcry_ios') {
            configString["waiting_dialog_config"]["custom_dialog_domains"] = ["firstcry.com"];
        }
        if(clientId == 'jio_android') {
            configString["waiting_dialog_config"]["custom_dialog_domains"] = ["jio.com", "jioconnect.com"];
        }
        if(clientId == 'dreamplug_android') {
            configString["fragment_config"]["otp_auto_submit_time"] = "3000";
            configString["fragment_config"]["long_press_to_stop_auto_submission"] = "false"
        }
        if(clientId.toLowerCase().indexOf('_ios') >= 0) {
            configString["pi_config"]["SBINB"] = [{featureName:"weblab.fragment_otp", featureValue:"0", valueType:"int"}];
            configString["pi_config"]["SBICC"] = [{featureName:"weblab.fragment_otp", featureValue:"0", valueType:"int"}];
            configString["pi_config"]["SBIDC"] = [{featureName:"weblab.fragment_otp", featureValue:"0", valueType:"int"}];
            configString["pi_config"]["SCB"] = [{featureName:"weblab.fragment_otp", featureValue:"0", valueType:"int"}];
        }
        if (clientId === "flipkart_android") {
            configString["waiting_dialog_config"]["messages"] = [
                { message: "Initiating Payment", duration: 3000 },
                { message: "Processing your payment", duration: 3000 },
                { message: "Connecting to Bank", duration: -1 }
            ];
        }
        try {
            var godelRC = JBridge.getResourceByName("godel_remotes_version");
            if (godelRC.indexOf("2.0") != -1) {
                configString["waiting_dialog_config"]["exclude_domains"] = configString["waiting_dialog_config"]["exclude_domains"].replace("redbus|","");
            }
        } catch (e) {
        // Ignored 0.6
        }
        console.log("JuspayTrackEvent:"+JSON.stringify(conditionForLogs));
    } catch(error){
        console.log("Error in modifying config string",error);
    }
    return JSON.stringify(configString);
}

try {
    window.getConfigString = getConfigString;
} catch(err) {

}
