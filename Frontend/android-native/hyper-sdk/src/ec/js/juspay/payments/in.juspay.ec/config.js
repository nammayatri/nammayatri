window.config_version = "1.0.11";

window.ec_headless_config_version = window.config_version;
var merchantId = "";
  try {
    var variables = JBridge.getConfigVariables();
    console.log("config: " + variables);
    eval(variables);
    merchantId = (clientId && clientId != "null") ? clientId.split("_")[0] : "";
  } catch(err) {
    console.error(err)
  }

var clientId = "";
    try {
        clientId = window.__payload.client_id;
    } catch(err) {
     console.error(err)
    }

var forceGPAYIntentClientIds = ["netmeds_android"]

var forceGPAYIntent = function() {
        if(forceGPAYIntentClientIds.indexOf(clientId) != -1)  return true;
        else return false;
}

var gpayAllowedMethodsFunc = function() {
    try
    {
        var clientId = window.__payload.payload.clientId;
        switch(clientId) {
            case "picasso_android":
                console.log(clientId , " config");
                return ["UPI","CARD"];
            default:
                console.log(clientId , " config");
                return ["UPI"];
        }
    } catch(e) {
       console.log(e," Returning UPI");
       return ["UPI"];
    }
}

var getPollingConfig = function(){
    try {
        var clientId = window.__payload.payload.clientId;
        switch(clientId){
            case "zoomin_android":
            case "ixigoprod_android":
            case "1mg_android":
            case "Symbol_android":
            case "udit_juspay_android":
            case "vies_ola_android":
            case "tendercuts_android":
            case "zoomin_test_android":
            case "Gaana_android":
            case "gaana_android":
            case "zoomin_ios":
            case "ixigoprod_ios":
            case "1mg_ios":
            case "Symbol_ios":
            case "udit_juspay_ios":
            case "vies_ola_ios":
            case "tendercuts_ios":
            case "zoomin_test_ios":
            case "Gaana_ios":
            case "gaana_ios":
            case "com.swiggy_ios":
            case "curefit_android":
            case "curefit_ios":
            case "trulymadly_android":
                return {duration: 300000, repeat: 5000};
            default:
                return {duration:3000, repeat:5000};
        }
    } catch(e){
        console.log("Returning default");
        return {duration:3000, repeat:5000};
    }
}

var defaultConfig = function() {
    return {
        payment_methods: [ 'CARD' ,'NB','UPI','CASH','WALLET'],
        wallet_config : [
            {
                wallet : "PHONEPE",
                nature : "SDK",
                method : ""
            },
            {
                wallet : "PAYTM",
                nature : "DIRECT",
                method : ""
            },
            {
                wallet : "AMAZONPAY",
                nature : "SDK",
                method : "TOKENIZED"
            }
           ],
        otp_page_enabled : false,
        upi_sdk_enabled : true,
        force_gpay_intent : forceGPAYIntent(),
        mandate_vpas : ["@upi","@paytm"], //List of vpa handles eligible for mandate collect txn
        gpayAllowedMethods : gpayAllowedMethodsFunc(), //This function returns allowedMethods CARD or UPI based on clientId
        obfuscate_details : {
            merchant_specific : {
                //Get these keys from dashboard
            },
            common : {key : "82DD480598A5402C9982652FBE88D59A", version : "2020-07-21"} ,
            merchant_input : {version : "2017-04-26"}
        },
        encode_switch : "obfuscate", //Can be obfuscate or plain
        encode_type : "common", //common or merchant_specific
        pollingConfig : getPollingConfig(),
        allowed_apps_for_merchant : {
            "Symbol" : ["cred"],
            "zoomin_test" : ["cred"],
            "zoomin":["cred"],
            "ixigoprod":["cred"],
            "1mg":["cred"],
            "udit_juspay": ["cred"],
            "vies_ola" :["cred"],
            "tendercuts": ["cred"],
            "Gaana" :["cred"],
            "gaana" : ["cred"]
        },
        package_name_for_app2app_communication : {
            cred_sandbox : "com.dreamplug.androidapp.dev",
            cred : "com.dreamplug.androidapp"
        },
        shouldAllowEligibilityInInitiate : ["Symbol","zoomin_test","zoomin","udit_juspay"],
        keys:{
            "sandbox":{
                credKey : "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQC8+jHCldrR/ETIjXhQDjgMz4+iJ+tpiy6sXg1SGW5VrrWIU/B6Vn06tOWRWNKQEnAKczG0bEsrMvhmzW7uVkHMuj9hI/gNxigso6hUjpbKw7OCdQYhPxUWVZuxQdf8g0LOu/N0swZZO+ldiNiaj+WQH284qnVLl5Nz0LeDntYptQIDAQAB"
            },
            "prod":{
                credKey: "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzHHLFnOqJ2LjHcAI1WvF6mpNv2zeokY0DbX46W/P4Rc6AVBg5CUcLPYjLed32qtQaHK6PhPsTwtTN9QLczhN6X9mp+1GK5Yxm/1DOC2PsvSkF3ZacSFYx1KHDVRjG3dPPj+fe3EBXkktOG/yVRbo0LlIaVk+6mEfI9Tn0+9SBJ+1y7y4EBGc+Wd/A5tN5dKJrzsuoQYuI9tKEtAIGsiHp93vYIKr2bDTV2Kl+iqzcQAJIqOOxVTahDGv9qDurSrm6wrs3XCoeCSWGiLB/MKqYdPzgYD2uctPkGAxmRCKshMXlIveRfskMTa562K72d5ZKsc1ehkEnQ8M8yzQ4f4QMQIDAQAB"
            }
        },
        intentPolling : {duration:12000, repeat:3000},
        collectPolling: {duration:300000, repeat:5000}
    }
}

window.getConfig = defaultConfig;
