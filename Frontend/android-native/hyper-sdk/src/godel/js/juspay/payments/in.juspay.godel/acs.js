if(typeof window.acsAlreadyCalled == "undefined"){
window.acsVersion="1.0rc2_74";
/* DO NOT TOUCH THE ABOVE LINE*/

/*
 * JUSPAY CONFIDENTIAL
 * __________________
 *
 * [2012] - [2017] JusPay Technologies Pvt Ltd
 * All Rights Reserved.
 *
 * NOTICE:  All information contained herein is, and remains
 * the property of JusPay Technologies Pvt Ltd. The intellectual
 * and technical concepts contained
 * herein are proprietary to JusPay Techologies Pvt Ltd
 * and may be covered by Indian Patents Office and Foreign Patents,
 * patents in process, and are protected by trade secret or copyright law.
 * Dissemination of this information or reproduction of this material
 * is strictly forbidden unless prior written permission is obtained
 * from JusPay Technologies Pvt Ltd.
 */
if (!String.prototype.endsWith) {
    String.prototype.endsWith = function(search, this_len) {
        if (typeof this_len === "undefined" || this_len > this.length) {
            this_len = this.length;
        }
        return this.substring(this_len - search.length, this_len) === search;
    };
}

// Below is poly fill for btoa & atob, which supports unicode characters
if (window) {
    	(function() {
		if (window.btoa) {
			const previousBtoa = window.btoa;
			function insideBtoa(str) {
				return encodeURIComponent(str).replace(
					/%([0-9A-F]{2})/g,
					function toSolidBytes(match, p1) {
						return String.fromCharCode('0x' + p1);
					},
				);
			}
			window.btoa = function(input) {
				return previousBtoa(insideBtoa(input));
			};
		}
		if (window.atob) {
			const previousAtob = window.atob;
			function outsideAtob(str) {
				return decodeURIComponent(
					str
						.split('')
						.map(function(c) {
							return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
						})
						.join(''),
				);
			}
			window.atob = function(input) {
				return outsideAtob(previousAtob(input));
			};
		}
	})();
}

function fnValidator (methodName) {
    return typeof methodName == 'function';
}

var getScreenWidth = function () {
    if(document && document.body && typeof document.body.getBoundingClientRect == "function" && document.body.getBoundingClientRect().width)
        return document.body.getBoundingClientRect().width;
    return window.screen.width;
}

if(typeof juspayContext === "undefined"){
    window.juspayContext = {};
}

window.isPostMessageEnabled = false;
window.iframe = {};

var space= document.createElement('div');
space.style.height="200px";
space.id="juspaySpaceDiv";
if(document.querySelector('body'))
     document.querySelector('body').appendChild(space);

(function(){
    window.onAcsFunctionCalled = function(fn, args, callBackFunction) {
//        console.log("Method: " + fnName + " with args: " + args + " and callback: " + callback + " is being called!");
        GK[fn](args, callBackFunction);
    }

    window.__FN_INDEX = 0;
    window.__PROXY_FN = [];
    var map = function(fn) {
        if(typeof window.__FN_INDEX !== 'undefined' && window.__FN_INDEX !== null) {
            proxyFnName = 'F' + window.__FN_INDEX;
            window.__PROXY_FN[proxyFnName] = fn;
            window.__FN_INDEX++;
            return proxyFnName;
        } else {
            return null;
        }
    }

    JuspayCaller = function(method){
        this.method = method;
        this.arguments = [];
    };

    var functionNames = [
    "showToast", "createUber","dismissUber","showRetryOptions","resetOtpFragmentToWaitingState",
    "showSlidingUber","updateState","scrollTo","setBank","enableRegenerateOTP","disableRegenerateOTP",
    "showReloadDialog","goForward","canGoForward","canGoBack",
    "stopLoading","loadUrl","posturl","dismissReloadDialog",
    "setNetworkListener","resetSmsReadTime",
    "shoutOut","trackJsError","setPollingForSmsEnabled","trackFallback",
    "setLastFourDigitsOfCard","requestKeyboardShow","requestNumericKeyboardShow",
    "requestPasswordKeyboardShow","requestKeyboardHide","disableOTPOption","disablePasswordOption",
    "setCardBrand","setStatusTextOnEvent","setExcludeURLs","showNetbankingDefaultFragment",
    "showPasswordHelperFragment","showGenericPasswordFragment","showACSOptions",
    "showOtpFragment","removeFragment","showManualOtpFragment","setPasswordValue","hidePasswordHelper",
    "hideAssistanceFragment","showWaitingFragment","storeLastChosenAccount","addDataToSharedPrefs",
    "trackUserError","trackDomCheckFailure","trackJsInfo","trackAcsVersion","trackJsOnPageLoad","trackPageView",
    "trackEvent","trackEvent2","clearCacheIfExist","changeNextActionText","setNumberOfExtraParameterAllowed",
    "requestPhoneKeyboardShow","setSessionAttribute","removeAttribute","resetSession","resetBank",
    "noPageMatched","pageMatched","domCheckFailure","fetchSmsFromInbox","transactionCompleted","transactionStarted",
    "hideBlur","setSmsBackReadTime","setJavascriptToOpenWindows","getUrl","getPastUrls",
    "getPastUrlsSize","getRemarksForBank","onIframeFinished","hideWaitingFragment","setupMPIN","saveMPINPreference",
    "startMPIN", "setShouldAutoSubmitOtp","trackAction","trackLifeCycle","trackAuthValue"];

    window.Gatekeeper = {};
    Gatekeeper["invoke"] = function(fnName,args,callback){
        // console.log("Calling Interface method: " + fnName + " with args: " + btoa(args) + " and callback: " + callback);
        ACSGatekeeper.invoke(fnName, btoa(args), callback);
    }

    Gatekeeper["onCustomerIdSubmit"] = function() {
        // ignored
    }
    Gatekeeper["showNetbankingCustomerIdFragment"] = function() {
        // ignored
    }
    Gatekeeper["setCustomerIdCheckBoxState"] = function () {
        // ignored
    }

    Gatekeeper.getSessionAttribute = function (key) {
        if (fnValidator(ACSGatekeeper.getSessionAttribute)) {
            return ACSGatekeeper.getSessionAttribute(key);
        } else {
            return ACSGatekeeper.invokeInDUIGatekeeper("getSessionAttribute", key);
        }
    }

    Gatekeeper.getDataFromSharedPrefs = function (key) {
        if (fnValidator(ACSGatekeeper.getDataFromSharedPrefs)) {
            return ACSGatekeeper.getDataFromSharedPrefs(key);
        } else {
            return ACSGatekeeper.invokeInDUIGatekeeper("getDataFromSharedPrefs", key);
        }
    }

    Gatekeeper.getGodelInitParams = function() {
        try {
            if (fnValidator(ACSGatekeeper.getSessionInfo)) {
                var sessionInfo = ACSGatekeeper.getSessionInfo();
                if (sessionInfo === "__failed") {
                    sessionInfo = "{}";
                }
                return sessionInfo;
            } else {
                return ACSGatekeeper.invokeInDUIGatekeeper("getSessionInfo");
            }
        } catch (err) {
            console.log(err);
            return "{}";
        }
    }

    Gatekeeper.getCurrentUrl = function() {
        return Gatekeeper.getSessionAttribute("currentUrl");
    }

    Gatekeeper.getLastFourDigitsOfCard = function() {
        return Gatekeeper.getSessionAttribute("lastFourDigitsOfCard");
    }

    Gatekeeper.getLastChosenAccount = function() {
        return Gatekeeper.getSessionAttribute(GK.getBank()+"_lastChosenAccount");
    }

    Gatekeeper.getRemarksForBank = function() {
        return Gatekeeper.getSessionAttribute("remarks");
    }

    Gatekeeper.getCurrentBank = function() {
        return Gatekeeper.getSessionAttribute("bank");
    }

    Gatekeeper.getCurrentFragment = function() {
        return Gatekeeper.getSessionAttribute("currentFragment");
    }

    Gatekeeper.getGodelVersion = function () {
        if (fnValidator(ACSGatekeeper.getResourceByName)) {
            return ACSGatekeeper.getResourceByName("godel_version");
        } else {
            return ACSGatekeeper.invokeInDUIGatekeeper("getResourceByName", "godel_version");
        }
    }

    Gatekeeper.isFirstPage = function(){
        return Gatekeeper.getSessionAttribute("isFirstPage");
    }

    Gatekeeper.loadFirstPage = function(){
        return Gatekeeper.runInJuspayWebView("loadFirstPage");
    }

    Gatekeeper.goBack = function(){
        return Gatekeeper.runInJuspayWebView("goBack");
    }

    Gatekeeper.reload = function(){
        return Gatekeeper.runInJuspayWebView("reload");
    }

    Gatekeeper.getBankCustomerId = function(){
        return undefined;
    }

    Gatekeeper.isOnline = function () {
        if (fnValidator(ACSGatekeeper.isOnline)) {
            return ACSGatekeeper.isOnline();
        } else {
            return ACSGatekeeper.invokeInDUIGatekeeper("isOnline");
        }
    }

    Gatekeeper.getConfig = function(){
        return Gatekeeper.getSessionAttribute("config");
    }

    Gatekeeper.runInJuspayWebView = function(command){
        return Gatekeeper.invoke("runInJuspayWebView",command,null);
    }

    Gatekeeper.showCancelTransactionDialog = function() {
        if(window.isPostMessageEnabled && typeof window.iframe.postMessage == "function"){
            window.iframe.postMessage(GK.localHelper("showCancelTransactionDialog"),"*");
        }
        Gatekeeper.invoke("showCancelTransactionDialog",JSON.stringify([]),null);
    }

    Gatekeeper.isEnabled = function(key) {
        try {
            return typeof juspayContext.web_lab_rules[key] == "undefined" || juspayContext.web_lab_rules[key] == 1 || ( parseInt(juspayContext.web_lab_rules[key] * 100) > parseInt(Math.random() * 100) )
        } catch(err) {
            console.error(err);
            return false
        }

    }

    var createWrapper = function(fnName, callee) {
        Gatekeeper[fnName] = (function(){
          var functionName = fnName;
          var f = function() {
             var obj = new JuspayCaller(functionName);
             totalArgs = arguments.length;
             for(var j = 0; j < totalArgs - 1; j++){
                 obj["arguments"].push("" + arguments[j]);
             }
             if(totalArgs > 0 && typeof arguments[totalArgs-1] == "function"){
                return Gatekeeper[callee](functionName, JSON.stringify(obj["arguments"]), map(arguments[totalArgs-1]));
             } else if(totalArgs > 0) {
                obj["arguments"].push("" + arguments[totalArgs - 1]);
             }
             return Gatekeeper[callee](functionName, JSON.stringify(obj["arguments"]), null);
          }
          return f
        })();
    }

    for(var i=0;i<functionNames.length;i++){
       createWrapper(functionNames[i], "invoke");
    }
})();

var jp_trackevent2 = Gatekeeper.getDataFromSharedPrefs("jp_ihavetrackevent2"); // this decides whether new track method is available in godel or not

window.trackEventv2_0 = function(category,action,label,value, newLabel, key){
    if(jp_trackevent2 == "true"){
        Gatekeeper.trackEvent2(category,action,label,value, newLabel, key);
    }else{
        Gatekeeper.trackEvent(category,action,label,value);
    }
}

window.trackJsInfo = function(label,value, newLabel, key){
    if(jp_trackevent2 == "true"){
        Gatekeeper.trackEvent2("acs","info",label,value, newLabel, key);
    }else{
        Gatekeeper.trackJsInfo(label,value);
    }
}

window.trackEventv2_0default = function(category,action,label,value){
    if(jp_trackevent2){
        Gatekeeper.trackEvent2(category,action,label,value, label, "data");
    }else{
        Gatekeeper.trackEvent(category,action,label,value);
    }
}

if(typeof juspayContext === "undefined"){
    window.juspayContext = {};
}
window.GK = new function() { //need to be called only once, so constructor must be used for creating object and assiging to window.GK variable
    var bank = "UNKNOWN";
    var page = "UNKNOWN";
    var uberEventName="UNKNOWN";
    var uberCallback;

    this.localHelper = function(val){
        return  {
                    "from": "GK",
                    "method": val
                };
    }

    this.localHelperWithArgs = function(method,data){
        return { "from":"GK",
                 "method":method,
                 "data": data
               }
    }

    this.smartReload = function(){
        var url_count= Gatekeeper.getPastUrlsSize();
        if(window.isPostMessageEnabled && typeof window.iframe.postMessage == "function"){
            window.iframe.postMessage(GK.localHelper("smartReload"),"*");
        }
        var config = Gatekeeper.getConfig();
        try {
            config = JSON.parse(config);
        } catch(err) {
            Gatekeeper.trackJsError(err.stack.toString())
            config = {};
        }

        if(window.location.href.indexOf('https://cardsecurity.enstage.com/ACSWeb/')>-1){
            Gatekeeper.goBack();
            alert('Going back');
        }
        if(Gatekeeper.isFirstPage()){
            Gatekeeper.loadFirstPage();
        }else if(config && config.non_reloadable_config && Array.isArray(config.non_reloadable_config.urls)){
            var blacklisted = config.non_reloadable_config.urls;
            var isUrlBlackListed = false;
            var url = window.location.href;
            for(var i = 0; i < blacklisted.length; i++) {
                if(new RegExp(blacklisted[i]).test(url)) {
                    isUrlBlackListed = true;
                    break;
                }
            }

            if(!isUrlBlackListed)
                Gatekeeper.reload();
        }
    };

    this.setBank = function(bankString) {
        bank = bankString;
    };

    this.getBank = function() {
        return bank;
    };

    this.onUberDismiss = function(props) {

    };

    this.getAuthCountForPwd = function(bankAuth){
        var lastChosenAuthOption = Gatekeeper.getDataFromSharedPrefs(bankAuth);
        if(lastChosenAuthOption.indexOf("PWD")<0){
            lastChosenAuthOption = "PWD_0";
        }
        var authCount = parseInt(lastChosenAuthOption.split('_')[1]);
        authCount++;
        return authCount;
    };

    this.getAuthCountForOtp = function(bankAuth){
        var lastChosenAuthOption = Gatekeeper.getDataFromSharedPrefs(bankAuth);
        if(lastChosenAuthOption.indexOf("OTP")<0){
            lastChosenAuthOption = "OTP_0";
        }
        var authCount = parseInt(lastChosenAuthOption.split('_')[1]);
        authCount++;
        return authCount;
    };

    this.setUberEvent = function(eventName,props,data) {
        uberCallback=null;
        if(props) {
            try {
                if(typeof(props.onPageLoad)==="undefined") {
                    onPageLoad = "destroy";
                }
                props = JSON.stringify(props);
                Gatekeeper.createUber(eventName,props,data);
                uberEventName=eventName;
            } catch(err) {
                window.trackEventv2_0("acs","error","uber_props_error","Function: setUberEvent and Error is "+String(err), "uber_props", "error");
            }
        }
    };

    this.setUberEventWithCallback= function(eventName,callback,props,data) {
        uberCallback=callback;
        if(props) {
            try {
                if(typeof(props.onPageLoad)==="undefined") {
                    onPageLoad = "destroy";
                }
                props = JSON.stringify(props);
                Gatekeeper.createUber(eventName,props,data);
                uberEventName=eventName;
            } catch(err) {
                window.trackEventv2_0("acs","error","uber_props_error","Function: setUberEventWithCallback and Error is "+String(err), "uber_props", "error");
            }
        }
    };

    this.resultFromUber = function(result) {
        if(uberCallback) {
            uberCallback(result);
            uberCallback=null;
        }
    };

    this.clickSubmitButton = function() {
        try {
            if (typeof(page["clickSubmitButton"])=="function") {
                page["clickSubmitButton"]();
                Gatekeeper.requestKeyboardHide();
                Gatekeeper.setPasswordValue("");
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function : clickSubmitButton and Error is "+String(err), "godel", "js_error");
        }
    };

    this.showPassword = function() {
        try {
            if (typeof(page["showPassword"])=="function") {
                page["showPassword"]();
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: showPassword and Error is "+String(err), "godel", "js_error");
        }
    };

    //check for password stage
    this.reachPasswordStage = function() {
        try {
            if (typeof(page["reachPasswordStage"])=="function") {
               page["reachPasswordStage"]();
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: reachPasswordStage and Error is "+String(err), "godel", "js_error");
        }
    };

    this.setPage = function(bankPage) {
        page = bankPage;
    };

    this.reachOtpStage = function() {
        try {
            if (typeof(page["reachOtpStage"]) == "function") {
               page["reachOtpStage"]();
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: reachOtpStage and Error is "+String(err), "godel", "js_error");
        }
    };

    this.regenerateOTP = function(){
        try {
            if(window.isPostMessageEnabled && typeof window.iframe.postMessage == "function"){
                 window.iframe.postMessage(GK.localHelper("regenerateOTP"),"*");
            }
            if (typeof(page["regenerateOtp"]) == "function") {
                page["regenerateOtp"]();
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: rgenerateOtp and Error is "+String(err), "godel", "js_error");
        }
    };

    this.injectOtpAndSubmit = function(otp) {
        try {
            if(window.isPostMessageEnabled && typeof window.iframe.postMessage == "function"){
                window.iframe.postMessage(GK.localHelperWithArgs("injectOtpAndSubmit",otp),"*");
            }else{
                __juspay.delayMe(function(){
                    if(typeof(page["submitOtp"]) == "function") {
                        page["submitOtp"](otp);
                    }
                });
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: injectOtpAndSubmit and Error is "+String(err), "godel", "js_error");
        }
    };

     this.showOtpFragmentOnManualRequest = function(){
         try{
            if(typeof(page["showOtpFragmentOnManualRequest"]) == "function") {
                page["showOtpFragmentOnManualRequest"]();
            }
         }catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: showOtpFragmentOnManualRequest and Error is "+String(err), "godel", "js_error");
         }
     };

    this.executePageFunction = function(resp){
        try {
            resp = JSON.parse(resp);
            if (typeof(page[resp.fnName]) == "function") {
                page[resp.fnName](resp.params);
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: "+fnName+" and Error is "+String(err), "godel", "js_error");
        }
    }

    this.uberData = function(data) {
        try {
            if(typeof(page["onDataFromUber"]) == "function") {
                page["onDataFromUber"](data);
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: uberData and Error is "+String(err), "godel", "js_error");
        }
    };

    this.nextAction = function(){
        try {
            if (typeof(page["nextAction"]) == "function") {
                page["nextAction"]();
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: nextAction and Error is "+ String(err), "godel", "js_error");
        }
    };

    this.backButtonPressed = function(shouldShowBackPressDialog) {
        try {
            if(shouldShowBackPressDialog == "true") {
                if(typeof(Gatekeeper.isShowingUber) == "function" && (Gatekeeper.isShowingUber("dialog") || Gatekeeper.isShowingUber("sliding_uber"))) {
                    window.trackEventv2_0default("acs","info","back_button","sending back_button_pressed to uber");
                    Gatekeeper.sendDataToUber("try { GK.backButtonPressed(); } catch(err) { console.log(err); Gatekeeper.destroyUber(); Gatekeeper.showCancelTransactionDialog(); }")
                    return;
                }
                if (typeof(page["backButtonPressed"]) == "function") {
                    page["backButtonPressed"]();
                    return;
                }
            }
            GK.showCancelTransactionDialog();
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: backButtonPressed and Error is "+ String(err), "godel", "js_error");
            GK.showCancelTransactionDialog();
        }
    };

    this.otpDetected = function(otp) {
        try {
            if (typeof(page["otpDetected"]) == "function" && (typeof otp != "undefined")) {
                page["otpDetected"](otp);
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: nextAction and Error is "+ String(err), "godel", "js_error");
        }
    }

    this.showFeedBackUber = function(){
        if(__uber.isUberEnabled("FEEDBACK_UBER")) {
            GK.setUberEventWithCallback("FEEDBACK_UBER", function(result) {
                if(result){
                    result = JSON.parse(result);
                    if(result.response == "ok"){
                        window.trackEventv2_0("acs","info","FEEDBACK_UBER",(result.user_feedback)?(JSON.stringify(result.user_feedback)):(JSON.stringify([])), "FEEDBACK","UBER");
                        GK.cancelTransactionAfterLoading();
                    }else if(result.response == "onBackPressed"){
                        window.trackEventv2_0("acs","info","FEEDBACK_UBER","back_pressed", "FEEDBACK","UBER");
                        Gatekeeper.showCancelTransactionDialog();
                    }
                    else if(result.response == "cancel" || result.response == "closed"){
                        window.trackEventv2_0("acs","info","FEEDBACK_UBER",result.response, "FEEDBACK","UBER");
                    }
                }
            },{type:"sliding_uber",showOnLoad:true,height:GK.getScreenPercent(83.33),gravity:3,onPageLoad:"dismiss",background:"translucent"},null);
        } else {
            Gatekeeper.showCancelTransactionDialog();
        }
    }

    this.isNetBanking = function(){
        var bank = GK.getBank();
        if(bank.slice(bank.length-2) == "NB")
            return true;
        return false;
    }

    this.showCancelTransactionDialog = function() {
        if((GK.isNetBanking())&&(__uber.isUberEnabled("FEEDBACK_UBER"))&&(__uber.initSessionCounterIncremental("FEEDBACK_UBER")<2)) {
            GK.showFeedBackUber();
        } else {
            Gatekeeper.showCancelTransactionDialog();
        }
    }

    this.cancelTransactionAfterLoading = function(){
        setTimeout( function(){
            Gatekeeper.cancelCurrentTransaction();
        },500);
    }

    this.setNetworkType = function(nwType){
        try {
            networkType = nwType;
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: setNetworkType and Error is "+ String(err), "godel", "js_error");
        }
    };

    this.fragmentInitialized = function(fragmentName) {
        try {
             currentFragment = fragmentName;
             window.trackEventv2_0("acs", "info", "fragment_initialized", fragmentName, "fragment", "initialized");
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: fragmentInitialized and Error is "+ String(err), "godel", "js_error");
        }
    };

    this.appStateReceived = function(appState) {
        try {
             currentAppState = appState;
             window.trackEventv2_0("acs", "info", "app_state", appState,"app","state");
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: fragmentInitialized and Error is "+ String(err), "godel", "js_error");
        }
    };
    this.setGodelVersion = function(version){
        try {
            godelVersion = version;
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: setGodelVersion and Error is "+ String(err), "godel", "js_error");
        }
    };
    this.setGodelRemotesVersion = function(version){
        try {
            godelRemotesVersion = version;
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: setGodelRemotesVersion and Error is "+ String(err), "godel", "js_error");
        }
    };

    this.shouldShowRetryOptions = function(){
        try {
            Gatekeeper.showRetryOptions();
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: shouldShowRetryOptions and Error is "+ String(err), "godel", "js_error");
        }
    };

    this.enterOtpManually = function(){
        try {
            if(window.isPostMessageEnabled && typeof window.iframe.postMessage == "function"){
                window.iframe.postMessage(GK.localHelper("enterOtpManually"),"*");
            }
            if (typeof(page["enterOtpManually"]) == "function") {
                page["enterOtpManually"]();
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: enterOtpManually and Error is "+ String(err), "godel", "js_error");
        }
    };

    this.getPercent = function(px){
        return (window.innerHeight*px/100.00);
    };

    this.getScreenPercent = function(px){
        return (window.screen.height * px/100.00);
    };

    this.onJsAlert = function(message){
        var bank_name = GK.getBank();
        message = message.toLowerCase();
        if(message){
            if (message.indexOf("payment is successful") > -1 || message.indexOf("hot payment successful") > -1 ||
                message.indexOf("payment successful") > -1 || message.indexOf("transaction successful") > -1||
                 message.indexOf("transaction processed successfully") > -1){
                __juspay.setPaymentStatus("BANK","SUCCESS");
            }
        }
    };
    this.receiveMessage = function(event){
        var dataReceived = event.data;
        if(dataReceived.from == "GK"){
            dataReceived.data ? GK[dataReceived.method](dataReceived.data) : GK[dataReceived.method]();
        }else if(Acculynk && typeof Acculynk.processMessage == "function"){
            Acculynk.processMessage(event);
        }else{
        }
    };
}();

var isObject = function(object){
    return (typeof object == "object");
}

var collectHtmlDump = function(){
    var domCollectionHtml = {};
    domCollectionHtml.htmlDump = [];
    try{
        if(document && __juspay.isMobUiEnabled("MOBUI_HTMLDUMP")){
            var docEl = document.documentElement;
            if(docEl){
                var htmlString ={};
                htmlString.htmldump= docEl.innerHTML;
                htmlString.url = window.location.href;
                domCollectionHtml.htmlDump.push(htmlString);
                var stringHtml = JSON.stringify(domCollectionHtml);
                window.trackEventv2_0("acs","info","dom_check_html1",stringHtml.substring(0,((stringHtml.length)/2)), "dom_check", "html1");
                window.trackEventv2_0("acs","info","dom_check_html2",stringHtml.substring(((stringHtml.length)/2)+1,stringHtml.length), "dom_check", "html2");
            }
        }
    }catch(err){
        window.trackEventv2_0("acs","error","dumping_error","Error while dumping html of the page","dumping","error");
    }
};

var showUpdateWebviewUber = function() {
    Gatekeeper.requestKeyboardHide();
    var userAgent = navigator.userAgent;
    if(typeof userAgent != "undefined" && navigator.userAgent.indexOf("; wv") != -1 && userAgent.indexOf("Chrome") != -1) {
        var webviewVersion = userAgent.substr(userAgent.indexOf("Chrome") + 7,2);
        if((webviewVersion < 57) && __uber.isUberEnabled("UPDATE_WEBVIEW") && __uber.initSessionCounterIncremental("updateWebviewUberShown")==1) {
            setTimeout(function() {
                GK.setUberEventWithCallback("UPDATE_WEBVIEW", function(result) {
                    window.trackEventv2_0("acs","info","UPDATE_WEBVIEW","uber_shown","UPDATE","WEBVIEW");
                    if( result == "ok") {
                        event_value = result+"_clicked";
                        window.trackEventv2_0("acs","info","UPDATE_WEBVIEW",event_value,"UPDATE","WEBVIEW");
                        Gatekeeper.dismissUber();
                        window.open("market://details?id=com.google.android.webview");

                    } else if( result == "cancel" || result == "closed") {
                        event_value = result+"_clicked";
                        window.trackEventv2_0("acs","info","UPDATE_WEBVIEW",event_value,"UPDATE","WEBVIEW");
                        Gatekeeper.dismissUber();
                    }
                },{type:"dialog",showOnLoad:true,height:GK.getScreenPercent(43),width:-2},null);
            },500);
        }
    }
}



var domFailureOccured = false;
var collectDataForDomFailure = function(){
    var totalElementsCount = 0;
    var textBoxArray = document.querySelectorAll("input[type=text]");
    var passwordBoxArray = document.querySelectorAll("input[type=password]");
    var radioButtonsArray = document.querySelectorAll("input[type=radio]");
    var submitButtonsArray = document.querySelectorAll("input[type=submit]");
    var telElementsArray = document.querySelectorAll("input[type=tel]");
    var imageArray = document.querySelectorAll('img');
    var linksArray = document.querySelectorAll('a');
    var domCollectionJson = {};


    collectHtmlDump();
    //To gather information about all text boxes
    domCollectionJson.textBox = [];
    for(var textCount=0;textCount<textBoxArray.length;textCount++){
        var details = {};
        details.name = textBoxArray[textCount].name;
        details.id = textBoxArray[textCount].id;
        details.className = textBoxArray[textCount].className;
        details.type = textBoxArray[textCount].type;
        domCollectionJson.textBox.push(details);
        totalElementsCount++;
    }

    domCollectionJson.link = [];
    for(var linkCount=0;linkCount<linksArray.length;linkCount++){
        var detailsLink = {};
        detailsLink.id = linksArray[linkCount].id;
        detailsLink.name = linksArray[linkCount].name;
        detailsLink.href = linksArray[linkCount].href;
        detailsLink.innerText = linksArray[linkCount].innerText;
        detailsLink.type = linksArray[linkCount].type;
        detailsLink.clickLink = linksArray[linkCount].onclick + " ";
        domCollectionJson.link.push(detailsLink);
    }

    //To gather information about all password type textBox
    domCollectionJson.passwordBox = [];
    for(var passCount=0;passCount<passwordBoxArray.length;passCount++){
        var detailsPassword = {};
        detailsPassword.name = passwordBoxArray[passCount].name;
        detailsPassword.id = passwordBoxArray[passCount].id;
        detailsPassword.type = passwordBoxArray[passCount].type;
        detailsPassword.className = passwordBoxArray[passCount].className;
        domCollectionJson.passwordBox.push(detailsPassword);
        totalElementsCount++;
    }

    //To gather information about all Radio Buttons
    domCollectionJson.radioButton = [];
    for(var radioCount=0;radioCount<radioButtonsArray.length;radioCount++){
        var detailsRadio = {};
        detailsRadio.name = radioButtonsArray[radioCount].name;
        detailsRadio.id = radioButtonsArray[radioCount].id;
        detailsRadio.value = radioButtonsArray[radioCount].value;
        detailsRadio.type = radioButtonsArray[radioCount].type;
        detailsRadio.className = radioButtonsArray[radioCount].className;
        domCollectionJson.radioButton.push(detailsRadio);
        totalElementsCount++;
    }

    //To gather information about all Submit Buttons
    domCollectionJson.submitButton = [];
    for(var submitCount=0;submitCount<submitButtonsArray.length;submitCount++){
        var detailsSubmit = {};
        detailsSubmit.name = submitButtonsArray[submitCount].name;
        detailsSubmit.id = submitButtonsArray[submitCount].id;
        detailsSubmit.value = submitButtonsArray[submitCount].value;
        detailsSubmit.backgroundImageLink = window.getComputedStyle(submitButtonsArray[submitCount],false).backgroundImage;
        detailsSubmit.className = submitButtonsArray[submitCount].className;
        detailsSubmit.type = submitButtonsArray[submitCount].type;
        domCollectionJson.submitButton.push(detailsSubmit);
        totalElementsCount++;
    }

    //To gather information about all images
    domCollectionJson.images = [];
    for(var imageCount=0;imageCount<imageArray.length;imageCount++){
        var detailsImages = {};
        detailsImages.src = imageArray[imageCount].src;
        detailsImages.name = imageArray[imageCount].name;
        detailsImages.type = imageArray[imageCount].type;
        domCollectionJson.images.push(detailsImages);
    }

    domCollectionJson.telArray = [];
    for(var elemCount=0;elemCount<telElementsArray.length;elemCount++){
        var details = {};
        details.name = telElementsArray[elemCount].name;
        details.id = telElementsArray[elemCount].id;
        details.className = telElementsArray[elemCount].className;
        details.type = telElementsArray[elemCount].type;
        domCollectionJson.telArray.push(details);
        totalElementsCount++;
    }

    if(totalElementsCount>0){
        window.trackEventv2_0("acs","info","dom_check_failure",JSON.stringify(domCollectionJson), "dom_check", "failure");
        Gatekeeper.trackAction("acs","info","page",JSON.stringify({is_dom_check_successful : false, url : window.location.href }));
    }
};

var endPages = function(bank_name) {
    var endPagesList = [
        {
            "bank":"SBINB",
            "links": [
                { "link": /faq/i,
                  "ubers": "FAQ"
                },
                { "link": /phishing/i,
                  "ubers": "PHISHING"
                },
                { "link": /Privacy_Statement/i,
                  "ubers": "PRIVACY"
                },
                { "link": /disclosure/i,
                  "ubers": "DISCLOSURE"
                },
                { "link": /mgmt/i,
                  "ubers": "MGMGT"
                },
                { "link": /security/i,
                  "ubers": "SECURITY"
                },
                { "link": /use/i,
                  "ubers": "USE"
                }
            ]
        }
    ];
    try{
        for(var i=0; i<endPagesList.length; i++) {
            if(endPagesList[i].bank === bank_name) {
                var anchors_array = document.querySelectorAll('a');
                var links_array = endPagesList[i].links;

                for(k = 0; k < anchors_array.length; k++)
                {
                    for(j = 0; j < links_array.length; j++)
                    {
                        if(anchors_array[k].getAttribute('onclick')){
                            if(links_array[j].link.test(anchors_array[k].getAttribute('onclick'))) {
                                var uber_name = bank_name + "_" + links_array[j].ubers;
                                anchors_array[k].setAttribute('href', 'javascript:GK.setUberEvent("' + uber_name + "\",{type:\"sliding_uber\",showOnLoad:true},null);");
                                anchors_array[k].removeAttribute("onclick");
                                break;
                            }
                        }

                    }
                }

            }
        }
    }catch(err){
        window.trackEventv2_0("acs","error","acs_js_error","Function: endPages and Error is "+String(err),"acs","js_error");
    }
};

window.__uber = new function(){ //call to be done only once
    this.initSessionCounterIncremental = function(name){
         if(Gatekeeper.getSessionAttribute(name)===""){
            Gatekeeper.setSessionAttribute(name,"1");
            return 1;
        }else{
            var counter = parseInt(Gatekeeper.getSessionAttribute(name));
            counter++;
            Gatekeeper.setSessionAttribute(name,String(counter));
            return counter;
        }
    };

    this.setWebLabRules=function(webLabRules) {
        try {
            juspayContext['web_lab_rules']=JSON.parse(webLabRules);
            window.trackEventv2_0default("config", "info", "weblab_rules", webLabRules);
            __juspay.publish("juspay_context_changed");
        } catch(err) {
            window.trackEventv2_0("acs","error","acs_js_error","Function: setWebLabRules,error parsing weblab. Error is "+String(err),"acs","js_error");
        }
    };

    // TODO: Can be refactored
    this.isUberEnabled=function(feature) {
        try {
            if( !__uber.checkIfUberFeatureEnabled("uber") || !__uber.checkIfUberFeatureEnabled("uber_"+GK.getBank()) || !__uber.checkIfUberFeatureEnabled(feature) )
            {
                window.trackEventv2_0("acs","weblab","uber_disabled","uber disabled for "+feature, "uber", "disabled");
                console.log(" Weblab is off for " + feature + ". Disabling JB..");
                return false;
            }
            else
                return true;
        } catch(err) {
            window.trackEventv2_0("acs","error","acs_js_error","Function: isUberEnabled and Error is "+String(err),"acs","js_error");
            return false;
        }
    };

    this.checkIfUberFeatureEnabled=function(feature) {
            if ( !__juspay.has(juspayContext.web_lab_rules,feature) ) {
                console.log("Weblab Json do not have feature: " + feature);
                return true;
            }
            try {
                return ( juspayContext.web_lab_rules[feature] == 1 );
            } catch(err) {
                console.log("Exception while checking for the value in config for: " + feature+" Error is "+String(err));
                return false;
            }
    };

    this.createConfirmDialog = function(text,ok,cancel){
        try{
            window.trackEventv2_0("uber","info","confirm_dialog","Popup dialog appeared giving password-otp switch", "confirm", "dialog");
            var response = confirm(text);
            if(response){
                if(typeof ok === "function"){
                    ok();
                }
            }else{
                if(typeof cancel === "function")
                {
                    cancel();
                }
            }
        }catch(err){
            window.trackEventv2_0("uber","error","uber_js_error","Function: createConfirmDialog and Error is "+String(err), "uber", "js_error");
        }
    };
    this.redirect = function(url){
        window.location.href=url;
    };
    this.findOnPage = function(stringList){
        try{
            var found = false;
            __juspay.setOnEach(stringList,function(string){
                if(window.find(string,true,true,true)){
                    found = true;
                    window.getSelection().collapseToEnd();
                }
            });
            return found;
        }catch(err){
            window.trackEventv2_0("uber","error","uber_js_error","Function: findOnPage and Error is "+String(err), "uber", "js_error");
        }
    };
}();

window.__juspay = new function() {

    var subscribers=[];
    var otpCollection = "__godel_otp_submissions";
    var defaultTTL = 1000*60*60*24*30;
    this.trackWebLabRules = function() {
        window.trackEventv2_0default("config", "info", "weblab_rules", JSON.stringify(juspayContext['web_lab_rules']));
        this.publish("juspay_context_changed");
    };
    this.showOtpFragmentWrapper = function(element){

    	if (/iP(hone|od|ad)/.test(navigator.platform)) {

            var osVersion = (navigator.appVersion).match(/OS (\d+)_(\d+)_?(\d+)?/);
            var godelVersion = Gatekeeper.getGodelVersion();

            if (!godelVersion) {
                return;
            }

            var versionArr = godelVersion.split(".");
            //Checking for godel version >= 0.7.6
            if (versionArr && (versionArr[0] >= 1 || versionArr[1] >= 8 || (versionArr.length > 2 && versionArr[1] >= 7 && versionArr[2] >= 6))) {
                if (parseInt(osVersion[1]) < 12) {
                    return;
                }
            } else {
                return;
            }
        }

        Gatekeeper.showOtpFragment();
        if(element){
            element.addEventListener("focus", function () {
               if(element != "undefined" && element) {
                    var otpValue = element.value;
                      __juspay.delayMe(function(){
                          Gatekeeper.setPasswordValue(otpValue);
                      },200);
               }
            }, false);
            element.addEventListener("input", function (e){
               var otpValue = element.value;
               if(otpValue != "undefined" && otpValue != null) {
                  Gatekeeper.setPasswordValue(otpValue);
               }
            }, false);

            //Don't remove this
            if (typeof juspayContext != "undefined" && typeof juspayContext["platform"] != "undefined" && juspayContext["platform"] == "ios") {
                element.addEventListener("keyup", function (e){
                   var otpValue = element.value;
                   if(otpValue != "undefined" && otpValue != null) {
                      Gatekeeper.setPasswordValue(otpValue);
                   }
                }, false);
            }
        }
    };

    this.resetField = function(element){
        if(element){
            element.addEventListener("click", function (){
                Gatekeeper.setPasswordValue("");
           }, false);
        }
    };

    this.publish=function(eventName) {
        try {
            var event ={ type : eventName };
            for (var subscriber in subscribers) {
                if(subscribers[subscriber].eventName==eventName)
                    subscribers[subscriber]["element"].notify(event);
            }
        } catch(err) {
            window.trackEventv2_0("acs","error","acs_js_error","Function: publish and Error is "+String(err),"acs","js_error");
        }
    };

    this.subscribe=function(eventName,element) {
        var subscriber=new Object();
        subscriber.eventName=eventName;
        subscriber.element=element;
        subscribers.push(subscriber);
    };

    this.setCss = function(selectorStr, cssStr) {
        try{
            var css = document.createElement("style");
            css.type = "text/css";
            css.innerHTML = cssStr;
            document.querySelector(selectorStr).appendChild(css);
        }catch(err){
            window.trackEventv2_0("acs","error","godel_js_error","Function: setCss and Error is "+String(err), "godel", "js_error");
        }
    };

    this.has=function has(object, key) {
        return object ? hasOwnProperty.call(object, key) : false;
    };

    this.removeElements = function(attrs) {
        var i=0;
        while(i<attrs.length) {
            if(attrs[i] && attrs[i].parentElement) {
                attrs[i].parentElement.removeChild(attrs[i]);
            }
            i++;
        }
    };

    this.modifyUI = function ( attrs) {
        for(var key in attrs) {
            if(key=="align_center") {
                var i=0;
                while (i<attrs[key].length) {
                    attrs[key][i++].align="center";
                }
            } else if(key=="add_break_before"){
                var i=0;
                while (i<attrs[key].length) {
                    var td_space = attrs[key][i].parentElement.insertBefore(document.createElement('td'),attrs[key][i]);
                    td_space.style.height = ((window.innerHeight*(1/32))*1.14).toString()+"px";
                    i++;
                }
            } else if(key=="display_none") {
                var i=0;
                while (i<attrs[key].length) {
                    attrs[key][i++].style.display="none";
                }
            } else if(key=="swap_elements"){
                var i=0;
                while(i<attrs[key].length && attrs[key].length%2===0) {
                    var parent2 = attrs[key][i+1].parentElement;
                    var parent1 = attrs[key][i].parentElement;
                    var ele1 =  attrs[key][i].cloneNode(true);
                    var ele2 =  attrs[key][i+1].cloneNode(true);
                    parent2.replaceChild(ele1,attrs[key][i+1]);
                    parent1.replaceChild(ele2,attrs[key][i]);
                    i=i+2;
                }
            } else if(key=="style_height") {
                for(var key2 in attrs[key]) {
                    var i=0,height=key2;
                    if(key2.indexOf("other")>-1) {
                       height = ((window.innerHeight) * attrs[key][key2][0]).toString()+"px";
                        i=1;
                    }
                    while(i<attrs[key][key2].length) {
                        attrs[key][key2][i].style.height = height; i++;
                    }
                }
            } else  if(key=="align_left") {
                var i=0;
                while (i<attrs[key].length) {
                    attrs[key][i++].align="left";
                }
            }else if(key=="align_right") {
                var i=0;
                while (i<attrs[key].length) {
                    attrs[key][i++].align="right";
                }
            } else if(key=="display_inline_block") {
                var i=0;
                while (i<attrs[key].length) {
                    attrs[key][i++].style.display="inline-block";
                }
            }  else if(key=="position_fixed") {
                var i=0;
                while (i<attrs[key].length) {
                    attrs[key][i++].style.position="fixed";
                }
            } else if(key=="style_width") {
                for(var key2 in attrs[key]) {
                    var i=0,width=key2;
                    if(key2.indexOf("other")>-1) {
                        width = ((window.innerWidth) * attrs[key][key2][0]).toString()+"px";
                        i=1;
                    }
                    while(i<attrs[key][key2].length) {
                        attrs[key][key2][i].style.width = width ; i++;
                    }
                }
            } else if(key=="border") {
                for(var key2 in attrs[key]) {
                    if(key2.indexOf("border_top")>-1) {
                        var i=0;
                        while(i<attrs[key][key2].length) {
                            attrs[key][key2][i].style.borderTop ="1px solid #ddd";
                            i++;
                        }
                    } else if(key2.indexOf("border_bottom")>-1) {
                        var i=0;
                        while(i<attrs[key][key2].length) {
                            attrs[key][key2][i].style.borderBottom ="1px solid #ddd";
                            i++;
                        }
                    }
                }
            } else if(key=="font_size"){
                for(var key2 in attrs[key]) {
                    if(key2.indexOf("other")>-1) {
                        var i=1;
                        while(i<attrs[key][key2].length) {
                            attrs[key][key2][i].style.fontSize = (attrs[key][key2][0]).toString()+"px";
                            i++;
                        }
                    }
                }
            }
        }
    };

    this.CatchPageModifyError = function(feature,err,page) {
        if (!Date.now) {
            Date.now = function() { return new Date().getTime(); };
        }
        var current_time = Math.floor(Date.now() / 1000);
        Gatekeeper.addDataToSharedPrefs(feature+"_ERROR_TIME",current_time.toString());
        window.trackEventv2_0("acs","error","modify_page_error","MOBUI_ERROR:" + String(err), "modify_page", "error");
    };

    this.setOnEach = function(elements, modifierFunction) {
        var what = Object.prototype.toString;
        if(what.call(elements) == "[object NodeList]" || what.call(elements) == "[object Array]") {
            for(var i = 0; i < elements.length; i++) {
                modifierFunction(elements[i]);
            }
        }
        else {
            modifierFunction(elements);
        }
    };

    this.merge = function(arr1, arr2) {
        var arr3 = [];
        for(var i = 0;i < arr1.length;i++) {
            arr3.push(arr1[i]);
        }
        for(i = 0;i < arr2.length;i++) {
            arr3.push(arr2[i]);
        }
        return arr3;
    };

    this.trackAuthMethod = function(status) {
        Gatekeeper.trackAuthValue(status);
    };

    this.trackPageStatus = function(status) {
        window.trackJsInfo("pageStatus", status, "page", "status");
    };

    this.trackUnblocker = function(status) {
        window.trackJsInfo("unblocker", status, "unblocker", "data");
    };

    this.trackPageClicks = function() {

        function isImage(element) {
            return element instanceof HTMLImageElement;
        }

        function isButton(element) {
            return (element.getAttribute("type") == "submit" || element.getAttribute("type") == "button");
        }
        function isLink(element) {
            return (element.getAttribute("href"));
        }
        function notifyClick(type, idType, idValue) {
            if(type == "BUTTON"){
                window.trackJsInfo("BUTTON_CLICK", idValue, "click", "button");
            } else if(type == "IMAGE"){
                window.trackJsInfo("IMG_CLICK", idValue, "click", "img");
            } else if(type == "LINK") {
                window.trackJsInfo("LINK_CLICK", idValue, "click", "link");
            }
            console.log(type + " clicked with " + idType + " " + idValue);
        }

        function logClick(element) {
            if(isImage(element)) {
                notifyClick("IMAGE", "src", element.getAttribute('src'));
            } else if (isButton(element)) {
                notifyClick("BUTTON", "name", element.getAttribute('name'));
            }else if(isLink(element)) {
                notifyClick("LINK", "href", element.getAttribute('href'));
            }
            else {
                console.log("unidentified object was clicked: " + element);
            }
        }

        var list = document.querySelectorAll("input[type=submit], input[type=button], img, a");

        for(var i=0; i<list.length; i++) {
            list[i].addEventListener("click", function(element){logClick(element.srcElement);});
        }
    };

    this.trackJsOnPageLoad = function() {
        Gatekeeper.trackJsOnPageLoad();
    };

    this.trackInvalidScenarios = function(searchText, logText){
        var wrongAuthCred = window.find(searchText, true ,false, true);
        if(wrongAuthCred){
            window.getSelection().collapseToEnd();
            Gatekeeper.trackUserError(logText);
        }
    };

    this.keepCheckingDomFor = function(searchText, callBackFunction, customWindow) {
        if(typeof customWindow == "undefined"){
            pageWindow = window;
        } else {
            pageWindow = customWindow;  //pages having DOM inside frames
        }

        var interval = setInterval(function() {
            if(pageWindow.find(searchText, true, false, true)) {
                if(typeof(callBackFunction) == "function"){
                window.getSelection().collapseToEnd();
                    callBackFunction();
                }
                clearInterval(interval);
            }
        },1000);

    };

    // Hack: Use with caution
    // The only way to cancel useless timers set by PGs
    this.clearAllTimersHack = function() {
        var x = setTimeout(function() {} ,10);

        //If the page has too many timers.. just give up
        if(x > 100) return false;

        for(i=0; i < x; i++) {
            clearTimeout(i);
        }
        return true;
    };

    this.disableExtrasForTextBox = function(element) {
        this.setOnEach(element, function(elt) {
            try{
                if(elt) {
                    elt.setAttribute("autocomplete", "off");
                    elt.setAttribute("autocorrect", "off");
                    elt.setAttribute("spellcheck", "false");
                    elt.setAttribute("autocapitalize", "off");
                }
            }catch(err){
                window.trackEventv2_0("acs","error","godel_js_error","Function: disableExtrasForTextBox and Error is "+String(err), "godel", "js_error");
            }
        });
    };

    this.scrollToElement = function(element) {
        //var position = element.getBoundingClientRect();
        //var x = window.pageXOffset + position.left;
        //var y = window.pageYOffset + position.top;

        if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios"){
            return;
        }

        var interval = __juspay.delayMe(function() {
            Element.prototype.documentOffsetTop = function () {
                return this.offsetTop + ( this.offsetParent ? this.offsetParent.documentOffsetTop() : 0 );
            };
            Element.prototype.documentOffsetLeft = function () {
                return this.offsetLeft + ( this.offsetParent ? this.offsetParent.documentOffsetLeft() : 0 );
            };
            if(typeof element.documentOffsetTop === "function" && typeof element.documentOffsetLeft === "function") {
                var top = (element.documentOffsetTop() - (window.innerHeight / 2 )) | 0;
                var left = (element.documentOffsetLeft() - (window.innerWidth / 2 )) | 0;
                window.scrollTo(left, top);
            }
            window.trackEventv2_0default("acs", "info", "scrolledTo", "<"+element.nodeName+" id="+element.id+" name="+element.name+" class="+element.className+" >");

        },500);
    };

    this.scrollOnFocusInputElements = function() {
        var inputElements = document.querySelectorAll("input");
        if(inputElements) {
            __juspay.setOnEach(inputElements,function(element) {
                element.addEventListener("focus", function (){
                    window.trackEventv2_0default("acs", "info", "focused", "<"+element.nodeName+" id="+element.id+" name="+element.name+" class="+element.className+" >");
                    __juspay.scrollToElement(element);
                }, false);
            });
        }
    };

    this.logKeyPressEvents = function(){
        var inputElements = document.querySelectorAll("input");
        if(inputElements) {
            __juspay.setOnEach(inputElements,function(element) {
                element.addEventListener("focus", function (){
                    Gatekeeper.onElementFocus(element.id);
                    element.addEventListener("keyup", function(evt){
                        Gatekeeper.addKeyOnPressed(true, element.id || element.name);
                    }, false);
                }, false);
                element.addEventListener("blur", function(){
                    Gatekeeper.addKeyOnPressed(false,element.id);
                }, false);
            });
        }
    };

    this.clickOnElement = function(element) {
        if(element){
            var clickEvent = document.createEvent('MouseEvent');
            clickEvent.initEvent('click', true, true);
            element.dispatchEvent(clickEvent);
            window.trackEventv2_0("acs","info","juspay_click","Element clicked by Juspay", "juspay", "click");
        }else{
            window.trackEventv2_0("acs","error","null_element_click","Trying to click an element which doesnot exist on page", "null_element", "click");
        }
    };

    this.delayMe = function(executable, time) {
        try {
            if(!time){
                time = 100;
            }
            window.setTimeout(executable, time);
        } catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function: delayMe and Error is "+String(err), "godel", "js_error");
        }
    };

    this.showOtpFragment = function(otpFragmentOptions) {
        if(otpFragmentOptions["initState"] == "hidden") {
            window.trackEventv2_0("acs","info","user_preference","OTP fragment minimized due to past failures to detect", "user", "preference");
            if(typeof otpFragmentOptions["element"] !== "undefined"){
                __juspay.showOtpFragmentWrapper(otpFragmentOptions["element"]);
            }else{
                __juspay.showOtpFragmentWrapper();
            }
            Gatekeeper.hideAssistanceFragment();
        }
        else {
            if(typeof otpFragmentOptions["element"] !== "undefined"){
                __juspay.showOtpFragmentWrapper(otpFragmentOptions["element"]);
            }else{
                __juspay.showOtpFragmentWrapper();
            }
        }
    }

   // Adding record for OTP status.To be called from submitOtp function, status will be set according to current fragment there.
    this.addOtpDecisionRecord = function(bank, payment_method, salt) {
        if(window.localStorage!==undefined) {
            time = new Date().getTime();
            var data;
            if(localStorage.getItem(otpCollection) != null && juspayContext) {
                data = localStorage.getItem(otpCollection);
                data = JSON.parse(data);
                // TTL value to ensure that this doesn't become permanent
                var ttl = parseInt(juspayContext.web_lab_rules["otpRecordTimeout"])
                if(!isNaN(ttl)) {
                    data["records"] = data["records"].filter(function(el){ return el.time + ttl  >= time  });
                    len = data["records"].length;
                    data["records"][len] =  {"bank" : bank, "payment_method": payment_method, "salt": salt, "status": "manual", "time":time};
                }else {
                    ttl = defaultTTL;
                    window.trackEventv2_0default("acs","error","ConfigTTL","Unable to retreive TTL from config");
                    data["records"] = data["records"].filter(function(el){ return el.time + ttl  >= time  });
                    len = data["records"].length;
                    data["records"][len] =  {"bank" : bank, "payment_method": payment_method, "salt": salt, "status": "manual", "time":time};
                }
            }else {
                 data = { "records": [
                    {"bank" : bank, "payment_method": payment_method, "salt": salt, "status": "manual", "time":time}
                ]};
            }
            localStorage.setItem(otpCollection, JSON.stringify(data));
        }else {
            window.trackEventv2_0default("acs","error","localStorage","Local Storage is turned off.");
        }
    }

    this.changeOtpDecisionRecordToAuto = function() {
        try {
            var data = localStorage.getItem(otpCollection);
            data = JSON.parse(data);
            len = data["records"].length;
            if(len>0) {
                if(data["records"][len-1].status == "manual") {
                    data["records"][len-1].status = "auto";
                    localStorage.setItem(otpCollection, JSON.stringify(data))
                }
                window.trackEventv2_0("acs","error","local_storage_update","UPDATED local storage. length is :" +len, "localStorage", "update");
            }
            else {
                window.trackEventv2_0default("acs","error","localStorage","No OTP related records exist. Aborting Update.");
            }
        }catch(err) {
            window.trackEventv2_0("acs","error","godel_js_error","Function : changeOtpDecisionRecordToAuto and Error is "+String(err), "godel", "js_error");
        }
    }


    this.isMobUiEnabled = function(feature) {
        try {
            if(feature.indexOf("MOBUI")!=-1){
                window.trackEventv2_0("Weblab","Info","GLOBAL_MOBUI",Gatekeeper.isEnabled("GLOBAL_MOBUI"), "global", "mobui");
                if(!Gatekeeper.isEnabled("GLOBAL_MOBUI")){
                    return false;
                }
            } else if(feature.indexOf("AUTO_CLICK")!=-1){
                 window.trackEventv2_0("Weblab","Info","GLOBAL_AUTO_CLICK",Gatekeeper.isEnabled("GLOBAL_AUTO_CLICK"), "global", "auto_click");
                 if(!Gatekeeper.isEnabled("GLOBAL_AUTO_CLICK")){
                     return false;
                 }
            }else if(feature.indexOf("AUTO_CHOOSE")!=-1){
                  window.trackEventv2_0("Weblab","Info","GLOBAL_AUTO_CHOOSE",Gatekeeper.isEnabled("GLOBAL_AUTO_CHOOSE"), "global", "auto_choose");
                  if(!Gatekeeper.isEnabled("GLOBAL_AUTO_CHOOSE")){
                      return false;
                  }
             }
            var featureEnabled = Gatekeeper.isEnabled(feature);
            window.trackEventv2_0default("Weblab","Info",feature,featureEnabled);
            if(Gatekeeper.getDataFromSharedPrefs(feature+"_ERROR_TIME")) {
                var timestamp = parseInt(Gatekeeper.getDataFromSharedPrefs(feature+"_ERROR_TIME"));
            }
            if (!Date.now) {
                Date.now = function() { return new Date().getTime(); }
            }
            var current_time = Math.floor(Date.now() / 1000);
            if(timestamp && (current_time - timestamp < 24*60*60)) {
                console.log("Disabling feature "+feature+" due to past errors");
                return false;
            }
            return featureEnabled;
        } catch(err) {
            console.log("Exception while checking for the value in config for: " + feature+" Error is "+String(err));
            return false;
        }
    }

    this.isFeatureEnabled = function(feature) {
        try{
            if(typeof Gatekeeper !=="undefined" && typeof Gatekeeper.isEnabled == "function"){
                return Gatekeeper.isEnabled(feature);
            }
        } catch(err) {
            console.log("Exception while checking if feature is enabled for : " + feature+" Error is "+String(err));
            return false;
        }
        return false;
    }

    this.isRupayEnabled = function(bank){
        return __juspay.isFeatureEnabled("GLOBAL_RUPAY") && __juspay.isFeatureEnabled("RUPAY_"+bank);
    }

    // Call this function before showOtpFragment call
    this.retreiveOtpDecision = function(bank, payment_method, salt) {
        if(window.localStorage!==undefined) {
            var data;
            var time = new Date().getTime();
            if(localStorage.getItem(otpCollection) != null & juspayContext) {
                data = localStorage.getItem(otpCollection);
                data = JSON.parse(data);
                var ttl = parseInt(juspayContext.web_lab_rules["otpRecordTimeout"]);
                if(!isNaN(ttl)) {
                    data["records"] = data["records"].filter(function(el) {
                        return el.time + ttl >= time && el.bank == bank &&
                        el.payment_method == payment_method && el.salt == salt;
                    });
                    len = data["records"].length;
                    return (len < 2 || data["records"][len-2].status == "auto") ? 1 : 0
                } else {
                    ttl = defaultTTL;
                    data["records"] = data["records"].filter(function(el) {
                        return el.time + ttl >= time && el.bank == bank &&
                        el.payment_method == payment_method && el.salt == salt;
                    });
                    len = data["records"].length;
                    return (len < 2 || data["records"][len-2].status == "auto") ? 1 : 0
                }
            }
        }
        return 1;
    }
    this.setPaymentStatus = function(payment_medium, status){
        try{
            var pStatus = "UNKNOWN";
            var key = "payment_status";
            //Add missing keywords to value list in small case
            var psValueMap = [
                {
                    "ps":"FAILURE",
                    "value":["n","fail","false","cancel","authorized","reject","unable","invalid","decline","gw00458","no ","insufficient","error","not","stop"]
                }, {
                    "ps":"SUCCESS",
                    "value":["100","success", "true","y"]
                }, {
                    "ps":"UNKNOWN",
                    "value":["na","pending"]
                }
            ];
            if(payment_medium && status){
                if(payment_medium == "BANK"){
                    key = "bank_payment_status";
                } else if(payment_medium == "AGGREGATOR"){
                    key = "aggregator_payment_status";
                } else if(payment_medium == "PAYMENT_GATEWAY"){
                    key = "pg_payment_status";
                } else {
                    window.trackEventv2_0("acs","error","Error setting inferred_payment_status","Wrong input for setPaymentStatus", "error", "setting inferred_payment_status");
                }
                if(status && psValueMap){
                    status = status.toLowerCase();
                    for(var i=0; i<psValueMap.length; i++) {
                        var val = psValueMap[i].value;
                        for(var j=0; j < val.length; j++) {
                            if(status.indexOf(val[j]) > -1) {
                                status = psValueMap[i].ps;
                                i = psValueMap.length;
                                break;
                            }
                        }
                    }
                }
                pStatus = {};
                pStatus[key]=status;
                window.trackEventv2_0default("acs","info",key,status);
                if(Gatekeeper && typeof Gatekeeper.setPaymentStatus == "function") {
                    window.trackEventv2_0("acs","info","Setting "+key,status, "setting", key);
                    Gatekeeper.setPaymentStatus(JSON.stringify(pStatus));
                } else {
                    window.trackEventv2_0("acs","error","Error setting inferred_payment_status","No function setPaymentStatus found", "error", "setting_inferred_payment_status");
                }
            }
            return;
        } catch(e) {
            window.trackEventv2_0("acs","error","Exception while inferring payment_status","Exception : "+e, "exception", "while_inferring_payment_status");
        }
    }
}

var modifyPage = function() {

    var cacheImageFiles = function(){
        var images = getImages();
        var source;
        var url;
        if(images){
            for( i=0;i<images.length;i++){
                source=images[i].src;
                var base =window.location;
                base= base.protocol + "//" + base.host ;
                if(source.indexOf("http:")==0||source.indexOf("https:")==0){
                    source=source;
                }
                else{
                    if(source.indexOf("/")==0){
                        source=base+source;
                    }
                    else{
                        source=base+"/"+source;
                    }
                }
                var img = document.createElement('img');
                img.src = images[i].src;
                if(images[i].src.indexOf(base.hostname) != -1) {
                    var canvas = document.createElement("canvas");
                    canvas.style.display= "none";
                    canvas.width = img.width;
                    canvas.height = img.height;
                    document.body.appendChild(canvas);
                    var ctx = canvas.getContext("2d");
                    ctx.drawImage(img,0,0,img.width,img.height);
                    var imgData;
                    try {
                        imgData = canvas.toDataURL();
                    } catch(err) {
                        window.trackEventv2_0("acs","error","godel_js_error","Error in canvas toDataURL"+err+" at "+window.location.href, "godel", "js_error");
                    }
                    if(imgData) {
                        var separated = imgData.split(",",2);
                        imgData = separated[1];
                        if(typeof Gatekeeper !=="undefined" && typeof Gatekeeper.addToCache == "function"){
                            try {
                                Gatekeeper.addToCache(source,imgData);
                            } catch (e){
                                window.trackEventv2_0("acs","error","godel_js_error","Error while adding image to cache:"+e, "godel", "js_error");
                            }
                        }
                    }
                }
            }
       }
       return;
    }

    var loc = document.location.href;
    window.addEventListener('load', __juspay.trackJsOnPageLoad());
    var pages = null;
    var clickOnElementToResetTimer = function(element,shouldShowWaitingFragment){
        element.addEventListener("click",function(){
            Gatekeeper.resetSmsReadTime();
            if(shouldShowWaitingFragment!==null){
                if(shouldShowWaitingFragment===true){
                    Gatekeeper.showWaitingFragment();
                }
            }
        });
    }
    var callDebitRedirectUber = function(){
        try{
            GK.setUberEventWithCallback("REDIRECT_DEBIT", function(result) {
                window.trackEventv2_0("acs","info","REDIRECT_DEBIT","uber_shown", "redirect", "debit");
                if( result == "ok") {
                    window.trackEventv2_0("acs","info","REDIRECT_DEBIT","try_debit_clicked", "redirect", "debit");
                    Gatekeeper.destroyUber();
                    Gatekeeper.showCancelTransactionDialog();
                } else if(result == "closed") {
                    window.trackEventv2_0("acs","info","REDIRECT_DEBIT","close_clicked", "redirect", "debit");
                }
            },{type:"dialog",showOnLoad:true,height:GK.getPercent(68),width:-2},null);
        }catch(err){
            window.trackEventv2_0("acs","error","Error calling uber","Error calling Redirect_debit Uber. Error is "+String(err), "error", "calling_uber");
        }
    }

    var otpTextFieldQuery = function(selector) {
        if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios"){
            if(typeof (Gatekeeper.otpTextFieldQuery) == "function"){
                Gatekeeper.otpTextFieldQuery(selector);
            }
        }
    }

    var focusElement = function(element) {
        if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios")
            return;
        element.focus();
    }

    var blurElement = function(element) {
        if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios")
            return;
        element.blur();
    }

    var extraFocusElement = function(selector) {
        if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios"){
            if(selector){
                selector.addEventListener("focus",function(){
                   if(typeof (Gatekeeper.removeFragment) == "function"){
                         Gatekeeper.removeFragment("removing because not supported");
                   }
               });
            }
        }
    }
    var numberTextFieldQuery = function(selector) {
        if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios") {
            if(typeof (Gatekeeper.numberTextFieldQuery) == "function"){
                Gatekeeper.numberTextFieldQuery(selector);
            }
        }
    }

    var removeAutoSuggestionBar = function(element) {
        if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios") {
            element.setAttribute("autocomplete", "off");
            element.setAttribute("autocorrect", "off");
            element.setAttribute("autocapitalize", "off");
            element.setAttribute("spellcheck", "off");
        }
    }

    var attachPhoneKeyboard = function(element) {
            try {
                __juspay.setOnEach(element,function(elt){
                    if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios"){
                        Gatekeeper.requestPhoneKeyboardShow(elt.outerHTML);
                        //elt.setAttribute("pattern","[0-9]*");
                        elt.addEventListener("focus",function(){
                            __juspay.delayMe(function(){
                                Gatekeeper.requestPhoneKeyboardShow(elt.outerHTML);
                            },200);
                        },false);
                    } else {
                        elt.addEventListener("focus",function(){
                            __juspay.delayMe(function(){
                                Gatekeeper.requestPhoneKeyboardShow();
                            },200);
                        },false);
                    }
                    window.trackEventv2_0default("acs", "info", "focused", "<"+elt.nodeName+" id="+elt.id+" name="+elt.name+" class="+elt.className+" >");
                });
            } catch(err) {
                window.trackEventv2_0("acs","error","godel_js_error","Function: attachPhoneKeyboard and Error is "+String(err), "godel", "js_error");
            }
        }

    var attachAlphaNumericPasswordHelper = function(passwordElement) {

        var pwdFocuser = function () {
             __juspay.delayMe(function(){
                 Gatekeeper.showPasswordHelperFragment();
                 Gatekeeper.requestPasswordKeyboardShow();
                 __juspay.scrollToElement(passwordElement);
             },100);
             var passwordValue = passwordElement.value;
             if(passwordValue != undefined && passwordValue != null) {
                 //TODO - set a variable to check if fragment is created in java and then setPasswordValue
                 __juspay.delayMe(function(){
                     Gatekeeper.setPasswordValue(passwordElement.value);
                 },200);
             }
             window.trackEventv2_0default("acs", "info", "focused", "<"+passwordElement.nodeName+" id="+passwordElement.id+" name="+passwordElement.name+" class="+passwordElement.className+" >");
        }
        passwordElement.addEventListener("focus", pwdFocuser, false);

        passwordElement.addEventListener("input", function (e){
            var passwordValue = passwordElement.value;
            if(passwordValue != undefined && passwordValue != null) {
                Gatekeeper.setPasswordValue(passwordValue);
            }
        }, false);

        return function() {
            passwordElement.removeEventListener("focus", pwdFocuser, false);
        }
    }
    var attachPhonePasswordHelper = function(passwordElement) {

        passwordElement.addEventListener("focus", function () {
            Gatekeeper.showPasswordHelperFragment();
            Gatekeeper.requestPhoneKeyboardShow();
            var passwordValue = passwordElement.value;
            if(passwordValue != undefined && passwordValue != null) {
                //TODO - set a variable to check if fragment is created in java and then setPasswordValue
                __juspay.delayMe(function(){
                    Gatekeeper.setPasswordValue(passwordValue);
                },200);
            }
        window.trackEventv2_0default("acs", "info", "focused", "<"+passwordElement.nodeName+" id="+passwordElement.id+" name="+passwordElement.name+" class="+passwordElement.className+" >");
        }, false);

        passwordElement.addEventListener("input", function (e){
            try{
                var passwordValue = passwordElement.value;
                if(passwordValue != undefined && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue);
                }
            }catch(err){
                window.trackEventv2_0("acs","error","godel_js_error","Function: attachAlphaNumericPasswordHelper while adding input listener and Error is "+String(err), "godel", "js_error");
            }
        }, false);
    }
    var makePageScrollable = function() {
        var body = document.querySelector('body');
        if(body){
            old_html = body.innerHTML;
            new_html = "<div id='OuterWrapper'><div id='InnerWrapper'>" + old_html + "</div></div>";
            body.innerHTML = new_html;
            document.querySelector('div[id="InnerWrapper"]').style.width = window.innerWidth.toString() +"px";
            document.querySelector('div[id="OuterWrapper"]').style.width = 1.5 * window.innerWidth.toString() +"px";
            document.querySelector('div[id="InnerWrapper"]').style.height = (window.innerHeight*1.08).toString() +"px";
            document.querySelector('div[id="OuterWrapper"]').style.height = (1.5 * window.innerHeight*1.05).toString() +"px";
        }
    }

    var setupLoginPageWithUsernameAndSubmitButton = function(usernameSelector, submitSelector) {
        setupLoginPageHelper("",usernameSelector,submitSelector);
    }

    var setupLoginPageWithSubmitButton = function(passwordSelector, usernameSelector, submitSelector) {
        setupLoginPageHelper(passwordSelector,usernameSelector,submitSelector);
    }
    var setupLoginPageHelper = function(passwordSelector,usernameSelector,submitBtnSelector){
        var usernameElement = document.querySelector(usernameSelector);
        var submitElement = document.querySelector(submitBtnSelector);
        var passwordElement="";
        if(passwordSelector!=""){
            passwordElement = document.querySelector(passwordSelector);
            attachAlphaNumericPasswordHelper(passwordElement);
        }
        if(usernameElement && submitElement){
            removeAutoSuggestionBar(usernameElement);
            usernameElement.addEventListener("focus", function (){
                Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                window.trackEventv2_0default("acs", "info", "focused", "<"+usernameElement.nodeName+" id="+usernameElement.id+" name="+usernameElement.name+" class="+usernameElement.className+" >");
            }, false);

            var bankCustomerId = Gatekeeper.getBankCustomerId();
            if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined") {
                bankCustomerId = "";
            }
            usernameElement.value = bankCustomerId;
            var textRegex = /TEXT/i;
            var numRegex = /(NUMBER|NUMERIC)/i;
            var telRegex = /TEL/i;
            var disableAutofocus = Gatekeeper.getSessionAttribute("disableAutoFocus");
            if(disableAutofocus == "true"){
                Gatekeeper.setSessionAttribute("disableAutoFocus", "false");
            }
            if(usernameElement.value == ""){
                Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                if(numRegex.test(usernameElement.getAttribute("type")) || telRegex.test(usernameElement.getAttribute("type"))){
                    numberTextFieldQuery(usernameSelector);
                    attachPhoneKeyboard(usernameElement);
                    __juspay.delayMe(function(){
                        Gatekeeper.requestNumericKeyboardShow();
                        __juspay.scrollToElement(usernameElement);
                    }, 100);
                }else if(textRegex.test(usernameElement.getAttribute("type"))){
                    removeAutoSuggestionBar(usernameElement);
                    __juspay.delayMe(function(){
                        if(disableAutofocus != "true"){
                            Gatekeeper.requestKeyboardShow();
                        }
                        __juspay.scrollToElement(usernameElement);
                    }, 100);
                }else{
                    Gatekeeper.shoutOut("REGEX not matched in set up login page with submit");
                    __juspay.scrollToElement(usernameElement);
                }
                if(disableAutofocus == "true"){
                    usernameElement.blur();
                } else {
                    focusElement(usernameElement);
                }
            } else if(passwordElement) {
                passwordElement.blur();
                window.trackEventv2_0("acs","info","cust_id_populated","Cust Id was populated", "cust_id", "populated");
                if(disableAutofocus != "true"){
                    focusElement(passwordElement);
                }
            }else{
                usernameElement.blur();
                focusElement(usernameElement);
            }
            submitElement.addEventListener("click", function() {
                var usernameElement = document.querySelector(usernameSelector);
                if(usernameElement.value){
                    Gatekeeper.onCustomerIdSubmit(usernameElement.value);
                    Gatekeeper.requestKeyboardHide();
                }
            });
        }
    }
    var addMetaTag = function(){
        var metaTag=document.createElement('meta');
        metaTag.name = "viewport"
        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
        document.getElementsByTagName('head')[0].appendChild(metaTag);
    }
    var autoClickButton = function(elementSelector, description, bankName){
        try{
            if(typeof(elementSelector)!= "undefined" && elementSelector!=""){
                if(typeof(description)== "undefined" || description ==""){
                    var description = 'Element is '+elementSelector;
                }
                var element = document.querySelector(elementSelector);
                if(element && __juspay.isMobUiEnabled("AUTO_CLICK_"+bankName)){
                    __juspay.clickOnElement(element);
                    window.trackEventv2_0default("acs", "info", "AUTO_CLICK", description);
                }
            }else{
                window.trackEventv2_0("acs","error","acs_js_error","Function: autoClickButton Invalid Element Selector on "+bankName,"acs","js_error");
            }
        }catch(err){
            window.trackEventv2_0("acs","error","godel_js_error","Function: autoClickButton Error is "+String(err), "godel", "js_error");
        }
    }
    var initRupayIframe = function(){
        if(__juspay.isRupayEnabled("")) {        // checking only global rupay configuration
            Gatekeeper.removeFragment("PAGE_LOAD");
            window.isPostMessageEnabled = true;
            var f = setTimeout(function(){
                window.iframe = document.querySelector('iframe[id=iframeIssuer]') ? document.querySelector('iframe[id=iframeIssuer]').contentWindow : document;
            },5000);
        }
    }

    var listenForPostMessages = function() {
        //Check if inject_acs_into_iframes flag is set. If it is set, acs is being inserted into every js
        //file in every iframe. And if this is an iframe, setup the onmessage listener to listen
        //for events from top window
        var injectingIntoIframes = Gatekeeper.getSessionAttribute("inject_acs_into_iframes");
        var isIframe = window.top !== window.self;

        if(isIframe && injectingIntoIframes) {
            window.addEventListener("message",GK.receiveMessage,false);
        }
    }

    var otpFragmentHelper = function(otpSelector, resendSelector, isAlphanumeric){
        try{
            Gatekeeper.removeFragment("Reached otp page of "+Gatekeeper.getCurrentBank());
            Gatekeeper.setPollingForSmsEnabled(true);
            if(typeof(otpSelector)!= "undefined" && otpSelector!=""){
                var otpField = document.querySelector(otpSelector);
                if(otpField) {
                    otpField.blur();
                    __juspay.showOtpFragmentWrapper(otpField);

                    if (/iP(hone|od|ad)/.test(navigator.platform)) { //iOS
                        numberTextFieldQuery(otpSelector);
                        var currentBank = Gatekeeper.getCurrentBank();
                        //AXIS DC/CC
                        if (currentBank === 'AXIS') {
                            otpField.style.cssText = "width: 100%; padding: 10px 20px; margin: 8px 0 15px; box-sizing: border-box; border: 1px solid #e4e4e4; font-size: 14px; outline: 0;"
                        }
                        var disabledBanks = ["AXISNB", "SCB", "IOBNB", "KOTAKNB"];
                        if (disabledBanks.indexOf(currentBank) == -1) {
                            attachPhoneKeyboard(otpField);
                        }
                    } else if(typeof(isAlphanumeric)!= "undefined" && isAlphanumeric!="" && isAlphanumeric){
                        //Dont assign Phone keyboard
                    } else { //Android
                        otpTextFieldQuery(otpSelector);
                        attachPhoneKeyboard(otpField);
                    }
                    __juspay.trackPageStatus("INPUT_OTP");
                    __juspay.trackAuthMethod("OTP");
                    __juspay.scrollToElement(otpField);

                    otpField.addEventListener("click",function(){
                        Gatekeeper.hideAssistanceFragment();
                        if (isAlphanumeric){
                            Gatekeeper.requestPasswordKeyboardShow();
                            window.trackEventv2_0default("acs","info","BANK_WITH_ALPHANUMERIC_OTP", "PI is " +Gatekeeper.getCurrentBank());
                        }else{
                            Gatekeeper.requestNumericKeyboardShow();
                        }
                    }, false);
                    window.trackEventv2_0default("acs","info","new_otp_fragment", "PI is " +Gatekeeper.getCurrentBank());
                }
                if(typeof(resendSelector)!= "undefined" && resendSelector!=""){
                    var resendOtp = document.querySelector(resendSelector);
                    if(resendOtp){
                        Gatekeeper.enableRegenerateOTP();
                        resendOtp.addEventListener("click",function(){
                            Gatekeeper.resetOtpFragmentToWaitingState();
                            Gatekeeper.resetSmsReadTime();
                        },false);
                    }else{
                        Gatekeeper.disableRegenerateOTP();
                    }
                }else{
                    Gatekeeper.disableRegenerateOTP();
                }
            }
        }catch(err){
            window.trackEventv2_0("acs","error","godel_js_error","Function: otpFragmentHelper Error is "+String(err), "godel", "js_error");
        }
    }
    var passwordFragmentHelper = function(passwordSelector){
        var fun = function() {};
        try{
            Gatekeeper.removeFragment("Reached Password Page of "+Gatekeeper.getCurrentBank());
            if(typeof(passwordSelector)!="undefined" && passwordSelector!=null){
                var passwordElement = document.querySelector(passwordSelector);
                if(passwordElement){
                    __juspay.trackPageStatus("INPUT_3DS");
                    __juspay.trackAuthMethod("OTP");
                    blurElement(passwordElement);
                    fun = attachAlphaNumericPasswordHelper(passwordElement);
                    focusElement(passwordElement);
                    window.trackEventv2_0default("acs","info","new_password_fragment", "PI is " +Gatekeeper.getCurrentBank())
                    passwordElement.addEventListener("focus",function(){
                        __juspay.scrollToElement(passwordElement);
                    });
                }
            }
        }catch(err){
            window.trackEventv2_0("acs","error","godel_js_error","Function:passwordFragmentHelper Error is"+String(err), "godel", "js_error");
        }
        return fun;
    }
    var accountSelectionPage = function(accountSelector,bankName,confirmBtnSelector,remarksField){
        __juspay.trackPageStatus("CHOOSE_ACCOUNT");
        if(typeof(accountSelector)!= "undefined" && accountSelector !=null){
            var account = document.querySelectorAll(accountSelector);
            if(!account[0] && bankName=="HDFCNB"){ // HDFCNB old bank already have accounts (variable)
                account= accounts;
            }
            if(typeof(confirmBtnSelector)!="undefined" && confirmBtnSelector !=""){
                 var confirmBtn= document.querySelector(confirmBtnSelector);
                 if(confirmBtn && account.length){
                     confirmBtn.addEventListener("click",function(){
                         for(var i = 1; i < account.length; i++) {
                             if(account[i].selected == "true") {
                                 Gatekeeper.storeLastChosenAccount(bankName,account[i].innerText);
                                 console.log(bankName+" account " +Gatekeeper.getLastChosenAccount(bankName));
                                 break;
                             }
                         }
                         window.trackEventv2_0("acs","info","ACCOUNT STORED",bankName, "account", "stored");
                     });
                     if(account[0]){
                        if(bankName!="HDFCNB" && account[0].innerText.indexOf("Select") >=0){
                            account[1].selected="true";
                        }else{
                            account[0].selected= "true";
                        }
                        if(account.length<2 || account[1].selected){
                            if(__juspay.isMobUiEnabled("AUTO_CLICK_"+bankName)){
                                 if(__uber.initSessionCounterIncremental("AUTO_CLICK_CONFIRM") < 2){
                                    window.trackEventv2_0default("acs","info","AUTO_CLICK","SINGLE_ACCOUNT_SELECTED");
                                    __juspay.clickOnElement(confirmBtn);
                                 }
                            }
                        }else{
                            __juspay.trackPageStatus("MULTI_ACCOUNT_USER");
                            var lastChosen = Gatekeeper.getLastChosenAccount(bankName);
                            if(lastChosen){
                               for(var i = 1; i < account.length; i++) {
                                   if(account[i].innerText === lastChosen) {
                                       account[i].selected = "true";
                                       break;
                                   }
                               }
                            }
                        }
                    }
                 }
            }
            if(remarksField){
                var remarks = Gatekeeper.getRemarksForBank();
                if(remarks==null)
                    remarksField.value="Online Transaction";
                else
                    remarksField.value=remarks;
                __juspay.trackPageStatus("INPUT_NB_REMARKS");
            }
            Gatekeeper.showNetbankingDefaultFragment();
            Gatekeeper.changeNextActionText("Pay");
        }
    };

    var startPolling = function(url, params, callback, withCredentials) {
        var data = JSON.stringify(params);

        var xhr = new XMLHttpRequest();
        if(withCredentials == null) {
            withCredentials = true;
        }
        xhr.withCredentials = withCredentials;

        xhr.addEventListener("readystatechange", function () {
          if (this.readyState === 4) {
            callback(this.responseText);
          }
        });

        xhr.open("POST", url);
        xhr.setRequestHeader("content-type", "application/json");

        xhr.send(data);
    }

    var getImageInformation = function() {
        //To gather information about all images
        domCollectionJson = {};
        domCollectionJson.images = [];
        var imageArray = document.querySelectorAll('img');

        for(var imageCount=0;imageCount<imageArray.length;imageCount++){
            var detailsImages = {};
            if(typeof imageArray[imageCount] != "undefined" && typeof imageArray[imageCount].src != "undefined" && imageArray[imageCount].src.length <= 640) {  // Image src should be less than or equal to 62.5% of 1KB
                detailsImages.src = imageArray[imageCount].src;
            } else {
                detailsImages.src = "";
            }
            detailsImages.name = imageArray[imageCount].name;
            detailsImages.type = imageArray[imageCount].type;
            domCollectionJson.images.push(detailsImages);
        }
        return JSON.stringify(domCollectionJson);
    }


    var isNotEmpty = function (obj) {
      for(var prop in obj) {
          if(obj.hasOwnProperty(prop))
          return true;
      }
      return false;
    }

    var getPIG = function(bankCode) {
        if(typeof bankCode == "undefined") {
            return "UNKNOWN";
        } else if(bankCode.endsWith("NB")) {
            return "NB"
        } else if(bankCode.endsWith("DC")) {
            return "DC"
        } else if(bankCode.endsWith("CC")) {
            return "CC"
        }

        return "UNKNOWN";
    }

    var applyPageRules = function() {
        var loc = window.location.href;
        loc = loc.split(";")[0]
        var a = document.createElement("a");
        a.href = loc;
        var path = a.pathname + a.search;
        var page;
        var unknownPIimages = [];
        var UnknownPI = false;

        if(typeof getExternalPages === "function") {
            var externalPages = getExternalPages()
            pages = __juspay.merge(pages, externalPages)
        }

        for(var i = 0; i < pages.length; i++) {
            if(pages[i]["path"].test(path)) {
                hostname = pages[i]["hostname"]
                if((typeof(hostname) == "string" && hostname == a.hostname) || (hostname.test && hostname.test(a.hostname))) {
                    if(pages[i]["domCheck"] && pages[i]["domCheck"]()) {
                        page = pages[i];
                        __juspay.trackPageClicks();
                        if(typeof(page.bank) !== "undefined") {
                            Gatekeeper.setBank(page.bank);
                            GK.setBank(page.bank);
                        }
                        if(typeof Gatekeeper !=="undefined" && typeof Gatekeeper.isEnabled == "function"){
                            if(Gatekeeper.isEnabled("cacheImageV2") && typeof cacheImageFiles == "function"){
                                cacheImageFiles();
                            }
                        }
                        if(juspayContext && juspayContext['web_lab_rules']){
                            var currentPI = "pi_"+pages[i].bank;
                            var currentPIG = "pig_" + getPIG(pages[i].bank);
                            var pi_rules = juspayContext['web_lab_rules'][currentPI];
                            var pig_rules = juspayContext['web_lab_rules'][currentPIG];
                            var uber_rules = "uber_"+pages[i].bank;
                            var uber_weblab = juspayContext['web_lab_rules'][uber_rules];
                            if(typeof pig_rules!=="undefined" && pig_rules===0){
                                window.trackEventv2_0default("acs","info","pig_off_by_weblab","This PI Group is OFF by WebLab");
                            } else if(typeof pi_rules!=="undefined" && pi_rules===0){
                                window.trackEventv2_0default("acs","info","pi_off_by_weblab","This PI is OFF by WebLab");
                            }else{
                                if(typeof(pages[i]["action"]) == "function" ) {
                                    __juspay.scrollOnFocusInputElements();
                                    if(__juspay.isMobUiEnabled("HTML_DUMP_CHECK")){
                                        collectHtmlDump();
                                    }
                                    pages[i]["action"]();
                                    domFailureOccured = false;
                                    if(typeof uber_weblab==="undefined" && uber_weblab!==0){
                                        if(typeof endPages == "function"){
                                            endPages(pages[i].bank);
                                        }
                                    }
                                }
                            }
                        }else{
                            window.trackEventv2_0default("acs","error","no_weblab_rules_still","Web Lab Rules Still not found on page. So cannot switch off action");
                            if(typeof(pages[i]["action"]) == "function" ) {
                                __juspay.scrollOnFocusInputElements();
                                pages[i]["action"]();
                                domFailureOccured = false;
                            }
                        }
                        Gatekeeper.trackAction("acs","info","page",JSON.stringify({is_dom_check_successful : (!pages[i]["logDomCheckFalse"]), url : path }));
                        break;
                    } else {
                        if(!pages[i]["fallThrough"]) {
                            domFailureOccured = true;
//                            Gatekeeper.trackDomCheckFailure(loc);
                        }
                    }
                }
            }
           else {
             UnknownPI = true;
           }
            if(typeof Gatekeeper !=="undefined" && typeof Gatekeeper.isEnabled == "function"){
                if(Gatekeeper.isEnabled("log_typing_behaviour")){
                    __juspay.logKeyPressEvents();
                }
            }
        }
        if (UnknownPI){
          var piImageInfo = getImageInformation();

           if (piImageInfo != "{\"images\":[]}"){
               unknownPIimages.push(piImageInfo);
           }
           if (isNotEmpty(unknownPIimages)){
              window.trackEventv2_0default("acs","info","unknown_payment_instrument",JSON.stringify(unknownPIimages));
           }
        }
        return page;
    };

    var submitOtpHelper = function(otp, ipSelector, submitSelector) {
        Gatekeeper.shoutOut("Submit otp helper");
        Gatekeeper.showWaitingFragment();
        var elt = document.querySelector(ipSelector);
        if(elt) {
            if(otp==="undefined" || typeof(otp)=="undefined") {
                otp = "";
            }
            elt.value = otp;
            var submitBtn = document.querySelector(submitSelector);
            if(submitBtn && /img/i.test(submitBtn.tagName)) {
                console.log("Generating the MouseEvent for Click and dispatching it")
                var clickEvent = document.createEvent('MouseEvent');
                clickEvent.initEvent('click', true, true);
                submitBtn.dispatchEvent(clickEvent);
            }
            else if(submitBtn) {
                __juspay.clickOnElement(submitBtn);
            }
        }
    };
    var getImages = function(){
        var innerWindow=frames["bottom_frame"];
        var images;
        if(innerWindow){
            images = innerWindow.document.querySelectorAll("img");
        } else {
            images = document.querySelectorAll("img");
        }
        return images;
    }
    var axisCardMobileOptimisation = function(){
        var passwordField = document.querySelector('input[id="txtPassword"]');
        var otpField = document.querySelector('input[id="otpValue"]');
        var preGenOtpField = document.querySelector('input[id="pre_otpValue"]');
        var preGenOtpRadioBtn = document.querySelector('input[data-id="preGenerated_otp"]');
        var pwdRadioBtn = document.querySelector('input[data-id="static"]');
        var otpRadioBtn = document.querySelector('input[data-id="otp"]');
        var submitBtn = document.querySelectorAll('input[id="cmdSubmit"]');
        var cancel = document.querySelectorAll('a[href="javascript:ConfirmCancel();"]');
        var link= document.querySelectorAll('a[href="#"]');
        var details= document.getElementsByClassName("merchantName");
        var inputElements = document.getElementsByClassName("input-data");
        var para = document.getElementsByClassName("section-desc disable-for-ie");
        var radioBtns = document.getElementsByClassName("section-radio");
        if(details[0]){
            details[0].parentElement.style.fontSize="12px";
            details[0].style.fontSize="12px";
        }
        if(para[0]){
            para[0].childNodes[1].style.fontSize="12px";
        }
        if(cancel){
            __juspay.removeElements(cancel);
        }
        var i=0;
        if(submitBtn){
            while(i<submitBtn.length){
                submitBtn[i].style.textAlign="center";
                submitBtn[i].style.backgroundColor="#ddd";
                submitBtn[i].style.fontSize="12px";
                submitBtn[i].removeAttribute("src");
                if(inputElements){
                    inputElements[i].childNodes[0].style.textAlign="left";
                }
                i++;
            }
        }
    //PwD section
        var pwdSection=document.getElementById("static");
        if(pwdSection) {
            var forgotPwd = document.getElementsByClassName("forgot-password");
            if(inputElements[0]){
                inputElements[0].childNodes[0].innerText="Enter Password :";
            }
            if(forgotPwd[0] && link[0]){
                forgotPwd[0].style.textAlign="right";
                link[0].parentElement.innerHTML='<a href="#" onclick="forgot_password();" style="text-decoration: underline; font-size: 12px;">Forgot Password</a>';
            }
        } else {
    // Common for otp and pre-generated otp sections
            if(inputElements[0]){
                inputElements[0].childNodes[0].innerText="Enter OTP :";
                inputElements[0].childNodes[0].style.textAlign="right";
            }
            if(inputElements[1]){
                inputElements[1].childNodes[0].innerText="Enter OTP :";
                inputElements[1].childNodes[0].style.textAlign="right";
            }
        }
    //OTP Section
        var otpSection=document.getElementById("otp");
        if(otpSection){
            otpSection.childNodes[1].style.fontSize="10px";

            if(link[1]){
                link[1].parentElement.parentElement.style.textAlign="right";
                link[1].parentElement.parentElement.style.fontSize="12px";
            }
        }
    //pre-generated OTP
        var PreOtpSection=document.getElementById("preGenerated_otp");
        if(PreOtpSection) {
            var msg = document.getElementsByClassName("section-desc smallNotesNoteNavy");
            if(inputElements[2]){
                inputElements[2].childNodes[0].innerText="Enter OTP :";
                inputElements[2].childNodes[0].style.textAlign="right";
            }
            if(passwordField && submitBtn && otpField && preGenOtpField && radioBtns && details && msg && forgotPwd && otpSection && preGenOtpRadioBtn && otpRadioBtn && pwdSection && pwdRadioBtn){
                __juspay.modifyUI({"style_width":{"other":[0.9,passwordField,submitBtn[0],otpField,submitBtn[1],submitBtn[2],preGenOtpField]},
                                   "style_height":{"other":[(1/28),submitBtn[0],submitBtn[1],submitBtn[2]]},
                                   "border":{"border_top":[radioBtns[0],radioBtns[1],radioBtns[2]],
                                             "border_bottom":[details[0].parentElement.parentElement]},
                                   "swap_elements":[submitBtn[1].parentElement,otpSection.childNodes[1].childNodes[9],submitBtn[0].parentElement,forgotPwd[0],submitBtn[2].parentElement,msg[0]],
                                   "font_size":{"other":[12,preGenOtpRadioBtn.parentElement,link[1].parentElement.parentElement,otpRadioBtn.parentElement,pwdRadioBtn.parentElement,pwdSection.childNodes[1]]}});
            }
        }
    }
    //Log the browser user agent once every session
    var userAgent = navigator && navigator.userAgent;
    if(typeof userAgent != "undefined")
        window.trackEventv2_0default("acs", "info", "user_agent", userAgent);
    if((typeof userAgent != "undefined") && juspayContext && ((typeof juspayContext["platform"]!="undefined" && juspayContext["platform"] !="ios") || (typeof juspayContext["platform"]=="undefined"))
        && __uber.initSessionCounterIncremental("user_agent_log") < 2) {
        // Showing webview update for Chrome versions lower than 55
        webviewVersion = userAgent.substr(userAgent.indexOf("Chrome") + 7,2);
        if((webviewVersion == 53 || webviewVersion == 54) && __uber.isUberEnabled("WEBVIEW_ALERT") && __uber.initSessionCounterIncremental("webviewUberShown")==1) {
            setTimeout(function() {
                GK.setUberEventWithCallback("WEBVIEW_ALERT", function(result) {
                    window.trackEventv2_0default("acs","info","WEBVIEW_ALERT","uber_shown");
                    if( result == "ok" || result == "closed") {
                        event_value = result+"_clicked";
                        window.trackEventv2_0default("acs","info","WEBVIEW_ALERT",event_value);
                        Gatekeeper.dismissUber();
                    }
                },{type:"dialog",showOnLoad:true,height:GK.getScreenPercent(40),width:-2},null);
            },500);
        }
    }
    var VISA = "VISA", MASTERCARD = "MASTERCARD", RUPAY = "RUPAY", AMEX = "AMEX";
    pages = [
        /* production configs */

        { // ACS Return Page sending Authentication Response back to PG/Merchant
            path: /.*/,
            hostname: /.*/,
            state: "UNUSED",
            bank: "",
            local: false,
            domCheck: function() {
                var pares = document.querySelector('input[name=PaRes]')
                var md = document.querySelector('input[name=MD]')
                var pareq = document.querySelector('input[name=PaReq]')
                var termUrl = document.querySelector('input[name=TermUrl]') || document.querySelector('input[name=termUrl]');
                var accuResponseCode = document.querySelector('input[name=AccuResponseCode]'); // Rupay Page Auth Status
                if(pares && md) {
                    // we are sending back the pares information so that the authentication response can be used for analytics
                    window.trackJsInfo("authentication_response_pares", pares.value, "authentication", "response_pares");
                    window.trackJsInfo("authentication_response_md", md.value, "authentication", "response_md");
                }
                if(pareq && md){
                    window.trackJsInfo("authentication_request_pareq", pareq.value, "authentication", "request_pareq");
                    window.trackJsInfo("authentication_request_md", md.value, "authentication", "request_md");
                }
                if(termUrl){
                    window.trackJsInfo("authentication_termUrl", termUrl.value, "authentication", "termUrl");
                }
                if(pareq && md && termUrl){
                    Gatekeeper.setSessionAttribute("PAREQ", pareq.value);
                    Gatekeeper.setSessionAttribute("MD", md.value);
                    Gatekeeper.setSessionAttribute("TERM_URL", termUrl.value);
                }
                if(accuResponseCode) {
                    window.trackJsInfo("authentication_response_code", accuResponseCode.value, "authentication", "response_code");
                }
                return false
            },
            fallThrough: true
        },

        //HDFC
        { //HDFC: ACS Page
            path: /\/ACSWeb\/jsp\/payerAuthOptions.jsp/,
            hostname: "netsafe.hdfcbank.com",
            state: "UNUSED",
            bank: "HDFC",
            local: false,
            domCheck: function() {
                return document.querySelector('input[name=acsRadio][value=DYNAMIC]') &&
                (document.querySelector('input[name=acsRadio][value=payerAuth]') || document.querySelector('input[name=acsRadio][value=adsReg]')) ;
            },
            action: function() {
                var mastercard = document.querySelector('img[src="/ACSWeb/images/Mastercard_Securecode_bg.gif"]');
                var vbv = document.querySelector('img[src="/ACSWeb/images/vbv_logo.jpg"]');
                var passOption = document.querySelector("input[type=radio][value=payerAuth]");
                var otpOption = document.querySelector('input[name=acsRadio][value=DYNAMIC]');
                var submitBtn = document.querySelector("input[type=submit][name=sbmtBtn]");
                var setCount = 0;
                Gatekeeper.setSessionAttribute("hdfc_auth_both","true");
                if(Gatekeeper.isEnabled("isAutoAuthEnabledHdfc")){
                    var storedLastAuth = Gatekeeper.getDataFromSharedPrefs("HDFC_AUTH");
                    if(storedLastAuth){
                        var authCountPwd = GK.getAuthCountForPwd("HDFC_AUTH")
                        var authCountOtp = GK.getAuthCountForOtp("HDFC_AUTH")
                        if(authCountOtp > 2){
                            setCount++;
                            window.trackEventv2_0default("acs","info","directly_moving_to_otp_page_count",authCountOtp);
                            if(document.forms && document.forms["frmAcsOption"]) {
                                var frm = document.forms["frmAcsOption"];
                                var el = document.querySelector('input[name=acsRadio][value=DYNAMIC]');
                                if(el) {
                                    el.checked = true;
                                    var submitBtn = frm.querySelector("input[type=submit]");
                                    Gatekeeper.addDataToSharedPrefs("HDFC_AUTH","OTP_"+authCountOtp);
                                    if(__juspay.isMobUiEnabled("AUTO_CHOOSE_HDFC")){
                                        window.trackEventv2_0default("acs","info","AUTO_CLICK","AUTO_AUTH_CHOOSE");
                                        Gatekeeper.showWaitingFragment();
                                        __juspay.clickOnElement(submitBtn);
                                    }
                                }
                            }
                        }
                        if(authCountPwd > 2){
                            setCount++;
                            window.trackEventv2_0default("acs","info","directly_moving_to_pwd_page_count",authCountPwd);
                            var passOption = document.querySelector("input[type=radio][value=payerAuth]");//OTHER HDFC CREDIT/DEBIT Cards
                            var pwdOption = document.querySelector("input[type=radio][value=adsReg]"); //HDFC DEBIT MASTERCARD
                            if(passOption){
                                passOption.checked = true;
                            }
                            if(pwdOption){
                                pwdOption.checked = true;
                            }
                            var submitBtn = document.querySelector("input[type=submit][name=sbmtBtn]");
                            if(submitBtn){
                                Gatekeeper.addDataToSharedPrefs("HDFC_AUTH","PWD_"+authCountPwd);
                                if(__juspay.isMobUiEnabled("AUTO_CHOOSE_HDFC")){
                                    Gatekeeper.showWaitingFragment();
                                    window.trackEventv2_0default("acs","info","AUTO_CLICK","AUTO_AUTH_CHOOSE");
                                    __juspay.clickOnElement(submitBtn);
                                }
                            }
                        }
                    }
                }
                submitBtn.addEventListener("click",function(){
                    if(passOption.checked){
                        var authCountPwd = GK.getAuthCountForPwd("HDFC_AUTH")
                        Gatekeeper.addDataToSharedPrefs("HDFC_AUTH","PWD_"+authCountPwd)
                        window.trackEventv2_0("acs","info","HDFC_AUTH", "PWD_"+authCountPwd, "HDFC", "auth");
                    } else if(otpOption.checked){
                        var authCountOtp = GK.getAuthCountForOtp("HDFC_AUTH");
                        Gatekeeper.addDataToSharedPrefs("HDFC_AUTH","OTP_"+authCountOtp)
                        window.trackEventv2_0("acs","info","HDFC_AUTH", "OTP_"+authCountOtp, "HDFC", "auth");
                    }
                },false)
                if(mastercard) {
                    Gatekeeper.setCardBrand(MASTERCARD);
                } else {
                    if(vbv){
                        Gatekeeper.setCardBrand(VISA);
                    }
                }
                if(setCount== 0){
                    __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                    Gatekeeper.showACSOptions();
                }else{
                    Gatekeeper.showWaitingFragment();
                }
            },
            reachOtpStage: function() {
                Gatekeeper.showWaitingFragment();
                if(document.forms && document.forms["frmAcsOption"]) {
                    var frm = document.forms["frmAcsOption"];
                    var el = document.querySelector('input[name=acsRadio][value=DYNAMIC]');
                    if(el) {
                        el.checked = true;
                        var submitBtn = frm.querySelector("input[type=submit]");
                        if(submitBtn){
                            __juspay.clickOnElement(submitBtn);
                        }
                    }
                    else {
                        Gatekeeper.removeFragment("DOM Changed!!! Fragment shutting down..");
                    }
                }
            },
            reachPasswordStage: function() {
                Gatekeeper.showWaitingFragment();
                var passOption = document.querySelector("input[type=radio][value=payerAuth]");//OTHER HDFC CREDIT/DEBIT Cards
                var pwdOption = document.querySelector("input[type=radio][value=adsReg]"); //HDFC DEBIT MASTERCARD
                if(passOption){
                    passOption.checked = true;
                }
                if(pwdOption){
                    pwdOption.checked = true;
                }
                var submitBtn = document.querySelector("input[type=submit][name=sbmtBtn]");
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        { //HDFC: OTP Page
            path: /\/ACSWeb\/jsp\/dynamicAuth.jsp\?(transType=payerAuth|ERROR=TRUE|transType=adsReg)/,
            hostname: "netsafe.hdfcbank.com",
            state:"UNUSED",
            local:false,
            bank:"HDFC",

            domCheck: function() {
                return (document.querySelector('input[name=cmdSubmit][value=Submit]')) &&
                       (document.getElementById('txtOtpPassword'))
            },

            action: function() {
                var salt;
                var tdArray = document.querySelectorAll('td')
                for(var i = tdArray.length-1; i >= 0; i--) {
                    if((/XXXX/).test(tdArray[i].innerText)) {
                        salt = tdArray[i].innerText;
                        break;
                    }
                }
                window.trackEventv2_0default("acs","info","masked_card",salt);
                __juspay.addOtpDecisionRecord(this.bank, "card", salt);
                var regenerateBtn = "";
                if(document.querySelector("input[name=otpBtnReGen][type=button]")){
                    regenerateBtn = "input[name=otpBtnReGen]";
                } else if(document.querySelector("a[id=otpBtnReGen]")){
                    regenerateBtn = "a[id=otpBtnReGen]";
                }
                var shouldShowOtpFragment = __juspay.retreiveOtpDecision(this.bank,"card",salt)
                var otpFragmentOptions = {};
                if(!shouldShowOtpFragment){
                    otpFragmentOptions["initState"] = "hidden"
                }
                else{
                    otpFragmentOptions["initState"] = "visible"
                }
                var otpField = document.getElementById('txtOtpPassword');
                var submitBtn = document.querySelector('input[name=cmdSubmit][value=Submit]');
                otpFragmentHelper("#txtOtpPassword",regenerateBtn);
                submitBtn.addEventListener("focus", function(){
                    window.trackEventv2_0default("acs","info","HDFC","SUBMIT_CLICKED");
                    Gatekeeper.hideAssistanceFragment();
                });
                if(otpField && __juspay.isMobUiEnabled("MOBUI_HDFC") && regenerateBtn){
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        var cancelBtn = document.querySelector('input[name=cmdSubmit][value=Cancel]');
                        var details = document.querySelector('#c-container > div.section-data');
                        var resend = document.querySelectorAll('a');
                        var info = document.querySelector('#c-container > div.section-desc');
                        var otpLabel = otpField.parentElement.parentElement.childNodes[1];
                        var foot_txt = document.querySelector('#c-container > div.col-md-12.timeOutMsg.paddingZero.notePara');
                        if(cancelBtn && resend && details && info && otpLabel && foot_txt){
                            submitBtn.style.marginLeft="0px";
                            details.style.paddingTop="10px";
                            otpLabel.textContent="Enter One Time Password";
                            otpLabel.style.textAlign= "left";
                            submitBtn.parentElement.style.textAlign= "right";
                            var err_msg = document.getElementsByClassName("errorType");
                            __juspay.modifyUI({"style_width":{"other":[0.9,otpField,submitBtn,otpLabel]},"style_height":{"other":[1/18,otpField,submitBtn,otpLabel]},"font_size":{"other":[12,otpLabel]},
                                                            "border":{"border_top":[details,err_msg[0]],"border_bottom":[err_msg[0]]},"display_inline_block":[details],"add_break_before":[cancelBtn],
                                                            "display_none":[cancelBtn,info,foot_txt],"swap_elements":[resend[0],cancelBtn]});
                            var auth_option_present = Gatekeeper.getSessionAttribute("hdfc_auth_both");
                            if(auth_option_present == "true"){
                                var toPass = "Switch To Password";
                                var master = document.querySelector('img[src="/ACSWeb/images/Mastercard_Securecode_bg.gif"]');
                                var visa = document.querySelector('img[src*="/ACSWeb/images/vbv_logo"]');
                                var switchDiv = document.createElement('div');
                                document.querySelectorAll('div')[9].appendChild(switchDiv);
                                if(master && switchDiv) {
                                    switchDiv.innerHTML=toPass.link("https://"+window.location.hostname + "/ACSWeb/jsp/SCode.jsp");
                                } else if(visa && switchDiv) {
                                    switchDiv.innerHTML=toPass.link("https://"+window.location.hostname + "/ACSWeb/jsp/PayerAuth.jsp");
                                }
                                switchDiv.style.float="left";
                                switchDiv.querySelector('a').classList.add('aHyper');
                                switchDiv.querySelector('a').addEventListener("click",function(){
                                    window.trackEventv2_0default("acs","info","MOBUI","Switch to Password");
                                });
                            }
                        }
                        window.trackEventv2_0default("acs","info","MOBUI","OTP_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                    }
                }
                __juspay.subscribe("juspay_context_changed",this);
            },

            notify: function(event) {
                if( event.type == "juspay_context_changed" ) {
                    var cancelBtn = document.querySelector('input[type=button][value=Cancel]');
                    var auth_option_present = Gatekeeper.getSessionAttribute("hdfc_auth_both");
                    if(cancelBtn && auth_option_present == "true" && __uber.isUberEnabled("HDFC_WRONG_OTP") ) {
                        cancelBtn.onclick = function() {
                            GK.setUberEventWithCallback("HDFC_WRONG_OTP", function(result) {
                                // Stop auto submission of otp
                                Gatekeeper.setShouldAutoSubmitOtp(false);
                                var master = document.querySelector('img[src="/ACSWeb/images/Mastercard_Securecode_bg.gif"]');
                                var visa = document.querySelector('img[src*="/ACSWeb/images/vbv_logo"]');
                                if( result == "ok" ) {
                                    if(master) {
                                        window.location.href = "https://"+window.location.hostname + "/ACSWeb/jsp/SCode.jsp";
                                    } else if(visa) {
                                        window.location.href = "https://"+window.location.hostname + "/ACSWeb/jsp/PayerAuth.jsp";
                                    }
                                } else if( result == "cancel" ) {
                                    document.frmDynamicAuth.action = "/ACSWeb/com.enstage.entransact.servers.AccessControlServerSSL?perform=DYNAMIC_AUTHENTICATION&CANCEL=true";
                                    document.frmDynamicAuth.submit();
                                } else if(result == "closed") {
                                    Gatekeeper.dismissUber();
                                }
                            },{type:"dialog",showOnLoad:true},null);
                        }
                    }
                }
            },

            backButtonPressed: function() {
                try {
                    var auth_option_present = Gatekeeper.getSessionAttribute("hdfc_auth_both");
                    if(auth_option_present != "true"){
                        GK.showCancelTransactionDialog();
                    }else if( __uber.isUberEnabled("HDFC_WRONG_OTP") ) {
                        GK.setUberEventWithCallback("HDFC_WRONG_OTP", function(result) {
                            // Stop auto submission of otp
                            Gatekeeper.setShouldAutoSubmitOtp(false);
                            var master = document.querySelector('img[src="/ACSWeb/images/Mastercard_Securecode_bg.gif"]');
                            var visa = document.querySelector('img[src*="/ACSWeb/images/vbv_logo"]');
                            if( result=="ok" ) {
                               Gatekeeper.addDataToSharedPrefs("HDFC_AUTH","");
                               window.trackEventv2_0default("acs","info","hdfc_nb","resetting_auto_auth");
                                if(master) {
                                    window.location.href= "https://"+window.location.hostname + "/ACSWeb/jsp/SCode.jsp";
                                } else if(visa) {
                                    window.location.href= "https://"+window.location.hostname + "/ACSWeb/jsp/PayerAuth.jsp";
                                }
                            } else if( result=="cancel" ) {
                                document.frmDynamicAuth.action = "/ACSWeb/com.enstage.entransact.servers.AccessControlServerSSL?perform=DYNAMIC_AUTHENTICATION&CANCEL=true";
                                document.frmDynamicAuth.submit();
                            } else if(result == "closed") {
                                Gatekeeper.dismissUber();
                            }
                        },{type:"dialog",showOnLoad:true,onPageLoad:"dismiss"},null);
                    } else {
                        GK.showCancelTransactionDialog();
                    }
                } catch(err) {
                    window.trackEventv2_0("acs","error","acs_js_error","Function: HDFC Wrong OTP backButtonPressed "+String(err),"acs","js_error");
                    GK.showCancelTransactionDialog();
                }
            },
            submitOtp: function(otp) {
                __juspay.changeOtpDecisionRecordToAuto();
                submitOtpHelper(otp, "input[name=txtOtpPassword]", "input[name=cmdSubmit][value=Submit]");
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector("input[name=otpBtnReGen][type=button]") || document.querySelector("a[id=otpBtnReGen]");
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        { //HDFC: OTP New Page added on 07/02/2018
            path: /\/ACSWeb\/authJsp\/(authImprovedHopsTxnPage.jsp|authImprovedTxnPage.jsp)/,
            hostname: "netsafe.hdfcbank.com",
            state:"UNUSED",
            local:false,
            bank:"HDFC",

            domCheck: function() {
                return document.querySelector('input[name=txtOtpPassword][id=txtOtpPassword]') &&
                       (document.querySelector('button[name=cmdSubmit][id=cmdSubmit]')||document.querySelector('button[name=cmdSubmitDynamic][id=cmdSubmitDynamic]'));
            },
            action: function(){
                var otpField = document.querySelector('input[name=txtOtpPassword][id=txtOtpPassword]');
                var submitButton1 = document.querySelector('button[name=cmdSubmit][id=cmdSubmit]');
                var submitButton2 = document.querySelector('button[name=cmdSubmitDynamic][id=cmdSubmitDynamic]');
                var passwordTab = document.querySelector('button[id=staticAuthOpen]');
                var otpTab = document.querySelector('button[id=dynamicAuthOpen]');
                var mastercard = document.querySelector('img[alt*="Mastercard"]');
                var visaCard = document.querySelector('img[alt="Verified by Visa"]');
                var regenerateBtn = document.querySelector('a[id=otpBtnReGen][class=resend]');
                __juspay.delayMe(function(){
                    if(otpField && otpTab.classList.contains("active")){
                        otpField.type='text';
                        if(Gatekeeper.getSessionAttribute("hdfc_otp_page_already_called")!==""){
                            Gatekeeper.setSessionAttribute("hdfc_otp_page_already_called", "");
                        }else{
                            Gatekeeper.setSessionAttribute("hdfc_otp_page_already_called", "true");
                            otpFragmentHelper('input[name=txtOtpPassword][id=txtOtpPassword]','a[id=otpBtnReGen][class=resend]');
                        }
                    }
                },200);
                var cancelButton = document.querySelector('a[onclick*=cancelAction]');
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                    }, false);
                }
                if(otpTab){
                    otpTab.addEventListener("click",function(){
                        __juspay.delayMe(function(){
                            if(Gatekeeper.getSessionAttribute("hdfc_otp_page_already_called")==""){
                                otpFragmentHelper('input[name=txtOtpPassword][id=txtOtpPassword]','a[id=otpBtnReGen][class=resend]');
                            }
                        },200);
                    },false);
                }
                if (submitButton1){
                    submitButton1.addEventListener("click",function(){
                        Gatekeeper.hideAssistanceFragment();
                    },false);
                }
                if (submitButton2){
                    submitButton2.addEventListener("click",function(){
                        Gatekeeper.hideAssistanceFragment();
                    },false);
                }
                if(passwordTab){
                    passwordTab.addEventListener("click",function(){
                        __juspay.trackPageStatus("INPUT_3DS");
                        __juspay.trackAuthMethod("OTP");
                        Gatekeeper.removeFragment("Selected Password Option");
                    },false);
                }
                if(regenerateBtn){
                    regenerateBtn.addEventListener("click",function(){
                       window.trackEventv2_0default("acs","info","HDFC","RESEND_BUTTON_CLICKED");
                    },false);
                }
                if(mastercard){
                    Gatekeeper.setCardBrand(MASTERCARD);
                }
                if(visaCard){
                    Gatekeeper.setCardBrand(VISA);
                }
                if(__juspay.isFeatureEnabled("HDFC_AUTO_RESEND_OTP")){
                    var submitButton2 = document.querySelector('button[name=cmdSubmitDynamic][id=cmdSubmitDynamic]');
                    var submitButton1 = document.querySelector('button[name=cmdSubmit][id=cmdSubmit]');
                    var resendotp = setTimeout(function(){
                        var otpField = document.querySelector('input[name=txtOtpPassword][id=txtOtpPassword]');
                        var activeOtpTab = document.querySelector('button[id=dynamicAuthOpen][class="tab-section active"]');
                        var regenerateBtn = document.querySelector('a[id=otpBtnReGen][class=resend]');
                        if(document.activeElement != otpField && activeOtpTab && regenerateBtn){
                            __juspay.clickOnElement(regenerateBtn);
                        }
                    },20000);
                    submitButton2.addEventListener('click', function(){
                        clearTimeout(resendotp);
                    });
                    submitButton1.addEventListener('click', function(){
                        clearTimeout(resendotp);
                    });
                }
                window.trackEventv2_0default("acs","info","HDFC","HDFC_new_page");
            },
            submitOtp: function(otp){
                var submitButton1 = document.querySelector('button[name=cmdSubmit][id=cmdSubmit]');
                var submitButton2 = document.querySelector('button[name=cmdSubmitDynamic][id=cmdSubmitDynamic]');
                if (submitButton1){
                    submitOtpHelper(otp, "input[name=txtOtpPassword][id=txtOtpPassword]", "button[name=cmdSubmit][id=cmdSubmit]");
                    Gatekeeper.removeFragment("OTP Submitted");
                }
                if (submitButton2){
                    submitOtpHelper(otp, "input[name=txtOtpPassword][id=txtOtpPassword]", "button[name=cmdSubmitDynamic][id=cmdSubmitDynamic]");
                    Gatekeeper.removeFragment("OTP Submitted");
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[id=otpBtnReGen][class=resend]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        { //HDFC: Intermediate page
            path: /\/ACSWeb\/jsp\/dynamicAuth.jsp/,
            hostname: "netsafe.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFC",
            domCheck: function() {
                return  document.querySelectorAll('input[name=otpBtn]')
            },

            action: function() {
                __juspay.trackPageStatus("CONFIRM_OTP_GENERATION");
                var generateBtn = document.querySelector('input[name=otpBtnGen]');
                if(generateBtn){
                    clickOnElementToResetTimer(generateBtn,true);
                    if(__uber.initSessionCounterIncremental("clickForOtpHdfc")<5){
                        __juspay.clickOnElement(generateBtn);
                    }
                }
            }
        },

        { //3DS password stage
            path: /\/ACSWeb\/jsp\/(SCode|PayerAuth).jsp/,
            hostname: "netsafe.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFC",
            domCheck: function() {
                return document.querySelector('input[name=cmdSubmit][value=Submit]') && document.getElementById('txtPassword')
            },

            action: function() {
                __juspay.trackInvalidScenarios("Invalid Password", "Invalid_Password");
                var salt;
                var tdArray = document.querySelectorAll('td')
                for(var i = tdArray.length-1; i >= 0; i--) {
                    if((/XXXX/).test(tdArray[i].innerText)) {
                        salt = tdArray[i].innerText;
                        break;
                    }
                }
                window.trackEventv2_0default("acs","info","masked_card",salt);
                var pwdField = document.getElementById('txtPassword');
                var submitBtn = document.querySelector('input[name=cmdSubmit][value=Submit]');
                submitBtn.addEventListener("click",function(){
                    window.trackEventv2_0default("acs","info","HDFC","SUBMIT_CLICKED");
                });
                var err_msg = document.getElementsByClassName("errorType");
                if(pwdField && __juspay.isMobUiEnabled("MOBUI_HDFC")){
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        var cancelBtn = document.querySelector('input[type=button][value=Cancel]');
                        var resend = document.querySelectorAll('a');
                        var details = document.querySelector('#c-container > div.section-data');
                        var info = document.querySelector('#c-container > div.section-desc');
                        var pwdLabel = document.querySelector('#c-container > div.section-data > ul.security-data > li.password-filed > label');
                        var nameLabel = document.querySelector('#c-container > div.section-data > ul.security-data > li:nth-child(1) > label');
                        if(cancelBtn && resend && details && info && pwdLabel && nameLabel && err_msg[0]){
                            submitBtn.style.marginLeft="0px";
                            details.style.paddingTop="10px";
                            nameLabel.style.textAlign ="left";
                            pwdLabel.style.textAlign ="left";
                            pwdField.parentElement.parentElement.childNodes[1].textContent="Enter Password";
                            resend[0].parentElement.innerHTML = '<div align="right"><a href="javascript:forgot_password()">Reset Password</a></div>';
                            var resend = document.querySelectorAll('a');
                            __juspay.modifyUI({"style_width":{"other":[0.9,submitBtn,pwdField]},"style_height":{"other":[1/18,submitBtn,pwdField]},
                                "border":{"border_top":[details,err_msg[0]]},"display_inline_block":[details],"add_break_before":[cancelBtn,nameLabel.parentElement.parentElement],
                                "display_none":[cancelBtn,info],"swap_elements":[resend[0].parentElement,cancelBtn]});
                            var auth_option_present = Gatekeeper.getSessionAttribute("hdfc_auth_both");
                            if(auth_option_present == "true"){
                                var switchToOtp = "Switch To OTP";
                                var switchDiv = document.createElement('div');
                                document.querySelectorAll('div')[7].appendChild(switchDiv);
                                switchDiv.innerHTML=switchToOtp.link("https://"+ window.location.hostname + "/ACSWeb/jsp/dynamicAuth.jsp?transType=payerAuth");
                                switchDiv.style.float="left";
                                switchDiv.style.position="relative";
                                switchDiv.style.top="-10px";
                                document.querySelectorAll('a')[0].style.position="relative";
                                document.querySelectorAll('a')[0].style.top="5px";
                                document.querySelectorAll('a')[0].style.textDecoration="underline";
                                document.querySelectorAll('a')[1].style.textDecoration="underline";
                                document.querySelectorAll('a')[0].style.color="navy";
                                document.querySelectorAll('a')[1].style.color="navy";
                                document.querySelectorAll('a')[1].addEventListener("click",function(){
                                    window.trackEventv2_0default("acs","info","MOBUI","Switch to OTP");
                                });
                             }
                            window.trackEventv2_0default("acs","info","MOBUI","HDFC_PWD_PAGE");
                        }
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"3DS_PWD_PAGE");
                    }
                }
                passwordFragmentHelper('#txtPassword');
                __juspay.subscribe("juspay_context_changed",this);
            },

            notify: function(event) {
                if(event.type == "juspay_context_changed") {
                    var cancelBtn = document.querySelector('input[type=button][value=Cancel]');
                    if( cancelBtn && __uber.isUberEnabled("HDFC_WRONG_PASSWORD") ) {
                        cancelBtn.onclick = function() {
                            GK.setUberEventWithCallback("HDFC_WRONG_PASSWORD",function(result) {
                                // Stop auto submission of otp
                                Gatekeeper.setShouldAutoSubmitOtp(false);
                                if( result == "ok" ) {
                                    window.location.href = "https://"+ window.location.hostname + "/ACSWeb/jsp/dynamicAuth.jsp?transType=payerAuth";
                                } else if( result == "cancel" ) {
                                    document.frmPayerAuth.action = "/ACSWeb/com.enstage.entransact.servers.AccessControlServerSSL?perform=USER_AUTH&CANCEL=true";
                                    document.frmPayerAuth.submit();
                                } else if(result == "closed") {
                                    Gatekeeper.dismissUber();
                                }
                            },{type:"dialog",showOnLoad:true},null);
                        }
                    }
                }
            },

            backButtonPressed: function() {
                try {
                    if( __uber.isUberEnabled("HDFC_WRONG_PASSWORD") ) {
                        GK.setUberEventWithCallback("HDFC_WRONG_PASSWORD",function(result) {
                            // Stop auto submission of otp
                            Gatekeeper.setShouldAutoSubmitOtp(false);
                            if( result=="ok" ) {
                                Gatekeeper.addDataToSharedPrefs("HDFC_AUTH","");
                                window.trackEventv2_0default("acs","info","hdfc_nb","resetting_auto_auth");
                                window.location.href = "https://"+ window.location.hostname + "/ACSWeb/jsp/dynamicAuth.jsp?transType=payerAuth";
                            } else if( result == "cancel" ) {
                                document.frmPayerAuth.action = "/ACSWeb/com.enstage.entransact.servers.AccessControlServerSSL?perform=USER_AUTH&CANCEL=true";
                                document.frmPayerAuth.submit();
                            } else if(result == "closed") {
                                Gatekeeper.dismissUber();
                            }
                        },{type:"dialog",showOnLoad:true,onPageLoad:"dismiss"},null);
                    } else {
                        GK.showCancelTransactionDialog();
                    }
                } catch(err) {
                    window.trackEventv2_0("acs","error","acs_js_error","Function:HDFC Wrong Password backButtonPressed "+String(err),"acs","js_error");
                    GK.showCancelTransactionDialog();
                }
            },

            showPassword: function() {
                var pwdField = document.getElementById('txtPassword');
                var passwordValue = pwdField.value;
                if(passwordValue != undefined && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue)
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector("input[name=cmdSubmit][type=Submit]");
                __juspay.clickOnElement(submitBtn);
            }
        },
        //HDFC- PayZapp Card
        {
            path: /\/acs-web-v4\/EnrollWeb\/HDFCBank\/server\/AccessControlServer/,
            hostname: "hdfc-acs.wibmo.com",
            state: "UNUSED",
            local: false,
            bank: "PAYZAPP",
            domCheck: function(){
                return (document.querySelector('input[id=txtPasswordtoDisplay][type=password]') || document.querySelector('input[id=txtPassword][type=password]')) &&
                       document.querySelector('input[id=cmdSubmit][value=Submit]') &&
                       document.querySelector('img[src*=PAYZAPP]');
            },
            action: function(){
                var passwordField1 = document.querySelector('input[id=txtPassword][type=password]');
                var passwordField2 = document.querySelector('input[id=txtPasswordtoDisplay][type=password]');
                if(passwordField1){
                    passwordFragmentHelper('input[id=txtPassword][type=password]');
                }
                if(passwordField2){
                    passwordFragmentHelper('input[id=txtPasswordtoDisplay][type=password]');
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[id=cmdSubmit][value=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //FSS ATMPIN support
        {
            path: /\/(pgway|pgwayc)\/gateway\/payment\/paymentDebitwithPin/,
            hostname: "securepg.fssnet.co.in",
            state: "UNUSED",
            bank: "FSS",
            domCheck: function(){
                return document.querySelector('input[id="Ecom_Payment_Pin"]') &&
                       document.querySelector('a[href*=openPinWin]');
            },
            action: function(){
                var atmPin = document.querySelector('input[id="Ecom_Payment_Pin"]');
                var helpLink = document.querySelector('a[href*=openPinWin]');

                var imageHeadHdfc = document.querySelector('img[src*=hdfc\\.gif]');
                var imageHeadCanara = document.querySelector('img[src*=CanaraB\\.jpg]');
                var imageHeadIobDC = document.querySelector('img[src*=IOB\\.JPG]')
                var imageHeadUbiDC = document.querySelector('img[src*=logo_ubi]');
                var imageHeadAndhDC = document.querySelector('img[src*=ABK]');
                var pinerror = document.querySelector('span[id=pinerror]');
                var submitBtn = document.querySelector('input[type = button][id=submit1]');

                if(imageHeadHdfc){
                    Gatekeeper.setBank("HDFC");
                }else if(imageHeadCanara){
                    Gatekeeper.setBank("CANARA")
                }else if(imageHeadIobDC){
                    Gatekeeper.setBank("IOBDC");
                }else if(imageHeadUbiDC){
                    Gatekeeper.setBank("UBIDC");
                }else if(imageHeadAndhDC){
                    Gatekeeper.setBank("ANDHDC");
                }
                if(helpLink){
                    helpLink.style.display = 'none'
                }
                if(submitBtn && pinerror){
                    submitBtn.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","user_error",pinerror.innerText, "user", "error");
                    },false);
                }
                if(atmPin) {
                    atmPin.blur();
                    atmPin.addEventListener("focus",function(){
                        Gatekeeper.requestPhoneKeyboardShow();
                    });
                    focusElement(atmPin);
                }
                if(imageHeadCanara||imageHeadHdfc||imageHeadIobDC||imageHeadUbiDC||imageHeadAndhDC){
                    __juspay.trackPageStatus("ATMPIN");
                    __juspay.trackAuthMethod("ATM_PIN");
                }else{
                    var imageHeadUrl = document.querySelector('img');
                    if(imageHeadUrl){
                        window.trackJsInfo("fss_bank_image_url", imageHeadUrl.src, "fss", "bank_image_url");
                    }
                }
            }
        },
        //FSS ATM PIN securepayments
        {
            path: /\/(pgwayb|ipay)\/paymentFramepage/,
            hostname: "securepayments.fssnet.co.in",
            state: "UNUSED",
            bank: "FSS",
            domCheck: function(){
                return document.querySelector('input[id="Ecom_Payment_Pin"]') &&
                       document.querySelector('a[href*=openPinWin]');
            },
            action: function(){
                var atmPin = document.querySelector('input[id="Ecom_Payment_Pin"]');
                var helpLink = document.querySelector('a[href*=openPinWin]');
                var imageHeadSbi = document.querySelector('img[src*=sbi]')
                var imageHeadAxis = document.querySelector('img[src*=axisbank]')
                var submitBtn = document.querySelector('img[name=submitBtnname][src*=submit-btn]');
                var pinerror = document.querySelector('span[id=pinerror]');
                if(imageHeadSbi){
                    Gatekeeper.setBank("SBIDC");
                }else if(imageHeadAxis){
                    Gatekeeper.setBank("AXIS");
                }
                if(submitBtn && pinerror && helpLink){
                    helpLink.style.display = 'none'
                    submitBtn.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","user_error",pinerror.innerText, "user", "error");
                    },false);
                }
                if(atmPin) {
                    atmPin.blur();
                    atmPin.addEventListener("focus",function(){
                        Gatekeeper.requestPhoneKeyboardShow();
                    });
                    focusElement(atmPin);
                }
                if(imageHeadSbi||imageHeadAxis){
                    __juspay.trackPageStatus("ATMPIN");
                    __juspay.trackAuthMethod("ATM_PIN");
                }else{
                    var imageHeadUrl = document.querySelector('img');
                    if(imageHeadUrl){
                        window.trackJsInfo("fss_bank_image_url", imageHeadUrl.src, "fss", "bank_image_url");
                    }
                }
            }
        },
        {
            path:/.*\/mc\/pagResponse\.html/,
            hostname: "221.135.135.32",
            state: "UNUSED",
            local: false,
            bank: "",
            domCheck: function(){
                return true;
            },
            action: function(){
                if(window.find("Your bill payment") && window.find("successful!")){
                     __juspay.trackPageStatus("PAYMENT_SUCCESS");
                }
            }
        },
         // Billdesk ATM PIN
        {
            path: /\/pgidsk\/ProcessPayment/,
            hostname: "www.billdesk.com",
            state: "UNUSED",
            local: false,
            bank: "ATMBD",
            domCheck: function() {
                var atmPinLabel = document.querySelector("label");
                if(atmPinLabel){
                   if(!(atmPinLabel.innerText == "Enter ATM Pin" || atmPinLabel.innerText == "Enter ATM PIN" )){
                        return false
                    }
                }
                return atmPinLabel &&
                       document.querySelector('input[type=password][name=customerpin]') &&
                       document.querySelector('button[name=btnSubmit][type=button]')
            },
            action: function(){
                var atmPinField = document.querySelector('input[type=password][name=customerpin]');
                var imageHeadSbi = document.querySelector('img[src*=sbi-logo]');
                var imageHeadPnb = document.querySelector('img[src*=png-logo][alt=PNB\\ Bank]')
                var imageHeadBoiDC = document.querySelector('img[alt = Bank\\ of\\ India]');
                if(imageHeadSbi){
                    Gatekeeper.setBank("SBIDC");
                }else if(imageHeadPnb){
                    Gatekeeper.setBank("PNBDC");
                }else if(imageHeadBoiDC){
                    Gatekeeper.setBank("BOIDC");
                }
                if(atmPinField){
                    atmPinField.blur();
                    atmPinField.addEventListener("focus",function(){
                        Gatekeeper.requestPhoneKeyboardShow();
                    });
                    focusElement(atmPinField);
                }
                if(imageHeadPnb || imageHeadSbi ||imageHeadBoiDC){
                    __juspay.trackPageStatus("ATMPIN");
                    __juspay.trackAuthMethod("ATM_PIN");
                }else{
                    var imageHeadUrl = document.querySelector('img');
                    if(imageHeadUrl){
                        window.trackJsInfo("billdesk_bank_image", imageHeadUrl.src,  "billdesk", "bank_image");
                    }
                }
            }
        },
        //AxisCard added on June7th 2017
        {
            path: /\/acs-web-axis\/EnrollWeb\/AxisBank\/server/,
            hostname: "secure.axisbank.com",
            state: "UNUSED",
            local: false,
            bank: "AXIS",
            domCheck: function(){
                return document.querySelector('li.reason a[onclick*=doSendOTP]') &&
                document.querySelector('li a[onclick*=showSelectedOption]') &&
                document.querySelector('input[id=txtPassword][name=txtPassword]') &&
                document.querySelector('input[id=otpValue][name=otpValue]') &&
                document.querySelector('div[class=resentOtp]>a[onclick*=doSendOTP]') &&
                document.querySelector('a[onclick*=forgot_password]') &&
                document.querySelector('div[id=static][class=Btn]>a') &&
                document.querySelector('div[id=otp]>a')
            },

            action: function(){
                var passwordOption = document.querySelector('li a[onclick*=showSelectedOption]');
                var passwordField = document.querySelector('input[id=txtPassword][name=txtPassword]');
                var otpField = document.querySelector('input[id=otpValue][name=otpValue]');
                var forgotPassword = document.querySelector('a[onclick*=forgot_password]');
                var submitBtn = document.querySelector('div[id=static][class=Btn]>a');
                var submitOtp = document.querySelector('div[id=otp]>a');
                var errorMsg = document.querySelector('div[id=errorMsgId]>span[id=errorMsg]');
                var otpBlock = document.querySelector('div[id=otpMsg]');
                var otpOption = document.querySelector('li.reason a[onclick*=doSendOTP]');
                var authCountPwd = GK.getAuthCountForPwd("AXIS_AUTH");
                var authCountOtp = GK.getAuthCountForOtp("AXIS_AUTH");
                if(__uber.initSessionCounterIncremental("AXIS_ACS_OPTION_SHOW") < 2){
                    __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                    Gatekeeper.showACSOptions();
                    Gatekeeper.addDataToSharedPrefs("AXIS_OTP_AUTO_CLICK",0);
                    Gatekeeper.addDataToSharedPrefs("AXIS_PWD_AUTO_CLICK",0);
                }
                if(errorMsg){
                    if(errorMsg.innerText == "System error. Please try after sometime or contact customer care."){
                        otpFragmentHelper("input[id=otpValue][name=otpValue]",'div[class=resentOtp]>a[onclick*=doSendOTP]');
                    }
                }
                if(otpBlock){
                    if(otpBlock.style.display == "block"){
                        otpFragmentHelper("input[id=otpValue][name=otpValue]",'div[class=resentOtp]>a[onclick*=doSendOTP]');
                    }
                }
                otpOption.addEventListener("click",function(){
                      authCountPwd=0;
                      Gatekeeper.addDataToSharedPrefs("AXIS_OTP_AUTO_CLICK",1);
                      Gatekeeper.addDataToSharedPrefs("AXIS_PWD_AUTO_CLICK",1);
                });
                if(Gatekeeper.isEnabled("isAutoAuthEnabledAxis")){
                    var storedLastAuth = Gatekeeper.getDataFromSharedPrefs("AXIS_AUTH");
                    var otpOptionCheck = Gatekeeper.getDataFromSharedPrefs("AXIS_OTP_AUTO_CLICK");
                    var pwdOptionCheck = Gatekeeper.getDataFromSharedPrefs("AXIS_PWD_AUTO_CLICK");
                    if(storedLastAuth){
                        if(authCountPwd > 2 && document.activeElement != passwordField && pwdOptionCheck!=1){
                            Gatekeeper.removeFragment("Auto_Auth_Option_Enabled");
                            window.trackEventv2_0default("acs","info","directly_moving_to_password_page_count",authCountPwd);
                             __juspay.clickOnElement(passwordOption);
                        }else if(authCountOtp > 2 && otpOptionCheck != 1){
                            Gatekeeper.removeFragment("Auto_Auth_Option_Enabled");
                            window.trackEventv2_0default("acs","info","directly_moving_to_otp_page_count",authCountOtp);
                            __juspay.clickOnElement(otpOption);
                        }
                    }
                }
                passwordOption.addEventListener("click",function() {
                    authCountOtp=0;
                    Gatekeeper.addDataToSharedPrefs("AXIS_PWD_AUTO_CLICK",1);
                    passwordFragmentHelper("input[id=txtPassword][name=txtPassword]");
                });
                if(!(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios")) {
                    passwordField.addEventListener("click",function() {
                        passwordFragmentHelper("input[id=txtPassword][name=txtPassword]");
                    });
                }
                submitBtn.addEventListener("click",function() {
                    blurElement(passwordField);
                    if(passwordField.value){
                        Gatekeeper.addDataToSharedPrefs("AXIS_AUTH","PWD_"+authCountPwd);
                        window.trackEventv2_0("acs","info","AXIS_AUTH","PWD_"+authCountPwd, "axis", "auth");
                    }
                });
                if (submitOtp){
                    submitOtp.addEventListener("click",function(){
                       blurElement(otpField);
                       if(otpField.value){
                           Gatekeeper.addDataToSharedPrefs("AXIS_AUTH","OTP_"+authCountOtp);
                           window.trackEventv2_0("acs","info","AXIS_AUTH","OTP_"+authCountOtp, "axis", "auth");
                       }
                       __juspay.delayMe(function(){
                           Gatekeeper.requestKeyboardHide();
                           Gatekeeper.hideBlur();
                      },200);
                    });
                }
                forgotPassword.addEventListener("click",function() {
                    Gatekeeper.removeFragment("Forgot password flow");
                });
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('div[id=static][class=Btn]>a');
                if(submitBtn){
                __juspay.clickOnElement(submitBtn);
                }
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=otpValue][name=otpValue]", "div[id=otp]>a");
                Gatekeeper.removeFragment("OTP submitted");
            },
            regenerateOtp: function() {
                var resendLink = document.querySelector('div[class=resentOtp]>a[onclick*=doSendOTP]');
                if(resendLink){
                    __juspay.clickOnElement(resendLink);
                }
            },
            reachOtpStage: function() {
                var otpOption = document.querySelector('a[id=t2_otp]');
                __juspay.clickOnElement(otpOption);
            },
            reachPasswordStage: function() {
                var passwordOption = document.querySelector('a[id=t1_static]');
                __juspay.clickOnElement(passwordOption);
            }
        },
        {
            path: /\/mpin\/mpin\.aspx/,
            hostname: "application.axisbank.co.in",
            state: "UNUSED",
            local: false,
            bank: "AXIS",
            domCheck: function(){
                return document.querySelector('input[id=txt_MPIN]') &&
                       document.querySelector('a[id=btnVBV_MSC]') &&
                       document.querySelector('a[id=btnotp]');

            },
            action: function(){
            	mpinField = document.querySelector('input[id=txt_MPIN]');
            	mpinTabActive = document.querySelector('a[class=active]').textContent;
            	otpTab = document.querySelector('a[id=btnotp]');
            	vbvTab = document.querySelector('a[id=btnVBV_MSC]')
                otpTab.addEventListener("click",function(){
                    Gatekeeper.removeFragment("Selected OTP Option");
                }, false);
                vbvTab.addEventListener("click",function(){
                    Gatekeeper.removeFragment("Selected VBV Option");
                }, false);
                mpinField.addEventListener("focus",function(){
                    Gatekeeper.requestNumericKeyboardShow();
                }, false);
            }
        },
        {
            path: /\/acs-web-axis\/EnrollWeb\/AxisBank\/server\/(OtpServer|AccessControlServer)/,
            hostname: /(secure|secure1)\.axisbank\.com/,
            state: "UNUSED",
            local: false,
            bank: "AXIS",
            domCheck: function(){
                return document.querySelector('input[id=otpValue]') &&
                       document.querySelector('a[onclick*=validate]') &&
                       document.querySelector('a[onclick*=resend_otp]');

            },
            action: function(){
                otpFragmentHelper('input[id=otpValue]','a[onclick*=resend_otp]');
                var registerOption = document.querySelector('a[onclick*=doRegister]');
                if(registerOption){
                   __juspay.trackPageStatus("OTP_AND_REGISTER_PAGE");
                }else{
                    window.trackEventv2_0default("acs","info","axis","ONLY_OTP_NEW_PAGE");
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,"input[id=otpValue]","a[onclick*=validate]");
            },
            regenerateOtp: function(){
                __juspay.clickOnElement(document.querySelector('a[onclick*=resend_otp]'));
            }
        },
        {   //register page
            path: /\/acs-web-axis\/EnrollWeb\/AxisBank\/ads/,
            hostname: /(secure|secure1)\.axisbank\.com/,
            state: "UNUSED",
            local: false,
            bank: "AXIS",
            domCheck: function(){
                return document.querySelector('div.cardPayment') &&
                       (document.querySelector('input[id=atmPin]') || document.querySelector('input[id=cvv2]'))
                       && document.querySelector('a[onclick*=validate]');
            },
            action: function(){
                __juspay.trackPageStatus("Register_First_Page");
                Gatekeeper.removeFragment("register page")
                var atmPin = document.querySelector('input[id=atmPin]');
                var cvv2 = document.querySelector('input[id=cvv2]');
                var email = document.querySelector('input[id=email]');
                if(atmPin)
                    attachAlphaNumericPasswordHelper(atmPin);
                else if (cvv2)
                    attachAlphaNumericPasswordHelper(cvv2);
                if(email){
                    email.addEventListener("focus",function(){
                        Gatekeeper.showNetbankingDefaultFragment();
                        Gatekeeper.changeNextActionText("Continue");
                        Gatekeeper.requestKeyboardShow();
                    });
                }
            },
            clickSubmitButton: function(){
                var email = document.querySelector('input[id=email]');
                var cvv2 = document.querySelector('input[id=cvv2]')
                if(email){
                    focusElement(email);
                }else if(cvv2){
                    focusElement(cvv2);
                }else{
                    __juspay.clickOnElement(document.querySelector('a[onclick*=validate]'));
                }
            },
            nextAction: function(){
                var email = document.querySelector('input[id=email]');
                 if(email && email.value)
                    __juspay.clickOnElement(document.querySelector('a[onclick*=validate]'));
                 else
                    focusElement();
            }
        },
        {   //only OTP
            path: /\/ACSWeb\/EnrollWeb\/AxisBank\/server\/AccessControlServer/,
            hostname: "secure.axisbank.com",
            state: "UNUSED",
            local: false,
            bank: "AXIS",
            domCheck: function() {
                return document.querySelector('input[type=password][name=otpValue]') &&
                       document.querySelector('input[type=Image][name=I1]');
            },
            action: function(){
                var submitBtn = document.querySelector('input[type=Image][name=I1]');
                otpFragmentHelper('input[name=otpValue]','a[href*=resend_otp]');
                submitBtn.addEventListener("focus", function(){
                    window.trackEventv2_0default("acs","info","AXIS","SUBMIT_CLICKED");
                    Gatekeeper.hideAssistanceFragment();
                });
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,"input[name=otpValue]","input[name=I1]");
            },
            regenerateOtp: function(){
                var regenerateBtn =  document.querySelector('a[href*=resend_otp]')
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //AXIS Card only password
        {
            path: /\/ACSWeb\/EnrollWeb\/AxisBank\/server\/AccessControlServer/,
            hostname: "secure.axisbank.com",
            state: "UNUSED",
            local: false,
            bank: "AXIS",
            domCheck: function() {
                return document.querySelector('input[id=password][name=txtPassword]') &&
                       document.querySelector('input[id=submitButton][name=cmdSubmit]');
            },
            action: function() {
                var vbv =  document.querySelector('img[src="../images/vbv_logo.jpg"]');
                var mastercard = document.querySelector('img[src="../images/Mastercard_Securecode_bg.gif"]');
                if(mastercard) {
                    Gatekeeper.setCardBrand(MASTERCARD);
                } else {
                    if(vbv){
                        Gatekeeper.setCardBrand(VISA);
                    }
                }
                passwordFragmentHelper("input[id=password][name=txtPassword]");
            },
            clickSubmitButton: function() {
                var submitBtn =  document.querySelector('input[id=submitButton][name=cmdSubmit]');
                var pwdField = document.querySelector('input[id=password][name=txtPassword]');
                if(submitBtn && pwdField.value) {
                    __juspay.clickOnElement(submitBtn);
                }else
                    pwdField.focus();
            },
            showPassword: function() {
                var pwdField = document.querySelector('input[id=password][name=txtPassword]');
                var passwordValue = pwdField.value;
                if(passwordValue != "undefined" && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue)
                }
            }
        },
        { //Axis dc new page// non-register for password
            path: /\/ACSWeb\/EnrollWeb\/AxisBank\/server\/(AccessControlServer|OtpServer\?perform=authGenerateOtp)/,
            hostname: "secure.axisbank.com",
            state: "UNUSED",
            local: false,
            bank: "AXIS",
            domCheck: function() {
                return document.querySelector('input[id="otpValue"]') &&
                       document.querySelector('input[id="cmdSubmit"]')&&
                       document.querySelector('input[data-id="auth_register"]') &&
                       !document.querySelector('input[data-id="static"]');
            },
            action: function() {
                Gatekeeper.setPollingForSmsEnabled(true);
                var otpField = document.querySelector('input[id="otpValue"]');
                var GenerateOtpButton = document.querySelector('a[onclick="doSendOTP();"]');
                var preGenOtpRadioBtn = document.querySelector('input[data-id="preGenerated_otp"]');
                var otpRadioBtn = document.querySelector('input[data-id="otp"]');
                var preGenOtpField = document.querySelector('input[id="pre_otpValue"]');
                if(otpRadioBtn){
                    otpRadioBtn.addEventListener("click",function(){
                        otpFragmentHelper('input[id="otpValue"]','a[onclick*=doSendOTP]');
                    }, false);
                    if(!otpRadioBtn.checked && typeof Gatekeeper.showACSOtpOption !="undefined" && typeof Gatekeeper.showACSOtpOption === 'function'){
                        Gatekeeper.showACSOtpOption();
                    }else{
                        __juspay.clickOnElement(otpRadioBtn);
                    }
                }
                //if  Pregenerated OTP option selected
                if(preGenOtpRadioBtn){
                    preGenOtpRadioBtn.addEventListener("click",function(){
                        Gatekeeper.removeFragment('Clicked PreGenerated OTP option');
                        Gatekeeper.showNetbankingDefaultFragment();
                        Gatekeeper.changeNextActionText("Pay");
                        otpTextFieldQuery("input[id=pre_otpValue]");
                        attachPhoneKeyboard(preGenOtpField);
                        preGenOtpField.focus();
                    }, false);
                }
                if(__juspay.isMobUiEnabled("MOBUI_AXIS")){
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        axisCardMobileOptimisation();
                        window.trackEventv2_0default("acs","info","MOBUI","PRE_GENERATE/OTP_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"PRE_GENERATE/OTP_PAGE");
                    }
                }
            },
            reachOtpStage: function(){
                var otpRadio = document.querySelector('input[data-id="otp"]');
                if(otpRadio) {
                    __juspay.clickOnElement(otpRadio);
                }
            },
            submitOtp: function(otp){
                Gatekeeper.showWaitingFragment();
                var otpField = document.querySelector('input[id="otpValue"]');
                if(otpField){
                    if(otp==="undefined" || typeof(otp)=="undefined") {
                        otp = "";
                    }
                    otpField.value = otp;
                    var submitBtn = document.querySelectorAll('input[id="cmdSubmit"]');
                    if(submitBtn[0])
                        __juspay.clickOnElement(submitBtn[0]);
                    else
                        window.trackEventv2_0default("acs","info","MOBUI","submit button not detected");
                }else
                    window.trackEventv2_0default("acs","info","MOBUI","Unable to detect otpField for autosubmit");
            },
            regenerateOtp: function() {
                var resendLink = document.querySelector('a[onclick*=doSendOTP]');
                if(resendLink){
                    __juspay.clickOnElement(resendLink);
                }
            },
            nextAction: function(){
                var submitBtn = document.querySelectorAll('input[id="cmdSubmit"]');
                if(submitBtn){
                     __juspay.clickOnElement(submitBtn[1]);
                }
            }
        },
        { //Axis dc page back from forgot password | otp,password, pregenerateOTP page
            path: /\/ACSWeb\/EnrollWeb\/(common|AxisBank)\/(auth|server)\/(AccessControlServer|OtpServer\?perform=authGenerateOtp|authenticate\.jsp)/,
            hostname: "secure.axisbank.com",
            state: "UNUSED",
            local: false,
            bank: "AXIS",
            domCheck: function() {
                return document.querySelector('input[id="otpValue"]') && document.querySelector('input[id="cmdSubmit"]')
                    && document.querySelector('input[data-id="static"]') && document.querySelector('input[data-id="preGenerated_otp"]') ;
            },
            action: function(){
                var otpField = document.querySelector('input[id="otpValue"]');
                var GenerateOtpButton = document.querySelector('a[onclick="doSendOTP();"]');
                var passwordField = document.querySelector('input[id="txtPassword"]');
                var preGenOtpRadioBtn = document.querySelector('input[data-id="preGenerated_otp"]');
                var pwdRadioBtn = document.querySelector('input[data-id="static"]');
                var otpRadioBtn = document.querySelector('input[data-id="otp"]');
                var preGenOtpField = document.querySelector('input[id="pre_otpValue"]');
                var sentMsg = document.querySelector('p.errorType span[id=errorMsg]');
                if(otpRadioBtn){
                    otpRadioBtn.addEventListener("click",function(){
                        if(otpField){
                            otpFragmentHelper("input[id=otpValue]","a[onclick*=doSendOTP]");
                        }
                    }, false);
                }
                if(pwdRadioBtn){
                    pwdRadioBtn.addEventListener("click",function(){
                        if(passwordField) {
                            Gatekeeper.addDataToSharedPrefs("AXIS_AUTH","");
                            passwordFragmentHelper('input[id=txtPassword]');
                        }
                    }, false);
                }
                if(preGenOtpRadioBtn){
                    preGenOtpRadioBtn.addEventListener("click",function(){
                        Gatekeeper.removeFragment('Clicked PreGenerated OTP option')
                        otpTextFieldQuery("input[id=pre_otpValue]");
                        attachPhoneKeyboard(preGenOtpField);
                        Gatekeeper.showNetbankingDefaultFragment();
                        Gatekeeper.changeNextActionText("PAY");
                        preGenOtpField.focus();
                    },false);
                }
                if(otpRadioBtn && !otpRadioBtn.checked && pwdRadioBtn){
                    if(passwordField) {
                        passwordFragmentHelper('input[id=txtPassword]');
                    }
                    __juspay.delayMe(function(){
                        Gatekeeper.requestKeyboardHide();
                        passwordField.blur();
                        Gatekeeper.showACSOptions();
                    },1000);
                }
                if(__juspay.isMobUiEnabled("MOBUI_AXIS")){
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        axisCardMobileOptimisation();
                        window.trackEventv2_0default("acs","info","MOBUI","PWD/OTP_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"PWD/OTP_PAGE");
                    }
                }
            },
            nextAction:function(){
                var submitBtn = document.querySelectorAll('input[id="cmdSubmit"]');
                if(submitBtn)
                    __juspay.clickOnElement(submitBtn[2]);
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelectorAll('input[id="cmdSubmit"]');
                if(submitBtn[0]){
                    __juspay.clickOnElement(submitBtn[0]);
                }
            },
            reachPasswordStage: function() {
                Gatekeeper.removeFragment("Reached Password Stage");
                var passRadio = document.querySelector('input[data-id="static"]');
                if(passRadio) {
                    __juspay.clickOnElement(passRadio);
                }
            },
            reachOtpStage: function() {
                var otpRadio = document.querySelector('input[data-id="otp"]');
                if(otpRadio) {
                    __juspay.clickOnElement(otpRadio);
                }
            },
            submitOtp: function(otp){
                Gatekeeper.showWaitingFragment();
                var otpField = document.querySelector('input[id="otpValue"]');
                if(otpField){
                    if(otp==="undefined" || typeof(otp)=="undefined") {
                        otp = "";
                    }
                    otpField.value = otp;
                    var submitBtn = document.querySelectorAll('input[id="cmdSubmit"]');
                    if(submitBtn[1])
                        __juspay.clickOnElement(submitBtn[1]);
                    else
                        window.trackEventv2_0default("acs","info","MOBUI","submit button not detected");
                }else
                    window.trackEventv2_0default("acs","info","MOBUI","Unable to detect otpField for autosubmit");
            },
            regenerateOtp: function() {
                var reSendOtp = document.querySelector('a[onclick="doSendOTP();"]');
                if(reSendOtp){
                    __juspay.clickOnElement(reSendOtp);
                }
            }
        },
        //KotakCC:
        {
            path: /\/acs-web-v29\/EnrollWeb\/KotakBank\/server\/(AccessControlServer|OtpServer|EnrollServer)/,
            hostname: "acs7.enstage-sas.com",
            state: "UNUSED",
            bank: "KOTAKCC",
            local: false,
            domCheck: function() {
                return document.querySelector('div[id="js-contantOtp"]') &&
                        document.querySelector('div[id="js-contantVbv"]') &&
                        document.querySelector('input[id="otpValue"]') &&
                        document.querySelector('input[id="txtPassword"]') &&
                        document.querySelector('button[id="cmdSubmit"]') &&
                        document.querySelector('a[onclick*="doSendOTP"]');
            },
            action: function(){
                var passwordPage = document.querySelector('div[id="js-contantOtp"][style="display: none;"]');
                var passwordPage1 = document.querySelector('div[id="js-contantOtp"][style="display: none"]');
                var otpPage = document.querySelector('div[id="js-contantVbv"][style="display: none"]');
                var otpField = document.querySelector('input[id="otpValue"]');
                var passwordField = document.querySelector('input[id="txtPassword"]');
                var visaCard = document.querySelector('img[alt="Verified by Visa"]');

                if(visaCard){
                    Gatekeeper.setCardBrand(VISA);
                }

                __juspay.delayMe(function(){
                    if((passwordPage || passwordPage1) && otpPage){
                        __juspay.trackPageStatus("choose_auth_options");
                        Gatekeeper.removeFragment("auth_selection_page");
                    }
                },200);

                if((passwordPage || passwordPage1) && passwordField){
                    __juspay.trackPageStatus("input_password");
                    __juspay.trackAuthMethod("PASSWORD");
                    passwordFragmentHelper('input[id="txtPassword"]');
                }

                if(otpPage && otpField){
                    __juspay.trackPageStatus("input_otp");
                    __juspay.trackAuthMethod("OTP");
                    otpFragmentHelper('input[id="otpValue"]','a[onclick*="doSendOTP"]');
                }
            },
            submitOtp: function(otp) {
                var otpField = document.querySelector('input[id="otpValue"]');
                var otpSubmitButton = document.querySelector('input[id="otpValue"]').nextElementSibling.firstElementChild;
                var submitButton = document.querySelectorAll('button[id="cmdSubmit"]')[0];
                if(otpSubmitButton === submitButton){
                    otpField.value=otp;
                    try{
                        __juspay.clickOnElement(otpSubmitButton);
                    }catch(err){
                        Gatekeeper.removeFragment('Submit Button Dom Changed - Please Check');
                    }
                }
            },
            clickSubmitButton: function(){
                var passwordSubmitButton = document.querySelector('input[id="txtPassword"]').nextElementSibling.firstElementChild;
                var submitButton = document.querySelectorAll('button[id="cmdSubmit"]')[1];
                if(passwordSubmitButton === submitButton){
                    __juspay.clickOnElement(passwordSubmitButton);
                }
            },
            regenerateOtp: function() {
                var resendLink = document.querySelector('a[onclick*="doSendOTP"]');
                if(resendLink){
                    __juspay.clickOnElement(resendLink);
                }
            }
        },
        //KotakCC:
        {
           path: /\/(acs-web-v2|ACSWeb)\/EnrollWeb\/KotakBank\/server\/(AccessControlServer|OtpServer)/,
           hostname: "acs7.enstage-sas.com",
           state: "UNUSED",
           bank: "KOTAKCC",
           local: false,
           domCheck: function() {
               var returnValue = false;
               var creditCardCheck = document.querySelector('tr[class=merchantNameCreditTableHeader]');
               if(creditCardCheck) {
                   returnValue = true;
               }
               return (document.querySelector('input[id=txtPassword]') ||
                      (document.querySelector('input[id=txnAuthTypeOtp][type=radio]') &&
                        (document.querySelector('input[id=txnAuthTypePswd][type=radio]') ||
                          document.querySelector('input[id=txnAuthTypeRegister][type=radio]')))) &&
                      returnValue
           },
           action: function(){
               Gatekeeper.removeFragment("KOTAKCC page reached");
               var passRadio1 = document.querySelector('input[id=txnAuthTypePswd][type=radio]');
               var passRadio2 =document.querySelector('input[id=txnAuthTypeRegister][type=radio]');
               var otpRadio = document.querySelector('input[id=txnAuthTypeOtp][type=radio]');
               var passwordField = document.querySelector('input[id=txtPassword][type=password]');
               var otpField = document.querySelector("input[name=otpValue]");
               var resendLink = document.querySelector('a[onclick*=doSendOTP]');
               var visaCard = document.querySelector('img[alt="Verified by Visa"]');
               if(visaCard){
                    Gatekeeper.setCardBrand(VISA);
               }
               //some cards have only static password option
               if(passwordField && !(passRadio1 || passRadio2)){
                    passwordField.blur();
                    attachAlphaNumericPasswordHelper(passwordField);
                    focusElement(passwordField);
                    __juspay.trackPageStatus("INPUT_3DS")
                    __juspay.trackAuthMethod("OTP")
                    window.trackEventv2_0default("acs","info","kotakcc","only_password");
               }
               if((passRadio1 || passRadio2) && otpRadio){
                   if(__uber.initSessionCounterIncremental("KOTAK_ACS_OPTION_SHOW") < 2){
                       __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                       Gatekeeper.showACSOptions();
                   }
               }
               if(otpRadio){
                   otpRadio.addEventListener("click",function() {
                       otpFragmentHelper("input[name=otpValue]",'a[onclick*=doSendOTP]');
                   },false);
               }
               if(passRadio1){
                   passRadio1.addEventListener("click",function() {
                       passwordFragmentHelper("input[id=txtPassword][type=password]");
                   },false);
               }
               if(passRadio2){
                   passRadio2.addEventListener("click",function() {
                       passwordFragmentHelper("input[id=txtPassword][type=password]");
                   },false);
               }
               if(otpField && otpRadio.checked==true){
                   otpFragmentHelper("input[name=otpValue]",'a[onclick*=doSendOTP]');
               }
           },
           reachPasswordStage: function() {
               Gatekeeper.removeFragment("Reached Password Stage");
               var passRadio1 = document.querySelector('input[id=txnAuthTypePswd][type=radio]');
               var passRadio2 =document.querySelector('input[id=txnAuthTypeRegister][type=radio]');
               if(passRadio1) {
                   __juspay.clickOnElement(passRadio1);
               }
               if(passRadio2) {
                   __juspay.clickOnElement(passRadio2);
               }
           },
           reachOtpStage: function() {
               var otpRadio = document.querySelector('input[id=txnAuthTypeOtp][type=radio]');
               if(otpRadio) {
                   __juspay.clickOnElement(otpRadio);
               }
           },
           clickSubmitButton: function(){
               var submitBtn = document.querySelector('input[name=cmdSubmit][value=Submit]');
               if(submitBtn){
                   __juspay.clickOnElement(submitBtn);
               }
           },
           submitOtp: function(otp) {
               submitOtpHelper(otp, "input[name=otpValue]", "input[name=cmdSubmit][value=Submit]");
           },
           regenerateOtp: function() {
               var resendLink = document.querySelector('a[onclick*=doSendOTP]');
               if(resendLink){
                   __juspay.clickOnElement(resendLink);
               }
           }
        },
        //KOTAKDC Bank OTP Page
        {
            path: /\/v1\/acs\/legacy\/live/,
            hostname: /(secure-acs2ui-b1-indblr-blrtdc|secure-acs2ui-b1-indmum-mumrdc)\.wibmo\.com/,
            state: "UNUSED",
            bank: "KOTAKDC",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=otpValue]') &&
                      document.querySelector('a[id=submitBtn]') &&
                      document.querySelector('a[id=reSend]') &&
                      document.querySelector('img[src*="kotdb_logo"]');
            },
            action: function()
            {
                otpFragmentHelper('input[id=otpValue]','a[id=reSend]');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=otpValue]", "a[id=submitBtn]");
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('a[id=reSend]');
                __juspay.clickOnElement(resendLink);
            }
        },
        { //KOTAKDC new page added on aug 2020
        	path: /\/acs-v1\/processauth/,
        	hostname: /(b2-dw-sdc|b2-dw-pdc)\.enstage-sas\.com/,
        	state: "UNUSED",
        	bank: "KOTAKDC",
        	local: false,
        	domCheck: function() {
        	   return document.querySelector('a[id=submitBtn][type=submit]')&&
        	          document.querySelector('input[id=otpValue][type=password]')&&
        	          document.querySelector('a[class=resend]') &&
        	          document.querySelector('img[src*=debit_logo]');
        	},
        	action: function(){
                otpFragmentHelper('input[id=otpValue][type=password]','a[class=resend]');
        	},
        	submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=otpValue][type=password]", "a[id=submitBtn][type=submit]");
        	},
        	regenerateOtp: function() {
        		var resendLink = document.querySelector('a[class=resend]')
        		if(resendLink){
        		   __juspay.clickOnElement(resendLink);
        		}
        	}
        },
        //Kotak DC
        {
           path: /\/(acs-web-v2|ACSWeb)\/EnrollWeb\/KotakBank\/server\/(AccessControlServer|OtpServer)/,
           hostname: /(acs7|b2-kotak-sdc|b2-kotak-pdc)\.enstage-sas\.com/,
           state: "UNUSED",
           bank: "KOTAKDC",
           local: false,
           domCheck: function() {
               return (document.querySelector('input[name=cmdSubmit][value=Submit]') || document.querySelector('input[type=image][src*=btn_submit]')) &&
                      (document.querySelector('input[id=txtOtp][type=password]') || document.querySelector('input[id=otpValue][type=password]'))&&
                      (document.querySelector('a[id=resendOtpId][href*=reSendOtp]') || document.querySelector('a[href*=resend_otp]')) &&
                      document.querySelector('img[src*=debit_logo]');
           },
           action: function(){
                var otpField1 = document.querySelector('input[id=txtOtp][type=password]');
                var otpField2 = document.querySelector('input[id=otpValue][type=password]');
                var submitBtn1 =document.querySelector('input[name=cmdSubmit][value=Submit]');
                var submitBtn2 = document.querySelector('input[type=image][src*=btn_submit]');
                if(otpField1 && submitBtn1){
                    otpFragmentHelper('input[id=txtOtp]','a[id=resendOtpId][href*=reSendOtp]');
                }
                if(otpField2 && submitBtn2){
                    otpFragmentHelper('input[id=otpValue][type=password]','a[href*=resend_otp]');
                }
                var visaCard = document.querySelector('img[alt="Verified by Visa"]');
                if(visaCard){
                   Gatekeeper.setCardBrand(VISA);
                }
                var cancelButton = document.querySelector('img[src*=btn_cancel]');
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0default("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED");
                    }, false);
                }
           },
           submitOtp: function(otp) {
                var otpField1 = document.querySelector('input[id=txtOtp][type=password]');
                var otpField2 = document.querySelector('input[id=otpValue][type=password]');
                var submitBtn1 =document.querySelector('input[name=cmdSubmit][value=Submit]');
                var submitBtn2 = document.querySelector('input[type=image][src*=btn_submit]');
                if (otpField1 && submitBtn1){
                    submitOtpHelper(otp, "input[id=txtOtp]", "input[name=cmdSubmit][value=Submit]");
                }
                if (otpField2 && submitBtn2){
                    submitOtpHelper(otp, "input[id=otpValue][type=password]", "input[type=image][src*=btn_submit]");
                }
           },
           regenerateOtp: function() {
               var resendLink1 = document.querySelector('a[id=resendOtpId][href*=reSendOtp]');
               var resendLink2 = document.querySelector('a[href*=resend_otp]');
               if(resendLink1){
                   __juspay.clickOnElement(resendLink1);
               }
               if(resendLink2){
                  __juspay.clickOnElement(resendLink2);
               }
           }
        },
        //KOTAKCC Bank OTP Page
        {
            path: /\/v1\/acs\/legacy\/live/,
            hostname: /(secure-acs2ui-b1-indblr-blrtdc|secure-acs2ui-b1-indmum-mumrdc)\.wibmo\.com/,
            state: "UNUSED",
            bank: "KOTAKCC",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=otpValue]') &&
                      document.querySelector('a[onclick*=submitOTPform][class=prim__btn]') &&
                      document.querySelector('a[id=resendOtpform]') &&
                      document.querySelector('img[src*="kotcb_logo"]');
            },
            action: function()
            {
                otpFragmentHelper('input[id=otpValue]','a[id=resendOtpform]');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=otpValue]", "a[onclick*=submitOTPform][class=prim__btn]");
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('a[id=resendOtpform]');
                __juspay.clickOnElement(resendLink);
            }
        },
        {
            path:  /\/pay/,
            hostname: "pay.cred.club",
            state: "UNUSED",
            local: false,
            bank: "credlogin",
            domCheck: function(){
                return true;
            },
            action: function(){
                var otpFieldChecker = setInterval(function(){
                    var otpField = document.querySelector('input[inputmode="numeric"][autocomplete="one-time-code"]');
                    var submitButtonText = document.querySelector('button').innerText;
                    if(otpField){
                        clearInterval(otpFieldChecker);
                        if(submitButtonText == "Verify and Pay"){
                            otpFragmentHelper('input[inputmode="numeric"][autocomplete="one-time-code"]','span[class="sc-cTkxnA hMyOYf"]');
                        }
                    }
                },500)
            },
            submitOtp: function(otp){
                var otpField = document.querySelector('input[inputmode="numeric"][autocomplete="one-time-code"]');
                var submitButton = document.querySelector('button');
                var nativeInputValueSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, "value").set;
        		nativeInputValueSetter.call(otpField, otp);
        		var ev2 = new Event('input', { bubbles: true});
        		otpField.dispatchEvent(ev2);
        		submitButton.click();
        		Gatekeeper.removeFragment("OTP Submitted");
            },
            regenerateOtp: function(){
        		var regenerateBtn =  document.querySelector('span[class="sc-cTkxnA hMyOYf"]');
        		if(regenerateBtn){
        			__juspay.clickOnElement(regenerateBtn);
        		}
            }
        },
        {
            path:  /.*/,
            hostname: "credpay-stg.dreamplug.in",
            state: "UNUSED",
            local: false,
            bank: "credlogin",
            domCheck: function(){
                return true;
            },
            action: function(){
                var otpFieldChecker = setInterval(function(){
                    var otpField = document.querySelector('input[inputmode="numeric"][class="sc-khAkCZ bKtMhj"]');
                    if(otpField){
                        clearInterval(otpFieldChecker);
                        otpFragmentHelper('input[inputmode="numeric"][class="sc-khAkCZ bKtMhj"]','span[class="sc-gTgzoy fNQAnA"]');
                    }
                },500)
            },
            submitOtp: function(otp){
                var otpField = document.querySelector('input[inputmode="numeric"][class="sc-khAkCZ bKtMhj"]');
                var submitButton = document.querySelector('button[class="sc-dQoVA htuLvP"]');
                var nativeInputValueSetter = Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, "value").set;
        		nativeInputValueSetter.call(otpField, otp);
        		var ev2 = new Event('input', { bubbles: true});
        		otpField.dispatchEvent(ev2);
        		submitButton.click();
        		Gatekeeper.removeFragment("OTP Submitted");
            },
            regenerateOtp: function(){
        		var regenerateBtn =  document.querySelector('span[class="sc-gTgzoy fNQAnA"]');
        		if(regenerateBtn){
        			__juspay.clickOnElement(regenerateBtn);
        		}
            }
        },
//        //ICICIDC Payu secure new page
//        {
//           path: /\/payuAuth/,
//           hostname: "secure.payu.in",
//           state: "UNUSED",
//           local: false,
//           bank: "ICICIDC",
//           domCheck: function(){
//               return document.querySelector('input[id=otp][type=password][name=otp]') &&
//                        document.querySelector('input[id=onusOtpBtn][value=Submit]') &&
//                        document.querySelector('p[id=otpRequest] > a[id=requestOtpLink]');
//           },
//           action: function(){
//               otpFragmentHelper('input[id=otp][type=password][name=otp]','p[id=otpRequest] > a[id=requestOtpLink]');
//           },
//           submitOtp: function(otp){
//               submitOtpHelper(otp, 'input[id=otp][type=password][name=otp]', 'input[id=onusOtpBtn][value=Submit]');
//               Gatekeeper.removeFragment("OTP Submitted");
//           },
//           regenerateOtp: function(){
//               var regenerateBtn = document.querySelector('p[id=otpRequest] > a[id=requestOtpLink]')
//               if(regenerateBtn){
//                   __juspay.clickOnElement(regenerateBtn);
//               }
//           }
//        },
        /* icici credit card powered by amex otp page*/
        //wibmo OTP Page
        {
            path: /\/v1\/acs\/legacy\/live/,
            hostname: /(secure-acs2ui-b1-indblr-blrtdc|secure-acs2ui-b1-indmum-mumrdc)\.wibmo\.com/,
            state: "UNUSED",
            bank: "",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=otpValue]') &&
                      (document.querySelector('a[id=submitBtn]') || document.querySelector('input[id=submitBtn]')) &&
                      (document.querySelector('a[id=reSend]') || document.querySelector('a[href*=resend_otp]'));
            },
            action: function()
            {
                var fedLogo = document.querySelector('img[src*="fedbk_logo"]');
                var kotakLogo = document.querySelector('img[src*="kotdb_logo"]');
                var bobLogo = document.querySelector('img[src*="bobdb_logo"]');
                var resendBtn1 = document.querySelector('a[id=reSend]');
                var resendBtn2 = document.querySelector('a[href*=resend_otp]');
                var bankName = "";
                if(fedLogo){
                    bankName = "FEDDC";
                }else if(kotakLogo){
                    bankName = "KOTAKDC";
                }else if(bobLogo){
                    bankName = "BOBDC";
                }
                if(bankName !== "") {
                    Gatekeeper.setBank(bankName);
                    if(resendBtn1){
                        otpFragmentHelper('input[id=otpValue]','a[id=reSend]');
                    }
                    if(resendBtn2){
                        otpFragmentHelper('input[id=otpValue]','a[href*=resend_otp]');
                    }
                }
            },
            submitOtp: function(otp) {
                var submitBtn1 = document.querySelector('a[id=submitBtn]');
                var submitBtn2 = document.querySelector('input[id=submitBtn]');
                if(submitBtn1){
                    submitOtpHelper(otp, "input[id=otpValue]", "a[id=submitBtn]");
                }
                if(submitBtn2){
                    submitOtpHelper(otp, "input[id=otpValue]", "input[id=submitBtn]");
                }
            },
            regenerateOtp: function(){
                var resendBtn1 = document.querySelector('a[id=reSend]');
                var resendBtn2 = document.querySelector('a[href*=resend_otp]');
                if(resendBtn1){
                    __juspay.clickOnElement(resendBtn1);
                }
                if(resendBtn2){
                    __juspay.clickOnElement(resendBtn2);
                }
            }
        },
        //Payu OTP Page
        {
            path: /\/payuAuth/,
            hostname: "secure.payu.in",
            state: "UNUSED",
            local: false,
            bank: "",
            domCheck: function(){
              return document.querySelector('input[id=otp][type=password][name=otp]') &&
                       document.querySelector('input[id=onusOtpBtn][value=Submit]') &&
                       document.querySelector('p[id=otpRequest] > a[id=requestOtpLink]');
            },
            action: function(){
                var fedLogo = document.querySelector('img[src*="fed_logo.png"]');
                var canaraLogo = document.querySelector('img[src*="cana_logo.png"]');
                var iciciLogo = document.querySelector('img[src*="icici-bank-logo"]');
                var rblLogo = document.querySelector('img[src*="rbl_logo"]');
                var bankName = "";
                if(fedLogo){
                    bankName = "FEDDC";
                }else if(canaraLogo){
                    bankName = "CANARADC";
                }else if(iciciLogo){
                    bankName = "ICICIDC";
                }else if(rblLogo){
                    bankName = "RBLDC";
                }
                if(bankName !== "") {
                    Gatekeeper.setBank(bankName);
                    otpFragmentHelper('input[id=otp][type=password][name=otp]','p[id=otpRequest] > a[id=requestOtpLink]');
                }
            },
            submitOtp: function(otp){
              submitOtpHelper(otp, 'input[id=otp][type=password][name=otp]', 'input[id=onusOtpBtn][value=Submit]');
              Gatekeeper.removeFragment("OTP Submitted");
            },
            regenerateOtp: function(){
              var regenerateBtn = document.querySelector('p[id=otpRequest] > a[id=requestOtpLink]')
              if(regenerateBtn){
                  __juspay.clickOnElement(regenerateBtn);
              }
            }
        },
        //Paytm OTP Page
        {
            path: /\/instaproxy\/directbank/,
            hostname: "securegw.paytm.in",
            state: "UNUSED",
            local: false,
            bank: "",
            domCheck: function(){
              return document.querySelector('input[id=otp][class=input-otp]') &&
                       document.querySelector('button[id=pay][class=pay-btn]') &&
                       document.querySelector('a[id=resendOTP]') &&
                       document.querySelector('img[id=footerImg][src*=paytm]') &&
                       document.querySelector('span[id=poweredBy]');
            },
            action: function(){
                var hdfcLogo = document.querySelector('img[src*="HDFC"]');
                var canaraLogo = document.querySelector('img[src*="CANARA"]');
                var bankName = "";
                if(fedLogo){
                    bankName = "HDFC";
                }else if(canaraLogo){
                    bankName = "CANARADC";
                }
                if(bankName !== "") {
                    Gatekeeper.setBank(bankName);
                    otpFragmentHelper('input[id=otp][class=input-otp]','a[id=resendOTP]');
                }
            },
            submitOtp: function(otp){
              submitOtpHelper(otp, 'input[id=otp][class=input-otp]', 'button[id=pay][class=pay-btn]');
              Gatekeeper.removeFragment("OTP Submitted");
            },
            regenerateOtp: function(){
              var regenerateBtn = document.querySelector('a[id=resendOTP]')
              if(regenerateBtn){
                  __juspay.clickOnElement(regenerateBtn);
              }
            }
        },
        //RUPAY OTP Page
        {
            path: /\/PG\/tranRupay/,
            hostname: "hdfcbankpayments.hdfcbank.com",
            state: "UNUSED",
            bank: "RUPAY",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=otp][name=otp]') &&
                        document.querySelector('img[src*="paypage-images/rupay"]') &&
                        document.querySelector('button[id=submitBtnname]');
            },
            action: function()
            {
                otpFragmentHelper('input[id=otp][name=otp]','');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=otp][name=otp]", "button[id=submitBtnname]");
            }
        },
        {
            path: /\/ACSWeb\/EnrollWeb\/ICICIBank\/(server|ads)\/(AccessControlServer|ads2_amex|OtpServer)/,
            hostname: "www.3dsecure.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICICC",
            domCheck: function() {
                return document.querySelector('input[id="txtAutoOtp"]') &&
                        document.querySelector('a[href*="resend_otp"]') &&
                        document.querySelector('input[src*="btn_submit"]');
            },
            action: function(){
                otpFragmentHelper('input[id="txtAutoOtp"]','a[href*="resend_otp"]');
                var amex = document.querySelector('a[href*="ICICIBank/ads/ads2_amex"]');
                var visa = document.querySelector('img[src*="vbv_logo"]');
                if(visa){
                    Gatekeeper.setCardBrand(VISA);
                }
                if(amex){
                    Gatekeeper.setCardBrand(AMEX);
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id="txtAutoOtp"]', 'input[src*="btn_submit"]');
                Gatekeeper.removeFragment("OTP Submitted");
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[href*="resend_otp"]');
                __juspay.clickOnElement(regenerateBtn);
            }
        },
        /* icici credit card powered by amex */
        {
            path: /\/acspage\/cap\?/,
            hostname: "acs.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICICC",
            domCheck: function() {
                var mobileOptionPage = document.querySelector('div[id="pwdbaseotppage"]');
                if (mobileOptionPage!=null){
                    return document.querySelector('input[value="FromMob"]') &&
                        document.querySelector('input[type="submit"][name="submitval"]') &&
                        document.querySelector('div[id="pwdbaseotppage"]').style.display=="inline" &&
                        document.querySelector('img[src*=ICICIAmex]');
                }
            },
            action: function(){
                var mobileOption = document.querySelector('input[value="FromMob"]');
                var submitButton = document.querySelectorAll('input[type="submit"][name="submitval"]')[1];
                __juspay.trackPageStatus("ICICICC_AMEX_PAGE");
                if(mobileOption && submitButton!=null){
                    mobileOption.checked = true;
                    Gatekeeper.showNetbankingDefaultFragment();
                    if(__juspay.isFeatureEnabled("ICICICC_AUTO_CLICK")) {
                        __juspay.clickOnElement(submitButton);
                    }
                }
            },
            nextAction: function() {
                var submitButton = document.querySelectorAll('input[type="submit"][name="submitval"]')[1];
                if(submitButton!=null) {
                    __juspay.clickOnElement(submitButton);
                    Gatekeeper.removeFragment("Navigating to OTP Page");
                }
            }
        },
        /* icici credit card powered by amex otp page*/
        {
            path: /\/acspage\/cap\?/,
            hostname: "acs.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICICC",
            domCheck: function() {
                var otpOptionPage = document.querySelector('div[id="PASSWDPAGE"]');
                if (otpOptionPage!=null){
                    return document.querySelector('input[type="password"][id="txtAutoOtp"]') &&
                        document.querySelector('input[type="submit"][name="submitval"]') &&
                        document.querySelector('div[id="PASSWDPAGE"]').style.display=="inline" &&
                        document.querySelector('img[src*=ICICIAmex]');
                }
            },
            action: function(){
                var otpField = document.querySelector('input[type="password"][id="txtAutoOtp"]');
                var submitButton = document.querySelector('input[type="submit"][name="submitval"]');
                if(otpField && submitButton){
                    otpFragmentHelper('input[type="password"][id="txtAutoOtp"]','a[title="Resend OTP"]');
                }
            },
            submitOtp: function(otp){
                var submitButton = document.querySelector('input[type="submit"][name="submitval"]');
                if (submitButton){
                    submitOtpHelper(otp, "input[type=password][id=txtAutoOtp]", "input[type=submit][name=submitval]");
                    Gatekeeper.removeFragment("OTP Submitted");
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[title="Resend OTP"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //ICICIDC new page support added on 28/02/19
        {
        	path: /\/ACSWeb\/EnrollWeb\/ICICIBank\/(server|auth|ads)\/(AccessControlServer|VBV|SCode|ads2)/,
        	hostname: /(acs|acs1|acs2)\.icicibank\.com/,
        	state: "UNUSED",
        	local: false,
        	bank: "ICICIDC",
        	domCheck: function(){
        	  return (document.querySelector('input[id=txtAutoOtp]')) &&
        	          (document.querySelector('input[src*=btn_submit]') || document.querySelector('input[id=cmdSubmit]')) &&
        	          document.querySelector('a[href*=resend_otp]');
        	},
        	action: function(){
        	  var otpField = document.querySelector('input[id=txtAutoOtp]');
        	  otpFragmentHelper('input[id=txtAutoOtp]','a[href*=resend_otp]');
        	  var cancelButton = document.querySelector('img[src*=btn_cancel]');
        	  if(cancelButton){
        	      cancelButton.addEventListener("click",function(){
        	          window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
        	      }, false);
        	  }
        	},
        	submitOtp: function(otp){
        	  var submitButton1 = document.querySelector('input[src*=btn_submit]');
        	  var submitButton2 = document.querySelector('input[id=cmdSubmit]');
        	  if(submitButton1){
        	    submitOtpHelper(otp, 'input[id=txtAutoOtp]', 'input[src*=btn_submit]');
        	  }
        	  if(submitButton2){
        	    submitOtpHelper(otp, 'input[id=txtAutoOtp]', 'input[id=cmdSubmit]');
        	  }
        	},
        	regenerateOtp: function(){
        	  var regenerateBtn = document.querySelector('a[href*=resend_otp]');
        	  if(regenerateBtn){
        	      __juspay.clickOnElement(regenerateBtn);
        	  }
        	}
        },
         // ICICIDC: 3DS page
        {
            path: /\/acspage\/cap\?/,
            hostname: "acs.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICIDC",
            domCheck: function() {
                return document.querySelector('div[id="PASSWDPAGE"]') &&
                        document.querySelector('div[id="PASSWDPAGE"]').style.display=="inline" &&     // Form unique elements
                        (document.querySelector('input[id="enterPASS"]') && document.querySelector('form[name=passwdForm]') &&
                        document.querySelector('button[id=sendotp][type=submit]'));    // Input Dom Elements
            },
            action: function() {
                var passwordField = document.querySelector('input[id="enterPASS"]');
                var submitButton = document.querySelector('button[id=sendotp][type=submit]');
                var channelSelectOption = document.querySelector('a[title="Click Here)"][href*=showChannelSelectPage]');
                var mobileOption = document.querySelector('input[name="otpDestinationOption"][value="FromMob"]');
                var submitButton1 = document.querySelector('button[type=submit][onclick*=pwdBaseOtpChannelSelected]'); // submit button from choose otp destination page
                var footer2 = document.querySelector('div[id="PASSWDPAGE"]>div>footer');
                var visaCard = document.querySelector('img[alt="Verified by Visa"]');
                if(visaCard){
                    Gatekeeper.setCardBrand(VISA);
                }
                if(passwordField){
                    passwordField.addEventListener("click",function(){
                        passwordFragmentHelper('input[id=enterPASS]');
                    });
                }
                if(footer2) {
                    footer2.parentElement.removeChild(footer2);
                }
                if(channelSelectOption) {
                    channelSelectOption.addEventListener("click", function() {
                        __juspay.trackPageStatus("CHOOSE_OTP_DESTINATION");
                        Gatekeeper.removeFragment("OTP Auth Option Page");
                        if(mobileOption && __juspay.isMobUiEnabled("AUTO_CHOOSE_ICICI")) {
                            mobileOption.checked = true;
                            window.trackEventv2_0default("acs","info","AUTO_SELECT","MOBILE_NUMBER");
                            if(submitButton1){
                                window.trackEventv2_0default("acs","info","AUTO_CLICK","OTP")
                                __juspay.clickOnElement(submitButton1);
                            }
                        }
                        Gatekeeper.showNetbankingDefaultFragment();
                    });
                }
                if(__juspay.isMobUiEnabled("MOBUI_ICICIDC")){
                    try{
                        var header = document.querySelector('header');
                        var pinText = document.querySelectorAll('p[id=pwdsubheader]');
                        var pinField= document.querySelector('input[name=pin][id=enterPASS]');
                        var cancelBtn = document.querySelector('button[class=orange][onclick*=closeButton]');
                        var submitBtn = document.querySelector('button[class=orange][onclick*=submitPassword]');
                        var pinLabel = document.querySelector('label[id=securecode]');
                        var resendMessage=document.querySelector('span.read.resend');
                        var help= document.querySelector('a.fleft');
                        var otp= document.querySelector('span.read.auth');
                        var controls = document.querySelector('div.controls');
                        __juspay.removeElements([help,cancelBtn]);
                        header.style.paddingBottom="12px";
                        document.querySelector('span.read.resend').appendChild(channelSelectOption);
                        document.querySelector('span.read.auth').insertBefore(submitBtn,document.querySelector('span.read.auth').firstChild);
                        __juspay.modifyUI({"style_width":{"other":[0.9,submitBtn],"other2":[0.3,pinField],"other3":[0.95,resendMessage]},"style_height":{"other":[1/18,submitBtn]},
                                           "display_none":[pinText[1],pinText[0]]});
                        channelSelectOption.style.float="left";
                        resendMessage.style.position="relative";
                        resendMessage.style.top="15px";
                     }catch(err){
                         __juspay.CatchPageModifyError("MOBUI",err,"3DS_PIN");
                     }
                }
                if(__uber.initSessionCounterIncremental("ICICIDC_SHOW_ACS_OPTIONS") < 2){
                    __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                    __juspay.delayMe(function(){
                        Gatekeeper.requestKeyboardHide();
                        Gatekeeper.showACSOptions();
                    },200);
                }else{
                    passwordFragmentHelper('input[id=enterPASS]');
                }
            },
            reachOtpStage: function(){
                var channelSelectOption = document.querySelector('a[title="Click Here)"][href*=showChannelSelectPage]');
                __juspay.clickOnElement(channelSelectOption);
            },
            reachPasswordStage: function(){
                var pinField= document.querySelector('input[name=pin][id=enterPASS]');
                __juspay.clickOnElement(pinField);
            },
            clickSubmitButton: function() {
                var passwordField = document.querySelector('input[id="enterPASS"]');
                var submitButton = document.querySelector('button[id=sendotp][type=submit]')
                if(passwordField && passwordField != "" && submitButton) {
                    __juspay.clickOnElement(submitButton)
                    Gatekeeper.removeFragment("Submitting 3DS form");
                }
            },
            nextAction: function() {
                var submitButton = document.querySelector('button[type=submit][onclick*=pwdBaseOtpChannelSelected]');
                if(submitButton) {
                    __juspay.clickOnElement(submitButton);
                    Gatekeeper.removeFragment("Navigating to OTP Page");
                }
            }
        },
        /* icici debit card ACS page */
        {
            path: /\/acspage\/cap\?/,
            hostname: "acs.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICIDC",
            domCheck: function() {
                return (document.querySelector('input[type=radio][name=optinoption]') && document.querySelector('button[type=submit][class=orange]'))
                    || document.querySelector('input[id=enterPASS]') || document.querySelector('input[type="password"][id="txtAutoOtp"]');
            },
            action: function() {
                Gatekeeper.removeFragment("Reached OTP page");
                var passwordField= document.querySelector('input[id=enterPASS]');
                var otpField = document.querySelector('input[type="password"][id="txtAutoOtp"]');
                var otpPageForm = document.querySelector('form[name="passwdForm"]');
                var acsPage = document.querySelector('div[id=optinlandingpage]');
                var optionOtpPage = document.querySelector('div[id="optinotppage"][style="display: inline;"]');
                var pwdBaseOtpPage = document.querySelector('div[id="pwdbaseotppage"][style="display: inline;"]');
                var otpPage = document.querySelector('div[id="pwdbaseotppage"][style="display:none;"]');
                var footer1 = document.querySelector('div[id="enterotppage"]>div>footer');
                var visaCard = document.querySelector('img[alt="Verified by Visa"]');
                var otpLabel = document.querySelector('label[id="securecode"]');
                var footer = document.querySelector('footer');
                var rsndOTP = document.querySelector('a[title = "Resend OTP"]');
                var submitBtn = document.querySelector('button[type = "Submit"]');
                var cancelBtn = document.querySelector('button[id = "cancel"][title = "Cancel"]');
                if(visaCard){
                    Gatekeeper.setCardBrand(VISA);
                }
                if(footer1) {
                    footer1.parentElement.removeChild(footer1);
                }
                if(__juspay.isMobUiEnabled("MOBUI_ICICIDC")){
                   try{
                       addMetaTag();
                       if(cancelBtn){
                          __juspay.removeElements([document.querySelector('button[id = "cancel"][title = "Cancel"]')]);
                          var buttonDiv = document.querySelector('div[class=controls]');
                          var cancelLink  = document.createElement("a");
                          cancelLink.setAttribute("id", "cancel")
                          cancelLink.setAttribute("class", "orange")
                          cancelLink.setAttribute("onclick", "javascript:return closeButton(this)")
                          cancelLink.setAttribute("title", "Cancel")
                          cancelLink.textContent="Cancel"
                          buttonDiv.appendChild(cancelLink);
                          cancelLink.style.textDecoration="underline";
                          cancelLink.style.marginLeft="72%";
                          cancelLink.style.marginBottom="5%";
                          cancelLink.style.marginTop="2%";
                          cancelLink.style.color="#551a8b";
                          cancelLink.style.fontSize="14px";
                          cancelLink.style.fontFamily="Verdana";
                       }
                       if(footer){
                            footer.style.position="relative";
                       }
                       otpLabel.style.marginLeft = "11%";
                       otpLabel.style.width = "10%";
                       otpLabel.style.marginBottom = "1%";
                       otpLabel.style.marginTop = "3%";
                       otpField.style.width = "69%";
                       otpField.style.height = "32px";
                       otpField.style.marginBottom = "4%";
                       otpField.style.marginLeft = "15%";
                       otpField.style.marginRight = "10%";
                       otpField.style.fontSize = "15px";
                       otpField.style.border = "thin solid #b4b4b4";
                       otpField.style.paddingLeft = "1%";
                       rsndOTP.style.marginLeft = "65%";
                       rsndOTP.style.marginBottom = "5%";
                       if(submitBtn) {
                           submitBtn.style.marginRight = "20%";
                           submitBtn.style.marginLeft = "15%";
                           submitBtn.style.marginBottom = "5%";
                           submitBtn.style.width = "70%";
                           submitBtn.style.padding = "2%";
                           submitBtn.style.fontWeight = 'normal';
                           submitBtn.style.fontSize = '18px'
                           submitBtn.style.fontVariant = 'all-petite-caps'
                           submitBtn.style.color = '#ffffff';
                           submitBtn.style.backgroundColor= '#003399';
                           submitBtn.style.bordercolor= '#cccccc';
                       }
                   } catch(err) {
                       __juspay.CatchPageModifyError("MOBUI",err,"ICICIDC_PAGE");
                   }
                }
                if(otpField && otpPage) {
                    otpFragmentHelper('input[id="txtAutoOtp"]','a[title="Resend OTP"]');
                }
                if(acsPage && acsPage.style.display == 'inline'){
                    Gatekeeper.removeFragment("Reached Choose Auth page");
                    var submitBtn= document.querySelector('button[type=submit][class=orange]');
                    var radios= document.querySelectorAll('input[type=radio]');
                    var setCount=0;
                    if(Gatekeeper.isEnabled("isAutoAuthEnabledIciciDC")){
                        var storedLastAuth = Gatekeeper.getDataFromSharedPrefs("ICICIDC_AUTH");
                        if(storedLastAuth){
                            var authCountPwd = GK.getAuthCountForPwd("ICICIDC_AUTH");
                            var authCountOtp = GK.getAuthCountForOtp("ICICIDC_AUTH");
                            var passwordOption = document.querySelectorAll('input[type=radio]')[0];
                            var submitBtn = document.querySelector('button[type=submit][class=orange]');
                            var otpLink = document.querySelectorAll('input[type=radio]')[1];
                            if(authCountPwd>2){
                                setCount++;
                                window.trackEventv2_0default("acs","info","directly_moving_to_password_page_count",authCountOtp);
                                if(passwordOption){
                                    passwordOption.checked=true;
                                }
                                if(submitBtn){
                                    __juspay.clickOnElement(submitBtn);
                                    Gatekeeper.removeFragment("Clicked PIN option from ACS");
                                    var passwordField= document.querySelector('input[id=enterPASS]');
                                    var channelSelectOption = document.querySelector('button[class="orange"][onclick="return channelSelected(1);"]');
                                    if(passwordField) {
                                        passwordFragmentHelper("input[id=enterPASS]");
                                    }
                                    if(channelSelectOption) {
                                        __juspay.trackPageStatus("CHOOSE_OTP_DESTINATION");
                                        channelSelectOption.addEventListener("click", function() {
                                            Gatekeeper.removeFragment("OTP Auth Option Page");
                                            Gatekeeper.showNetbankingDefaultFragment();
                                        });
                                    }
                                }
                            } else if(authCountOtp>2){
                                setCount++;
                                window.trackEventv2_0default("acs","info","directly_moving_to_otp_page_count",authCountOtp);
                                if(__uber.initSessionCounterIncremental("ICICIDC_AUTH_SUBMIT_OTP_LINK") < 2 ){
                                    if(otpLink){
                                        otpLink.checked=true;
                                    }
                                    if(submitBtn && __juspay.isMobUiEnabled("AUTO_CHOOSE_ICICI")){
                                        window.trackEventv2_0default("acs","info","AUTO_CLICK","AUTO_AUTH_CHOOSE");
                                        __juspay.clickOnElement(submitBtn);
                                        Gatekeeper.removeFragment("Clicked OTP option from ACS");
                                    }
                                }
                            }
                        }
                    }
                    if(submitBtn) {
                        submitBtn.addEventListener("click",function(){
                            if(radios && radios[0].checked){
                                var authCountPwd= GK.getAuthCountForPwd("ICICIDC_AUTH");
                                Gatekeeper.addDataToSharedPrefs("ICICIDC_AUTH","PWD_"+authCountPwd);
                                window.trackEventv2_0("acs","info","ICICIDC_AUTH","PWD_"+authCountPwd, "ICICIDC", "auth");
                            } else if(radios && radios[1].checked){
                                var authCountOtp=GK.getAuthCountForOtp("ICICIDC_AUTH");
                                Gatekeeper.addDataToSharedPrefs("ICICIDC_AUTH","PWD_"+authCountOtp);
                                window.trackEventv2_0("acs","info","ICICIDC_AUTH","PWD_"+authCountOtp, "ICICIDC", "auth");
                            }
                        },false);
                    }
                    if(setCount==0){
                        __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                        Gatekeeper.showACSOptions();
                    }

                } else if(optionOtpPage || pwdBaseOtpPage) {
                    var mobileOption = document.querySelector('input[name="otpDestinationOption"][value="FromMob"][type="radio"]');
                    if(optionOtpPage)
                        var submitButton = document.querySelector('button[class="orange"][onclick="return channelSelected(1);"]');
                    else
                        var submitButton = document.querySelector('button[class="orange"][onclick*="pwdBaseOtpChannelSelected"]');

                    if(mobileOption && submitButton){
                        Gatekeeper.removeFragment("OTP Destination page");
                        mobileOption.checked = true;
                        Gatekeeper.showNetbankingDefaultFragment();
                        if(__juspay.isFeatureEnabled("ICICIDC_AUTO_CLICK")) {
                            __juspay.clickOnElement(submitButton);
                        }
                    }
                } else if(passwordField && document.querySelector('div[id="optinissuerquestionspage"][style="display: inline;"]')) {
                    passwordFragmentHelper("input[id=enterPASS]");
                }
            },
            reachOtpStage: function() {
                var otpLink = document.querySelectorAll('input[type=radio]')[1];
                var submitBtn = document.querySelector('button[type=submit][onclick*="showSelectedOption"]');
                if(otpLink){
                    otpLink.checked=true;
                }
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                    var mobileOption = document.querySelector('input[name="otpDestinationOption"][value="FromMob"][type="radio"]');
                    var submitButton = document.querySelector('button[class="orange"][onclick="return channelSelected(1);"]');

                    if(document.querySelector('div[id="optinotppage"][style="display: inline;"]')) {
                        if(mobileOption && submitButton){
                            mobileOption.checked = true;
                            Gatekeeper.showNetbankingDefaultFragment();
                            if(__juspay.isFeatureEnabled("ICICIDC_AUTO_CLICK")) {
                                __juspay.clickOnElement(submitButton);
                            }
                        }
                    }
                    Gatekeeper.removeFragment("Clicked OTP option from ACS");
                }
            },
            reachPasswordStage: function() {
                var passwordOption = document.querySelectorAll('input[type=radio]')[0];
                var submitBtn = document.querySelector('button[type=submit][onclick*="showSelectedOption"]');
                if(passwordOption){
                    passwordOption.checked=true;
                }
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                    Gatekeeper.removeFragment("Clicked PIN option from ACS");
                    // Adding support for 3DS here as the page doesn't redirect
                    var passwordField= document.querySelector('input[id=enterPASS]');
                    if(passwordField && document.querySelector('div[id=optinissuerquestionspage]')
                        && document.querySelector('div[id=optinissuerquestionspage]').style.display == "inline") {
                        Gatekeeper.removeFragment("Reached 3DS page");
                        attachAlphaNumericPasswordHelper(passwordField);
                        attachPhoneKeyboard(passwordField);
                    }
                }
            },
            nextAction: function() {
                var submitButton1 = document.querySelector('button[type="submit"][class="orange"][onclick="return channelSelected(1);"]');
                var submitButton2 = document.querySelector('button[class="orange"][onclick*="pwdBaseOtpChannelSelected"]');
                if(submitButton1) {
                    __juspay.clickOnElement(submitButton1);
                    Gatekeeper.removeFragment("Navigating to OTP Page");
                }
                if(submitButton2){
                    __juspay.clickOnElement(submitButton2);
                    Gatekeeper.removeFragment("Navigating to OTP Page");
                }
            },
            submitOtp: function(otp) {
                // Mandatory pin field present. So we can auto click on OTP submit
                var otpField = document.querySelector('input[type="password"][id="txtAutoOtp"]');
                var submitbutton = document.querySelector('button[class=orange][type=submit]');

                if(otpField && submitbutton ) {
                    submitOtpHelper(otp, "input[id=txtAutoOtp][type=password]", "div[id=PASSWDPAGE]>div>section>div[class=bottom-container]>div>button[type=submit]");
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector("button[class=orange][type=submit]");
                var otpTextField = document.querySelector('input[id=enterPASS][name=otpPassword]');
                var pinField = document.querySelector('input[id="txtpassword"][name="pin1"]');
                var pinField2 = document.querySelector('input[type="password"][name="pin2"]');
                var otpPageForm = document.querySelector('form[name="passwdForm"]');

                if(otpTextField && pinField2.value == "") {
                    pinField2.focus();
                } else if(otpTextField && pinField && pinField2 && submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            },
            regenerateOtp: function() {
                var resendOtp = document.querySelector('a[title="Resend OTP"]');
                if(resendOtp) {
                    __juspay.clickOnElement(resendOtp);
                }
            }
        },
        //Razorpay page support added on 6th june 2019
        {
            path: /\/v1\/payments\/.*\/(authorize|authenticate)/,
            hostname: "api.razorpay.com",
            state: "UNUSED",
            bank: "",
            domCheck: function(){
                return document.querySelector('button[id="submit-action"]') &&
                        document.querySelector('a[id="resend-action"]') &&
                        document.querySelector('input[autocomplete="one-time-code"]');
            },
            action: function(){
                Gatekeeper.removeFragment("Reached razorpay ACS page");
                var bankName = "";
                var hdfcLogo = document.querySelector('img[src*="HDFC.svg"]');
                var iciciLogo = document.querySelector('img[src*="ICIC.svg"]');

                __juspay.trackPageStatus("Reached razorpay ACS page");

                if(hdfcLogo){
                    bankName = "HDFC";
                }else if(iciciLogo){
                    bankName = "ICICI";
                }else{
                    getImageInformation();
                }

                if(bankName !== "" && __juspay.isFeatureEnabled("RAZORPAY_ACS_PAGE_SUPPORT")) {
                    Gatekeeper.setBank(bankName);
                    otpFragmentHelper('input[autocomplete="one-time-code"]','a[id="resend-action"]');
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[autocomplete="one-time-code"]','button[id="submit-action"]');
            },
            regenerateOtp: function(){
                var generateOtpBtn = document.querySelector('a[id="resend-action"]');
                __juspay.clickOnElement(generateOtpBtn);
            }
        },
        { //ICICI_CC: OTP page
            path: /\/ACSWeb\/EnrollWeb\/ICICIBank\/((server\/OtpServer)|(auth\/(VBV))|(auth\/SCode))|(server\/AccessControlServer)/,
            hostname: /www\.(3dsecure|3ds|3dsecure1)\.icicibank\.com/,
            state: "UNUSED",
            local: false,
            bank: "ICICICC",
            domCheck: function() {
               return ((document.querySelector("input[name=txtAutoOtp][type=password]") || document.querySelector("input[name=otpPassword][type=password]"))  &&
                      	document.querySelector('input[id=cmdSubmit]')) ||
                      	(document.querySelector('input[type=radio][value=toMobile]') &&
                        document.querySelector('input[type=image][name=I1]'));
            },
            action: function() {
                var toMobile = document.querySelector('input[type=radio][value=toMobile]');
                var submitBtn = document.querySelector('input[type=image][name=I1]');
                var otpField = document.querySelector("input[name=txtAutoOtp][type=password]");
                var otpSubmitBtn = document.querySelector('input[id=cmdSubmit]');
                var otpField1 = document.querySelector("input[name=otpPassword][type=password]");
                var cancelButton = document.querySelector('input[id=cmdCancel]');
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                    }, false);
                }
                if(toMobile && submitBtn && __juspay.isMobUiEnabled("AUTO_CLICK_ICICICC")){
                    __juspay.clickOnElement(toMobile);
                    __juspay.clickOnElement(submitBtn);
                }else if(otpField && otpSubmitBtn){
                    otpFragmentHelper('input[name=txtAutoOtp][type=password]','a[id=link][href*=resend_otp]');
                }else if (otpField1 && otpSubmitBtn){
                	otpFragmentHelper('input[name=otpPassword][type=password]','a[id=link][href*=resend_otp]');
                }
            },
            submitOtp: function(otp) {
                var el = document.querySelector("input[name=txtAutoOtp][type=password]") || document.querySelector("input[id=txtAutoOtp][type=password]");
                if(el) {
                    Gatekeeper.showWaitingFragment();
                    el.value = otp;
                    document.querySelector('input[id=cmdSubmit]').click();
                }
                if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"] !="ios"){
                    Gatekeeper.removeFragment("OTP submitted via OTP fragment");
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[id=link][href*=resend_otp]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        {   //entering wrong otp loads this page
            path: /\/ACSWeb\/EnrollWeb\/ICICIBank\/auth\/ForgotPasswordOtp.jsp/,
            hostname: "www.3dsecure.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICICC",
            domCheck: function() {
                if (document.querySelector("input[name=txtAutoOtp]")) {return true} else {return false};
            },
            action:function() {
                otpFragmentHelper("input[name=txtAutoOtp]","");
            },
            submitOtp: function(otp) {
                var el = document.querySelector("input[name=txtAutoOtp]");
                if(el) {
                    Gatekeeper.showWaitingFragment();
                    el.value = otp;
                    el.form.submit();
                }
            }
        },

        { //HSBC_CC: OTP page
            path: /\/SecurePay\/servlet\/Authenticate/,
            hostname: "www.evgr-securepay.hsbc.co.in",
            state: "UNUSED",
            local: false,
            bank: "HSBCCC",
            domCheck: function() {
                return document.querySelector('input[type=password][name=password]') &&
                        document.querySelector('img[src*=but_confirm]');
                        document.querySelector('img[src*=but_get_another_sms_otp_grey]');
            },
            action:function()
            {
                otpFragmentHelper("input[type=password][name=password]","img[src*=but_get_another_sms_otp_grey]");
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, "input[type=password][name=password]", "img[src*=but_confirm]")
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('img[src*=but_get_another_sms_otp_grey]');
                __juspay.clickOnElement(resendLink);
            }
        },
        { //CITI_cc/dc New page
            path: /\/acspage\/cap_nsapi/,
            hostname: "www.citibank.co.in",
            state: "UNUSED",
            local: false,
            bank: "CITI",
            domCheck: function() {
                return (document.querySelector('input[id=ipincode]') && document.querySelector('a[id=next]')) || (document.querySelector('input[id="otp1"]'));
            },
            action: function() {
                Gatekeeper.setPollingForSmsEnabled(true);
                var vbv_logo = document.querySelector('img[src*="Verified-by-visa"]');
                var mastercard_logo = document.querySelector('img[src*=mastercard_logo]');
                var passwordField = document.querySelector('input[id=ipincode]');
                var otpField = document.querySelector('input[id="otp"]');
                var otpField1 = document.querySelector('input[id="otp1"]');
                var pwdSubmitBtn = document.querySelector('a[id=next]');
                var ipinLink = document.querySelector('a[href="javascript:void(0)"][title="IPIN (Internet Pin)"]');
                __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                if(vbv_logo){
                    Gatekeeper.setCardBrand(VISA);
                }else if(mastercard_logo){
                    Gatekeeper.setCardBrand(MASTERCARD);
                }
                if (enabledFlag=== "ipin"){
                    if(ipinLink){
                        ipinLink.addEventListener("click",function(){
                            Gatekeeper.removeFragment('IPIN link is clicked');
                            passwordFragmentHelper('input[id=ipincode]');
                        },false);
                    }
                    passwordFragmentHelper('input[id=ipincode]');
                }
                if(enabledFlag === "optin otp"){
                    var wrongOTPBodyField = document.querySelector('body');
                    isWrongOtpFound = 0;
                    if(wrongOTPBodyField){
                        var wrongOtpText = wrongOTPBodyField.innerText;
                        if(wrongOtpText){
                            var isWrongOtpFound = wrongOtpText.search('OTP authentication failure');
                        }
                    }
                    if(isWrongOtpFound>-1){
                        Gatekeeper.removeFragment('Wrong Otp Entered')
                    }else if(otpField){
                        otpFragmentHelper('input[id=otp]','a[href*=modal]');
                    }
                    else {
                        if(otpField1) {
                            __juspay.showOtpFragmentWrapper(otpField1);
                        }
                    }
                    if(ipinLink){
                       ipinLink.addEventListener("click",function(){
                           Gatekeeper.removeFragment('IPIN LINK is clicked from OTP page');
                       },false);
                    }
                }
                if(enabledFlag === "right2"){
                    var wrongOTPBodyField = document.querySelector('body');
                    isWrongOtpFound = 0;
                    if(wrongOTPBodyField){
                        var wrongOtpText = wrongOTPBodyField.innerText;
                        if(wrongOtpText){
                            var isWrongOtpFound = wrongOtpText.search('OTP authentication failure');
                        }
                    }
                    if(isWrongOtpFound>-1){
                        Gatekeeper.removeFragment('Wrong Otp Entered')
                    }
                    else if(otpField1){
                        otpFragmentHelper('input[id=otp1]','a[href*=modal]');
                    }
                    else {
                        if(otpField) {
                            __juspay.showOtpFragmentWrapper(otpField);
                        }
                    }
                    if(ipinLink){
                        ipinLink.addEventListener("click",function(){
                            Gatekeeper.removeFragment('IPIN LINK is clicked from OTP page');
                        },false);
                    }
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('a[id=next]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            },
            submitOtp: function(otp){
                var otpContainer = document.querySelector("div[class=right1]")
                var ipinOtpContainer = document.querySelector("div[class=right2]")
                if(otpContainer && otpContainer.style.display!=="none") {
                    submitOtpHelper(otp, 'input[id="otp"]', 'a[onclick*="validateOTP(1);"]');
                } else if(ipinOtpContainer && ipinOtpContainer.style.display!=="none"){
                    submitOtpHelper(otp, 'input[id="otp1"]', 'a[onclick*="validateOTP(1);"]');
                }
            }
        },

        //CITI-NetBanking
        { //CITI_NB: login page
            path: /\/servlets\/TransReq/,
            hostname: "www.citibank.co.in",
            state: "UNUSED",
            local: false,
            bank: "CITINB",
            domCheck: function() {
                var cardNum1TxtBox = document.querySelector("input[type=text][name=cardNum1]")
                var cardNum2TxtBox = document.querySelector("input[type=text][name=cardNum2]")
                var cardNum3TxtBox = document.querySelector("input[type=text][name=cardNum3]")
                var cardNum4TxtBox = document.querySelector("input[type=text][name=cardNum4]")
                if(cardNum1TxtBox && cardNum2TxtBox && cardNum3TxtBox && cardNum4TxtBox) {
                    return true
                }
                else {
                    return false
                }
            },

            action: function() {
                Gatekeeper.removeFragment("focusing on grid text box");
                var firstElt = document.querySelector("input[id=CITI_CREDIT_CARD]") || document.querySelector("input[id=CITI_DEBIT_CARD]");
                if(firstElt){
                    firstElt.blur();
                }
                var passwordElement = document.querySelector("input[name=HtmlIPIN]");
                var textBox = ["cardNum1","cardNum2","cardNum3","cardNum4"];
                __juspay.keepCheckingDomFor("Please enter valid Card Credentials",
                    function() {
                        Gatekeeper.trackUserError("Invalid_card_credentials")
                    });

                __juspay.trackPageStatus("INPUT_CARD_DETAILS");
                // Remove the focus from the default box
                for (var i=0; i< textBox.length; i++){
                    var queryString = 'input[type=text][name='+textBox[i]+']';
                    // there are four boxes with the same name in different forms
                    var elements = document.querySelector(queryString);
                    numberTextFieldQuery(queryString);
                    attachPhoneKeyboard(elements);
                }
                // force the keyboard to show up.
                var cvvField = document.querySelector('input[name=HtmlCVVNum][class=textfield1]');
                otpTextFieldQuery("input[name=HtmlCVVNum][class=textfield1]");
                attachPhoneKeyboard(cvvField);
                var cardNumberField = document.querySelector("p.txt12 strong");
                var cardLastFourDigits = Gatekeeper.getLastFourDigitsOfCard();

                if(!cardLastFourDigits && cardNumberField) {
                    var cardNumber = cardNumberField.innerHTML;
                        if(cardNumber) {
                            digits = cardNumber.slice(cardNumber.length - 4);
                            if(!isNaN(digits)) {
                                Gatekeeper.setLastFourDigitsOfCard(digits);
                                cardLastFourDigits = digits;
                            }
                        }
                }
                if(firstElt){
                    firstElt.blur();
                    Gatekeeper.requestPhoneKeyboardShow();
                    focusElement(firstElt);
                }
            }
        },
        {
            path: /\/vbv\/auth/,
            hostname: /(acs|sliceacs)\.yappay\.in/,
            state: "UNUSED",
            local: false,
            bank: "SLICESBM",
            domCheck: function(){
                return (document.querySelector('input[id=otpbox1]') || document.querySelector('input[id=otp-number]')) &&
                       (document.querySelector('a[id=resendOTPbtn1]') || document.querySelector('a[class=resend-link]')) &&
                       (document.querySelector('input[id=otpverifybtn]') || document.querySelector('button[id=pay-button]')) &&
                       (document.querySelector('img[src*=SBM][class=partnerlogo]') || document.querySelector('img[src*=sbm-bank]')) &&
                       (document.querySelector('img[src*=Slice][id=bank_logo]') || document.querySelector('img[src*=slice-logo]'));

            },
            action: function(){
                var otpField1 = document.querySelector('input[id=otpbox1]');
                var otpField2 = document.querySelector('input[id=otp-number]');
                if(otpField1){
                    otpFragmentHelper('input[id=otpbox1]','a[id=resendOTPbtn1]');
                }
                if(otpField2){
                    otpFragmentHelper('input[id=otp-number]','a[class=resend-link]');
                }
            },
            submitOtp: function(otp){
                var otpField1 = document.querySelector('input[id=otpbox1]');
                var otpField2 = document.querySelector('input[id=otp-number]');
                var submitBtn1 = document.querySelector('input[id=otpverifybtn]');
                var submitBtn2 = document.querySelector('button[id=pay-button]');
                if(otpField1 && submitBtn1){
                    otpField1.value=otp;
                    try{
                        if (submitBtn1){
                            submitBtn1.disabled = false;
                            __juspay.clickOnElement(submitBtn1);
                        }
                    }catch(err){
                        Gatekeeper.removeFragment('Submit Button Dom Changed - Please Check');
                    }
                }
                if(otpField2 && submitBtn2){
                    otpField2.value=otp;
                    try{
                        if (submitBtn2){
                            submitBtn2.disabled = false;
                            __juspay.clickOnElement(submitBtn2);
                        }
                    }catch(err){
                        Gatekeeper.removeFragment('Submit Button Dom Changed - Please Check');
                    }
                }

            },
            regenerateOtp: function(){
                var regenerateBtn1 =  document.querySelector('a[id=resendOTPbtn1]');
                var regenerateBtn2 = document.querySelector('a[class=resend-link]');
                if(regenerateBtn1){
                    __juspay.clickOnElement(regenerateBtn1);
                }
                if(regenerateBtn2){
                    __juspay.clickOnElement(regenerateBtn2);
                }
            }
        },
        //RBL Bank OTP Page
        {
            path: /\/ACSWeb\/EnrollWeb\/RBLBank\/server\/AccessControlServer/,
            hostname: "b4-pdc.enstage-sas.com",
            state: "UNUSED",
            bank: "RBLDC",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=otpValue]') &&
                        document.querySelector('input[id=cmdSubmit]') &&
                        document.querySelector('a[id=otpLink]');
            },
            action: function()
            {
                otpFragmentHelper('input[id=otpValue]','a[id=otpLink]');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=otpValue]", "input[id=cmdSubmit]");
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('a[id=otpLink]');
                __juspay.clickOnElement(resendLink);
            }
        },
        //RBLCC Bank OTP Page
        {
            path: /\/v1\/acs\/legacy\/live/,
            hostname: /(secure-acs2ui-b1-indblr-blrtdc|secure-acs2ui-b1-indmum-mumrdc)\.wibmo\.com/,
            state: "UNUSED",
            bank: "RBLCC",
            local: false,
            domCheck: function() {
                return (document.querySelector('input[id=otpValue]') || document.querySelector('input[id=passReal]')) &&
                      (document.querySelector('button[id=submitBtn]') || document.querySelector('a[onclick*=otpSub]')) &&
                      document.querySelector('span[id=otpResend]') &&
                      document.querySelector('img[src*="rblbk_logo"]');
            },
            action: function()
            {
                var otpField1 = document.querySelector('input[id=otpValue]');
                var otpField2 = document.querySelector('input[id=passReal]');
                if (otpField1){
                    otpFragmentHelper('input[id=otpValue]','span[id=otpResend]');
                }
                if (otpField2){
                    otpFragmentHelper('input[id=passReal]','span[id=otpResend]');
                }
            },
            submitOtp: function(otp) {
                var submitBtn1 = document.querySelector('button[id=submitBtn]');
                var submitBtn2 = document.querySelector('a[onclick*=otpSub]');
                var otpField1 = document.querySelector('input[id=otpValue]');
                var otpField2 = document.querySelector('input[id=passReal]');
                if (submitBtn1 && otpField1){
                    submitOtpHelper(otp, "input[id=otpValue]", "button[id=submitBtn]");
                }
                if (submitBtn2 && otpField2){
                    submitOtpHelper(otp, "input[id=passReal]", "a[onclick*=otpSub]");
                }
                if (submitBtn2 && otpField1){
                    submitOtpHelper(otp, "input[id=otpValue]", "a[onclick*=otpSub]");
                }
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('span[id=otpResend]');
                __juspay.clickOnElement(resendLink);
            }
        },
        //RBL Bank OTP Page
        {
            path: /\/acspage\/cap/,
            hostname: "secure7.arcot.com",
            state: "UNUSED",
            bank: "RBLCC",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=enterPIN]') &&
                        document.querySelector('button[id=btnOtpSubmit]') &&
                        document.querySelector('button[id=btnResendSubmit]') &&
                        document.querySelector('img[alt*="RBL"]');
            },
            action: function()
            {
                submitBtnDisabled = document.querySelector('button[id=btnOtpSubmit][class="blue ng-binding btnDisable"]');
                if(submitBtnDisabled){
                    submitBtnDisabled.className="blue ng-binding"
                }
                otpFragmentHelper('input[id=enterPIN]','button[id=btnResendSubmit]');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=enterPIN]", "button[id=btnOtpSubmit]");
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('button[id=btnResendSubmit]');
                __juspay.clickOnElement(resendLink);
            }
        },
        // PAYTM page for new page added on 20-06-2017
        {
            path: /\/theia\/(processTransaction|payment)/,
            hostname: "securegw.paytm.in",
            state: "UNUSED",
            local: false,
            bank: "ICICIDC",
            domCheck: function () {
                return document.querySelector('input[id=otp][name=otp]') &&
                document.querySelector('button[id=confirmPayment]') &&
                document.querySelector('img[src*="ICICI"]');
            },
            action: function(){
                otpFragmentHelper('input[id=otp][name=otp]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, "input[id=otp][name=otp]", "button[id=confirmPayment]");
            }
        },
        //SBI
        { //SBI_CC: OTP page
            path: /\/acspage\/cap\?/,
            hostname: "secure4.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "SBICC",
            domCheck: function() {
                return document.querySelector("input[name=otp]") && document.querySelector("button[type=submit]") &&
                (document.querySelector("img[src*=sbi\\.jpg]") || document.querySelector('img[src*="tata_cards.gif"]'));
            },

            action: function() {
                __juspay.trackInvalidScenarios("One Time Password (OTP) entered by you is incorrect. Please try again.", "Invalid_OTP");
                var errorElt = document.querySelector('#info_error');
                var regenerateOtpLink = document.querySelector('a[href*=OnSubmitHandlerResend]');
                if(errorElt && errorElt.innerHTML.match(/valid OTP/)) {
                    Gatekeeper.removeFragment("SBI card wrong otp");
                }else if(errorElt && errorElt.innerHTML.match(/entered by you is incorrect/)){
                    Gatekeeper.removeFragment("SBI card wrong otp");
                }else {
                    var vbv = document.querySelector('img[src="GenericOTP_SBI_Visa/images/vpas_logo.gif"]');
                    if(vbv) {
                        Gatekeeper.setCardBrand(VISA);
                    }
                }
                otpFragmentHelper('input[name=otp]','a[href*=OnSubmitHandlerResend]');
                if(__juspay.isMobUiEnabled("MOBUI_SBICC") && regenerateOtpLink){
                    try{
                        var submitBtn= document.querySelector("button[type=submit]");
                        var helpBtn = document.querySelector('span[id=helpcontentfromjson] a');
                        var cancelBtn = document.querySelectorAll('span[id=helpcontentfromjson] a')[1];
                        var resend_otp = document.querySelector('a[href*=OnSubmitHandlerResend]');
                        var otpField= document.querySelector('input[name=otp][id=enterPIN]')
                        var breaks = document.querySelectorAll('p[id=pwdsubheader] br');
                        var otpLabel = document.querySelector('div[id=otpContent] label');
                        var extraDiv= document.createElement('div');
                        document.querySelector('div.controls').appendChild(extraDiv);
                        extraDiv.appendChild(resend_otp);
                        resend_otp.innerText="Resend OTP";
                        resend_otp.style.float="right";
                        resend_otp.style.color="blue";
                        resend_otp.style.paddingTop="7px";
                        resend_otp.style.paddingRight = "15px";
                        cancelBtn.style.float="left";
                        cancelBtn.style.paddingTop="10px";
                        cancelBtn.style.paddingLeft="20px";
                        __juspay.removeElements([otpLabel,document.querySelectorAll('div.read-only')[2]]);
                        document.querySelector('div.content-container').insertBefore(document.querySelector('div[id=otpContent]'),document.querySelector('div.content-container').secondChild);
                        document.querySelector('div.content-container').insertBefore(submitBtn,document.querySelector('div.content-container').thirdChild);
                        document.querySelectorAll('div.content-container')[1].insertBefore(document.querySelector('div.content-container'),document.querySelectorAll('div.content-container')[1].firstChild);
                        __juspay.modifyUI({"display_none":[helpBtn,breaks[6],breaks[5],breaks[4],breaks[3],breaks[2],breaks[1],breaks[0]],
                                            "style_width":{"other":[0.9,otpField],"other2":[0.92,submitBtn]},"style_height":{"other":[1/21,submitBtn],"other1":[1/26,otpField]}});
                        document.querySelector('p[id=pwdsubheader]').innerText="\n One Time Password (OTP) sent to Primary Cardholder's registered mobile no. ending with"
                                                + document.querySelector('p[id=pwdsubheader]').innerText.substr(document.querySelector('p[id=pwdsubheader]').innerText.indexOf("with")+4,10);
                        document.querySelector('p[id=pwdsubheader]').style.fontSize="9px";
                        document.querySelector('p[id=pwdsubheader]').style.paddingLeft="8px";
                        document.querySelector('h1[id=pwdheader]').style.fontSize="10px";
                        var elements = document.getElementsByClassName('read-only');
                        for (var i = 0; i < elements.length; i++) {
                           var labels= elements[i].querySelectorAll('label');
                           elements[i].style.display="block";
                           elements[i].style.padding="0px";
                           for(var j=0; j < labels.length; j++){
                               labels[j].style.fontSize = "9px";
                           }
                        }
                        otpField.style.marginTop="5px";
                        submitBtn.style.marginTop="8px";
                        submitBtn.style.position="relative";cancelBtn.style.position="relative";
                        submitBtn.style.left="-5px"; cancelBtn.style.right="6px";
                    }catch(err){
                        __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                    }
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, "input[name=otp]", "button[type=submit]");
            },
            regenerateOtp: function(simulate){
                OnSubmitHandlerResend();
            }
        },
        { //SBI_CC: OTP page
            path: /\/acs-v1\/(processauth|otpResend)/,
            hostname: /(csmu|csch)\.enstage-sas\.com/,
            state: "UNUSED",
            local: false,
            bank: "SBICC",
            domCheck: function() {
                return document.querySelector('input[id=otpBox][name=otp]') &&
                        document.querySelector('span[class=resendSpan]');
                        document.querySelector('button[id=smbt]');
            },
            action:function()
            {
                var SubmitBtn = document.querySelector('button[id=smbt]');
                SubmitBtn.setAttribute("class", "sub-btn");
                otpFragmentHelper("input[id=otpBox][name=otp]","span[class=resendSpan]");
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, "input[id=otpBox][name=otp]", "button[id=smbt]")
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('span[class=resendSpan]');
                __juspay.clickOnElement(resendLink);
            }
        },
        // SBIDC new page added on 20-06-2017
        {
            path: /\/bdacs\/SBIValidate/,
            hostname: "acs2.onlinesbi.com",
            state: "UNUSED",
            local: false,
            bank: "SBIDC",
            domCheck: function () {
                return (document.querySelector('input[name=customerotp]') || document.querySelector('input[name=customerpin]')) &&
                       document.querySelector('button[type=button][onclick*=ValidateForm]')
            },
            action: function(){
                var otpFieldDom1 = document.querySelector('input[name=customerotp]');
                var otpFieldDom2 = document.querySelector('input[name=customerpin]');
                var mastercard = document.querySelector('img[src*=mastercardSecure]');
                if(otpFieldDom1){
                    otpFragmentHelper('input[name=customerotp]','a[class*=request]');
                }
                else if (otpFieldDom2){
                    otpFragmentHelper('input[name=customerpin]','a[class*=request]');
                }
                if(mastercard){
                    Gatekeeper.setCardBrand(MASTERCARD);
                }
                var cancelButton = document.querySelector('a[href*=cancelForm]');
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                    }, false);
                }
                var submitClick = document.querySelector('button[type=button][onclick*=ValidateForm]');
                if (submitClick){
                    submitClick.addEventListener("click",function(){
                       __juspay.delayMe(function(){
                           Gatekeeper.requestKeyboardHide();
                          Gatekeeper.hideBlur();
                      },200);
                    });
                }
                window.trackEventv2_0default("acs","info","sbidc","sbi_new_domain_page");
            },
            submitOtp: function(otp){
                var otpFieldDom1 = document.querySelector('input[name=customerotp]');
                var otpFieldDom2 = document.querySelector('input[name=customerpin]');
                if(otpFieldDom1){
                    submitOtpHelper(otp, "input[name=customerotp]", "button[type=button][onclick*=ValidateForm]");
                    Gatekeeper.removeFragment("OTP Submitted");
                }
                else if(otpFieldDom2){
                    submitOtpHelper(otp, "input[name=customerpin]", "button[type=button][onclick*=ValidateForm]");
                    Gatekeeper.removeFragment("OTP Submitted");
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[class*=request]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },

        //SBIDC Payu new page added on 20-06-2017
        {
            path: /\/directTxnAuth/,
            hostname: /(secure4|secure)\.payu\.in/,
            state: "UNUSED",
            local: false,
            bank: "SBIDC",
            domCheck: function(){
                return document.querySelector('input[id=otp][type=tel][name=otp]') &&
                         document.querySelector('input[id=otpSubmit][value=Submit]') &&
                         document.querySelector('input[id=atm][type=tel][name=atm]') &&
                         document.querySelector('input[id=atmSubmit][value=Submit]') &&
                         document.querySelector('img[src*=sbi_logo]');
            },
            action: function(){
                var otpDiv = document.querySelector('div[id=forOTP][class=container]');
                var atmDiv = document.querySelector('div[id=forATM][class=container]');
                if (otpDiv){
                    otpFragmentHelper('input[id=otp]','p[id=otpRequest] > a[id=requestOtpLink]');
                }
                else if (atmDiv){
                    Gatekeeper.showNetbankingDefaultFragment();
                    __juspay.trackPageStatus("ATMPIN");
                    __juspay.trackAuthMethod("ATM_PIN");
                }
                var switchToAtm = document.querySelector('span[id=backToATM]');
                if (switchToAtm){
                    switchToAtm.addEventListener("click",function(){
                       Gatekeeper.showNetbankingDefaultFragment();
                       __juspay.trackPageStatus("ATMPIN");
                       __juspay.trackAuthMethod("ATM_PIN");
                    });
                }
                var switchToOtp = document.querySelector('span[id=backToOTP]');
                if (switchToOtp){
                    switchToOtp.addEventListener("click",function(){
                       otpFragmentHelper('input[id=otp]','p[id=otpRequest] > a[id=requestOtpLink]');
                       __juspay.delayMe(function(){
                           Gatekeeper.requestKeyboardHide();
                          Gatekeeper.hideBlur();
                      },200);
                    });
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otp]', 'input[id=otpSubmit][value=Submit]');
                Gatekeeper.removeFragment("OTP Submitted");
            },
            nextAction: function() {
                var confirm_btn = document.querySelector('input[id=atmSubmit][value=Submit]');
                if (confirm_btn){
                    __juspay.clickOnElement(confirm_btn);
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('p[id=otpRequest] > a[id=requestOtpLink]')
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },

        //SBIDC Payu secure new with only otp option 22/11/2017
        {
           path: /\/(directTxnAuth|directTxnOptions)/,
           hostname: /(secure4|secure)\.payu\.in/,
           state: "UNUSED",
           local: false,
           bank: "SBIDC",
           domCheck: function(){
               return document.querySelector('input[id=otp][type=tel][name=otp]') &&
                        document.querySelector('input[id=otpSubmit][value=Submit]') &&
                        document.querySelector('p[id=otpRequest] > a[id=requestOtpLink]') &&
                        (document.querySelector('img[src*=sbi_logo]') || document.querySelector('img[src*=SBI]'));
           },
           action: function(){
               otpFragmentHelper('input[id=otp]','p[id=otpRequest] > a[id=requestOtpLink]');
           },
           submitOtp: function(otp){
               submitOtpHelper(otp, 'input[id=otp]', 'input[id=otpSubmit][value=Submit]');
               Gatekeeper.removeFragment("OTP Submitted");
           },
           regenerateOtp: function(){
               var regenerateBtn = document.querySelector('p[id=otpRequest] > a[id=requestOtpLink]')
               if(regenerateBtn){
                   __juspay.clickOnElement(regenerateBtn);
               }
           }
        },
        //SBIDC Payu
        {
           path: /\/(directTxnAuth|directTxnOptions)/,
           hostname: "secure.payu.in",
           state: "UNUSED",
           local: false,
           bank: "SBIDC",
           domCheck: function(){
               return document.querySelector('input[id=otp][type=tel][name=otp]') &&
                        document.querySelector('input[type=submit][value=Submit]') &&
                        document.querySelector('img[src*=sbi_logo]');
           },
           action: function(){
               otpFragmentHelper('input[id=otp]','p[id=otpRequest] > a');
               var switchToAtm = document.querySelector('span[id=backToATM]');
               if (switchToAtm){
                   switchToAtm.addEventListener("click",function(){
                      Gatekeeper.removeFragment('Clicked atm option');
                      __juspay.trackPageStatus("ATMPIN");
                      __juspay.trackAuthMethod("ATM_PIN");
                   });
               }
               var switchToOtp = document.querySelector('span[id=backToOTP]');
               if (switchToOtp){
                   switchToOtp.addEventListener("click",function(){
                      otpFragmentHelper('input[id=otp]','p[id=otpRequest] > a');
                      __juspay.delayMe(function(){
                          Gatekeeper.requestKeyboardHide();
                          Gatekeeper.hideBlur();
                     },200);
                   });
               }
           },
           submitOtp: function(otp){
               submitOtpHelper(otp, 'input[id=otp]', 'input[type=submit][value=Submit]');
           },
           regenerateOtp: function(){
               var regenerateBtn = document.querySelector('p[id=otpRequest] > a')
               if(regenerateBtn){
                   __juspay.clickOnElement(regenerateBtn);
               }
           }
        },
        { //SBINB - login page
            path: /\/(merchant|mmerchant|npcimandate)\/(merchantprelogin|loginsubmit|npciprelogin).htm/,
            hostname:/(www|m|merchant)?\.onlinesbi\.(sbi|com)/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function () {
                return document.querySelector('input[id=username][type=text]') &&
                       document.querySelector('input[id=label2][type=password]') &&
                       document.querySelector('input[type=submit][value=Login]');
            },
            showUberDialog:function(props,data) {
                props.type = "dialog";
                props.height = props.height || 320;
                props.width = props.width || -2;
                if(props.showOnLoad) {
                    setTimeout(function(){
                        Gatekeeper.showUber(false);
                    },500);
                    props.showOnLoad = false;
                }
                GK.setUberEventWithCallback("SBINB_RESET_CREDENTIALS", function(result) {
                    window.trackEventv2_0("acs","info","SBINB_RESET_CREDENTIALS","uber_shown", "SBINB", "reset_credentials");
                    if( result == "ok" ) {
                        window.trackEventv2_0("acs","info","SBINB_RESET_CREDENTIALS","recover_username_clicked", "SBINB", "reset_credentials");
                        window.location.href="https://retail.onlinesbi.com/retail/forgotusername.htm?bankCode=0";
                    } else if( result== "cancel" ) {
                        window.trackEventv2_0("acs","info","SBINB_RESET_CREDENTIALS","reset_pwd_clicked", "SBINB", "reset_credentials");
                        window.location.href="https://retail.onlinesbi.com/retail/troubleloginhome.htm?bankCode=0";
                    } else if(result == "closed") {
                        window.trackEventv2_0("acs","info","SBINB_RESET_CREDENTIALS","close_clicked", "SBINB", "reset_credentials");
                    }
                },props,data);
            },
            action: function() {
                Gatekeeper.setSessionAttribute("disableAutoFocus", "true"); showUpdateWebviewUber();
                var userId = document.querySelector('input[id=username][type=text]');
                var passwordField = document.querySelector('input[id=label2][type=password]');
                var submitBtn = document.querySelector('input[type=submit][value=Login]');
                var resetBtn = document.querySelector("input[type=reset][value=Reset]");
                var nbBlocked = window.find("Your Account is locked as a security measure", true, false, true);
                var invalidCredentials = document.querySelector('div[class="col-sm-12 content_inner text-center"]>span');
                var sbiHeaderLogoBig = document.querySelector('a[id="sbi_logo"]');
                var sbiHeaderLogoSmall = document.querySelector('a[title="OnlineSBI"]');
                var veriSignImageFooter = document.querySelector('img[alt="Verisign"]');

                if(sbiHeaderLogoBig){
                    sbiHeaderLogoBig.setAttribute("href","#")  //disabling links
                }
                if(sbiHeaderLogoSmall){
                    sbiHeaderLogoSmall.setAttribute("href","#")  //disabling links
                }
                if(veriSignImageFooter){
                    veriSignImageFooter.setAttribute("href","#")  //disabling links
                }
                if(nbBlocked) {
                    Gatekeeper.trackUserError("NB_account_blocked");
                    window.getSelection().collapseToEnd();
                    this.showUberDialog({showOnLoad:true,onPageLoad:"destroy"},JSON.stringify({
                        message:"Reset password to unblock your account"
                    }));
                }
                if(invalidCredentials){
                    var invalidCredentialsInnerText = invalidCredentials.innerText;
                    if(invalidCredentialsInnerText === "Invalid Username or Password"){
                        Gatekeeper.trackUserError("Invalid_username_or_password");
                        invalidCredentials.focus();
                        if(__uber.isUberEnabled("SBINB_RESET_CREDENTIALS")) {
                            var wrongAttemptCount = __uber.initSessionCounterIncremental("sbinb_wrong_credentials");
                            if(wrongAttemptCount == 2) {
                                this.showUberDialog({showOnLoad:true,onPageLoad:"destroy"},null);
                            } else if(wrongAttemptCount == 3) {
                                this.showUberDialog({showOnLoad:true,onPageLoad:"destroy"},JSON.stringify({
                                    message:"Wrong password will lock your account for 24 hours"
                                }));
                            }
                        }
                    }
                }
                if(userId && passwordField && submitBtn){
                    __juspay.trackPageStatus("INPUT_NB_LOGIN");
                    __juspay.trackAuthMethod("LOGIN");
                    setupLoginPageWithSubmitButton('input[id=label2][type=password]','input[id=username][type=text]',
                                                    'input[type=submit][value=Login]');
                }
                if(resetBtn){
                    resetBtn.addEventListener("click", function() {
                        userId.focus();
                    });
                }
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector("input[type=Submit][value=Login]");
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            },
            nextAction: function() {
                var userId = document.querySelector('input[id=username][type=text]');
                var passwordField = document.querySelector('input[id=label2][type=password]');
                var submitBtn = document.querySelector('input[type=submit][value=Login]');
                if(document.activeElement == userId){
                    passwordField.focus();
                }
                else{
                    if(document.activeElement != passwordField && passwordField.value == ""){
                        passwordField.focus();
                    }
                    else{
                        __juspay.clickOnElement(submitBtn);
                    }
                }
            },
            backButtonPressed: function() {
                try {
                    var passwordElement = document.querySelector('input[type=password][name=password]');
                    var submitElement = document.querySelector("input[type=submit][value=Login]");
                    var cancelTransaction =  document.querySelector('a[href="redirect.htm"]');
                    if(__uber.isUberEnabled("REDIRECT_DEBIT") && __uber.initSessionCounterIncremental("uberShownSBINBBP")==1
                        && passwordElement && submitElement) {
                        callDebitRedirectUber();
                    } else {
                        GK.showCancelTransactionDialog();
                    }
                } catch(err) {
                    GK.showCancelTransactionDialog();
                }
            }
        },
        { //SBINB: account selection page
            path: /\/merchant\/(merchantdisplay|loginsubmit).htm/,
            hostname:/(www|m|merchant)?\.onlinesbi.com/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector("input[type=submit][name=Submit]");
            },
            action: function() {
                Gatekeeper.removeFragment("Reached Account Selection Page");
                var numberOfAccounts = document.querySelectorAll('input[type=radio]').length
                var backButtonPressed = Gatekeeper.getSessionAttribute("backButtonPressed");
                var isSelectPresent = document.querySelector('tr[class=selectedRow]');
                var confirmBtn = document.querySelector("input[type=submit][name=Submit]")
                var accounts = document.getElementsByTagName("label");

                if(isSelectPresent) {
                        accountSelectionPage('label',"SBINB","input[type=submit][name=Submit]",'');
                }
            },
            nextAction: function(){
                var confirmBtn = document.querySelector("input[type=submit][name=Submit]")
                __juspay.clickOnElement(confirmBtn);
            }
        },
        {//SBINB: confirm details page
            path: /\/merchant\/merchantinter.htm/,
            hostname:/(www|m|merchant)?\.onlinesbi.com/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return (document.querySelector("input[type=button][name=Button2]") &&
                document.querySelector("input[type=hidden][name=smartotpflag]"));

            },
            action: function() {
                var submitBtn = document.querySelector("input[type=button][name=Button2]");
                var checkbox = document.querySelector('input[name="aiplChkBox"][id="aiplChkBox"]');
                var backButton = document.querySelector('input[id=backButton]');
                var smartOTP = document.querySelector("input[type=hidden][name=smartotpflag]")
                var smartotpflag = smartOTP.value;
                var otp_mode = document.querySelector("input[type=radio][name=smartOTP]");
                var online_otp = document.querySelector("input[type=radio][id=onlineOTP]")
                var offline_otp = document.querySelector("input[type=radio][id=offileOTP]")
                if(backButton){
                    backButton.addEventListener('click', function() {
                        Gatekeeper.setSessionAttribute("backButtonPressed", "TRUE");
                    });
                }
                if(checkbox){
                    __juspay.clickOnElement(checkbox);
                    checkbox.checked = true;
                }
                if(submitBtn){
                    __juspay.trackPageStatus("CONFIRMATION_PAGE");
                    if (smartotpflag === "Y" && otp_mode){
                        Gatekeeper.setSessionAttribute("SBINB_GEN_OTP_USING_APP", "Y");
                        Gatekeeper.removeFragment("Customer proceeds with SB Secure OTP APP, Can't AutoConfirm");
                        window.trackEventv2_0default("acs","info","sbinb","SBINB_GEN_OTP_USING_APP");
                        __juspay.scrollToElement(otp_mode);
                        Gatekeeper.showNetbankingDefaultFragment();
                        Gatekeeper.changeNextActionText("Confirm");
                        if(online_otp){
                            online_otp.checked = true;
                        }
                        submitBtn.addEventListener("click", function() {
                            if (online_otp.checked == true) {
                                window.trackEventv2_0("acs","info","sbinb_otp_mode", "online_mode", "sbinb", "otp_mode");
                            }
                            if (offline_otp.checked == true) {
                                window.trackEventv2_0("acs","info","sbinb_otp_mode", "offline_mode", "sbinb", "otp_mode");
                            }
                        });
                    } else {
                        Gatekeeper.setSessionAttribute("SBINB_GEN_OTP_USING_APP","N");
                        if(__juspay.isMobUiEnabled("AUTO_CLICK_SBINB")){
                            window.trackEventv2_0default("acs","info","AUTO_CLCIK","CONFIRMATION_PAGE");
                            __juspay.clickOnElement(submitBtn);
                        }
                    }
                } else{
                    Gatekeeper.removeFragment("Could not find submit button, DOM changed");
                }
            },
            nextAction: function() {
                var confirm_btn = document.querySelector("input[type=button][name=Button2]");
                if (confirm_btn){
                    __juspay.clickOnElement(confirm_btn);
                }
            }
        },
        {
            path: /\/retail\/forgotusernameconfirm\.htm/,
            hostname:/(www|m|merchant)?\.onlinesbi.com/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector("input[type=text][name=cifno]");
            },
            action: function() {
                __juspay.trackUnblocker("DETAILS_ERROR_PAGE");
                var expired=window.find('Your session has expired',true,false,true);
                var accDetailsForm = document.querySelector('form[name=forgotloginpwdform]');
                var errorMsg = document.querySelector("td[class=errormsg]");

                if (expired) {
                    window.getSelection().collapseToEnd();
                    Gatekeeper.destroyUber();
                    Gatekeeper.removeFragment("Session expired");
                    return;
                }
                if(errorMsg && errorMsg.innerHTML) {
                    try {
                        var currentUberProps = JSON.parse(Gatekeeper.getCurrentUberProps());
                        if(currentUberProps) {
                            if(currentUberProps.isShowing) {
                                var captchaImg = document.querySelector("img[alt=Captcha]");
                                if(captchaImg) {
                                    var canvas = document.createElement("canvas");
                                    canvas.style.display= "none";
                                    canvas.width = captchaImg.width;
                                    canvas.height = captchaImg.height;
                                    document.body.appendChild(canvas);
                                    var ctx = canvas.getContext("2d");
                                    ctx.drawImage(captchaImg,0,0,captchaImg.width,captchaImg.height);
                                    var imgData = canvas.toDataURL();
                                    if(imgData) {
                                        Gatekeeper.sendDataToUber("setCaptcha('"+imgData+"')");
                                        Gatekeeper.sendDataToUber("GK.acsData('"+errorMsg.innerHTML.trim()+"');");
                                        return;
                                    }
                                }
                            }
                        }
                    } catch(err) {
                        window.trackEventv2_0("acs","error","SBINB_WRONG_DETAILS","Function: action and Error is "+String(err), "SBINB", "wrong_details");
                    }
                    Gatekeeper.destroyUber();
                    Gatekeeper.removeFragment("Error uber props");
                }
            },
            onDataFromUber: function(data) {
                 var cifno = document.querySelector("input[id=cifno]");
                 var countryCode = document.querySelector("select[id=selCountry]");
                 var countryOptions = countryCode && countryCode.querySelectorAll("country_Code1");
                 var mobileNumber = document.querySelector("input[id=mobileNumber]");
                 var captchaValue = document.querySelector("input[name=captchaValue]");
                 var submit = document.querySelector("input[id=go]");
                 countryCode.value = "91"
                 if(data) {
                     try {
                         var values = JSON.parse(data);
                         if(values.linkClicked == "true") {
                            Gatekeeper.destroyUber();
                            Gatekeeper.removeFragment("Redirecting to Reset password fragment");
                            window.location.href="https://retail.onlinesbi.com/retail/troubleloginhome.htm?bankCode=0";
                         }
                         else {
                             if(cifno && values.cifNo) {
                                 cifno.value = "000000"+values.cifNo;
                             }
                             if(countryCode && countryOptions && values.countryCode) {
                                 for(var i=0;i<countryOptions.length;i++) {
                                     if(countryOptions[i].text==values.countryCode) {
                                         countryCode.selectedIndex = i;
                                         selectCountryCode();
                                     }
                                 }
                             }
                             if(mobileNumber && values.mobileNumber) {
                                 mobileNumber.value = values.mobileNumber;
                             }
                             if(captchaValue && values.captchaValue) {
                                 captchaValue.value = values.captchaValue;
                             }
                             if(submit) {
                                 __juspay.clickOnElement(submit);
                                 window.trackEventv2_0default("acs","info","SBINB_RECOVER_USERNAME","submit_clicked");
                             }
                         }
                     } catch(err) {
                         Gatekeeper.destroyUber();
                         Gatekeeper.removeFragment("Invalid data from uber");
                         window.trackEventv2_0default("acs","error","data_from_uber","Function: onDataFromUber and Error is "+String(err));
                     }
                 }
            }
        },
        {  //SBINB: Recover username success page
             path: /\/retail\/forgotloginusernameconfirm\.htm/,
             hostname:/(www|m|merchant)?\.onlinesbi.com/,
             state: "UNUSED",
             local: false,
             bank: "SBINB",
             domCheck: function() {
                 return document.querySelector("td[class=confirmsucc]");
             },
             action: function() {
                __juspay.trackUnblocker("RECOVER_USERNAME_SUCCESSFUL");
                Gatekeeper.removeFragment("Recover username success");
             }
        },
        { //SBINB:OTP page
            path: /\/(merchant|npcimandate)\/(smsenablehighsecurity|resendsmsotp|smsenablehighsecurityconfirm|merchantinter).htm/,
            hostname:/(www|m|merchant)?\.onlinesbi\.(com|sbi)/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector("input[name=securityPassword][type=password]") &&
                       document.querySelector("button[id=confirmButton]");
            },
            action: function() {
                Gatekeeper.removeFragment("Reached OTP Page");
                var otpField = document.querySelector("input[name=securityPassword]");
                var resetBtn = document.querySelector("input[type=reset][name=Reset]");
                if(resetBtn){
                    __juspay.resetField(resetBtn);
                }
                if(otpField){
                    otpField.blur();
                    if (Gatekeeper.getSessionAttribute("SBINB_GEN_OTP_USING_APP") === "Y"){
                        __juspay.trackPageStatus("INPUT_MANUAL_OTP");
                        __juspay.trackAuthMethod("OTP");
                        attachPhonePasswordHelper(otpField);
                        focusElement(otpField);
                        __juspay.scrollToElement(otpField);
                    }else{
                        otpFragmentHelper("input[name=securityPassword]","a[onclick*= resendOTP]");
                    }
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[onclick = "resendOTP();"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[name=securityPassword]", "button[id=confirmButton]");
            },
            clickSubmitButton: function(){
                var otpField = document.querySelector("input[name=securityPassword]");
                var submitBtn = document.querySelector("button[id=confirmButton]");
                if(otpField.value!="" && submitBtn) {
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        {
            path: /\/npcimandate\/loginsubmit.htm/,
            hostname: "merchant.onlinesbi.com",
            state: "UNUSED",
            bank: "SBINB",
            domCheck: function(){
                return document.querySelector('input[type=checkbox][id=checkbox]') &&
                       document.querySelector('button[type=submit][id=Go]');
            },
            action: function() {
                var checkbox = document.querySelector('input[type=checkbox][id=checkbox]');
                var submitBtn = document.querySelector('button[type=submit][id=Go]');
                if(checkbox){
                    checkbox.checked = true;
                }
                if(__juspay.isMobUiEnabled("AUTO_CLICK_SBINB")){
                    window.trackEventv2_0default("acs","info","AUTO_CLCIK","MANDATE_CONFIRMATION_PAGE");
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        {
            path: /\/v1\/payments\/.*\/(authenticate|callback)/,
            hostname: "api.razorpay.com",
            state: "UNUSED",
            bank: "MANDATE",
            domCheck: function(){
                return document.querySelector('img[src*=nach]') &&
                        document.querySelector('img[src*=npci]') &&
                        document.querySelector('button[type=submit]');
            },
            action: function() {
                var submitBtn = document.querySelector('button[type=submit][id=submit-btn]');
                if(__juspay.isMobUiEnabled("AUTO_CLICK_MANDATE")){
                    window.trackEventv2_0default("acs","info","AUTO_CLCIK","RAZORPAY_MANDATE_CONFIRMATION_PAGE");
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        {
            path: /\/npcimandate\/smsenablehighsecurityconfirm.htm/,
            hostname: "merchant.onlinesbi.com",
            state: "UNUSED",
            bank: "MANDATE",
            domCheck: function(){
                return document.querySelector('input[id=merchantCode][value=E_NPCI]') &&
                        document.querySelector('a[onclick*=redirectToHandler]');
            },
            action: function() {
                var clickHere = document.querySelector('a[onclick*=redirectToHandler]');
                if(__juspay.isMobUiEnabled("AUTO_CLICK_MANDATE")){
                    window.trackEventv2_0default("acs","info","AUTO_CLCIK","SBI_CONFIRMATION_PAGE");
                    __juspay.clickOnElement(clickHere);
                }
            }
        },
        {
            path: /\/onmags\/bankResponse/,
            hostname: "enach.npci.org.in",
            state: "UNUSED",
            bank: "",
            domCheck: function(){
                return document.querySelector('input[id=Umrn_Success]') &&
                        document.querySelector('a[id=redirect]');
            },
            action: function() {
                var clickHere = document.querySelector('a[id=redirect]');
                if(__juspay.isMobUiEnabled("AUTO_CLICK_MANDATE")){
                    window.trackEventv2_0default("acs","info","AUTO_CLCIK","NPCI_CONFIRMATION_PAGE");
                    __juspay.clickOnElement(clickHere);
                }
            }
        },
        { //SBINB:Recover username OTP page
            path: /\/retail\/forgotusernameconfirm.htm/,
            hostname:/(www|m|merchant)?\.onlinesbi.com/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector("input[name=securityPassword][type=password]") ||
                       document.querySelector("input[type=button][name=Button4]");
            },
            action: function() {
                __juspay.trackUnblocker("INPUT_OTP_PAGE");
                Gatekeeper.destroyUber();
                otpFragmentHelper("input[name=securityPassword]","a[onclick*= resendOTP]");
            },
            regenerateOtp: function() {
                resendOTP();
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[name=securityPassword]", "input[type=button][name=Button4]");
                window.trackEventv2_0("acs","info","SBINB_RECOVER_USERNAME","otp_auto_detect", "SBINB", "recover_username");

            }
        },
        {
            path: /\/retail\/troubleloginhome.htm/,
            hostname: "retail.onlinesbi.com",
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector('form[name=otpErrorForm]') || document.querySelector('form[name=troubleLoginHome]')
            },
            action: function() {
                Gatekeeper.removeFragment("SBINB Reset Password");
                var expired=window.find('Your session has expired',true,false,true);
                var issueCode = document.querySelector("select[name=issueCode]");
                var options = issueCode && issueCode.querySelectorAll("option");
                var nextStep = document.querySelector("input[name=nextStep]");
                if (expired) {
                    window.getSelection().collapseToEnd();
                    if(__uber.initSessionCounterIncremental("resetSBINBForgotPassword")==1){
                        window.location.reload(true);
                    }
                } else if(issueCode && options && nextStep) {
                    for(var i =0;i<options.length;i++) {
                        if(options[i].value==="loginPassword") {
                            issueCode.selectedIndex=i;
                            if(nextStep) {
                                Gatekeeper.showWaitingFragment();
                                __juspay.clickOnElement(nextStep);
                            }
                        }
                    }
                }
            }
        },

        {
            path: /\/retail\/troublelogindetails.htm/,
            hostname: "retail.onlinesbi.com",
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector('form[name=forgotloginpwdform]')
            },
            action: function() {
                __juspay.trackUnblocker("DETAILS_PAGE_ONE");
                var expired=window.find('Your session has expired',true,false,true);
                var accDetailsForm = document.querySelector('form[name=forgotloginpwdform]');
                if (expired) {
                    window.getSelection().collapseToEnd();
                    Gatekeeper.destroyUber();
                    Gatekeeper.removeFragment("Session expired");
                    return;
                } else {
                    GK.setUberEvent("SBINB_RESET_PWD_HELP",{type:"sliding_uber",width:-1,height:-1,showOnLoad:false});
                }
                setTimeout(function(){
                    try {
                        var currentUberProps = JSON.parse(Gatekeeper.getCurrentUberProps());
                        if(currentUberProps) {
                            if(currentUberProps.isShowing) {
                                __juspay.trackUnblocker("UBER_FORM_SHOWN");
                                var captchaImg = document.querySelector("img[alt=Captcha]");
                                if(captchaImg) {
                                    var canvas = document.createElement("canvas");
                                    canvas.style.display= "none";
                                    canvas.width = captchaImg.width;
                                    canvas.height = captchaImg.height;
                                    document.body.appendChild(canvas);
                                    var ctx = canvas.getContext("2d");
                                    ctx.drawImage(captchaImg,0,0,captchaImg.width,captchaImg.height);
                                    var imgData = canvas.toDataURL();
                                    if(imgData) {
                                        var bankCustomerId = Gatekeeper.getBankCustomerId();
                                        if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined") {
                                            bankCustomerId = "";
                                        }
                                        Gatekeeper.sendDataToUber("setUserName('"+bankCustomerId+"')");
                                        Gatekeeper.sendDataToUber("setCaptcha('"+imgData+"')");
                                        window.trackEventv2_0("acs","info","SBINB_RESET_PASSWORD","CAPTCHA_DISPLAYED", "SBINB", "reset_password");
                                        return;
                                    }
                                }
                            }
                        }
                    } catch(err) {
                        window.trackEventv2_0("acs","error","uber_props_error","Function: action and Error is "+String(err), "uber_props", "error");
                    }
                    Gatekeeper.destroyUber();
                    Gatekeeper.removeFragment("Error while uber props");
                },10000);

            },
            onDataFromUber: function(data) {
                var username = document.querySelector("input[id=userName]");
                var accountNo = document.querySelector("input[id=accountNo]");
                var countryCode = document.querySelector("select[id=selCountry]");
                var countryOptions = countryCode && countryCode.querySelectorAll("option");
                var mobileNumber = document.querySelector("input[id=mobileNo]");
                var dob = document.querySelector("input[id=datepicker5]");
                var captchaValue = document.querySelector("input[name=captchaValue]");
                var submit = document.querySelector("input[id=go]");
                if(data) {
                    try {
                        var values = JSON.parse(data);
                        if(values.hasLoaded == true) {
                            try {
                                var currentUberProps = JSON.parse(Gatekeeper.getCurrentUberProps());
                                if(currentUberProps) {
                                    if(currentUberProps.isShowing) {
                                        var bankCustomerId = Gatekeeper.getBankCustomerId();
                                        if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined") {
                                            bankCustomerId = "";
                                        }
                                        Gatekeeper.sendDataToUber("setUserName('"+bankCustomerId+"')");
                                        var captchaImg = document.querySelector("img[alt=Captcha]");
                                        if(captchaImg) {
                                            var canvas = document.createElement("canvas");
                                            canvas.style.display= "none";
                                            canvas.width = captchaImg.width;
                                            canvas.height = captchaImg.height;
                                            document.body.appendChild(canvas);
                                            var ctx = canvas.getContext("2d");
                                            ctx.drawImage(captchaImg,0,0,captchaImg.width,captchaImg.height);
                                            var imgData = canvas.toDataURL();
                                            if(imgData) {
                                                Gatekeeper.sendDataToUber("setCaptcha('"+imgData+"')");
                                                return;
                                            }
                                        }
                                    }
                                }
                            } catch(err) {
                                window.trackEventv2_0("acs","error","uber_props_error","Function: action and Error is "+String(err), "uber_props", "error");
                            }
                            Gatekeeper.destroyUber();
                            Gatekeeper.removeFragment("Error while uber props");
                            return;
                        }
                        if(values.linkClicked == "true") {
                            Gatekeeper.destroyUber();
                            Gatekeeper.removeFragment("Redirecting to Recover Username Uber");
                            window.location.href="https://retail.onlinesbi.com/retail/forgotusername.htm?bankCode=0";
                        }
                        else {
                            if(username && values.userName) {
                                username.value = values.userName;
                            }
                            if(accountNo && values.accountNo) {
                                accountNo.value = values.accountNo;
                            }
                            if(countryCode && countryOptions && values.countryCode) {
                                for(var i=0;i<countryOptions.length;i++) {
                                    if(countryOptions[i].text==values.countryCode) {
                                        countryCode.selectedIndex = i;
                                        selectCountryCode();
                                    }
                                }
                            }
                            if(mobileNumber && values.mobileNumber) {
                                mobileNumber.value = values.mobileNumber;
                            }
                            if(dob && values.dob) {
                                dob.value = values.dob;
                            }
                            if(captchaValue && values.captchaValue) {
                                captchaValue.value = values.captchaValue;
                            }
                            if(submit) {
                                __juspay.clickOnElement(submit);
                                window.trackEventv2_0("acs","info","SBINB_RESET_PASSWORD","submit_clicked", "SBINB", "reset_password");
                            }
                        }
                    } catch(err) {
                        Gatekeeper.destroyUber();
                        Gatekeeper.removeFragment("Invalid data from uber");
                        window.trackEventv2_0default("acs","error","data_from_uber","Function: onDataFromUber and Error is "+String(err));
                    }
                }
            }
        },

        { //SBINB Reset Password Error Page
            path: /\/retail\/forgotloginpwd.htm/,
            hostname: "retail.onlinesbi.com",
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector('form[name=forgotloginpwdform]') || document.querySelector('form[name=otpErrorform]')
            },
            action: function() {
                __juspay.trackUnblocker("DETAILS_ERROR_PAGE");
                var expired=window.find('Your session has expired',true,false,true);
                var accDetailsForm = document.querySelector('form[name=forgotloginpwdform]');
                var errorMsg = document.querySelector("td[class=errormsg]");
                if (expired) {
                    window.getSelection().collapseToEnd();
                    Gatekeeper.destroyUber();
                    Gatekeeper.removeFragment("Session expired");
                    return;
                }
                if(errorMsg && errorMsg.innerHTML) {
                try {
                    var currentUberProps = JSON.parse(Gatekeeper.getCurrentUberProps());
                    if(currentUberProps) {
                        if(currentUberProps.isShowing) {
                            var captchaImg = document.querySelector("img[alt=Captcha]");
                            if(captchaImg) {
                                var canvas = document.createElement("canvas");
                                canvas.style.display= "none";
                                canvas.width = captchaImg.width;
                                canvas.height = captchaImg.height;
                                document.body.appendChild(canvas);
                                var ctx = canvas.getContext("2d");
                                ctx.drawImage(captchaImg,0,0,captchaImg.width,captchaImg.height);
                                var imgData = canvas.toDataURL();
                                if(imgData) {
                                    Gatekeeper.sendDataToUber("setCaptcha('"+imgData+"')");
                                    Gatekeeper.sendDataToUber("GK.acsData('"+errorMsg.innerHTML.trim()+"');");
                                    window.trackEventv2_0("acs","info","SBINB_RESET_PASSWORD","CAPTCHA_DISPLAYED", "SBINB", "reset_password");
                                    return;
                                }
                            }
                        }
                    }
                    } catch(err) {
                        window.trackEventv2_0("acs","error","sbinb_wrong_details","Function: action and Error is "+String(err),"SBINB","wrong_details");
                        Gatekeeper.destroyUber();
                        Gatekeeper.removeFragment("Error uber props");
                    }
                }
            },
            onDataFromUber: function(data) {
                var username = document.querySelector("input[id=userName]");
                var accountNo = document.querySelector("input[id=accountNo]");
                var countryCode = document.querySelector("select[id=selCountry]");
                var countryOptions = countryCode && countryCode.querySelectorAll("option");
                var mobileNumber = document.querySelector("input[id=mobileNo]");
                var dob = document.querySelector("input[id=datepicker5]");
                var captchaValue = document.querySelector("input[name=captchaValue]");
                var submit = document.querySelector("input[id=go]");
                if(data) {
                    try {
                        var values = JSON.parse(data);
                        if(values.linkClicked == "true") {
                            Gatekeeper.destroyUber();
                            Gatekeeper.removeFragment("Redirecting to Recover username fragment");
                            window.location.href="https://retail.onlinesbi.com/retail/forgotusername.htm?bankCode=0";
                        }
                        else {
                            if(username && values.userName) {
                                username.value = values.userName;
                            }
                            if(accountNo && values.accountNo) {
                                accountNo.value = values.accountNo;
                            }
                            if(countryCode && countryOptions && values.countryCode) {
                                for(var i=0;i<countryOptions.length;i++) {
                                    if(countryOptions[i].text==values.countryCode) {
                                        countryCode.selectedIndex = i;
                                        selectCountryCode();
                                    }
                                }
                            }
                            if(mobileNumber && values.mobileNumber) {
                                mobileNumber.value = values.mobileNumber;
                            }
                            if(dob && values.dob) {
                                dob.value = values.dob;
                            }
                            if(captchaValue && values.captchaValue) {
                                captchaValue.value = values.captchaValue;
                            }
                            if(submit) {
                               __juspay.clickOnElement(submit);
                               window.trackEventv2_0("acs","info","SBINB_RESET_PASSWORD","submit_clicked", "SBINB", "reset_password");
                            }
                        }
                    } catch(err) {
                        Gatekeeper.destroyUber();
                        Gatekeeper.removeFragment("Invalid data from uber");
                        window.trackEventv2_0("acs","error","uber_props_error","Function: action and Error is "+String(err), "uber_props", "error");
                    }
                }
            }
        },

        {
             path: /\/retail\/forgotusername.htm/,
             hostname: "retail.onlinesbi.com",
             state: "UNUSED",
             local: false,
             bank: "SBINB",
             domCheck: function() {
                 return document.querySelector('form[name=otpErrorForm]') || document.querySelector('form[name=forgotloginusenameform]')
             },
             action: function() {
                __juspay.trackUnblocker("DETAILS_PAGE_ONE");
                 Gatekeeper.removeFragment("SBINB Recover Username");
                 var expired=window.find('Your session has expired',true,false,true);
                 if (expired) {
                     window.getSelection().collapseToEnd();
                     if(__uber.initSessionCounterIncremental("resetSBINBForgotUsername")==1){
                         window.location.reload(true);
                     }
                 } else {
                     Gatekeeper.showWaitingFragment();
                     GK.setUberEvent("SBINB_RESET_UNAME_HELP", {type:"sliding_uber", width:-1,height:-1,showOnLoad:false });
                 }
                 setTimeout(function(){
                       try {
                           var currentUberProps = JSON.parse(Gatekeeper.getCurrentUberProps());
                           if(currentUberProps) {
                               if(currentUberProps.isShowing) {
                                    __juspay.trackUnblocker("UBER_FORM_SHOWN");
                                    var captchaImg = document.querySelector("img[alt=Captcha]");
                                    if(captchaImg) {
                                        var canvas = document.createElement("canvas");
                                        canvas.style.display= "none";
                                        canvas.width = captchaImg.width;
                                        canvas.height = captchaImg.height;
                                        document.body.appendChild(canvas);
                                        var ctx = canvas.getContext("2d");
                                        ctx.drawImage(captchaImg,0,0,captchaImg.width,captchaImg.height);
                                        var imgData = canvas.toDataURL();
                                        if(imgData) {
                                            Gatekeeper.sendDataToUber("setCaptcha('"+imgData+"')");
                                            window.trackEventv2_0("acs","error","SBINB_RECOVER_USERNAME","CAPTCHA_DISPLAYED","SBINB","recover_username");
                                            return;
                                        }
                                    }
                               }
                           }
                       } catch(err) {
                           window.trackEventv2_0("acs","error","uber_props_error","Function: action and Error is "+String(err), "uber_props", "error");
                           Gatekeeper.destroyUber();
                           Gatekeeper.removeFragment("Error while uber props");
                       }
               },10000);

             },
             onDataFromUber: function(data) {
                 var cifno = document.querySelector("input[id=cifno]");
                 var countryCode = document.querySelector("select[id=selCountry]");
                 var countryOptions = countryCode && countryCode.querySelectorAll("country_Code1");
                 var mobileNumber = document.querySelector("input[id=mobileNumber]");
                 var captchaValue = document.querySelector("input[name=captchaValue]");
                 var submit = document.querySelector("input[id=go]");
                 countryCode.value = "91"
                 if(data) {
                     try {
                         var values = JSON.parse(data);
                         if(values.hasLoaded == true || values.pageLoaded == true) {
                             try {
                                var currentUberProps = JSON.parse(Gatekeeper.getCurrentUberProps());
                                if(currentUberProps) {
                                   if(currentUberProps.isShowing) {
                                         var captchaImg = document.querySelector("img[alt=Captcha]");
                                         if(captchaImg) {
                                             var canvas = document.createElement("canvas");
                                             canvas.style.display= "none";
                                             canvas.width = captchaImg.width;
                                             canvas.height = captchaImg.height;
                                             var ctx = canvas.getContext("2d");
                                             ctx.drawImage(captchaImg,0,0,captchaImg.width,captchaImg.height);
                                             var imgData = canvas.toDataURL();
                                             if(imgData) {
                                                 Gatekeeper.sendDataToUber("setCaptcha('"+imgData+"')");
                                                 return;
                                             }
                                         }
                                   }
                                }
                             } catch(err) {
                                window.trackEventv2_0("acs","error","uber_props_error","Function: action and Error is "+String(err), "uber_props", "error");
                                Gatekeeper.destroyUber();
                                Gatekeeper.removeFragment("Error while uber props");
                             }
                             return;
                         }
                         else if(values.linkClicked == "true") {
                             Gatekeeper.destroyUber();
                             Gatekeeper.removeFragment("Redirecting to Reset password fragment");
                             window.location.href="https://retail.onlinesbi.com/retail/troubleloginhome.htm?bankCode=0";
                         }
                         else
                         {
                             if(cifno && values.cifNo) {
                                 cifno.value = "000000"+values.cifNo;
                             }
                             if(countryCode && countryOptions && values.countryCode) {
                                 for(var i=0;i<countryOptions.length;i++) {
                                     if(countryOptions[i].text==values.countryCode) {
                                         countryCode.selectedIndex = i;
                                         selectCountryCode();
                                     }
                                 }
                             }
                             if(mobileNumber && values.mobileNumber) {
                                 mobileNumber.value = values.mobileNumber;
                             }
                             if(captchaValue && values.captchaValue) {
                                 captchaValue.value = values.captchaValue;
                             }
                             if(submit) {
                                 __juspay.clickOnElement(submit);
                                 window.trackEventv2_0("acs","info","SBINB_RECOVER_USERNAME","submit_clicked", "SBINB", "recover_username");
                             }
                         }
                     } catch(err) {
                         Gatekeeper.destroyUber();
                         Gatekeeper.removeFragment("Invalid data from uber");
                         window.trackEventv2_0default("acs","error","data_from_uber","Function: onDataFromUber and Error is "+String(err));
                     }
                 }
             }
         },

        { //SBINB Reset Password OTP Page
            path: /\/retail\/forgotloginpwd.htm/,
            hostname: "retail.onlinesbi.com",
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck:function() {
                return document.querySelector("input[name=securityPassword]") &&
                                       document.querySelector("input[type=button][name=Button4]");
            },
            action: function() {
                __juspay.trackUnblocker("INPUT_OTP_PAGE");
                Gatekeeper.destroyUber();
                Gatekeeper.removeFragment("SBINB reset password otp page reached")
                setTimeout(function(){
                    otpFragmentHelper('input[name=securityPassword]',"a[onclick*= resendOTP]");
                },2000);
            },
            regenerateOtp: function() {
                resendOTP('no');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[name=securityPassword]", "input[type=button][name=Button4]");
                __juspay.trackUnblocker("otp_auto_detect");
            }
        },
        { //SBI_NB: session error
            path: /\/merchant\/loginerror.htm/,
            hostname:/(www|m|merchant)?\.onlinesbi.com/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck:function() {
                return true;
            },
            action: function() {
                if(window.find("Session Error", true, false, true)){
                    Gatekeeper.trackUserError("session_error");
                    window.getSelection().collapseToEnd();
                }
                Gatekeeper.removeFragment("Session Error");
            }
        },
        { //SBI_NB: session timeout
            path: /\/merchant\/sessiontimeoutconfirm.htm/,
            hostname:/(www|m|merchant)?\.onlinesbi.com/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck:function() {
                return true;
            },
            action: function() {
                if(window.find("Your session has expired.", true, false, true)){
                    Gatekeeper.trackUserError("session_timeout");
                    window.getSelection().collapseToEnd();
                }
                Gatekeeper.removeFragment("Session Time out");
            }
        },
        { // SBINB Maintanance page
            path: /\/(sbi_merchant_maintenance|sbi_maintenance)/,
            hostname:/(www|m|merchant)?\.onlinesbi.com/,
            bank: "SBINB",
            domCheck:function() {
                var el1 = document.querySelector('div[class=txt]');
                var el2 = document.querySelector('div[class=foot_txt]');
                if(el1 && el2){
                    return el1.innerText.indexOf("Will be back soon") > -1 && el2.innerText.indexOf("maintenance page") > -1;
                } else {
                    return false;
                }
            },
            action: function() {
                try{
                    __juspay.trackUnblocker("MAINTENANCE_PAGE");
                    if(__uber.isUberEnabled("REDIRECT_DEBIT") && __uber.initSessionCounterIncremental("uberShownSBINBBP")==1) {
                        callDebitRedirectUber();
                    }
                } catch(err) {
                    window.trackEventv2_0default("acs","error","REDIRECT_DEBIT","error_loading_uber"+String(err));
                }
            },
            backButtonPressed: function(){
                try{
                    if(__uber.isUberEnabled("REDIRECT_DEBIT") && __uber.initSessionCounterIncremental("uberShownSBINBBP")==1) {
                        callDebitRedirectUber();
                    } else {
                        GK.showCancelTransactionDialog();
                    }
                } catch(err) {
                    window.trackEventv2_0default("acs","error","REDIRECT_DEBIT","error_loading_uber_on_bp"+String(err));
                    GK.showCancelTransactionDialog();
                }
            }
        },
        { //SBI_NB STATUS PAGE
            path: /\/merchant\/smsenablehighsecurityconfirm.htm/,
            hostname:/(www|m|merchant)?\.onlinesbi.com/,
            bank: "SBINB",
            domCheck:function() {
                return true;
            },
            action: function() {
                Gatekeeper.removeFragment("Reached STATUS page");
                __juspay.trackPageStatus("SBINB_STATUS_PAGE");
                var tdarray = document.querySelectorAll("td")
                var flag =0;
                for (i=0; i< tdarray.length;i++)
                {
                    if(/Status/i.test(tdarray[i].innerText) )
                    {
                        flag = i;
                    }
                }
                if( flag !== 0){
                    window.trackEventv2_0("acs","info","SBINB_STATUS","-Content: "+tdarray[flag+1].innerText,"SBINB", "status");
                }
            }
        },
        //SBINB new page otp page
        {
            path: /\/(merchant|mmerchant)\/(smsenablehighsecurity|resendsmsotp).htm/,
            hostname: /merchant\.onlinesbi\.(sbi|com)/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck:function() {
                return document.querySelector('input[name=securityPassword][type=password]') &&
                      document.querySelector('input[type=submit][id=confirmButton]')
            },
            action: function(){
                var resendButton = document.querySelector('a[onclick*=resendOTP]')
                if(resendButton){
                    otpFragmentHelper("input[name=securityPassword][type=password]","a[onclick*= resendOTP]");
                }else{
                    otpFragmentHelper("input[name=securityPassword][type=password]");
                }
            },
            regenerateOtp: function() {
                var resendButton = document.querySelector('a[onclick*="resendOTP"]');
                if(resendButton){
                    __juspay.clickOnElement(resendButton);
                }
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[name=securityPassword][type=password]","input[type=submit][id=confirmButton]");
            }
        },
        //SBINB:remark page
        {
            path: /\/(merchant|mmerchant)\/merchantinter\.htm/,
            hostname: /merchant\.onlinesbi\.(sbi|com)/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector('form[name=merchantConfirm]') &&
                        document.querySelector('button[id=confirmButton]') &&
                        document.querySelector('button[id=backButton]');
            },
            action: function(){
                var textCheck = document.querySelector('h1[class="text_e"]').textContent;
                var cancelButton = document.querySelector('a[href*="redirect.htm"]');
                window.trackEventv2_0default("acs","info","SBINB","REACHED_SBINB_REMARK_PAGE");
                if(__juspay.isFeatureEnabled("SBINB_AUTO_CLICK")) {
                    if(textCheck.includes("Verify and confirm")){
                        var confirmButton = document.querySelector('button[id=confirmButton]');
                        __juspay.clickOnElement(confirmButton);
                        window.trackEventv2_0default("acs","info","SBINB","AUTO_CLICKING_REMARK_PAGE");
                    }
                }
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                    }, false);
                }
            }
        },
        //SBINB:account selection page
        {
            path: /\/(merchant|mmerchant)\/(merchantdisplay|loginsubmit)\.htm/,
            hostname: /merchant\.onlinesbi\.(sbi|com)/,
            state: "UNUSED",
            local: false,
            bank: "SBINB",
            domCheck: function() {
                return document.querySelector("tr[onclick*='selectAccountNo']") &&
                        document.querySelector('button[type=submit][id=Go]');
            },
            action: function(){
                var cancelButton = document.querySelector('a[href*="redirect.htm"]');
                var textCheck = document.querySelector("tr[onclick*='selectAccountNo']");
                var radioButton = document.querySelectorAll("input[type='radio']");
                window.trackEventv2_0default("acs","info","SBINB","REACHED_SBINB_ACCOUNT_SELECTION_PAGE");
                if(__juspay.isFeatureEnabled("SBINB_AUTO_CLICK")) {
                    if(textCheck && radioButton.length === 1){
                        var confirmButton = document.querySelector('button[type=submit][id=Go]');
                        __juspay.clickOnElement(confirmButton);
                        window.trackEventv2_0default("acs","info","SBINB","AUTO_CLICKING_SINGLE_ACCOUNT_SELECTION_PAGE");
                    }
                }
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                    }, false);
                }
            }
        },
        //SBIDC: new billdesk page
        {
            path: /\/pgidsk\/ProcessPayment/,
            hostname: "www.billdesk.com",
            state: "UNUSED",
            local: false,
            bank: "SBIDC",
            domCheck: function () {
                var labelOtp = document.querySelector("label");
                if(labelOtp){
                    if(labelOtp.innerText !== "Enter One Time Password (OTP) "){
                        return false;
                    }
                }
                return labelOtp && document.querySelector('input[name=customerpin]') &&
                       document.querySelector('button[type=button]') &&
                       document.querySelector('img[src*=sm3]');
            },
            action: function(){
                var submitBtn = document.querySelector('button[type=button]');
                if(submitBtn){
                    otpFragmentHelper('input[name=customerpin]','a[class=request-link]');
                    window.trackEventv2_0default("acs","info","billdesk_page","sbidc");
                }
            },
            submitOtp:function(otp){
                submitOtpHelper(otp, "input[name=customerpin]", "button[type=button]");
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector("a[class=request-link]");
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },

        //SBI: billdesk page
        //TODO: Will cause issue
        {
            path: /\/pgidsk\/ProcessPayment/,
            hostname: "www.billdesk.com",
            state: "UNUSED",
            local: false,
            bank: "SBIDC",
            domCheck: function () {
                var labelOtp = document.querySelector("label[for=txtOTP]");
                if(labelOtp){
                    if(labelOtp.innerText !== "One Time Password"){
                        return false;
                    }
                }
                return labelOtp && document.querySelector('input[type=password]') &&
                       document.querySelector('button[type=button][name=btnSubmit]') &&
                       document.querySelector('img[src*=sbi]');
            },
            action: function(){
                var submitBtn = document.querySelector('button[type=button][name=btnSubmit]');
                if(submitBtn){
                    window.trackEventv2_0default("acs","info","billdesk_page","sbidc");
                    otpFragmentHelper('input[type=password]','p[class=requestOTP] > a');
                }

            },
            submitOtp:function(otp){
                submitOtpHelper(otp, "input[type=password]", "button[name=btnSubmit]");
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector("p[class=requestOTP] > a")
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        // SBIDC new page added on 18-04-2017
        {
            path: /\/bdacs\/SBIValidate/,
            hostname: "acs2.onlinesbi.com",
            state: "UNUSED",
            local: false,
            bank: "SBIDC",
            domCheck: function () {
                return (document.querySelector('input[name=customerotp]') || document.querySelector('input[name=customerpin]')) &&
                       document.querySelector('button[type=button][onclick*=ValidateForm]')
            },
            action: function(){
                var otpFieldDom1 = document.querySelector('input[name=customerotp]');
                var otpFieldDom2 = document.querySelector('input[name=customerpin]');
                var mastercard = document.querySelector('img[src*=mastercardSecure]');
                var otpField = otpFieldDom1 ? otpFieldDom1 : otpFieldDom2;
                if(otpField == otpFieldDom1){
                    otpFragmentHelper('input[name=customerotp]','a[class*=request]');
                }else{
                    otpFragmentHelper('input[name=customerpin]','a[class*=request]');
                }
                var cancelButton = document.querySelector('a[href*=cancelForm]');
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                    }, false);
                }
                if(mastercard){
                    Gatekeeper.setCardBrand(MASTERCARD);
                }
                window.trackEventv2_0default("acs","info","sbidc","sbi_new_domain_page");
            },
            submitOtp: function(otp){
                var otpFieldDom1 = document.querySelector('input[name=customerotp]');
                var otpFieldDom2 = document.querySelector('input[name=customerpin]');
                if(otpFieldDom1)
                    submitOtpHelper(otp, "input[name=customerotp]", "button[type=button][onclick*=ValidateForm]");
                else if(otpFieldDom2)
                    submitOtpHelper(otp, "input[name=customerpin]", "button[type=button][onclick*=ValidateForm]");
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[class*=request]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //YESDC OTP page
        {
            path: /\/PAReq\.do/,
            hostname: "3dsecure.yesbank.in",
            state: "UNUSED",
            bank: "YESDC",
            domCheck: function(){
                return  document.querySelector('input[name="otp"][type="password"]') &&
                        document.querySelector('img[src*="submitbutton"]') &&
                        document.querySelector('img[src*="yesbank"]');
            },
            action: function(){
                otpFragmentHelper('input[name="otp"][type="password"]','img[src*="resendbutton"]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[name="otp"][type="password"]', 'img[src*="submitbutton"]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('img[src*="resendbutton"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //YESDC OTP Page
        {
            path: /\/acs-v1\/processauth/,
            hostname: "sdc-yb.enstage-sas.com",
            state: "UNUSED",
            bank: "YESDC",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=otpValue]') &&
                        document.querySelector('img[src*="8172/images/logo"]') &&
                        document.querySelector('a[id=submitBtn]') &&
                        document.querySelector('a[onclick*=resendOtpform]');
            },
            action: function()
            {
                otpFragmentHelper('input[id=otpValue]','a[onclick*=resendOtpform]');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=otpValue]", "a[id=submitBtn]");
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('a[onclick*=resendOtpform]');
                __juspay.clickOnElement(resendLink);
            }
        },
        //YESCC:otp page support 26/12/2017
        {
            path: /\/acs-web-v12\/EnrollWeb\/YESBank\/server\/AccessControlServer|OtpServer/,
            hostname: "pdc-yb.enstage-sas.com",
            state: "UNUSED",
            local: false,
            bank: "YESCC",
            domCheck: function() {
                return document.querySelector('input[name=otpValue][id=otpValue]') &&
                       document.querySelector("input[src*=btn_submit]") &&
                       document.querySelector("form[action*=YESBank]") &&
                       document.querySelector("span[id=ms_timer]") &&
                       document.querySelector("a[href*=resend_otp]");
            },
            action: function(){
                var otpField = document.querySelector('input[name=otpValue][id=otpValue]');
                var regenerateBtn = document.querySelector("a[href*=resend_otp]");
                var masterCard = document.querySelector("img[src*=Mastercard_Securecode_bg]");
                otpFragmentHelper("input[name=otpValue][id=otpValue]","a[href*=resend_otp]");
                if(masterCard) {
                    Gatekeeper.setCardBrand(MASTERCARD);
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, "input[name=otpValue][id=otpValue]", "input[src*=btn_submit]");
                Gatekeeper.removeFragment("otp submitted from juspay fragment");
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector("a[href*=resend_otp]");
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //ICICI NB OTP Page(with Grid)
        {
            path: /\/corp\/(Finacle|AuthenticationController)/,
            hostname: "shopping.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name="Action.SUBMIT_TRANSACTION"][type=submit]') || document.querySelector('input[type="Submit"][name="Action.TRAN_PWD"]') &&
                    document.querySelector('input[name="TranRequestManagerFG.ONE_TIME_PASSWORD__"]');
            },
            action: function() {
                Gatekeeper.removeFragment("OTP Page reached");
                var otpTB = document.querySelector('input[name="TranRequestManagerFG.ONE_TIME_PASSWORD__"]');
                var grid1 = document.getElementById('TranRequestManagerFG.GRID_CARD_AUTH_VALUE_1__');
                var grid2 = document.getElementById('TranRequestManagerFG.GRID_CARD_AUTH_VALUE_2__');
                var grid3 = document.getElementById('TranRequestManagerFG.GRID_CARD_AUTH_VALUE_3__');
                var submitButton = document.querySelector('input[name="Action.SUBMIT_TRANSACTION"][type=submit]');

                if(typeof Gatekeeper.setSmsBackReadTime == "function") {
                    Gatekeeper.setSmsBackReadTime(2*60*1000);
                }
                var cancelButton = document.querySelector("a[title=Proceed][id=Caption27691536]");
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                    }, false);
                }
                if (otpTB) {
//                    attachPhoneKeyboard(otpTB); commenting because of samsung half heyboard issue
                    if(/iP(hone|od|ad)/.test(navigator.platform)){
                        otpTB.blur();
                        otpTextFieldQuery('input[name="TranRequestManagerFG.ONE_TIME_PASSWORD__"]');
                        // Adding style explicitly since changing the input type affects the element check in CSS style.
                        otpTB.style.cssText = "width: 100%; resize: none; padding-top: 1em; padding-bottom: 1em; height: 1.5em; outline: 0; color: black; font-size: 1em; border: 0;";
                    }
                    otpTB.addEventListener("click", function (){
                        Gatekeeper.hideAssistanceFragment();
                    }, false);
                }

                var activateOtpField = function() {
                    otpTB.blur();
                    __juspay.scrollToElement(otpTB);
                    __juspay.trackPageStatus("INPUT_OTP");
                    __juspay.trackAuthMethod("OTP");
                    __juspay.delayMe(function (){
                        Gatekeeper.requestKeyboardHide();
                        __juspay.showOtpFragmentWrapper(otpTB);
                    });  //100ms delay to hide the keyboard
                }
                if(otpTB && !(grid1 && grid2 && grid3)){
                    activateOtpField();
                }
                else if (otpTB && (grid1 && grid2 && grid3)){
                    activateOtpField();
                    Gatekeeper.setShouldAutoSubmitOtp(false);
                }
                else if (grid1 && grid2 && grid3){
                    grid1.blur();
                    focusElement(grid1);
                    __juspay.trackPageStatus("INPUT_GRID_VALUES");
                    __juspay.trackAuthMethod("GRID_VALUE");
                    var clickHandler = function(){
                         Gatekeeper.hideAssistanceFragment();
                    }
                    grid1.addEventListener("click",clickHandler,false);
                    grid2.addEventListener("click",clickHandler,false);
                    grid3.addEventListener("click",clickHandler,false);

                    grid3.addEventListener("input",function(){
                        if(grid3.value.length == 2){
                            grid3.blur();
                            Gatekeeper.requestKeyboardHide();
                            if(otpTB){
                                activateOtpField();
                                __juspay.delayMe(function(){
                                    __juspay.scrollToElement(otpTB);
                                },200);
                            }
                        }
                    },false);
                }
            },
            submitOtp: function(otp) {
                var otpTB = document.querySelector('input[name="TranRequestManagerFG.ONE_TIME_PASSWORD__"]');
                var grid1 = document.getElementById('TranRequestManagerFG.GRID_CARD_AUTH_VALUE_1__');
                var grid2 = document.getElementById('TranRequestManagerFG.GRID_CARD_AUTH_VALUE_2__');
                var grid3 = document.getElementById('TranRequestManagerFG.GRID_CARD_AUTH_VALUE_3__');
                var submitButton = document.querySelector('input[name="Action.SUBMIT_TRANSACTION"][type=submit]');
                if(otpTB && !(grid1 && grid2 && grid3)){
                    otpTB.value=otp;
                    submitButton.click();
                }
                else if (otpTB && (grid1 && grid2 && grid3)){
                    var otpTB = document.querySelector('input[name="TranRequestManagerFG.ONE_TIME_PASSWORD__"]');
                    otpTB.value=otp;
                    focusElement(grid1);
                    Gatekeeper.hideAssistanceFragment();
                    Gatekeeper.removeFragment("otp submitted from fragment once");
                }
            }
        },
        { // ICICINB: login page
             path: /corp\/(BANKAWAY)|(AuthenticationController)/,
             hostname: "shopping.icicibank.com",
             state: "UNUSED",
             local: false,
             bank: "ICICINB",
             domCheck: function() {
                return (document.querySelector('input[name="AuthenticationFG.USER_PRINCIPAL"][type=text]') &&
                           document.querySelector('input[name="AuthenticationFG.ACCESS_CODE"][type=password]') &&
                           document.querySelector('input[name="Action.VALIDATE_CREDENTIALS"][type=submit]') )
                           || (document.querySelector('input[type="Submit"][id=initiateProceedButton]'))
             },
             action: function(){
                Gatekeeper.showWaitingFragment();
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");

                var _this= this;

                var mpinCallback = function(response) {
                    try {
                        response = JSON.parse(response);
                        if(response.continueWithMPIN) {
                            _this.startMpin();
                        } else {
                            Gatekeeper.hideWaitingFragment();
                            setupLoginPageWithSubmitButton('input[name="AuthenticationFG.ACCESS_CODE"][type=password]',
                                                            'input[name="AuthenticationFG.USER_PRINCIPAL"][type=text]',
                                                            'input[name="Action.VALIDATE_CREDENTIALS"][type=Submit]');
                            var mobileLogin = document.querySelector('input[name="AuthenticationFG.LOGIN_MOBILE"][type="tel"]');
                             if(mobileLogin){
                                mobileLogin.addEventListener("focus", function (){
                                    Gatekeeper.removeFragment("Selected Mobile Login");
                                    window.trackEventv2_0("acs", "info", "ICICINB_MOBILE_LOGIN", "Login_using_mobile_number", "ICICINB", "mobile_login");
                                }, false);
                             }
                        }
                    } catch(error) {
                        window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error in mpin callback: " + error, "MPIN", "exception");
                    }
                }
                try {
                    Gatekeeper.setupMPIN(this.bank, JSON.stringify(this.getNBPayload()), mpinCallback);
                } catch (error) {
                    // Failed to fire setMPIN
                    window.trackEventv2_0("acs", "error", "MPIN_Exception", "Unable to setup Mpin: " + error, "MPIN", "exception");
                    mpinCallback(JSON.stringify({continueWithMPIN : false}));

                    // get failing data
                    var nbData = JSON.stringify(this.getNBPayload())
                    window.trackEventv2_0("acs", "info", "MPIN_payload", "getNBPayload: " + unescape(encodeURIComponent(nbData)), "MPIN", "payload");
                }
             },
             nextAction: function() {
                 var usernameTextBox = document.querySelector('input[name="AuthenticationFG.USER_PRINCIPAL"][type=text]');                //Login page
                 var submitButtonLoginPage = document.querySelector('input[name="Action.VALIDATE_CREDENTIALS"][type=Submit]');         //Submit Button on first button
                 var passwordField = document.querySelector('input[name="AuthenticationFG.ACCESS_CODE"][type=password]');
                 var validateOkButton = document.querySelector('input[value=OK]');
                 //If error box is found, Next button of Fragment should click on OK button of webPage
                 if(validateOkButton){
                     __juspay.clickOnElement(validateOkButton);
                 } else if((document.activeElement == usernameTextBox) && passwordField){
                     passwordField.focus();
                     Gatekeeper.requestPasswordKeyboardShow();
                     Gatekeeper.showPasswordHelperFragment();
                 }
                 else if((document.activeElement == passwordField) && submitButtonLoginPage) {
                     __juspay.clickOnElement(submitButtonLoginPage);
                     Gatekeeper.requestKeyboardHide();
                 }
             },
             clickSubmitButton: function(){
                 var submitBtn = document.querySelector('input[name="Action.VALIDATE_CREDENTIALS"][type=Submit]');
                 if(submitBtn){
                     __juspay.clickOnElement(submitBtn);
                 }
             },
             backButtonPressed: function() {
                  try {
                      var passwordElement = document.querySelector('input[type=password][name="AuthenticationFG.ACCESS_CODE"]');
                      var submitElement = document.querySelector('input[type=submit][name="Action.VALIDATE_CREDENTIALS"]');
                      if(__uber.isUberEnabled("REDIRECT_DEBIT") && __uber.initSessionCounterIncremental("ICICINBR")==1
                      && passwordElement && submitElement){
                          callDebitRedirectUber();
                      } else {
                          GK.showCancelTransactionDialog();
                      }
                  } catch(err) {
                      GK.showCancelTransactionDialog();
                  }
             },
             getNBPayload: function() {
                try {
                    var transactionDetails = document.getElementById("QS").value.split("|");
                    var payload = {};
                    for(var i = 0; i < transactionDetails.length; i++) {
                        var current = transactionDetails[i].split("~");
                        payload[current[0]] = current[1];
                    }

                    payload.bank = {};
                    var inputs = document.getElementsByTagName("input");
                    for(var i = 0; i < inputs.length; i++) {
                        payload.bank[inputs[i]["id"]] = inputs[i]["value"];
                    }

                    var sessionData = JSON.parse(Gatekeeper.getSessionAttribute("sessionData"));

                    return {
                        payload: payload,
                        packageName: sessionData.package_name,
                        appName: sessionData.app_name,
                        osd : []
                    }
                } catch(error) {
                    window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error while making payload: " + error, "MPIN", "exception");
                    return {};
                }
             },
             startMpin: function() {
                try {
                    var data = this.getNBPayload();

                    var callback = function(resp) {
                        try {
                            resp = JSON.parse(resp);
                            if(resp.redirectUrl) {
                                window.location.href = resp.redirectUrl;
                            } else {
                                setupLoginPageWithSubmitButton('input[name="AuthenticationFG.ACCESS_CODE"][type=password]',
                                                                'input[name="AuthenticationFG.USER_PRINCIPAL"][type=text]',
                                                                'input[name="Action.VALIDATE_CREDENTIALS"][type=Submit]');
                            }
                        } catch(error) {
                            window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error in startMPIN callback: " + error, "MPIN", "exception");
                        }
                    }

                    Gatekeeper.showWaitingFragment();
                    Gatekeeper.startMPIN(this.bank, JSON.stringify(data), callback);
                } catch(error) {
                    window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error trying to start MPIN: " + error, "MPIN", "exception");
                }
             }
        },
        //ICICI NB OTP Page(with ATM PIN)
        {
            path: /\/corp\/(Finacle|AuthenticationController)/,
            hostname: "shopping.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name="Action.VALIDATE_MOBILE_OTP_PIN"][type="Submit"]') &&
                    document.querySelector('input[name="AuthenticationFG.LOGIN_OTP"][type="password"]') &&
                    document.querySelector('input[name="AuthenticationFG.LOGIN_ATM_PIN"][type="password"]');
            },
            action: function() {
                    Gatekeeper.removeFragment("OTP Page reached");
                    var otpField = document.querySelector('input[name="AuthenticationFG.LOGIN_OTP"][type="password"]');
                    var atmPin = document.querySelector('input[name="AuthenticationFG.LOGIN_ATM_PIN"][type="password"]');

                    if(typeof Gatekeeper.setSmsBackReadTime == "function") {
                        Gatekeeper.setSmsBackReadTime(2*60*1000);
                    }
                    var cancelButton = document.querySelector("a[title=Proceed][id=Caption27691536]");
                    if(cancelButton){
                        cancelButton.addEventListener("click",function(){
                            window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                        }, false);
                    }
                    otpField.blur();
                    __juspay.showOtpFragmentWrapper(otpField);
                    otpTextFieldQuery('input[name="AuthenticationFG.LOGIN_OTP"][type="password"]');
                    attachPhoneKeyboard(otpField);
                    __juspay.trackPageStatus("INPUT_OTP");
                    __juspay.trackAuthMethod("OTP");
                    __juspay.scrollToElement(otpField);

                    otpField.addEventListener("click",function(){
                        Gatekeeper.hideAssistanceFragment();
                        Gatekeeper.requestNumericKeyboardShow();
                    }, false);

                    atmPin.addEventListener("focus", function (){
                        Gatekeeper.hideAssistanceFragment();
                        Gatekeeper.requestNumericKeyboardShow();
                        window.trackEventv2_0("acs", "info", "ICICINB_ATM_PIN", "focused_atm_pin_field", "ICICINB", "atm_pin");
                    }, false);
            },
            submitOtp: function(otp) {

                Gatekeeper.shoutOut("OTP Populated");
                var otpField = document.querySelector('input[name="AuthenticationFG.LOGIN_OTP"][type="password"]');
                var atmPin = document.querySelector('input[name="AuthenticationFG.LOGIN_ATM_PIN"][type="password"]');

                if(otp==="undefined" || typeof(otp)=="undefined") {
                    otp = "";
                }
                otpField.value = otp;
                otpField.type = 'text';
                Gatekeeper.removeFragment("Focused ATM PIN Field");
                atmPin.focus();

            }
        },
        {//ICICINB: password and user id reset page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return (document.querySelector('input[name="Action.CONTINUE_MODE"][type=submit]'));
            },
            action: function(){
                var proceed = document.querySelector('input[name="Action.CONTINUE_MODE"][type=submit]');
                if(proceed){
                    __juspay.clickOnElement(proceed);
                    window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "proceed_auto_clicked", "ICICINB", "reset_password");
                }
            }
        },
        {// icicinb password reset page 1st page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name="LoginAltFlowFG.USER_PRINCIPAL"][type="text"]') && document.querySelector('input[name="Action.VALIDATE_USERID"][type="submit"]');
            },
            action: function(){
                __juspay.trackUnblocker("DETAILS_PAGE_ONE");
                var userid = document.querySelector('input[name="LoginAltFlowFG.USER_PRINCIPAL"][type="text"]');
                var checkuserid = document.querySelector('input[name="Action.VALIDATE_USERID"][type="submit"]');
                var bankCustomerId = Gatekeeper.getBankCustomerId();
                if(bankCustomerId==="undefined" ||typeof( bankCustomerId)=="undefined") {
                    bankCustomerId = "";
                }
                userid.value= bankCustomerId;
                focusElement(userid);
                Gatekeeper.requestKeyboardShow();
                Gatekeeper.showNetbankingDefaultFragment();
                checkuserid.addEventListener('click',function(){
                    window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "continue_clicked", "ICICINB", "reset_password");
                },false);
            },
            nextAction: function(){
                var checkuserid = document.querySelector('input[name="Action.VALIDATE_USERID"][type="submit"]');
                if(checkuserid){
                    __juspay.clickOnElement(checkuserid);
                    window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "continue_clicked", "ICICINB", "reset_password");
                }
            }
        },
        {// icicinb password reset 2nd page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name="LoginAltFlowFG.USER_PRINCIPAL"][type="text"]') && document.querySelector('input[name="LoginAltFlowFG.MOBILE_NUMBER"][type="text"]');
            },
            action: function(){
                __juspay.trackUnblocker("DETAILS_PAGE_TWO");
                var userid = document.querySelector('input[name="LoginAltFlowFG.USER_PRINCIPAL"][type="text"]');
                var p_mobilenum = document.querySelector('input[name="LoginAltFlowFG.MOBILE_NUMBER"][type="text"]');
                var checkp_mobilenum = document.querySelector('input[name="Action.VALIDATE_MOBILE_EMAIL"][type="submit"]');
                if(p_mobilenum&& checkp_mobilenum){
                    numberTextFieldQuery('input[name="LoginAltFlowFG.MOBILE_NUMBER"]');
                    attachPhoneKeyboard(p_mobilenum);
                    focusElement(p_mobilenum);
                    Gatekeeper.showNetbankingDefaultFragment();
                    checkp_mobilenum.addEventListener('click',function(){
                        window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "continue_clicked", "ICICINB", "reset_password");
                    },false);
                }
            },
            nextAction: function(){
                var checkp_mobilenum = document.querySelector('input[name="Action.VALIDATE_MOBILE_EMAIL"][type="submit"]');
                if(checkp_mobilenum){
                    __juspay.clickOnElement(checkp_mobilenum);
                    window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "continue_clicked", "ICICINB", "reset_password");
                }
            }
        },
        {// icicinb password reset otp page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name="LoginAltFlowFG.OTP"][type="password"]') && document.querySelector('input[name="LoginAltFlowFG.USER_ID"][type="text"]');
            },
            action: function(){
                otpFragmentHelper('input[name="LoginAltFlowFG.OTP"]',"");
            },
            submitOtp: function(otp) {
                var otp_pwd = document.querySelector('input[name="LoginAltFlowFG.OTP"]');
                var submit_otp = document.querySelector('input[name="Action.AUTHENTICATE_USER"][type="submit"]');
                if(otp_pwd && submit_otp) {
                    submitOtpHelper(otp, 'input[name="LoginAltFlowFG.OTP"]', 'input[name="Action.AUTHENTICATE_USER"][type="submit"]');
                    window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "otp_auto_detect", "ICICINB", "reset_password");
                }
            }
        },
        {// icicinb password reset 4th page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name="LoginAltFlowFG.USER_ID"][type="text"]') && document.querySelector('input[name="LoginAltFlowFG.LOGIN_PWD"][type="password"]') && document.querySelector('input[name="LoginAltFlowFG.RETYPE_LOGIN_PWD"][type="password"]');
            },
            action: function(){
                var pwd = document.querySelector('input[name="LoginAltFlowFG.LOGIN_PWD"][type="password"]');
                var re_pwd = document.querySelector('input[name="LoginAltFlowFG.RETYPE_LOGIN_PWD"][type="password"]');
                var submitBtn = document.querySelector('input[name="Action.SET_PASSWORD"][type=Submit]');
                __juspay.trackUnblocker("RESET_PASSWORD");
                attachAlphaNumericPasswordHelper(pwd);
                focusElement(pwd);
                window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "password_focused", "ICICINB", "reset_password");
                submitBtn.addEventListener('click',function(){
                     window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "submit_clicked", "ICICINB", "reset_password");
                },false);
            },

            clickSubmitButton: function(){
                var pwd = document.querySelector('input[name="LoginAltFlowFG.LOGIN_PWD"][type="password"]');
                var re_pwd = document.querySelector('input[name="LoginAltFlowFG.RETYPE_LOGIN_PWD"][type="password"]');
                var submitBtn = document.querySelector('input[name="Action.SET_PASSWORD"][type=Submit]');
                if((document.activeElement == pwd) && submitBtn)
                {
                    __juspay.scrollToElement(re_pwd);
                    attachAlphaNumericPasswordHelper(re_pwd);
                    re_pwd.focus();
                    Gatekeeper.requestPasswordKeyboardShow();
                }
                else if(submitBtn && (document.activeElement == re_pwd)){
                     __juspay.clickOnElement(submitBtn);
                     window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "submit_clicked", "ICICINB", "reset_password");
                }
            },
            showPassword: function() {
                var pwd = document.querySelector('input[name="LoginAltFlowFG.LOGIN_PWD"][type="password"]');
                var re_pwd = document.querySelector('input[name="LoginAltFlowFG.RETYPE_LOGIN_PWD"][type="password"]');
                var pwdValue = document.querySelector('input[name="LoginAltFlowFG.LOGIN_PWD"][type="password"]').value;
                var re_pwdValue = document.querySelector('input[name="LoginAltFlowFG.RETYPE_LOGIN_PWD"][type="password"]').value;

                if(pwdValue != undefined && pwdValue != null && document.activeElement == pwd) {
                    Gatekeeper.setPasswordValue(pwdValue);
                }
            },
            showPassword: function() {
                var pwd = document.querySelector('input[name="LoginAltFlowFG.LOGIN_PWD"][type="password"]');
                var re_pwd = document.querySelector('input[name="LoginAltFlowFG.RETYPE_LOGIN_PWD"][type="password"]');
                var pwdValue = document.querySelector('input[name="LoginAltFlowFG.LOGIN_PWD"][type="password"]').value;
                var re_pwdValue = document.querySelector('input[name="LoginAltFlowFG.RETYPE_LOGIN_PWD"][type="password"]').value;

                if(re_pwdValue != undefined && re_pwdValue != null && document.activeElement == re_pwd){
                    Gatekeeper.setPasswordValue(re_pwdValue);
                }
            }
        },
        {// icicinb password reset success page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                var domEl = document.getElementById("Caption24377068");
                if(domEl){
                return (domEl.innerHTML =="Generate Password Online") && document.getElementById("Caption24377039");
                }
            },
            action: function(){
                var loginbtn= document.getElementById("Caption24377039");
                Gatekeeper.removeFragment("success_page");
                __juspay.trackUnblocker("RESET_PASSWORD_SUCCESSFUL");
                loginbtn.addEventListener('click',function(){
                    window.trackEventv2_0("acs", "info", "ICICINB_RESET_PASSWORD", "LOGIN_CLICKED", "ICICINB", "reset_password");
                },false);
            }
        },
        {// icicinb userid reset 1st page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name="CustomLoginGetUserIDFG.ACCOUNT_ID"][type="text"]') && document.querySelector('input[name="Action.VALIDATE_ACCOUNT"][type="submit"]');
            },
            action: function(){
                var accountNo = document.querySelector('input[name="CustomLoginGetUserIDFG.ACCOUNT_ID"][type="text"]');
                var checkaccountNo = document.querySelector('input[name="Action.VALIDATE_ACCOUNT"][type="submit"]');
                __juspay.trackUnblocker("DETAILS_PAGE_ONE");
                numberTextFieldQuery('input[name="CustomLoginGetUserIDFG.ACCOUNT_ID"]');
                attachPhoneKeyboard(accountNo);
                focusElement(accountNo);
                Gatekeeper.showNetbankingDefaultFragment();
                checkaccountNo.addEventListener('click',function(){
                    window.trackEventv2_0("acs", "info", "ICICINB_RECOVER_USERID", "continue_clicked ", "ICICINB", "recover_userid");
                },false);
            },
            nextAction: function(){
                var checkaccountNo = document.querySelector('input[name="Action.VALIDATE_ACCOUNT"][type="submit"]');
                window.trackEventv2_0("acs", "info", "ICICINB_RECOVER_USERID", "continue_clicked ", "ICICINB", "recover_userid");
                __juspay.clickOnElement(checkaccountNo);
            }
        },
        {// icicinb userid reset 2nd page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name="CustomLoginGetUserIDFG.ACCOUNT_ID_NEW"][type="text"]') && document.querySelector('input[name="CustomLoginGetUserIDFG.MOBILE_NUMBER"][type="text"]');
            },
            action: function(){
                var u_mobilenum = document.querySelector('input[name="CustomLoginGetUserIDFG.MOBILE_NUMBER"][type="text"]');
                var checku_mobilenum = document.querySelector('input[name="Action.VALIDATE_ACC_MOBILE_EMAIL"][type="submit"]');
                numberTextFieldQuery('input[name="CustomLoginGetUserIDFG.MOBILE_NUMBER"]');
                attachPhoneKeyboard(u_mobilenum);
                __juspay.trackUnblocker("DETAILS_PAGE_TWO");
                focusElement(u_mobilenum);
                Gatekeeper.showNetbankingDefaultFragment();
                checku_mobilenum.addEventListener('click',function(){
                    window.trackEventv2_0("acs", "info", "ICICINB_RECOVER_USERID", "continue_clicked", "ICICINB", "recover_userid");
                },false);
            },
            nextAction: function(){
                var checku_mobilenum = document.querySelector('input[name="Action.VALIDATE_ACC_MOBILE_EMAIL"][type="submit"]');
                if(checku_mobilenum){
                    __juspay.clickOnElement(checku_mobilenum);
                    window.trackEventv2_0("acs", "info", "ICICINB_RECOVER_USERID", "continue_clicked", "ICICINB", "recover_userid");
                }
            }
        },
        {// icicinb user id reset success page
            path:/\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                var domEl = document.getElementById("Caption24377068");
                if(domEl){
                    return (domEl.innerHTML =="Know your User ID") && document.getElementById("Caption24377039");
                }
            },
            action: function(){
                var loginbtn=document.getElementById("Caption24377039");
                var pwd_reset = document.querySelector('p.pad_top > span.OrangeButton > span > a');
                Gatekeeper.removeFragment("success_page");
                __juspay.trackUnblocker("RECOVER_USERID_SUCCESSFUL");
                loginbtn.addEventListener('click',function(){
                    window.trackEventv2_0("acs", "info", "ICICINB_RECOVER_USERID", "login_clicked", "ICICINB", "recover_userid");
                },false);
                if(pwd_reset){
                    pwd_reset.addEventListener('click',function(){
                        window.trackEventv2_0("acs", "info", "ICICINB_RECOVER_USERID", "pwd_reset_clicked", "ICICINB", "recover_userid");
                    },false);
                }
            }
        },
        { //ICICI_NB: login page
            path: /\/BANKAWAY/,
            hostname: /(m|infinity)\.icicibank\.co\.in/,
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[name=CorporateSignonCorpId]') &&
                       document.querySelector('input[name=CorporateSignonPassword][type=password]') &&
                       document.querySelector('input[id=GetMigrationStatus]');
            },
            action: function() {
                Gatekeeper.setSessionAttribute("disableAutoFocus", "true"); showUpdateWebviewUber();

                setupLoginPageWithSubmitButton("input[type=password][name=CorporateSignonPassword]",
                                "input[name=CorporateSignonCorpId]",
                                "input[id=GetMigrationStatus]");
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector("input[type=button][id=GetMigrationStatus]");
                __juspay.clickOnElement(submitBtn);
             },
            showPassword: function() {
                var  passwordValue = document.querySelector("input[type=password][name=CorporateSignonPassword]").value
                if(passwordValue != undefined && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue)
                }
            },
            nextAction: function() {
                var usernameTextBox=document.querySelector('input[type=text][name=CorporateSignonCorpId]'); //Login page
                var submitButtonLoginPage = document.querySelector('input[id=GetMigrationStatus]');         //Submit Button on first button
                var passwordField = document.querySelector('input[name=CorporateSignonPassword][type=password]');

                if(document.activeElement == usernameTextBox){
                    passwordField.focus();
                    Gatekeeper.requestPasswordKeyboardShow();
                    Gatekeeper.showPasswordHelperFragment();
                } else {
                    __juspay.clickOnElement(submitButtonLoginPage);
                    submitButtonLoginPage.focus();
                    Gatekeeper.showNetbankingDefaultFragment();
                    Gatekeeper.requestKeyboardHide();
                }
            }
        },
        // ICICI NB : remarks page
        {
            path: /\/corp\/AuthenticationController/,
            hostname: "shopping.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[id="TranRequestManagerFG.CP_REMARKS_SINGLE"]') &&
                       document.querySelector('input[name="Action.SUBMIT_TRANSACTION"][id="SUBMIT_TRANSACTION"]');
            },
            action: function() {
                __juspay.trackPageStatus("INPUT_NB_REMARKS");
                var remarksField = document.querySelector('input[id="TranRequestManagerFG.CP_REMARKS_SINGLE"]');
                focusElement(remarksField);
                Gatekeeper.showNetbankingDefaultFragment();
                Gatekeeper.changeNextActionText("Pay");
            },
            nextAction: function() {
                var submitBtn = document.querySelector('input[name="Action.SUBMIT_TRANSACTION"][id="SUBMIT_TRANSACTION"]');
                __juspay.clickOnElement(submitBtn);
                Gatekeeper.requestKeyboardHide();
            }
        },
        { //ICICI_NB: remarks page
            path: /\/corp\/AuthenticationController/,
            hostname: "infinity.icicibank.com",
            state: "UNUSED",
            local: false,
            bank: "ICICINB",
            domCheck: function() {
                return document.querySelector('input[type=submit][id=CONTINUE_TRANSACTION]') && document.getElementById('TranRequestManagerFG.ENT_REMARKS');
            },
            action: function() {
                var remarksField = document.body.querySelector("#TranRequestManagerFG\\.ENT_REMARKS");
                var remarksLabel = document.body.querySelector("#TranRequestManagerFG\\.ENT_REMARKS_Label");
                var remarks = Gatekeeper.getRemarksForBank();
                if(remarks != null){
                    remarksField.value = remarks;
                }
                focusElement(remarksField);
                Gatekeeper.showNetbankingDefaultFragment();
                Gatekeeper.changeNextActionText("Pay");
                __juspay.trackPageStatus("INPUT_NB_REMARKS");
            },
            nextAction: function() {
                var submitBtn = document.body.querySelector("#CONTINUE_TRANSACTION");
                __juspay.clickOnElement(submitBtn);
                Gatekeeper.requestKeyboardHide();
            }
        },
        //HDFC_NB
        { //HDFC_NB: enter customer_id page
            path: /\/netbanking\/(merchant|nach)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",

            local: false,
            bank: "HDFCNB",
            domCheck: function(){
                return true;
            },
            showUberDialog:function(props,data) {
                var innerWindow = frames["bottom_frame"];
                var doc = innerWindow && innerWindow.document;
                props.type = "dialog";
                props.height = props.height || 320;
                props.width = props.width || -2;
                if(props.showOnLoad) {
                    setTimeout(function(){Gatekeeper.showUber(false);},500);
                    props.showOnLoad = false;
                }
                var ipinField = doc && doc.querySelector('input[name=fldPassword]');
                if(ipinField){
                    GK.setUberEventWithCallback("HDFCNB_RESET_IPIN", function(result) {
                        window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","uber_shown", "HDFCNB", "reset_ipin");
                        if( result == "ok" ) {
                            window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","reset_ipin_clicked", "HDFCNB", "reset_ipin");
                            Gatekeeper.setSessionAttribute("reset_custid_autoclick","false");
                            window.location.href = "https://"+ window.location.hostname + "/netbanking/IpinThrDC.htm";
                        }
                        else if(result=="cancel") {
                            window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","recover_custid_clicked", "HDFCNB", "reset_ipin");
                            Gatekeeper.setSessionAttribute("reset_custid_autoclick","true");
                            window.location.href = "https://"+ window.location.hostname + "/netbanking/";
                        }
                        else if(result == "closed") {
                            window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","close_clicked", "HDFCNB", "reset_ipin");
                        }
                    },props,data);
                }
                else {
                    GK.setUberEventWithCallback("HDFCNB_RECOVER_CUSTID", function(result) {
                        window.trackEventv2_0("acs","info","HDFCNB_RECOVER_CUSTID","uber_shown", "HDFCNB", "recover_custid");
                        if( result == "ok" ) {
                            window.trackEventv2_0("acs","info","HDFCNB_RECOVER_CUSTID","recover_custid_clicked", "HDFCNB", "recover_custid");
                            Gatekeeper.setSessionAttribute("reset_custid_autoclick","true");
                            window.location.href = "https://"+ window.location.hostname + "/netbanking/";
                        }
                        else if(result=="cancel") {
                            window.trackEventv2_0("acs","info","HDFCNB_RECOVER_CUSTID","reset_ipin_clicked", "HDFCNB", "recover_custid");
                            Gatekeeper.setSessionAttribute("reset_custid_autoclick","false");
                            window.location.href = "https://"+ window.location.hostname + "/netbanking/IpinThrDC.htm";
                        }
                        else if(result == "closed") {
                            window.trackEventv2_0("acs","info","HDFCNB_RECOVER_CUSTID","close_clicked", "HDFCNB", "recover_custid");
                        }
                    },props,data);
                }
            },
            action: function() {
                showUpdateWebviewUber(); // uncomment focusElement method calls when removing this call
                var innerWindow = frames["bottom_frame"]
                var doc = innerWindow && innerWindow.document;
                var loginBtn1 = doc && doc.querySelector('img[src="/gif/login.gif"]');
                var customerIdField = doc && doc.querySelector("input[name=fldLoginUserId]");
                var continueBtn = doc && doc.querySelector('img[src="/gif/continue.gif"]'); // coming only for new webview which dont have secure image check option
                var ipinField = doc && doc.querySelector('input[name=fldPassword]');
                var forgotUserId = doc && doc.querySelector('a[onclick="return fretrieveCustIdEpi();"]');

                if(ipinField && customerIdField && !loginBtn1 && continueBtn){
                //anie
                    Gatekeeper.removeFragment("Merchant who has both username and password field")
                    if(ipinField!=""){
                        attachAlphaNumericPasswordHelper(ipinField);
                    }
                    if(customerIdField && continueBtn){
                        removeAutoSuggestionBar(customerIdField);
                        customerIdField.addEventListener("focus", function (){
                            Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                            window.trackEventv2_0default("acs", "info", "focused", "<"+customerIdField.nodeName+" id="+customerIdField.id+" name="+customerIdField.name+" class="+customerIdField.className+" >");
                        }, false);

                        var bankCustomerId = Gatekeeper.getBankCustomerId();
                        if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined") {
                            bankCustomerId = "";
                        }
                        customerIdField.value = bankCustomerId;
                        var textRegex = /TEXT/i;
                        var numRegex = /(NUMBER|NUMERIC)/i;
                        var telRegex = /TEL/i;
                        var disableAutofocus = Gatekeeper.getSessionAttribute("disableAutoFocus");
                        if(disableAutofocus == "true"){
                            Gatekeeper.setSessionAttribute("disableAutoFocus", "false");
                        }
                        if(customerIdField.value == ""){
                            Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                            if(numRegex.test(customerIdField.getAttribute("type")) || telRegex.test(customerIdField.getAttribute("type"))){
                                numberTextFieldQuery(usernameSelector);
                                attachPhoneKeyboard(customerIdField);
                                __juspay.delayMe(function(){
                                    Gatekeeper.requestNumericKeyboardShow();
                                    __juspay.scrollToElement(customerIdField);
                                }, 100);
                            }else if(textRegex.test(customerIdField.getAttribute("type"))){
                                removeAutoSuggestionBar(customerIdField);
                                __juspay.delayMe(function(){
                                    if(disableAutofocus != "true"){
                                        Gatekeeper.requestKeyboardShow();
                                    }
                                    __juspay.scrollToElement(customerIdField);
                                }, 100);
                            }else{
                                Gatekeeper.shoutOut("REGEX not matched in set up login page with submit");
                                __juspay.scrollToElement(customerIdField);
                            }
                            if(disableAutofocus == "true"){
                                customerIdField.blur();
                            } else {
                                focusElement(customerIdField);
                            }
                        } else if(ipinField) {
                            ipinField.blur();
                            window.trackEventv2_0default("acs","info","cust_id_populated","Cust Id was populated");
                            if(disableAutofocus != "true"){
                                focusElement(ipinField);
                            }
                        }else{
                            customerIdField.blur();
                            focusElement(customerIdField);
                        }
                        continueBtn.addEventListener("click", function() {
                            var customerIdField = doc && doc.querySelector("input[name=fldLoginUserId]");
                            if(customerIdField.value){
                                Gatekeeper.onCustomerIdSubmit(customerIdField.value);
                                Gatekeeper.requestKeyboardHide();
                            }
                        });
                    }
                }
                if(customerIdField && !ipinField && __juspay.isMobUiEnabled("MOBUI_HDFCNB")) {
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        if(!doc){ doc=parent.document.querySelector('frame[name="bottom_frame"]').contentDocument;}
                        var first_check = setInterval( function() {
                            doc.querySelector('form').width=getScreenWidth();
                            var table = doc.querySelectorAll('table')[1];
                            var row = table.childNodes[0].childNodes[0];
                            var innerrow = row.childNodes[0].childNodes[0].childNodes[0].childNodes[0];
                            var forgotlink =innerrow.childNodes[0].querySelector('table').childNodes[0].childNodes[3];
                            var text =innerrow.childNodes[0].querySelector('table').childNodes[0].childNodes[4];
                            var continuetable =doc.querySelectorAll('table[class="label"]')[1];
                            var footerDoc = parent.document.querySelector('frame[name="top_frame"]').contentDocument;
                            var footer = footerDoc.querySelectorAll('p[class="footer"] > a');
                            var footerDiv = footerDoc.querySelector('form[name="frmFooter"]');

                            var elements_list = [row.childNodes[2],innerrow.childNodes[1],innerrow.parentElement.childNodes[9],
                                                  doc.querySelectorAll('img')[1],doc.querySelectorAll('a')[3],continuetable.querySelectorAll('tbody tr td')[0],continuetable.querySelectorAll('tbody tr td')[1],
                                                  footer[1], footer[0]];
                            if(table && continuetable && row && innerrow && row.childNodes[2],innerrow.childNodes[1] && forgotlink && text &&
                                doc.querySelectorAll('table')[2] && doc.querySelector('td[bgcolor="#EDF0F4"]') && continueBtn.parentElement.parentElement && innerrow.parentElement.parentElement &&
                                innerrow.parentElement.childNodes[9] && doc.querySelectorAll('img')[1] && doc.querySelectorAll('a')[3]) {
                                clearInterval(first_check);
                                __juspay.removeElements(elements_list);
                                footerDiv.style.width = getScreenWidth().toString();
                                doc.querySelectorAll('a')[2].removeAttribute("href");
                                doc.querySelectorAll('table')[0].setAttribute("width",(getScreenWidth()).toString());
                                table.setAttribute("width",(getScreenWidth()).toString());
                                footerDiv.style.width = getScreenWidth().toString();
                                __juspay.modifyUI({"align_center":[innerrow.parentElement.parentElement,innerrow.parentElement,innerrow,doc.querySelectorAll('a')[2],continueBtn.parentElement.parentElement],"add_break_before":[forgotlink,text,continuetable]});
                                doc.querySelectorAll('table')[2].setAttribute("width",(getScreenWidth()).toString());
                                doc.querySelector('td[bgcolor="#EDF0F4"]').style.paddingLeft=getScreenWidth()*0.25;
                                doc.querySelector('td[bgcolor="#EDF0F4"]').style.paddingRight=getScreenWidth()*0.25;
                                window.trackEventv2_0default("acs","info","MOBUI","CUST_ID_PAGE");
                            }
                        },100);
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"CUST_ID_PAGE");
                    }
                }
                //TODO catch user error using keepCheckingDomFor
                if(customerIdField) {
                    if(!ipinField) {
                        numberTextFieldQuery("input[name=fldLoginUserId]");
//                        Gatekeeper.requestKeyboardShow(); // uncomment when removing showUpdateWebviewUber
                        removeAutoSuggestionBar(customerIdField);
                        //
//                        focusElement(customerIdField); // uncomment when removing showUpdateWebviewUber

                        __juspay.trackPageStatus("INPUT_LOGIN_USERNAME");
                        __juspay.trackAuthMethod("LOGIN");
                        var bankCustomerId = Gatekeeper.getBankCustomerId();
                        if(bankCustomerId==="undefined" ||typeof( bankCustomerId)=="undefined") {
                            bankCustomerId = "";
                        }
                        customerIdField.value = bankCustomerId;
                        Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                        if(forgotUserId){
                            forgotUserId.addEventListener("click", function() {
                                Gatekeeper.removeFragment("User selected forgot user id");
                            },false);
                        }
                        if(continueBtn) {
                            continueBtn.addEventListener("click", function() {
                                if(customerIdField.value) {
                                    Gatekeeper.onCustomerIdSubmit(customerIdField.value);
                                }
                            })
                        }

                        var nextPageCheckLooper = setInterval(function() {
                            var doc = frames["bottom_frame"] && frames["bottom_frame"].document;

                            var ipinField = doc && doc.querySelector('input[name=fldPassword]');
                            var checkbox = doc && doc.querySelector("#chkrsastu");
                            var loginBtn = doc && doc.querySelector('img[src="/gif/login.gif"]');
                            if(ipinField){
                                ipinField.blur();
                                focusElement(ipinField);
                                Gatekeeper.requestPasswordKeyboardShow();
                                __juspay.trackPageStatus("INPUT_LOGIN_PASSWORD");
                                __juspay.trackAuthMethod("PASSWORD");
                                attachAlphaNumericPasswordHelper(ipinField);
                                Gatekeeper.showPasswordHelperFragment();
                                Gatekeeper.changeNextActionText("Continue");
                                __juspay.scrollToElement(ipinField);
                                clearInterval(nextPageCheckLooper);

                                if (__juspay.isMobUiEnabled("MOBUI_HDFCNB")) {
                                    try {
                                        var metaTag=document.createElement('meta');
                                        metaTag.name = "viewport"
                                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                                        if(!doc){
                                            doc=parent.document.querySelector('frame[name="bottom_frame"]').contentDocument;
                                        }
                                        doc.querySelector('form').width=getScreenWidth();
                                        var check = setInterval(function() {
                                            if(!doc) { doc=parent.document.querySelector('frame[name="bottom_frame"]').contentDocument;}
                                            var loginBtn = doc && doc.querySelector('img[src="/gif/login.gif"]');
                                            var checkbox = doc && doc.querySelector("#chkrsastu");
                                            var table = doc.querySelectorAll('table')[1];
                                            var table0=doc.querySelectorAll('table')[0];
                                            var row =table.childNodes[1].childNodes[0];
                                            var row0=table0.childNodes[1].childNodes[0];
                                            var passtable=doc.querySelectorAll('table[class="label"]')[1];
                                            var del=doc.querySelectorAll('table[class="label"]')[2];
                                            var table3 = doc.querySelectorAll('table[class="label"]')[3];
                                            var del2 = doc.querySelector('td[background= "/gif/log-right.gif"]');
                                            var table2 = doc.querySelector('table[bordercolor= "#6666CC"]');
                                            var pad = doc.querySelector('td[bgcolor="#EDF0F4"]');
                                            var p = doc.querySelector('p[class="fPinPwd"]');

                                            if(table2) { var del3 = table2.childNodes[1].childNodes[2]; }
                                            else { var del3 = null; }
                                            if(row.childNodes[2] && row0.childNodes[3] &&
                                                passtable.childNodes[1].childNodes[0].childNodes[2] && pad &&
                                                    del && table3.childNodes[1].childNodes[0]  && del2 && del3 && loginBtn && p) {
                                                clearInterval(check);
                                                passtable.childNodes[1].childNodes[0].childNodes[2].innerText="Password: ";
                                                doc.querySelectorAll('table')[1].style.width=getScreenWidth();
                                                doc.querySelectorAll('table')[0].style.width=getScreenWidth();
                                                __juspay.modifyUI({"align_left":[row.childNodes[1],row0.childNodes[2]],"add_break_before":[p]});
                                                __juspay.removeElements([row.childNodes[2],row0.childNodes[3],del,del2,del3,table3.childNodes[1].childNodes[0]])
                                                table2.width=getScreenWidth();
                                                pad.style.paddingRight = getScreenWidth()*0.25;
                                                pad.style.paddingLeft = getScreenWidth()*0.25;
                                                doc.querySelectorAll('a img')[1].parentElement.removeAttribute("href");
                                                if(doc.querySelectorAll('img').length==3){
                                                    doc.querySelector('p[class="fPinPwd"]').parentElement.setAttribute("nowrap",true)
                                                }
                                                window.trackEventv2_0default("acs","info","MOBUI","PASSWORD_PAGE");
                                                if(checkbox) {
                                                    checkbox.checked = true;
                                                }
                                                loginBtn.addEventListener("click",function(){
                                                    if(!checkbox.checked){
                                                        checkbox.checked = true;
                                                    }
                                                },false);
                                            }
                                        },100);
                                    } catch(err) {
                                    __juspay.CatchPageModifyError("MOBUI",err,"PASSWORD_PAGE");
                                    }
                                }
                            }
                            else {
                                var wrongCIDMessage = /You are not authorized to do this transaction/
                                var fbody = frames["bottom_frame"].document.querySelector("body");
                                if(wrongCIDMessage.test(fbody.innerText)) {
                                    Gatekeeper.removeFragment("hdfcnb_wrong_cid_entered")
                                    Gatekeeper.changeNextActionText("Retry");
                                    clearInterval(nextPageCheckLooper);
                                }
                            }
                            if(checkbox) {
                                __juspay.clickOnElement(checkbox);
                                checkbox.checked = true;
                            }
                        }, 300);
                        window.addEventListener("keyup",function(e) {
                            if(e.keyCode == "13") {
                                 Gatekeeper.onCustomerIdSubmit(customerIdField.value);
                                 //TODO: Remove this and add click on submit button.
                                 if(typeof fLogon == "function"){
                                    fLogon();
                                 }
                            }
                        },false);
                    }
                }
            },
            backButtonPressed: function() {
                try {
                    var doc = frames["bottom_frame"] && frames["bottom_frame"].document;
                    var passwordElement = null;
                    var submitElement = null;
                    if(doc){
                        passwordElement = doc.querySelector("input[name='fldPassword']");
                        submitElement = doc.querySelector("img[src='/gif/login.gif']");
                    }
                    var fbody = frames["bottom_frame"].document.querySelector("body");
                    var wrongCIDMessage = /You are not authorized to do this transaction/
                    if(fbody && wrongCIDMessage.test(fbody.innerText)) {
                        if(__uber.initSessionCounterIncremental("HDFCWU")==1) {
                            this.showUberDialog({showOnLoad:true,onPageLoad:"destroy"},null);
                        } else {
                            GK.showCancelTransactionDialog();
                        }
                    } else if(( passwordElement == null || !passwordElement) && __uber.initSessionCounterIncremental("HDFCDCR")==1 && __uber.isUberEnabled("REDIRECT_DEBIT")){
                        callDebitRedirectUber();
                    } else if(passwordElement && __uber.initSessionCounterIncremental("HDFCDPE")==1 && __uber.isUberEnabled("REDIRECT_DEBIT")){
                        callDebitRedirectUber();
                    } else {
                        GK.showCancelTransactionDialog();
                    }

                } catch(err) {
                    GK.showCancelTransactionDialog();
                }
            },
            clickSubmitButton: function() {
                //TODO: Remove this and add click on submit button.
                var doc = frames["bottom_frame"] && frames["bottom_frame"].document;
                var passwordField = doc && doc.querySelector('input[name=fldPassword]');
                var continueBtn = doc && doc.querySelector('img[src="/gif/continue.gif"]'); // coming only for new webview which dont have secure image check option
                var checkbox = doc && doc.querySelector("#chkrsastu");
                var loginBtn = doc && doc.querySelector('img[src="/gif/login.gif"]') || doc && doc.querySelector('a[class*="login-btn"][onclick*="return fLogon"]');

                if(loginBtn && passwordField){
                     if(checkbox && !(checkbox.checked)) {
                         checkbox.checked = true;
                     }
                     __juspay.clickOnElement(loginBtn);
                     Gatekeeper.changeNextActionText("Login");
                     Gatekeeper.removeFragment("submit_button_clicked");
                 }
                if(continueBtn){
                    __juspay.clickOnElement(continueBtn);
                }
            },
            showPassword: function() {
                var doc = frames["bottom_frame"] && frames["bottom_frame"].document;
                var ipinField = doc && doc.querySelector('input[name=fldPassword]');
                var passwordValue = ipinField.value;
                if(passwordValue != undefined && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue)
                }
            },
            nextAction: function() {
                var doc = frames["bottom_frame"] && frames["bottom_frame"].document;
                var continueBtn = doc && doc.querySelector(".login_tab a img");
                var loginBtn = doc && doc.querySelector('img[src="/gif/login.gif"]');
                var ipinField = doc && doc.querySelector('input[name=fldPassword]');
                if(continueBtn && ipinField && !loginBtn){
                    ipinField.focus();
                }else if(continueBtn){
                    __juspay.clickOnElement(continueBtn);
                } else if (loginBtn){
                     __juspay.clickOnElement(loginBtn);
                     Gatekeeper.requestKeyboardHide();
                }
            }
        },
        //HDFC NB - Error page
        {
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function(){
                var invalid = document.querySelector('strong');
                if(invalid){
                    return (invalid.innerText.indexOf("You are not registered for NetBanking.")>-1)
                }
            },
            action: function(){
               Gatekeeper.removeFragment("User is not registered for Netbanking");
            }
        },
        //HDFC choose account page
        {
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function(){
                return document.querySelector('img[alt=Continue][src*=confirm]') &&
                       document.querySelector('a[onclick*=cancel]')
            },
            action: function(){
                var confirmBtn = document.querySelector('img[alt=Continue][src*=confirm]');
                if(confirmBtn){
                    __juspay.trackPageStatus("CHOOSE_ACCOUNT");
                    if(typeof count != "undefined" && count && count == 1 && __juspay.isMobUiEnabled("AUTO_CLICK_HDFCNB")){  //count is the variable from Hdfc js that has the number of accounts list
                        __juspay.trackPageStatus("SINGLE_ACCOUNT_SELECTED");
                        window.trackEventv2_0default("acs","info","AUTO_CLICK","SINGLE_ACCOUNT_SELECTED");
                        __juspay.clickOnElement(confirmBtn);
                    }
                }
            },
            nextAction: function(){
                var confirmBtn = document.querySelector('img[alt=Continue][src*=confirm]');
                __juspay.clickOnElement(confirmBtn);
            }
        },
        //HDFCNB: Security questions
        {
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function(){
                return document.querySelector("input[name=fldAnswer]") &&
                    document.querySelector('img[alt=submit][src*=submit]');
            },
            setStyle: function(elements){
                var i=0;
                while(i<elements.length) {
                    elements[i].style.display= "inline-block";
                }
            },
            action: function(){
                var continueBtn = document.querySelector('img[alt=submit][src*=submit]');
                var securityQuestion = document.querySelector("input[name=fldAnswer]");
                var secQues1 = document.querySelectorAll(".input_text[name='fldAnswer']")[0];
                var secQues2 = document.querySelectorAll(".input_text[name='fldAnswer']")[1];
                if(continueBtn && securityQuestion){
                    secQues2.focus();
                    secQues2.blur();
                    secQues1.focus();
                    Gatekeeper.showNetbankingDefaultFragment();
                    Gatekeeper.changeNextActionText("Continue");
                    __juspay.trackPageStatus("INPUT_SECURITY_QUESTION");
                    __juspay.trackAuthMethod("SECURITY_QUESTION");

                }
                if(__juspay.isMobUiEnabled("MOBUI_HDFCNB")){
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        var table2 =document.querySelector('table[class="content_area"]');
                        table2.childNodes[1].removeChild(table2.childNodes[1].childNodes[0]);
                        table2.childNodes[1].removeChild(table2.childNodes[1].childNodes[1]);
                        var img = document.querySelectorAll('img')[2];
                        var  x= img;
                        var i=0;
                        while(i<7){
                            x.setAttribute("align","left");
                            x=x.parentElement;
                            i++;
                        }
                        document.querySelectorAll('td')[10].parentElement.removeChild(document.querySelectorAll('td')[10]);
                        document.querySelectorAll('td')[12].parentElement.removeChild(document.querySelectorAll('td')[12]);
                        var i=9;
                        while(i<13){
                            document.querySelectorAll('td')[i++].style.width=getScreenWidth()*0.9;
                            document.querySelectorAll('td')[i++].style.display="inline-block"
                        }
                        document.querySelectorAll('td')[9].parentElement.style.display= "inline-block";
                        document.querySelectorAll('td')[9].parentElement.parentElement.style.display= "inline-block";
                        document.querySelectorAll('td')[9].parentElement.parentElement.parentElement.style.display= "inline-block";
                        document.querySelectorAll('td')[11].parentElement.style.display= "inline-block";
                        document.querySelectorAll('td')[9].style.width="546px";
                        document.querySelectorAll('td')[11].style.width="546px";
                        var empt = document.querySelectorAll('td')[11].parentElement.parentElement.insertBefore(document.createElement('td'),document.querySelectorAll('td')[11].parentElement);
                        empt.style.height=(window.innerHeight * 1/32*1.14).toString();
                        window.trackEventv2_0default("acs","info","MOBUI","SECURITY_QUESTIONS_PAGE");
                    } catch(err){
                        __juspay.CatchPageModifyError("MOBUI",err,"SECURITY_QUESTIONS_PAGE");
                    }
                }
            },
            nextAction: function(){
                var continueBtn = document.querySelector('img[alt=submit][src*=submit]');
                var secQues1 = document.querySelectorAll(".input_text[name='fldAnswer']")[0];
                var secQues2 = document.querySelectorAll(".input_text[name='fldAnswer']")[1];
                var secQues1Val= secQues1.value;
                var secQues2Val = secQues2.value;

                if(secQues1 && secQues2 && continueBtn){
                    if(secQues1Val.length != 0 && secQues2Val.length == 0){
                        secQues2.focus();
                    }
                    else if(secQues1Val != 0 && secQues2Val != 0){
                        __juspay.clickOnElement(continueBtn);
                    }
                }
            }
        },
        {
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function(){
                return document.querySelector('input[name=fldOtpToken]')&&
                       document.querySelector('img[alt=Submit]');
            },
            action: function(){
                var otpField = document.querySelector('input[name=fldOtpToken]');
                var submitOtpBtn = document.querySelector('img[alt=Submit]');
                if(otpField && submitOtpBtn){
                    otpFragmentHelper('input[name=fldOtpToken]','');
                    if(__juspay.isMobUiEnabled("MOBUI_HDFCNB")) {
                        try{
                            var metaTag=document.createElement('meta');
                            metaTag.name = "viewport"
                            metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                            document.getElementsByTagName('head')[0].appendChild(metaTag);
                            var table =document.querySelector('table[class="content"]');
                            table.width=getScreenWidth();
                            table.childNodes[1].childNodes[0].childNodes[1].childNodes[2].width=getScreenWidth();
                            var table2 =document.querySelector('table[class="content_area"]');
                            table2.style.margin="0 auto";
                            document.querySelector('td[class="spacer"]').padding="0px";
                            document.querySelector('table[class="content"]').width= getScreenWidth();
                            var otp = document.querySelector('input[name="fldOtpToken"]');
                            document.querySelector('td[class="spacer"]').style.padding="0 px";
                            var align_center = [document.querySelector('input[name="fldOtpAuth"]').parentElement,table2.parentElement,otp.parentElement,otp.parentElement.parentElement.childNodes[1]];
                            __juspay.modifyUI({"align_center":align_center,"style_width":{"other":[0.9,table,document.querySelector('table[class="body_content"]'),document.querySelector('table[class="content"]')],"other2":[(1/6),document.querySelector('input[name="fldOtpAuth"]')]},"add_break_before":[table2.parentElement.childNodes[3]]})
                            window.trackEventv2_0default("acs","info","MOBUI","OTP_PAGE");
                        } catch(err) {
                            __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                        }
                    }
                }
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp,"input[name=fldOtpToken]","img[alt=Submit]");
            }
        },

        {  //HDFC_NB: reset custid details entry page
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function(){
                return document.querySelector('input[name="fldMobileNumber"][type="text"]') && document.querySelector('select[name="datefldDateOfBirth"]');
            },
            action:function(){
                __juspay.trackUnblocker("RESET_CUSTID_DETAILS");
                var mobilenumber = document.querySelector('input[name="fldMobileNumber"][type="text"]');
                var date = document.querySelector('span[id="selectdatefldDateOfBirth"]');
                var month = document.querySelector('span[id="selectmonthfldDateOfBirth"]');
                var year =  document.querySelector('span[id="selectyearfldDateOfBirth"]');
                var captchafield = document.querySelector('input[name="fldCaptcha"]');
                var continueBtn=document.querySelectorAll('a img')[1];
                document.querySelectorAll('td[class="DataLeftAligned"]')[5].style.display="none"
                document.querySelectorAll('td[class="DataLeftAligned"]')[4].style.display="none"
                numberTextFieldQuery("input[name=fldMobileNumber]");
                attachPhoneKeyboard(mobilenumber);
                captchafield.addEventListener("click",function(){
                    __juspay.delayMe(function(){
                    Gatekeeper.requestPasswordKeyboardShow();
                    },200);
                    window.trackEventv2_0("acs", "info", "HDFCNB_RECOVER_CUSTID","captcha_focused" , "HDFCNB", "recover_custid");
                },false);
                __juspay.scrollToElement(mobilenumber);
                continueBtn.addEventListener("click", function (){
                    captchafield.value = (captchafield.value).toUpperCase() ;
                }, false);
                focusElement(mobilenumber);
            }
        },
        {  //hdfc_nb login page after wrong attempt
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck:function() {
                return document.querySelector('input[name="fldCustId"][type="text"]') && document.querySelector('input[name="fldPassword"][type="password"]') && document.querySelector('a img')
            },
            action: function() {
                Gatekeeper.showPasswordHelperFragment();
                Gatekeeper.changeNextActionText("Continue");
                var custid = document.querySelector('input[name="fldCustId"][type="text"]');
                var pwd = document.querySelector('input[name="fldPassword"][type="password"]');
                attachAlphaNumericPasswordHelper(pwd);
                var checkbox = document.querySelector("#chkrsastu");
                if(checkbox){
                    checkbox.checked = true;
                }
                if (__juspay.isMobUiEnabled("MOBUI_HDFCNB")) {
                    try {
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        var doc = document;
                        doc.querySelector('form').width=getScreenWidth();
                        var table = doc.querySelectorAll('table')[1];
                        var table0=doc.querySelectorAll('table')[0];
                        var row =table.childNodes[1].childNodes[0];
                        row.removeChild(row.childNodes[2]);
                        var row0=table0.childNodes[1].childNodes[0];
                        row0.removeChild(row0.childNodes[3]);
                        var passtable=doc.querySelectorAll('table[class="label"]')[1];
                        passtable.childNodes[1].childNodes[0].childNodes[2].innerText="Password: ";
                        var del=doc.querySelectorAll('table[class="label"]')[2];
                        del.parentElement.removeChild(del);
                        var table3 = doc.querySelectorAll('table[class="label"]')[2];
                        table3.childNodes[1].removeChild(table3.childNodes[1].childNodes[0]);
                        doc.querySelectorAll('table')[1].style.width=getScreenWidth();
                        doc.querySelectorAll('table')[0].style.width=getScreenWidth();
                        row.childNodes[1].align="left";
                        row0.childNodes[2].align="left";
                        doc.querySelectorAll('table')[1].style.width=getScreenWidth();
                        doc.querySelectorAll('table')[0].style.width=getScreenWidth();
                        var del2 = doc.querySelector('td[background= "/gif/log-right.gif"]');
                        del2.parentElement.removeChild(del2);
                        var table2 = doc.querySelector('table[bordercolor= "#6666CC"]');
                        var del3 = table2.childNodes[1].childNodes[2];
                        del3.parentElement.removeChild(del3);
                        table2.width=getScreenWidth();
                        var pad = doc.querySelector('td[bgcolor="#EDF0F4"]');
                        pad.style.paddingRight = getScreenWidth()*0.25;
                        pad.style.paddingLeft = getScreenWidth()*0.25;
                        var p = doc.querySelector('p[class="fPinPwd"]');
                        p.parentElement.insertBefore(document.createElement('br'),p);
                        var a =doc.querySelectorAll('a img')[1].parentElement;
                        a.removeAttribute("href");
                        if(doc.querySelectorAll('img').length==3){
                            document.querySelector('p[class="fPinPwd"]').parentElement.setAttribute("nowrap",true)
                        }
                        window.trackEventv2_0default("acs","info","MOBUI","RELOGIN_PAGE");
                    } catch(err) {
                        if (!Date.now) {
                            Date.now = function() { return new Date().getTime(); }
                        }
                        var current_time = Math.floor(Date.now() / 1000);
                        Gatekeeper.addDataToSharedPrefs("MOBUI_HDFCNB_ERROR_TIME",current_time.toString());
                        window.trackEventv2_0default("acs","info","MOBUI","RELOGIN_PAGE");
                    }
                }
            },
            clickSubmitButton:function () {
                var loginBtn = document.querySelector('a img');
                __juspay.clickOnElement(loginBtn);
                Gatekeeper.removeFragment("login page");
            }
        },
        { //hdfc_nb recover cust id uber flow otp page
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck:function() {
                return  document.querySelector('a img[alt="Continue"]')  && document.querySelector('input[name="fldOtp"][type="text"]')
            },
            action: function() {
               __juspay.trackUnblocker("INPUT_OTP_PAGE");
               otpFragmentHelper('input[name=fldOtp]',"");
            },
            submitOtp: function(otp) {
                var elt=  document.querySelector('input[name="fldOtp"]');
                if(elt) {
                    window.trackEventv2_0("acs","info","HDFCNB_RECOVER_CUSTID","otp_auto_detect", "HDFCNB", "recover_custid");
                    elt.value = otp;
                    var submit = document.querySelector('a img');
                    __juspay.clickOnElement(submit);
                    Gatekeeper.hideAssistanceFragment();
                    Gatekeeper.removeFragment("Confirm clciked in final reset page");
                }
            }
        },

        {  //hdfc_nb reset ipin uber flow otp page
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck:function() {
             return  document.querySelector('img[src="/gif/continue.gif"]') && document.querySelector('input[name="fldOtpToken"][type="text"]')
            },
            action: function() {
                __juspay.trackUnblocker("INPUT_OTP_PAGE");
                otpFragmentHelper('input[name="fldOtpToken"]',"");
            },
            submitOtp: function(otp) {
                var elt=  document.querySelector('input[name="fldOtpToken"]');
                var submit = document.querySelector('a img');
                if(elt) {
                    window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","otp_auto_detect", "HDFCNB", "reset_ipin");
                    elt.value = otp;
                    __juspay.clickOnElement(submit)
                    Gatekeeper.hideAssistanceFragment();
                }
            }
        },
        {  //hdfc_nb reset ipin final page
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck:function() {
                return  document.querySelector('input[name="fldPin"][type="password"]') && document.querySelector('input[name="fldNewPass"]') && document.querySelector('input[name="fldNewPassAgain"]')
            },
            action:function() {
                __juspay.trackUnblocker("RESET_PASSWORD");
                var atmpin= document.querySelector('input[name="fldPin"][type="password"]');
                var newpass= document.querySelector('input[name="fldNewPass"]');
                var newpassrepeat =document.querySelector('input[name="fldNewPassAgain"]');
                var confirm =document.querySelectorAll('a img')[0];
                attachPhonePasswordHelper(atmpin);
                attachAlphaNumericPasswordHelper(newpass);
                attachAlphaNumericPasswordHelper(newpassrepeat);
                Gatekeeper.changeNextActionText("Continue");
                if(atmpin.value=="")
                    focusElement(atmpin);
                else if (newpass.value=="")
                    focusElement(newpass);
                else if(newpassrepeat.value=="")
                    focusElement(newpassrepeat);
                confirm.addEventListener("click",function(){
                    if(atmpin.value.length==4) {
                        Gatekeeper.removeFragment("reset_ipin_final_page");
                    }
                },false);

            },
            clickSubmitButton: function() {
                var atmpin= document.querySelector('input[name="fldPin"][type="password"]');
                var newpass= document.querySelector('input[name="fldNewPass"]');
                var newpassrepeat =document.querySelector('input[name="fldNewPassAgain"]');
                var confirm =document.querySelectorAll('a img')[0];
                if(document.activeElement==atmpin) {
                    Gatekeeper.changeNextActionText("Continue");
                    newpass.focus();
                }
                else if(document.activeElement==newpass) {
                    newpassrepeat.focus();
                    Gatekeeper.changeNextActionText("Confirm");
                }
                else if(document.activeElement==newpassrepeat) {
                    __juspay.clickOnElement(confirm);
                    Gatekeeper.removeFragment("reset_ipin_final_page");
                }
            }
        },
        { // HDFC RESET IPIN ENTRY PAGE
            path: /\/netbanking\/IpinResetUsingOTP.htm/,
            hostname:"netbanking.hdfcbank.com",
            state:"UNUSED",
            local:false,
            bank:"HDFCNB",
            domCheck: function(){
                return document.querySelector('input[name="fldLoginUserId"]') && document.querySelector('form[name="frmRegIPin"]');
            },
            action:function() {
                window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","entry_page", "HDFCNB", "reset_ipin");
                var customerIdField = document.querySelector('input[name="fldLoginUserId"]');
                var continueBtn = document.querySelector("a img");
                if(customerIdField) {
                    numberTextFieldQuery("input[name=fldLoginUserId]");
                    attachPhoneKeyboard(customerIdField);
                    var bankCustomerId = Gatekeeper.getBankCustomerId();
                    if(bankCustomerId==="undefined" ||typeof( bankCustomerId)=="undefined") {
                        bankCustomerId = "";
                    }
                    customerIdField.value = bankCustomerId;
                    focusElement(customerIdField);
                    Gatekeeper.showNetbankingDefaultFragment();
                    __juspay.scrollToElement(customerIdField);
                    if(continueBtn) {
                        continueBtn.addEventListener("click", function() {
                            window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","continue_clicked", "HDFCNB", "reset_ipin");
                        },false);
                     }
                }
            },
            nextAction: function() {
                var customerIdField = document.querySelector('input[name="fldLoginUserId"]');
                var continueBtn = document.querySelector("a img");
                window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","continue_clicked", "HDFCNB", "reset_ipin");
                __juspay.clickOnElement(continueBtn);
            }
        },
        {  //HDFC_NB: reset ipin  choose debit card page
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function() {
                return document.querySelector('form[name="frmDCList"]');
            },
            action:function() {
                __juspay.trackUnblocker("CHOOSE_CARD");
                var cardone = document.querySelectorAll('a')[0];
                var cardtwo = document.querySelectorAll('a')[1];
                if(!cardtwo)
                    __juspay.clickOnElement(cardone);
            }
        },
        { //HDFC_NB: reset ipin flow second captcha page
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function() {
                var element= document.querySelectorAll('td')[5] ;
                if(element) {
                    return (document.querySelector("input[name=fldMobile][type=checkbox]") ) && ((element.innerText.indexOf("Note: A Password would be"))>-1);
                }
            },
            action:function() {
                 __juspay.trackUnblocker("RESET_IPIN_CAPTCHA2");
                var mobNumChkBox = document.querySelector("input[name=fldMobile][type=checkbox]");
                var captchafield = document.querySelector('input[name="fldCaptcha"][class="input_password"]');
                var continueBtn = document.querySelectorAll('a img');
                Gatekeeper.showNetbankingDefaultFragment();
                if(mobNumChkBox) {
                    if(mobNumChkBox.checked==false) {
                        mobNumChkBox.checked="true";
                    }
                }
                if(captchafield) {
                    document.querySelectorAll('a img')[1].style.position="relative";
                    document.querySelector('td[colspan="2"]').style.left="-100px";
                    document.querySelector('td[colspan="3"]').style.display="none";
                    document.querySelectorAll('a img')[1].style.top="-20px";
                    document.querySelectorAll('a img')[1].style.left="400px"
                    document.querySelector('td[colspan="2"]').style.position="relative";
                    attachAlphaNumericPasswordHelper(captchafield);
                    focusElement(captchafield);
                    __juspay.scrollToElement(captchafield);
                    continueBtn[1].addEventListener("click", function() {
                        captchafield.value=  (captchafield.value).toUpperCase();
                        if(captchafield.value.length==6) {
                            Gatekeeper.removeFragment("second captcha page");
                        }
                    },false);
                } else {
                    __juspay.clickOnElement(continueBtn[0]);
                    window.trackEventv2_0("acs", "info", "HDFCNB_RESET_IPIN","captcha2_null" , "HDFCNB", "reset_ipin");
                    Gatekeeper.removeFragment("second captcha page");
                }
            },
            clickSubmitButton: function() {
                var captchafield = document.querySelector('input[name="fldCaptcha"][class="input_password"]');
                var continueBtn = document.querySelectorAll('a img');
                if(captchafield){
                    captchafield.value = (captchafield.value).toUpperCase();
                    __juspay.clickOnElement(continueBtn[1]);
                    Gatekeeper.removeFragment("second captcha page");
                } else {
                    __juspay.clickOnElement(continueBtn[0]);
                    window.trackEventv2_0("acs", "info", "HDFCNB_RESET_IPIN","captcha2_null" , "HDFCNB", "reset_ipin");
                    Gatekeeper.removeFragment("second captcha page");
                }
            }
        },
        {  //HDFC_NB: reset password uber flow captcha page
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function() {
                return document.querySelector('input[name="fldLoginUserId"][readonly="true"]') && document.querySelector('input[name="fldCaptcha"][class="input_password"]')
            },
            action:function() {
                __juspay.trackUnblocker("RESET_IPIN_CAPTCHA1");
                var captchafield= document.querySelector('input[name="fldCaptcha"][class="input_password"]');
                var continueBtn = document.querySelectorAll('a img');
                Gatekeeper.showNetbankingDefaultFragment();
                if(captchafield) {
                    captchafield.addEventListener("focus",function(){
                        __juspay.delayMe(function(){
                            Gatekeeper.requestPasswordKeyboardShow();
                        },200);
                    },false);
                    focusElement(captchafield);
                    __juspay.scrollToElement(captchafield);
                    continueBtn[1].addEventListener("click", function() {
                        captchafield.value=  (captchafield.value).toUpperCase();
                        if(captchafield.value.length==6)
                            Gatekeeper.removeFragment("first captcha page");
                    },false);
                } else {
                    __juspay.clickOnElement(continueBtn[0]);
                    Gatekeeper.removeFragment("first captcha page");
                }
            },
            nextAction: function(){
                var captchafield= document.querySelector('input[name="fldCaptcha"][class="input_password"]');
                var continueBtn = document.querySelectorAll('a img');
                if(captchafield){
                    captchafield.value = (captchafield.value).toUpperCase() ;
                    Gatekeeper.removeFragment("first captcha page");
                    __juspay.clickOnElement(continueBtn[1]);
                } else {
                    __juspay.clickOnElement(continueBtn[0]);
                    window.trackEventv2_0("acs", "info", "HDFCNB_RESET_IPIN","captcha1_null", "HDFCNB", "reset_ipin");
                    Gatekeeper.removeFragment("first captcha page");
                }
            }
        },
        { // HDFCNB: AUTO CLICK RESET CUSTID  LINK PAGE
            path: /\/netbanking/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function() {
                return !(Gatekeeper.getSessionAttribute("reset_custid_autoclick") == "false") && document.querySelector('frame[name="login_page"]');
            },
            action:function() {
                var forgotcustidlink;
                window.trackEventv2_0("acs","info","HDFCNB_RECOVER_CUSTID","entry_page", "HDFCNB", "recover_custid");
                Gatekeeper.setSessionAttribute("reset_custid_autoclick","false");
                try {
                    var frame = parent.document.getElementsByTagName('frame')[0] ; //.contentDocument.querySelectorAll('a[href="javascript:void(0)"][onClick="return fretrieveCustId();"]')[0] ;
                    if(frame){
                        var doc = frame.contentDocument;
                        if(doc) {
                            forgotcustidlink = doc.querySelectorAll('a[href="javascript:void(0)"][onClick="return fretrieveCustId();"]')[0] ;
                        }
                    }
                    var frame2 = parent.document.getElementsByTagName('frame')[1] ;
                    if(frame2){
                        var doc = frame2.contentDocument;
                        if(doc) {
                            var forgot = doc.querySelectorAll('a[href="javascript:void(0)"][onClick="return fretrieveCustId();"]')[0] ;
                        }
                    }
                    var forgotlink = document.querySelectorAll('a[href="javascript:void(0)"][onClick="return fretrieveCustId();"]')[0];
                    if(forgotcustidlink) {
                        __juspay.clickOnElement(forgotcustidlink);
                    }
                    if(forgotlink) {
                        __juspay.clickOnElement(forgotlink);
                    }
                    if(forgot) {
                        __juspay.clickOnElement(forgot);
                    }
                } catch(err) {
                    window.trackEventv2_0default("acs","error","exception caught","Function: Error is "+ String(err));
                }
                Gatekeeper.removeFragment("Removing_fragments");
            }
        },
        { //hdfcnb trigger uber on invalid password or customer id
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank:"HDFCNB",
            domCheck:function(){
                var invalid=document.querySelector('strong');
                if(invalid){
                    return (((invalid.innerText.indexOf("you have entered is invalid"))>-1) || ((invalid.innerText.indexOf("You are not authorized to do"))>-1) )
                }
            },
            showUberDialog:function(props,data) {
                props.type = "dialog";
                props.height = props.height || 320;
                props.width = props.width || -2;
                if(props.showOnLoad) {
                    setTimeout(function(){
                    Gatekeeper.showUber(false);
                    },500);
                    props.showOnLoad = false;
                }
                GK.setUberEventWithCallback("HDFCNB_RESET_IPIN", function(result) {
                    window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","uber_shown", "HDFCNB", "reset_ipin")
                    if( result == "ok" ) {
                        window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","reset_ipin_clicked", "HDFCNB", "reset_ipin");
                        Gatekeeper.setSessionAttribute("reset_custid_autoclick","false");
                        window.location.href = "https://"+ window.location.hostname + "/netbanking/IpinResetUsingOTP.htm";
                    } else if(result == "cancel") {
                        window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","recover_custid_clicked", "HDFCNB", "reset_ipin");
                        Gatekeeper.setSessionAttribute("reset_custid_autoclick","true");
                        window.location.href = "https://"+ window.location.hostname + "/netbanking/";
                    } else if(result == "closed") {
                        window.trackEventv2_0("acs","info","HDFCNB_RESET_IPIN","close_clicked", "HDFCNB", "reset_ipin");
                    }
                },props,data);
            },
            action:function(){
                __juspay.trackUnblocker("INVALID_ID_OR_IPIN");
                Gatekeeper.changeNextActionText("Back");
                if(__uber.isUberEnabled("HDFCNB_RESET_IPIN")) {
                    var wrongAttemptCount = __uber.initSessionCounterIncremental("HDFCNB_ERROR_COUNT");
                    var invalid=document.querySelector('strong');
                    if(invalid && ((invalid.innerText.indexOf("you have entered is invalid"))>-1) ) {
                        this.showUberDialog({showOnLoad:true,onPageLoad:"destroy"},null);
                    } else if(wrongAttemptCount > 1) {
                        this.showUberDialog({showOnLoad:true,onPageLoad:"destroy"},null);
                    } else if(wrongAttemptCount == 4) {
                        this.showUberDialog({showOnLoad:true,onPageLoad:"destroy"},JSON.stringify({
                            message:"Wrong password will lock your account for 24 hours"
                        }));
                    }
                }
            },
            nextAction:function() {
                var loginBtn= document.querySelector('p img');
                __juspay.clickOnElement(loginBtn);
                Gatekeeper.removeFragment("login page");
            }
        },
        { // HDFCNB PAGE AFTER SUCCESSFUL IPIN RESET
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function() {
                var element = document.querySelector('h1');
                if(element) {
                    return ((element.innerText.indexOf("Change Password - Complete"))>-1);
                } else {
                    return false;
                }
            },
            action:function(){
                __juspay.trackUnblocker("RESET_IPIN_SUCCESSFUL");
            }
        },
        { // HDFCNB : PAGE AFTER SUCCESSFUL CUSTID RECOVERY
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function() {
                var element =document.querySelectorAll('strong')[1];
                if(element) {
                    return ((element.innerText.indexOf("Customer ID"))>-1) && document.querySelector('span[class="headingLable"]');
                } else {
                    return false;
                }
            },
            action:function(){
                __juspay.trackUnblocker("RECOVER_CUSTID_SUCCESSFUL");
            }
        },
        {  //HDFC_NB: Wrong Ipin or NB disabled
            path: /\/netbanking\/(entry|epientry)/,
            hostname: "netbanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function(){
                return (document.querySelector('form[name="frmMain"]')) && (document.querySelector('form[name="frmEpi"]'));
            },
            action:function(){
                __juspay.trackInvalidScenarios("The Customer ID/IPIN (password) you have entered is invalid, please try again", "Invalid_Password");
                __juspay.trackInvalidScenarios("Sorry, access to NetBanking has been disabled.", "Net banking disabled: crossed invalid credentials limits");
            }
        },
        //hdfcnb new page
        {
            path: /\/meap\/apps\/services\/preview\/meap\/mobilewebapp/,
            hostname: "mobilebanking.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFCNB",
            domCheck: function(){
                return document.querySelector('img[src*=HDFCBANK][alt=HDFC\\ BANK]');
            },
            action: function(){
                var loginIdFieldChecker = setInterval(function(){
                    var passwordField = document.querySelector('input[id=fldPassword][type=password]')
                    var loginIdField = document.querySelector('input[type=number][id=fldLoginUserId]')
                    if(loginIdField){
                        clearInterval(loginIdFieldChecker)
                        if(!passwordField){
                            loginIdField.blur();
                            loginIdField.setAttribute("type","text");
                            window.trackEventv2_0default("acs","info","hdfcnb","changed_type_attribute_text");
                            setupLoginPageWithUsernameAndSubmitButton("input[id=fldLoginUserId]","input[type=Submit][value=Continue]");
                            __juspay.trackPageStatus("INPUT_LOGIN_USERNAME");
                            __juspay.trackAuthMethod("LOGIN");
                        }
                        var pwdCheck = setInterval(function(){
                            var passwordField = document.querySelector('input[id=fldPassword][type=password]')
                            var checkbox = document.querySelector('input[type=checkbox][id=chkLogin]');
                            var loginBtn = document.querySelector('input[type=submit][value=Login]')
                            if(passwordField && checkbox && loginBtn){
                                clearInterval(pwdCheck)
                                passwordFragmentHelper('input[id=fldPassword][type=password]');
                                checkbox.checked = true;
                                __juspay.trackPageStatus("INPUT_LOGIN_PASSWORD");
                                __juspay.trackAuthMethod("PASSWORD");
                            }
                        },500)
                        var isSelectPresentChecker = setInterval(function(){
                            var isSelectPresent = document.querySelector('select[id=fldCodAcctNo] option')
                            var confirmBtn = document.querySelector('input[value=Confirm]')
                            if(isSelectPresent && confirmBtn){
                                clearInterval(isSelectPresentChecker);
                                accountSelectionPage('select[id=fldCodAcctNo] option',"HDFCNB",'input[value=Confirm]','');
                            }
                        },500)
                        var otpFieldChecker = setInterval(function(){
                            if(document.querySelector('input[id=fldOtpToken]')){
                               clearInterval(otpFieldChecker);
                               otpFragmentHelper('input[id=fldOtpToken]',"");
                            }
                        },500)
                    }
                },500)
            },
            nextAction: function(){
                var loginIdField = document.querySelector('input[type=text][id=fldLoginUserId]')
                var passwordField = document.querySelector('input[id=fldPassword][type=password]')
                var continueBtn = document.querySelector('input[type=Submit][value=Continue]');
                var isSelectPresent = document.querySelector('select[id=fldCodAcctNo]')
                var confirmBtn = document.querySelector('input[value=Confirm]')
                if(loginIdField && !passwordField){
                    if(loginIdField.value === ""){
                        loginIdField.focus();
                    }
                    else if(continueBtn){
                        __juspay.clickOnElement(continueBtn);
                    }
                }
                if(isSelectPresent && confirmBtn){
                    __juspay.clickOnElement(confirmBtn);
                }
            },
            clickSubmitButton: function(){
                var loginBtn = document.querySelector('input[type=submit][value=Login]')
                if(loginBtn){
                    __juspay.clickOnElement(loginBtn);
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id=fldOtpToken]','input[type=submit][name=fldOtpAuth][value=Ok]');
            }
        },
        { //KOTAKNB new Login Page
            path: /\/pmtgt(\/ksecLogin\.jsp)?/,
            hostname:"www.kotak.com",
            state: "UNUSED",
            local: false,
            bank: "KOTAKNB",
            domCheck: function(){
               return document.querySelector("input[id=crn]") &&
                        document.querySelector('input[id=pswd][type=password]') &&
                        document.querySelector("a[id=secure-login01][onclick*=button1submit]");
            },
            action: function(){

                var loginViaMobileBankingTab = document.querySelector("a[href*=tab-01]");

                var crnTab = document.querySelector("a[href*=tab-crn]");
                var crnfield1 = document.querySelector("input[id=crn]");
                var passwordField = document.querySelector('input[id=pswd][type=password]');

                var nicknameTab = document.querySelector("a[href*=tab-nickname]");
                var nickNameField = document.querySelector("input[id=crnAlias]");
                var passwordFieldNickName = document.querySelector('input[id=pswd1][class=nickname-password][name=passwordAlias]');

                var loginViaDebitCardTab = document.querySelector("a[href*=tab-02]");
                var crnfield2 = document.querySelector('input[id=e-crn]');
                var atmPinField = document.querySelector('input[id=atm-pin]');
                var debitCardList = document.querySelector('select[id=cardNumber]');

                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                Gatekeeper.removeFragment("Clicked net or mobile banking tab");
                focusElement(crnfield1);
                Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                Gatekeeper.requestPhoneKeyboardShow();

                passwordField.addEventListener("focus",function(){
                    attachAlphaNumericPasswordHelper(passwordField);
                    Gatekeeper.showPasswordHelperFragment();
                },false);
                if(nickNameField){
                    nickNameField.addEventListener("focus",function(){
                        Gatekeeper.removeFragment("nick name field clicked");
                    },false);
                }
                if(passwordFieldNickName){
                    passwordFieldNickName.addEventListener("focus",function(){
                        attachAlphaNumericPasswordHelper(passwordFieldNickName);
                        Gatekeeper.showPasswordHelperFragment();
                    },false);
                }
                if(crnTab && crnfield1){
                    crnTab.addEventListener("focus",function(){
                        Gatekeeper.removeFragment("Clicked net or mobile banking tab");
                        focusElement(crnfield1);
                        Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                        Gatekeeper.requestPhoneKeyboardShow();
                    },false);
                }
                if(nicknameTab && nickNameField){
                    nicknameTab.addEventListener("click",function(){
                        Gatekeeper.removeFragment("Clicked nick name tab");
                        focusElement(nickNameField);
                    },false);
                }
                if(crnfield1){
                    crnfield1.addEventListener("focus",function(){
                        Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                        Gatekeeper.requestPhoneKeyboardShow();
                    },false);
                }
                if(loginViaDebitCardTab && crnfield2){
                    loginViaDebitCardTab.addEventListener("click",function(){
                        Gatekeeper.removeFragment("Clicked debit card tab");
                        focusElement(crnfield2);
                        Gatekeeper.requestPhoneKeyboardShow();
                    },false);
                }
                if(debitCardList){
                    debitCardList.addEventListener("focus",function(){
                        Gatekeeper.removeFragment("debit Card List clicked");
                    },false);
                }
                if(atmPinField){
                    atmPinField.addEventListener("focus",function(){
                        Gatekeeper.removeFragment("atm pin field clicked");
                        Gatekeeper.requestPhoneKeyboardShow();
                    },false);
                }
                if(loginViaMobileBankingTab){
                    loginViaMobileBankingTab.addEventListener("focus",function(){
                        Gatekeeper.removeFragment("Clicked net or mobile banking tab");
                    },false);
                }
            },
            clickSubmitButton: function(){
                var submitBtn1 = document.querySelector("a[id=secure-login01][onclick*=button1submit]");
                var submitBtn2 = document.querySelector("a[id=secure-login02][onclick*=button2submit]");
                var activeTab = document.querySelector("#authOption");
                if(activeTab){
                    var activeTabvalue = activeTab.value;
                    if(activeTabvalue && submitBtn1 && submitBtn2){
                        if(activeTabvalue == "authCRN"){
                            __juspay.clickOnElement(submitBtn1);
                        }
                        if(activeTabvalue == "authCRNAlias"){
                            if(submitBtn2){
                                __juspay.clickOnElement(submitBtn2);
                            }else{
                                window.trackEventv2_0("acs","error","modify_page_error",bank + ":Submit button dom changed", "modify_page", "error");
                            }
                        }
                    }
                }
            },
            nextAction: function(){
                var passwordField = document.querySelector('input[id=pswd][type=password]');
                passwordField.focus();
            }
        },
        { //KOTAKNB Login Page
            path: /\/pg\/(ksecLogin|authenticateLogin|mobauthenticateLogin)\.jsp/,
            hostname:"www.kotak.com",
            state: "UNUSED",
            local: false,
            bank: "KOTAKNB",
            domCheck: function(){
               return document.querySelector("input[id=crn][type=number]") &&
                      document.querySelector('input[id=pswd][type=password]') &&
                      document.querySelector("a[id=pay]")  &&
                      document.querySelector('input[id=crnAlias]') &&
                      document.querySelector('input[id=pswd1][type=password]') &&
                      document.querySelector('input[id=atm-pin]') &&
                      document.querySelector('input[id=e-crn][type=number]') &&
                      document.querySelector("a[id=secure-login01][onclick*=button1submit]") &&
                      document.querySelector("a[id=secure-login02][onclick*=button2submit]");
            },
            action: function(){
                var loginViaDebitCardTab = document.querySelector('a[id=ui-id-2]')
                var crnTab = document.querySelector('a[id=ui-id-3][onclick*=authCRN]');
                var nickNameTab = document.querySelector('a[id=ui-id-4][onclick*=authCRNAlias]');
                var atmPinField = document.querySelector('input[id=atm-pin]');

                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");

                var _this = this;
                var mpinCallback = function(response) {
                    try {
                        Gatekeeper.hideWaitingFragment();
                        response = JSON.parse(response)

                        if(response.continueWithMPIN) {
                            _this.showMPINDialog(response);
                            _this.poll();
                        } else {
                            window.trackEventv2_0("acs", "info", "MPIN_modal_shown", "false", "mpin", "modal_shown");
                            setupLoginPageWithSubmitButton("input[id=pswd][type=password]",
                                                           "input[id=crn][type=number]",
                                                           "a[id=secure-login01][onclick*=button1submit]");
                            if(nickNameTab){
                                nickNameTab.addEventListener("click",function(){
                                    Gatekeeper.removeFragment("Clicked Nick Name tab under Login via Netbanking");
                                    setupLoginPageWithSubmitButton("input[id=pswd1][type=password]",
                                                                   "input[id=crnAlias]",
                                                                   "a[id=secure-login02][onclick*=button2submit]");
                                },false);
                            }
                            if(crnTab){
                                crnTab.addEventListener("click",function(){
                                    Gatekeeper.removeFragment("Clicked Crn tab under Login via Netbanking");
                                    setupLoginPageWithSubmitButton("input[id=pswd][type=password]",
                                                                   "input[id=crn][type=number]",
                                                                   "a[id=secure-login01][onclick*=button1submit]");
                                },false);
                            }
                            if(loginViaDebitCardTab && atmPinField){
                                loginViaDebitCardTab.addEventListener("click",function(){
                                    Gatekeeper.removeFragment("Clicked Login via Debit card Tab");
                                    attachPhoneKeyboard(atmPinField);
                                },false);
                            }
                        }
                    } catch(error) {
                        window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error in mpin callback: " + error, "MPIN", "exception");
                    }
                }

                Gatekeeper.setupMPIN(this.bank, "{}", mpinCallback);
            },
            nextAction: function(){
                var passwordField = document.querySelector('input[id=pswd]')
                var passwordField1 = document.querySelector('input[id=pswd1]')
                var activeTab = document.querySelector("#authOption");
                if(activeTab){
                   var activeTabvalue = activeTab.value;
                   if(activeTabvalue && passwordField && passwordField1){
                       if(activeTabvalue == "authCRN"){
                           passwordField.focus();
                       }
                       if(activeTabvalue == "authCRNAlias"){
                           passwordField1.focus();
                       }
                   }
               }
            },
            clickSubmitButton: function(){
                var submitBtn1 = document.querySelector("a[id=secure-login01][onclick*=button1submit]");
                var submitBtn2 = document.querySelector("a[id=secure-login02][onclick*=button2submit]");
                var activeTab = document.querySelector("#authOption");
                if(activeTab){
                    var activeTabvalue = activeTab.value;
                    if(activeTabvalue && submitBtn1 && submitBtn2){
                        if(activeTabvalue == "authCRN"){
                            __juspay.clickOnElement(submitBtn1);
                        }
                        if(activeTabvalue == "authCRNAlias"){
                            __juspay.clickOnElement(submitBtn2);
                        }
                    }
                }
            },
            showMPINDialog: function(response) {
                try{
                    _this = this;
                    var modalStart;
                    var modalEnd;
                    if(window.__JUSPAY_MPIN_MODAL) {
                        window.__JUSPAY_MPIN_MODAL.addCss();
                        window.__JUSPAY_MPIN_MODAL.insertButton();

                        document.getElementById("inline-kotak-button").addEventListener("click", function() {
                            window.trackEventv2_0("acs", "info", "MPIN_pay_with_kotak_click", "true","mpin", "pay_with_kotak_click");
                            _this.startMpin()
                        })

                        if(response.mpin_preference !== "NETBANKING") {
                            window.trackEventv2_0("acs", "info", "MPIN_modal_shown", "true", "mpin", "modal_shown");
                            modalStart = Date.now();
                            window.__JUSPAY_MPIN_MODAL.show();

                            document.getElementById("modal-nb-button").addEventListener("click", function() {
                                window.__JUSPAY_MPIN_MODAL.hide();
                                modalEnd = Date.now();
                                window.trackEventv2_0("acs", "info", "MPIN_modal_action", "NETBANKING", "MPIN", "modal_action");
                                window.trackEventv2_0("acs", "info", "MPIN_time_spent_on_modal", ((modalEnd - modalStart)/1000).toString() + "s","MPIN", "time_spent_on_modal")
                                Gatekeeper.saveMPINPreference(_this.bank, "NETBANKING");
                            })

                            document.getElementById("modal-kotak-button").addEventListener("click", function() {
                                modalEnd = Date.now();
                                window.trackEventv2_0("acs", "info", "MPIN_modal_action", "MPIN","MPIN", "modal_action");
                                window.trackEventv2_0("acs", "info", "MPIN_time_spent_on_modal", ((modalEnd - modalStart)/1000).toString() + "s","MPIN", "time_spent_on_modal")
                                _this.startMpin();
                            })
                        } else {
                            window.trackEventv2_0("acs", "info", "MPIN_modal_shown", "false", "mpin", "modal_shown");
                        }
                    } else {
                        //Modal script may not have loaded yet. Wait for it
                        window.__JUSPAY_MPIN_MODAL_CALLBACK = function() {
                            _this.showMPINDialog(response);
                        }
                    }
                } catch (err){
                    console.error(err);
                    window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error while showing MPINDialog: " + err, "MPIN", "exception");
                }
            },
            startMpin: function(preference) {
                try {
                    var payload = document.getElementsByName('msg')[0].value;
                    Gatekeeper.showWaitingFragment();

                    var data = {
                        payload: payload,
                        isGodel: true,
                        packageName: Gatekeeper.getSessionAttribute('package_name') //TODO add missing session attribute to native code
                    }

                    var callback = function(response) {
                        try {
                            response = JSON.parse(response);
                            if(response.redirectUrl) {
                                window.location.href = response.redirectUrl;
                            } else {
                                window.__JUSPAY_MPIN_MODAL.hide();
                            }
                        } catch(error) {
                            window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error in mpin callback: " + error, "MPIN", "exception");
                        }
                    }

                    Gatekeeper.startMPIN(this.bank, JSON.stringify(data), callback);
                } catch(error) {
                    window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error while starting MPIN: " + error, "MPIN", "exception");
                }
            },
            poll: function() {
                try {
                    var payload = document.getElementsByName('msg')[0].value;
                    console.log("Polling")
                    var mpin = JSON.parse(Gatekeeper.getConfig()).mpin_config || {};
                    var specs = mpin[this.bank] || {};
                    var statusCheckUrl = specs.pollingUrl;

                    var postParams = {
                        transactionId: payload.split('|')[3],
                        merchantId: payload.split('|')[2]
                    }

                    var responseCallback = function(response) {
                        try {
                            console.log("Callback Response: ", response);
                            try {
                                response = JSON.parse(response);
                                response = response.content || {};
                            } catch(err) {
                                response = {};
                            }

                            if(response.redirectUrl) {
                                window.trackEventv2_0default("acs", "info", "MPIN", "Got redirect URL from poller");
                                var params = (response.redirectParams) ? "?msg=" + response.redirectParams : ""
                                window.trackEventv2_0("acs", "info", "MPIN_redirecting", "true", "MPIN", "redirecting");
                                window.location.href = response.redirectUrl + params;
                            } else {
                             setTimeout(function() {
                                 startPolling(statusCheckUrl, postParams, responseCallback, false);
                             }, 1000);
                            }
                        } catch(error) {
                            window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error in poll response: " + error, "MPIN", "exception");
                        }
                    }

                    startPolling(statusCheckUrl, postParams, responseCallback, false);
                } catch(error) {
                    window.trackEventv2_0("acs", "error", "MPIN_Exception", "Error in poll function: " + error, "MPIN", "exception");
                }
            }
        },
        //KOTAKNB CRN INFO PAGE
        {
            path: /\/wbg\/knowYourKtkCrn\.jsp/,
            hostname: "www.kotak.com",
            state: "UNUSED",
            local: false,
            bank: "KOTAKNB",
            domCheck: function(){
                return true;
            },
            action: function(){
                __juspay.trackUnblocker("CRN_INFO");
            }
        },
        //KOTAKNB OTP PAGE
        {
            path: /\/(pg|pggbm|pmtgt)\/(authenticateLogin|mobemasTwoFactoAuth|mobauthenticateLogin|emasCheck)\.jsp/,
            hostname:"www.kotak.com",
            state: "UNUSED",
            local: false,
            bank: "KOTAKNB",
            domCheck: function(){
                return document.querySelector("input[name=smspin]") && document.querySelector("a[onclick*=button1submit]");
            },
            action: function(){
                Gatekeeper.removeFragment("Reached OTP page");
                Gatekeeper.setPollingForSmsEnabled(true);
                var resendLink = document.querySelector("a[onclick*=openSmsGen]");
                var otpField = document.querySelector("input[name=smspin]");
                var submitBtn = document.querySelector("a[onclick*=button1submit]");
                var getOtpOnIvrLink = document.querySelector("a[href*=getIVRcall]");
                if(getOtpOnIvrLink){
                    getOtpOnIvrLink.addEventListener("click",function(){
                        Gatekeeper.removeFragment("OTP requested on Call");
                    });
                }
                if(otpField){
                    otpFragmentHelper('input[name=smspin]','a[onclick*=openSmsGen]');
                    if(window.location.href.indexOf("emasCheck")>-1){
                        //If it was an error Page
                        Gatekeeper.removeFragment("Error in OTP");
                        focusElement(otpField);
                        __juspay.trackPageStatus("INPUT_OTP");
                        __juspay.trackAuthMethod("OTP");
                    }else{
                        __juspay.showOtpFragmentWrapper(otpField);
                    }
                }
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector("a[onclick*=openSmsGen]");
                if(resendLink){
                    __juspay.clickOnElement(resendLink);
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, "input[name=smspin]", "a[onclick*=button1submit]");
            }
        },
       // BANK OF INDIA NB
        {   //BOINB Retail: select banking services page
            path: /.*\/retail\/jsp\/ebpp\/TestSignOn\.aspx/,
            hostname: "starconnectcbs.bankofindia.com",
            state: "UNUSED",
            bank: "BOINB",
            local: false,
            domCheck: function(){
                var initPage = document.querySelector('td > div');
                if(initPage && initPage.innerText.indexOf("Internet Banking Services")) {
                    return true;
                }
            },
            action: function(){
                if(__juspay.isMobUiEnabled("MOBUI_BOINB")) {
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        document.querySelector('table[dir="ltr"]').style.width = "100%";
                        var lineBreaks = document.querySelectorAll('br');
                        var disclaimer = document.querySelectorAll('table')[2];
                        var merchantRedirect = document.querySelectorAll('center')[2];
                        var rem_array = [disclaimer, merchantRedirect];
                        for(var i=0;i<lineBreaks.length;i++){
                            if(i<3) {
                                rem_array.push(lineBreaks[i]);
                                rem_array.push(lineBreaks[lineBreaks.length-i-1]);
                            }
                        }
                        __juspay.removeElements(rem_array);
                        window.trackEventv2_0default("acs","info","MOUI","ACCOUNT_TYPE_SELECTION_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"ACCOUNT_TYPE_SELECTION_PAGE");
                    }
                }
            }
        },
        {  //BOINB Retail: login page
            path: /.*\/ebpp\/RetShoppingMallSignOn\.aspx/,
            hostname: "starconnectcbs.bankofindia.com",
            state: "UNUSED",
            bank: "BOINB",
            local: false,
            domCheck: function(){
                return document.querySelector('input[id=CorporateSignonCorpId]')
                       && document.querySelector('input[id=CorporateSignonPassword]');
            },
            action: function(){
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                setupLoginPageWithSubmitButton("input[id=CorporateSignonPassword]",
                                               "input[id=CorporateSignonCorpId]",
                                               "input[id=button3][type=Submit]");
                var userIdField = document.querySelector("input[id=CorporateSignonCorpId]");
                var resetBtn = document.querySelector("input[name=clear]");
                if(typeof Gatekeeper.setJavascriptToOpenWindows == "function"){
                    Gatekeeper.setJavascriptToOpenWindows(false);
                }
                if(__juspay.isMobUiEnabled("MOBUI_BOINB")) {
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        var vkeyboard = document.querySelector('div[class=Keyboard]');
                        var vkeyboardCheck = document.querySelector('div[class=recomand]');
                        var extraDiv = document.querySelector('div[id=Demo]');
                        var notice = document.querySelector('div[class=notice]');
                        var footer = document.querySelector('div[class=footer]');
                        var midcont = document.querySelector('div[class=midcont]');
                        var cont = document.querySelector('div[class=cont]');
                        var Lshadow = document.querySelector('div[class=Lshadow]');
                        var Rshadow = document.querySelector('div[class=Rshadow]');
                        var userId = document.getElementById('CorporateSignonCorpId');
                        var pwd = document.getElementById('CorporateSignonPassword');
                        var lineBreaks = document.querySelectorAll('br');
                        var tables = document.querySelectorAll('table');
                        var body = document.querySelector('body');
                        var rem_array = [vkeyboard, vkeyboardCheck, extraDiv, notice, footer, tables[1]];

                        midcont.style.width = "300px";
                        cont.style.height = "0px";
                        userId.style.float = "left";
                        userId.style.margin = "25 0 5 20px";
                        pwd.style.float = "left";
                        pwd.style.margin = "25 0 5 20px";
                        Lshadow.style.height = window.innerHeight.toString()+"px";
                        Rshadow.style.height = window.innerHeight.toString()+"px";

                        for(var i=1;i<lineBreaks.length;i++){
                            if(i<5)
                                rem_array.push(lineBreaks[i]);
                        }
                        __juspay.removeElements(rem_array);
                        window.trackEventv2_0default("acs","info","MOBUI","LOGIN_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                    }
                }
                if(resetBtn){
                    resetBtn.addEventListener("click", function() {
                        userIdField.focus();
                    });
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector("input[id=button3][type=Submit]");
                __juspay.clickOnElement(submitBtn);
            },
            showPassword: function(){
                var passwordElement = document.querySelector('input[id=CorporateSignonPassword][type=Password]');
                if(passwordElement){
                    var passwordValue = passwordElement.value;
                    if(passwordValue != undefined && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue)
                    }
                }
            },
            nextAction: function() {
                var passwordField = document.querySelector('input[id=CorporateSignonPassword]');
                if(passwordField){
                    passwordField.focus();
                }
            }
        },
        { //BOINB: remarks page

            path: /.*\/retail\/jsp\/ebpp\/CrpShoppingMallMakePayment\.aspx/,
            hostname: "starconnectcbs.bankofindia.com",
            state: "UNUSED",
            bank: "BOINB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[name="bills.PaymentRemarks"]') && document.querySelector('input[type=SUBMIT][name="Action.Bills.ShoppingMall.MakePayment.Pay"]');
            },
            action: function() {
                var remarksField = document.querySelector('input[name="bills.PaymentRemarks"]');
                accountSelectionPage('select[name = UserAccountsIndex] option',"BOINB",'input[type=SUBMIT][name="Action.Bills.ShoppingMall.MakePayment.Pay"]',remarksField);
            },//type="SUBMIT" name="Action.Bills.ShoppingMall.MakePayment.Pay"
            nextAction: function() {
                var submitBtn = document.querySelector('input[type=SUBMIT][name="Action.Bills.ShoppingMall.MakePayment.Pay"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        {
        //BOINB OTP page
            path: /\/web\/L001\/retail\/jsp\/ebpp\/ShoppingMallTxnLogin\.aspx/,
            hostname: "starconnectcbs.bankofindia.com",
            state: "UNUSED",
            bank: "BOINB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=rTxnOtp]') && document.querySelector('input[id=rTxnOtpSubmit]');
            },
            action: function() {
                otpFragmentHelper('input[id=rTxnOtp]',"");
                if(__juspay.isMobUiEnabled("MOBUI_BOINB")) {
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        var otpText = document.querySelectorAll('p[align="justify"][class="normalText"]')[0];
                        var banklogo = document.querySelector('tr > td:nth-child(1)');
                        var otpId = document.querySelectorAll('form[name="rTxnOtpSubmit"] > table > tbody > tr')[0];
                        var tables = document.querySelectorAll('table');
                        var otpSubmit = document.querySelector('input[id="rTxnOtpSubmit"]');

                        banklogo.style.width = "50%";
                        __juspay.removeElements([otpText, otpId, tables[(tables.length)-1]]);
                        otpSubmit.style.padding = "5px";
                        otpSubmit.style.margin = "30 0 0 0";
                        otpSubmit.style.width = "100%";
                        otpField.style.padding = "5px";
                        window.trackEventv2_0default("acs","info","MOBUI","OTP_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                    }
                }
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=rTxnOtp]", "input[type=submit]");
            }
        },
        {
        //BOINB Retail: confirm page, transaction password
            path: /.*\/retail\/jsp\/ebpp\/ShoppingMallTxnLogin\.aspx/,
            hostname: "starconnectcbs.bankofindia.com",
            state: "UNUSED",
            bank: "BOINB",
            local: false,
            domCheck: function(){
                return (document.querySelector('input[name="ValCorpTxnPwdUserName"]') && document.querySelector('input[name="ValCorpTxnPwdTxnPwd"]')) || (document.querySelector('form[name="frmResetContinue"]'));
            },
            action: function(){
                __juspay.trackPageStatus("TXN_PWD");
                __juspay.trackAuthMethod("TXN_PASSWORD");
                var redirectBtn = document.querySelector('img[onclick="OnResetContinue();"]');
                if(redirectBtn){
                    __juspay.clickOnElement(redirectBtn);
                }else if(document.querySelector('input[name="ValCorpTxnPwdUserName"]')&&document.querySelector('input[name="ValCorpTxnPwdTxnPwd"]')){
                    setupLoginPageWithSubmitButton('input[name="ValCorpTxnPwdTxnPwd"]',
                    'input[name="ValCorpTxnPwdUserName"]',
                    'input[name="Action.bills.ShoppingMall.TxnLogin.Ok"]');
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[name="Action.bills.ShoppingMall.TxnLogin.Ok"][type=SUBMIT]');
                __juspay.clickOnElement(submitBtn);
            },
            showPassword: function(){
                var passwordElement = document.querySelector( 'input[name="ValCorpTxnPwdTxnPwd"][type=Password]');
                if(passwordElement){
                    var passwordValue = passwordElement.value;
                    if(passwordValue != undefined && passwordValue != null) {
                        Gatekeeper.setPasswordValue(passwordValue)
                    }
                }
            },
            nextAction: function() {
                var loginIdField = document.querySelector('input[name="ValCorpTxnPwdUserName"]');
                var passwordField = document.querySelector('input[name="ValCorpTxnPwdTxnPwd"]');
                var submitBtn = document.querySelector('input[name="Action.bills.ShoppingMall.TxnLogin.Ok"][type=SUBMIT]');
                if(passwordField){
                    passwordField.focus();
                }
            }
        },
        {
            //IOB_NB: login page
            path: /ibanking\/(login|iobBILDEntry)\.do/,
            hostname: "www.iobnet.co.in",
            state: "UNUSED",
            local: false,
            bank: "IOBNB",
            domCheck: function(){
                return document.querySelector('input[name=loginId]') &&
                       document.querySelector('input[name=password][type=password]') &&
                       document.querySelector('input[name=indlogin]') &&
                       document.querySelector('input[type=reset]');
            },
            action: function() {
                if(__juspay.isMobUiEnabled("MOBUI_IOBNB")) {
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        document.querySelectorAll('td')[6].removeAttribute("style");
                        __juspay.removeElements([document.querySelectorAll('td')[8],document.querySelectorAll('td')[21]]);
                        __juspay.modifyUI({"add_break_before":[document.querySelector("input[type=password][name=password]").parentElement.parentElement]})
                        window.trackEventv2_0default("acs","info","MOBUI","LOGIN_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                    }
                }

                var loginid = document.querySelector('input[name=loginId]');
                var submitBtn = document.querySelector('input[name=indlogin]');
                var resetBtn = document.querySelector('input[type=reset]');
                setupLoginPageWithSubmitButton("input[type=password][name=password]",
                                "input[name=loginId]",
                                "input[name=indlogin]");
                submitBtn.addEventListener("click",function() {
                    var bankCustomerId = loginid.value;
                    if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined") {
                        bankCustomerId = "";
                    }
                    Gatekeeper.onCustomerIdSubmit(bankCustomerId);
                },false);
                resetBtn.addEventListener("click",function() {
                    loginid.focus();
                },false);
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector("input[type=submit][name=indlogin]");
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            },
            nextAction: function() {
                var usernameTextBox=document.querySelector('input[type=text][name=loginId]'); //Login page
                var submitButtonLoginPage = document.querySelector('input[name=indlogin]');         //Submit Button on first button
                var passwordField = document.querySelector('input[name=password][type=password]');
                if((document.activeElement == usernameTextBox) && passwordField){
                    passwordField.blur();
                    passwordField.focus();
                    Gatekeeper.requestPasswordKeyboardShow();
                }else if((document.activeElement == passwordField) && submitButtonLoginPage){
                     __juspay.clickOnElement(submitButtonLoginPage);
                }else if(usernameTextBox.value ==""){
                     usernameTextBox.blur();
                     usernameTextBox.focus();
                }else if(passwordField.value ==""){
                     passwordField.blur();
                     passwordField.focus();
                }else if(submitButtonLoginPage){
                    __juspay.clickOnElement(submitButtonLoginPage);
                }
            },
            backButtonPressed: function() {
                try {
                    if(__uber.isUberEnabled("IOBNB_RESET_CREDENTIALS") && __uber.initSessionCounterIncremental("uberShownIOBNBBP")==1) {
                        window.trackEventv2_0("acs","info","IOBNB_RESET_CREDENTIALS","uber_shown", "IOBNB", "reset_credentials");
                        GK.setUberEventWithCallback("IOBNB_RESET_CREDENTIALS", function(result) {
                            if( result == "ok" ) {
                                window.trackEventv2_0("acs","info","IOBNB_RESET_CREDENTIALS","reset_pwd_clicked", "IOBNB", "reset_credentials");
                                window.location.href = "https://"+ window.location.hostname + "/ibanking/resetpasswordpage.do?I=I"
                            } else if(result == "closed") {
                                window.trackEventv2_0("acs","info","IOBNB_RESET_CREDENTIALS","close_clicked", "IOBNB", "reset_credentials");
                            } else if(result == "cancel") {
                                window.trackEventv2_0("acs","info","IOBNB_RESET_CREDENTIALS","cancel_clicked", "IOBNB", "reset_credentials");
                                Gatekeeper.dismissUber();
                             }
                        },{type:"dialog",showOnLoad:true,height:320,width:-2},null);
                    } else {
                        GK.showCancelTransactionDialog();
                    }
                } catch(err) {
                    GK.showCancelTransactionDialog();
                }
            }
        },
        { //IOB_NB: reset password details page
            path:/ibanking\/resetpasswordpage\.do\?I\=I/,
            hostname: "www.iobnet.co.in",
            state: "UNUSED",
            bank: "IOBNB",
            local: false,
            domCheck: function() {
                return  document.querySelector("input[type=text][name=loginid]") && document.querySelector("input[type=text][name=accountNumber]") && document.querySelector("input[type=text][name=emailid]") ;
            },
            action: function() {
                __juspay.trackUnblocker("DETAILS_PAGE");
                Gatekeeper.showNetbankingDefaultFragment();
                var loginid = document.querySelector("input[type=text][name=loginid]") ;
                var accountNumber = document.querySelector("input[name=accountNumber]");
                var emailid = document.querySelector("input[type=text][name=emailid]");
                var submitBtn = document.querySelector("input[type=button][name=subm]");
                if(accountNumber){
                    numberTextFieldQuery("input[name=accountNumber]");
                }
                attachPhoneKeyboard(accountNumber);
                var element = document.querySelector('td[class="txterror"][align="center"]');
                if(element && ((element.innerText.indexOf("has already been sent"))>-1)) {
                    __juspay.trackUnblocker("PASSWORD_ALREADY_RESET");
                }
                var bankCustomerId = Gatekeeper.getBankCustomerId();
                if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined") {
                    bankCustomerId = "";
                }
                loginid.value = bankCustomerId;
                if(loginid.value.length==0) {
                    focusElement(loginid);
                    __juspay.scrollToElement(loginid);
                    Gatekeeper.requestPasswordKeyboardShow();
                    Gatekeeper.showNetbankingDefaultFragment();
                }
                loginid.addEventListener("focus",function() {
                    Gatekeeper.changeNextActionText("Continue");
                },false);
                accountNumber.addEventListener("focus",function() {
                    Gatekeeper.changeNextActionText("Continue");
                },false);
                emailid.addEventListener("focus",function() {
                    Gatekeeper.changeNextActionText("Submit");
                },false);
                submitBtn.addEventListener("click",function() {
                    window.trackEventv2_0("acs","info","IOBNB_RESET_PASSWORD","submit_clicked", "IOBNB", "reset_password");
                },false);

            },
            nextAction: function() {
                var loginid = document.querySelector("input[type=text][name=loginid]") ;
                var accountNumber = document.querySelector("input[type=text][name=accountNumber]");
                var emailid = document.querySelector("input[type=text][name=emailid]");
                var submitBtn = document.querySelector("input[type=button][name=subm]") ;
                if(document.activeElement==loginid) {
                    accountNumber.focus();
                } else if (document.activeElement==accountNumber) {
                    emailid.focus();
                } else if (document.activeElement==emailid) {
                    if(accountNumber.value.length!=15) {
                        accountNumber.focus();
                    } else {
                        __juspay.clickOnElement(submitBtn);
                        window.trackEventv2_0("acs","info","IOBNB_RESET_PASSWORD","submit_clicked", "IOBNB", "reset_password");
                    }
                }
            }
        },
        { //IOB_NB: Reset password otp page
            path:/ibanking\/resetpassword\.do/,
            hostname: "www.iobnet.co.in",
            state: "UNUSED",
            bank: "IOBNB",
            local: false,
            domCheck: function() {
                return  document.querySelector("input[type=button][name=ftpasswdbutton]") &&  document.querySelector("input[type=password][name=otp]");
            },
            action:function() {
                __juspay.trackUnblocker("INPUT_OTP_PAGE");
                otpFragmentHelper('input[name=otp]',"");
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp,"input[name=otp]","input[type=button][name=ftpasswdbutton]");
                window.trackEventv2_0("acs","info","IOBNB_RESET_PASSWORD","otp_auto_detect", "IOBNB", "reset_password");
            }
        },
        { //IOB_NB: reset password successful page
            path:/ibanking\/resetpassword\.do/,
            hostname: "www.iobnet.co.in",
            state: "UNUSED",
            bank: "IOBNB",
            local: false,
            domCheck: function() {
                var element = document.querySelector('td[class="heading"][align="center"]');
                return (element && (element.innerText.indexOf("Password Generated successfully")>-1));
            },
            action:function(){
                __juspay.trackUnblocker("RESET_PASSWORD_SUCCESSFUL");
            }
        },
        { //IOB_NB: txnpwd page
            path:/ibanking\/loginsubmit\.do/,
            hostname: "www.iobnet.co.in",
            state: "UNUSED",
            bank: "IOBNB",
            local: false,
            domCheck: function() {
                return  document.querySelector("input[type=password][name=txnPasswd]") && document.querySelector("select[name=debitAc]");
                },
                //Choosing account used in prev transaction when there are multiple accounts
            action: function(){
                Gatekeeper.removeFragment("Reached transaction page of IOBNB");
//                accountSelectionPage('option',"IOBNB",'','');
                var pinField = document.querySelector('input[name=txnPasswd]');
                if(pinField){
                    passwordFragmentHelper('input[name=txnPasswd]');
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[name=subm]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        //IOBNB_otp page
        {
            path: /ibanking\/tppsubmit\.do/,
            hostname: "www.iobnet.co.in",
            state: "UNUSED",
            bank: "IOBNB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[name=subm][type=button]') && document.querySelector('input[name=txnOtp][type=password]');
            },
            action: function(){
                otpFragmentHelper('input[name=txnOtp]',"");
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[name=txnOtp]", "input[name=subm][type=button]");
            }
        },
        { //OBC_DC: OTP page
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "OBCDC",
            domCheck: function() {
                return document.querySelector("input[name=otp]") && document.querySelector("#sendotp") && document.querySelector("img[title='OBC Bank']");
            },
            action: function() {
                otpFragmentHelper('input[name=otp]','a[title=Resend\\ OTP]');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[name=otp]", "#sendotp");
            },
            regenerateOtp: function(){
                if(typeof(reSend) === "function"){
                    reSend();
                }
            }
        },
        //TMB_DC
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "TMBDC",
            domCheck: function(){
                return  document.querySelector('input[name = "pin"][type = "password"]') &&
                        document.querySelector('input[type=submit][value=Submit]') &&
                        document.querySelector('img[alt="Tamilnad Mercantile Bank"]')
            },
            action: function(){
                passwordFragmentHelper('input[name = pin]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //TMB: NB login page
        {
            path: /corp\/AuthenticationController/,
            hostname: "www.tmbnet.in",
            state: "UNUSED",
            local: false,
            bank: "TMBNB",
            domCheck: function(){
                return document.querySelector('input[id="VALIDATE_CREDENTIALS"]') &&
                        document.querySelector('input[id="AuthenticationFG.ACCESS_CODE"]')&&
                        document.querySelector('input[id="AuthenticationFG.USER_PRINCIPAL"]');
            },
            action: function(){
                var userIdField = document.querySelector('input[id="AuthenticationFG.USER_PRINCIPAL"]');
                var passwordField = document.querySelector('input[id="AuthenticationFG.ACCESS_CODE"]');
                var submitBtn = document.querySelector('input[id="VALIDATE_CREDENTIALS"]');
                setupLoginPageWithSubmitButton('input[id="AuthenticationFG.ACCESS_CODE"]', 'input[id="AuthenticationFG.USER_PRINCIPAL"]', 'input[id="VALIDATE_CREDENTIALS"]');
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
            },
            nextAction: function(){
                var passwordField = document.querySelector('input[id="AuthenticationFG.ACCESS_CODE"]');
                passwordField.focus();
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[id="VALIDATE_CREDENTIALS"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        {   //PNB_NB: login page
            path: /corp\/(AuthenticationController|Finacle)/,
            hostname: "netbanking.netpnb.com",
            state: "UNUSED",
            local: false,
            bank: "PNBNB",
            domCheck: function(){
                return document.querySelector("input[type=text][name='AuthenticationFG.USER_PRINCIPAL']") ||
                    document.querySelector("input[type=password][name='AuthenticationFG.ACCESS_CODE']") ||
                    document.querySelector("input[type=text][name='TranRequestManagerFG.ENT_REMARKS']") ||
                    document.querySelector("input[type=password][name='TranRequestManagerFG.TRANSACTION_PASSWORD__']") ||
                    document.querySelector("input[type=password][name='TranRequestManagerFG.OTP']") ||
                    document.querySelector("input[id='AuthenticationFG.OTP'][name='AuthenticationFG.OTP']");
            },
            action: function() {
                Gatekeeper.setSessionAttribute("disableAutoFocus", "true"); showUpdateWebviewUber();

                var userIdField = document.querySelector("input[type=text][name='AuthenticationFG.USER_PRINCIPAL']");
                var continueBtn = document.querySelector("input[type=Submit][name='Action.STU_VALIDATE_CREDENTIALS']");
                var passField = document.querySelector("input[type=password][name='AuthenticationFG.ACCESS_CODE']");
                var loginBtn = document.querySelector("input[type=Submit][name='Action.VALIDATE_STU_CREDENTIALS']");
                var isSelectPresent = document.querySelector("select[name='TranRequestManagerFG.INITOR_ACCOUNT']");
                var remarksField = document.querySelector("input[type=text][name='TranRequestManagerFG.ENT_REMARKS']")
                var chooseCtnBtn = document.querySelector("input[type=Submit][name='Action.CONTINUE_TRANSACTION']");
                var txnPassField = document.querySelector("input[type=password][name='TranRequestManagerFG.TRANSACTION_PASSWORD__']");
                var otpField = document.querySelector("input[type=password][name='TranRequestManagerFG.OTP']");
                var resendOtpOncall = document.querySelector("input[id='CUSTOM_RESEND_OTP'][name='Action.CUSTOM_RESEND_OTP']");
                var submitBtn = document.querySelector("input[type=Submit][name='Action.SUBMIT_TRANSACTION']");
                var onlyOtpOption = document.querySelector("input[id='AuthenticationFG.OTP'][name='AuthenticationFG.OTP']");
                var onlyOtpContinue = document.querySelector("input[id='AUTHENTICATE_USER'][name='Action.AUTHENTICATE_USER']");
                setupLoginPageWithUsernameAndSubmitButton('input[type=text][name=AuthenticationFG\\.USER_PRINCIPAL]',
                                                          "input[type=Submit][name='Action.STU_VALIDATE_CREDENTIALS']");
                if(passField && loginBtn) {
                    __juspay.trackPageStatus("INPUT_LOGIN_PASSWORD");
                    __juspay.trackAuthMethod("PASSWORD");
                    passwordFragmentHelper("input[type=password][name='AuthenticationFG.ACCESS_CODE']");
                    __juspay.scrollToElement(passField);
                }
                if(txnPassField && submitBtn && otpField) {
                    __juspay.trackPageStatus("TXN_PASSWORD_PAGE");
                    __juspay.trackAuthMethod("TXN_PASSWORD");
                    var clickCheck = Gatekeeper.getSessionAttribute("otp_onCall_button_click");
                    var wrongOtpCheck = Gatekeeper.getSessionAttribute("wrong_otp/password_entered");
                    if (clickCheck=="true"||wrongOtpCheck=="true"){
                        passwordFragmentHelper("input[type=password][name='TranRequestManagerFG.TRANSACTION_PASSWORD__']");
                        otpField.addEventListener("focus", function(){
                            Gatekeeper.removeFragment("selected on call otp");
                        },false);
                    }
                    if (clickCheck!="true"){
                        passwordFragmentHelper("input[type=password][name='TranRequestManagerFG.TRANSACTION_PASSWORD__']");
                        otpField.addEventListener("click", function(){
                            __juspay.showOtpFragmentWrapper(otpField);
                            Gatekeeper.requestNumericKeyboardShow();
                            __juspay.scrollToElement(otpField);
                            Gatekeeper.hideAssistanceFragment();
                        },false);
                        otpField.addEventListener("focus", function(){
                            __juspay.showOtpFragmentWrapper(otpField);
                            __juspay.scrollToElement(otpField);
                        },false);
                        resendOtpOncall.addEventListener("click",function(){
                            Gatekeeper.removeFragment("selected on call otp");
                            Gatekeeper.setSessionAttribute("otp_onCall_button_click", "true");
                        },false);
                    }
                }
                if(onlyOtpOption && onlyOtpContinue){
                    otpFragmentHelper("input[id='AuthenticationFG.OTP'][name='AuthenticationFG.OTP']")
                }
                if(__juspay.isMobUiEnabled("MOBUI_PNBNB")) {
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);
                        var resizedLogo = "https://"+ window.location.hostname + "/corp/L001/consumer/images/branding-logo.png";
                        var err = document.querySelector('div[id="MessageDisplay_TABLE"]');
                        if(err) err.style.width = window.innerWidth.toString()+"px";
                        if((userIdField && continueBtn) || (passField && loginBtn)) {
                            var yellow = document.querySelector('div[id="YellowBarlgn-PNB"]');
                            var body = document.querySelector('body');
                            var imgdiv = document.querySelector('div[id="global"]').insertBefore(document.createElement('div'),document.querySelector('div[id="topbar"]'));
                            var img = imgdiv.appendChild(document.createElement('img'));
                            var footer = document.querySelector('div[id="footerPnb"]');
                            var vkeyboard = document.querySelector('img[id="virtual_keyboard_password"]');
                            var vkeyboardHelp = document.querySelector('span[id="span_Caption17691192"]');
                            var adImg = document.querySelector('img[id="Image31371927"]');
                            var divForRetryTrans = document.querySelector('span[id="HDisplay1.Rb5.C1"]');
                            var mainDiv = document.querySelector('div.login_user_container');
                            var userLabel = document.querySelector('label[id=UserId_Text]');

                            __juspay.removeElements([vkeyboard, vkeyboardHelp, footer, document.querySelector('div[id="HDisplay1.Rowset5"]')]);
                            document.querySelector('div[id="notespPnb"]').innerHTML = document.querySelector('div[id="notePnb1"]').innerHTML;
                            document.querySelector('div[id="HDisplay1"]').style.padding = "0px";
                            document.querySelector('div[id="notespPnb"]').style.color = "white";
                            document.querySelector('div[id="notespPnb"]').align="center";
                            document.querySelector('div[id="notespPnb"]').style.width=window.innerWidth.toString()+"px";
                            document.querySelectorAll('div[id="header"]')[0].parentElement.removeChild(document.querySelectorAll('div[id="header"]')[0]);
                            img.src = resizedLogo;
                            document.querySelector('div[id="global"]').insertBefore(yellow,document.querySelector('div[id="topbar"]'));
                            document.querySelector('div[id="notespPnb"]').style.margin="0 auto";
                            yellow.style.width=window.innerWidth.toString()+"px";
                            imgdiv.style.width=window.innerWidth.toString()+"px";
                            img.style.width=window.innerWidth.toString()+"px";
                            document.querySelector('div[id="notespPnb"]').parentElement.style.width=window.innerWidth.toString()+"px";
                            document.querySelector('div[id="notespPnb"]').parentElement.parentElement.style.width=window.innerWidth.toString()+"px";
                            yellow.style.margin="0";
                            var div = document.querySelector('div[id="HDisplay1.Ra1.C1"]');
                            var i=0;
                            while(i<6) {
                                div = div.parentElement; div.style.width=window.innerWidth.toString()+"px"; i++;
                            }
                            img.style.width=window.innerWidth.toString()+"px";
                            if(adImg) {
                                divForRetryTrans.appendChild(continueBtn);
                                __juspay.removeElements([adImg]);
                            }
                            if(mainDiv){
                                mainDiv.style.position="relative";
                                mainDiv.style.left="15px";
                            }
                            if(userIdField && continueBtn){
                                userIdField.style.position=continueBtn.style.position="relative";
                                userIdField.style.top="35px";continueBtn.style.top="25px";
                                userIdField.style.left= "-35px";
                                continueBtn.style.left="-95px";
                            }
                            if(userLabel){
                                userLabel.style.position="relative";
                                userLabel.style.left="50px";
                            }
                        } else if(txnPassField && submitBtn) {
                            txnPassField.style.width = "160px";
                            txnPassField.parentElement.style.width = "160px";
                            document.querySelector('body').style.height=window.innerHeight.toString()+"px" ;
                            var transactionDetails = document.querySelectorAll('span[class="querytextleft"]');
                            var vkeyboardDiv = document.querySelectorAll('span[class="querytextright"]');
                            var extraDiv = document.querySelector('div[id="topbar"]');
                            var submitBtn = document.querySelector('div[id="NavPanel"]');
                            var footer = document.querySelector('div[id="footerPnb"]');
                            var bigLogo = document.getElementById("wrapper1-PNB");
                            var imgLogo = document.createElement("img");
                            var extraDiv2 = document.getElementById("topbar");
                            var screenWidth = window.innerWidth.toString();
                            var backButton =document.querySelector('span[id="NavPanel.Ra1.C2"]');

                            bigLogo.removeChild(document.querySelector('div[id="headerouter"]'));
                            document.querySelector('div[id="global"]').style.minWidth = screenWidth+"px";
                            submitBtn.style.float = "left";
                            submitBtn.style.position = "relative";
                            submitBtn.style.left = "16%";
                            __juspay.removeElements([footer, topbar, vkeyboardDiv[vkeyboardDiv.length-1].childNodes[1], backButton]);
                            imgLogo.src = resizedLogo;
                            imgLogo.style.width = screenWidth+"px";
                            bigLogo.appendChild(imgLogo);
                            for(var i=0;i<transactionDetails.length;i++) {
                                var transDetail = document.querySelectorAll('span[class="querytextleft"]')[i];
                                transDetail.style.padding = "7px 1px 1px 10px";
                                if(i < transactionDetails.length-1)
                                    transDetail.style.width = "21%";
                                else
                                    transDetail.style.width = "14%";
                            }
                        } else if(isSelectPresent) {
                            var mainContent = document.querySelector('p[id="NavPanel1.Ra1"]');
                            var transactionDetails = document.querySelectorAll('span[class="querytextleft"]');
                            var submit = document.querySelector("input[type=Submit][id=CONTINUE_TRANSACTION]");
                            if(mainContent) {
                                mainContent.style.width = "60%";
                            }
                            submit.style.float = "left";
                            submit.style.position = "relative";
                            submit.style.left = "65%";
                            for(var i=0;i<transactionDetails.length;i++) {
                                var transDetail = document.querySelectorAll('span[class="querytextleft"]')[i];
                                transDetail.style.padding = "7px 1px 1px 10px";
                                transDetail.style.width = "9%";
                            }
                            document.querySelector('div[id=logo-PNB]').style.width=window.innerWidth+"px";
                            document.querySelector('div[id=header]').style.width=window.innerWidth+"px";
                            var section= document.querySelectorAll('div.section');
                            for(var i=0;i<section.length;i++){
                                section[i].style.width=window.innerWidth+"px";
                            }
                            __juspay.removeElements([document.querySelector('div[id=footerPnb]')]);
                        }
                        var formToScroll = document.querySelector("form[name=AuthenticationFG]");
                        var formToScroll2 = document.querySelector("form[name=TranRequestManagerFG]");
                        if(formToScroll) {
                            formToScroll.setAttribute("style","overflow:hidden");
                        } else if(formToScroll2) {
                            formToScroll2.setAttribute("style","overflow:hidden");
                        }
                        window.trackEventv2_0default("acs","info","MOBUI","LOGIN_PAGE");
                    } catch(err){
                        __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                    }
                }
                if(isSelectPresent) {
                    accountSelectionPage('select[title=Account] option',"PNBNB","input[type=Submit][name='Action.CONTINUE_TRANSACTION']",remarksField)
                }
            },
            nextAction: function() {
                var userIdField = document.querySelector("input[type=text][name='AuthenticationFG.USER_PRINCIPAL']");
                var continueBtn = document.querySelector("input[type=Submit][name='Action.STU_VALIDATE_CREDENTIALS']");
                var remarksField = document.querySelector("input[type=text][name='TranRequestManagerFG.ENT_REMARKS']")
                var chooseCtnBtn = document.querySelector("input[type=Submit][name='Action.CONTINUE_TRANSACTION']");
                if(continueBtn && userIdField.value ==""){
                    userIdField.blur();
                    userIdField.focus();
                } else if(userIdField && continueBtn){
                    __juspay.clickOnElement(continueBtn);
                    Gatekeeper.showNetbankingDefaultFragment();
                    Gatekeeper.requestKeyboardHide();
                } else if(remarksField && chooseCtnBtn){
                    __juspay.clickOnElement(chooseCtnBtn);
                }
            },
            clickSubmitButton: function(){
                var passField = document.querySelector("input[type=password][name='AuthenticationFG.ACCESS_CODE']");
                var loginBtn = document.querySelector("input[type=Submit][name='Action.VALIDATE_STU_CREDENTIALS']");
                var txnPassField = document.querySelector("input[type=password][name='TranRequestManagerFG.TRANSACTION_PASSWORD__']");
                var submitBtn = document.querySelector("input[type=Submit][name='Action.SUBMIT_TRANSACTION']");
                var otpField = document.querySelector("input[type=password][name='TranRequestManagerFG.OTP']");

                if(loginBtn && passField.value!=""){
                    __juspay.clickOnElement(loginBtn);
                }
                if(loginBtn && passField.value =="") {
                    passField.blur();
                    passField.focus();
                }
                if(txnPassField && otpField) {
                    __juspay.scrollToElement(otpField);
                    otpField.focus();
                }
            },
            submitOtp: function(otp) {
                var otpField = document.querySelector("input[type=password][name='TranRequestManagerFG.OTP']");
                var submitBtn = document.querySelector("input[type=Submit][name='Action.SUBMIT_TRANSACTION']");
                var txnPassField = document.querySelector("input[type=password][name='TranRequestManagerFG.TRANSACTION_PASSWORD__']");
                var onlyOtpOption = document.querySelector("input[id='AuthenticationFG.OTP'][name='AuthenticationFG.OTP']");
                var onlyOtpContinue = document.querySelector("input[id='AUTHENTICATE_USER'][name='Action.AUTHENTICATE_USER']");
                if(otpField && submitBtn && txnPassField.value!=""){
                    Gatekeeper.setSessionAttribute("wrong_otp/password_entered", "true");
                    submitOtpHelper(otp,"input[type=password][name='TranRequestManagerFG.OTP']", "input[type=Submit][name='Action.SUBMIT_TRANSACTION']");
                    Gatekeeper.removeFragment("otp submitted from juspay fragment");
                }
                if(otpField && submitBtn && txnPassField.value==""){
                    txnPassField.blur();
                    txnPassField.focus();
                    otpField.addEventListener("focus", function(){
                        Gatekeeper.removeFragment("otp submitted from fragment once");
                    },false);
                }
                if(onlyOtpOption && onlyOtpContinue){
                    submitOtpHelper(otp,"input[id='AuthenticationFG.OTP'][name='AuthenticationFG.OTP']", "input[id='AUTHENTICATE_USER'][name='Action.AUTHENTICATE_USER']");
                    Gatekeeper.removeFragment("otp submitted from juspay fragment");
                }
            },
            backButtonPressed:function(){
                var userIdField = document.querySelector("input[type=text][name='AuthenticationFG.USER_PRINCIPAL']");
                var continueBtn = document.querySelector("input[type=Submit][name='Action.STU_VALIDATE_CREDENTIALS']");
                var passField = document.querySelector("input[type=password][name='AuthenticationFG.ACCESS_CODE']");
                var loginBtn = document.querySelector("input[type=Submit][name='Action.VALIDATE_STU_CREDENTIALS']");

                if((userIdField && continueBtn || passField && loginBtn) && __uber.isUberEnabled("REDIRECT_DEBIT") && __uber.initSessionCounterIncremental("PNBNBDCR")==1) {
                    callDebitRedirectUber();
                } else {
                    GK.showCancelTransactionDialog();
                }
            }
        },
        {  //IND_NB: login page
            path: /servlet\/ibs\.servlets\.(IBSGenericPaymentServlet|IBSAccountSummaryServlet)/,
            hostname: "www.indianbank.net.in",
            state: "UNUSED",
            local: false,
            bank: "INDNB",
            domCheck: function(){
                return document.querySelector("input[type='text'][name='uid']") ||
                    document.querySelector("input[type='password'][name='password']") ||
                    document.querySelector("input[type='password'][name='otp']") ||
                    document.querySelector("input[type='button'][name='LoginButton'][value='I  Agree']");
            },
            action: function() {
                var autoclick = document.querySelector("input[type='button'][name='LoginButton'][value='I  Agree']");
                if(autoclick) {
                    window.trackEventv2_0default("acs","info","DISCLAIMER_PAGE","auto_clicked_agree");
                    __juspay.clickOnElement(autoclick);
                }
                var loginIdField = document.querySelector("input[type='text'][name='uid']");
                var passField = document.querySelector("input[type='password'][name='pwd']");
                var loginSubmit = document.querySelector("input[type='submit'][name='btClear']");
                var txnPassField = document.querySelector("input[type='password'][name='password']");
                var txnPassSubmit = document.querySelector("a[href*=submitRequest]");
                var otpField = document.querySelector("input[type='password'][name='otp']");
                var otpSubmit = document.querySelector("a[href*=submitRequest]")
                var resendOtp = document.querySelector('a[href="javascript:resend()"]');
                var error = document.getElementById("aftersubmitError");
                if(loginIdField && passField){
                    setupLoginPageWithSubmitButton("input[type='password'][name='pwd']","input[type='text'][name='uid']","input[type='submit'][name='btClear']");
                    __juspay.trackPageStatus("INPUT_NB_LOGIN");
                    __juspay.trackAuthMethod("LOGIN");
                    if(__juspay.isMobUiEnabled("MOBUI_INDNB")) {
                        try{
                            var metaTag=document.createElement('meta');
                            metaTag.name = "viewport"
                            metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                            document.getElementsByTagName('head')[0].appendChild(metaTag);
                            var error = document.getElementById("aftersubmitError");
                            var rem=document.querySelectorAll('img')[1].parentElement.parentElement.parentElement.parentElement.childNodes[2];
                            var instruction = document.getElementsByClassName("mobile");
                            var norton = document.getElementsByClassName("IdrbtTable");
                            document.querySelectorAll('img')[1].parentElement.removeAttribute("href");
                            norton[0].align="center";
                            if(!error){
                                var rem_array = [rem,document.querySelectorAll('table')[3],document.querySelectorAll('table')[5],instruction[0].parentElement];
                                __juspay.removeElements(rem_array);
                            } else if(error){
                                __juspay.removeElements([rem,document.querySelectorAll('table')[4],document.querySelectorAll('table')[6],instruction[0].parentElement]);
                            }
                            window.trackEventv2_0default("acs","info","MOBUI","LOGIN_PAGE");
                        } catch(err) {
                            __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                        }
                    }
                    loginSubmit.addEventListener("click",function(){
                        Gatekeeper.removeFragment("submitBtn Clicked");
                    });
                }
                if(txnPassField && txnPassSubmit) {
                    passwordFragmentHelper("input[type='password'][name='password']");
                }
                if(otpField && otpSubmit) {
                    if(error && error.innerText.indexOf("Incorrect One Time Password")>-1){
                        Gatekeeper.removeFragment("Incorrect OTP");
                    } else {
                        otpFragmentHelper('input[type=password][name=otp]','a[href="javascript:resend()"]');
                    }
                }
            },
            nextAction: function(){
                var loginIdField = document.querySelector("input[type='text'][name='uid']");
                var passField = document.querySelector("input[type='password'][name='pwd']");
                if(document.activeElement == loginIdField && passField){
                    passField.blur();
                    passField.focus();
                    Gatekeeper.requestPasswordKeyboardShow();
                }
            },
            clickSubmitButton: function(){
                var passField = document.querySelector("input[type='password'][name='pwd']");
                var loginSubmit = document.querySelector("input[type='submit'][name='btClear']");
                var txnPassField = document.querySelector("input[type='password'][name='password']");
                var txnPassSubmit = document.querySelector("a[href='javascript:submitRequest();']");
                if(passField && loginSubmit ){
                    __juspay.clickOnElement(loginSubmit);
                }
                if(txnPassField && txnPassSubmit) {
                    __juspay.clickOnElement(txnPassSubmit);
                }
            },
            submitOtp: function(otp) {
                var otpField = document.querySelector("input[name='otp']");
                var otpSubmit = document.querySelector("a[href='javascript:submitRequest();']");
                if(otpField && otpSubmit){
                    submitOtpHelper(otp,"input[name='otp']", "a[href='javascript:submitRequest();']");
                }
            },
            regenerateOtp: function(){
                var resendOtp = document.querySelector('a[href="javascript:resend()"]');
                if(resendOtp) {
                    __juspay.clickOnElement(resendOtp);
                }
            }
        },
        { // INDNB Resend OTP Page
            path: /servlet\/ibs\.servlets\.IBSMFAServlet/,
            hostname: "www.indianbank.net.in",
            state: "UNUSED",
            local: false,
            bank: "INDNB",
            domCheck: function(){
                return document.querySelector("input[type='password'][name='otp']") &&
                        document.querySelector("a[href*=submitRequest]");
            },
            action: function(){
                var otpField = document.querySelector("input[type='password'][name='otp']");
                var otpSubmit = document.querySelector("a[href*=submitRequest]")
                var resendOtp = document.querySelector('a[href="javascript:resend()"]');
                var error = document.getElementById("aftersubmitError");
                if(otpField && otpSubmit) {
                    if(error && error.innerText.indexOf("Incorrect One Time Password")>-1){
                        Gatekeeper.removeFragment("Incorrect OTP");
                    } else {
                        otpFragmentHelper('input[type=password][name=otp]','a[href="javascript:resend()"]');
                    }
                }
            },
            submitOtp: function(otp) {
                var otpField = document.querySelector("input[name='otp']");
                var otpSubmit = document.querySelector("a[href='javascript:submitRequest();']");
                if(otpField && otpSubmit){
                    submitOtpHelper(otp,"input[name='otp']", "a[href='javascript:submitRequest();']");
                }
            },
            regenerateOtp: function(){
                var resendOtp = document.querySelector('a[href="javascript:resend()"]');
                if(resendOtp) {
                    __juspay.clickOnElement(resendOtp);
                }
            }
        },
        {// ALLAHABAD BANK NB LOGIN-PAGE
           path: /\/servlet\/ibs\.servlets\.(IBSThirdPartyPaymentServlet|IBSLoginServlet)/,
           hostname: "www.allbankonline.in",
           state: "UNUSED",
           local: false,
           bank: "ALLBNB",
           domCheck: function(){
               return document.querySelector('input[type=text][name=uid]') &&
                      document.querySelector('input[type=password][name=pwd]') &&
                      document.querySelector('input[type=submit][value=Login]');
           },
           action: function() {
               var loginBtn = document.querySelector('input[type=submit][value=Login]');
               var styleMediaQuery = document.querySelector('style');
               var allahabadImg = document.querySelector('img[src*="banner"]');
               if(__juspay.isMobUiEnabled("MOBUI_ALLBNB")) {
                   loginBtn.style.height = "50%";
                   loginBtn.style.marginTop = "28px";
                   loginBtn.style.marginBottom = "13px";
                   loginBtn.style.width = "75%";
                   loginBtn.style.marginLeft = "25px";
                   loginBtn.style.cursor = "pointer";
                   loginBtn.style.borderRadius = "0.65em";
                   loginBtn.style.color = "white";
                   loginBtn.style.backgroundColor = "#333E84";
                   loginBtn.style.fontWeight = "bold";
                   loginBtn.style.border = "2px solid #333E84";
                   styleMediaQuery.textContent="@media screen and (max-device-width)";
                   allahabadImg.style.width=360;
               }
               __juspay.trackPageStatus("INPUT_NB_LOGIN");
               __juspay.trackAuthMethod("LOGIN");
               setupLoginPageWithSubmitButton("input[type=password][name=pwd]","input[type=text][name=uid]","input[type=submit][value=Login]");
           },
           nextAction: function() {
               var passwordField = document.querySelector('input[type=password][name=pwd]');
               passwordField.focus();
           },
           clickSubmitButton: function() {
               var submitBtn = document.querySelector("input[type=Submit][value=Login]");
               __juspay.clickOnElement(submitBtn);
           }
        },
        {// ALLAHABAD BANK ON TRANSACTION-PASSWORD-PAGE
           path: /\/servlet\/ibs\.servlets\.(IBSAccountSummaryServlet|IBSThirdPartyPaymentServlet)/,
           hostname: "www.allbankonline.in",
           state: "UNUSED",
           local: false,
           bank: "ALLBNB",
           domCheck: function() {
               return document.querySelector('input[type=password][name=password]') &&
                      document.querySelector('button[name="subm"][type="button"]') ;
           },
           action: function() {
               var script = document.querySelector('title').parentElement.children[3];
               var headingText = document.querySelector('h2');
               var allahabadImg  = document.querySelector('img[src*="banner"]');
               var pageElements = document.querySelectorAll('.col-sm-12');
               var elementInputs = document.querySelectorAll('.form-control');
               var submitBtnText = document.querySelector('button[onclick*="submit"]>font');
               var submitBtn = document.querySelector('button[onclick*="submit"]');
               var cancelBtn = document.querySelector('button[onclick*="reject"]');
               var clearBtn = document.querySelector('button[onclick*="reset"]');
               var loginPageLayout = document.querySelector('div[class="well"]');
               var accountOption = document.querySelector('select[name="fromAcc"]');
               if(__juspay.isMobUiEnabled("MOBUI_ALLBNB")) {
                   if(headingText.textContent.includes("Allahabad")) {
                       headingText.style.fontSize = "20px";
                       script.removeAttribute('href');
                       allahabadImg.width = 360;
                       if(pageElements[0].textContent.includes("Merchant Code") && pageElements[1].textContent.includes("Merchant Name") &&  pageElements[2].textContent.includes("Reference") && pageElements[3].textContent.includes("Amount")&&  pageElements[4].textContent.includes("Debit")&& pageElements[5].textContent.includes("Transaction")) {
                            __juspay.modifyUI({"font_size":{"other":[12,pageElements[0],pageElements[1],pageElements[2],pageElements[3],pageElements[4],pageElements[5]]}});
                       }
                   elementInputs[0].style.marginBottom ="-38px";
                   elementInputs[1].style.marginBottom ="-38px";
                   elementInputs[2].style.marginBottom ="-38px";
                   elementInputs[3].style.marginBottom ="-38px";
                   elementInputs[4].style.marginBottom ="-38px";
                   elementInputs[5].style.marginBottom ="-38px";
                   elementInputs[4].style.fontSize = "16px" ;
                   elementInputs[1].style.textAlign = "center" ;
                   elementInputs[3].style.textAlign = "center";
                   elementInputs[0].style.height ="45px";
                   elementInputs[1].style.height ="45px";
                   elementInputs[2].style.height ="45px";
                   elementInputs[3].style.height ="45px";
                   elementInputs[4].style.height ="45px";
                   elementInputs[5].style.height ="45px";
                   __juspay.modifyUI({"font_size":{"other":[24,elementInputs[0],elementInputs[1],elementInputs[2],elementInputs[3],elementInputs[4],elementInputs[5]]},"style_width":{"other":[0.8,elementInputs[0],elementInputs[1],elementInputs[2],elementInputs[3],elementInputs[4],elementInputs[5]]}});
                   submitBtnText.style.fontSize = "16px";
                   submitBtn.style.marginTop = "40px";
                   submitBtn.style.height = "35px";
                   submitBtn.style.width = "30%";
                   cancelBtn.parentNode.removeChild(cancelBtn);
                   clearBtn.firstElementChild.style.fontSize = "16px";
                   clearBtn.style.marginTop = "40px";
                   clearBtn.style.height = "35px";
                   clearBtn.style.width = "30%";
                   loginPageLayout.style.marginTop = "20px";
                   if(accountOption.length == 2) {
                      accountOption[1].selected = true;
                   } else {
                      accountOption[0].selected = true;
                   }
                   }
               }
               passwordFragmentHelper("input[type=password][name=password]");
           },
           clickSubmitButton: function() {
               var submitBtn = document.querySelector('button[name="subm"][type="button"]');
               __juspay.clickOnElement(submitBtn);
           }
        },
        {// ALLAHABAD BANK ON PRESS-CONFIRM
            path: /\/servlet\/ibs\.servlets\.IBSThirdPartyPaymentServlet/,
            hostname: "www.allbankonline.in",
            state: "UNUSED",
            local: false,
            bank: "ALLBNB",
            domCheck: function(){
                return document.querySelector('button[name="confr"][type="button"]');
            },
            action: function() {
                var allahabadImg = document.querySelector('img[src*="banner"]');
                var pageElements = document.querySelectorAll('.col-sm-6');
                var headingText = document.querySelector('h2');
                var backBtn = document.querySelector('button[onclick*="back"]');
                var confirmButtonText = document.querySelector('[onclick*="submitRequest"]>font');
                var confirmBtn = document.querySelector('button[onclick*="submit"]');
                if(__juspay.isMobUiEnabled("MOBUI_ALLBNB")) {
                    Gatekeeper.removeFragment("Reached Transaction Password Page");
                    if(headingText.textContent.includes("Allahabad")) {
                        allahabadImg.width = 360 ;
                        headingText.style.fontSize = "18px";
                    if(pageElements[0].textContent.includes("Merchant Code") && pageElements[2].textContent.includes("Merchant Name") && pageElements[4].textContent.includes("Reference") && pageElements[6].textContent.includes("Amount")&& pageElements[8].textContent.includes("Debit")) {
                        __juspay.modifyUI({"font_size":{"other":[14,pageElements[0],pageElements[1],pageElements[2],pageElements[3],pageElements[4],pageElements[5],pageElements[6],pageElements[7],pageElements[8],pageElements[9]]}});
                    }
                    backBtn.parentNode.removeChild(backBtn);
                    confirmButtonText.style.fontSize = "16px";
                    confirmBtn.style.marginTop = "-10px";
                    confirmBtn.style.height = "35px";
                    confirmBtn.style.width = "30%" ;
                    }
                }
                Gatekeeper.showNetbankingDefaultFragment();
            },
            nextAction: function() {
                var submitBtn = document.querySelector('button[name="confr"][type="button"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        {// ALLAHABAD BANK ON OTP-PAGE
            path: /\/servlet\/ibs\.servlets\.IBSOTPServlet/,
            hostname: "www.allbankonline.in",
            state: "UNUSED",
            local: false,
            bank: "ALLBNB",
            domCheck: function() {
                return document.querySelector('input[type=password][name=otp]') &&
                       document.querySelector('button[onclick*="SubmitToAuthenticate"]') &&
                       document.querySelector('button[onclick*="SubmitToRegenerate"]');
            },
            action: function() {
                var headingText = document.querySelector('h2');
                var otpText = document.querySelector('.col-sm-12');
                var submitBtn = document.querySelector('button[onclick*="Submit"]');
                var submitBtnText = document.querySelector('button[onclick*="Submit"]>font');
                var otpInput = document.querySelector('input[type=password][name=otp]');
                var logoutBtn = document.querySelector('button[onclick*="logout"]');
                var resendBtn = document.querySelector('button[onclick*="SubmitToRegenerate"]');
                var instructionText = document.querySelector('ul');
                if(__juspay.isMobUiEnabled("MOBUI_ALLBNB")) {
                    if (headingText.textContent.includes("User")){
                        headingText.style.fontSize = "29px";
                        otpText.style.fontSize = "18px";
                        otpInput.style.height = "40px";
                        otpInput.style.fontSize = "38px";
                        submitBtnText .style.fontSize = "15px";
                        submitBtn.style.height = "40px";
                        logoutBtn.parentNode.removeChild(logoutBtn);
                        resendBtn.firstElementChild.style.fontSize = "13px";
                        resendBtn.style.height = "41px";
                        instructionText.style.fontSize = "small";
                    }
                }
                otpFragmentHelper('input[type=password][name=otp]','button[onclick*="SubmitToRegenerate"]');
            },
            submitotp: function(otp){
                submitotpHelper(otp,'input[type=password][name=otp]','button[onclick*="SubmitToAuthenticate"]');
            },
            regenerateotp: function(){
                var regenerateBtn = document.querySelector('button[onclick*="SubmitToRegenerate"]');
                __juspay.clickOnElement(regenerateBtn);
            }
        },
        {// ALLAHABAD BANK ON SESSION-OUT
            path: /\/jsp\/startnew\.jsp/,
            hostname: "www.allbankonline.in",
            state: "UNUSED",
            local: false,
            bank: "ALLBNB",
            domCheck: function() {
                var iframe = frames["indexBottom"].document;
                return iframe.querySelector('input[type=text][name=uid]') &&
                       iframe.querySelector('input[type=password][name=pwd]') &&
                       iframe.querySelector('input[type=submit][value=Login]');
            },
            action: function(){
                Gatekeeper.removeFragment("Reached Session out Page");
                var iframe = frames["indexBottom"].document;
                var loginBtn = iframe.querySelector('input[type=submit][value=Login]');
                var styleMediaQuery = iframe.querySelector('style');
                var allahabadImg = iframe.querySelector('img[src*="banner"]');
                if(__juspay.isMobUiEnabled("MOBUI_ALLBNB")){
                    loginBtn.style.height = "50%"
                    loginBtn.style.marginTop = "28px"
                    loginBtn.style.marginBottom = "13px"
                    loginBtn.style.width = "75%"
                    loginBtn.style.marginLeft = "25px"
                    loginBtn.style.cursor = "pointer";
                    loginBtn.style.borderRadius = "0.65em";
                    loginBtn.style.color = " white";
                    loginBtn.style.backgroundColor = " #333E84";
                    loginBtn.style.fontWeight = "bold";
                    loginBtn.style.border = "2px solid #333E84";
                    styleMediaQuery.textContent="@media screen and (max-device-width)";
                    allahabadImg.style.width=360;
                }
            }
        },
        //TJSB DC Password Page
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "TJSBDC",
            domCheck: function(){
                return document.querySelector('input[type=password][name=pin]') && document.querySelector('input[type=submit][value=Submit]') && /TJSB/.test(document.querySelector('img').src)
            },
            action: function() {
                var errorMsg = document.querySelector('td span');
                if(errorMsg && errorMsg.innerHTML.match(/The password that you have entered is incorrect/) ){
                    __juspay.trackInvalidScenarios("The password that you have entered is incorrect","Invalid password");
                }
                passwordFragmentHelper('input[type=password][name=pin]');
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
               __juspay.clickOnElement(submitBtn);
            }
        },


        //BOM card Password Page
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "BOMDC",
            domCheck: function(){
                return document.querySelector('input[type=password][name=pin]') && document.querySelector('input[type=submit][value=Submit]') && /BOM/.test(document.querySelector('img').src)
            },
            action: function() {
                var errorMsg = document.querySelector('td span');
                if(errorMsg && errorMsg.innerHTML.match(/The password that you have entered is incorrect/) ){
                    __juspay.trackInvalidScenarios("The password that you have entered is incorrect","Invalid password");
                }
                passwordFragmentHelper('input[type=password][name=pin]');
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                if(submitBtn){
                   __juspay.clickOnElement(submitBtn);
                }
            }
        },

        //City-Union card Password Page
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "CITUDC",
            domCheck: function(){
                return document.querySelector('input[type=password][name=pin]') && document.querySelector('input[type=submit][value=Submit]') && /CityUnionBank/.test(document.querySelector('img').src)
            },
            action: function() {
                var errorMsg = document.querySelector('td span');
                if(errorMsg && errorMsg.innerHTML.match(/The password that you have entered is incorrect/) ){
                    __juspay.trackInvalidScenarios("The password that you have entered is incorrect","Invalid password");
                }
                passwordFragmentHelper('input[type=password][name=pin]');
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //Indus DC ACS_option
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "INDUSDC",
            domCheck: function(){
                return document.querySelector('input[type=radio][id="static"]') &&
                       document.querySelector('input[type=radio][id="otp"]') &&
                       /IBLDCV001/.test(document.querySelector('img').src)
            },
            action: function() {
                var vbv = document.querySelector('img[src*=vpas_logo]');
                if(vbv){
                    Gatekeeper.setCardBrand(VISA);
                }
                __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                Gatekeeper.showACSOptions();
            },
            reachOtpStage: function() {
                var passOption = document.querySelector('input[type=radio][id="static"]');
                var otpOption = document.querySelector('input[type=radio][id="otp"]');
                if(otpOption){
                    passOption.checked = false;
                    otpOption.checked = true;

                }
                var submitBtn = document.querySelector("button[type=button][id=continue]");
                if(submitBtn){
                    Gatekeeper.showWaitingFragment();
                    __juspay.clickOnElement(submitBtn);
                }
            },
            reachPasswordStage: function() {
                var passOption = document.querySelector('input[type=radio][id="static"]');
                var otpOption = document.querySelector('input[type=radio][id="otp"]');
                if(passOption){
                    otpOption.checked = false;
                    passOption.checked = true;
                }
                var submitBtn = document.querySelector("button[type=button][id=continue]");
                if(submitBtn){
                    Gatekeeper.showWaitingFragment();
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        //Indus card Password Page
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "INDUSDC",
            domCheck: function(){
                return document.querySelector('input[type=password][name=pin]') &&
                       document.querySelector('input[type=submit][value=Submit]') &&
                       /Indus/.test(document.querySelector('img').src)
            },
            action: function() {
                var errorMsg = document.querySelector('td span');
                if(errorMsg && errorMsg.innerHTML.match(/The password that you have entered is incorrect/) ){
                    __juspay.trackInvalidScenarios("The password that you have entered is incorrect","Invalid password");
                }
                passwordFragmentHelper('input[type=password][name=pin]');
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
               __juspay.clickOnElement(submitBtn);
            }
        },
        //Indus Ind OTP
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "INDUSDC",
            domCheck: function() {
                return document.querySelector('img[id=banklogo][alt="IndusInd Bank"]') &&
                       document.querySelector('input[name=otp][id=enterPIN]') &&
                       document.querySelector('button[type=button][id=sendotp]') &&
                       document.querySelector('input[name=disclaimer][id=disclaimer]');
            },
            action: function(){
                var otpField = document.querySelector('input[name=otp][id=enterPIN]');
                var termsAndCondition = document.querySelector('input[name=disclaimer][id=disclaimer]');
                if(termsAndCondition){
                    termsAndCondition.checked = true;
                }
            }
        },
        //INDUS IND Net Banking Login Page
        {
            path: /\/corp\/BANKAWAY/,
            hostname: "indusnet.indusind.com",
            state: "UNUSED",
            local: false,
            bank: "INDUSNB",
            domCheck: function() {
                return document.querySelector('input[name=CorporateSignonCorpId]') &&
                    document.querySelector('input[name=CorporateSignonPassword]') &&
                    document.querySelector('input[value=login][id=submit]');
            },
            action: function() {
                Gatekeeper.removeFragment("login page");
                setupLoginPageWithSubmitButton("input[name=CorporateSignonPassword]","input[name=CorporateSignonCorpId]","input[value=login][id=submit]");
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
            },
            nextAction: function() {
                var passwordField = document.querySelector('input[name=CorporateSignonPassword]')
                if(passwordField){
                    passwordField.focus();
                }
            },
            clickSubmitButton: function() {
                var submitButton = document.querySelector('input[value=login][id=submit]');
                if(submitButton){
                    __juspay.clickOnElement(submitButton);
                }
            }
        },
        //Andhra NB retail
        {
            path: /BankAwayRetail/,
            hostname: "www.onlineandhrabank.net.in",
            state: "UNUSED",
            local: false,
            bank: "ANDHNB",
            domCheck: function() {
                return document.querySelector('input[name=CorporateSignonCorpId]') &&
                document.querySelector('input[name=CorporateSignonPassword][type=PASSWORD]') &&
                document.querySelector('input[id=button1][type=button]') &&
                document.querySelector('input[name=RESET]');
            },
            action: function() {
                Gatekeeper.removeFragment("login page");
                var loginField = document.querySelector('input[name=CorporateSignonCorpId]');
                var resetBtn = document.querySelector('input[name=RESET]');
                resetBtn.addEventListener("click", function() {
                    loginField.focus();
                }, false);
                setupLoginPageWithSubmitButton("input[name=CorporateSignonPassword]","input[name=CorporateSignonCorpId]","input[type=button][id=button1]");
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
            },
            nextAction: function() {
                var passwordField = document.querySelector('input[name=CorporateSignonPassword][type=PASSWORD]')
                if(passwordField){
                    passwordField.focus();
                }
            },
            clickSubmitButton: function() {
                var submitButton = document.querySelector('input[type=button][id=button1]');
                if(submitButton){
                    __juspay.clickOnElement(submitButton);
                }
            }
        },
        //BBK NB
        {
            path: /BankAwayRetail/,
            hostname: "ebanking.bbkindia.com",
            state: "UNUSED",
            local: false,
            bank: "BBKNB",
            domCheck: function() {
                return document.querySelector('input[name=CorporateSignonCorpId][type=TEXT]') &&
                document.querySelector('input[id=button1][type=button]') &&
                document.querySelector('input[name=CorporateSignonPassword][type=PASSWORD]')
            },
            action: function() {
                Gatekeeper.removeFragment("login page");
                var corporateId= document.querySelector('input[name=CorporateSignonCorpId]');
                var resetBtn = document.querySelector("input[name=RESET][type=RESET]");
                if(resetBtn){
                    resetBtn.addEventListener("click", function() {
                        corporateId.focus();
                    });
                }
                setupLoginPageWithSubmitButton('input[name=CorporateSignonPassword][type=PASSWORD]','input[name=CorporateSignonCorpId][type=TEXT]','input[type=button][id=button1]');
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
            },
            clickSubmitButton: function() {
                var submitButton = document.querySelector('input[id=button1][type=button]');
                if(submitButton){
                    __juspay.clickOnElement(submitButton);
                }
            },
            nextAction: function() {
                var passwordField = document.querySelector('input[name=CorporateSignonPassword][type=PASSWORD]');
                if(passwordField){
                    passwordField.focus();
                }
            }
        },
        //Andhra Bank new OTP page
        {
            path: /\/(ACSWeb|acs-web-v22)\/EnrollWeb\/AndhraBank\/server/,
            hostname: "b10-pdc.enstage-sas.com" ,
            state: "UNUSED",
            bank: "ANDHDC",
            domCheck: function(){
                return  document.querySelector('input[id=otpValue][type=password]') &&
                    document.querySelector('input[type=image][src*=btn_submit]');
            },
            action: function(){
                submitBtn = document.querySelector('input[type=image][src*=btn_submit]');
                var errorMsg = document.querySelector(".clean-error");
                if(errorMsg && errorMsg.innerHTML.match(/OTP verification failed/) ){
                    __juspay.trackInvalidScenarios("The OTP that you have entered is incorrect","Invalid OTP");
                }
                otpFragmentHelper('input[id=otpValue][name=otpValue]','a[href*=resend_otp]');
                if(submitBtn){
                    if(__juspay.isMobUiEnabled("MOBUI_ANDH")) {
                        try{
                            submitBtn.style.width = "70px"
                            submitBtn.style.height = "40px"
                        }
                        catch(err){
                            __juspay.CatchPageModifyError("MOBUI",err,"SUBMIT_FIELDS");
                        }
                    }
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otpValue]','input[type=Image][src*=btn_submit]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[href*=resend_otp]')
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },

        //FEDDC: New page: added on 30th  May 2019
        {
            path: /\/mdpayacs\/pareq/,
            hostname: "www.thecardservicesonline.com",
            state: "UNUSED",
            bank: "FEDDC",
            domCheck: function(){
                return document.querySelector('input[id=OTP]') &&
                       document.querySelector('input[id=IDCT_BUTID][type=submit]') &&
                       document.querySelector('a[href*=RESEND]') &&
                       document.querySelector('link[href*="Federal_logo"]');
            },
            action: function(){
                otpFragmentHelper('input[id=OTP]','a[href*=RESEND]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=OTP]','input[id=IDCT_BUTID][type=submit]');
            },
            regenerateOtp: function(){
                var generateOtpBtn = document.querySelector('a[href*=RESEND]');
                __juspay.clickOnElement(generateOtpBtn);
            }
        },

        {//FEDDC: New page: added on Jan 2020
            path: /\/acs-v1\/processauth/,
            hostname: "acs9-fd.enstage-sas.com",
            state: "UNUSED",
            bank: "FEDDC",
            domCheck: function(){
                return document.querySelector('input[id="otpValue"]') &&
                       document.querySelector('a[id="submit_id"]') &&
                       document.querySelector('a[onclick*="resendOtpform"]');
            },
            action: function(){
                otpFragmentHelper('input[id="otpValue"]','a[onclick*="resendOtpform"]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id="otpValue"]','a[id="submit_id"]');
                Gatekeeper.removeFragment("OTP Submitted");
            },
            regenerateOtp: function(){
                var generateOtpBtn = document.querySelector('a[onclick*="resendOtpform"]');
                __juspay.clickOnElement(generateOtpBtn);
            }
        },

        //Jammu Kashmir Bank Password Page
        {
            path: /\/ACSWeb\/EnrollWeb\/JKBank\/server\/AccessControlServer/,
            hostname: "cardsecurity.enstage.com",
            state: "UNUSED",
            bank: "JKDC",
            domCheck: function(){
                return  document.querySelector('#txtPassword') &&
                        document.querySelector('input[id=cmdSubmit][value=Submit]');
            },
            action: function(){
                var errorElement = document.querySelector('div strong font');
                if(errorElement && errorElement.innerHTML.match(/Invalid.*/)){
                    Gatekeeper.shoutOut("ERROR IN Jammu Kashmir BANK ");
                    __juspay.trackInvalidScenarios("Invalid","Invalid password");
                }
                passwordFragmentHelper('#txtPassword');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[id=cmdSubmit][value=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //Karur Vysya Bank OTP Page
        {
            path: /\/acs-web-v19\/EnrollWeb\/KVB\/server\/(AccessControlServer|OtpServer)/,
            hostname: "b1-kvb.enstage-sas.com",
            state: "UNUSED",
            bank: "KVBDC",
            domCheck: function(){
                return  document.querySelector('a[class*="submitotp"][onclick*="postServlet"]') &&
                        document.querySelector('input[id=otpValue]') &&
                        document.querySelector('img[src*="8116/images/logo"]');
            },
            action: function(){
                otpFragmentHelper('input[id=otpValue]','a[onclick*="resend_otp"]');
                if(__juspay.isMobUiEnabled("MOBUI_KVBDC")) {
                    try{
                        addMetaTag();
                        var submitBtn = document.querySelector('div[class="Btn submit"]');
                        var submitDiv = document.querySelector('div[class="Btn submit"]').parentElement;
                        var regenerateBtn = document.querySelector('a[onclick*="resend_otp"]');
                        submitBtn.style.width = "150px";
                        submitBtn.style.marginLeft = "50%";
                        submitDiv.style.marginTop = "50px";
                        regenerateBtn.style.marginTop="5px"
                    }
                    catch(err){
                        __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                    }
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otpValue]', 'a[class*="submitotp"][onclick*="postServlet"]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[onclick*="resend_otp"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //Karnataka DC OTP PAGE
        {
            path: /\/acs-web-v23\/EnrollWeb\/KTB\/server\/OtpServer/,
            hostname: /(pdc-ktk|sdc-ktk)\.enstage-sas\.com/,
            state:"UNUSED",
            bank: "KTBDC",
            domCheck: function(){
                return document.querySelector('img[src*="8131/images/logo"]') &&
                       document.querySelector('input[id=otpValue][type=password]') &&
                       document.querySelector('input[type=image][src*=btn_submit]') &&
                       document.querySelector('a[href*=resend_otp]');
            },
            action: function(){
                var submitButton = document.querySelector('input[type=image][src*=btn_submit]');
                var visa_logo = document.querySelector('img[src*=vbv_logo]');
                if(visa_logo){
                   Gatekeeper.setCardBrand(VISA);
                }
                otpFragmentHelper('input[id=otpValue][type=password]','a[href*=resend_otp]',false);
                submitButton.addEventListener("click",function(){
                    window.trackEventv2_0default("ui","click","KTBDC","SUBMIT_BUTTON_CLICKED");
                    Gatekeeper.hideAssistanceFragment();
                },false);
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otpValue][type=password]', 'input[type=image][src*=btn_submit]');
            },
            regenerateOtp: function(){
                var resendOtpButton = document.querySelector('a[href*=resend_otp]');
                if(resendOtpButton){
                    __juspay.clickOnElement(resendOtpButton);
                }
            }
        },
        //Karur vysya Netbanking Mobile optimisation
        {
            path: /\/B001\/merchant/,
            hostname: "www.kvbin.com",
            state: "UNUSED",
            local: false,
            bank: "KVBNB",
            domCheck: function(){
                var iframe = document.querySelector('iframe');
                if(iframe) {
                    var doc = iframe.contentDocument;
                }
                return iframe && doc /* && userId;*/
            },
            action: function(){
                var iframe = document.querySelector('iframe');
                if(iframe) {
                    var doc = iframe.contentDocument;
                }
                if(doc) {
                    var formsToScroll = doc.querySelectorAll('form');
                    if(formsToScroll){
                        for(var i = 0; i < formsToScroll.length; i++){
                            formsToScroll[i].setAttribute("style","overflow:scroll");
                        }
                    }
                    var logintable = doc.querySelectorAll('table')[0];
                    var table = doc.querySelectorAll('table')[1];
                    var userId = table.childNodes[1].childNodes[0];
                    var rows = doc.getElementsByClassName("row");
                    var loginbtn = table.childNodes[1].childNodes[4].childNodes[5].childNodes[3];
                    if(__juspay.isMobUiEnabled("MOBUI_KVBNB") && doc){
                        try{
                            var metaTag=document.createElement('meta');
                            metaTag.name = "viewport"
                            metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                            document.getElementsByTagName('head')[0].appendChild(metaTag);
                            setTimeout(function(){
                                document.querySelector('body').style.height="100%";
                                document.querySelector('body').style.width="100%";
                                doc.querySelector('body').style.height ="100%";
                                doc.querySelector('body').style.width ="100%";
                            },1000);
                            window.trackEventv2_0("acs","info","MOBUI_KVBNB","KVBNB_LOGIN_PAGE", "MOBUI", "kvbnb");
                            logintable.parentElement.parentElement.removeAttribute("style");
                            __juspay.modifyUI({"display_inline_block":[table,rows[6]],"style_width":{"other":[1,logintable.parentElement.parentElement],"other2":[0.835,table]}});
                            __juspay.removeElements([userId.childNodes[1],table.childNodes[1].childNodes[4].childNodes[1],table.childNodes[1].childNodes[6],table.childNodes[1].childNodes[8],rows[0].childNodes[7],rows[4],rows[5],rows[7]]);
                            logintable.parentElement.parentElement.style.height=window.innerWidth * 0.5 + "px";
                            __juspay.modifyUI({"add_break_before":[loginbtn]});
                            table.childNodes[1].childNodes[4].childNodes[2].style.paddingBottom = "50px";
                            rows[3].childNodes[1].innerHTML ='I / We acknowledge and accept the <a style = "color:red;" a href="terms.html" target="_blank">Terms and Conditions</a> applicable and available on the site';
                            window.trackEventv2_0default("acs","info","MOBUI","LOGIN_PAGE");
                        } catch(err) {
                            __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                        }
                    }
                    var userIdField = doc.querySelector('input[name="fldLoginUserId"]');
                    var pwdField = doc.querySelector('input[name="fldPassword"][type=password]');
                    var submitBtn = doc.querySelector('input[name="imageField"][type=button]');
                    var bankCustomerId = Gatekeeper.getBankCustomerId();
                    if(bankCustomerId){
                        if(bankCustomerId==="undefined" ||typeof( bankCustomerId)=="undefined") {
                            bankCustomerId = "";
                        }
                    }
                    if(userIdField && submitBtn){
                        userIdField.value= bankCustomerId;
                        __juspay.trackPageStatus("INPUT_NB_LOGIN");
                        __juspay.trackAuthMethod("LOGIN");
                        userIdField.addEventListener("focus", function (){
                            Gatekeeper.showNetbankingCustomerIdFragment("continue");
                            __juspay.scrollToElement(userIdField);
                        }, false);
                        focusElement(userIdField);
                        Gatekeeper.showNetbankingCustomerIdFragment("continue");
                        attachAlphaNumericPasswordHelper(pwdField);
                        if(userIdField.value!=""){
                            blurElement(pwdField);
                            focusElement(pwdField);
                        }
                        submitBtn.addEventListener("click", function() { // on Submitting login credentials the inner frame passes the values to main document
                             Gatekeeper.onCustomerIdSubmit(document.querySelector('input[type=hidden][name=fldLoginUserId]').value);
                        },false);
                    }
                }
            },
            clickSubmitButton: function(){
                var iframe = document.querySelector('iframe');
                if(iframe) {
                    var doc = iframe.contentDocument;
                }
                if(doc) {
                    var submitBtn = doc.querySelector('input[name="imageField"][type=button]');
                     var pwdField = doc.querySelector('input[name="fldPassword"][type=password]');
                    if(submitBtn){
                        if(pwdField.value != ""){
                            Gatekeeper.removeFragment("submit_button_clicked_with_password_value_nonempty");
                        }
                         __juspay.clickOnElement(submitBtn);
                    }
                }
            },
            nextAction: function() {
                var iframe = document.querySelector('iframe');
                if(iframe) {
                    var doc = iframe.contentDocument;
                }
                if(doc) {
                    var userIdField = doc.querySelector('input[name="fldLoginUserId"]');
                    var pwdField = doc.querySelector('input[name="fldPassword"][type=password]');
                    var submitBtn = doc.querySelector('input[name="imageField"][type=button]');
                    if(userIdField && pwdField && submitBtn){
                        if(doc.activeElement==userIdField) {
                            pwdField.focus();
                             __juspay.scrollToElement(pwdField);
                        }
                    }
                }
            }
        },

        //KVBNB pages
        {
        path: /\/B001\/internet/,
        hostname: "www.kvbin.com",
        state: "UNUSED",
        local: false,
        bank: "KVBNB",
        domCheck: function(){
                return true;
        },
        action: function(){
            var frameCheckLooperSubmitBtn=setInterval(function(){
                var iframe = document.querySelector('frame[name=frame_txn]');
                if(iframe) {
                    var doc = iframe.contentDocument;
                    if(doc){
                        var form = doc.querySelector('form[name=frmmain]');
                        if(form){
                            var submitBtn = doc.querySelector('input[id=fldinitiate]');
                            if(submitBtn && submitBtn.parentElement.parentElement ){
                                submitBtn.parentElement.parentElement.setAttribute("style","float:left")
                                __juspay.trackPageStatus("CHOOSE_ACCOUNT");
                                clearInterval(frameCheckLooperSubmitBtn);
                            }
                        }
                    }
                }
            },500);
            var frameCheckLooperConfirmBtn=setInterval(function(){
                var iframe = document.querySelector('frame[name=frame_txn]');
                if(iframe) {
                    var doc = iframe.contentDocument;
                    if(doc){
                        var form = doc.querySelector('form[name=frmmain]');
                        if(form){
                            var confirmBtn = doc.querySelector('input[id=btnConfirm]');
                            if(confirmBtn && confirmBtn.parentElement.parentElement){
                                confirmBtn.parentElement.parentElement.setAttribute("style","float:left")
                                __juspay.trackPageStatus("CONFIRM");
                                clearInterval(frameCheckLooperConfirmBtn);
                            }
                        }
                    }
                }
            },500);
            var frameCheckLooperOtpBtn=setInterval(function(){
                var iframe = document.querySelector('frame[name=frame_txn]');
                if(iframe) {
                    var doc = iframe.contentDocument;
                    if(doc){
                        var form = doc.querySelector('form[name=frmmain]');
                        if(form){
                            var submitOtpBtn = doc.querySelector('input[name=fldsubmit]');
                            if(submitOtpBtn && submitOtpBtn.parentElement.parentElement){
                                submitOtpBtn.parentElement.parentElement.setAttribute("style","float:left")
                                __juspay.trackPageStatus("INPUT_OTP");
                                __juspay.trackAuthMethod("OTP");
                                clearInterval(frameCheckLooperOtpBtn);
                            }
                        }
                    }
                }
            },500);
        }
        },
        {
            path: /\/B001\/epiredirect\.jsp/,
            hostname: "www.kvbin.com",
            state: "UNUSED",
            local: false,
            bank: "KVBNB",
            domCheck: function(){
                    return false;
            }
        },

        //IOBDC: New page: added on 30th  May 2019
        {
            path: /\/acs-web-v28\/EnrollWeb\/IOB\/server/,
            hostname: "b11-pdc.enstage-sas.com",
            state: "UNUSED",
            bank: "IOBDC",
            domCheck: function(){
                return document.querySelector('input[id="otpValue"]') &&
                       document.querySelector('a[class="active submitotp"][onclick*="postServlet"]') &&
                       document.querySelector('a[onclick*="resend_otp"]');
            },
            action: function(){
                otpFragmentHelper('input[id="otpValue"]','a[onclick*="resend_otp"]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id="otpValue"]','a[class="active submitotp"][onclick*="postServlet"]');
            },
            regenerateOtp: function(){
                var generateOtpBtn = document.querySelector('a[onclick*="resend_otp"]');
                __juspay.clickOnElement(generateOtpBtn);
            }
        },

        //ALLBDC: New page: added on 30th  May 2019
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "ALLBDC",
            domCheck: function(){
                return document.querySelector("input[id='otpentrypin']") &&
                       document.querySelector("button[id='sendotp'][value='Submit OTP']") &&
                       document.querySelector("img[title='Allahabad Bank']");
            },
            action: function(){
                otpFragmentHelper('input[id="otpentrypin"]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id="otpentrypin"]','button[id="sendotp"][value="Submit OTP"]');
            }
        },

        //JKDC: New page: added on 30th  May 2019
        {
            path: /\/acspage\/cap/,
            hostname: "secure7.arcot.com",
            state: "UNUSED",
            bank: "JKDC",
            domCheck: function(){
                return document.querySelector('input[id="enterPIN"][name="otp"]') &&
                       document.querySelector('button[id="sendotp"][title="Submit"]') &&
                       document.querySelector('a[onclick*=reSend]') &&
                       document.querySelector('img[src*=jkbank]');
            },
            action: function(){
                otpFragmentHelper('input[id="enterPIN"][name="otp"]','a[onclick*=reSend]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id="enterPIN"][name="otp"]','button[id="sendotp"][title="Submit"]');
            },
            regenerateOtp: function(){
                var generateOtpBtn = document.querySelector('a[onclick*=reSend]');
                __juspay.clickOnElement(generateOtpBtn);
            }
        },

        //Indian Overseas Bank Password Page
        {
            path: /\/ACSWeb\/EnrollWeb\/IOB\/server\/AccessControlServer/,
            hostname: "b11-pdc.enstage-sas.com",
            state: "UNUSED",
            bank: "IOBDC",
            domCheck: function(){
                return  document.querySelector('input[name="txtPassword"]') &&
                        document.querySelector('form[action*="IOB"]') &&
                        document.querySelector('input[name="cmdSubmit"][value="Submit"]');
            },
            action: function(){
                var errorElement = document.querySelector('div strong font');
                if(errorElement && errorElement.innerHTML.match(/Invalid.*/)){
                    __juspay.trackInvalidScenarios("Invalid","Invalid password");
                }
                passwordFragmentHelper('input[name="txtPassword"]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[name="cmdSubmit"][value="Submit"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //Bank Of Baroda Password Page
        {
            path: /\/ACSWeb\/EnrollWeb\/BOBCards\/server\/AccessControlServer/,
            hostname: "cardsecurity.enstage.com",
            state: "UNUSED",
            bank: "BOBDC",
            domCheck: function(){
                return  document.querySelector('input[name="txtPassword"]') &&
                        document.querySelector('form[action*="BOBCards"]') &&
                        document.querySelector('input[name="cmdSubmit"][value="Submit"]');
            },
            action: function(){
                var errorElement = document.querySelector('div strong font');
                if(errorElement && errorElement.innerHTML.match(/Invalid.*/)){
                    Gatekeeper.shoutOut("ERROR IN BOB BANK ");
                    __juspay.trackInvalidScenarios("Invalid","Invalid password");
                }
                passwordFragmentHelper('input[name="txtPassword"]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[name="cmdSubmit"][value="Submit"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //Bank Of Baroda OTP Page
        {
            path: /\/acs-web-v16\/EnrollWeb\/BOB\/server\/AccessControlServer/,
            hostname: "b3-bob.enstage-sas.com",
            state: "UNUSED",
            bank: "BOBDC",
            domCheck: function(){
                return  document.querySelector('input[src*="btn_submit"]') &&
                        document.querySelector('input[id=otpValue]') &&
                        document.querySelector('img[src*="8129/images/logo"]');
            },
            action: function(){
                otpFragmentHelper('input[id=otpValue]','a[href*="resend_otp"]');
                if(__juspay.isMobUiEnabled("MOBUI_BOBDC")) {
                    try{
                        addMetaTag();
                        var regenerateDiv = document.querySelector('span[class*="resendOTP"]').parentElement;
                        regenerateDiv.style.height="30px";
                        regenerateDiv.style.marginTop="20px";
                    }
                    catch(err){
                        __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                    }
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otpValue]', 'input[src*="btn_submit"]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[href*="resend_otp"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        { //BOB_CC: OTP page
            path: /\/acs-web-v13\/EnrollWeb\/BOBCards\/server\/AccessControlServer|OtpServer/,
            hostname: "b18-pdc.enstage-sas.com",
            state: "UNUSED",
            local: false,
            bank: "BOBCC",
            domCheck: function() {
                return document.querySelector('input[id=otpValue][name=otpValue]') &&
                        document.querySelector('a[href*="resend_otp"]');
                        document.querySelector('a[href*="submitOTPform"]');
            },
            action:function()
            {
                otpFragmentHelper("input[id=otpValue][name=otpValue]","a[href*='resend_otp']");
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, "input[id=otpValue][name=otpValue]", "a[href*='submitOTPform']")
            },
            regenerateOtp: function(){
                var resendLink = document.querySelector('a[href*="resend_otp"]');
                __juspay.clickOnElement(resendLink);
            }
        },
        // YESNB: new login support
        {
            path: /\/netbanking\/merchant/,
            hostname: "netbanking.yesbank.co.in",
            state: "UNUSED",
            bank: "YESNB",
            domCheck: function(){
                return true;
            },
            action: function(){
                Gatekeeper.removeFragment("Reached Login Page ");
                var myVar = setInterval(function(){
                    var iframe = document.querySelector("frame");
                    if(iframe) {
                        var iframeDoc = iframe.contentDocument;
                        while(iframeDoc.querySelector("frame")){
                            iframeDoc = iframeDoc.querySelector("frame").contentDocument;
                            if(iframeDoc.querySelector('img[src="images/eng/index/Tab_Welcome2NB.gif"]')){
                                clearInterval(myVar);
                                contentLoad();
                            }
                        }
                    }
                },500);
                var contentLoad = function(){
                    var frameCheck = setInterval(function(){
                        var iframe = document.querySelector("frame");
                        if(iframe){
                            var iframeDoc = iframe.contentDocument;
                            if(iframeDoc){
                                clearInterval(frameCheck);
                                while(iframeDoc.querySelector("frame")){
                                    iframeDoc = iframeDoc.querySelector("frame").contentDocument;
                                }
                                var formToScroll = iframeDoc.querySelector("form[name=frmLogon]");
                                var customerIdField = iframeDoc.querySelector('input[name="fldLoginUserId"]');
                                var ipinField = iframeDoc.querySelector('input[name="fldPassword"]');
                                var submitBtn = iframeDoc.querySelector('img[src*=button_login]');
                                var forgotID= iframeDoc.querySelector('a[href="ForgotLoginId.html"]');
                                if(formToScroll){
                                    formToScroll.setAttribute("style","overflow:scroll");
                                }

                                 if(customerIdField) {
                                    customerIdField.blur();
                                    customerIdField.addEventListener("focus", function (){
                                        numberTextFieldQuery("input[name=fldLoginUserId]");
                                        Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                                    }, false);
                                    customerIdField.focus();
                                    var bankCustomerId = Gatekeeper.getBankCustomerId();
                                    if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined") {
                                        bankCustomerId = "";
                                    }
                                    customerIdField.value = bankCustomerId;
                                }
                                if(ipinField){
                                    attachAlphaNumericPasswordHelper(ipinField);
                                    ipinField.addEventListener("focus", function (){
//                                        Gatekeeper.showPasswordHelperFragment();
                                    }, false);
                                    __juspay.trackPageStatus("INPUT_LOGIN");
                                    __juspay.trackAuthMethod("LOGIN");
                                        Gatekeeper.changeNextActionText("Login");
                                    __juspay.scrollToElement(ipinField);
                                }
                                forgotID.parentElement.addEventListener("click",function() {
                                    Gatekeeper.removeFragment('forgotId button Clicked');
                                });
                                submitBtn.addEventListener("click",function(){
                                    if(customerIdField.value){
                                        Gatekeeper.onCustomerIdSubmit(customerIdField.value);
                                    }
                                    Gatekeeper.removeFragment("entered credentials");
                                },false);
                                if(__juspay.isMobUiEnabled("MOBUI_YESNB")) {
                                    var mobuiCheck = setInterval(function(){
                                        if(iframeDoc){
                                            clearInterval(mobuiCheck);
                                            var metaTag=document.createElement('meta');
                                            metaTag.name = "viewport"
                                            metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                                            document.getElementsByTagName('head')[0].appendChild(metaTag);
                                            var leftColumn = iframeDoc.querySelector('img[src="images/eng/index/Tab_Welcome2NB.gif"]');
                                            var rightColumn = iframeDoc.querySelectorAll('td table')[20];
                                            var bottomTerms = iframeDoc.querySelectorAll('td table')[21];
                                            var bottomAds= iframeDoc.querySelectorAll('td table')[23];
                                            var vkcb = iframeDoc.querySelector('img[src="images/eng/blue/New_Yesb.gif"]');
                                            var topOne = iframeDoc.querySelectorAll('td table')[1];
                                            var topTwo = iframeDoc.querySelectorAll('td table')[2];
                                            var topCurve = iframeDoc.querySelector('img[src="images/eng/index/mainbody_Curve_top1.gif"]');
                                            var bottomCurve = iframeDoc.querySelector('img[src="images/eng/index/mainbody_Curve_bottom1.gif"]');
                                            var loginCurveTop = iframeDoc.querySelector('img[src="images/eng/index/login_curve_top.gif"]');
                                            var loginCurveBottom = iframeDoc.querySelector('img[src="images/eng/index/login_curve_bottom.gif"]');
                                            var loginButton = iframeDoc.querySelectorAll('a[class=bodylink1]')[1];
                                            var form = iframeDoc.querySelector('form');
                                            var type = iframeDoc.querySelector('input[name=fldPass]');
                                            var topSpace = iframeDoc.querySelector('img[src="images/eng/index/space.gif"]');
                                            var disclaimer = iframeDoc.querySelectorAll('a[class="bodylink1"]')[10];
                                            var bottomPart = iframeDoc.querySelectorAll('td table')[9];
                                            if(leftColumn && rightColumn && bottomTerms && bottomAds && vkcb && topOne && topTwo && topCurve && bottomCurve && loginCurveTop && loginCurveBottom && loginButton && topSpace && disclaimer && type && form){
                                                clearInterval(mobuiCheck);
                                                try{
                                                    var elements_list = [leftColumn.parentElement.parentElement.parentElement.parentElement.parentElement,rightColumn,bottomTerms,bottomAds,vkcb.parentElement,topCurve,disclaimer.parentElement];
                                                    __juspay.removeElements(elements_list);
                                                    __juspay.modifyUI({"add_break_before":[type.parentElement.parentElement,loginButton.parentElement.parentElement.parentElement.parentElement],"style_height":{"other":[0,topSpace]},"style_width":{"other":[1,topOne,topTwo,bottomCurve,loginCurveTop,loginCurveBottom,bottomPart,form.querySelector('table')]}});
                                                    window.trackEventv2_0default("acs","info","MOBUI","LOGIN_PAGE");
                                                }catch(err){
                                                    __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                                                }
                                            }
                                        }
                                    },100);
                                }
                            }
                            var trnPassword = iframeDoc.querySelector('input[name=fldPin]');
                        }
                    },1000);
                }
                contentLoad();
                var transactionPasswordFieldCheck = setInterval(function(){
                    var frame = document.querySelector("frame[name=bottom_frame]");
                    if(frame){
                        var iframeDoc = frame.contentDocument;
                            if(iframeDoc){
                                var txnPassField = iframeDoc.querySelector('input[id=fldPin]');
                                    if(txnPassField){
                                        clearInterval(transactionPasswordFieldCheck);
                                        __juspay.trackPageStatus("INPUT_OTP");
                                        __juspay.trackAuthMethod("OTP");
                                        __juspay.showOtpFragmentWrapper(txnPassField);
                                        Gatekeeper.requestKeyboardHide();
                                        Gatekeeper.setPollingForSmsEnabled(true);
                                        __juspay.scrollToElement(txnPassField);
                                        txnPassField.addEventListener("click", function (){
                                            Gatekeeper.hideAssistanceFragment();
                                        }, false);
                                    }
                            }
                    }

                },1000);
            },
            submitOtp: function(otp){
                var iframe = document.querySelector("frame[name=bottom_frame]");
                if(iframe){
                    var iframeDoc = iframe.contentDocument;
                    while(iframeDoc.querySelector("frame")){
                        iframeDoc = iframeDoc.querySelector("frame").contentDocument;
                    }
                    if(iframeDoc){
                        var txnPassField = iframeDoc.querySelector('input[id=fldPin]');
                        var otpSubmit = iframeDoc.querySelector('img[src*=submit_but]')
                        if(txnPassField && otpSubmit){
                            txnPassField.value = otp;
                            __juspay.clickOnElement(otpSubmit);
                            Gatekeeper.removeFragment("Submitting OTP");
                        }
                    }
                }
            },
            nextAction: function(){
                var iframe = document.querySelector("frame");
                if(iframe){
                    var iframeDoc = iframe.contentDocument;
                    while(iframeDoc.querySelector("frame")){
                        iframeDoc = iframeDoc.querySelector("frame").contentDocument;
                    }
                    if(iframeDoc){
                        var ipinField = iframeDoc.querySelector('input[name="fldPassword"]');
                        if(ipinField){
                            ipinField.focus();
                        }
                    }
                }
            },
            clickSubmitButton: function(){
                var iframe = document.querySelector("frame");
                if(iframe){
                    var iframeDoc = iframe.contentDocument;
                    while(iframeDoc.querySelector("frame")){
                        iframeDoc = iframeDoc.querySelector("frame").contentDocument;
                    }
                    if(iframeDoc){
                        var submitBtn = iframeDoc.querySelector('img[src*=button_login]');
                        if(submitBtn){
                            __juspay.clickOnElement(submitBtn);
                            var secondPage = setInterval(function(){
                                var iframeDc = document.querySelector("frame").contentDocument;
                                var selectElement = iframeDc.querySelector("select");
                                if(selectElement) {
                                     selectElement.focus();
                                     clearInterval(secondPage);
                                }
                            },500);
                        }
                    }
                }
            }
        },
        //CanaraDC: New page: added on 10th  April 2018
        {
            path: /\/acs-web-v15\/EnrollWeb\/CanaraBank\/server/,
            hostname: "b8-pdc.enstage-sas.com",
            state: "UNUSED",
            bank: "CANARADC",
            domCheck: function(){
                return document.querySelector('input[type=password][id=otpValue]') &&
                       document.querySelector('a[class*=submitotp][onclick*=postServlet]');
            },
            action: function(){
                otpFragmentHelper('input[id=otpValue]','a[onclick*=resend_otp]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otpValue]','a[class*=submitotp][onclick*=postServlet]');
            },
            regenerateOtp: function(){
                var generateOtpBtn = document.querySelector('a[onclick*=resend_otp]');
                if(generateOtpBtn){
                    __juspay.clickOnElement(generateOtpBtn);
                }
            }
        },
        //CanaraNB: login page
        {
            path: /\/entry\/merchantretail/,
            hostname: "netbanking.canarabank.in",
            state: "UNUSED",
            bank: "CANARANB",
            domCheck: function(){
                return true;
            },
            action: function(){
                Gatekeeper.removeFragment("Reached Login Page ");
                var frameCheck = setInterval(function(){
                    var iframe = document.querySelector("iframe");
                    if(iframe){
                        var iframeDoc = iframe.contentDocument;
                        if(iframeDoc){
                            var formToScroll = iframeDoc.querySelector("form[name=frmLogon]");
                            var userIdField = iframeDoc.querySelector("input[name=fldLoginUserId]");
                            var passwordField = iframeDoc.querySelector('input[name="fldPassword"]');
                            if(formToScroll){
                                formToScroll.setAttribute("style","overflow:scroll");
                            }
                            if(userIdField) {
                                userIdField.addEventListener("click",function(){
                                    Gatekeeper.removeFragment('Focused on userId');
                                    userIdField.focus();
                                });
                            }
                            if(passwordField){
                                attachAlphaNumericPasswordHelper(passwordField);
                                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                                __juspay.trackAuthMethod("LOGIN");
                            }
                            clearInterval(frameCheck);
                        }
                    }
                },1000);
            },
            clickSubmitButton: function(){
                var iframe = document.querySelector("iframe");
                if(iframe){
                    var iframeDoc = iframe.contentDocument;
                    if(iframeDoc){
                        var submitBtn = iframeDoc.querySelector('button[name="imageField"]');
                        if(submitBtn){
                            __juspay.clickOnElement(submitBtn)
                        }
                    }
                }
            }
        },
        {
        //canaranb account selection page
            path: /\/entry\/internet/,
            hostname: "netbanking.canarabank.in",
            state: "UNUSED",
            bank: "CANARANB",
            domCheck: function(){
                return true;
            },
            action: function(){
                Gatekeeper.removeFragment('Reachecd account selection page')
                Gatekeeper.setPollingForSmsEnabled(true);
                var checkInvalidPage = setInterval(function(){
                    var closeButton = document.querySelector("input[type=submit][value=Close]");
                    if(closeButton){
                        clearInterval(checkInvalidPage);
                        if(__uber.initSessionCounterIncremental("canaraCLoseButtonClick")==1){
                            closeButton.setAttribute("onclick","1");
                            closeButton.addEventListener("click",function(){window.history.back();});
                            __juspay.trackPageStatus("INVALID_LOGIN");
                        }
                    }
                },1000);
                var fieldCheck = setInterval(function(){
                    var iframe = document.querySelector("frame[name=frame_txn]");
                    if(iframe){
                        var iframeDoc = iframe.contentDocument;
                        var isSelectPresent = iframeDoc.querySelector('select[id=fldsrcaccountno]');
                        var remarksField = iframeDoc.querySelector('input[id="fldnarrative"]');

                        if(isSelectPresent){
                            clearInterval(fieldCheck);
                            var accounts = isSelectPresent.options;
                            __juspay.trackPageStatus("CHOOSE_ACCOUNT_INPUT_REMARKS");
                            var lastChosen = Gatekeeper.getLastChosenAccount("CANARANB");
                            if(accounts.length == 2){
                                accounts[1].selected = "true";
                            }
                            else if(accounts.length > 2){
                            Gatekeeper.showNetbankingDefaultFragment();
                                if(lastChosen) {
                                    for(var i = 0; i < accounts.length; i++) {
                                        if(accounts[i].text === lastChosen) {
                                            accounts[i].selected = "true";
                                        }
                                    }
                                }
                                for(var i = 0; i < accounts.length; i++) {
                                    if(accounts[i].selected == "true") {
                                        Gatekeeper.storeLastChosenAccount("CANARANB",accounts[i].text);
                                        break;
                                    }
                                }
                            }

                        }
                        if(remarksField){
                            Gatekeeper.showNetbankingDefaultFragment();
                            var remarks = Gatekeeper.getRemarksForBank();
                            if(remarks != null){
                                remarksField.value=remarks;
                            }
                        }
                    }
                },500);

                var txnfieldCheck = setInterval(function(){
                    var iframe = document.querySelector("frame[name=frame_txn]");
                    if(iframe){
                        var iframeDoc = iframe.contentDocument;
                        var otpField = iframeDoc.querySelector('input[id="otppwd"]')
                        var txnPwdField = iframeDoc.querySelector('input[id="fldtxnpin1"]');
                        var formToScroll = iframeDoc.querySelector('form[name="frmmain"]');
                        var count = 0;
                        if(formToScroll){
                            formToScroll.setAttribute("style","overflow:scroll");
                        }
                        if(txnPwdField || otpField){
                            clearInterval(txnfieldCheck);
                        }
                        if(txnPwdField){
                            Gatekeeper.removeFragment('Reached Transaction Password Field');
                            attachAlphaNumericPasswordHelper(txnPwdField);
                            txnPwdField.blur();
                            focusElement(txnPwdField);
                        }
                        if(otpField){
                            __juspay.trackPageStatus("INPUT_3DS_OTP");
                            __juspay.trackAuthMethod("OTP");
                            otpField.addEventListener("focus",function(){
                                __juspay.showOtpFragmentWrapper(otpField);
                                otpTextFieldQuery('input[id="otppwd"]');
                                Gatekeeper.requestPhoneKeyboardShow();
                                count++;
                                if(count !== 1){
                                    Gatekeeper.hideAssistanceFragment();
                                }
                            },false);
                            otpField.addEventListener("click",function(){
                                Gatekeeper.hideAssistanceFragment();
                                Gatekeeper.requestPhoneKeyboardShow();
                            });
                        }
                    }
                },1000);

            },
            nextAction: function() {
                try {
                    var iframe = document.querySelector("frame[name=frame_txn]");
                    var iframeDoc = iframe.contentDocument;
                    var confirmBtn = iframeDoc.querySelector('input[id="btnConfirm"]');
                    var submitBtn = iframeDoc.querySelector('input[id="fldinitiate"]');
                    if(submitBtn){
                        __juspay.clickOnElement(submitBtn);
                    }
                    if(confirmBtn && !submitBtn ){
                        __juspay.trackPageStatus("CONFIRM");
                        __juspay.clickOnElement(confirmBtn);
                    }
                } catch(err) {
                    window.trackEventv2_0("acs","error","modify_page_error","Failed to Modify Page: " + String(err), "modify_page", "error");
                }
            },
            clickSubmitButton: function(){
                try {
                    var iframe = document.querySelector("frame[name=frame_txn]");
                    var iframeDoc = iframe.contentDocument;
                    var otpField = iframeDoc.querySelector('input[id="otppwd"]')
                    var txnPwdField = iframeDoc.querySelector('input[id="fldtxnpin1"]');
                    if(otpField){
                        txnPwdField.blur();
                        otpField.focus();
                    }
                } catch(err) {
                    window.trackEventv2_0("acs","error","modify_page_error","Failed to Modify Page: " + String(err), "modify_page", "error");
                }
            },
            submitOtp: function(otp) {
                try {
                    var iframe = document.querySelector("frame[name=frame_txn]");
                    var iframeDoc = iframe.contentDocument;
                    var otpField = iframeDoc.querySelector('input[id="otppwd"]')
                    var otpSubmit = iframeDoc.querySelector('input[name="fldsubmit"]');
                    if(otpField && otpSubmit){
                        if(otp==="undefined" || typeof(otp)=="undefined") {
                            otp = "";
                        }
                        otpField.value = otp;
                        __juspay.clickOnElement(otpSubmit);
                        if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]!="ios"){
                            Gatekeeper.removeFragment('OTP Submitted via fragment');
                        }
                    }
                } catch(err) {
                    window.trackEventv2_0("acs","error","modify_page_error","Failed to Modify Page: " + String(err), "modify_page", "error");
                }
                Gatekeeper.removeFragment("Submitted OTP using OTP Fragment");
            }
        },

       //Axis Bank
        { //AXIS_NB: Login Page
            path: /(.*\/ebpp\/RetShoppingMallSignOn\.aspx)|BANKAWAY/,
            hostname: /www\.(axisbank|axisbiconnect)\.co\.in/,
            state: "UNUSED",
            bank: "AXISNB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[name=CorporateSignonCorpId]')
                       && document.querySelector('input[name=CorporateSignonPassword]');
            },
            action: function() {
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                Gatekeeper.setSessionAttribute("disableAutoFocus", "true"); showUpdateWebviewUber();
                setupLoginPageWithSubmitButton("input[type=password][name=CorporateSignonPassword]",
                                               "input[name=CorporateSignonCorpId]",
                                               "input[id=button1][value=Submit]");
            },

            clickSubmitButton: function() {
                var submitBtn = document.querySelector("input[id=button1][value=Submit]");
                __juspay.clickOnElement(submitBtn);
            },

            showPassword: function() {
               var  passwordValue = document.querySelector("input[type=password][name=CorporateSignonPassword]").value
               if(passwordValue != undefined && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue)
               }
            },
            nextAction: function() {
                var loginIdField = document.querySelector('input[name=CorporateSignonCorpId]');
                var passwordField = document.querySelector('input[name=CorporateSignonPassword]');
                var submitBtn = document.querySelector("input[id=button1][value=Submit]");
                if(document.activeElement == loginIdField)
                {
                    passwordField.focus();
                }
                else
                {
                    if(document.activeElement != passwordField && passwordField.value == "") {
                        passwordField.focus();
                    }
                    else {
                        __juspay.clickOnElement(submitBtn);
                    }
                }
            },
            backButtonPressed: function(){
                var loginIdField = document.querySelector('input[name=CorporateSignonCorpId]');
                var passwordField = document.querySelector('input[name=CorporateSignonPassword]');
                if(loginIdField && passwordField && __uber.isUberEnabled("REDIRECT_DEBIT") && __uber.initSessionCounterIncremental("AXISNBDCR")==1) {
                    callDebitRedirectUber();
                } else {
                    GK.showCancelTransactionDialog();
                }
            }
        },
        { //AXIS_NB: new Login Page added on 28/02/19
            path: /wps\/portal\/rBanking\/AxisSMRetailLogin\/axissmretailpage/,
            hostname: "retail.axisbank.co.in",
            state: "UNUSED",
            bank: "AXISNB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[name=FORM_LOGINID]')
                && document.querySelector('input[name=pwd]')
                && document.querySelector('input[name=SMsubmit]');
            },
            action: function() {
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                setupLoginPageWithSubmitButton("input[name=pwd]","input[name=FORM_LOGINID]","input[name=SMsubmit]");
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[name=SMsubmit]');
                __juspay.clickOnElement(submitBtn);
            },
            showPassword: function() {
                var  passwordValue = document.querySelector('input[name=pwd]').value
                if(passwordValue != undefined && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue)
                }
            },
            nextAction: function() {
                var loginIdField = document.querySelector('input[name=FORM_LOGINID]');
                var passwordField = document.querySelector('input[name=pwd]');
                var submitBtn = document.querySelector('input[name=SMsubmit]');
                if(document.activeElement == loginIdField)
                {
                    passwordField.focus();
                }
                else
                {
                    if(document.activeElement != passwordField && passwordField.value == "") {
                        passwordField.focus();
                    }
                    else {
                        __juspay.clickOnElement(submitBtn);
                    }
                }
            }
        },
        //Handling dom-check-error which comes in intermediate page for AxisNb
        {
            path: /wps\/portal\/rBanking\/AxisSMRetailLogin\/axissmretailpage/,
            hostname: "retail.axisbank.co.in",
            state: "UNUSED",
            bank: "AXISNB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=ipcbuttonsubmit][value=Continue]')
            },
            action: function(){
                window.trackEventv2_0default("acs","info","AXISNB","dom_check_fix_intermediate_page");
            }
        },
        { //AXIS_NB: After Error new Login Page
            path: /wps\/portal\/rBanking\/(AxisSMRetailLogin|axisebanking)\/(axissmretailpage|AxisRetailLogin)/,
            hostname: "retail.axisbank.co.in",
            state: "UNUSED",
            bank: "AXISNB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=loginId][type=text]') &&
                       document.querySelector('input[id=newPassword][type=password]') &&
                       document.querySelector('input[value=Login][type=submit]')
            },
            action: function() {
                var debitCardOptionTab = document.querySelector('li[id=debitCardOption]');
                var userIdOptionTab = document.querySelector('li[id=userIdOption]');
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                setupLoginPageWithSubmitButton("input[type=password][name=pwd]","input[id=loginId]","input[value=Login][type=submit]");

                var _this = this;
                var mpinCallback = function(resp) {
                    resp = JSON.parse(resp);
                    if(resp.continueWithMPIN) {
                        _this.startMpin();
                    } else {
                        Gatekeeper.hideWaitingFragment();
                        setupLoginPageWithSubmitButton("input[type=password][name=pwd]",
                                               "input[id=loginId]",
                                               "input[value=Login][type=submit]");
//                        if(debitPinField) {
//                            debitPinField.addEventListener("focus",function(){
//                                Gatekeeper.requestNumericKeyboardShow();
//                            }, false);
//                            debitCardField.addEventListener("focus",function(){
//                                Gatekeeper.requestNumericKeyboardShow();
//                            }, false);
//                        }
        //                if(debitCardField &&  debitPinField){
        //                    attachPhoneKeyboard(debitPinField);
        //                    attachPhoneKeyboard(debitCardField);
        //                }
                        if(debitCardOptionTab){
                            debitCardOptionTab.addEventListener("click",function(){
                                __juspay.trackPageStatus("INPUT_DC_LOGIN");
                                __juspay.trackAuthMethod("LOGIN");
                                Gatekeeper.removeFragment("Clicked DebitCard Tab");
                            }, false);
                        }
                        if(userIdOptionTab){
                            userIdOptionTab.addEventListener("click",function(){
                                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                                __juspay.trackAuthMethod("LOGIN");
                                setupLoginPageWithSubmitButton("input[type=password][name=pwd]",
                                                               "input[id=loginId]",
                                                               "input[value=Login][type=submit]");
                            }, false);
                        }
                    }
                }

                var data = {data: this.getNBPayload()};
                Gatekeeper.setupMPIN(this.bank, JSON.stringify(data), mpinCallback);
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector("input[value=Login][type=submit]");
                __juspay.clickOnElement(submitBtn);
            },
            nextAction: function() {
                var loginIdField = document.querySelector('input[id=loginId]');
                var passwordField = document.querySelector('input[name=pwd]');
                var submitBtn = document.querySelector("input[value=Login][type=submit]");
                if(document.activeElement == loginIdField){
                    passwordField.focus();
                }
                else{
                    if(document.activeElement != passwordField && passwordField.value == "") {
                        passwordField.focus();
                    }
                    else {
                        __juspay.clickOnElement(submitBtn);
                    }
                }
            },

            getNBPayload: function() {
                var payload = {inputParams: {}, queryParams: {}};
                var inputs = document.getElementsByTagName("input");
                var queryParams = window.location.href.substr(window.location.href.indexOf("?") + 1).split("&");

                for(var i = 0; i < inputs.length; i ++) {
                    if(inputs[i].type === "hidden") {
                        payload.inputParams[inputs[i].name] = inputs[i].value
                    }
                }

                for(var i = 0; i < queryParams.length; i ++) {
                    var key = queryParams[i].split("=")[0];
                    var value = queryParams[i].split("=")[1];
                    payload.queryParams[key] = value;
                }
                return payload;
            },
            startMpin: function() {
                var data = this.getNBPayload();

                var callback = function(resp) {
                    resp = JSON.parse(resp);

                    if(resp.redirectUrl) {
                        window.location.href = resp.redirectUrl;
                    }
                }

                Gatekeeper.startMPIN(this.bank, JSON.stringify(data), callback);
            }
        },
        { //AXIS_NB: remarks page
            path: /\/AxisSMRetailLogin\/axissmrepayments/,
            hostname: "retail.axisbank.co.in",
            state: "UNUSED",
            bank: "AXISNB",
            local: false,
            domCheck: function() {
                //PLease use dom check for fields that are needed for us
               return document.querySelector('input[name="TranRequestManagerFG.ENT_REMARKS"]')
                      && document.querySelector('input[type=Submit][value=Pay]');
            },
            action: function() {
                var remarksField = document.querySelector('input[name="TranRequestManagerFG.ENT_REMARKS"]');
                var submitBtn = document.querySelector('input[type=Submit][value=Pay]');
                //Choosing account used in prev transaction when there are multiple accounts
                var isSelectPresent = document.querySelector('select[name=TranRequestManagerFG\\.INITOR_ACCOUNT]')
                if(isSelectPresent) {
                    try {
                        var span = isSelectPresent.parentElement.parentElement.parentElement.parentElement;
                        if(span) {
                            span.style = "position: relative ; left: -18%"
                        }
                    } catch(err) {
                        window.trackEventv2_0("acs","error","modify_page_error","Failed to Modify Page: " + String(err), "modify_page", "error");
                    }
                }
                var text = document.querySelector('span[class="selectboxit-text"]')
                if(text) {
                    text.innerText = text.innerText.substring(0,20);
                }
                if(isSelectPresent) {
                      accountSelectionPage('select[name=TranRequestManagerFG\\.INITOR_ACCOUNT] option',"AXISNB",'input[type=Submit][value=Pay]',remarksField);
                }
                Gatekeeper.requestKeyboardHide();
            },
            nextAction: function() {
                var submitBtn = document.querySelector('input[type=Submit][value=Pay]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        { //AXIS_NB:OTP page
            path: /\/myportal\/rBanking\/AxisSMRetailLogin\/axissmrepayments/,
            hostname: "retail.axisbank.co.in",
            state: "UNUSED",
            bank: "AXISNB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id=orignipdef]') && document.querySelector('input[type=Submit]');
            },
            action: function() {
                var otpField = document.querySelector("input[id=orignipdef]");
                var otpOnIvr = document.querySelector('input[name = "TranRequestManagerFG.IVR_CALLBACK"]');
                var resendOtp = document.querySelector('input[name="TranRequestManagerFG.RESEND_OTP"]');
                var submitButton = document.querySelector('input[type=Submit]');

                var checkResendOtpDisplay = setInterval(function(){
                    var resendOtp1 = document.querySelector('input[name="TranRequestManagerFG.RESEND_OTP"]');
                    var resendOtpStyle = resendOtp1.style;
                    var resendOtpDisplay = resendOtpStyle.display;
                    if(resendOtpDisplay === "none"){
                        Gatekeeper.disableRegenerateOTP();
                        clearInterval(checkResendOtpDisplay);
                    }
                },1000);

                if(otpOnIvr){
                    Gatekeeper.disableRegenerateOTP();
                    otpOnIvr.addEventListener("click",function(){
                        Gatekeeper.removeFragment("OTP on Call");
                    }, false);
                }

                var accounts = document.querySelector('select[name="TranRequestManagerFG.INITOR_ACCOUNT"]');
                if(accounts.length>=2){
                    Gatekeeper.setShouldAutoSubmitOtp(false);
                }

                var cancelButton = document.querySelector('input[id*=SHP_CANCEL_TRANSACTION]');
                if(cancelButton){
                    cancelButton.addEventListener("click",function(){
                        window.trackEventv2_0("acs","info","dropout_reason","CANCEL_BUTTON_CLICKED", "dropout", "reason");
                    }, false);
                }
                otpFragmentHelper('input[id=orignipdef]','input[name="TranRequestManagerFG.RESEND_OTP"]');
                __juspay.scrollToElement(otpField);
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=orignipdef]", "input[type=SUBMIT]");
            },
            regenerateOtp: function() {
                var generateOtpLink = document.querySelector('input[name="TranRequestManagerFG.RESEND_OTP"]');
                if(generateOtpLink)
                    __juspay.clickOnElement(generateOtpLink);
                    clickOnElementToResetTimer(generateOtpLink,false);
            }
        },
        { //PNB:Expiry validation page
            path: /\/acs-web-v17\/EnrollWeb\/PNB\/server\/AccessControlServer/,
            hostname: /(pdc|sdc)\.enstage-sas\.com/,
            state: "UNUSED",
            local: false,
            bank: "PNBDC",
            domCheck: function() {
                return document.querySelector('input[type=password][name=pin]') &&
                       document.querySelector("input[type=image][src*=btn_submit]");
            },
            action: function() {
                var atmPinField = document.querySelector('input[type=password][name=pin]');
                var masterCard = document.querySelector('img[alt*="Mastercard"]');
                if(masterCard){
                    Gatekeeper.setCardBrand(MASTERCARD);
                }
                __juspay.trackPageStatus("INPUT_ATM");
                __juspay.trackAuthMethod("ATM");
                atmPinField.addEventListener("focus",function(){
                        Gatekeeper.requestPhoneKeyboardShow();
                });
            }
        },
        {//PNB_DC : new otp_page
            path: /\/acs-web-v17\/EnrollWeb\/PNB\/server\/(OtpServer|EnrollServer)/,
            hostname: /(pdc|sdc|acs2|acs2-rdc)\.enstage-sas\.com/,
            state: "UNUSED",
            local: false,
            bank: "PNBDC",
            domCheck: function(){
                return  document.querySelector("input[type=password][id=otpValue]") &&
                document.querySelector('form[action*="PNB"]') &&
                document.querySelector("input[type=image][src*=btn_submit]");
            },
            action: function() {
                var errorElt = window.find("OTP verification failed. Please enter valid OTP sent by the bank.", true, false, true);
                if(errorElt){
                    if(errorElt.innerHTML.match(/One Time Password .*/)) {
                        __juspay.trackPageStatus("INVALID_OTP");
                        Gatekeeper.removeFragment("PNB card wrong otp");
                    }
                } else {
                    var mastercard = document.querySelector('img[src*="Mastercard_Securecode_bg"]');
                    if(mastercard) {
                        Gatekeeper.setCardBrand(MASTERCARD);
                    }
                    otpFragmentHelper('input[id=otpValue]','a[href*=resend_otp]');
                }
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[type=password][id=otpValue]", "input[type=image][src*=btn_submit]");
                if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]!="ios"){
                    Gatekeeper.removeFragment('OTP submitted via OTP fragment');
                }
            },
            regenerateOtp: function() {
                var generateOtpLink = document.querySelector('a[href*=resend_otp]');
                if(generateOtpLink)
                    __juspay.clickOnElement(generateOtpLink);
            }
        },
        {//PNB_DC : otp destination page
            path: /\/acs-web-v17\/EnrollWeb\/PNB\/server\/(OtpServer|EnrollServer)/,
            hostname: /(pdc|sdc|acs2|acs2-rdc)\.enstage-sas\.com/,
            state: "UNUSED",
            local: false,
            bank: "PNBDC",
            domCheck: function(){
                return  document.querySelector('input[value="toMobile"]');
            },
            action: function() {
                __juspay.trackPageStatus("OTP_DESTINATION_PAGE");
            }
        },
        //Airtel Bank OTP page
        {
           path: /\/acs-web-v27\/EnrollWeb\/MIAirtel\/server\/(EnrollServer|OtpServer)/,
           hostname: "mi16-pdc.enstage-sas.com",
           state: "UNUSED",
           local: false,
           bank: "AIRTEL",
           domCheck: function(){
               return document.querySelector('input[id=otpValue]') &&
                   document.querySelector('input[name="submits"][class="btn primary"]') &&
                   document.querySelector('img[src*="Airtel-PaymentBank"]') &&
                   document.querySelector('a[onclick*="resend_otp"]');
           },
           action: function(){
               var otpField = document.querySelector('input[id=otpValue]');
               var masterCard = document.querySelector('img[src*="Mastercard"]');
               if(otpField) {
                   otpFragmentHelper('input[id=otpValue]','a[onclick*="resend_otp"]');
               }
               if(masterCard){
                    Gatekeeper.setCardBrand(MASTERCARD);
               }
           },
           submitOtp: function(otp){
               submitOtpHelper(otp, 'input[id=otpValue]', 'input[name="submits"][class="btn primary"]');
               Gatekeeper.removeFragment("OTP Submitted");
           },
           regenerateOtp: function(){
               var regenerateBtn = document.querySelector('a[onclick*="resend_otp"]')
               if(regenerateBtn){
                   __juspay.clickOnElement(regenerateBtn);
               }
           }
        },
        {
            path: /\/ecom\/v2/,
            hostname: "ecom.airtelbank.com",
            state: "UNUSED",
            local: false,
            bank: "AIRTEL",
            domCheck: function() {
                return document.querySelector('button[class="go-btn ng-scope"]') &&
                       document.querySelectorAll('img[src*="airtel-bank-logo.23710c66"]');
            },
            action: function(){
                var goBtn = document.querySelector('button[class="go-btn ng-scope"]');
                goBtn.addEventListener("click",function(){
                    var otpFieldChecker = setInterval(function(){
                        var otpField = document.querySelector('input[name=otp][ng-model="user.otp"][class="ng-pristine ng-untouched ng-valid ng-isolate-scope ng-valid-maxlength"]');
                        if(otpField){
                            clearInterval(otpFieldChecker);
                            otpFragmentHelper('input[name=otp][ng-model="user.otp"][class="ng-pristine ng-untouched ng-valid ng-isolate-scope ng-valid-maxlength"]');
                            var resendChecker = setInterval(function(){
                                var resendBtn = document.querySelectorAll('button[class="resend-otp ng-scope"]')[1];
                                if(resendBtn){
                                    clearInterval(resendChecker);
                                    Gatekeeper.enableRegenerateOTP();
                                }
                            },500)
                        }
                    },500)
                }, false);
            },
            submitOtp: function(otp) {
                var otpField = document.querySelectorAll('input[name=otp][ng-model="user.otp"]')[1];
                var submitButton = document.querySelectorAll('button[type="submit"]')[1];
                if(submitButton){
                    otpField.value=otp;
                    document.querySelectorAll('input[name=otp][ng-model="user.otp"]')[1].dispatchEvent(new Event("change"));
                    try{
                        __juspay.clickOnElement(submitButton);
                        Gatekeeper.removeFragment('OTP submitted');
                    }catch(err){
                        Gatekeeper.removeFragment('Submit Button Dom Changed - Please Check');
                    }
                }
            },
            regenerateOtp: function(){
              var regenerateBtn = document.querySelectorAll('button[class="resend-otp ng-scope"]')[1];
              if(regenerateBtn){
                  __juspay.clickOnElement(regenerateBtn);
              }
            }
        },
        {//CUB LOGIN PAGE
            path: /\/servletWrapper\/ibs\.servlets\.IBSWAPServlet/,
            hostname:"www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck : function(){
                return document.querySelector("input[id=cif][type=text]") &&
                    document.querySelector("input[id=psswd1][type=tel]") &&
                    document.querySelector("input[class=btn][type=submit]");
            },
            action: function(){
                var userId = document.querySelector("input[id=cif][type=text]");
                var mpinId = document.querySelector("input[id=psswd1][type=tel]");
                var submitBtn = document.querySelector("input[class=btn][type=submit]");
                var userId2 = document.querySelector("input[id=uname][type=text]");
                var mpinId2 = document.querySelector("input[id=psswd2][type=password]");
                var radio1 = document.querySelector("input[type=radio][id=logintype2]");
                var radio2 = document.querySelector("input[type=radio][id=logintype1]");
                if(__juspay.isMobUiEnabled("MOBUI_CUBNB")){
                    try{
                        if(userId && submitBtn){
                            userId.style.marginTop = "20px";
                            mpinId.style.marginTop = "20px";
                            userId.style.borderTop = " 1px groove #d3d3d3";
                            userId.style.borderLeft = " 1px groove #d3d3d3";
                            userId.style.borderRight = " 1px solid #d3d3d3";
                            mpinId.style.borderTop = " 1px groove #d3d3d3";
                            mpinId.style.borderLeft = " 1px groove #d3d3d3";
                            mpinId.style.borderRight = " 1px solid #d3d3d3";
                            submitBtn.style.width = "70%";
                        }
                        if(userId2 && submitBtn){
                            userId2.style.marginTop = "20px";
                            mpinId2.style.marginTop = "20px";
                            userId2.style.borderTop = " 1px groove #d3d3d3";
                            userId2.style.borderLeft = " 1px groove #d3d3d3";
                            userId2.style.borderRight = " 1px solid #d3d3d3";
                            mpinId2.style.borderTop = " 1px groove #d3d3d3";
                            mpinId2.style.borderLeft = " 1px groove #d3d3d3";
                            mpinId2.style.borderRight = " 1px solid #d3d3d3";
                            submitBtn.style.width = "70%";
                        }
                    }catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                    }
                }
                if(radio1 && radio1.checked == true){
                    __juspay.trackPageStatus("INPUT_MPIN_LOGIN");
                    __juspay.trackAuthMethod("MPIN");
                    Gatekeeper.removeFragment("Reached Mpin login page of CUB Netbanking");
                    setupLoginPageWithSubmitButton("input[id=psswd1][type=tel]", "input[id=cif][type=text]", "input[class=btn][type=submit]");
                }else if(radio2 && radio2.checked == true && mpinId2){
                    Gatekeeper.removeFragment("CUBNB focused on password field after customer id");
                    mpinId2.blur();
                    attachAlphaNumericPasswordHelper(mpinId2);
                    mpinId2.focus();
                    __juspay.scrollToElement(mpinId2);
                }
                radio2.addEventListener("click", function(){
                    __juspay.trackPageStatus("INPUT_NB_LOGIN");
                    __juspay.trackAuthMethod("LOGIN");
                    Gatekeeper.removeFragment("CUB Netbanking");
                    if(userId){
                        userId.blur();
                        Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                        var bankCustomerId = Gatekeeper.getBankCustomerId();
                        if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined"){
                            bankCustomerId = "";
                            userId.focus();
                        }
                        userId.value = bankCustomerId;
                    }
                })
                radio1.addEventListener("click", function(){
                    __juspay.trackPageStatus("INPUT_MPIN_LOGIN");
                    __juspay.trackAuthMethod("MPIN");
                    setupLoginPageWithSubmitButton("input[id=psswd1][type=tel]", "input[id=cif][type=text]", "input[class=btn][type=submit]");
                })
            },
            nextAction: function(){
                var custId = document.querySelector("input[id=cif][type=text]");
                var mpinId = document.querySelector("input[id=psswd1][type=tel]");
                var submitBtn = document.querySelector("input[class=btn][type=submit]");
                var radio1 = document.querySelector("input[type=radio][id=logintype2]");
                var radio2 = document.querySelector("input[type=radio][id=logintype1]");
                if(radio2 && radio2.checked == true){
                    __juspay.clickOnElement(submitBtn);
                }
                if(radio1 && radio1.checked == true){
                    if(document.activeElement == custId){
                        mpinId.focus();
                    }
                }
            },
            clickSubmitButton : function(){
                var submitBtn = document.querySelector("input[class=btn][type=submit]");
                var radio2 = document.querySelector("input[type=radio][id=logintype1]");
                var radio1 = document.querySelector("input[type=radio][id=logintype2]");
                if(radio1 && radio1.checked == true){
                    __juspay.clickOnElement(submitBtn);
                    //auto clicking ok button on popup dialog after 3 sec
                    setTimeout(function(){
                        autoClickButton('input[type=button][value=OK]','SUPPRESS_ERROR_DIALOG',this.bank)
                    },3000);
                }
                if(radio2 && radio2.checked == true){
                    __juspay.clickOnElement(submitBtn);
                    setTimeout(function(){
                        autoClickButton('input[type=button][value=OK]','SUPPRESS_ERROR_DIALOG',this.bank)
                    },3000);
                }
            }
        },
        //account selection page
        {
            path: /\/servletWrapper\/ibs\.servlets\.(IBSWAPServlet|IBSGenericPaymentServlet)/,
            hostname:"www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck : function(){
                return (document.querySelector("select[name=fromAcc]") &&
                    document.querySelector("input[id=gobutton][type=button]") &&
                    (document.querySelector("input[type=password][name=password]") ||
                    document.querySelector("input[type=tel][name=password1]")) &&
                    document.querySelector("input[type=button][value=Cancel]"));
            },
            action : function(){
                Gatekeeper.removeFragment("CUB netbanking account selection page");
                var accountSelector = document.querySelector("select[name=fromAcc]");
                var passwordField = document.querySelector("input[type=password][name=password]");
                var mpinField = document.querySelector("input[type=tel][name=password1]");
                __juspay.trackPageStatus("CHOOSE_ACCOUNT");
                if(accountSelector.childElementCount==2){
                    accountSelector.children[1].selected = true;
                }
                if(passwordField){
                    passwordField.blur();
                    attachAlphaNumericPasswordHelper(passwordField);
                    passwordField.focus();
                }else if(mpinField){
                    mpinField.blur();
                    attachPhonePasswordHelper(mpinField);
                    mpinField.focus();
                }
            },
            clickSubmitButton : function() {
                var submitButton1 = document.querySelector("input[id=gobutton][type=button]");
                __juspay.clickOnElement(submitButton1);
                setTimeout(function(){
                    autoClickButton('input[type=button][value=OK]','SUPPRESS_ERROR_DIALOG',this.bank);
                },3000);
            }
        },
        //Auth selection page
        {
            path: /\/servletWrapper\/ibs\.servlets\.IBSMFAAuthenticationServlet/,
            hostname:"www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck : function(){
                return (document.querySelector("input[type=radio][name=MFAtype]") &&
                    document.querySelector("input[type=button][value=Proceed]"));
            },
            action : function(){
                __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                Gatekeeper.showNetbankingDefaultFragment();
                Gatekeeper.changeNextActionText("Proceed");
            },
            nextAction : function(){
                var proceedBtn = document.querySelector("input[type=button][value=Proceed]");
                __juspay.clickOnElement(proceedBtn);
            }
        },
        //CUB OTP PAGE
        {
            path: /\/servletWrapper\/ibs\.servlets\.IBSWAPServlet\?HandleID\=OTP_CHECK/,
            hostname:"www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck : function(){
                return document.querySelector("input[type=tel][name=TempPwd]") &&
                    document.querySelector("img[src*=onlinecub1]") &&
                    document.querySelector("input[type=button][value=Submit]");
            },
            action : function() {
                otpFragmentHelper("input[type=tel][name=TempPwd]", "a[id=reotp]");
            },
            submitOtp : function(otp){
                submitOtpHelper(otp, "input[type=tel][name=TempPwd]", "input[type=button][value=Submit]");
                Gatekeeper.removeFragment("OTP Submitted");
            },
            regenerateOtp : function(){
                var resendLink = document.querySelector("a[id=reotp]");
                if(resendLink){
                    __juspay.clickOnElement(resendLink);
                }
            }
        },
        //CUBNB Amount Confirmation Page
        {
            path : /\/servletWrapper\/ibs\.servlets\.IBSMFAAuthenticationServlet/,
            hostname : "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                    return document.querySelector("img[src*=onlinecub1]") &&
                        document.querySelector("input[type=button][value=Back]") &&
                        document.querySelector("input[type=button][id=gobutton]");
            },
            action : function(){
                __juspay.trackPageStatus("AMOUNT_CONFIRMATION");
                if(!__juspay.isMobUiEnabled("AUTO_CLICK_CUBNB")){
                    Gatekeeper.showNetbankingDefaultFragment();
                    Gatekeeper.changeNextActionText("Submit");

                }else{
                    autoClickButton('input[type=button][id=gobutton]','AMOUNT_CONFIRMATION_PAGE',this.bank);
                }
            },
            nextAction : function(){
                var submitButton = document.querySelector("input[type=button][id=gobutton]");
                __juspay.clickOnElement(submitButton);
            }
        },
        // CUBNB ATM CARD PIN PAGE
        {
            path : /\/servletWrapper\/ibs\.servlets\.IBSMFAAuthenticationServlet/,
            hostname : "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                return (document.querySelector("input[id=cardNo][name=cardNo]") &&
                    document.querySelector("input[id=cardExp][name=cardExp]") &&
                    document.querySelector("input[id=Pin][name=Pin]") &&
                    document.querySelector("input[type=button][value=Submit]"));
            },
            action : function(){
                Gatekeeper.removeFragment("reached Atm pin page");
                var atmCardField = document.querySelector("input[id=cardNo][name=cardNo");
                var expiryField = document.querySelector("input[id=cardExp][name=cardExp]");
                var atmPinField = document.querySelector("input[id=Pin][name=Pin]");
                __juspay.trackPageStatus("ATMPIN");
                __juspay.trackAuthMethod("ATM_PIN");
                Gatekeeper.removeFragment("Focused on ATM Card field");
                focusElement(atmCardField);
                attachPhonePasswordHelper(atmCardField);
                atmCardField.blur();
                atmCardField.focus();
                Gatekeeper.changeNextActionText("Continue");
                atmCardField.addEventListener("click", function(){
                    Gatekeeper.changeNextActionText("Continue");
                })
                expiryField.addEventListener("click", function(){
                    Gatekeeper.removeFragment("Focused on ATM expiry field");
                    expiryField.focus();
                    attachPhonePasswordHelper(expiryField);
                    expiryField.blur();
                    expiryField.focus();
                    Gatekeeper.changeNextActionText("Continue");
                })
                atmPinField.addEventListener("click", function(){
                    Gatekeeper.removeFragment("Focused on ATM PIN field");
                    attachPhonePasswordHelper(atmPinField);
                    atmPinField.blur();
                    atmPinField.focus();
                    Gatekeeper.changeNextActionText("Submit");
                })
            },
            clickSubmitButton : function(){
                var atmCardField = document.querySelector("input[id=cardNo][name=cardNo");
                var expiryField = document.querySelector("input[id=cardExp][name=cardExp]");
                var atmPinField = document.querySelector("input[id=Pin][name=Pin]");
                var submitButton = document.querySelector("input[type=button][value=Submit]");
                if(document.activeElement == atmCardField){
                    Gatekeeper.removeFragment("Focused on ATM expiry field");
                    expiryField.focus();
                    attachPhonePasswordHelper(expiryField);
                    expiryField.blur();
                    expiryField.focus();
                    Gatekeeper.changeNextActionText("Continue");
                } else if(document.activeElement == expiryField){
                    Gatekeeper.removeFragment("Focused on ATM PIN field");
                    atmPinField.focus();
                    attachPhonePasswordHelper(atmPinField);
                    atmPinField.blur();
                    atmPinField.focus();
                    Gatekeeper.changeNextActionText("Submit");
                } else if(document.activeElement == atmPinField){
                    __juspay.clickOnElement(submitButton);
                    setTimeout(function(){
                        autoClickButton('input[type=button][value=OK]','SUPPRESS_ERROR_DIALOG',this.bank)
                    },3000);
                }
            }
        },
       //Redirection Page
        {
            path : /\/servletWrapper\/ibs\.servlets\.IBSMFAAuthenticationServlet\?HandleID\=OTP_MFA/,
            hostname : "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                     return document.querySelector("a[id=reotp]") &&
                         document.querySelector("input[type=tel][name=TempPwd]");
            },
            action : function(){
                Gatekeeper.removeFragment("Reached redirection page");
                __juspay.trackPageStatus("REDIRECTION_PAGE");
            }
        },
       //transaction time out page
        {
            path : /\/servletWrapper\/ibs\.servlets\.IBSMFAAuthenticationServlet\?EncReq/,
            hostname : "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                     return document.querySelector("input[id=MeNuShOw][type=button]") &&
                         document.querySelector("input[class=button][value=Back]") &&
                         document.querySelector("font[color=red]") &&
                         (document.querySelector("font[color=red]").innerHTML.indexOf('Your request could not be processed') != -1);
            },
            action : function(){
                Gatekeeper.removeFragment("Reached expired page");
                __juspay.trackPageStatus("TXN_EXPIRED");
            }
        },
        //Forgot User ID page
        {
            path : /\/servletWrapper\/ibs\.servlets\.IBSOnlineForgotPasswordServlet/,
            hostname : "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                return  document.querySelector("input[name=accountNumber][type=tel]") &&
                    document.querySelector("input[name=mobileNumber][type=tel]") &&
                    document.querySelector("input[class=button][value=Back]") &&
                    document.querySelector("input[class=button][value=Submit]");
            },
            action : function(){
                Gatekeeper.removeFragment("Reached Forgot UserID page");
                var accountIdField = document.querySelector("input[name=accountNumber][type=tel]");
                var mobileNumberField = document.querySelector("input[name=mobileNumber][type=tel]");
                var backBtn = document.querySelector("input[class=button][value=Back]");
                var submitButton = document.querySelector("input[class=button][value=Submit]");
                __juspay.trackPageStatus("FORGOT_LOGIN_DETAILS");
                accountIdField.blur();
                focusElement(accountIdField);
                Gatekeeper.showNetbankingDefaultFragment();
                accountIdField.addEventListener("click", function(){
                    Gatekeeper.removeFragment("Clicked on accountId field");
                    accountIdField.blur();
                    Gatekeeper.showNetbankingDefaultFragment();
                    accountIdField.focus();
                })
                mobileNumberField.addEventListener("click", function(){
                    Gatekeeper.removeFragment("Clicked on mobilenumber field");
                    mobileNumberField.blur();
                    Gatekeeper.showNetbankingDefaultFragment();
                    Gatekeeper.changeNextActionText("Submit");
                    mobileNumberField.focus();
                })
            },
            nextAction : function(){
                var accountIdField = document.querySelector("input[name=accountNumber][type=tel]");
                var mobileNumberField = document.querySelector("input[name=mobileNumber][type=tel]");
                var submitButton = document.querySelector("input[class=button][value=Submit]");
                if(document.activeElement == accountIdField){
                    Gatekeeper.removeFragment("Focused on mobilenumber field");
                    mobileNumberField.blur();
                    Gatekeeper.showNetbankingDefaultFragment();
                    Gatekeeper.changeNextActionText("Submit");
                    mobileNumberField.focus();
                }else if(document.activeElement == mobileNumberField){
                    __juspay.clickOnElement(submitButton);
                    setTimeout(function(){
                        autoClickButton('input[type=button][value=OK]','SUPPRESS_ERROR_DIALOG',this.bank)
                    },3000);
                }
            }
        },
        //CUBNB Corp Login
        {
            path : /\/servlet\/cb\.servlets\.CBLoginServlet/,
            hostname : "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                return document.querySelector("input[name=cid][type=text]") &&
                    document.querySelector("input[name=uid][type=text]") &&
                    document.querySelector("button[name=btClear][type=submit]");
            },
            action : function(){
                var corpId = document.querySelector("input[name=cid][type=text]");
                var userId = document.querySelector("input[name=uid][type=text]");
                var footer = document.querySelectorAll('div');
                if(__juspay.isMobUiEnabled("MOBUI_CUBNB")){
                    try{
                        footer[37].style.position= "static";
                        }catch(err){
                            __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                        }
                }
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                corpId.blur();
                Gatekeeper.showNetbankingDefaultFragment();
                focusElement(corpId);
                corpId.addEventListener("focus", function(){
                    Gatekeeper.removeFragment("CUBNB focused on corp id");
                    corpId.blur();
                    Gatekeeper.showNetbankingDefaultFragment();
                    corpId.focus();
                })
                userId.addEventListener("focus", function(){
                    Gatekeeper.removeFragment("CUBNB focused on user id");
                    userId.blur();
                    Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                    userId.focus();
                })
            },
            nextAction : function(){
                var userId = document.querySelector("input[name=uid][type=text]");
                var corpId = document.querySelector("input[name=cid][type=text]");
                var submitBtn = document.querySelector("button[name=btClear][type=submit]");
                var bankCustomerId = Gatekeeper.getBankCustomerId();
                if(document.activeElement == corpId){
                    Gatekeeper.removeFragment("Focused on User id field");
                    userId.blur();
                    Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                    userId.focus();
                    if(bankCustomerId==="undefined" || typeof( bankCustomerId)=="undefined") {
                        bankCustomerId = "";
                        userId.blur();
                        userId.focus();
                    }
                    userId.value = bankCustomerId;
                }else if(document.activeElement == userId){
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        //CUBNB login1 page
        {
            path: /\/servlet\/ibs\.servlets\.IBSLoginServlet/,
            hostname: "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                return document.querySelector('input[name=uid]') &&
                       document.querySelector('input[name=btClear][type="submit"]')
            },
            action: function(){
                var userIdField = document.querySelector('input[name=uid]');
                var submitBtn = document.querySelector('input[name=btClear][type="submit"]');
                var ipinField = document.querySelector('input[name=pwd][type=password]');
                if(!ipinField){
                     setupLoginPageWithUsernameAndSubmitButton("input[name=uid]",'input[name=btClear][type="submit"]');
                     __juspay.trackPageStatus("INPUT_LOGIN_USERNAME");
                     __juspay.trackAuthMethod("LOGIN");
                }
                var checkIpinField = setInterval(function(){
                    var ipinField = document.querySelector('input[name=pwd][type=password]');
                    if(ipinField){
                        __juspay.trackPageStatus("INPUT_LOGIN_PASSWORD");
                        __juspay.trackAuthMethod("PASSWORD");
                        passwordFragmentHelper('input[name=pwd][type=password]');
                        clearInterval(checkIpinField);
                    }
                },1000);
            },
            clickSubmitButton: function() {
                var loginField = document.querySelector('input[name=uid]');
                var passwordField = document.querySelector('input[name=pwd][type=password]');
                var submitBtn = document.querySelector('input[name=btClear][type="submit"]');
                if(document.activeElement == loginField)
                {
                    passwordField.focus();
                }
                else
                {
                    if(document.activeElement != passwordField && passwordField.value == "") {
                        passwordField.focus();
                    }
                    else {
                        __juspay.clickOnElement(submitBtn);
                    }
                }
            },
            nextAction: function() {
                var loginBtn = document.querySelector('input[name=btClear][type="submit"]');
                if (loginBtn){
                    __juspay.clickOnElement(loginBtn);
                }
            }
        },
        // CUBNB account selection, remarks and transaction password page
        {
            path: /\/servlet\/ibs\.servlets\.IBSOnlinePaymentServlet/,
            hostname: "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                return  document.querySelector('input[name="remarks"][type="text"]') &&
                        document.querySelector('select[name="fromAcc"]')
            },
            action: function() {
                __juspay.trackPageStatus("INPUT_REMARKS");
                var txnPwdField = document.querySelector('input[name="password"][type="password"]');
                var remarksField = document.querySelector('input[name="remarks"][type="text"]');
//                accountSelectionPage('select[name="fromAcc"] option',"CUBNB",'',remarksField);
                Gatekeeper.requestKeyboardHide();
                if(txnPwdField) {
                    passwordFragmentHelper('input[name="password"][type="password"]');
                }
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('img[src="/img/submit.gif"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //CUBNB OTP/Grid Selection page
        {
            path: /\/servlet\/ibs\.servlets\.IBSMFAAuthenticationServlet/,
            hostname: "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                return document.querySelector('input[value="OTP"][type="radio"]') &&
                       document.querySelector('input[value="GRID"][type="radio"]')
            },
            action: function(){
                var otpField = document.querySelector('input[name="otppassword"][type="password"]');
                Gatekeeper.showNetbankingDefaultFragment();
                Gatekeeper.changeNextActionText("Submit");
                __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
            },
            nextAction: function(){
                var submitBtn = document.querySelector('a[href="javascript:callServlet()"]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn)
                }
            }
        },
        //CUBNB OTP page
        {
            path: /\/servlet\/ibs\.servlets\.IBSMFAAuthenticationServlet/,
            hostname: "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck:function(){
                return document.querySelector('input[name="otppassword"][type="password"]') &&
                       document.querySelector('a[href="javascript:callServlet(\'OTP_VERIFY_MFA\')"]')
            },
            action: function(){
                otpFragmentHelper('input[name="otppassword"]','a[href="javascript:callServlet(\'MOB_RESEND_MFA\')"]');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp,'input[name="otppassword"]', 'a[href="javascript:callServlet(\'OTP_VERIFY_MFA\')"]');
            },
            regenerateOtp: function() {
                var resendLink = document.querySelector('a[href="javascript:callServlet(\'MOB_RESEND_MFA\')"]');
                if(resendLink){
                    __juspay.clickOnElement(resendLink);
                }
            }
        },
        {   // CUBNB GRID PAGE
            path: /\/servlet\/ibs\.servlets\.IBSMFAAuthenticationServlet/,
            hostname: "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck:function(){
                return document.querySelector('a[href="javascript:callServlet(\'GRID_VERIFY_MFA\')"]') &&
                       document.querySelector('input[name="GridPassword1"][type="text"]')
            },
            action: function(){
                Gatekeeper.showNetbankingDefaultFragment();
                Gatekeeper.changeNextActionText("Submit");
                if(document.title === 'Grid Code'){
                    Gatekeeper.removeFragment("Grid Page reached");
                    var grid1 = document.querySelector('input[name="GridPassword1"][type="text"]');
                    var grid2 = document.querySelector('input[name="GridPassword2"][type="text"]');
                    var grid3 = document.querySelector('input[name="GridPassword3"][type="text"]');
                    if(grid1 && grid2 && grid3){
                        __juspay.trackPageStatus("INPUT_GRID_VALUE");
                        __juspay.trackAuthMethod("GRID_VALUE");
                        grid1.blur();
                        focusElement(grid1);
                    }
                }
            },
            nextAction: function(){
                var submitBtn = document.querySelector('a[href="javascript:callServlet(\'GRID_VERIFY_MFA\')"]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn)
                }
            }
        },
        //CUBNB Confirm page/last page
        {
            path: /\/servlet\/ibs\.servlets\.IBSMFAAuthenticationServlet/,
            hostname: "www.onlinecub.net",
            state: "UNUSED",
            local: false,
            bank: "CUBNB",
            domCheck: function(){
                return document.querySelector('img[src="/img/confirm.gif"]');
            },
            action: function(){
                Gatekeeper.removeFragment("Reached Confirmation Page");
                Gatekeeper.showNetbankingDefaultFragment();
                Gatekeeper.changeNextActionText('CONFIRM');
                __juspay.trackPageStatus("CONFIRM");
            },
            nextAction: function(){
                var submitBtn = document.querySelector('img[src="/img/confirm.gif"]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        {//CITI-BOOKMYSHOW-OTP PAGE
            path: /\/payserv\/doBillDeskOTP\.aspx/,
            hostname: /(services\.in|services-in)\.bookmyshow\.com/,
            state: "UNUSED",
            local: false,
            bank: "CITI",
            domCheck: function(){
                return document.querySelector('input[name="txtOTP"][id="txtOTP"]') &&
                    document.querySelector('a[class="cta_confirm"][id="btnSubmitOTP"]') &&
                    document.querySelector('img[title="Citibank"]');
            },
            action: function(){
                otpFragmentHelper('input[name="txtOTP"][id="txtOTP"]','a[class="resent_otp"][id="btnresentotp"]');
                var cvv = document.querySelector('input[id=txtCVV]');
                if(cvv)
                    cvv.addEventListener('click',function(){Gatekeeper.hideAssistanceFragment()});
            },
            regenerateOtp: function() {
                var resendOtpLink = document.querySelector('a[class="resent_otp"][id="btnresentotp"]');
                if(resendOtpLink)
                    __juspay.clickOnElement(resendOtpLink);
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, 'input[name="txtOTP"][id="txtOTP"]', 'a[class="cta_confirm"][id="btnSubmitOTP"]');
            }
        },
        {
            //CITI_cc/dc New page
            path: /\/AcsAuthenticationService\/authentication\/initiate/,
            hostname: "www.citibank.co.in",
            state: "UNUSED",
            local: false,
            bank: "CITI",
            domCheck: function() {
                return (document.querySelector('input[id=ipincode]') && document.querySelector('a[onclick*=validateIpinValue]')) ||
                        (document.querySelector('input[id="otpcode"]') && document.querySelector('a[onclick*=validateOtpValue]'));
            },
            action: function() {
                var ipinField= document.querySelector('input[id=ipincode]');
                var otpLink = document.querySelector('a[href="javascript:void(0)"][title="OTP (One Time Password)"]');
                var ipinLink= document.querySelector('a[href="javascript:void(0)"][title="IPIN (Internet Pin)"]');
                var firstTime =1;
                var otpDivision = document.querySelector('li[id=otpLink]');
                var ipinDivision =  document.querySelector('li[id=ipinLink]');
                if(otpDivision && otpDivision.style.display != "none"){
                    if(otpLink){
                        otpLink.addEventListener("click",function(){
                            if(firstTime==1){
                                 otpFragmentHelper("input[id=otpcode]","a[href*=modal]");
                                 firstTime =0;
                            }
                        });
                        if(__juspay.isMobUiEnabled("MOBUI_CITI")) {
                            try{
                                addMetaTag();
                                var otpField = document.querySelector('input[class = "inputBoxLarge"]');
                                var resendBlock = document.querySelector('p[class = "padtop1"]');
                                var btnBlock = document.querySelector('div[class = "footCont3"]');
                                var formFieldText = document.querySelector('div[class = "formFieldTxt1"]').childNodes[1];
                                var nextBtn = document.querySelector('ul[id = "footerlink"]').childNodes[1];
                                var cancelBtn = document.querySelector('li[class = "bggrey"]');
                                var otpText = document.querySelector('div[class="formbar1"]').childNodes[1];
                                var elements_list = [resendBlock.childNodes[2]];
                                __juspay.removeElements(elements_list);
                                formFieldText.style.color = "black";
                                btnBlock.style.marginTop = "1%";
                                resendBlock.style.marginTop = "12px";
                                resendBlock.style.textAlign = "end";
                                resendBlock.childNodes[1].innerText = "Resend OTP";
                                resendBlock.childNodes[1].style.textDecoration = "underline";
                                resendBlock.childNodes[1].style.fontSize = "110%";
                                otpField.style.width = "100%";
                                otpField.style.fontSize = "130%";
                                nextBtn.style.marginRight = "13%";
                                nextBtn.style.width = "30%";
                                nextBtn.style.padding  = "2%";
                                cancelBtn.style.width = "30%";
                                cancelBtn.style.padding = "2%";
                                otpText.style.fontSize = "13px";
                                otpText.style.fontWeight = "600";
                            }
                            catch(err){
                                __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                            }
                        }
                    }
                }
                if(ipinDivision && ipinDivision.style.display != "none"){
                    if(ipinField ){
                        attachAlphaNumericPasswordHelper(ipinField);
                    }
                    if(ipinLink){
                        ipinLink.addEventListener("click",function(){
                            if(ipinField && document.activeElement != ipinField){
                                focusElement(ipinField);
                            }
                            firstTime =1;
                        },false);
                    }
                }else{
                    if(otpLink)
                        __juspay.clickOnElement(otpLink);
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('a[onclick*=validateIpinValue]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,"input[id=otpcode]","a[onclick*=validateOtpValue]");
            },
            regenerateOtp: function(){
                var resend = document.querySelector("a[href*=modal]");
                if(resend){
                    __juspay.clickOnElement(resend);
                }
            }
        },
        //SCB Cards support: Added on 18-04-2017
        {
            path: /\/acspage\/cap\?RID/,
            hostname: "cardsecurity.standardchartered.com",
            state: "UNUSED",
            local: false,
            bank: "SCB",
            domCheck: function() {
                return true;
            },
            action: function(){
                //Adding Interval since some field is not completely getting loaded
                var otpFieldChecker = setInterval(function(){
                    var otpField = document.querySelector('input[type=password][id=enterPIN]');
                    var creditCardImage = document.querySelector('img[src*=SCB_CREDIT_VBV]');
                    var debitCardImage = document.querySelector('img[src*=SCB_DEBIT_VBV]');
                    if(otpField) {
                        clearInterval(otpFieldChecker);
                        if(creditCardImage){
                            window.trackEventv2_0default("acs","info","SCB","CreditCard");
                        }
                        if(debitCardImage){
                            window.trackEventv2_0default("acs","info","SCB","DebitCard");
                        }
                        otpFragmentHelper('input[id=enterPIN]',"");
                    }
                },500);
            },
            //Angualr JS: Inbuilt function from bank js, hard-coding the post values
            submitOtp: function(otp){
                var pin = {};
                pin.action = "validateOTP";
                pin.value = escape(otp);
                pin.locale = "en_US";
                $("#pin").val(JSON.stringify(pin));
                document.passwdForm.submit() // After assigning all values, submitting the password form
                Gatekeeper.removeFragment("Submitted OTP using OTP Fragment");
            }
        },
        //SCB NB support
        {
            path: /\/nfs\/(ddpayments_redirection_login|login)/,
            hostname: "ibank.standardchartered.co.in",
            state: "UNUSED",
            local: false,
            bank: "SCBNB",
            domCheck: function() {
                return document.querySelector('input[name="j_username"][type="text"]') &&
                       document.querySelector('input[name="j_password"][type="password"]') &&
                       document.querySelector('input[name="Login"][type="submit"]');
            },

            action: function() {
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                setupLoginPageWithSubmitButton('input[name="j_password"][type="password"]',
                                               'input[name="j_username"][type="text"]',
                                               'input[name="Login"][type="submit"]');
                if(__juspay.isMobUiEnabled("MOBUI_SCBNB")){
                    try {
                        var extraDiv1 = document.querySelectorAll('table[class=tbl_layout] > tbody > tr')[0];
                        var extraDiv2 = document.querySelectorAll('table[class=tbl_layout] > tbody > tr')[4];
                        var extraDiv3 = document.querySelector('#command > table > tbody > tr > td > table > tbody > tr:nth-child(1) > td:nth-child(1)');
                        var extraDiv4 = document.querySelector('#command > table > tbody > tr > td > table > tbody > tr:nth-child(1) > td:nth-child(3)');
                        var extraDiv5 = document.querySelector('#command > table > tbody > tr > td > table > tbody > tr:nth-child(2) > td > table > tbody:nth-child(4) > tr:nth-child(1)');
                        var extraDiv6 = document.querySelectorAll('table[class=tbl_layout] > tbody > tr')[5];
                        var extraDiv7 = document.querySelector('td.tbl_content_top.txt_note_disclamer').parentNode;
                        var extraBreak = document.querySelector('table[class=tbl_layout] > tbody > tr > td > br');
                        var extraHeader = document.querySelector('#command > h1');
                        var table = document.querySelectorAll('table > tbody > tr > td')[0];
                        var mainBody = document.querySelectorAll('table[class=tbl_layout]');
                        var extraDiv8 = document.querySelector('td[class=tbl_sprt_bottom]').parentNode;
                        var extraDiv9 = document.querySelectorAll('td[class=tbl_header]')[0];
                        var extraDiv10 = document.querySelectorAll('td[class=tbl_content_top]')[3];
                        var extraDiv11 = document.querySelectorAll('td[class=tbl_content_middle]')[1];
                        var extraDiv12 = document.querySelectorAll('td[class=tbl_content_top]>table')[3];
                        var lineBreaks = document.querySelectorAll('br');
                        var tableDivs = document.querySelectorAll('td[class=tbl_content_top]');
                        var loginForm = document.querySelector('table[class=tbl_form]');
                        var registerForm = document.querySelector('table[class=tbl_login_grey]');
                        var errorPage = document.querySelector('span[class=txt_error]');
                        var useridField = document.querySelector('input[name="j_username"][type="text"]');
                        var pwdField = document.querySelector('input[name="j_password"][type="password"]');
                        var submitBtnArrow = document.querySelector('span[class = "button"]');
                        var submitBtn = document.querySelector('input[name="Login"][type="submit"]');
                        var removeElements = [extraDiv4, extraDiv5, extraDiv6, extraDiv7, extraDiv8, extraDiv9, extraDiv10, extraDiv11, extraDiv12, extraHeader, extraBreak];
                        if(submitBtnArrow && submitBtn){
                            submitBtnArrow.style.width = "70px";
                            submitBtnArrow.style.textAlign = "center";
                            submitBtnArrow.style.fontSize = "14px";
                            submitBtnArrow.style.paddingLeft = "20px";
                            submitBtn.style.fontVariant = "all-petite-caps";
                            submitBtn.style.width = "64px";
                            submitBtn.style.textAlign = "left";
                            submitBtn.style.fontSize = "15px";
                        }
                        loginForm.style.height = "250px";
                        useridField.style.fontSize = "larger";
                        useridField.style.padding = "5px";
                        pwdField.style.padding = "5px";
                        pwdField.style.fontSize = "larger";
                        if(errorPage){
                            loginForm.style.width = "400px";
                            loginForm.appendChild(registerForm);
                            errorPage.style.fontSize = "larger";
                            loginForm.style.height = "300px";
                            __juspay.removeElements([registerForm]);
                        }
                        for(var a=0;a<lineBreaks.length;a++) {
                            removeElements.push(lineBreaks[a]);
                        }
                        __juspay.removeElements(removeElements);
                        if(table) {
                            table.style.height="100%";
                        }
                        mainBody[0].style.width = "200px";
                        mainBody[1].style.width = "400px";
                        for(var a=0;a<tableDivs.length;a++) {
                            tableDivs[a].style.height = "100%";
                            tableDivs[a].style.display = "block";
                            tableDivs[a].style.maxWidth = window.innerWidth.toString()+"px";
                        }
                        loginForm.style.height = "200px";
                        registerForm.style.height = "200px";
                        window.trackEventv2_0default("acs","info","MOBUI","LOGIN_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                    }
                }
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[name="Login"][type="submit"]');
                __juspay.clickOnElement(submitBtn);
            },
            showPassword: function() {
               var  passwordValue = document.querySelector('input[name="j_password"][type="password"]').value
               if(passwordValue != undefined && passwordValue != null) {
                    Gatekeeper.setPasswordValue(passwordValue);
               }
            },
            nextAction: function() {
                var passwordField = document.querySelector('input[name="j_password"][type="password"]');
                if(passwordField){
                    passwordField.focus();
                }
            }
        },
        {
            path: /\/nfs\/ibank\/ddbill_payment_entry/,
            hostname: "ibank.standardchartered.co.in",
            state: "UNUSED",
            local: false,
            bank: "SCBNB",
            domCheck: function(){
                return document.querySelector('select[id="debitSelectIndex0"]') &&
                       document.querySelector('input[id="trnRemarks"][type="text"]') &&
                       document.querySelector('input[type="submit"][value="Pay"]');
            },
            action: function(){
                var remarksField = document.querySelector('input[id="trnRemarks"][type="text"]');
                accountSelectionPage('select[id="debitSelectIndex0"] option',"SCBNB",'input[type="submit"][value="Pay"]',remarksField);
            },
            nextAction: function() {
                var submitBtn = document.querySelector('input[type="submit"][value="Pay"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        {
            path: /\/nfs\/ibank\/ddbill_payment_confirm/,
            hostname: "ibank.standardchartered.co.in",
            state: "UNUSED",
            local: false,
            bank: "SCBNB",
            domCheck: function(){
                return document.querySelector('input[type="password"][name="otp_security_token"]') &&
                        document.querySelector('input[type="button"][value="Confirm"]') &&
                        document.querySelector('input[id="termsAcceptanceFlag1"][type="checkbox"]');
            },
            action: function(){
                var termsAndCondition = document.querySelector('input[id="termsAcceptanceFlag1"][type="checkbox"]');
                if(termsAndCondition){
                    termsAndCondition.click();
                }
                otpFragmentHelper('input[name="otp_security_token"]','input[name="ResendOTP"][type="button"]');
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, 'input[name="otp_security_token"]', 'input[type="button"][value="Confirm"]');
            },
            regenerateOtp: function() {
                var resendLink = document.querySelector('input[name="ResendOTP"][type="button"]');
                if(resendLink){
                 __juspay.clickOnElement(resendLink);
                }
            }
        },

        /* IDBI DC */
        { // IDBI DC select authentication type (Password or OTP)
            path: /\/ACSWeb\/EnrollWeb\/IDBIBank\/server\/AccessControlServer\?(idct|perform=USER_AUTH)/,
            hostname: "secureonline.idbibank.com",
            state: "UNUSED",
            local: false,
            bank: "IDBIDC",
            domCheck: function() {
                return document.querySelector('input[type=radio][value=password]') && document.querySelector('input[type=radio][value=otp]');
            },
            action: function() {
                __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                Gatekeeper.setCardBrand(VISA);
                Gatekeeper.showACSOptions();
            },
            reachOtpStage: function() {
                var otpRadio = document.querySelector('input[type=radio][value=otp]');
                if(otpRadio) {
                    __juspay.clickOnElement(otpRadio);
                }
                var submitButton = document.querySelector('a[href = "javascript:redirectOnSubmit();"]');
                if(submitButton) {
                    __juspay.clickOnElement(submitButton);
                }
            },
            reachPasswordStage: function() {
                var passwordRadio = document.querySelector('input[type=radio][value=password]');
                if(passwordRadio) {
                    __juspay.clickOnElement(passwordRadio);
                }
                var submitButton = document.querySelector('a[href = "javascript:redirectOnSubmit();"]');
                if(submitButton) {
                    __juspay.clickOnElement(submitButton);
                }
            }
        },

        { // IDBI DC Password Page
            path: /\/ACSWeb\/EnrollWeb\/IDBIBank\/auth\/VBV.jsp/,
            hostname: "secureonline.idbibank.com",
            state: "UNUSED",
            local: false,
            bank: "IDBIDC",
            domCheck: function() {
                return document.querySelector('input[type=password][name=txtPassword]') && document.querySelector('input[type=submit][value=Submit]')
            },
            action: function() {
                Gatekeeper.setCardBrand(VISA);
                passwordFragmentHelper('input[type=password][id=txtPassword]');
            },
            clickSubmitButton: function() {
                var submitButton = document.querySelector('input[type=submit][value=Submit]');
                if(submitButton) {
                    __juspay.clickOnElement(submitButton);
                }
            }
        },

        { // IDBI DC OTP options Page
            path: /\/ACSWeb\/EnrollWeb\/common\/auth\/AuthOtpdestination\.jsp/,
            hostname: "secureonline.idbibank.com",
            state: "UNUSED",
            local: false,
            bank: "IDBIDC",
            domCheck: function() {
                return document.querySelector('input[type=radio][value=toMobile]') && document.querySelector('input[type=Image][name=I1]')
            },
            action: function() {
                Gatekeeper.removeFragment("OTP destination options page loaded");
                __juspay.trackPageStatus("CHOOSE_CHANNEL_OTP");
            }
        },

        { // IDBIDC OTP submit page
            path: /\/ACSWeb\/EnrollWeb\/IDBIBank\/server\/OtpServer/,
            hostname: "secureonline.idbibank.com",
            state: "UNUSED",
            local: "false",
            bank: "IDBIDC",
            domCheck: function() {
                return document.querySelector('input[type=password][id=otpValue]') &&
                       document.querySelector('input[type=Image][name=I1]');
            },
            action: function() {
                otpFragmentHelper('input[id=otpValue]',"");
            },
            submitOtp: function(otp) {
                submitOtpHelper(otp, 'input[id=otpValue]', 'input[type=Image][name=I1]');
            }
        },

        //IDBI NB
        {   path: /\/corp\/BANKAWAY/,
            hostname: "inet.idbibank.co.in",
            state: "UNUSED",
            local: false,
            bank: "IDBINB",
            domCheck: function() {
                return document.querySelector('input[type=text][name=CorporateSignonCorpId]') &&
                document.querySelector('input[name=CorporateSignonPassword][type=PASSWORD]') &&
                document.querySelector('input[type=submit][name="Action\\.ShoppingMall\\.Signon"]');
            },
            action: function() {
                Gatekeeper.removeFragment("login page");
                setupLoginPageWithSubmitButton("input[name=CorporateSignonPassword][type=PASSWORD]","input[type=text][name=CorporateSignonCorpId]","input[type=submit][name=Action\\.ShoppingMall\\.Signon]");
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                window.trackEventv2_0default("acs", "info", "IDBINB", "IDBINB Login page reached Successfully");

                if(window.find("Invalid Username or Password", true ,false, true)){
                    window.getSelection().collapseToEnd();
                    Gatekeeper.trackUserError("INVALID_LOGIN_CREDENTIALS");
                }
            },
            nextAction: function() {
                var userIdField = document.querySelector('input[type=text][name=CorporateSignonCorpId]');
                var passwordField = document.querySelector('input[name=CorporateSignonPassword][type=PASSWORD]');
                if(passwordField && userIdField.value!= ""){
                     passwordField.focus();
                }else{
                     userIdField.focus();
                }
            },
            clickSubmitButton: function() {
                var submitButton = document.querySelector('input[type=submit][name="Action\\.ShoppingMall\\.Signon"]');
                if(submitButton){
                    __juspay.clickOnElement(submitButton);
                }
            }
        },

        //BOB NB corporate login
        {
            path: /\/BankAwayCorporate\/.*\/CorpShoppingMallLogin.aspx/,
            hostname: "www.bobibanking.com",
            state: "UNUSED",
            local: false,
            bank: "BOBNB",
            domCheck: function() {
                return document.querySelector('input[name=CorporateSignonCorpId]') &&
                       document.querySelector('input[id=btnSubmit][type=button]') &&
                       document.querySelector('input[name=CorporateSignonUserName');
            },
            action: function() {
                Gatekeeper.setSessionAttribute("disableAutoFocus", "true"); showUpdateWebviewUber();

                Gatekeeper.removeFragment("login page");
                setupLoginPageWithUsernameAndSubmitButton("input[name=CorporateSignonUserName]","input[type=button][id=btnSubmit]");
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                var corp_id = document.querySelector('input[name=CorporateSignonCorpId');
                if(corp_id){
                    focusElement(corp_id);
                    setTimeout(function(){
                        __juspay.scrollToElement(corp_id);
                    },1000);
                }
                window.trackEventv2_0default("acs", "info", "BOBNB", "BOBINB Login page reached Successfully");
            },
            nextAction: function() {
                var user_id = document.querySelector('input[name=CorporateSignonUserName');
                var submitButton = document.querySelector('input[type=button][id=btnSubmit]');
                if(user_id.value === ""){
                    user_id.focus();
                    Gatekeeper.changeNextActionText("Submit");
                }
                else if(submitButton){
                    __juspay.clickOnElement(submitButton);
                }
            }
        },
        {  //BOI DC
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "BOIDC",
            domCheck: function(){
                return  document.querySelector('input[name = "pin"][type = "password"]') &&
                        document.querySelector('input[type=submit][value=Submit]') &&
                        document.querySelector("img[src*=bankofindia\\.gif]");
            },
            action: function(){
                passwordFragmentHelper('input[name = pin][type = "password"]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        // BOI DC- OTP
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "BOIDC",
            domCheck: function() {
                return document.querySelector('input[type=password][id=otp]') &&
                       document.querySelector('a[name=submitval][title=Submit]') &&
                       document.querySelector('img[src*=bankofindia][alt = Bank\\ of\\ India]');
            },
            action: function(){
                otpFragmentHelper('input[id=otp]','span[id=resendbutton] > a');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otp]', 'a[name=submitval][title=Submit]');
            },
            regenerateOtp: function() {
                var reSendOtp = document.querySelector('span[id=resendbutton] > a');
                if(reSendOtp){
                    __juspay.clickOnElement(reSendOtp);
                }
            }
        },
        //BOI CC
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "BOICC",
            domCheck: function() {
                return document.querySelector('input[type=button][value=Submit]') &&
                       document.querySelector('input[type=password][id=otp]') &&
                       document.querySelector('img[src*=en_US_BOI_CREDIT_OTPFolder]');
            },
            action: function(){
                otpFragmentHelper('input[id=otp]','span[id=resendbutton] > a');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otp]','input[type=button][value=Submit]');
            },
            regenerateOtp: function() {
                var reSendOtp = document.querySelector('span[id=resendbutton] > a');
                if(reSendOtp){
                    __juspay.clickOnElement(reSendOtp);

                }
            }
        },

        //HDFC IVR OTP Page
        {   path: /\/pay\/start-hdfc-otp/,
            hostname: "api.juspay.in",
            state: "UNUSED",
            local: false,
            bank: "HDFCIVR",
            domCheck: function() {
                return document.querySelector("input[id=otp-input][type=tel]");
            },
            action: function(){
                otpFragmentHelper("input[id=otp-input]");
            },
            submitOtp: function(otp) {
                var otpField = document.querySelector('input[id=otp-input]');
                var submit = document.querySelector("button[id=submit-otp][type=submit]");
                if(otpField){
                    otpField.value=otp;
                    try{
                        submit.disabled = false;
                        __juspay.clickOnElement(submit);
                    }catch(err){
                        Gatekeeper.removeFragment('Submit Button Dom Changed - Please Check');
                    }
                }
            }
        },
        {  //AmexCC : new OTP Page 2022
            path: /\/(otc|3ds)/,
            hostname: "safekey-1.americanexpress.com",
            state: "UNUSED",
            bank: "AMEXCC",
            domCheck: function(){
                return true;
            },
            action: function(){
                Gatekeeper.setPollingForSmsEnabled(true);
                var fieldCheck = setInterval(function() {
                    var otpField = document.querySelector('input[id=OTC]');
                    var amexImage = document.querySelector('img[alt="SafeKey"][class=header-logo]');
                    if(otpField && amexImage){
                        clearInterval(fieldCheck);
                        otpFragmentHelper('input[id=OTC]','button[id="otcResendBtn"]');
                    }
                },1000);
            },
            submitOtp: function(otp) {
                var otpField = document.querySelector('input[id=OTC]');
                if(otpField){
                    otpField.value=otp;
                    try{
                        validateOTC();
                        Gatekeeper.removeFragment('Submitted OTP via Fragment');
                    }catch(err){
                        Gatekeeper.removeFragment('Submit Button Dom Changed - Please Check');
                    }
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('button[id="otcResendBtn"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        {  //AmexCC : new OTP Page
            path: /\/GravityACS\/processing/,
            hostname: "acs-safekey.americanexpress.com",
            state: "UNUSED",
            bank: "AMEXCC",
            domCheck: function(){
                return true;
            },
            action: function(){
                Gatekeeper.setPollingForSmsEnabled(true);
                var fieldCheck = setInterval(function() {
                    var otpField = document.querySelector('input[id=OTC]');
                    var amexImage = document.querySelector('img[src*="aexp-static"]');
                    if(otpField && amexImage){
                        clearInterval(fieldCheck);
                        otpFragmentHelper('input[id=OTC]','a[id="resendCodeLink"]');
//                        updateNow.addEventListener('click', function(){
//                            Gatekeeper.removeFragment('user_selected_update_now');
//                        });
//                        termsConditions.addEventListener('click', function(){
//                            Gatekeeper.removeFragment('user_selected_terms_condition');
//                        });
                    }
                },1000);
            },
            submitOtp: function(otp) {
                var otpField = document.querySelector('input[id=OTC]');
                if(otpField){
                    otpField.value=otp;
                    try{
                        validateOTC();
                        Gatekeeper.removeFragment('Submitted OTP via Fragment');
                    }catch(err){
                        Gatekeeper.removeFragment('Submit Button Dom Changed - Please Check');
                    }
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[id="resendCodeLink"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        {  //Paypal checkout page
            path: /\/webapps\/hermes/,
            hostname: "www.paypal.com",
            state: "UNUSED",
            bank: "",
            domCheck: function(){
                return document.querySelector("input[type=submit][id=confirmButtonTop]")
                    && document.querySelector("div[id=paypalLogo]");
            },
            action: function(){
                window.trackEventv2_0default("acs","info","Aggregator","PAYPAL");
                Gatekeeper.setJavascriptToOpenWindows(false);
                window.trackEventv2_0default("acs","weblab","setJavascriptToOpenWindows","false");
            }
        },
        // BOMNB Account type selection page
        {
           path: /\/servlet\/ibs\.servlets\.IBSLoginServlet/,
           hostname: "www.mahaconnect.in",
           state: "UNUSED",
           bank: "BOMNB",
           domCheck: function(){
               return document.querySelector('img[src*=bom-logo]') &&
               document.querySelector('img[src*=login-retail]') &&
               document.querySelector('img[src*=login-corporate]');
           },
           action: function(){
               if(__juspay.isFeatureEnabled("BOMNB_AUTO_CLICK")) {
                   var retailButton = document.querySelector('img[src*=login-retail]');
                   __juspay.clickOnElement(retailButton);
               }
               if(__juspay.isMobUiEnabled("MOBUI_BOMNB")){
                   try{
                       var metaTag=document.createElement('meta');
                       metaTag.name = "viewport"
                       metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                       document.getElementsByTagName('head')[0].appendChild(metaTag);
                       var instructions = document.querySelector('div[id=content-mn][class=content-login]');
                       var corporateButton = document.querySelector('img[src*=login-corporate]');
                       var retailButton = document.querySelector('img[src*=login-retail]');
                       var page = document.querySelector('div[id=container]');
                       var body = document.querySelector('div[class*=grids]');
                       var footer = document.querySelector('div[id=footer]');
                       var contactLink = document.querySelector('a[href*=ContactUs]');

                       __juspay.removeElements([instructions]);
                       page.style.width = "268px";
                       body.style.width = "268px";
                       body.style.height = "200px";
                       __juspay.modifyUI({"style_width":{"110":[corporateButton, retailButton]}});
                       footer.appendChild(contactLink);
                       window.trackEventv2_0default("acs","info","MOBUI","ACCOUNT_TYPE_SELECTION_PAGE");
                   } catch(err) {
                       __juspay.CatchPageModifyError("MOBUI",err,"ACCOUNT_TYPE_SELECTION_PAGE");
                   }
               }
           }
        },
        //BOMNB opening new window
        {
            path: /\/InternetBanking\/ib\/IntermediatePage\.jsf/,
            hostname: "www.mahaconnect.in",
            state: "UNUSED",
            local: false,
            bank: "BOMNB",
            domCheck: function() {
                return document.getElementById("j_idt17_header");
            },
            action: function(){
                window.location="https://www.mahaconnect.in/InternetBanking/ib/ExternalLoginRetail.jsf?redirect=true&lt=R";
            }
        },

        //BOMNB login page
        {
            path: /\/(InternetBanking|InternetBanking1)\/ib\/ExternalLoginRetail\.jsf/,
            hostname: "www.mahaconnect.in",
            state: "UNUSED",
            local: false,
            bank: "BOMNB",
            domCheck: function() {
                return document.getElementById("userLogin:userName") &&
                    document.getElementById("userLogin:password") &&
                    document.getElementById("userLogin:loginButton");
            },
            action: function(){
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                if(__juspay.isMobUiEnabled("MOBUI_BOMNB")){
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "user-scalable=1"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);

                        var info = document.getElementById("loginInfoPanel");
                        var image = document.getElementById("j_idt43");
                        var loginPanel = document.getElementById("userLogin:loginPanel");
                        var helpLink = document.getElementById("userLanguage:j_idt39_content");
                        var userId = document.getElementById("userLogin:userName");
                        var password = document.getElementById("userLogin:password");
                        var login = document.getElementById("userLogin:loginButton");
                        var reset = document.getElementById("userLogin:j_idt35");
                        var error = document.getElementById("userLogin:j_idt27");
                        var sslImage = document.getElementById("j_idt23");
                        var hiddenElement1 = document.getElementById("j_idt13-resizer");
                        var hiddenElement2 = document.getElementById("j_idt19");
                        var hiddenElement3 = document.getElementById("j_idt23-resizer");
                        var virtualKeyboardUserid = document.querySelectorAll('img[src*=keyboard]')[0];
                        var virtualKeyboardPassword = document.querySelectorAll('img[src*=keyboard]')[1];
                        var body = document.querySelector('body');
                        var elements_list = [sslImage, hiddenElement1, hiddenElement2, hiddenElement3, info, image, virtualKeyboardUserid, virtualKeyboardPassword];
                        __juspay.removeElements(elements_list);
                        if (error){
                            error.style.width = "100%";
                        }
                        loginPanel.style.width = "98%";
                        helpLink.style.width = "100%";
                        userId.style.height = "20px";
                        password.style.height = "20px";
                        login.style.width = "120px";
                        reset.style.width = "120px";
                        login.style.height = "40px";
                        reset.style.height = "40px";
                        helpLink.style.paddingTop="20px";
                        body.style.overflow="scroll";
                        body.style.position="static";
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"LOGIN_PAGE");
                    }
                }
                var useridField = document.getElementById("userLogin:userName");
                var passwordField = document.getElementById("userLogin:password");
                var resetButton = document.getElementById("userLogin:j_idt35");
                useridField.focus();
                Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                Gatekeeper.requestPhoneKeyboardShow();
                passwordField.addEventListener("focus",function(){
                    attachAlphaNumericPasswordHelper(passwordField);
                    Gatekeeper.showPasswordHelperFragment();
                },false);
                useridField.addEventListener("focus",function(){
                    Gatekeeper.showNetbankingCustomerIdFragment("Continue");
                },false);
                resetButton.addEventListener("click",function(){
                    passwordField.value='';
                },false);
            },
            clickSubmitButton: function(){
                var submitButton =  document.getElementById("userLogin:loginButton");
                __juspay.clickOnElement(submitButton);
            },
            nextAction: function(){
                var passwordField = document.getElementById("userLogin:password");
                passwordField.focus();
            }
        },
        //BOMNB transaction password page
        {
            path: /\/(InternetBanking|InternetBanking1)\/ib\/ExternalLoginRetail|MerchantPaymentParent\.jsf/,
            hostname: "www.mahaconnect.in",
            state: "UNUSED",
            local: false,
            bank: "BOMNB",
            domCheck: function() {
                return document.getElementById("mainform:j_idt36") &&
                    document.getElementById("mainform:txnPassword")
            },
            action: function(){
                __juspay.trackPageStatus("TRANSACTION_PASSWORD_LOGIN");
                __juspay.trackAuthMethod("TXN_PASSWORD");
                if(__juspay.isMobUiEnabled("MOBUI_BOMNB")){
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "user-scalable=1"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);

                        var footer = document.getElementById("mainform:j_idt113");
                        var helpIcon = document.getElementById("mainform:helpicon");
                        var virtualKeyboardPassword = document.querySelector('img[src*=keyboard]');
                        var body = document.querySelector('body');
                        var space= document.createElement('div');
                        var elements_list = [footer, helpIcon, virtualKeyboardPassword];
                        __juspay.removeElements(elements_list);
                        __juspay.delayMe(function(){
                            if (jQuery) {
                                $(window).off("resize")
                            }
                        },200)
                        transactionInfo.style.width="360px";
                        transactionInfo.style.top="350px";
                        space.style.height="300px";
                        body.appendChild(space);
                        body.style.overflow="scroll";
                        body.style.position="static";
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"TRANSACTION_PASSWORD_PAGE");
                    }
                }
            }
        },
        //BOMNB OTP page
        {
            path: /\/(InternetBanking|InternetBanking1)\/ib\/MerchantPaymentParent\.jsf/,
            hostname: "www.mahaconnect.in",
            state: "UNUSED",
            local: false,
            bank: "BOMNB",
            domCheck: function() {
                return document.getElementById("mainform:j_idt36") &&
                    document.getElementById("mainform:OTPEntered")
            },
            action: function(){
                __juspay.trackPageStatus("OTP_LOGIN");
                __juspay.trackAuthMethod("OTP");
                if(__juspay.isMobUiEnabled("MOBUI_BOMNB")){
                    try{
                        var metaTag=document.createElement('meta');
                        metaTag.name = "viewport"
                        metaTag.content = "user-scalable=1"
                        document.getElementsByTagName('head')[0].appendChild(metaTag);

                        var transactionInfo = document.getElementById("mainform:j_idt36");
                        var footer = document.getElementById("mainform:j_idt113");
                        var helpIcon = document.getElementById("mainform:helpicon");
                        var virtualKeyboardOtp = document.querySelector('img[src*=keyboard]');
                        var body = document.querySelector('body');
                        var space= document.createElement('div');
                        var elements_list = [footer, helpIcon, virtualKeyboardOtp];
                        __juspay.removeElements(elements_list);
                        __juspay.delayMe(function(){
                            if (jQuery) {
                                $(window).off("resize")
                            }
                        },200)
                        transactionInfo.style.width="360px";
                        transactionInfo.style.top="460px";
                        space.style.height="300px";
                        body.appendChild(space);
                        body.style.overflow="scroll";
                        body.style.position="static";
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"TRANSACTION_PASSWORD_PAGE");
                    }
                }
            }
        },
        //BOB NB : Login id page
        {   path: /BankAwayRetail.*\/RetailShoppingMallLogin.aspx/,
            hostname: "www.bobibanking.com",
            state: "UNUSED",
            local: false,
            bank: "BOBNB",
            domCheck: function() {
                return document.querySelector('input[name=CorporateSignonCorpId]') &&
                       document.querySelector('input[id=btnSubmit][type=button]');
            },
            action: function() {
                Gatekeeper.setSessionAttribute("disableAutoFocus", "true"); showUpdateWebviewUber();

                Gatekeeper.removeFragment("Reached User Id page");
                setupLoginPageWithUsernameAndSubmitButton("input[name=CorporateSignonCorpId]","input[type=button][id=btnSubmit]");
                setTimeout(function(){
                    __juspay.scrollToElement(document.querySelector('input[name=CorporateSignonCorpId]'))
                },1000);
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
            },
            nextAction: function() {
                var submitButton = document.querySelector('input[id=btnSubmit][type=button]');
                if(submitButton){
                    __juspay.clickOnElement(submitButton);
                }
            }
        },

        //BOBNB: Mobile number entry page
         {
            path: /BankAwayRetail.*\/MobileDetails.aspx/,
            hostname: "www.bobibanking.com",
            state: "UNUSED",
            bank: "BOBNB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id="txtMobileNo"]') &&
                       document.querySelector('input[id="btnSubmit"][type="button"]');
            },
            action: function() {
               var mobileNumberField = document.querySelector('input[id="txtMobileNo"]');
               if(mobileNumberField){
                   mobileNumberField.blur();
                   __juspay.scrollToElement(mobileNumberField);
                   focusElement(mobileNumberField);
                   numberTextFieldQuery("input[id=txtMobileNo]");
                   attachPhoneKeyboard(mobileNumberField);
                   __juspay.trackPageStatus("INPUT_MOBILE_NUMBER");
               }
               Gatekeeper.showNetbankingDefaultFragment();
               Gatekeeper.changeNextActionText("Submit");
            },
            nextAction: function() {
                var submitBtn = document.querySelector('input[id="btnSubmit"][type="button"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        {//BOBNB OTP page
            path: /BankAwayRetail.*\/OTPDetails.aspx/,
            hostname: "www.bobibanking.com",
            state: "UNUSED",
            bank: "BOBNB",
            local: false,
            domCheck: function() {
                return document.querySelector('input[id="txtOTP"]') &&
                       document.querySelector('input[id="btnSubmit"][type="submit"]');
            },
            action: function() {
                otpFragmentHelper('input[id="txtOTP"]',"");
            },
            submitOtp: function(otp) {
               submitOtpHelper(otp, 'input[id="txtOTP"]','input[id="btnSubmit"][type="submit"]');
           }
        },
        // BOBNB: Password page
        {   path: /BankAwayRetail.*\/RetailShoppingMallPassword.aspx/,
            hostname: "www.bobibanking.com",
            state: "UNUSED",
            local: false,
            bank: "BOBNB",
            domCheck: function() {
                return document.querySelector('input[id=txtPassword][type="password"]') &&
                       document.querySelector('input[id=btnLogin][type=Submit]');
            },
            action: function() {
                passwordFragmentHelper('input[id=txtPassword]');
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[id=btnLogin][type=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //BOBNB: Account Selection
        {
            path: /BankAwayRetail.*\/CrpShoppingMallMakePayment.aspx/,
            hostname: "www.bobibanking.com",
            state: "UNUSED",
            local: false,
            bank: "BOBNB",
            domCheck: function() {
                return document.querySelector('select[name="UserAccountsIndex"]') &&
                       document.querySelector('input[name="Action.Bills.ShoppingMall.MakePayment.Pay"]');
            },
            action: function() {
                accountSelectionPage('select[name="UserAccountsIndex"] option',"BOBNB",'input[name="Action.Bills.ShoppingMall.MakePayment.Pay"]','');
            },
            nextAction: function() {
                var submitBtn = document.querySelector('input[name="Action.Bills.ShoppingMall.MakePayment.Pay"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        {
            //BOBNB Transaction password
            path: /BankAwayRetail.*\/ShoppingMallTxnLogin.aspx/,
            hostname: "www.bobibanking.com",
            state: "UNUSED",
            local: false,
            bank: "BOBNB",
            domCheck: function() {
                return document.querySelector('input[id=ValCorpTxnPwdUserName][type="TEXT"]') &&
                       document.querySelector('input[id=ValCorpTxnPwdTxnPwd][type=PASSWORD]') &&
                       document.querySelector('input[name="Action.ArcotCITRUS.ShoppingMall.TxnLogin.Ok"]');
            },
            action: function() {
                __juspay.trackPageStatus("INPUT_NB_TXN_LOGIN");
                setupLoginPageWithSubmitButton('input[id=ValCorpTxnPwdTxnPwd][type=PASSWORD]',
                                               'input[id=ValCorpTxnPwdUserName][type=TEXT]',
                                               'input[name="Action.ArcotCITRUS.ShoppingMall.TxnLogin.Ok"]');
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[name="Action.Arcot.bills.ShoppingMall.TxnLogin.Ok"]');
                if(submitBtn){
                __juspay.clickOnElement(submitBtn);
                }
            },
            nextAction: function() {
                var passwordField = document.querySelector('input[id=ValCorpTxnPwdTxnPwd][type=PASSWORD]')
                if(passwordField){
                    passwordField.focus();
                }
            }
        },
        // UCONB:Login page
        {
            path: /BankAwayRetail.*\/RetShoppingMallSignOn.aspx/,
            hostname: "www.ucoebanking.com",
            state: "UNUSED",
            bank: "UCONB",
            domCheck: function(){
                return  document.querySelector('input[name=CorporateSignonCorpId][type=TEXT]') &&
                        document.querySelector('input[type=PASSWORD][name=CorporateSignonPassword]') &&
                        document.querySelector('input[id=button1][type=button]');
            },
            action: function(){
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                setupLoginPageWithSubmitButton('input[type=PASSWORD][name=CorporateSignonPassword]',
                'input[name=CorporateSignonCorpId][type=TEXT]',
                'input[id=button1][type=button]');

            },
            nextAction: function(){
                var passwordField = document.querySelector('input[type=PASSWORD][name=CorporateSignonPassword]');
                if(passwordField){
                    passwordField.focus();
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[id=button1][type=button]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        //UCONB: Account selection page
        {
            path: /BankAwayRetail.*\/CrpShoppingMallMakePayment.aspx/,
            hostname: "www.ucoebanking.com",
            state: "UNUSED",
            bank: "UCONB",
            domCheck: function(){
                return  document.querySelector('input[name=bills\\.PaymentRemarks][type=TEXT]') &&
                        document.querySelector('input[type=SUBMIT][name=Action\\.Bills\\.ShoppingMall\\.MakePayment\\.Pay]') &&
                        document.querySelector('select[name=UserAccountsIndex]');
            },
            action: function(){
                var remarksField = document.querySelector('input[name=bills\\.PaymentRemarks][type=TEXT]')
                accountSelectionPage('select[name=UserAccountsIndex] option',"UCONB",'input[type=SUBMIT][name=Action\\.Bills\\.ShoppingMall\\.MakePayment\\.Pay]',remarksField);
            },
            nextAction: function(){
                var submitBtn = document.querySelector('input[type=SUBMIT][name=Action\\.Bills\\.ShoppingMall\\.MakePayment\\.Pay]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //UCONB OTP page:
        {
            path: /BankAwayRetail.*\/OTPAuthentication.aspx/,
            hostname: "www.ucoebanking.com",
            state: "UNUSED",
            bank: "UCONB",
            domCheck: function(){
                return  document.querySelector('input[id=passcodeEntByUser][type=PASSWORD]') &&
                        document.querySelector('input[id=button7][type=SUBMIT][value=CONFIRM]') &&
                        document.querySelector('input[id=button7][type=SUBMIT][value=RESEND\\ OTP]')
            },
            action: function(){
                otpFragmentHelper('input[id=passcodeEntByUser]','input[id=button7][type=SUBMIT][value=RESEND\\ OTP]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=passcodeEntByUser]', 'input[id=button7][type=SUBMIT][value=CONFIRM]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('input[id=button7][type=SUBMIT][value=RESEND\\ OTP]');
                __juspay.clickOnElement(regenerateBtn);
            }
        },
        //CORPCC Password Page
        {
            path: /\/ACSWeb\/EnrollWeb\/CorporationBank\/server\/AccessControlServer/,
            hostname: "b4-pdc.enstage-sas.com",
            state: "UNUSED",
            bank: "CORPCC",
            domCheck: function(){
                return  document.querySelector('input[name="txtPassword"]') &&
                        document.querySelector('input[name="cmdSubmit"]') &&
                        document.querySelector('form[action*="CorporationBank"]');
            },
            action: function(){
                passwordFragmentHelper('input[name="txtPassword"]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[name="cmdSubmit"]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //CORPDC OTP page
        {
            path: /\/ACSWeb\/EnrollWeb\/MICorporation\/server\/AccessControlServer/,
            hostname: "pdc-mi.enstage-sas.com",
            state: "UNUSED",
            bank: "CORPDC",
            domCheck: function(){
                return  document.querySelector('input[name="otpValue"]') &&
                        document.querySelector('input[src*="btn_submit"]') &&
                        document.querySelector('img[src*="8190_logo"]');
            },
            action: function(){
                var otpField = document.querySelector('input[name="otpValue"]');
                var vbv_logo = document.querySelector('img[alt*="Visa"]');
                if(vbv_logo){
                   Gatekeeper.setCardBrand(VISA);
                }
                otpFragmentHelper('input[name="otpValue"]','');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[name="otpValue"]', 'input[src*="btn_submit"]');
            }
        },
        //Federal Bank NetBanking: Login:
        {
            path: /\/corp\/AuthenticationController/,
            hostname: "www.fednetbank.com",
            state: "UNUSED",
            bank: "FEDNB",
            domCheck: function(){
                return document.querySelector('input[type=text][id=AuthenticationFG\\.USER_PRINCIPAL]') &&
                       document.querySelector('button[type=Submit][id=VALIDATE_CREDENTIALS]') &&
                       document.querySelector('input[type=password][id=AuthenticationFG\\.ACCESS_CODE]');
            },
            action: function(){
                var userIdField =document.querySelector('input[type=text][id=AuthenticationFG\\.USER_PRINCIPAL]');
                var passwordField = document.querySelector('input[type=password][id=AuthenticationFG\\.ACCESS_CODE]');
                var submitBtn = document.querySelector('button[type=Submit][id=VALIDATE_CREDENTIALS]');
                if(userIdField && passwordField && submitBtn){
                    __juspay.trackPageStatus("INPUT_NB_LOGIN");
                    __juspay.trackAuthMethod("LOGIN");
                    setupLoginPageWithSubmitButton('input[type=password][id=AuthenticationFG\\.ACCESS_CODE]',
                                                   'input[type=text][id=AuthenticationFG\\.USER_PRINCIPAL]',
                                                   'button[type=Submit][id=VALIDATE_CREDENTIALS]');
                }
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('button[type=Submit][id=VALIDATE_CREDENTIALS]');
                if(submitBtn) {
                    __juspay.clickOnElement(submitBtn);
                }
            },
            nextAction: function() {
                var passwordElement = document.querySelector('input[type=password][id=AuthenticationFG\\.ACCESS_CODE]');
                if( passwordElement)
                {
                    passwordElement.focus();
                }
            }
        },
        {  //Sarawat Bank 3ds page : visa card
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "SARDC",
            domCheck: function(){
                return  document.querySelector('input[name=pin][type=password]') &&
                        document.querySelector('input[type=submit][value=Submit]') &&
                        document.querySelector('img[title*=Saraswat\\ bank]');
            },
            action: function(){
                var vbv_logo = document.querySelector('img[title*=Verified\\ by\\ Visa][name="vpasLogo"]');
                if(vbv_logo){
                   Gatekeeper.setCardBrand(VISA);
                }
                passwordFragmentHelper('input[name=pin][type=password]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //INDDC : OTP destination page
        {
            path: /\/ACSWeb\/EnrollWeb\/IndianBank\/server\/AccessControlServer/,
            hostname: "b10-pdc.enstage-sas.com",
            state: "UNUSED",
            bank: "INDDC",
            domCheck: function(){
               return  document.querySelector('input[value=toMobile][type=radio]') &&
                       document.querySelector('input[type=Image][src*=btn_submit]') &&
                       document.querySelector('input[type=radio][value=toNeither]');
            },
            action: function(){
                var mobileRadioBtn = document.querySelector('input[value=toMobile][type=radio]');
                var submitBtn = document.querySelector('input[type=Image][src*=btn_submit]');
                if(mobileRadioBtn && submitBtn){
                    mobileRadioBtn.checked = true;
                    __juspay.trackPageStatus("CHOOSE_OTP_DESTINATION");
                    if(__juspay.isMobUiEnabled("AUTO_CLICK_INDDC")){
                        __juspay.clickOnElement(submitBtn);
                        window.trackEventv2_0default("acs","info","AUTO_CLICK","CHOOSE_OTP_DESTINATION");
                        Gatekeeper.showToast("OTP sent to mobile");
                    }else{
                        Gatekeeper.showNetbankingDefaultFragment();
                    }
                }
            },
            nextAction: function(){
                var submitBtn = document.querySelector('input[type=Image][src*=btn_submit]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        //INDDC : OTP page
        {
            path: /\/ACSWeb\/EnrollWeb\/IndianBank\/server\/OtpServer/,
            hostname: "b10-pdc.enstage-sas.com",
            state: "UNUSED",
            bank: "INDDC",
            local: false,
            domCheck: function(){
                return  document.querySelector('input[id=otpValue][type=password]') &&
                        document.querySelector('input[src*=btn_submit][type=Image]');
            },
            action: function(){
                otpFragmentHelper('input[id=otpValue]','a[href*=resend_otp]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id=otpValue]','input[src*=btn_submit][type=Image]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[href*=resend_otp]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        // VijayaNB login
        {
            path: /\/NASApp\/AuthenticationController/,
            hostname: "www.vijayabankonline.in",
            state: "UNUSED",
            bank: "VIJAYANB",
            domCheck: function(){
                return  document.querySelector('input[id=AuthenticationFG\\.USER_PRINCIPAL][type=text]') &&
                        document.querySelector('input[id=AuthenticationFG\\.ACCESS_CODE][type=password]')&&
                        document.querySelector('input[id=VALIDATE_CREDENTIALS][type=Submit]');
            },
            action: function(){
                var loginIdField = document.querySelector('input[id=AuthenticationFG\\.USER_PRINCIPAL][type=text]');
                var passwordField = document.querySelector('input[id=AuthenticationFG\\.ACCESS_CODE][type=password]');
                var submitBtn = document.querySelector('input[id=VALIDATE_CREDENTIALS][type=Submit]');
                if(loginIdField && passwordField && submitBtn){
                    setupLoginPageWithSubmitButton('input[id=AuthenticationFG\\.ACCESS_CODE][type=password]','input[id=AuthenticationFG\\.USER_PRINCIPAL][type=text]','input[id=VALIDATE_CREDENTIALS][type=Submit]');
                    __juspay.trackPageStatus("INPUT_LOGIN");
                    __juspay.trackAuthMethod("LOGIN");
                }
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('input[id=VALIDATE_CREDENTIALS][type=Submit]');
                var passwordElement = document.querySelector('input[id=AuthenticationFG\\.ACCESS_CODE][type=password]');
                var captchaField = document.querySelector('input[id=AuthenticationFG\\.VERIFICATION_CODE][type=text]');
                if(captchaField){
                    passwordElement.focus();
                }
                if(submitBtn && passwordElement.value) {
                    __juspay.clickOnElement(submitBtn);
                }else{
                    passwordElement.focus();
                }
            },
            nextAction: function() {
                var passwordElement = document.querySelector('input[id=AuthenticationFG\\.ACCESS_CODE][type=password]');
                if(passwordElement)
                {
                    passwordElement.focus();
                }
            }
        },
        //SYNDC
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "SYNDC",
            domCheck: function(){
                return document.querySelector('img[id=banklogo][title="Syndicate Bank"]');
            },
            action: function(){
                var submitBtn = document.querySelector('button[id=sendotp][type=submit]');
                if(submitBtn){
                    otpFragmentHelper('input[id=otpentrypin][name=otp]','a[onClick*=reSend]');
                }
                passwordFragmentHelper('input[id=enterPASS][type=password]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id=otpentrypin][name=otp]','button[id=sendotp][type=submit]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[onClick*=reSend]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('button[id=sendotp][type=submit]')
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }

        },
          //BOMDC
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "BOMDC",
            domCheck: function(){
                return document.querySelector('input[type=password][id=enterPIN]') &&
                       document.querySelector('button[type=button][id=sendotp]') &&
                       document.querySelector('img[id=banklogo][src*=mahabank_vbv]');
            },
            action: function() {
                var checkbox = document.querySelector('input[type=checkbox][id=disclaimer]');
                if(checkbox){
                    checkbox.checked = true;
                }
                otpFragmentHelper('input[id=enterPIN]','a[onclick*=reSend]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id=enterPIN]','button[type=button][id=sendotp]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[onclick*=reSend]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //SIB DC
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "SIBDC",
            domCheck: function(){
                return  document.querySelector('img[id=banklogo][src*=sec5_SIB_Visa]') ||
                        document.querySelector('img[id=banklogo][src*=sec5_SIB_MC]');
            },
            action: function(){
                var submitBtn = document.querySelector('button[id=sendotp][type=submit]');
                if(submitBtn){
                    otpFragmentHelper('input[id=otpentrypin][name=otpentrypin]','a[onClick*=reSend]');
                }
                passwordFragmentHelper('input[id=enterPASS][type=password]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id=otpentrypin][name=otpentrypin]','button[id=sendotp][type=submit]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[onClick*=reSend]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('button[id=sendotp][type=submit]')
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }

        },
        { //IDFCNB new page - login page
            path: /\/checkout/,
            hostname:"my.idfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "IDFCNB",
            domCheck:function() {
                return document.querySelector('input[name=j_username]') &&
                       document.querySelector('input[name=j_password]') &&
                       document.querySelector('button[type=submit][class*=btn-submit-txnlogin]');
            },
            action: function(){
                __juspay.trackPageStatus("INPUT_NB_LOGIN");
                __juspay.trackAuthMethod("LOGIN");
                setupLoginPageWithSubmitButton('input[name=j_password]', 'input[name=j_username]', 'button[type=submit][class*=btn-submit-txnlogin]');
                document.querySelector('input[name=j_username]').dispatchEvent(new Event("change"));
            },
            clickSubmitButton: function() {
                var submitBtn = document.querySelector('button[type=submit][class*=btn-submit-txnlogin]');
                __juspay.clickOnElement(submitBtn);
             },
            nextAction: function() {
                var passwordField = document.querySelector('input[name=j_password]')
                passwordField.focus();
            }
        },
        { // IDFCNB otp page
            path: /\/ecom/,
            hostname: "my.idfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "IDFCNB",
            domCheck: function() {
                return document.querySelector('label[lp-i18n="Account Number"]') &&
                       document.querySelector('img[ng-src*="idfc-web-logo.png"]');
            },
            action: function(){
                var payButtonBlock = document.querySelector('div[ng-show*="startButtons"][class="idfc-button-container idfc-updated-button-container"]');
                if(payButtonBlock){
                    Gatekeeper.removeFragment('reached OTP page');
                    document.querySelector('button[lp-i18n="Pay"]').click();
                }
                var otpFieldChecker = setInterval(function(){
                    var otpField = document.querySelectorAll('input[name="otp"][ng-model*="otpValue"]')[1];
                    var otpFiledShown = document.querySelector('input[class="form-control ng-pristine ng-untouched ng-valid ng-not-empty"]');
                    if(otpField && otpFiledShown){
                        clearInterval(otpFieldChecker);
                        otpFragmentHelper('input[name="otp"][ng-model*="otpValue"]');
                        Gatekeeper.enableRegenerateOTP();
                    }
                },500)
            },
            submitOtp: function(otp) {
                var otpField = document.querySelectorAll('input[name="otp"][ng-model*="otpValue"]')[1];
                var submitButton = document.querySelectorAll('button[name="verifyOTP"]')[1];
                if(submitButton){
                    otpField.value=otp;
                    document.querySelectorAll('input[name="otp"][ng-model*="otpValue"]')[1].dispatchEvent(new Event("change"));
                    try{
                        __juspay.clickOnElement(submitButton);
                        Gatekeeper.removeFragment('OTP submitted');
                    }catch(err){
                        Gatekeeper.removeFragment('Submit Button Dom Changed - Please Check');
                    }
                }
            },
            regenerateOtp: function(){
              var regenerateBtn = document.querySelectorAll('button[name="resendOTP"]')[1];
              if(regenerateBtn){
                  __juspay.clickOnElement(regenerateBtn);
              }
            }
        },
        //IDFCDC OTP Page
        {
            path: /\/acspage\/cap\?RID/,
            hostname: "secure7.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "IDFCDC",
            domCheck: function() {
                return document.querySelector("input[id=enterPIN][type=password]")&&
                        document.querySelector("button[id=sendotp][type=submit]")&&
                        document.querySelector("img[src*=GenericOTP_IDFC]");
            },
            action: function(){
                if(__juspay.isMobUiEnabled("MOBUI_IDFCDC")){
                    try{
                        var footerText = document.querySelector("span[id=footertext]");
                        var backGround = document.getElementsByClassName("gradient")[0];
                        var controlsSubmit = document.getElementsByClassName("controlssubmit")[0];
                        footerText.remove();
                        backGround.style.height = "368px";
                        controlsSubmit.style.top = "350px";
                        controlsSubmit.children[2].style.marginLeft = "0px";
                        controlsSubmit.children[2].style.marginTop = "10px";
                        controlsSubmit.children[3].style.marginTop = "7px";
                        controlsSubmit.children[3].style.marginRight = "7px";
                        window.trackEventv2_0default("acs","info","MOBUI","OTP_PAGE");
                    } catch(err) {
                        __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                    }
                }
                otpFragmentHelper('input[id=enterPIN][type=password]','a[onclick*="reSend"]');
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, "input[id=enterPIN][type=password]", "button[id=sendotp][type=submit]");
            },
            regenerateOtp: function(){
                var resend =document.querySelector('a[onclick*="reSend"]');
                if(resend){
                    __juspay.clickOnElement(resend);
                }
            }
        },
        //DCB Bank
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "DCBBNK",
            domCheck: function(){
                return  document.querySelector('input[type=password][name=pin]') &&
                        document.querySelector('input[type=submit][value=Submit]') &&
                        document.querySelector('img[src*=DCB]');
            },
            action: function(){
                passwordFragmentHelper('input[type=password][name=pin]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
       //PMC DC
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "PMCDC",
            domCheck: function(){
                return  document.querySelector('input[type=password][name=pin]') &&
                        document.querySelector('input[type=submit][value=Submit]') &&
                        document.querySelector('img[src*=pmc_ads_logo]');
            },
            action: function(){
                passwordFragmentHelper('input[type=password][name=pin]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
        //DHANDC
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "DHANDC",
            domCheck: function(){
                return  document.querySelector('input[type=password][name=pin]') &&
                        document.querySelector('input[type=submit][value=Submit]') &&
                        document.querySelector('img[src*=DhanlaxmiBankLogo]');
            },
            action: function(){
                passwordFragmentHelper('input[type=password][name=pin]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //SVCB DC
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "SVCBDC",
            domCheck: function(){
                return  document.querySelector('input[type=password][name=pin]') &&
                        document.querySelector('input[type=submit][value=Submit]') &&
                        document.querySelector('img[src*=svcb][name=memberLogo]');
            },
            action: function(){
                passwordFragmentHelper('input[type=password][name=pin]');
            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[type=submit][value=Submit]');
                __juspay.clickOnElement(submitBtn);
            }
        },
        //DBS Bank
        {
            path: /\/acspage\/cap/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            bank: "DBSDC",
            domCheck: function(){
                return  (document.querySelector('input[id=password][type=password][name=pin]') || document.querySelector('input[id=otp][type=password]')) &&
                        (document.querySelector('input[type=submit][value=Submit]') || document.querySelector('input[type=button][value=Submit]')) &&
                        document.querySelector('a[href*=OnSubmitHandlerResend]') &&
                        document.querySelector('img[src*=DBSVBV][name=memberLogo]');
            },
            action: function(){
                var otpField1 = document.querySelector('input[id=password][type=password][name=pin]');
                var otpField2 = document.querySelector('input[id=otp][type=password]');
                if(otpField1) {
                    otpFragmentHelper('input[id=password][type=password][name=pin]','a[href*=OnSubmitHandlerResend]',true);
                }
                if(otpField2){
                    otpFragmentHelper('input[id=otp][type=password]','a[href*=OnSubmitHandlerResend]',true);
                }
            },
            submitOtp: function(otp){
                var submitBtn1 = document.querySelector('input[type=submit][value=Submit]');
                var submitBtn2 = document.querySelector('input[type=button][value=Submit]');
                var otpField1 = document.querySelector('input[id=password][type=password][name=pin]');
                var otpField2 = document.querySelector('input[id=otp][type=password]');
                if (otpField1 && submitBtn1){
                    submitOtpHelper(otp, 'input[id=password][type=password][name=pin]', 'input[type=submit][value=Submit]');
                }
                if (otpField2 && submitBtn2){
                   submitOtpHelper(otp, 'input[id=otp][type=password]', 'input[type=button][value=Submit]');
               }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[href*=OnSubmitHandlerResend]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        {
            path: /\/(PAReq|ImsControl)\.do/,
            hostname: "dbsbank.euronet3dsecure.com",
            state: "UNUSED",
            bank: "DBSDC",
            domCheck: function(){
                return document.querySelector('img[src*=dbsbanklogo]') &&
                       document.querySelector('img[src*=logoVisaVerified]') &&
                       document.querySelector('input[id=otp][type=password]') &&
                       document.querySelector('img[src*=submitbutton]') &&
                       document.querySelector('img[src*=resendbutton]')
            },
            action: function(){
                var okButtonInResendOtpDialogParentDiv = document.querySelector('div[class=modalDialog][id=light_div_1]');
                var okButtonInResendOtpDialog = document.querySelector('img[src*=okbutton]');
                if(okButtonInResendOtpDialogParentDiv && okButtonInResendOtpDialogParentDiv.style.display == "block" && okButtonInResendOtpDialog){
                    autoClickButton('img[src*=okbutton]','ok_button_resend_dialog',this.bank)
                }
                Gatekeeper.setCardBrand(VISA);
                otpFragmentHelper('input[id=otp][type=password]','img[src*=resendbutton]');

            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otp][type=password]', 'img[src*=submitbutton]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('img[src*=resendbutton]')
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //Indus CC - Choose Auth option
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "INDUSCC",
            domCheck: function(){
                return document.querySelector('input[type=radio][id=static]') &&
                       document.querySelector('input[type=radio][id=otp]') &&
                       document.querySelector('button[type=button][id=continue]') &&
                       (document.querySelector('img[src*=IBLCreditMC]') || document.querySelector('img[src*=IBLCreditVisa]'));
            },
            action: function() {
                var vbv = document.querySelector('img[src*=vpas_logo]');
                if(vbv){
                    Gatekeeper.setCardBrand(VISA);
                }
                __juspay.trackPageStatus("CHOOSE_AUTH_OPTIONS");
                Gatekeeper.showACSOptions();
            },
            reachOtpStage: function() {
                var passOption = document.querySelector('input[type=radio][id=static]');
                var otpOption = document.querySelector('input[type=radio][id=otp]');
                var submitBtn = document.querySelector('button[type=button][id=continue]')
                if(otpOption){
                    passOption.checked = false;
                    otpOption.checked = true;
                }
                if(submitBtn){
                    __juspay.clickOnElement(submitBtn);
                }
            },
            reachPasswordStage: function() {
                var passOption = document.querySelector('input[type=radio][id="static"]');
                var otpOption = document.querySelector('input[type=radio][id="otp"]');
                var submitBtn = document.querySelector('button[type=button][id=continue]')
                if(passOption){
                    otpOption.checked = false;
                    passOption.checked = true;
                }
                if(submitBtn){
                    Gatekeeper.showWaitingFragment();
                    __juspay.clickOnElement(submitBtn);
                }
            }
        },
         //Indus card OTP page
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank: "INDUSCC",
            domCheck: function(){
                return document.querySelector('input[type=password][id=otpentrypin]') &&
                document.querySelector('button[type=submit][id=sendotp]') &&
                (document.querySelector('img[src*=IBLCreditMC]') || document.querySelector('img[src*=IBLCreditVisa]'));
            },
            action: function() {
                var checkbox = document.querySelector('input[type=checkbox][id=disclaimer]');
                otpFragmentHelper("input[id=otpentrypin]",'a[onclick*=reSend]');
                if(checkbox){
                    checkbox.checked = true;
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id=otpentrypin]','button[type=submit][id=sendotp]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[onclick*=reSend]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        {
            path: /\/acspage\/cap\?/,
            hostname: "secure5.arcot.com",
            state: "UNUSED",
            local: false,
            bank:"",
            domCheck: function(){
                return document.querySelector('input[id=passwordfield]') &&
                       document.querySelector('input[id=mltconfirmpassword]');
            },
            action:function(){
                __juspay.trackPageStatus("SET_PASSWORD_PAGE");
            }
        },
        {
            path: /\/pgidsk\/ProcessPayment/,
            hostname: "www.billdesk.com",
            state: "UNUSED",
            local: false,
            bank:"billdesk",
            domCheck: function(){
                return document.querySelector('img[src*=billdesk-logo]')  &&
                       document.querySelector('input[name=netbank]') &&
                       document.querySelector('input[id=MPesa]')
            },
            action: function(){
                __juspay.trackPageStatus("Payment_Instrument_Selection_Page");
            }
        },
        //Code to get the payment status from aggregator response
        {
            path: /.*_resp/,
            hostname: "www.citruspay.com",
            state: "UNUSED",
            bank: "",
            local: false,
            domCheck: function() {
                return false
            }
        },
        //dump PayU response hidden elements
        {
            path: /response/,
            hostname: "secure.payu.in",
            state: "UNUSED",
            bank: "",
            local: false,
            domCheck: function() {
                return false
            }
        },
        {
            path: /\/servlets\/PgTransResp\_Web/,
            hostname: "www.citibank.co.in",
            state: "UNUSED",
            bank: "",
            local: false,
            domCheck: function() {
                return false
            }
        },
        //dump ICICIPG
        {
            path: /.*Response/,
            hostname: "3dsecure.payseal.com",
            state: "UNUSED",
            bank: "",
            local: false,
            domCheck: function() {
                return false
            }
        },
        //snapdeal checkout page
        {
            path: /\/checkout/,
            hostname: "m.snapdeal.com",
            state: "UNUSED",
            bank: "",
            domCheck: function(){
                return  document.querySelector('div[id="checkoutContent"]');
            },
            action:function(){
                Gatekeeper.removeFragment('Reached Snapdeal Checkout Page');
            }
        },
        //dump zaakpay
        {
            path: /.*(Response|response)/,
            hostname: "api.zaakpay.com",
            state: "UNUSED",
            bank: "",
            local: false,
            domCheck: function() {
                return false
            }
        },
        /* MiGS page before PAReq submit */
        {
            path: /(.*\/vpcpay\?o=pt.*)|(.*\/ssl\?paymentId=.*)/,
            hostname: "migs.mastercard.com.au",
            domCheck: function(){return  document.querySelector('form[name="PAReq"]');},
            state: "MIGSPAReq",
            bank: "",
            action: function(){
                /* MiGS has an artificial 5 sec wait.
                   Clearing it and submitting the form*/
                if(__juspay.clearAllTimersHack()) {
                    document.PAReq.submit();
                }
            }
        },
        //PMCNB customer id page
        {
            path: /\/ib-retail-web\/tenant\/index/,
            hostname: "paybill.pmcbank.co.in",
            state: "UNUSED",
            local: false,
            bank: "PMCNB",
            domCheck: function(){
                return document.querySelector('input[name=login][id=login]') &&
                        document.querySelector('input[name=preLoginSubmit][id=preLoginSubmit]');
            },
            action: function() {
                var custIdField = document.querySelector('input[name=login][id=login]');
                var submitButton = document.querySelector('input[name=preLoginSubmit][id=preLoginSubmit]');
                custIdField.focus();
                Gatekeeper.requestPasswordKeyboardShow();
                __juspay.scrollToElement(submitButton);
                Gatekeeper.showNetbankingCustomerIdFragment("Continue");
            },
            nextAction: function(){
                var submitButton = document.querySelector('input[name=preLoginSubmit][id=preLoginSubmit]');
                __juspay.clickOnElement(submitButton);
            }
        },
        //HDFC Rupay card OTP page
        {
            path: /\/ACSWeb\/RupayRedirectAuthentication/,
            hostname: "netsafe.hdfcbank.com",
            state: "UNUSED",
            local: false,
            bank: "HDFC",
            domCheck: function(){
                return document.querySelector('input[id="otpPassword"]') &&
                        document.querySelector('a[href*="resendotp"]') &&
                        document.querySelector('img[src*="rupay-logo"]') &&
                        document.querySelector('input[id="cmdSubmit"]');
            },
            action: function() {
                Gatekeeper.setCardBrand(RUPAY);
                if(__juspay.isRupayEnabled("HDFC")){
                    otpFragmentHelper('input[id="otpPassword"]','a[href*="resendotp"]');
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id="otpPassword"]','input[id="cmdSubmit"]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[href*="resendotp"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },

        //Andra Rupay card OTP page
        {
            path: /\/NPCI_IAS_AB/,
            hostname: "securepayments.andhrabank.in",
            state: "UNUSED",
            local: false,
            bank: "ANDHDC",
            domCheck: function(){
                return document.querySelector('input[id=txtotp]') &&
                        document.querySelector('a[id=btnregn]') &&
                        document.querySelector('img[src*="rupay"]') &&
                        document.querySelector('input[id=btnverify]');
            },
            action: function() {
                Gatekeeper.setCardBrand(RUPAY);
                if(__juspay.isRupayEnabled("ANDHDC")){
                    otpFragmentHelper('input[id=txtotp]','a[id=btnregn]');
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id=txtotp]','input[id="btnverify"]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[id=btnregn]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //VIJAYA Rupay card OTP page
        {
            path: /\/Rupay\/ias/,
            hostname: "www.vijayabankonline.com",
            state: "UNUSED",
            local: false,
            bank: "VIJAYADC",
            domCheck: function(){
                return document.querySelector('input[id=otp]') &&
                        document.querySelector('a[id=linkResend]') &&
                        document.querySelector('img[src*="RuPayLogo"]') &&
                        document.querySelector('input[id=btnSubmit]');
            },
            action: function() {
                Gatekeeper.setCardBrand(RUPAY);
                if(__juspay.isRupayEnabled("VIJAYADC")){
                    otpFragmentHelper('input[id=otp]','a[id=linkResend]');
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[id=otp]','input[id="btnSubmit"]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[id=linkResend]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //SBI Rupay card OTP page
        {
            path: /\/bdacs\/SBIRUPAYDetails/,
            hostname: "acs2.onlinesbi.com",
            state: "UNUSED",
            local: false,
            bank: "SBIDC",
            domCheck: function(){
                return document.querySelector('input[name="customerotp"]') &&
                        document.querySelector('button[onclick*="ValidateForm"]') &&
                        document.querySelector('img[src*="rupay-logo"]') &&
                        document.querySelector('a[class="request-link"]') &&
                        document.querySelector('img[src*="sbi-logo"]');
            },
            action: function() {
                Gatekeeper.setCardBrand(RUPAY);
                if(__juspay.isRupayEnabled("SBIDC")){
                    otpFragmentHelper('input[name="customerotp"]','a[class="request-link"]');
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp,'input[name="customerotp"]','button[onclick*="ValidateForm"]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[class="request-link"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //IND RUPAY new page support
        {
            path: /\/NPCI_IAS_IB\/authOTP/,
            hostname: "prdrupayias.insolutionsglobal.com",
            state: "UNUSED",
            local: false,
            bank: "INDDC",
            domCheck: function(){
                return document.querySelector('input[id=txtotp][type=password][name=txtotp]') &&
                        document.querySelector('input[id=btnverify]') &&
                        document.querySelector('a[id=btnregn]') &&
                        document.querySelector('img[src*=NPCI_IAS_IB]') &&
                        document.querySelector('img[src*=rupay]');
            },
            action: function(){
                Gatekeeper.setCardBrand(RUPAY);
                var otpField = document.querySelector('input[id=txtotp][type=password][name=txtotp]');
                if(otpField &&__juspay.isRupayEnabled("INDDC")) {
                    otpFragmentHelper('input[id=txtotp][type=password][name=txtotp]','a[id=btnregn]');
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=txtotp][type=password][name=txtotp]', 'input[id=btnverify]');
                Gatekeeper.removeFragment("OTP Submitted");
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[id=btnregn]');
                __juspay.clickOnElement(regenerateBtn);
            }
        },
        //CUBDC RUPAY PAGE
        {
            path:  /\/naradaacsweb\/acs\/authenticate/,
            hostname: "paysecure.yalamanchili.in",
            state: "UNUSED",
            bank: "CUBDC",
            local : false,
            domCheck: function(){
                return document.querySelector('input[id=otp]') &&
                        document.querySelector('input[id="otpbut"][value="Submit"]') &&
                        document.querySelector('img[src*="RUPAY"]');
            },
            action: function(){
                var cubLogo = document.querySelector('img[src*=CUBDD_logo]');
                var lvbLogo = document.querySelector('img[src*=LVBDD_logo]');
                var bankName = "";
                Gatekeeper.setCardBrand(RUPAY);
                if(cubLogo){
                    bankName = "CUBDC";
                }else if(lvbLogo){
                    bankName = "LVBDC";
                }else{
                    getImageInformation();
                }
                if(bankName !== "") {
                    Gatekeeper.setBank(bankName);
                    if(__juspay.isRupayEnabled(bankName)){
                        otpFragmentHelper('input[id=otp]');
                    }
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=otp]', 'input[id="otpbut"][value="Submit"]');
            }
        },
        //COMMON ENSTAGE NEW RUPAY PAGE
        {
            path: /\/rupay-web-v1\/EnrollWeb\/NPCI\/server\/AcquirerHandler/,
            hostname: /(rupay-fb|rupay-sb|rupay-fsb|redtmprup|rup-sb2)\.enstage-sas\.com/,
            state: "UNUSED",
            local: false,
            bank: "",
            domCheck: function(){
                return document.querySelector('input[id="otpPassword"]') &&
                        (document.querySelector('a[onclick*="validateOTP"]') || document.querySelector('input[id="submitButton"]')) &&
                        document.querySelector('a[href*="resendotp"]') &&
                        document.querySelector('img[src*="rupay-logo"]');
            },
            action: function(){
                var otpField = document.querySelector('input[id="otpPassword"]');
                var pnbLogo = document.querySelector('img[src*="8170/images/bank-logo"]');
                var axisLogo = document.querySelector('img[src*="8111/images/bank-logo"]');
                var idbiLogo = document.querySelector('img[src*="8114/images/bank-logo"]');
                var indLogo = document.querySelector('img[src*="8122/images/bank-logo"]');
                var ktbLogo = document.querySelector('img[src*="8131/images/bank-logo"]');
                var canaraLogo = document.querySelector('img[src*="8121/images/bank-logo"]');
                var iobLogo = document.querySelector('img[src*="8119/images/bank-logo"]');
                var andhraLogo = document.querySelector('img[src*="8113/images/bank-logo"]');
                var ucoLogo = document.querySelector('img[src*="8136/images/bank-logo"]');
                var southIndianLogo = document.querySelector('img[src*="8166/images/bank-logo"]');
                var paytmLogo = document.querySelector('img[src*="8195/images/bank-logo"]');
                var sgbLogo = document.querySelector('img[src*="SGB/bank-logo"]');
                var bankName = "";
                Gatekeeper.setCardBrand(RUPAY);
                if(indLogo){
                    bankName = "INDDC";
                }else if(axisLogo){
                    bankName = "AXISDC";
                }else if(idbiLogo){
                    bankName = "IDBIDC";
                }else if(sgbLogo){
                    bankName = "SGBDC";
                }else if(pnbLogo){
                    bankName = "PNBDC";
                }else if(ktbLogo){
                    bankName = "KTBDC";
                }else if(canaraLogo){
                    bankName = "CANARADC";
                }else if(iobLogo){
                    bankName = "IOBDC";
                }else if(andhraLogo){
                    bankName = "ANDHDC";
                }else if(ucoLogo){
                    bankName = "UCODC";
                }else if(southIndianLogo){
                    bankName = "SIDC";
                }else if(paytmLogo){
                    bankName = "PAYTM";
                }else{
                    getImageInformation();
                }
                if(bankName !== "") {
                    Gatekeeper.setBank(bankName);
                    if(__juspay.isRupayEnabled(bankName)){
                        otpFragmentHelper('input[id="otpPassword"]','a[href*="resendotp"]');
                    }
                }
            },
            submitOtp: function(otp){
                submitBtn1 = document.querySelector('a[onclick*="validateOTP"]');
                submitBtn2 = document.querySelector('input[id="submitButton"]');
                if(submitBtn1){
                    submitOtpHelper(otp, 'input[id="otpPassword"]', 'a[onclick*="validateOTP"]');
                }
                if(submitBtn2){
                    submitOtpHelper(otp, 'input[id="otpPassword"]', 'input[id="submitButton"]');
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[href*="resendotp"]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //COMMON FISGLOBAL NEW RUPAY PAGE
        {
            path:  /\/app\/ui\/v2.0\/paysec\/otppage/,
            hostname: "dps.fisglobal.com",
            state: "UNUSED",
            bank: "",
            local : false,
            domCheck: function(){
                return document.querySelector("img[src*='RuPayLogo']") &&
                        document.querySelector('button[class*="btn-primary"][value="Submit"]') &&
                        document.querySelector('p[id="lnk"] a') &&
                        document.querySelector('input[id=password]');
            },
            action: function(){
                var otpField = document.querySelector('input[id=password]');
                var paytmLogo = document.querySelector("img[src*='608032/logo']");
                var bomLogo = document.querySelector("img[src*='652155/logo']");
                var bankName = "";
                Gatekeeper.setCardBrand(RUPAY);
                if(paytmLogo){
                    bankName = "PAYTM";
                }else if(bomLogo){
                    bankName = "BOMDC";
                }else{
                    getImageInformation();
                }
                if(bankName !== "") {
                    Gatekeeper.setBank(bankName);
                    if(__juspay.isRupayEnabled(bankName)){
                        otpFragmentHelper('input[id=password]','p[id="lnk"] a');
                    }
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id=password]', 'button[class*="btn-primary"][value="Submit"]');
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('p[id="lnk"] a');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //SYND Rupay Support
        {
           path: /\/ReachWeb\/Renderer\/common\/validateotp\.jsp/,
           hostname: "tab.syndicatebank.in",
           state: "UNUSED",
           local: false,
           bank: "SYNDC",
           domCheck: function(){
               return document.querySelector("input[id='otpval']") &&
                        document.querySelector("input[id='Submit']") &&
                        document.querySelector("base[href*='syndicatebank']") &&
                        document.querySelector("a[id='regen']") &&
                        document.querySelector("img[src*='rupay-logo']");
           },
           action: function(){
               Gatekeeper.setCardBrand(RUPAY);
               if(__juspay.isRupayEnabled("SYNDC")){
                  otpFragmentHelper('input[id="otpval"]','a[id="regen"]');
               }
           },
           submitOtp: function(otp){
               submitOtpHelper(otp, 'input[id="otpval"]', 'input[id="Submit"]');
           },
           regenerateOtp: function(){
               var regenerateBtn = document.querySelector("a[id='regen']");
               if(regenerateBtn){
                  __juspay.clickOnElement(regenerateBtn);
               }
           }
        },
        //Allahabad Rupay Support
        {
           path: /\/servlet\/ibs\.servlets\.IBSPaysecureControlGiverServlet/,
           hostname: "allbankonline.in",
           state: "UNUSED",
           local: false,
           bank: "ALLBDC",
           domCheck: function(){
               return document.querySelector("input[name='otp']") &&
                        document.querySelector("input[name='subm']") &&
                        document.querySelector("img[src*=AllahabadBank]") &&
                        document.querySelector("img[src*=rupay]");
           },
           action: function(){
               Gatekeeper.setCardBrand(RUPAY);
               if(__juspay.isRupayEnabled("ALLBDC")){
                  otpFragmentHelper('input[name="otp"]');
               }
           },
           submitOtp: function(otp){
               submitOtpHelper(otp, 'input[name="otp"]', 'input[name="subm"]');
           }
        },
        //COMMON FSSNET NEW RUPAY PAGE
        {
            path: /\/acsauthserver\/redirectrefresh/,
            hostname: "acs.fssnet.co.in",
            state: "UNUSED",
            local: false,
            bank: "",
            domCheck: function(){
                return document.querySelector("input[id='otpval']") &&
                        document.querySelector("input[id='otpsubmit']") &&
                        document.querySelector("img[class='rupayLogo']") &&
                        (document.querySelector("input[href='resendOTP']") || document.querySelector("a[href*='resendOTP']"));
            },
            action: function(){
                var ubiLogo = document.querySelector("input[value='UBI']");
                var cbiLogo =document.querySelector("input[value='CBI']");
                var bankName = "";
                Gatekeeper.setCardBrand(RUPAY);
                if(ubiLogo){
                    bankName = "UBIDC";
                }else if(cbiLogo){
                    bankName = "CBIDC";
                }else{
                    getImageInformation();
                }
                if(bankName !== "") {
                    Gatekeeper.setBank(bankName);
                    if(__juspay.isRupayEnabled(bankName)){
                        var regenerateBtn1 = document.querySelector("input[href='resendOTP']");
                        var regenerateBtn2 = document.querySelector("a[href*='resendOTP']");
                        if(regenerateBtn1){
                            otpFragmentHelper('input[id="otpval"]','input[href="resendOTP"]');
                        }else if(regenerateBtn2){
                            otpFragmentHelper('input[id="otpval"]','a[href*="resendOTP"]');
                        }
                    }
                }
            },
            submitOtp: function(otp){
                submitOtpHelper(otp, 'input[id="otpval"]', 'input[id="otpsubmit"]');
            },
            regenerateOtp: function(){
                var regenerateBtn1 = document.querySelector("input[href='resendOTP']");
                var regenerateBtn2 = document.querySelector("a[href*='resendOTP']");
                if(regenerateBtn1){
                   __juspay.clickOnElement(regenerateBtn1);
                }
                else if(regenerateBtn2){
                   __juspay.clickOnElement(regenerateBtn2);
                }
            }
        },
        //COMMON FSSNET NEW VISA PAGE
        {
            path:  /\/acsauthserver\/staticrefresh.htm/,
            hostname: "acs.fssnet.co.in",
            state: "UNUSED",
            bank: "",
            local : false,
            domCheck: function(){
                return  document.querySelector("input[id=OTPmain]") &&
                          document.querySelector("input[id=otproceed]") &&
                          document.querySelector("input[id=resenOTPBut]");
            },
            action: function() {
               var ubiLogo = document.querySelector("input[value='UBI']");
               var visaLogo = document.querySelector("img[class*='visaLogo']");
               var bankName = "";
               if(visaLogo){
                   Gatekeeper.setCardBrand(VISA);
               }
               if(ubiLogo){
                   bankName = "UBIDC";
               }
               else{
                   getImageInformation();
               }
               if(bankName !== "") {
                   Gatekeeper.setBank(bankName);
                   otpFragmentHelper("input[id=OTPmain]","input[id=resenOTPBut]");
                }
             },
            submitOtp: function(otp) {
                submitOtpHelper(otp, "input[id=OTPmain]", "input[id=otproceed]");
                },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector("input[id=resenOTPBut]");
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        {
            path:  /txns\/consent/,
            hostname: /(sandbox|api)\.juspay\.in/,
            state: "UNUSED",
            local: false,
            bank: "TOKENIZATIONCONSENT",
            domCheck: function(){
                return document.querySelector('button[id="securePay"][type="button"]') &&
                        document.querySelector('button[id="skip"][type="button"]');
            },
            action: function(){
                  var skipButton = document.querySelector('button[id="skip"][type="button"]');
                  var allowButton = document.querySelector('button[id="securePay"][type="button"]');
                  var juspaySpaceDiv = document.getElementById("juspaySpaceDiv");
                  juspaySpaceDiv.remove(); // removing to fix scroll issue
                  skipButton.addEventListener("click", function (){
                    window.trackEventv2_0("acs","info","juspay_click","Consent Skipped for Tokenization", "juspay", "click");
                  }, false);
                  allowButton.addEventListener("click", function (){
                    window.trackEventv2_0("acs","info","juspay_click","Consent Allowed for Tokenization", "juspay", "click");
                  }, false);
            },
             backButtonPressed: function() {
                  try {
                      var skipButton = document.querySelector('button[id="skip"][type="button"]');
                      skipButton.click();
                  } catch(err) {
                      GK.showCancelTransactionDialog();
                  }
             }
        },
        //Irctc login page - mmt
        {
            path: /\/eticketing\/(wsapplogin|home)/,
            hostname: "www.irctc.co.in",
            state: "UNUSED",
            bank: "IRCTC_LOGIN",
            local : false,
            domCheck: function(){
                return document.querySelector('input[name=j_password][type=password]') &&
                       document.querySelector('input[name=j_captcha][type=text]') &&
                       document.querySelector('input[id=loginbuttonw][type=button]') &&
                       document.querySelector('input[id=loginCancel][type=button]') &&
                       document.querySelector('input[id=loginForget][type=button]')
            },
            action: function (){
                try {
                    Gatekeeper.removeFragment('Reached Irctc Login Page');
                    var capchaField = document.querySelector('input[name=j_captcha][type=text]');
                    var submitBtn = document.querySelector('input[id=loginbuttonw][type=button]') ;
                    var forgotPasswordBtn = document.querySelector('input[id=loginForget]');
                    var userIdLabel = document.querySelectorAll('div[class="col-xs-5 labeltxt"]') && document.querySelectorAll('div[class="col-xs-5 labeltxt"]')[0];
                    var passwordLabel = document.querySelectorAll('div[class="col-xs-5 labeltxt"]') && document.querySelectorAll('div[class="col-xs-5 labeltxt"]')[1];
                    var captchaLabel = document.querySelectorAll('div[class="col-xs-5 labeltxt"]') && document.querySelectorAll('div[class="col-xs-5 labeltxt"]')[2];
                    var passwordField = document.querySelector('input[name=j_password][type=password]');
                    var cancelBtn = document.querySelector('input[id=loginCancel][type=button]');
                    var capchaWarningText = document.querySelectorAll('div[class="col-xs-12 labeltxt"]') && document.querySelectorAll('div[class="col-xs-12 labeltxt"]')[0];

                    this.capchaFieldFocus = function(){
                        Gatekeeper.removeFragment("Capcha field selected");

                    }
                    this.passwordFieldFocus = passwordFragmentHelper('input[name=j_password][type=password]');

                    capchaField.addEventListener('focus',this.capchaFieldFocus,false);

                    submitBtn.addEventListener('click',function(){
                        window.trackEventv2_0("acs", "info", "IRCTC_LOGIN_PAGE", "submitButtonClicked", "irctc", "login_page");
                    },false);
                    cancelBtn.addEventListener('click',function(){
                        window.trackEventv2_0("acs", "info", "IRCTC_LOGIN_PAGE", "cancelButtonClicked", "irctc", "login_page");
                    },false);
                    forgotPasswordBtn.addEventListener('click',function(){
                        window.trackEventv2_0("acs", "info", "IRCTC_LOGIN_PAGE", "forgotPasswordButtonClicked", "irctc", "logic_page");
                        Gatekeeper.setSessionAttribute("userOptedForgotPassword", "true")
                    },false);

                    if(__juspay.isMobUiEnabled("MOBUI_IRCTC_LOGIN")){
                        try{

                            if( userIdLabel.innerText == "User ID :"){
                                userIdLabel.style.fontSize = "15px";
                                userIdLabel.style.fontWeight = "bold";
                                userIdLabel.parentElement.style.marginTop = "10px"
                                userIdLabel.parentElement.style.marginLeft = "auto"
                            }
                            if( passwordLabel.innerText == "Password :"){
                                passwordLabel.style.fontSize = "15px";
                                passwordLabel.style.fontWeight = "bold";
                                passwordLabel.parentElement.style.marginTop = "10px"
                                passwordLabel.parentElement.style.marginLeft = "auto"
                            }
                            if( captchaLabel.innerText == "Captcha :"){
                                captchaLabel.style.fontSize = "15px";
                                captchaLabel.style.fontWeight = "bold";
                                captchaLabel.parentElement.style.marginTop = "10px"
                                captchaLabel.parentElement.style.marginLeft = "auto"
                            }

                            if (capchaWarningText.innerText == "Captcha letters are case sensitive and to be entered in Upper Case only"){
                                capchaWarningText.style.color = "red";
                            }
                            passwordField.style.width = "165%";
                            passwordField.style.height = "30px";
                            passwordField.style.fontSize = "20px";

                            capchaField.style.width = "165%";
                            capchaField.style.height = "30px";
                            capchaField.style.fontSize = "20px";

                            submitBtn.style.width = "50%";
                            submitBtn.style.height = "33px";
                            submitBtn.style.float = "right";
                            submitBtn.style.fontSize = "14px";
                            submitBtn.style.color = "black";
                            submitBtn.style.fontWeight = "bold";
                            submitBtn.style.background = "#8db7f3";
                            submitBtn.style.marginRight = "5%";

                            cancelBtn.style.width = "36%";
                            cancelBtn.style.height = "33px";
                            cancelBtn.style.float = "left";
                            cancelBtn.style.fontSize = "14px";
                            cancelBtn.style.color = "black";
                            cancelBtn.style.fontWeight = "bold";
                            cancelBtn.style.background = "#8db7f3";
                            cancelBtn.style.marginLeft = "-12%";
                            cancelBtn.style.background = "#979797";

                            forgotPasswordBtn.style.float = "left";
                            forgotPasswordBtn.style.border = "none";
                            forgotPasswordBtn.style.width = "30%";
                            forgotPasswordBtn.style.background = "white";
                            forgotPasswordBtn.style.color = "#4A90E2";
                            forgotPasswordBtn.style.fontWeight = "bold";
                            forgotPasswordBtn.style.marginLeft = "6%";
                            forgotPasswordBtn.style.marginTop = "5%"
                            forgotPasswordBtn.style.textDecoration = "underline";
                            forgotPasswordBtn.removeAttribute('type');
                        } catch(err) {
                            __juspay.CatchPageModifyError("MOBUI",err,"IRCTCLOGIN");
                        }
                    }
                } catch(err) {
                    console.error(err)
                }

            },
            clickSubmitButton: function(){
                var submitBtn = document.querySelector('input[id=loginbuttonw][type=button]') ;
                __juspay.clickOnElement(submitBtn);
            }
//  ** Making this feature non-functionable as per merchant request **

//            showOtpFragmentOnManualRequest: function(){
//                var capchaField = document.querySelector('input[name=j_captcha][type=text]');
//                if(capchaField){
//                    capchaField.removeEventListener('focus', this.capchaFieldFocus, false);
//                    capchaField.addEventListener('click',function(){
//                        Gatekeeper.hideAssistanceFragment();
//                    },false)
//                }
//                if(typeof this.passwordFieldFocus == "function"){
//                    this.passwordFieldFocus();
//                }
//                Gatekeeper.removeFragment("showOtpFragmentOnManualRequest function called");
//                otpFragmentHelper("input[name=j_password][type=password]","",true);
//                Gatekeeper.requestKeyboardHide();
//            },
//
//            submitOtp: function(otp){
//                var submitBtn = document.querySelector('input[id=loginbuttonw][type=button]') ;
//                var passwordField = document.querySelector('input[name=j_password][type=password]');
//                var capchaField = document.querySelector('input[name=j_captcha][type=text]');
//                if(submitBtn && passwordField){
//                    if((!capchaField) || (capchaField.value != "")){
//                        submitOtpHelper(otp, "input[name=j_password][type=password]", "input[id=loginbuttonw][type=button]");
//                    }else{
//                        focusElement(capchaField);
//                        passwordField.value = otp;
//                    }
//                    Gatekeeper.removeFragment("OTP Submitted");
//                }
//            }
        },
        //Vijaya Bank RUPAY PAGE
        {
            path:  /.*/,
            hostname: "www.payments.vijayabankonline.in",
            state: "UNUSED",
            bank: "VIJAYADC",
            local : false,
            domCheck: function(){
                return true;
            },
            action: function(){
                if(typeof Gatekeeper.onIframeFinished == "function") {
                    Gatekeeper.onIframeFinished();
                }
                window.addEventListener("message",GK.receiveMessage,false);
                setTimeout(function(){
                    var otpField = document.querySelector("input[id='otp']");
                    if (otpField){
                        Gatekeeper.setCardBrand(RUPAY);
                        if( __juspay.isRupayEnabled("VIJAYADC")) {
                            otpFragmentHelper('input[id=otp]');
                        }
                    }else{
                       window.trackEventv2_0("acs", "info", "dom_check_failure", "vijayadc_otp_page", "dom_check", "failure");
                       Gatekeeper.trackAction("acs","info","page",JSON.stringify({is_dom_check_successful : false, url : window.location.href }));
                    }
                },2000);
            },
            submitOtp: function(otp){
                var otpField = document.querySelector("input[id='otp']");
                var submitBtn = document.querySelector("input[id='ButtonValidate']");
                if (otpField && submitBtn){
                    submitOtpHelper(otp, 'input[id=otp]', 'input[id="ButtonValidate"]');
                }else{
                    window.trackEventv2_0("acs", "info", "dom_check_failure", "vijayadc_otp_page", "dom_check", "failure");
                    Gatekeeper.trackAction("acs","info","page",JSON.stringify({is_dom_check_successful : false, url : window.location.href }));
                }
            }
        },
        //COMMON FISGLOBAL RUPAY PAGE
        {
            path:  /.*/,
            hostname: "ias.fisglobal.com",
            state: "UNUSED",
            bank: "",
            local : false,
            domCheck: function(){
                return true;
            },
            action: function(){
                if(typeof Gatekeeper.onIframeFinished == "function") {
                    Gatekeeper.onIframeFinished();
                }
                window.addEventListener("message",GK.receiveMessage,false);
                setTimeout(function(){
                    var otpField = document.querySelector('input[id=password]');
                    var paytmLogo = document.querySelector("img[src*='608032/logo']");
                    var bomLogo = document.querySelector("img[src*='652155/logo']");
                    var bankName = "";
                    if(paytmLogo){
                        bankName = "PAYTM";
                    }else if(bomLogo){
                        bankName = "BOMDC";
                    }else{
                        getImageInformation();
                    }
                    if(isObject(otpField) && bankName !== "") {
                        Gatekeeper.setBank(bankName);
                        Gatekeeper.setCardBrand(RUPAY);
                        if(__juspay.isRupayEnabled(bankName)){
                            otpFragmentHelper('input[id=password]','p[id="lnk"] a');
                        }
                    }
                },2000);
            },
            submitOtp: function(otp){
                var submitBtn = document.querySelector('input[class*="myButton"][value="Submit"]');
                var otpField = document.querySelector('input[id=password]');
                if (otpField && submitBtn){
                    submitOtpHelper(otp, 'input[id=password]', 'input[class*="myButton"][value="Submit"]');
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('p[id="lnk"] a');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },

        //COMMON ENSTAGE RUPAY PAGE
        {
            path:  /.*/,
            hostname: /(rupay|rupay-pnb|www)\.(enstage-sas|billdesk)\.(in|com)/,
            state: "UNUSED",
            bank: "",
            local : false,
            domCheck: function(){
                return true;
            },
            action: function(){
                initRupayIframe(); //Adding because top and iframe both have billdesk url
                if(typeof Gatekeeper.onIframeFinished == "function") {
                    Gatekeeper.onIframeFinished();
                }
                window.addEventListener("message",GK.receiveMessage,false);
                setTimeout(function(){
                    var otpField = document.querySelector('input[id=otpPassword]');
                    var bankImg = document.querySelector("div[class=container]");
                    if(bankImg){
                        var style = bankImg.currentStyle || window.getComputedStyle(bankImg, false);
                        var bankLogo = style.backgroundImage;
                        var bankName = "";
                        if(bankLogo.indexOf("8170_npci_bg")>-1){
                            bankName = "PNBDC";
                        }else if(bankLogo.indexOf("8131_npci_bg")>-1){
                            bankName = "KTBDC";
                        }else if(bankLogo.indexOf("8121_npci_bg")>-1){
                            bankName = "CANARADC";
                        }else if(bankLogo.indexOf("8111_npci_bg")>-1){
                            bankName = "AXISDC";
                        }else if(bankLogo.indexOf("8114_npci_bg")>-1){
                            bankName = "IDBIDC";
                        }else if(bankLogo.indexOf("8122_npci_bg")>-1){
                            bankName = "INDDC";
                        }else if(bankLogo.indexOf("8129_npci_bg")>-1){
                            bankName = "BOBDC";
                        }else if(bankLogo.indexOf("8164_npci_bg")>-1){
                          bankName = "UBIDC";
                        }else if(bankLogo.indexOf("8119_npci_bg")>-1){
                          bankName = "IOBDC";
                        }else{
                            window.trackEventv2_0("acs","info","bank_image", bankLogo, "bank", "image");
                            getImageInformation();
                        }
                        if(otpField && bankName !== "") {
                            Gatekeeper.setBank(bankName);
                            Gatekeeper.setCardBrand(RUPAY);
                            if(__juspay.isRupayEnabled(bankName)){
                                otpFragmentHelper('input[id=otpPassword]','span[class=resend]>a[href*=resendotp]');
                                if(__juspay.isMobUiEnabled("MOBUI_RUPAY")){
                                    try{
                                        var metaTag=document.createElement('meta');
                                        metaTag.name = "viewport"
                                        metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                                        var regenerateBtn = document.querySelector('span[class=resend]');
                                        var cancelBtn = document.querySelector('a[class=cancel]');
                                        regenerateBtn.style.marginTop="30px";
                                        cancelBtn.style.float="left";
                                        cancelBtn.style.marginTop="10px";
                                        window.trackEventv2_0default("acs","info","MOBUI","OTP_PAGE");
                                    } catch(err) {
                                        __juspay.CatchPageModifyError("MOBUI",err,"OTP_PAGE");
                                    }
                                }
                            }
                        }
                    }
                },2000);
            },
            submitOtp: function(otp){
                var submitBtn = document.querySelector('a[class*=submitotp]');
                var otpField = document.querySelector('input[id=otpPassword]');
                if (otpField && submitBtn){
                    submitOtpHelper(otp, 'input[id=otpPassword]', 'a[class*=submitotp]');
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('span[class=resend]>a[href*=resendotp]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //COMMON BILLDESK RUPAY PAGE
        {
            path:  /.*/,
            hostname: "online.billdesk.com",
            state: "UNUSED",
            bank: "",
            local : false,
            domCheck: function(){
                return true;
            },
            action: function(){
                if(typeof Gatekeeper.onIframeFinished == "function") {
                    Gatekeeper.onIframeFinished();
                }
                window.addEventListener("message",GK.receiveMessage,false);
                setTimeout(function(){
                    var otpField = document.querySelector('input[id=otpPassCode]');
                    var sbiLogo = document.querySelector('img[src*=sbi-logo]');
                    var bankName = "";
                    if(sbiLogo){
                        bankName = "SBIDC";
                    }else{
                        getImageInformation();
                    }
                    if(isObject(otpField) && bankName !== "") {
                        Gatekeeper.setBank(bankName);
                        Gatekeeper.setCardBrand(RUPAY);
                        if( __juspay.isRupayEnabled(bankName)){
                            otpFragmentHelper('input[id=otpPassCode]','a[class=request-link]');
                        }
                    }
                },2000);
            },
            submitOtp: function(otp){
                var submitBtn = document.querySelector('button[id=proceed]');
                var otpField = document.querySelector('input[id=otpPassCode]');
                if (otpField && submitBtn){
                    submitOtpHelper(otp, 'input[id=otpPassCode]', 'button[id=proceed]');
                }
            },
            regenerateOtp: function(){
                var regenerateBtn = document.querySelector('a[class=request-link]');
                if(regenerateBtn){
                    __juspay.clickOnElement(regenerateBtn);
                }
            }
        },
        //COMMON yalamanchili RUPAY PAGE
        {
            path:  /.*/,
            hostname: "paysecure.yalamanchili.in",
            state: "UNUSED",
            bank: "",
            local : false,
            domCheck: function(){
                return true;
            },
            action: function(){
                if(typeof Gatekeeper.onIframeFinished == "function") {
                    Gatekeeper.onIframeFinished();
                }
                window.addEventListener("message",GK.receiveMessage,false);
                setTimeout(function(){
                    var otpField = document.querySelector('input[id=otp]');
                    var cubLogo = document.querySelector('img[src*=CUBDD_logo]');
                    var lvbLogo = document.querySelector('img[src*=LVBDD_logo]');
                    var bankName = "";
                    if(cubLogo){
                        bankName = "CUBDC";
                    }else if(lvbLogo){
                        bankName = "LVBDC";
                    }else{
                        getImageInformation();
                    }
                    if(isObject(otpField) && bankName !== "") {
                        Gatekeeper.setBank(bankName);
                        Gatekeeper.setCardBrand(RUPAY);
                        if( __juspay.isRupayEnabled(bankName)) {
                            otpFragmentHelper('input[id=otp]','');
                        }
                    }
                },2000);
            },
            submitOtp: function(otp){
                var submitBtn = document.querySelector('input[id="otpbut"][value="Submit"]');
                var otpField = document.querySelector('input[id=otp]');
                if (otpField && submitBtn){
                    submitOtpHelper(otp, 'input[id=otp]', 'input[id="otpbut"][value="Submit"]');
                }
            }
        },
        {
            path : /.*/,
            hostname : "www.firstdatamerchantservices.com",
            domCheck: function(){
                return true;
            },
            action : function(){
                initRupayIframe();
            }
        },
        {
            path : /.*/,
            hostname : "www.billdesk.com",
            domCheck: function(){
                return true;
            },
            action : function(){
               initRupayIframe();
            }
        },
        {
            path : /.*/,
            hostname : /sodexo\.(com|in)/,
            state: "UNUSED",
            local: false,
            bank: "sodexo",
            domCheck: function(){
                return true;
            },
            action : function(){
               initRupayIframe();
            }
        },
        //VIES 3DS
        {
            path : /pay\/start/,
            hostname : /(sandbox|api)\.juspay\.in/,
            domCheck: function(){
                return (window.juspay_payload && window.juspay_payload.returnUrl && (window.juspay_payload.returnUrl.indexOf("vies")!=-1));
            },
            action : function(){
                //Wait for the 3DS iframe to be attached
                window.isPostMessageEnabled = true;
                if(window.MutationObserver) {
                    var options = { childList: true, subtree: true };

                    var callback = function(mutations) {
                        for(var i = 0; i < mutations.length; i++) {
                            var mutation = mutations[i];

                            if(mutation.type == "childList" && mutation.addedNodes) {
                                for(var j = 0; j < mutation.addedNodes.length; j++) {
                                    var node = mutation.addedNodes[j];

                                    if(node.id == "Cardinal-ElementContainer") {
                                        var iframe = node.querySelector('iframe#Cardinal-CCA-IFrame');
                                        if(iframe) {
                                            //set window.iframe so that dui events can be communicated to it with postMessage()
                                            window.iframe = iframe.contentWindow;

                                            //Make bank iframe full screen
                                            if(__juspay.isMobUiEnabled("MOBUI_CARDINAL")){
                                                try{
                                                    var iframeDiv = document.querySelector('div[id = "Cardinal-ModalContent"]');
                                                    var cardinalDiv = document.querySelector('div[id = "Cardinal-Modal"]');
                                                    cardinalDiv.style.height="100%";
                                                    cardinalDiv.style.width="100%";
                                                    iframeDiv.style.height="100%";
                                                    iframeDiv.style.width="100%";
                                                    iframe.style.height="100%";
                                                    iframe.style.width="100%";
                                                    window.trackEventv2_0default("acs","info","MOBUI","CARDINAL_OTP_PAGE");
                                                } catch(err) {
                                                    __juspay.CatchPageModifyError("MOBUI",err,"CARDINAL_OTP_PAGE");
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    var observer = new MutationObserver(callback);
                    observer.observe(document.body, options);
                }
            }
        },
        {
            path:/\/pay\/start\/zoomcar/,
            hostname: "api.juspay.in",
            state: "UNUSED",
            local: false,
            bank: "",
            domCheck: function() {
                return document.querySelector('form[id="otpForm"]');
            },
            action: function(){
                Gatekeeper.hideWaitingFragment();
            }
        },
//        {
//            path: /\/(v2|v3|vies)\/pay\/start/,
//            hostname: "api.juspay.in",
//            state: "UNUSED",
//            local: false,
//            bank: "",
//            domCheck: function() {
//                return true;
//            },
//            action: function(){
//                Gatekeeper.showWaitingFragment();
//            }
//        },
//        {
//            path:/\/pay\/start/,
//            hostname: "api.juspay.in",
//            state: "UNUSED",
//            local: false,
//            bank: "",
//            domCheck: function() {
//                return document.querySelector('div[id="juspay-logo-container"][style="display:none;"]');
//            },
//            action: function(){
//                Gatekeeper.showWaitingFragment();
//            }
//        },
        //EC
        {//Setting viewport meta tag for ExpressCheckout pages
            path:/\//,
            hostname: "api.juspay.in",
            state: "UNUSED",
            local: false,
            bank: "",
            domCheck: function() {
                return true;
            },
            action: function(){
                var metaTag=document.createElement('meta');
                metaTag.name = "viewport"
                metaTag.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
                if(document.getElementsByTagName('head')[0]){
                    document.getElementsByTagName('head')[0].appendChild(metaTag);
                }
            }
        },
        {
            path : /.*/,
            hostname : /.*/,
            logDomCheckFalse : true,
            domCheck: function(){
                return true;
            },
            action : function(){
                initRupayIframe();
            }
        }
    ];


    var networkType = null;
    var currentFragment = null;
    var currentAppState = null;
    var godelVersion = null;
    var godelRemotesVersion = null;
    listenForPostMessages();    //not compatible with 0.6rc9
    var page = applyPageRules();

    if(page) {
         if(typeof(page.bank) != "undefined") {
            GK.setBank(page.bank);

            var bank = page.bank;
            var bankImageSourceMap = [
                   {"bank":["AMEXCC"],                                  "bankImage":"amex"},
                   {"bank":["AXIS","AXISNB"],                           "bankImage":"axis"},
                   {"bank":["BOI"],                                     "bankImage":"boi"},
                   {"bank":["PNBDC"],                                   "bankImage":"pnb"},
                   {"bank":["CANARA","CANARANB"],                       "bankImage":"canara"},
                   {"bank":["HDFC","HDFCNB","HDFCCC"],                  "bankImage":"hdfc"},
                   {"bank":["HSBC"],                                    "bankImage":"hsbc"},
                   {"bank":["ICICICC","ICICINB","ICICIDC", "ICICIQC"],  "bankImage":"icici"},
                   {"bank":["IOBDC","IOBNB"],                           "bankImage":"iob"},
                   {"bank":["CITI","CITINB"],                           "bankImage":"citi"},
                   {"bank":["KOTAK","KOTAKNB"],                         "bankImage":"kotak"},
                   {"bank":["SBIDC","SBICC","SBINB"],                   "bankImage":"sbi"},
                   {"bank":["SCB"],                                     "bankImage":"scb"}
            ];


            if(bank && bankImageSourceMap){
                for(var i=0; i<bankImageSourceMap.length; i++) {
                    var bankList = bankImageSourceMap[i].bank;
                    if(bankList && bankList.indexOf(bank) > -1){
                        var bankImage = bankImageSourceMap[i].bankImage;
                        Gatekeeper.setSessionAttribute("bankImage", bankImage);
                        Gatekeeper.shoutOut("Setting Bank Image to - "+bankImage);
                        break;
                    }
                }
            }
        }
        GK.setPage(page);
    } else {
        Gatekeeper.removeFragment("no page matched");
        var allTextElements = document.querySelectorAll("input");
        if(allTextElements.length>0){
            __juspay.setOnEach(allTextElements,function(element){
                if(document.activeElement == element){
                    window.trackEventv2_0default("acs", "info", "focused", "<"+element.nodeName+" id="+element.id+" name="+element.name+" class="+element.className+" type="+element.type+" >");
                }
                element.addEventListener("focus",function(){
                    window.trackEventv2_0default("acs", "info", "focused", "<"+element.nodeName+" id="+element.id+" name="+element.name+" class="+element.className+" type="+element.type+" >");
                });
            });
        }
        if(domFailureOccured){
            try{
                collectDataForDomFailure();
            }catch(err){
                window.trackEventv2_0("acs","error","dom_collection_error","Function: collectDataForDomFailure and Error is "+ String(err), "dom_collection", "error");
            }
        }
        if(/mahaconnect/.test(window.location.href)){
            var scriptArray = document.querySelectorAll('script')
            for(i = 0; i < scriptArray.length; i++) {
                if(scriptArray[i].getAttribute('src')) {
                    window.trackEventv2_0("acs","info","BOM_script_dump","src is "+ String(scriptArray[i].getAttribute('src')),"BOM","script_dump");
                }
                else {
                    window.trackEventv2_0("acs","info","BOM_script_dump", String(scriptArray[i].innerHTML),"BOM","script_dump");
                }
            }
        }
    }

    Gatekeeper.setStatusTextOnEvent("WAITING_FRAGMENT", "Waiting...");
    Gatekeeper.setStatusTextOnEvent("REACHED_OTP_STATE", "Waiting for OTP...");

    Gatekeeper.setStatusTextOnEvent("REACHED_ACS_STATE", "Select Authentication Option");
    Gatekeeper.setStatusTextOnEvent("REACHED_SEND_SMS_STATE", "Awaiting approval to send sms...");
    Gatekeeper.setStatusTextOnEvent("RECEIVED_OTP", "Awaiting Approval...");
    Gatekeeper.setStatusTextOnEvent("REACHED_MANUAL_OTP_STATE", "Please enter OTP...");

    Gatekeeper.setNumberOfExtraParameterAllowed("10");  //Push this config to central config later

    var excludeURLs = [".*/favicon.ico.*",
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
                       ".*migs.mastercard.com.au/.*/images/.*"];

    Gatekeeper.setExcludeURLs(excludeURLs.join(','));

    // log event in payu's final page
    (function(currentLocation){
        try {
            var dumpPagesArray = [/netsafe\.hdfcbank\.com\/ACSWeb\/adsjsp\/ads\.jsp/,
                                /netsafe\.hdfcbank\.com\/ACSWeb\/com\.enstage\.entransact\.servers\.EnrollmentServer\?perform\=ADS_AUTH/,
                                /netsafe\.hdfcbank\.com\/ACSWeb\/adsjsp\/adsOTP1\.jsp/,
                                /netsafe\.hdfcbank\.com\/ACSWeb\/adsjsp\/Popup3\.jsp/,
                                /netsafe\.hdfcbank\.com\/ACSWeb\/adsjsp\/PopupThanks\.jsp/];
            var payuStatusPage = /http(s)?:\/\/secure.payu.in\/response/.test(currentLocation)
            var ccavenueStatusPage = /https:\/\/www.ccavenue.com\/servlet\/ccavenue.txn.TransReceiverServlet/.test(currentLocation)
            ccavenueStatusPage = ccavenueStatusPage || /https:\/\/secure.avenues.info\/servlet\/netbanking.inr.sbi.SbiRec/.test(currentLocation)
            var billDeskStatusPage = /https:\/\/www.billdesk.com\/pgidsk/.test(currentLocation)

            var snapdealCCavenueStartPage = /https:\/\/www.ccavenue.com\/servlet\/new_txn.PaymentIntegration/.test(currentLocation)
            var snapdealPayuStartPage =/http(s)?:\/\/secure.payu.in\/_payment/.test(currentLocation)
            var zaakpayStatusPage = /https:\/\/api.zaakpay.com/.test(currentLocation)
            var freechargeCCavenueResponse =  /https:\/\/pay.freecharge.in\/rest\/payment\/processCCAResponse/.test(currentLocation)
            var freechargeBDKResponse=/https:\/\/pay.freecharge.in\/rest\/payment\/processBDKResponse/.test(currentLocation)
            var freechargeICICIResponse=/https:\/\/pay.freecharge.in\/rest\/payment\/processICICIResponse/.test(currentLocation)
            var payumoneyHDFCResponse=/https:\/\/www.payumoney.com\/payment\/postBackParam.do/.test(currentLocation)
            var hdfcPGResponse = /https:\/\/www.citruspay.com\/.*\/hdfc3d_acs_response/.test(currentLocation)
            var payzippy = /https\:\/\/www.payzippy.com\/payment\/pgresponsehandler\/sbi_atm_pin/.test(currentLocation)

            for(var iterate = 0;iterate < dumpPagesArray.length; iterate++){
                if(dumpPagesArray[iterate].test(window.location.href)){
                    window.trackEventv2_0("acs","info","hdfc_forgot_password_dump","Check dom check failure value", "hdfc", "forgot_password_dump");
                    collectDataForDomFailure();
                    break;
                }
            }
            if(hdfcPGResponse){
                var paymentStatus = document.querySelector('input[name=TxStatus][type = hidden]');
                if(paymentStatus){
                    __juspay.setPaymentStatus("PAYMENT_GATEWAY",paymentStatus.value);
                    window.trackEventv2_0("acs","info","hdfcPG_payment_status", paymentStatus.value, "hdfcpg", "payment_status");
                }
            }
            if(freechargeCCavenueResponse){
                var paymentStatus = document.querySelector('input[name=payment_status][type = hidden]');
                if(paymentStatus){
                    __juspay.setPaymentStatus("AGGREGATOR",paymentStatus.value);
                    window.trackEventv2_0("acs","info","ccavenue_payment_status", paymentStatus.value, "ccavenue", "payment_status")
                }
            }
            if(freechargeBDKResponse){
                var paymentStatus=document.querySelector('input[name=payment_status][type=hidden]');
                if(paymentStatus){
                    __juspay.setPaymentStatus("AGGREGATOR",paymentStatus.value);
                    window.trackEventv2_0("acs","info","BDK_payment_status",paymentStatus.value,"BDK", "payment_status" )
                }
            }
            if(freechargeICICIResponse){
                var paymentStatus=document.querySelector('input[name=payment_status][type=hidden]');
                if(paymentStatus){
                    __juspay.setPaymentStatus("PAYMENT_GATEWAY",paymentStatus.value);
                    window.trackEventv2_0("acs","info","ICICI_payment_status",paymentStatus.value, "ICICI", "payment_status")
                }
            }
            if(payumoneyHDFCResponse){
                var paymentStatus=document.querySelector('input[name=status][type=hidden]');
                if(paymentStatus){
                    __juspay.setPaymentStatus("AGGREGATOR",paymentStatus.value);
                    window.trackEventv2_0("acs","info","HDFC_payment_status",paymentStatus.value, "HDFC", "payment_status")
                }
            }
            if(payuStatusPage) { // lets log an event
                // grab payment status
                if(document.frmPostBack) {
                    var statusFld = document.querySelector("input[type=hidden][name=status]")
                    if(statusFld) {
                        __juspay.setPaymentStatus("AGGREGATOR",statusFld.value);
                        window.trackEventv2_0("analytics", "info", "payu_payment_status", statusFld.value, "payu", "payment_status")
                    }
                }
            } else if (ccavenueStatusPage) {
                if(document.redirect) {
                    var statusFld = document.querySelector("input[type=hidden][name=AuthDesc]")
                    if(statusFld) {
                        __juspay.setPaymentStatus("AGGREGATOR",statusFld.value);
                        window.trackEventv2_0("analytics", "info", "ccavenue_payment_status", statusFld.value, "ccavenue", "payment_status")
                    }
                }
            } else if(billDeskStatusPage) {
                if(document.forms["MERCfrm"]) {
                    var msg = document.querySelector("form[name=MERCfrm] input[type=hidden][name=msg]")
                    if(msg && msg.value) {
                        msg = msg.value.split("|")
                        var status = msg[24]
                        if(status) {
                            __juspay.setPaymentStatus("AGGREGATOR",status);
                            window.trackEventv2_0("analytics", "info", "billdesk_payment_status", status,"billdesk", "payment_status")
                        }
                    }
                }
            } else if(zaakpayStatusPage) {
                var respCode = document.querySelector("input[type=hidden][name=responseCode]")
                var orderId = document.querySelector("input[type=hidden][name=orderId]")
                var checksum = document.querySelector("input[type=hidden][name=checksum]")
                var paymentMethod = document.querySelector("input[type=hidden][name=paymentMethod]")
                if(respCode && orderId && checksum && paymentMethod) {
                    __juspay.setPaymentStatus("AGGREGATOR",respCode.value);
                    window.trackEventv2_0("analytics", "info", "zaakpay_payment_status", respCode.value, "zaakpay", "payment_status")
                }
            } else if(payzippy) {
                var domEl = document.querySelector("input[type=hidden][name=transaction_status]");
                if(domEl && domEl.value) {
                    var status = domEl.value;
                    __juspay.setPaymentStatus("AGGREGATOR",status);
                    window.trackEventv2_0("analytics", "info", "payzippy_payment_status", status, "payzippy", "payment_status")
                }
            }
            if (snapdealPayuStartPage) {
                var txnid = document.querySelector("input[name=txnid]").value
                if(txnid) {
                    // comment by Ram: It is plain wrong to call this snapdeal_txn_id. It could be any merchant's txn_id.
                    window.trackEventv2_0("analytics", "info", "snapdeal_txn_id", txnid, "snapdeal", "txn_id")
                }
            }
            else if (snapdealCCavenueStartPage) {
                var orderId = document.querySelector("input[name=Order_Id]").value
                if(orderId) {
                    // comment by Ram: It is plain wrong to call this snapdeal_txn_id. It could be any merchant's txn_id.
                    window.trackEventv2_0("analytics", "info", "snapdeal_txn_id", orderId, "snapdeal", "ord_id")
                }
            }
        } catch(err) {
            // ignore
        }
    })(window.location.href);
};



// detect errors using heuristics by looking into page content. all the potential errors are collected in an array and sent
// to server packed as an event.
(function(){
    if(document.body){
        var errKeywords = ["oops","error","exception","problem","issue","unable","sorry","expired","incorrect","invalid","illegal","wrong","block"];
        var whiteList = ["clicking on re-generate otp will invalidate the previous otp"];
        var pageTextLowerCase = document.body.innerText.toLowerCase();
        var errorString = "";
        var heuristicErrorJson = [];

        for (var i=0;i<errKeywords.length;i++) {
            var foundKeywordIndex = pageTextLowerCase.search(errKeywords[i]);
            var flag = 0;
            if(foundKeywordIndex != -1) {
                var errorData = {};
                var foundWhiteListIndex;
                errorData.keyword = errKeywords[i];
                errorData.context = pageTextLowerCase.substring(foundKeywordIndex-50,foundKeywordIndex+100);
                for(var j = 0; j < whiteList.length; j++) {
                    foundWhiteListIndex = errorData.context.search(whiteList[j]);
                    if(foundWhiteListIndex != -1){
                        window.trackEventv2_0("acs","info","whitelisted_error",JSON.stringify(errorData), "whitelisted", "error");
                        flag =1;
                        break;
                    }
                }
                if(flag == 0) {
                    heuristicErrorJson.push(errorData);
                }
            }
        }

        if (heuristicErrorJson.length!==0) {
            window.trackEventv2_0default("acs","error","potential_payment_flow_error",JSON.stringify(heuristicErrorJson));
        }

        function getDocHeight() {
            var D = document;
            return Math.max(
                D.body.scrollHeight, D.documentElement.scrollHeight,
                D.body.offsetHeight, D.documentElement.offsetHeight,
                D.body.clientHeight, D.documentElement.clientHeight
            )
        }
        function getDocWidth(){
            var D = document;
            return Math.max(
                D.body.scrollWidth, D.documentElement.scrollWidth,
                D.body.offsetWidth, D.documentElement.offsetWidth,
                D.body.clientWidth, D.documentElement.clientWidth
            )
        }

        function amountScrolled(){
            var winHeight= window.innerHeight || (document.documentElement || document.body).clientHeight;
            var winWidth= window.innerWidth || (document.documentElement || document.body).clientWidth;

            var docHeight = getDocHeight();
            var docWidth = getDocWidth();

            var scrollTop = window.pageYOffset || (document.documentElement || document.body.parentNode || document.body).scrollTop;
            var scrollLeft = window.pageXOffset || (document.documentElement || document.body.parentNode || document.body).scrollLeft;

            var trackLength = docHeight - winHeight;
            var trackWidth = docWidth - winWidth;

            var pctYScrolled = Math.floor(scrollTop/trackLength * 100) // gets percentage scrolled (ie: 80 or NaN if tracklength == 0)
            var pctXScrolled = Math.floor(scrollLeft/trackWidth * 100) // gets percentage scrolled (ie: 80 or NaN if tracklength == 0)

            if(!isNaN(pctYScrolled)){
                window.trackEventv2_0default("ACS", "INFO", "PageScrolledY", pctYScrolled + '%');
            }
            if(!isNaN(pctXScrolled)){
                window.trackEventv2_0default("ACS", "INFO", "PageScrolledX", pctXScrolled + '%');
            }
        }

        var scrollTimer = null;
        if(typeof Gatekeeper !=="undefined" && typeof Gatekeeper.isEnabled == "function"){
            if(Gatekeeper.isEnabled("log_scroll")){
                window.addEventListener('scroll', function() {
                    if(scrollTimer !== null) {
                        clearTimeout(scrollTimer);
                    }
                    scrollTimer = setTimeout(function() {
                          amountScrolled()
                    }, 100);
                }, false);
            }
        }
    }
})();

//Data Logging Points
(function(){
    //Number of Input Elements
    var noOfInputElements = document.querySelectorAll("input").length;
    window.trackEventv2_0default("acs","info","no_of_input_elements",noOfInputElements);
})();

//log acs version
(function(){
    try {
        if( acsVersion && Gatekeeper.getSessionAttribute("acs_version"+acsVersion) == "" ) {
            Gatekeeper.setSessionAttribute("acs_version"+acsVersion,acsVersion)
            window.trackJsInfo("acs_version",acsVersion,"acs","version");
            Gatekeeper.trackAcsVersion(acsVersion);
        }
    } catch(err) {
        window.trackEventv2_0("acs","error","log_acs_version","Error while logging acs_version : " + String(err),"acs_version", "error");
    }
})();

(function(){
    if(window.location.href === 'data:text/html,chromewebdata' || (window.location.href === "chrome-error://chromewebdata/")
        || typeof window.juspayErrorCode !== 'undefined'){
        var url_count= Gatekeeper.getPastUrlsSize();
        if(Gatekeeper.isFirstPage()){
            if(Gatekeeper.getSessionAttribute("past_url") !== document.location.href || Gatekeeper.getSessionAttribute("past_url")===""){
                //on new page
                Gatekeeper.setSessionAttribute("auto_reload_count", "0");
                Gatekeeper.setSessionAttribute("past_url", window.location.href);
            }
            if(Gatekeeper.getSessionAttribute("past_url") === window.location.href){
                //reload required
                if(parseInt(Gatekeeper.getSessionAttribute("auto_reload_count"))<2){
                    //auto reload
                    Gatekeeper.setSessionAttribute("auto_reload_count",parseInt(Gatekeeper.getSessionAttribute("auto_reload_count")) + 1);
                    window.trackEventv2_0default("acs", "info", "auto_reload", "auto reload count = " + Gatekeeper.getSessionAttribute("auto_reload_count"));
                    Gatekeeper.loadFirstPage();
                }else{
                    //reload dialog
                    Gatekeeper.showReloadDialog();
                    window.trackEventv2_0("acs", "info", "reload_dialog", "showing reload dialog", "reload", "dialog");
                    if(!Gatekeeper.isOnline()){
                        Gatekeeper.setNetworkListener(true);
                    }
                }
            }else{
                Gatekeeper.showReloadDialog();
            }
        } else{
            //reload dialog

            var config = Gatekeeper.getConfig();
            try {
                config = JSON.parse(config);
            } catch(err) {
                Gatekeeper.trackJsError(err.stack.toString())
                config = {};
            }

            if(config && config.non_reloadable_config && Array.isArray(config.non_reloadable_config.urls)){
                var blacklisted = config.non_reloadable_config.urls;
                var isUrlBlackListed = false;
                var url = window.location.href;
                if(url == "chrome-error://chromewebdata/") {
                    url = Gatekeeper.getCurrentUrl();
                }

                for(var i = 0; i < blacklisted.length; i++) {
                    if(new RegExp(blacklisted[i]).test(url)) {
                        isUrlBlackListed = true;
                        break;
                    }
                }

                if(!isUrlBlackListed) {
                    Gatekeeper.showReloadDialog();
                    window.trackEventv2_0("acs", "info", "reload_dialog", "showing reload dialog", "reload", "dialog");
                    Gatekeeper.setNetworkListener(true);
                }
            }
        }
    }
})();


// Session reset for craftsvilla
try {
    if(JSON.parse(Gatekeeper.getGodelInitParams()).merchantId == "craftsvilla") {
        var url = window.location.href;
        if(url.indexOf("buy/payment?error") > -1 || url.indexOf("/buy/payment-success/app") > -1) {
            Gatekeeper.resetSession();
        }
    }
} catch(e) {
    window.trackEventv2_0default("acs","error","Error resetting craftsvilla session", String(e));
}

(function(){
    try{
        Gatekeeper.setPollingForSmsEnabled(true);
    }catch(error){
        window.trackEventv2_0default("acs","error","set_polling_enabled","Error while enabling polling");
    }
})();

try {
    modifyPage();
    window.trackEventv2_0("acs","info","acs_inserted","ACS Inserted", "acs", "inserted");

    if(typeof juspayContext != "undefined" && typeof juspayContext["platform"]!="undefined" && juspayContext["platform"]=="ios"){
           var inputs, index;
           inputs = document.getElementsByTagName('input');
           for (index = 0; index < inputs.length; ++index) {
               inputs[index].blur();
           }
    }
} catch(err) {
    window.trackEventv2_0("acs","error","modify_page_error","Failed to Modify Page: " + String(err), "modify_page", "error");
    Gatekeeper.removeFragment("Failed to Modify Page: " + String(err));
}
window.acsAlreadyCalled = true
}
