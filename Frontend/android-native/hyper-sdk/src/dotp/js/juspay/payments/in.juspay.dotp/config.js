window.config_version = "2.0.21";

console.log("Hello, DOTP!");


if (window.JOS && typeof window.JOS.registerVersion == "function"){
  window.JOS.registerVersion(window.JOS.self)("config")(window.config_version)();
}

//----------------------------- Global Variables -------------------------------//

var wrap_content = -2;
var match_parent = -1;

//----------------------------- Javascript Polyfill -------------------------------//

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

//----------------------------- Javascript Polyfill -------------------------------//


getMerchantName = function(){
  var cid = window.__payload.payload.clientId;
  if (cid == "gaana_android"){
     return " to Gaana";
  }
  if (cid.includes("ixigo")){
     return " to Ixigo";
  }
  else return "";
}


var UIConfig = { submitButtonColor : "#0099FF"
   , isTextInAllCaps : false
   , isFullScreenActivity : false
   , resendColor : "#0099FF"
   , toolbarBackground : "#FFFFFFFF"
   , background : "#FFFFFFFF" //submitButtonTextColor
   , toolbarTextColor : "#333333"
   , toolbarBackImage :  "ic_back_arrow"
   , primaryFont : "OpenSans"
   , toolbarTextSize : 18
   , toolbarTextFontFace : "Regular"
   , cornerRadius : 5.0
   , otpEditTextBackground : "#FFF5F6F7"
   , headerText : "Pay"
   , merchantName : getMerchantName()
   , toolbarPrimaryTextMargin : 8
   , dialogPrimaryTextFontFace : "Regular"
   , fontWeight : "-Regular"
   , showAmountInToolbar : false
}

var UIConfigBms = { submitButtonColor : "#407FF4"// "#F34F61"
   , isTextInAllCaps : true
   , isFullScreenActivity : true
   , resendColor : "#FF0192FA"
   , toolbarBackground : "#FF424556"
   , background : "#FFFFFFFF"
   , toolbarTextColor : "#FFFFFFFF"
   , toolbarBackImage : "ic_back_arrow_white"
   , primaryFont : "Roboto"
   , toolbarTextSize : 18
   , toolbarTextFontFace : "Regular"
   , cornerRadius : 4.0
   , otpEditTextBackground : "#FFF5F6F7"
   , headerText : "Pay to "
   , merchantName : "BookMyShow"
}

var UIConfigBmsNew = { submitButtonColor : "#F84464"// "#F34F61"
   , isTextInAllCaps : false
   , isFullScreenActivity : true
   , resendColor : "#DC3558"
   , toolbarBackground : "#FF424556"
   , background : "#FFFFFFFF"
   , toolbarTextColor : "#FFFFFFFF"
   , toolbarBackImage : "ic_back_arrow_white"
   , primaryFont : "Roboto"
   , toolbarTextSize : 18
   , toolbarTextFontFace : "Regular"
   , cornerRadius : 4.0
   , otpEditTextBackground : "#FCFCFC"
   , headerText : "Pay to "
   , merchantName : "BookMyShow"
   , submitButtonInactiveColor : "#C0C0C0"
   , submitButtonAnimationColor : "#DC3558"
   , submitButtonCornerRadius : 8.0
   , otpEditTextColor : "#121212"
   , resendTextFontFace : "Regular"
   , otpEditTextSize : 26
   , otpInfoMargin : 8
   , editTextFocusColor : "#0099FF"
   , fontWeight : "-Bold"
   , showAmountInToolbar : true
   , setStroke : true
}

var UIConfigSwiggy = { submitButtonColor : "#5BB24F"
   , isTextInAllCaps : true
   , isFullScreenActivity : false
   , resendColor : "#FFFC821D"
   , toolbarBackground : "#FFFFFFFF"
   , background : "#FFFFFFFF"
   , toolbarTextColor : "#FF333333"
   , toolbarBackImage :  "ic_back_arrow"
   , primaryFont : "Gilroy"
   , toolbarTextSize : 18
   , toolbarTextFontFace : "Medium"
   , cornerRadius : 4.0
   , otpEditTextBackground : "#FFF5F6F7"
   , headerText : "Pay to "
   , merchantName : "SWIGGY"
}

var UIConfigIdea = { submitButtonColor : "#0071BC"
    , isTextInAllCaps : false
    , isFullScreenActivity : true
    , resendColor : "#0071BC"
    , toolbarBackground : "toolbar_background"
    , background : "#ffffff"
    , toolbarTextColor : "#333333"
    , toolbarBackImage : "toolbar_back_arrow"
    , primaryFont : "Arial"
    , toolbarTextSize : 20
    , toolbarTextFontFace : "Regular"
    , cornerRadius : 5.0
    , otpEditTextBackground : "#F5F6F7"
    , headerText : "Pay to "
    , merchantName : "Idea"
    , toolbarPrimaryTextMargin : 12
    , toolbarLetterSpacing : 0.0
    , toolbarButtonLayoutHeight : wrap_content
    , toolbarButtonLayoutWidth : wrap_content
    , toolbarButtonMargin : 16
    , cancelDialogNoButton : "#0071BC"
    , cancelDialogYesButton : "#BEBEBE"
    , dialogPrimaryTextColor : "#99000000"
    , dialogHeaderTextColor : "#DE000000"
    , dialogBoxCornerRadius : 8.0
    , invertDialogCTA : true
    , dialogPrimaryTextFontFace : "Regular"
    , popUpImage : "https://assets.juspay.in/juspay/assets/images/error.png"
    , fontWeight : "-SemiBold"
    }

var UIConfigVodaIdea = { submitButtonColor : "#EB0000"
    , isTextInAllCaps : false
    , isFullScreenActivity : true
    , resendColor : "#2F3043"
    , toolbarBackground : "#ffffff"
    , background : "#ffffff"
    , toolbarTextColor : "#2F3043"
    , toolbarBackImage : "toolbar_back_arrow"
    , primaryFont : "Vi"
    , toolbarTextSize : 20
    , toolbarTextFontFace : "Regular"
    , cornerRadius : 5.0
    , otpEditTextBackground : "#FFFFFF"
    , headerText : "Pay to "
    , merchantName : "VI"
    , toolbarPrimaryTextMargin : 12
    , toolbarLetterSpacing : 0.0
    , toolbarButtonLayoutHeight : 64
    , toolbarButtonLayoutWidth : wrap_content
    , toolbarButtonMargin : 16
    , cancelDialogNoButton : "#2F3043"
    , cancelDialogYesButton : "#BEBEBE"
    , dialogPrimaryTextColor : "#99000000"
    , dialogHeaderTextColor : "#DE000000"
    , dialogBoxCornerRadius : 8.0
    , invertDialogCTA : true
    , dialogPrimaryTextFontFace : "Regular"
    , popUpImage : "https://assets.juspay.in/juspay/assets/images/error.png"
    , editTextFocusColor : "#2F3043"
    , fontWeight : "-SemiBold"
    , resendColor : "#EB0000"
    , toolbarHeight : 64
    , setStroke : true
    }

  var UIConfigVodafone =
    { submitButtonColor : "#EE2737"
    , isTextInAllCaps : false
    , isFullScreenActivity : true
    , resendColor : "#2F3043"
    , toolbarBackground : "#ffffff"
    , background : "#ffffff"
    , toolbarTextColor : "#2F3043"
    , toolbarBackImage : "toolbar_back_arrow"
    , primaryFont : "Vi"
    , toolbarTextSize : 20
    , toolbarTextFontFace : "Regular"
    , cornerRadius : 5.0
    , otpEditTextBackground : "#F5F6F7"
    , headerText : "Pay to "
    , merchantName : "Vodafone Idea Ltd"
    , toolbarPrimaryTextMargin : 12
    , toolbarLetterSpacing : 0.0
    , toolbarButtonLayoutHeight : wrap_content
    , toolbarButtonLayoutWidth : wrap_content
    , toolbarButtonMargin : 16
    , cancelDialogNoButton : "#2F3043"
    , cancelDialogYesButton : "#BEBEBE"
    , dialogPrimaryTextColor : "#99000000"
    , dialogHeaderTextColor : "#DE000000"
    , dialogBoxCornerRadius : 8.0
    , invertDialogCTA : true
    , dialogPrimaryTextFontFace : "Regular"
    , popUpImage : "https://assets.juspay.in/juspay/assets/images/error.png"
    }


var UIConfigGrofers = { submitButtonColor : "#D76935"
     , isTextInAllCaps : true
     , isFullScreenActivity : false
     , cancelDialogNoButton : "#e96125"
     , cancelDialogYesButton : "#e96125"
     , resendColor : "#D76935"
     , toolbarBackground : "#151C1E"
     , background : "#FFFFFFFF"
     , toolbarTextColor : "#FFFFFFFF"
     , toolbarBackImage : "ic_back_arrow_white"
     , primaryFont : "Celias"
     , toolbarTextSize : 18
     , toolbarTextFontFace : "Regular"
     , cornerRadius : 4.0
     , otpEditTextBackground : "#F5F6F7"
     , headerText : "Pay to "
     , merchantName : "Grofers"
     }

 var UIConfigBigbasket = { submitButtonColor : "#e56167"
     , isTextInAllCaps : true
     , isFullScreenActivity : false
     , resendColor : "#e56167"
     , toolbarBackground : "#689f38"
     , background : "#FFFFFFFF"
     , toolbarTextColor : "#FFFFFFFF"
     , toolbarBackImage : "ic_bb_back_white"
     , primaryFont : "ProximaNova"
     , toolbarTextSize : 20
     , toolbarTextFontFace : "Regular"
     , cornerRadius : 3.0
     , otpEditTextBackground : "#F5F6F7"
     , headerText : "Pay to "
     , merchantName : "BigBasket"
     }

var UIConfigOnecard = { submitButtonColor : "#0093f7"
     , isTextInAllCaps : false
     , isFullScreenActivity : false
     , resendColor : "#FFFC821D"
     , toolbarBackground : "#0c161b"
     , background : "#FFFFFFFF"
     , toolbarTextColor : "#FFFFFF"
     , toolbarBackImage :  "ic_back_arrow_white"
     , resendColor : "#0093f7"
     , toolbarPrimaryTextMargin : 16
     , toolbarTextAlignment : "center"
     , toolbarLetterSpacing : 0.0
     , toolbarButtonLayoutHeight : 90
     , toolbarButtonLayoutWidth : wrap_content
     , toolbarButtonMargin : 16
     , primaryFont : "Manrope"
     , toolbarTextSize : 20
     , toolbarTextFontFace : "Regular"
     , cornerRadius : 4.0
     , otpEditTextBackground : "#FFFFFFFF"
     , headerText : "Pay to "
     , merchantName : "OneCard"
     , setStroke : true
     , cancelDialogNoButton : "#0093f7"
     , cancelDialogYesButton : "#0093f7"
     , showAmountInToolbar : false
     , toolbarHeight : 90
     , editTextFocusColor : "#008ECC"
     , fontWeight : "-Bold"
   }

var UIConfigCroma =
  { submitButtonColor : "#61A6A1"
  , isTextInAllCaps : false
  , isFullScreenActivity : true
  , resendColor : "#61A6A1"
  , toolbarBackground : "#61A6A1"
  , background : "#EBECED"
  , toolbarTextColor : "#FFFFFF"
  , toolbarBackImage : "toolbar_back_arrow"
  , primaryFont : "Nunito"
  , toolbarTextSize : 20
  , toolbarTextFontFace : "Regular"
  , cornerRadius : 4.0
  , otpEditTextBackground : "#F5F6F7"
  , headerText : "Pay to "
  , merchantName : "Croma"
  , toolbarPrimaryTextMargin : 12
  , toolbarLetterSpacing : 0.0
  , toolbarButtonLayoutHeight : wrap_content
  , toolbarButtonLayoutWidth : wrap_content
  , toolbarButtonMargin : 16
  , cancelDialogNoButton : "#61A6A1"
  , cancelDialogYesButton : "#BEBEBE"
  , dialogPrimaryTextColor : "#99000000"
  , dialogHeaderTextColor : "#DE000000"
  , dialogBoxCornerRadius : 10.0
  , invertDialogCTA : true
  , dialogPrimaryTextFontFace : "Regular"
  , popUpImage : "https://assets.juspay.in/juspay/assets/images/error.png"
  }


var UIConfigTruefan =
  { submitButtonColor : "#FF327F"
  , isTextInAllCaps : false
  , isFullScreenActivity : true
  , resendColor : "#FF327F"
  , toolbarBackground : "#1F1D6F"
  , background : "#EBECED"
  , toolbarTextColor : "#FFFFFF"
  , toolbarBackImage : "toolbar_back_arrow"
  , toolbarTextAlignment : "center"
  , primaryFont : "Roboto"
  , toolbarTextSize : 18
  , toolbarTextFontFace : "Bold"
  , cornerRadius : 12.0
  , otpEditTextBackground : "#F5F6F7"
  , headerText : "Pay to "
  , merchantName : "Truefan"
  , toolbarPrimaryTextMargin : 12
  , toolbarLetterSpacing : 0.0
  , toolbarButtonLayoutHeight : wrap_content
  , toolbarButtonLayoutWidth : wrap_content
  , toolbarButtonMargin : 16
  , cancelDialogNoButton : "#FF327F"
  , cancelDialogYesButton : "#BEBEBE"
  , dialogPrimaryTextColor : "#99000000"
  , dialogHeaderTextColor : "#DE000000"
  , dialogBoxCornerRadius : 12.0
  , invertDialogCTA : true
  , dialogPrimaryTextFontFace : "Regular"
  , popUpImage : "https://assets.juspay.in/juspay/assets/images/error.png"
  }

var UIConfigDineout =
  { submitButtonColor : "#FF7B73"
  , isTextInAllCaps : false
  , isFullScreenActivity : true
  , resendColor : "#2F3043"
  , toolbarBackground : "#ffffff"
  , background : "#ffffff"
  , toolbarTextColor : "#333333"
  , toolbarBackImage : "toolbar_back_arrow"
  , primaryFont : "Metropolis"
  , toolbarTextSize : 18
  , toolbarTextFontFace : "SemiBold"
  , cornerRadius : 5.0
  , otpEditTextBackground : "#F5F6F7"
  , headerText : "Pay to "
  , merchantName : "Dineout"
  , toolbarPrimaryTextMargin : 12
  , toolbarLetterSpacing : 0.0
  , toolbarButtonLayoutHeight : wrap_content
  , toolbarButtonLayoutWidth : wrap_content
  , toolbarButtonMargin : 16
  , cancelDialogNoButton : "#2F3043"
  , cancelDialogYesButton : "#BEBEBE"
  , dialogPrimaryTextColor : "#99000000"
  , dialogHeaderTextColor : "#DE000000"
  , dialogBoxCornerRadius : 8.0
  , invertDialogCTA : true
  , dialogPrimaryTextFontFace : "Regular"
  , popUpImage : "https://assets.juspay.in/juspay/assets/images/error.png"
  }

var UIConfigOkCredit = 
  { submitButtonColor : "#1C873B" 
  , isTextInAllCaps : false
  , isFullScreenActivity : false
  , resendColor : "#1C873B"   
  , toolbarBackground : "#ffffff"  
  , background : "#ffffff"
  , toolbarTextColor : "#212121"  
  , toolbarBackImage : "toolbar_back_arrow" 
  , primaryFont : "NotoSans" 
  , toolbarTextSize : 18
  , toolbarTextFontFace : "SemiBold"
  , cornerRadius : 0.0
  , otpEditTextBackground : "#FFFFFF"
  , headerText : "Pay to "
  , merchantName : "OkCredit"
  , toolbarPrimaryTextMargin : 12
  , toolbarLetterSpacing : 0.0
  , toolbarButtonLayoutHeight : wrap_content
  , toolbarButtonLayoutWidth : wrap_content
  , toolbarButtonMargin : 16
  , cancelDialogNoButton : "#1C873B"
  , cancelDialogYesButton : "#BEBEBE"
  , dialogPrimaryTextColor : "#99000000"
  , editTextFocusColor : "#1C873B"
  , dialogHeaderTextColor : "#DE000000"
  , dialogBoxCornerRadius : 8.0
  , invertDialogCTA : true
  , setStroke : false
  , dialogPrimaryTextFontFace : "Regular"
  , popUpImage : "https://assets.juspay.in/juspay/assets/images/error.png"
  }

var UIConfigTatacliq = { submitButtonColor : "#DA1C5C"
  , isTextInAllCaps : false
  , isFullScreenActivity : true
  , resendColor : "#DA1C5C"
  , toolbarBackground : "#1F2021"
  , background : "#FFFFFFFF"
  , toolbarTextColor : "#FFFFFFFF"
  , toolbarBackImage : "ic_back_arrow_white"
  , primaryFont : "Overpass"
  , toolbarTextSize : 18
  , toolbarTextFontFace : "Regular"
  , cornerRadius : 4.0
  , otpEditTextBackground : "#FFFFFF"
  , headerText : "Pay to "
  , merchantName : "TataCliq"
  , submitButtonCornerRadius : 25.0
  , fontWeight : "-Bold"
  , showAmountInToolbar : false
  , setStroke : true
  , editTextFocusColor : "#000000"
  , submitButtonAnimationColor : "#B5144A"
  , submitButtonText : "Submit & Pay  ₹ "
  , submitButtonWidth : 240
  , submittingButtonText : "Processing Payment"
  }

  var zee5UIConfig = {
    isTextInAllCaps : true
    , isFullScreenActivity : false
    , resendColor : "#FFFC821D"
    , toolbarBackground : "#2a0a2b"
    , background : "#FFFFFFFF" //submitButtonTextColor
    , toolbarTextColor : "#FFFFFF"
    , toolbarBackImage :  "toolbar_back_arrow"
    , primaryFont : "NotoSans"
    , toolbarTextSize : 18
    , toolbarTextFontFace : "Regular"
    , cornerRadius : 4.0
    , otpEditTextBackground : "#FFF5F6F7"
    , headerText : "Pay to "
    , merchantName : "Zee5"
    , fontWeight : "-Regular"
    , submitButtonColor : "#2a0a2b"
 }
 var UIConfigAcko = {
  submitButtonColor: "#37C87E"
  , isTextInAllCaps: false
  , isFullScreenActivity: false
  , resendColor: "#0099FF"
  , toolbarBackground: "#FFFFFFFF"
  , background: "#FFFFFFFF"
  , toolbarTextColor: "#121212"
  , toolbarBackImage: "toolbar_back_arrow"
  , primaryFont: "Inter"
  , toolbarTextSize: 16
  , toolbarTextFontFace: "SemiBold"
  , toolbarLetterSpacing: 0.0
  , cornerRadius: 4.0
  , otpEditTextBackground: "#FFF5F6F7"
  , headerText: "Pay to "
  , setStroke : true
  , fontWeight : "-Bold"
  , merchantName: "Acko"
  , showAmountInToolbar: false
  , otpVerificationTextColor: "#000000"
  , hintTextColor: "#929292"
  , editTextFocusColor: "#4F34D2"
  , footerBackground : "#F9F9F9"
}
  var UIConfigTimesPrimeLight = {
    submitButtonColor: "#4A17C5"
    , isTextInAllCaps: true
    , isFullScreenActivity: false
    , resendColor: "#4A17C5"
    , toolbarBackground: "#FFFFFFFF"
    , background: "#FFFFFFFF"
    , toolbarTextColor: "#333333"
    , toolbarBackImage: "toolbar_back_arrow"
    , primaryFont: "Mulish"
    , toolbarTextSize: 16
    , toolbarTextFontFace: "Bold"
    , toolbarLetterSpacing: 0.0
    , cornerRadius: 4.0
    , otpEditTextBackground: "#FFF5F6F7"
    , headerText: "Pay to "
    , setStroke : true
    , fontWeight : "-Bold"
    , merchantName: "TimesPrime"
    , showAmountInToolbar: false
  }

  var UIConfigTimesPrimeDark = {
      resendColor : "#C68AFF"
    , isTextInAllCaps: true
    , isFullScreenActivity: false
    , primaryFont: "Mulish"
    , toolbarTextSize: 16
    , toolbarTextFontFace: "Bold"
    , toolbarLetterSpacing: 0.0
    , cornerRadius: 4.0
    , headerText: "Pay to "
    , merchantName: "TimesPrime"
    , showAmountInToolbar: false
    , toolbarTextColor : "#EFEFEF"
    , toolbarBackground : "#0F1011"
    , background : "#0F1011"
    , toolbarBackImage : "tl_dark"
    , otpEditTextBackground : "#181818"
    , submitButtonColor : "#8E20E2"
    , brandImage : "juspay_dark"
    , setStroke : true
    , fontWeight : "-Bold"
    , editTextFocusColor : "#CB86FF"
    , otpVerificationTextColor : "#EFEFEF"
    , submitButtonTextColor : "#EFEFEF"
    , footerBackground : "#0F1011"
    , editTextFontColor : "#EFEFEF"
    , hintTextColor : "#EFEFEF"
    , dividerBackgroundColor : "#0F1011"
    , shouldAddBankImageBackground : true
    , bankIconBackgroundColor : "#FFFFFF"
  }

  var UIConfigTimesPrime = function(){
    if(isDark()){
        return UIConfigTimesPrimeDark;
    } else{
        return UIConfigTimesPrimeLight;
    }
  }

  function isDark () {
    var isDark = false;
    try{
      isDark = window.theme == "dark" ? true : false;
    } catch(err){
      isDark = false;
    }
    return isDark;
  }


 var getConfig = function() {
     var id = window.__payload.payload.clientId;
     var godel_version = JBridge.getResourceByName("godel_version");
     console.log("Config id:", id);
     if(id.includes("swiggy")) {
         return UIConfigSwiggy;
     } else if(id.includes("bms")) {
         if (godel_version == "2.0.0")
          return UIConfigBms;
         else 
          return UIConfigBmsNew;
     } else if(id.startsWith("idea")) {
          return UIConfigIdea;
     } else if(id.includes("vodaidea") || id == "vodafone_ios") {
          return UIConfigVodaIdea;
     } else if(id.includes("onecard")) {
          return UIConfigOnecard;
     } else if(id.includes("grofers")) {
          return UIConfigGrofers;
     } else if(id.includes("bigbasket") || id.startsWith("bb")) {
          return UIConfigBigbasket;
     } else if(id.includes("vodafone")) {
          return UIConfigVodafone;
     } else if(id.includes("croma")) {
          return UIConfigCroma;
     } else if(id.includes("truefan")) {
          return UIConfigTruefan;
     } else if(id.includes("dineout")) {
          return UIConfigDineout;
    } else if(id.includes("okcredit")) {
          return UIConfigOkCredit;
    } else if(id.includes("TUL_TMP")) {
          return UIConfigTatacliq; 
    } else if(id.includes("zee5")) {
          return zee5UIConfig;
    } else if(id.toLowerCase().includes("timesprime")) {
        return UIConfigTimesPrime();
    } else if(id.toLowerCase().includes("acko")){
      return UIConfigAcko;
    } else {
          return UIConfig ;
    }
 }

var getIsAutoSubmitAllowed = function() {
    var id = window.__payload.payload.clientId;
    console.log("Config id:", id);
    if(id.includes("swiggy")) {
        return false;
    } else if(id.includes("bms")) {
        return false;
    } else if(id.includes("idea")) {
        return false;
    } else {
        return false ;
    }
}

var shouldEnableAutoSubmit = function() {
  try {
    var version = parseInt(window.dotp_version.split("_")[1])
    if( version >= 23) {
      var x = JBridge.getFromSharedPrefs("stagger-auto-submit-dotp");
      var y = {};
      y[window.dotp_version] = Math.round(Math.random() * 100 % 100);
      if (typeof x == "string" && x != "__failed") {
        try {
          y = JSON.parse(x)
        } catch (e) {
          //ignored
        }
      }
      JBridge.setInSharedPrefs("stagger-auto-submit-dotp", JSON.stringify(y));
      return y[window.dotp_version] < 20;
    }
  } catch (e) {
    // ignored
  }
  return false;
}


var FlowConfig = { allowSmsReader : true
   , allowSmsConsentReader : true
   , allowClipboardReader : false
   , showSmsReaderCounter : 3
   , showSmsConsentReaderCounter : 3
   , showClipboardReaderCounter : 3
   , allowAutoSubmit : shouldEnableAutoSubmit()
   , delayAutoSubmitInSeconds : 3
   , globalAutoReadTimerInSeconds : 15
   , delayResendInSeconds : 3
 }

 var FlowConfigIdea = { allowSmsReader : true
  , allowSmsConsentReader : true
  , allowClipboardReader : false
  , showSmsReaderCounter : 3
  , showSmsConsentReaderCounter : 3
  , showClipboardReaderCounter : 3
  , allowAutoSubmit : shouldEnableAutoSubmit()
  , delayAutoSubmitInSeconds : 3
  , globalAutoReadTimerInSeconds : 30
  , delayResendInSeconds : 3
}

var FlowConfigVodafone =
  { allowSmsReader : true
  , allowSmsConsentReader : true
  , allowClipboardReader : false
  , showSmsReaderCounter : 3
  , showSmsConsentReaderCounter : 3
  , showClipboardReaderCounter : 3
  , allowAutoSubmit : shouldEnableAutoSubmit()
  , delayAutoSubmitInSeconds : 3
  , globalAutoReadTimerInSeconds : 30
  , delayResendInSeconds : 3
  }

var getFlowConfig = function () {
    var id = window.__payload.payload.clientId;
    if (id.includes("idea")) {
        return FlowConfigIdea;
    } else if(id.includes("vodafone")){
        return FlowConfigVodafone;
    } else {
        return FlowConfig;
    }
}

window.flowconfig = getFlowConfig();
window.uiconfig = getConfig();

var payload = {
    fileName: 'payments/in.juspay.dotp/v1-config.jsa'
}
JBridge.runInJuspayBrowser("onBundleLoaded", JSON.stringify(payload), null);
