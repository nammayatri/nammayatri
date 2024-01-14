import * as Language_Types from "../Language.Types/index.js";
var getHI = function (stringKey) {
    if (stringKey instanceof Language_Types.INACCURATE_DATE_AND_TIME) {
        return "\u0917\u0932\u0924 \u0926\u093f\u0928\u093e\u0902\u0915 \u0914\u0930 \u0938\u092e\u092f!";
    };
    if (stringKey instanceof Language_Types.ADJUST_YOUR_DEVICE_DATE_AND_TIME_AND_TRY_AGAIN) {
        return "\u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0928\u0947 \u0938\u0947\u091f\u093f\u0902\u0917 \u0910\u092a \u0938\u0947 \u0926\u093f\u0928\u093e\u0902\u0915 \u0914\u0930 \u0938\u092e\u092f \u0915\u094b \u0938\u094d\u0935\u091a\u093e\u0932\u093f\u0924 (\u0928\u0947\u091f\u0935\u0930\u094d\u0915-\u092a\u094d\u0930\u0926\u0924\u094d\u0924) \u092a\u0930 \u0938\u0947\u091f \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.THE_CURRENT_DATE_AND_TIME_IS) {
        return "\u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u0926\u093f\u0928\u093e\u0902\u0915 \u0914\u0930 \u0938\u092e\u092f \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.GO_TO_SETTING) {
        return "\u0938\u0947\u091f\u093f\u0902\u0917\u094d\u0938 \u092e\u0947\u0902 \u091c\u093e\u0913";
    };
    if (stringKey instanceof Language_Types.LETS_GET_STARTED) {
        return "\u091a\u0932\u093f\u090f \u0936\u0941\u0930\u0942 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION) {
        return "\u0906\u092a\u0915\u093e \u0906\u0935\u0947\u0926\u0928 \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u091c\u092e\u093e \u0915\u0930 \u0926\u093f\u092f\u093e \u0917\u092f\u093e \u0939\u0948 \u0914\u0930 \u0938\u0924\u094d\u092f\u093e\u092a\u0928 \u0915\u0947 \u0905\u0927\u0940\u0928 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.VIEW_STATUS) {
        return "\u0938\u094d\u0925\u093f\u0924\u093f \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GO_HOME) {
        return "\u0917\u094b \u0939\u094b\u092e";
    };
    if (stringKey instanceof Language_Types.SELECT_LANGUAGE) {
        return "\u092d\u093e\u0937\u093e \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.WHICH_LANGUAGE_DO_YOU_PREFER) {
        return "\u0906\u092a \u0915\u094c\u0928 \u0938\u0940 \u092d\u093e\u0937\u093e \u092a\u0938\u0902\u0926 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.T_C) {
        return "\u0909\u092a\u092f\u094b\u0917 \u0915\u0940 \u0936\u0930\u094d\u0924\u0947 \u092a\u094d\u0930\u093e\u0907\u0935\u0947\u0938\u093f \u092a\u0949\u0932\u093f\u0938\u0940";
    };
    if (stringKey instanceof Language_Types.ENTER_MOBILE_NUMBER) {
        return "\u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.BY_CLICKING_CONTINUE_YOU_WILL_BE_AGREEING_TO_OUR) {
        return "\u091c\u093e\u0930\u0940 \u0930\u0916\u0947\u0902 \u092a\u0930 \u0915\u094d\u0932\u093f\u0915 \u0915\u0930\u0915\u0947, \u0906\u092a \u0939\u092e\u093e\u0930\u0940 \u092c\u093e\u0924 \u0938\u0947 \u0938\u0939\u092e\u0924 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_OTP) {
        return "\u0913\u091f\u0940\u092a\u0940 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DIDNT_RECIEVE_OTP) {
        return "\u0913\u091f\u0940\u092a\u0940 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0928\u0939\u0940\u0902 \u0939\u0941\u0906? ";
    };
    if (stringKey instanceof Language_Types.RESEND_OTP) {
        return "<a href=\"\">\u0913\u091f\u0940\u092a\u0940 \u092a\u0941\u0928\u0903 \u092d\u0947\u091c\u0947\u0902</a>";
    };
    if (stringKey instanceof Language_Types.PLEASE_ENTER_VALID_OTP) {
        return "\u0915\u0943\u092a\u092f\u093e \u0938\u0939\u0940 OTP \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.INVALID_MOBILE_NUMBER) {
        return "\u0917\u0932\u0924 \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.REGISTER) {
        return "\u0930\u091c\u093f\u0938\u094d\u091f\u0930 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.MOBILE_NUMBER) {
        return "\u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.AUTO_READING_OTP) {
        return "\u0911\u091f\u094b \u0930\u0940\u0921\u093f\u0902\u0917 \u0913\u091f\u0940\u092a\u0940 ...";
    };
    if (stringKey instanceof Language_Types.UPLOAD_DRIVING_LICENSE) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.UPLOAD_BACK_SIDE) {
        return "\u092a\u093f\u091b\u0932\u093e \u092d\u093e\u0917 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.UPLOAD_FRONT_SIDE) {
        return "\u0905\u092a\u0928\u0947 DL \u0915\u093e \u092b\u094b\u091f\u094b \u0938\u093e\u0907\u0921 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.BACK_SIDE) {
        return "\u092a\u093f\u091b\u0932\u093e \u092d\u093e\u0917";
    };
    if (stringKey instanceof Language_Types.FRONT_SIDE) {
        return "\u0906\u092a\u0915\u0947 DL \u0915\u093e \u092b\u094b\u091f\u094b \u0938\u093e\u0907\u0921";
    };
    if (stringKey instanceof Language_Types.NEXT) {
        return "\u0905\u0917\u0932\u093e \u092a\u0943\u0937\u094d\u0920";
    };
    if (stringKey instanceof Language_Types.LICENSE_INSTRUCTION_PICTURE) {
        return "\u0915\u0943\u092a\u092f\u093e \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0915\u0947 \u0926\u094b\u0928\u094b\u0902 \u092a\u0915\u094d\u0937\u094b\u0902 \u0915\u0940 \u0938\u094d\u092a\u0937\u094d\u091f \u0924\u0938\u094d\u0935\u0940\u0930\u0947\u0902 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LICENSE_INSTRUCTION_CLARITY) {
        return "\u0938\u0941\u0928\u093f\u0936\u094d\u091a\u093f\u0924 \u0915\u0930\u0947\u0902 \u0915\u093f \u092b\u094b\u091f\u094b \u0914\u0930 \u0938\u092d\u0940 \u0935\u093f\u0935\u0930\u0923 \u0938\u094d\u092a\u0937\u094d\u091f \u0930\u0942\u092a \u0938\u0947 \u0926\u093f\u0916\u093e\u0908 \u0926\u0947 \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.REGISTRATION_STEPS) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u091a\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.PROGRESS_SAVED) {
        return "\u0906\u092a\u0915\u0940 \u092a\u094d\u0930\u0917\u0924\u093f \u0938\u0939\u0947\u091c \u0932\u0940 \u0917\u0908 \u0939\u0948, \u0906\u092a \u0915\u093f\u0938\u0940 \u092d\u0940 \u091c\u093e\u0928\u0915\u093e\u0930\u0940 \u0915\u094b \u092c\u0926\u0932\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u092a\u093f\u091b\u0932\u0947 \u091a\u0930\u0923\u094b\u0902 \u092a\u0930 \u0935\u093e\u092a\u0938 \u091c\u093e \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.DRIVING_LICENSE) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938";
    };
    if (stringKey instanceof Language_Types.AADHAR_CARD) {
        return "\u0906\u0927\u093e\u0930 \u0915\u093e\u0930\u094d\u0921";
    };
    if (stringKey instanceof Language_Types.BANK_DETAILS) {
        return "\u092c\u0948\u0902\u0915 \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.VEHICLE_DETAILS) {
        return "\u0935\u093e\u0939\u0928 \u0915\u0940 \u0938\u0942\u091a\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.UPLOAD_FRONT_BACK) {
        return "\u0906\u0917\u0947 \u0914\u0930 \u092a\u0940\u091b\u0947 \u0915\u0947 \u092d\u093e\u0917 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.EARNINGS_WILL_BE_CREDITED) {
        return "\u0906\u092a\u0915\u0940 \u0915\u092e\u093e\u0908 \u092f\u0939\u093e\u0902 \u0915\u094d\u0930\u0947\u0921\u093f\u091f \u0915\u0940 \u091c\u093e\u090f\u0917\u0940";
    };
    if (stringKey instanceof Language_Types.FILL_VEHICLE_DETAILS) {
        return "\u0905\u092a\u0928\u0947 \u0935\u093e\u0939\u0928 \u0915\u093e \u0935\u093f\u0935\u0930\u0923 \u092d\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.FOLLOW_STEPS) {
        return "\u0930\u091c\u093f\u0938\u094d\u091f\u0930 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0915\u0943\u092a\u092f\u093e \u0928\u0940\u091a\u0947 \u0926\u093f\u090f \u0917\u090f \u091a\u0930\u0923\u094b\u0902 \u0915\u093e \u092a\u093e\u0932\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.REGISTRATION) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.UPLOAD_ADHAAR_CARD) {
        return "\u0906\u0927\u093e\u0930 \u0915\u093e\u0930\u094d\u0921 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADHAAR_INTRUCTION_PICTURE) {
        return "\u0915\u0943\u092a\u092f\u093e \u0906\u0927\u093e\u0930 \u0915\u093e\u0930\u094d\u0921 \u0915\u0947 \u0926\u094b\u0928\u094b\u0902 \u092a\u0915\u094d\u0937\u094b\u0902 \u0915\u0940 \u0938\u094d\u092a\u0937\u094d\u091f \u0924\u0938\u094d\u0935\u0940\u0930\u0947\u0902 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_VEHICLE_DETAILS) {
        return "\u0935\u093e\u0939\u0928 \u0935\u093f\u0935\u0930\u0923 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.VEHICLE_REGISTRATION_NUMBER) {
        return "\u0935\u093e\u0939\u0928 \u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0938\u0902\u0916\u094d\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.RE_ENTER_VEHICLE_REGISTRATION_NUMBER) {
        return "\u0935\u093e\u0939\u0928 \u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0938\u0902\u0916\u094d\u092f\u093e \u092a\u0941\u0928\u0903 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_VEHICLE_NO) {
        return "\u0935\u093e\u0939\u0928 \u0938\u0902\u0916\u094d\u092f\u093e \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.VEHICLE_TYPE) {
        return "\u0935\u093e\u0939\u0928 \u0915\u093e \u092a\u094d\u0930\u0915\u093e\u0930";
    };
    if (stringKey instanceof Language_Types.VEHICLE_MODEL_NAME) {
        return "\u0935\u093e\u0939\u0928 \u092e\u0949\u0921\u0932 \u0915\u093e \u0928\u093e\u092e";
    };
    if (stringKey instanceof Language_Types.ENTER_MODEL_NAME) {
        return "\u092e\u0949\u0921\u0932 \u0915\u093e \u0928\u093e\u092e \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.VEHICLE_COLOUR) {
        return "\u0935\u093e\u0939\u0928 \u0915\u093e \u0930\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.ENTER_VEHICLE_COLOUR) {
        return "\u0935\u093e\u0939\u0928 \u0915\u093e \u0930\u0902\u0917 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.UPLOAD_REGISTRATION_CERTIFICATE) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902 (\u0906\u0930\u0938\u0940)";
    };
    if (stringKey instanceof Language_Types.UPLOAD_RC) {
        return "\u0906\u0930\u0938\u0940 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PREVIEW) {
        return "\u0907\u092e\u0947\u091c \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CHOOSE_VEHICLE_TYPE) {
        return "\u0935\u093e\u0939\u0928 \u0915\u093e \u092a\u094d\u0930\u0915\u093e\u0930 \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.BENIFICIARY_NUMBER) {
        return "\u0932\u093e\u092d\u093e\u0930\u094d\u0925\u0940 \u0916\u093e\u0924\u093e \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.RE_ENTER_BENIFICIARY_NUMBER) {
        return "\u0932\u093e\u092d\u093e\u0930\u094d\u0925\u0940 \u0916\u093e\u0924\u093e \u0938\u0902\u0916\u094d\u092f\u093e \u092a\u0941\u0928\u0903 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.IFSC_CODE) {
        return "IFSC \u0915\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.SENDING_OTP) {
        return "\u0913\u091f\u0940\u092a\u0940 \u092d\u0947\u091c \u0930\u0939\u0947 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT_WHILE_IN_PROGRESS) {
        return "\u0915\u0943\u092a\u092f\u093e \u092a\u094d\u0930\u0917\u0924\u093f \u0915\u0947 \u0926\u094c\u0930\u093e\u0928 \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN) {
        return "\u0906\u092a\u0915\u0947 \u0905\u0928\u0941\u0930\u094b\u0927 \u0915\u093e \u0938\u092e\u092f \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b \u0917\u092f\u093e \u0939\u0948 \u092b\u093f\u0930 \u0938\u0947 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER) {
        return "\u0924\u094d\u0930\u0941\u091f\u093f \u0939\u0941\u0908 \u0915\u0943\u092a\u092f\u093e \u092c\u093e\u0926 \u092e\u0947\u0902 \u092a\u0941\u0928: \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_OTP_SENT_TO) {
        return " \u092a\u0930 \u092d\u0947\u091c\u0947 \u0917\u090f OTP \u0915\u094b \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.OTP_SENT_TO) {
        return " \u092a\u0930 OTP \u092d\u0947\u091c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.COUNTRY_CODE_INDIA) {
        return "+91";
    };
    if (stringKey instanceof Language_Types.ENTER_ACCOUNT_NUMBER) {
        return "\u0916\u093e\u0924\u093e \u0938\u0902\u0916\u094d\u092f\u093e \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.ADD_BANK_DETAILS) {
        return "\u092c\u0948\u0902\u0915 \u0935\u093f\u0935\u0930\u0923 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_IFSC_CODE) {
        return "IFSC \u0915\u094b\u0921 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SUBMIT) {
        return "\u091c\u092e\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PERSONAL_DETAILS) {
        return "\u0935\u094d\u092f\u0915\u094d\u0924\u093f\u0917\u0924 \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.LANGUAGES) {
        return "\u092c\u094b\u0932\u0940";
    };
    if (stringKey instanceof Language_Types.HELP_AND_FAQ) {
        return "\u0938\u0939\u093e\u092f\u0924\u093e \u0914\u0930 \u0905\u0915\u094d\u0938\u0930 \u092a\u0942\u091b\u0947 \u091c\u093e\u0928\u0947 \u0935\u093e\u0932\u0947 \u092a\u094d\u0930\u0936\u094d\u0928";
    };
    if (stringKey instanceof Language_Types.ABOUT) {
        return "\u090f\u092a \u0915\u0947 \u092c\u093e\u0930\u0947 \u092e\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LOGOUT) {
        return "\u0932\u0949\u0917 \u0906\u0909\u091f";
    };
    if (stringKey instanceof Language_Types.UPDATE) {
        return "\u0905\u092a\u0921\u0947\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.EDIT) {
        return "\u0938\u0902\u092a\u093e\u0926\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.AUTO) {
        return "\u0911\u091f\u094b";
    };
    if (stringKey instanceof Language_Types.NAME) {
        return "\u0928\u093e\u092e";
    };
    if (stringKey instanceof Language_Types.PRIVACY_POLICY) {
        return "\u0917\u094b\u092a\u0928\u0940\u092f\u0924\u093e \u0928\u0940\u0924\u093f";
    };
    if (stringKey instanceof Language_Types.LOGO) {
        return "\u092a\u094d\u0930\u0924\u0940\u0915 \u091a\u093f\u0928\u094d\u0939";
    };
    if (stringKey instanceof Language_Types.ABOUT_APP_DESCRIPTION) {
        return "\u092f\u093e\u0924\u094d\u0930\u093f\u092f\u094b\u0902 \u0915\u0947 \u0938\u093e\u0925 \u0921\u094d\u0930\u093e\u0907\u0935\u0930\u094b\u0902 \u0915\u094b \u091c\u094b\u0921\u093c\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u092f\u093e\u0924\u094d\u0930\u0940 \u092a\u093e\u0930\u094d\u091f\u0928\u0930 \u090f\u0915 \u0916\u0941\u0932\u093e \u092e\u0902\u091a \u0939\u0948\u0964 \u0910\u092a \u0921\u094d\u0930\u093e\u0907\u0935\u0930\u094b\u0902 \u0915\u0947 \u0932\u093f\u090f \u092f\u093e\u0924\u094d\u0930\u093f\u092f\u094b\u0902 \u0915\u094b \u0922\u0942\u0902\u0922\u0928\u093e \u0906\u0938\u093e\u0928 \u092c\u0928\u093e\u0924\u093e \u0939\u0948\u0964 \u0914\u0930 \u0938\u0947\u0935\u093e \u092a\u094d\u0930\u0926\u093e\u0924\u093e\u0913\u0902 \u0915\u0947 \u0938\u093e\u0925 \u091c\u094b\u0921\u093c\u0915\u0930 \u0907\u0928 \u0935\u093f\u0915\u0932\u094d\u092a\u094b\u0902 \u0915\u093e \u0932\u093e\u092d \u0909\u0920\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.TERMS_AND_CONDITIONS) {
        return "\u0928\u093f\u092f\u092e \u090f\u0935\u0902 \u0936\u0930\u094d\u0924\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.UPDATE_VEHICLE_DETAILS) {
        return "\u0935\u093e\u0939\u0928 \u0935\u093f\u0935\u0930\u0923 \u0905\u092a\u0921\u0947\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NOTE) {
        return "\u091f\u093f\u092a\u094d\u092a\u0923\u0940:";
    };
    if (stringKey instanceof Language_Types.VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS) {
        return "\u0935\u093f\u0936\u093f\u0937\u094d\u091f \u0936\u093f\u0915\u093e\u092f\u0924\u094b\u0902 \u0915\u0947 \u0932\u093f\u090f \u092e\u093e\u0908 \u0930\u093e\u0907\u0921\u094d\u0938 \u0905\u0928\u0941\u092d\u093e\u0917 \u092a\u0930 \u091c\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.THANK_YOU_FOR_WRTITTING_US) {
        return "\u0939\u092e\u0947\u0902 \u0932\u093f\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0927\u0928\u094d\u092f\u0935\u093e\u0926!";
    };
    if (stringKey instanceof Language_Types.WE_HAVE_RECIEVED_YOUR_ISSUE) {
        return "\u0939\u092e\u0947\u0902 \u0906\u092a\u0915\u093e \u092e\u0941\u0926\u094d\u0926\u093e \u092e\u093f\u0932 \u0917\u092f\u093e \u0939\u0948\u0964 \u0939\u092e \u0915\u0941\u091b \u0926\u0947\u0930 \u092e\u0947\u0902 \u0906\u092a\u0938\u0947 \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947\u0902\u0917\u0947";
    };
    if (stringKey instanceof Language_Types.GO_TO_HOME) {
        return "\u0917\u094b \u0939\u094b\u092e";
    };
    if (stringKey instanceof Language_Types.YOUR_RECENT_RIDE) {
        return "\u0906\u092a\u0915\u0940 \u0939\u093e\u0932 \u0915\u0940 \u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.YOUR_RECENT_TRIP) {
        return "Your Recent Trip";
    };
    if (stringKey instanceof Language_Types.ALL_TOPICS) {
        return "\u0938\u092d\u0940 \u0935\u093f\u0937\u092f";
    };
    if (stringKey instanceof Language_Types.REPORT_AN_ISSUE_WITH_THIS_TRIP) {
        return "\u0907\u0938 \u092f\u093e\u0924\u094d\u0930\u093e \u092e\u0947\u0902 \u0915\u093f\u0938\u0940 \u0938\u092e\u0938\u094d\u092f\u093e \u0915\u0940 \u0930\u093f\u092a\u094b\u0930\u094d\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOU_RATED) {
        return "\u0906\u092a\u0928\u0947 \u092e\u0942\u0932\u094d\u092f\u093e\u0902\u0915\u0928 \u0915\u093f\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.VIEW_ALL_RIDES) {
        return "\u0938\u092d\u0940 \u0938\u0935\u093e\u0930\u0940 \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.WRITE_TO_US) {
        return "\u0939\u092e\u0947 \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.SUBJECT) {
        return "\u0935\u093f\u0937\u092f";
    };
    if (stringKey instanceof Language_Types.YOUR_EMAIL_ID) {
        return "\u0906\u092a\u0915\u0940 \u0908\u092e\u0947\u0932 \u0906\u0908\u0921\u0940";
    };
    if (stringKey instanceof Language_Types.DESCRIBE_YOUR_ISSUE) {
        return "\u0905\u092a\u0928\u0940 \u0938\u092e\u0938\u094d\u092f\u093e \u0915\u093e \u0935\u0930\u094d\u0923\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GETTING_STARTED_AND_FAQ) {
        return "\u0906\u0930\u0902\u092d \u0915\u0930\u0928\u093e \u0914\u0930 \u0905\u0915\u094d\u0938\u0930 \u092a\u0942\u091b\u0947 \u091c\u093e\u0928\u0947 \u0935\u093e\u0932\u0947 \u092a\u094d\u0930\u0936\u094d\u0928";
    };
    if (stringKey instanceof Language_Types.FOR_OTHER_ISSUES_WRITE_TO_US) {
        return "\u0905\u0928\u094d\u092f \u092e\u0941\u0926\u094d\u0926\u094b\u0902 \u0915\u0947 \u0932\u093f\u090f, \u0939\u092e\u0947\u0902 \u0932\u093f\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CALL_SUPPORT_CENTER) {
        return "\u0915\u0949\u0932 \u0938\u092a\u094b\u0930\u094d\u091f \u0938\u0947\u0902\u091f\u0930";
    };
    if (stringKey instanceof Language_Types.YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE) {
        return "\u0906\u092a \u092f\u0939\u093e\u0902 \u091c\u093f\u0938 \u0938\u092e\u0938\u094d\u092f\u093e \u0915\u093e \u0938\u093e\u092e\u0928\u093e \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902 \u0909\u0938\u0915\u093e \u0935\u0930\u094d\u0923\u0928 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.REGISTRATION_CERTIFICATE_IMAGE) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 (\u0906\u0930\u0938\u0940) \u091b\u0935\u093f";
    };
    if (stringKey instanceof Language_Types.HOME) {
        return "\u0939\u094b\u092e";
    };
    if (stringKey instanceof Language_Types.RIDES) {
        return "\u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.MY_RIDES) {
        return "\u092e\u0947\u0930\u0940 \u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.PROFILE) {
        return "\u092a\u094d\u0930\u094b\u092b\u093c\u093e\u0907\u0932";
    };
    if (stringKey instanceof Language_Types.ENTER_DRIVING_LICENSE_NUMBER) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0928\u0902\u092c\u0930 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.WHERE_IS_MY_LICENSE_NUMBER) {
        return "\u092e\u0947\u0930\u093e \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0928\u0902\u092c\u0930 \u0915\u0939\u093e\u0902 \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.TRIP_DETAILS) {
        return "\u092f\u093e\u0924\u094d\u0930\u093e \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.BY_CASH) {
        return "\u0928\u0915\u0926 \u0926\u094d\u0935\u093e\u0930\u093e";
    };
    if (stringKey instanceof Language_Types.ONLINE_) {
        return "\u0911\u0928\u0932\u093e\u0907\u0928";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE_POPUP) {
        return "<u>\u0917\u094b \u0911\u0928\u0932\u093e\u0907\u0928</u>";
    };
    if (stringKey instanceof Language_Types.REPORT_AN_ISSUE) {
        return "\u092e\u093e\u092e\u0932\u0947 \u0915\u0940 \u0930\u093f\u092a\u094b\u0930\u094d\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DISTANCE) {
        return "\u0926\u0942\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.TIME_TAKEN) {
        return "\u0938\u092e\u092f \u0932\u093f\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.CALL) {
        return "\u0915\u0949\u0932";
    };
    if (stringKey instanceof Language_Types.START_RIDE) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CANCEL_RIDE) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0930\u0926\u094d\u0926 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.END_RIDE) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0905\u0902\u0924 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RIDE_COMPLETED_WITH) {
        return "\u0938\u0935\u093e\u0930\u0940 \u092a\u0942\u0930\u0940 \u0939\u0941\u0908";
    };
    if (stringKey instanceof Language_Types.COLLECT_AMOUNT_IN_CASH) {
        return "\u0928\u0915\u0926 \u092e\u0947\u0902 \u0930\u093e\u0936\u093f \u091c\u092e\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CASH_COLLECTED) {
        return "\u0928\u0915\u0926 \u090f\u0915\u0924\u094d\u0930\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.OFFLINE) {
        return "\u0911\u092b\u0932\u093e\u0907\u0928";
    };
    if (stringKey instanceof Language_Types.ACCEPT_FOR) {
        return "\u0907\u0938\u0915\u0947 \u0932\u093f\u090f \u0938\u094d\u0935\u0940\u0915\u093e\u0930 \u0915\u0930\u0947\u0902:";
    };
    if (stringKey instanceof Language_Types.DECLINE) {
        return "\u0930\u0926\u094d\u0926 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.REQUEST) {
        return "\u0930\u0940\u0915\u094d\u0935\u0947\u0938\u094d\u091f";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_OFFLINE) {
        return "\u0906\u092a \u0911\u092b\u093c\u0932\u093e\u0907\u0928 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS) {
        return "\u0906\u092a \u0907\u0938 \u0938\u092e\u092f \u0935\u094d\u092f\u0938\u094d\u0924 \u0939\u0948\u0902\u0964 \u092f\u093e\u0924\u094d\u0930\u093e \u0905\u0928\u0941\u0930\u094b\u0927 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0911\u0928\u0932\u093e\u0907\u0928 \u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE) {
        return "\u0911\u092b\u0932\u093e\u0907\u0928 \u091c\u093e\u0928\u0947 \u0938\u0947 \u0906\u092a\u0915\u094b \u0915\u094b\u0908 \u0938\u0935\u093e\u0930\u0940 \u0928\u0939\u0940\u0902 \u092e\u093f\u0932\u0947\u0917\u0940";
    };
    if (stringKey instanceof Language_Types.CANCEL) {
        return "\u0930\u0926\u094d\u0926 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GO_OFFLINE) {
        return "\u0911\u092b\u093c \u0932\u093e\u0907\u0928 \u0939\u094b \u091c\u093e\u0913";
    };
    if (stringKey instanceof Language_Types.IS_WAITING_FOR_YOU) {
        return "\u092a\u093f\u0915\u0905\u092a \u0915\u0930\u0928\u0947 \u091c\u093e \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ON_A_RIDE) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0936\u0941\u0930\u0942 \u0939\u0941\u0908...";
    };
    if (stringKey instanceof Language_Types.PLEASE_ASK_RIDER_FOR_THE_OTP) {
        return "\u0915\u0943\u092a\u092f\u093e \u0913\u091f\u0940\u092a\u0940 \u0915\u0947 \u0932\u093f\u090f \u0930\u093e\u0907\u0921\u0930 \u0938\u0947 \u092a\u0942\u091b\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.COMPLETED_) {
        return "\u092a\u0942\u0930\u093e \u0939\u0941\u0906";
    };
    if (stringKey instanceof Language_Types.CANCELLED_) {
        return "\u0930\u0926\u094d\u0926";
    };
    if (stringKey instanceof Language_Types.ENTER_RC_NUMBER) {
        return "\u0906\u0930\u0938\u0940 \u0928\u0902\u092c\u0930 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ALLOW_ACCESS) {
        return "\u0909\u092a\u092f\u094b\u0917 \u0915\u0940 \u0905\u0928\u0941\u092e\u0924\u093f \u0926\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.WE_NEED_SOME_ACCESS) {
        return "\u0939\u092e\u0947\u0902 \u0915\u0941\u091b \u0910\u0915\u094d\u0938\u0947\u0938 \u091a\u093e\u0939\u093f\u090f";
    };
    if (stringKey instanceof Language_Types.WHERE_IS_MY_RC_NUMBER) {
        return "\u092e\u0947\u0930\u093e \u0906\u0930\u0938\u0940 \u0928\u0902\u092c\u0930 \u0915\u0939\u093e\u0902 \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.THANK_YOU_FOR_WRITING_TO_US) {
        return "\u0939\u092e\u0947\u0902 \u0932\u093f\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0927\u0928\u094d\u092f\u0935\u093e\u0926!";
    };
    if (stringKey instanceof Language_Types.RIDER) {
        return "\u0938\u0935\u093e\u0930";
    };
    if (stringKey instanceof Language_Types.TRIP_ID) {
        return "\u091f\u094d\u0930\u093f\u092a \u0906\u0908\u0921\u0940";
    };
    if (stringKey instanceof Language_Types.NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST) {
        return "\u0910\u092a \u092c\u0902\u0926 \u0939\u094b\u0928\u0947 \u092a\u0930 \u0906\u092a\u0915\u094b \u0906\u0928\u0947 \u0935\u093e\u0932\u0940 \u0938\u0935\u093e\u0930\u0940 \u0905\u0928\u0941\u0930\u094b\u0927 \u0926\u093f\u0916\u093e\u0928\u0947 \u0915\u0940 \u0906\u0935\u0936\u094d\u092f\u0915\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP) {
        return "\u0910\u092a \u0915\u0947 \u0932\u093f\u090f \u092c\u0948\u091f\u0930\u0940 \u0911\u092a\u094d\u091f\u093f\u092e\u093e\u0907\u091c\u093c\u0947\u0936\u0928 \u0915\u094b \u0905\u0915\u094d\u0937\u092e \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f, \u0910\u092a \u092c\u0902\u0926 \u0939\u094b\u0928\u0947 \u092a\u0930 \u0906\u092a\u0915\u094b \u0911\u0928\u0932\u093e\u0907\u0928 \u0930\u0916\u0928\u0947 \u0915\u0940 \u0906\u0935\u0936\u094d\u092f\u0915\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.NEED_IT_TO_AUTOSTART_YOUR_APP) {
        return "\u0910\u092a \u092c\u0902\u0926 \u0939\u094b\u0928\u0947 \u092a\u0930 \u0906\u092a\u0915\u094b \u0911\u0928\u0932\u093e\u0907\u0928 \u0930\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0905\u092a\u0928\u0947 \u0910\u092a \u0915\u094b \u0911\u091f\u094b\u0938\u094d\u091f\u093e\u0930\u094d\u091f \u0915\u0930\u0928\u0947 \u0915\u0940 \u0906\u0935\u0936\u094d\u092f\u0915\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.NEED_IT_TO_ENABLE_LOCATION) {
        return "\u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u092a\u093e\u0930\u094d\u091f\u0928\u0930 \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0915\u0947 \u0932\u094b\u0915\u0947\u0936\u0928 \u0915\u0940 \u0928\u093f\u0917\u0930\u093e\u0928\u0940 \u0915\u0947 \u0932\u093f\u090f \u0905\u092a\u0928\u093e \u0938\u094d\u0925\u093e\u0928 \u0938\u093e\u091d\u093e \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0932\u094b\u0915\u0947\u0936\u0928 \u0921\u0947\u091f\u093e \u090f\u0915\u0924\u094d\u0930 \u0915\u0930\u0924\u093e \u0939\u0948, \u0924\u092c \u092d\u0940 \u091c\u092c \u0910\u092a \u092c\u0902\u0926 \u0939\u094b \u092f\u093e \u0909\u092a\u092f\u094b\u0917 \u092e\u0947\u0902 \u0928 \u0939\u094b\u0964";
    };
    if (stringKey instanceof Language_Types.OVERLAY_TO_DRAW_OVER_APPLICATIONS) {
        return "\u0905\u0928\u0941\u092a\u094d\u0930\u092f\u094b\u0917\u094b\u0902 \u092a\u0930 \u0906\u0915\u0930\u094d\u0937\u093f\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0913\u0935\u0930\u0932\u0947";
    };
    if (stringKey instanceof Language_Types.BATTERY_OPTIMIZATIONS) {
        return "\u092c\u0948\u091f\u0930\u0940 \u0905\u0928\u0941\u0915\u0942\u0932\u0928";
    };
    if (stringKey instanceof Language_Types.AUTO_START_APPLICATION_IN_BACKGROUND) {
        return "\u092a\u0943\u0937\u094d\u0920\u092d\u0942\u092e\u093f \u092e\u0947\u0902 \u0911\u091f\u094b\u0938\u094d\u091f\u093e\u0930\u094d\u091f \u0906\u0935\u0947\u0926\u0928";
    };
    if (stringKey instanceof Language_Types.LOCATION_ACCESS) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0905\u092d\u093f\u0917\u092e";
    };
    if (stringKey instanceof Language_Types.STEP) {
        return "\u0915\u0926\u092e";
    };
    if (stringKey instanceof Language_Types.PAID) {
        return "\u092a\u094d\u0930\u0926\u0924\u094d\u0924";
    };
    if (stringKey instanceof Language_Types.PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL) {
        return "\u0915\u0943\u092a\u092f\u093e \u0939\u092e\u0947\u0902 \u092c\u0924\u093e\u090f\u0902 \u0915\u093f \u0906\u092a \u0915\u094d\u092f\u094b\u0902 \u0930\u0926\u094d\u0926 \u0915\u0930\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.MANDATORY) {
        return "\u0905\u0928\u093f\u0935\u093e\u0930\u094d\u092f";
    };
    if (stringKey instanceof Language_Types.ENTERED_WRONG_OTP) {
        return "\u0917\u0932\u0924 \u0913\u091f\u0940\u092a\u0940 \u0926\u0930\u094d\u091c \u0915\u093f\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.OTP_INVALID_FOR_THIS_VEHICLE_VARIANT) {
        return "\u0905\u0935\u0948\u0927 \u0913\u091f\u0940\u092a\u0940 - \u0935\u093e\u0939\u0928 \u0915\u093e \u092a\u094d\u0930\u0915\u093e\u0930 \u0938\u0935\u093e\u0930\u0940 \u0915\u0947 \u092a\u094d\u0930\u0915\u093e\u0930 \u0938\u0947 \u092e\u0947\u0932 \u0928\u0939\u0940\u0902 \u0916\u093e \u0930\u0939\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.COPIED) {
        return "\u0915\u0949\u092a\u0940 \u0915\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.BANK_NAME) {
        return "\u092c\u0948\u0902\u0915 \u0915\u093e \u0928\u093e\u092e";
    };
    if (stringKey instanceof Language_Types.AADHAR_DETAILS) {
        return "\u0906\u0927\u093e\u0930 \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.AADHAR_NUMBER) {
        return "\u0906\u0927\u093e\u0930 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.FRONT_SIDE_IMAGE) {
        return "\u0938\u093e\u092e\u0928\u0947 \u0915\u0940 \u0913\u0930 \u091b\u0935\u093f";
    };
    if (stringKey instanceof Language_Types.BACK_SIDE_IMAGE) {
        return " \u092c\u0948\u0915 \u0938\u093e\u0907\u0921 \u0907\u092e\u0947\u091c";
    };
    if (stringKey instanceof Language_Types.STILL_NOT_RESOLVED) {
        return "\u0905\u092d\u0940 \u092d\u0940 \u0939\u0932 \u0928\u0939\u0940\u0902 \u0939\u0948? \u0939\u092e\u0947\u0902 \u092c\u0941\u0932\u093e\u0913";
    };
    if (stringKey instanceof Language_Types.CASE_TWO) {
        return "\u092c) ";
    };
    if (stringKey instanceof Language_Types.NON_DISCLOUSER_AGREEMENT) {
        return "\u0915\u094b\u0908 \u0905\u0938\u094d\u0935\u0940\u0915\u0930\u0923 \u0938\u092e\u091d\u094c\u0924\u093e \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.DATA_COLLECTION_AUTHORITY) {
        return "\u0938\u0940) \u092e\u0948\u0902 \u0905\u092a\u0928\u0940 \u091c\u093e\u0928\u0915\u093e\u0930\u0940 \u090f\u0915\u0924\u094d\u0930 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f Juspay \u0915\u094b \u0928\u093f\u092f\u0941\u0915\u094d\u0924 \u0914\u0930 \u0905\u0927\u093f\u0915\u0943\u0924 \u0915\u0930\u0924\u093e \u0939\u0942\u0902, \u0914\u0930 \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0938\u0947, \u092e\u0948\u0902 \u0909\u092a\u092f\u094b\u0917 \u0915\u0940 \u0936\u0930\u094d\u0924\u094b\u0902 \u0914\u0930 \u0917\u094b\u092a\u0928\u0940\u092f\u0924\u093e \u0928\u0940\u0924\u093f \u0938\u0947 \u0938\u0939\u092e\u0924 \u0939\u0942\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.SOFTWARE_LICENSE) {
        return "\u0938\u0949\u092b\u093c\u094d\u091f\u0935\u0947\u092f\u0930 \u0932\u093e\u0907\u0938\u0947\u0902\u0938";
    };
    if (stringKey instanceof Language_Types.LOAD_MORE) {
        return "\u0914\u0930 \u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ARE_YOU_SURE_YOU_WANT_TO_LOGOUT) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0932\u0949\u0917 \u0906\u0909\u091f \u0915\u0930\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.GO_BACK) {
        return "\u092a\u0940\u091b\u0947 \u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0935\u093e\u0915\u0908 \u0938\u0935\u093e\u0930\u0940 \u0916\u0924\u094d\u092e \u0915\u0930\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.THANK_YOU_FOR_REGISTERING_US) {
        return "\u0939\u092e\u093e\u0930\u0947 \u0938\u093e\u0925 \u0930\u091c\u093f\u0938\u094d\u091f\u0930 \u0915\u0947 \u0932\u093f\u090f \u0927\u0928\u094d\u092f\u0935\u093e\u0926!";
    };
    if (stringKey instanceof Language_Types.UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU) {
        return "\u0926\u0941\u0930\u094d\u092d\u093e\u0917\u094d\u092f \u0938\u0947, \u0939\u092e \u0906\u092a\u0915\u0947 \u0932\u093f\u090f \u0905\u092d\u0940 \u0924\u0915 \u0909\u092a\u0932\u092c\u094d\u0927 \u0928\u0939\u0940\u0902 \u0939\u0948\u0902\u0964 \u0939\u092e \u0906\u092a\u0915\u094b \u091c\u0932\u094d\u0926 \u0939\u0940 \u0938\u0942\u091a\u093f\u0924 \u0915\u0930\u0947\u0902\u0917\u0947\u0964";
    };
    if (stringKey instanceof Language_Types.EMPTY_RIDES) {
        return "\u0916\u093e\u0932\u0940 \u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.YOU_HAVE_NOT_TAKEN_A_TRIP_YET) {
        return "\u0906\u092a\u0928\u0947 \u0905\u092d\u0940 \u0924\u0915 \u0915\u094b\u0908 \u092f\u093e\u0924\u094d\u0930\u093e \u0928\u0939\u0940\u0902 \u0915\u0940 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.BOOK_NOW) {
        return "\u0905\u092d\u0940 \u092c\u0941\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RESEND_OTP_IN) {
        return "<u>\u0913\u091f\u0940\u092a\u0940 \u092b\u093f\u0930 \u0938\u0947 \u092d\u0947\u091c\u0947\u0902 ";
    };
    if (stringKey instanceof Language_Types.WE_NEED_ACCESS_TO_YOUR_LOCATION) {
        return "\u0939\u092e\u0947\u0902 \u0906\u092a\u0915\u0947 \u0932\u094b\u0915\u0947\u0936\u0928 \u0915\u0940 \u0906\u0935\u0936\u094d\u092f\u0915\u0924\u093e \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.YOUR_LOCATION_HELPS_OUR_SYSTEM) {
        return "\u0906\u092a\u0915\u093e \u0932\u094b\u0915\u0947\u0936\u0928 \u0939\u092e\u093e\u0930\u0947 \u0938\u093f\u0938\u094d\u091f\u092e \u0915\u094b \u0906\u0938-\u092a\u093e\u0938 \u0915\u0947 \u0938\u092d\u0940 \u0911\u091f\u094b \u0915\u094b \u092e\u0948\u092a \u0915\u0930\u0928\u0947 \u092e\u0947\u0902 \u092e\u0926\u0926 \u0915\u0930\u0924\u093e \u0939\u0948 \u0914\u0930 \u0906\u092a\u0915\u094b \u0938\u092c\u0938\u0947 \u0924\u0947\u091c \u0938\u0935\u093e\u0930\u0940 \u092a\u094d\u0930\u0926\u093e\u0928 \u0915\u0930\u0924\u093e \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.NO_INTERNET_CONNECTION) {
        return "\u0915\u094b\u0908 \u0907\u0902\u091f\u0930\u0928\u0947\u091f \u0915\u0928\u0947\u0915\u094d\u0936\u0928 \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN) {
        return "\u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0928\u093e \u0907\u0902\u091f\u0930\u0928\u0947\u091f \u0915\u0928\u0947\u0915\u094d\u0936\u0928 \u091c\u093e\u0902\u091a\u0947\u0902 \u0914\u0930 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.TRY_AGAIN) {
        return "\u092a\u0941\u0928: \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GRANT_ACCESS) {
        return " \u090f\u0915\u094d\u0938\u0947\u0938 \u092a\u094d\u0930\u0926\u093e\u0928 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.YOUR_LIMIT_EXCEEDED_TRY_AGAIN_AFTER_10_MIN) {
        return "\u0906\u092a \u0938\u0940\u092e\u093e \u092a\u093e\u0930 \u0915\u0930 \u0917\u090f \u0939\u0948\u0902, 10 \u092e\u093f\u0928\u091f \u0915\u0947 \u092c\u093e\u0926 \u092a\u0941\u0928: \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_REFERRAL_MOBILE_NUMBER) {
        return "\u0930\u0947\u092b\u093c\u0930\u0932 \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.APPLY) {
        return "\u0932\u093e\u0917\u0942";
    };
    if (stringKey instanceof Language_Types.HAVE_A_REFERRAL) {
        return "\u090f\u0915 \u0930\u0947\u092b\u0930\u0932 \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.ADD_HERE) {
        return "\u092f\u0939\u093e\u0902 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.REFERRAL_APPLIED) {
        return "\u0930\u0947\u092b\u093c\u0930\u0932 \u0932\u093e\u0917\u0942!";
    };
    if (stringKey instanceof Language_Types.SMALLEDIT) {
        return "\u0938\u0902\u092a\u093e\u0926\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_DRIVING_LICENSE) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.HELP) {
        return "\u0938\u0939\u093e\u092f\u0924\u093e?";
    };
    if (stringKey instanceof Language_Types.INVALID_DL_NUMBER) {
        return "\u0917\u0932\u0924 \u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.DRIVING_LICENSE_NUMBER) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.RE_ENTER_DRIVING_LICENSE_NUMBER) {
        return "\u092b\u093f\u0930 \u0938\u0947 \u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.ENTER_DL_NUMBER) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0928\u0902\u092c\u0930 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SELECT_DATE_OF_BIRTH) {
        return "\u091c\u0928\u094d\u092e \u0915\u0940 \u0924\u093e\u0930\u0940\u0916 \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DATE_OF_BIRTH) {
        return "\u091c\u0928\u094d\u092e \u0915\u0940 \u0924\u093e\u0930\u0940\u0916";
    };
    if (stringKey instanceof Language_Types.WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION) {
        return "\u0906\u0938\u093e\u0928 \u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u0915\u0947 \u0932\u093f\u090f \u090f\u0915 \u091f\u094d\u092f\u0942\u091f\u094b\u0930\u093f\u092f\u0932 \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_MINIMUM_FIFTEEN_CHARACTERS) {
        return "\u0928\u094d\u092f\u0942\u0928\u0924\u092e 15 \u0935\u0930\u094d\u0923 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_YOUR_FRIEND) {
        return "\u0905\u092a\u0928\u0947 \u0926\u094b\u0938\u094d\u0924 \u0915\u094b \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE) {
        return "\u0915\u0943\u092a\u092f\u093e \u091b\u0935\u093f \u0915\u0940 \u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0930\u0924\u0947 \u0939\u0941\u090f \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.VALIDATING) {
        return "\u092a\u0941\u0937\u094d\u091f\u093f \u091c\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.VERIFICATION_PENDING) {
        return "\u091c\u093e\u0901\u091a \u091c\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.VERIFICATION_FAILED) {
        return "\u091c\u093e\u0901\u091a \u0905\u0938\u092b\u0932";
    };
    if (stringKey instanceof Language_Types.NO_DOC_AVAILABLE) {
        return "\u0915\u094b\u0908 \u0926\u0938\u094d\u0924\u093e\u0935\u0947\u091c \u0909\u092a\u0932\u092c\u094d\u0927 \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.ISSUE_WITH_DL_IMAGE) {
        return "\u0910\u0938\u093e \u0932\u0917\u0924\u093e \u0939\u0948 \u0915\u093f \u0906\u092a\u0915\u0940 DL \u0907\u092e\u0947\u091c \u092e\u0947\u0902 \u0915\u0941\u091b \u0938\u092e\u0938\u094d\u092f\u093e \u0939\u0948, \u0939\u092e\u093e\u0930\u0940 \u0938\u0939\u093e\u092f\u0924\u093e \u091f\u0940\u092e \u091c\u0932\u094d\u0926 \u0939\u0940 \u0906\u092a\u0938\u0947 \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947\u0917\u0940\u0964";
    };
    if (stringKey instanceof Language_Types.STILL_HAVE_SOME_DOUBT) {
        return "\u0905\u092d\u0940 \u092d\u0940 \u0915\u0941\u091b \u0938\u0902\u0926\u0947\u0939 \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.ISSUE_WITH_RC_IMAGE) {
        return "\u0910\u0938\u093e \u0932\u0917\u0924\u093e \u0939\u0948 \u0915\u093f \u0906\u092a\u0915\u0940 RC \u0907\u092e\u0947\u091c \u092e\u0947\u0902 \u0915\u0941\u091b \u0938\u092e\u0938\u094d\u092f\u093e \u0939\u0948, \u0939\u092e\u093e\u0930\u0940 \u0938\u0939\u093e\u092f\u0924\u093e \u091f\u0940\u092e \u091c\u0932\u094d\u0926 \u0939\u0940 \u0906\u092a\u0938\u0947 \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947\u0917\u0940\u0964";
    };
    if (stringKey instanceof Language_Types.PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT) {
        return "\u0915\u0943\u092a\u092f\u093e \u091b\u0935\u093f \u0915\u0947 \u0932\u093f\u090f \u091c\u093e\u0901\u091a \u0915\u0930\u0947\u0902 \u0915\u093f \u0935\u0948\u0927 \u0926\u0938\u094d\u0924\u093e\u0935\u0947\u091c\u093c \u091b\u0935\u093f \u0939\u0948 \u092f\u093e \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED) {
        return "\u0913\u0939! \u0906\u092a\u0915\u093e \u0906\u0935\u0947\u0926\u0928 \u0916\u093e\u0930\u093f\u091c \u0915\u0930 \u0926\u093f\u092f\u093e \u0917\u092f\u093e \u0939\u0948\u0964 \u0915\u0943\u092a\u092f\u093e \u092a\u0941\u0928: \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.INVALID_DRIVING_LICENSE) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938";
    };
    if (stringKey instanceof Language_Types.LIMIT_EXCEEDED_FOR_DL_UPLOAD) {
        return "\u0906\u0930\u0938\u0940 \u0905\u092a\u0932\u094b\u0921 \u0915\u0947 \u0932\u093f\u090f \u0938\u0940\u092e\u093e \u092a\u093e\u0930";
    };
    if (stringKey instanceof Language_Types.INVALID_VEHICLE_REGISTRATION_CERTIFICATE) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u0935\u093e\u0939\u0928 RC";
    };
    if (stringKey instanceof Language_Types.LIMIT_EXCEEDED_FOR_RC_UPLOAD) {
        return "\u0906\u0930\u0938\u0940 \u0905\u092a\u0932\u094b\u0921 \u0915\u0947 \u0932\u093f\u090f \u0938\u0940\u092e\u093e \u092a\u093e\u0930";
    };
    if (stringKey instanceof Language_Types.YOUR_DOCUMENTS_ARE_APPROVED) {
        return "\u0906\u092a\u0915\u0947 \u0926\u0938\u094d\u0924\u093e\u0935\u0947\u091c\u093c \u0938\u094d\u0935\u0940\u0915\u0943\u0924 \u0939\u0948\u0902\u0964 \u0938\u0939\u093e\u092f\u0924\u093e \u091f\u0940\u092e \u0906\u092a\u0915\u0947 \u0916\u093e\u0924\u0947 \u0915\u094b \u091c\u0932\u094d\u0926 \u0939\u0940 \u0938\u0915\u094d\u0937\u092e \u0915\u0930 \u0926\u0947\u0917\u0940\u0964 \u0906\u092a \u0905\u092a\u0928\u093e \u0916\u093e\u0924\u093e \u0938\u0915\u094d\u0937\u092e \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0938\u0939\u093e\u092f\u0924\u093e \u091f\u0940\u092e \u0915\u094b \u092d\u0940 \u0915\u0949\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.APPLICATION_STATUS) {
        return "\u0906\u0935\u0947\u0926\u0928 \u0915\u0940 \u0938\u094d\u0925\u093f\u0924\u093f";
    };
    if (stringKey instanceof Language_Types.FOR_SUPPORT) {
        return "\u0938\u092e\u0930\u094d\u0925\u0928 \u0915\u0947 \u0932\u093f\u090f";
    };
    if (stringKey instanceof Language_Types.CONTACT_US) {
        return " \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.IMAGE_VALIDATION_FAILED) {
        return "\u091b\u0935\u093f \u0915\u093e \u0938\u0924\u094d\u092f\u093e\u092a\u0928 \u0935\u093f\u092b\u0932 \u0930\u0939\u093e";
    };
    if (stringKey instanceof Language_Types.IMAGE_NOT_READABLE) {
        return "\u091b\u0935\u093f \u092a\u0922\u093c\u0928\u0947 \u092f\u094b\u0917\u094d\u092f \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.IMAGE_LOW_QUALITY) {
        return "\u091b\u0935\u093f \u0917\u0941\u0923\u0935\u0924\u094d\u0924\u093e \u0905\u091a\u094d\u091b\u0940 \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.IMAGE_INVALID_TYPE) {
        return "\u092c\u0936\u0930\u094d\u0924\u0947 \u091b\u0935\u093f \u092a\u094d\u0930\u0915\u093e\u0930 \u0935\u093e\u0938\u094d\u0924\u0935\u093f\u0915 \u092a\u094d\u0930\u0915\u093e\u0930 \u0938\u0947 \u092e\u0947\u0932 \u0928\u0939\u0940\u0902 \u0916\u093e\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.IMAGE_DOCUMENT_NUMBER_MISMATCH) {
        return "\u0907\u0938 \u091b\u0935\u093f \u092e\u0947\u0902 \u0926\u0938\u094d\u0924\u093e\u0935\u0947\u091c\u093c \u0938\u0902\u0916\u094d\u092f\u093e \u0907\u0928\u092a\u0941\u091f \u0915\u0947 \u0938\u093e\u0925 \u092e\u0947\u0932 \u0928\u0939\u0940\u0902 \u0916\u093e \u0930\u0939\u0940 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.IMAGE_EXTRACTION_FAILED) {
        return "\u091b\u0935\u093f \u0928\u093f\u0937\u094d\u0915\u0930\u094d\u0937\u0923 \u0935\u093f\u092b\u0932";
    };
    if (stringKey instanceof Language_Types.IMAGE_NOT_FOUND) {
        return "\u091b\u0935\u093f \u0928\u0939\u0940\u0902 \u092e\u093f\u0932\u0940";
    };
    if (stringKey instanceof Language_Types.IMAGE_NOT_VALID) {
        return "\u091b\u0935\u093f \u092e\u093e\u0928\u094d\u092f \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DRIVER_ALREADY_LINKED) {
        return "\u0905\u0928\u094d\u092f \u0926\u0938\u094d\u0924\u093e\u0935\u0947\u091c\u093c \u092a\u0939\u0932\u0947 \u0938\u0947 \u0939\u0940 \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0915\u0947 \u0938\u093e\u0925 \u091c\u0941\u0921\u093c\u093e \u0939\u0941\u0906 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DL_ALREADY_UPDATED) {
        return "\u0915\u094b\u0908 \u0915\u093e\u0930\u094d\u0930\u0935\u093e\u0908 \u0906\u0935\u0936\u094d\u092f\u0915 \u0928\u0939\u0940\u0902\u0964 \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0915\u093e \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u092a\u0939\u0932\u0947 \u0938\u0947 \u0939\u0940 \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0938\u0947 \u091c\u0941\u0921\u093c\u093e \u0939\u0941\u0906 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.RC_ALREADY_LINKED) {
        return "\u0935\u093e\u0939\u0928 RC \u0909\u092a\u0932\u092c\u094d\u0927 \u0928\u0939\u0940\u0902 \u0939\u0948\u0964 \u0905\u0928\u094d\u092f \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0938\u0947 \u091c\u0941\u0921\u093c\u093e";
    };
    if (stringKey instanceof Language_Types.RC_ALREADY_UPDATED) {
        return "\u0915\u094b\u0908 \u0915\u093e\u0930\u094d\u0930\u0935\u093e\u0908 \u0906\u0935\u0936\u094d\u092f\u0915 \u0928\u0939\u0940\u0902\u0964 \u0935\u093e\u0939\u0928 \u0906\u0930\u0938\u0940 \u092a\u0939\u0932\u0947 \u0938\u0947 \u0939\u0940 \u091a\u093e\u0932\u0915 \u0938\u0947 \u091c\u0941\u0921\u093c\u093e \u0939\u0941\u0906 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DL_ALREADY_LINKED) {
        return "\u091a\u093e\u0932\u0915 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0909\u092a\u0932\u092c\u094d\u0927 \u0928\u0939\u0940\u0902 \u0939\u0948\u0964 \u0905\u0928\u094d\u092f \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0938\u0947 \u091c\u0941\u0921\u093c\u093e";
    };
    if (stringKey instanceof Language_Types.SOMETHING_WENT_WRONG) {
        return "\u0915\u0941\u091b \u0917\u0932\u0924 \u0939\u094b \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.PICKUP) {
        return "\u0909\u0920\u093e\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.TRIP) {
        return "\u092f\u093e\u0924\u094d\u0930\u093e";
    };
    if (stringKey instanceof Language_Types.CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER) {
        return "\u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u092e\u0947\u0902, \u0939\u092e \u0915\u0947\u0935\u0932 \u0915\u0930\u094d\u0928\u093e\u091f\u0915 \u092a\u0902\u091c\u0940\u0915\u0943\u0924 \u0938\u0902\u0916\u094d\u092f\u093e \u0915\u0940 \u0905\u0928\u0941\u092e\u0924\u093f \u0926\u0947\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.UPDATED_AT) {
        return "\u0905\u092a\u0921\u0947\u091f \u0939\u0941\u0906";
    };
    if (stringKey instanceof Language_Types.DATE_OF_REGISTRATION) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u0915\u0940 \u0924\u093f\u0925\u093f";
    };
    if (stringKey instanceof Language_Types.SELECT_DATE_OF_ISSUE) {
        return "\u091c\u093e\u0930\u0940 \u0915\u0930\u0928\u0947 \u0915\u0940 \u0924\u093f\u0925\u093f \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DATE_OF_ISSUE) {
        return "\u091c\u093e\u0930\u0940 \u0915\u0930\u0928\u0947 \u0915\u0940 \u0924\u093f\u0925\u093f";
    };
    if (stringKey instanceof Language_Types.PROVIDE_DATE_OF_ISSUE_TEXT) {
        return "\u0915\u094d\u0937\u092e\u093e \u0915\u0930\u0947\u0902, \u0939\u092e \u0906\u092a\u0915\u0947 DL \u0915\u094b \u0938\u0924\u094d\u092f\u093e\u092a\u093f\u0924(verify) \u0928\u0939\u0940\u0902 \u0915\u0930 \u0938\u0915\u0947, \u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0928\u0947 DL \u0915\u094b \u0938\u0924\u094d\u092f\u093e\u092a\u093f\u0924(verify) \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f <b> \u091c\u093e\u0930\u0940 \u0915\u0930\u0928\u0947 \u0915\u0940 \u0924\u093f\u0925\u093f \u091a\u0941\u0928\u0947\u0902(Date of Issue) </b> \u092a\u094d\u0930\u0926\u093e\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PROVIDE_DATE_OF_REGISTRATION_TEXT) {
        return "\u0915\u094d\u0937\u092e\u093e \u0915\u0930\u0947\u0902, \u0939\u092e \u0906\u092a\u0915\u0947 RC \u0915\u094b \u0938\u0924\u094d\u092f\u093e\u092a\u093f\u0924(verify) \u0928\u0939\u0940\u0902 \u0915\u0930 \u0938\u0915\u0947, \u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0928\u0947 RC \u0915\u094b \u0938\u0924\u094d\u092f\u093e\u092a\u093f\u0924(verify) \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f <b> \u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u0915\u0940 \u0924\u093f\u0925\u093f(Date of Registration) </b> \u092a\u094d\u0930\u0926\u093e\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SELECT_DATE_OF_REGISTRATION) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u0915\u0940 \u0924\u093f\u0925\u093f \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.TRIP_COUNT) {
        return "\u0906\u091c \u0915\u0940 \u092f\u093e\u0924\u094d\u0930\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.TODAYS_EARNINGS) {
        return "\u0906\u091c \u0915\u0940 \u0915\u092e\u093e\u0908";
    };
    if (stringKey instanceof Language_Types.BONUS_EARNED) {
        return "\u092c\u094b\u0928\u0938 \u0915\u092e\u093e\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.GOT_IT) {
        return "\u0938\u092e\u091d \u0917\u092f\u093e!";
    };
    if (stringKey instanceof Language_Types.WHAT_IS_NAMMA_YATRI_BONUS) {
        return "\u092c\u094b\u0928\u0938 \u0915\u094d\u092f\u093e \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.BONUS_PRIMARY_TEXT) {
        return "\u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u092c\u094b\u0928\u0938 \u0935\u0939 \u0905\u0924\u093f\u0930\u093f\u0915\u094d\u0924 \u0930\u093e\u0936\u093f \u0939\u0948 \u091c\u094b \u0906\u092a\u0928\u0947 \u092a\u093f\u0915\u0905\u092a \u0936\u0941\u0932\u094d\u0915, \u0917\u094d\u0930\u093e\u0939\u0915 \u092c\u0916\u094d\u0936\u0940\u0936 \u0914\u0930 \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u091c\u094b\u0921\u093c\u0928\u0947 \u0915\u0947 \u0930\u0942\u092a \u092e\u0947\u0902 \u092e\u0940\u091f\u0930 \u0936\u0941\u0932\u094d\u0915 \u0915\u0947 \u090a\u092a\u0930 \u0905\u0930\u094d\u091c\u093f\u0924 \u0915\u0940 \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.BONUS_SECONDARY_TEXT) {
        return "\u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u092c\u094b\u0928\u0938 \u0930\u093e\u0936\u093f \u0906\u092a\u0915\u0940 \u0915\u0941\u0932 \u0915\u092e\u093e\u0908 \u0915\u093e \u0939\u093f\u0938\u094d\u0938\u093e \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.SAME_REENTERED_RC_MESSAGE) {
        return "\u0915\u0943\u092a\u092f\u093e \u0938\u0941\u0928\u093f\u0936\u094d\u091a\u093f\u0924 \u0915\u0930\u0947\u0902 \u0915\u093f \u092b\u093f\u0930 \u0938\u0947 \u0926\u0930\u094d\u091c \u0915\u093f\u092f\u093e \u0917\u092f\u093e \u0906\u0930\u0938\u0940 \u0928\u0902\u092c\u0930 \u090a\u092a\u0930 \u0926\u093f\u090f \u0917\u090f \u0906\u0930\u0938\u0940 \u0928\u0902\u092c\u0930 \u0915\u0947 \u0938\u092e\u093e\u0928 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.SAME_REENTERED_DL_MESSAGE) {
        return "\u092a\u0941\u0928\u0903 \u0926\u0930\u094d\u091c \u0915\u093f\u092f\u093e \u0917\u092f\u093e \u0921\u0940\u090f\u0932 \u0928\u0902\u092c\u0930 \u090a\u092a\u0930 \u0926\u093f\u090f \u0917\u090f \u0921\u0940\u090f\u0932 \u0928\u0902\u092c\u0930 \u0938\u0947 \u092e\u0947\u0932 \u0928\u0939\u0940\u0902 \u0916\u093e\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.WHERE_IS_MY_ISSUE_DATE) {
        return "\u091c\u093e\u0930\u0940 \u0915\u0930\u0928\u0947 \u0915\u0940 \u0924\u093f\u0925\u093f \u0915\u0939\u093e\u0901 \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.WHERE_IS_MY_REGISTRATION_DATE) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u0924\u093f\u0925\u093f \u0915\u0939\u093e\u0902 \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.EARNINGS_CREDITED_IN_ACCOUNT) {
        return "\u0906\u092a\u0915\u0940 \u0915\u092e\u093e\u0908 \u0907\u0938 \u0916\u093e\u0924\u0947 \u092e\u0947\u0902 \u091c\u092e\u093e \u0915\u0940 \u091c\u093e\u090f\u0917\u0940";
    };
    if (stringKey instanceof Language_Types.INVALID_PARAMETERS) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u092e\u093e\u092a\u0926\u0923\u094d\u0921";
    };
    if (stringKey instanceof Language_Types.UNAUTHORIZED) {
        return "\u0905\u0928\u0927\u093f\u0915\u0943\u0924";
    };
    if (stringKey instanceof Language_Types.INVALID_TOKEN) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u091f\u094b\u0915\u0928";
    };
    if (stringKey instanceof Language_Types.SOME_ERROR_OCCURED_IN_OFFERRIDE) {
        return "\u0911\u092b\u093c\u0930\u0930\u093e\u0907\u0921 \u092e\u0947\u0902 \u0915\u0941\u091b \u0924\u094d\u0930\u0941\u091f\u093f \u0939\u0941\u0908";
    };
    if (stringKey instanceof Language_Types.SELECT_VEHICLE_TYPE) {
        return "\u0935\u093e\u0939\u0928 \u0915\u093e \u092a\u094d\u0930\u0915\u093e\u0930 \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RIDE) {
        return "\u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.NO_LOCATION_UPDATE) {
        return "\u0915\u094b\u0908 \u0938\u094d\u0925\u093e\u0928 \u0905\u0926\u094d\u092f\u0924\u0928 \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.GOT_IT_TELL_US_MORE) {
        return "\u0938\u092e\u091d \u0917\u090f, \u0939\u092e\u0947\u0902 \u0914\u0930 \u092c\u0924\u093e\u090f\u0902?";
    };
    if (stringKey instanceof Language_Types.WRITE_A_COMMENT) {
        return "\u091f\u093f\u092a\u094d\u092a\u0923\u0940 \u0932\u093f\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.HOW_WAS_YOUR_RIDE_WITH) {
        return "\u0906\u092a\u0915\u0940 \u0930\u093e\u0907\u0921 \u0915\u0948\u0938\u0940 \u0930\u0939\u0940";
    };
    if (stringKey instanceof Language_Types.RUDE_BEHAVIOUR) {
        return "\u0905\u0936\u093f\u0937\u094d\u091f \u0935\u094d\u092f\u0935\u0939\u093e\u0930";
    };
    if (stringKey instanceof Language_Types.LONG_WAITING_TIME) {
        return "\u0932\u0902\u092c\u0947 \u0938\u092e\u092f \u0924\u0915 \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e";
    };
    if (stringKey instanceof Language_Types.DIDNT_COME_TO_PICUP_LOCATION) {
        return "\u092a\u093f\u0915\u0905\u092a \u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u0928\u0939\u0940\u0902 \u0906\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.HELP_US_WITH_YOUR_REASON) {
        return "\u0905\u092a\u0928\u0947 \u0915\u093e\u0930\u0923 \u0938\u0947 \u0939\u092e\u093e\u0930\u0940 \u092e\u0926\u0926 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.MAX_CHAR_LIMIT_REACHED) {
        return "\u0905\u0927\u093f\u0915\u0924\u092e \u0935\u0930\u094d\u0923 \u0938\u0940\u092e\u093e \u092a\u0942\u0930\u0940 \u0939\u0941\u0908,";
    };
    if (stringKey instanceof Language_Types.SHOW_ALL_OPTIONS) {
        return "\u0938\u092d\u0940 \u0935\u093f\u0915\u0932\u094d\u092a \u0926\u093f\u0916\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.UPDATE_REQUIRED) {
        return "\u0905\u0926\u094d\u092f\u0924\u0928 \u0906\u0935\u0936\u094d\u092f\u0915 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE) {
        return "\u0915\u0943\u092a\u092f\u093e \u0938\u0947\u0935\u093e \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0910\u092a \u0915\u094b \u0905\u092a\u0921\u0947\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NOT_NOW) {
        return "\u0905\u092d\u0940 \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.OF) {
        return "\u092e\u0947\u0902 \u0938\u0947";
    };
    if (stringKey instanceof Language_Types.DROP) {
        return "\u0921\u094d\u0930\u0949\u092a";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT) {
        return "\u0915\u0943\u092a\u092f\u093e \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SETTING_YOU_OFFLINE) {
        return "\u0939\u092e \u0906\u092a\u0915\u094b \u0911\u092b\u093c\u0932\u093e\u0907\u0928 \u0938\u0947\u091f \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.SETTING_YOU_ONLINE) {
        return "\u0939\u092e \u0906\u092a\u0915\u094b \u0911\u0928\u0932\u093e\u0907\u0928 \u0938\u0947\u091f \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.SETTING_YOU_SILENT) {
        return "\u0939\u092e \u0906\u092a\u0915\u094b \u0938\u093e\u0907\u0932\u0947\u0902\u091f \u0938\u0947\u091f \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.VIEW_BREAKDOWN) {
        return "\u092c\u094d\u0930\u0947\u0915\u0921\u093e\u0909\u0928 \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.APP_INFO) {
        return "\u0910\u092a \u0938\u0947\u091f\u093f\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.OTHER) {
        return "\u0905\u0928\u094d\u092f";
    };
    if (stringKey instanceof Language_Types.VEHICLE_ISSUE) {
        return "\u0935\u093e\u0939\u0928 \u0915\u093e \u092e\u093e\u092e\u0932\u093e";
    };
    if (stringKey instanceof Language_Types.FARE_UPDATED) {
        return "\u0915\u093f\u0930\u093e\u092f\u093e \u0905\u092a\u0921\u0947\u091f \u0915\u093f\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES) {
        return "\u092c\u093e\u0930-\u092c\u093e\u0930 \u0930\u0926\u094d\u0926 \u0915\u0930\u0928\u0947 \u0938\u0947 \u0938\u0935\u093e\u0930\u0940 \u0915\u092e \u0939\u094b\u0917\u0940 \u0914\u0930 \u0930\u0947\u091f\u093f\u0902\u0917 \u0915\u092e \u0939\u094b\u0917\u0940";
    };
    if (stringKey instanceof Language_Types.CONTINUE) {
        return "\u091c\u093e\u0930\u0940 \u0930\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CONFIRM_PASSWORD) {
        return "\u092a\u093e\u0938\u0935\u0930\u094d\u0921 \u0915\u0940 \u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0940\u091c\u093f\u092f\u0947";
    };
    if (stringKey instanceof Language_Types.DEMO_MODE) {
        return "\u0921\u0947\u092e\u094b \u092e\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.PASSWORD) {
        return "\u092a\u093e\u0938\u0935\u0930\u094d\u0921";
    };
    if (stringKey instanceof Language_Types.ENTER_DEMO_MODE_PASSWORD) {
        return "\u0921\u0947\u092e\u094b \u092e\u094b\u0921 \u092a\u093e\u0938\u0935\u0930\u094d\u0921 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DEMO_MODE_DISABLED) {
        return "\u0921\u0947\u092e\u094b \u092e\u094b\u0921 \u0905\u0915\u094d\u0937\u092e";
    };
    if (stringKey instanceof Language_Types.ONLINE_VIA_DEMO_MODE) {
        return "\u0911\u0928\u0932\u093e\u0907\u0928 (\u0921\u0947\u092e\u094b)";
    };
    if (stringKey instanceof Language_Types.MORE) {
        return "more";
    };
    if (stringKey instanceof Language_Types.LESS) {
        return "less";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_AT_PICKUP) {
        return "\u0906\u092a \u092a\u093f\u0915\u0905\u092a \u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.WAITING_FOR_CUSTOMER) {
        return "";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_NOTIFIED) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0905\u0927\u093f\u0938\u0942\u091a\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.I_ARRIVED) {
        return "\u092e\u0948\u0902 \u0906 \u0917\u092f\u093e \u0939\u0942\u0901";
    };
    if (stringKey instanceof Language_Types.ESTIMATED_RIDE_FARE) {
        return "\u0905\u0928\u0941\u092e\u093e\u0928\u093f\u0924 \u0938\u0935\u093e\u0930\u0940 \u0915\u093f\u0930\u093e\u092f\u093e: ";
    };
    if (stringKey instanceof Language_Types.PICKUP_TOO_FAR) {
        return "\u092a\u093f\u0915\u0905\u092a \u092c\u0939\u0941\u0924 \u0926\u0942\u0930 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_NOT_PICKING_CALL) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u0949\u0932 \u0928\u0939\u0940\u0902 \u0909\u0920\u093e \u0930\u0939\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.TRAFFIC_JAM) {
        return "\u091f\u094d\u0930\u0948\u092b\u093c\u093f\u0915 \u091c\u093e\u092e";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_WAS_RUDE) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u092c\u0926\u0924\u092e\u0940\u091c\u0940 \u0915\u0930 \u0930\u0939\u093e \u0925\u093e";
    };
    if (stringKey instanceof Language_Types.ALL_MESSAGES) {
        return "\u0938\u092d\u0940 \u0938\u0902\u0926\u0947\u0936";
    };
    if (stringKey instanceof Language_Types.MESSAGES) {
        return "\u0938\u0902\u0926\u0947\u0936\u0947";
    };
    if (stringKey instanceof Language_Types.ADD_A_COMMENT) {
        return "\u090f\u0915 \u091f\u093f\u092a\u094d\u092a\u0923\u0940 \u091c\u094b\u0921\u093c\u0928\u0947";
    };
    if (stringKey instanceof Language_Types.POST_COMMENT) {
        return "\u091f\u093f\u092a\u094d\u092a\u0923\u0940 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_YOUR_COMMENT) {
        return "\u0905\u092a\u0928\u0940 \u091f\u093f\u092a\u094d\u092a\u0923\u0940 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NO_NOTIFICATIONS_RIGHT_NOW) {
        return "\u0905\u092d\u0940 \u0915\u094b\u0908 \u0938\u0942\u091a\u0928\u093e \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.NO_NOTIFICATIONS_RIGHT_NOW_DESC) {
        return "\u0915\u094b\u0908 \u0928\u0908 \u0938\u0942\u091a\u0928\u093e \u0906\u0928\u0947 \u092a\u0930 \u0939\u092e \u0906\u092a\u0915\u094b \u0938\u0942\u091a\u093f\u0924 \u0915\u0930\u0947\u0902\u0917\u0947";
    };
    if (stringKey instanceof Language_Types.ALERTS) {
        return "\u0905\u0932\u0930\u094d\u091f\u0938";
    };
    if (stringKey instanceof Language_Types.YOUR_COMMENT) {
        return "\u0906\u092a\u0915\u0940 \u091f\u093f\u092a\u094d\u092a\u0923\u093f\u092f\u093e\u0902";
    };
    if (stringKey instanceof Language_Types.SHOW_MORE) {
        return "\u0905\u0927\u093f\u0915 \u0926\u093f\u0916\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.LOAD_OLDER_ALERTS) {
        return "\u0914\u0930 \u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CONTEST) {
        return "\u092a\u094d\u0930\u0924\u093f\u092f\u094b\u0917\u093f\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.YOUR_REFERRAL_CODE_IS_LINKED) {
        return "\u0906\u092a\u0915\u093e \u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921 \u0932\u093f\u0902\u0915 \u0939\u094b \u0917\u092f\u093e \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.YOU_CAN_NOW_EARN_REWARDS) {
        return "\u0905\u092c \u0906\u092a \u0917\u094d\u0930\u093e\u0939\u0915\u094b\u0902 \u0915\u094b \u0930\u0947\u092b\u093c\u0930 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u092a\u0941\u0930\u0938\u094d\u0915\u093e\u0930 \u0905\u0930\u094d\u091c\u093f\u0924 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902!";
    };
    if (stringKey instanceof Language_Types.COMING_SOON) {
        return "\u091c\u0932\u094d\u0926 \u0906 \u0930\u0939\u093e \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.COMING_SOON_DESCRIPTION) {
        return "\u0939\u092e \u0906\u092a\u0915\u094b \u0930\u0947\u092b\u093c\u0930\u0932 \u0915\u093e\u0930\u094d\u092f\u0915\u094d\u0930\u092e \u092e\u0947\u0902 \u0936\u093e\u092e\u093f\u0932 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0915\u093e\u092e \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902\u0964 \u0905\u0927\u093f\u0915 \u091c\u093e\u0928\u0915\u093e\u0930\u0940 \u0915\u0947 \u0932\u093f\u090f \u0905\u0932\u0930\u094d\u091f \u092a\u0947\u091c \u0926\u0947\u0916\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.REFERRAL_CODE_HINT) {
        return "6 \u0905\u0902\u0915\u094b\u0902 \u0915\u093e \u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CONFIRM_REFERRAL_CODE) {
        return "\u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921 \u0915\u0940 \u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CONFIRM_REFERRAL_CODE_HINT) {
        return "\u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921 \u092b\u093f\u0930 \u0938\u0947 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOUR_REFERRAL_CODE) {
        return "\u0906\u092a\u0915\u093e \u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.FIRST_REFERRAL_SUCCESSFUL) {
        return "\u092a\u0939\u0932\u093e \u0930\u0947\u092b\u093c\u0930\u0932 \u0938\u092b\u0932!\x0a\u0907\u0928\u093e\u092e \u0905\u0928\u0932\u0949\u0915 \u0915\u093f\u092f\u093e \u0917\u092f\u093e!";
    };
    if (stringKey instanceof Language_Types.AWAITING_REFERRAL_RIDE) {
        return "\u0930\u0947\u092b\u0930\u0932 \u0930\u093e\u0907\u0921 \u0915\u093e \u0907\u0902\u0924\u091c\u093e\u0930 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT) {
        return "\u091c\u092c \u0906\u092a \u0930\u0947\u092b\u093c\u0930\u0932 \u0905\u0932\u0930\u094d\u091f \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0947\u0902 \u0924\u094b \x0a\u0907\u0938 \u0938\u094d\u0925\u093e\u0928 \u0915\u094b \u091a\u0947\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.REFERRED_CUSTOMERS) {
        return "\u0930\u0947\u092b\u093c\u0930 \u0915\u093f\u090f \u0917\u090f \u0917\u094d\u0930\u093e\u0939\u0915";
    };
    if (stringKey instanceof Language_Types.ACTIVATED_CUSTOMERS) {
        return "\u0938\u0915\u094d\u0930\u093f\u092f \u0917\u094d\u0930\u093e\u0939\u0915";
    };
    if (stringKey instanceof Language_Types.REFERRAL_CODE_LINKING) {
        return "\u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921 \u0932\u093f\u0902\u0915\u093f\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.CONTACT_SUPPORT) {
        return "\u0938\u0902\u092a\u0930\u094d\u0915 \u0938\u0939\u093e\u092f\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.CALL_SUPPORT) {
        return "\u0938\u0939\u093e\u092f\u0924\u093e \u0915\u094b \u0915\u0949\u0932 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT) {
        return "\u0906\u092a \u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u0938\u092a\u094b\u0930\u094d\u091f \u091f\u0940\u092e \u0915\u094b \u0915\u0949\u0932 \u0915\u0930\u0928\u0947 \u0935\u093e\u0932\u0947 \u0939\u0948\u0902\u0964 \u0915\u094d\u092f\u093e \u0906\u092a \u0906\u0917\u0947 \u092c\u0922\u093c\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.REFERRAL_ENROLMENT) {
        return "\u0930\u0947\u092b\u0930\u0932 \u0928\u093e\u092e\u093e\u0902\u0915\u0928";
    };
    if (stringKey instanceof Language_Types.REFERRALS) {
        return "\u0930\u0947\u092b\u0930\u0932";
    };
    if (stringKey instanceof Language_Types.LINK_REFERRAL_CODE) {
        return "\u0932\u093f\u0902\u0915 \u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.DRIVER_DETAILS) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0915\u093e \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.FOR_UPDATES_SEE_ALERTS) {
        return "\u0905\u092a\u0921\u0947\u091f \u0915\u0947 \u0932\u093f\u090f, \u0905\u0932\u0930\u094d\u091f \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SHARE_OPTIONS) {
        return "\u0935\u093f\u0915\u0932\u094d\u092a \u0938\u093e\u091d\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_PASSWORD) {
        return "\u092a\u093e\u0938 \u0935\u0930\u094d\u0921 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOUR_VEHICLE) {
        return "\u0906\u092a\u0915\u093e \u0935\u093e\u0939\u0928";
    };
    if (stringKey instanceof Language_Types.BOOKING_OPTIONS) {
        return "\u092c\u0941\u0915\u093f\u0902\u0917 \u0935\u093f\u0915\u0932\u094d\u092a";
    };
    if (stringKey instanceof Language_Types.CONFIRM_AND_CHANGE) {
        return "\u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0930\u0947\u0902 \u0914\u0930 \u092c\u0926\u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RIDE_FARE) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0915\u093f\u0930\u093e\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.RENTAL_FARE) {
        return "\u0915\u093f\u0930\u093e\u092f\u0947 \u0915\u093f\u0930\u093e\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.RIDE_DISTANCE) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0915\u0940 \u0926\u0942\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.DURATION) {
        return "\u0905\u0935\u0927\u093f";
    };
    if (stringKey instanceof Language_Types.START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS) {
        return "\u0907\u0928 \u0924\u094d\u0935\u0930\u093f\u0924 \u091a\u0948\u091f \u0938\u0941\u091d\u093e\u0935\u094b\u0902 \u0915\u093e \u0909\u092a\u092f\u094b\u0917 \u0915\u0930\u0915\u0947 \u0905\u092a\u0928\u0940 \u091a\u0948\u091f \u092a\u094d\u0930\u093e\u0930\u0902\u092d \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.MESSAGE) {
        return "\u0938\u0902\u0926\u0947\u0936";
    };
    if (stringKey instanceof Language_Types.START_YOUR_CHAT_WITH_THE_DRIVER) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0915\u0947 \u0938\u093e\u0925 \u0905\u092a\u0928\u0940 \u091a\u0948\u091f \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.I_AM_ON_MY_WAY) {
        return "\u092e\u0948\u0902 \u0930\u093e\u0938\u094d\u0924\u0947 \u092e\u0947\u0902 \u0939\u0942\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.GETTING_DELAYED_PLEASE_WAIT) {
        return "\u0926\u0947\u0930\u0940 \u0939\u094b \u0930\u0939\u0940 \u0939\u0948\u0964 \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.UNREACHABLE_PLEASE_CALL_BACK) {
        return "\u092a\u0939\u0941\u0902\u091a \u092f\u094b\u0917\u094d\u092f \u0928\u0939\u0940\u0902 \u0939\u0948, \u0935\u093e\u092a\u0938 \u0915\u0949\u0932 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.ARE_YOU_STARING) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0906 \u0930\u0939\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.PLEASE_COME_SOON) {
        return "\u0915\u0943\u092a\u092f\u093e \u091c\u0932\u094d\u0926\u0940 \u0906 \u091c\u093e\u090f\u0901\u0964";
    };
    if (stringKey instanceof Language_Types.OK_I_WILL_WAIT) {
        return "\u092e\u0948\u0902 \u0907\u0902\u0924\u091c\u093c\u093e\u0930 \u0915\u0930\u0942\u0902\u0917\u093e\u0964";
    };
    if (stringKey instanceof Language_Types.I_HAVE_ARRIVED) {
        return "\u092e\u0948\u0902 \u092a\u0939\u0942\u0902\u091a \u0917\u092f\u093e \u0939\u0942\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.PLEASE_COME_FAST_I_AM_WAITING) {
        return "\u091c\u0932\u094d\u0926\u0940 \u0906\u0907\u090f, \u092e\u0948\u0902 \u0907\u0902\u0924\u091c\u093e\u0930 \u0915\u0930 \u0930\u0939\u093e \u0939\u0942\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT_I_WILL_BE_THERE) {
        return "\u0915\u0943\u092a\u092f\u093e \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0930\u0947\u0902, \u092e\u0948\u0902 \u0906 \u0930\u0939\u093e \u0939\u0942\u0901\u0964";
    };
    if (stringKey instanceof Language_Types.LOOKING_FOR_YOU_AT_PICKUP) {
        return "\u092e\u0948\u0902 \u092a\u093f\u0915\u0905\u092a \u092a\u0930 \u0939\u0942\u0901\u0964";
    };
    if (stringKey instanceof Language_Types.SILENT) {
        return "\u0938\u093e\u0907\u0932\u0947\u0902\u091f";
    };
    if (stringKey instanceof Language_Types.TRY_SILENT_MODE) {
        return "\u091f\u094d\u0930\u093e\u0907 \u0938\u093e\u0907\u0932\u0947\u0902\u091f \u092e\u094b\u0921?";
    };
    if (stringKey instanceof Language_Types.SILENT_MODE_PROMPT) {
        return "\u0905\u0917\u0930 \u0906\u092a \u092a\u0930\u0947\u0936\u093e\u0928 \u0928\u0939\u0940\u0902 \u0939\u094b\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902, \u0924\u094b \u0906\u092a \u0907\u0938\u0915\u0947 \u092c\u091c\u093e\u092f \u0938\u093e\u0907\u0932\u0947\u0902\u091f \u092e\u094b\u0921 \u092e\u0947\u0902 \u0938\u094d\u0935\u093f\u091a \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.GO_SILENT) {
        return "\u0938\u093e\u0907\u0932\u0947\u0902\u091f \u0939\u094b \u091c\u093e\u0913";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE) {
        return "\u0917\u094b!";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE_PROMPT) {
        return "\u0906\u092a \u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u092e\u0947\u0902 \u0911\u092b\u093c\u0932\u093e\u0907\u0928 \u0939\u0948\u0902\u0964\x0a\u0930\u093e\u0907\u0921 \u0905\u0928\u0941\u0930\u094b\u0927 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f, \u0905\u092d\u0940 \u0911\u0928\u0932\u093e\u0907\u0928 \u0939\u094b\u0902!";
    };
    if (stringKey instanceof Language_Types.LIVE_DASHBOARD) {
        return "\u0932\u093e\u0907\u0935 \u0906\u0901\u0915\u0921\u093c\u0947 \u0921\u0948\u0936\u092c\u094b\u0930\u094d\u0921";
    };
    if (stringKey instanceof Language_Types.CLICK_TO_ACCESS_YOUR_ACCOUNT) {
        return "\u0905\u092a\u0928\u0947 \u0916\u093e\u0924\u0947 \u0924\u0915 \u092a\u0939\u0941\u0902\u091a\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u092f\u0939\u093e\u0902 \u0915\u094d\u0932\u093f\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_ALTERNATE_NUMBER) {
        return "\u0905\u092a\u0928\u093e \u0926\u0942\u0938\u0930\u093e \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0910\u0921 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.ENTER_ALTERNATE_MOBILE_NUMBER) {
        return "\u0905\u092a\u0928\u093e \u0926\u0942\u0938\u0930\u093e \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u090f\u0902\u091f\u0930 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER) {
        return "\u0915\u0943\u092a\u092f\u093e 10 \u0928\u0902\u092c\u0930 \u0915\u093e \u0938\u0939\u0940 \u0928\u0902\u092c\u0930 \u090f\u0902\u091f\u0930 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_MOBILE_NUMBER) {
        return "\u0926\u0942\u0938\u0930\u093e \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.REMOVE) {
        return "\u0928\u093f\u0915\u093e\u0932\u0947";
    };
    if (stringKey instanceof Language_Types.REMOVE_ALTERNATE_NUMBER) {
        return "\u0926\u0942\u0938\u0930\u093e \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0939\u091f\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0905\u092a\u0928\u093e \u0926\u0942\u0938\u0930\u093e \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0928\u093f\u0915\u093e\u0932\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.YES_REMOVE_IT) {
        return "\u0939\u093e\u0901, \u0907\u0938\u0947 \u0928\u093f\u0915\u093e\u0932 \u0926\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NUMBER_REMOVED_SUCCESSFULLY) {
        return "\u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u0928\u093f\u0915\u093e\u0932 \u0926\u093f\u092f\u093e \u0917\u092f\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.EDIT_ALTERNATE_MOBILE_NUMBER) {
        return "\u0926\u0942\u0938\u0930\u0947 \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u092e\u0947\u0902 \u092c\u0926\u0932\u093e\u0935 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.NUMBER_ADDED_SUCCESSFULLY) {
        return "\u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u090f\u0921\u0947\u0921";
    };
    if (stringKey instanceof Language_Types.NUMBER_EDITED_SUCCESSFULLY) {
        return "\u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0905\u092a\u0921\u0947\u091f\u0947\u0921";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_MOBILE_OTP_LIMIT_EXCEED) {
        return "\u0913\u091f\u0940\u092a\u0940 \u0938\u0940\u092e\u093e \u092a\u093e\u0930 \u0939\u094b \u0917\u0908, \u0928\u0902\u092c\u0930 \u0914\u0930 \u0913\u091f\u0940\u092a\u0940 \u092b\u093f\u0930 \u0938\u0947 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.WRONG_OTP) {
        return "\u0915\u0943\u092a\u092f\u093e \u0938\u0939\u0940 \u0913\u091f\u0940\u092a\u0940 \u090f\u0902\u091f\u0930 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ATTEMPTS_LEFT) {
        return "3 \u0914\u0930 \u092a\u094d\u0930\u092f\u093e\u0938 \u092c\u093e\u0915\u0940 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.ATTEMPT_LEFT) {
        return "\u092a\u094d\u0930\u092f\u093e\u0938 \u092c\u093e\u0915\u0940 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.OTP_LIMIT_EXCEEDED) {
        return "\u0906\u092a\u0915\u0940 \u0913\u091f\u0940\u092a\u0940 \u0915\u0940 \u0938\u0940\u092e\u093e \u092a\u093e\u0930 \u0939\u094b \u0917\u0908 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.OTP_LIMIT_EXCEEDED_MESSAGE) {
        return "\u0906\u092a\u0928\u0947 \u0905\u092a\u0928\u0940 \u0913\u091f\u0940\u092a\u0940 \u0915\u0940 \u0938\u0940\u092e\u093e \u092a\u093e\u0930 \u0915\u0930 \u0932\u0940 \u0939\u0948\u0902\u0964 \u0915\u0943\u092a\u092f\u093e 10 \u092e\u093f\u0928\u091f \u092c\u093e\u0926 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.TRY_AGAIN_LATER) {
        return "\u092c\u093e\u0926 \u092e\u0947\u0902 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.NUMBER_ALREADY_EXIST_ERROR) {
        return "\u092f\u0939 \u0928\u0902\u092c\u0930 \u0915\u093f\u0938\u0940 \u0914\u0930 \u0905\u0915\u093e\u0909\u0902\u091f \u092e\u0947\u0902 \u0907\u0938\u094d\u0924\u0947\u092e\u093e\u0932 \u092e\u0947\u0902 \u0939\u0948 ! \u0915\u0943\u092a\u092f\u093e \u0926\u0942\u0938\u0930\u093e \u0928\u0902\u092c\u0930 \u0910\u0921 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.VERIFICATION_IS_TAKING_A_BIT_LONGER) {
        return "\u0910\u0938\u093e \u0932\u0917\u0924\u093e \u0939\u0948 \u0915\u093f \u0906\u092a\u0915\u093e \u0938\u0924\u094d\u092f\u093e\u092a\u0928 \u0905\u092a\u0947\u0915\u094d\u0937\u093e \u0938\u0947 \u0925\u094b\u0921\u093c\u093e \u0905\u0927\u093f\u0915 \u0938\u092e\u092f \u0932\u0947 \u0930\u0939\u093e \u0939\u0948\u0964\x0a\u0906\u092a \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0947 \u0932\u093f\u090f \u0938\u0939\u093e\u092f\u0924\u093e \u0938\u0947 \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.COMPLETE_ONBOARDING) {
        return "\u092a\u0942\u0930\u094d\u0923 \u0911\u0928\u092c\u094b\u0930\u094d\u0921\u093f\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.ADD_ALTERNATE_NUMBER_IN_MEANTIME) {
        return "\u0907\u0938 \u092a\u094d\u0930\u094b\u0938\u0947\u0938 \u0915\u094b \u092a\u0942\u0930\u093e \u0939\u094b\u0928\u0947 \u092e\u0947\u0902 2 \u0915\u093e\u092e\u0915\u093e\u091c\u0940 \u0926\u093f\u0928 \x0a \u0924\u0915 \u0932\u0917 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0907\u0938 \u092c\u0940\u091a, \u0906\u092a \x0a\u090f\u0915 \u0935\u0948\u0915\u0932\u094d\u092a\u093f\u0915 \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u091c\u094b\u0921\u093c \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS) {
        return "\u0907\u0938 \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0935\u093e\u0932\u093e \u0935\u094d\u092f\u0915\u094d\u0924\u093f \u092a\u0939\u0932\u0947 \u0938\u0947 \u092e\u094c\u091c\u0942\u0926 \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.OTP_) {
        return "OTP";
    };
    if (stringKey instanceof Language_Types.MAPS) {
        return "Maps";
    };
    if (stringKey instanceof Language_Types.DEMO) {
        return "DEMO";
    };
    if (stringKey instanceof Language_Types.PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP) {
        return "\u0915\u0943\u092a\u092f\u093e \u0917\u094d\u0930\u093e\u0939\u0915 \u0938\u0947 \u0913\u091f\u0940\u092a\u0940 \u0915\u0947 \u0932\u093f\u090f \u092a\u0942\u091b\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DELETE) {
        return "\u092e\u093f\u091f\u093e\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.VIEW) {
        return "\u0926\u0947\u0916\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.ISSUE_NO) {
        return "\u0928\u093f\u0930\u094d\u0917\u0924 \u0938\u0902\u0916\u094d\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.ADD_VOICE_NOTE) {
        return "\u0935\u0949\u0907\u0938 \u0928\u094b\u091f \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.VOICE_NOTE_ADDED) {
        return "\u0935\u0949\u092f\u0938 \u0928\u094b\u091f \u091c\u094b\u0921\u093c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.SUBMIT_ISSUE_DETAILS) {
        return "\u0938\u092e\u0938\u094d\u092f\u093e \u0935\u093f\u0935\u0930\u0923 \u092a\u094d\u0930\u0938\u094d\u0924\u0941\u0924 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.IMAGE_PREVIEW) {
        return "\u091b\u0935\u093f \u092a\u0942\u0930\u094d\u0935\u093e\u0935\u0932\u094b\u0915\u0928";
    };
    if (stringKey instanceof Language_Types.RIDE_REPORT_ISSUE) {
        return "\u0938\u092e\u0938\u094d\u092f\u093e \u0915\u0940 \u0930\u093f\u092a\u094b\u0930\u094d\u091f \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0930\u093e\u0907\u0921 \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADDED_IMAGES) {
        return "\u091c\u094b\u0921\u093c\u0947 \u0917\u090f \u091a\u093f\u0924\u094d\u0930";
    };
    if (stringKey instanceof Language_Types.NO_IMAGES_ADDED) {
        return "\u0915\u094b\u0908 \u091a\u093f\u0924\u094d\u0930 \u0928\u0939\u0940\u0902 \u091c\u094b\u0921\u093c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.ASK_DETAILS_MESSAGE) {
        return "\u0915\u0943\u092a\u092f\u093e \u0915\u0941\u091b \u0914\u0930 \u0935\u093f\u0935\u0930\u0923 \u0926\u0947\u0902\u0964 \u0906\u092a \u092c\u0947\u0939\u0924\u0930 \u0922\u0902\u0917 \u0938\u0947 \u0935\u093f\u0938\u094d\u0924\u0943\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u091a\u093f\u0924\u094d\u0930 \u092f\u093e \u0935\u0949\u0907\u0938 \u0928\u094b\u091f \u092d\u0940 \u092d\u0947\u091c \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.ASK_DETAILS_MESSAGE_REVERSED) {
        return "\u0915\u0943\u092a\u092f\u093e \u0916\u094b\u0908 \u0939\u0941\u0908 \u0935\u0938\u094d\u0924\u0941 \u0915\u0947 \u092c\u093e\u0930\u0947 \u092e\u0947\u0902 \u0905\u0927\u093f\u0915 \u091c\u093e\u0928\u0915\u093e\u0930\u0940 \u0938\u093e\u091d\u093e \u0915\u0930\u0947\u0902\u0964 \u0906\u092a \u092c\u0947\u0939\u0924\u0930 \u0922\u0902\u0917 \u0938\u0947 \u0935\u093f\u0938\u094d\u0924\u0943\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u091a\u093f\u0924\u094d\u0930 \u092f\u093e \u0935\u0949\u0907\u0938 \u0928\u094b\u091f \u092d\u0940 \u092d\u0947\u091c \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.SELECT_OPTION) {
        return "\u0915\u0943\u092a\u092f\u093e \u0939\u092e\u0947\u0902 \u092c\u0924\u093e\u090f\u0902 \u0915\u093f \u0915\u094d\u092f\u093e \u0906\u092a \u0907\u0928\u092e\u0947\u0902 \u0938\u0947 \u0915\u093f\u0938\u0940 \u0915\u093e \u0938\u093e\u092e\u0928\u093e \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.SELECT_OPTION_REVERSED) {
        return "\u0906\u092a \u0907\u0938 \u092e\u0941\u0926\u094d\u0926\u0947 \u0915\u094b \u0915\u0948\u0938\u0947 \u0938\u0941\u0932\u091d\u093e\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.ISSUE_SUBMITTED_MESSAGE) {
        return "\u0935\u093f\u0935\u0930\u0923 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0939\u0941\u0906! \u0906\u092a\u0915\u0940 \u0938\u092e\u0938\u094d\u092f\u093e \u0915\u093e \u0938\u092e\u093e\u0927\u093e\u0928 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0939\u092e\u093e\u0930\u0940 \u091f\u0940\u092e 24 \u0918\u0902\u091f\u0947 \u0915\u0947 \u092d\u0940\u0924\u0930 \u0906\u092a\u0915\u094b \u0915\u0949\u0932 \u0915\u0930\u0947\u0917\u0940\u0964";
    };
    if (stringKey instanceof Language_Types.I_DONT_KNOW_WHICH_RIDE) {
        return "\u092e\u0941\u091d\u0947 \u0928\u0939\u0940\u0902 \u092a\u0924\u093e \u0915\u093f \u0915\u094c\u0928 \u0938\u0940 \u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.REPORT_ISSUE_CHAT_PLACEHOLDER) {
        return "\u0905\u092a\u0928\u0947 \u092e\u093e\u092e\u0932\u0947 \u0915\u093e \u0935\u0930\u094d\u0923\u0928 \u0915\u0930\u0947\u0902\u0964 \u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 24 \u0918\u0902\u091f\u0947 \u0915\u0947 \u092d\u0940\u0924\u0930 \u0907\u0938\u0947 \u0939\u0932 \u0915\u0930\u0928\u0947 \u0915\u093e \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902\u0917\u0947\u0964";
    };
    if (stringKey instanceof Language_Types.ADDED_VOICE_NOTE) {
        return "\u091c\u094b\u0921\u093c\u093e \u0906\u0935\u093e\u091c \u0928\u094b\u091f";
    };
    if (stringKey instanceof Language_Types.NO_VOICE_NOTE_ADDED) {
        return "\u0915\u094b\u0908 \u0935\u0949\u092f\u0938 \u0928\u094b\u091f \u0928\u0939\u0940\u0902 \u091c\u094b\u0921\u093c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.CALL_CUSTOMER_TITLE) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u094b \u0915\u0949\u0932 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CALL_CUSTOMER_DESCRIPTION) {
        return "\u0906\u092a \u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u094b \u0915\u0949\u0932 \u0915\u0930\u0928\u0947 \u0935\u093e\u0932\u0947 \u0939\u0948\u0902\u0964 \u0915\u094d\u092f\u093e \u0906\u092a\u0915\u0940 \u0906\u0917\u0947 \u092c\u0922\u093c\u0928\u0947 \u0915\u0940 \u0907\u091a\u094d\u091b\u093e \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.PLACE_CALL) {
        return "\u0915\u0949\u0932 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_IMAGE) {
        return "\u091b\u0935\u093f \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_ANOTHER) {
        return "\u090f\u0915 \u0914\u0930 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.IMAGES_ADDED) {
        return "\u091b\u0935\u093f\u092f\u094b\u0902 \u0915\u094b \u091c\u094b\u0921\u093c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.ISSUE_SUBMITTED_TEXT) {
        return "\u091c\u093c\u0930\u093e \u0920\u0939\u0930\u094b! \u0939\u092e \u0906\u092a\u0915\u0940 \u0938\u092e\u0938\u094d\u092f\u093e \u0915\u094b \u0939\u0932 \u0915\u0930\u0928\u0947 \u092a\u0930 \u0915\u093e\u092e \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.CHOOSE_AN_OPTION) {
        return "\u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u090f\u0915 \u0935\u093f\u0915\u0932\u094d\u092a \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.IMAGE_ADDED) {
        return "\u091b\u0935\u093f \u091c\u094b\u0921\u093c\u0940 \u0917\u0908";
    };
    if (stringKey instanceof Language_Types.DONE) {
        return "\u092a\u0942\u0930\u094d\u0923";
    };
    if (stringKey instanceof Language_Types.RECORD_VOICE_NOTE) {
        return "\u0930\u093f\u0915\u0949\u0930\u094d\u0921 \u0906\u0935\u093e\u091c \u0928\u094b\u091f";
    };
    if (stringKey instanceof Language_Types.HELP_AND_SUPPORT) {
        return "\u092e\u0926\u0926 \u0938\u092e\u0930\u094d\u0925\u0928";
    };
    if (stringKey instanceof Language_Types.MORE_OPTIONS) {
        return "\u0905\u0927\u093f\u0915 \u0935\u093f\u0915\u0932\u094d\u092a";
    };
    if (stringKey instanceof Language_Types.ONGOING_ISSUES) {
        return "\u091a\u0932 \u0930\u0939\u0947 \u092e\u0941\u0926\u094d\u0926\u0947";
    };
    if (stringKey instanceof Language_Types.RESOLVED_ISSUES) {
        return "\u0938\u0941\u0932\u091d\u093e\u090f \u0917\u090f \u092e\u0941\u0926\u094d\u0926\u0947";
    };
    if (stringKey instanceof Language_Types.RESOLVED_ISSUE) {
        return "\u0939\u0932 \u0915\u093f\u092f\u093e \u0917\u092f\u093e \u092e\u0941\u0926\u094d\u0926\u093e";
    };
    if (stringKey instanceof Language_Types.ONGOING_ISSUE) {
        return "\u091a\u0932 \u0930\u0939\u0947 \u092e\u0941\u0926\u094d\u0926\u0947";
    };
    if (stringKey instanceof Language_Types.LOST_ITEM) {
        return "\u0916\u094b\u0908 \u0939\u0941\u0908 \u0935\u0938\u094d\u0924\u0941";
    };
    if (stringKey instanceof Language_Types.RIDE_RELATED_ISSUE) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0938\u0902\u092c\u0902\u0927\u0940 \u0938\u092e\u0938\u094d\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.APP_RELATED_ISSUE) {
        return "\u0910\u092a \u0938\u0902\u092c\u0902\u0927\u093f\u0924 \u0938\u092e\u0938\u094d\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.FARE_RELATED_ISSUE) {
        return "\u0915\u093f\u0930\u093e\u092f\u093e \u0938\u0902\u092c\u0902\u0927\u093f\u0924 \u092e\u0941\u0926\u094d\u0926\u093e";
    };
    if (stringKey instanceof Language_Types.ISSUE_NUMBER) {
        return "\u0928\u093f\u0930\u094d\u0917\u0924 \u0938\u0902\u0916\u094d\u092f\u093e  ";
    };
    if (stringKey instanceof Language_Types.REMOVE_ISSUE) {
        return "\u092e\u0941\u0926\u094d\u0926\u093e \u0939\u091f\u093e\u0913";
    };
    if (stringKey instanceof Language_Types.CALL_SUPPORT_NUMBER) {
        return "\u0938\u092e\u0930\u094d\u0925\u0928 \u0938\u0947 \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YEARS_AGO) {
        return " \u0938\u093e\u0932 \u092a\u0939\u0932\u0947";
    };
    if (stringKey instanceof Language_Types.MONTHS_AGO) {
        return " \u092e\u0939\u0940\u0928\u0947 \u092a\u0939\u0932\u0947";
    };
    if (stringKey instanceof Language_Types.DAYS_AGO) {
        return " \u0926\u093f\u0928 \u092a\u0939\u0932\u0947";
    };
    if (stringKey instanceof Language_Types.HOURS_AGO) {
        return " \u0918\u0902\u091f\u0947 \u092a\u0939\u0932\u0947";
    };
    if (stringKey instanceof Language_Types.MIN_AGO) {
        return " \u092e\u093f\u0928\u091f \u092a\u0939\u0932\u0947";
    };
    if (stringKey instanceof Language_Types.SEC_AGO) {
        return " \u0938\u0947\u0915\u0902\u0921 \u092a\u0939\u0932\u0947";
    };
    if (stringKey instanceof Language_Types.LOADING) {
        return "\u0932\u094b\u0921 \u0939\u094b \u0930\u0939\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.MAX_IMAGES) {
        return "\u0905\u0927\u093f\u0915\u0924\u092e 3 \u091a\u093f\u0924\u094d\u0930 \u0905\u092a\u0932\u094b\u0921 \u0915\u093f\u090f \u091c\u093e \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.APP_RELATED) {
        return "\u0910\u092a \u0938\u0902\u092c\u0902\u0927\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.FARE) {
        return "\u0915\u093f\u0930\u093e\u092f\u093e \u0938\u0902\u092c\u0902\u0927\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.RIDE_RELATED) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0938\u0902\u092c\u0902\u0927\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.LOST_AND_FOUND) {
        return "\u0932\u0949\u0938\u094d\u091f \u090f\u0902\u0921 \u092b\u093e\u0909\u0902\u0921 ";
    };
    if (stringKey instanceof Language_Types.REPORT_LOST_ITEM) {
        return "\u0916\u094b\u0908 \u0939\u0941\u0908 \u0935\u0938\u094d\u0924\u0941 \u0915\u0940 \u0938\u0942\u091a\u0928\u093e \u0926\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.MAKE_YOURSELF_AVAILABLE_FOR) {
        return "\u0928\u093f\u092e\u094d\u0928\u0932\u093f\u0916\u093f\u0924 \u0915\u0947 \u0932\u093f\u090f \u0938\u094d\u0935\u092f\u0902 \u0915\u094b \u0909\u092a\u0932\u092c\u094d\u0927 \u0915\u0930\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.SELECT_YOUR_GENDER) {
        return "\u0905\u092a\u0928\u093e \u0932\u093f\u0902\u0917 \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.FEMALE) {
        return "\u092e\u0939\u093f\u0932\u093e";
    };
    if (stringKey instanceof Language_Types.MALE) {
        return "\u092a\u0941\u0930\u0941\u0937";
    };
    if (stringKey instanceof Language_Types.PREFER_NOT_TO_SAY) {
        return "\u091a\u0941\u092a \u0930\u0939\u0928\u093e \u092a\u0938\u0902\u0926 \u0915\u0930\u0942\u0902\u0917\u093e";
    };
    if (stringKey instanceof Language_Types.GENDER) {
        return "\u0932\u093f\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.SET_NOW) {
        return "\u0905\u092d\u0940 \u0938\u0947\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES) {
        return "\u0905\u0927\u093f\u0915 \u0930\u093e\u0907\u0921 \u092a\u093e\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0905\u092a\u0928\u0940 \u092a\u094d\u0930\u094b\u092b\u093c\u093e\u0907\u0932 \u092a\u0942\u0930\u0940 \u0915\u0930\u0947\u0902!";
    };
    if (stringKey instanceof Language_Types.UPDATE_NOW) {
        return "\u0905\u092d\u0940 \u0905\u092a\u0921\u0947\u091f \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.CONFIRM) {
        return "\u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GENDER_UPDATED) {
        return "\u0932\u093f\u0902\u0917 \u0905\u0926\u094d\u092f\u0924\u0928";
    };
    if (stringKey instanceof Language_Types.CORPORATE_ADDRESS) {
        return "\u0915\u0949\u0930\u094d\u092a\u094b\u0930\u0947\u091f \u092a\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.CORPORATE_ADDRESS_DESCRIPTION) {
        return "\u091c\u0938\u092a\u0947 \u091f\u0947\u0915\u094d\u0928\u094b\u0932\u0949\u091c\u0940\u091c \u092a\u094d\u0930\u093e\u0907\u0935\u0947\u091f \u0932\u093f\u092e\u093f\u091f\u0947\u0921 <br> \u0917\u093f\u0930\u093f\u091c\u093e \u092c\u093f\u0932\u094d\u0921\u093f\u0902\u0917, \u0928\u0902\u092c\u0930 817, \u0917\u0923\u092a\u0924\u093f \u092e\u0902\u0926\u093f\u0930 \u0930\u094b\u0921, 8\u0935\u093e\u0902 \u092c\u094d\u0932\u0949\u0915, \u0915\u094b\u0930\u092e\u0902\u0917\u0932\u093e, \u092c\u0947\u0902\u0917\u0932\u0941\u0930\u0941, \u0915\u0930\u094d\u0928\u093e\u091f\u0915 560095, \u092d\u093e\u0930\u0924\u0964";
    };
    if (stringKey instanceof Language_Types.CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL) {
        return "\u0935\u0947\u092c\u0938\u093e\u0907\u091f : <u>https://nammayatri.in/</u>";
    };
    if (stringKey instanceof Language_Types.REGISTERED_ADDRESS) {
        return "\u092a\u0902\u091c\u0940\u0915\u0943\u0924 \u092a\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.REGISTERED_ADDRESS_DESCRIPTION) {
        return "\u091c\u0938\u094d\u092a\u0947 \u091f\u0947\u0915\u094d\u0928\u094b\u0932\u0949\u091c\u0940\u091c \u092a\u094d\u0930\u093e\u0907\u0935\u0947\u091f \u0932\u093f\u092e\u093f\u091f\u0947\u0921 <br> \u0938\u094d\u091f\u0948\u0932\u093f\u092f\u0928 \u092c\u093f\u091c\u0928\u0947\u0938 \u0938\u0947\u0902\u091f\u0930, \u0928\u0902\u092c\u0930 444, \u0924\u0940\u0938\u0930\u0940 \u0914\u0930 \u091a\u094c\u0925\u0940 \u092e\u0902\u091c\u093f\u0932, 18\u0935\u0940\u0902 \u092e\u0947\u0928, 6\u0935\u093e\u0902 \u092c\u094d\u0932\u0949\u0915, \u0915\u094b\u0930\u092e\u0902\u0917\u0932\u093e \u092c\u0947\u0902\u0917\u0932\u0941\u0930\u0941, \u0915\u0930\u094d\u0928\u093e\u091f\u0915- 560095, \u092d\u093e\u0930\u0924\u0964";
    };
    if (stringKey instanceof Language_Types.REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL) {
        return "\u0935\u0947\u092c\u0938\u093e\u0907\u091f : <u>https://nammayatri.in/</u>";
    };
    if (stringKey instanceof Language_Types.ZONE_CANCEL_TEXT_DROP) {
        return "\u0906\u092a\u0915\u093e \u0917\u094d\u0930\u093e\u0939\u0915 \u0936\u093e\u092f\u0926 \u0938\u092e\u092f \u092a\u0930 \u092e\u0947\u091f\u094d\u0930\u094b \u0938\u094d\u091f\u0947\u0936\u0928 \u092a\u0939\u0941\u0902\u091a\u0928\u0947 \u0915\u0940 \u0939\u0921\u093c\u092c\u0921\u093c\u0940 \u092e\u0947\u0902 \u0939\u0948! \x0a \u0939\u092e \u0906\u092a\u0938\u0947 \u0905\u0928\u0941\u0930\u094b\u0927 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u093f \u0930\u0926\u094d\u0926 \u0928 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.ZONE_CANCEL_TEXT_PICKUP) {
        return "\u0906\u092a\u0915\u093e \u0917\u094d\u0930\u093e\u0939\u0915 \u0936\u093e\u092f\u0926 \u0905\u092a\u0928\u0947 \u0917\u0902\u0924\u0935\u094d\u092f \u0924\u0915 \u092a\u0939\u0941\u0902\u091a\u0928\u0947 \u0915\u0940 \u0939\u0921\u093c\u092c\u0921\u093c\u0940 \u092e\u0947\u0902 \u0939\u0948\u0964 \x0a \u0939\u092e \u0906\u092a\u0938\u0947 \u0905\u0928\u0941\u0930\u094b\u0927 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u093f \u0930\u0926\u094d\u0926 \u0928 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.RANKINGS) {
        return "\u0930\u0948\u0902\u0915\u093f\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.GETTING_THE_LEADERBOARD_READY) {
        return "\u0932\u0940\u0921\u0930\u092c\u094b\u0930\u094d\u0921 \u0924\u0948\u092f\u093e\u0930 \u0915\u093f\u092f\u093e \u091c\u093e \u0930\u0939\u093e \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT_WHILE_WE_UPDATE_THE_DETAILS) {
        return "\u091c\u092c \u0924\u0915 \u0939\u092e \u0935\u093f\u0935\u0930\u0923 \u0905\u092a\u0921\u0947\u091f \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u0943\u092a\u092f\u093e \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LAST_UPDATED) {
        return "\u0906\u0916\u0930\u0940 \u0905\u092a\u0921\u0947\u091f: ";
    };
    if (stringKey instanceof Language_Types.CONGRATULATIONS_YOU_ARE_RANK) {
        return "\u092c\u0927\u093e\u0908 \u0939\u094b ! \u0906\u092a\u0915\u0940 \u0930\u0948\u0902\u0915 \u0939\u0948 ";
    };
    if (stringKey instanceof Language_Types.YOU) {
        return " (\u0906\u092a)";
    };
    if (stringKey instanceof Language_Types.DAILY) {
        return "\u0926\u0948\u0928\u093f\u0915";
    };
    if (stringKey instanceof Language_Types.WEEKLY) {
        return "\u0938\u093e\u092a\u094d\u0924\u093e\u0939\u093f\u0915";
    };
    if (stringKey instanceof Language_Types.ACCEPT_RIDES_TO_ENTER_RANKINGS) {
        return "\u0930\u0948\u0902\u0915\u093f\u0902\u0917 \u092e\u0947\u0902 \u092a\u094d\u0930\u0935\u0947\u0936 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0938\u0935\u093e\u0930\u0940 \u0938\u094d\u0935\u0940\u0915\u093e\u0930 \u0915\u0930\u0947\u0902!";
    };
    if (stringKey instanceof Language_Types.OTP_HAS_BEEN_RESENT) {
        return "OTP \u092b\u093f\u0930 \u0938\u0947 \u092d\u0947\u091c \u0926\u093f\u092f\u093e \u0917\u092f\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_RESENDING_OTP) {
        return "OTP \u0921\u093e\u0932\u0928\u0947 \u0915\u0940 \u0938\u0940\u092e\u093e \u0916\u0924\u094d\u092e \u0939\u094b \u0917\u0908 \u0939\u0948, \u0915\u0943\u092a\u092f\u093e OTP \u0926\u094b\u092c\u093e\u0930\u093e \u092d\u0947\u091c\u0928\u0947 \u0915\u0940 \u0915\u094b\u0936\u093f\u0936 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER) {
        return "OTP \u092a\u0941\u0928\u0903 \u092d\u0947\u091c\u0928\u0947 \u0915\u0940 \u0938\u0940\u092e\u093e \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b \u0917\u0908 \u0939\u0948, \u0915\u0943\u092a\u092f\u093e \u092c\u093e\u0926 \u092e\u0947\u0902 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN) {
        return "\u0913\u091f\u0940\u092a\u0940 \u092a\u0947\u091c \u0915\u0940 \u0938\u092e\u092f \u0938\u0940\u092e\u093e \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b \u0917\u0908 \u0939\u0948, \u0915\u0943\u092a\u092f\u093e \u0913\u091f\u0940\u092a\u0940 \u0915\u0947 \u0932\u093f\u090f \u092b\u093f\u0930 \u0938\u0947 \u0905\u0928\u0941\u0930\u094b\u0927 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN) {
        return "\u0915\u0941\u091b \u0924\u0915\u0928\u0940\u0915\u0940 \u0938\u092e\u0938\u094d\u092f\u093e \u0939\u0941\u0908 \u0939\u0948, \u0915\u0943\u092a\u092f\u093e \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.INVALID_REFERRAL_CODE) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.ISSUE_REMOVED_SUCCESSFULLY) {
        return "\u0938\u092e\u0938\u094d\u092f\u093e \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u0939\u091f\u093e \u0926\u0940 \u0917\u0908";
    };
    if (stringKey instanceof Language_Types.OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER) {
        return "OTP \u0921\u093e\u0932\u0928\u0947 \u0915\u0940 \u0938\u0940\u092e\u093e \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b \u0917\u0908 \u0939\u0948, \u0915\u0943\u092a\u092f\u093e \u092c\u093e\u0926 \u092e\u0947\u0902 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER) {
        return "\u0906\u092a\u0928\u0947 \u092c\u0939\u0941\u0924 \u0938\u0947 \u0905\u092e\u093e\u0928\u094d\u092f \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u093f\u090f \u0939\u0948\u0902, \u0915\u0943\u092a\u092f\u093e \u092c\u093e\u0926 \u092e\u0947\u0902 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.INVALID_REFERRAL_NUMBER) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u0930\u0947\u092b\u093c\u0930\u0932 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER) {
        return "\u0915\u0941\u091b \u0924\u0915\u0928\u0940\u0915\u0940 \u0938\u092e\u0938\u094d\u092f\u093e \u0939\u0941\u0908 \u0939\u0948, \u0915\u0943\u092a\u092f\u093e \u092c\u093e\u0926 \u092e\u0947\u0902 \u0926\u094b\u092c\u093e\u0930\u093e \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.WAIT_TIME) {
        return "\u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0938\u092e\u092f";
    };
    if (stringKey instanceof Language_Types.WAIT_TIMER) {
        return "\u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u091f\u093e\u0907\u092e\u0930";
    };
    if (stringKey instanceof Language_Types.HOW_LONG_WAITED_FOR_PICKUP) {
        return "\u0906\u092a\u0915\u094b \u0926\u093f\u0916\u093e\u0924\u093e \u0939\u0948 \u0915\u093f \u0906\u092a\u0928\u0947 \u092a\u093f\u0915\u0905\u092a \u092a\u0930 \u0915\u093f\u0924\u0928\u0940 \u0926\u0947\u0930 \u0924\u0915 \u0907\u0902\u0924\u091c\u093e\u0930 \u0915\u093f\u092f\u093e \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_WILL_PAY_FOR_EVERY_MINUTE) {
        return "\u0930\u093e\u0939\u0915 \u0915\u094b \u092a\u0939\u0932\u0947 {} \u092e\u093f\u0928\u091f \u0915\u0947 \u092c\u093e\u0926 \u092a\u094d\u0930\u0924\u094d\u092f\u0947\u0915 \u092e\u093f\u0928\u091f \u0915\u0947 \u0907\u0902\u0924\u091c\u093e\u0930 \u0915\u0947 \u0932\u093f\u090f \u20b91.5 \u0915\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0928\u093e \u0939\u094b\u0917\u093e\u0964";
    };
    if (stringKey instanceof Language_Types.OTHERS) {
        return "\u0905\u0928\u094d\u092f";
    };
    if (stringKey instanceof Language_Types.ENTER_SECOND_SIM_NUMBER) {
        return "\u0926\u0942\u0938\u0930\u093e \u0938\u093f\u092e \u0928\u0902\u092c\u0930 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_NUMBER) {
        return "\u0935\u0948\u0915\u0932\u094d\u092a\u093f\u0915 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.SELECT_THE_LANGUAGES_YOU_CAN_SPEAK) {
        return "\u0935\u0947 \u092d\u093e\u0937\u093e\u090f\u0901 \u091a\u0941\u0928\u0947\u0902 \u091c\u093f\u0928\u094d\u0939\u0947\u0902 \u0906\u092a \u092c\u094b\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER) {
        return "\u0915\u0943\u092a\u092f\u093e \u0915\u0941\u091b \u0938\u092e\u092f \u092c\u093e\u0926 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_NUMBER_CANNOT_BE_ADDED) {
        return "\u0926\u0942\u0938\u0930\u093e \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0910\u0921 \u0928\u0939\u0940\u0902 \u0939\u094b \u0938\u0915\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.OTP_RESENT) {
        return "\u0913\u091f\u0940\u092a\u0940 \u092b\u093f\u0930 \u0938\u0947 \u092d\u0947\u091c\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.OTP_RESEND_LIMIT_EXCEEDED) {
        return "OTP \u092a\u0941\u0928\u0903 \u092d\u0947\u091c\u0928\u0947 \u0915\u0940 \u0938\u0940\u092e\u093e \u092a\u093e\u0930 \u0939\u094b \u0917\u0908";
    };
    if (stringKey instanceof Language_Types.SEDAN) {
        return "\u0938\u0947\u0921\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.SUV) {
        return "\u090f\u0938\u092f\u0942\u0935\u0940";
    };
    if (stringKey instanceof Language_Types.HATCHBACK) {
        return "\u0939\u0948\u091a\u092c\u0948\u0915";
    };
    if (stringKey instanceof Language_Types.AUTO_RICKSHAW) {
        return "\u0911\u091f\u094b \u0930\u093f\u0915\u094d\u0936\u093e";
    };
    if (stringKey instanceof Language_Types.TAXI) {
        return "\u0928\u0949\u0928-\u090f\u0938\u0940 \u091f\u0948\u0915\u094d\u0938\u0940";
    };
    if (stringKey instanceof Language_Types.TAXI_PLUS) {
        return "\u090f\u0938\u0940 \u091f\u0948\u0915\u094d\u0938\u0940";
    };
    if (stringKey instanceof Language_Types.MY_PROFILE) {
        return "\u092e\u0947\u0930\u0940 \u092a\u094d\u0930\u094b\u092b\u093c\u093e\u0907\u0932";
    };
    if (stringKey instanceof Language_Types.SETTINGS) {
        return "\u0938\u0947\u091f\u093f\u0902\u0917\u094d\u0938";
    };
    if (stringKey instanceof Language_Types.REG_NUMBER) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u0938\u0902\u0916\u094d\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.TYPE) {
        return "\u092a\u094d\u0930\u0915\u093e\u0930";
    };
    if (stringKey instanceof Language_Types.MODEL_NAME) {
        return "\u092e\u0949\u0921\u0932 \u0915\u093e \u0928\u093e\u092e";
    };
    if (stringKey instanceof Language_Types.COLOUR) {
        return "\u0930\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.BADGES) {
        return "\u092c\u0948\u091c";
    };
    if (stringKey instanceof Language_Types.CALL_CUSTOMER_SUPPORT) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u094b \u0915\u0949\u0932 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.EDIT_RC) {
        return "\u0906\u0930\u0938\u0940 \u0938\u0902\u092a\u093e\u0926\u093f\u0924 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DELETE_RC) {
        return "\u0906\u0930\u0938\u0940 \u0939\u091f\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.DEACTIVATE_RC) {
        return "\u0906\u0930\u0938\u0940 \u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ACTIVATE_RC) {
        return "\u0906\u0930\u0938\u0940 \u0938\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ACTIVE_RC_ON_ANOTHER_DRIVER) {
        return " \u0915\u093f\u0938\u0940 \u0905\u0928\u094d\u092f \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0916\u093e\u0924\u0947 \u092a\u0930 \u0938\u0915\u094d\u0930\u093f\u092f \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.CALL_DRIVER_OR_CONTACT_SUPPORT) {
        return "\u092f\u0926\u093f \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0909\u092a\u0932\u092c\u094d\u0927 \u0928\u0939\u0940\u0902 \u0939\u0948 \u0924\u094b \u0906\u0930\u0938\u0940 \u0915\u094b \u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0915\u094b \u0915\u0949\u0932 \u0915\u0930\u0947\u0902 \u092f\u093e \u0938\u0939\u093e\u092f\u0924\u093e \u0938\u0947 \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CALL_DRIVER) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0915\u094b \u092c\u0941\u0932\u093e\u0913";
    };
    if (stringKey instanceof Language_Types.SKIP) {
        return "\u091b\u094b\u0921\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.ACTIVE_STR) {
        return "\u0938\u0915\u094d\u0930\u093f\u092f";
    };
    if (stringKey instanceof Language_Types.INACTIVE_RC) {
        return "\u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f";
    };
    if (stringKey instanceof Language_Types.CONFIRMATION_FOR_DELETING_RC) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0935\u093e\u0915\u0908 \u0905\u092a\u0928\u0940 \u0906\u0930\u0938\u0940 \u0939\u091f\u093e\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.CONFIRMATION_FOR_DEACTIVATING_RC) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0935\u093e\u0915\u0908 \u0905\u092a\u0928\u0940 \u0906\u0930\u0938\u0940 \u0915\u094b \u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.CONFIRMATION_FOR_ACTIVATING_RC) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0935\u093e\u0915\u0908 \u0906\u0930\u0938\u0940 \u0915\u094b \u0938\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.YES_DELETE) {
        return "\u0939\u093e\u0901, \u0939\u091f\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.ADD_NEW_RC) {
        return "\u0906\u0930\u0938\u0940 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CONNECT_CALL_ANONYMOUSLY) {
        return "\u0939\u092e \u0906\u092a\u0915\u094b \u0917\u0941\u092e\u0928\u093e\u092e \u0930\u0942\u092a \u0938\u0947 \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0938\u0947 \u091c\u094b\u0921\u093c\u0928\u0947 \u0915\u093e \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902\u0917\u0947, \u0915\u0943\u092a\u092f\u093e \u0906\u0930\u0938\u0940 \u0915\u094b \u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0928\u0947 \u0915\u093e \u0905\u0928\u0941\u0930\u094b\u0927 \u0915\u0930\u0928\u0947 \u0938\u0947 \u092a\u0939\u0932\u0947 \u0905\u092a\u0928\u093e \u092a\u0930\u093f\u091a\u092f \u0926\u0947\u0902.";
    };
    if (stringKey instanceof Language_Types.YES_ACTIVATE) {
        return "\u0939\u093e\u0901, \u0938\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YES_DEACTIVATE) {
        return "\u0939\u093e\u0901, \u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.THIS_WILL_DEACTIVATE_CURRENTLY_ACTIVE_RC) {
        return "\u092f\u0939 \u0906\u092a\u0915\u0940 \u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u092e\u0947\u0902 \u0938\u0915\u094d\u0930\u093f\u092f \u0906\u0930\u0938\u0940 \u0915\u094b \u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f \u0915\u0930 \u0926\u0947\u0917\u093e.";
    };
    if (stringKey instanceof Language_Types.REMOVED) {
        return "\u0928\u093f\u0915\u093e\u0932\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.IS_ACTIVE_NOW) {
        return " \u0905\u092d\u0940 \u0938\u0915\u094d\u0930\u093f\u092f \u0939\u0948.";
    };
    if (stringKey instanceof Language_Types.DEACTIVATED) {
        return "\u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f";
    };
    if (stringKey instanceof Language_Types.LANGUAGE_UPDATED) {
        return "\u092d\u093e\u0937\u093e \u0905\u0926\u094d\u092f\u0924\u0928";
    };
    if (stringKey instanceof Language_Types.SINGLE_RC_CANNOT_BE_DELETED) {
        return "\u090f\u0915\u0932 \u0906\u0930\u0938\u0940 \u0915\u094b \u0939\u091f\u093e\u092f\u093e \u0928\u0939\u0940\u0902 \u091c\u093e \u0938\u0915\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.CANCELLATION_RATE) {
        return "\u0930\u0926\u094d\u0926\u0940\u0915\u0930\u0923 \u0926\u0930";
    };
    if (stringKey instanceof Language_Types.RIDES_CANCELLED) {
        return "\u0930\u0926\u094d\u0926 \u0915\u0940 \u0917\u0908 \u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.EARNINGS_MISSED) {
        return "\u091b\u0942\u091f\u0940 \u0939\u0941\u0908 \u0915\u092e\u093e\u0908";
    };
    if (stringKey instanceof Language_Types.SUMMARY) {
        return "\u0938\u093e\u0930\u093e\u0902\u0936";
    };
    if (stringKey instanceof Language_Types.NAMMA_BONUS) {
        return "\u0928\u092e\u094d\u092e\u093e \u092c\u094b\u0928\u0938";
    };
    if (stringKey instanceof Language_Types.TRIPS_COMPLETED) {
        return "\u092a\u0942\u0930\u094d\u0923 \u092f\u093e\u0924\u094d\u0930\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.LATE_NIGHT_TRIPS) {
        return "\u0926\u0947\u0930 \u0930\u093e\u0924 \u0915\u0940 \u092f\u093e\u0924\u094d\u0930\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.ABOUT_ME) {
        return "\u092e\u0947\u0930\u0947 \u092c\u093e\u0930\u0947 \u092e\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ABOUT_VEHICLE) {
        return "\u0935\u093e\u0939\u0928 \u0915\u0947 \u092c\u093e\u0930\u0947 \u092e\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD) {
        return "\u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YEARS_OLD) {
        return "\u0938\u093e\u0932 \u092a\u0941\u0930\u093e\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.HOMETOWN) {
        return "\u0917\u0943\u0939\u0928\u0917\u0930";
    };
    if (stringKey instanceof Language_Types.MISSED_OPPORTUNITY) {
        return "\u091b\u0942\u091f\u0947 \u0939\u0941\u090f \u0905\u0935\u0938\u0930";
    };
    if (stringKey instanceof Language_Types.HOW_OLD_IS_YOUR_VEHICLE) {
        return "\u0906\u092a\u0915\u093e \u0935\u093e\u0939\u0928 \u0915\u093f\u0924\u0928\u093e \u092a\u0941\u0930\u093e\u0928\u093e \u0939\u0948 (\u0935\u0930\u094d\u0937\u094b\u0902 \u092e\u0947\u0902)?";
    };
    if (stringKey instanceof Language_Types.ENTER_NAME_OF_VEHICLE) {
        return "\u0905\u092a\u0928\u0947 \u0935\u093e\u0939\u0928 \u0915\u093e \u0928\u093e\u092e \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NEW_) {
        return "\u0928\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.TODAY) {
        return "\u0906\u091c";
    };
    if (stringKey instanceof Language_Types.TOTAL_MONEY_COLLECTED) {
        return "\u0915\u0941\u0932 \u090f\u0915\u0924\u094d\u0930\u093f\u0924 \u0927\u0928";
    };
    if (stringKey instanceof Language_Types.FARE_EARNED_OF_THE_DAY) {
        return "\u0926\u093f\u0928 \u0915\u0947 \u0932\u093f\u090f \u0905\u0930\u094d\u091c\u093f\u0924 \u0915\u093f\u0930\u093e\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.GST_PLUS_PAYABLE) {
        return "GST + \u0926\u0947\u092f \u0936\u0941\u0932\u094d\u0915";
    };
    if (stringKey instanceof Language_Types.TO_CONTINUE_USING_YATRI_SATHI) {
        return "\u092f\u093e\u0924\u094d\u0930\u0940 \u0938\u093e\u0925\u0940 \u0915\u093e \u0909\u092a\u092f\u094b\u0917 \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f, \u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0928\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u0942\u0930\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PAY) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LATER) {
        return "\u092c\u093e\u0926 \u092e\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GREAT_JOB) {
        return "\u0905\u091a\u094d\u091b\u093e \u0915\u093e\u092e!";
    };
    if (stringKey instanceof Language_Types.FEE_BREAKUP) {
        return "\u0936\u0941\u0932\u094d\u0915 \u0935\u093f\u091a\u094d\u091b\u0947\u0926";
    };
    if (stringKey instanceof Language_Types.YATRI_SATHI_FEE_PAYABLE_FOR_DATE) {
        return "\u092f\u093e\u0924\u094d\u0930\u0940 \u0938\u093e\u0925\u0940 \u0936\u0941\u0932\u094d\u0915 \u0932\u093e\u0917\u0942";
    };
    if (stringKey instanceof Language_Types.FEE_CORRESPONDING_TO_THE_DISTANCE) {
        return "\u092a\u094d\u0930\u0924\u093f \u092f\u093e\u0924\u094d\u0930\u093e \u0924\u092f \u0915\u0940 \u0917\u0908 \u0926\u0942\u0930\u0940 \u0915\u0947 \u0905\u0928\u0941\u0930\u0942\u092a \u0936\u0941\u0932\u094d\u0915 \u0915\u0940 \u0917\u0923\u0928\u093e \u0928\u0940\u091a\u0947 \u0926\u093f\u0916\u093e\u090f \u0905\u0928\u0941\u0938\u093e\u0930 \u0915\u0940 \u091c\u093e\u090f\u0917\u0940\u0964";
    };
    if (stringKey instanceof Language_Types.PLATFORM_FEE) {
        return "\u092a\u094d\u0932\u0947\u091f\u092b\u093e\u0930\u094d\u092e \u0936\u0941\u0932\u094d\u0915";
    };
    if (stringKey instanceof Language_Types.GST) {
        return "GST";
    };
    if (stringKey instanceof Language_Types.TOTAL_PAYABLE) {
        return "\u0915\u0941\u0932 \u0926\u0947\u092f";
    };
    if (stringKey instanceof Language_Types.VIEW_DETAILS) {
        return "\u0935\u093f\u0935\u0930\u0923 \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PAYMENT_SUCCESSFUL) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0938\u092b\u0932!";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0932\u0902\u092c\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.PAYMENT_FAILED) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0935\u093f\u092b\u0932 \u0930\u0939\u0940";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING_DESC) {
        return "\u091a\u093f\u0902\u0924\u093e \u0928 \u0915\u0930\u0947\u0902, \u0906\u092a \u0905\u092d\u0940 \u092d\u0940 \u0926\u093f\u0928 \u092d\u0930 \u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u093e \u091c\u093e\u0930\u0940 \u0930\u0916 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902!";
    };
    if (stringKey instanceof Language_Types.PAYMENT_FAILED_DESC) {
        return "\u0906\u092a \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u093e \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902, \u092f\u093e \u0905\u092a\u0928\u0947 \u0928\u093f\u0915\u091f\u0924\u092e \u092f\u093e\u0924\u094d\u0930\u0940 \u0938\u093e\u0925\u0940 \u092c\u0942\u0925 \u092a\u0930 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.WE_WILL_NOTIFY_WHEN_PAYMENT_SUCCESS) {
        return "\u0906\u092a\u0915\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u0938\u092b\u0932 \u0939\u094b\u0928\u0947 \u092a\u0930 \u0939\u092e \u0938\u0942\u091a\u093f\u0924 \u0915\u0930\u0947\u0902\u0917\u0947";
    };
    if (stringKey instanceof Language_Types.CONTINUE_TAKING_RIDES) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u093e \u091c\u093e\u0930\u0940 \u0930\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOUR_PREVIOUS_PAYMENT_IS_PENDING) {
        return "\u0906\u092a\u0915\u093e \u092a\u093f\u091b\u0932\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u0905\u092d\u0940 \u092d\u0940 \u0932\u0902\u092c\u093f\u0924 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.GOVERMENT_CHARGES) {
        return "\u0938\u0930\u0915\u093e\u0930\u0940 \u0936\u0941\u0932\u094d\u0915";
    };
    if (stringKey instanceof Language_Types.OKAY) {
        return "\u0920\u0940\u0915 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.NO_PAYMENT_HISTORY_AVAILABLE) {
        return "\u0915\u094b\u0908 \u092d\u0941\u0917\u0924\u093e\u0928 \u0907\u0924\u093f\u0939\u093e\u0938 \u0909\u092a\u0932\u092c\u094d\u0927 \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.ENTER_AADHAAR_NUMBER) {
        return "\u0906\u0927\u093e\u0930 \u0938\u0902\u0916\u094d\u092f\u093e/\u092f\u0942\u0906\u0908\u0921\u0940 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.AADHAAR_LINKING_REQUIRED) {
        return "\u0906\u0927\u093e\u0930 \u0932\u093f\u0902\u0915 \u0915\u0930\u0928\u093e \u0906\u0935\u0936\u094d\u092f\u0915 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.AADHAAR_LINKING_REQUIRED_DESCRIPTION) {
        return "\u092f\u093e\u0924\u094d\u0930\u0940 \u0938\u093e\u0925\u0940 \u0915\u0947 \u0932\u093f\u090f \u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f, \u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0928\u093e \u0906\u0927\u093e\u0930 \u0906\u0908\u0921\u0940 \u0932\u093f\u0902\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.BY_CLICKING_THIS_YOU_WILL_BE_AGREEING_TO_OUR_TC) {
        return "\u091c\u093e\u0930\u0940 \u0930\u0916\u0947\u0902 \u092a\u0930 \u0915\u094d\u0932\u093f\u0915 \u0915\u0930\u0915\u0947, \u0906\u092a \u0939\u092e\u093e\u0930\u0947 &nbsp; \u0938\u0947 \u0938\u0939\u092e\u0924 \u0939\u094b\u0924\u0947 \u0939\u0948\u0902; <a href=\"\">T&Cs</a>";
    };
    if (stringKey instanceof Language_Types.TERMS_AND_CONDITIONS_SHORT) {
        return "\u0928\u093f\u092f\u092e \u090f\u0935\u0902 \u0936\u0930\u094d\u0924\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.TC_TAIL) {
        return "";
    };
    if (stringKey instanceof Language_Types.OTP_SENT_TO_AADHAAR_NUMBER) {
        return "\u0906\u092a\u0915\u0947 \u0906\u0927\u093e\u0930 \u0938\u0947 \u091c\u0941\u0921\u093c\u0947 \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u092a\u0930 \u0913\u091f\u0940\u092a\u0940 \u092d\u0947\u091c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.ENTER_SIX_DIGIT_OTP) {
        return "\u091b\u0939 \u0905\u0902\u0915\u0940\u092f \u0913\u091f\u0940\u092a\u0940 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LINK_AADHAAR_ID) {
        return "\u0906\u0927\u093e\u0930 \u0906\u0908\u0921\u0940 \u0932\u093f\u0902\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NO_MOBILE_NUMBER_REGISTERED) {
        return "\u0906\u0927\u093e\u0930 \u0928\u0902\u092c\u0930 \u0915\u0947 \u0938\u093e\u0925 \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0930\u091c\u093f\u0938\u094d\u091f\u0930\u094d\u0921 \u0928\u0939\u0940\u0902 \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.EXCEED_OTP_GENERATION_LIMIT) {
        return "\u0905\u0927\u093f\u0915\u0924\u092e OTP \u091c\u0928\u0930\u0947\u0936\u0928 \u0938\u0940\u092e\u093e \u092a\u093e\u0930 \u0939\u094b \u0917\u0908\u0964 \u0915\u0943\u092a\u092f\u093e \u0915\u0941\u091b \u0938\u092e\u092f \u092c\u093e\u0926 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.AADHAAR_NUMBER_NOT_EXIST) {
        return "\u0906\u0927\u093e\u0930 \u0928\u0902\u092c\u0930 \u092e\u094c\u091c\u0942\u0926 \u0928\u0939\u0940\u0902 \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.INVALID_OTP) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u0913\u091f\u0940\u092a\u0940";
    };
    if (stringKey instanceof Language_Types.NO_SHARE_CODE) {
        return "\u0915\u094b\u0908 \u0936\u0947\u092f\u0930 \u0915\u094b\u0921 \u092a\u094d\u0930\u0926\u093e\u0928 \u0928\u0939\u0940\u0902 \u0915\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.WRONG_SHARE_CODE) {
        return "\u0917\u093c\u0932\u0924 \u0936\u0947\u092f\u0930 \u0915\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.INVALID_SHARE_CODE) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u0936\u0947\u092f\u0930 \u0915\u094b\u0921\u0964 \u0932\u0902\u092c\u093e\u0908 4 \u0939\u094b\u0928\u0940 \u091a\u093e\u0939\u093f\u090f \u0914\u0930 \u0907\u0938\u092e\u0947\u0902 \u0915\u0947\u0935\u0932 \u0938\u0902\u0916\u094d\u092f\u093e\u090f\u0902 \u0939\u094b\u0928\u0940 \u091a\u093e\u0939\u093f\u090f\u0964";
    };
    if (stringKey instanceof Language_Types.SESSION_EXPIRED) {
        return "\u0938\u0924\u094d\u0930 \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u0941\u0906\u0964 \u0915\u0943\u092a\u092f\u093e \u092a\u094d\u0930\u0915\u094d\u0930\u093f\u092f\u093e \u0926\u094b\u092c\u093e\u0930\u093e \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.OTP_ATTEMPT_EXCEEDED) {
        return "OTP \u092a\u094d\u0930\u092f\u093e\u0938 \u092a\u093e\u0930 \u0939\u094b \u0917\u090f\u0964 \u0915\u0943\u092a\u092f\u093e \u092a\u094d\u0930\u0915\u094d\u0930\u093f\u092f\u093e \u0926\u094b\u092c\u093e\u0930\u093e \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.UPSTREAM_INTERNAL_SERVER_ERROR) {
        return "\u0905\u092a\u0938\u094d\u091f\u094d\u0930\u0940\u092e \u0938\u094d\u0930\u094b\u0924/\u0938\u0930\u0915\u093e\u0930\u0940 \u0938\u094d\u0930\u094b\u0924 \u0906\u0902\u0924\u0930\u093f\u0915 \u0938\u0930\u094d\u0935\u0930 \u0924\u094d\u0930\u0941\u091f\u093f\u0964 \u0915\u0943\u092a\u092f\u093e \u092a\u094d\u0930\u0915\u094d\u0930\u093f\u092f\u093e \u0926\u094b\u092c\u093e\u0930\u093e \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_ALREADY_COMPLETED) {
        return "\u0932\u0947\u0928-\u0926\u0947\u0928 \u092a\u0939\u0932\u0947 \u0939\u0940 \u092a\u0942\u0930\u093e \u0939\u094b \u091a\u0941\u0915\u093e \u0939\u0948\u0964 \u0907\u0938 \u0932\u0947\u0928-\u0926\u0947\u0928 \u092a\u0930 \u0906\u0917\u0947 \u0915\u093e\u0930\u094d\u0930\u0935\u093e\u0908 \u0928\u0939\u0940\u0902 \u0915\u0940 \u091c\u093e \u0938\u0915\u0924\u0940\u0964";
    };
    if (stringKey instanceof Language_Types.GOTO_YOUR_NEAREST_BOOTH) {
        return "\u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0928\u093e \u0906\u0927\u093e\u0930 \u0938\u0924\u094d\u092f\u093e\u092a\u093f\u0924 \u0915\u0930\u093e\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0905\u092a\u0928\u0947 \u0928\u091c\u0926\u0940\u0915\u0940 \u092c\u0942\u0925 \u092a\u0930 \u092a\u0939\u0941\u0902\u091a\u0947\u0902.";
    };
    if (stringKey instanceof Language_Types.AADHAAR_ALREADY_LINKED) {
        return "\u0906\u0927\u093e\u0930 \u092a\u0939\u0932\u0947 \u0938\u0947 \u0939\u0940 \u0932\u093f\u0902\u0915 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.OPTIONAL) {
        return " (\u0935\u0948\u0915\u0932\u094d\u092a\u093f\u0915)";
    };
    if (stringKey instanceof Language_Types.DOWNLOAD_STATEMENT) {
        return "\u0938\u094d\u091f\u0947\u091f\u092e\u0947\u0902\u091f \u0921\u093e\u0909\u0928\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SELECT_A_DATE_RANGE) {
        return "\u0935\u093f\u0935\u0930\u0923 \u0921\u093e\u0909\u0928\u0932\u094b\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u090f\u0915 \u0924\u093f\u0925\u093f \u0938\u0940\u092e\u093e \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.FEE_PAYMENT_HISTORY) {
        return "\u0936\u0941\u0932\u094d\u0915 \u092d\u0941\u0917\u0924\u093e\u0928 \u0907\u0924\u093f\u0939\u093e\u0938";
    };
    if (stringKey instanceof Language_Types.LANGUAGES_SPOKEN) {
        return "\u092d\u093e\u0937\u093e \u092e\u0948\u0902 \u092c\u094b\u0932 \u0938\u0915\u0924\u093e \u0939\u0942\u0901";
    };
    if (stringKey instanceof Language_Types.VIEW_PAYMENT_HISTORY) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0907\u0924\u093f\u0939\u093e\u0938 \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RIDE_TYPE) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0915\u093e \u092a\u094d\u0930\u0915\u093e\u0930";
    };
    if (stringKey instanceof Language_Types.PLACE_CALL_REQUEST) {
        return "\u0915\u0949\u0932 \u0905\u0928\u0941\u0930\u094b\u0927 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RC_STATUS) {
        return "\u0906\u0930\u0938\u0940 \u0938\u094d\u0925\u093f\u0924\u093f";
    };
    if (stringKey instanceof Language_Types.RATED_BY_USERS1) {
        return "";
    };
    if (stringKey instanceof Language_Types.RATED_BY_USERS2) {
        return "\u0909\u092a\u092f\u094b\u0917\u0915\u0930\u094d\u0924\u093e\u0913\u0902 \u0926\u094d\u0935\u093e\u0930\u093e \u092e\u0942\u0932\u094d\u092f\u093e\u0902\u0915\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.MONTHS) {
        return "\u092e\u0939\u0940\u0928\u0947";
    };
    if (stringKey instanceof Language_Types.ENTER_AADHAAR_DETAILS) {
        return "\u0906\u0927\u093e\u0930 \u0935\u093f\u0935\u0930\u0923 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CALL_REQUEST_HAS_BEEN_PLACED) {
        return "\u0906\u092a\u0915\u093e \u0915\u0949\u0932 \u0905\u0928\u0941\u0930\u094b\u0927 \u0930\u0916 \u0926\u093f\u092f\u093e \u0917\u092f\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.RC_ADDED_SUCCESSFULLY) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u091c\u094b\u0921\u093c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.OFFER_APPLIED) {
        return "\u092a\u094d\u0930\u0938\u094d\u0924\u093e\u0935 \u0932\u093e\u0917\u0942";
    };
    if (stringKey instanceof Language_Types.YOUR_EARNINGS) {
        return "\u0906\u092a\u0915\u0940 \u0915\u092e\u093e\u0908";
    };
    if (stringKey instanceof Language_Types.NUMBER_OF_RIDES) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0915\u0940 \u0938\u0902\u0916\u094d\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.FARE_BREAKUP) {
        return "\u0915\u093f\u0930\u093e\u092f\u093e \u092c\u094d\u0930\u0947\u0915\u0905\u092a";
    };
    if (stringKey instanceof Language_Types.MY_PLAN) {
        return "\u092e\u0947\u0930\u0940 \u092f\u094b\u091c\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.YOUR_DUES) {
        return "\u0906\u092a\u0915\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f";
    };
    if (stringKey instanceof Language_Types.YOUR_DUES_DESCRIPTION) {
        return "\u0906\u092a\u0928\u0947 \u0905\u092a\u0928\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u091a\u0941\u0915\u093e\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u090f\u0915 \u0911\u091f\u094b\u092a\u0947 \u0938\u094d\u0925\u093e\u092a\u093f\u0924 \u0915\u093f\u092f\u093e \u0939\u0948\u0964 \u0939\u092e \u0938\u094d\u0935\u091a\u093e\u0932\u093f\u0924 \u0930\u0942\u092a \u0938\u0947 \u092f\u0939 \u0938\u0941\u0928\u093f\u0936\u094d\u091a\u093f\u0924 \u0915\u0930\u0928\u0947 \u0915\u093e \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902\u0917\u0947 \u0915\u093f \u0906\u092a\u0915\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u0939\u092e\u0947\u0936\u093e \u0938\u092e\u092f \u092a\u0930 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u093f\u092f\u093e \u091c\u093e\u090f\u0964";
    };
    if (stringKey instanceof Language_Types.CURRENT_DUES) {
        return "\u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f";
    };
    if (stringKey instanceof Language_Types.YOUR_LIMIT) {
        return "\u0906\u092a\u0915\u0940 \u0938\u0940\u092e\u093e";
    };
    if (stringKey instanceof Language_Types.DUE_DETAILS) {
        return "\u0926\u0947\u092f \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.TRIP_DATE) {
        return "\u092f\u093e\u0924\u094d\u0930\u093e \u0924\u093f\u0925\u093f";
    };
    if (stringKey instanceof Language_Types.AMOUNT) {
        return "\u0930\u093e\u0936\u093f";
    };
    if (stringKey instanceof Language_Types.VIEW_DUE_DETAILS) {
        return "\u0909\u091a\u093f\u0924 \u0935\u093f\u0935\u0930\u0923 \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CURRENT_PLAN) {
        return "\u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u092f\u094b\u091c\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_PLAN) {
        return "\u0935\u0948\u0915\u0932\u094d\u092a\u093f\u0915 \u092f\u094b\u091c\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_DETAILS) {
        return "\u0938\u094d\u0935\u0924\u0903 \u092d\u0941\u0917\u0924\u093e\u0928 \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.CANCEL_AUTOPAY_STR) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0930\u0926\u094d\u0926 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.WE_MIGHT_BE_LOST) {
        return "\u0909\u0939 \u0913\u0939! \u0939\u092e \u0916\u094b \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.EXEPERIENCING_ERROR) {
        return "\u0924\u094d\u0930\u0941\u091f\u093f \u0915\u093e \u0905\u0928\u0941\u092d\u0935 \u0939\u094b \u0930\u0939\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.ENJOY_THESE_BENEFITS) {
        return "\u0907\u0928 \u0932\u093e\u092d\u094b\u0902 \u0915\u093e \u0906\u0928\u0902\u0926 \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CHOOSE_YOUR_PLAN) {
        return "\u0905\u092d\u0940 \u090f\u0915 \u092f\u094b\u091c\u0928\u093e \u091a\u0941\u0928\u0947\u0902!";
    };
    if (stringKey instanceof Language_Types.SKIP_FOR_NOW) {
        return "\u0905\u092d\u0940 \u0915\u0947 \u0932\u093f\u090f \u091b\u094b\u0921\u093c \u0926\u0947";
    };
    if (stringKey instanceof Language_Types.SEVEN_DAY_FREE_TRIAL_ACTIVATED) {
        return "7-\u0926\u093f\u0935\u0938\u0940\u092f \u0928\u093f\u0903\u0936\u0941\u0932\u094d\u0915 \u092a\u0930\u0940\u0915\u094d\u0937\u0923 \u0938\u0915\u094d\u0930\u093f\u092f!";
    };
    if (stringKey instanceof Language_Types.TAKE_UNLIMITED_RIDES_FOR_THE_NEXT_SEVEN_DAYS) {
        return "\u092a\u0939\u0932\u0947 7 \u0926\u093f\u0928\u094b\u0902 \u0915\u0947 \u0932\u093f\u090f \u0905\u0938\u0940\u092e\u093f\u0924 \u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.EVERY_RIDE_AT_ZERO_COMMISSION) {
        return "\u092a\u094d\u0930\u0924\u094d\u092f\u0947\u0915 \u0938\u0935\u093e\u0930\u0940 \u0936\u0942\u0928\u094d\u092f \u0915\u092e\u0940\u0936\u0928 \u092a\u0930!";
    };
    if (stringKey instanceof Language_Types.EARN_UPTO_PER_DAY) {
        return "\u092a\u094d\u0930\u0924\u093f\u0926\u093f\u0928 \u20b9{} \u0924\u0915 \u0915\u092e\u093e\u090f\u0902!";
    };
    if (stringKey instanceof Language_Types.HOW_THIS_WORKS) {
        return "\u092f\u0939 \u0915\u0948\u0938\u0947 \u0915\u093e\u092e \u0915\u0930\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.SIGN_UP_FOR_AUTOPAY_BY_PAYING_JUST) {
        return "\u0915\u0947\u0935\u0932 \u20b91 \u0915\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0915\u0947 \u0911\u091f\u094b\u092a\u0947 \u0915\u0947 \u0932\u093f\u090f \u0938\u093e\u0907\u0928 \u0905\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GET_REMINDED_ABOUT_YOUR_PLAN_SETUP) {
        return "\u0905\u092a\u0928\u0947 \u092a\u094d\u0932\u093e\u0928 \u0938\u0947\u091f\u0905\u092a \u0915\u0947 \u092c\u093e\u0930\u0947 \u092e\u0947\u0902 \u092f\u093e\u0926 \u0926\u093f\u0932\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.FREE_TRIAL_REMINDER) {
        return "\u0926\u093f\u0928 5: \u0928\u093f\u0903\u0936\u0941\u0932\u094d\u0915 \u092a\u0930\u0940\u0915\u094d\u0937\u0923 \u0905\u0928\u0941\u0938\u094d\u092e\u093e\u0930\u0915";
    };
    if (stringKey instanceof Language_Types.PLAN_STARTS) {
        return "\u0926\u093f\u0928 7: \u092f\u094b\u091c\u0928\u093e \u092a\u094d\u0930\u093e\u0930\u0902\u092d";
    };
    if (stringKey instanceof Language_Types.EASY_AUTOMATIC_PAYMENTS_START) {
        return "\u0906\u0938\u093e\u0928, \u0938\u094d\u0935\u091a\u093e\u0932\u093f\u0924 \u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u094d\u0930\u093e\u0930\u0902\u092d";
    };
    if (stringKey instanceof Language_Types.FREE_UNTIL) {
        return " \u0924\u0915 \u092e\u0941\u092b\u094d\u0924";
    };
    if (stringKey instanceof Language_Types.PER_RIDE) {
        return "\u092a\u094d\u0930\u0924\u093f \u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.PER_DAY) {
        return "\u092a\u094d\u0930\u0924\u093f \u0926\u093f\u0928";
    };
    if (stringKey instanceof Language_Types.OFFER) {
        return "\u0911\u092b\u0930";
    };
    if (stringKey instanceof Language_Types.OFFERS) {
        return "\u0911\u092b\u0930";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ON_THE_FREE_TRIAL) {
        return "\u0906\u092a \u0928\u093f\u0903\u0936\u0941\u0932\u094d\u0915 \u092a\u0930\u0940\u0915\u094d\u0937\u0923 \u092a\u0930 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_BEFORE_THE_TRAIL_PERIOD_EXPIRES) {
        return "\u092a\u0930\u0940\u0915\u094d\u0937\u0923 \u0905\u0935\u0927\u093f \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b\u0928\u0947 \u0938\u0947 \u092a\u0939\u0932\u0947 \u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GET_FREE_TRAIL_UNTIL) {
        return "\u0924\u0915 \u0928\u093f\u0903\u0936\u0941\u0932\u094d\u0915 \u092a\u0930\u0940\u0915\u094d\u0937\u0923 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CLEAR_DUES) {
        return "\u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u091a\u0941\u0915\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING_ALERT) {
        return "\u26a0\ufe0f \u092d\u0941\u0917\u0924\u093e\u0928 \u0932\u0902\u092c\u093f\u0924! \u26a0\ufe0f";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING_ALERT_DESC) {
        return "\u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u092a\u0930 \u092f\u093e\u0924\u094d\u0930\u093e \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f, \u0905\u092a\u0928\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u091a\u0941\u0915\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.LOW_ACCOUNT_BALANCE) {
        return "\u0915\u092e \u0916\u093e\u0924\u093e \u0936\u0947\u0937";
    };
    if (stringKey instanceof Language_Types.LOW_ACCOUNT_BALANCE_DESC) {
        return "\u0906\u092a\u0915\u0947 \u092c\u0948\u0902\u0915 \u0916\u093e\u0924\u0947 \u0915\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u0915\u092e \u0939\u0948\u0964 \u0928\u093f\u0930\u094d\u092c\u093e\u0927 \u0938\u0935\u093e\u0930\u0940 \u0915\u093e \u0906\u0928\u0902\u0926 \u0932\u0947\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u092a\u0948\u0938\u0947 \u091c\u094b\u0921\u093c\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.OKAY_GOT_IT) {
        return "\u0920\u0940\u0915 \u0939\u0948, \u0938\u092e\u091d \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.LIMITED_TIME_OFFER) {
        return "\u0906\u092a\u0915\u0947 \u0932\u093f\u090f \u0938\u0940\u092e\u093f\u0924 \u0938\u092e\u092f \u0915\u093e \u0911\u092b\u0930!";
    };
    if (stringKey instanceof Language_Types.JOIN_NOW) {
        return "\u0905\u092d\u0940 \u0936\u093e\u092e\u093f\u0932 \u0939\u094b\u0902";
    };
    if (stringKey instanceof Language_Types.AUTOMATIC_PAYMENTS_WILL_APPEAR_HERE) {
        return "\u0938\u094d\u0935\u091a\u093e\u0932\u093f\u0924 \u092d\u0941\u0917\u0924\u093e\u0928 \u092f\u0939\u093e\u0902 \u0926\u093f\u0916\u093e\u0908 \u0926\u0947\u0902\u0917\u0947";
    };
    if (stringKey instanceof Language_Types.MANUAL_PAYMENTS) {
        return "\u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u092d\u0941\u0917\u0924\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.MANUAL_PAYMENTS_WILL_APPEAR_HERE) {
        return "\u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u092d\u0941\u0917\u0924\u093e\u0928 \u092f\u0939\u093e\u0902 \u0926\u093f\u0916\u093e\u0908 \u0926\u0947\u0902\u0917\u0947";
    };
    if (stringKey instanceof Language_Types.NO_AUTOMATIC_PAYMENTS_DESC) {
        return "\u0906\u092a\u0915\u093e \u0911\u091f\u094b\u092a\u0947 \u0907\u0924\u093f\u0939\u093e\u0938 \u0906\u092a\u0938\u0947 \u0936\u0941\u0932\u094d\u0915 \u0932\u0947\u0928\u0947 \u0915\u0947 \u092c\u093e\u0926 \u092f\u0939\u093e\u0902 \u0926\u093f\u0916\u093e\u0908 \u0926\u0947\u0917\u093e";
    };
    if (stringKey instanceof Language_Types.NO_MANUAL_PAYMENTS_DESC) {
        return "\u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u091a\u0941\u0915\u093e\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0906\u092a\u0915\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u0907\u0924\u093f\u0939\u093e\u0938, \u092f\u0926\u093f \u0915\u094b\u0908 \u0939\u094b, \u092f\u0939\u093e\u0902 \u0926\u093f\u0916\u093e\u0908 \u0926\u0947\u0917\u093e\u0964";
    };
    if (stringKey instanceof Language_Types.PAYMENT_HISTORY) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0907\u0924\u093f\u0939\u093e\u0938";
    };
    if (stringKey instanceof Language_Types.TAP_A_PLAN_TO_VIEW_DETAILS) {
        return "\u0935\u093f\u0935\u0930\u0923 \u0926\u0947\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0915\u093f\u0938\u0940 \u092f\u094b\u091c\u0928\u093e \u092a\u0930 \u091f\u0948\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.HOW_IT_WORKS) {
        return "\u091c\u093e\u0928\u093f\u090f \u092f\u0939 \u0915\u0948\u0938\u0947 \u0915\u093e\u092e \u0915\u0930\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.ZERO_COMMISION) {
        return "\u0936\u0942\u0928\u094d\u092f \u0915\u092e\u0940\u0936\u0928";
    };
    if (stringKey instanceof Language_Types.EARN_TODAY_PAY_TOMORROW) {
        return "\u0906\u091c \u0915\u092e\u093e\u090f\u0901, \u0915\u0932 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PAY_ONLY_IF_YOU_TAKE_RIDES) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0947\u0935\u0932 \u0924\u092d\u0940 \u0915\u0930\u0947\u0902 \u091c\u092c \u0906\u092a \u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLANS) {
        return "\u092f\u094b\u091c\u0928\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.PLAN) {
        return "\u092f\u094b\u091c\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.DAY) {
        return "\u0926\u093f\u0928";
    };
    if (stringKey instanceof Language_Types.MANAGE_PLAN) {
        return "\u092f\u094b\u091c\u0928\u093e \u092a\u094d\u0930\u092c\u0902\u0927\u093f\u0924 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.VIEW_AUTOPAY_DETAILS) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0935\u093f\u0935\u0930\u0923 \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SWITCH_AND_SAVE) {
        return "\u0938\u094d\u0935\u093f\u091a \u0915\u0930\u0947\u0902 \u0914\u0930 \u0938\u0939\u0947\u091c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SWITCH_AND_SAVE_DESC) {
        return "\u0906\u092a\u0928\u0947 \u0906\u091c 7 \u0938\u0947 \u0905\u0927\u093f\u0915 \u092f\u093e\u0924\u094d\u0930\u093e\u090f\u0902 \u092a\u0942\u0930\u0940 \u0915\u0940 \u0939\u0948\u0902\u0964 \u0921\u0947\u0932\u0940 \u0905\u0928\u0932\u093f\u092e\u093f\u091f\u0947\u0921 \u092f\u094b\u091c\u0928\u093e \u092a\u0930 \u0938\u094d\u0935\u093f\u091a \u0915\u0930\u0915\u0947 \u20b910 \u0924\u0915 \u092c\u091a\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.SWITCH_NOW) {
        return "\u0905\u092d\u0940 \u0938\u094d\u0935\u093f\u091a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PAYMENT_MODE_CHANGED_TO_MANUAL) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u092e\u094b\u0921 \u0915\u094b \u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u092e\u0947\u0902 \u092c\u0926\u0932 \u0926\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.PAYMENT_MODE_CHANGED_TO_MANUAL_DESC) {
        return "\u0906\u092a\u0928\u0947 \u0905\u092a\u0928\u093e UPI \u0911\u091f\u094b\u092a\u0947 \u0930\u094b\u0915 \u0926\u093f\u092f\u093e \u0939\u0948\u0964 \u0906\u092a \u0905\u092a\u0928\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u0930\u0942\u092a \u0938\u0947 \u091a\u0941\u0915\u093e \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_PAYMENTS) {
        return "\u0938\u094d\u0935\u0924\u0903 \u092d\u0941\u0917\u0924\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_ON) {
        return "\u0932\u0947\u0928\u0926\u0947\u0928 \u091a\u093e\u0932\u0942";
    };
    if (stringKey instanceof Language_Types.SUCCESS) {
        return "\u0938\u092b\u0932\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.RIDES_TAKEN_ON) {
        return "\u092f\u093e\u0924\u094d\u0930\u093e \u0915\u0940 \u0924\u093e\u0930\u0940\u0916";
    };
    if (stringKey instanceof Language_Types.JOIN_PLAN) {
        return "\u092f\u094b\u091c\u0928\u093e \u092e\u0947\u0902 \u0936\u093e\u092e\u093f\u0932 \u0939\u094b\u0902";
    };
    if (stringKey instanceof Language_Types.JOIN_NAMMAA_YATRI) {
        return "\u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u0938\u0947 \u091c\u0941\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CANCEL_AUTOPAY_AND_PAY_MANUALLY) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0930\u0926\u094d\u0926 \u0915\u0930\u0947\u0902 \u0914\u0930 \u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u0930\u0942\u092a \u0938\u0947 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLAN_ACTIVATED_SUCCESSFULLY) {
        return "\u092f\u094b\u091c\u0928\u093e \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u0938\u0915\u094d\u0930\u093f\u092f";
    };
    if (stringKey instanceof Language_Types.DUES_CLEARED_SUCCESSFULLY) {
        return "\u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u091a\u0941\u0915\u093e\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.NOT_PLANNING_TO_TAKE_RIDES) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u0947 \u0915\u0940 \u092f\u094b\u091c\u0928\u093e \u0928\u0939\u0940\u0902 \u092c\u0928\u093e \u0930\u0939\u0947?";
    };
    if (stringKey instanceof Language_Types.RETRY_PAYMENT_STR) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u093e \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PAUSE_AUTOPAY_STR) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0930\u094b\u0915\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_STR) {
        return "\u0938\u0947\u091f\u0905\u092a \u0911\u091f\u094b\u092a\u0947";
    };
    if (stringKey instanceof Language_Types.VIEW_RIDE_DETAILS) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0935\u093f\u0935\u0930\u0923 \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ACCOUNT) {
        return "\u0916\u093e\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_IS_NOT_ENABLED_YET) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0905\u092d\u0940 \u0924\u0915 \u0938\u0915\u094d\u0937\u092e \u0928\u0939\u0940\u0902 \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.ENABLE_AUTOPAY_DESC) {
        return "\u092a\u0930\u0947\u0936\u093e\u0928\u0940 \u092e\u0941\u0915\u094d\u0924 \u092d\u0941\u0917\u0924\u093e\u0928 \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0905\u092d\u0940 \u0911\u091f\u094b\u092a\u0947 \u0938\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902!";
    };
    if (stringKey instanceof Language_Types.ENABLE_AUTOPAY_NOW) {
        return "\u0905\u092d\u0940 \u0911\u091f\u094b\u092a\u0947 \u0938\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_PENDING_STR) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0932\u0902\u092c\u093f\u0924!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_PENDING_DESC_STR) {
        return "\u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0930\u0947\u0902 \u092f\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u093e \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902\u0964 \u0905\u0924\u093f\u0930\u093f\u0915\u094d\u0924 \u092d\u0941\u0917\u0924\u093e\u0928 \u0935\u093e\u092a\u0938 \u0915\u0930 \u0926\u093f\u092f\u093e \u091c\u093e\u090f\u0917\u093e\u0964";
    };
    if (stringKey instanceof Language_Types.REFRESH_STR) {
        return "\u0938\u094d\u0925\u093f\u0924\u093f \u091c\u093e\u0901\u091a\u093f\u090f";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_DETAILS) {
        return "\u0932\u0947\u0928\u0926\u0947\u0928 \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.RIDE_DETAILS) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.MY_PLAN_TITLE) {
        return "\u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u092a\u094d\u0932\u093e\u0928\u094d\u0938";
    };
    if (stringKey instanceof Language_Types.SWITCH_TO) {
        return "{} \u092f\u094b\u091c\u0928\u093e \u092a\u0930 \u0938\u094d\u0935\u093f\u091a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_TRY_AGAIN) {
        return "\u0915\u0943\u092a\u092f\u093e \u092a\u0941\u0928: \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLAN_NOT_FOUND) {
        return "\u092f\u094b\u091c\u0928\u093e \u0928\u0939\u0940\u0902 \u092e\u093f\u0932\u0940";
    };
    if (stringKey instanceof Language_Types.MANDATE_NOT_FOUND) {
        return "\u091c\u0928\u093e\u0926\u0947\u0936 \u0928\u0939\u0940\u0902 \u092e\u093f\u0932\u093e";
    };
    if (stringKey instanceof Language_Types.ACTIVE_MANDATE_EXISTS) {
        return "\u0938\u0915\u094d\u0930\u093f\u092f \u092e\u0948\u0902\u0921\u0947\u091f \u092a\u0939\u0932\u0947 \u0938\u0947 \u092e\u094c\u091c\u0942\u0926 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.NO_ACTIVE_MANDATE_EXIST) {
        return "\u0915\u094b\u0908 \u0938\u0915\u094d\u0930\u093f\u092f \u0905\u0927\u093f\u0926\u0947\u0936 \u092e\u094c\u091c\u0942\u0926 \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.NO_PLAN_FOR_DRIVER) {
        return "\u0915\u094b\u0908 \u092f\u094b\u091c\u0928\u093e \u0928\u0939\u0940\u0902 \u092e\u093f\u0932\u0940";
    };
    if (stringKey instanceof Language_Types.INVALID_PAYMENT_MODE) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u092d\u0941\u0917\u0924\u093e\u0928 \u092e\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.INVALID_AUTO_PAY_STATUS) {
        return "\u0905\u092e\u093e\u0928\u094d\u092f \u0911\u091f\u094b\u092a\u0947 \u0938\u094d\u0925\u093f\u0924\u093f";
    };
    if (stringKey instanceof Language_Types.MAX_AMOUNT) {
        return "\u0905\u0927\u093f\u0915\u0924\u092e \u0930\u093e\u0936\u093f";
    };
    if (stringKey instanceof Language_Types.FREQUENCY) {
        return "\u0906\u0935\u0943\u0924\u094d\u0924\u093f";
    };
    if (stringKey instanceof Language_Types.STATRED_ON) {
        return "\u0936\u0941\u0930\u0942 \u0939\u0941\u0906";
    };
    if (stringKey instanceof Language_Types.EXPIRES_ON) {
        return "\u0938\u092e\u093e\u092a\u094d\u0924\u093f \u092a\u0930";
    };
    if (stringKey instanceof Language_Types.SWITCHED_PLAN) {
        return "\u0938\u094d\u0935\u093f\u091a\u094d\u0921 \u092a\u094d\u0932\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.ONETIME) {
        return "\u090f\u0915 \u092c\u093e\u0930";
    };
    if (stringKey instanceof Language_Types.FORTNIGHTLY) {
        return "\u092a\u093e\u0915\u094d\u0937\u093f\u0915";
    };
    if (stringKey instanceof Language_Types.MONTHLY) {
        return "\u092e\u093e\u0938\u093f\u0915";
    };
    if (stringKey instanceof Language_Types.BIMONTHLY) {
        return "\u0926\u094d\u0935\u093f\u092e\u093e\u0938\u093f\u0915";
    };
    if (stringKey instanceof Language_Types.QUARTERLY) {
        return "\u0924\u094d\u0930\u0948\u092e\u093e\u0938\u093f\u0915";
    };
    if (stringKey instanceof Language_Types.HALFYEARLY) {
        return "\u0905\u0930\u094d\u0927\u0935\u093e\u0930\u094d\u0937\u093f\u0915";
    };
    if (stringKey instanceof Language_Types.YEARLY) {
        return "\u0935\u093e\u0930\u094d\u0937\u093f\u0915";
    };
    if (stringKey instanceof Language_Types.ASPRESENTED) {
        return "\u091c\u092c \u092d\u0940 \u092a\u094d\u0930\u0938\u094d\u0924\u0941\u0924 \u0915\u093f\u092f\u093e \u091c\u093e\u090f";
    };
    if (stringKey instanceof Language_Types.DAILY_PER_RIDE_DESC) {
        return "19 \u0938\u093f\u0924\u092e\u094d\u092c\u0930 \u0924\u0915 \u0915\u094b\u0908 \u0936\u0941\u0932\u094d\u0915 \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.FIRST_FREE_RIDE) {
        return "\u092a\u0939\u0932\u0940 \u0938\u0935\u093e\u0930\u0940 \u092e\u0941\u092b\u093c\u094d\u0924";
    };
    if (stringKey instanceof Language_Types.JOIN_THE_UNLIMITED_PLAN) {
        return "\u0935\u093f\u0936\u0947\u0937 \u091b\u0942\u091f \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0905\u092c \u0921\u0947\u0932\u0940 \u0905\u0928\u0932\u093f\u092e\u093f\u091f\u0947\u0921 \u092f\u094b\u091c\u0928\u093e \u092e\u0947\u0902 \u0936\u093e\u092e\u093f\u0932 \u0939\u094b\u0902";
    };
    if (stringKey instanceof Language_Types.MAYBE_LATER) {
        return "\u0936\u093e\u092f\u0926 \u092c\u093e\u0926 \u092e\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOUR_DUES_DESCRIPTION_MANUAL) {
        return "\u0915\u0943\u092a\u092f\u093e \u0928\u093f\u0930\u094d\u092c\u093e\u0927 \u0938\u0935\u093e\u0930\u0940 \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0940 \u0938\u0940\u092e\u093e \u0924\u0915 \u092a\u0939\u0941\u0902\u091a\u0928\u0947 \u0938\u0947 \u092a\u0939\u0932\u0947 \u0905\u092a\u0928\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u091a\u0941\u0915\u093e \u0926\u0947\u0902\u0964 \u0906\u092a \u0906\u0938\u093e\u0928, \u092a\u0930\u0947\u0936\u093e\u0928\u0940 \u092e\u0941\u0915\u094d\u0924 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0947 \u0932\u093f\u090f <b>UPI \u0911\u091f\u094b\u092a\u0947</b>\u092d\u0940 \u0938\u0947\u091f \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.DO_YOU_WANT_TO_CANCEL) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0930\u0926\u094d\u0926 \u0915\u0930\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.DO_YOU_WANT_TO_CANCEL_DESC) {
        return "\u0906\u092a \u0911\u091f\u094b\u092a\u0947 \u0930\u0926\u094d\u0926 \u0915\u0930\u0928\u0947 \u0935\u093e\u0932\u0947 \u0939\u0948\u0902\u0964\x0a\u0938\u094d\u0935\u091a\u093e\u0932\u093f\u0924 \u092d\u0941\u0917\u0924\u093e\u0928 \u092b\u093f\u0930 \u0938\u0947 \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0906\u092a\u0915\u094b \u092b\u093f\u0930 \u0938\u0947 \u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f \u0915\u0930\u0928\u093e \u0939\u094b\u0917\u093e\u0964";
    };
    if (stringKey instanceof Language_Types.YOUR_PAYMENT_WAS_UNSUCCESSFUL) {
        return "\u0906\u092a\u0915\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u094d\u0930\u092f\u093e\u0938 \u0905\u0938\u092b\u0932 \u0930\u0939\u093e\u0964";
    };
    if (stringKey instanceof Language_Types.PAYMENT_CANCELLED) {
        return "\u0906\u092a\u0928\u0947 \u0905\u092a\u0928\u093e UPI \u0911\u091f\u094b\u092a\u0947 \u0930\u0926\u094d\u0926 \u0915\u0930 \u0926\u093f\u092f\u093e \u0939\u0948. \u0906\u092a \u0905\u092a\u0928\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u0930\u0942\u092a \u0938\u0947 \u091a\u0941\u0915\u093e \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.UPI_AUTOPAY_S) {
        return "UPI \u0911\u091f\u094b\u092a\u0947";
    };
    if (stringKey instanceof Language_Types.MANUAL_PAYMENT_STR) {
        return "\u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u092d\u0941\u0917\u0924\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.DAILY_UNLIMITED) {
        return "\u0921\u0947\u0932\u0940 \u0905\u0928\u0932\u093f\u092e\u093f\u091f\u0947\u0921";
    };
    if (stringKey instanceof Language_Types.DAILY_PER_RIDE) {
        return "\u0926\u0948\u0928\u093f\u0915 \u092a\u094d\u0930\u0924\u093f \u0938\u0935\u093e\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.DAILY_UNLIMITED_PLAN_DESC) {
        return "\u0939\u0930 \u0926\u093f\u0928 \u0905\u0938\u0940\u092e\u093f\u0924 \u0938\u0935\u093e\u0930\u0940 \u0915\u093e \u0906\u0928\u0902\u0926 \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DAILY_PER_RIDE_PLAN_DESC) {
        return "\u092a\u094d\u0930\u0924\u093f \u0926\u093f\u0928 \u0905\u0927\u093f\u0915\u0924\u092e \u20b935 \u0924\u0915";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_CANCELLED) {
        return "\u0911\u091f\u094b\u092a\u0947 \u092d\u0941\u0917\u0924\u093e\u0928 \u0930\u0926\u094d\u0926";
    };
    if (stringKey instanceof Language_Types.NO) {
        return "\u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.YES_CANCEL) {
        return "\u0939\u093e\u0902, \u0930\u0926\u094d\u0926 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PAY_TO_JOIN_THIS_PLAN) {
        return "\u092f\u094b\u091c\u0928\u093e \u0938\u0947 \u091c\u0941\u0921\u093c\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u20b91 \u0915\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.OFFERS_NOT_APPLICABLE) {
        return "\u092a\u0942\u0930\u093e \u0939\u094b\u0928\u0947 \u0924\u0915 \u0911\u092b\u0930 \u0932\u093e\u0917\u0942 \u0928\u0939\u0940\u0902 \u0939\u094b\u0902\u0917\u0947";
    };
    if (stringKey instanceof Language_Types.PAUSED_STR) {
        return "\u0930\u094b\u0915\u0947 \u0917\u090f";
    };
    if (stringKey instanceof Language_Types.PENDING_STR) {
        return "\u0932\u0902\u092c\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.SWITCH_PLAN_STR) {
        return "\u092f\u094b\u091c\u0928\u093e \u092c\u0926\u0932\u0947\u0902?";
    };
    if (stringKey instanceof Language_Types.PLAN_SWITCHED_TO) {
        return "\u092f\u094b\u091c\u0928\u093e \u0915\u094b \u0905\u0926\u094d\u092f\u0924\u0928 \u0915\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.OFFERS_APPLICABLE_ON_DAILY_UNLIMITED) {
        return "\u0928\u094b\u091f: \u0911\u092b\u0930 \u0915\u0947\u0935\u0932 \u0921\u0947\u0932\u0940 \u0905\u0928\u0932\u093f\u092e\u093f\u091f\u0947\u0921 \u092a\u094d\u0932\u093e\u0928 \u092a\u0930 \u0932\u093e\u0917\u0942 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.DAILY_UNLIMITED_OFFER_NOT_AVAILABLE) {
        return "\u0928\u094b\u091f: \u0911\u092b\u0930 \u0915\u0947\u0935\u0932 \u0921\u0947\u0932\u0940 \u0905\u0928\u0932\u093f\u092e\u093f\u091f\u0947\u0921 \u092a\u094d\u0932\u093e\u0928 \u092a\u0930 \u0932\u093e\u0917\u0942 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.NO_RIDES_NO_CHARGE) {
        return "\u092f\u0926\u093f \u0906\u092a \u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0924\u0947 \u0939\u0948\u0902 \u0924\u094b \u0939\u0940 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GET_SPECIAL_OFFERS) {
        return "\u0935\u093f\u0936\u0947\u0937 \u0911\u092b\u093c\u0930 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.VALID_ONLY_IF_PAYMENT) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u0942\u0930\u093e \u0939\u094b\u0928\u0947 \u092a\u0930 \u0939\u0940 \u092e\u093e\u0928\u094d\u092f";
    };
    if (stringKey instanceof Language_Types.HELP_STR) {
        return "\u0938\u0939\u093e\u092f\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.REFRESH_STRING) {
        return "\u092a\u0941\u0928\u0903 \u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CHAT_FOR_HELP) {
        return "\u091a\u0948\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.VIEW_FAQs) {
        return "\u092a\u094d\u0930\u0936\u094d\u0928?";
    };
    if (stringKey instanceof Language_Types.FIND_HELP_CENTRE) {
        return "\u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0947\u0902\u0926\u094d\u0930 \u0922\u0942\u0902\u0922\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CONTACT) {
        return "\u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GO_TO_LOCATION) {
        return "\u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u091c\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.NO_HELP_CENTER_IS_ACTIVE_NOW) {
        return "\u0905\u092d\u0940 \u0915\u094b\u0908 \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0947\u0902\u0926\u094d\u0930 \u0938\u0915\u094d\u0930\u093f\u092f \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.HELP_CENTERS_LOCATION_WILL_APPEAR_HERE_ONCE_THEY_ARE_ACTIVE) {
        return "\u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0947\u0902\u0926\u094d\u0930\u094b\u0902 \u0915\u0947 \u0938\u0915\u094d\u0930\u093f\u092f \u0939\u094b\u0928\u0947 \u092a\u0930 \u0909\u0928\u0915\u0947 \u0938\u094d\u0925\u093e\u0928 \u092f\u0939\u093e\u0902 \u0926\u093f\u0916\u093e\u0908 \u0926\u0947\u0902\u0917\u0947";
    };
    if (stringKey instanceof Language_Types.SUPPORT) {
        return "\u0938\u0939\u093e\u092f\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.NEED_HELP_JOINING_THE_PLAN) {
        return "\u092f\u094b\u091c\u0928\u093e \u092f\u093e \u0911\u091f\u094b\u092a\u0947 \u092e\u0947\u0902 \u0936\u093e\u092e\u093f\u0932 \u0939\u094b\u0928\u0947 \u092e\u0947\u0902 \u092e\u0926\u0926 \u091a\u093e\u0939\u093f\u090f?";
    };
    if (stringKey instanceof Language_Types.NEED_HELP) {
        return "\u092e\u0926\u0926 \u0915\u0940 \u091c\u093c\u0930\u0942\u0930\u0924 \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_NOW_TO_GET_SPECIAL_DISCOUNTS) {
        return "\u0935\u093f\u0936\u0947\u0937 \u091b\u0942\u091f \u092a\u093e\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0905\u092d\u0940 \u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SETUP_NOW) {
        return "\u0905\u092d\u0940 \u0938\u0947\u091f\u0905\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GO_TO_VEHICLE_DETAILS) {
        return "\u0935\u093e\u0939\u0928 \u0935\u093f\u0935\u0930\u0923 \u092a\u0930 \u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.CLOSE) {
        return "\u092c\u0902\u0926 \u0915\u0930\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.RC_DEACTIVATED) {
        return "\u0906\u0930\u0938\u0940 \u0928\u093f\u0937\u094d\u0915\u094d\u0930\u093f\u092f \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.RC_DEACTIVATED_DETAILS) {
        return "\u0906\u092a\u0915\u0940 \u0906\u0930\u0938\u0940 \u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u092e\u0947\u0902 \u0915\u093f\u0938\u0940 \u0905\u0928\u094d\u092f \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0915\u0947 \u0916\u093e\u0924\u0947 \u092a\u0930 \u0938\u0915\u094d\u0930\u093f\u092f \u0939\u0948\u0964 \u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u093e \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0906\u0930\u0938\u0940 \u0938\u0915\u094d\u0930\u093f\u092f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_MOBILITY) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u0940 \u0917\u0924\u093f\u0936\u0940\u0932\u0924\u093e \u0915\u092e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_DISABILITY) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u0940 \u0935\u093f\u0915\u0932\u093e\u0902\u0917\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_VISION) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u0940 \u0915\u092e \u0926\u0943\u0937\u094d\u091f\u093f \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_HEARING_IMPAIRMENT) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u094b \u0915\u092e \u0938\u0941\u0928\u093e\u0908 \u0926\u0947\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.HELP_WITH_THEIR_MOBILITY_AID) {
        return "\u0909\u0928\u0915\u0940 \u0917\u0924\u093f\u0936\u0940\u0932\u0924\u093e \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_ASSIST_THEM_IF_NEEDED) {
        return "\u0906\u0935\u0936\u094d\u092f\u0915\u0924\u093e \u092a\u0921\u093c\u0928\u0947 \u092a\u0930 \u0909\u0928\u0915\u0940 \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.MESSAGE_THEM_AT_PICKUP) {
        return "\u092a\u093f\u0915\u0905\u092a \u092a\u0930 \u0909\u0928\u094d\u0939\u0947\u0902 \u0938\u0902\u0926\u0947\u0936 \u092d\u0947\u091c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SOUND_HORN_ONCE_AT_PICKUP) {
        return "\u092a\u093f\u0915\u0905\u092a \u092a\u0930 \u090f\u0915 \u092c\u093e\u0930 \u0939\u0949\u0930\u094d\u0928 \u092c\u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_CALL_AND_AVOID_CHATS) {
        return "\u0915\u0943\u092a\u092f\u093e \u0915\u0949\u0932 \u0915\u0930\u0947\u0902 \u0914\u0930 \u091a\u0948\u091f \u0938\u0947 \u092c\u091a\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_CHAT_AND_AVOID_CALLS) {
        return "\u0915\u0943\u092a\u092f\u093e \u091a\u0948\u091f \u0915\u0930\u0947\u0902 \u0914\u0930 \u0915\u0949\u0932 \u0938\u0947 \u092c\u091a\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_GO_TO_EXACT_PICKUP) {
        return "\u0915\u0943\u092a\u092f\u093e \u0938\u091f\u0940\u0915 \u092a\u093f\u0915\u0905\u092a \u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_POOR_VISION_SOUND_HORN_AT_PICKUP) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u0940 \u0915\u092e \u0926\u0943\u0937\u094d\u091f\u093f \u0939\u0948. \u092a\u093f\u0915\u0905\u092a \u092a\u0930 \u090f\u0915 \u092c\u093e\u0930 \u0939\u0949\u0930\u094d\u0928 \u092c\u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_POOR_HEARING_MESSAGE_THEM_AT_PICKUP) {
        return "\u0930\u093e\u0907\u0921\u0930 \u0915\u0940 \u0915\u092e \u0938\u0941\u0928\u093e\u0908 \u0926\u0947\u0924\u0940 \u0939\u0948. \u092a\u093f\u0915\u0905\u092a \u092a\u0930 \u0909\u0928\u094d\u0939\u0947\u0902 \u0938\u0902\u0926\u0947\u0936 \u092d\u0947\u091c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_MOBILITY_STORE_THEIR_SUPPORT_AT_PICKUP) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u0940 \u0915\u092e \u0917\u0924\u093f\u0936\u0940\u0932\u0924\u093e \u0939\u0948. \u0915\u0943\u092a\u092f\u093e \u0909\u0928\u0915\u0940 \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0930\u0947\u0902 \u0914\u0930 \u092a\u0948\u0938\u0947\u0902\u091c\u0930 \u0938\u0940\u091f \u0915\u0947 \u092a\u0940\u091b\u0947 \u0909\u0928\u0915\u093e \u0938\u092e\u0930\u094d\u0925\u0928 \u0938\u094d\u091f\u094b\u0930 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_DISABILITY_PLEASE_ASSIST_THEM) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u092e\u0947\u0902 \u090f\u0915 \u0935\u093f\u0915\u0932\u093e\u0902\u0917\u0924\u093e \u0939\u0948. \u0915\u0943\u092a\u092f\u093e \u0909\u0928\u0915\u0940 \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0930\u0947\u0902 \u091c\u0948\u0938\u0947 \u0906\u092a \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_MAY_NEED_ASSISTANCE) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u094b \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0940 \u0906\u0935\u0936\u094d\u092f\u0915\u0924\u093e \u0939\u094b \u0938\u0915\u0924\u0940 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.LEARN_MORE) {
        return "\u0914\u0930 \u0905\u0927\u093f\u0915 \u091c\u093e\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_MOBILITY_GO_TO_EXACT_LOC) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u0940 \u0915\u092e \u0917\u0924\u093f\u0936\u0940\u0932\u0924\u093e \u0939\u0948. \u0915\u0943\u092a\u092f\u093e \u0938\u091f\u0940\u0915 \u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_POOR_HEARING_CHAT_WITH_THEM_INSTEAD_OF_CALLING) {
        return "\u0930\u093e\u0907\u0921\u0930 \u0915\u0940 \u0915\u092e \u0938\u0941\u0928\u093e\u0908 \u0926\u0947\u0924\u0940 \u0939\u0948. \u0915\u0943\u092a\u092f\u093e \u0909\u0928\u0915\u0947 \u0938\u093e\u0925 \u091a\u0948\u091f \u0915\u0930\u0947\u0902 \u0915\u0949\u0932 \u0915\u0940 \u092c\u091c\u093e\u092f";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_VISION_CALL_THEM_INSTEAD_OF_CHATTING) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0915\u0940 \u0915\u092e \u0926\u0943\u0937\u094d\u091f\u093f \u0939\u0948. \u0915\u0943\u092a\u092f\u093e \u091a\u0948\u091f \u0915\u0940 \u092c\u091c\u093e\u092f \u0909\u0928\u094d\u0939\u0947\u0902 \u0915\u0949\u0932 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_HELP_THEM_AS_YOU_CAN) {
        return "\u0915\u0943\u092a\u092f\u093e \u092f\u0925\u093e\u0938\u0902\u092d\u0935 \u0909\u0928\u0915\u0940 \u092e\u0926\u0926 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE) {
        return "\u091c\u093e\u0928\u0947\u0902 \u0915\u093f \u0906\u092a \u0935\u093f\u0936\u0947\u0937 \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0940 \u0906\u0935\u0936\u094d\u092f\u0915\u0924\u093e \u0935\u093e\u0932\u0947 \u0917\u094d\u0930\u093e\u0939\u0915\u094b\u0902 \u0915\u0940 \u092e\u0926\u0926 \u0915\u0948\u0938\u0947 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.ASSISTANCE_REQUIRED) {
        return "\u0938\u0939\u093e\u092f\u0924\u093e \u0906\u0935\u0936\u094d\u092f\u0915 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.SAVED_DUE_TO_ZERO_COMMISSION) {
        return "\u0936\u0942\u0928\u094d\u092f \u0915\u092e\u0940\u0936\u0928 \u0915\u0947\x0a\u0915\u093e\u0930\u0923 \u092c\u091a \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.TIP_EARNED_FROM_CUSTOMER) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0938\u0947 \u091f\u093f\u092a\x0a\u0905\u0930\u094d\u091c\u093f\u0924 \u0915\u0940 \u0917\u0908";
    };
    if (stringKey instanceof Language_Types.COLLECT_VIA_CASE_UPI) {
        return "\u0928\u0915\u0926/\u092f\u0942\u092a\u0940\u0906\u0908 \u0915\u0947 \u092e\u093e\u0927\u094d\u092f\u092e \u0938\u0947 \u090f\u0915\u0924\u094d\u0930 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.FARE_COLLECTED) {
        return "\u0915\u093f\u0930\u093e\u092f\u093e \u090f\u0915\u0924\u094d\u0930 \u0915\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.RATE_YOUR_RIDE_WITH1) {
        return "";
    };
    if (stringKey instanceof Language_Types.RATE_YOUR_RIDE_WITH2) {
        return "\u0915\u0947 \u0938\u093e\u0925 \u0905\u092a\u0928\u0940 \u0930\u093e\u0907\u0921 \u0915\u094b \u0930\u0947\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.HELP_US_WITH_YOUR_FEEDBACK) {
        return "\u0906\u092a\u0915\u0940 \u092a\u094d\u0930\u0924\u093f\u0915\u094d\u0930\u093f\u092f\u093e \u0938\u0947 \u0939\u092e\u0947\u0902 \u0938\u0939\u093e\u092f\u0924\u093e \u092e\u093f\u0932\u0924\u0940 \u0939\u0948 (\u0935\u0948\u0915\u0932\u094d\u092a\u093f\u0915)";
    };
    if (stringKey instanceof Language_Types.COLLECT_CASH) {
        return "\u0928\u0915\u0926 \u0907\u0915\u091f\u094d\u0920\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ONLINE_PAYMENT) {
        return "\u0911\u0928\u0932\u093e\u0907\u0928 \u092d\u0941\u0917\u0924\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.RIDE_COMPLETED) {
        return "\u0938\u0935\u093e\u0930\u0940 \u092a\u0942\u0930\u0940 \u0939\u0941\u0908";
    };
    if (stringKey instanceof Language_Types.SUBMIT_FEEDBACK) {
        return "\u092a\u094d\u0930\u0924\u093f\u092a\u0941\u0937\u094d\u091f\u093f \u0926\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.BADGE_EARNED) {
        return "\u092c\u0948\u091c \u0905\u0930\u094d\u091c\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.PURPLE_RIDE_CHAMPION) {
        return "\u092a\u0930\u094d\u092a\u0932 \u0930\u093e\u0907\u0921 \u091a\u0948\u0902\u092a\u093f\u092f\u0928";
    };
    if (stringKey instanceof Language_Types.PURPLE_RIDE) {
        return "\u092a\u0930\u094d\u092a\u0932 \u0930\u093e\u0907\u0921";
    };
    if (stringKey instanceof Language_Types.PROCEED_TO_CHAT) {
        return "\u091a\u0948\u091f \u0915\u0947 \u0938\u093e\u0925 \u0906\u0917\u0947 \u092c\u0922\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_CONSIDER_CALLING_THEM) {
        return "\u0939\u094b \u0938\u0915\u0924\u093e \u0939\u0948 \u0915\u093f \u0935\u0947 \u091a\u0948\u091f \u092a\u095d\u0928\u0947 \u092e\u0947 \u0938\u0915\u094d\u0937\u092e \u0928 \u0939\u094b. \u0915\u0943\u092a\u092f\u093e \u0909\u0928\u094d\u0939\u0947\u0902 \u0915\u0949\u0932 \u0915\u0930\u0928\u0947 \u092a\u0930 \u0935\u093f\u091a\u093e\u0930 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.JOIN_A_PLAN_TO_START_EARNING) {
        return "\u0915\u092e\u093e\u0908 \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u090f\u0915 \u092f\u094b\u091c\u0928\u093e \u092e\u0947\u0902 \u0936\u093e\u092e\u093f\u0932 \u0939\u094b\u0902!";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE_PROMPT_SUBSCRIBE) {
        return "\u0906\u092a \u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u092e\u0947\u0902 \u0911\u092b\u093c\u0932\u093e\u0907\u0928 \u0939\u0948\u0902\u0964\x0a\u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u090f\u0915 \u092f\u094b\u091c\u0928\u093e \u092e\u0947\u0902 \u0936\u093e\u092e\u093f\u0932 \u0939\u094b\u0902|";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE_PROMPT_PAYMENT_PENDING) {
        return "\u0906\u092a \u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u092e\u0947\u0902 \u0911\u092b\u093c\u0932\u093e\u0907\u0928 \u0939\u0948\u0902\u0964\x0a\u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0905\u092a\u0928\u0940 \u092f\u094b\u091c\u0928\u093e \u0915\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u0942\u0930\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.COMPLETE_PAYMENT_TO_CONTINUE) {
        return "\u092f\u093e\u0924\u094d\u0930\u0940 \u0938\u093e\u0925\u0940 \u0915\u093e \u0909\u092a\u092f\u094b\u0917 \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f, \u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0928\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u0942\u0930\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PENDING_CAPS) {
        return "\u0936\u0947\u0937";
    };
    if (stringKey instanceof Language_Types.FAILURE) {
        return "\u0905\u0938\u092b\u0932";
    };
    if (stringKey instanceof Language_Types.PAYMENT_MODE) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u092e\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.TXN_ID) {
        return "\u0911\u0930\u094d\u0921\u0930 \u0906\u0908\u0921\u0940";
    };
    if (stringKey instanceof Language_Types.AMOUNT_PAID) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0930\u093e\u0936\u093f";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_SCHEDULED) {
        return "\u0905\u0927\u093f\u0938\u0942\u091a\u0928\u093e \u0905\u0928\u0941\u0938\u0942\u091a\u093f\u0924!";
    };
    if (stringKey instanceof Language_Types.MANUAL_DUES) {
        return "\u092e\u0948\u0928\u0941\u0905\u0932 \u0926\u0947\u092f \u0930\u093e\u0936\u093f";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_IN_PROGRESS) {
        return "\u0911\u091f\u094b\u092a\u0947 \u092a\u094d\u0930\u0915\u094d\u0930\u093f\u092f\u093e \u091c\u093e\u0930\u0940 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.MANUAL_DUE_OVERVIEW) {
        return "\u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u0926\u0947\u092f \u0905\u0935\u0932\u094b\u0915\u0928";
    };
    if (stringKey instanceof Language_Types.MANUAL_DUE_AS_AUTOPAY_EXECUTION_FAILED) {
        return "\u0911\u091f\u094b\u092a\u0947 \u092a\u094d\u0930\u0915\u094d\u0930\u093f\u092f\u093e \u0915\u0947 \u0905\u0938\u092b\u0932 \u0939\u094b\u0928\u0947 \u0915\u0947 \u0915\u093e\u0930\u0923 \u092e\u0948\u0928\u0941\u0905\u0932 \u0926\u094d\u0935\u093e\u0930\u093e \u0926\u0947\u092f \u0930\u093e\u0936\u093f";
    };
    if (stringKey instanceof Language_Types.CLEAR_MANUAL_DUES) {
        return "\u092e\u0948\u0928\u0941\u0905\u0932 \u0926\u094d\u0935\u093e\u0930\u093e \u0926\u0947\u092f \u0930\u093e\u0936\u093f \u0915\u094b \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0902|";
    };
    if (stringKey instanceof Language_Types.MANUAL_DUE_DETAILS) {
        return "\u092e\u0948\u0928\u0941\u0905\u0932 \u0926\u0947\u092f \u0930\u093e\u0936\u093f \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_DUE_DETAILS) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0926\u0947\u092f \u0930\u093e\u0936\u093f \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.SWITCHED_TO_MANUAL) {
        return "*\u092d\u0941\u0917\u0924\u093e\u0928 \u092e\u094b\u0921 \u0911\u091f\u094b\u092a\u0947 \u0938\u0947 \u092e\u0948\u0928\u0941\u0905\u0932 \u092e\u094b\u0921 \u092a\u0930 \u092c\u0926\u0932\u093e \u0917\u092f\u093e \u0939\u0948 \u0915\u094d\u092f\u094b\u0902\u0915\u093f \u0911\u091f\u094b\u092a\u0947 \u092a\u0942\u0930\u093e \u0928\u0939\u0940\u0902 \u0939\u094b \u0938\u0915\u093e|";
    };
    if (stringKey instanceof Language_Types.SPLIT_PAYMENT) {
        return "*\u0911\u091f\u094b\u092a\u0947 \u0915\u094d\u0930\u093f\u092f\u093e\u0928\u094d\u0935\u093f\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u094b \u0935\u093f\u092d\u093e\u091c\u093f\u0924 \u0915\u093f\u092f\u093e \u0917\u092f\u093e \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.GST_INCLUDE) {
        return "(18% \u091c\u0940\u090f\u0938\u091f\u0940 \u0938\u0939\u093f\u0924)";
    };
    if (stringKey instanceof Language_Types.SCHEDULED_AT) {
        return "\u0938\u092e\u092f \u092a\u0930 \u0928\u093f\u0930\u094d\u0927\u093e\u0930\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.PAYMENT_STATUS) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0938\u094d\u0925\u093f\u0924\u093f";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_ATTEMPTING) {
        return "\u0905\u0927\u093f\u0938\u0942\u091a\u0928\u093e \u0915\u093e \u092a\u094d\u0930\u092f\u093e\u0938";
    };
    if (stringKey instanceof Language_Types.EXECUTION_SCHEDULED) {
        return "\u0915\u094d\u0930\u093f\u092f\u093e\u0928\u094d\u0935\u093f\u0924 \u0939\u094b\u0928\u0947 \u0915\u0940 \u092f\u094b\u091c\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.EXECUTION_ATTEMPTING) {
        return "\u0915\u094d\u0930\u093f\u092f\u093e\u0928\u094d\u0935\u093f\u0924 \u0915\u093e \u092a\u094d\u0930\u092f\u093e\u0938 \u0939\u094b \u0930\u0939\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.EXECUTION_SUCCESS) {
        return "\u0915\u094d\u0930\u093f\u092f\u093e\u0928\u094d\u0935\u093f\u0924 \u0938\u092b\u0932 \u0939\u0941\u0908";
    };
    if (stringKey instanceof Language_Types.SCHEDULED) {
        return "\u0928\u093f\u0930\u094d\u0927\u093e\u0930\u093f\u0924";
    };
    if (stringKey instanceof Language_Types.ONE_TIME_SETTLEMENT) {
        return "\u090f\u0915 \u092c\u093e\u0930 \u092e\u0947\u0902 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0902|";
    };
    if (stringKey instanceof Language_Types.PAYMENT_SCHEDULED) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0928\u093f\u0930\u094d\u0927\u093e\u0930\u093f\u0924 \u0939\u0941\u0906 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.RETRY_AUTOPAY) {
        return "\u0911\u091f\u094b\u092a\u0947 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902 ";
    };
    if (stringKey instanceof Language_Types.RETRY_STR) {
        return "\u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902 ";
    };
    if (stringKey instanceof Language_Types.ONGOING_PAYMENT_EXECUTION) {
        return "\u092e\u0948\u0928\u094d\u092f\u0941\u0905\u0932 \u092d\u0941\u0917\u0924\u093e\u0928 \u091c\u093e\u0930\u0940 \u0939\u0948, \u0915\u0943\u092a\u092f\u093e \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.OFFER_CARD_BANNER_TITLE) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0915\u0930\u0947\u0902 \u0914\u0930 {} \u0924\u0915 \u0915\u0947\u0935\u0932 \u20b915/\u0926\u093f\u0928 \u092a\u0947 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.OFFER_CARD_BANNER_DESC) {
        return "1 \u0928\u0935\u0902\u092c\u0930 \u0924\u0915 \u0915\u0947\u0935\u0932 \u20b915/\u0926\u093f\u0928 \u092a\u0947 \u0915\u0930\u0947\u0902|";
    };
    if (stringKey instanceof Language_Types.OFFER_CARD_BANNER_ALERT) {
        return "\u0906\u092a\u0915\u093e \u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0936\u0947\u0937 \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.OR) {
        return "\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.COLLECT_CASH_DIRECTLY) {
        return "\u0938\u0940\u0927\u0947 \u0928\u0915\u0926 \u091c\u092e\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.OR_COLLECT_CASH_DIRECTLY) {
        return "\u092f\u093e \u0938\u0940\u0927\u0947 \u0928\u0915\u0926 \u091c\u092e\u093e \u0915\u0930\u0947\u0902|";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_TO_ACCEPT_PAYMENT) {
        return "\u0938\u0940\u0927\u0947 \u0905\u092a\u0928\u0947 \u092c\u0948\u0902\u0915 \u0916\u093e\u0924\u0947 \u092e\u0947\u0902 \u092d\u0941\u0917\u0924\u093e\u0928 \u0938\u094d\u0935\u0940\u0915\u093e\u0930 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f, \u0905\u092a\u0928\u0947 \u092a\u094d\u0932\u093e\u0928 \u092a\u0930 \u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DOWNLOAD_QR) {
        return "\u0915\u094d\u092f\u0942\u0906\u0930 \u0921\u093e\u0909\u0928\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.USE_THIS_QR_TO_COLLECT_PAYMENT) {
        return "\u0906\u092a \u0938\u0935\u093e\u0930\u0940 \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b\u0928\u0947 \u0915\u0947 \u092c\u093e\u0926 \u092d\u0941\u0917\u0924\u093e\u0928 \u090f\u0915\u0924\u094d\u0930 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0907\u0938 \u0915\u094d\u092f\u0942\u0906\u0930 \u0915\u094b\u0921 \u0915\u093e \u0909\u092a\u092f\u094b\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.AMOUNT_WILL_DEPOSITED_TO_BANK_ACCOUNT) {
        return "\u0930\u093e\u0936\u093f \u0909\u0938 \u092c\u0948\u0902\u0915 \u0916\u093e\u0924\u0947 \u092e\u0947\u0902 \u091c\u092e\u093e \u0915\u0940 \u091c\u093e\u090f\u0917\u0940 \u091c\u093f\u0938\u0947 \u0906\u092a UPI \u0911\u091f\u094b\u092a\u0947 \u0915\u0947 \u0932\u093f\u090f \u0909\u092a\u092f\u094b\u0917 \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.GET_DIRECTLY_TO_YOUR_BANK_ACCOUNT) {
        return "\u0905\u092a\u0928\u0947 \u092c\u0948\u0902\u0915 \u0916\u093e\u0924\u0947 \u092e\u0947\u0902 \u0938\u0940\u0927\u0947 \u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PAYMENT) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.QR_CODE) {
        return "\u0915\u094d\u092f\u0942\u0906\u0930 \u0915\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.GET_QR_CODE) {
        return "\u0915\u094d\u092f\u0942\u0906\u0930 \u0915\u094b\u0921 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.EXECUTION_FAILED) {
        return "\u0915\u094d\u0930\u093f\u092f\u093e\u0928\u094d\u0935\u093f\u0924\u093f \u0905\u0938\u092b\u0932 \u0939\u0941\u0908";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_FAILED) {
        return "\u0938\u0942\u091a\u0928\u093e \u0905\u0938\u092b\u0932 \u0939\u0941\u0908";
    };
    if (stringKey instanceof Language_Types.PAY_NOW) {
        return "\u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CLEAR_DUES_BANNER_TITLE) {
        return "\u0928\u0949\u0928-\u0938\u094d\u091f\u0949\u092a \u0938\u0935\u093e\u0930\u0940 \u0915\u093e \u0906\u0928\u0902\u0926 \u0932\u0947\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0936\u0947\u0937 \u0930\u093e\u0936\u093f(\u20b9{}) \u091a\u0941\u0915\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.COLLECT_VIA_UPI_QR_OR_CASH) {
        return "UPI,QR \u092f\u093e \u0928\u0915\u0926 \u0915\u0947 \u092e\u093e\u0927\u094d\u092f\u092e \u0938\u0947 \u091c\u092e\u093e \u0915\u0930\u0947\u0902|";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_DEBITED_ON) {
        return "\u0930\u093e\u0936\u093f \u0921\u0947\u092c\u093f\u091f \u0915\u0940 \u0924\u093f\u0925\u093f";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_ATTEMPTED_ON) {
        return "\u0921\u0947\u092c\u093f\u091f \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0940 \u0924\u093f\u0925\u093f";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_AND_PAYMENT_SUCCESSFUL) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0914\u0930 \u092d\u0941\u0917\u0924\u093e\u0928 \u0938\u092b\u0932!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_SUCCESSFUL) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0938\u092b\u0932!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_AND_PAYMENT_PENDING) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0914\u0930 \u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u094d\u0930\u0932\u0902\u092c\u093f\u0924!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_PENDING) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u092a\u094d\u0930\u0932\u0902\u092c\u093f\u0924!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_AND_PAYMENT_FAILED) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0914\u0930 \u092d\u0941\u0917\u0924\u093e\u0928 \u0905\u0938\u092b\u0932!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_FAILED) {
        return "\u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0905\u0938\u092b\u0932!";
    };
    if (stringKey instanceof Language_Types.ONE_TIME_REGISTERATION) {
        return "\u090f\u0915 \u092c\u093e\u0930 \u092a\u0902\u091c\u0940\u0915\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.CLEARANCE_AND_REGISTERATION) {
        return "\u0938\u094d\u0935\u0940\u0915\u0943\u0924\u093f \u0914\u0930 \u092a\u0902\u091c\u0940\u0915\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.UPI_AUTOPAY_SETUP) {
        return "UPI \u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a";
    };
    if (stringKey instanceof Language_Types.WATCH_VIDEO_FOR_HELP) {
        return "\u092e\u0926\u0926 \u0915\u0947 \u0932\u093f\u090f \u0935\u0940\u0921\u093f\u092f\u094b \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING_SOFT_NUDGE) {
        return "\u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u0938\u0940\u092e\u093e \u091c\u0932\u094d\u0926 \u0939\u0940 \u092a\u0942\u0930\u0940 \u0939\u094b \u091c\u093e\u090f\u0917\u0940. \u0928\u0949\u0928-\u0938\u094d\u091f\u0949\u092a \u0938\u0935\u093e\u0930\u0940 \u0915\u093e \u0906\u0928\u0902\u0926 \u0932\u0947\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u091a\u0941\u0915\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.CLEAR_YOUR_DUES_EARLY) {
        return "\u0905\u092a\u0928\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u091c\u0932\u094d\u0926\u0940 \u091a\u0941\u0915\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.DUE_LIMIT_WARNING_BANNER_TITLE) {
        return "\u0906\u092a\u0915\u0940 \u20b9{} \u0915\u0940 \u0926\u0947\u092f \u0938\u0940\u092e\u093e \u091c\u0932\u094d\u0926 \u0939\u0940 \u092a\u0942\u0930\u0940 \u0939\u094b \u091c\u093e\u090f\u0917\u0940";
    };
    if (stringKey instanceof Language_Types.SCHEDULED_ON) {
        return "\u0928\u093f\u0930\u094d\u0927\u093e\u0930\u093f\u0924 \u0924\u093f\u0925\u093f";
    };
    if (stringKey instanceof Language_Types.ATTEMPTED_ON) {
        return "\u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0940 \u0924\u093f\u0925\u093f";
    };
    if (stringKey instanceof Language_Types.DEBITED_ON) {
        return "\u0921\u0947\u092c\u093f\u091f \u0915\u0940 \u0924\u093e\u0930\u0940\u0916";
    };
    if (stringKey instanceof Language_Types.FREE_TRIAL_ENDING_IN_2_DAYS) {
        return "2 \u0926\u093f\u0928\u094b\u0902 \u092e\u0947\u0902 \u092e\u0941\u092b\u094d\u0924 \u092a\u0930\u0940\u0915\u094d\u0937\u0923 \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b \u0930\u0939\u093e \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.FREE_TRIAL_ENDING_TOMORROW) {
        return "\u0915\u0932 \u092e\u0941\u092b\u094d\u0924 \u092a\u0930\u0940\u0915\u094d\u0937\u0923 \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b \u0930\u0939\u093e \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.FREE_TRIAL_ENDS_TONIGHT) {
        return "\u0906\u091c \u0930\u093e\u0924 \u092e\u0941\u092b\u094d\u0924 \u092a\u0930\u0940\u0915\u094d\u0937\u0923 \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b\u0924\u093e \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.JOIN_A_PLAN_TO_CONTINUE_TAKING_RIDES) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u093e \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0940 \u092f\u094b\u091c\u0928\u093e \u092e\u0947\u0902 \u0936\u093e\u092e\u093f\u0932 \u0939\u094b\u0902";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_FOR_EASY_PAYMENTS) {
        return "\u0906\u0938\u093e\u0928 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0947 \u0932\u093f\u090f \u0911\u091f\u094b\u092a\u0947 \u0938\u0947\u091f\u0905\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LOW_DUES_CLEAR_POPUP_DESC) {
        return "\u0928\u0949\u0928-\u0938\u094d\u091f\u0949\u092a \u0938\u0935\u093e\u0930\u0940 \u0915\u093e \u0906\u0928\u0902\u0926 \u0932\u0947\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0905\u092d\u0940 \u0905\u092a\u0928\u0940 \u0936\u0947\u0937 \u0930\u093e\u0936\u093f \u091a\u0941\u0915\u093e\u090f\u0901";
    };
    if (stringKey instanceof Language_Types.DUES_PENDING) {
        return "\u26a0\ufe0f \u0926\u0947\u092f \u0930\u093e\u0936\u093f \u092a\u094d\u0930\u0932\u0902\u092c\u093f\u0924! \u26a0\ufe0f";
    };
    if (stringKey instanceof Language_Types.DAYS) {
        return "\u0926\u093f\u0928";
    };
    if (stringKey instanceof Language_Types.ACTIVE_PLAN) {
        return "\u0938\u0915\u094d\u0930\u093f\u092f \u092f\u094b\u091c\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.DOWNGRADE_AVAILABLE_ONLY_FOR_AC_VEHICLES) {
        return "\u0917\u093e\u0921\u093c\u0940 \u0915\u094b \u0921\u093e\u0909\u0928\u0917\u094d\u0930\u0947\u0921 \u0915\u0930\u0928\u0947 \u0915\u093e \u0935\u093f\u0915\u0932\u094d\u092a \u0915\u0947\u0935\u0932 \u090f\u0938\u0940 \u0935\u093e\u0939\u0928\u094b\u0902 \u0915\u0947 \u0932\u093f\u090f \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_1) {
        return "\u0905\u092a\u0928\u0947 \u0935\u093e\u0939\u0928 \u0915\u094b \u0905\u092a\u0917\u094d\u0930\u0947\u0921 \u0915\u0930\u0928\u0947 \u0938\u0947 \u0906\u092a ";
    };
    if (stringKey instanceof Language_Types.DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_2) {
        return "\u0915\u0940 \u0938\u0935\u093e\u0930\u0940 \u0914\u0930";
    };
    if (stringKey instanceof Language_Types.DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_3) {
        return " \u0915\u0940 \u0938\u0935\u093e\u0930\u0940 \u0926\u094b\u0928\u094b\u0902 \u0932\u0947 \u0938\u0915\u0947\u0902\u0917\u0947";
    };
    if (stringKey instanceof Language_Types.AC_CAB) {
        return "\u090f\u0938\u0940 \u0915\u0948\u092c";
    };
    if (stringKey instanceof Language_Types.AC_SUV) {
        return "\u090f\u0938\u0940 \u090f\u0938\u092f\u0942\u0935\u0940";
    };
    if (stringKey instanceof Language_Types.DOWNGRADE_VEHICLE) {
        return "\u0921\u093e\u0909\u0928\u0917\u094d\u0930\u0947\u0921 \u0935\u093e\u0939\u0928";
    };
    if (stringKey instanceof Language_Types.WHAT_ARE_PURPLE_RIDES) {
        return "\u092a\u0930\u094d\u092a\u0932 \u0938\u0935\u093e\u0930\u0940 \u0915\u094d\u092f\u093e \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.ECONOMICAL) {
        return "\u0915\u093f\u092b\u093c\u093e\u092f\u0924\u0940";
    };
    if (stringKey instanceof Language_Types.SPACIOUS) {
        return "\u0935\u093f\u0936\u093e\u0932";
    };
    if (stringKey instanceof Language_Types.COMFY) {
        return "\u0906\u0930\u093e\u092e\u0926\u093e\u092f\u0915";
    };
    if (stringKey instanceof Language_Types.PEOPLE) {
        return "\u0932\u094b\u0917";
    };
    if (stringKey instanceof Language_Types.GO_TO) {
        return "\u0917\u094b-\u091f\u0942";
    };
    if (stringKey instanceof Language_Types.SELECT_ON_MAP) {
        return "\u092e\u093e\u0928\u091a\u093f\u0924\u094d\u0930 \u092a\u0930 \u091a\u092f\u0928 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CONFIRM_LOCATION_STR) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0915\u0940 \u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SAVE_LOCATION_STR) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0938\u0939\u0947\u091c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.REMOVE_PREF_LOC) {
        return "\u092a\u0938\u0902\u0926\u0940\u0926\u093e \u0938\u094d\u0925\u093e\u0928 \u0939\u091f\u093e\u090f\u0901 ";
    };
    if (stringKey instanceof Language_Types.CONF_REMOVE_PREF_LOC) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a \u0935\u093e\u0915\u0908 \u092a\u0938\u0902\u0926\u0940\u0926\u093e \u0938\u094d\u0925\u093e\u0928 \u0939\u091f\u093e\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902?";
    };
    if (stringKey instanceof Language_Types.YES_REMOVE) {
        return "\u0939\u093e\u0902, \u0939\u091f\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_LOCATION) {
        return "\u0938\u094d\u0925\u093e\u0928 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_ANOTHER_LOCATION) {
        return "\u0905\u0928\u094d\u092f \u0938\u094d\u0925\u093e\u0928 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ADD_A_GOTO_LOC) {
        return "\u091c\u093e\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0938\u094d\u0925\u093e\u0928 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_LEFT) {
        return "\u0917\u094b-\u091f\u0942 \u0938\u094d\u0925\u093e\u0928 \u0936\u0947\u0937 \u0939\u0948\u0902:";
    };
    if (stringKey instanceof Language_Types.CURRENT_LOCATION) {
        return "\u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u0938\u094d\u0925\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.CONF_GOTO_LOC) {
        return "\u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u091c\u093e\u0928\u0947 \u0915\u0940 \u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GOTO_LOCS) {
        return "\u0938\u094d\u0925\u093e\u0928\u094b\u0902 \u092a\u0930 \u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.LOCATION_STR) {
        return "\u0938\u094d\u0925\u093e\u0928";
    };
    if (stringKey instanceof Language_Types.ADD_TAG) {
        return "\u091f\u0948\u0917 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ONLY_ONE_LOC_CAN_ADDED) {
        return "(\u0915\u0947\u0935\u0932 \u090f\u0915 \u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u092f\u0939 \u091f\u0948\u0917 \u0939\u094b \u0938\u0915\u0924\u093e \u0939\u0948)";
    };
    if (stringKey instanceof Language_Types.SAVE_AS) {
        return "\u0907\u0938 \u0930\u0942\u092a \u092e\u0947\u0902 \u0938\u0939\u0947\u091c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NO_GOTO_LOC_ADDED) {
        return "\u0905\u092d\u0940 \u0924\u0915 \u0915\u094b\u0908 \u0917\u094b-\u091f\u0942 \u0938\u094d\u0925\u093e\u0928 \u0928\u0939\u0940\u0902 \u091c\u094b\u0921\u093c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_HELPS_YOU) {
        return "\u0917\u094b-\u091f\u0942 \u0932\u094b\u0915\u0947\u0936\u0928 \u0906\u092a\u0915\u094b \u0905\u092a\u0928\u0947 \u092a\u0938\u0902\u0926\u0940\u0926\u093e \u0938\u094d\u0925\u093e\u0928\u094b\u0902 \u0914\u0930 \u0909\u0938\u0915\u0947 \u0906\u0938\u092a\u093e\u0938 \u0938\u0935\u093e\u0930\u0940 \u0922\u0942\u0902\u0922\u0928\u0947 \u092e\u0947\u0902 \u092e\u0926\u0926 \u0915\u0930\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_VERY_CLOSE) {
        return "\u0906\u092a \u201c\u0917\u094b-\u091f\u0942\u201d \u0938\u094d\u0925\u093e\u0928 \u0915\u0947 \u092c\u0939\u0941\u0924 \u0915\u0930\u0940\u092c \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.GOTO_IS_APPLICABLE_FOR) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0909\u0928 \u0938\u094d\u0925\u093e\u0928\u094b\u0902 \u0915\u0947 \u0932\u093f\u090f \u0932\u093e\u0917\u0942 \u0939\u0948 \x0a \u091c\u094b \u0906\u092a\u0915\u0947 \u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u0938\u094d\u0925\u093e\u0928 \x0a \u0938\u0947 \u0915\u092e \u0938\u0947 \u0915\u092e 3 \u0915\u093f\u092e\u0940 \u0926\u0942\u0930 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.GOTO_MAYBE_REDUCED) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0915\u092e \u0939\u094b \u0938\u0915\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.CANCEL_OF_GOTO) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0938\u0935\u093e\u0930\u0940 \u0915\u094b \u0930\u0926\u094d\u0926 \u0915\u0930\u0928\u0947 \u0938\u0947 \u0906\u092a\u0915\u0947 \u0932\u093f\u090f \u201c\u0917\u094b-\u091f\u0942\u201d \u0935\u093f\u0915\u0932\u094d\u092a \u0915\u092e \u0939\u094b \u091c\u093e\u090f\u0902\u0917\u0947!";
    };
    if (stringKey instanceof Language_Types.MORE_GOTO_RIDE_COMING) {
        return "\u0905\u0927\u093f\u0915 \u201c\u0917\u094b-\u091f\u0942\u201d \u0938\u0935\u093e\u0930\u0940 \u0906 \u0930\u0939\u0940 \u0939\u0948\u0902!";
    };
    if (stringKey instanceof Language_Types.MORE_GOTO_RIDE_COMING_DESC) {
        return "\u0915\u0943\u092a\u092f\u093e \u0911\u0928\u0932\u093e\u0907\u0928 \u0930\u0939\u0947\u0902; \u0939\u092e \u0906\u092a\u0915\u0947 \u201c\u0917\u094b-\u091f\u0942\u201d \u0938\u094d\u0925\u093e\u0928 \u0915\u0947 \u0932\u093f\u090f \u0905\u0927\u093f\u0915 \u0938\u0935\u093e\u0930\u0940 \u0905\u0928\u0941\u0930\u094b\u0927 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930 \u0930\u0939\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.GOTO_REDUCED_TO_ZERO) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0915\u094b \u0918\u091f\u093e\u0915\u0930 \u0936\u0942\u0928\u094d\u092f \u0915\u0930 \u0926\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.DUE_TO_MULTIPLE_CANCELLATIONS) {
        return "\u090f\u0915\u093e\u0927\u093f\u0915 \u0930\u0926\u094d\u0926\u0940\u0915\u0930\u0923 \u0915\u0947 \u0915\u093e\u0930\u0923, \u0917\u093f\u0928\u0924\u0940 \u0915\u092e \u0915\u0930 \u0926\u0940 \u0917\u0908 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.OK_GOT_IT) {
        return "\u0920\u0940\u0915 \u0939\u0948, \u0938\u092e\u091d \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.GOTO_REDUCED_TO) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0915\u094b \u0918\u091f\u093e\u0915\u0930";
    };
    if (stringKey instanceof Language_Types.VALIDITY_EXPIRED_DESC) {
        return "\u0906\u092a\u0915\u0940 30 \u092e\u093f\u0928\u091f \u0915\u0940 \u0935\u0948\u0927\u0924\u093e \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u094b \u0917\u0908 \u0939\u0948\u0964 \u0939\u092e \u0907\u0938 \u0938\u092e\u092f \u0906\u092a\u0915\u0947 \u0905\u0928\u0941\u0930\u094b\u0927 \u0915\u094b \u092a\u0942\u0930\u093e \u0915\u0930\u0928\u0947 \u092e\u0947\u0902 \u0905\u0938\u092e\u0930\u094d\u0925 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.KNOW_MORE) {
        return "\u0905\u0927\u093f\u0915 \u091c\u093e\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.THIS_FEATURE_WILL_BE_APPLICABLE) {
        return "\u092f\u0939 \u0938\u0941\u0935\u093f\u0927\u093e \u0924\u092c \u0932\u093e\u0917\u0942 \u0939\u094b\u0917\u0940 \u091c\u092c \u0906\u092a \u0905\u092a\u0928\u0947 \u0935\u0930\u094d\u0924\u092e\u093e\u0928 \u0938\u094d\u0925\u093e\u0928 \u0938\u0947 \u0915\u092e \u0938\u0947 \u0915\u092e 3 \u0915\u093f\u092e\u0940 \u0926\u0942\u0930 \u0939\u094b\u0902\u0964\x0a\x0a\u0938\u094d\u0925\u093e\u0928 \u092a\u094d\u0930\u093e\u0925\u092e\u093f\u0915\u0924\u093e\u090f\u0902 \u0938\u0915\u094d\u0930\u093f\u092f \u0939\u094b\u0928\u0947 \u092a\u0930 \u0915\u0947\u0935\u0932 30 \u092e\u093f\u0928\u091f \u0915\u0947 \u0932\u093f\u090f \u0935\u0948\u0927 \u0939\u094b\u0924\u0940 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_ADDED) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0915\u093e \u0938\u094d\u0925\u093e\u0928 \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u091c\u094b\u0921\u093c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_REMOVED) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u0939\u091f\u093e \u0926\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_UPDATED) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0915\u093e \u0938\u094d\u0925\u093e\u0928 \u0938\u092b\u0932\u0924\u093e\u092a\u0942\u0930\u094d\u0935\u0915 \u0905\u092a\u0921\u0947\u091f \u0915\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_IS_ENABLED) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0932\u094b\u0915\u0947\u0936\u0928 \u0938\u0915\u094d\u0937\u092e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_IS_DISABLED) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0905\u0915\u094d\u0937\u092e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.GOTO_LOCATIONS) {
        return "\u0917\u094b-\u091f\u0942 \u0932\u094b\u0915\u0947\u0936\u0928";
    };
    if (stringKey instanceof Language_Types.CHOOSE_A_GOTO_LOC) {
        return "\u0906\u0928\u0947-\u091c\u093e\u0928\u0947 \u0915\u093e \u0938\u094d\u0925\u093e\u0928 \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOU_HAVE_ONLY_LEFT_FOR_TODAY) {
        return "\u0906\u092a \u0915\u0947\u0935\u0932 \u0906\u091c \u0915\u0947 \u0932\u093f\u090f \u0939\u0940 \u0928\u093f\u0915\u0932\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.YES_ENABLE) {
        return "\u0939\u093e\u0901, \u0938\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NO_GOTO_LOCS_ADDED_YET) {
        return "\u0905\u092d\u0940 \u0924\u0915 \u0915\u094b\u0908 \u201c\u0917\u094b-\u091f\u0942\u201d \u0938\u094d\u0925\u093e\u0928 \u0928\u0939\u0940\u0902 \u091c\u094b\u0921\u093c\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.NO_GOTO_LOCS_ADDED_YET_DESC) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0932\u094b\u0915\u0947\u0936\u0928 \u0906\u092a\u0915\u094b \u0905\u092a\u0928\u0947 \u092a\u0938\u0902\u0926\u0940\u0926\u093e \u0938\u094d\u0925\u093e\u0928\u094b\u0902 \u0914\u0930 \u0909\u0938\u0915\u0947 \u0906\u0938\u092a\u093e\u0938 \u0938\u0935\u093e\u0930\u0940 \u0922\u0942\u0902\u0922\u0928\u0947 \u092e\u0947\u0902 \u092e\u0926\u0926 \u0915\u0930\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.ENABLE_GOTO) {
        return "\u0917\u094b-\u091f\u0942 \u0938\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GO_TO_CANCELLATION_TITLE) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0915\u092e \u0915\u093f\u092f\u093e \u091c\u093e \u0938\u0915\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.GO_TO_CANCELLATION_DESC) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0938\u0935\u093e\u0930\u0940 \u0915\u094b \u0930\u0926\u094d\u0926 \u0915\u0930\u0928\u0947 \u0938\u0947 \u0906\u092a\u0915\u0947 \u0932\u093f\u090f \u201c\u0917\u094b-\u091f\u0942\u201d \u0935\u093f\u0915\u0932\u094d\u092a \u0915\u092e \u0939\u094b \u091c\u093e\u090f\u0902\u0917\u0947!";
    };
    if (stringKey instanceof Language_Types.REPORT_ISSUE) {
        return "\u0930\u093f\u092a\u094b\u0930\u094d\u091f \u092e\u0941\u0926\u094d\u0926\u093e";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ALMOST_AT_LOCATION) {
        return "\u0906\u092a \u0932\u0917\u092d\u0917 \u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u091c\u093e\u0928\u0947 \u0935\u093e\u0932\u0947 \u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u0939\u0948\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_NOT_FOUND) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0928\u0939\u0940\u0902 \u092e\u093f\u0932\u093e";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_DOES_NOT_EXIST) {
        return "\u0938\u094d\u0925\u093e\u0928 \u092e\u094c\u091c\u0942\u0926 \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_LIMIT_REACHED) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0938\u0940\u092e\u093e \u092a\u0942\u0930\u0940 \u0939\u094b \u0917\u0908";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_NOT_FOUND) {
        return "\u0905\u0928\u0941\u0930\u094b\u0927 \u0928\u0939\u0940\u0902 \u092e\u093f\u0932\u093e";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST) {
        return "\u0905\u0928\u0941\u0930\u094b\u0927 \u092e\u094c\u091c\u0942\u0926 \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED) {
        return "\u0926\u0948\u0928\u093f\u0915 \u0909\u092a\u092f\u094b\u0917 \u0938\u0940\u092e\u093e \u092a\u0942\u0930\u0940 \u0939\u094b \u0917\u0908";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE) {
        return "\u092a\u0939\u0932\u0947 \u0938\u0947 \u0939\u0940 \u0938\u0915\u094d\u0930\u093f\u092f";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_OUTSIDE_SERVICE_AREA) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0938\u0947\u0935\u093e \u0915\u094d\u0937\u0947\u0924\u094d\u0930 \u0915\u0947 \u092c\u093e\u0939\u0930 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.NEW_LOCATION_TOO_CLOSE_TO_PREVIOUS_HOME_LOCATION) {
        return "\u0938\u094d\u0925\u093e\u0928 \u092a\u093f\u091b\u0932\u0947 \u0938\u094d\u0925\u093e\u0928 \u0915\u0947 \u092c\u0939\u0941\u0924 \u0915\u0930\u0940\u092c \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_DOES_NOT_BELONG_TO_DRIVER) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0906\u092a\u0915\u093e \u0928\u0939\u0940\u0902 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_DELETE_WHILE_ACTIVE_ERROR) {
        return "\u0938\u0915\u094d\u0930\u093f\u092f \u0930\u0939\u0924\u0947 \u0939\u0941\u090f \u0938\u094d\u0925\u093e\u0928 \u0928\u0939\u0940\u0902 \u0939\u091f\u093e\u092f\u093e \u091c\u093e \u0938\u0915\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.DISABLE_GOTO_STR) {
        return "\u091c\u093e\u0928\u0947-\u091c\u093e\u0928\u0947 \u0915\u094b \u0905\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902?";
    };
    if (stringKey instanceof Language_Types.YOU_STILL_HAVE_TIME_LEFT) {
        return "\u0906\u092a\u0915\u0947 \u201c\u0917\u094b-\u091f\u0942\u201d \u0915\u0947 \u0905\u0928\u0941\u0930\u094b\u0927 \u092a\u0930 \u0905\u092d\u0940 \u092d\u0940 \u0938\u092e\u092f \u092c\u091a\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.YES_DISABLE) {
        return "\u0939\u093e\u0901, \u0905\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DRAG_TO_ADJUST) {
        return "\u0905\u092a\u0928\u093e \u0938\u094d\u0925\u093e\u0928 \u0938\u092e\u093e\u092f\u094b\u091c\u093f\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0916\u0940\u0902\u091a\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LOCATION_ALREADY_EXISTS) {
        return "\u0938\u094d\u0925\u093e\u0928 \u092a\u0939\u0932\u0947 \u0938\u0947 \u092e\u094c\u091c\u0942\u0926 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.MIN_LEFT) {
        return "\u092e\u093f\u0928\u091f \u0936\u0947\u0937";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_REACHED) {
        return "\u201c\u0917\u094b-\u091f\u0942\u201d \u0938\u094d\u0925\u093e\u0928 \u092a\u0930 \u092a\u0939\u0941\u0901\u091a \u0917\u092f\u093e!";
    };
    if (stringKey instanceof Language_Types.GET_READY_FOR_YS_SUBSCRIPTION) {
        return "\u092f\u093e\u0924\u094d\u0930\u0940 \u0938\u093e\u0925\u0940 \u092f\u094b\u091c\u0928\u093e\u0913\u0902 \u0915\u0947 \u0932\u093f\u090f \u0924\u0948\u092f\u093e\u0930 \u0939\u094b \u091c\u093e\u090f\u0902!";
    };
    if (stringKey instanceof Language_Types.SIGNUP_EARLY_FOR_SPECIAL_OFFERS) {
        return "\u0935\u093f\u0936\u0947\u0937 \u0911\u092b\u0930 \u0915\u0947 \u0932\u093f\u090f \u091c\u0932\u094d\u0926\u0940 \u0938\u093e\u0907\u0928 \u0905\u092a \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GUARANTEED_FIXED_PRICE) {
        return "1 \u091c\u0928\u0935\u0930\u0940 2025 \u0924\u0915 \u0915\u0940\u092e\u0924 \u092e\u0947\u0902 \u0915\u094b\u0908 \u092c\u0926\u0932\u093e\u0935 \u0928\u0939\u0940\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.INTRODUCTORY_OFFER_TO_BE_ANNOUNCED_SOON) {
        return "\u092a\u094d\u0930\u093e\u0930\u0902\u092d\u093f\u0915 \u092a\u094d\u0930\u0938\u094d\u0924\u093e\u0935 \u091c\u0932\u094d\u0926 \u0939\u0940 \u0918\u094b\u0937\u093f\u0924 \u0915\u093f\u092f\u093e \u091c\u093e\u090f\u0917\u093e!";
    };
    if (stringKey instanceof Language_Types.NO_CHARGES_TILL) {
        return "31 \u092e\u093e\u0930\u094d\u091a, 2024 \u0924\u0915 \u0915\u094b\u0908 \u0936\u0941\u0932\u094d\u0915 \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.AND) {
        return " \u0914\u0930 ";
    };
    if (stringKey instanceof Language_Types.DIRECT_PAYMENT_NO_COMMISSIONS) {
        return "\u0938\u0940\u0927\u093e \u092d\u0941\u0917\u0924\u093e\u0928.\x0a\u0915\u094b\u0908 \u0915\u092e\u0940\u0936\u0928 \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_PAYS_DIRECTLY) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0906\u092a\u0915\u094b \u0938\u0940\u0927\u0947 \u0928\u0915\u0926 \u092f\u093e \u092f\u0942\u092a\u0940\u0906\u0908 \u0915\u0947 \u092e\u093e\u0927\u094d\u092f\u092e \u0938\u0947 \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0924\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.HUNDRED_PERCENT_FARE_GOES_TO_YOU) {
        return "100% \u0915\u093f\u0930\u093e\u092f\u093e\x0a\u0906\u092a\u0915\u094b \u091c\u093e\u0924\u093e \u0939\u0948!";
    };
    if (stringKey instanceof Language_Types.FARE_SHOWN_IS_FARE_YOU_GET) {
        return "\u0926\u093f\u0916\u093e\u092f\u093e \u0917\u092f\u093e \u0915\u093f\u0930\u093e\u092f\u093e \u0935\u0939\u0940 \u0915\u093f\u0930\u093e\u092f\u093e \u0939\u0948 \u091c\u094b \u0906\u092a\u0915\u094b \u092e\u093f\u0932\u0924\u093e \u0939\u0948\u0964\x0a\u0915\u094b\u0908 \u091b\u093f\u092a\u093e \u0939\u0941\u0906 \u0936\u0941\u0932\u094d\u0915 \u0928\u0939\u0940\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION) {
        return "\u0913\u092a\u0928\x0a\u092e\u094b\u092c\u093f\u0932\u093f\u091f\u0940 \u0915\u094d\u0930\u093e\u0902\u0924\u093f \u0915\u093e \u0939\u093f\u0938\u094d\u0938\u093e \u092c\u0928\u0947\u0902!";
    };
    if (stringKey instanceof Language_Types.OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT) {
        return "\u0939\u092e\u093e\u0930\u093e \u0921\u0947\u091f\u093e \u0914\u0930 \u0909\u0924\u094d\u092a\u093e\u0926 \u0930\u094b\u0921\u092e\u0948\u092a \u0938\u092d\u0940 \u0915\u0947 \u0932\u093f\u090f \u092a\u093e\u0930\u0926\u0930\u094d\u0936\u0940 \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.ENABLE_LOCATION_PERMISSION) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0905\u0928\u0941\u092e\u0924\u093f \u0938\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.PLEASE_ENABLE_LOCATION_PERMISSION_FOR) {
        return "\u0915\u0943\u092a\u092f\u093e \u0907\u0938\u0915\u0947 \u0932\u093f\u090f \u0938\u094d\u0925\u093e\u0928 \u0905\u0928\u0941\u092e\u0924\u093f \u0938\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENABLE_LOCATION) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0938\u0915\u094d\u0937\u092e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.YOUR_DETECTED_LOCATION_IS) {
        return "\u0906\u092a\u0915\u093e \u092a\u0924\u093e \u0932\u0917\u093e\u092f\u093e \u0917\u092f\u093e \u0938\u094d\u0925\u093e\u0928 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.LANGUAGE_DETECTED) {
        return "\u092d\u093e\u0937\u093e \u0915\u093e \u092a\u0924\u093e \u091a\u0932\u093e";
    };
    if (stringKey instanceof Language_Types.CHANGE_LANGUAGE_STR) {
        return "\u092d\u093e\u0937\u093e \u092c\u0926\u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SELECT_LOCATION) {
        return "\u0938\u094d\u0925\u093e\u0928 \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SELECT_LOCATION_DESC) {
        return "\u0935\u0939 \u0938\u094d\u0925\u093e\u0928 \u091a\u0941\u0928\u0947\u0902 \u091c\u0939\u093e\u0901 \u0906\u092a \u0938\u0935\u093e\u0930\u0940 \u0915\u0930\u0928\u093e \u091a\u093e\u0939\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.SELECT_LANGUAGE_DESC) {
        return "\u0935\u0939 \u092d\u093e\u0937\u093e \u091a\u0941\u0928\u0947\u0902 \u091c\u093f\u0938\u0947 \u0906\u092a \u092a\u0922\u093c \u0938\u0915\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.CONFIRM_LANGUAGE) {
        return "\u092d\u093e\u0937\u093e \u0915\u0940 \u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GET_STARTED) {
        return "\u0936\u0941\u0930\u0942 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_YOUR_MOBILE_NUMBER) {
        return "\u0905\u092a\u0928\u093e \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_ACCESS) {
        return "\u0938\u0942\u091a\u0928\u093e \u092a\u0939\u0941\u0902\u091a";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_ACCESS_DESC) {
        return "\u0938\u093f\u092b\u093e\u0930\u093f\u0936 \u0915\u0940 \u091c\u093e\u0924\u0940 \u0939\u0948, \u091c\u093f\u0938\u0938\u0947 \u0906\u092a \u0915\u092d\u0940 \u092d\u0940 \u0938\u0902\u0926\u0947\u0936 \u091b\u0942\u091f \u0928\u0939\u0940\u0902 \u091c\u093e\u090f\u0902";
    };
    if (stringKey instanceof Language_Types.WATCH_VIDEO) {
        return "\u0935\u0940\u0921\u093f\u092f\u094b \u0926\u0947\u0916\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DL_VERIFICATION_FAILED) {
        return "\u0921\u0940\u090f\u0932 \u0938\u0924\u094d\u092f\u093e\u092a\u0928 \u0935\u093f\u092b\u0932 \u0939\u0941\u0906\u0964 \u0915\u0943\u092a\u092f\u093e \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0940 \u0924\u093f\u0925\u093f \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902 \u0914\u0930 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RC_VERIFICATION_FAILED) {
        return "\u0906\u0930\u0938\u0940 \u0938\u0924\u094d\u092f\u093e\u092a\u0928 \u0935\u093f\u092b\u0932 \u0939\u0941\u0906\u0964 \u0915\u0943\u092a\u092f\u093e \u091c\u093e\u0930\u0940 \u0930\u0916\u0928\u0947 \u0915\u0940 \u0924\u093f\u0925\u093f \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902 \u0914\u0930 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DL_UPLOAD_FAILED) {
        return "\u0921\u0940\u090f\u0932 \u0905\u092a\u0932\u094b\u0921 \u0935\u093f\u092b\u0932 \u0939\u0941\u0906";
    };
    if (stringKey instanceof Language_Types.RC_UPLOAD_FAILED) {
        return "\u0906\u0930\u0938\u0940 \u0905\u092a\u0932\u094b\u0921 \u0935\u093f\u092b\u0932 \u0939\u0941\u0906";
    };
    if (stringKey instanceof Language_Types.PLEASE_RETRY_THE_UPLOAD_AGAIN) {
        return "\u0915\u0943\u092a\u092f\u093e \u0905\u092a\u0932\u094b\u0921 \u0915\u094b \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.RC_AND_DL_UPLOAD_FAILED) {
        return "\u0906\u0930\u0938\u0940 \u0914\u0930 \u0921\u0940\u090f\u0932 \u0905\u092a\u0932\u094b\u0921 \u0935\u093f\u092b\u0932 \u0939\u0941\u0906\u0964";
    };
    if (stringKey instanceof Language_Types.RC_UPLOAD_LIMIT_REACHED) {
        return "\u0906\u0930\u0938\u0940 \u0905\u092a\u0932\u094b\u0921 \u0938\u0940\u092e\u093e \u092a\u093e\u0930 \u0939\u094b \u0917\u0908 \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.DL_UPLOAD_LIMIT_REACHED) {
        return "\u0921\u0940\u090f\u0932 \u0905\u092a\u0932\u094b\u0921 \u0938\u0940\u092e\u093e \u092a\u093e\u0930 \u0939\u094b \u0917\u0908 \u0939\u0948\u0964";
    };
    if (stringKey instanceof Language_Types.RETRY_UPLOAD) {
        return "\u0905\u092a\u0932\u094b\u0921 \u092a\u0941\u0928\u0903 \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.VEHICLE_REGISTERATON_CERTIFICATE) {
        return "\u0935\u093e\u0939\u0928 \u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930";
    };
    if (stringKey instanceof Language_Types.GRANT_PERMISSIONS) {
        return "\u0905\u0928\u0941\u092e\u0924\u093f\u092f\u093e\u0902 \u0926\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.SUBSCRIPTION_PLAN_STR) {
        return "\u0938\u0926\u0938\u094d\u092f\u0924\u093e \u092f\u094b\u091c\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.COMPLETE_AUTOPAY_LATER) {
        return "\u092c\u093e\u0926 \u092e\u0947\u0902 '\u092e\u0947\u0930\u0940 \u092f\u094b\u091c\u0928\u093e' \u0938\u0947\u0915\u094d\u0936\u0928 \u0938\u0947 \u0938\u094d\u0935\u0924: \u092d\u0941\u0917\u0924\u093e\u0928 \u092a\u0942\u0930\u093e \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.START_EARNING_IN_FOUR_STEPS) {
        return "{} \u0938\u0930\u0932 \u0915\u0926\u092e\u094b\u0902 \u092e\u0947\u0902 \u0915\u092e\u093e\u0908 \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.COMPLETE) {
        return "\u092a\u0942\u0930\u093e";
    };
    if (stringKey instanceof Language_Types.HOW_TO_UPLOAD) {
        return "\u0905\u092a\u0932\u094b\u0921 \u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.TAKE_CLEAR_PICTURE_DL) {
        return "\u0905\u092a\u0928\u0947 \u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0915\u0947 \u092b\u094b\u091f\u094b \u0938\u093e\u0907\u0921 \u0915\u093e \u0938\u094d\u092a\u0937\u094d\u091f \u091a\u093f\u0924\u094d\u0930 \u090f\u0915 \u0938\u092e\u0924\u0932 \u0938\u0924\u0939 \u092a\u0930 \u0932\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.ENSURE_ADEQUATE_LIGHT) {
        return "\u0938\u0941\u0928\u093f\u0936\u094d\u091a\u093f\u0924 \u0915\u0930\u0947\u0902 \u0915\u093f \u092a\u094d\u0930\u0915\u093e\u0936 \u092a\u0930\u094d\u092f\u093e\u092a\u094d\u0924 \u0939\u0948 \u0914\u0930 \u0938\u092d\u0940 \u0935\u093f\u0935\u0930\u0923 \u0938\u094d\u092a\u0937\u094d\u091f \u0926\u093f\u0916\u093e\u0908 \u0926\u0947";
    };
    if (stringKey instanceof Language_Types.FIT_DL_CORRECTLY) {
        return "\u0928\u0940\u091a\u0947 \u0926\u093f\u0916\u093e\u090f \u0917\u090f \u0915\u094d\u0937\u0947\u0924\u094d\u0930 \u092e\u0947\u0902 \u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0915\u094b \u0938\u0939\u0940 \u0922\u0902\u0917 \u0938\u0947 \u092b\u093f\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.TAKE_PHOTO) {
        return "\u092b\u094b\u091f\u094b \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.FIT_RC_CORRECTLY) {
        return "\u0928\u0940\u091a\u0947 \u0926\u093f\u0916\u093e\u090f \u0917\u090f \u0915\u094d\u0937\u0947\u0924\u094d\u0930 \u092e\u0947\u0902 \u0930\u091c\u093f\u0938\u094d\u091f\u094d\u0930\u0947\u0936\u0928 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0915\u094b \u0938\u0939\u0940 \u0922\u0902\u0917 \u0938\u0947 \u092b\u093f\u091f \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.TAKE_CLEAR_PICTURE_RC) {
        return "\u0905\u092a\u0928\u0947 \u0930\u091c\u093f\u0938\u094d\u091f\u094d\u0930\u0947\u0936\u0928 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0915\u0947 \u092b\u094b\u091f\u094b \u0938\u093e\u0907\u0921 \u0915\u093e \u0938\u094d\u092a\u0937\u094d\u091f \u091a\u093f\u0924\u094d\u0930 \u090f\u0915 \u0938\u092e\u0924\u0932 \u0938\u0924\u0939 \u092a\u0930 \u0932\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.DL_UPLOADED) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0905\u092a\u0932\u094b\u0921 \u0939\u094b \u0917\u092f\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.RC_UPLOADED) {
        return "\u0930\u091c\u093f\u0938\u094d\u091f\u094d\u0930\u0947\u0936\u0928 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0905\u092a\u0932\u094b\u0921 \u0939\u094b \u0917\u092f\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.DL_UPLOADING) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0905\u092a\u0932\u094b\u0921 \u0939\u094b \u0930\u0939\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.RC_UPLOADING) {
        return "\u0930\u091c\u093f\u0938\u094d\u091f\u094d\u0930\u0947\u0936\u0928 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0905\u092a\u0932\u094b\u0921 \u0939\u094b \u0930\u0939\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.RETAKE_RC) {
        return "\x0a\u0915\u0943\u092a\u092f\u093e \u0906\u0930\u0938\u0940 \u0915\u0947 \u0938\u093e\u0925 \u092b\u094b\u091f\u094b \u092b\u093f\u0930 \u0938\u0947 \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RETAKE_DL) {
        return "\x0a\u0915\u0943\u092a\u092f\u093e \u0921\u0940\u090f\u0932 \u0915\u0947 \u0938\u093e\u0925 \u092b\u094b\u091f\u094b \u092b\u093f\u0930 \u0938\u0947 \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CONFIRM_AND_UPLOAD) {
        return "\u092a\u0941\u0937\u094d\u091f\u093f \u0915\u0930\u0947\u0902 \u0914\u0930 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RETAKE_PHOTO) {
        return "\u092b\u094b\u091f\u094b \u092b\u093f\u0930 \u0938\u0947 \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LETS_GET_YOU_TRIP_READY) {
        return "\u0906\u0907\u090f \u0939\u092e \u0906\u092a\u0915\u0940 \u092f\u093e\u0924\u094d\u0930\u093e \u0915\u0947 \u0932\u093f\u090f \u0924\u0948\u092f\u093e\u0930 \u0939\u094b\u0902!";
    };
    if (stringKey instanceof Language_Types.GOT_AN_OTP) {
        return "OTP \u092e\u093f\u0932\u093e?";
    };
    if (stringKey instanceof Language_Types.DRIVING_LICENSE_DETAILS) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u093f\u0902\u0917 \u0932\u093e\u0907\u0938\u0947\u0902\u0938 \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.VEHICLE_REGISTRATION_DETAILS) {
        return "\u0935\u093e\u0939\u0928 \u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u0935\u093f\u0935\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.UPLOAD_PHOTO) {
        return "\u092b\u094b\u091f\u094b \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CLEAR_IMAGE) {
        return "\u0938\u094d\u092a\u0937\u094d\u091f \u091a\u093f\u0924\u094d\u0930";
    };
    if (stringKey instanceof Language_Types.BLURRY_IMAGE) {
        return "\u0927\u0941\u0902\u0927\u0932\u093e \u091a\u093f\u0924\u094d\u0930";
    };
    if (stringKey instanceof Language_Types.CROPPED_CORRECTLY) {
        return "\u0938\u0939\u0940 \u0922\u0902\u0917 \u0938\u0947 \u0915\u094d\u0930\u0949\u092a \u0915\u093f\u092f\u093e \u0917\u092f\u093e";
    };
    if (stringKey instanceof Language_Types.WRONG_CROPPING) {
        return "\u0917\u0932\u0924 \u0915\u094d\u0930\u0949\u092a\u093f\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.CHANGE_LOCATION) {
        return "\u0938\u094d\u0925\u093e\u0928 \u092c\u0926\u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.CHANGE_CITY) {
        return "\u0936\u0939\u0930 \u092c\u0926\u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.UPLOAD_REGISTRATION_CERTIFICATE_STR) {
        return "\u092a\u0902\u091c\u0940\u0915\u0930\u0923 \u092a\u094d\u0930\u092e\u093e\u0923\u092a\u0924\u094d\u0930 \u0905\u092a\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.TAKE_A_PHOTO) {
        return "\u090f\u0915 \u0924\u0938\u094d\u0935\u0940\u0930 \u0932\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.GALLERY) {
        return "\u0917\u0948\u0932\u0930\u0940";
    };
    if (stringKey instanceof Language_Types.GET_FULL_PAYMENT) {
        return "\u0917\u094d\u0930\u093e\u0939\u0915 \u0938\u0947 \u0938\u0940\u0927\u093e \u0914\u0930 \u092a\u0942\u0930\u093e \u092d\u0941\u0917\u0924\u093e\u0928 \u0932\u093f\u091c\u093f\u092f\u0947";
    };
    if (stringKey instanceof Language_Types.SELECT_CITY_STR) {
        return "\u0936\u0939\u0930 \u091a\u0941\u0928\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DETECTING_LOCATION) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0915\u093e \u092a\u0924\u093e \u0932\u0917\u093e\u092f\u093e \u091c\u093e \u0930\u0939\u093e \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.UNABLE_TO_DETECT_YOUR_LOCATION) {
        return "\u0906\u092a\u0915\u0947 \u0938\u094d\u0925\u093e\u0928 \u0915\u093e \u092a\u0924\u093e \u0932\u0917\u093e\u0928\u0947 \u092e\u0947\u0902 \u0905\u0938\u092e\u0930\u094d\u0925";
    };
    if (stringKey instanceof Language_Types.RC_FAILED_DESC) {
        return "\u0915\u0943\u092a\u092f\u093e \u0915\u0941\u091b \u0938\u092e\u092f \u092c\u093e\u0926 \u0906\u0930\u0938\u0940 \u091c\u094b\u0921\u093c\u0928\u0947 \u0915\u093e \u092a\u094d\u0930\u092f\u093e\u0938 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.RC_IN_PROGRESS_DESC) {
        return "\u0938\u0924\u094d\u092f\u093e\u092a\u093f\u0924 \u0939\u094b\u0924\u0947 \u0939\u0940 \u0906\u0930\u0938\u0940 \u0906\u092a\u0915\u0940 \u092a\u094d\u0930\u094b\u092b\u093c\u093e\u0907\u0932 \u092e\u0947\u0902 \u091c\u094b\u0921\u093c \u0926\u0940 \u091c\u093e\u090f\u0917\u0940";
    };
    if (stringKey instanceof Language_Types.RC_VERIFICATION_SUCCESS) {
        return "\u0906\u0930\u0938\u0940 \u0938\u0924\u094d\u092f\u093e\u092a\u0928 \u0938\u092b\u0932!";
    };
    if (stringKey instanceof Language_Types.RC_VERIFICATION_FAILED_STATUS) {
        return "\u0906\u0930\u0938\u0940 \u0938\u0924\u094d\u092f\u093e\u092a\u0928 \u0935\u093f\u092b\u0932!";
    };
    if (stringKey instanceof Language_Types.RC_VERIFICATION_IN_PROGRESS) {
        return "\u0906\u0930\u0938\u0940 \u0938\u0924\u094d\u092f\u093e\u092a\u0928 \u091c\u093e\u0930\u0940 \u0939\u0948...";
    };
    if (stringKey instanceof Language_Types.WE_ARE_NOT_LIVE_IN_YOUR_AREA) {
        return "\u0939\u092e \u0905\u092d\u0940 \u0906\u092a\u0915\u0947 \u0915\u094d\u0937\u0947\u0924\u094d\u0930 \u092e\u0947\u0902 \u0928\u0939\u0940\u0902 \u0930\u0939\u0924\u0947 \u0939\u0948\u0902!\x0a\u091c\u0932\u094d\u0926 \u0939\u0940 \u0906\u092a\u0915\u0947 \u0936\u0939\u0930 \u092e\u0947\u0902 \u0906 \u0930\u0939\u0947 \u0939\u0948\u0902!";
    };
    if (stringKey instanceof Language_Types.LOCATION_UNSERVICEABLE) {
        return "\u0938\u094d\u0925\u093e\u0928 \u0905\u0928\u0941\u092a\u092f\u094b\u0917\u0940";
    };
    if (stringKey instanceof Language_Types.UNABLE_TO_GET_YOUR_LOCATION) {
        return "\u0906\u092a\u0915\u093e \u0938\u094d\u0925\u093e\u0928 \u092a\u094d\u0930\u093e\u092a\u094d\u0924 \u0915\u0930\u0928\u0947 \u092e\u0947\u0902 \u0905\u0938\u092e\u0930\u094d\u0925!";
    };
    if (stringKey instanceof Language_Types.TURN_OFF_ANY_MOCK_LOCATION_APP_AND_RESTART) {
        return "\u0906\u092a\u0915\u0947 \u0926\u094d\u0935\u093e\u0930\u093e \u0909\u092a\u092f\u094b\u0917 \u0915\u093f\u090f \u091c\u093e \u0930\u0939\u0947 \u0915\u093f\u0938\u0940 \u092d\u0940 \u092e\u0949\u0915 \u0932\u094b\u0915\u0947\u0936\u0928 \u0910\u092a \u0915\u094b \u092c\u0902\u0926 \u0915\u0930\u0947\u0902 \u0914\u0930 \u0910\u092a \u0915\u094b \u092a\u0941\u0928\u0930\u093e\u0930\u0902\u092d \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.REFERRAL_CODE_NUMBER) {
        return "";
    };
    if (stringKey instanceof Language_Types.WELCOME_TEXT) {
        return "";
    };
    if (stringKey instanceof Language_Types.ABOUT_TEXT) {
        return "";
    };
    if (stringKey instanceof Language_Types.EARNED_ON_APP) {
        return "";
    };
    if (stringKey instanceof Language_Types.TRAVELLED_ON_APP) {
        return "";
    };
    if (stringKey instanceof Language_Types.WITH) {
        return "";
    };
    if (stringKey instanceof Language_Types.YOU_DONT_HAVE_ANY_PAYMENTS) {
        return "";
    };
    if (stringKey instanceof Language_Types.RESUMED_AUTOPAY) {
        return "\u0911\u091f\u094b\u092a\u0947 \u092b\u093f\u0930 \u0938\u0947 \u0936\u0941\u0930\u0942";
    };
    if (stringKey instanceof Language_Types.CANCEL_ANYWAY) {
        return "";
    };
    if (stringKey instanceof Language_Types.VALIDITY_EXPIRED_STR) {
        return "";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_NOT_PRESENT) {
        return "";
    };
    if (stringKey instanceof Language_Types.ENTER_AADHAAR_OTP_) {
        return "";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_DUE_OVERVIEW) {
        return "";
    };
    if (stringKey instanceof Language_Types.DUE_OVERVIEW) {
        return "";
    };
    if (stringKey instanceof Language_Types.CHOOSE_LANGUAGE) {
        return "";
    };
    if (stringKey instanceof Language_Types.BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR) {
        return "";
    };
    if (stringKey instanceof Language_Types.THIS_EXTRA_AMOUNT_THE_CUSTOMER_WILL_PAY) {
        return "\u092f\u0939 \u0935\u0939 \u0905\u0924\u093f\u0930\u093f\u0915\u094d\u0924 \u0930\u093e\u0936\u093f \u0939\u0948 \u091c\u094b \u0917\u094d\u0930\u093e\u0939\u0915 \u0906\u092a\u0915\u094b \u092d\u0941\u0917\u0924\u093e\u0928 \u0915\u0930\u0947\u0917\u093e \u0915\u094d\u092f\u094b\u0902\u0915\u093f \u0906\u092a\u0928\u0947 {} \u092e\u093f\u0928\u091f \u0938\u0947 \u0905\u0927\u093f\u0915 \u092a\u094d\u0930\u0924\u0940\u0915\u094d\u0937\u093e \u0915\u0940 \u0939\u0948";
    };
    if (stringKey instanceof Language_Types.TEN_DIGIT_MOBILE_NUMBER) {
        return "10 \u0905\u0902\u0915\u094b\u0902 \u0915\u093e \u092e\u094b\u092c\u093e\u0907\u0932 \u0928\u0902\u092c\u0930";
    };
    if (stringKey instanceof Language_Types.BOOTH_CHARGES) {
        return "\u092c\u0942\u0925 \u091a\u093e\u0930\u094d\u091c";
    };
    if (stringKey instanceof Language_Types.BOOTH_CHARGES_INCLUDED) {
        return "\u092c\u0942\u0925 \u0936\u0941\u0932\u094d\u0915 \u0936\u093e\u092e\u093f\u0932: \u20b9{}";
    };
    if (stringKey instanceof Language_Types.TOTAL_AMOUNT) {
        return "\u0915\u0941\u0932 \u0930\u093e\u0936\u093f";
    };
    if (stringKey instanceof Language_Types.PLEASE_ADD_RC) {
        return "\u0915\u0943\u092a\u092f\u093e \u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f RC \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.LOCATION_CANNOT_BE_ADDED_WHILE_GOTO_ACTIVE) {
        return "\u0935\u093f\u0915\u0932\u094d\u092a \u0915\u0947\u0935\u0932 \u0924\u092d\u0940 \u0909\u092a\u0932\u092c\u094d\u0927 \u0939\u094b\u0924\u093e \u0939\u0948 \u091c\u092c \u0917\u094b-\u091f\u0942 \u0938\u0915\u094d\u0937\u092e \u0928 \u0939\u094b";
    };
    if (stringKey instanceof Language_Types.LOCATION_CANNOT_BE_ADDED_WHILE_ON_RIDE) {
        return "\u091c\u092c \u0938\u0935\u093e\u0930\u0940 \u091a\u0932 \u0930\u0939\u0940 \u0939\u094b \u0924\u094b \u0935\u093f\u0915\u0932\u094d\u092a \u0909\u092a\u0932\u092c\u094d\u0927 \u0928\u0939\u0940\u0902 \u0939\u094b\u0924\u093e";
    };
    if (stringKey instanceof Language_Types.ADD_GOTO) {
        return "\u0917\u094b-\u091f\u0942 \u091c\u094b\u0921\u093c\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.NO_OPEN_MARKET_RIDES) {
        return "\u0915\u094b\u0908 \u0938\u0935\u093e\u0930\u0940 \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.ACCOUNT_BLOCKED) {
        return "\u0916\u093e\u0924\u093e \u0905\u0935\u0930\u0941\u0926\u094d\u0927!";
    };
    if (stringKey instanceof Language_Types.YOU_HAVE_BEEN_BLOCKED_FROM_TAKING_RIDES) {
        return "\u0906\u092a\u0915\u094b \u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u0947 \u0938\u0947 \u0930\u094b\u0915 \u0926\u093f\u092f\u093e \u0917\u092f\u093e \u0939\u0948\u0964\x0a\u0915\u0943\u092a\u092f\u093e \u0938\u0939\u093e\u092f\u0924\u093e \u0915\u0947 \u0932\u093f\u090f \u0938\u0939\u093e\u092f\u0924\u093e \u0938\u0947 \u0938\u0902\u092a\u0930\u094d\u0915 \u0915\u0930\u0947\u0902\u0964";
    };
    if (stringKey instanceof Language_Types.DISMISS) {
        return "\u0916\u093e\u0930\u093f\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DRIVER_REFERRAL_CODE) {
        return "\u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.APP_QR_CODE) {
        return "\u0910\u092a \u0915\u094d\u092f\u0942\u0906\u0930 \u0915\u094b\u0921";
    };
    if (stringKey instanceof Language_Types.START_TAKING_RIDES_AND_REFER) {
        return "\u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u0921\u094d\u0930\u093e\u0907\u0935\u0930 \u0910\u092a \u092a\u0930 \u0938\u093e\u0907\u0928 \u0905\u092a \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0938\u0935\u093e\u0930\u0940 \u0932\u0947\u0928\u093e \u0914\u0930 \u0921\u094d\u0930\u093e\u0907\u0935\u0930\u094b\u0902 \u0915\u094b \u0930\u0947\u092b\u0930 \u0915\u0930\u0928\u093e \u0936\u0941\u0930\u0942 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.REFERRED_DRIVERS) {
        return "\u0938\u0902\u0926\u0930\u094d\u092d\u093f\u0924 \u0921\u094d\u0930\u093e\u0907\u0935\u0930";
    };
    if (stringKey instanceof Language_Types.RIDE_LEADERBOARD) {
        return "\u0930\u093e\u0907\u0921 \u0932\u0940\u0921\u0930\u092c\u094b\u0930\u094d\u0921";
    };
    if (stringKey instanceof Language_Types.YOUR_RANK) {
        return "\u0906\u092a\u0915\u093e \u0930\u0948\u0902\u0915";
    };
    if (stringKey instanceof Language_Types.NOT_AVAILABLE_YET) {
        return "\u0905\u092d\u0940 \u0924\u0915 \u0909\u092a\u0932\u092c\u094d\u0927 \u0928\u0939\u0940\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_REFERRAL_CODE) {
        return "\u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.HAVE_A_REFERRAL_CODE) {
        return "\u0915\u094d\u092f\u093e \u0906\u092a\u0915\u0947 \u092a\u093e\u0938 \u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921 \u0939\u0948?";
    };
    if (stringKey instanceof Language_Types.COMPLETE_STEPS_TO_APPLY_REFERRAL) {
        return "\u0930\u0947\u092b\u0930\u0932 \u0915\u094b\u0921 \u0932\u093e\u0917\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0909\u092a\u0930\u094b\u0915\u094d\u0924 \u091a\u0930\u0923\u094b\u0902 \u0915\u094b \u092a\u0942\u0930\u093e \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.DOWNLOAD_NAMMA_YATRI) {
        return "\u0928\u092e\u094d\u092e\u093e \u092f\u093e\u0924\u094d\u0930\u0940 \u0921\u093e\u0909\u0928\u0932\u094b\u0921 \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_CODE) {
        return "\u0915\u094b\u0921 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    if (stringKey instanceof Language_Types.COMPLETE_REGISTRATION) {
        return "\u092a\u0942\u0930\u093e \u092a\u0902\u091c\u0940\u0915\u0930\u0923";
    };
    if (stringKey instanceof Language_Types.START_TIME) {
        return "\u0936\u0941\u0930\u0941\u0906\u0924 \u0915\u093e \u0938\u092e\u092f";
    };
    if (stringKey instanceof Language_Types.START_ODO_READING) {
        return "\u0936\u0941\u0930\u0941\u0906\u0924\u0940 \u0913\u0921\u094b \u092a\u0922\u093c\u0928\u093e";
    };
    if (stringKey instanceof Language_Types.RIDE_START) {
        return "\u0938\u0935\u093e\u0930\u0940 \u092a\u094d\u0930\u093e\u0930\u0902\u092d";
    };
    if (stringKey instanceof Language_Types.RIDE_END) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0915\u093e \u0905\u0902\u0924";
    };
    if (stringKey instanceof Language_Types.RIDE_STARTED_AT) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0936\u0941\u0930\u0942 \u0939\u0941\u0908:";
    };
    if (stringKey instanceof Language_Types.RIDE_ENDED_AT) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0938\u092e\u093e\u092a\u094d\u0924 \u0939\u0941\u0908:";
    };
    if (stringKey instanceof Language_Types.ODOMETER_READING) {
        return "\u0913\u0921\u094b\u092e\u0940\u091f\u0930 \u0930\u0940\u0921\u093f\u0902\u0917";
    };
    if (stringKey instanceof Language_Types.PICKED_UP_AT) {
        return "\u092a\u093f\u0915-\u0905\u092a \u0915\u093e \u0938\u092e\u092f";
    };
    if (stringKey instanceof Language_Types.RIDE_TIME) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0915\u093e \u0938\u092e\u092f";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ON_A_RENTAL_RIDE) {
        return "\u0906\u092a \u090f\u0915 \u0915\u093f\u0930\u093e\u092f\u0947 \u0915\u0940 \u0938\u0935\u093e\u0930\u0940 \u092a\u0930 \u0939\u0948\u0902";
    };
    if (stringKey instanceof Language_Types.ENTER_END_RIDE_OTP) {
        return "\u0938\u0935\u093e\u0930\u0940 \u0938\u092e\u093e\u092a\u094d\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093f\u090f \u0913\u091f\u0940\u092a\u0940 \u0926\u0930\u094d\u091c \u0915\u0930\u0947\u0902";
    };
    throw new Error("Failed pattern match at Resources.Localizable.HI (line 7, column 5 - line 1125, column 73): " + [ stringKey.constructor.name ]);
};
export {
    getHI
};
