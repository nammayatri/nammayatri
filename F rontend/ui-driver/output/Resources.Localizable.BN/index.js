import * as Language_Types from "../Language.Types/index.js";
var getBN = function (stringKey) {
    if (stringKey instanceof Language_Types.INACCURATE_DATE_AND_TIME) {
        return "\u09ad\u09c1\u09b2 \u09a4\u09be\u09b0\u09bf\u0996 \u0993 \u09b8\u09ae\u09af\u09bc!";
    };
    if (stringKey instanceof Language_Types.ADJUST_YOUR_DEVICE_DATE_AND_TIME_AND_TRY_AGAIN) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09c7\u099f\u09bf\u0982\u09b8 \u0985\u09cd\u09af\u09be\u09aa \u09a5\u09c7\u0995\u09c7 \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc (\u09a8\u09c7\u099f\u0993\u09af\u09bc\u09be\u09b0\u09cd\u0995-\u09aa\u09cd\u09b0\u09a6\u09a4\u09cd\u09a4) \u09a4\u09be\u09b0\u09bf\u0996 \u098f\u09ac\u0982 \u09b8\u09ae\u09af\u09bc \u09b8\u09c7\u099f \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.THE_CURRENT_DATE_AND_TIME_IS) {
        return "\u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8 \u09a4\u09be\u09b0\u09bf\u0996 \u098f\u09ac\u0982 \u09b8\u09ae\u09af\u09bc \u09b9\u09b2";
    };
    if (stringKey instanceof Language_Types.GO_TO_SETTING) {
        return "\u09b8\u09c7\u099f\u09bf\u0982\u09b8 \u098f \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.LETS_GET_STARTED) {
        return "\u099a\u09b2 \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09bf";
    };
    if (stringKey instanceof Language_Types.YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0986\u09ac\u09c7\u09a6\u09a8 \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u099c\u09ae\u09be \u09a6\u09c7\u0993\u09af\u09bc\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7 \u098f\u09ac\u0982 \u09af\u09be\u099a\u09be\u0987\u09af\u09bc\u09c7\u09b0 \u0985\u09a7\u09c0\u09a8\u09c7 \u09b0\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.VIEW_STATUS) {
        return "\u09b8\u09cd\u09a5\u09bf\u09a4\u09bf \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GO_HOME) {
        return "\u09ac\u09b0\u09bf\u09a4\u09c7 \u099c";
    };
    if (stringKey instanceof Language_Types.SELECT_LANGUAGE) {
        return "\u09ad\u09be\u09b7\u09be \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0";
    };
    if (stringKey instanceof Language_Types.WHICH_LANGUAGE_DO_YOU_PREFER) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09cb\u09a8 \u09ad\u09be\u09b7\u09be \u09aa\u099b\u09a8\u09cd\u09a6 \u0995\u09b0\u09c7\u09a8?";
    };
    if (stringKey instanceof Language_Types.NEXT) {
        return "\u09aa\u09b0\u09ac\u09b0\u09cd\u09a4\u09c0";
    };
    if (stringKey instanceof Language_Types.T_C) {
        return "\u09b6\u09b0\u09cd\u09a4\u09be\u09ac\u09b2\u09c0";
    };
    if (stringKey instanceof Language_Types.ENTER_MOBILE_NUMBER) {
        return "\u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.BY_CLICKING_CONTINUE_YOU_WILL_BE_AGREEING_TO_OUR) {
        return "\u0985\u09ac\u09bf\u09b0\u09a4 \u0995\u09cd\u09b2\u09bf\u0995 \u0995\u09b0\u09c7, \u0986\u09aa\u09a8\u09bf \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09b8\u09ae\u09cd\u09ae\u09a4 \u09b9\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_OTP) {
        return "\u0993\u099f\u09bf\u09aa\u09bf \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DIDNT_RECIEVE_OTP) {
        return "\u0993\u099f\u09bf\u09aa\u09bf \u0997\u09cd\u09b0\u09b9\u09a3 \u0995\u09b0\u09c7\u09a8\u09bf? ";
    };
    if (stringKey instanceof Language_Types.RESEND_OTP) {
        return "<a href=\"\">\u0993\u099f\u09bf\u09aa\u09bf \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09aa\u09cd\u09b0\u09c7\u09b0\u09a3 \u0995\u09b0\u09c1\u09a8</a>";
    };
    if (stringKey instanceof Language_Types.PLEASE_ENTER_VALID_OTP) {
        return "\u09ac\u09c8\u09a7 \u0993\u099f\u09bf\u09aa\u09bf \u09b2\u09bf\u0996\u09c1\u09a8 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.INVALID_MOBILE_NUMBER) {
        return "\u0985\u0995\u09be\u09b0\u09cd\u09af\u0995\u09b0 \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.REGISTER) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09a8";
    };
    if (stringKey instanceof Language_Types.MOBILE_NUMBER) {
        return "\u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.AUTO_READING_OTP) {
        return "\u0985\u099f\u09cb \u09b0\u09bf\u09a1\u09bf\u0982 \u0993\u099f\u09bf\u09aa\u09bf ...";
    };
    if (stringKey instanceof Language_Types.UPLOAD_DRIVING_LICENSE) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.UPLOAD_BACK_SIDE) {
        return "\u09aa\u09bf\u099b\u09a8\u09c7 \u09aa\u09bf\u099b\u09a8\u09c7 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.UPLOAD_FRONT_SIDE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09a1\u09bf\u098f\u09b2 \u098f\u09b0 \u09ab\u099f\u09cb \u09b8\u09be\u0987\u09a1 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.BACK_SIDE) {
        return "\u09aa\u09bf\u099b\u09a8 \u09a6\u09bf\u0995";
    };
    if (stringKey instanceof Language_Types.FRONT_SIDE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09a1\u09bf\u098f\u09b2 \u098f\u09b0 \u09ab\u099f\u09cb \u09b8\u09be\u0987\u09a1";
    };
    if (stringKey instanceof Language_Types.LICENSE_INSTRUCTION_PICTURE) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8\u09c7\u09b0 \u0989\u09ad\u09af\u09bc \u09aa\u0995\u09cd\u09b7\u09c7\u09b0 \u09aa\u09b0\u09bf\u09b7\u09cd\u0995\u09be\u09b0 \u099b\u09ac\u09bf \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LICENSE_INSTRUCTION_CLARITY) {
        return "\u09ab\u099f\u09cb \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u09b8\u09ae\u09b8\u09cd\u09a4 \u09ac\u09bf\u09ac\u09b0\u09a3 \u09b8\u09cd\u09aa\u09b7\u09cd\u099f\u09ad\u09be\u09ac\u09c7 \u09a6\u09c3\u09b6\u09cd\u09af\u09ae\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.REGISTRATION_STEPS) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u0995\u09b0\u09a3 \u09aa\u09a6\u0995\u09cd\u09b7\u09c7\u09aa";
    };
    if (stringKey instanceof Language_Types.PROGRESS_SAVED) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u0997\u09cd\u09b0\u0997\u09a4\u09bf \u09b8\u0982\u09b0\u0995\u09cd\u09b7\u09a3 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7, \u0986\u09aa\u09a8\u09bf \u0995\u09cb\u09a8\u0993 \u09a4\u09a5\u09cd\u09af \u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09a8 \u0995\u09b0\u09a4\u09c7 \u09aa\u09c2\u09b0\u09cd\u09ac\u09ac\u09b0\u09cd\u09a4\u09c0 \u09aa\u09a6\u0995\u09cd\u09b7\u09c7\u09aa\u09c7 \u09ab\u09bf\u09b0\u09c7 \u09af\u09c7\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.DRIVING_LICENSE) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8";
    };
    if (stringKey instanceof Language_Types.AADHAR_CARD) {
        return "\u0986\u09a7\u09be\u09b0 \u0995\u09be\u09b0\u09cd\u09a1";
    };
    if (stringKey instanceof Language_Types.BANK_DETAILS) {
        return "\u09ac\u09cd\u09af\u09be\u0982\u0995 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.VEHICLE_DETAILS) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.UPLOAD_FRONT_BACK) {
        return "\u09b8\u09be\u09ae\u09a8\u09c7 \u098f\u09ac\u0982 \u09aa\u09bf\u099b\u09a8\u09c7\u09b0 \u09a6\u09bf\u0995 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.EARNINGS_WILL_BE_CREDITED) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0989\u09aa\u09be\u09b0\u09cd\u099c\u09a8 \u098f\u0996\u09be\u09a8\u09c7 \u0995\u09cd\u09b0\u09c7\u09a1\u09bf\u099f \u09b9\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.FILL_VEHICLE_DETAILS) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09ac\u09bf\u09b6\u09a6 \u09aa\u09c2\u09b0\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.FOLLOW_STEPS) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09a8\u09c0\u099a\u09c7\u09b0 \u09aa\u09a6\u0995\u09cd\u09b7\u09c7\u09aa\u0997\u09c1\u09b2\u09bf \u0985\u09a8\u09c1\u09b8\u09b0\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.REGISTRATION) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u0995\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.UPLOAD_ADHAAR_CARD) {
        return "\u0986\u09a7\u09be\u09b0 \u0995\u09be\u09b0\u09cd\u09a1 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADHAAR_INTRUCTION_PICTURE) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0986\u09a7\u09be\u09b0 \u0995\u09be\u09b0\u09cd\u09a1\u09c7\u09b0 \u0989\u09ad\u09af\u09bc \u09aa\u0995\u09cd\u09b7\u09c7\u09b0 \u09aa\u09b0\u09bf\u09b7\u09cd\u0995\u09be\u09b0 \u099b\u09ac\u09bf \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_VEHICLE_DETAILS) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09ac\u09bf\u09b6\u09a6 \u09af\u09c1\u0995\u09cd\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.VEHICLE_REGISTRATION_NUMBER) {
        return "\u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u09b0\u09c7\u099c\u09bf\u09b8\u09cd\u099f\u09cd\u09b0\u09c7\u09b6\u09a8 \u09b8\u09be\u09b0\u09cd\u099f\u09bf\u09ab\u09bf\u0995\u09c7\u099f \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.RE_ENTER_VEHICLE_REGISTRATION_NUMBER) {
        return "\u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u09b0\u09c7\u099c\u09bf\u09b8\u09cd\u099f\u09cd\u09b0\u09c7\u09b6\u09a8 \u09b8\u09be\u09b0\u09cd\u099f\u09bf\u09ab\u09bf\u0995\u09c7\u099f \u09a8\u09ae\u09cd\u09ac\u09b0 \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_VEHICLE_NO) {
        return "\u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u09a8\u0982 \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.VEHICLE_TYPE) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09a7\u09b0\u09a8";
    };
    if (stringKey instanceof Language_Types.VEHICLE_MODEL_NAME) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09ae\u09a1\u09c7\u09b2 \u09a8\u09be\u09ae";
    };
    if (stringKey instanceof Language_Types.ENTER_MODEL_NAME) {
        return "\u09ae\u09a1\u09c7\u09b2 \u09a8\u09be\u09ae \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.VEHICLE_COLOUR) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09b0\u0999";
    };
    if (stringKey instanceof Language_Types.ENTER_VEHICLE_COLOUR) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09b0\u0999 \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.UPLOAD_REGISTRATION_CERTIFICATE) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u0995\u09b0\u09a3 \u09b6\u0982\u09b8\u09be\u09aa\u09a4\u09cd\u09b0 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8 (\u0986\u09b0\u09b8\u09bf)";
    };
    if (stringKey instanceof Language_Types.UPLOAD_RC) {
        return "\u0986\u09aa\u09b2\u09cb\u09a1 \u0986\u09b0\u09b8\u09bf";
    };
    if (stringKey instanceof Language_Types.PREVIEW) {
        return "\u09aa\u09c2\u09b0\u09cd\u09ac\u09b0\u09c2\u09aa";
    };
    if (stringKey instanceof Language_Types.CHOOSE_VEHICLE_TYPE) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09a7\u09b0\u09a3 \u099a\u09af\u09bc\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.BENIFICIARY_NUMBER) {
        return "\u09b8\u09c1\u09ac\u09bf\u09a7\u09be\u09ad\u09cb\u0997\u09c0 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u09a8\u0982";
    };
    if (stringKey instanceof Language_Types.RE_ENTER_BENIFICIARY_NUMBER) {
        return "\u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6\u09c7\u09b0 \u09b8\u09c1\u09ac\u09bf\u09a7\u09be\u09ad\u09cb\u0997\u09c0 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u09a8\u0982";
    };
    if (stringKey instanceof Language_Types.IFSC_CODE) {
        return "\u0986\u0987\u098f\u09ab\u098f\u09b8\u09b8\u09bf \u0995\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.SENDING_OTP) {
        return "\u0993\u099f\u09bf\u09aa\u09bf \u09aa\u09cd\u09b0\u09c7\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT_WHILE_IN_PROGRESS) {
        return "\u0985\u0997\u09cd\u09b0\u0997\u09a4\u09bf\u09b0 \u09b8\u09ae\u09af\u09bc \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09a8\u09c1\u09b0\u09cb\u09a7\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER) {
        return "\u09a4\u09cd\u09b0\u09c1\u099f\u09bf \u0998\u099f\u09c7\u099b\u09c7 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09aa\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_OTP_SENT_TO) {
        return " \u09a8\u09ae\u09cd\u09ac\u09b0\u09c7 \u09aa\u09be\u09a0\u09be\u09a8\u09cb \u0993\u099f\u09bf\u09aa\u09bf \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OTP_SENT_TO) {
        return " \u09a8\u09ae\u09cd\u09ac\u09b0\u09c7 \u0993\u099f\u09bf\u09aa\u09bf \u09aa\u09be\u09a0\u09be\u09a8\u09cb \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.COUNTRY_CODE_INDIA) {
        return "+91";
    };
    if (stringKey instanceof Language_Types.ENTER_ACCOUNT_NUMBER) {
        return "\u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u09a8\u0982 \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_BANK_DETAILS) {
        return "\u09ac\u09cd\u09af\u09be\u0982\u0995\u09c7\u09b0 \u09ac\u09bf\u09b6\u09a6 \u09af\u09c1\u0995\u09cd\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_IFSC_CODE) {
        return "\u0986\u0987\u098f\u09ab\u098f\u09b8\u09b8\u09bf \u0995\u09cb\u09a1 \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.SUBMIT) {
        return "\u099c\u09ae\u09be \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.PERSONAL_DETAILS) {
        return "\u09ac\u09cd\u09af\u0995\u09cd\u09a4\u09bf\u0997\u09a4 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.LANGUAGES) {
        return "\u09ad\u09be\u09b7\u09be";
    };
    if (stringKey instanceof Language_Types.HELP_AND_FAQ) {
        return "\u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.ABOUT) {
        return "\u0985\u09cd\u09af\u09be\u09aa \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.LOGOUT) {
        return "\u09aa\u09cd\u09b0\u09b8\u09cd\u09a5\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.UPDATE) {
        return "\u0986\u09aa\u09a1\u09c7\u099f";
    };
    if (stringKey instanceof Language_Types.EDIT) {
        return "\u09b8\u09ae\u09cd\u09aa\u09be\u09a6\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.AUTO) {
        return "\u0985\u099f\u09cb";
    };
    if (stringKey instanceof Language_Types.NAME) {
        return "\u09a8\u09be\u09ae";
    };
    if (stringKey instanceof Language_Types.PRIVACY_POLICY) {
        return "\u0997\u09cb\u09aa\u09a8\u09c0\u09af\u09bc\u09a4\u09be \u09a8\u09c0\u09a4\u09bf";
    };
    if (stringKey instanceof Language_Types.LOGO) {
        return "\u09b2\u09cb\u0997\u09cb";
    };
    if (stringKey instanceof Language_Types.ABOUT_APP_DESCRIPTION) {
        return "\u099c\u09be\u099f\u09bf \u09b8\u09be\u09a5\u09bf \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u099a\u09be\u09b2\u0995\u09a6\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09a6\u09c7\u09b0 \u09b8\u0982\u09af\u09cb\u0997 \u0995\u09b0\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u098f\u0995\u099f\u09bf \u0989\u09a8\u09cd\u09ae\u09c1\u0995\u09cd\u09a4 \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u099f\u09ab\u09b0\u09cd\u09ae\u0964 \u0985\u09cd\u09af\u09be\u09aa\u099f\u09bf \u09aa\u09cd\u09b0\u09b8\u09cd\u09a4\u09be\u09ac\u09bf\u09a4 \u09aa\u099b\u09a8\u09cd\u09a6\u09b8\u0987 \u09b9\u09be\u09b0 \u09b8\u09b9 \u099a\u09be\u09b2\u0995\u09a6\u09c7\u09b0 \u09b8\u09a8\u09cd\u09a7\u09be\u09a8 \u0995\u09b0\u09be \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09a6\u09c7\u09b0 \u09aa\u0995\u09cd\u09b7\u09c7 \u09b8\u09c1\u09ac\u09bf\u09a7\u09be\u099c\u09a8\u0995 \u0995\u09b0\u09c7 \u09a4\u09cb\u09b2\u09c7\u0964 \u0995\u09cb\u09a8\u0993 \u09b0\u09be\u0987\u09a1 \u09ad\u09bf\u09a4\u09cd\u09a4\u09bf\u0995 \u0995\u09ae\u09bf\u09b6\u09a8 \u09a8\u09c7\u0987, \u0995\u09c7\u09ac\u09b2 \u09ae\u09be\u09b8\u09bf\u0995 \u09b8\u09be\u09ac\u09b8\u09cd\u0995\u09cd\u09b0\u09bf\u09aa\u09b6\u09a8 \u0986\u0995\u09be\u09b0\u09c7 \u0985\u09b2\u09cd\u09aa \u09aa\u09b0\u09bf\u09ae\u09be\u09a3\u09c7 \u0985\u09b0\u09cd\u09a5 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TERMS_AND_CONDITIONS) {
        return "\u09b6\u09b0\u09cd\u09a4\u09be\u09ac\u09b2\u09c0";
    };
    if (stringKey instanceof Language_Types.UPDATE_VEHICLE_DETAILS) {
        return "\u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u09ac\u09bf\u09b6\u09a6 \u0986\u09aa\u09a1\u09c7\u099f \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NOTE) {
        return "\u09ac\u09bf\u0983\u09a6\u09cd\u09b0\u0983:";
    };
    if (stringKey instanceof Language_Types.VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS) {
        return "\u09b0\u09be\u0987\u09a1 \u09b8\u0982\u0995\u09cd\u09b0\u09be\u09a8\u09cd\u09a4 \u0985\u09ad\u09bf\u09af\u09cb\u0997\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u0986\u09ae\u09be\u09b0 \u09b0\u09be\u0987\u09a1\u09b8 \u09ac\u09bf\u09ad\u09be\u0997\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.THANK_YOU_FOR_WRTITTING_US) {
        return "\u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b2\u09c7\u0996\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09a7\u09a8\u09cd\u09af\u09ac\u09be\u09a6!";
    };
    if (stringKey instanceof Language_Types.WE_HAVE_RECIEVED_YOUR_ISSUE) {
        return "\u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09ae\u09b8\u09cd\u09af\u09be\u099f\u09bf \u09aa\u09c1\u09a8\u09b0\u09c1\u09a6\u09cd\u09a7\u09be\u09b0 \u0995\u09b0\u09c7\u099b\u09bf\u0964 \u0986\u09ae\u09b0\u09be \u0995\u09bf\u099b\u09c1 \u09b8\u09ae\u09af\u09bc \u0986\u09aa\u09a8\u09be\u09b0 \u0995\u09be\u099b\u09c7 \u09aa\u09cc\u0981\u099b\u09c7 \u09af\u09be\u09ac\u0964";
    };
    if (stringKey instanceof Language_Types.GO_TO_HOME) {
        return "\u09ac\u09be\u09a1\u09bc\u09bf\u09a4\u09c7 \u09af\u09c7\u09a4\u09c7";
    };
    if (stringKey instanceof Language_Types.YOUR_RECENT_RIDE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09be\u09ae\u09cd\u09aa\u09cd\u09b0\u09a4\u09bf\u0995 \u09af\u09be\u09a4\u09cd\u09b0\u09be";
    };
    if (stringKey instanceof Language_Types.YOUR_RECENT_TRIP) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09be\u09ae\u09cd\u09aa\u09cd\u09b0\u09a4\u09bf\u0995 \u099f\u09cd\u09b0\u09bf\u09aa";
    };
    if (stringKey instanceof Language_Types.ALL_TOPICS) {
        return "\u09b8\u09ae\u09b8\u09cd\u09a4 \u09ac\u09bf\u09b7\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.REPORT_AN_ISSUE_WITH_THIS_TRIP) {
        return "\u098f\u0987 \u099f\u09cd\u09b0\u09bf\u09aa \u09a6\u09bf\u09af\u09bc\u09c7 \u098f\u0995\u099f\u09bf \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b0\u09bf\u09aa\u09cb\u09b0\u09cd\u099f \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.YOU_RATED) {
        return "\u0986\u09aa\u09a8\u09bf \u09b0\u09c7\u099f \u0995\u09b0\u09c7\u099b\u09c7\u09a8:";
    };
    if (stringKey instanceof Language_Types.VIEW_ALL_RIDES) {
        return "\u09b8\u09ae\u09b8\u09cd\u09a4 \u099f\u09cd\u09b0\u09bf\u09aa\u09cd\u09b8 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.WRITE_TO_US) {
        return "\u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SUBJECT) {
        return "\u09ac\u09bf\u09b7\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.YOUR_EMAIL_ID) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0987\u09ae\u09c7\u09b2 \u0986\u0987\u09a1\u09bf";
    };
    if (stringKey instanceof Language_Types.DESCRIBE_YOUR_ISSUE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09ae\u09b8\u09cd\u09af\u09be\u099f\u09bf \u09ac\u09b0\u09cd\u09a3\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.GETTING_STARTED_AND_FAQ) {
        return "\u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09be \u098f\u09ac\u0982 FAQss";
    };
    if (stringKey instanceof Language_Types.FOR_OTHER_ISSUES_WRITE_TO_US) {
        return "\u0985\u09a8\u09cd\u09af\u09be\u09a8\u09cd\u09af \u0987\u09b8\u09cd\u09af\u09c1\u0997\u09c1\u09b2\u09bf\u09b0 \u099c\u09a8\u09cd\u09af, \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CALL_SUPPORT_CENTER) {
        return "\u0995\u09b2 \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8 \u0995\u09c7\u09a8\u09cd\u09a6\u09cd\u09b0";
    };
    if (stringKey instanceof Language_Types.YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE) {
        return "\u0986\u09aa\u09a8\u09bf \u098f\u0996\u09be\u09a8\u09c7 \u09af\u09c7 \u09b8\u09ae\u09b8\u09cd\u09af\u09be\u09b0 \u09ae\u09c1\u0996\u09cb\u09ae\u09c1\u0996\u09bf \u09b9\u09af\u09bc\u09c7\u099b\u09c7\u09a8 \u09a4\u09be \u09ac\u09b0\u09cd\u09a3\u09a8\u09be \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.REGISTRATION_CERTIFICATE_IMAGE) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u0995\u09b0\u09a3 \u09b6\u0982\u09b8\u09be\u09aa\u09a4\u09cd\u09b0 (\u0986\u09b0\u09b8\u09bf) \u099a\u09bf\u09a4\u09cd\u09b0";
    };
    if (stringKey instanceof Language_Types.HOME) {
        return "\u09ac\u09be\u09a1\u09bc\u09bf";
    };
    if (stringKey instanceof Language_Types.RIDES) {
        return "\u09b0\u09be\u0987\u09a1\u09b8";
    };
    if (stringKey instanceof Language_Types.MY_RIDES) {
        return "\u0986\u09ae\u09be\u09b0 \u09b0\u09be\u0987\u09a1\u09b8";
    };
    if (stringKey instanceof Language_Types.PROFILE) {
        return "\u09aa\u09cd\u09b0\u09cb\u09ab\u09be\u0987\u09b2";
    };
    if (stringKey instanceof Language_Types.ENTER_DRIVING_LICENSE_NUMBER) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.WHERE_IS_MY_LICENSE_NUMBER) {
        return "\u0986\u09ae\u09be\u09b0 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8 \u09a8\u09ae\u09cd\u09ac\u09b0\u099f\u09bf \u0995\u09cb\u09a5\u09be\u09af\u09bc?";
    };
    if (stringKey instanceof Language_Types.TRIP_DETAILS) {
        return "\u09ad\u09cd\u09b0\u09ae\u09a3\u09c7\u09b0 \u09ac\u09bf\u09b6\u09a6";
    };
    if (stringKey instanceof Language_Types.BY_CASH) {
        return "\u09a8\u0997\u09a6\u09c7";
    };
    if (stringKey instanceof Language_Types.ONLINE_) {
        return "\u0985\u09a8\u09b2\u09be\u0987\u09a8";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE_POPUP) {
        return "<u>\u09af\u09be\u0993\u09af\u09bc\u09be \u0985\u09a8\u09b2\u09be\u0987\u09a8</u>";
    };
    if (stringKey instanceof Language_Types.REPORT_AN_ISSUE) {
        return "\u098f\u0995\u099f\u09bf \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b0\u09bf\u09aa\u09cb\u09b0\u09cd\u099f";
    };
    if (stringKey instanceof Language_Types.DISTANCE) {
        return "\u09a6\u09c2\u09b0\u09a4\u09cd\u09ac";
    };
    if (stringKey instanceof Language_Types.TIME_TAKEN) {
        return "\u09b8\u09ae\u09af\u09bc \u09a8\u09bf\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.CALL) {
        return "\u0995\u09b2";
    };
    if (stringKey instanceof Language_Types.START_RIDE) {
        return "\u09af\u09be\u09a4\u09cd\u09b0\u09be \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CANCEL_RIDE) {
        return "\u09af\u09be\u09a4\u09cd\u09b0\u09be \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09c7\u09a8 \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09a4\u09c7 \u099a\u09be\u09a8 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09ac\u09b2\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.MANDATORY) {
        return "\u09ac\u09be\u09a7\u09cd\u09af\u09a4\u09be\u09ae\u09c2\u09b2\u0995";
    };
    if (stringKey instanceof Language_Types.END_RIDE) {
        return "\u09b6\u09c7\u09b7 \u09af\u09be\u09a4\u09cd\u09b0\u09be";
    };
    if (stringKey instanceof Language_Types.RIDE_COMPLETED_WITH) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09be\u09a4\u09cd\u09b0\u09be \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3";
    };
    if (stringKey instanceof Language_Types.COLLECT_AMOUNT_IN_CASH) {
        return "\u09a8\u0997\u09a6 \u0985\u09b0\u09cd\u09a5 \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CASH_COLLECTED) {
        return "\u09a8\u0997\u09a6 \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.OFFLINE) {
        return "\u0985\u09ab\u09b2\u09be\u0987\u09a8";
    };
    if (stringKey instanceof Language_Types.ACCEPT_FOR) {
        return "\u099c\u09a8\u09cd\u09af \u0997\u09cd\u09b0\u09b9\u09a3:";
    };
    if (stringKey instanceof Language_Types.DECLINE) {
        return "\u09aa\u09a4\u09a8";
    };
    if (stringKey instanceof Language_Types.REQUEST) {
        return "\u0985\u09a8\u09c1\u09b0\u09cb\u09a7";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_OFFLINE) {
        return "\u0986\u09aa\u09a8\u09bf \u0985\u09ab\u09b2\u09be\u0987\u09a8";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS) {
        return "\u0986\u09aa\u09a8\u09bf \u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8\u09c7 \u09ac\u09cd\u09af\u09b8\u09cd\u09a4\u0964 \u099f\u09cd\u09b0\u09bf\u09aa \u0985\u09a8\u09c1\u09b0\u09cb\u09a7\u0997\u09c1\u09b2\u09bf \u09aa\u09c7\u09a4\u09c7 \u0985\u09a8\u09b2\u09be\u0987\u09a8\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE) {
        return "\u0985\u09ab\u09b2\u09be\u0987\u09a8\u09c7 \u09af\u09be\u0993\u09af\u09bc\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u0995\u09cb\u09a8\u0993 \u09af\u09be\u09a4\u09cd\u09b0\u09be \u09aa\u09be\u09ac\u09c7 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.CANCEL) {
        return "\u09ac\u09be\u09a4\u09bf\u09b2";
    };
    if (stringKey instanceof Language_Types.GO_OFFLINE) {
        return "\u0985\u09ab\u09b2\u09be\u0987\u09a8 \u09af\u09c7\u09a4\u09c7";
    };
    if (stringKey instanceof Language_Types.IS_WAITING_FOR_YOU) {
        return "\u09a4\u09cb\u09ae\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ON_A_RIDE) {
        return "\u0986\u09aa\u09a8\u09bf \u098f\u0995\u099f\u09bf \u09af\u09be\u09a4\u09cd\u09b0\u09be\u09af\u09bc \u0986\u099b\u09c7\u09a8 ...";
    };
    if (stringKey instanceof Language_Types.PLEASE_ASK_RIDER_FOR_THE_OTP) {
        return "\u0993\u099f\u09bf\u09aa\u09bf\u09b0 \u099c\u09a8\u09cd\u09af \u09b0\u09be\u0987\u09a1\u09be\u09b0\u0995\u09c7 \u099c\u09bf\u099c\u09cd\u099e\u09be\u09b8\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.COMPLETED_) {
        return "\u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3";
    };
    if (stringKey instanceof Language_Types.CANCELLED_) {
        return "\u09ac\u09be\u09a4\u09bf\u09b2";
    };
    if (stringKey instanceof Language_Types.WE_NEED_SOME_ACCESS) {
        return "\u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u0985\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09c7\u09b8 \u0985\u09a8\u09c1\u09b8\u09b0\u09a3 \u0995\u09b0\u09c1\u09a8!";
    };
    if (stringKey instanceof Language_Types.ALLOW_ACCESS) {
        return "\u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0\u09c7\u09b0 \u0985\u09a8\u09c1\u09ae\u09a4\u09bf";
    };
    if (stringKey instanceof Language_Types.THANK_YOU_FOR_WRITING_TO_US) {
        return "\u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b2\u09c7\u0996\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09a7\u09a8\u09cd\u09af\u09ac\u09be\u09a6!";
    };
    if (stringKey instanceof Language_Types.RIDER) {
        return "\u09b0\u09be\u0987\u09a1\u09be\u09b0";
    };
    if (stringKey instanceof Language_Types.TRIP_ID) {
        return "\u099f\u09cd\u09b0\u09bf\u09aa \u0986\u0987\u09a1\u09bf";
    };
    if (stringKey instanceof Language_Types.NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST) {
        return "\u0985\u09cd\u09af\u09be\u09aa\u099f\u09bf \u09ac\u09cd\u09af\u09be\u0995\u0997\u09cd\u09b0\u09be\u0989\u09a8\u09cd\u09a1\u09c7 \u09a5\u09be\u0995\u09be\u0995\u09be\u09b2\u09c0\u09a8 \u0986\u0997\u09a4 \u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u09aa\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP) {
        return "\u09aa\u09cd\u09b0\u09b8\u09cd\u09a4\u09be\u09ac\u09bf\u09a4, \u0985\u09cd\u09af\u09be\u09aa\u099f\u09bf\u0995\u09c7 \u09a6\u09c0\u09b0\u09cd\u0998\u0995\u09be\u09b2 \u09ac\u09cd\u09af\u09be\u0995\u0997\u09cd\u09b0\u09be\u0989\u09a8\u09cd\u09a1\u09c7 \u099a\u09be\u09b2\u09be\u09a4\u09c7 \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.NEED_IT_TO_AUTOSTART_YOUR_APP) {
        return "\u0985\u09cd\u09af\u09be\u09aa\u099f\u09bf\u0995\u09c7 \u09aa\u099f\u09ad\u09c2\u09ae\u09bf\u09a4\u09c7 \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09b0\u09c7\u0996\u09c7 \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u0995\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.NEED_IT_TO_ENABLE_LOCATION) {
        return "\u099c\u09be\u099f\u09bf \u09b8\u09be\u09a5\u09bf \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09a8\u09bf\u09b0\u09c0\u0995\u09cd\u09b7\u09a3\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u099f\u09bf \u09ad\u09be\u0997 \u0995\u09b0\u09c7 \u09a8\u09bf\u09a4\u09c7 \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09a4\u09c7 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7\u09b0 \u09a1\u09c7\u099f\u09be \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7, \u098f\u09ae\u09a8\u0995\u09bf \u0985\u09cd\u09af\u09be\u09aa\u099f\u09bf \u09ac\u09a8\u09cd\u09a7 \u09a5\u09be\u0995\u09b2\u09c7\u0993 \u09ac\u09be \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0 \u09a8\u09be \u0995\u09b0\u09be \u09b9\u09af\u09bc\u0964";
    };
    if (stringKey instanceof Language_Types.OVERLAY_TO_DRAW_OVER_APPLICATIONS) {
        return "\u0985\u09cd\u09af\u09be\u09aa\u09cd\u09b2\u09bf\u0995\u09c7\u09b6\u09a8\u0997\u09c1\u09b2\u09bf \u0986\u0981\u0995\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.BATTERY_OPTIMIZATIONS) {
        return "\u09ac\u09cd\u09af\u09be\u099f\u09be\u09b0\u09bf \u0985\u09aa\u09cd\u099f\u09bf\u09ae\u09be\u0987\u099c\u09c7\u09b6\u09a8";
    };
    if (stringKey instanceof Language_Types.AUTO_START_APPLICATION_IN_BACKGROUND) {
        return "\u09aa\u099f\u09ad\u09c2\u09ae\u09bf\u09a4\u09c7 \u0985\u099f\u09cb\u09b8\u09cd\u099f\u09be\u09b0\u09cd\u099f \u0985\u09cd\u09af\u09be\u09aa";
    };
    if (stringKey instanceof Language_Types.LOCATION_ACCESS) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u0985\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09c7\u09b8";
    };
    if (stringKey instanceof Language_Types.ENTER_RC_NUMBER) {
        return "\u0986\u09b0\u09b8\u09bf \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.WHERE_IS_MY_RC_NUMBER) {
        return "\u0986\u09ae\u09be\u09b0 \u0986\u09b0\u09b8\u09bf \u09a8\u09ae\u09cd\u09ac\u09b0\u099f\u09bf \u0995\u09cb\u09a5\u09be\u09af\u09bc?";
    };
    if (stringKey instanceof Language_Types.STEP) {
        return "\u09aa\u09a6\u0995\u09cd\u09b7\u09c7\u09aa";
    };
    if (stringKey instanceof Language_Types.PAID) {
        return "\u09aa\u09cd\u09b0\u09a6\u09a4\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.ENTERED_WRONG_OTP) {
        return "\u09ad\u09c1\u09b2 \u0993\u099f\u09bf\u09aa\u09bf \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.OTP_INVALID_FOR_THIS_VEHICLE_VARIANT) {
        return "\u0985\u09ac\u09c8\u09a7 OTP - \u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09a7\u09b0\u09a8 \u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u09aa\u09cd\u09b0\u0995\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09ae\u09c7\u09b2\u09c7 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.COPIED) {
        return "\u0985\u09a8\u09c1\u09b2\u09bf\u09aa\u09bf";
    };
    if (stringKey instanceof Language_Types.BANK_NAME) {
        return "\u09ac\u09cd\u09af\u09be\u0982\u0995\u09c7\u09b0 \u09a8\u09be\u09ae";
    };
    if (stringKey instanceof Language_Types.AADHAR_DETAILS) {
        return "\u0986\u09a7\u09be\u09b0\u09c7\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.AADHAR_NUMBER) {
        return "\u0986\u09a7\u09be\u09b0 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.FRONT_SIDE_IMAGE) {
        return "\u09b8\u09be\u09ae\u09a8\u09c7\u09b0 \u09a6\u09bf\u0995\u09c7\u09b0 \u099a\u09bf\u09a4\u09cd\u09b0";
    };
    if (stringKey instanceof Language_Types.BACK_SIDE_IMAGE) {
        return "\u09aa\u09bf\u099b\u09a8\u09c7\u09b0 \u09a6\u09bf\u0995\u09c7\u09b0 \u099a\u09bf\u09a4\u09cd\u09b0";
    };
    if (stringKey instanceof Language_Types.STILL_NOT_RESOLVED) {
        return "\u098f\u0996\u09a8\u0993 \u09b8\u09ae\u09be\u09a7\u09be\u09a8 \u09a8\u09be? \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u0995\u09b2";
    };
    if (stringKey instanceof Language_Types.CASE_TWO) {
        return "\u0996)";
    };
    if (stringKey instanceof Language_Types.NON_DISCLOUSER_AGREEMENT) {
        return "\u0995\u09cb\u09a8 \u09aa\u09cd\u09b0\u0995\u09be\u09b6 \u099a\u09c1\u0995\u09cd\u09a4\u09bf \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.DATA_COLLECTION_AUTHORITY) {
        return "\u0997) \u0986\u09ae\u09bf \u098f\u0987\u09ad\u09be\u09ac\u09c7 \u0986\u09ae\u09be\u09b0 \u09a4\u09a5\u09cd\u09af \u09b8\u0982\u0997\u09cd\u09b0\u09b9\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u099c\u09c1\u09b8\u09cd\u09aa\u0995\u09c7 \u09a8\u09bf\u09af\u09bc\u09cb\u0997 \u0993 \u0985\u09a8\u09c1\u09ae\u09cb\u09a6\u09a8 \u0995\u09b0\u09bf \u098f\u09ac\u0982 \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u0993\u09af\u09bc\u09be\u09b0 \u09ae\u09be\u09a7\u09cd\u09af\u09ae\u09c7 \u0986\u09ae\u09bf \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0\u09c7\u09b0 \u09b6\u09b0\u09cd\u09a4\u09be\u09a6\u09bf \u098f\u09ac\u0982 \u0997\u09cb\u09aa\u09a8\u09c0\u09af\u09bc\u09a4\u09be \u09a8\u09c0\u09a4\u09bf\u09ae\u09be\u09b2\u09be\u09b0 \u09b8\u09be\u09a5\u09c7 \u09b8\u09ae\u09cd\u09ae\u09a4 \u09b9\u0987";
    };
    if (stringKey instanceof Language_Types.SOFTWARE_LICENSE) {
        return "\u09b8\u09ab\u09cd\u099f\u0993\u09af\u09bc\u09cd\u09af\u09be\u09b0 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8";
    };
    if (stringKey instanceof Language_Types.LOAD_MORE) {
        return "\u0986\u09b0 \u09a2\u09c1\u0995\u09be\u0993";
    };
    if (stringKey instanceof Language_Types.ARE_YOU_SURE_YOU_WANT_TO_LOGOUT) {
        return "\u0986\u09aa\u09a8\u09bf \u09b2\u0997 \u0986\u0989\u099f \u0995\u09b0\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.GO_BACK) {
        return "\u09ab\u09bf\u09b0\u09c7 \u09af\u09be\u0993";
    };
    if (stringKey instanceof Language_Types.THANK_YOU_FOR_REGISTERING_US) {
        return "\u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09a7\u09a8\u09cd\u09af\u09ac\u09be\u09a6!";
    };
    if (stringKey instanceof Language_Types.UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU) {
        return "\u09a6\u09c1\u09b0\u09cd\u09ad\u09be\u0997\u09cd\u09af\u0995\u09cd\u09b0\u09ae\u09c7, \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u098f\u0996\u09a8\u0993 \u0989\u09aa\u09b2\u09ac\u09cd\u09a7 \u09a8\u09c7\u0987\u0964 \u0986\u09ae\u09b0\u09be \u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u0985\u09ac\u09b9\u09bf\u09a4 \u0995\u09b0\u09ac\u0964";
    };
    if (stringKey instanceof Language_Types.ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u09af\u09c7 \u0986\u09aa\u09a8\u09bf \u09af\u09be\u09a4\u09cd\u09b0\u09be\u099f\u09bf \u09b6\u09c7\u09b7 \u0995\u09b0\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.EMPTY_RIDES) {
        return "\u0996\u09be\u09b2\u09bf \u09b0\u09be\u0987\u09a1";
    };
    if (stringKey instanceof Language_Types.YOU_HAVE_NOT_TAKEN_A_TRIP_YET) {
        return "\u0986\u09aa\u09a8\u09bf \u098f\u0996\u09a8\u0993 \u098f\u0995\u099f\u09bf \u099f\u09cd\u09b0\u09bf\u09aa \u09a8\u09bf\u099a\u09cd\u099b\u09c7\u09a8 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.BOOK_NOW) {
        return "\u098f\u0996\u09a8\u0987 \u09ac\u09c1\u0995 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RESEND_OTP_IN) {
        return "<u>\u0993\u099f\u09bf\u09aa\u09bf \u0987\u09a8 \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09aa\u09be\u09a0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.WE_NEED_ACCESS_TO_YOUR_LOCATION) {
        return "\u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7\u09b0 \u0985\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09c7\u09b8 \u09a6\u09b0\u0995\u09be\u09b0!";
    };
    if (stringKey instanceof Language_Types.YOUR_LOCATION_HELPS_OUR_SYSTEM) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u099f\u09bf \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b8\u09bf\u09b8\u09cd\u099f\u09c7\u09ae\u0995\u09c7 \u0985\u099f\u09cb\u09b8 \u09a6\u09cd\u09ac\u09be\u09b0\u09be \u09b8\u09ae\u09b8\u09cd\u09a4 \u0995\u09be\u099b\u09be\u0995\u09be\u099b\u09bf \u09ae\u09be\u09a8\u099a\u09bf\u09a4\u09cd\u09b0 \u0995\u09b0\u09a4\u09c7 \u098f\u09ac\u0982 \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09a6\u09cd\u09b0\u09c1\u09a4\u09a4\u09ae \u09af\u09be\u09a4\u09cd\u09b0\u09be \u09b8\u09ae\u09cd\u09ad\u09ac \u0995\u09b0\u09a4\u09c7 \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u0995\u09b0\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.NO_INTERNET_CONNECTION) {
        return "\u0995\u09cb\u09a8\u0993 \u0987\u09a8\u09cd\u099f\u09be\u09b0\u09a8\u09c7\u099f \u09b8\u0982\u09af\u09cb\u0997 \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u0987\u09a8\u09cd\u099f\u09be\u09b0\u09a8\u09c7\u099f \u09b8\u0982\u09af\u09cb\u0997\u099f\u09bf \u09aa\u09b0\u09c0\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TRY_AGAIN) {
        return "\u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0";
    };
    if (stringKey instanceof Language_Types.GRANT_ACCESS) {
        return "\u0985\u09a8\u09c1\u09a6\u09be\u09a8 \u0985\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09c7\u09b8";
    };
    if (stringKey instanceof Language_Types.YOUR_LIMIT_EXCEEDED_TRY_AGAIN_AFTER_10_MIN) {
        return "\u0986\u09aa\u09a8\u09bf \u09b8\u09c0\u09ae\u09be \u099b\u09be\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u09a8, 10 \u09ae\u09bf\u09a8\u09bf\u099f\u09c7\u09b0 \u09aa\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_REFERRAL_MOBILE_NUMBER) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.APPLY) {
        return "\u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.HAVE_A_REFERRAL) {
        return "\u098f\u0995\u099f\u09bf \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0986\u099b\u09c7?";
    };
    if (stringKey instanceof Language_Types.ADD_HERE) {
        return "\u098f\u0996\u09be\u09a8\u09c7 \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.REFERRAL_APPLIED) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u0997!";
    };
    if (stringKey instanceof Language_Types.SMALLEDIT) {
        return "\u09b8\u09ae\u09cd\u09aa\u09be\u09a6\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.ADD_DRIVING_LICENSE) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8 \u09af\u09c1\u0995\u09cd\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.HELP) {
        return "\u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af?";
    };
    if (stringKey instanceof Language_Types.INVALID_DL_NUMBER) {
        return "\u0985\u09ac\u09c8\u09a7 \u09a1\u09bf\u098f\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.DRIVING_LICENSE_NUMBER) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.RE_ENTER_DRIVING_LICENSE_NUMBER) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_DL_NUMBER) {
        return "\u09a1\u09bf\u098f\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SELECT_DATE_OF_BIRTH) {
        return "\u099c\u09a8\u09cd\u09ae \u09a4\u09be\u09b0\u09bf\u0996 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DATE_OF_BIRTH) {
        return "\u099c\u09a8\u09cd\u09ae \u09a4\u09be\u09b0\u09bf\u0996";
    };
    if (stringKey instanceof Language_Types.WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION) {
        return "\u09b8\u09b9\u099c \x0a \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u0995\u09b0\u09a3\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u098f\u0995\u099f\u09bf \u099f\u09bf\u0989\u099f\u09cb\u09b0\u09bf\u09af\u09bc\u09be\u09b2 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_MINIMUM_FIFTEEN_CHARACTERS) {
        return "\u09ae\u09bf\u09a8\u09bf\u09af\u09bc\u09be\u09ae 15 \u0985\u0995\u09cd\u09b7\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_YOUR_FRIEND) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09a8\u09cd\u09a7\u09c1 \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE) {
        return "\u099a\u09bf\u09a4\u09cd\u09b0\u099f\u09bf \u09ac\u09c8\u09a7 \u0995\u09b0\u09be\u09b0 \u09b8\u09ae\u09af\u09bc \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.VALIDATING) {
        return "\u09ac\u09c8\u09a7\u0995\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.VERIFICATION_PENDING) {
        return "\u09af\u09be\u099a\u09be\u0987\u0995\u09b0\u09a3 \u09ae\u09c1\u09b2\u09a4\u09c1\u09ac\u09bf";
    };
    if (stringKey instanceof Language_Types.VERIFICATION_FAILED) {
        return "\u09af\u09be\u099a\u09be\u0987 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5";
    };
    if (stringKey instanceof Language_Types.NO_DOC_AVAILABLE) {
        return "\u0995\u09cb\u09a8 \u09a8\u09a5\u09bf \u0989\u09aa\u09b2\u09ac\u09cd\u09a7";
    };
    if (stringKey instanceof Language_Types.ISSUE_WITH_DL_IMAGE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09a1\u09bf\u098f\u09b2 \u099a\u09bf\u09a4\u09cd\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u0995\u09bf\u099b\u09c1 \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b0\u09af\u09bc\u09c7\u099b\u09c7 \u09ac\u09b2\u09c7 \u09ae\u09a8\u09c7 \u09b9\u099a\u09cd\u099b\u09c7, \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8 \u09a6\u09b2\u099f\u09bf \u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u0995\u09b0\u09ac\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.STILL_HAVE_SOME_DOUBT) {
        return "\u098f\u0996\u09a8\u0993 \u0995\u09bf\u099b\u09c1 \u09b8\u09a8\u09cd\u09a6\u09c7\u09b9 \u0986\u099b\u09c7?";
    };
    if (stringKey instanceof Language_Types.ISSUE_WITH_RC_IMAGE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0986\u09b0\u09b8\u09bf \u099a\u09bf\u09a4\u09cd\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u0995\u09bf\u099b\u09c1 \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b0\u09af\u09bc\u09c7\u099b\u09c7 \u09ac\u09b2\u09c7 \u09ae\u09a8\u09c7 \u09b9\u099a\u09cd\u099b\u09c7, \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8 \u09a6\u09b2\u099f\u09bf \u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u0995\u09b0\u09ac\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT) {
        return "\u09ac\u09c8\u09a7 \u09a1\u0995\u09c1\u09ae\u09c7\u09a8\u09cd\u099f \u0987\u09ae\u09c7\u099c \u0995\u09bf\u09a8\u09be \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u099a\u09bf\u09a4\u09cd\u09b0\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u099a\u09c7\u0995 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED) {
        return "\u0993\u09ab\u09b8! \u0986\u09aa\u09a8\u09be\u09b0 \u0986\u09ac\u09c7\u09a6\u09a8 \u09aa\u09cd\u09b0\u09a4\u09cd\u09af\u09be\u0996\u09cd\u09af\u09be\u09a8 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9\u09aa\u09c2\u09b0\u09cd\u09ac\u0995 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.INVALID_DRIVING_LICENSE) {
        return "\u0985\u09ac\u09c8\u09a7 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8";
    };
    if (stringKey instanceof Language_Types.LIMIT_EXCEEDED_FOR_DL_UPLOAD) {
        return "\u09a1\u09bf\u098f\u09b2 \u0986\u09aa\u09b2\u09cb\u09a1\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09c0\u09ae\u09be\u09ac\u09a6\u09cd\u09a7\u09a4\u09be \u099b\u09be\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.INVALID_VEHICLE_REGISTRATION_CERTIFICATE) {
        return "\u0985\u09ac\u09c8\u09a7 \u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u0995\u09b0\u09a3 \u09b6\u0982\u09b8\u09be\u09aa\u09a4\u09cd\u09b0";
    };
    if (stringKey instanceof Language_Types.LIMIT_EXCEEDED_FOR_RC_UPLOAD) {
        return "\u0986\u09b0\u09b8\u09bf \u0986\u09aa\u09b2\u09cb\u09a1\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09c0\u09ae\u09be\u09ac\u09a6\u09cd\u09a7\u09a4\u09be \u099b\u09be\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u0997\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.YOUR_DOCUMENTS_ARE_APPROVED) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09a8\u09a5\u09bf \u0985\u09a8\u09c1\u09ae\u09cb\u09a6\u09bf\u09a4\u0964 \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8 \u09a6\u09b2 \u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09ac\u09c7\u0964 \u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09a4\u09c7 \u0986\u09aa\u09a8\u09bf \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8 \u09a6\u09b2\u0995\u09c7 \u0995\u09b2 \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.APPLICATION_STATUS) {
        return "\u0986\u09ac\u09c7\u09a6\u09a8\u09aa\u09a4\u09cd\u09b0\u09c7\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be";
    };
    if (stringKey instanceof Language_Types.FOR_SUPPORT) {
        return "\u09b8\u09ae\u09b0\u09cd\u09a5\u09a8 \u099c\u09a8\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.CONTACT_US) {
        return "\u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.IMAGE_VALIDATION_FAILED) {
        return "\u099a\u09bf\u09a4\u09cd\u09b0\u09c7\u09b0 \u09ac\u09c8\u09a7\u09a4\u09be \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.IMAGE_NOT_READABLE) {
        return "\u099a\u09bf\u09a4\u09cd\u09b0 \u09aa\u09a0\u09a8\u09af\u09cb\u0997\u09cd\u09af \u09a8\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.IMAGE_LOW_QUALITY) {
        return "\u099a\u09bf\u09a4\u09cd\u09b0\u09c7\u09b0 \u09ae\u09be\u09a8 \u09ad\u09be\u09b2 \u09a8\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.IMAGE_INVALID_TYPE) {
        return "\u09aa\u09cd\u09b0\u09a6\u09a4\u09cd\u09a4 \u099a\u09bf\u09a4\u09cd\u09b0\u09c7\u09b0 \u09a7\u09b0\u09a3\u099f\u09bf \u09aa\u09cd\u09b0\u0995\u09c3\u09a4 \u09a7\u09b0\u09a3\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09ae\u09c7\u09b2\u09c7 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.IMAGE_DOCUMENT_NUMBER_MISMATCH) {
        return "\u098f\u0987 \u099a\u09bf\u09a4\u09cd\u09b0\u09c7\u09b0 \u09a1\u0995\u09c1\u09ae\u09c7\u09a8\u09cd\u099f \u09a8\u09ae\u09cd\u09ac\u09b0\u099f\u09bf \u0987\u09a8\u09aa\u09c1\u099f\u099f\u09bf\u09b0 \u09b8\u09be\u09a5\u09c7 \u09ae\u09c7\u09b2\u09c7 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.IMAGE_EXTRACTION_FAILED) {
        return "\u099a\u09bf\u09a4\u09cd\u09b0 \u09a8\u09bf\u09b7\u09cd\u0995\u09be\u09b6\u09a8 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.IMAGE_NOT_FOUND) {
        return "\u099b\u09ac\u09bf\u099f\u09bf \u0996\u09c1\u0981\u099c\u09c7 \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.IMAGE_NOT_VALID) {
        return "\u099a\u09bf\u09a4\u09cd\u09b0 \u09ac\u09c8\u09a7 \u09a8\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.DRIVER_ALREADY_LINKED) {
        return "\u0985\u09a8\u09cd\u09af\u09be\u09a8\u09cd\u09af \u09a1\u0995 \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09c1\u0995\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.DL_ALREADY_UPDATED) {
        return "\u0995\u09b0\u09cd\u09ae \u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u099c\u09a8. \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8 \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09c1\u0995\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.RC_ALREADY_LINKED) {
        return "\u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u0986\u09b0\u09b8\u09bf \u0989\u09aa\u09b2\u09ad\u09cd\u09af \u09a8\u09af\u09bc\u0964 \u0985\u09a8\u09cd\u09af\u09be\u09a8\u09cd\u09af \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09c1\u0995\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.RC_ALREADY_UPDATED) {
        return "\u0995\u09b0\u09cd\u09ae \u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u099c\u09a8. \u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u0986\u09b0\u09b8\u09bf \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09c1\u0995\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.DL_ALREADY_LINKED) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8 \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09af\u09bc \u09a8\u09be\u0964 \u0985\u09a8\u09cd\u09af\u09be\u09a8\u09cd\u09af \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09c1\u0995\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.SOMETHING_WENT_WRONG) {
        return "\u0995\u09bf\u099b\u09c1 \u09ad\u09c1\u09b2 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.PICKUP) {
        return "\u09aa\u09bf\u0995\u0986\u09aa";
    };
    if (stringKey instanceof Language_Types.TRIP) {
        return "\u099f\u09cd\u09b0\u09bf\u09aa";
    };
    if (stringKey instanceof Language_Types.CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER) {
        return "\u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8\u09c7, \u0986\u09ae\u09b0\u09be \u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u09aa\u09b6\u09cd\u099a\u09bf\u09ae\u09ac\u0999\u09cd\u0997 \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09bf\u09a4 \u09a8\u09ae\u09cd\u09ac\u09b0 \u0985\u09a8\u09c1\u09ae\u09cb\u09a6\u09a8 \u0995\u09b0\u09bf";
    };
    if (stringKey instanceof Language_Types.UPDATED_AT) {
        return "\u09ae\u09cd\u09af\u09be\u09aa \u09b6\u09c7\u09b7 \u0986\u09aa\u09a1\u09c7\u099f";
    };
    if (stringKey instanceof Language_Types.TRIP_COUNT) {
        return "\u0986\u099c\u0995\u09c7\u09b0 \u099f\u09cd\u09b0\u09bf\u09aa\u09b8";
    };
    if (stringKey instanceof Language_Types.TODAYS_EARNINGS) {
        return "\u0986\u099c\u0995\u09c7\u09b0 \u0989\u09aa\u09be\u09b0\u09cd\u099c\u09a8";
    };
    if (stringKey instanceof Language_Types.BONUS_EARNED) {
        return "\u09ac\u09cb\u09a8\u09be\u09b8 \u0985\u09b0\u09cd\u099c\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.GOT_IT) {
        return "\u09ac\u09c1\u099d\u09c7\u099b\u09bf!";
    };
    if (stringKey instanceof Language_Types.WHAT_IS_NAMMA_YATRI_BONUS) {
        return "\u09ac\u09cb\u09a8\u09be\u09b8 \u0995\u09bf?";
    };
    if (stringKey instanceof Language_Types.BONUS_PRIMARY_TEXT) {
        return "\u09a8\u09ae\u09cd\u09ae\u09be \u09af\u09be\u09a4\u09cd\u09b0\u09c0 \u09ac\u09cb\u09a8\u09be\u09b8 \u09b9\u09b2 \u0985\u09a4\u09bf\u09b0\u09bf\u0995\u09cd\u09a4 \u09aa\u09b0\u09bf\u09ae\u09be\u09a3 \u09af\u09be \u0986\u09aa\u09a8\u09bf \u09ae\u09bf\u099f\u09be\u09b0 \u099a\u09be\u09b0\u09cd\u099c\u09c7\u09b0 \u0989\u09aa\u09b0\u09c7 \u09aa\u09bf\u0995\u0986\u09aa \u099a\u09be\u09b0\u09cd\u099c, \u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u099f\u09bf\u09aa\u09b8 \u098f\u09ac\u0982 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u09af\u09cb\u0997\u09c7\u09b0 \u0986\u0995\u09be\u09b0\u09c7 \u0985\u09b0\u09cd\u099c\u09a8 \u0995\u09b0\u09c7\u099b\u09c7\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.BONUS_SECONDARY_TEXT) {
        return "\u09a8\u09ae\u09cd\u09ae\u09be \u09af\u09be\u09a4\u09cd\u09b0\u09c0 \u09ac\u09cb\u09a8\u09be\u09b8\u09c7\u09b0 \u09aa\u09b0\u09bf\u09ae\u09be\u09a3 \u0986\u09aa\u09a8\u09be\u09b0 \u09ae\u09cb\u099f \u0989\u09aa\u09be\u09b0\u09cd\u099c\u09a8\u09c7\u09b0 \u0985\u0982\u09b6\u0964";
    };
    if (stringKey instanceof Language_Types.DATE_OF_REGISTRATION) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09a8\u09c7\u09b0 \u09a4\u09be\u09b0\u09bf\u0996";
    };
    if (stringKey instanceof Language_Types.SELECT_DATE_OF_ISSUE) {
        return "\u0987\u09b8\u09cd\u09af\u09c1\u09b0 \u09a4\u09be\u09b0\u09bf\u0996 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DATE_OF_ISSUE) {
        return "\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u098f\u09b0 \u09a4\u09be\u09b0\u09bf\u0996";
    };
    if (stringKey instanceof Language_Types.PROVIDE_DATE_OF_ISSUE_TEXT) {
        return "\u09a6\u09c1\u0983\u0996\u09bf\u09a4, \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09bf\u09b6\u09a6\u099f\u09bf \u09ac\u09c8\u09a7\u09a4\u09be \u09a6\u09bf\u09a4\u09c7 \u09aa\u09be\u09b0\u09bf \u09a8\u09be, \u0986\u09aa\u09a8\u09be\u09b0 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8\u099f\u09bf \u09ac\u09c8\u09a7\u09a4\u09be \u09aa\u09c7\u09a4\u09c7 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 <b> \u0987\u09b8\u09cd\u09af\u09c1\u09b0 \u09a4\u09be\u09b0\u09bf\u0996 </b> \u09b8\u09b0\u09ac\u09b0\u09be\u09b9 \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.PROVIDE_DATE_OF_REGISTRATION_TEXT) {
        return "\u09a6\u09c1\u0983\u0996\u09bf\u09a4 \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09bf\u09b6\u09a6\u099f\u09bf \u09ac\u09c8\u09a7\u09a4\u09be \u09a6\u09bf\u09a4\u09c7 \u09aa\u09be\u09b0\u09bf \u09a8\u09be, \u0986\u09aa\u09a8\u09be\u09b0 \u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09ac\u09bf\u09b6\u09a6\u099f\u09bf \u09ac\u09c8\u09a7\u09a4\u09be \u09aa\u09c7\u09a4\u09c7 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 <b> \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u0995\u09b0\u09a3\u09c7\u09b0 \u09a4\u09be\u09b0\u09bf\u0996 </b> \u09b8\u09b0\u09ac\u09b0\u09be\u09b9 \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.SELECT_DATE_OF_REGISTRATION) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u0995\u09b0\u09a3\u09c7\u09b0 \u09a4\u09be\u09b0\u09bf\u0996 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SAME_REENTERED_RC_MESSAGE) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u09b9\u09af\u09bc\u09c7 \u09a8\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.SAME_REENTERED_DL_MESSAGE) {
        return "\u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09be \u09a1\u09bf\u098f\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u0989\u09aa\u09b0\u09c7 \u09aa\u09cd\u09b0\u09a6\u09a4\u09cd\u09a4 \u09a1\u09bf\u098f\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0\u099f\u09bf\u09b0 \u09b8\u09be\u09a5\u09c7 \u09ae\u09c7\u09b2\u09c7 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.WHERE_IS_MY_ISSUE_DATE) {
        return "\u0986\u09ae\u09be\u09b0 \u0987\u09b8\u09cd\u09af\u09c1 \u09a4\u09be\u09b0\u09bf\u0996 \u0995\u09cb\u09a5\u09be\u09af\u09bc?";
    };
    if (stringKey instanceof Language_Types.WHERE_IS_MY_REGISTRATION_DATE) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09c7\u09b0 \u09a4\u09be\u09b0\u09bf\u0996 \u0995\u09cb\u09a5\u09be\u09af\u09bc?";
    };
    if (stringKey instanceof Language_Types.EARNINGS_CREDITED_IN_ACCOUNT) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0989\u09aa\u09be\u09b0\u09cd\u099c\u09a8 \u098f\u0987 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f\u09c7 \u099c\u09ae\u09be \u09a6\u09c7\u0993\u09af\u09bc\u09be \u09b9\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.INVALID_PARAMETERS) {
        return "\u0985\u09ac\u09c8\u09a7 \u09aa\u09b0\u09be\u09ae\u09bf\u09a4\u09bf";
    };
    if (stringKey instanceof Language_Types.UNAUTHORIZED) {
        return "\u0985\u09a8\u09a8\u09c1\u09ae\u09cb\u09a6\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.INVALID_TOKEN) {
        return "\u0985\u09ac\u09c8\u09a7 \u099f\u09cb\u0995\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.SOME_ERROR_OCCURED_IN_OFFERRIDE) {
        return "\u0985\u09ab\u09be\u09b0\u09be\u0987\u09a1\u09c7 \u0995\u09bf\u099b\u09c1 \u09a4\u09cd\u09b0\u09c1\u099f\u09bf \u0998\u099f\u09c7\u099b\u09bf\u09b2";
    };
    if (stringKey instanceof Language_Types.SELECT_VEHICLE_TYPE) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09a7\u09b0\u09a3 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RIDE) {
        return "\u09af\u09be\u09a4\u09cd\u09b0\u09be";
    };
    if (stringKey instanceof Language_Types.NO_LOCATION_UPDATE) {
        return "\u0995\u09cb\u09a8\u0993 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u0986\u09aa\u09a1\u09c7\u099f \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.GOT_IT_TELL_US_MORE) {
        return "\u09aa\u09c7\u09af\u09bc\u09c7\u099b\u09bf, \u0986\u09b0\u0993 \u09ac\u09b2\u09c1\u09a8?";
    };
    if (stringKey instanceof Language_Types.WRITE_A_COMMENT) {
        return "\u098f\u0995\u099f\u09bf \u09ae\u09a8\u09cd\u09a4\u09ac\u09cd\u09af \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.HOW_WAS_YOUR_RIDE_WITH) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09af\u09be\u09a4\u09cd\u09b0\u09be \u0995\u09c7\u09ae\u09a8 \u099b\u09bf\u09b2";
    };
    if (stringKey instanceof Language_Types.RUDE_BEHAVIOUR) {
        return "\u0985\u09ad\u09a6\u09cd\u09b0 \u0986\u099a\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.LONG_WAITING_TIME) {
        return "\u09a6\u09c0\u09b0\u09cd\u0998 \u09b8\u09ae\u09af\u09bc \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be";
    };
    if (stringKey instanceof Language_Types.DIDNT_COME_TO_PICUP_LOCATION) {
        return "\u09aa\u09bf\u0995\u0986\u09aa \u09b2\u09cb\u0995\u09c7\u09b6\u09a8\u09c7 \u0986\u09b8\u09c7\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.HELP_US_WITH_YOUR_REASON) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0995\u09be\u09b0\u09a3 \u09a6\u09bf\u09af\u09bc\u09c7 \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.MAX_CHAR_LIMIT_REACHED) {
        return "\u09b8\u09b0\u09cd\u09ac\u09cb\u099a\u09cd\u099a \u099a\u09b0\u09bf\u09a4\u09cd\u09b0\u09c7\u09b0 \u09b8\u09c0\u09ae\u09be \u09aa\u09cc\u0981\u099b\u09c7\u099b\u09c7,";
    };
    if (stringKey instanceof Language_Types.SHOW_ALL_OPTIONS) {
        return "\u09b8\u09ae\u09b8\u09cd\u09a4 \u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09a6\u09c7\u0996\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.UPDATE_REQUIRED) {
        return "\u0986\u09aa\u09a1\u09c7\u099f \u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u099c\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE) {
        return "\u09aa\u09b0\u09bf\u09b7\u09c7\u09ac\u09be \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7 \u0985\u09cd\u09af\u09be\u09aa\u09cd\u09b2\u09bf\u0995\u09c7\u09b6\u09a8 \u0986\u09aa\u09a1\u09c7\u099f \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NOT_NOW) {
        return "\u098f\u0996\u09a8 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.OF) {
        return "\u098f\u09b0";
    };
    if (stringKey instanceof Language_Types.DROP) {
        return "\u09a1\u09cd\u09b0\u09aa";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT) {
        return "\u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9\u09aa\u09c2\u09b0\u09cd\u09ac\u0995 \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SETTING_YOU_OFFLINE) {
        return "\u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u0985\u09ab\u09b2\u09be\u0987\u09a8 \u09b8\u09c7\u099f \u0995\u09b0\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.SETTING_YOU_ONLINE) {
        return "\u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u0985\u09a8\u09b2\u09be\u0987\u09a8\u09c7 \u09b8\u09c7\u099f \u0995\u09b0\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.SETTING_YOU_SILENT) {
        return "\u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09b8\u09be\u0987\u09b2\u09c7\u09a8\u09cd\u099f \u09b8\u09c7\u099f \u0995\u09b0\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.VIEW_BREAKDOWN) {
        return "\u09ac\u09cd\u09b0\u09c7\u0995\u09a1\u09be\u0989\u09a8 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.APP_INFO) {
        return "\u0985\u09cd\u09af\u09be\u09aa\u09cd\u09b2\u09bf\u0995\u09c7\u09b6\u09a8 \u09a4\u09a5\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.OTHER) {
        return "\u0985\u09a8\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.VEHICLE_ISSUE) {
        return "\u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.FARE_UPDATED) {
        return "\u09ad\u09be\u09a1\u09bc\u09be \u0986\u09aa\u09a1\u09c7\u099f \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES) {
        return "\u0998\u09a8 \u0998\u09a8 \u09ac\u09be\u09a4\u09bf\u09b2\u0995\u09b0\u09a3\u0997\u09c1\u09b2\u09bf \u0995\u09ae \u09b0\u09be\u0987\u09a1 \u098f\u09ac\u0982 \u0995\u09ae \u09b0\u09c7\u099f\u09bf\u0982\u09af\u09bc\u09c7\u09b0 \u09a6\u09bf\u0995\u09c7 \u09aa\u09b0\u09bf\u099a\u09be\u09b2\u09bf\u09a4 \u0995\u09b0\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.CONTINUE) {
        return "\u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.CONFIRM_PASSWORD) {
        return "\u09aa\u09be\u09b8\u0993\u09af\u09bc\u09be\u09b0\u09cd\u09a1 \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DEMO_MODE) {
        return "\u09a1\u09c7\u09ae\u09cb \u09ae\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.PASSWORD) {
        return "\u09aa\u09be\u09b8\u0993\u09af\u09bc\u09be\u09b0\u09cd\u09a1";
    };
    if (stringKey instanceof Language_Types.ENTER_DEMO_MODE_PASSWORD) {
        return "\u09a1\u09c7\u09ae\u09cb \u09ae\u09cb\u09a1 \u09aa\u09be\u09b8\u0993\u09af\u09bc\u09be\u09b0\u09cd\u09a1 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DEMO_MODE_DISABLED) {
        return "\u09a1\u09c7\u09ae\u09cb \u09ae\u09cb\u09a1 \u0985\u0995\u09cd\u09b7\u09ae";
    };
    if (stringKey instanceof Language_Types.ONLINE_VIA_DEMO_MODE) {
        return "\u0985\u09a8\u09b2\u09be\u0987\u09a8 (\u09a1\u09c7\u09ae\u09cb)";
    };
    if (stringKey instanceof Language_Types.MORE) {
        return "\u0986\u09b0\u0993";
    };
    if (stringKey instanceof Language_Types.LESS) {
        return "\u0995\u09ae";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_AT_PICKUP) {
        return "\u0986\u09aa\u09a8\u09bf \u09aa\u09bf\u0995\u0986\u09aa \u09b2\u09cb\u0995\u09c7\u09b6\u09a8\u09c7 \u0986\u099b\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.WAITING_FOR_CUSTOMER) {
        return "\u0986\u09aa\u09a8\u09bf \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u099b\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_NOTIFIED) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995 \u09ac\u09bf\u099c\u09cd\u099e\u09aa\u09cd\u09a4\u09bf";
    };
    if (stringKey instanceof Language_Types.I_ARRIVED) {
        return "\u0986\u09ae\u09bf \u098f\u09b8\u09c7\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.ESTIMATED_RIDE_FARE) {
        return "\u0986\u09a8\u09c1\u09ae\u09be\u09a8\u09bf\u0995 \u09b0\u09be\u0987\u09a1 \u09ad\u09be\u09a1\u09bc\u09be:";
    };
    if (stringKey instanceof Language_Types.PICKUP_TOO_FAR) {
        return "\u09aa\u09bf\u0995\u0986\u09aa \u0996\u09c1\u09ac \u09a6\u09c2\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_NOT_PICKING_CALL) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995 \u0995\u09b2 \u09ac\u09be\u099b\u09be\u0987 \u0995\u09b0\u099b\u09c7\u09a8 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.TRAFFIC_JAM) {
        return "\u099f\u09cd\u09b0\u09be\u09ab\u09bf\u0995 \u099c\u09cd\u09af\u09be\u09ae";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_WAS_RUDE) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995 \u0985\u09ad\u09a6\u09cd\u09b0 \u099b\u09bf\u09b2";
    };
    if (stringKey instanceof Language_Types.ALL_MESSAGES) {
        return "\u09b8\u09ae\u09b8\u09cd\u09a4 \u09ac\u09be\u09b0\u09cd\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.MESSAGES) {
        return "\u09ac\u09be\u09b0\u09cd\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.ADD_A_COMMENT) {
        return "\u098f\u0995\u099f\u09bf \u09ae\u09a8\u09cd\u09a4\u09ac\u09cd\u09af \u09af\u09c1\u0995\u09cd\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.POST_COMMENT) {
        return "\u09aa\u09cb\u09b8\u09cd\u099f \u09ae\u09a8\u09cd\u09a4\u09ac\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.ENTER_YOUR_COMMENT) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09ae\u09a8\u09cd\u09a4\u09ac\u09cd\u09af \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NO_NOTIFICATIONS_RIGHT_NOW) {
        return "\u098f\u0996\u09a8\u0987 \u0995\u09cb\u09a8\u0993 \u09ac\u09bf\u099c\u09cd\u099e\u09aa\u09cd\u09a4\u09bf \u09a8\u09c7\u0987!";
    };
    if (stringKey instanceof Language_Types.NO_NOTIFICATIONS_RIGHT_NOW_DESC) {
        return "\u09af\u0996\u09a8 \u09a8\u09a4\u09c1\u09a8 \u0995\u09cb\u09a8\u0993 \u09ac\u09bf\u099c\u09cd\u099e\u09aa\u09cd\u09a4\u09bf \u0986\u09b8\u09ac\u09c7 \u09a4\u0996\u09a8 \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u099c\u09be\u09a8\u09be\u09ac";
    };
    if (stringKey instanceof Language_Types.ALERTS) {
        return "\u09b8\u09a4\u09b0\u09cd\u0995\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.YOUR_COMMENT) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09ae\u09a8\u09cd\u09a4\u09ac\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.SHOW_MORE) {
        return "\u0986\u09b0\u0993 \u09a6\u09c7\u0996\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.LOAD_OLDER_ALERTS) {
        return "\u09aa\u09c1\u09b0\u09be\u09a8\u09cb \u09b8\u09a4\u09b0\u09cd\u0995\u09a4\u09be\u0997\u09c1\u09b2\u09bf \u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CONTEST) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf\u09af\u09cb\u0997\u09bf\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.YOUR_REFERRAL_CODE_IS_LINKED) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1 \u09b2\u09bf\u0999\u09cd\u0995\u09af\u09c1\u0995\u09cd\u09a4!";
    };
    if (stringKey instanceof Language_Types.YOU_CAN_NOW_EARN_REWARDS) {
        return "\u0986\u09aa\u09a8\u09bf \u098f\u0996\u09a8 \u0997\u09cd\u09b0\u09be\u09b9\u0995\u09a6\u09c7\u09b0 \u0989\u09b2\u09cd\u09b2\u09c7\u0996 \u0995\u09b0\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u09aa\u09c1\u09b0\u09b7\u09cd\u0995\u09be\u09b0 \u0985\u09b0\u09cd\u099c\u09a8 \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8!";
    };
    if (stringKey instanceof Language_Types.COMING_SOON) {
        return "\u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u0986\u09b8\u099b\u09c7!";
    };
    if (stringKey instanceof Language_Types.COMING_SOON_DESCRIPTION) {
        return "\u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u09aa\u09cd\u09b0\u09cb\u0997\u09cd\u09b0\u09be\u09ae\u09c7 \u09ac\u09cb\u09b0\u09cd\u09a1\u09c7 \u09a8\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u0993\u09af\u09bc\u09be\u09b0 \u0995\u09be\u099c \u0995\u09b0\u099b\u09bf\u0964 \u0986\u09b0\u0993 \u09a4\u09a5\u09cd\u09af\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09a4\u09b0\u09cd\u0995\u09a4\u09be \u09aa\u09c3\u09b7\u09cd\u09a0\u09be\u099f\u09bf \u09a6\u09c7\u0996\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.REFERRAL_CODE_HINT) {
        return "6-\u0985\u0999\u09cd\u0995\u09c7\u09b0 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CONFIRM_REFERRAL_CODE) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1 \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CONFIRM_REFERRAL_CODE_HINT) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09be\u09b2 \u0995\u09cb\u09a1 \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.YOUR_REFERRAL_CODE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b0\u09c7\u09ab\u09be\u09b0\u09be\u09b2 \u0995\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.FIRST_REFERRAL_SUCCESSFUL) {
        return "\u09aa\u09cd\u09b0\u09a5\u09ae \u09b0\u09c7\u09ab\u09be\u09b0\u09be\u09b2 \u09b8\u09ab\u09b2! \x0a \u09aa\u09c1\u09b0\u09b7\u09cd\u0995\u09be\u09b0 \u0986\u09a8\u09b2\u0995!";
    };
    if (stringKey instanceof Language_Types.AWAITING_REFERRAL_RIDE) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09be\u09b2 \u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u0996\u09a8 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u09b8\u09a4\u09b0\u09cd\u0995\u09a4\u09be \u09aa\u09be\u09ac\u09c7\u09a8 \u09a4\u0996\u09a8 \u098f\u0987 \u09b8\u09cd\u09a5\u09be\u09a8\u099f\u09bf \u09aa\u09b0\u09c0\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.REFERRED_CUSTOMERS) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09a6\u09c7\u09b0 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09a8\u09cd\u09b8";
    };
    if (stringKey instanceof Language_Types.ACTIVATED_CUSTOMERS) {
        return "\u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0997\u09cd\u09b0\u09be\u09b9\u0995";
    };
    if (stringKey instanceof Language_Types.REFERRAL_CODE_LINKING) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1 \u09b2\u09bf\u0999\u09cd\u0995\u09bf\u0982";
    };
    if (stringKey instanceof Language_Types.CONTACT_SUPPORT) {
        return "\u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8";
    };
    if (stringKey instanceof Language_Types.CALL_SUPPORT) {
        return "\u0995\u09b2 \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT) {
        return "\u0986\u09aa\u09a8\u09bf \u099c\u09be\u09a4\u09cd\u09b0\u09bf \u09b8\u09be\u09a5\u09bf \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8 \u09a6\u09b2\u0995\u09c7 \u0995\u09b2 \u0995\u09b0\u09a4\u09c7 \u099a\u09b2\u09c7\u099b\u09c7\u09a8\u0964 \u0986\u09aa\u09a8\u09bf \u0995\u09bf \u098f\u0997\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.REFERRAL_ENROLMENT) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u09a4\u09be\u09b2\u09bf\u0995\u09be\u09ad\u09c1\u0995\u09cd\u09a4\u09bf";
    };
    if (stringKey instanceof Language_Types.REFERRALS) {
        return "\u09aa\u09cd\u09b0\u099a\u09be\u09b0";
    };
    if (stringKey instanceof Language_Types.LINK_REFERRAL_CODE) {
        return "\u09b2\u09bf\u0999\u09cd\u0995 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.DRIVER_DETAILS) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.FOR_UPDATES_SEE_ALERTS) {
        return "\u0986\u09aa\u09a1\u09c7\u099f\u09c7\u09b0 \u099c\u09a8\u09cd\u09af, \u09b8\u09a4\u09b0\u09cd\u0995\u09a4\u09be\u0997\u09c1\u09b2\u09bf \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SHARE_OPTIONS) {
        return "\u09ad\u09be\u0997 \u09ac\u09bf\u0995\u09b2\u09cd\u09aa";
    };
    if (stringKey instanceof Language_Types.ENTER_PASSWORD) {
        return "\u09aa\u09be\u09b8\u0993\u09af\u09bc\u09be\u09b0\u09cd\u09a1 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.YOUR_VEHICLE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09be\u09b9\u09a8";
    };
    if (stringKey instanceof Language_Types.BOOKING_OPTIONS) {
        return "\u09ac\u09c1\u0995\u09bf\u0982 \u0985\u09aa\u09b6\u09a8";
    };
    if (stringKey instanceof Language_Types.CONFIRM_AND_CHANGE) {
        return "\u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09a8 \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.MAKE_YOURSELF_AVAILABLE_FOR) {
        return "\u098f\u099b\u09be\u09a1\u09bc\u09be\u0993 \u09b0\u09be\u0987\u09a1 \u0997\u09cd\u09b0\u09b9\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SILENT_MODE_PROMPT) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u09a6\u09bf \u09ac\u09bf\u09b0\u0995\u09cd\u09a4 \u09b9\u09a4\u09c7 \u09a8\u09be \u099a\u09be\u09a8 \u09a4\u09ac\u09c7 \u0986\u09aa\u09a8\u09bf \u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09c7 \u09b8\u09be\u0987\u09b2\u09c7\u09a8\u09cd\u099f \u09ae\u09cb\u09a1\u09c7 \u09b8\u09cd\u09af\u09c1\u0987\u099a \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.GO_SILENT) {
        return "\u099a\u09c1\u09aa \u0995\u09b0\u09c7 \u09af\u09be\u0993";
    };
    if (stringKey instanceof Language_Types.TRY_SILENT_MODE) {
        return "\u09a8\u09c0\u09b0\u09ac \u09ae\u09cb\u09a1 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09ac\u09c7\u09a8?";
    };
    if (stringKey instanceof Language_Types.RIDE_FARE) {
        return "\u09b0\u09be\u0987\u09a1 \u09ad\u09be\u09a1\u09bc\u09be";
    };
    if (stringKey instanceof Language_Types.RIDE_DISTANCE) {
        return "\u09af\u09be\u09a4\u09cd\u09b0\u09be \u09a6\u09c2\u09b0\u09a4\u09cd\u09ac";
    };
    if (stringKey instanceof Language_Types.START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS) {
        return "\u098f\u0987 \u09a6\u09cd\u09b0\u09c1\u09a4 \u099a\u09cd\u09af\u09be\u099f \u09aa\u09b0\u09be\u09ae\u09b0\u09cd\u09b6\u0997\u09c1\u09b2\u09bf \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0 \u0995\u09b0\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u099a\u09cd\u09af\u09be\u099f \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.START_YOUR_CHAT_WITH_THE_DRIVER) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u099a\u09cd\u09af\u09be\u099f \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.MESSAGE) {
        return "\u09ac\u09be\u09b0\u09cd\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.I_AM_ON_MY_WAY) {
        return "\u0986\u09ae\u09bf \u0997\u09a8\u09cd\u09a4\u09ac\u09cd\u09af\u09c7\u09b0 \u09aa\u09a5\u09c7";
    };
    if (stringKey instanceof Language_Types.GETTING_DELAYED_PLEASE_WAIT) {
        return "\u09ac\u09bf\u09b2\u09ae\u09cd\u09ac \u09b9\u099a\u09cd\u099b\u09c7, \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.UNREACHABLE_PLEASE_CALL_BACK) {
        return "\u0985\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09c7\u09b8\u09af\u09cb\u0997\u09cd\u09af, \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09ab\u09bf\u09b0\u09c7 \u0995\u09b2 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ARE_YOU_STARING) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u099b\u09c7\u09a8?";
    };
    if (stringKey instanceof Language_Types.PLEASE_COME_SOON) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u0986\u09b8\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OK_I_WILL_WAIT) {
        return "\u0986\u099a\u09cd\u099b\u09be \u0986\u09ae\u09bf \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09ac";
    };
    if (stringKey instanceof Language_Types.I_HAVE_ARRIVED) {
        return "\u0986\u09ae\u09bf \u098f\u09b8\u09c7\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.PLEASE_COME_FAST_I_AM_WAITING) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09a6\u09cd\u09b0\u09c1\u09a4 \u0986\u09b8\u09c1\u09a8, \u0986\u09ae\u09bf \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT_I_WILL_BE_THERE) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8, \u0986\u09ae\u09bf \u09b8\u09c7\u0996\u09be\u09a8\u09c7 \u09a5\u09be\u0995\u09ac";
    };
    if (stringKey instanceof Language_Types.LOOKING_FOR_YOU_AT_PICKUP) {
        return "\u09aa\u09bf\u0995-\u0986\u09aa\u09c7 \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u0996\u09c1\u0981\u099c\u099b\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.SILENT) {
        return "\u09a8\u09c0\u09b0\u09ac";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE) {
        return "\u09af\u09be\u0993\u09af\u09bc\u09be!";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE_PROMPT) {
        return "\u0986\u09aa\u09a8\u09bf \u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8\u09c7 \u0985\u09ab\u09b2\u09be\u0987\u09a8\u09c7 \u09b0\u09af\u09bc\u09c7\u099b\u09c7\u09a8 \x0a \u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u0985\u09a8\u09c1\u09b0\u09cb\u09a7\u0997\u09c1\u09b2\u09bf \u09aa\u09c7\u09a4\u09c7, \u098f\u0996\u09a8\u0987 \u0985\u09a8\u09b2\u09be\u0987\u09a8\u09c7 \u09af\u09be\u09a8!";
    };
    if (stringKey instanceof Language_Types.LIVE_DASHBOARD) {
        return "\u09b2\u09be\u0987\u09ad \u09b8\u09cd\u099f\u09cd\u09af\u09be\u099f\u09be\u09b8 \u09a1\u09cd\u09af\u09be\u09b6\u09ac\u09cb\u09b0\u09cd\u09a1";
    };
    if (stringKey instanceof Language_Types.CLICK_TO_ACCESS_YOUR_ACCOUNT) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u0985\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09c7\u09b8 \u0995\u09b0\u09a4\u09c7 \u098f\u0996\u09be\u09a8\u09c7 \u0995\u09cd\u09b2\u09bf\u0995 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_ALTERNATE_NUMBER) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09a8\u09ae\u09cd\u09ac\u09b0 \u09af\u09c1\u0995\u09cd\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_ALTERNATE_MOBILE_NUMBER) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.EDIT_ALTERNATE_MOBILE_NUMBER) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b8\u09ae\u09cd\u09aa\u09be\u09a6\u09a8\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u098f\u0995\u099f\u09bf \u09ac\u09c8\u09a7 10-\u0985\u0999\u09cd\u0995\u09c7\u09b0 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_MOBILE_NUMBER) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.REMOVE) {
        return "\u0985\u09aa\u09b8\u09be\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.REMOVE_ALTERNATE_NUMBER) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b8\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u09af\u09c7 \u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0\u099f\u09bf \u09b8\u09b0\u09bf\u09af\u09bc\u09c7 \u09ab\u09c7\u09b2\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.YES_REMOVE_IT) {
        return "\u09b9\u09cd\u09af\u09be\u0981, \u098f\u099f\u09bf \u09b8\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.NUMBER_REMOVED_SUCCESSFULLY) {
        return "\u09a8\u09ae\u09cd\u09ac\u09b0 \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u09b8\u09b0\u09be\u09a8\u09cb \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.NUMBER_ADDED_SUCCESSFULLY) {
        return "\u09a8\u09ae\u09cd\u09ac\u09b0 \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u09af\u09c1\u0995\u09cd\u09a4 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.NUMBER_EDITED_SUCCESSFULLY) {
        return "\u09a8\u09ae\u09cd\u09ac\u09b0 \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u0986\u09aa\u09a1\u09c7\u099f \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_MOBILE_OTP_LIMIT_EXCEED) {
        return "\u0993\u099f\u09bf\u09aa\u09bf \u09b8\u09c0\u09ae\u09be \u099b\u09be\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u0997\u09c7\u099b\u09c7, \u0986\u09ac\u09be\u09b0 \u09a8\u09ae\u09cd\u09ac\u09b0 \u098f\u09ac\u0982 \u0993\u099f\u09bf\u09aa\u09bf \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.WRONG_OTP) {
        return "\u09ac\u09c8\u09a7 \u0993\u099f\u09bf\u09aa\u09bf \u09b2\u09bf\u0996\u09c1\u09a8 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 ";
    };
    if (stringKey instanceof Language_Types.ATTEMPTS_LEFT) {
        return " \u099a\u09c7\u09b7\u09cd\u099f\u09be \u09ac\u09be\u0995\u09bf";
    };
    if (stringKey instanceof Language_Types.ATTEMPT_LEFT) {
        return " \u09ac\u09be\u09ae \u099a\u09c7\u09b7\u09cd\u099f\u09be";
    };
    if (stringKey instanceof Language_Types.OTP_LIMIT_EXCEEDED) {
        return "\u0993\u099f\u09bf\u09aa\u09bf \u09b8\u09c0\u09ae\u09be \u099b\u09be\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u0997\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.OTP_LIMIT_EXCEEDED_MESSAGE) {
        return "\u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u0993\u099f\u09bf\u09aa\u09bf \u09b8\u09c0\u09ae\u09be\u09a4\u09c7 \u09aa\u09cc\u0981\u099b\u09c7\u099b\u09c7\u09a8\u0964 10 \u09ae\u09bf\u09a8\u09bf\u099f\u09c7\u09b0 \u09aa\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.TRY_AGAIN_LATER) {
        return "\u09aa\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NUMBER_ALREADY_EXIST_ERROR) {
        return "\u0985\u09a8\u09cd\u09af \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09c1\u0995\u09cd\u09a4 \u09a8\u09ae\u09cd\u09ac\u09b0! \u0985\u09a8\u09cd\u09af \u09a8\u09ae\u09cd\u09ac\u09b0 \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_ALTERNATE_NUMBER_IN_MEANTIME) {
        return "\u098f\u0987 \u09aa\u09cd\u09b0\u0995\u09cd\u09b0\u09bf\u09af\u09bc\u09be\u099f\u09bf 2 \u0995\u09be\u09b0\u09cd\u09af\u09a6\u09bf\u09ac\u09b8\u09c7\u09b0 \u09b8\u09ae\u09af\u09bc \u09a8\u09bf\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7 \x0a \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u09b9\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u0964 \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7, \u0986\u09aa\u09a8\u09bf \u098f\u0995\u099f\u09bf \u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09af\u09c1\u0995\u09cd\u09a4 \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.VERIFICATION_IS_TAKING_A_BIT_LONGER) {
        return "\u09ae\u09a8\u09c7 \u09b9\u099a\u09cd\u099b\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u09af\u09be\u099a\u09be\u0987\u0995\u09b0\u09a3\u09c7 \u0995\u09bf\u099b\u09c1\u099f\u09be \x0a\u09aa\u09cd\u09b0\u09a4\u09cd\u09af\u09be\u09b6\u09bf\u09a4 \u09b8\u09ae\u09af\u09bc\u09c7\u09b0 \u099a\u09c7\u09af\u09bc\u09c7 \u09ac\u09c7\u09b6\u09bf \u09b8\u09ae\u09af\u09bc \u09a8\u09bf\u099a\u09cd\u099b\u09c7\u0964\x0a\u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u0995\u09b0\u09a4\u09c7 \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.COMPLETE_ONBOARDING) {
        return "\u0985\u09a8\u09ac\u09cb\u09b0\u09cd\u09a1\u09bf\u0982 \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3";
    };
    if (stringKey instanceof Language_Types.PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS) {
        return "\u098f\u0987 \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b8\u09b9 \u09ac\u09cd\u09af\u0995\u09cd\u09a4\u09bf \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7 \u09ac\u09bf\u09a6\u09cd\u09af\u09ae\u09be\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.OTP_) {
        return "OTP";
    };
    if (stringKey instanceof Language_Types.MAPS) {
        return "Maps";
    };
    if (stringKey instanceof Language_Types.DELETE) {
        return "\u09ae\u09c1\u099b\u09c7 \u09ab\u09c7\u09b2\u09be";
    };
    if (stringKey instanceof Language_Types.VIEW) {
        return "\u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ISSUE_NO) {
        return "\u0987\u09b8\u09cd\u09af\u09c1 \u09a8\u0982 ";
    };
    if (stringKey instanceof Language_Types.ADD_VOICE_NOTE) {
        return "\u09ad\u09af\u09bc\u09c7\u09b8 \u09a8\u09cb\u099f \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.VOICE_NOTE_ADDED) {
        return "\u09ad\u09af\u09bc\u09c7\u09b8 \u09a8\u09cb\u099f \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.SUBMIT_ISSUE_DETAILS) {
        return "\u0987\u09b8\u09cd\u09af\u09c1 \u09ac\u09bf\u09ac\u09b0\u09a3 \u099c\u09ae\u09be \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.IMAGE_PREVIEW) {
        return "\u099b\u09ac\u09bf\u09b0 \u09aa\u09c2\u09b0\u09cd\u09ac\u09b0\u09c2\u09aa";
    };
    if (stringKey instanceof Language_Types.RIDE_REPORT_ISSUE) {
        return "\u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b0\u09bf\u09aa\u09cb\u09b0\u09cd\u099f \u0995\u09b0\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u098f\u0995\u099f\u09bf \u09b0\u09be\u0987\u09a1 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADDED_IMAGES) {
        return "\u099b\u09ac\u09bf \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.NO_IMAGES_ADDED) {
        return "\u0995\u09cb\u09a8 \u099b\u09ac\u09bf \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.ASK_DETAILS_MESSAGE) {
        return "\u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0986\u09b0\u09cb \u0995\u09bf\u099b\u09c1 \u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4 \u099c\u09be\u09a8\u09be\u09a8\u0964 \u0986\u09aa\u09a8\u09bf \u0986\u09b0\u0993 \u09ad\u09be\u09b2\u09ad\u09be\u09ac\u09c7 \u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4 \u099c\u09be\u09a8\u09be\u09a4\u09c7 \u099b\u09ac\u09bf \u09ac\u09be \u09ad\u09af\u09bc\u09c7\u09b8 \u09a8\u09cb\u099f \u09aa\u09be\u09a0\u09be\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.ASK_DETAILS_MESSAGE_REVERSED) {
        return "\u09b9\u09be\u09b0\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u0993\u09af\u09bc\u09be \u0986\u0987\u099f\u09c7\u09ae \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09c7 \u0986\u09b0\u09cb \u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4 \u09b6\u09c7\u09af\u09bc\u09be\u09b0 \u0995\u09b0\u09c1\u09a8. \u0986\u09aa\u09a8\u09bf \u0986\u09b0\u0993 \u09ad\u09be\u09b2\u09ad\u09be\u09ac\u09c7 \u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4 \u099c\u09be\u09a8\u09be\u09a4\u09c7 \u099b\u09ac\u09bf \u09ac\u09be \u09ad\u09af\u09bc\u09c7\u09b8 \u09a8\u09cb\u099f \u09aa\u09be\u09a0\u09be\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.SELECT_OPTION) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u09a6\u09bf \u098f\u0987\u0997\u09c1\u09b2\u09bf\u09b0 \u0995\u09cb\u09a8\u099f\u09bf\u09b0 \u09ae\u09c1\u0996\u09cb\u09ae\u09c1\u0996\u09bf \u09b9\u09a8 \u09a4\u09ac\u09c7 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09ac\u09b2\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SELECT_OPTION_REVERSED) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf\u09ad\u09be\u09ac\u09c7 \u098f\u0987 \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b8\u09ae\u09be\u09a7\u09be\u09a8 \u0995\u09b0\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.ISSUE_SUBMITTED_MESSAGE) {
        return "\u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4 \u09aa\u09cd\u09b0\u09be\u09aa\u09cd\u09a4! \u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b8\u09ae\u09be\u09a7\u09be\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09a6\u09b2 \u0986\u09aa\u09a8\u09be\u0995\u09c7 24 \u0998\u09a8\u09cd\u099f\u09be\u09b0 \u09ae\u09a7\u09cd\u09af\u09c7 \u0995\u09b2 \u0995\u09b0\u09ac\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.I_DONT_KNOW_WHICH_RIDE) {
        return "\u0986\u09ae\u09bf \u099c\u09be\u09a8\u09bf \u09a8\u09be \u0995\u09cb\u09a8 \u09b0\u09be\u0987\u09a1";
    };
    if (stringKey instanceof Language_Types.REPORT_ISSUE_CHAT_PLACEHOLDER) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09ae\u09b8\u09cd\u09af\u09be\u099f\u09bf \u09ac\u09b0\u09cd\u09a3\u09a8\u09be. \u0986\u09ae\u09b0\u09be 24 \u0998\u09a8\u09cd\u099f\u09be\u09b0 \u09ae\u09a7\u09cd\u09af\u09c7 \u098f\u099f\u09bf \u09b8\u09ae\u09be\u09a7\u09be\u09a8 \u0995\u09b0\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09ac\u0964";
    };
    if (stringKey instanceof Language_Types.PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP) {
        return "\u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0997\u09cd\u09b0\u09be\u09b9\u0995\u0995\u09c7 OTP-\u098f\u09b0 \u099c\u09a8\u09cd\u09af \u099c\u09bf\u099c\u09cd\u099e\u09be\u09b8\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADDED_VOICE_NOTE) {
        return "\u09ad\u09af\u09bc\u09c7\u09b8 \u09a8\u09cb\u099f \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.NO_VOICE_NOTE_ADDED) {
        return "\u0995\u09cb\u09a8 \u09ad\u09af\u09bc\u09c7\u09b8 \u09a8\u09cb\u099f \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.CALL_CUSTOMER_TITLE) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u0995\u09c7 \u0995\u09b2 \u0995\u09b0\u09ac\u09c7\u09a8?";
    };
    if (stringKey instanceof Language_Types.CALL_CUSTOMER_DESCRIPTION) {
        return "\u0986\u09aa\u09a8\u09bf \u0997\u09cd\u09b0\u09be\u09b9\u0995\u0995\u09c7 \u098f\u0995\u099f\u09bf \u0995\u09b2 \u0995\u09b0\u09a4\u09c7 \u099a\u09b2\u09c7\u099b\u09c7\u09a8\u09f7 \u0986\u09aa\u09a8\u09bf \u0995\u09bf \u098f\u0997\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.PLACE_CALL) {
        return "\u0995\u09b2 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_IMAGE) {
        return "\u099b\u09ac\u09bf \u09af\u09cb\u0997 \u0995\u09b0";
    };
    if (stringKey instanceof Language_Types.ADD_ANOTHER) {
        return "\u0986\u09b0\u09c7\u0995\u099f\u09bf \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.IMAGES_ADDED) {
        return "\u099b\u09ac\u09bf \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.ISSUE_SUBMITTED_TEXT) {
        return "\u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0! \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b8\u09ae\u09be\u09a7\u09be\u09a8\u09c7 \u0995\u09be\u099c \u0995\u09b0\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.CHOOSE_AN_OPTION) {
        return "\u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u0993\u09af\u09bc\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u098f\u0995\u099f\u09bf \u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09ac\u09c7\u099b\u09c7 \u09a8\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.IMAGE_ADDED) {
        return "\u0987\u09ae\u09c7\u099c \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.DONE) {
        return "\u09b8\u09ae\u09cd\u09aa\u09a8\u09cd\u09a8";
    };
    if (stringKey instanceof Language_Types.RECORD_VOICE_NOTE) {
        return "\u09ad\u09af\u09bc\u09c7\u09b8 \u09a8\u09cb\u099f \u09b0\u09c7\u0995\u09b0\u09cd\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.HELP_AND_SUPPORT) {
        return "\u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u09b8\u09b9\u09af\u09cb\u0997\u09c0\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.MORE_OPTIONS) {
        return "\u0986\u09b0\u0993 \u09ac\u09bf\u0995\u09b2\u09cd\u09aa";
    };
    if (stringKey instanceof Language_Types.ONGOING_ISSUES) {
        return "\u099a\u09b2\u09ae\u09be\u09a8 \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.RESOLVED_ISSUES) {
        return "\u09b8\u09ae\u09be\u09a7\u09be\u09a8 \u0995\u09b0\u09be \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.RESOLVED_ISSUE) {
        return "\u09b8\u09ae\u09be\u09a7\u09be\u09a8 \u0995\u09b0\u09be \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.ONGOING_ISSUE) {
        return "\u099a\u09b2\u09ae\u09be\u09a8 \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.LOST_ITEM) {
        return "\u09b9\u09be\u09b0\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u0993\u09af\u09bc\u09be \u0986\u0987\u099f\u09c7\u09ae";
    };
    if (stringKey instanceof Language_Types.RIDE_RELATED_ISSUE) {
        return "\u09b0\u09be\u0987\u09a1 \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09bf\u09a4 \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.APP_RELATED_ISSUE) {
        return "\u0985\u09cd\u09af\u09be\u09aa \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09bf\u09a4 \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.FARE_RELATED_ISSUE) {
        return "\u09ad\u09be\u09a1\u09bc\u09be \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09bf\u09a4 \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.MAX_IMAGES) {
        return "\u09b8\u09b0\u09cd\u09ac\u09cb\u099a\u09cd\u099a \u09e9\u099f\u09bf \u099b\u09ac\u09bf \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09be \u09af\u09be\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.ISSUE_NUMBER) {
        return "\u0987\u09b8\u09cd\u09af\u09c1 \u09a8\u0982  ";
    };
    if (stringKey instanceof Language_Types.REMOVE_ISSUE) {
        return "\u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b8\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.CALL_SUPPORT_NUMBER) {
        return "\u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u09b8\u09ae\u09b0\u09cd\u09a5\u09a8";
    };
    if (stringKey instanceof Language_Types.YEARS_AGO) {
        return " \u0985\u09a8\u09c7\u0995 \u09ac\u099b\u09b0 \u0986\u0997\u09c7";
    };
    if (stringKey instanceof Language_Types.MONTHS_AGO) {
        return " \u09ae\u09be\u09b8 \u0995\u09a4\u0995 \u09aa\u09c2\u09b0\u09cd\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.DAYS_AGO) {
        return " \u09a6\u09bf\u09a8 \u0986\u0997\u09c7";
    };
    if (stringKey instanceof Language_Types.HOURS_AGO) {
        return " \u0998\u09a8\u09cd\u099f\u09be \u0986\u0997\u09c7";
    };
    if (stringKey instanceof Language_Types.MIN_AGO) {
        return " \u09ae\u09bf\u09a8\u09bf\u099f \u0986\u0997\u09c7";
    };
    if (stringKey instanceof Language_Types.SEC_AGO) {
        return " \u09b8\u09c7\u0995\u09c7\u09a8\u09cd\u09a1 \u0986\u0997\u09c7";
    };
    if (stringKey instanceof Language_Types.LOADING) {
        return "\u09b2\u09cb\u09a1 \u09b9\u099a\u09cd\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.APP_RELATED) {
        return "\u0985\u09cd\u09af\u09be\u09aa \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.FARE) {
        return "\u09ad\u09be\u09a1\u09bc\u09be \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.RIDE_RELATED) {
        return "\u09b0\u09be\u0987\u09a1 \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.LOST_AND_FOUND) {
        return "\u09b9\u09be\u09b0\u09be\u09a8\u09cb \u098f\u09ac\u0982 \u09aa\u09cd\u09b0\u09be\u09aa\u09cd\u09a4\u09bf";
    };
    if (stringKey instanceof Language_Types.REPORT_LOST_ITEM) {
        return "\u09b9\u09be\u09b0\u09be\u09a8\u09cb \u0986\u0987\u099f\u09c7\u09ae \u09b0\u09bf\u09aa\u09cb\u09b0\u09cd\u099f \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SELECT_YOUR_GENDER) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b2\u09bf\u0999\u09cd\u0997 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8";
    };
    if (stringKey instanceof Language_Types.FEMALE) {
        return "\u09ae\u09b9\u09bf\u09b2\u09be";
    };
    if (stringKey instanceof Language_Types.MALE) {
        return "\u09aa\u09c1\u09b0\u09c1\u09b7";
    };
    if (stringKey instanceof Language_Types.PREFER_NOT_TO_SAY) {
        return "\u09ac\u09b2\u09a4\u09c7 \u09a8\u09be \u09aa\u099b\u09a8\u09cd\u09a6";
    };
    if (stringKey instanceof Language_Types.GENDER) {
        return "\u09b2\u09bf\u0999\u09cd\u0997";
    };
    if (stringKey instanceof Language_Types.SET_NOW) {
        return "\u098f\u0996\u09a8 \u09b8\u09c7\u099f \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.COMPLETE_YOUR_PROFILE_AND_FIND_MORE_RIDES) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09cd\u09b0\u09cb\u09ab\u09be\u0987\u09b2 \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u0986\u09b0\u09cb \u09b0\u09be\u0987\u09a1 \u0996\u09c1\u0981\u099c\u09c1\u09a8!";
    };
    if (stringKey instanceof Language_Types.UPDATE_NOW) {
        return "\u098f\u0996\u09a8 \u09b9\u09be\u09b2\u09a8\u09be\u0997\u09be\u09a6 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CONFIRM) {
        return "\u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GENDER_UPDATED) {
        return "\u09b2\u09bf\u0999\u09cd\u0997 \u0986\u09aa\u09a1\u09c7\u099f \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.REGISTERED_ADDRESS) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09bf\u09a4 \u09a0\u09bf\u0995\u09be\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.REGISTERED_ADDRESS_DESCRIPTION) {
        return "\u09b8\u09cd\u099f\u09cd\u09af\u09be\u09b2\u09bf\u09af\u09bc\u09a8 \u09ac\u09bf\u099c\u09a8\u09c7\u09b8 \u09b8\u09c7\u09a8\u09cd\u099f\u09be\u09b0, \u09a8\u0982 444, 3\u09af\u09bc \u098f\u09ac\u0982 4\u09b0\u09cd\u09a5 \u09a4\u09b2\u09be, 18 \u09a4\u09ae \u09aa\u09cd\u09b0\u09a7\u09be\u09a8, 6 \u09a4\u09ae \u09ac\u09cd\u09b2\u0995, \u0995\u09cb\u09b0\u09be\u09ae\u0999\u09cd\u0997\u09b2\u09be, \u09ac\u09c7\u0999\u09cd\u0997\u09be\u09b2\u09c1\u09b0\u09c1, \u0995\u09b0\u09cd\u09a3\u09be\u099f\u0995- 560095, \u09ad\u09be\u09b0\u09a4";
    };
    if (stringKey instanceof Language_Types.ZONE_CANCEL_TEXT_DROP) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0997\u09cd\u09b0\u09be\u09b9\u0995 \u09b8\u09ae\u09cd\u09ad\u09ac\u09a4 \u09b8\u09ae\u09af\u09bc\u09ae\u09a4\u09cb \u09ae\u09c7\u099f\u09cd\u09b0\u09cb \u09b8\u09cd\u099f\u09c7\u09b6\u09a8\u09c7 \u09aa\u09cc\u0981\u099b\u09be\u09a8\u09cb\u09b0 \u099c\u09a8\u09cd\u09af \u09a4\u09be\u09a1\u09bc\u09be\u09b9\u09c1\u09a1\u09bc\u09cb \u0995\u09b0\u099b\u09c7\u09a8! \x0a \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09ac\u09be\u09a4\u09bf\u09b2 \u09a8\u09be \u0995\u09b0\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u0995\u09b0\u099b\u09bf\u0964";
    };
    if (stringKey instanceof Language_Types.ZONE_CANCEL_TEXT_PICKUP) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0997\u09cd\u09b0\u09be\u09b9\u0995 \u09b8\u09ae\u09cd\u09ad\u09ac\u09a4 \u09a4\u09be\u09a6\u09c7\u09b0 \u0997\u09a8\u09cd\u09a4\u09ac\u09cd\u09af\u09c7 \u09aa\u09cc\u0981\u099b\u09be\u09a8\u09cb\u09b0 \u099c\u09a8\u09cd\u09af \u09a4\u09be\u09a1\u09bc\u09be\u09b9\u09c1\u09a1\u09bc\u09cb \u0995\u09b0\u099b\u09c7\u09a8\u0964 \x0a \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09ac\u09be\u09a4\u09bf\u09b2 \u09a8\u09be \u0995\u09b0\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u0995\u09b0\u099b\u09bf\u0964";
    };
    if (stringKey instanceof Language_Types.RANKINGS) {
        return "\u09b0\u200d\u09cd\u09af\u09be\u0999\u09cd\u0995\u09bf\u0982";
    };
    if (stringKey instanceof Language_Types.GETTING_THE_LEADERBOARD_READY) {
        return "\u09b2\u09bf\u09a1\u09be\u09b0\u09ac\u09cb\u09b0\u09cd\u09a1 \u09a4\u09c8\u09b0\u09bf \u0995\u09b0\u09be \u09b9\u099a\u09cd\u099b\u09c7!";
    };
    if (stringKey instanceof Language_Types.PLEASE_WAIT_WHILE_WE_UPDATE_THE_DETAILS) {
        return "\u0986\u09ae\u09b0\u09be \u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4 \u0986\u09aa\u09a1\u09c7\u099f \u0995\u09b0\u09be \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4 \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LAST_UPDATED) {
        return "\u09b8\u09b0\u09cd\u09ac\u09b6\u09c7\u09b7 \u0986\u09aa\u09a1\u09c7\u099f: ";
    };
    if (stringKey instanceof Language_Types.CONGRATULATIONS_YOU_ARE_RANK) {
        return "\u0985\u09ad\u09bf\u09a8\u09a8\u09cd\u09a6\u09a8! \u0986\u09aa\u09a8\u09be\u09b0 \u09b0\u09cd\u09af\u09be\u0999\u09cd\u0995 \u09b9\u09b2 ";
    };
    if (stringKey instanceof Language_Types.YOU) {
        return " (\u0986\u09aa\u09a8\u09bf)";
    };
    if (stringKey instanceof Language_Types.DAILY) {
        return "\u09a6\u09c8\u09a8\u09bf\u0995";
    };
    if (stringKey instanceof Language_Types.WEEKLY) {
        return "\u09b8\u09be\u09aa\u09cd\u09a4\u09be\u09b9\u09bf\u0995";
    };
    if (stringKey instanceof Language_Types.ACCEPT_RIDES_TO_ENTER_RANKINGS) {
        return "\u09b0\u200c\u09cd\u09af\u09be\u0999\u09cd\u0995\u09bf\u0982 \u098f \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09a4\u09c7 \u09b0\u09be\u0987\u09a1 \u0997\u09cd\u09b0\u09b9\u09a3 \u0995\u09b0\u09c1\u09a8!";
    };
    if (stringKey instanceof Language_Types.OTP_HAS_BEEN_RESENT) {
        return "\u0993\u099f\u09bf\u09aa\u09bf \u0986\u09ac\u09be\u09b0 \u09aa\u09be\u09a0\u09be\u09a8\u09cb \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_RESENDING_OTP) {
        return "OTP \u0987\u09a8\u09aa\u09c1\u099f \u09b8\u09c0\u09ae\u09be \u09aa\u09cc\u0981\u099b\u09c7\u099b\u09c7, \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 OTP \u09aa\u09be\u09a0\u09be\u09a8\u09cb\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OTP_RESENT_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER) {
        return "OTP \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09aa\u09be\u09a0\u09be\u09a8\u09cb\u09b0 \u09b8\u09c0\u09ae\u09be \u09aa\u09cc\u0981\u099b\u09c7 \u0997\u09c7\u099b\u09c7, \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u09aa\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OTP_PAGE_HAS_BEEN_EXPIRED_PLEASE_REQUEST_OTP_AGAIN) {
        return "OTP \u09aa\u09c3\u09b7\u09cd\u09a0\u09be\u09b0 \u09ae\u09c7\u09af\u09bc\u09be\u09a6 \u09b6\u09c7\u09b7 \u09b9\u09af\u09bc\u09c7 \u0997\u09c7\u099b\u09c7, \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 OTP \u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN) {
        return "\u0995\u09bf\u099b\u09c1 \u09aa\u09cd\u09b0\u09af\u09c1\u0995\u09cd\u09a4\u09bf\u0997\u09a4 \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7, \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.INVALID_REFERRAL_CODE) {
        return "\u0985\u09ac\u09c8\u09a7 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.ISSUE_REMOVED_SUCCESSFULLY) {
        return "\u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u09b8\u09ae\u09be\u09a7\u09be\u09a8 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER) {
        return "OTP \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6\u09c7\u09b0 \u09b8\u09ae\u09af\u09bc\u09b8\u09c0\u09ae\u09be \u09aa\u09cc\u0981\u099b\u09c7 \u0997\u09c7\u099b\u09c7, \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u09aa\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER) {
        return "\u0986\u09aa\u09a8\u09bf \u0985\u09a8\u09c7\u0995\u0997\u09c1\u09b2\u09bf \u0985\u09ac\u09c8\u09a7 \u09aa\u09cd\u09b0\u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c7\u099b\u09c7\u09a8, \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u09aa\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8\u09f7";
    };
    if (stringKey instanceof Language_Types.INVALID_REFERRAL_NUMBER) {
        return "\u0985\u09ac\u09c8\u09a7 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.SOMETHING_WENT_WRONG_TRY_AGAIN_LATER) {
        return "\u0995\u09bf\u099b\u09c1 \u09aa\u09cd\u09b0\u09af\u09c1\u0995\u09cd\u09a4\u09bf\u0997\u09a4 \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7, \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u09aa\u09b0\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.WAIT_TIME) {
        return "\u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be\u09b0 \u09b8\u09ae\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.WAIT_TIMER) {
        return "\u099f\u09be\u0987\u09ae\u09be\u09b0 \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.HOW_LONG_WAITED_FOR_PICKUP) {
        return "\u0986\u09aa\u09a8\u09bf \u09aa\u09bf\u0995\u0986\u09aa\u09c7 \u0995\u09a4\u0995\u09cd\u09b7\u09a3 \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c7\u099b\u09c7\u09a8 \u09a4\u09be \u09a6\u09c7\u0996\u09be\u09af\u09bc\u0964";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_WILL_PAY_FOR_EVERY_MINUTE) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995 \u20b91.5\x0a\u09aa\u09cd\u09b0\u09a5\u09ae {} \u09ae\u09bf\u09a8\u09bf\u099f\u09c7\u09b0 \u09aa\u09b0 \u09aa\u09cd\u09b0\u09a4\u09bf \u09ae\u09bf\u09a8\u09bf\u099f \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09ac\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.OTHERS) {
        return "\u0985\u09a8\u09cd\u09af\u09be\u09a8\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.ENTER_SECOND_SIM_NUMBER) {
        return "\u09a6\u09cd\u09ac\u09bf\u09a4\u09c0\u09af\u09bc \u09b8\u09bf\u09ae \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_NUMBER) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_NUMBER_CANNOT_BE_ADDED) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09a8\u09ae\u09cd\u09ac\u09b0 \u09af\u09c1\u0995\u09cd\u09a4 \u0995\u09b0\u09be \u09af\u09be\u09ac\u09c7 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.SELECT_THE_LANGUAGES_YOU_CAN_SPEAK) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09a5\u09be \u09ac\u09b2\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8 \u098f\u09ae\u09a8 \u09ad\u09be\u09b7\u09be \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09b8\u0982\u0996\u09cd\u09af\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09c0\u09ae\u09be \u0985\u09a4\u09bf\u0995\u09cd\u09b0\u09ae \u0995\u09b0\u09c7\u099b\u09c7\u09f7";
    };
    if (stringKey instanceof Language_Types.OTP_RESENT) {
        return "OTP \u0986\u09ac\u09be\u09b0 \u09aa\u09be\u09a0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.OTP_RESEND_LIMIT_EXCEEDED) {
        return "\u0993\u099f\u09bf\u09aa\u09bf \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09aa\u09be\u09a0\u09be\u09a8\u09cb\u09b0 \u09b8\u09c0\u09ae\u09be \u0985\u09a4\u09bf\u0995\u09cd\u09b0\u09ae \u0995\u09b0\u09c7\u099b\u09c7\u09f7";
    };
    if (stringKey instanceof Language_Types.SEDAN) {
        return "\u09b8\u09c7\u09a1\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.SUV) {
        return "SUV";
    };
    if (stringKey instanceof Language_Types.HATCHBACK) {
        return "\u09b9\u09cd\u09af\u09be\u099a\u09ac\u09cd\u09af\u09be\u0995";
    };
    if (stringKey instanceof Language_Types.AUTO_RICKSHAW) {
        return "\u0985\u099f\u09cb \u09b0\u09bf\u0995\u09cd\u09b8\u09be";
    };
    if (stringKey instanceof Language_Types.TAXI) {
        return "\u09a8\u09a8-\u098f\u09b8\u09bf \u099f\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09bf";
    };
    if (stringKey instanceof Language_Types.TAXI_PLUS) {
        return "\u098f\u09b8\u09bf \u099f\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09bf";
    };
    if (stringKey instanceof Language_Types.MY_PROFILE) {
        return "\u0986\u09ae\u09be\u09b0 \u09aa\u09cd\u09b0\u09cb\u09ab\u09be\u0987\u09b2";
    };
    if (stringKey instanceof Language_Types.SETTINGS) {
        return "\u09b8\u09c7\u099f\u09bf\u0982\u09b8";
    };
    if (stringKey instanceof Language_Types.REG_NUMBER) {
        return "\u09b0\u09c7\u099c\u09bf\u09b8\u09cd\u099f\u09cd\u09b0\u09c7\u09b6\u09a8 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.TYPE) {
        return "\u099f\u09be\u0987\u09aa";
    };
    if (stringKey instanceof Language_Types.MODEL_NAME) {
        return "\u09ae\u09a1\u09c7\u09b2\u09c7\u09b0 \u09a8\u09be\u09ae";
    };
    if (stringKey instanceof Language_Types.COLOUR) {
        return "\u09b0\u0999";
    };
    if (stringKey instanceof Language_Types.BADGES) {
        return "\u09ac\u09cd\u09af\u09be\u099c";
    };
    if (stringKey instanceof Language_Types.CALL_CUSTOMER_SUPPORT) {
        return "\u0995\u09be\u09b8\u09cd\u099f\u09ae\u09be\u09b0 \u09b8\u09be\u09aa\u09cb\u09b0\u09cd\u099f\u09c7 \u0995\u09b2 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.EDIT_RC) {
        return "\u09b8\u09ae\u09cd\u09aa\u09be\u09a6\u09a8\u09be \u0995\u09b0\u09c1\u09a8 RC";
    };
    if (stringKey instanceof Language_Types.DELETE_RC) {
        return "\u09ae\u09c1\u099b\u09c7 \u09ab\u09c7\u09b2\u09be RC";
    };
    if (stringKey instanceof Language_Types.DEACTIVATE_RC) {
        return "\u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09c1\u09a8 RC";
    };
    if (stringKey instanceof Language_Types.ACTIVATE_RC) {
        return "\u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09c1\u09a8 RC";
    };
    if (stringKey instanceof Language_Types.ACTIVE_RC_ON_ANOTHER_DRIVER) {
        return " \u0985\u09a8\u09cd\u09af \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f\u09c7 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc!";
    };
    if (stringKey instanceof Language_Types.CALL_DRIVER_OR_CONTACT_SUPPORT) {
        return "RC \u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09a4\u09c7 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u0995\u09c7 \u0995\u09b2 \u0995\u09b0\u09c1\u09a8 \u09ac\u09be \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b8\u09ae\u09cd\u09ad\u09ac \u09a8\u09be \u09b9\u09b2\u09c7 \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CALL_DRIVER) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u0995\u09c7 \u0995\u09b2 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SKIP) {
        return "\u098f\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.ACTIVE_STR) {
        return "\u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.INACTIVE_RC) {
        return "\u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.CONFIRMATION_FOR_DELETING_RC) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u09af\u09c7 \u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 RC \u09ae\u09c1\u099b\u09c7 \u09ab\u09c7\u09b2\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.CONFIRMATION_FOR_DEACTIVATING_RC) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u09af\u09c7 \u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 RC \u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09a4\u09c7 \u099a\u09be\u09a8?- ";
    };
    if (stringKey instanceof Language_Types.CONFIRMATION_FOR_ACTIVATING_RC) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0986\u09aa\u09a8\u09bf RC \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09a4\u09c7 \u099a\u09be\u09a8?- ";
    };
    if (stringKey instanceof Language_Types.YES_DELETE) {
        return "\u09b9\u09cd\u09af\u09be\u0981, \u09ae\u09c1\u099b\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_NEW_RC) {
        return "\u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8 RC";
    };
    if (stringKey instanceof Language_Types.CONNECT_CALL_ANONYMOUSLY) {
        return "\u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09ac\u09c7\u09a8\u09be\u09ae\u09c7 \u09b8\u0982\u09af\u09cb\u0997 \u0995\u09b0\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09ac RC \u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09be\u09b0 \u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u0995\u09b0\u09be\u09b0 \u0986\u0997\u09c7 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09a8\u09bf\u099c\u09c7\u09b0 \u09aa\u09b0\u09bf\u099a\u09af\u09bc \u09a6\u09bf\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.YES_ACTIVATE) {
        return "\u09b9\u09cd\u09af\u09be\u0981, \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.YES_DEACTIVATE) {
        return "\u09b9\u09cd\u09af\u09be\u0981, \u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.THIS_WILL_DEACTIVATE_CURRENTLY_ACTIVE_RC) {
        return "\u098f\u099f\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8\u09c7 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc RC \u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09ac\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.REMOVED) {
        return "\u09b8\u09b0\u09be\u09a8\u09cb \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.IS_ACTIVE_NOW) {
        return " \u098f\u0996\u09a8 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc.";
    };
    if (stringKey instanceof Language_Types.DEACTIVATED) {
        return "\u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.LANGUAGE_UPDATED) {
        return "\u09ad\u09be\u09b7\u09be \u0986\u09aa\u09a1\u09c7\u099f \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.SINGLE_RC_CANNOT_BE_DELETED) {
        return "\u098f\u0995\u0995 \u0986\u09b0\u09b8\u09bf \u09ae\u09c1\u099b\u09c7 \u09ab\u09c7\u09b2\u09be \u09af\u09be\u09ac\u09c7 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.CANCELLATION_RATE) {
        return "\u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09be\u09b0 \u09b9\u09be\u09b0";
    };
    if (stringKey instanceof Language_Types.RIDES_CANCELLED) {
        return "\u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09be \u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u09b8\u0982\u0996\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.EARNINGS_MISSED) {
        return "\u09ae\u09bf\u09b8\u09a1 \u0986\u09b0\u09cd\u09a8\u09bf\u0982\u09b8";
    };
    if (stringKey instanceof Language_Types.SUMMARY) {
        return "\u09b8\u09be\u09b0\u09be\u0982\u09b6";
    };
    if (stringKey instanceof Language_Types.NAMMA_BONUS) {
        return "\u09a8\u09be\u09ae\u09cd\u09ae\u09be \u09ac\u09cb\u09a8\u09be\u09b8";
    };
    if (stringKey instanceof Language_Types.TRIPS_COMPLETED) {
        return "\u09b8\u09ae\u09be\u09aa\u09cd\u09a4 \u09ad\u09cd\u09b0\u09ae\u09a3";
    };
    if (stringKey instanceof Language_Types.LATE_NIGHT_TRIPS) {
        return "\u09b2\u09c7\u099f \u09a8\u09be\u0987\u099f \u099f\u09cd\u09b0\u09bf\u09aa";
    };
    if (stringKey instanceof Language_Types.ABOUT_ME) {
        return "\u0986\u09ae\u09be\u09b0 \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09c7";
    };
    if (stringKey instanceof Language_Types.ABOUT_VEHICLE) {
        return "\u09ac\u09be\u09b9\u09a8 \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09c7";
    };
    if (stringKey instanceof Language_Types.ADD) {
        return "\u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.YEARS_OLD) {
        return "\u09ac\u099b\u09b0 \u09ac\u09af\u09bc\u09b8\u09c0";
    };
    if (stringKey instanceof Language_Types.HOMETOWN) {
        return "\u09b9\u09cb\u09ae \u099f\u09be\u0989\u09a8";
    };
    if (stringKey instanceof Language_Types.MISSED_OPPORTUNITY) {
        return "\u09b8\u09c1\u09af\u09cb\u0997 \u09ae\u09bf\u09b8 \u0995\u09b0\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.HOW_OLD_IS_YOUR_VEHICLE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09ac\u09af\u09bc\u09b8 \u0995\u09a4 (\u09ac\u099b\u09b0\u09c7)?";
    };
    if (stringKey instanceof Language_Types.ENTER_NAME_OF_VEHICLE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09a8\u09be\u09ae \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NEW_) {
        return "\u09a8\u09a4\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TODAY) {
        return "\u0986\u099c";
    };
    if (stringKey instanceof Language_Types.TOTAL_MONEY_COLLECTED) {
        return "\u09ae\u09cb\u099f \u0985\u09b0\u09cd\u09a5 \u09b8\u0982\u0997\u09c3\u09b9\u09c0\u09a4";
    };
    if (stringKey instanceof Language_Types.FARE_EARNED_OF_THE_DAY) {
        return "\u09a6\u09bf\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u0985\u09b0\u09cd\u099c\u09bf\u09a4 \u09ad\u09be\u09a1\u09bc\u09be";
    };
    if (stringKey instanceof Language_Types.GST_PLUS_PAYABLE) {
        return "GST + \u09ab\u09bf \u09aa\u09cd\u09b0\u09a6\u09c7\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.TO_CONTINUE_USING_YATRI_SATHI) {
        return "Yatri Sathi \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0 \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7, \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PAY) {
        return "\u09ac\u09c7\u09a4\u09a8";
    };
    if (stringKey instanceof Language_Types.LATER) {
        return "\u09aa\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.GREAT_JOB) {
        return "\u09a6\u09be\u09b0\u09c2\u09a8 \u0995\u09be\u099c!";
    };
    if (stringKey instanceof Language_Types.FEE_BREAKUP) {
        return "\u09ab\u09bf \u09ac\u09cd\u09b0\u09c7\u0995\u0986\u09aa";
    };
    if (stringKey instanceof Language_Types.YATRI_SATHI_FEE_PAYABLE_FOR_DATE) {
        return "Yatri Sathi \u09ab\u09bf \u099c\u09a8\u09cd\u09af \u09aa\u09cd\u09b0\u09a6\u09c7\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.FEE_CORRESPONDING_TO_THE_DISTANCE) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf \u099f\u09cd\u09b0\u09bf\u09aa\u09c7 \u09ad\u09cd\u09b0\u09ae\u09a3 \u0995\u09b0\u09be \u09a6\u09c2\u09b0\u09a4\u09cd\u09ac\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09bf\u09a4 \u098f\u0995\u099f\u09bf \u09ab\u09bf, \u09a8\u09c0\u099a\u09c7 \u09a6\u09c7\u0996\u09be\u09a8\u09cb \u09b9\u09bf\u09b8\u09be\u09ac\u09c7 \u0997\u09a3\u09a8\u09be \u0995\u09b0\u09be \u09b9\u09ac\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.PLATFORM_FEE) {
        return "\u09aa\u09cd\u09b2\u09cd\u09af\u09be\u099f\u09ab\u09b0\u09cd\u09ae \u09ab\u09bf";
    };
    if (stringKey instanceof Language_Types.GST) {
        return "GST";
    };
    if (stringKey instanceof Language_Types.TOTAL_PAYABLE) {
        return "\u09ae\u09cb\u099f \u09aa\u09cd\u09b0\u09a6\u09c7\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.VIEW_DETAILS) {
        return "\u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PAYMENT_SUCCESSFUL) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09b8\u09ab\u09b2!";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ae\u09c1\u09b2\u09a4\u09c1\u09ac\u09bf";
    };
    if (stringKey instanceof Language_Types.PAYMENT_FAILED) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING_DESC) {
        return "\u099a\u09bf\u09a8\u09cd\u09a4\u09be \u0995\u09b0\u09ac\u09c7\u09a8 \u09a8\u09be, \u0986\u09aa\u09a8\u09bf \u098f\u0996\u09a8\u0993 \u09a6\u09bf\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b0\u09be\u0987\u09a1\u0997\u09c1\u09b2\u09bf \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8!";
    };
    if (stringKey instanceof Language_Types.PAYMENT_FAILED_DESC) {
        return "\u0986\u09aa\u09a8\u09bf \u0986\u09ac\u09be\u09b0 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8, \u0985\u09a5\u09ac\u09be \u0986\u09aa\u09a8\u09be\u09b0 \u09a8\u09bf\u0995\u099f\u09b8\u09cd\u09a5 Yatri Sathi \u09ac\u09c1\u09a5\u09c7 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.WE_WILL_NOTIFY_WHEN_PAYMENT_SUCCESS) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09b8\u09ab\u09b2 \u09b9\u09b2\u09c7 \u0986\u09ae\u09b0\u09be \u0985\u09ac\u09b9\u09bf\u09a4 \u0995\u09b0\u09ac";
    };
    if (stringKey instanceof Language_Types.CONTINUE_TAKING_RIDES) {
        return "\u09b0\u09be\u0987\u09a1 \u0995\u09b0\u09be \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.YOUR_PREVIOUS_PAYMENT_IS_PENDING) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09c2\u09b0\u09cd\u09ac\u09c7\u09b0 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u098f\u0996\u09a8\u0993 \u09ac\u09be\u0995\u09bf \u0986\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.GOVERMENT_CHARGES) {
        return "\u09b8\u09b0\u0995\u09be\u09b0\u09bf \u099a\u09be\u09b0\u09cd\u099c";
    };
    if (stringKey instanceof Language_Types.OKAY) {
        return "\u09a0\u09bf\u0995 \u0986\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.NO_PAYMENT_HISTORY_AVAILABLE) {
        return "\u0995\u09cb\u09a8 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u0987\u09a4\u09bf\u09b9\u09be\u09b8 \u0989\u09aa\u09b2\u09ac\u09cd\u09a7 \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.ENTER_AADHAAR_NUMBER) {
        return "\u0986\u09a7\u09be\u09b0 \u09a8\u09ae\u09cd\u09ac\u09b0/\u0987\u0989\u0986\u0987\u09a1\u09bf \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_AADHAAR_OTP_) {
        return "\u0986\u09a7\u09be\u09b0 \u0993\u099f\u09bf\u09aa\u09bf \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.AADHAAR_LINKING_REQUIRED) {
        return "\u0986\u09a7\u09be\u09b0 \u09b2\u09bf\u0999\u09cd\u0995 \u0995\u09b0\u09be \u0986\u09ac\u09b6\u09cd\u09af\u0995";
    };
    if (stringKey instanceof Language_Types.AADHAAR_LINKING_REQUIRED_DESCRIPTION) {
        return "\u09af\u09be\u09a4\u09cd\u09b0\u09c0 \u09b8\u09be\u09a5\u09c0\u09b0 \u099c\u09a8\u09cd\u09af \u0997\u09be\u09a1\u09bc\u09bf \u099a\u09be\u09b2\u09be\u09a8\u09cb \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09a4\u09c7, \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \x0a \u0986\u09aa\u09a8\u09be\u09b0 \u0986\u09a7\u09be\u09b0 \u0986\u0987\u09a1\u09bf \u09b2\u09bf\u0999\u09cd\u0995 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.BY_CLICKING_THIS_YOU_WILL_BE_AGREEING_TO_OUR_TC) {
        return "\u0985\u09ac\u09bf\u09b0\u09a4 \u0995\u09cd\u09b2\u09bf\u0995 \u0995\u09b0\u09c7, \u0986\u09aa\u09a8\u09bf \u0986\u09ae\u09be\u09a6\u09c7\u09b0 &nbsp; <a href=\"\">T&Cs</a>\u09f7";
    };
    if (stringKey instanceof Language_Types.TERMS_AND_CONDITIONS_SHORT) {
        return "T&C";
    };
    if (stringKey instanceof Language_Types.TC_TAIL) {
        return "";
    };
    if (stringKey instanceof Language_Types.OTP_SENT_TO_AADHAAR_NUMBER) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0986\u09a7\u09be\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09b2\u09bf\u0999\u09cd\u0995 \u0995\u09b0\u09be \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0\u09c7 OTP \u09aa\u09be\u09a0\u09be\u09a8\u09cb \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.ENTER_SIX_DIGIT_OTP) {
        return "\u099b\u09af\u09bc \u09b8\u0982\u0996\u09cd\u09af\u09be\u09b0 \u0993\u099f\u09bf\u09aa\u09bf \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LINK_AADHAAR_ID) {
        return "\u0986\u09a7\u09be\u09b0 \u0986\u0987\u09a1\u09bf \u09b2\u09bf\u0999\u09cd\u0995 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NO_MOBILE_NUMBER_REGISTERED) {
        return "\u0986\u09a7\u09be\u09b0 \u09a8\u09ae\u09cd\u09ac\u09b0\u09c7\u09b0 \u09b8\u09be\u09a5\u09c7 \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09bf\u09a4 \u098f\u0995\u099f\u09bf \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09a8\u09c7\u0987\u0964";
    };
    if (stringKey instanceof Language_Types.EXCEED_OTP_GENERATION_LIMIT) {
        return "\u0993\u099f\u09bf\u09aa\u09bf \u09a4\u09c8\u09b0\u09bf\u09b0 \u09b8\u09b0\u09cd\u09ac\u09cb\u099a\u09cd\u099a \u09b8\u09c0\u09ae\u09be \u099b\u09be\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u0997\u09c7\u099b\u09c7\u0964 \u0995\u09bf\u099b\u09c1 \u09b8\u09ae\u09af\u09bc\u09c7\u09b0 \u09ae\u09a7\u09cd\u09af\u09c7 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8.";
    };
    if (stringKey instanceof Language_Types.AADHAAR_NUMBER_NOT_EXIST) {
        return "\u0986\u09a7\u09be\u09b0 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09a8\u09c7\u0987\u0964";
    };
    if (stringKey instanceof Language_Types.INVALID_OTP) {
        return "\u0985\u09ac\u09c8\u09a7 OTP";
    };
    if (stringKey instanceof Language_Types.NO_SHARE_CODE) {
        return "\u0995\u09cb\u09a8 \u09b6\u09c7\u09af\u09bc\u09be\u09b0 \u0995\u09cb\u09a1 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09be \u09b9\u09af\u09bc \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.WRONG_SHARE_CODE) {
        return "\u09ad\u09c1\u09b2 \u09b6\u09c7\u09af\u09bc\u09be\u09b0 \u0995\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.INVALID_SHARE_CODE) {
        return "\u0985\u09ac\u09c8\u09a7 \u09b6\u09c7\u09af\u09bc\u09be\u09b0 \u0995\u09cb\u09a1\u0964 \u09a6\u09c8\u09b0\u09cd\u0998\u09cd\u09af 4 \u09b9\u0993\u09af\u09bc\u09be \u0989\u099a\u09bf\u09a4 \u098f\u09ac\u0982 \u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u09b8\u0982\u0996\u09cd\u09af\u09be \u09a5\u09be\u0995\u09be \u0989\u099a\u09bf\u09a4\u0964";
    };
    if (stringKey instanceof Language_Types.SESSION_EXPIRED) {
        return "\u09b8\u09ae\u09af\u09bc \u09ae\u09c7\u09af\u09bc\u09be\u09a6 \u09b6\u09c7\u09b7. \u0986\u09ac\u09be\u09b0 \u09aa\u09cd\u09b0\u0995\u09cd\u09b0\u09bf\u09af\u09bc\u09be \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8.";
    };
    if (stringKey instanceof Language_Types.OTP_ATTEMPT_EXCEEDED) {
        return "OTP \u09aa\u09cd\u09b0\u09af\u09bc\u09be\u09b8 \u0985\u09a4\u09bf\u0995\u09cd\u09b0\u09ae \u0995\u09b0\u09c7\u099b\u09c7\u09f7 \u0986\u09ac\u09be\u09b0 \u09aa\u09cd\u09b0\u0995\u09cd\u09b0\u09bf\u09af\u09bc\u09be \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.UPSTREAM_INTERNAL_SERVER_ERROR) {
        return "\u0986\u09aa\u09b8\u09cd\u099f\u09cd\u09b0\u09bf\u09ae \u0989\u09ce\u09b8/\u09b8\u09b0\u0995\u09be\u09b0\u09bf \u0989\u09ce\u09b8 \u0985\u09ad\u09cd\u09af\u09a8\u09cd\u09a4\u09b0\u09c0\u09a3 \u09b8\u09be\u09b0\u09cd\u09ad\u09be\u09b0 \u09a4\u09cd\u09b0\u09c1\u099f\u09bf\u0964 \u0986\u09ac\u09be\u09b0 \u09aa\u09cd\u09b0\u0995\u09cd\u09b0\u09bf\u09af\u09bc\u09be \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8.";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_ALREADY_COMPLETED) {
        return "\u09b2\u09c7\u09a8\u09a6\u09c7\u09a8 \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7 \u09b8\u09ae\u09cd\u09aa\u09a8\u09cd\u09a8 \u09b9\u09af\u09bc\u09c7\u099b\u09c7. \u098f\u0987 \u09b2\u09c7\u09a8\u09a6\u09c7\u09a8\u09c7 \u0986\u09b0 \u0995\u09be\u099c \u0995\u09b0\u09be \u09af\u09be\u09ac\u09c7 \u09a8\u09be.";
    };
    if (stringKey instanceof Language_Types.GOTO_YOUR_NEAREST_BOOTH) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0986\u09a7\u09be\u09b0 \u09af\u09be\u099a\u09be\u0987 \u0995\u09b0\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u09a8\u09bf\u0995\u099f\u09b8\u09cd\u09a5 \u09ac\u09c1\u09a5\u09c7 \u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.AADHAAR_ALREADY_LINKED) {
        return "\u0986\u09a7\u09be\u09b0 \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7\u0987 \u09b2\u09bf\u0999\u09cd\u0995 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.OPTIONAL) {
        return " (\u0990\u099a\u09cd\u099b\u09bf\u0995)";
    };
    if (stringKey instanceof Language_Types.DOWNLOAD_STATEMENT) {
        return "\u09ac\u09bf\u09ac\u09c3\u09a4\u09bf \u09a1\u09be\u0989\u09a8\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SELECT_A_DATE_RANGE) {
        return "\u09ac\u09bf\u09ac\u09c3\u09a4\u09bf\u099f\u09bf \u09a1\u09be\u0989\u09a8\u09b2\u09cb\u09a1 \u0995\u09b0\u09a4\u09c7 \u098f\u0995\u099f\u09bf \u09a4\u09be\u09b0\u09bf\u0996\u09c7\u09b0 \u09b8\u09c0\u09ae\u09be \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.FEE_PAYMENT_HISTORY) {
        return "\u09ab\u09bf \u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u0987\u09a4\u09bf\u09b9\u09be\u09b8";
    };
    if (stringKey instanceof Language_Types.LANGUAGES_SPOKEN) {
        return "\u0995\u09a5\u09bf\u09a4 \u09ad\u09be\u09b7\u09be";
    };
    if (stringKey instanceof Language_Types.VIEW_PAYMENT_HISTORY) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u0987\u09a4\u09bf\u09b9\u09be\u09b8 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RIDE_TYPE) {
        return "\u09b0\u09be\u0987\u09a1 \u099f\u09be\u0987\u09aa";
    };
    if (stringKey instanceof Language_Types.PLACE_CALL_REQUEST) {
        return "\u09b8\u09cd\u09a5\u09be\u09a8 \u0995\u09b2 \u0985\u09a8\u09c1\u09b0\u09cb\u09a7";
    };
    if (stringKey instanceof Language_Types.RC_STATUS) {
        return "\u0986\u09b0\u09b8\u09bf \u09b8\u09cd\u099f\u09cd\u09af\u09be\u099f\u09be\u09b8";
    };
    if (stringKey instanceof Language_Types.RATED_BY_USERS1) {
        return "";
    };
    if (stringKey instanceof Language_Types.RATED_BY_USERS2) {
        return "\u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0\u0995\u09be\u09b0\u09c0 \u09a6\u09cd\u09ac\u09be\u09b0\u09be \u09b0\u09c7\u099f";
    };
    if (stringKey instanceof Language_Types.MONTHS) {
        return "\u09ae\u09be\u09b8";
    };
    if (stringKey instanceof Language_Types.ENTER_AADHAAR_DETAILS) {
        return "\u0986\u09a7\u09be\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CALL_REQUEST_HAS_BEEN_PLACED) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0995\u09b2\u09c7\u09b0 \u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.RC_ADDED_SUCCESSFULLY) {
        return "\u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09a8 \u09b6\u0982\u09b8\u09be\u09aa\u09a4\u09cd\u09b0 \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.OFFER_APPLIED) {
        return "\u0985\u09ab\u09be\u09b0 \u09aa\u09cd\u09b0\u09af\u09cb\u099c\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.YOUR_EARNINGS) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0989\u09aa\u09be\u09b0\u09cd\u099c\u09a8";
    };
    if (stringKey instanceof Language_Types.NUMBER_OF_RIDES) {
        return "\u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u09b8\u0982\u0996\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.FARE_BREAKUP) {
        return "\u09ab\u09c7\u09af\u09bc\u09be\u09b0 \u09ac\u09cd\u09b0\u09c7\u0995\u0986\u09aa";
    };
    if (stringKey instanceof Language_Types.MY_PLAN) {
        return "\u0986\u09ae\u09be\u09b0 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.YOUR_DUES) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09be\u0993\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.YOUR_DUES_DESCRIPTION) {
        return "\u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09aa\u09b0\u09bf\u09b6\u09cb\u09a7 \u0995\u09b0\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u098f\u0995\u099f\u09bf \u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f \u0986\u09aa \u0995\u09b0\u09c7\u099b\u09c7\u09a8\u0964 \u0986\u09ae\u09b0\u09be \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc\u09ad\u09be\u09ac\u09c7 \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09ac \u09af\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09b8\u09ac\u09b8\u09ae\u09af\u09bc \u09b8\u09ae\u09af\u09bc\u09ae\u09a4\u09cb \u09aa\u09b0\u09bf\u09b6\u09cb\u09a7 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u0964";
    };
    if (stringKey instanceof Language_Types.CURRENT_DUES) {
        return "\u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8 \u09aa\u09be\u0993\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.YOUR_LIMIT) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09c0\u09ae\u09be";
    };
    if (stringKey instanceof Language_Types.DUE_DETAILS) {
        return "\u09aa\u09cd\u09b0\u09be\u09aa\u09cd\u09af \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.TRIP_DATE) {
        return "\u09ad\u09cd\u09b0\u09ae\u09a3\u09c7\u09b0 \u09a4\u09be\u09b0\u09bf\u0996";
    };
    if (stringKey instanceof Language_Types.AMOUNT) {
        return "\u09aa\u09b0\u09bf\u09ae\u09be\u09a3";
    };
    if (stringKey instanceof Language_Types.VIEW_DUE_DETAILS) {
        return "\u09a8\u09bf\u09b0\u09cd\u09a6\u09bf\u09b7\u09cd\u099f \u09ac\u09bf\u09ac\u09b0\u09a3 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CURRENT_PLAN) {
        return "\u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.ALTERNATE_PLAN) {
        return "\u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_DETAILS) {
        return "\u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.CANCEL_AUTOPAY_STR) {
        return "\u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.WE_MIGHT_BE_LOST) {
        return "\u0993\u09b9! \u0986\u09ae\u09b0\u09be \u09b9\u09be\u09b0\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7 \u09aa\u09be\u09b0\u09bf";
    };
    if (stringKey instanceof Language_Types.EXEPERIENCING_ERROR) {
        return "\u09a4\u09cd\u09b0\u09c1\u099f\u09bf\u09b0 \u09b8\u09ae\u09cd\u09ae\u09c1\u0996\u09c0\u09a8";
    };
    if (stringKey instanceof Language_Types.ENJOY_THESE_BENEFITS) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09af\u09cb\u0997\u09a6\u09be\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09c1\u09ac\u09bf\u09a7\u09be";
    };
    if (stringKey instanceof Language_Types.CHOOSE_YOUR_PLAN) {
        return "\u098f\u0996\u09a8 \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8 \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8!";
    };
    if (stringKey instanceof Language_Types.SKIP_FOR_NOW) {
        return "\u098f\u0996\u09a8 \u099c\u09a8\u09cd\u09af \u09b2\u09be\u09ab\u09be\u09b2\u09be\u09ab\u09bf";
    };
    if (stringKey instanceof Language_Types.SEVEN_DAY_FREE_TRIAL_ACTIVATED) {
        return "7-\u09a6\u09bf\u09a8\u09c7\u09b0 \u09ac\u09bf\u09a8\u09be\u09ae\u09c2\u09b2\u09cd\u09af\u09c7 \u099f\u09cd\u09b0\u09be\u09af\u09bc\u09be\u09b2 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc!";
    };
    if (stringKey instanceof Language_Types.TAKE_UNLIMITED_RIDES_FOR_THE_NEXT_SEVEN_DAYS) {
        return "\u09aa\u09cd\u09b0\u09a5\u09ae 7 \u09a6\u09bf\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09c0\u09ae\u09be\u09b9\u09c0\u09a8 \u09b0\u09be\u0987\u09a1 \u09a8\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.EVERY_RIDE_AT_ZERO_COMMISSION) {
        return "\u099c\u09bf\u09b0\u09cb \u0995\u09ae\u09bf\u09b6\u09a8\u09c7 \u09aa\u09cd\u09b0\u09a4\u09bf\u099f\u09bf \u09b0\u09be\u0987\u09a1!";
    };
    if (stringKey instanceof Language_Types.EARN_UPTO_PER_DAY) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf\u09a6\u09bf\u09a8 \u20b9{} \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4 \u0989\u09aa\u09be\u09b0\u09cd\u099c\u09a8 \u0995\u09b0\u09c1\u09a8!";
    };
    if (stringKey instanceof Language_Types.HOW_THIS_WORKS) {
        return "\u0995\u09bf\u09ad\u09be\u09ac\u09c7 \u098f\u0987 \u0995\u09be\u099c \u0995\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.SIGN_UP_FOR_AUTOPAY_BY_PAYING_JUST) {
        return "\u09ae\u09be\u09a4\u09cd\u09b0 \u20b91 \u09a6\u09bf\u09af\u09bc\u09c7 \u0985\u099f\u09cb\u09aa\u09c7-\u098f\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09be\u0987\u09a8 \u0986\u09aa \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GET_REMINDED_ABOUT_YOUR_PLAN_SETUP) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8 \u09b8\u09c7\u099f\u0986\u09aa \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09c7 \u09ae\u09a8\u09c7 \u0995\u09b0\u09bf\u09af\u09bc\u09c7 \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.FREE_TRIAL_REMINDER) {
        return "\u09a6\u09bf\u09a8 5: \u09ac\u09bf\u09a8\u09be\u09ae\u09c2\u09b2\u09cd\u09af\u09c7 \u099f\u09cd\u09b0\u09be\u09af\u09bc\u09be\u09b2 \u0985\u09a8\u09c1\u09b8\u09cd\u09ae\u09be\u09b0\u0995";
    };
    if (stringKey instanceof Language_Types.PLAN_STARTS) {
        return "\u09a6\u09bf\u09a8 7: \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be \u09b6\u09c1\u09b0\u09c1";
    };
    if (stringKey instanceof Language_Types.EASY_AUTOMATIC_PAYMENTS_START) {
        return "\u09b8\u09b9\u099c, \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09b6\u09c1\u09b0\u09c1 \u09b9\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.FREE_UNTIL) {
        return " \u09ac\u09bf\u09a8\u09be\u09ae\u09c2\u09b2\u09cd\u09af\u09c7 \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.PER_RIDE) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf \u09af\u09be\u09a4\u09cd\u09b0\u09be\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.PER_DAY) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf\u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.OFFER) {
        return "\u0985\u09ab\u09be\u09b0";
    };
    if (stringKey instanceof Language_Types.OFFERS) {
        return "\u0985\u09ab\u09be\u09b0";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ON_THE_FREE_TRIAL) {
        return "\u0986\u09aa\u09a8\u09bf \u09ac\u09bf\u09a8\u09be\u09ae\u09c2\u09b2\u09cd\u09af\u09c7 \u099f\u09cd\u09b0\u09be\u09af\u09bc\u09be\u09b2 \u0986\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_BEFORE_THE_TRAIL_PERIOD_EXPIRES) {
        return "\u099f\u09cd\u09b0\u09be\u09af\u09bc\u09be\u09b2\u09c7\u09b0 \u09ae\u09c7\u09af\u09bc\u09be\u09a6 \u09b6\u09c7\u09b7 \u09b9\u0993\u09af\u09bc\u09be\u09b0 \u0986\u0997\u09c7 \u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GET_FREE_TRAIL_UNTIL) {
        return "\u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4 \u09ac\u09bf\u09a8\u09be\u09ae\u09c2\u09b2\u09cd\u09af\u09c7 \u099f\u09cd\u09b0\u09be\u09af\u09bc\u09be\u09b2 \u09aa\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.CLEAR_DUES) {
        return "\u0995\u09cd\u09b2\u09bf\u09af\u09bc\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING_ALERT) {
        return "\u26a0\ufe0f \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ac\u09be\u0995\u09bf \u0986\u099b\u09c7! \u26a0\ufe0f";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING_ALERT_DESC) {
        return "\u09a8\u09ae\u09cd\u09ae\u09be \u09af\u09be\u09a4\u09cd\u09b0\u09c0\u09a4\u09c7 \u09b0\u09be\u0987\u09a1 \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7, \u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09aa\u09b0\u09bf\u09b6\u09cb\u09a7 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LOW_ACCOUNT_BALANCE) {
        return "\u0995\u09ae \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u09ac\u09cd\u09af\u09be\u09b2\u09c7\u09a8\u09cd\u09b8";
    };
    if (stringKey instanceof Language_Types.LOW_ACCOUNT_BALANCE_DESC) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09cd\u09af\u09be\u0999\u09cd\u0995 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u09ac\u09cd\u09af\u09be\u09b2\u09c7\u09a8\u09cd\u09b8 \u0995\u09ae\u0964 \u09a8\u09bf\u09b0\u09ac\u099a\u09cd\u099b\u09bf\u09a8\u09cd\u09a8 \u09b0\u09be\u0987\u09a1 \u0989\u09aa\u09ad\u09cb\u0997 \u0995\u09b0\u09a4\u09c7 \u099f\u09be\u0995\u09be \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.OKAY_GOT_IT) {
        return "\u09a0\u09bf\u0995 \u0986\u099b\u09c7, \u09ac\u09c1\u099d\u09c7\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.LIMITED_TIME_OFFER) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09c0\u09ae\u09bf\u09a4 \u09b8\u09ae\u09af\u09bc\u09c7\u09b0 \u0985\u09ab\u09be\u09b0!";
    };
    if (stringKey instanceof Language_Types.JOIN_NOW) {
        return "\u098f\u0996\u09a8\u0987 \u09af\u09cb\u0997 \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.AUTOMATIC_PAYMENTS_WILL_APPEAR_HERE) {
        return "\u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u098f\u0996\u09be\u09a8\u09c7 \u09aa\u09cd\u09b0\u09a6\u09b0\u09cd\u09b6\u09bf\u09a4 \u09b9\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.MANUAL_PAYMENTS) {
        return "\u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f";
    };
    if (stringKey instanceof Language_Types.MANUAL_PAYMENTS_WILL_APPEAR_HERE) {
        return "\u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u098f\u0996\u09be\u09a8\u09c7 \u09aa\u09cd\u09b0\u09a6\u09b0\u09cd\u09b6\u09bf\u09a4 \u09b9\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.NO_AUTOMATIC_PAYMENTS_DESC) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u099f\u09cb\u09aa\u09c7\u0987 \u0987\u09a4\u09bf\u09b9\u09be\u09b8 \u098f\u0996\u09be\u09a8\u09c7 \u09aa\u09cd\u09b0\u09a6\u09b0\u09cd\u09b6\u09bf\u09a4 \u09b9\u09ac\u09c7 \u098f\u0995\u09ac\u09be\u09b0 \u0986\u09aa\u09a8\u09bf \u098f\u0995\u0987 \u099c\u09a8\u09cd\u09af \u099a\u09be\u09b0\u09cd\u099c \u0995\u09b0\u09be \u09b9\u09b2\u09c7";
    };
    if (stringKey instanceof Language_Types.NO_MANUAL_PAYMENTS_DESC) {
        return "\u09ac\u0995\u09c7\u09af\u09bc\u09be \u09aa\u09b0\u09bf\u09b6\u09cb\u09a7\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u0987\u09a4\u09bf\u09b9\u09be\u09b8 \u098f\u0996\u09be\u09a8\u09c7 \u09aa\u09cd\u09b0\u09a6\u09b0\u09cd\u09b6\u09bf\u09a4 \u09b9\u09ac\u09c7, \u09af\u09a6\u09bf \u09a5\u09be\u0995\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.PAYMENT_HISTORY) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u0987\u09a4\u09bf\u09b9\u09be\u09b8";
    };
    if (stringKey instanceof Language_Types.PLANS) {
        return "\u09af\u09cb\u0997 \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.PLAN) {
        return "\u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.DAY) {
        return "\u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.TAP_A_PLAN_TO_VIEW_DETAILS) {
        return "\u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4 \u09a6\u09c7\u0996\u09a4\u09c7 \u098f\u0995\u099f\u09bf \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be \u0986\u09b2\u09a4\u09cb \u099a\u09be\u09aa\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.HOW_IT_WORKS) {
        return "\u0995\u09bf\u09ad\u09be\u09ac\u09c7 \u098f\u099f\u09be \u0995\u09be\u099c \u0995\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.ZERO_COMMISION) {
        return "\u099c\u09bf\u09b0\u09cb \u0995\u09ae\u09bf\u09b6\u09a8";
    };
    if (stringKey instanceof Language_Types.EARN_TODAY_PAY_TOMORROW) {
        return "\u0986\u099c\u0987 \u0989\u09aa\u09be\u09b0\u09cd\u099c\u09a8 \u0995\u09b0\u09c1\u09a8, \u0986\u0997\u09be\u09ae\u09c0\u0995\u09be\u09b2 \u0985\u09b0\u09cd\u09a5 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PAY_ONLY_IF_YOU_TAKE_RIDES) {
        return "\u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u0986\u09aa\u09a8\u09bf \u09af\u09a6\u09bf \u09b0\u09be\u0987\u09a1 \u0995\u09b0\u09c7\u09a8 \u09a4\u09ac\u09c7\u0987 \u0985\u09b0\u09cd\u09a5 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.MANAGE_PLAN) {
        return "\u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be \u09aa\u09b0\u09bf\u099a\u09be\u09b2\u09a8\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.VIEW_AUTOPAY_DETAILS) {
        return "\u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SWITCH_AND_SAVE) {
        return "\u09b8\u09c1\u0987\u099a \u098f\u09ac\u0982 \u09b8\u09c7\u09ad \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SWITCH_AND_SAVE_DESC) {
        return "\u0986\u09aa\u09a8\u09bf \u0986\u099c 7\u099f\u09bf\u09b0 \u09ac\u09c7\u09b6\u09bf \u09b0\u09be\u0987\u09a1 \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u0995\u09b0\u09c7\u099b\u09c7\u09a8\u0964 \u09a6\u09c8\u09a8\u09bf\u0995 \u0986\u09a8\u09b2\u09bf\u09ae\u09bf\u099f\u09c7\u09a1 \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8\u09c7 \u09b8\u09cd\u09af\u09c1\u0987\u099a \u0995\u09b0\u09c7 \u20b910 \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4 \u09b8\u099e\u09cd\u099a\u09af\u09bc \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SWITCH_NOW) {
        return "\u098f\u0996\u09a8\u0987 \u09aa\u09be\u09b2\u09cd\u099f\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.PAYMENT_MODE_CHANGED_TO_MANUAL) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ae\u09cb\u09a1 \u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u098f \u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09bf\u09a4 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.PAYMENT_MODE_CHANGED_TO_MANUAL_DESC) {
        return "\u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 UPI \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09ac\u09bf\u09b0\u09be\u09ae \u09a6\u09bf\u09af\u09bc\u09c7\u099b\u09c7\u09a8\u0964 \u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2\u09bf \u09b8\u09be\u09ab \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_PAYMENTS) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_ON) {
        return "\u09b2\u09c7\u09a8\u09a6\u09c7\u09a8 \u099a\u09be\u09b2\u09c1";
    };
    if (stringKey instanceof Language_Types.SUCCESS) {
        return "\u09b8\u09ab\u09b2";
    };
    if (stringKey instanceof Language_Types.DEBITED_ON) {
        return "\u09aa\u09c7\u0987\u09a1 \u0985\u09a8";
    };
    if (stringKey instanceof Language_Types.RIDES_TAKEN_ON) {
        return "\u09b0\u09be\u0987\u09a1 \u09a8\u09c7\u0993\u09af\u09bc\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.JOIN_PLAN) {
        return "\u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8\u09c7 \u09af\u09cb\u0997\u09a6\u09be\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.JOIN_NAMMAA_YATRI) {
        return "\u09a8\u09ae\u09cd\u09ae\u09be \u09af\u09be\u09a4\u09cd\u09b0\u09c0\u09a4\u09c7 \u09af\u09cb\u0997 \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.CANCEL_AUTOPAY_AND_PAY_MANUALLY) {
        return "\u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2\u09bf \u0985\u09b0\u09cd\u09a5 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PLAN_ACTIVATED_SUCCESSFULLY) {
        return "\u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8 \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.DUES_CLEARED_SUCCESSFULLY) {
        return "\u09ac\u0995\u09c7\u09af\u09bc\u09be \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u09b8\u09be\u09ab \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.NOT_PLANNING_TO_TAKE_RIDES) {
        return "\u09b0\u09be\u0987\u09a1 \u0995\u09b0\u09be\u09b0 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be \u0995\u09b0\u099b\u09c7\u09a8 \u09a8\u09be?";
    };
    if (stringKey instanceof Language_Types.RETRY_PAYMENT_STR) {
        return "\u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PAUSE_AUTOPAY_STR) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09ac\u09bf\u09b0\u09be\u09ae \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_STR) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u0986\u09ac\u09be\u09b0 \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.VIEW_RIDE_DETAILS) {
        return "\u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ACCOUNT) {
        return "\u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_IS_NOT_ENABLED_YET) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u098f\u0996\u09a8\u09cb \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09a8\u09bf!";
    };
    if (stringKey instanceof Language_Types.ENABLE_AUTOPAY_DESC) {
        return "\u099d\u09be\u09ae\u09c7\u09b2\u09be-\u09ae\u09c1\u0995\u09cd\u09a4 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09a4\u09c7 \u098f\u0996\u09a8\u0987 \u09b8\u09cd\u09ac\u09a4\u0983\u09aa\u09c7 \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09c1\u09a8!";
    };
    if (stringKey instanceof Language_Types.ENABLE_AUTOPAY_NOW) {
        return "\u098f\u0996\u09a8\u0987 \u09b8\u09cd\u09ac\u09a4\u0983\u09aa\u09c7 \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_PENDING_STR) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u09ae\u09c1\u09b2\u09a4\u09c1\u09ac\u09bf \u0986\u099b\u09c7!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_PENDING_DESC_STR) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09aa\u09cd\u09b0\u0995\u09cd\u09b0\u09bf\u09af\u09bc\u09be \u0995\u09b0\u09be \u09b9\u099a\u09cd\u099b\u09c7\u0964 \u098f\u0995\u099f\u09bf \u0986\u09aa\u09a1\u09c7\u099f \u09b9\u09b2\u09c7 \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u099c\u09be\u09a8\u09be\u09ac\u0964\u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c1\u09a8 \u09ac\u09be \u0986\u09ac\u09be\u09b0 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u0995\u09b0\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8\u0964 \u0985\u09a4\u09bf\u09b0\u09bf\u0995\u09cd\u09a4 \u0985\u09b0\u09cd\u09a5 \u09ab\u09c7\u09b0\u09a4 \u09a6\u09c7\u0993\u09af\u09bc\u09be \u09b9\u09ac\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.REFRESH_STR) {
        return "\u09b0\u09bf\u09ab\u09cd\u09b0\u09c7\u09b6 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_DETAILS) {
        return "\u09b2\u09c7\u09a8\u09a6\u09c7\u09a8\u09c7\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.RIDE_DETAILS) {
        return "\u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.MY_PLAN_TITLE) {
        return "\u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09af\u09be\u09a4\u09cd\u09b0\u09c0 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.SWITCH_TO) {
        return "\u098f\u09a4\u09c7 \u09aa\u09be\u09b2\u09cd\u099f\u09be\u09a8 {}";
    };
    if (stringKey instanceof Language_Types.PLEASE_TRY_AGAIN) {
        return "\u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9\u09aa\u09c2\u09b0\u09cd\u09ac\u0995 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PLAN_NOT_FOUND) {
        return "\u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8 \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.MANDATE_NOT_FOUND) {
        return "\u09ae\u09cd\u09af\u09be\u09a8\u09cd\u09a1\u09c7\u099f \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.ACTIVE_MANDATE_EXISTS) {
        return "\u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0986\u09a6\u09c7\u09b6 \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7\u0987 \u09ac\u09bf\u09a6\u09cd\u09af\u09ae\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.NO_ACTIVE_MANDATE_EXIST) {
        return "\u0995\u09cb\u09a8 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0986\u09a6\u09c7\u09b6 \u09ac\u09bf\u09a6\u09cd\u09af\u09ae\u09be\u09a8 \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.NO_PLAN_FOR_DRIVER) {
        return "\u0995\u09cb\u09a8 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.INVALID_PAYMENT_MODE) {
        return "\u0985\u09ac\u09c8\u09a7 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ae\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.INVALID_AUTO_PAY_STATUS) {
        return "\u0985\u09ac\u09c8\u09a7 \u09b8\u09cd\u09ac\u09a4\u0983\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09b8\u09cd\u09a5\u09bf\u09a4\u09bf";
    };
    if (stringKey instanceof Language_Types.MAX_AMOUNT) {
        return "\u09b8\u09b0\u09cd\u09ac\u09cb\u099a\u09cd\u099a \u09aa\u09b0\u09bf\u09ae\u09be\u09a3";
    };
    if (stringKey instanceof Language_Types.FREQUENCY) {
        return "\u09ab\u09cd\u09b0\u09bf\u0995\u09cb\u09af\u09bc\u09c7\u09a8\u09cd\u09b8\u09bf";
    };
    if (stringKey instanceof Language_Types.STATRED_ON) {
        return "\u09b6\u09c1\u09b0\u09c1 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.EXPIRES_ON) {
        return "\u09ae\u09c7\u09af\u09bc\u09be\u09a6 \u09b6\u09c7\u09b7 \u09b9\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.SWITCHED_PLAN) {
        return "\u09b8\u09c1\u0987\u099a\u09a1 \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.PAYMENT_CANCELLED) {
        return "\u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 UPI \u0985\u099f\u09cb\u09aa\u09c7 \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09c7\u099b\u09c7\u09a8\u0964 \u0986\u09aa\u09a8\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2\u09bf \u09b8\u09be\u09ab \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.UPI_AUTOPAY_S) {
        return "UPI \u0985\u099f\u09cb\u09aa\u09c7";
    };
    if (stringKey instanceof Language_Types.MANUAL_PAYMENT_STR) {
        return "\u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f";
    };
    if (stringKey instanceof Language_Types.DAILY_UNLIMITED) {
        return "\u09a6\u09c8\u09a8\u09bf\u0995 \u09b8\u09c0\u09ae\u09be\u09b9\u09c0\u09a8";
    };
    if (stringKey instanceof Language_Types.DAILY_PER_RIDE) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf \u09af\u09be\u09a4\u09cd\u09b0\u09be\u09af\u09bc \u09aa\u09cd\u09b0\u09a4\u09bf\u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.DAILY_UNLIMITED_PLAN_DESC) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf\u09a6\u09bf\u09a8 \u09b8\u09c0\u09ae\u09be\u09b9\u09c0\u09a8 \u09b0\u09be\u0987\u09a1 \u0989\u09aa\u09ad\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DAILY_PER_RIDE_PLAN_DESC) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf\u09a6\u09bf\u09a8 \u09b8\u09b0\u09cd\u09ac\u09cb\u099a\u09cd\u099a \u20b935 \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.PAY_TO_JOIN_THIS_PLAN) {
        return "\u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8\u09c7 \u09af\u09cb\u0997 \u09a6\u09bf\u09a4\u09c7 \u20b91 \u09aa\u09c7 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OFFERS_NOT_APPLICABLE) {
        return "\u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u09a8\u09be \u09b9\u09b2\u09c7 \u0985\u09ab\u09be\u09b0 \u09aa\u09cd\u09b0\u09af\u09cb\u099c\u09cd\u09af \u09a8\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.PAUSED_STR) {
        return "\u09ac\u09bf\u09b0\u09a4\u09bf \u09a6\u09c7\u0993\u09af\u09bc\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.PENDING_STR) {
        return "\u09ac\u09bf\u099a\u09be\u09b0\u09be\u09a7\u09c0\u09a8";
    };
    if (stringKey instanceof Language_Types.PLAN_SWITCHED_TO) {
        return "\u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be \u09b8\u09cd\u09af\u09c1\u0987\u099a \u0995\u09b0\u09be";
    };
    if (stringKey instanceof Language_Types.SWITCH_PLAN_STR) {
        return "\u09b8\u09cd\u09af\u09c1\u0987\u099a \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.OFFERS_APPLICABLE_ON_DAILY_UNLIMITED) {
        return "\u09a6\u09cd\u09b0\u09b7\u09cd\u099f\u09ac\u09cd\u09af: \u0985\u09ab\u09be\u09b0\u0997\u09c1\u09b2\u09bf \u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u09a1\u09c7\u0987\u09b2\u09bf \u0986\u09a8\u09b2\u09bf\u09ae\u09bf\u099f\u09c7\u09a1 \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8\u09c7 \u09aa\u09cd\u09b0\u09af\u09cb\u099c\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.DAILY_UNLIMITED_OFFER_NOT_AVAILABLE) {
        return "\u09a6\u09cd\u09b0\u09b7\u09cd\u099f\u09ac\u09cd\u09af: \u09aa\u09cd\u09b0\u09a4\u09bf\u09a6\u09bf\u09a8\u09c7\u09b0 \u09b8\u09c0\u09ae\u09be\u09b9\u09c0\u09a8 \u0985\u09ab\u09be\u09b0\u0997\u09c1\u09b2\u09bf \u098f\u0987 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be\u09af\u09bc \u09aa\u09cd\u09b0\u09af\u09cb\u099c\u09cd\u09af \u09a8\u09af\u09bc!";
    };
    if (stringKey instanceof Language_Types.NO_RIDES_NO_CHARGE) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u09a6\u09bf \u09af\u09be\u09a4\u09cd\u09b0\u09be \u0995\u09b0\u09c7\u09a8 \u09a4\u09ac\u09c7\u0987 \u0985\u09b0\u09cd\u09a5 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GET_SPECIAL_OFFERS) {
        return "\u09ac\u09bf\u09b6\u09c7\u09b7 \u0985\u09ab\u09be\u09b0 \u09aa\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.VALID_ONLY_IF_PAYMENT) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u0995\u09b0\u09be\u09b0 \u09aa\u09b0\u09cd\u09af\u09be\u09af\u09bc\u0995\u09cd\u09b0\u09ae\u09c7 \u09ae\u09be\u09a8\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.HELP_STR) {
        return "\u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af";
    };
    if (stringKey instanceof Language_Types.REFRESH_STRING) {
        return "\u09b0\u09bf\u09ab\u09cd\u09b0\u09c7\u09b6 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CHAT_FOR_HELP) {
        return "\u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u099a\u09cd\u09af\u09be\u099f";
    };
    if (stringKey instanceof Language_Types.VIEW_FAQs) {
        return "\u09aa\u09cd\u09b0\u09b6\u09cd\u09a8\u0997\u09c1\u09b2\u09bf \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.FIND_HELP_CENTRE) {
        return "\u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u0995\u09c7\u09a8\u09cd\u09a6\u09cd\u09b0 \u0996\u09c1\u0981\u099c\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CONTACT) {
        return "\u09af\u09cb\u0997\u09be\u09af\u09cb\u0997";
    };
    if (stringKey instanceof Language_Types.GO_TO_LOCATION) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.NO_HELP_CENTER_IS_ACTIVE_NOW) {
        return "\u098f\u0987 \u09ae\u09c1\u09b9\u09c2\u09b0\u09cd\u09a4\u09c7 \u0995\u09cb\u09a8\u09cb \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u0995\u09c7\u09a8\u09cd\u09a6\u09cd\u09b0 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.HELP_CENTERS_LOCATION_WILL_APPEAR_HERE_ONCE_THEY_ARE_ACTIVE) {
        return "\u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u0995\u09c7\u09a8\u09cd\u09a6\u09cd\u09b0\u0997\u09c1\u09b2\u09bf\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u0997\u09c1\u09b2\u09bf \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u09b9\u09af\u09bc\u09c7 \u0997\u09c7\u09b2\u09c7 \u098f\u0996\u09be\u09a8\u09c7 \u0989\u09aa\u09b8\u09cd\u09a5\u09bf\u09a4 \u09b9\u09ac\u09c7\u09f7";
    };
    if (stringKey instanceof Language_Types.SUPPORT) {
        return "\u09b8\u09ae\u09b0\u09cd\u09a5\u09a8";
    };
    if (stringKey instanceof Language_Types.NEED_HELP_JOINING_THE_PLAN) {
        return "\u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be \u09ac\u09be \u0985\u099f\u09cb\u09aa\u09c7\u09a4\u09c7 \u09af\u09cb\u0997\u09a6\u09be\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u09a6\u09b0\u0995\u09be\u09b0?";
    };
    if (stringKey instanceof Language_Types.GO_TO_VEHICLE_DETAILS) {
        return "\u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8\u09c7\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.CLOSE) {
        return "\u09ac\u09a8\u09cd\u09a7";
    };
    if (stringKey instanceof Language_Types.RC_DEACTIVATED) {
        return "RC \u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.RC_DEACTIVATED_DETAILS) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0986\u09b0\u09b8\u09bf \u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8\u09c7 \u0985\u09a8\u09cd\u09af \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09c7\u09b0 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f\u09c7 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0986\u099b\u09c7\u0964 \u09b0\u09be\u0987\u09a1 \u09a8\u09c7\u0993\u09af\u09bc\u09be \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09a4\u09c7 \u098f\u0995\u099f\u09bf RC \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NEED_HELP) {
        return "\u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u09a6\u09b0\u0995\u09be\u09b0?";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_NOW_TO_GET_SPECIAL_DISCOUNTS) {
        return "\u09ac\u09bf\u09b6\u09c7\u09b7 \u099b\u09be\u09a1\u09bc \u09aa\u09c7\u09a4\u09c7 \u098f\u0996\u09a8\u0987 \u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SETUP_NOW) {
        return "\u098f\u0996\u09a8 \u09b8\u09c7\u099f\u0986\u09aa";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_MOBILITY) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u099a\u09be\u09b2\u09a8\u09be \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b0\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_DISABILITY) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u098f\u0995\u099f\u09bf \u09aa\u09cd\u09b0\u09a4\u09bf\u09ac\u09a8\u09cd\u09a7\u09bf \u0986\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_VISION) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u09a6\u09c1\u09b0\u09cd\u09a6\u09b6\u09be\u09b0\u09cd\u09af \u09a6\u09b0\u09cd\u09b6\u09a8 \u09b0\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_HEARING_IMPAIRMENT) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u09a6\u09c1\u09b0\u09cd\u09a6\u09b6\u09be\u09b0\u09cd\u09af \u09b6\u09cd\u09b0\u09ac\u09a3 \u09b0\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.HELP_WITH_THEIR_MOBILITY_AID) {
        return "\u09a4\u09be\u09a6\u09c7\u09b0 \u099a\u09b2\u09a8\u09be\u09b0 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_ASSIST_THEM_IF_NEEDED) {
        return "\u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u099c\u09a8\u09c7 \u09a4\u09be\u09a6\u09c7\u09b0 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.MESSAGE_THEM_AT_PICKUP) {
        return "\u09aa\u09bf\u0995\u0986\u09aa\u09c7 \u09a4\u09be\u09a6\u09c7\u09b0 \u09ae\u09c7\u09b8\u09c7\u099c \u09aa\u09be\u09a0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.SOUND_HORN_ONCE_AT_PICKUP) {
        return "\u09aa\u09bf\u0995\u0986\u09aa\u09c7 \u098f\u0995\u09ac\u09be\u09b0 \u09b9\u09b0\u09cd\u09a8 \u09ac\u09be\u099c\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_CALL_AND_AVOID_CHATS) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0995\u09b2 \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u099a\u09cd\u09af\u09be\u099f \u098f\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_CHAT_AND_AVOID_CALLS) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u099a\u09cd\u09af\u09be\u099f \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u0995\u09b2 \u098f\u09a1\u09bc\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_GO_TO_EXACT_PICKUP) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09a8\u09bf\u09b0\u09cd\u09a6\u09bf\u09b7\u09cd\u099f \u09aa\u09bf\u0995\u0986\u09aa \u09b8\u09cd\u09a5\u09be\u09a8\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_POOR_VISION_SOUND_HORN_AT_PICKUP) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u09a6\u09c1\u09b0\u09cd\u09a6\u09b6\u09be\u09b0\u09cd\u09af \u09a6\u09b0\u09cd\u09b6\u09a8 \u09b0\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u09aa\u09bf\u0995\u0986\u09aa\u09c7 \u098f\u0995\u09ac\u09be\u09b0 \u09b9\u09b0\u09cd\u09a8 \u09ac\u09be\u099c\u09be\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_POOR_HEARING_MESSAGE_THEM_AT_PICKUP) {
        return "\u09b0\u09be\u0987\u09a1\u09be\u09b0\u09c7\u09b0 \u09a6\u09c1\u09b0\u09cd\u09a6\u09b6\u09be\u09b0\u09cd\u09af \u09b6\u09cd\u09b0\u09ac\u09a3 \u09b0\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u09aa\u09bf\u0995\u0986\u09aa\u09c7 \u09a4\u09be\u09a6\u09c7\u09b0 \u09ae\u09c7\u09b8\u09c7\u099c \u09a6\u09bf\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_MOBILITY_STORE_THEIR_SUPPORT_AT_PICKUP) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u099a\u09be\u09b2\u09a8\u09be \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b0\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09a4\u09be\u09a6\u09c7\u09b0 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u0995\u09b0\u09c7 \u09aa\u09cd\u09af\u09be\u09b8\u09c7\u099e\u09cd\u099c\u09be\u09b0 \u0986\u09b8\u09a8\u09c7\u09b0 \u09aa\u09bf\u099b\u09a8\u09c7 \u09a4\u09be\u09a6\u09c7\u09b0 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u09b8\u09cd\u099f\u09cb\u09b0 \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_DISABILITY_PLEASE_ASSIST_THEM) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u098f\u0995\u099f\u09bf \u09aa\u09cd\u09b0\u09a4\u09bf\u09ac\u09a8\u09cd\u09a7\u09bf \u0986\u099b\u09c7\u0964 \u09a4\u09be\u09a6\u09c7\u09b0 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u0995\u09b0\u09c1\u09a8 \u09af\u09c7\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_MAY_NEED_ASSISTANCE) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u099c\u09a8 \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.LEARN_MORE) {
        return "\u0986\u09b0\u0993 \u099c\u09be\u09a8\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_MOBILITY_GO_TO_EXACT_LOC) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u099a\u09be\u09b2\u09a8\u09be \u09b8\u09ae\u09b8\u09cd\u09af\u09be \u09b0\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09a8\u09bf\u09b0\u09cd\u09a6\u09bf\u09b7\u09cd\u099f \u09aa\u09bf\u0995\u0986\u09aa \u09b8\u09cd\u09a5\u09be\u09a8\u09c7 \u09af\u09be\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_POOR_HEARING_CHAT_WITH_THEM_INSTEAD_OF_CALLING) {
        return "\u09b0\u09be\u0987\u09a1\u09be\u09b0\u09c7\u09b0 \u09a6\u09c1\u09b0\u09cd\u09a6\u09b6\u09be\u09b0\u09cd\u09af \u09b6\u09cd\u09b0\u09ac\u09a3 \u09b0\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u0995\u09b2 \u0995\u09b0\u09be\u09b0 \u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09c7 \u09a4\u09be\u09a6\u09c7\u09b0 \u099a\u09cd\u09af\u09be\u099f \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_HAS_LOW_VISION_CALL_THEM_INSTEAD_OF_CHATTING) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u09a6\u09c1\u09b0\u09cd\u09a6\u09b6\u09be\u09b0\u09cd\u09af \u09a6\u09b0\u09cd\u09b6\u09a8 \u09b0\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u099a\u09cd\u09af\u09be\u099f \u0995\u09b0\u09be\u09b0 \u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09c7 \u09a4\u09be\u09a6\u09c7\u09b0 \u0995\u09b2 \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.PLEASE_HELP_THEM_AS_YOU_CAN) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u09c7\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8 \u09b8\u09c7\u0987 \u09ae\u09a7\u09cd\u09af\u09c7 \u09a4\u09be\u09a6\u09c7\u09b0 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LEARN_HOW_YOU_CAN_HELP_CUSTOMERS_REQUIRING_SPECIAL_ASSISTANCE) {
        return "\u09ac\u09bf\u09b6\u09c7\u09b7 \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be\u09b0 \u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u099c\u09a8 \u098f\u09ae\u09a8 \u0997\u09cd\u09b0\u09be\u09b9\u0995\u09a6\u09c7\u09b0 \u0986\u09aa\u09a8\u09bf \u0995\u09c0\u09ad\u09be\u09ac\u09c7 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8 \u09a4\u09be \u099c\u09be\u09a8\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ASSISTANCE_REQUIRED) {
        return "\u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u099c\u09a8";
    };
    if (stringKey instanceof Language_Types.SAVED_DUE_TO_ZERO_COMMISSION) {
        return "\u099c\u09bf\u09b0\u09cb \u0995\u09ae\u09bf\u09b6\u09a8\u09c7\u09b0\x0a\u0995\u09be\u09b0\u09a3\u09c7 \u09b8\u0982\u09b0\u0995\u09cd\u09b7\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.TIP_EARNED_FROM_CUSTOMER) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995\u09c7\u09b0 \u0995\u09be\u099b \u09a5\u09c7\u0995\u09c7\x0a\u0985\u09b0\u09cd\u099c\u09bf\u09a4 \u099f\u09bf\u09aa";
    };
    if (stringKey instanceof Language_Types.COLLECT_VIA_CASE_UPI) {
        return "\u0995\u09cd\u09af\u09be\u09b6/\u0987\u0989\u09aa\u09bf\u0986\u0987 \u098f\u09b0 \u09ae\u09be\u09a7\u09cd\u09af\u09ae\u09c7 \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.FARE_COLLECTED) {
        return "\u09ad\u09be\u09a1\u09bc\u09be \u09b8\u0982\u0997\u09c3\u09b9\u09c0\u09a4";
    };
    if (stringKey instanceof Language_Types.RATE_YOUR_RIDE_WITH1) {
        return "";
    };
    if (stringKey instanceof Language_Types.RATE_YOUR_RIDE_WITH2) {
        return "\u098f\u09b0 \u09b8\u09be\u09a5\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u09b0\u09be\u0987\u09a1\u0995\u09c7 \u09b0\u09c7\u099f \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.HELP_US_WITH_YOUR_FEEDBACK) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09cd\u09b0\u09a4\u09bf\u0995\u09cd\u09b0\u09bf\u09af\u09bc\u09be \u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af \u0995\u09b0\u09c7 (\u0990\u099a\u09cd\u099b\u09bf\u0995)";
    };
    if (stringKey instanceof Language_Types.COLLECT_CASH) {
        return "\u09a8\u0997\u09a6 \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ONLINE_PAYMENT) {
        return "\u0985\u09a8\u09b2\u09be\u0987\u09a8 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f";
    };
    if (stringKey instanceof Language_Types.RIDE_COMPLETED) {
        return "\u09b0\u09be\u0987\u09a1 \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.SUBMIT_FEEDBACK) {
        return "\u09aa\u09cd\u09b0\u09a4\u09bf\u0995\u09cd\u09b0\u09bf\u09af\u09bc\u09be \u099c\u09ae\u09be \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.BADGE_EARNED) {
        return "\u09ac\u09cd\u09af\u09be\u099c \u0985\u09b0\u09cd\u099c\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.PURPLE_RIDE_CHAMPION) {
        return "\u09aa\u09be\u09b0\u09cd\u09aa\u09b2 \u09b0\u09be\u0987\u09a1 \u099a\u09cd\u09af\u09be\u09ae\u09cd\u09aa\u09bf\u09af\u09bc\u09a8";
    };
    if (stringKey instanceof Language_Types.PURPLE_RIDE) {
        return "\u09ac\u09c7\u0997\u09c1\u09a8\u09bf \u09b0\u09be\u0987\u09a1";
    };
    if (stringKey instanceof Language_Types.PROCEED_TO_CHAT) {
        return "\u099a\u09cd\u09af\u09be\u099f \u09a6\u09bf\u09af\u09bc\u09c7 \u098f\u0997\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_CONSIDER_CALLING_THEM) {
        return "\u09a4\u09be\u09b0\u09be \u099a\u09cd\u09af\u09be\u099f \u09aa\u09a1\u09bc\u09a4\u09c7 \u09b8\u0995\u09cd\u09b7\u09ae \u09a8\u09be\u0993 \u09b9\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7. \u09a4\u09be\u09a6\u09c7\u09b0 \u0995\u09b2 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OR) {
        return "\u09ac\u09be";
    };
    if (stringKey instanceof Language_Types.COLLECT_CASH_DIRECTLY) {
        return "\u09b8\u09b0\u09be\u09b8\u09b0\u09bf \u09a8\u0997\u09a6 \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OR_COLLECT_CASH_DIRECTLY) {
        return "\u0985\u09a5\u09ac\u09be \u09b8\u09b0\u09be\u09b8\u09b0\u09bf \u09a8\u0997\u09a6 \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_TO_ACCEPT_PAYMENT) {
        return "\u09b8\u09b0\u09be\u09b8\u09b0\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09cd\u09af\u09be\u0999\u09cd\u0995 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f\u09c7 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0997\u09cd\u09b0\u09b9\u09a3 \u0995\u09b0\u09a4\u09c7, \u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8\u09c7 \u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f \u0986\u09aa \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DOWNLOAD_QR) {
        return "QR \u09a1\u09be\u0989\u09a8\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.USE_THIS_QR_TO_COLLECT_PAYMENT) {
        return "\u0986\u09aa\u09a8\u09bf \u09b0\u09be\u0987\u09a1 \u09b6\u09c7\u09b7 \u09b9\u0993\u09af\u09bc\u09be\u09b0 \u09aa\u09b0\u09c7 \u0985\u09b0\u09cd\u09a5 \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09a4\u09c7 \u098f\u0987 QR \u0995\u09cb\u09a1 \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0 \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.AMOUNT_WILL_DEPOSITED_TO_BANK_ACCOUNT) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u09c7 \u09ac\u09cd\u09af\u09be\u0999\u09cd\u0995 \u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f\u099f\u09bf UPI \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0 \u0995\u09b0\u099b\u09c7\u09a8 \u09a4\u09be\u09a4\u09c7 \u0985\u09b0\u09cd\u09a5 \u099c\u09ae\u09be \u0995\u09b0\u09be \u09b9\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.GET_DIRECTLY_TO_YOUR_BANK_ACCOUNT) {
        return "UPI \u0986\u0987\u09a1\u09bf\u09a4\u09c7 \u09b8\u09b0\u09be\u09b8\u09b0\u09bf \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09aa\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.PAYMENT) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f";
    };
    if (stringKey instanceof Language_Types.QR_CODE) {
        return "QR \u0995\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.GET_QR_CODE) {
        return "QR \u0995\u09cb\u09a1 \u09aa\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.COMPLETE_PAYMENT_TO_CONTINUE) {
        return "Yatri Sathi \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0 \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7, \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.COLLECT_VIA_UPI_QR_OR_CASH) {
        return "UPI QR \u09ac\u09be \u0995\u09cd\u09af\u09be\u09b6\u09c7\u09b0 \u09ae\u09be\u09a7\u09cd\u09af\u09ae\u09c7 \u09b8\u0982\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PENDING_CAPS) {
        return "\u09aa\u09c7\u09a8\u09cd\u09a1\u09bf\u0982";
    };
    if (stringKey instanceof Language_Types.FAILURE) {
        return "\u09ac\u09cd\u09af\u09b0\u09cd\u09a5\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.PAYMENT_MODE) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ae\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.TXN_ID) {
        return "\u0985\u09b0\u09cd\u09a1\u09be\u09b0 \u0986\u0987\u09a1\u09bf";
    };
    if (stringKey instanceof Language_Types.AMOUNT_PAID) {
        return "\u09aa\u09cd\u09b0\u09a6\u09c7\u09af\u09bc \u09aa\u09b0\u09bf\u09ae\u09be\u09a3";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_SCHEDULED) {
        return "\u09ac\u09bf\u099c\u09cd\u099e\u09aa\u09cd\u09a4\u09bf \u09a8\u09bf\u09b0\u09cd\u09a7\u09be\u09b0\u09bf\u09a4!";
    };
    if (stringKey instanceof Language_Types.MANUAL_DUES) {
        return "\u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u09ac\u0995\u09c7\u09af\u09bc\u09be";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_IN_PROGRESS) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09aa\u09cd\u09b0\u0997\u09a4\u09bf\u09a4\u09c7 \u0986\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.MANUAL_DUE_OVERVIEW) {
        return "\u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u09a1\u09bf\u0989 \u0993\u09ad\u09be\u09b0\u09ad\u09bf\u0989";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_DUE_OVERVIEW) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09a1\u09bf\u0989 \u0993\u09ad\u09be\u09b0\u09ad\u09bf\u0989";
    };
    if (stringKey instanceof Language_Types.MANUAL_DUE_AS_AUTOPAY_EXECUTION_FAILED) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u098f\u0995\u09cd\u09b8\u09bf\u0995\u09bf\u0989\u09b6\u09a8 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u0993\u09af\u09bc\u09be\u09af\u09bc \u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u09ac\u0995\u09c7\u09af\u09bc\u09be";
    };
    if (stringKey instanceof Language_Types.CLEAR_MANUAL_DUES) {
        return "\u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09aa\u09b0\u09bf\u09b7\u09cd\u0995\u09be\u09b0 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DUE_OVERVIEW) {
        return "\u09a8\u09bf\u09b0\u09cd\u09a7\u09be\u09b0\u09bf\u09a4 \u0993\u09ad\u09be\u09b0\u09ad\u09bf\u0989";
    };
    if (stringKey instanceof Language_Types.MANUAL_DUE_DETAILS) {
        return "\u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_DUE_DETAILS) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.SWITCHED_TO_MANUAL) {
        return "*\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ae\u09cb\u09a1 \u09ae\u09cd\u09af\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b2\u09c7 \u09b8\u09cd\u09af\u09c1\u0987\u099a \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7 \u09af\u09c7\u09b9\u09c7\u09a4\u09c1 \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09aa\u09c2\u09b0\u09a3 \u0995\u09b0\u09be \u09af\u09be\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.SPLIT_PAYMENT) {
        return "*\u0985\u099f\u09cb\u09aa\u09c7\u0987 \u0995\u09be\u09b0\u09cd\u09af\u0995\u09b0 \u0995\u09b0\u09a4\u09c7 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09ac\u09bf\u09ad\u0995\u09cd\u09a4 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.GST_INCLUDE) {
        return "(18% GST \u09b8\u09b9)";
    };
    if (stringKey instanceof Language_Types.SCHEDULED_AT) {
        return "\u09a8\u09bf\u09b0\u09cd\u09a7\u09be\u09b0\u09bf\u09a4 \u09b8\u09ae\u09af\u09bc\u09c7";
    };
    if (stringKey instanceof Language_Types.PAYMENT_STATUS) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09b8\u09cd\u099f\u09cd\u09af\u09be\u099f\u09be\u09b8";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_ATTEMPTING) {
        return "\u09ac\u09bf\u099c\u09cd\u099e\u09aa\u09cd\u09a4\u09bf\u09b0 \u09aa\u09cd\u09b0\u099a\u09c7\u09b7\u09cd\u099f\u09be";
    };
    if (stringKey instanceof Language_Types.EXECUTION_SCHEDULED) {
        return "\u09b8\u09ae\u09cd\u09aa\u09be\u09a6\u09a8\u09be \u09a8\u09bf\u09b0\u09cd\u09a7\u09be\u09b0\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.EXECUTION_ATTEMPTING) {
        return "\u09b8\u09ae\u09cd\u09aa\u09be\u09a6\u09a8\u09be \u0995\u09b0\u09be\u09b0 \u09aa\u09cd\u09b0\u099a\u09c7\u09b7\u09cd\u099f\u09be";
    };
    if (stringKey instanceof Language_Types.EXECUTION_SUCCESS) {
        return "\u09b8\u09ae\u09cd\u09aa\u09be\u09a6\u09a8\u09be \u09b8\u09ab\u09b2";
    };
    if (stringKey instanceof Language_Types.PAYMENT_SCHEDULED) {
        return "\u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09a8\u09bf\u09b0\u09cd\u09a7\u09be\u09b0\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.OFFER_CARD_BANNER_TITLE) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 {} \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4 \u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u20b915/\u09a6\u09bf\u09a8 \u09aa\u09c7 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OFFER_CARD_BANNER_DESC) {
        return "1 \u09a8\u09ad\u09c7\u09ae\u09cd\u09ac\u09b0 \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4 \u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u20b915/\u09a6\u09bf\u09a8 \u09aa\u09c7 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.OFFER_CARD_BANNER_ALERT) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u09ae\u09c1\u09b2\u09a4\u09c1\u09ac\u09bf \u0986\u099b\u09c7!";
    };
    if (stringKey instanceof Language_Types.EXECUTION_FAILED) {
        return "\u09b8\u09ae\u09cd\u09aa\u09be\u09a6\u09a8\u09be \u09ac\u09cd\u09af\u09b0\u09cd\u09a5";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_FAILED) {
        return "\u09ac\u09bf\u099c\u09cd\u099e\u09aa\u09cd\u09a4\u09bf \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.PAY_NOW) {
        return "\u098f\u0996\u09a8\u0987 \u0985\u09b0\u09cd\u09a5 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CLEAR_DUES_BANNER_TITLE) {
        return "\u09a8\u09a8-\u09b8\u09cd\u099f\u09aa \u09b0\u09be\u0987\u09a1 \u0989\u09aa\u09ad\u09cb\u0997 \u0995\u09b0\u09a4\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09b8\u09be\u09ab \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_DEBITED_ON) {
        return "\u09b2\u09c7\u09a8\u09a6\u09c7\u09a8 \u09a1\u09c7\u09ac\u09bf\u099f \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.TRANSACTION_ATTEMPTED_ON) {
        return "\u09b2\u09c7\u09a8\u09a6\u09c7\u09a8\u09c7\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_AND_PAYMENT_SUCCESSFUL) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u098f\u09ac\u0982 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09b8\u09ab\u09b2!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_SUCCESSFUL) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u09b8\u09ab\u09b2!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_AND_PAYMENT_PENDING) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u098f\u09ac\u0982 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ae\u09c1\u09b2\u09a4\u09c1\u09ac\u09bf!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_PENDING) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u09ae\u09c1\u09b2\u09a4\u09c1\u09ac\u09bf!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_AND_PAYMENT_FAILED) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u098f\u09ac\u0982 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7!";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_SETUP_FAILED) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7!";
    };
    if (stringKey instanceof Language_Types.ONE_TIME_REGISTERATION) {
        return "\u098f\u0995\u09ac\u09be\u09b0 \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09a8";
    };
    if (stringKey instanceof Language_Types.CLEARANCE_AND_REGISTERATION) {
        return "\u0995\u09cd\u09b2\u09bf\u09af\u09bc\u09be\u09b0\u09c7\u09a8\u09cd\u09b8 + \u09b0\u09c7\u099c\u09bf\u09b8\u09cd\u099f\u09cd\u09b0\u09c7\u09b6\u09a8";
    };
    if (stringKey instanceof Language_Types.UPI_AUTOPAY_SETUP) {
        return "UPI \u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa";
    };
    if (stringKey instanceof Language_Types.WATCH_VIDEO_FOR_HELP) {
        return "\u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09ad\u09bf\u09a1\u09bf\u0993 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PAYMENT_PENDING_SOFT_NUDGE) {
        return "\u09aa\u09cd\u09b0\u09be\u09aa\u09cd\u09af \u09b8\u09c0\u09ae\u09be \u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u09aa\u09cc\u0981\u099b\u09c7 \u09af\u09be\u09ac\u09c7\u0964\u09a8\u09a8-\u09b8\u09cd\u099f\u09aa \u09b0\u09be\u0987\u09a1\u0997\u09c1\u09b2\u09bf \u0989\u09aa\u09ad\u09cb\u0997 \u0995\u09b0\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09be\u09ab \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09aa\u09b0\u09bf\u09b7\u09cd\u0995\u09be\u09b0 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CLEAR_YOUR_DUES_EARLY) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09a4\u09be\u09a1\u09bc\u09be\u09a4\u09be\u09a1\u09bc\u09bf \u09b8\u09be\u09ab \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DUE_LIMIT_WARNING_BANNER_TITLE) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09af\u09a5\u09be\u09b8\u09ae\u09af\u09bc\u09c7 \u20b9{} \u09a1\u09b2\u09be\u09b0 \u09b8\u09c0\u09ae\u09be \u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u09aa\u09cc\u0981\u099b\u09c7 \u09af\u09be\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.FREE_TRIAL_ENDING_IN_2_DAYS) {
        return "\u09ac\u09bf\u09a8\u09be\u09ae\u09c2\u09b2\u09cd\u09af\u09c7 \u099f\u09cd\u09b0\u09be\u09af\u09bc\u09be\u09b2 2 \u09a6\u09bf\u09a8\u09c7\u09b0 \u09ae\u09a7\u09cd\u09af\u09c7 \u09b6\u09c7\u09b7!";
    };
    if (stringKey instanceof Language_Types.FREE_TRIAL_ENDING_TOMORROW) {
        return "\u09ac\u09bf\u09a8\u09be\u09ae\u09c2\u09b2\u09cd\u09af\u09c7 \u099f\u09cd\u09b0\u09be\u09af\u09bc\u09be\u09b2 \u0986\u0997\u09be\u09ae\u09c0\u0995\u09be\u09b2 \u09b6\u09c7\u09b7!";
    };
    if (stringKey instanceof Language_Types.FREE_TRIAL_ENDS_TONIGHT) {
        return "\u09ac\u09bf\u09a8\u09be\u09ae\u09c2\u09b2\u09cd\u09af\u09c7 \u099f\u09cd\u09b0\u09be\u09af\u09bc\u09be\u09b2 \u0986\u099c \u09b0\u09be\u09a4\u09c7 \u09b6\u09c7\u09b7!";
    };
    if (stringKey instanceof Language_Types.JOIN_A_PLAN_TO_CONTINUE_TAKING_RIDES) {
        return "\u09b0\u09be\u0987\u09a1\u0997\u09c1\u09b2\u09bf \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09be\u0993\u09af\u09bc\u09be\u09b0 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be\u09af\u09bc \u09af\u09cb\u0997 \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.SETUP_AUTOPAY_FOR_EASY_PAYMENTS) {
        return "\u09b8\u09b9\u099c \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f\u0986\u09aa \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SCHEDULED_ON) {
        return "\u099b\u09be\u09a1\u09bc\u09c7\u09b0 \u09a4\u09be\u09b0\u09bf\u0996";
    };
    if (stringKey instanceof Language_Types.ATTEMPTED_ON) {
        return "\u09aa\u09cd\u09b0\u099a\u09c7\u09b7\u09cd\u099f\u09be\u09b0 \u09a4\u09be\u09b0\u09bf\u0996";
    };
    if (stringKey instanceof Language_Types.LOW_DUES_CLEAR_POPUP_DESC) {
        return "\u0985-\u09b8\u09cd\u099f\u09aa \u09b0\u09be\u0987\u09a1\u0997\u09c1\u09b2\u09bf \u0989\u09aa\u09ad\u09cb\u0997 \u0995\u09b0\u09a4\u09c7 \u098f\u0996\u09a8\u0987 \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09b8\u09be\u09ab \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DUES_PENDING) {
        return "\u26a0\ufe0f \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09ae\u09c1\u09b2\u09a4\u09c1\u09ac\u09bf! \u26a0\ufe0f";
    };
    if (stringKey instanceof Language_Types.DAYS) {
        return "\u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.ACTIVE_PLAN) {
        return "\u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.DOWNGRADE_AVAILABLE_ONLY_FOR_AC_VEHICLES) {
        return "\u09a1\u09be\u0989\u09a8\u0997\u09cd\u09b0\u09c7\u09a1 \u0997\u09be\u09a1\u09bc\u09bf \u0985\u09aa\u09b6\u09a8 \u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u098f\u09b8\u09bf \u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u099c\u09a8\u09cd\u09af \u0989\u09aa\u09b2\u09ac\u09cd\u09a7";
    };
    if (stringKey instanceof Language_Types.DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_1) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u09a1\u09be\u0989\u09a8\u0997\u09cd\u09b0\u09c7\u09a1 \u0995\u09b0\u09b2\u09c7 \u0986\u09aa\u09a8\u09bf ";
    };
    if (stringKey instanceof Language_Types.DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_2) {
        return "\u098f\u09ac\u0982";
    };
    if (stringKey instanceof Language_Types.DOWNGRADING_VEHICLE_WILL_ALLOW_YOU_TO_TAKE_BOTH_3) {
        return " \u09b0\u09be\u0987\u09a1 \u0989\u09ad\u09af\u09bc\u0987 \u09a8\u09bf\u09a4\u09c7 \u09aa\u09be\u09b0\u09ac\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.AC_CAB) {
        return "\u098f\u09b8\u09bf \u0995\u09cd\u09af\u09be\u09ac";
    };
    if (stringKey instanceof Language_Types.AC_SUV) {
        return "\u098f\u09b8\u09bf \u098f\u09b8\u0987\u0989\u09ad\u09bf";
    };
    if (stringKey instanceof Language_Types.DOWNGRADE_VEHICLE) {
        return "\u09a1\u09be\u0989\u09a8\u0997\u09cd\u09b0\u09c7\u09a1 \u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8";
    };
    if (stringKey instanceof Language_Types.WHAT_ARE_PURPLE_RIDES) {
        return "\u09aa\u09be\u09b0\u09cd\u09aa\u09b2 \u09b0\u09be\u0987\u09a1\u09b8 \u0995\u09bf?";
    };
    if (stringKey instanceof Language_Types.ECONOMICAL) {
        return "\u0985\u09b0\u09cd\u09a5\u09a8\u09c8\u09a4\u09bf\u0995";
    };
    if (stringKey instanceof Language_Types.SPACIOUS) {
        return "\u09aa\u09cd\u09b0\u09b6\u09b8\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.COMFY) {
        return "\u0986\u09b0\u09be\u09ae\u09a6\u09be\u09af\u09bc\u0995";
    };
    if (stringKey instanceof Language_Types.PEOPLE) {
        return "\u09ae\u09be\u09a8\u09c1\u09b7";
    };
    if (stringKey instanceof Language_Types.GO_TO) {
        return "\u0997\u09cb-\u099f\u09c1";
    };
    if (stringKey instanceof Language_Types.SELECT_ON_MAP) {
        return "\u09ae\u09be\u09a8\u099a\u09bf\u09a4\u09cd\u09b0\u09c7 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CONFIRM_LOCATION_STR) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SAVE_LOCATION_STR) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u0982\u09b0\u0995\u09cd\u09b7\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.REMOVE_PREF_LOC) {
        return "\u09aa\u099b\u09a8\u09cd\u09a6\u09c7\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.CONF_REMOVE_PREF_LOC) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u09af\u09c7 \u0986\u09aa\u09a8\u09bf \u09aa\u099b\u09a8\u09cd\u09a6\u09c7\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u099f\u09bf \u09b8\u09b0\u09be\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.YES_REMOVE) {
        return "\u09b9\u09cd\u09af\u09be\u0981, \u09b8\u09b0\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_LOCATION) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_ANOTHER_LOCATION) {
        return "\u0985\u09a8\u09cd\u09af \u098f\u0995\u099f\u09bf \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_A_GOTO_LOC) {
        return "\u09af\u09c7\u09a4\u09c7 \u09af\u09c7\u09a4\u09c7 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_LEFT) {
        return "\u0997\u09cb-\u099f\u09c1 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09ac\u09be\u0995\u09bf \u0986\u099b\u09c7:";
    };
    if (stringKey instanceof Language_Types.CURRENT_LOCATION) {
        return "\u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.CONF_GOTO_LOC) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7 \u09af\u09be\u09a8 \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GOTO_LOCS) {
        return "\u09b8\u09cd\u09a5\u09be\u09a8\u09c7 \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.LOCATION_STR) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.ADD_TAG) {
        return "\u099f\u09cd\u09af\u09be\u0997 \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ONLY_ONE_LOC_CAN_ADDED) {
        return "(\u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u098f\u0995\u099f\u09bf \u09b8\u09cd\u09a5\u09be\u09a8\u09c7 \u098f\u0987 \u099f\u09cd\u09af\u09be\u0997 \u09a5\u09be\u0995\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7)";
    };
    if (stringKey instanceof Language_Types.SAVE_AS) {
        return "\u098f\u09ad\u09be\u09ac\u09c7 \u09b8\u0982\u09b0\u0995\u09cd\u09b7\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NO_GOTO_LOC_ADDED) {
        return "\u098f\u0996\u09a8\u0993 \u0995\u09cb\u09a5\u09be\u0993 \u09af\u09be\u0993\u09af\u09bc\u09be\u09b0 \u09b2\u09cb\u0995\u09c7\u09b6\u09a8 \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_HELPS_YOU) {
        return "\u09b2\u09cb\u0995\u09c7\u09b6\u09a8\u09c7 \u09af\u09be\u09a8 \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u099b\u09a8\u09cd\u09a6\u09c7\u09b0 \u099c\u09be\u09af\u09bc\u0997\u09be\u09af\u09bc \u098f\u09ac\u0982 \u0986\u09b6\u09c7\u09aa\u09be\u09b6\u09c7 \u09b0\u09be\u0987\u09a1 \u0996\u09c1\u0981\u099c\u09c7 \u09aa\u09c7\u09a4\u09c7 \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u0995\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_VERY_CLOSE) {
        return "\u0986\u09aa\u09a8\u09bf \u201c\u0997\u09cb-\u099f\u09c1\u201d \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7\u09b0 \u0996\u09c1\u09ac \u0995\u09be\u099b\u09be\u0995\u09be\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.GOTO_IS_APPLICABLE_FOR) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u09b2\u09cb\u0995\u09c7\u09b6\u09a8\u09c7\u09b0 \u0995\u09cd\u09b7\u09c7\u09a4\u09cd\u09b0\u09c7 \u09aa\u09cd\u09b0\u09af\u09cb\u099c\u09cd\u09af \x0a \u09af\u09c7\u0997\u09c1\u09b2\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09a5\u09c7\u0995\u09c7 \u0995\u09ae\u09aa\u0995\u09cd\u09b7\u09c7 3 \u0995\u09bf\u09ae\u09bf \u09a6\u09c2\u09b0\u09c7 \x0a\u0964";
    };
    if (stringKey instanceof Language_Types.GOTO_MAYBE_REDUCED) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u0995\u09ae\u09c7 \u09af\u09c7\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.CANCEL_OF_GOTO) {
        return "\u098f\u0995\u099f\u09bf \u201c\u0997\u09cb-\u099f\u09c1\u201d \u09b0\u09be\u0987\u09a1 \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09b2\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u201c\u0997\u09cb-\u099f\u09c1\u201d \u09ac\u09bf\u0995\u09b2\u09cd\u09aa\u0997\u09c1\u09b2\u09bf \u09b9\u09cd\u09b0\u09be\u09b8 \u09aa\u09be\u09ac\u09c7!";
    };
    if (stringKey instanceof Language_Types.MORE_GOTO_RIDE_COMING) {
        return "\u0986\u09b0\u09cb \u201c\u0997\u09cb-\u099f\u09c1\u201d \u09b0\u09be\u0987\u09a1 \u0986\u09b8\u099b\u09c7!";
    };
    if (stringKey instanceof Language_Types.MORE_GOTO_RIDE_COMING_DESC) {
        return "\u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0985\u09a8\u09b2\u09be\u0987\u09a8\u09c7 \u09a5\u09be\u0995\u09c1\u09a8; \u0986\u09ae\u09b0\u09be \u0986\u09aa\u09a8\u09be\u09b0 \u201c\u0997\u09cb-\u099f\u09c1\u201d \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u0986\u09b0\u09cb \u09b0\u09be\u0987\u09a1\u09c7\u09b0 \u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u09aa\u09be\u099a\u09cd\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.GOTO_REDUCED_TO_ZERO) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u0995\u09ae\u09c7 \u09b6\u09c2\u09a8\u09cd\u09af \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.DUE_TO_MULTIPLE_CANCELLATIONS) {
        return "\u098f\u0995\u09be\u09a7\u09bf\u0995 \u09ac\u09be\u09a4\u09bf\u09b2\u0995\u09b0\u09a3\u09c7\u09b0 \u0995\u09be\u09b0\u09a3\u09c7, \u0997\u09a3\u09a8\u09be \u0995\u09be\u099f\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.OK_GOT_IT) {
        return "\u09a0\u09bf\u0995 \u0986\u099b\u09c7, \u09ac\u09c1\u099d\u09c7\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.GOTO_REDUCED_TO) {
        return "\u0995\u09ae\u09bf\u09a4 \u201c\u0997\u09cb-\u099f\u09c1\u201d";
    };
    if (stringKey instanceof Language_Types.VALIDITY_EXPIRED_DESC) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 30 \u09ae\u09bf\u09a8\u09bf\u099f\u09c7\u09b0 \u09ae\u09c7\u09af\u09bc\u09be\u09a6 \u09b6\u09c7\u09b7 \u09b9\u09af\u09bc\u09c7 \u0997\u09c7\u099b\u09c7\u0964 \u0986\u09ae\u09b0\u09be \u098f\u0987 \u09ae\u09c1\u09b9\u09c2\u09b0\u09cd\u09a4\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u09aa\u09c2\u09b0\u09a3 \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u099b\u09bf \u09a8\u09be\u0964";
    };
    if (stringKey instanceof Language_Types.KNOW_MORE) {
        return "\u0986\u09b0\u09cb \u099c\u09be\u09a8\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.THIS_FEATURE_WILL_BE_APPLICABLE) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u0996\u09a8 \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u09b0\u09cd\u09a4\u09ae\u09be\u09a8 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09a5\u09c7\u0995\u09c7 \u0995\u09ae\u09aa\u0995\u09cd\u09b7\u09c7 3 \u0995\u09bf\u09ae\u09bf \u09a6\u09c2\u09b0\u09c7 \u09a5\u09be\u0995\u09ac\u09c7\u09a8 \u09a4\u0996\u09a8 \u098f\u0987 \u09ac\u09c8\u09b6\u09bf\u09b7\u09cd\u099f\u09cd\u09af\u099f\u09bf \u09aa\u09cd\u09b0\u09af\u09cb\u099c\u09cd\u09af \u09b9\u09ac\u09c7\u09f7\x0a\x0a\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7\u09b0 \u09aa\u099b\u09a8\u09cd\u09a6\u0997\u09c1\u09b2\u09bf \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09be \u09b9\u09b2\u09c7 \u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 30 \u09ae\u09bf\u09a8\u09bf\u099f\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09ac\u09c8\u09a7\u0964";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_ADDED) {
        return "\u0997\u09cb-\u099f\u09c1 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_REMOVED) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u09aa\u09c1\u09a8\u09b0\u09c1\u09a6\u09cd\u09a7\u09be\u09b0 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_UPDATED) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u098f\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u09ab\u09b2\u09ad\u09be\u09ac\u09c7 \u0986\u09aa\u09a1\u09c7\u099f \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_IS_ENABLED) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_IS_DISABLED) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u0985\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.GOTO_LOCATIONS) {
        return "\u0997\u09cb-\u099f\u09c1 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.CHOOSE_A_GOTO_LOC) {
        return "\u098f\u0995\u099f\u09bf \u0997\u09a8\u09cd\u09a4\u09ac\u09cd\u09af \u099a\u09af\u09bc\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.YOU_HAVE_ONLY_LEFT_FOR_TODAY) {
        return "\u0986\u09aa\u09a8\u09bf \u09b6\u09c1\u09a7\u09c1\u09ae\u09be\u09a4\u09cd\u09b0 \u0986\u099c\u0995\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b0\u0993\u09af\u09bc\u09be\u09a8\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.YES_ENABLE) {
        return "\u09b9\u09cd\u09af\u09be\u0981 \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NO_GOTO_LOCS_ADDED_YET) {
        return "\u098f\u0996\u09a8\u0993 \u0995\u09cb\u09a8 \u0997\u09cb-\u099f\u09c1 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.NO_GOTO_LOCS_ADDED_YET_DESC) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u0997\u09c1\u09b2\u09bf \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u099b\u09a8\u09cd\u09a6\u09c7\u09b0 \u099c\u09be\u09af\u09bc\u0997\u09be\u09af\u09bc \u098f\u09ac\u0982 \u0986\u09b6\u09c7\u09aa\u09be\u09b6\u09c7 \u09b0\u09be\u0987\u09a1 \u0996\u09c1\u0981\u099c\u09c7 \u09aa\u09c7\u09a4\u09c7 \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be \u0995\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.ENABLE_GOTO) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GO_TO_CANCELLATION_TITLE) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u0995\u09ae\u09c7 \u09af\u09c7\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.GO_TO_CANCELLATION_DESC) {
        return "\u098f\u0995\u099f\u09bf \u201c\u0997\u09cb-\u099f\u09c1\u201d \u09b0\u09be\u0987\u09a1 \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09be \u09b9\u09b2\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u201c\u0997\u09cb-\u099f\u09c1\u201d \u09ac\u09bf\u0995\u09b2\u09cd\u09aa\u0997\u09c1\u09b2\u09bf \u09b9\u09cd\u09b0\u09be\u09b8 \u09aa\u09be\u09ac\u09c7!";
    };
    if (stringKey instanceof Language_Types.REPORT_ISSUE) {
        return "\u09b0\u09bf\u09aa\u09cb\u09b0\u09cd\u099f \u09b8\u09ae\u09b8\u09cd\u09af\u09be";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ALMOST_AT_LOCATION) {
        return "\u0986\u09aa\u09a8\u09bf \u09aa\u09cd\u09b0\u09be\u09af\u09bc \u099a\u09b2\u09c7 \u098f\u09b8\u09c7\u099b\u09c7\u09a8 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7\u0964";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_NOT_FOUND) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_DOES_NOT_EXIST) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09ac\u09bf\u09a6\u09cd\u09af\u09ae\u09be\u09a8 \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_LIMIT_REACHED) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7\u09b0 \u09b8\u09c0\u09ae\u09be \u09aa\u09cc\u0981\u099b\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_NOT_FOUND) {
        return "\u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09af\u09bc\u09a8\u09bf";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_DOES_NOT_EXIST) {
        return "\u0985\u09a8\u09c1\u09b0\u09cb\u09a7 \u09ac\u09bf\u09a6\u09cd\u09af\u09ae\u09be\u09a8 \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_DAILY_USAGE_LIMIT_REACHED) {
        return "\u09a6\u09c8\u09a8\u09bf\u0995 \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0\u09c7\u09b0 \u09b8\u09c0\u09ae\u09be \u09aa\u09cc\u0981\u099b\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.DRIVER_GO_HOME_REQUEST_ALREADY_ACTIVE) {
        return "\u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7\u0987 \u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_OUTSIDE_SERVICE_AREA) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09aa\u09b0\u09bf\u09b7\u09c7\u09ac\u09be \u098f\u09b2\u09be\u0995\u09be\u09b0 \u09ac\u09be\u0987\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.NEW_LOCATION_TOO_CLOSE_TO_PREVIOUS_HOME_LOCATION) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u099f\u09bf \u0986\u0997\u09c7\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7\u09b0 \u0996\u09c1\u09ac \u0995\u09be\u099b\u09be\u0995\u09be\u099b\u09bf";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_DOES_NOT_BELONG_TO_DRIVER) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u0986\u09aa\u09a8\u09be\u09b0 \u09a8\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.DRIVER_HOME_LOCATION_DELETE_WHILE_ACTIVE_ERROR) {
        return "\u09b8\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u09a5\u09be\u0995\u09be\u0995\u09be\u09b2\u09c0\u09a8 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09ae\u09c1\u099b\u09c7 \u09ab\u09c7\u09b2\u09be \u09af\u09be\u09ac\u09c7 \u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.DISABLE_GOTO_STR) {
        return "\u09af\u09be\u09a4\u09c7 \u0985\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09ac\u09c7\u09a8?";
    };
    if (stringKey instanceof Language_Types.YOU_STILL_HAVE_TIME_LEFT) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u201c\u0997\u09cb-\u099f\u09c1\u201d \u0985\u09a8\u09c1\u09b0\u09cb\u09a7\u09c7 \u098f\u0996\u09a8\u0993 \u09b8\u09ae\u09af\u09bc \u09ac\u09be\u0995\u09bf \u0986\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.YES_DISABLE) {
        return "\u09b9\u09cd\u09af\u09be\u0981, \u09a8\u09bf\u09b7\u09cd\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DRAG_TO_ADJUST) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u09be\u09ae\u099e\u09cd\u099c\u09b8\u09cd\u09af \u0995\u09b0\u09a4\u09c7 \u099f\u09c7\u09a8\u09c7 \u0986\u09a8\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LOCATION_ALREADY_EXISTS) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u0987\u09a4\u09bf\u09ae\u09a7\u09cd\u09af\u09c7\u0987 \u09ac\u09bf\u09a6\u09cd\u09af\u09ae\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.MIN_LEFT) {
        return "\u09ae\u09bf\u09a8\u09bf\u099f \u09ac\u09be\u0995\u09bf";
    };
    if (stringKey instanceof Language_Types.GOTO_LOC_REACHED) {
        return "\u201c\u0997\u09cb-\u099f\u09c1\u201d \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8\u09c7 \u09aa\u09cc\u0981\u099b\u09c7 \u0997\u09c7\u099b\u09c7!";
    };
    if (stringKey instanceof Language_Types.GET_READY_FOR_YS_SUBSCRIPTION) {
        return "\u09af\u09be\u09a4\u09cd\u09b0\u09c0 \u09b8\u09be\u09a5\u09c0 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u09aa\u09cd\u09b0\u09b8\u09cd\u09a4\u09c1\u09a4 \u09b9\u09a8!";
    };
    if (stringKey instanceof Language_Types.GUARANTEED_FIXED_PRICE) {
        return "1 \u099c\u09be\u09a8\u09c1\u09af\u09bc\u09be\u09b0\u09c0, 2025 \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4 \u09a8\u09bf\u09b0\u09cd\u09a6\u09bf\u09b7\u09cd\u099f \u09ae\u09c2\u09b2\u09cd\u09af\u09c7\u09b0 \u09a8\u09bf\u09b6\u09cd\u099a\u09af\u09bc\u09a4\u09be";
    };
    if (stringKey instanceof Language_Types.INTRODUCTORY_OFFER_TO_BE_ANNOUNCED_SOON) {
        return "\u09aa\u09b0\u09bf\u099a\u09af\u09bc\u09ae\u09c2\u09b2\u0995 \u0985\u09ab\u09be\u09b0 \u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u0998\u09cb\u09b7\u09a3\u09be \u0995\u09b0\u09be \u09b9\u09ac\u09c7!";
    };
    if (stringKey instanceof Language_Types.NO_CHARGES_TILL) {
        return "\u09e9\u09e7 \u09a1\u09bf\u09b8\u09c7\u09ae\u09cd\u09ac\u09b0 \u09aa\u09b0\u09cd\u09af\u09a8\u09cd\u09a4 \u0995\u09cb\u09a8\u09cb \u099a\u09be\u09b0\u09cd\u099c \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.SIGNUP_EARLY_FOR_SPECIAL_OFFERS) {
        return "\u09ac\u09bf\u09b6\u09c7\u09b7 \u0985\u09ab\u09be\u09b0\u0997\u09c1\u09b2\u09bf\u09b0 \u099c\u09a8\u09cd\u09af \u09a4\u09be\u09a1\u09bc\u09be\u09a4\u09be\u09a1\u09bc\u09bf \u09b8\u09be\u0987\u09a8 \u0986\u09aa \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DO_YOU_WANT_TO_CANCEL) {
        return "\u0986\u09aa\u09a8\u09bf \u0995\u09bf \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09a4\u09c7 \u099a\u09be\u09a8?";
    };
    if (stringKey instanceof Language_Types.DO_YOU_WANT_TO_CANCEL_DESC) {
        return "\u0986\u09aa\u09a8\u09bf \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09a4\u09c7 \u099a\u09b2\u09c7\u099b\u09c7\u09a8\u09f7\x0a\u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09a4\u09c7 \u0986\u09ac\u09be\u09b0 \u09b8\u09cd\u09ac\u09a4\u0983\u09aa\u09c7 \u09b8\u09c7\u099f \u0986\u09aa \u0995\u09b0\u09a4\u09c7 \u09b9\u09ac\u09c7\u09f7";
    };
    if (stringKey instanceof Language_Types.NO) {
        return "\u09a8\u09be";
    };
    if (stringKey instanceof Language_Types.YES_CANCEL) {
        return "\u09b9\u09cd\u09af\u09be\u0981, \u09ac\u09be\u09a4\u09bf\u09b2";
    };
    if (stringKey instanceof Language_Types.AUTOPAY_CANCELLED) {
        return "\u0985\u099f\u09cb\u09aa\u09c7 \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.YOUR_DUES_DESCRIPTION_MANUAL) {
        return "\u09a8\u09bf\u09b0\u09ac\u099a\u09cd\u099b\u09bf\u09a8\u09cd\u09a8 \u09b0\u09be\u0987\u09a1\u0997\u09c1\u09b2\u09bf \u099a\u09be\u09b2\u09bf\u09af\u09bc\u09c7 \u09af\u09c7\u09a4\u09c7 \u09b8\u09c0\u09ae\u09be\u09a4\u09c7 \u09aa\u09cc\u0981\u099b\u09be\u09a8\u09cb\u09b0 \u0986\u0997\u09c7 \u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u0986\u09aa\u09a8\u09be\u09b0 \u09ac\u0995\u09c7\u09af\u09bc\u09be \u09aa\u09b0\u09bf\u09b6\u09cb\u09a7 \u0995\u09b0\u09c1\u09a8\u09f7 \u0986\u09aa\u09a8\u09bf \u09b8\u09b9\u099c\u09c7, \u099d\u09be\u09ae\u09c7\u09b2\u09be\u09ae\u09c1\u0995\u09cd\u09a4 \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u09c7\u09b0 \u099c\u09a8\u09cd\u09af <b>\u098f\u0995\u099f\u09bf UPI \u0985\u099f\u09cb\u09aa\u09c7 \u09b8\u09c7\u099f \u0986\u09aa</b> \u0995\u09b0\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.AND) {
        return " \u098f\u09ac\u0982 ";
    };
    if (stringKey instanceof Language_Types.DIRECT_PAYMENT_NO_COMMISSIONS) {
        return "\u09b8\u09b0\u09be\u09b8\u09b0\u09bf \u0985\u09b0\u09cd\u09a5\u09aa\u09cd\u09b0\u09a6\u09be\u09a8\u0964\x0a\u0995\u09cb\u09a8 \u0995\u09ae\u09bf\u09b6\u09a8 \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.CUSTOMER_PAYS_DIRECTLY) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995 \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09b8\u09b0\u09be\u09b8\u09b0\u09bf \x0a\u09a8\u0997\u09a6 \u09ac\u09be UPI \u098f\u09b0 \u09ae\u09be\u09a7\u09cd\u09af\u09ae\u09c7 \u0985\u09b0\u09cd\u09a5 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09c7";
    };
    if (stringKey instanceof Language_Types.HUNDRED_PERCENT_FARE_GOES_TO_YOU) {
        return "\u09ad\u09be\u09a1\u09bc\u09be\u09b0 100%\x0a\u0986\u09aa\u09a8\u09be\u09b0 \u0995\u09be\u099b\u09c7 \u09af\u09be\u09ac\u09c7!";
    };
    if (stringKey instanceof Language_Types.FARE_SHOWN_IS_FARE_YOU_GET) {
        return "\u09a6\u09c7\u0996\u09be\u09a8\u09cb \u09ad\u09be\u09a1\u09bc\u09be \u09b9\u09b2 \u0986\u09aa\u09a8\u09bf \u09af\u09c7 \u09ad\u09be\u09a1\u09bc\u09be \u09aa\u09be\u09ac\u09c7\u09a8\u0964\x0a\u0995\u09cb\u09a8 \u09b2\u09c1\u0995\u09be\u09a8\u09cb \u099a\u09be\u09b0\u09cd\u099c \u09a8\u09c7\u0987\u0964";
    };
    if (stringKey instanceof Language_Types.BE_A_PART_OF_OPEN_MOBILITY_REVOLUTION) {
        return "\u0993\u09aa\u09c7\u09a8 \u09ae\u09cb\u09ac\u09bf\u09b2\u09bf\u099f\u09bf \u09b0\u09c7\u09ad\u09cb\u09b2\u09bf\u0989\u09b6\u09a8\u09c7\u09b0 \u0985\u0982\u09b6 \u09b9\u09cb\u09a8\x0a!";
    };
    if (stringKey instanceof Language_Types.OUR_DATA_AND_PRODUCT_ARE_TRANSPARENT) {
        return "\u0986\u09ae\u09be\u09a6\u09c7\u09b0 \u09a1\u09c7\u099f\u09be \u098f\u09ac\u0982 \u09aa\u09a3\u09cd\u09af\u09c7\u09b0 \u09b0\u09cb\u09a1\u09ae\u09cd\u09af\u09be\u09aa\x0a\u09b8\u0995\u09b2\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09cd\u09ac\u099a\u09cd\u099b\u09f7";
    };
    if (stringKey instanceof Language_Types.ENABLE_LOCATION_PERMISSION) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u0985\u09a8\u09c1\u09ae\u09a4\u09bf \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.PLEASE_ENABLE_LOCATION_PERMISSION_FOR) {
        return "\u098f\u09b0 \u099c\u09a8\u09cd\u09af \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u0985\u09a8\u09c1\u09ae\u09a4\u09bf \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENABLE_LOCATION) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u0995\u09cd\u09b7\u09ae \u0995\u09b0\u09c1\u09a8\u09f7";
    };
    if (stringKey instanceof Language_Types.YOUR_DETECTED_LOCATION_IS) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b8\u09a8\u09be\u0995\u09cd\u09a4 \u0995\u09b0\u09be \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b9\u09b2";
    };
    if (stringKey instanceof Language_Types.LANGUAGE_DETECTED) {
        return "\u09ad\u09be\u09b7\u09be \u09b6\u09a8\u09be\u0995\u09cd\u09a4 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.CHANGE_LANGUAGE_STR) {
        return "\u09ad\u09be\u09b7\u09be \u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SELECT_LOCATION) {
        return "\u09b8\u09cd\u09a5\u09be\u09a8 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SELECT_LOCATION_DESC) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u09c7\u0996\u09be\u09a8\u09c7 \u09b0\u09be\u0987\u09a1 \u0995\u09b0\u09a4\u09c7 \u099a\u09be\u09a8 \u09b8\u09c7\u099f\u09bf \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SELECT_LANGUAGE_DESC) {
        return "\u0986\u09aa\u09a8\u09bf \u09aa\u09a1\u09bc\u09a4\u09c7 \u09aa\u09be\u09b0\u09c7\u09a8 \u09ad\u09be\u09b7\u09be \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CONFIRM_LANGUAGE) {
        return "\u09ad\u09be\u09b7\u09be \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GET_STARTED) {
        return "\u098f\u09ac\u09be\u09b0 \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09be \u09af\u09be\u0995";
    };
    if (stringKey instanceof Language_Types.ENTER_YOUR_MOBILE_NUMBER) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_ACCESS) {
        return "\u09ac\u09bf\u099c\u09cd\u099e\u09aa\u09cd\u09a4\u09bf \u0985\u09cd\u09af\u09be\u0995\u09cd\u09b8\u09c7\u09b8";
    };
    if (stringKey instanceof Language_Types.NOTIFICATION_ACCESS_DESC) {
        return "\u09aa\u09b0\u09be\u09ae\u09b0\u09cd\u09b6 \u0995\u09b0\u09be \u09b9\u09af\u09bc, \u09af\u09be\u09a4\u09c7 \u0986\u09aa\u09a8\u09bf \u0995\u0996\u09a8\u0993 \u09ac\u09be\u09b0\u09cd\u09a4\u09be \u09b9\u09be\u09b0\u09bf\u09df\u09c7 \u09a8\u09be \u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.WATCH_VIDEO) {
        return "\u09ad\u09bf\u09a1\u09bf\u0993 \u09a6\u09c7\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DL_VERIFICATION_FAILED) {
        return "\u09a1\u09bf\u098f\u09b2 \u09af\u09be\u099a\u09be\u0987\u0995\u09b0\u09a3 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u09a6\u09bf\u09a8 \u09b8\u09ae\u09be\u09aa\u09cd\u09a4\u09bf \u09b2\u09bf\u0996\u09c1\u09a8 \u098f\u09ac\u0982 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RC_VERIFICATION_FAILED) {
        return "\u0986\u09b0\u09b8\u09bf \u09af\u09be\u099a\u09be\u0987\u0995\u09b0\u09a3 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7\u0964 \u09a6\u09bf\u09a8 \u09b8\u09ae\u09be\u09aa\u09cd\u09a4\u09bf \u09b2\u09bf\u0996\u09c1\u09a8 \u098f\u09ac\u0982 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DL_UPLOAD_FAILED) {
        return "\u09a1\u09bf\u098f\u09b2 \u0986\u09aa\u09b2\u09cb\u09a1 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.RC_UPLOAD_FAILED) {
        return "\u0986\u09b0\u09b8\u09bf \u0986\u09aa\u09b2\u09cb\u09a1 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.PLEASE_RETRY_THE_UPLOAD_AGAIN) {
        return "\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0986\u09aa\u09b2\u09cb\u09a1 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RC_AND_DL_UPLOAD_FAILED) {
        return "\u0986\u09b0\u09b8\u09bf \u098f\u09ac\u0982 \u09a1\u09bf\u098f\u09b2 \u0986\u09aa\u09b2\u09cb\u09a1 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.RC_UPLOAD_LIMIT_REACHED) {
        return "\u0986\u09b0\u09b8\u09bf \u0986\u09aa\u09b2\u09cb\u09a1 \u09b8\u09c0\u09ae\u09be \u0985\u09a4\u09bf\u0995\u09cd\u09b0\u09be\u09a8\u09cd\u09a4 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.DL_UPLOAD_LIMIT_REACHED) {
        return "\u09a1\u09bf\u098f\u09b2 \u0986\u09aa\u09b2\u09cb\u09a1 \u09b8\u09c0\u09ae\u09be \u0985\u09a4\u09bf\u0995\u09cd\u09b0\u09be\u09a8\u09cd\u09a4 \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.RETRY_UPLOAD) {
        return "\u0986\u09aa\u09b2\u09cb\u09a1 \u0986\u09ac\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.VEHICLE_REGISTERATON_CERTIFICATE) {
        return "\u09af\u09be\u09a8\u09ac\u09be\u09b9\u09a8 \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09a8 \u09b8\u09a8\u09a6\u09aa\u09a4\u09cd\u09b0";
    };
    if (stringKey instanceof Language_Types.GRANT_PERMISSIONS) {
        return "\u0985\u09a8\u09c1\u09ae\u09a4\u09bf \u09a6\u09bf\u09a8";
    };
    if (stringKey instanceof Language_Types.SUBSCRIPTION_PLAN_STR) {
        return "\u09b8\u09be\u09ac\u09b8\u09cd\u0995\u09cd\u09b0\u09bf\u09aa\u09b6\u09a8 \u09aa\u09cd\u09b2\u09cd\u09af\u09be\u09a8";
    };
    if (stringKey instanceof Language_Types.COMPLETE_AUTOPAY_LATER) {
        return "\u09aa\u09b0\u09c7 '\u0986\u09ae\u09be\u09b0 \u09aa\u09b0\u09bf\u0995\u09b2\u09cd\u09aa\u09a8\u09be' \u0985\u09a8\u09c1\u09ad\u09be\u0997 \u09a5\u09c7\u0995\u09c7 \u09b8\u09cd\u09ac\u09af\u09bc\u0982\u0995\u09cd\u09b0\u09bf\u09af\u09bc \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.START_EARNING_IN_FOUR_STEPS) {
        return "{} \u099f\u09bf \u09b8\u09b9\u099c \u09aa\u09a6\u0995\u09cd\u09b7\u09c7\u09aa\u09c7 \u0986\u09b0\u09cd\u099c\u09a8 \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.COMPLETE) {
        return "\u09b8\u09ae\u09be\u09aa\u09cd\u09a4";
    };
    if (stringKey instanceof Language_Types.HOW_TO_UPLOAD) {
        return "\u0995\u09bf\u09ad\u09be\u09ac\u09c7 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09ac\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.TAKE_CLEAR_PICTURE_DL) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8\u09c7\u09b0 \u09ab\u099f\u09cb \u09aa\u09be\u09b6\u09c7\u09b0 \u09aa\u09cd\u09b0\u09be\u09a8\u09cd\u09a4\u09c7 \u09aa\u09b0\u09bf\u09b7\u09cd\u0995\u09be\u09b0 \u099b\u09ac\u09bf \u09a4\u09c1\u09b2\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.ENSURE_ADEQUATE_LIGHT) {
        return "\u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u099c\u09a8\u09ae\u09a4\u09cb \u0986\u09b2\u09cb \u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u09b8\u09ae\u09b8\u09cd\u09a4 \u09ac\u09bf\u09b6\u09a6\u0997\u09c1\u09b2\u09bf \u09aa\u09b0\u09bf\u09b7\u09cd\u0995\u09be\u09b0\u09ad\u09be\u09ac\u09c7 \u09a6\u09c7\u0996\u09be \u09af\u09be\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.FIT_DL_CORRECTLY) {
        return "\u09a6\u09c7\u0996\u09be\u09a8\u09cb \u098f\u09b2\u09be\u0995\u09be\u09af\u09bc \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8\u099f\u09bf \u09b8\u09a0\u09bf\u0995\u09ad\u09be\u09ac\u09c7 \u09ab\u09bf\u099f \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TAKE_PHOTO) {
        return "\u099b\u09ac\u09bf \u09a4\u09cb\u09b2";
    };
    if (stringKey instanceof Language_Types.FIT_RC_CORRECTLY) {
        return "\u09a6\u09c7\u0996\u09be\u09a8\u09cb \u098f\u09b2\u09be\u0995\u09be\u09af\u09bc \u09b0\u09c7\u099c\u09bf\u09b8\u09cd\u099f\u09cd\u09b0\u09c7\u09b6\u09a8 \u09b8\u09a8\u09a6\u09aa\u09a4\u09cd\u09b0\u099f\u09bf \u09b8\u09a0\u09bf\u0995\u09ad\u09be\u09ac\u09c7 \u09ab\u09bf\u099f \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TAKE_CLEAR_PICTURE_RC) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u09b0\u09c7\u099c\u09bf\u09b8\u09cd\u099f\u09cd\u09b0\u09c7\u09b6\u09a8 \u09b8\u09a8\u09a6\u09aa\u09a4\u09cd\u09b0\u09c7\u09b0 \u09ab\u099f\u09cb \u09aa\u09be\u09b6\u09c7\u09b0 \u09aa\u09cd\u09b0\u09be\u09a8\u09cd\u09a4\u09c7 \u09aa\u09b0\u09bf\u09b7\u09cd\u0995\u09be\u09b0 \u099b\u09ac\u09bf \u09a4\u09c1\u09b2\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.DL_UPLOADED) {
        return "\u09a1\u09bf \u098f\u09b2 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.RC_UPLOADED) {
        return "\u0986\u09b0\u09b8\u09bf \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.DL_UPLOADING) {
        return "\u09a1\u09bf \u098f\u09b2 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09be \u09b9\u099a\u09cd\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.RC_UPLOADING) {
        return "\u0986\u09b0\u09b8\u09bf \u0986\u09aa\u09b2\u09cb\u09a1 \u09b9\u099a\u09cd\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.RETAKE_RC) {
        return "\x0a\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u0986\u09b0\u09b8\u09bf \u09b8\u09b9 \u099b\u09ac\u09bf \u09a8\u09a4\u09c1\u09a8 \u09a4\u09c1\u09b2\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RETAKE_DL) {
        return "\x0a\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09a1\u09bf \u098f\u09b2 \u09b8\u09b9 \u099b\u09ac\u09bf \u09a8\u09a4\u09c1\u09a8 \u09a4\u09c1\u09b2\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CONFIRM_AND_UPLOAD) {
        return "\u09a8\u09bf\u09b6\u09cd\u099a\u09bf\u09a4 \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RETAKE_PHOTO) {
        return "\u09ab\u099f\u09cb \u09a8\u09a4\u09c1\u09a8 \u09a4\u09c1\u09b2\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LETS_GET_YOU_TRIP_READY) {
        return "\u0986\u09b8\u09c1\u09a8 \u0986\u09aa\u09a8\u09bf \u099f\u09cd\u09b0\u09bf\u09aa \u09aa\u09cd\u09b0\u09b8\u09cd\u09a4\u09c1\u09a4 \u0995\u09b0\u09be \u09af\u09be\u0995!";
    };
    if (stringKey instanceof Language_Types.GOT_AN_OTP) {
        return "\u098f\u0995\u099f\u09bf OTP \u09aa\u09c7\u09af\u09bc\u09c7\u099b\u09c7\u09a8?";
    };
    if (stringKey instanceof Language_Types.DRIVING_LICENSE_DETAILS) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09bf\u0982 \u09b2\u09be\u0987\u09b8\u09c7\u09a8\u09cd\u09b8\u09c7\u09b0 \u09ac\u09bf\u09b6\u09a6 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.VEHICLE_REGISTRATION_DETAILS) {
        return "\u0997\u09be\u09a1\u09bc\u09bf\u09b0 \u09b0\u09c7\u099c\u09bf\u09b8\u09cd\u099f\u09cd\u09b0\u09c7\u09b6\u09a8\u09c7\u09b0 \u09ac\u09bf\u09ac\u09b0\u09a3";
    };
    if (stringKey instanceof Language_Types.UPLOAD_PHOTO) {
        return "\u09ab\u099f\u09cb \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CLEAR_IMAGE) {
        return "\u09aa\u09b0\u09bf\u09b7\u09cd\u0995\u09be\u09b0 \u099b\u09ac\u09bf";
    };
    if (stringKey instanceof Language_Types.BLURRY_IMAGE) {
        return "\u0985\u09b8\u09cd\u09aa\u09b7\u09cd\u099f \u099b\u09ac\u09bf";
    };
    if (stringKey instanceof Language_Types.CROPPED_CORRECTLY) {
        return "\u09b8\u09a0\u09bf\u0995\u09ad\u09be\u09ac\u09c7 \u0995\u09cd\u09b0\u09aa \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.WRONG_CROPPING) {
        return "\u09ad\u09c1\u09b2 \u0995\u09cd\u09b0\u09aa\u09bf\u0982";
    };
    if (stringKey instanceof Language_Types.CHANGE_LOCATION) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.CHANGE_CITY) {
        return "\u09b6\u09b9\u09b0 \u09aa\u09b0\u09bf\u09ac\u09b0\u09cd\u09a4\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.UPLOAD_REGISTRATION_CERTIFICATE_STR) {
        return "\u09b0\u09c7\u099c\u09bf\u09b8\u09cd\u099f\u09cd\u09b0\u09c7\u09b6\u09a8 \u09b8\u09be\u09b0\u09cd\u099f\u09bf\u09ab\u09bf\u0995\u09c7\u099f \u0986\u09aa\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.TAKE_A_PHOTO) {
        return "\u098f\u0995\u099f\u09bf \u099b\u09ac\u09bf \u09a4\u09c1\u09b2\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.GALLERY) {
        return "\u0997\u09cd\u09af\u09be\u09b2\u09be\u09b0\u09bf";
    };
    if (stringKey instanceof Language_Types.GET_FULL_PAYMENT) {
        return "\u0997\u09cd\u09b0\u09be\u09b9\u0995 \u09a5\u09c7\u0995\u09c7 \u09b8\u09b0\u09be\u09b8\u09b0\u09bf \u098f\u09ac\u0982 \u09aa\u09c2\u09b0\u09cd\u09a3 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u0997\u09cd\u09b0\u09b9\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.SELECT_CITY_STR) {
        return "\u09b6\u09b9\u09b0 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DETECTING_LOCATION) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u09a8\u09be\u0995\u09cd\u09a4 \u0995\u09b0\u09be \u09b9\u099a\u09cd\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.UNABLE_TO_DETECT_YOUR_LOCATION) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09b8\u09a8\u09be\u0995\u09cd\u09a4 \u0995\u09b0\u09a4\u09c7 \u0985\u0995\u09cd\u09b7\u09ae";
    };
    if (stringKey instanceof Language_Types.RC_FAILED_DESC) {
        return "\u0995\u09bf\u099b\u09c1 \u09b8\u09ae\u09af\u09bc \u09aa\u09b0\u09c7 RC \u09af\u09cb\u0997 \u0995\u09b0\u09be\u09b0 \u099a\u09c7\u09b7\u09cd\u099f\u09be \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RC_IN_PROGRESS_DESC) {
        return "\u09af\u09be\u099a\u09be\u0987 \u09b9\u09af\u09bc\u09c7 \u0997\u09c7\u09b2\u09c7 RC \u0986\u09aa\u09a8\u09be\u09b0 \u09aa\u09cd\u09b0\u09cb\u09ab\u09be\u0987\u09b2\u09c7 \u09af\u09cb\u0997 \u0995\u09b0\u09be \u09b9\u09ac\u09c7";
    };
    if (stringKey instanceof Language_Types.RC_VERIFICATION_SUCCESS) {
        return "\u0986\u09b0\u09b8\u09bf \u09af\u09be\u099a\u09be\u0987 \u09b8\u09ab\u09b2!";
    };
    if (stringKey instanceof Language_Types.RC_VERIFICATION_FAILED_STATUS) {
        return "\u0986\u09b0\u09b8\u09bf \u09af\u09be\u099a\u09be\u0987 \u09ac\u09cd\u09af\u09b0\u09cd\u09a5!";
    };
    if (stringKey instanceof Language_Types.RC_VERIFICATION_IN_PROGRESS) {
        return "RC \u09af\u09be\u099a\u09be\u0987\u0995\u09b0\u09a3 \u099a\u09b2\u099b\u09c7...";
    };
    if (stringKey instanceof Language_Types.WE_ARE_NOT_LIVE_IN_YOUR_AREA) {
        return "\u0986\u09ae\u09b0\u09be \u098f\u0996\u09a8\u0993 \u0986\u09aa\u09a8\u09be\u09b0 \u098f\u09b2\u09be\u0995\u09be\u09af\u09bc \u09a5\u09be\u0995\u09bf \u09a8\u09be!\x0a\u09b6\u09c0\u0998\u09cd\u09b0\u0987 \u0986\u09aa\u09a8\u09be\u09b0 \u09b6\u09b9\u09b0\u09c7 \u0986\u09b8\u099b\u09bf!";
    };
    if (stringKey instanceof Language_Types.LOCATION_UNSERVICEABLE) {
        return "\u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u0985\u09aa\u09cd\u09b0\u099a\u09b2\u09bf\u09a4";
    };
    if (stringKey instanceof Language_Types.UNABLE_TO_GET_YOUR_LOCATION) {
        return "\u0986\u09aa\u09a8\u09be\u09b0 \u0985\u09ac\u09b8\u09cd\u09a5\u09be\u09a8 \u09aa\u09c7\u09a4\u09c7 \u0985\u0995\u09cd\u09b7\u09ae!";
    };
    if (stringKey instanceof Language_Types.TURN_OFF_ANY_MOCK_LOCATION_APP_AND_RESTART) {
        return "\u0986\u09aa\u09a8\u09bf \u09af\u09c7 \u0995\u09cb\u09a8\u0993 \u09ae\u0995 \u09b2\u09cb\u0995\u09c7\u09b6\u09a8 \u0985\u09cd\u09af\u09be\u09aa \u09ac\u09cd\u09af\u09ac\u09b9\u09be\u09b0 \u0995\u09b0\u099b\u09c7\u09a8 \u09a4\u09be \u09ac\u09a8\u09cd\u09a7 \u0995\u09b0\u09c1\u09a8 \u098f\u09ac\u0982 \u0985\u09cd\u09af\u09be\u09aa\u099f\u09bf \u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u099a\u09be\u09b2\u09c1 \u0995\u09b0\u09c1\u09a8\u0964";
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
    if (stringKey instanceof Language_Types.CHOOSE_LANGUAGE) {
        return "";
    };
    if (stringKey instanceof Language_Types.DEMO) {
        return "";
    };
    if (stringKey instanceof Language_Types.CORPORATE_ADDRESS) {
        return "";
    };
    if (stringKey instanceof Language_Types.CORPORATE_ADDRESS_DESCRIPTION) {
        return "";
    };
    if (stringKey instanceof Language_Types.CORPORATE_ADDRESS_DESCRIPTION_ADDITIONAL) {
        return "";
    };
    if (stringKey instanceof Language_Types.REGISTERED_ADDRESS_DESCRIPTION_ADDITIONAL) {
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
        return "\u09aa\u09c1\u09a8\u09b0\u09be\u09af\u09bc \u099a\u09be\u09b2\u09c1 \u09b8\u09cd\u09ac\u09a4\u0983\u09aa\u09c7";
    };
    if (stringKey instanceof Language_Types.ONETIME) {
        return "";
    };
    if (stringKey instanceof Language_Types.FORTNIGHTLY) {
        return "";
    };
    if (stringKey instanceof Language_Types.MONTHLY) {
        return "";
    };
    if (stringKey instanceof Language_Types.BIMONTHLY) {
        return "";
    };
    if (stringKey instanceof Language_Types.QUARTERLY) {
        return "";
    };
    if (stringKey instanceof Language_Types.HALFYEARLY) {
        return "";
    };
    if (stringKey instanceof Language_Types.YEARLY) {
        return "";
    };
    if (stringKey instanceof Language_Types.ASPRESENTED) {
        return "";
    };
    if (stringKey instanceof Language_Types.FIRST_FREE_RIDE) {
        return "";
    };
    if (stringKey instanceof Language_Types.DAILY_PER_RIDE_DESC) {
        return "";
    };
    if (stringKey instanceof Language_Types.JOIN_THE_UNLIMITED_PLAN) {
        return "";
    };
    if (stringKey instanceof Language_Types.MAYBE_LATER) {
        return "";
    };
    if (stringKey instanceof Language_Types.YOUR_PAYMENT_WAS_UNSUCCESSFUL) {
        return "";
    };
    if (stringKey instanceof Language_Types.JOIN_A_PLAN_TO_START_EARNING) {
        return "";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE_PROMPT_SUBSCRIBE) {
        return "";
    };
    if (stringKey instanceof Language_Types.GO_ONLINE_PROMPT_PAYMENT_PENDING) {
        return "";
    };
    if (stringKey instanceof Language_Types.SCHEDULED) {
        return "";
    };
    if (stringKey instanceof Language_Types.ONE_TIME_SETTLEMENT) {
        return "\u098f\u0995\u0995\u09be\u09b2\u09c0\u09a8 \u09aa\u09c7\u09ae\u09c7\u09a8\u09cd\u099f \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.RETRY_AUTOPAY) {
        return "";
    };
    if (stringKey instanceof Language_Types.RETRY_STR) {
        return "";
    };
    if (stringKey instanceof Language_Types.ONGOING_PAYMENT_EXECUTION) {
        return "";
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
    if (stringKey instanceof Language_Types.BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR) {
        return "";
    };
    if (stringKey instanceof Language_Types.THIS_EXTRA_AMOUNT_THE_CUSTOMER_WILL_PAY) {
        return "\u098f\u0987 \u0985\u09a4\u09bf\u09b0\u09bf\u0995\u09cd\u09a4 \u09aa\u09b0\u09bf\u09ae\u09be\u09a3 \u0997\u09cd\u09b0\u09be\u09b9\u0995 \u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09aa\u09cd\u09b0\u09a6\u09be\u09a8 \u0995\u09b0\u09ac\u09c7 \u0995\u09be\u09b0\u09a3 \u0986\u09aa\u09a8\u09bf {} \u09ae\u09bf\u09a8\u09bf\u099f\u09c7\u09b0 \u09ac\u09c7\u09b6\u09bf \u0985\u09aa\u09c7\u0995\u09cd\u09b7\u09be \u0995\u09b0\u09c7\u099b\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.TEN_DIGIT_MOBILE_NUMBER) {
        return "10-\u09b8\u0982\u0996\u09cd\u09af\u09be\u09b0 \u09ae\u09cb\u09ac\u09be\u0987\u09b2 \u09a8\u09ae\u09cd\u09ac\u09b0";
    };
    if (stringKey instanceof Language_Types.BOOTH_CHARGES) {
        return "\u09ac\u09c1\u09a5 \u099a\u09be\u09b0\u09cd\u099c";
    };
    if (stringKey instanceof Language_Types.BOOTH_CHARGES_INCLUDED) {
        return "\u09ac\u09c1\u09a5 \u099a\u09be\u09b0\u09cd\u099c \u0985\u09a8\u09cd\u09a4\u09b0\u09cd\u09ad\u09c1\u0995\u09cd\u09a4: \u20b9{}";
    };
    if (stringKey instanceof Language_Types.TOTAL_AMOUNT) {
        return "\u09ae\u09cb\u099f \u09aa\u09b0\u09bf\u09ae\u09be\u09a3";
    };
    if (stringKey instanceof Language_Types.PLEASE_ADD_RC) {
        return "\u0985\u09a8\u09c1\u0997\u09cd\u09b0\u09b9 \u0995\u09b0\u09c7 \u09b0\u09be\u0987\u09a1 \u09a8\u09bf\u09a4\u09c7 RC \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.LOCATION_CANNOT_BE_ADDED_WHILE_GOTO_ACTIVE) {
        return "\u0997\u09cb-\u099f\u09c1 \u09b8\u0995\u09cd\u09b7\u09ae \u09a8\u09be \u09a5\u09be\u0995\u09b2\u09c7\u0987 \u09ac\u09bf\u0995\u09b2\u09cd\u09aa \u0989\u09aa\u09b2\u09ac\u09cd\u09a7";
    };
    if (stringKey instanceof Language_Types.LOCATION_CANNOT_BE_ADDED_WHILE_ON_RIDE) {
        return "\u0985\u09aa\u09b6\u09a8 \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09af\u09bc \u09a8\u09be \u09af\u0996\u09a8 \u09b0\u09be\u0987\u09a1 \u099a\u09b2\u099b\u09c7";
    };
    if (stringKey instanceof Language_Types.ADD_GOTO) {
        return "\u098f\u09a1 \u0997\u09cb-\u099f\u09c1";
    };
    if (stringKey instanceof Language_Types.NO_OPEN_MARKET_RIDES) {
        return "\u09b0\u09be\u0987\u09a1 \u09a8\u09c7\u0987";
    };
    if (stringKey instanceof Language_Types.ACCOUNT_BLOCKED) {
        return "\u0985\u09cd\u09af\u09be\u0995\u09be\u0989\u09a8\u09cd\u099f \u0985\u09ac\u09b0\u09c1\u09a6\u09cd\u09a7!";
    };
    if (stringKey instanceof Language_Types.YOU_HAVE_BEEN_BLOCKED_FROM_TAKING_RIDES) {
        return "\u0986\u09aa\u09a8\u09be\u0995\u09c7 \u09b0\u09be\u0987\u09a1 \u0995\u09b0\u09be \u09a5\u09c7\u0995\u09c7 \u09ac\u09cd\u09b2\u0995 \u0995\u09b0\u09be \u09b9\u09af\u09bc\u09c7\u099b\u09c7\u0964\x0a\u09a6\u09af\u09bc\u09be \u0995\u09b0\u09c7 \u09b8\u09be\u09b9\u09be\u09af\u09cd\u09af\u09c7\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09b9\u09be\u09af\u09bc\u09a4\u09be\u09b0 \u09b8\u09be\u09a5\u09c7 \u09af\u09cb\u0997\u09be\u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8\u0964";
    };
    if (stringKey instanceof Language_Types.DISMISS) {
        return "\u0996\u09be\u09b0\u09bf\u099c";
    };
    if (stringKey instanceof Language_Types.DRIVER_REFERRAL_CODE) {
        return "\u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.APP_QR_CODE) {
        return "\u0985\u09cd\u09af\u09be\u09aa \u0995\u09bf\u0989\u0986\u09b0 \u0995\u09cb\u09a1";
    };
    if (stringKey instanceof Language_Types.START_TAKING_RIDES_AND_REFER) {
        return "\u09a8\u09ae\u09cd\u09ae\u09be \u09af\u09be\u09a4\u09cd\u09b0\u09c0 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0 \u0985\u09cd\u09af\u09be\u09aa\u09c7 \u09b8\u09be\u0987\u09a8 \u0986\u09aa \u0995\u09b0\u09a4\u09c7 \u09b0\u09be\u0987\u09a1 \u0995\u09b0\u09be \u098f\u09ac\u0982 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0\u09a6\u09c7\u09b0 \u09b0\u09c7\u09ab\u09be\u09b0 \u0995\u09b0\u09be \u09b6\u09c1\u09b0\u09c1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.REFERRED_DRIVERS) {
        return "\u0989\u09b2\u09cd\u09b2\u09c7\u0996\u09bf\u09a4 \u09a1\u09cd\u09b0\u09be\u0987\u09ad\u09be\u09b0";
    };
    if (stringKey instanceof Language_Types.RIDE_LEADERBOARD) {
        return "\u09b0\u09be\u0987\u09a1 \u09b2\u09bf\u09a1\u09be\u09b0\u09ac\u09cb\u09b0\u09cd\u09a1";
    };
    if (stringKey instanceof Language_Types.YOUR_RANK) {
        return "\u09a4\u09cb\u09ae\u09be\u09b0 \u09aa\u09a6";
    };
    if (stringKey instanceof Language_Types.NOT_AVAILABLE_YET) {
        return "\u09a8\u09be \u09aa\u09be\u0993\u09af\u09bc\u09be \u098f\u0996\u09a8\u09cb";
    };
    if (stringKey instanceof Language_Types.ENTER_REFERRAL_CODE) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.HAVE_A_REFERRAL_CODE) {
        return "\u098f\u0995\u099f\u09bf \u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1 \u0986\u099b\u09c7?";
    };
    if (stringKey instanceof Language_Types.COMPLETE_STEPS_TO_APPLY_REFERRAL) {
        return "\u09b0\u09c7\u09ab\u09be\u09b0\u09c7\u09b2 \u0995\u09cb\u09a1 \u09aa\u09cd\u09b0\u09af\u09bc\u09cb\u0997 \u0995\u09b0\u09a4\u09c7 \u0989\u09aa\u09b0\u09c7\u09b0 \u09a7\u09be\u09aa\u0997\u09c1\u09b2\u09bf \u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.DOWNLOAD_NAMMA_YATRI) {
        return "\u09a8\u09ae\u09cd\u09ae\u09be \u09af\u09be\u09a4\u09cd\u09b0\u09c0 \u09a1\u09be\u0989\u09a8\u09b2\u09cb\u09a1 \u0995\u09b0\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_CODE) {
        return "\u0995\u09cb\u09a1 \u09b2\u09bf\u0996\u09c1\u09a8";
    };
    if (stringKey instanceof Language_Types.COMPLETE_REGISTRATION) {
        return "\u09b8\u09ae\u09cd\u09aa\u09c2\u09b0\u09cd\u09a3 \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09a8";
    };
    if (stringKey instanceof Language_Types.RENTAL_FARE) {
        return "\u09ad\u09be\u09a1\u09bc\u09be \u09ad\u09be\u09a1\u09bc\u09be";
    };
    if (stringKey instanceof Language_Types.DURATION) {
        return "\u09b8\u09ae\u09af\u09bc\u0995\u09be\u09b2";
    };
    if (stringKey instanceof Language_Types.START_ODO_READING) {
        return "\u09b6\u09c1\u09b0\u09c1\u09b0 \u0993\u09a1\u09cb \u09aa\u09a0\u09a8";
    };
    if (stringKey instanceof Language_Types.START_TIME) {
        return "\u09b6\u09c1\u09b0\u09c1\u09b0 \u09b8\u09ae\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.RIDE_TIME) {
        return "\u09b0\u09be\u0987\u09a1 \u09b8\u09ae\u09af\u09bc";
    };
    if (stringKey instanceof Language_Types.RIDE_START) {
        return "\u09af\u09be\u09a4\u09cd\u09b0\u09be \u09b6\u09c1\u09b0\u09c1";
    };
    if (stringKey instanceof Language_Types.RIDE_END) {
        return "\u09b0\u09be\u0987\u09a1 \u09b6\u09c7\u09b7";
    };
    if (stringKey instanceof Language_Types.RIDE_STARTED_AT) {
        return "\u09af\u09be\u09a4\u09cd\u09b0\u09be \u09b6\u09c1\u09b0\u09c1 \u09b9\u09b2";
    };
    if (stringKey instanceof Language_Types.RIDE_ENDED_AT) {
        return "\u09b0\u09be\u0987\u09a1 \u09b6\u09c7\u09b7 \u09b9\u09af\u09bc\u09c7\u099b\u09c7 \u098f";
    };
    if (stringKey instanceof Language_Types.ODOMETER_READING) {
        return "\u09a6\u09c2\u09b0\u09a4\u09cd\u09ac\u09ae\u09be\u09aa\u09a3\u09c0 \u09aa\u09a1\u09bc\u09be";
    };
    if (stringKey instanceof Language_Types.PICKED_UP_AT) {
        return "\u09aa\u09bf\u0995 \u0986\u09aa \u09b8\u09ae\u09af\u09bc \u09b9\u09b2";
    };
    if (stringKey instanceof Language_Types.UPCOMING_STOP) {
        return "\u0986\u09b8\u09a8\u09cd\u09a8 \u09b8\u09cd\u099f\u09aa";
    };
    if (stringKey instanceof Language_Types.LAST_STOP) {
        return "\u09b6\u09c7\u09b7 \u09b8\u09cd\u099f\u09aa";
    };
    if (stringKey instanceof Language_Types.YOU_ARE_ON_A_RENTAL_RIDE) {
        return "\u0986\u09aa\u09a8\u09bf \u098f\u0995\u099f\u09bf \u09ad\u09be\u09a1\u09bc\u09be \u09b0\u09be\u0987\u09a1\u09c7 \u0986\u099b\u09c7\u09a8";
    };
    if (stringKey instanceof Language_Types.ENTER_END_RIDE_OTP) {
        return "\u09b6\u09c7\u09b7 \u09b0\u09be\u0987\u09a1 \u0993\u099f\u09bf\u09aa\u09bf \u09aa\u09cd\u09b0\u09ac\u09c7\u09b6 \u0995\u09b0\u09c1\u09a8";
    };
    throw new Error("Failed pattern match at Resources.Localizable.BN (line 7, column 5 - line 1127, column 59): " + [ stringKey.constructor.name ]);
};
export {
    getBN
};
