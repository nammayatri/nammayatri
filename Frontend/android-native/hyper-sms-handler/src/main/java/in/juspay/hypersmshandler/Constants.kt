package `in`.juspay.hypersmshandler

internal object Constants {
    internal object LogCategory {
        const val ACTION = "action"
        const val API_CALL = "api_call"
        const val LIFECYCLE = "lifecycle"
    }

    internal object LogSubCategory {
        const val SYSTEM = "system"
        const val SDK = "external_sdk"
        const val HYPER_SDK = "hyper_sdk"
    }

    internal object LogLevel {
        const val DEBUG = "debug"
        const val WARNING = "warning"
        const val ERROR = "error"
    }

    internal object Labels {
        const val SMS_RECEIVER = "sms_receiver"
        const val BROADCAST_RECEIVER = "broadcast_receiver"
        const val SMS_CONSENT = "sms_consent"
        const val UTIL = "utils"
    }
}
