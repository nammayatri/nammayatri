package `in`.juspay.mobility.sdk.hyper.constants

import androidx.annotation.Keep

@Keep
class LogSubCategory {
    @Keep
    object Action {
        const val SYSTEM = "system"
        const val USER = "user"
        const val DUI = "dynamic_ui"
    }

    @Keep
    object LifeCycle {
        const val HYPER_SDK = "hypersdk"
        const val ANDROID = "android"
    }

    @Keep
    object ApiCall {
        const val NETWORK = "network"
        const val SDK = "external_sdk"
    }

    @Keep
    object Context {
        const val DEVICE = "device"
    }
}
