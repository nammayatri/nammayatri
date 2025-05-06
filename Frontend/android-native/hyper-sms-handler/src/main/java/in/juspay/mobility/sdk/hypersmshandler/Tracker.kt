package `in`.juspay.mobility.sdk.hypersmshandler

import androidx.annotation.Keep

@Keep
interface Tracker {
    fun trackAction(subCategory: String, level: String, label: String, key: String?, value: Any)
    fun trackAndLogException(
        tag: String,
        category: String,
        subCategory: String,
        label: String,
        description: String,
        throwable: Throwable
    )
}
