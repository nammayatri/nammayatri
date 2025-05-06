package `in`.juspay.mobility.sdk.hyper.core

import org.json.JSONObject

interface TrackerInterface {
    // Add more tracker methods here as and when required
    fun trackAction(subCategory: String, level: String, label: String, key: String?, value: Any)
    fun trackAndLogException(
        tag: String,
        category: String,
        subCategory: String,
        label: String,
        description: String,
        throwable: Throwable
    )

    fun trackApiCalls(
        subCategory: String,
        level: String,
        label: String,
        statusCode: Int?,
        url: String?,
        startTime: Long?,
        endTime: Long?,
        payload: Any?,
        response: Any?,
        method: String?
    )

    fun trackLifecycle(subCategory: String, level: String, label: String, key: String?, value: Any?)
    fun track(logLine: JSONObject)
    fun addLogToPersistedQueue(logLine: JSONObject)
}
