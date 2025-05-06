package `in`.juspay.mobility.sdk.hypersmshandler

import android.Manifest
import android.content.IntentFilter
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Build
import androidx.annotation.Keep
import androidx.core.content.PermissionChecker
import `in`.juspay.mobility.sdk.hypersmshandler.Constants
import org.json.JSONArray
import org.json.JSONException
import org.json.JSONObject

class SmsServices @Keep constructor(val smsComponents: SmsComponents) {
    var smsConsentHandler: SmsConsentHandler? = null
        private set

    @Keep
    fun createSMSConsent() {
        try {
            if (smsConsentHandler == null) {
                val packageManager = smsComponents.context.packageManager
                val packageName = "com.google.android.gms"
                if (packageManager.checkPermission(
                        Manifest.permission.READ_SMS,
                        packageName
                    ) == PackageManager.PERMISSION_GRANTED
                ) {
                    smsConsentHandler = object : SmsConsentHandler(smsComponents) {
                        override fun resetConsentHandler() {
                            resetSmsConsentHandler()
                        }
                    }
                    smsConsentHandler?.setIntentReceivedCallback(null)
                }
            }
        } catch (e: Exception) {
            smsComponents.tracker.trackAndLogException(
                LOG_TAG,
                Constants.LogCategory.LIFECYCLE,
                Constants.LogSubCategory.HYPER_SDK,
                Constants.Labels.SMS_CONSENT,
                "Exception happened while initializing",
                e
            )
        }
    }

    /**
     * Function used to restart consent handler.
     * This is to be called after consent dialog is hidden
     * and after timeout of consent api
     */
    fun resetSmsConsentHandler() {
        smsConsentHandler?.unregisterConsent()
        smsConsentHandler = object : SmsConsentHandler(smsComponents) {
            override fun resetConsentHandler() {
                resetSmsConsentHandler()
            }
        }
        smsConsentHandler?.setIntentReceivedCallback(null)
    }

    @Keep
    fun unregisterSmsConsent() {
        smsConsentHandler?.unregisterConsent()
        smsConsentHandler = null
    }

    @Keep
    fun fetchSms(query: String?, smsContentUri: String?, additionalQuery: String?): String {
        return readSmsFromInbox(smsContentUri, query, additionalQuery)
    }

    @Keep
    fun createSMSReceiver(): JuspayDuiHook? {
        try {
            val iff = IntentFilter()
            iff.addAction("android.provider.Telephony.SMS_RECEIVED")
            iff.priority = 999
            if (checkIfGranted(Manifest.permission.RECEIVE_SMS)) {
                val smsReceiver = SmsReceiver(this)
                smsReceiver.setIntentFilter(iff)
                return smsReceiver
            }
        } catch (e: Throwable) {
            smsComponents.tracker.trackAndLogException(
                LOG_TAG,
                Constants.LogCategory.ACTION,
                Constants.LogSubCategory.SYSTEM,
                Constants.Labels.SMS_RECEIVER,
                "Failed to register SMS broadcast receiver (Ignoring)",
                e
            )
        }
        return null
    }

    @Keep
    fun createSmsReceiverForConsent(): JuspayDuiHook? {
        try {
            val smsReceiver = SmsReceiver(this)
            smsReceiver.setIntentFilter(null)
            return smsReceiver
        } catch (e: Exception) {
            smsComponents.tracker.trackAndLogException(
                LOG_TAG,
                Constants.LogCategory.API_CALL,
                Constants.LogSubCategory.SDK,
                Constants.Labels.SMS_CONSENT,
                "Failed to register SMS Consent",
                e
            )
        }
        return null
    }

    @Keep
    fun createSendSMSReceiver(): JuspayDuiHook {
        return SentReceiver(smsComponents)
    }

    @Keep
    fun createSmsRetriever(): JuspayDuiHook {
        return SmsRetriever(smsComponents)
    }

    @Keep
    fun createDeliveredSMSReceiver(): JuspayDuiHook {
        return DeliverReceiver()
    }

    fun readSmsFromInbox(
        smsContentUri: String?,
        whereCondition: String?,
        additionalQuery: String?
    ): String {
        var smsContentUriFinal = smsContentUri
        var receivedMsgs: JSONArray? = null
        if (checkIfGranted(Manifest.permission.READ_SMS)) {
            if (smsContentUriFinal == null || smsContentUriFinal == "") {
                smsContentUriFinal = "inbox"
            }
            val uriSMSURI = Uri.parse("content://sms/$smsContentUriFinal")
            val reqCols = arrayOf("_id", "address", "body", "date", "status", "type")
            val sortOrder = "date DESC"
            val whereConditionStr = StringBuilder()
            val queryArgs: Array<String>
            val queryArgsList: MutableList<String> = ArrayList()
            try {
                val queries: JSONArray
                if (additionalQuery == null || additionalQuery == "") {
                    val defaultQuery = JSONObject()
                    defaultQuery.put("field", "date")
                    defaultQuery.put("operator", ">")
                    defaultQuery.put("selector", "?")
                    defaultQuery.put("value", whereCondition)
                    queries = JSONArray()
                    queries.put(defaultQuery)
                } else {
                    queries = JSONArray(additionalQuery)
                }
                for (q in 0 until queries.length()) {
                    val condition = queries.getJSONObject(q)
                    val field = condition.getString("field")
                    val operator = condition.getString("operator")
                    val selector = condition.getString("selector")
                    val value = condition.getString("value")
                    val next = condition.optString("next", "")
                    val separator = condition.optString("separator", ",")
                    var nextC = ""
                    if (q != queries.length()) {
                        nextC = " $next "
                    }
                    whereConditionStr.append(field).append(" ").append(operator).append(" ")
                        .append(selector)
                        .append(nextC)
                    for (`val` in value.split(separator.toRegex()).dropLastWhile { it.isEmpty() }
                        .toTypedArray()) {
                        queryArgsList.add(`val`.trim { it <= ' ' })
                    }
                }
            } catch (e: JSONException) {
                smsComponents.tracker.trackAndLogException(
                    LOG_TAG,
                    Constants.LogCategory.ACTION,
                    Constants.LogSubCategory.SYSTEM,
                    Constants.Labels.UTIL,
                    "ReadSmsFromInbox JSONException",
                    e
                )
            }
            queryArgs = queryArgsList.toTypedArray<String>()
            try {
                smsComponents.context.contentResolver.query(
                    uriSMSURI,
                    reqCols,
                    whereConditionStr.toString(),
                    queryArgs,
                    sortOrder
                ).use { cur ->
                    var from: String?
                    var body: String?
                    var time: String
                    var status: String?
                    var type: String?
                    receivedMsgs = if ((cur?.count ?: 0) > 0) JSONArray() else null
                    while (cur != null && cur.moveToNext()) {
                        from = cur.getString(1)
                        body = cur.getString(2)
                        time = cur.getLong(3).toString()
                        status = cur.getString(4)
                        type = cur.getString(5)
                        val msg = JSONObject()
                        msg.put("from", from)
                        msg.put("body", body)
                        msg.put("time", time)
                        msg.put("status", status)
                        msg.put("type", type)
                        receivedMsgs?.put(msg)
                    }
                }
            } catch (e: Exception) {
                smsComponents.tracker.trackAndLogException(
                    LOG_TAG,
                    Constants.LogCategory.ACTION,
                    Constants.LogSubCategory.SYSTEM,
                    Constants.Labels.UTIL,
                    "Exception while trying to read previous sms from Inbox",
                    e
                )
            }
        } else {
            smsComponents.tracker.trackAction(
                Constants.LogSubCategory.SYSTEM,
                Constants.LogLevel.ERROR,
                Constants.Labels.UTIL,
                "readsmsfrominbox",
                "No permission to read SMS"
            )
        }
        return if (receivedMsgs == null || receivedMsgs?.length() == 0) "[]" else receivedMsgs.toString()
    }

    private fun checkIfGranted(permission: String): Boolean {
        val context = smsComponents.context
        return if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
            PermissionChecker.checkSelfPermission(
                context,
                permission
            ) == PermissionChecker.PERMISSION_GRANTED
        } else {
            try {
                val packageManager = context.packageManager
                val packageName = context.packageName
                packageManager.checkPermission(
                    permission,
                    packageName
                ) == PackageManager.PERMISSION_GRANTED
            } catch (error: Throwable) {
                smsComponents.tracker.trackAndLogException(
                    LOG_TAG,
                    Constants.LogCategory.ACTION,
                    Constants.LogSubCategory.SYSTEM,
                    Constants.Labels.UTIL,
                    "Exception trying to fetch permission info: $permission returning FALSE",
                    error
                )
                false
            }
        }
    }

    companion object {
        var CONSENT_DENIED = "DENIED"
        private val LOG_TAG = SmsServices::class.java.simpleName
    }
}
