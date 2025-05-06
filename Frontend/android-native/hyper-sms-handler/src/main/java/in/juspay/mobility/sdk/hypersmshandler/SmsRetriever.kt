package `in`.juspay.mobility.sdk.hypersmshandler

import android.app.Activity
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.Build
import com.google.android.gms.auth.api.phone.SmsRetriever
import com.google.android.gms.common.api.CommonStatusCodes
import com.google.android.gms.common.api.Status
import org.json.JSONArray
import org.json.JSONObject

internal class SmsRetriever(
    private val smsComponents: SmsComponents
) : BroadcastReceiver(), JuspayDuiHook {
    private var otp: JSONArray = JSONArray()
    private var hasTimedOut = false

    override fun onReceive(context: Context, intent: Intent) {
        if (SmsRetriever.SMS_RETRIEVED_ACTION == intent.action) {
            val extras = intent.extras ?: return
            val status = extras[SmsRetriever.EXTRA_STATUS] as Status?
            when (status?.statusCode ?: CommonStatusCodes.CANCELED) {
                CommonStatusCodes.SUCCESS -> {
                    val message = extras[SmsRetriever.EXTRA_SMS_MESSAGE] as String?
                    val msg = JSONObject()
                    try {
                        msg.put(
                            "from",
                            "UNKNOWN_BANK"
                        ) // User consent API will not give the sender name
                        msg.put("body", message)
                        msg.put("time", System.currentTimeMillis().toString())
                    } catch (_: Exception) {
                    }
                    otp.put(msg)
                    smsComponents.smsEventInterface.onSmsRetrieverEvent(
                        SmsEventInterface.RetrieverEvents.ON_RECEIVE,
                        otp.toString()
                    )
                    otp = JSONArray()
                }

                CommonStatusCodes.TIMEOUT -> {
                    hasTimedOut = true
                    smsComponents.smsEventInterface.onSmsRetrieverEvent(
                        SmsEventInterface.RetrieverEvents.ON_RECEIVE,
                        "TIMEOUT"
                    )
                }
            }
        }
    }

    override fun attach(activity: Activity) {
        val client = SmsRetriever.getClient(
            smsComponents.context
        )
        val task = client.startSmsRetriever()
        task.addOnSuccessListener {
            val filter = IntentFilter(SmsRetriever.SMS_RETRIEVED_ACTION)
            filter.addAction(Intent.ACTION_AIRPLANE_MODE_CHANGED)
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                activity.registerReceiver(this, filter, Context.RECEIVER_EXPORTED)
            } else {
                @Suppress("UnspecifiedRegisterReceiverFlag")
                activity.registerReceiver(this, filter)
            }
            smsComponents.smsEventInterface
                .onSmsRetrieverEvent(SmsEventInterface.RetrieverEvents.ON_ATTACH, "SUCCESS")
        }
        task.addOnFailureListener {
            smsComponents.smsEventInterface
                .onSmsRetrieverEvent(SmsEventInterface.RetrieverEvents.ON_ATTACH, "FAILURE")
        }
    }

    override fun execute(activity: Activity, operation: String?, args: JSONObject?): String {
        if (operation != null) {
            when (operation) {
                "getOtp" -> {
                    if (otp.length() != 0) {
                        smsComponents.smsEventInterface.onSmsRetrieverEvent(
                            SmsEventInterface.RetrieverEvents.ON_EXECUTE,
                            otp.toString()
                        )
                        otp = JSONArray()
                        return "SUCCESS"
                    } else if (hasTimedOut) {
                        smsComponents.smsEventInterface.onSmsRetrieverEvent(
                            SmsEventInterface.RetrieverEvents.ON_EXECUTE,
                            "TIMEOUT"
                        )
                    }
                    return "SUCCESS"
                }

                "cancel" -> return "SUCCESS"
            }
        }
        return "FAILURE"
    }

    override fun detach(activity: Activity) {
        activity.unregisterReceiver(this)
    }
}
