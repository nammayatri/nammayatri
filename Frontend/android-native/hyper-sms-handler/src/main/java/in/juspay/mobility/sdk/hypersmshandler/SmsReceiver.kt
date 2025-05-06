package `in`.juspay.mobility.sdk.hypersmshandler

import android.app.Activity
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.Build
import android.telephony.SmsMessage
import com.google.android.gms.auth.api.phone.SmsRetriever
import `in`.juspay.mobility.sdk.hypersmshandler.Constants
import org.json.JSONArray
import org.json.JSONException
import org.json.JSONObject
import java.util.Locale

internal class SmsReceiver(
    private val smsServices: SmsServices
) : BroadcastReceiver(), JuspayDuiHook, OnResultHook {
    private var interFilter: IntentFilter? = null
    private val tracker: Tracker = smsServices.smsComponents.tracker

    override fun onReceive(context: Context, intent: Intent) {
        try {
            if ("android.provider.Telephony.SMS_RECEIVED" == intent.action) {
                tryReceiveMessage(intent)
            }
        } catch (e: Exception) {
            tracker.trackAndLogException(
                LOG_TAG,
                Constants.LogCategory.ACTION,
                Constants.LogSubCategory.SYSTEM,
                Constants.Labels.BROADCAST_RECEIVER,
                "Failed to receive sms",
                e
            )
        }
    }

    @Suppress("unused")
    fun getMaskedMessage(message: String): String {
        return message.replace("[0-9]".toRegex(), "X")
    }

    @Throws(JSONException::class)
    private fun tryReceiveMessage(intent: Intent) {
        val bundle = intent.extras
        val msgs: Array<SmsMessage?>
        var msgFrom: String
        var msgBody: String
        var msgTime: String
        val receivedMsgs: JSONArray
        if (bundle != null) {
            val pdus = bundle["pdus"] as? Array<*> ?: arrayOf<ByteArray>()
            msgs = arrayOfNulls(pdus.size)
            receivedMsgs = JSONArray()
            for (i in msgs.indices) {
                val smsMsg = SmsMessage.createFromPdu(pdus[i] as ByteArray)
                val originatedAddress = smsMsg.originatingAddress
                msgFrom = originatedAddress?.uppercase(Locale.getDefault()) ?: ""
                msgBody = smsMsg.messageBody.uppercase(Locale.getDefault())
                msgTime = smsMsg.timestampMillis.toString()
                msgs[i] = smsMsg
                val msg = JSONObject()
                msg.put("from", msgFrom)
                msg.put("body", msgBody)
                msg.put("time", msgTime)
                receivedMsgs.put(msg)
            }
            if (receivedMsgs.length() > 0) {
                smsServices.smsComponents.smsEventInterface.onSmsReceiverEvent(receivedMsgs.toString())
            }
        }
    }

    fun setIntentFilter(iff: IntentFilter?) {
        interFilter = iff
    }

    override fun attach(activity: Activity) {
        if (interFilter == null) {
            val smsConsentHandler = smsServices.smsConsentHandler
            if (smsConsentHandler == null) {
                tracker.trackAction(
                    Constants.LogSubCategory.SYSTEM,
                    Constants.LogLevel.ERROR,
                    Constants.Labels.SMS_RECEIVER,
                    "missing",
                    "SmsConsentHandler"
                )
                return
            }
            checkAndLaunchConsentDialog(smsConsentHandler)
            smsConsentHandler.setIntentReceivedCallback(
                Runnable {
                    checkAndLaunchConsentDialog(
                        smsConsentHandler
                    )
                }
            )
        } else {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                smsServices.smsComponents.context.registerReceiver(
                    this,
                    interFilter,
                    Context.RECEIVER_EXPORTED
                )
            } else {
                @Suppress("UnspecifiedRegisterReceiverFlag")
                smsServices.smsComponents.context.registerReceiver(this, interFilter)
            }
        }
    }

    private fun checkAndLaunchConsentDialog(smsConsentHandler: SmsConsentHandler) {
        val intent = smsConsentHandler.consentIntent
        if (intent != null) {
            smsServices.smsComponents.smsEventInterface
                .onSmsConsentEvent(intent, SMS_CONSENT_REQUEST, null)
        }
    }

    override fun execute(activity: Activity, operation: String?, args: JSONObject?): String {
        try {
            return if (args != null && args.has("smsReadStartTime")) {
                smsServices.readSmsFromInbox(null, args.getString("smsReadStartTime"), null)
            } else {
                smsServices.readSmsFromInbox(
                    null,
                    (System.currentTimeMillis() - 60000).toString(),
                    null
                )
            }
        } catch (e: JSONException) {
            tracker.trackAndLogException(
                LOG_TAG,
                Constants.LogCategory.ACTION,
                Constants.LogSubCategory.SYSTEM,
                Constants.Labels.BROADCAST_RECEIVER,
                "Exception while trying to read sms from Inbox: ",
                e
            )
        }
        return "[]"
    }

    override fun detach(activity: Activity) {
        try {
            if (interFilter == null) {
                val smsConsentHandler = smsServices.smsConsentHandler
                smsConsentHandler?.setIntentReceivedCallback(null)
            } else {
                smsServices.smsComponents.context.unregisterReceiver(this)
            }
        } catch (ignored: Exception) {
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?): Boolean {
        if (requestCode == SMS_CONSENT_REQUEST) {
            smsServices.resetSmsConsentHandler()
            if (data == null) {
                smsServices.smsComponents.smsEventInterface
                    .onActivityResultEvent(SmsServices.CONSENT_DENIED)
                return true
            }
            when (resultCode) {
                Activity.RESULT_OK -> {
                    var message = data.getStringExtra(SmsRetriever.EXTRA_SMS_MESSAGE)
                    try {
                        val msg = JSONObject()
                        msg.put(
                            "from",
                            "UNKNOWN_BANK"
                        ) // User consent API will not give the sender name
                        msg.put("body", message)
                        msg.put("time", System.currentTimeMillis().toString())
                        message = msg.toString()
                        smsServices.smsComponents.smsEventInterface.onActivityResultEvent(message)
                        tracker.trackAction(
                            Constants.LogSubCategory.SYSTEM,
                            Constants.LogLevel.DEBUG,
                            Constants.Labels.BROADCAST_RECEIVER,
                            "on_activity_result",
                            "Response sent back to microapp"
                        )
                    } catch (e: JSONException) {
                        smsServices.smsComponents.smsEventInterface
                            .onActivityResultEvent(SmsServices.CONSENT_DENIED)
                        tracker.trackAndLogException(
                            LOG_TAG,
                            Constants.LogCategory.API_CALL,
                            Constants.LogSubCategory.SDK,
                            Constants.Labels.SMS_CONSENT,
                            "JSON Exception",
                            e
                        )
                    }
                }

                Activity.RESULT_CANCELED -> {
                    tracker.trackAction(
                        Constants.LogSubCategory.SYSTEM,
                        Constants.LogLevel.DEBUG,
                        Constants.Labels.BROADCAST_RECEIVER,
                        "on_activity_result",
                        "User denied SMS consent"
                    )
                    smsServices.smsComponents.smsEventInterface.onActivityResultEvent("DENIED")
                }
            }
            return true
        }
        return false
    }

    companion object {
        private val LOG_TAG = SmsReceiver::class.java.simpleName
        private const val SMS_CONSENT_REQUEST = 11
    }
}
