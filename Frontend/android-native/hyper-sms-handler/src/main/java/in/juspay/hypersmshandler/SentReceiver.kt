package `in`.juspay.hypersmshandler

import android.app.Activity
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.Build
import org.json.JSONObject

internal class SentReceiver(
    private val smsComponents: SmsComponents
) : BroadcastReceiver(), JuspayDuiHook {
    override fun onReceive(context: Context, intent: Intent) {
        smsComponents.smsEventInterface.onSentReceiverEvent(resultCode)
    }

    override fun attach(activity: Activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            activity.registerReceiver(this, IntentFilter("SMS_SENT"), Context.RECEIVER_EXPORTED)
        } else {
            @Suppress("UnspecifiedRegisterReceiverFlag")
            activity.registerReceiver(this, IntentFilter("SMS_SENT"))
        }
    }

    override fun execute(activity: Activity, operation: String?, args: JSONObject?): String? {
        return null
    }

    override fun detach(activity: Activity) {
        activity.unregisterReceiver(this)
    }
}
