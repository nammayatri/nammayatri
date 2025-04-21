package `in`.juspay.hypersmshandler

import android.app.Activity
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.Build
import android.widget.Toast
import org.json.JSONObject

internal class DeliverReceiver : BroadcastReceiver(), JuspayDuiHook {
    override fun onReceive(context: Context, intent: Intent) {
        when (resultCode) {
            Activity.RESULT_OK -> Toast.makeText(context, "SMS DELIVERED", Toast.LENGTH_SHORT)
                .show()

            Activity.RESULT_CANCELED -> Toast.makeText(
                context,
                "SMS NOT DELIVERED",
                Toast.LENGTH_SHORT
            ).show()
        }
    }

    override fun attach(activity: Activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            activity.registerReceiver(
                this,
                IntentFilter("SMS_DELIVERED"),
                Context.RECEIVER_EXPORTED
            )
        } else {
            @Suppress("UnspecifiedRegisterReceiverFlag")
            activity.registerReceiver(this, IntentFilter("SMS_DELIVERED"))
        }
    }

    override fun execute(activity: Activity, operation: String?, args: JSONObject?): String? {
        return null
    }

    override fun detach(activity: Activity) {
        activity.unregisterReceiver(this)
    }
}
