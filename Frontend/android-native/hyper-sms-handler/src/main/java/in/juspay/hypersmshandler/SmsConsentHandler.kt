package `in`.juspay.hypersmshandler

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.Build
import com.google.android.gms.auth.api.phone.SmsRetriever
import com.google.android.gms.common.api.CommonStatusCodes
import com.google.android.gms.common.api.Status
import java.util.concurrent.Executors

abstract class SmsConsentHandler(private val smsComponents: SmsComponents) : BroadcastReceiver() {
    var consentIntent: Intent? = null
        private set
    private val context: Context = smsComponents.context
    private var intentReceivedCallback: Runnable? = null

    init {
        smsConsentPool.execute {
            startListener()
            val intentFilter = IntentFilter(SmsRetriever.SMS_RETRIEVED_ACTION)
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                context.registerReceiver(this, intentFilter, Context.RECEIVER_EXPORTED)
            } else {
                @Suppress("UnspecifiedRegisterReceiverFlag")
                context.registerReceiver(this, intentFilter)
            }
        }
    }

    private fun startListener() {
        val tracker = smsComponents.tracker
        val task = SmsRetriever.getClient(context).startSmsUserConsent(null)
        task.addOnSuccessListener {
            tracker.trackAction(
                Constants.LogSubCategory.SYSTEM,
                Constants.LogLevel.DEBUG,
                Constants.Labels.SMS_CONSENT,
                "sms_consent_listener",
                "SmsConsent listener started successfully"
            )
        }
        task.addOnFailureListener { e: Exception ->
            tracker.trackAndLogException(
                LOG_TAG,
                Constants.LogCategory.API_CALL,
                Constants.LogSubCategory.SDK,
                Constants.Labels.SMS_CONSENT,
                "SmsConsent listener failed to start",
                e
            )
        }
    }

    override fun onReceive(context: Context, intent: Intent) {
        if (SmsRetriever.SMS_RETRIEVED_ACTION != intent.action) {
            return
        }
        val extras = intent.extras ?: return
        val status = extras[SmsRetriever.EXTRA_STATUS] as Status?
        when (status?.statusCode ?: CommonStatusCodes.CANCELED) {
            CommonStatusCodes.SUCCESS -> {
                consentIntent = extras.getParcelable(SmsRetriever.EXTRA_CONSENT_INTENT)
                intentReceivedCallback?.run()
            }

            CommonStatusCodes.TIMEOUT -> {
//              Adding reset consent here to restart the listener.
//              Consent listener has a timeout of five minutes,
//              Which is started in initiate, if micro-app is opened
//              after consent has timed out it will not get the consent dialog
                resetConsentHandler()
            }
        }
    }

    abstract fun resetConsentHandler()
    fun unregisterConsent() {
        smsConsentPool.execute {
            try {
                context.unregisterReceiver(this)
            } catch (ignored: Exception) {
            }
        }
    }

    fun setIntentReceivedCallback(intentReceivedCallback: Runnable?) {
        this.intentReceivedCallback = intentReceivedCallback
    }

    companion object {
        private const val LOG_TAG = "SmsConsentHandler"
        private val smsConsentPool = Executors.newSingleThreadExecutor()
    }
}
