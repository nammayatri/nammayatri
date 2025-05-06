package `in`.juspay.mobility.sdk.hypersmshandler

import android.content.Intent
import android.os.Bundle
import androidx.annotation.Keep

@Keep
interface SmsEventInterface {
    // Used on onReceive of SmsReceiver
    fun onSmsReceiverEvent(data: String)

    // Used on onReceive of SmsConsent or attach of SmsReceiver
    fun onSmsConsentEvent(intent: Intent, requestCode: Int, bundle: Bundle?)

    // Used on onActivityResult on SmsReceiver(SmsConsentFlow)
    fun onActivityResultEvent(result: String)

    // Used on onReceive of SentReceiver
    fun onSentReceiverEvent(resultCode: Int)

    // Used on onReceiver, attach, execute of SmsRetriever
    fun onSmsRetrieverEvent(event: RetrieverEvents, data: String)

    @Keep
    enum class RetrieverEvents {
        ON_ATTACH,
        ON_RECEIVE,
        ON_EXECUTE
    }
}
