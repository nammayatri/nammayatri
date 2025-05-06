package `in`.juspay.mobility.sdk.hypersmshandler

import android.content.Context
import androidx.annotation.Keep

@Keep
interface SmsComponents {
    val tracker: Tracker
    val smsEventInterface: SmsEventInterface
    val context: Context
}
