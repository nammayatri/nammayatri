package `in`.juspay.hypersdk.lifecycle

import android.content.Intent
import android.content.IntentSender
import android.os.Bundle
import androidx.fragment.app.Fragment
import `in`.juspay.hyper.constants.Labels
import `in`.juspay.hyper.constants.LogCategory
import `in`.juspay.hyper.constants.LogSubCategory
import `in`.juspay.hypersdk.core.JuspayServices
import `in`.juspay.hypersdk.ui.IntentSenderDelegate
import java.util.Queue
import java.util.concurrent.ConcurrentLinkedQueue

internal class HyperIntentSenderDelegate(
    juspayServices: JuspayServices
) : IntentSenderDelegate {
    private val juspayServices: JuspayServices
    private val intentSenderQueue: Queue<IntentQueueData>

    init {
        this.juspayServices = juspayServices
        intentSenderQueue = ConcurrentLinkedQueue()
    }

    fun fragmentAttached() {
        for (data in intentSenderQueue) {
            startIntentSenderForResult(
                data.intentSender,
                data.requestCode,
                data.fillInIntent,
                data.flagMask,
                data.flagValues,
                data.extraFlags,
                data.options
            )
        }
    }

    override fun startIntentSenderForResult(
        intentSender: IntentSender,
        requestCode: Int,
        fillInIntent: Intent?,
        flagMask: Int,
        flagValues: Int,
        extraFlags: Int,
        options: Bundle?
    ) {
        try {
            val fragment: Fragment? = juspayServices.fragment
            if (fragment != null && fragment.isAdded) {
                fragment.startIntentSenderForResult(
                    intentSender,
                    requestCode,
                    fillInIntent,
                    flagMask,
                    flagValues,
                    extraFlags,
                    options
                )
            } else {
                intentSenderQueue.add(
                    IntentQueueData(
                        intentSender,
                        requestCode,
                        fillInIntent,
                        flagMask,
                        flagValues,
                        extraFlags,
                        options
                    )
                )
            }
        } catch (e: Exception) {
            juspayServices.sdkTracker.trackException(
                LogCategory.LIFECYCLE,
                LogSubCategory.LifeCycle.ANDROID,
                Labels.Android.START_INTENT_SENDER_FOR_RESULT,
                "Exception in startIntentSenderForResult",
                e
            )
        }
    }

    fun clearQueue() {
        intentSenderQueue.clear()
    }

    private data class IntentQueueData(
        val intentSender: IntentSender,
        val requestCode: Int,
        val fillInIntent: Intent?,
        val flagMask: Int,
        val flagValues: Int,
        val extraFlags: Int,
        val options: Bundle?
    )
}
