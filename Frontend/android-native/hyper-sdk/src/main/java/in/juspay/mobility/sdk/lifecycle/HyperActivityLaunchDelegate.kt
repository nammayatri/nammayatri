package `in`.juspay.mobility.sdk.lifecycle

import android.content.Intent
import android.os.Bundle
import androidx.fragment.app.Fragment
import `in`.juspay.mobility.sdk.core.JuspayServices
import `in`.juspay.mobility.sdk.ui.ActivityLaunchDelegate
import java.util.Queue
import java.util.concurrent.ConcurrentLinkedQueue

internal class HyperActivityLaunchDelegate(
    juspayServices: JuspayServices
) : ActivityLaunchDelegate {
    private val juspayServices: JuspayServices
    private val startActivityQueue: Queue<IntentQueueData>

    init {
        this.juspayServices = juspayServices
        startActivityQueue = ConcurrentLinkedQueue()
    }

    fun fragmentAttached() {
        for (data in startActivityQueue) {
            startActivityForResult(data.intent, data.requestCode, data.bundle)
        }
    }

    override fun startActivityForResult(intent: Intent, requestCode: Int, bundle: Bundle?) {
        val fragment: Fragment? = juspayServices.fragment
        if (fragment != null && fragment.isAdded) {
            fragment.startActivityForResult(intent, requestCode, bundle)
        } else {
            startActivityQueue.add(IntentQueueData(intent, requestCode, bundle))
        }
    }

    fun clearQueue() {
        startActivityQueue.clear()
    }

    private data class IntentQueueData(val intent: Intent, val requestCode: Int, val bundle: Bundle?)
}
