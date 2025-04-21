package `in`.juspay.hypersdk.lifecycle

import androidx.fragment.app.Fragment
import `in`.juspay.hypersdk.core.JuspayServices
import `in`.juspay.hypersdk.ui.RequestPermissionDelegate
import java.util.Queue
import java.util.concurrent.ConcurrentLinkedQueue

internal class HyperRequestPermissionDelegate(
    juspayServices: JuspayServices
) : RequestPermissionDelegate {
    private val juspayServices: JuspayServices
    private val requestQueue: Queue<RequestQueueData>

    init {
        this.juspayServices = juspayServices
        requestQueue = ConcurrentLinkedQueue()
    }

    fun fragmentAttached() {
        for (data in requestQueue) {
            requestPermission(data.requests, data.requestCode)
        }
    }

    override fun requestPermission(permissions: Array<String>, permissionId: Int) {
        val fragment: Fragment? = juspayServices.fragment
        if (fragment != null && fragment.isAdded) {
            fragment.requestPermissions(permissions, permissionId)
        } else {
            requestQueue.add(RequestQueueData(permissions, permissionId))
        }
    }

    fun clearQueue() {
        requestQueue.clear()
    }

    private class RequestQueueData(val requests: Array<String>, val requestCode: Int)
}
