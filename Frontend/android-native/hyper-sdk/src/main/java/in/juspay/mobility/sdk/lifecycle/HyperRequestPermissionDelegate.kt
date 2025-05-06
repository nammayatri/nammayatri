package `in`.juspay.mobility.sdk.lifecycle

import androidx.fragment.app.Fragment
import `in`.juspay.mobility.sdk.core.JuspayServices
import `in`.juspay.mobility.sdk.ui.RequestPermissionDelegate
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
