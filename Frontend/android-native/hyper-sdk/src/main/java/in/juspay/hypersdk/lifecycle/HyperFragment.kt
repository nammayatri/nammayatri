package `in`.juspay.hypersdk.lifecycle

import android.content.Context
import android.content.Intent
import android.os.Bundle
import androidx.fragment.app.Fragment
import java.util.LinkedList

internal class HyperFragment : Fragment() {
    private val onPauseListeners = LinkedList<EventListener>()
    private val onStopListeners = LinkedList<EventListener>()
    private val onResumeListeners = LinkedList<EventListener>()
    private val onDestroyListeners = LinkedList<EventListener>()
    private val onSaveInstanceListeners = LinkedList<EventListener>()
    private val onAttachListeners = LinkedList<EventListener>()
    private val onActivityResultListeners = LinkedList<ActivityResultHolder>()
    private val onRequestPermissionsResultListeners = LinkedList<RequestPermissionResult>()

    override fun onAttach(context: Context) {
        super.onAttach(context)
        for (eventListener in onAttachListeners) {
            eventListener.onEvent("{}", this)
        }
    }

    override fun onPause() {
        super.onPause()
        for (eventListener in onPauseListeners) {
            eventListener.onEvent("{}", this)
        }
    }

    override fun onResume() {
        super.onResume()
        for (eventListener in onResumeListeners) {
            eventListener.onEvent("{}", this)
        }
    }

    override fun onStop() {
        super.onStop()
        for (eventListener in onStopListeners) {
            eventListener.onEvent("{}", this)
        }
    }

    override fun onDestroy() {
        super.onDestroy()
        for (eventListener in onDestroyListeners) {
            eventListener.onEvent("{}", this)
        }
        for (event in FragmentEvent.values()) {
            unRegisterForEvent(event)
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        for (eventListener in onSaveInstanceListeners) {
            eventListener.onEvent("{}", this)
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        super.onActivityResult(requestCode, resultCode, data)
        for (eventListener in onActivityResultListeners) {
            eventListener.onActivityResult(requestCode, resultCode, data)
        }
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<String>,
        grantResults: IntArray
    ) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        for (eventListener in onRequestPermissionsResultListeners) {
            eventListener.onRequestPermissionsResult(requestCode, permissions, grantResults)
        }
    }

    fun registerForEvent(event: FragmentEvent, listener: EventListener) {
        when (event) {
            FragmentEvent.ON_PAUSE -> onPauseListeners.add(listener)
            FragmentEvent.ON_RESUME -> onResumeListeners.add(listener)
            FragmentEvent.ON_STOP -> onStopListeners.add(listener)
            FragmentEvent.ON_DESTROY -> onDestroyListeners.add(listener)
            FragmentEvent.ON_SAVED_STATE_INSTANCE -> onSaveInstanceListeners.add(listener)
            FragmentEvent.ON_ATTACH -> onAttachListeners.add(listener)
            else -> Unit
        }
    }

    fun registerOnActivityResult(listener: ActivityResultHolder) {
        onActivityResultListeners.add(listener)
    }

    fun registerOnRequestPermissionResult(listener: RequestPermissionResult) {
        onRequestPermissionsResultListeners.add(listener)
    }

    private fun unRegisterForEvent(event: FragmentEvent) {
        when (event) {
            FragmentEvent.ON_PAUSE -> onPauseListeners.clear()
            FragmentEvent.ON_RESUME -> onResumeListeners.clear()
            FragmentEvent.ON_STOP -> onStopListeners.clear()
            FragmentEvent.ON_DESTROY -> onDestroyListeners.clear()
            FragmentEvent.ON_SAVED_STATE_INSTANCE -> onSaveInstanceListeners.clear()
            FragmentEvent.ON_ATTACH -> onAttachListeners.clear()
            FragmentEvent.ON_ACTIVITY_RESULT -> onActivityResultListeners.clear()
            FragmentEvent.ON_REQUEST_PERMISSION_RESULT -> onRequestPermissionsResultListeners.clear()
        }
    }
}
