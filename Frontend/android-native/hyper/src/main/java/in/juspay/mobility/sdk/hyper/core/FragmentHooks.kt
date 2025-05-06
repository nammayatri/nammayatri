package `in`.juspay.mobility.sdk.hyper.core

import android.content.Intent
import android.content.IntentSender
import android.os.Bundle

interface FragmentHooks {
    fun startActivityForResult(intent: Intent, requestCode: Int, bundle: Bundle?)
    fun requestPermission(permissions: Array<String>, requestCode: Int)
    fun startIntentSenderForResult(
        intentSender: IntentSender,
        requestCode: Int,
        fillInIntent: Intent?,
        flagMask: Int,
        flagValues: Int,
        extraFlags: Int,
        options: Bundle?
    )
}
