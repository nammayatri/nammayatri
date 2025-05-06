package `in`.juspay.mobility.sdk.lifecycle

import android.content.Intent

internal interface ActivityResultHolder {
    fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?)
}
