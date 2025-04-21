package `in`.juspay.hypersdk.lifecycle

import android.content.Intent

internal interface ActivityResultHolder {
    fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?)
}
