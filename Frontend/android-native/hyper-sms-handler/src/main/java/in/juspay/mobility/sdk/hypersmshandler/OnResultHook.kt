package `in`.juspay.mobility.sdk.hypersmshandler

import android.content.Intent
import androidx.annotation.Keep

@Keep
interface OnResultHook {
    /**
     * Handler that is invoked when an activity that is started gives out a result. Whether this result is consumed or
     * not should be reported by using the return value. If `true` is returned then the result will not be passed
     * to other hooks or the micro-app.
     *
     * @param requestCode The code that is given to `startActivityForResult(Intent, int, Bundle)`} method.
     * @param resultCode  The result code returned by the finished activity.
     * @param data        Any data that is emitted by the finished activity.
     * @return `true` if the result is consumed, `false` otherwise.
     */
    fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?): Boolean
}
