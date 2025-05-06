package `in`.juspay.mobility.sdk.hypersmshandler

import android.app.Activity
import androidx.annotation.Keep
import org.json.JSONObject

/**
 * Created by Veera.Subbiah on 19/04/17.
 */
@Keep
interface JuspayDuiHook {
    fun attach(activity: Activity)
    fun execute(activity: Activity, operation: String?, args: JSONObject?): String?
    fun detach(activity: Activity)
}
