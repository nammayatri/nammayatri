package `in`.juspay.hyper.core

import android.app.Activity
import android.content.Context
import org.json.JSONObject

interface BridgeComponents {
    val context: Context
    val activity: Activity?
    val fragmentHooks: FragmentHooks
    val trackerInterface: TrackerInterface
    val callbackInvoker: CallbackInvoker
    val fileProviderInterface: FileProviderInterface
    val jsCallback: JsCallback?
    val sdkName: String
    val sdkConfig: JSONObject
    val clientId: String?
    val sessionInfoInterface: SessionInfoInterface
}
