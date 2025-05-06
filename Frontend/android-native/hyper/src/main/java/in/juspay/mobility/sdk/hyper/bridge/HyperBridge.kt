package `in`.juspay.mobility.sdk.hyper.bridge

import android.content.Intent
import `in`.juspay.mobility.sdk.hyper.core.BridgeComponents

abstract class HyperBridge protected constructor(
    protected val bridgeComponents: BridgeComponents
) : HBridge {

    override val interfaceName: String
        get() = javaClass.simpleName

    open fun reset() {}
    open fun terminate() {}
    open fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?): Boolean {
        return false
    }

    open fun onRequestPermissionResult(
        requestCode: Int,
        permissions: Array<String>,
        grantResults: IntArray
    ): Boolean {
        return false
    }
}
