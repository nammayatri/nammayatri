package `in`.juspay.mobility.sdk.hyper.bridge

import android.webkit.JavascriptInterface
import org.json.JSONArray

class BridgeList : HBridge {
    val bridgeList: MutableMap<String, HyperBridge> = mutableMapOf()

    override val interfaceName: String
        get() = javaClass.simpleName

    fun addHyperBridge(bridge: HyperBridge) {
        bridgeList[bridge.interfaceName] = bridge
    }

    @get:JavascriptInterface
    val bridgeKeys: String
        get() = JSONArray(bridgeList.keys).toString()
}
