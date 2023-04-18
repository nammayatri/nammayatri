package in.juspay.mobility.appcommon;

import android.content.Context;
import android.content.SharedPreferences;
import android.webkit.JavascriptInterface;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hypersdk.data.KeyValueStore;

public class AppCommonBridge extends HyperBridge  {

    public AppCommonBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
    }

    @Override
    public void reset() {

    }
}
