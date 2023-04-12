package in.juspay.mobility.driver;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.MobilityCommonBridge;

public class MobilityDriverBridge extends MobilityCommonBridge {
    public MobilityDriverBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);

        bridgeComponents.getJsCallback().addJsToWebView("");
    }
}
