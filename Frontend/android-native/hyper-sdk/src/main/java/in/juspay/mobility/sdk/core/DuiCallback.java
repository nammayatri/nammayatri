package in.juspay.mobility.sdk.core;

import in.juspay.mobility.sdk.hyper.core.JsCallback;

public interface DuiCallback extends JsCallback {
    DuiLogger getLogger();

    InflateView getInflateView();
}
