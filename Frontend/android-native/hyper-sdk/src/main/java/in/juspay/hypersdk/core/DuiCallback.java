package in.juspay.hypersdk.core;

import in.juspay.hyper.core.JsCallback;

public interface DuiCallback extends JsCallback {
    DuiLogger getLogger();

    InflateView getInflateView();
}
