package in.juspay.mobility.app.overlayMessage;

import android.content.Context;
import android.view.View;

public interface ViewInterface {
    View createView(String payload, Context context, ServiceInterface serviceInterface);

    void destroyView();

    View getView();
}
