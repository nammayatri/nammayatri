package in.juspay.mobility.app;

import android.content.Context;

import com.facebook.react.ReactApplication;
import com.facebook.react.ReactHost;
import com.facebook.react.bridge.ReactContext;

public class ReactUtils {
    public static void emitReactEvent (Context context, String event, Object payload) {
        ReactApplication reactApplication= (ReactApplication) context.getApplicationContext();
        if (reactApplication != null) {
            ReactHost host = reactApplication.getReactHost();
            if (host != null) {
                ReactContext reactContext = host.getCurrentReactContext();
                if (reactContext != null){
                    reactContext.getJSModule(ReactContext.RCTDeviceEventEmitter.class).emit(event, payload);
                }
            }
        }
    }
}
