package in.juspay.mobility.app;

import static in.juspay.hyper.core.JuspayCoreLib.getApplicationContext;

import android.app.Activity;
import android.content.Context;
import android.util.Log;

import com.clevertap.android.signedcall.init.SignedCallAPI;
import com.clevertap.android.signedcall.interfaces.MissedCallNotificationOpenedHandler;
import com.clevertap.android.signedcall.models.MissedCallNotificationOpenResult;

import org.json.JSONException;
import org.json.JSONObject;

import java.lang.ref.WeakReference;


public class MissedCallActionsHandler implements MissedCallNotificationOpenedHandler {
    private final WeakReference<CleverTapSignedCall> cleverTapSignedCall;

    public MissedCallActionsHandler(CleverTapSignedCall cleverTapSignedCall) {
        this.cleverTapSignedCall = new WeakReference<>(cleverTapSignedCall);
    }

    @Override
    public void onMissedCallNotificationOpened(Context context, MissedCallNotificationOpenResult result) {
        String actionId = result.action.actionID;
        boolean isDriver = result.callDetails.calleeCuid.contains("driver");
        String receiverCuid = result.callDetails.callerCuid;
        boolean isMissed = true;
        String callContext = isDriver ? "Customer" : "Driver";
        String remoteContext = isDriver ? "Driver" : "Customer";
        String callerCuid = result.callDetails.calleeCuid;
        JSONObject config = new JSONObject();
        try {
            config.put("isDriver", isDriver);
            config.put("isMissed", isMissed);
            config.put("receiverCuid", receiverCuid);
            config.put("callerCuid", callerCuid);
            config.put("callContext", callContext);
            config.put("remoteContext", remoteContext);
        } catch (JSONException e) {
            Log.e("MissedCallHandler", "Error creating JSON config", e);
        }
        switch (actionId) {
            case "callback":
                Log.d("MissedCallHandler", "Voip Notification Callback Pressed," + " actionId: " + actionId);
                CleverTapSignedCall signedCall = cleverTapSignedCall.get();
                if (signedCall != null) {
                    signedCall.voipDialer(config.toString(), CleverTapSignedCall.phone, "push", null);
                } else {
                    Log.w("MissedCallHandler",
                            "SignedCall reference lost – skipping VOIP dialer launch");
                }
                break;
            case "dismiss":
                Log.d("MissedCallHandler", "Voip Notification dismissal");
                SignedCallAPI.getInstance().dismissMissedCallNotification(getApplicationContext());
                break;
            default:
                Log.d("MissedCallHandler", "Unknown action: " + actionId);
        }
    }
}
