package in.juspay.mobility.app;

import static android.Manifest.permission.RECORD_AUDIO;
import static in.juspay.hyper.core.JuspayCoreLib.getApplicationContext;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.provider.Settings;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.cardview.widget.CardView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import com.clevertap.android.signedcall.enums.SignallingChannel;
import com.clevertap.android.signedcall.exception.CallException;
import com.clevertap.android.signedcall.init.SignedCallAPI;
import com.clevertap.android.signedcall.interfaces.MissedCallNotificationOpenedHandler;
import com.clevertap.android.signedcall.interfaces.OutgoingCallResponse;
import com.clevertap.android.signedcall.models.MissedCallNotificationOpenResult;
import org.json.JSONException;
import org.json.JSONObject;

import in.juspay.hyper.core.BridgeComponents;


public class MissedCallActionsHandler implements MissedCallNotificationOpenedHandler {
    private Context context;
    private Activity activity;
    private CleverTapSignedCall cleverTapSignedCall;
    public MissedCallActionsHandler(Context context, Activity activity){
        this.context = context;
        this.activity = activity;
        cleverTapSignedCall = new CleverTapSignedCall(context,activity);
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
                System.out.println("signedcall missed");
                cleverTapSignedCall.voipDialer(config.toString(), CleverTapSignedCall.phone, "push",null);
                break;
            case "dismiss":
                // Logic for dismissing the notification
                break;
            default:
                Log.d("MissedCallHandler", "Unknown action: " + actionId);
        }
    }
}
