package in.juspay.mobility.common;

import static in.juspay.hyper.core.JuspayCoreLib.getApplicationContext;

import android.content.Context;
import android.util.Log;

import androidx.core.content.ContextCompat;
import com.clevertap.android.signedcall.exception.CallException;
import com.clevertap.android.signedcall.init.SignedCallAPI;
import com.clevertap.android.signedcall.interfaces.MissedCallNotificationOpenedHandler;
import com.clevertap.android.signedcall.interfaces.OutgoingCallResponse;
import com.clevertap.android.signedcall.models.MissedCallNotificationOpenResult;
import org.json.JSONException;
import org.json.JSONObject;

public class MissedCallActionsHandler implements MissedCallNotificationOpenedHandler {

    @Override
    public void onMissedCallNotificationOpened(Context context, MissedCallNotificationOpenResult result) {
        String actionId = result.action.actionID;
        switch (actionId) {
            case "callback":
                makeSignedCall(result);
                break;
            case "message":
                // Logic for sending a message
                break;
            case "dismiss":
                // Logic for dismissing the notification
                break;
            default:
                Log.d("MissedCallHandler", "Unknown action: " + actionId);
        }
    }

    public void makeSignedCall(MissedCallNotificationOpenResult result) {
        OutgoingCallResponse outgoingCallResponseListener = new OutgoingCallResponse() {
            @Override
            public void onSuccess() {
                //App is notified on the main thread when the call-request is accepted and being processed by the signalling channel
            }
            @Override
            public void onFailure(CallException callException) {
                //App is notified on the main thread when the call is failed
                Log.d("SignedCall: ", "error code: " + callException.getErrorCode()
                    + "\n error message: " + callException.getMessage()
                    + "\n error explanation: " + callException.getExplanation());

                System.out.println("Signed call: " + "Failed signed call");
            }
        };

        JSONObject callOptions = new JSONObject();
        boolean isCustomer = result.callDetails.callContext.contains("driver");
        try {
            callOptions.put("remote_context", isCustomer ? "Customer Calling" : "Driver Calling");
        } catch (JSONException e) {
            e.printStackTrace();
        }

        SignedCallAPI.getInstance().call(getApplicationContext(), result.callDetails.callerCuid, isCustomer ? "Calling Driver" : "Calling Customer", callOptions, outgoingCallResponseListener);
    }
}


