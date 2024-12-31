package in.juspay.mobility.common;

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
import com.clevertap.android.signedcall.exception.CallException;
import com.clevertap.android.signedcall.init.SignedCallAPI;
import com.clevertap.android.signedcall.interfaces.MissedCallNotificationOpenedHandler;
import com.clevertap.android.signedcall.interfaces.OutgoingCallResponse;
import com.clevertap.android.signedcall.models.MissedCallNotificationOpenResult;
import org.json.JSONException;
import org.json.JSONObject;


public class MissedCallActionsHandler implements MissedCallNotificationOpenedHandler {
    private Context context;
    private Activity activity;

    public MissedCallActionsHandler(Context context, Activity activity){
        this.context = context;
        this.activity = activity;
    }
    @Override
    public void onMissedCallNotificationOpened(Context context, MissedCallNotificationOpenResult result) {
        String actionId = result.action.actionID;
        switch (actionId) {
            case "callback":
                makeSignedCall(context,result);
                break;
            case "dismiss":
                // Logic for dismissing the notification
                break;
            default:
                Log.d("MissedCallHandler", "Unknown action: " + actionId);
        }
    }

    public void showAlertForMicrophonePermission() {
        System.out.println("signed call inside showAlertForMicrophonePermission");

        if (context == null || activity == null) {
            Log.e("PermissionDialog", "Context or Activity is null, cannot show dialog");
            System.out.println("Signed call: act or contxt null signed call missed call handler");
            return;
        }
        System.out.println("Signed call: 3act or contxt null signed call missed call handler");

        // Create an AlertDialog Builder
        AlertDialog.Builder builder = new AlertDialog.Builder(activity);
        builder.setCancelable(false); // Prevent dismissing the dialog by touching outside
        System.out.println("Signed call: 4act or context null signed call missed call handler");

        // Inflate the custom layout
        LayoutInflater inflater = activity.getLayoutInflater();
        if (inflater == null) {
            System.out.println("Signed call: inflator null signed call missed call handler");
            Log.e("PermissionDialog", "LayoutInflater is null, cannot show dialog");
            return;
        }
        System.out.println("Signed call: 5 act or contxt null signed call missed call handler");

        ConstraintLayout constraintLayout = (ConstraintLayout) inflater.inflate(
                in.juspay.mobility.common.R.layout.microphone_permission_dialog_2, null
        );

        // Customize the layout's views (optional)
        CardView cardView = constraintLayout.findViewById(in.juspay.mobility.common.R.id.dialogCardView2);
        if (cardView != null) {
            cardView.setCardElevation(8); // Example customization
            cardView.setRadius(16);
        }

        // Set layout params if needed
        ViewGroup.LayoutParams layoutParams = new ConstraintLayout.LayoutParams(
                ConstraintLayout.LayoutParams.MATCH_PARENT,
                ConstraintLayout.LayoutParams.WRAP_CONTENT
        );
        constraintLayout.setLayoutParams(layoutParams);

        // Set the inflated layout as the dialog view
        builder.setView(constraintLayout);
        System.out.println("Signed call: desi null signed call missed call handler");

        // Handle positive button click
        builder.setPositiveButton("Go to Settings", (dialog, which) -> {
            Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
            Uri uri = Uri.fromParts("package", activity.getPackageName(), null);
            intent.setData(uri);
            activity.startActivity(intent);
        });

        // Handle negative button click
        builder.setNegativeButton("Cancel", (dialog, which) -> dialog.cancel());

        // Run the dialog creation on the UI thread
        activity.runOnUiThread(() -> {
            AlertDialog alertDialog = builder.create();
            alertDialog.show();
        });
    }
    public void makeSignedCall(Context context, MissedCallNotificationOpenResult result) {
        String ny_customer = context.getString(R.string.namma_yatri_customer);
        String ny_driver = context.getString(R.string.namma_yatri_driver);

        OutgoingCallResponse outgoingCallResponseListener = new OutgoingCallResponse() {
            @Override
            public void onSuccess() {
                // App is notified on the main thread when the call-request is accepted and being processed
                Log.d("SignedCall", "Signed call success missedcall handler");
            }

            @Override
            public void onFailure(CallException callException) {
                // Log failure details
                Log.d("SignedCall: ", "error code: " + callException.getErrorCode()
                    + "\n error message: " + callException.getMessage()
                    + "\n error explanation: " + callException.getExplanation());

                System.out.println("Signed call: Failed signed call missed call handler");
                // if (context instanceof Activity) {
                if(callException.getErrorCode() == CallException.MicrophonePermissionNotGrantedException.getErrorCode()) {
                    System.out.println("Signed call: Failed bro came here still 1 5001 signed call missed call handler");
                    System.out.println("Signed call: Failed bro came here still 2 5001 signed call missed call handler");

                    showAlertForMicrophonePermission();
                    System.out.println("Signed call: Failed bro came here still 3 5001 signed call missed call handler");
                    System.out.println("Signed call: Failed bro came here still 4 5001 signed call missed call handler");

                }
            }
        };

        JSONObject callOptions = new JSONObject();
        boolean isDriver = result.callDetails.calleeCuid.contains("driver");
        String callContext, remoteContext;

        if (isDriver) {
            callContext = ny_customer.isEmpty() ? "Namma Yatri Customer" : ny_customer;
            remoteContext = ny_driver.isEmpty() ? "Namma Yatri Driver" : ny_driver;
        } else {
            callContext = ny_driver.isEmpty() ? "Namma Yatri Driver" : ny_driver;
            remoteContext = ny_customer.isEmpty() ? "Namma Yatri Customer" : ny_customer;
        }
        try {
            callOptions.put("remote_context", remoteContext);
        } catch (JSONException e) {
            e.printStackTrace();
        }

        // Initiate the signed call
        SignedCallAPI.getInstance().call(
            getApplicationContext(),
            result.callDetails.callerCuid,
            callContext,
            callOptions,
            outgoingCallResponseListener
        );
    }

}




