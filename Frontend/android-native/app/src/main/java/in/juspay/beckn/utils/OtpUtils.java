package in.juspay.mobility.utils;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.util.Log;


import androidx.annotation.NonNull;

import com.google.android.gms.auth.api.phone.SmsRetriever;
import com.google.android.gms.auth.api.phone.SmsRetrieverClient;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.Task;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;

import in.juspay.hypersdk.HyperFragment;
import in.juspay.hypersdk.core.JuspayDuiHook;

// Uncomment to generate signature for OTP reader
// import android.util.Log;
// import in.juspay.mobility.Test;

public class OtpUtils extends BroadcastReceiver implements JuspayDuiHook {

    String initiateCallback;
    String processCallback;
    HyperFragment hyperFragment;
    JSONArray otp;
    boolean hasTimedOut;

    public OtpUtils(HyperFragment fragment, String callback){
        super();
        initiateCallback = callback;
        hyperFragment = fragment;
        otp = new JSONArray();
        hasTimedOut = false;
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        if (SmsRetriever.SMS_RETRIEVED_ACTION.equals(intent.getAction())) {
            Bundle extras = intent.getExtras();
            Status status = (Status) extras.get(SmsRetriever.EXTRA_STATUS);

            switch(status.getStatusCode()) {
                case CommonStatusCodes.SUCCESS:
                    String message = (String) extras.get(SmsRetriever.EXTRA_SMS_MESSAGE);
                    JSONObject msg = new JSONObject();

                    try {
                        msg.put("from", "UNKNOWN_BANK"); // User consent API will not give the sender name
                        msg.put("body", message);
                        msg.put("time", String.valueOf(System.currentTimeMillis()));
                    } catch (JSONException e) {
                        e.printStackTrace();
                    } catch (Exception e){
                        e.printStackTrace();
                    }

                    otp.put(msg);
                    if(processCallback!=null){
                        hyperFragment.getDuiInterface().invokeCallbackInDUIWebview(processCallback, otp.toString());
                        otp = new JSONArray();
                        processCallback = null;
                    }
                    break;
                case CommonStatusCodes.TIMEOUT:
                    hasTimedOut = true;
                    if(processCallback!=null) {
                        hyperFragment.getDuiInterface().invokeCallbackInDUIWebview(processCallback, "TIMEOUT");
                    }
                    break;
            }
        }
    }

    @Override
    public void attach(Activity activity) {
        SmsRetrieverClient client = SmsRetriever.getClient(activity.getApplicationContext() /* context */);
        Task<Void> task = client.startSmsRetriever();
        task.addOnSuccessListener(aVoid -> {
//            Log.d("SMS - TOKEN", new Test(activity.getApplicationContext()).getAppSignatures().toString());
            IntentFilter filter = new IntentFilter(SmsRetriever.SMS_RETRIEVED_ACTION);
            filter.addAction(Intent.ACTION_AIRPLANE_MODE_CHANGED);
            activity.registerReceiver(OtpUtils.this, filter);
            hyperFragment.getDuiInterface().invokeCallbackInDUIWebview(initiateCallback, "SUCCESS");
        });

        task.addOnFailureListener(e -> {
            hyperFragment.getDuiInterface().invokeCallbackInDUIWebview(initiateCallback, "FAILURE");
        });
    }

    @Override
    public String execute(Activity activity, String cmd, JSONObject jsonObject, String callback) {
        switch (cmd) {
            case "getOtp":
                if (otp.length() != 0) {
                    hyperFragment.getDuiInterface().invokeCallbackInDUIWebview(callback, otp.toString());
                    otp = new JSONArray();
                    return "SUCCESS";
                } else if (hasTimedOut) {
                    hyperFragment.getDuiInterface().invokeCallbackInDUIWebview(callback, "TIMEOUT");
                }
                processCallback = callback;
                return "SUCCESS";
            case "cancel":
                processCallback = null;
                return "SUCCESS";
            default:
                return "FAILURE";
        }
    }

    @Override
    public void detach(Activity activity) {
        activity.unregisterReceiver(this);
    }
}
