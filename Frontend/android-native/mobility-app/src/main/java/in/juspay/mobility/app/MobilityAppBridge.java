package in.juspay.mobility.app;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.os.Bundle;
import android.util.Log;
import android.webkit.JavascriptInterface;

import androidx.annotation.NonNull;

import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.Task;
import com.google.android.play.core.review.ReviewInfo;
import com.google.android.play.core.review.ReviewManager;
import com.google.android.play.core.review.ReviewManagerFactory;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessaging;

import java.util.Locale;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hypersdk.data.KeyValueStore;

public class MobilityAppBridge extends HyperBridge  {

    private FirebaseAnalytics mFirebaseAnalytics;

    private final String CALLBACKS = "CALLBACKS";

    public MobilityAppBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(bridgeComponents.getContext());
    }

    @JavascriptInterface
    public void factoryResetApp(){
        if (bridgeComponents.getActivity() != null ) {
            final PackageManager pm = bridgeComponents.getActivity().getPackageManager();
            final Intent intent = pm.getLaunchIntentForPackage(bridgeComponents.getActivity().getPackageName());
            bridgeComponents.getActivity().finishAffinity(); // Finishes all activities.
            bridgeComponents.getContext().startActivity(intent);    // Start the launch activity
        }
    }

    @Override
    public void reset() {

    }

    // Firebase Functions
    @JavascriptInterface
    public void firebaseLogEvent(String event) {
        Bundle params = new Bundle();
        mFirebaseAnalytics.logEvent(event, params);
    }

    @JavascriptInterface
    public void firebaseLogEventWithParams(String event,String paramKey,String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey,paramValue);
        mFirebaseAnalytics.logEvent(event, params);
    }

    @JavascriptInterface
    public void firebaseLogEventWithTwoParams(String event,String paramKey1,String paramValue1,String paramKey2,String paramValue2) {
        Bundle params = new Bundle();
        params.putString(paramKey1,paramValue1);
        params.putString(paramKey2,paramValue2);
        mFirebaseAnalytics.logEvent(event, params);
    }
    @JavascriptInterface
    public void firebaseUserID (String id){
        mFirebaseAnalytics.setUserId(id);
    }
    @JavascriptInterface
    public void setFCMToken(final String callback) {
        ExecutorManager.runOnMainThread(new Runnable() {
            @Override
            public void run() {
                FirebaseMessaging.getInstance().getToken()
                        .addOnCompleteListener(new OnCompleteListener<String>() {
                            @Override
                            public void onComplete(@NonNull Task<String> task) {
                                if (!task.isSuccessful()) {
//                                    Log.w(LOG_TAG, "Fetching FCM registration token failed", task.getException());
                                    return;
                                }
                                // Get new FCM registration token
                                String token = task.getResult();
                                // Log and toast
//                                Log.d(LOG_TAG, "TOKEN TOKEN: ");
//                                Log.d(LOG_TAG, token);
                                KeyValueStore.write(bridgeComponents.getContext(),bridgeComponents.getSdkName(),"FCM_TOKEN",token);
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');", callback,token);
                                if (callback != null) {
                                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                }
                            }
                        });
            }
        });
    }

    @JavascriptInterface
    public void launchInAppRatingPopup(){
        ReviewManager manager = ReviewManagerFactory.create(bridgeComponents.getContext());
        Task<ReviewInfo> request = manager.requestReviewFlow();
        request.addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                // We can get the ReviewInfo object
                ReviewInfo reviewInfo = task.getResult();
                Task<Void> flow = manager.launchReviewFlow(bridgeComponents.getActivity(), reviewInfo);
                flow.addOnCompleteListener(task1 -> {
                    // The flow has finished. The API does not indicate whether the user
                    // reviewed or not, or even whether the review dialog was shown.
                });
            } else {
                // There was some problem, log or handle the error code.
            }
        });
    }
}
