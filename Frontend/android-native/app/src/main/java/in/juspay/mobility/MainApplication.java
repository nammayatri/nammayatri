package in.juspay.mobility;

import static in.juspay.mobility.BuildConfig.MERCHANT_TYPE;
import static in.juspay.mobility.Utils.getInnerPayload;
import static in.juspay.mobility.Utils.getService;
import static in.juspay.mobility.app.Utils.minimizeApp;

import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;

import com.clevertap.android.sdk.CleverTapAPI;
import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Objects;
import java.util.UUID;
import java.util.Vector;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.mobility.app.WidgetService;
import in.juspay.mobility.app.services.MobilityAppUpdate;
import in.juspay.services.HyperServices;

public class MainApplication extends Application {
    private static final String LOG_TAG = "MAIN_APPLICATION_HYPER_SERVICE";
    @Override
    public void onCreate() {
        System.out.println("MainApplication_onCreate " + System.currentTimeMillis());
        super.onCreate();
        Context context = getApplicationContext();
        System.out.println("INITIATE CALL " + System.currentTimeMillis());
        SDKHolder.getInstance(context).initApp();
    }
}
