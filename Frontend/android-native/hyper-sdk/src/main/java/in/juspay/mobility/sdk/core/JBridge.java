package in.juspay.mobility.sdk.core;

import static java.lang.Integer.parseInt;
import static in.juspay.mobility.sdk.security.EncryptionHelper.bytesToHexString;

import android.animation.ValueAnimator;
import android.app.AlertDialog;
import android.app.DatePickerDialog;
import android.content.BroadcastReceiver;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.animation.AccelerateInterpolator;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.DecelerateInterpolator;
import android.view.animation.LinearInterpolator;
import android.view.animation.RotateAnimation;
import android.view.inputmethod.InputMethodManager;
import android.webkit.JavascriptInterface;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import com.airbnb.lottie.LottieAnimationView;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.X509EncodedKeySpec;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import javax.net.ssl.SSLHandshakeException;

import in.juspay.mobility.sdk.hyper.bridge.HyperBridge;
import in.juspay.mobility.sdk.hyper.bridge.ThreeDS2Bridge;
import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogLevel;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.R;
import in.juspay.mobility.sdk.mystique.SwypeLayout;
import in.juspay.mobility.sdk.security.EncryptionHelper;
import in.juspay.mobility.sdk.security.HyperSSLSocketFactory;
import in.juspay.mobility.sdk.security.JOSEUtils;
import in.juspay.mobility.sdk.utils.network.JuspayHttpsResponse;
import in.juspay.mobility.sdk.utils.network.NetUtils;
import in.juspay.mobility.sdk.utils.network.SessionizedNetUtils;


public class JBridge extends DuiInterface {
    private static final String LOG_TAG = "JBridge";

    @Nullable
    private NetUtils netUtils;
    @Nullable
    private NetUtils netUtilsSsl;

    private final int JUSPAY_LOADER_ID = 898989;

    @Nullable
    private BroadcastReceiver broadcastReceiver = null;

    @NonNull
    private final Set<String> acceptedCerts;

    @NonNull
    private final AtomicInteger apiTag = new AtomicInteger(0);


    public JBridge(@NonNull JuspayServices juspayServices) {
        super(juspayServices);
        acceptedCerts = new HashSet<>();

        try {
            netUtils = new SessionizedNetUtils(super.sessionInfo, 0, 0, false);
            netUtilsSsl = new SessionizedNetUtils(super.sessionInfo, 0, 0, true);
        } catch (Exception e) {
            juspayServices.getSdkTracker().trackAndLogException(
                    LOG_TAG,
                    LogCategory.ACTION,
                    LogSubCategory.Action.SYSTEM,
                    Labels.System.JBRIDGE,
                    "Error while instantiating NetUtils",
                    e
            );
        }
    }

    public static float dpToPx(float dp, Context context) {
        Resources resources = context.getResources();
        DisplayMetrics metrics = resources.getDisplayMetrics();
        return dp * ((float) metrics.densityDpi / DisplayMetrics.DENSITY_DEFAULT);
    }

    @Override
    public void reset() {
        super.reset();
        unRegisterReceiver();
    }

    @JavascriptInterface
    @Override
    public void exitApp(int i, String s) {
        SwypeLayout.clear();
        super.exitApp(i, s);
    }

    @JavascriptInterface
    public void registerReceiver(@NonNull String action) {
        if (broadcastReceiver != null) return;
        broadcastReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, final Intent intent) {
                receiverCallback(intent);
            }
        };
        LocalBroadcastManager.getInstance(juspayServices.getContext()).registerReceiver(broadcastReceiver, new IntentFilter(action));
    }

    @JavascriptInterface
    public void unRegisterReceiver() {
        if (broadcastReceiver == null) return;
        LocalBroadcastManager.getInstance(juspayServices.getContext()).unregisterReceiver(broadcastReceiver);
        broadcastReceiver = null;
    }

    @JavascriptInterface
    public void showJuspayLoader(final String loaderConfig) {
        ExecutorManager.runOnMainThread(() -> {
            if (activity == null) return;
            if (activity.findViewById(JUSPAY_LOADER_ID) != null) {
                return;
            }
            int rotationDuration = 2100, animationDuration = 1000;
            float startAlpha = 0.0f, endAlpha = 1.0f;
            String message = "Processing your payment";

            try {
                JSONObject config = new JSONObject(loaderConfig);
                rotationDuration = Integer.parseInt(config.optString("rotationDuration", "2100"));
                animationDuration = Integer.parseInt(config.optString("animationDuration", "1000"));
                startAlpha = Float.parseFloat(config.optString("startAlpha", "0.0"));
                endAlpha = Float.parseFloat(config.optString("endAlpha", "1.0"));
                message = config.optString("message", "Processing your payment");
            } catch (Exception ignored) {
            }

            final LinearLayout parent = new LinearLayout(activity);
            parent.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT));
            parent.setBackgroundColor(Color.parseColor("#ffffff"));
            parent.setGravity(Gravity.CENTER);
            parent.setId(JUSPAY_LOADER_ID);
            parent.setClickable(true);

            LinearLayout contentHolder = new LinearLayout(activity);
            contentHolder.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
            contentHolder.setOrientation(LinearLayout.VERTICAL);
            contentHolder.setGravity(Gravity.CENTER_HORIZONTAL);

            ImageView imageView = new ImageView(activity);
            imageView.setBackgroundResource(R.drawable.juspay_icon);
            imageView.setLayoutParams(new LinearLayout.LayoutParams((int) (dpToPx(48, activity)), (int) (dpToPx(48, activity))));

            RotateAnimation anim = new RotateAnimation(0f, 350f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f);
            anim.setInterpolator(new LinearInterpolator());
            anim.setRepeatCount(Animation.INFINITE);
            anim.setDuration(rotationDuration);
            imageView.startAnimation(anim);

            TextView textView = new TextView(activity);
            textView.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
            textView.setPadding(0, (int) dpToPx(10, activity), 0, (int) dpToPx(20, activity));
            textView.setTextSize(16);
            textView.setTextColor(Color.parseColor("#000000"));
            textView.setText(message);

            ImageView juspaySageImage = new ImageView(activity);
            juspaySageImage.setBackgroundResource(R.drawable.juspay_safe);
            juspaySageImage.setLayoutParams(new LinearLayout.LayoutParams((int) (dpToPx(90, activity)), (int) (dpToPx(12, activity))));
            ((LinearLayout.LayoutParams) juspaySageImage.getLayoutParams()).setMargins(0, (int) dpToPx(24, activity), 0, 0);

            contentHolder.addView(imageView);
            contentHolder.addView(juspaySageImage);
            contentHolder.addView(textView);

            parent.addView(contentHolder);

            Animation fadeIn = new AlphaAnimation(startAlpha, endAlpha);
            fadeIn.setInterpolator(new DecelerateInterpolator());
            fadeIn.setDuration(animationDuration);

            parent.setAnimation(fadeIn);

            ViewGroup rootView = (ViewGroup) juspayServices.getContainer();

            if (rootView != null) {
                rootView.addView(parent);
            }
        });

    }

    @JavascriptInterface
    public void hideJuspayLoader(final String loaderConfig) {
        ExecutorManager.runOnMainThread(() -> {
            if (activity == null) return;
            final View parent = activity.findViewById(JUSPAY_LOADER_ID);
            if (parent == null) {
                return;
            }
            int animationDuration = 1000;
            float startAlpha = 1.0f, endAlpha = 0.0f;
            try {
                JSONObject config = new JSONObject(loaderConfig);
                animationDuration = Integer.parseInt(config.optString("animationDuration", "1000"));
                startAlpha = Float.parseFloat(config.optString("startAlpha", "1.0"));
                endAlpha = Float.parseFloat(config.optString("endAlpha", "0.0"));
            } catch (Exception ignored) {
            }
            Animation fadeOut = new AlphaAnimation(startAlpha, endAlpha);
            fadeOut.setInterpolator(new AccelerateInterpolator());
            fadeOut.setDuration(animationDuration);
            parent.setAnimation(fadeOut);

            ViewGroup rootView = (ViewGroup) juspayServices.getContainer();
            if (rootView != null) {
                rootView.removeView(parent);
            }
        });
    }

    @JavascriptInterface
    public boolean isShimmerPossible() {
        try {
            Class.forName("com.facebook.shimmer.ShimmerFrameLayout");
            Class.forName("com.facebook.shimmer.Shimmer");
            return true;
        } catch (Exception ignored) {
        }
        return false;
    }

    @JavascriptInterface
    public boolean checkCustomTabs() {
        try {
            Class.forName("androidx.browser.customtabs.CustomTabsIntent");
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    @JavascriptInterface
    public boolean checkPhonePeSdk() {
        try {
            Class.forName("com.phonepe.android.sdk.api.PhonePe");
            Class.forName("com.phonepe.android.sdk.api.PhonePeInitException");
            Class.forName("com.phonepe.android.sdk.api.builders.TransactionRequestBuilder");
            Class.forName("com.phonepe.android.sdk.base.model.TransactionRequest");
            return true;
        } catch (Exception ignored) {
        }
        return false;
    }

    @Deprecated
    @JavascriptInterface
    public void checkPhonePeSdk(final String cb) {
        invokeCallbackInDUIWebview(cb, String.valueOf(checkPhonePeSdk()));
    }

    @JavascriptInterface
    public void checkAmazonNonTokenSdk(final String cb) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            Class.forName("androidx.browser.customtabs.CustomTabsIntent");
            invokeCallbackInDUIWebview(cb, "true");
        } catch (ClassNotFoundException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.SDK, Labels.SDK.AMAZON_UTILS, "Amazon Sdk Not found Exception", e);
            invokeCallbackInDUIWebview(cb, "false");
        }
    }

    private void receiverCallback(Intent intent) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            String action = intent.getAction();
            if (action == null) {
                throw new Exception("action is null");
            }
            if (action.equals("customtab-result")) {
                handleCustomTabResult(intent);
            } else {
                sdkTracker.trackApiCalls(LogSubCategory.ApiCall.SDK, LogLevel.ERROR, Labels.SDK.RECEIVER_CALLBACK, null, null, null, null, null, "unknown_intent", null);
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.SDK, Labels.SDK.RECEIVER_CALLBACK, "JSON Exception", e);
        }

    }


    public void handleCustomTabResult(Intent intent) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            JSONObject jsonObject;
            if (intent != null) {
                jsonObject = PaymentUtils.toJSON(intent.getExtras());
                invokeCallbackInDUIWebview((String) listenerMap.get("customtab-result-cb"), jsonObject.toString());
            } else {
                invokeCallbackInDUIWebview((String) listenerMap.get("customtab-result-cb"), "{}");
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.SDK, Labels.SDK.CUSTOM_TAB, "JSON Exception", e);
            invokeCallbackInDUIWebview((String) listenerMap.get("customtab-result-cb"), "{}");
        }
    }

    @Deprecated
    @JavascriptInterface
    public void getPackageName(final String cb) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            String packageName = juspayServices.getContext().getPackageName();
            invokeCallbackInDUIWebview(cb, packageName);
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "NULL Pointer Exception while getting package name", e);
            invokeCallbackInDUIWebview(cb, "ERROR");
        }
    }


    @JavascriptInterface
    public String getBuildInfo() {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            JSONObject buildInfo = new JSONObject();
            buildInfo.put("BOARD", Build.BOARD);
            buildInfo.put("BRAND", Build.BRAND);
            buildInfo.put("CPU_ABI", Build.CPU_ABI);
            buildInfo.put("CPU_ABI2", Build.CPU_ABI2);
            buildInfo.put("DEVICE", Build.DEVICE);
            buildInfo.put("DISPLAY", Build.DISPLAY);
            buildInfo.put("FINGERPRINT", Build.FINGERPRINT);
            buildInfo.put("HARDWARE", Build.HARDWARE);
            buildInfo.put("HOST", Build.HOST);
            buildInfo.put("ID", Build.ID);
            buildInfo.put("MANUFACTURER", Build.MANUFACTURER);
            buildInfo.put("MODEL", Build.MODEL);
            buildInfo.put("PRODUCT", Build.PRODUCT);
            buildInfo.put("RADIO", Build.getRadioVersion());
            buildInfo.put("TAGS", Build.TAGS);
            buildInfo.put("TIME", Build.TIME);
            buildInfo.put("USER", Build.USER);

            buildInfo.put("SUPPORTED_32_BIT_ABIS", new JSONArray(Build.SUPPORTED_32_BIT_ABIS));
            buildInfo.put("SUPPORTED_64_BIT_ABIS", new JSONArray(Build.SUPPORTED_64_BIT_ABIS));
            buildInfo.put("SUPPORTED_ABIS", new JSONArray(Build.SUPPORTED_ABIS));

            JSONObject version = new JSONObject();

            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                version.put("BASE_OS", Build.VERSION.BASE_OS);
                version.put("INCREMENTAL", Build.VERSION.INCREMENTAL);
                version.put("PREVIEW_SDK_INT", Build.VERSION.PREVIEW_SDK_INT);
                version.put("SECURITY_PATCH", Build.VERSION.SECURITY_PATCH);
            }

            version.put("RELEASE", Build.VERSION.RELEASE);
            version.put("SDK_INT", Build.VERSION.SDK_INT);

            buildInfo.put("VERSION", version);
            return buildInfo.toString();

        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception fetching build info", e);
        }
        return "";
    }


    @Deprecated
    @JavascriptInterface
    public void amazonNonTokenPay(String url, final String cb) {
        launchCustomTab(url, cb);
    }


    @JavascriptInterface
    public void launchCustomTab(String url, final String cb) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        listenerMap.put("customtab-result-cb", cb);
        try {
            if (juspayServices.isPaused()) {
                sdkTracker.trackAction(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.SDK, Labels.SDK.CUSTOM_TAB, "onPause called before launch customtab");
                unRegisterReceiver();
                JSONObject json = new JSONObject();
                json.put("status", "onPause");
                invokeCallbackInDUIWebview(cb, json.toString());
            } else {
                Intent intent = new Intent(juspayServices.getContext(), CustomtabActivity.class);
                intent.putExtra("url", url);
                registerReceiver(CustomtabActivity.CUSTOMTAB_RESULT);
                juspayServices.startActivityForResult(intent, -1, null);
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.SDK, Labels.SDK.CUSTOM_TAB, "Exception at launch customtab", e);
            unRegisterReceiver();
            invokeCallbackInDUIWebview(cb, e.toString());
        }
    }


    @JavascriptInterface
    public boolean isCCTSupportedChromeAvailable(final String testUrl) {
        try {
            ArrayList<ResolveInfo> appInfo = CustomtabActivity.getCustomTabsPackages(juspayServices.getContext(), testUrl);
            return CustomtabActivity.isChromeInstalled(appInfo);
        } catch (Exception ignored) {
        }
        return false;
    }

    // PHONEPE Functions

    @SuppressWarnings("unused")
    private Map<String, String> getDecodedQueryParameters(String query) throws UnsupportedEncodingException {
        if (query == null || query.trim().length() < 1) {
            return null;
        }
        HashMap<String, String> parameters = new HashMap<>();
        String[] pairs = query.split("&");
        for (String pair : pairs) {
            int index = pair.indexOf("=");
//            parameters.put(pair.substring(0, index), pair.substring (index + 1));
            parameters.put(URLDecoder.decode(pair.substring(0, index), "UTF-8").trim(), URLDecoder.decode(pair
                    .substring(index + 1), "UTF-8").trim());
        }
        return parameters;
    }

    private int versionCompare(String ver1, @SuppressWarnings("SameParameterValue") String ver2) {
        if (TextUtils.isEmpty(ver1) || TextUtils.isEmpty(ver2)) {
            return 1;
        }
        String[] vals1 = ver1.split("\\.");
        String[] vals2 = ver2.split("\\.");
        int i = 0;
        while (i < vals1.length && i < vals2.length && vals1[i].equalsIgnoreCase(vals2[i])) {
            i++;
        }
        if (i < vals1.length && i < vals2.length) {
            int diff = Integer.valueOf(vals1[i]).compareTo(Integer.valueOf(vals2[i]));
            return Integer.signum(diff);
        }
        return Integer.signum(vals1.length - vals2.length);
    }

    @JavascriptInterface
    public void startPaytmRequest(String sdkParams, String version, String cb) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        try {
            JSONObject jsonObject = new JSONObject(sdkParams);
            int PAYTM_REQUEST_CODE = 116;
            if (versionCompare((version), "8.6.0") < 0) {
                Intent paytmIntent = new Intent();
                Bundle bundle = new Bundle();
                bundle.putString("nativeSdkForMerchantAmount", jsonObject.optString("nativeSdkForMerchantAmount"));
                bundle.putString("orderid", jsonObject.optString("orderid"));
                bundle.putString("mid", jsonObject.optString("mid"));
                bundle.putString("txnToken", jsonObject.optString("txnToken"));
                paytmIntent.setComponent(new ComponentName("net.one97.paytm", jsonObject.optString("net.one97.paytm")));
                paytmIntent.putExtra("paymentmode", jsonObject.optInt("paymentmode"));
                paytmIntent.putExtra("bill", bundle);
                juspayServices.startActivityForResult(paytmIntent, PAYTM_REQUEST_CODE, null);
                juspayServices.sdkDebug("paytmSDkParams1", paytmIntent.toString());
            } else {
                Intent paytmIntent = new Intent();
                paytmIntent.setComponent(new ComponentName("net.one97.paytm", jsonObject.optString("net.one97.paytm")));
                paytmIntent.putExtra("paymentmode", jsonObject.optInt("paymentmode"));
                paytmIntent.putExtra("enable_paytm_invoke", jsonObject.optBoolean("enable_paytm_invoke"));
                paytmIntent.putExtra("paytm_invoke", jsonObject.optBoolean("paytm_invoke"));
                paytmIntent.putExtra("price", jsonObject.optString("price"));
                paytmIntent.putExtra("nativeSdkEnabled", jsonObject.optBoolean("nativeSdkEnabled"));
                paytmIntent.putExtra("orderid", jsonObject.optString("orderid"));
                paytmIntent.putExtra("txnToken", jsonObject.optString("txnToken"));
                paytmIntent.putExtra("mid", jsonObject.optString("mid"));
                juspayServices.startActivityForResult(paytmIntent, PAYTM_REQUEST_CODE, null);
                juspayServices.sdkDebug("paytmSDkParams2", paytmIntent.toString());
            }
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.SDK, Labels.SDK.PAYTM_UTILS, "Paytm Init Exception", e);
            invokeCallbackInDUIWebview(cb, e.toString());
        }
    }

    @Deprecated
    @JavascriptInterface
    public void startPhonepeRequest(String base64Body, String checksum, String apiEndPoint, final String cb) {
        invokeCallbackInDUIWebview(cb, "Function deprecated");
    }

    /**
     * @return true if supported phonepe app is present
     */
    @JavascriptInterface
    public boolean doesPhonePeAppExist(final String packageName) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        PackageInfo packageInfo = null;
        final Context context = juspayServices.getContext();
        long phonePeVersionCode = -1L;
        try {
            packageInfo = context.getPackageManager().getPackageInfo(packageName, PackageManager.GET_ACTIVITIES);
            phonePeVersionCode = packageInfo.versionCode;
        } catch (PackageManager.NameNotFoundException e) {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.DEBUG, Labels.System.JBRIDGE, "Failed to get phonepe package name", e);
        }

        if (packageInfo == null) {
            return false;
        }

        return phonePeVersionCode > 94033;
    }

    /**
     * @return return phonepe versioncode if phonepe app is installed on
     * user device
     */
    @JavascriptInterface
    public long getPhonePeVersionCode(final String packageName) {
        PackageManager packageManager = juspayServices.getContext().getPackageManager();

        if (doesPhonePeAppExist(packageName)) {
            long versionCode = -1L;
            try {
                PackageInfo info = packageManager.getPackageInfo(packageName, PackageManager.GET_ACTIVITIES);
                versionCode = info.versionCode;
            } catch (PackageManager.NameNotFoundException ignored) {
            }
            return versionCode;
        }

        return -1L;
    }

    /**
     * @param url Intent upi url to be launched by phonepe app
     *            onActivityResult is captured internally in js.
     */
    @JavascriptInterface
    public void startPhonepeRequest(final String url, final String packageName) {
        Intent i = new Intent(Intent.ACTION_VIEW);
        i.setData(Uri.parse(url));
        i.setPackage(packageName);
        int PHONEPE_REQUEST_CODE = 113;
        juspayServices.startActivityForResult(i, PHONEPE_REQUEST_CODE, null);

    }

    // EC UPI BASED JS Interface functions

    @JavascriptInterface
    public void handlePhonepayActivityResult(String response) {
        String phonepeTxnCallback = "";
        invokeCallbackInDUIWebview(phonepeTxnCallback, response);
    }

    @JavascriptInterface
    public String findApps(String payload) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        Context context = juspayServices.getContext();

        PackageManager pm = context.getPackageManager();
        Intent upi_apps = new Intent();
        upi_apps.setData(Uri.parse(payload));
        List<ResolveInfo> launchables;
        launchables = pm.queryIntentActivities(upi_apps, 0);
        Collections.sort(launchables, new ResolveInfo.DisplayNameComparator(pm));

        JSONArray apps = new JSONArray();

        for (ResolveInfo resolveInfo : launchables) {
            JSONObject jsonObject = new JSONObject();
            try {
                ApplicationInfo ai = pm.getApplicationInfo(resolveInfo.activityInfo.packageName, 0);
                jsonObject.put("packageName", ai.packageName);
                jsonObject.put("appName", pm.getApplicationLabel(ai));

                apps.put(jsonObject);
            } catch (JSONException e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Error While add to json", e);
            } catch (PackageManager.NameNotFoundException e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Error while searching for the app", e);
            }
        }

        return apps.toString();
    }

    @JavascriptInterface
    public void drawAppIcon(String array) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            JSONArray jsonArray = new JSONArray(array);

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject app = jsonArray.getJSONObject(i);
                PackageManager pm = juspayServices.getContext().getPackageManager();
                ApplicationInfo ai = pm.getApplicationInfo(app.getString("packageName"), 0);
                drawIcon(ai.loadIcon(pm), parseInt(app.getString("id")));
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Error happened while parsing json", e);
        }
    }

    private void drawIcon(final Drawable icon, final int id) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        ExecutorManager.runOnMainThread(() -> {
            if (activity == null) {
                return;
            }

            View view = activity.findViewById(id);
            ImageView imageView = new ImageView(activity);
            imageView.setImageDrawable(icon);

            if (view != null) {
                ((ViewGroup) view).addView(imageView);
            } else {
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.ERROR, Labels.System.JBRIDGE, "draw_icon", "No view at " + id + " found to attach the image.");
            }
        });
    }

    @JavascriptInterface
    public String callAPI(final String method, final String url, final String data, final String headers, final boolean shouldEncodeToFormData, final boolean isSSLPinned, final String callback) {
        return callAPIWithOptions(method, url, data, headers, shouldEncodeToFormData, isSSLPinned, (new JSONObject()).toString(), callback);
    }

    @JavascriptInterface
    public String callAPIWithOptions(final String method, final String url, final String data, final String headers, final boolean shouldEncodeToFormData, final boolean isSSLPinned, String options, final String callback) {
        Log.i("CALLAPI","Called Req -> " + url + " " + System.currentTimeMillis());
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        final DynamicUI dynamicUI = juspayServices.getDynamicUI();
        JSONObject jsonOptions1;
        try {
            jsonOptions1 = new JSONObject(options);
        } catch (Exception ignored) {
            jsonOptions1 = new JSONObject();
        }

        final JSONObject jsonOptions = jsonOptions1;

        final long startTime = System.currentTimeMillis();
        String tag = "tag" + apiTag.incrementAndGet();
        sdkTracker.trackApiCalls(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.BEFORE_REQUEST, null, url, tag, startTime, null, JSONObject.NULL, null, method, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
        // Don't merge until sanitize method is implemented for data

        NetUtils netUtilsTemp;
        if (isSSLPinned) {
            netUtilsTemp = this.netUtilsSsl;
        } else {
            netUtilsTemp = this.netUtils;
        }

        if (netUtilsTemp == null) {
            try {
                if (isSSLPinned) {
                    netUtilsTemp = this.netUtilsSsl = new SessionizedNetUtils(super.sessionInfo, 0, 0, true);
                } else {
                    netUtilsTemp = this.netUtils = new SessionizedNetUtils(super.sessionInfo, 0, 0, false);
                }
            } catch (Exception e) {
                juspayServices.getSdkTracker().trackAndLogException(
                        LOG_TAG,
                        LogCategory.ACTION,
                        LogSubCategory.Action.SYSTEM,
                        Labels.System.JBRIDGE,
                        "Error while instantiating NetUtils in callAPI",
                        e
                );
            }
        }

        if (netUtilsTemp == null) {
            if (callback != null) {
                String base64Data = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
                String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s','%s');", callback, "failure", base64Data, -1, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), "%7B%7D");

                dynamicUI.addJsToWebView(javascript);
            }
            sdkTracker.trackApiCalls(LogSubCategory.ApiCall.NETWORK, LogLevel.ERROR, Labels.Network.NETWORK_CALL, -1, url, null, startTime, System.currentTimeMillis(), data, "Unable to create netUtils object", method, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
            return "";
        }

        final NetUtils netUtils = netUtilsTemp;
        try {
            if (isSSLPinned) {
                netUtils.setSslSocketFactory(new HyperSSLSocketFactory(acceptedCerts).getSslSocketFactory());
            }
        } catch (Exception e) {
            JuspayLogger.e(LOG_TAG, "Exception: ", e);
        }

        ExecutorManager.runOnApiThread(() -> {
            JuspayHttpsResponse response = null;
            try {
                HashMap<String, String> headersMap = toMap(headers);
                HashMap<String, String> dataMap = shouldEncodeToFormData ? toMap(data) : null;

                if ("GET".equals(method)) {
                    response = new JuspayHttpsResponse(netUtils.doGet(url, headersMap, dataMap, jsonOptions, tag));
                } else if ("HEAD".equals(method)) {
                    response = new JuspayHttpsResponse(netUtils.doHead(url, headersMap, dataMap, jsonOptions, tag));
                } else if ("POST".equals(method)) {
                    if (dataMap == null) {
                        response = new JuspayHttpsResponse(netUtils.postUrl(new URL(url), headersMap, data, jsonOptions, tag));
                    } else {
                        response = new JuspayHttpsResponse(netUtils.postUrl(new URL(url), headersMap, dataMap, jsonOptions, tag));
                    }
                } else if ("DELETE".equals(method)) {
                    if (dataMap == null) {
                        response = new JuspayHttpsResponse(netUtils.deleteUrl(new URL(url), headersMap, data, jsonOptions, tag));
                    } else {
                        response = new JuspayHttpsResponse(netUtils.deleteUrl(new URL(url), headersMap, dataMap, jsonOptions, tag));
                    }
                } else if ("PUT".equals(method)) {
                    response = new JuspayHttpsResponse(netUtils.doPut(juspayServices.getContext(), new URL(url), data.getBytes(), headersMap, netUtils, jsonOptions, tag));
                }
            } catch (SSLHandshakeException e) {
                sdkTracker.trackAndLogApiException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.NETWORK, Labels.Network.NETWORK_CALL, startTime, System.currentTimeMillis(), data, url, method, "SSLHandshakeException while calling api", e, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
                response = new JuspayHttpsResponse(-2, "SSL Handshake Failed".getBytes(), null);
            } catch (SocketTimeoutException e) {
                sdkTracker.trackAndLogApiException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.NETWORK, Labels.Network.NETWORK_CALL, startTime, System.currentTimeMillis(), data, url, method, "SocketTimeoutException while calling api", e, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
                response = new JuspayHttpsResponse(-3, "Socket Timeout".getBytes(), null);
            } catch (IOException e) {
                sdkTracker.trackAndLogApiException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.NETWORK, Labels.Network.NETWORK_CALL, startTime, System.currentTimeMillis(), data, url, method, "IOException while calling api", e, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
                response = new JuspayHttpsResponse(-1, "Network Error".getBytes(), null);
            } catch (Exception e) {
                sdkTracker.trackAndLogApiException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.NETWORK, Labels.Network.NETWORK_CALL, startTime, System.currentTimeMillis(), data, url, method, "Exception while calling api", e, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
                byte[] responseBytes = new byte[0];
                if (e.getLocalizedMessage() != null) {
                    responseBytes = e.getLocalizedMessage().getBytes();
                }
                response = new JuspayHttpsResponse(-1, responseBytes, null);
            }
            if (response != null) {
                if (response.responsePayload != null) {
                    sdkTracker.trackApiCalls(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.NETWORK_CALL, response.responseCode, url, tag, startTime, System.currentTimeMillis(), data, new String(response.responsePayload), method, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
                } else {
                    sdkTracker.trackApiCalls(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.NETWORK_CALL, response.responseCode, url, tag, startTime, System.currentTimeMillis(), data, null, method, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
                }
                if (response.responseCode == -1 || response.responseCode == -2 || response.responseCode == -3) {
                    byte[] payload = "{}".getBytes(); // (apiResponse.responsePayload == null || apiResponse.responsePayload.length == 0)? "{}".getBytes() : apiResponse.responsePayload;
                    String base64Data = Base64.encodeToString(payload, Base64.NO_WRAP);
//                        JuspayLogger.sdkDebug("Response inserted: ", base64Data + " " + apiResponse.responseCode);

                    if (callback != null) {
                        String javascript;
                        try {
                            javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s','%s', '%s');", callback, "failure", base64Data, response.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), "", URLEncoder.encode(Arrays.toString(payload), "UTF-8").replace("+", "%20"));
                        } catch (UnsupportedEncodingException e) {
                            javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');", callback, "failure", base64Data, response.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP));
                        }
                        dynamicUI.addJsToWebView(javascript);
                    }
                } else {
                    String base64Data;
                    String urlEncodedData = "";
                    String base64Headers = "";
                    if (response.responsePayload == null) {
                        base64Data = "";
                    } else {
                        String payload = new String(response.responsePayload);
                        try {
                            String respData = new JSONObject(payload).toString();
                            juspayServices.sdkDebug("message", respData);
                            base64Data = Base64.encodeToString(respData.getBytes(), Base64.NO_WRAP);
                            urlEncodedData = URLEncoder.encode(respData, "UTF-8").replace("+", "%20");
                        } catch (Exception e) {
                            base64Data = Base64.encodeToString(payload.getBytes(), Base64.NO_WRAP);
                            try {
                                urlEncodedData = URLEncoder.encode(payload, "UTF-8").replace("+", "%20");
                            } catch (Exception f) {
                                //ignored
                            }
                            JuspayLogger.e(LOG_TAG, "This happened: ", e);
                        }
                    }
                    juspayServices.sdkDebug("Response inserted: ", base64Data + " " + response.responseCode);
                    // ------------------ Adding headers to the arguments ------------------------
                    if (response.headers != null) {
                        JSONObject payload = new JSONObject();

                        for (Map.Entry<String, List<String>> pair : response.headers.entrySet()) {
                            try {
                                payload.put(pair.getKey(), new JSONArray(pair.getValue()));
                            } catch (Exception ignored) {
                            }
                        }
                        try {
                            juspayServices.sdkDebug("headers", payload.toString());
                            base64Headers = Base64.encodeToString(payload.toString().getBytes(), Base64.NO_WRAP);
                        } catch (Exception e) {
                            JuspayLogger.e(LOG_TAG, "This happened: ", e);
                        }
                    }
                    juspayServices.sdkDebug("Headers inserted: ", base64Headers + " " + response.responseCode);

                    if (callback != null) {
                        String javascript = String.format("console.log(\"Reached js\",Date.now()); window.callUICallback('%s','%s','%s','%s','%s','%s', '%s');",
                                callback, "success", base64Data, response.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), base64Headers, urlEncodedData);


                        juspayServices.sdkDebug("Js inserted: ", javascript);
                        Log.i("CALLAPI","Called Resp -> " + System.currentTimeMillis());
                        Log.i("CALLAPI","Called postMessage for" + url + "  -> " + System.currentTimeMillis());
//                            dynamicUI.postMessage(String.format("{\"callback\" : \"%s\", \"args\" : [\"%s\",\"%s\",\"%s\",\"%s\",\"%s\", \"%s\"]}",callback, "success", base64Data, response.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), base64Headers, urlEncodedData));
                        dynamicUI.addJsToWebView(javascript);
                    }
                }
            } else {
                sdkTracker.trackApiCalls(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.NETWORK_CALL, -1, url, tag, startTime, System.currentTimeMillis(), data, "failure", method, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));

                if (callback != null) {
                    String base64Data = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s','%s');", callback, "failure", base64Data, -1, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), "%7B%7D");

                    dynamicUI.addJsToWebView(javascript);
                }
            }

        });
//        @SuppressLint("StaticFieldLeak") AsyncTask<Object, Object, Object> asyncTask = new AsyncTask<Object, Object, Object>() {
//            @Override
//            protected void onPostExecute(Object o) {
//                if (o != null) {
//                    JuspayHttpsResponse apiResponse = (JuspayHttpsResponse) o;
//                    if (apiResponse.responsePayload != null) {
//                        sdkTracker.trackApiCalls(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.NETWORK_CALL, apiResponse.responseCode, url, tag, startTime, System.currentTimeMillis(), data, new String(apiResponse.responsePayload), method, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
//                    } else {
//                        sdkTracker.trackApiCalls(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.NETWORK_CALL, apiResponse.responseCode, url, tag, startTime, System.currentTimeMillis(), data, null, method, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
//                    }
//                    if (apiResponse.responseCode == -1 || apiResponse.responseCode == -2 || apiResponse.responseCode == -3) {
//                        byte[] payload = "{}".getBytes(); // (apiResponse.responsePayload == null || apiResponse.responsePayload.length == 0)? "{}".getBytes() : apiResponse.responsePayload;
//                        String base64Data = Base64.encodeToString(payload, Base64.NO_WRAP);
////                        JuspayLogger.sdkDebug("Response inserted: ", base64Data + " " + apiResponse.responseCode);
//
//                        if (callback != null) {
//                            String javascript;
//                            try {
//                                javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s','%s', '%s');", callback, "failure", base64Data, apiResponse.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), "", URLEncoder.encode(Arrays.toString(payload), "UTF-8").replace("+", "%20"));
//                            } catch (UnsupportedEncodingException e) {
//                                javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');", callback, "failure", base64Data, apiResponse.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP));
//                            }
//                            dynamicUI.addJsToWebView(javascript);
//                        }
//                    } else {
//                        String base64Data;
//                        String urlEncodedData = "";
//                        String base64Headers = "";
//                        if (apiResponse.responsePayload == null) {
//                            base64Data = "";
//                        } else {
//                            String payload = new String(apiResponse.responsePayload);
//                            try {
//                                String respData = new JSONObject(payload).toString();
//                                juspayServices.sdkDebug("message", respData);
//                                base64Data = Base64.encodeToString(respData.getBytes(), Base64.NO_WRAP);
//                                urlEncodedData = URLEncoder.encode(respData, "UTF-8").replace("+", "%20");
//                            } catch (Exception e) {
//                                base64Data = Base64.encodeToString(payload.getBytes(), Base64.NO_WRAP);
//                                try {
//                                    urlEncodedData = URLEncoder.encode(payload, "UTF-8").replace("+", "%20");
//                                } catch (Exception f) {
//                                    //ignored
//                                }
//                                JuspayLogger.e(LOG_TAG, "This happened: ", e);
//                            }
//                        }
//                        juspayServices.sdkDebug("Response inserted: ", base64Data + " " + apiResponse.responseCode);
//                        // ------------------ Adding headers to the arguments ------------------------
//                        if (apiResponse.headers != null) {
//                            JSONObject payload = new JSONObject();
//
//                            for (Map.Entry<String, List<String>> pair : apiResponse.headers.entrySet()) {
//                                try {
//                                    payload.put(pair.getKey(), new JSONArray(pair.getValue()));
//                                } catch (Exception ignored) {
//                                }
//                            }
//                            try {
//                                juspayServices.sdkDebug("headers", payload.toString());
//                                base64Headers = Base64.encodeToString(payload.toString().getBytes(), Base64.NO_WRAP);
//                            } catch (Exception e) {
//                                JuspayLogger.e(LOG_TAG, "This happened: ", e);
//                            }
//                        }
//                        juspayServices.sdkDebug("Headers inserted: ", base64Headers + " " + apiResponse.responseCode);
//
//                        if (callback != null) {
//                            String javascript = String.format("" +
//                                            "console.log(\"Reached js\",Date.now()); window.callUICallback('%s','%s','%s','%s','%s','%s', '%s');",
//                                    callback, "success", base64Data, apiResponse.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), base64Headers, urlEncodedData);
//
//
//                            juspayServices.sdkDebug("Js inserted: ", javascript);
//                            Log.i("CALLAPI","Called Resp -> " + System.currentTimeMillis());
//                            Log.i("CALLAPI","Called postMessage for" + url + "  -> " + System.currentTimeMillis());
////                            dynamicUI.postMessage(String.format("{\"callback\" : \"%s\", \"args\" : [\"%s\",\"%s\",\"%s\",\"%s\",\"%s\", \"%s\"]}",callback, "success", base64Data, apiResponse.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), base64Headers, urlEncodedData));
//                            dynamicUI.addJsToWebView(javascript);
//                        }
//                    }
//                } else {
//                    sdkTracker.trackApiCalls(LogSubCategory.ApiCall.NETWORK, LogLevel.INFO, Labels.Network.NETWORK_CALL, -1, url, tag, startTime, System.currentTimeMillis(), data, "failure", method, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
//
//                    if (callback != null) {
//                        String base64Data = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
//                        String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s','%s');", callback, "failure", base64Data, -1, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), "%7B%7D");
//
//                        dynamicUI.addJsToWebView(javascript);
//                    }
//                }
//
//            }
//
//            @Override
//            protected JuspayHttpsResponse doInBackground(Object[] params) {
//                try {
//                    HashMap<String, String> headersMap = toMap(headers);
//                    HashMap<String, String> dataMap = shouldEncodeToFormData ? toMap(data) : null;
//
//                    if ("GET".equals(method)) {
//                        return new JuspayHttpsResponse(netUtils.doGet(url, headersMap, dataMap, jsonOptions, tag));
//                    } else if ("HEAD".equals(method)) {
//                        return new JuspayHttpsResponse(netUtils.doHead(url, headersMap, dataMap, jsonOptions, tag));
//                    } else if ("POST".equals(method)) {
//                        if (dataMap == null) {
//                            return new JuspayHttpsResponse(netUtils.postUrl(new URL(url), headersMap, data, jsonOptions, tag));
//                        } else {
//                            return new JuspayHttpsResponse(netUtils.postUrl(new URL(url), headersMap, dataMap, jsonOptions, tag));
//                        }
//                    } else if ("DELETE".equals(method)) {
//                        if (dataMap == null) {
//                            return new JuspayHttpsResponse(netUtils.deleteUrl(new URL(url), headersMap, data, jsonOptions, tag));
//                        } else {
//                            return new JuspayHttpsResponse(netUtils.deleteUrl(new URL(url), headersMap, dataMap, jsonOptions, tag));
//                        }
//                    } else if ("PUT".equals(method)) {
//                        return new JuspayHttpsResponse(netUtils.doPut(juspayServices.getContext(), new URL(url), data.getBytes(), headersMap, netUtils, jsonOptions, tag));
//                    }
//                    return null;
//                } catch (SSLHandshakeException e) {
//                    sdkTracker.trackAndLogApiException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.NETWORK, Labels.Network.NETWORK_CALL, startTime, System.currentTimeMillis(), data, url, method, "SSLHandshakeException while calling api", e, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
//                    return new JuspayHttpsResponse(-2, "SSL Handshake Failed".getBytes(), null);
//                } catch (SocketTimeoutException e) {
//                    sdkTracker.trackAndLogApiException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.NETWORK, Labels.Network.NETWORK_CALL, startTime, System.currentTimeMillis(), data, url, method, "SocketTimeoutException while calling api", e, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
//                    return new JuspayHttpsResponse(-3, "Socket Timeout".getBytes(), null);
//                } catch (IOException e) {
//                    sdkTracker.trackAndLogApiException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.NETWORK, Labels.Network.NETWORK_CALL, startTime, System.currentTimeMillis(), data, url, method, "IOException while calling api", e, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
//                    return new JuspayHttpsResponse(-1, "Network Error".getBytes(), null);
//                } catch (Exception e) {
//                    sdkTracker.trackAndLogApiException(LOG_TAG, LogCategory.API_CALL, LogSubCategory.ApiCall.NETWORK, Labels.Network.NETWORK_CALL, startTime, System.currentTimeMillis(), data, url, method, "Exception while calling api", e, jsonOptions.optJSONArray("channels"), jsonOptions.optJSONObject("rootLogFields"));
//                    byte[] responseBytes = new byte[0];
//                    if (e.getLocalizedMessage() != null) {
//                        responseBytes = e.getLocalizedMessage().getBytes();
//                    }
//                    return new JuspayHttpsResponse(-1, responseBytes, null);
//                }
//            }
//        };
//
//        asyncTask.executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR);
        return tag;
    }
            private HashMap<String, String> toMap(String jsonString) throws JSONException {
                HashMap<String, String> map = new HashMap<>();
                JSONObject json;
                try {
                    json = new JSONObject(jsonString);
                } catch (JSONException e) {
                    juspayServices.sdkDebug(LOG_TAG, "Not a json string. Passing as such");
                    return null;
                }
                Iterator<?> keys = json.keys();

                while (keys.hasNext()) {
                    String key = (String) keys.next();
                    String value = json.getString(key);
                    map.put(key, value);
                }

                return map;
            }

    @JavascriptInterface
    public void cancelAPI(String tag) {
        NetUtils.cancelAPICall(tag, juspayServices.getSdkTracker());
    }

    @JavascriptInterface
    public void writeFileToDisk(String data, String fileName, final String callback) {
        String response = juspayServices.getFileProviderService().writeFileToDisk(juspayServices.getContext(), data, fileName);
        invokeCallbackInDUIWebview(callback, response);
    }

    @JavascriptInterface
    public void openApp(String packageName, String payload, String action, int flag, int requestCode) {
        Intent i = new Intent();
        i.setPackage(packageName);
        i.setAction(action);
        i.setData(Uri.parse(payload));
        i.setFlags(flag);
        juspayServices.startActivityForResult(i, requestCode, null);
    }

    public long dateToMillisecond(String date) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd", Locale.getDefault());
        Date newDate;
        try {
            newDate = sdf.parse(date);
            if (newDate != null) {
                return newDate.getTime();
            }
        } catch (ParseException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Error in date to millis", e);
        }
        return 0;
    }

    @Nullable
    private DatePickerDialog newDialogWithoutDateField(final String callback) {
        final Calendar myCalendar = Calendar.getInstance();

        DatePickerDialog.OnDateSetListener onDateSelected = (view, year, monthOfYear, dayOfMonth) -> {
            String date = year + "/" + (monthOfYear + 1) + "/" + dayOfMonth;
            invokeCallbackInDUIWebview(callback, date);
        };

        DatePickerDialog.OnCancelListener onCancelListener = dialogInterface -> invokeCallbackInDUIWebview(callback, "NaN-NaN");

        DatePickerDialog.OnDismissListener onDismissListener = dialogInterface -> invokeCallbackInDUIWebview(callback, "NaN-NaN");

        DatePickerDialog dpd = null;
        if (activity != null) {
            dpd = new DatePickerDialog(activity, AlertDialog.THEME_HOLO_DARK, onDateSelected,
                    myCalendar.get(Calendar.YEAR),
                    myCalendar.get(Calendar.MONTH),
                    myCalendar.get(Calendar.DAY_OF_MONTH));
            dpd.setOnCancelListener(onCancelListener);
            dpd.setOnDismissListener(onDismissListener);

        }

        return dpd;
    }

    @JavascriptInterface
    public void startDatePicker(final String callback, final String minDate, final String maxDate) {
        ExecutorManager.runOnMainThread(() -> {
            DatePickerDialog dpd = newDialogWithoutDateField(callback);
            if (dpd == null) return;
            if (minDate != null && !minDate.isEmpty() && !minDate.equals("undefined")) {
                dpd.getDatePicker().setMinDate(dateToMillisecond(minDate));
            }
            if (maxDate != null && !maxDate.isEmpty() && !maxDate.equals("undefined")) {
                dpd.getDatePicker().setMaxDate(dateToMillisecond(maxDate));
            }
            View dayPicker = dpd.getDatePicker().findViewById(Resources.getSystem().getIdentifier("day", "id", "android"));
            if (dayPicker != null)
                dayPicker.setVisibility(View.GONE);

            dpd.show();
        });
    }

    @JavascriptInterface
    public String encryptRSA(String publicKey, String content) {
        byte[] encrypted = encryptRSAHelper(publicKey, content);
        return encrypted == null ? "" : Base64.encodeToString(encrypted, Base64.NO_WRAP);
    }

    public byte[] encryptRSAHelper(String publicKeyContent, String content) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            final String algorithm = "RSA/ECB/PKCS1Padding";

            publicKeyContent = publicKeyContent
                    .replace("-----BEGIN PUBLIC KEY-----\n", "")
                    .replace("-----END PUBLIC KEY-----", "");

            KeyFactory keyFactory = KeyFactory.getInstance("RSA");

            X509EncodedKeySpec keySpec = new X509EncodedKeySpec(Base64.decode(publicKeyContent, Base64.DEFAULT));
            Key publicKey = keyFactory.generatePublic(keySpec);

            Cipher cipher = Cipher.getInstance(algorithm);
            cipher.init(Cipher.ENCRYPT_MODE, publicKey);

            return cipher.doFinal(content.getBytes());
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception when encrypting using RSA", e);
        }

        return null;
    }

    @JavascriptInterface
    public void hideSoftInput() {
        if (activity != null && activity.getCurrentFocus() != null) {
            InputMethodManager inputMethodManager = (InputMethodManager) activity.getSystemService(Context.INPUT_METHOD_SERVICE);
            if (inputMethodManager != null) {
                inputMethodManager.hideSoftInputFromWindow(activity.getCurrentFocus().getWindowToken(), InputMethodManager.RESULT_UNCHANGED_SHOWN);
            }
        }
    }

    @JavascriptInterface
    public int cursorPosition(int id) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        try {
            if (activity != null) {
                EditText et = activity.findViewById(id);
                if (et != null) {
                    return et.getSelectionStart();
                }
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Cursor Position Exception", e);
        }
        return 0;
    }

    @JavascriptInterface
    public String getResourceByName(String name, String type, String packagename) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        String result = "0";
        try {
            int resourceId = juspayServices.getContext().getResources().getIdentifier(name, type, packagename);
            if (resourceId > 0) {
                result = super.getResourceById(resourceId);
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Get Resource Exception", e);
        }
        return result;
    }

    @JavascriptInterface
    public String getStatusBarHeight(String name, String type, String packagename) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        String result = "0";
        try {
            int resourceId = juspayServices.getContext().getResources().getIdentifier(name, type, packagename);
            if (resourceId > 0) {
                result = "" + juspayServices.getContext().getResources().getDimensionPixelSize(resourceId);
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Get Resource Exception", e);
        }
        return result;
    }

    @JavascriptInterface
    public double getDensity() {
        return (double) juspayServices.getContext().getResources().getDisplayMetrics().densityDpi / DisplayMetrics.DENSITY_DEFAULT;
    }

    @JavascriptInterface
    public void startLottieAnimation(final int id, final String animation, final boolean loop, final double min, final double max) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        ExecutorManager.runOnMainThread(() -> {
            LottieAnimationView animationView;
            try {
                if (activity == null) return;
                animationView = activity.findViewById(id);
                animationView.enableMergePathsForKitKatAndAbove(true);
                animationView.setAnimation(animation);
                animationView.setRepeatCount(loop ? ValueAnimator.INFINITE : 0);
                animationView.setMinAndMaxProgress((float) min, (float) max);

                animationView.playAnimation();
            } catch (Exception e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while playing Lottie animation", e);
            }
        });
    }

    @JavascriptInterface
    public String getSHA256Hash(@Nullable String data) {
        if (data == null) return null;

        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        // generate a hash
        MessageDigest digest;
        String hash;
        try {
            digest = MessageDigest.getInstance("SHA-256");
            digest.update(data.getBytes());
            hash = bytesToHexString(digest.digest());

            JuspayLogger.d(LOG_TAG, "result is " + hash);
            return hash;
        } catch (NoSuchAlgorithmException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception caught trying to SHA-256 hash", e);
        }

        return null;
    }

    @JavascriptInterface
    public String getDeviceInfo() {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        JSONObject session = juspayServices.getSessionInfo().getSessionData();
        try {
            session.put("android_id_raw", juspayServices.getSessionInfo().getAndroidId());
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while extracting android id", e);
            return getSessionInfo();
        }
        return session.toString();
    }

    @JavascriptInterface
    public void addCertificates(String json) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            JSONArray array = new JSONArray(json);
            for (int i = 0; i < array.length(); i++) {
                acceptedCerts.add(array.getString(i));
            }
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while manipulating JSON", e);
        }
    }

    @JavascriptInterface
    public String jweEncrypt(String data, String headers, String wrappingKey) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            return String.format("{\"error\":false,\"payload\":\"%s\"}", JOSEUtils.jweEncrypt(data, headers, Base64.decode(wrappingKey, Base64.NO_WRAP)));
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while trying to encrypt JSON Web Token", e);
            return String.format("{\"error\":true,\"payload\":\"%s\"}", e);
        }
    }

    @JavascriptInterface
    public String jwsSign(String data, String headers, String alias) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            KeyPair keyPair = EncryptionHelper.getKeyPair(alias);
            return String.format("{\"error\":false,\"payload\":\"%s\"}", JOSEUtils.jwsSign(data, headers, keyPair.getPrivate()));
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while trying to sign JSON Web Token", e);
            return String.format("{\"error\":true,\"payload\":\"%s\"}", e);
        }
    }

    @JavascriptInterface
    public String jweDecrypt(String cipher, String alias) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            KeyPair keyPair = EncryptionHelper.getKeyPair(alias);
            JSONObject result = new JSONObject();
            result.put("payload", JOSEUtils.jweDecrypt(cipher, keyPair.getPrivate()));
            result.put("error", false);

            return result.toString();
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while trying to decrypt JSON Web Token", e);
            return String.format("{\"error\":true,\"payload\":\"%s\"}", e);
        }
    }

    @JavascriptInterface
    public boolean jwsVerify(String signed, String publicKey) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            return JOSEUtils.jwsVerify(signed, Base64.decode(publicKey, Base64.NO_WRAP));
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while trying to verify JSON Web Token", e);
            return false;
        }
    }

    @Deprecated
    @JavascriptInterface
    public String rsaEncryption(String plainText, String algo, String pubKey) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        try {
            RSAPublicKey key = (RSAPublicKey) KeyFactory.getInstance("RSA")
                    .generatePublic(new X509EncodedKeySpec(Base64.decode(pubKey, Base64.NO_WRAP)));

            Cipher cipher = Cipher.getInstance(algo);
            cipher.init(Cipher.ENCRYPT_MODE, key);
            byte[] encryptedBytes = cipher.doFinal(plainText.getBytes());

            return String.format("{\"error\":false,\"payload\":\"%s\"}", Base64.encodeToString(encryptedBytes, Base64.NO_WRAP));
        } catch (Exception e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.JBRIDGE, "Exception while trying to encrypt using RSA", e);
            return String.format("{\"error\":true,\"payload\":\"%s\"}", e);
        }
    }

    @JavascriptInterface
    public void blurBackground(String viewId, String targetViewId, final int blurRadius) {

    }

    @JavascriptInterface
    public static String hmacDigest(String msg, String keyString, String algo) {
        String digest = null;
        try {
            SecretKeySpec key = new SecretKeySpec((keyString).getBytes(StandardCharsets.UTF_8), algo);
            Mac mac = Mac.getInstance(algo);
            mac.init(key);
            byte[] bytes = mac.doFinal(msg.getBytes(StandardCharsets.US_ASCII));
            StringBuilder hash = new StringBuilder();
            for (byte aByte : bytes) {
                String hex = Integer.toHexString(0xFF & aByte);
                if (hex.length() == 1) {
                    hash.append('0');
                }
                hash.append(hex);
            }
            digest = hash.toString();
        } catch (Exception ignored) {
        }
        return digest;
    }

    @JavascriptInterface
    public boolean isNoLimitsActivity() {
        if (activity == null) {
            return false;
        }
        int flg = activity.getWindow().getAttributes().flags;
        return (flg & WindowManager.LayoutParams.FLAG_LAYOUT_NO_LIMITS) == WindowManager.LayoutParams.FLAG_LAYOUT_NO_LIMITS;
    }

    @JavascriptInterface
    public String get3DS2SdkList() {
        JSONArray arr = new JSONArray();
        for (Map.Entry<String, HyperBridge> bridge : juspayServices.getJBridgeList().entrySet()) {
            if (bridge.getValue() instanceof ThreeDS2Bridge) {
                String sdkName = ((ThreeDS2Bridge) bridge.getValue()).getThreeDS2SdkName();
                arr.put(sdkName);
            }
        }
        return arr.toString();
    }

    @JavascriptInterface
    public void shareLink(String textTobeShared, String title, String cb) {
        Intent myIntent = new Intent(Intent.ACTION_SEND);
        myIntent.setType("text/plain");
        myIntent.putExtra(Intent.EXTRA_TEXT, textTobeShared);
        juspayServices.startActivityForResult(Intent.createChooser(myIntent, title), -1, null);
        invokeCallbackInDUIWebview(cb, "true");
    }

    @JavascriptInterface
    public void copyLink(String textTobeCopied, String label, String cb) {
        ClipboardManager clipboard = (ClipboardManager) juspayServices.getContext().getSystemService(Context.CLIPBOARD_SERVICE);
        ClipData clip = ClipData.newPlainText(label, textTobeCopied);
        clipboard.setPrimaryClip(clip);
        invokeCallbackInDUIWebview(cb, "true");
    }

    @JavascriptInterface
    public void attachBase64ImageToId(final String base64Data, final String id) {
        try {
            ImageView imgView = activity.findViewById(Integer.parseInt(id));
            byte[] decodedBytes = Base64.decode(base64Data, Base64.DEFAULT);
            Bitmap decodedBitmap = BitmapFactory.decodeByteArray(decodedBytes, 0, decodedBytes.length);
            imgView.setImageBitmap(decodedBitmap);
        } catch (Exception ignored) {
        }
    }
}
