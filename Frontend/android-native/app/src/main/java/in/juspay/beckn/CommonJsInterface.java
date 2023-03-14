package in.juspay.mobility;

import android.Manifest;
import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.Activity;

import android.app.AlertDialog;
import android.app.DatePickerDialog;
import android.app.PendingIntent;
import android.app.TimePickerDialog;
import android.content.ActivityNotFoundException;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.PorterDuff;
import android.graphics.Typeface;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.LayerDrawable;
import android.graphics.drawable.ShapeDrawable;
import android.graphics.drawable.shapes.RoundRectShape;
import android.graphics.pdf.PdfDocument;
import android.location.Address;
import android.location.Geocoder;
import android.location.Location;
import android.location.LocationManager;
import android.media.Image;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Looper;
import android.os.PowerManager;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import android.provider.Settings;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.ActionMode;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;
import android.view.inputmethod.InputMethodManager;
import android.webkit.JavascriptInterface;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.TimePicker;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.core.location.LocationManagerCompat;

import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.airbnb.lottie.LottieAnimationView;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationAvailability;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.LocationSettingsRequest;
import com.google.android.gms.location.LocationSettingsResult;
import com.google.android.gms.location.LocationSettingsStates;
import com.google.android.gms.location.LocationSettingsStatusCodes;
import com.google.android.gms.location.Priority;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.ButtCap;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.maps.model.Cap;
import com.google.android.gms.maps.model.CustomCap;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Dot;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.MapStyleOptions;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PatternItem;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
//import com.google.android.material.snackbar.Snackbar;
//import com.google.firebase.installations.FirebaseInstallations;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.security.auth.callback.Callback;
import java.io.ByteArrayOutputStream;
import java.lang.Math;

import in.juspay.mobility.utils.AcceptRejectOverlaySheetService;
import in.juspay.mobility.utils.CheckPermissionAutoStart;
import in.juspay.mobility.utils.CheckPermissionOverlay;
import in.juspay.mobility.utils.LocationService;
import in.juspay.mobility.utils.LocationUpdateAlarm;
import in.juspay.mobility.utils.LocationUpdateService;
import in.juspay.mobility.utils.NotificationUtils;
import in.juspay.mobility.utils.OtpUtils;
import in.juspay.hypersdk.HyperFragment;
import in.juspay.hypersdk.core.JBridge;
import in.juspay.hypersdk.core.JuspayDuiHook;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.utils.network.JuspayHttpResponse;
import in.juspay.hypersdk.utils.network.NetUtils;
import in.juspay.mystique.DynamicUI;

import static android.Manifest.permission.ACCESS_BACKGROUND_LOCATION;
import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static androidx.core.app.ActivityCompat.startActivityForResult;
import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static android.view.View.LAYER_TYPE_SOFTWARE;

public class CommonJsInterface extends JBridge {


    private static final String LOG_TAG = "Beckn_JsInterface";
    private static final String LOCATE_ON_MAP = "LocateOnMap";
    private static final String DISCOUNT_END = "Discount End Date";
    private static final String DISCOUNT_START = "Discount Start Date";
    private static final String CURRENT_LOCATION = "ic_customer_current_location";
    private static final String CURRENT_LOCATION_LATLON = "Current Location";
    private Activity activity;
    private JuspayServices juspayServices;
    private Context context;
    private DynamicUI dynamicUI;
    private GoogleMap googleMap;
    private static final int MAP_ZOOM_LEVEL = 15;
    private FusedLocationProviderClient client;
    private Marker userPositionMarker;
    private HashMap<String,JSONObject> markersElement = new HashMap<String,JSONObject>();// = new JSONObject();
    private JSONObject markers = new JSONObject();
    private double lastLatitudeValue;
    private double lastLongitudeValue;
    private int lastFocusedEditView;
    private Polyline polylines = null; //TODO Implement polylies using key-value pair
    private FirebaseAnalytics mFirebaseAnalytics;
    private static Calendar current = Calendar.getInstance();
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    public static final int REQUEST_CALL = 8;
    public static String phoneNumber;
    public static String invoice =null;
    public static boolean permissionCheck = false;
    public static String storeCallBackPopUp = null;
    private LottieAnimationView animationView;


    public CommonJsInterface(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super(juspayServices, activity, fragment);
        this.context = activity;
        this.dynamicUI = juspayServices.getDynamicUI();
        this.activity = activity;
        this.juspayServices = juspayServices;
        client = LocationServices.getFusedLocationProviderClient(context);
//        polylines = new ArrayList<>();
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
    }

   CommonJsInterface(@NonNull JuspayServices juspayServices, Activity activity, @Nullable HyperFragment browserFragment) {
       super(juspayServices, activity, browserFragment);
       try {
           JSONObject headerObj = new JSONObject();
           headerObj.put("X-Client-Id", context.getResources().getString(R.string.service));
           boolean b = setAnalyticsHeader(headerObj.toString());
       } catch (Exception e) {
           e.printStackTrace();
       }
       this.juspayServices = juspayServices;
       this.activity = activity;
       this.dynamicUI = juspayServices.getDynamicUI();
   }

    @JavascriptInterface
    public void toast(String msg) {
        Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
    }

    @JavascriptInterface
    public void setKeysInSharedPrefs(String key, String value) {
        KeyValueStore.write(juspayServices, key, value);
        setEnvInNativeSharedPrefKeys(key, value);
    }

    /* The below commented code is for opening browser inApp for payments
    Commenting the code for future use */

    /*
    private static String callbackGlb = null;

    @JavascriptInterface
    public void loadACSCallback(final String key,final String value,final String service) {
        System.out.println("--===-=---=-=-=-=----->> HERE IN LOAD CALLBACK ACS");
        System.out.println(key);
        System.out.println(value);
        if (dynamicUI != null && callbackGlb !=null) {
            Log.e(LOG_TAG, "Dynamic UI is not null");
            String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callbackGlb, key, value, service);
            Log.e(LOG_TAG, javascript);
            dynamicUI.addJsToWebView(javascript);
        } else {
            Log.e(LOG_TAG, "Dynamic UI is null");
        }
    }

    @JavascriptInterface
    public void loadACS(final String url, final String webViewId, final String acsCode,final String callback){
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                callbackGlb = callback;
                System.out.println("WEBVIEW ID ->>"+webViewId);
                final WebView webView1 = activity.findViewById(Integer.parseInt(webViewId));
                webView1.getSettings().setJavaScriptEnabled(true);
                webView1.getSettings().setAllowContentAccess(true);
                webView1.getSettings().setDomStorageEnabled(true);
                webView1.getSettings().setAllowUniversalAccessFromFileURLs(false);
                webView1.getSettings().setAllowFileAccessFromFileURLs(false);

                webView1.addJavascriptInterface(new CommonJsInterface(juspayServices,activity,browserFragment), "JBridge");
                webView1.setWebViewClient(new WebViewClient(){
                    @Nullable
                    @Override
                    public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {
                        return super.shouldInterceptRequest(view, request);
                    }

                    @Override
                    public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
                        if (dynamicUI != null) {
                            loadACSCallback("STATUS","NETWORK_ERROR_D", String.valueOf(errorCode));
                        } else {
                            Log.e(LOG_TAG, "Dynamic UI is null");
                        }
                    }

                    @Override
                    public void onReceivedHttpError(WebView view, WebResourceRequest request, WebResourceResponse errorResponse) {
                        super.onReceivedHttpError(view, request, errorResponse);
                    }

                    @Override
                    public void onReceivedError(WebView view, WebResourceRequest request, WebResourceError error) {
                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                            if (dynamicUI != null) {
                                loadACSCallback("STATUS","NETWORK_ERROR", String.valueOf(error.getErrorCode()));
                            } else {
                                Log.e(LOG_TAG, "Dynamic UI is null");
                            }
                        }
                    }

                    @Override
                    public void onPageStarted(WebView view, String url, Bitmap favicon) {
                        if (dynamicUI != null){
                            loadACSCallback("URL_LOADING",url,"STARTED");
                            loaderText("Please Wait", "While we are processing your payment");
                            toggleLoader(true);
                        } else {
                            Log.e(LOG_TAG, "Dynamic UI is null");
                        }
                    }

                    @Override
                    public void onPageFinished(WebView view, String url) {
                        String scriptData = acsCode;
                        Log.e(LOG_TAG, "onPageFinished" + scriptData);

                        loadACSCallback("URL_LOADING", url, "LOADED");
                        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                            view.evaluateJavascript("javascript:(" + scriptData + ")()", null);
                        }else{
                            // TODO check this is required
                            view.loadUrl("javascript:("+scriptData + ")()");
                        }
                        new Timer().schedule(new TimerTask() {
                            @Override
                            public void run() {
                                toggleLoader(false);
                            }
                        }, 600L);
                    }
                });
                webView1.loadUrl(url);
            }
        });
    }
    */

    @JavascriptInterface
    public void setEnvInNativeSharedPrefKeys(String key, String value){
        SharedPreferences sharedPref = context.getSharedPreferences(
                activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString(key, value);
        editor.apply();

    }

    @JavascriptInterface
    public String getPayloadSignature(String payload) {
        return ((MainActivity) activity).getPayloadSignature(payload);
    }

    @JavascriptInterface
    public void closeApp() {
        activity.finish();
    }

    @JavascriptInterface
    public void minimizeApp() {
        Intent startMain = new Intent(Intent.ACTION_MAIN);
        startMain.addCategory(Intent.CATEGORY_HOME);
        startMain.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        activity.startActivity(startMain);
    }

    @JavascriptInterface
    public String getKeysInSharedPrefs(String key) {
        return KeyValueStore.read(juspayServices, key, "__failed");
    }


    @JavascriptInterface
    public String getKeyInNativeSharedPrefKeys(String key) {
        SharedPreferences sharedPref = context.getSharedPreferences(
                            context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        return sharedPref.getString(key,"__failed");
    }

    @JavascriptInterface
    public void removeKeysInSharedPrefs(String key) {
        KeyValueStore.remove(juspayServices, key);
        removeKeysInNativeSharedPrefs(key);
    }

    private void removeKeysInNativeSharedPrefs(String key){
        if (!key.equals(""))
        {
            SharedPreferences sharedPref = context.getSharedPreferences(
                    activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            SharedPreferences.Editor editor = sharedPref.edit();
            editor.remove(key);
            editor.apply();
        }
    }

    @JavascriptInterface
    public void setShadow(final int level,
                          final String _viewIds,
                          final String _backgroundColors,
                          String _blurValues,
                          final String _shadowColors,
                          String _dxs, String _dys,
                          String _spreads,
                          final String _factors) {
        try {
            final JSONArray viewIds;
            viewIds = new JSONArray(_viewIds);
            final JSONArray blurValues = new JSONArray(_blurValues);
            final JSONArray dxs = new JSONArray(_dxs);
            final JSONArray dys = new JSONArray(_dys);
            final JSONArray spreads = new JSONArray(_spreads);
            final JSONArray backgroundColors = new JSONArray(_backgroundColors);
            final JSONArray shadowColors = new JSONArray(_shadowColors);
            final JSONArray factors = new JSONArray(_factors);
            if (activity != null) activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    View[] shadowView = new View[blurValues.length()];
                    try {
                        for (int i = 0; i < blurValues.length(); i++)
                            shadowView[i] = activity.findViewById(viewIds.getInt(i));
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
                        View view;
                        if (shadowView[0] != null) {
                            View parent = (View) shadowView[0].getParent();
                            View gParent = (View) parent.getParent();
                            // TODO: Generalize for n levels. But as of now, there is no requirement for more than 2 levels.
                            if (level == 2) {
                                view = gParent;
                            } else {
                                view = parent;
                            }
                            try {
                                view.setBackground(generateBackgroundWithShadow(level, shadowView, backgroundColors, blurValues, shadowColors, dxs, dys, spreads, factors));
                            } catch (Exception e) {
                            }
                        }
                    }
                }
            });
        } catch (Exception e) {
        }
    }
    private Drawable generateBackgroundWithShadow(int level,
                                                  View[] view,
                                                  JSONArray backgroundColor,
                                                  JSONArray blurValue,
                                                  JSONArray shadowColor,
                                                  JSONArray dxValue,
                                                  JSONArray dyValue,
                                                  JSONArray spreadValue,
                                                  JSONArray factor) {
        View parent = (View) view[0].getParent();
        View gParent = null;
        //  TODO: Generalize for n levels. But there is no requirement for more than 2 levels.
        if (level == 2) {
            gParent = (View) parent.getParent();
        }
        Drawable[] drawables = new Drawable[blurValue.length()];
        try {
            for (int i = 0; i < blurValue.length(); i++) {
                float cornerRadiusValue = blurValue.getInt(i) * (float) factor.getDouble(i);
                int shadowColorValue = Color.parseColor(shadowColor.getString(i));
                int backgroundColorValue = Color.parseColor(backgroundColor.getString(i));
                float[] outerRadius = {cornerRadiusValue, cornerRadiusValue, cornerRadiusValue,
                        cornerRadiusValue, cornerRadiusValue, cornerRadiusValue, cornerRadiusValue,
                        cornerRadiusValue};
                ShapeDrawable shapeDrawable = new ShapeDrawable();
                shapeDrawable.getPaint().setColor(backgroundColorValue);
                shapeDrawable.getPaint().setShadowLayer(cornerRadiusValue, dxValue.getInt(i) * (float) factor.getDouble(i), dyValue.getInt(i) * (float) factor.getDouble(i), shadowColorValue);
                parent.setLayerType(LAYER_TYPE_SOFTWARE, null);
                shapeDrawable.setShape(new RoundRectShape(outerRadius, null, null));
                drawables[i] = shapeDrawable;
            }
        } catch (JSONException e) {
        }
        LayerDrawable drawable = new LayerDrawable(drawables);
        try {
            for (int i = 0; i < drawables.length; i++) {
                int left = view[i].getLeft();
                int top = view[i].getTop();
                int right = parent.getWidth() - view[i].getRight();
                int bottom = parent.getHeight() - view[i].getBottom();
                if (level == 2 && gParent != null) {
                    left += parent.getLeft();
                    top += parent.getTop();
                    right += gParent.getWidth() - parent.getRight();
                    bottom += gParent.getHeight() - parent.getBottom();
                }
                if (activity != null) {
                    left = left - (int) dpToPx(spreadValue.getInt(i), activity);
                    top = top - (int) dpToPx(spreadValue.getInt(i), activity);
                    right = right - (int) dpToPx(spreadValue.getInt(i), activity);
                    bottom = bottom - (int) dpToPx(spreadValue.getInt(i), activity);
                }
                drawable.setLayerInset(i, left, top, right, bottom);
            }
        } catch (JSONException e) {
        }
        return drawable;
    }

    @JavascriptInterface
    public void callApi(final String method, final String url, final String data, final String headers, final boolean isSSLPinned, final String callback) {
        System.out.println("ListAPIUrl1 " + url);
        callApi(method, url, data, headers, true, isSSLPinned, callback);
    }

    @JavascriptInterface
    public void callAPI(final String method, final String url, final String data, final String headers, final boolean isSSLPinned, final String callback) {
        System.out.println("ListAPIUrl2 " + url);
        callApi(method, url, data, headers, isSSLPinned, callback);
    }


    @JavascriptInterface
    public void callApi(final String method, final String url, final String data, final String headers, final boolean shouldEncodeToFormData, final boolean isSSLPinned, final String callback) {
        System.out.println("ListAPIUrl3 " + url);
        final NetUtils netUtils;

        try {
            netUtils = new NetUtils(0, 0, isSSLPinned);
        } catch (Exception e) {
            return;
        }


        @SuppressLint("StaticFieldLeak") AsyncTask<Object, Object, Object> asyncTask = new AsyncTask<Object, Object, Object>() {
            @Override
            protected void onPostExecute(Object o) {
                if (o != null) {
                    JuspayHttpResponse apiResponse = (JuspayHttpResponse) o;
                    Log.d("api response ----", apiResponse.toString());
                    if (apiResponse.responseCode == -1 || apiResponse.responseCode == -2) {
                        byte[] payload = "{}".getBytes();
                        String base64Data = Base64.encodeToString(payload, Base64.NO_WRAP);

                        String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');",
                                callback, "failure", base64Data, apiResponse.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP));

                        if (dynamicUI != null) {
                            dynamicUI.addJsToWebView(javascript);
                        }
                    } else {
                        String base64Data = "";
                        String base64Headers = "";
                        if (apiResponse.responsePayload == null) {
                            base64Data = "";
                        } else {
                            String payload = new String(apiResponse.responsePayload);
                            Log.d(LOG_TAG, "onPostExecute: " + payload);

                            try {
                                base64Data = Base64.encodeToString(new JSONObject(payload).toString().getBytes(), Base64.NO_WRAP);
                            } catch (JSONException e) {
                                try {
                                    base64Data = Base64.encodeToString(new JSONArray(payload).toString().getBytes(), Base64.NO_WRAP);
                                } catch (JSONException ee) {
                                    base64Data = Base64.encodeToString(payload.getBytes(),Base64.NO_WRAP);
                                    ee.printStackTrace();
                                }
                            }
                        }
                        // -----------------   - Adding headers to the arguments ------------------------
                        if (apiResponse.headers != null) {
                            JSONObject payload = new JSONObject();
                            Iterator it = apiResponse.headers.entrySet().iterator();
                            while (it.hasNext()) {
                                Map.Entry pair = (Map.Entry) it.next();
                                try {
                                    payload.put((String) pair.getKey(), new JSONArray((List<String>) pair.getValue()));
                                } catch (Exception ignored) {
                                }
                            }
                            try {
                                base64Headers = Base64.encodeToString(payload.toString().getBytes(), Base64.NO_WRAP);
                            } catch (Exception e) {
                            }
                        }
                        Log.d(LOG_TAG, "onPostExecute: base64Data = " + base64Data);
                        String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s','%s');",
                                callback, "success", base64Data, apiResponse.responseCode, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP), base64Headers);

                        if (dynamicUI != null) {
                            dynamicUI.addJsToWebView(javascript);
                        }
                    }
                } else {
                    String base64Data = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');", callback, "failure", base64Data, -1, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP));

                    if (dynamicUI != null) {
                        dynamicUI.addJsToWebView(javascript);
                    }
                }
            }

            @Override
            protected JuspayHttpResponse doInBackground(Object[] params) {
                try {
                    HashMap<String, String> headersMap = toMap(headers);
                    HashMap<String, String> dataMap = shouldEncodeToFormData ? toMap(data) : null;

                    if ("GET".equals(method)) {
                        return new JuspayHttpResponse(netUtils.doGet(url, headersMap, dataMap));
                    } else if ("POST".equals(method)) {
                        if (dataMap == null) {
                            return new JuspayHttpResponse(netUtils.postUrl(new URL(url), headersMap, data));
                        } else {
                            return new JuspayHttpResponse(netUtils.postUrl(new URL(url), headersMap, dataMap));
                        }
                    } else if ("DELETE".equals(method)) {
                        if (dataMap == null) {
                            return new JuspayHttpResponse(netUtils.deleteUrl(new URL(url), headersMap, data));
                        } else {
                            return new JuspayHttpResponse(netUtils.deleteUrl(new URL(url), headersMap, dataMap));
                        }
                    }
                    return null;
                } catch (IOException e) {
                    return new JuspayHttpResponse(-1, "Network Error".getBytes(), null);
                } catch (Exception e) {
                    return new JuspayHttpResponse(-1, e.getLocalizedMessage().getBytes(), null);
                }
            }

            private HashMap<String, String> toMap(String jsonString) throws JSONException {
                HashMap<String, String> map = new HashMap<>();
                JSONObject json;
                try {
                    json = new JSONObject(jsonString);
                } catch (JSONException e) {
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
        };

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
            asyncTask.executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR);
        } else {
            asyncTask.execute(null, null, null);
        }
    }

    @JavascriptInterface
    public void hideKeyboardOnNavigation(boolean permission) {
        View view = this.activity.getCurrentFocus();
        if (view == null) {
            view = new View(this.activity);
        }
        InputMethodManager imm = (InputMethodManager) this.context.getSystemService(Activity.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(view.getWindowToken(), 0);
    }

    static String storeCallB = "RefreshPage";
    @JavascriptInterface
    public void storeCallBackForNotification(String callback) {
        storeCallB = callback;
    }


    public static void callingStoreCall(DynamicUI dynamicUII){
        if (dynamicUII != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s');",
                    storeCallB);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void setStoreCallBackPopUp(String callback) {
        storeCallBackPopUp = callback;
    }

    @JavascriptInterface
    public void deletePopUpCallBack(String dummy) {
        System.out.println("delete popup callback");
        storeCallBackPopUp = null;
        MainActivity.getInstance().stopAllocationNotification();
    }

    public static void callingStoreCallBackPopUp (DynamicUI dynamicUII, JSONObject entity_payload){
        if (dynamicUII != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBackPopUp, entity_payload.toString());
            dynamicUII.addJsToWebView(javascript);
        }
    }
    static String storeCallBCustomer = "RefreshPagee";
    @JavascriptInterface
    public void storeCallBackCustomer(String callback) {
        storeCallBCustomer = callback;
    }


    public static void callingStoreCallCustomer(DynamicUI dynamicUII, String notificationType){
        if (dynamicUII != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
            storeCallBCustomer,notificationType);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    static String storeCallBTime = "TimeUpdate";
    @JavascriptInterface
    public void storeCallBackTime(String callback) {
        storeCallBTime = callback;
    }


    public static void callingStoreCallBackTime(DynamicUI dynamicUII, String time){
        if (dynamicUII != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
            storeCallBTime,time);
            System.out.println("time CommonJsInterface "+time);
            System.out.println("time javascript "+javascript);
            Log.d(LOG_TAG, javascript);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void sharePass(String msg) {
        Intent sendIntent = new Intent();

        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.putExtra(Intent.EXTRA_TEXT, msg);
        sendIntent.setType("text/plain");

        Intent shareIntent = Intent.createChooser(sendIntent, null);

        activity.startActivity(shareIntent);
    }

    @JavascriptInterface
    public void toggleLoader(final boolean visible) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                View loader = activity.findViewById(R.id.loaderLayout);
                if (visible) {
                    loader.setVisibility(View.VISIBLE);
                    loader.setOnClickListener(new View.OnClickListener() {
                        @Override
                        public void onClick(View v) { // Added this to prevent invisible touches through the loader
                            System.out.println("LOADER CLICKED");
                        }
                    });
                } else {
                    loader.setVisibility(View.GONE);
                }
            }
        });
    }

    @JavascriptInterface
    public String getVersionName (){
        PackageManager manager = context.getPackageManager();
        PackageInfo info = null;
        try {
            info = manager.getPackageInfo(context.getPackageName(), PackageManager.GET_ACTIVITIES);
        } catch (PackageManager.NameNotFoundException e) {
            e.printStackTrace();
        }
        // Toast.makeText(context,
        //     "PackageName = " + info.packageName + "\nVersionCode = "
        //     + info.versionCode + "\nVersionName = ", Toast.LENGTH_SHORT).show();
        return info.versionName;
    }

    @JavascriptInterface
    public int getVersionCode (){
        PackageManager manager = context.getPackageManager();
        PackageInfo info = null;
        try {
            info = manager.getPackageInfo(context.getPackageName(), PackageManager.GET_ACTIVITIES);
        } catch (PackageManager.NameNotFoundException e) {
            e.printStackTrace();
        }
        // Toast.makeText(context,
        //     "PackageName = " + info.packageName + "\nVersionCode = "
        //     + info.versionCode + "\nVersionName = ", Toast.LENGTH_SHORT).show();
        return info.versionCode;
    }

    @JavascriptInterface
    public String getManufacturerName () {
        return Build.MANUFACTURER;
    }

    @JavascriptInterface
    public int getAndroidVersion (){
        return Build.VERSION.SDK_INT;
    }
    @JavascriptInterface
    public String getPackageName (){
        PackageManager manager = context.getPackageManager();
        PackageInfo info = null;
        try {
            info = manager.getPackageInfo(context.getPackageName(), PackageManager.GET_ACTIVITIES);
        } catch (PackageManager.NameNotFoundException e) {
            e.printStackTrace();
        }
        // Toast.makeText(context,
        //     "PackageName = " + info.packageName + "\nVersionCode = "
        //     + info.versionCode + "\nVersionName = ", Toast.LENGTH_SHORT).show();
        return info.packageName;
    }

    @JavascriptInterface
    public void setFCMToken(final String callback) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                FirebaseMessaging.getInstance().getToken()
                        .addOnCompleteListener(new OnCompleteListener<String>() {
                            @Override
                            public void onComplete(@NonNull Task<String> task) {
                                if (!task.isSuccessful()) {
                                    Log.w(LOG_TAG, "Fetching FCM registration token failed", task.getException());
                                    return;
                                }
                                // Get new FCM registration token
                                String token = task.getResult();
                                // Log and toast
                                Log.d(LOG_TAG, "TOKEN TOKEN: ");
                                Log.d(LOG_TAG, token);
                                SharedPreferences sharedPref = context.getSharedPreferences(
                                        context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                                SharedPreferences.Editor editor = sharedPref.edit();
                                editor.putString("FCM_TOKEN", token);
                                editor.apply();
                                setKeysInSharedPrefs("FCM_TOKEN", token);
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                callback,token);
                                if (dynamicUI != null) {
                                    dynamicUI.addJsToWebView(javascript);
                                }
                            }
                        });
            }
        });
    }

    @JavascriptInterface
    public void openWhatsAppSupport (String contactNumber){
        String url = "https://api.whatsapp.com/send?phone="+contactNumber;
        Intent i = new Intent(Intent.ACTION_VIEW);
        i.setData(Uri.parse(url));
        context.startActivity(i);
    }

    @JavascriptInterface
    public void loaderText(final String mainMsg, final String subMsg) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                TextView mainloaderText = activity.findViewById(R.id.loaderMainText);
                TextView subloaderText = activity.findViewById(R.id.loaderSubText);
                mainloaderText.setText(mainMsg);
                subloaderText.setText(subMsg);
            }
        });
    }

    @JavascriptInterface
    public void timePicker(final String callback) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                final Calendar c = Calendar.getInstance();
                int hour = c.get(Calendar.HOUR_OF_DAY);
                int minute = c.get(Calendar.MINUTE);
                Log.e(LOG_TAG, "Time picker called");
                TimePickerDialog timePickerDialog = new TimePickerDialog(activity, new TimePickerDialog.OnTimeSetListener() {
                    @Override
                    public void onTimeSet(TimePicker timePicker, int hourOfDay, int minute) {
                        if (dynamicUI != null) {
                            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d,%d);",
                                callback, hourOfDay, minute);
                            dynamicUI.addJsToWebView(javascript);
                        }
                    }
                }, hour, minute, false);
                timePickerDialog.show();
            }
        });
    }

    @JavascriptInterface
    public void datePicker(final String callback,String label) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                final Calendar c = Calendar.getInstance();
                int mYear = c.get(Calendar.YEAR);
                int mMonth = c.get(Calendar.MONTH);
                int mDate = c.get(Calendar.DATE);

                DatePickerDialog datePickerDialog = new DatePickerDialog(activity, new DatePickerDialog.OnDateSetListener() {
                    @Override
                    public void onDateSet(DatePicker datePicker, int year, int month, int date) {
                        if (dynamicUI != null && callback != null) {
                            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d,%d,%d);",
                                callback, year, month, date);
                            dynamicUI.addJsToWebView(javascript);
                        }
                    }
                }, mYear, mMonth, mDate);

                switch (label){
                    case DatePickerLabels.MINIMUM_EIGHTEEN_YEARS :
                        Calendar maxDateDOB = Calendar.getInstance();
                        maxDateDOB.set(Calendar.DAY_OF_MONTH, mDate);
                        maxDateDOB.set(Calendar.MONTH, mMonth);
                        maxDateDOB.set(Calendar.YEAR, mYear - 18);
                        datePickerDialog.getDatePicker().setMaxDate(maxDateDOB.getTimeInMillis());
                        break;
                    case DatePickerLabels.MAXIMUM_PRESENT_DATE:
                        datePickerDialog.getDatePicker().setMaxDate(System.currentTimeMillis()-1000);
                        break;
                }

                datePickerDialog.show();
            }
        });
    }

    static String storeCallBDriverLocationPermission = "storeCallBack";
    static String storeCallBOverlayPermission = "storeCallBackOverLay";
    static String storeCallBInternetAction = "storeCallBackInternet";
    static String storeCallBackImageUpload = "imageUpload";
    static String storeCallBBatteryUsagePermission = "";


    @JavascriptInterface
    public void storeCallBackDriverLocationPermission(String callback) {
        System.out.println("CommonJsInterface storecallBackDriverLocationPermission()");
        storeCallBDriverLocationPermission = callback;
    }

    @JavascriptInterface
    public void storeCallBackImageUpload(String callback) {
        System.out.println("CommonJsInterface ()");
        storeCallBackImageUpload = callback;
    }

    @JavascriptInterface
    public void storeCallBackInternetAction(String callback) {
        System.out.println("CommonJsInterface storeCallBackInternetAction()");
        storeCallBInternetAction = callback;
    }


    @JavascriptInterface
    public void storeCallBackOverlayPermission(String callback) {
        System.out.println("CommonJsInterface storeCallBackOverlayPermission()");
        storeCallBOverlayPermission = callback;
    }

    @JavascriptInterface
    public void storeCallBackBatteryUsagePermission(String callback) {
        System.out.println("CommonJsInterface storeCallBackOverlayPermission()");
        storeCallBBatteryUsagePermission = callback;
    }

    public static void callingStoreCallBackBatteryUsagePermission(DynamicUI dynamicUII, String isPermission){
        if (dynamicUII != null && !storeCallBBatteryUsagePermission.equals("")) {
            System.out.println("CommonJsInterface callingStoreCallBackBatteryUsagePermission()");
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBBatteryUsagePermission, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallBackDriverLocationPermission(DynamicUI dynamicUII, String isPermission){
        if (dynamicUII != null && !storeCallBDriverLocationPermission.equals("storeCallBack")) {
            System.out.println("CommonJsInterface callinfStoreCallBackDriverLocationPermission()");
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBDriverLocationPermission, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallImageUpload(DynamicUI dynamicUII, String stringImage,String imageName){
        System.out.println("zxc callingStoreCallImageUpload");
        if (dynamicUII != null) {
            if (storeCallBackImageUpload == "imageUpload"){
                System.out.println("zxc callback not setted");
            }
            System.out.println("zxc stringImage" + stringImage);
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                    storeCallBackImageUpload,stringImage, imageName);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallBackInternetAction(DynamicUI dynamicUII, String isPermission){
        if (dynamicUII != null && !storeCallBInternetAction.equals("storeCallBackInternet")) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
            storeCallBInternetAction, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallBackOverlayPermission(DynamicUI dynamicUII, String isPermission){
        if (dynamicUII != null && !storeCallBOverlayPermission.equals("storeCallBackOverLay")) {
            System.out.println("CommonJsInterface callingStoreCallBackOverlayPermission()");
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBOverlayPermission, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void requestBatteryPermission(){
        try {
            if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                Intent intent = new Intent(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
                Uri uri = Uri.fromParts("package", getPackageName(), null);
                intent.setData(uri);
                String packageName = context.getPackageName();
                PowerManager pm = (PowerManager) context.getSystemService(android.content.Context.POWER_SERVICE);
                if (pm.isIgnoringBatteryOptimizations(packageName)) {
                    intent.setAction(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
                }
                else {
                    intent.setAction(Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
                }
                context.startActivity(intent);
            }
        }catch(ActivityNotFoundException e){
            e.printStackTrace();
            Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
            Uri uri = Uri.fromParts("package", getPackageName(), null);
            intent.setData(uri);
            context.startActivity(intent);
        }
    }

    @JavascriptInterface
    public boolean isBatteryPermissionEnabled(){
        PowerManager powerManager = (PowerManager) activity.getSystemService(Context.POWER_SERVICE);
        return (powerManager.isIgnoringBatteryOptimizations(context.getPackageName()));
    }

    @JavascriptInterface
    public void disableActionEditText( final String id)
    {
        EditText editText = activity.findViewById(Integer.parseInt(id));
        if (editText!= null) {
            editText.setCustomSelectionActionModeCallback(new ActionMode.Callback() {
                @Override
                public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
                    return false;
                }

                @Override
                public void onDestroyActionMode(ActionMode mode) {
                }

                @Override
                public boolean onCreateActionMode(ActionMode mode, Menu menu) {
                    return false;
                }

                @Override
                public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
                    return false;
                }
            });
            editText.setLongClickable(false);
            editText.setTextIsSelectable(false);
      }
    }

    @JavascriptInterface
    public void startLocationPollingAPI() {
        startLocationUpdates();
        Intent locationUpdateService = new Intent(activity, LocationUpdateService.class);
        locationUpdateService.putExtra("action", "start");
        System.out.println( "In startLocationPollingAPI intent" + locationUpdateService);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            System.out.println( "startForegroundService" + "env");
            activity.startForegroundService(locationUpdateService);
        } else {
            System.out.println( "startService" + "env");
            activity.startService(locationUpdateService);
        }

        //start alarmManager
        LocationUpdateAlarm locationUpdateAlarm = new LocationUpdateAlarm();
        locationUpdateAlarm.startAlarm(context);
    }

    @JavascriptInterface
    public void stopLocationPollingAPI() {
        Intent locationUpdateService = new Intent(activity, LocationUpdateService.class);
        locationUpdateService.putExtra("action", "stop");
        activity.startService(locationUpdateService);

        Intent popUpService = new Intent(context, AcceptRejectOverlaySheetService.class);
        context.stopService(popUpService);
        Log.i(LOG_TAG, "stopLocationPollingAPI " );

        //cancel alarmManager
        LocationUpdateAlarm locationUpdateAlarm = new LocationUpdateAlarm();
        locationUpdateAlarm.stopAlarm(context);
    }

    //to check if works fine
    @JavascriptInterface
    public boolean isLocationPermissionEnabled() {
        return !(ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED);
    }

    @JavascriptInterface
    public boolean isOverlayPermissionEnabled() {
        return Settings.canDrawOverlays(context);
    }

    @JavascriptInterface
    public boolean isLocationEnabled() {
        System.out.println("CommonJsInterface isLocationEnabled()");
        LocationManager locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        return locationManager != null && LocationManagerCompat.isLocationEnabled(locationManager);
    }

    private void startLocationUpdates() {
        LocationRequest lReq = LocationRequest.create();
        lReq.setPriority(Priority.PRIORITY_HIGH_ACCURACY);
        lReq.setInterval(10000);
        lReq.setFastestInterval(2500);
        JSONObject logPayload = new JSONObject();
        final SdkTracker sdkTracker = this.juspayServices.getSdkTracker();
        SharedPreferences sharedPref = context.getSharedPreferences(
                context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
//        String token = sharedPref.getString("USER_ID","null");
        String token1 = getKeyInNativeSharedPrefKeys("PersonId");
        String key1 = context.getResources().getString(R.string.service);
//        System.out.println("DriverId CommonJsInterface "+token);
//        System.out.println("DriverId CommonJsInterface "+token1+" registration ID "+sharedPref.getString("RegistrationToken","null"));


        // if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
        //     return;
        // }
        if(isLocationPermissionEnabled()){
            client.requestLocationUpdates(lReq,
                    new LocationCallback() {
                        @SuppressLint("MissingPermission")
                        @Override
                        public void onLocationResult(LocationResult locationResult) {
                            super.onLocationResult(locationResult);
                            Location lastLocation = locationResult.getLastLocation();
                            if (lastLocation != null) {
                                Log.e("startLocationUpdates", lastLocation.getLatitude() + "/" + lastLocation.getLongitude());
                                Double lat = lastLocation.getLatitude();
                                Double lng = lastLocation.getLongitude();
                                try {
                                    Date currentTime = Calendar.getInstance().getTime();
                                    logPayload.put("driver_id", token1);
                                    logPayload.put("latitude", lat);
                                    logPayload.put("longitude", lng);
                                    logPayload.put("time",currentTime);
                                }
                                catch (Exception e) {
                                    e.printStackTrace();
                                }if (key1.equals("yatripartner")){
                                    sdkTracker.trackContext("sdk", PaymentConstants.LogLevel.INFO,"network_call",logPayload);
                                }
                                else {
                                    System.out.println("getResourced else");
                                }
                                lastLatitudeValue = lat;
                                lastLongitudeValue = lng;
                            } else {
                                Log.e(LOG_TAG, "Couldn't get location from updates");
                            }
                        }

                        @Override
                        public void onLocationAvailability(LocationAvailability locationAvailability) {
                            super.onLocationAvailability(locationAvailability);
                        }
                    },
                    Looper.getMainLooper()
            );
        }
    }

    public int getScreenHeight() {
        DisplayMetrics displayMetrics = new DisplayMetrics();
        activity.getWindowManager().getDefaultDisplay().getMetrics(displayMetrics);
        return displayMetrics.heightPixels;
    }

    public int getScreenWidth() {
        DisplayMetrics displayMetrics = new DisplayMetrics();
        activity.getWindowManager().getDefaultDisplay().getMetrics(displayMetrics);
        return displayMetrics.widthPixels;
    }

    @JavascriptInterface
    public void requestLocation() {
        try {
            final boolean permissionEnabled = isLocationPermissionEnabled();
            if (!permissionEnabled)
            {
                requestPermission();
            }
            final boolean enabled = isLocationEnabled();
            if (!enabled) {
                GoogleApiClient googleApiClient = new GoogleApiClient.Builder(activity).addApi(LocationServices.API).build();
                googleApiClient.connect();

                LocationRequest lReq = LocationRequest.create();
                lReq.setPriority(Priority.PRIORITY_HIGH_ACCURACY);
                lReq.setInterval(10000);
                lReq.setFastestInterval(10000 / 2);

                LocationSettingsRequest.Builder lBuilder = new LocationSettingsRequest.Builder().addLocationRequest(lReq);
                lBuilder.setAlwaysShow(true);
                PendingResult result = LocationServices.SettingsApi.checkLocationSettings(googleApiClient, lBuilder.build());

                //noinspection unchecked
                result.setResultCallback(new ResultCallback<LocationSettingsResult>() {
                    public void onResult(@NonNull LocationSettingsResult result) {
                        final Status status = result.getStatus();
                        switch (status.getStatusCode()) {
                            case LocationSettingsStatusCodes.SUCCESS:
                                Log.e(LOG_TAG, "LocationSettingsStatusCodes" +getKeysInSharedPrefs("LOCATION_STATUS") );
                                if (!((context.getResources().getString(R.string.service).equals("yatripartner")) && getKeysInSharedPrefs("LOCATION_STATUS").equals("START"))){
                                    startLocationUpdates();
                                }
                                break;

                            case LocationSettingsStatusCodes.RESOLUTION_REQUIRED:
                                try {
                                    status.startResolutionForResult(activity, activity.getResources().getInteger(R.integer.LOCATION_RESOLUTION_REQUEST_CODE));
                                } catch (IntentSender.SendIntentException e) {
                                    e.printStackTrace();
                                    Log.e(LOG_TAG, "Location services can't be enabled");
                                }
                                break;

                            case LocationSettingsStatusCodes.SETTINGS_CHANGE_UNAVAILABLE:
                                Log.e(LOG_TAG, "Location services unavailable");
                                break;
                        }
                    }
                });
            } else {
                if (!(context.getResources().getString(R.string.service).equals("yatripartner"))){
                    startLocationUpdates();
                }
            }

        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in get current position", e);
        }
    }



    public void requestPermission() {
        try {
            if ((context.getResources().getString(R.string.service).equals("yatripartner"))){
                if ((Build.VERSION.SDK_INT < Build.VERSION_CODES.Q)) {
                    ActivityCompat.requestPermissions(activity, new String[]{ACCESS_FINE_LOCATION}, 1);
                }
                else if((Build.VERSION.SDK_INT == Build.VERSION_CODES.Q)){
                    ActivityCompat.requestPermissions(activity, new String[]{ACCESS_FINE_LOCATION, ACCESS_BACKGROUND_LOCATION}, 1);
                }
                else if((Build.VERSION.SDK_INT > Build.VERSION_CODES.Q)){
                    Intent intent =  new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
                    Uri uri = Uri.fromParts("package",getPackageName(),null);
                    intent.setData(uri);
                    context.startActivity(intent);
                    //startActivityForResult(activity, new Intent(android.provider.Settings.ACTION_LOCATION_SOURCE_SETTINGS), 0, new Bundle());
                }
            } else {
                ActivityCompat.requestPermissions(activity, new String[]{ACCESS_FINE_LOCATION}, 1);
            }
        }catch (Exception e) {
            Log.e(LOG_TAG, "Exception in request permission", e);
        }
    }


    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.M)
    public void checkOverlayPermission(){
        System.out.println("CommonJsInterface checkOverlayPermission()");
        if(!Settings.canDrawOverlays(context)){
            requestOverlayPermission();
            System.out.print("After request permission");
        }

    }

    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.M)
    public void requestAutoStartPermission() {
        CheckPermissionAutoStart.getInstance().getAutoStartPermission(context);
    }

    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.M)
    public void requestOverlayPermission() {
        try {
            Intent intent = new Intent(context, CheckPermissionOverlay.class);
            context.startActivity(intent);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in request permission", e);
        }
    }

    @JavascriptInterface
    public boolean isNetworkAvailable() {
        ConnectivityManager connectivityManager = (ConnectivityManager) activity.getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo activeNetworkInfo = null;
        if (connectivityManager != null) {
            activeNetworkInfo = connectivityManager.getActiveNetworkInfo();
        }
        return activeNetworkInfo != null && activeNetworkInfo.isConnected();
    }

    @JavascriptInterface
    public void attach(String id, String argumentsJson, String callbackFunctionName) {
        switch (id) {
            case "SMS_RETRIEVER":
                detach(new String[]{id});
                 if(browserFragment != null) {
                     JuspayDuiHook juspayDuiHook = new OtpUtils(browserFragment, callbackFunctionName);
                     if (juspayDuiHook != null) {
                         super.listenerMap.put(id, juspayDuiHook);
                         juspayDuiHook.attach(activity);
                     }
                 }
                break;
            default:
                super.attach(id, argumentsJson, callbackFunctionName);
        }

    }

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
    public void firebaseScreenNameLog (String screenName){
        Bundle bundle = new Bundle();
        bundle.putString(FirebaseAnalytics.Param.SCREEN_NAME, screenName);
        bundle.putString(FirebaseAnalytics.Param.SCREEN_CLASS, "MainActivity");
        mFirebaseAnalytics.logEvent(FirebaseAnalytics.Event.SCREEN_VIEW, bundle);
    }

    @JavascriptInterface
    public void firebaseUserID (String id){
        mFirebaseAnalytics.setUserId(id);
    }

    @JavascriptInterface
    public void showDialer(String phoneNum) {
        Intent intent = new Intent();
        intent.setAction(Intent.ACTION_CALL);
        phoneNumber = phoneNum;
        if (ContextCompat.checkSelfPermission(activity,Manifest.permission.CALL_PHONE) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(activity,new String[]{Manifest.permission.CALL_PHONE},REQUEST_CALL);
        }
        else
        {
            if (intent != null) {
                phoneNumber = "tel:" + phoneNum;
                intent.setData(Uri.parse(phoneNumber));
                activity.startActivity(intent);
            }

        }
    }

    @JavascriptInterface
    public void openNavigation(double slat, double slong, double dlat, double dlong) {
        try {
            Uri gmmIntentUri = Uri.parse("google.navigation:q="+String.valueOf(dlat)+","+String.valueOf(dlong));
            Intent mapIntent = new Intent(Intent.ACTION_VIEW, gmmIntentUri);
            mapIntent.setPackage("com.google.android.apps.maps");
            activity.startActivity(mapIntent);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Can't open google maps", e);
        }
    }

    @JavascriptInterface
    public void openUrlInApp(String url) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
//                    WebView myWebView = activity.findViewById(R.id.webView);
//                    myWebView.clearHistory();
//                    myWebView.loadUrl(url);
//                    myWebView.setVisibility(View.VISIBLE);
//                    activity.getIntent().putExtra("request_url", url);
                    Intent httpIntent = new Intent(Intent.ACTION_VIEW);
                    httpIntent.setData(Uri.parse(url));
                    activity.startActivity(httpIntent);
                } catch (Exception e) {
                    Log.e(LOG_TAG, "Exception occurred while calling WebView", e);
                }
            }
        });
    }

    @JavascriptInterface
    public void openUrlInMailApp(String mailId) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    Intent intent=new Intent(Intent.ACTION_SEND);
                    String[] recipients={mailId};
                    intent.putExtra(Intent.EXTRA_EMAIL, recipients);
                    // intent.putExtra(Intent.EXTRA_SUBJECT,"Subject text here...");
                    // intent.putExtra(Intent.EXTRA_TEXT,"Body of the content here...");
                    // intent.putExtra(Intent.EXTRA_CC,"mailcc@gmail.com");
                    intent.setType("text/html");
                    intent.setPackage("com.google.android.gm");
                    activity.startActivity(Intent.createChooser(intent, "Send mail"));
                } catch (Exception e) {
                    Log.e(LOG_TAG, "Exception occurred while calling mail", e);
                }
            }
        });
    }
    @JavascriptInterface
    public void getLocationAndDistance(double dlat, double dlng, String callback) {
        Log.i(LOG_TAG, "getLocationAndDistance called: " + dlat + " " + dlng);
        try {
            if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
                Log.e(LOG_TAG, "getLocationAndDistance: Location permission not granted");
                return;
            }
            client.getLastLocation().addOnSuccessListener(activity, new OnSuccessListener<Location>() {
                @Override
                public void onSuccess(Location location) {
                    if (location != null) {
                        Double slat = location.getLatitude();
                        Double slng = location.getLongitude();
                        float[] results = new float[1];
                        Location.distanceBetween(slat, slng, dlat, dlng, results);
                        Log.d(LOG_TAG, "getLocationAndDistance: " + slat + " " + slng + " " + results[0]);
                        if (callback != null && dynamicUI != null) {
                            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%f,%f,%f);",
                                    callback, slat, slng, results[0]);
                            dynamicUI.addJsToWebView(javascript);
                        }
                    } else {
                        Log.e(LOG_TAG, "Current position not known3");
                    }
                }
            });
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception occurred in getLocationAndDistance " + e.getMessage());
            e.printStackTrace();
        }
    }

    @JavascriptInterface
    public String getAAID()
    {
        try{
            AdvertisingIdClient.Info adInfo = AdvertisingIdClient.getAdvertisingIdInfo(context);
            String myId = adInfo != null ? adInfo.getId() : null;
            Log.i("UIDMY",myId);
            return myId;
        }catch (Exception e) {
            return "No ad id";
        }
    }

    @JavascriptInterface
    public void factoryResetApp(){
        // Systems at 29/Q and later don't allow relaunch, but System.exit(0) on
        // all supported systems will relaunch ... but by killing the process, then
        // restarting the process with the back stack intact. We must make sure that
        // the launch activity is the only thing in the back stack before exiting.
//        SharedPreferences.Editor.clear().apply();

        final PackageManager pm = activity.getPackageManager();
        final Intent intent = pm.getLaunchIntentForPackage(activity.getPackageName());
        activity.finishAffinity(); // Finishes all activities.
        activity.startActivity(intent);    // Start the launch activity
//        System.exit(0);    // System finishes and automatically relaunches us.
    }

    @JavascriptInterface
    public void getLocationName(String latitude, String longitude, String defaultText, String callback) {
        if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            return;
        }
        Double lat ;
        Double lng ;
        LocationRequest mLocationRequest = LocationRequest.create();
        mLocationRequest.setPriority(Priority.PRIORITY_HIGH_ACCURACY);
        LocationCallback mLocationCallback = new LocationCallback() {
            @Override
            public void onLocationResult(LocationResult locationResult) {
                if (locationResult == null) {
                    return;
                }else{
                    System.out.println("LOCATION RESULT" + locationResult);
                }
            }
        };
        client.requestLocationUpdates(mLocationRequest, mLocationCallback, null);
        client.getLastLocation().addOnSuccessListener(activity, new OnSuccessListener<Location>() {
            @Override
            public void onSuccess(Location location) {
                if (location != null) {
                    Double lat = location.getLatitude();
                    Double lng = location.getLongitude();
                    lastLatitudeValue = lat;
                    lastLongitudeValue = lng;
                    Log.d(LOG_TAG, "getCurrentPosition" + lat + lng);
                    } else {
                    Log.e(LOG_TAG, "Current position not known1");
                }
            }
        }).addOnFailureListener(activity, new OnFailureListener() {
            @Override
            public void onFailure(@NonNull Exception e) {
                Log.e(LOG_TAG, "Current position not known2");
            }
        });
        if(defaultText.equals(CURRENT_LOCATION_LATLON)){
            latitude = String.valueOf(lastLatitudeValue);
            longitude = String.valueOf(lastLongitudeValue);
        }
        System.out.println( "TEMPLOGGG   " + latitude + " <- lat " + " , lon ->" + longitude + defaultText + callback);
        Geocoder geocoder = new Geocoder(activity, Locale.getDefault());
        StringBuilder strReturnedAddress;
        try {
            List<Address> addresses = geocoder.getFromLocation(Double.parseDouble(latitude), Double.parseDouble(longitude), 1);
            if (addresses != null && addresses.size() > 0) {
                strReturnedAddress = new StringBuilder();
                Address returnedAddress = addresses.get(0);
                for (int i = 0; i <= returnedAddress.getMaxAddressLineIndex(); i++) {
                    strReturnedAddress.append(returnedAddress.getAddressLine(i)).append(",");
                }
                Log.d(LOG_TAG, "getLocationName:" + strReturnedAddress);
            } else {
                strReturnedAddress = new StringBuilder(defaultText);
                Log.e(LOG_TAG, "Can't fetch current Address");
            }
            if (callback != null && dynamicUI != null) {
                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                        callback, String.valueOf(latitude), String.valueOf(longitude), strReturnedAddress);
                Log.d(LOG_TAG, "getCurrent___Position___inside if" + latitude + longitude);
                dynamicUI.addJsToWebView(javascript);
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception occurred in getting Location Name " + e.getMessage());
            e.printStackTrace();
        }
    }

    @JavascriptInterface
    public void requestKeyboardShow(final String id) {
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    try {
                        if (activity != null && browserFragment != null) {
                            int currentId = Integer.parseInt(id);
                            InputMethodManager inputMethodManager = (InputMethodManager) activity.getSystemService(Context.INPUT_METHOD_SERVICE);
                            View editText = activity.findViewById(currentId);
                            View prevEditText = null;
                            if (lastFocusedEditView != -1) {
                                prevEditText = activity.findViewById(lastFocusedEditView);
                            }
                            if (inputMethodManager != null && editText != null) {
                                if (prevEditText != null && lastFocusedEditView != currentId) {
                                    prevEditText.clearFocus();
                                }
                                editText.requestFocus();
                                inputMethodManager.showSoftInput(editText, InputMethodManager.SHOW_IMPLICIT);
                            }
                            if (currentId != lastFocusedEditView) {
                                lastFocusedEditView = Integer.parseInt(id);
                            }
                        }
                    } catch (Exception e) {
                        Log.e(LOG_TAG, "Keyboard Exception" +e.toString() );
                    }
                }
            });
        }
    }

    @JavascriptInterface
    public String getCurrentLatLong() {
        return "{\"lat\":\"" + lastLatitudeValue + "\",\"long\":\"" + lastLongitudeValue + "\"}";
    }

    @JavascriptInterface
    public void initiateLocationServiceClient() {
        if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            return;
        }
        LocationRequest locationRequest = LocationRequest.create();
        locationRequest.setPriority(Priority.PRIORITY_HIGH_ACCURACY);
        locationRequest.setInterval(1000);
        locationRequest.setFastestInterval(500);
        LocationCallback mLocationCallback = new LocationCallback() {
            @Override
            public void onLocationResult(LocationResult locationResult) {
                if (locationResult == null) {
                    return;
                }
            }
        };
        client.requestLocationUpdates(locationRequest, mLocationCallback, Looper.getMainLooper());

        LocationSettingsRequest.Builder builder = new LocationSettingsRequest.Builder()
                .addLocationRequest(locationRequest);
        builder.setAlwaysShow(true);

        GoogleApiClient googleApiClient = new GoogleApiClient.Builder(activity).addApi(LocationServices.API).build();
        googleApiClient.connect();

        PendingResult<LocationSettingsResult> result =
                LocationServices.SettingsApi.checkLocationSettings(googleApiClient, builder.build());
        result.setResultCallback(new ResultCallback<LocationSettingsResult>() {
            @Override
            public void onResult(LocationSettingsResult result) {
                final Status status = result.getStatus();
                final LocationSettingsStates state = result.getLocationSettingsStates();
                switch (status.getStatusCode()) {
                    case LocationSettingsStatusCodes.SUCCESS:
                        break;
                    case LocationSettingsStatusCodes.RESOLUTION_REQUIRED:
                        try {
                            status.startResolutionForResult(
                                    activity, 1000);
                        } catch (IntentSender.SendIntentException e) {
                        }
                        break;
                    case LocationSettingsStatusCodes.SETTINGS_CHANGE_UNAVAILABLE:
                        break;
                }
            }
        });


    }

    @JavascriptInterface
    public void getCurrentPosition(String callback) {
        if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            return;
        }

        LocationRequest mLocationRequest = LocationRequest.create();
        mLocationRequest.setPriority(Priority.PRIORITY_HIGH_ACCURACY);
        LocationCallback mLocationCallback = new LocationCallback() {
            @Override
            public void onLocationResult(LocationResult locationResult) {
                if (locationResult == null) {
                    return;
                }else{
                    System.out.println("LOCATION RESULT" + locationResult);
                }
            }
        };
        client.requestLocationUpdates(mLocationRequest, mLocationCallback, null);
        client.getLastLocation().addOnSuccessListener(activity, new OnSuccessListener<Location>() {
            @Override
            public void onSuccess(Location location) {
                if (location != null) {
                    Double lat = location.getLatitude();
                    Double lng = location.getLongitude();
                    lastLatitudeValue = lat;
                    lastLongitudeValue = lng;
                    Log.d(LOG_TAG, "getCurrentPosition" + lat + lng);
                    if (callback != null && dynamicUI != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                                callback, String.valueOf(lat), String.valueOf(lng));
                        dynamicUI.addJsToWebView(javascript);
                    }
                } else {
                    Log.e(LOG_TAG, "Current position not known1");
                }
            }
        }).addOnFailureListener(activity, new OnFailureListener() {
            @Override
            public void onFailure(@NonNull Exception e) {
                Log.e(LOG_TAG, "Current position not known2");
            }
        });
    }
    private NetworkInfo getInfo() {
        return ((ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo();
    }

    @JavascriptInterface
    public boolean isInternetAvailable() {
        NetworkInfo info = getInfo();
        if (info != null)
            return true;
        else
            return false;
    }

    @JavascriptInterface
    public void previewImage ( String base64Image){
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if (!base64Image.equals("") && base64Image!=null){
                        byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                        Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);

                        AlertDialog.Builder builder = new AlertDialog.Builder(activity);
                        builder.setCancelable(true);
                        ImageView imagePreview = new ImageView(activity);
                        imagePreview.setImageBitmap(decodedByte);

                        DisplayMetrics displayMetrics = new DisplayMetrics();
                        activity.getWindowManager().getDefaultDisplay().getMetrics(displayMetrics);
                        int screenHeight = displayMetrics.heightPixels;
                        int width = displayMetrics.widthPixels;
                        imagePreview.setMinimumHeight(screenHeight/2);
                        imagePreview.setMinimumWidth(width);

                        ViewGroup.LayoutParams layoutParams= new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,LinearLayout.LayoutParams.MATCH_PARENT);
                        imagePreview.setLayoutParams(layoutParams);
                        builder.setView(imagePreview);
                        AlertDialog alertDialog = builder.create();
                        alertDialog.show();
                    }
                } catch (Exception e){
                    e.printStackTrace();
                }
            }
        });
    }

    @JavascriptInterface
    public void renderBase64Image (String base64Image, String id){
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if (!base64Image.equals("") && base64Image!=null && id!=null){
                        byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                        Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);
                        ImageView pureScriptImageView;
                        pureScriptImageView = activity.findViewById(Integer.parseInt(id));
                        pureScriptImageView.setImageBitmap(decodedByte);
                        pureScriptImageView.setScaleType(ImageView.ScaleType.CENTER_CROP);
                        pureScriptImageView.setClipToOutline(true);
                        pureScriptImageView.setBackground(activity.getDrawable(R.drawable.round_circle));
                    }
                } catch (Exception e){
                    e.printStackTrace();
                }
            }
        });
    }

    @JavascriptInterface
    public void startLottieProcess(String rawJson, String id, boolean repeat, float speed) {
        if (activity != null) activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    animationView = activity.findViewById(Integer.parseInt(id));
                    animationView.setAnimationFromJson(getJsonFromResources(rawJson));
                    animationView.loop(repeat);
                    animationView.setSpeed(speed);
                    animationView.playAnimation();
                } catch (Exception e) {
                    Log.d("TAG", "exception in startLottieAnimation" , e);
                }
            }
        });
    }

    private String getJsonFromResources(String rawJson){
        InputStream inputStreams = activity.getResources().openRawResource(activity.getResources().getIdentifier(rawJson, "raw", getPackageName()));
        Writer writer = new StringWriter();
        char[] buffer = new char[1024];
        try {
            Reader reader = new BufferedReader(new InputStreamReader(inputStreams, "UTF-8"));
            int n;
            while ((n = reader.read(buffer)) != -1) {
                writer.write(buffer, 0, n);
            }
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                inputStreams.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return writer.toString();
    }

    //###########################################################################
    //region: MAP FUNCTIONS Starts-----------------------------------------------
    //###########################################################################

    static String storeMapCallBack = null;
    @JavascriptInterface
    public void storeCallBackLocateOnMap(String callback) {
        System.out.println("storeCallBackLocateOnMap" + callback);
        storeMapCallBack = callback;
    }
    @JavascriptInterface
    public void showMap(final String pureScriptId, final String mapType, final float zoom, final String callback) {
        try {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                    FragmentManager supportFragmentManager = ((FragmentActivity) activity).getSupportFragmentManager();
                    FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                    fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                    fragmentTransaction.commitAllowingStateLoss();
                    getMapAsync(mapFragment, mapType, callback, pureScriptId, zoom);
                }
            });
        } catch (Exception e) {
            Log.e("ADD_MARKER", e.toString());
        }
    } //NEW

    @JavascriptInterface
    public void mapSnapShot(final String pureScriptId, final String json, final String routeType, final boolean actualRoute, final String callback) {
        try {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                    FragmentManager supportFragmentManager = ((FragmentActivity) activity).getSupportFragmentManager();
                    FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                    fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                    fragmentTransaction.commitAllowingStateLoss();
                    mapFragment.getMapAsync(new OnMapReadyCallback() {
                        @Override
                        public void onMapReady(GoogleMap googleMap) {
                            CommonJsInterface.this.googleMap = googleMap;
                            CommonJsInterface.this.googleMap.getUiSettings().setAllGesturesEnabled(false);
                            markers = new JSONObject();
                            markersElement.put(pureScriptId, markers);
                            CommonJsInterface.this.googleMap.setOnMapLoadedCallback (new GoogleMap.OnMapLoadedCallback () {
                                @Override
                                public synchronized void onMapLoaded() {
                                    System.out.println("onMapLoaded");
                                    System.out.println(json);
                                    showRoute(json, routeType, "#323643", actualRoute, "dest_marker", "src_marker", 8);
                                    final Handler handler = new Handler();
                                    handler.postDelayed(new Runnable() {
                                        @Override
                                        public void run() {
                                            GoogleMap.SnapshotReadyCallback callback2=new GoogleMap.SnapshotReadyCallback () {
                                                Bitmap bitmap;
                                                @Override
                                                public void onSnapshotReady(Bitmap snapshot) {
                                                    bitmap=snapshot;
                                                    String encImage = "";
                                                    try {
                                                        ByteArrayOutputStream baos = new ByteArrayOutputStream();
                                                        bitmap.compress(Bitmap.CompressFormat.JPEG,80,baos);
                                                        byte[] b = baos.toByteArray();
                                                        encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                                                    } catch (Exception e) {
                                                        e.printStackTrace();
                                                    }

                                                    if(dynamicUI != null && callback != null){
                                                        Log.i("callback encoded image 2", encImage);
                                                        String javascript = String.format("window.callUICallback('%s','%s');", callback, encImage);
                                                        Log.e(LOG_TAG, javascript);
                                                        dynamicUI.addJsToWebView(javascript);
                                                    }
                                                }
                                            };CommonJsInterface.this.googleMap.snapshot (callback2);
                                        }
                                    }, 2000);
                                }
                            });
                        }
                    });
                }
            });
        } catch (Exception e) {
            Log.e("ADD_MARKER", e.toString());
        }
    }

    @JavascriptInterface
    public void showRoute(final String json, final String style, final String trackColor, final boolean isActual, final String sourceMarker, final String destMarker, final int polylineWidth) {
        ArrayList<Polyline> lines = new ArrayList<>();
//        polylines.add(lines);
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if(googleMap!=null) {
                    System.out.println("inside_showRoute");
                    PolylineOptions polylineOptions = new PolylineOptions();
                    int color = Color.parseColor(trackColor);
                    try {
                        JSONObject jsonObject = new JSONObject(json);
                        JSONArray coordinates = jsonObject.getJSONArray("points");
                        JSONArray journeyCoordinates = jsonObject.getJSONArray("journeyCoordinates");
                        JSONObject sourceCoordinates = (JSONObject) journeyCoordinates.get(0);
                        JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length()-1);
                        double sourceLat = sourceCoordinates.getDouble("lat");
                        double sourceLong = sourceCoordinates.getDouble("lng");
                        double destLat = destCoordinates.getDouble("lat");
                        double destLong = destCoordinates.getDouble("lng");

                        double source_lat, source_lng, destination_lat, destination_lng;
                        if (sourceLat <= destLat) {
                            source_lat = sourceLat - 0.4*(destLat-sourceLat);
                            destination_lat = destLat + 0.1*(destLat-sourceLat);
                        } else {
                            source_lat = sourceLat + 0.1*(sourceLat-destLat);
                            destination_lat = destLat - 0.4*(sourceLat-destLat);
                        }
                        if (sourceLong <= destLong) {
                            source_lng = sourceLong - 0.09*(destLong-sourceLong);
                            destination_lng = destLong + 0.09*(destLong-sourceLong);
                        } else {
                            source_lng = sourceLong + 0.09*(sourceLong-destLong);
                            destination_lng = destLong - 0.09*(sourceLong-destLong);
                        }

                        if(googleMap!=null) {
                            try {
                                LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                                LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                                LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                                googleMap.moveCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                            } catch (IllegalArgumentException e) {
                                LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                                LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                                LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                                googleMap.moveCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                            }
                            catch(Exception e){
                                System.out.println("In mmove camera in catch exception " + e);
                            }
                        }
//
                        if(isActual){
                            for (int i = coordinates.length() -1 ; i >= 0 ; i--) {
                                JSONObject coordinate = (JSONObject) coordinates.get(i);
                                double lng = coordinate.getDouble("lng");
                                double lat = coordinate.getDouble("lat");
                                polylineOptions.add(new LatLng(lat, lng));
                            }
                        }else{
                            LatLng fromPointObj = new LatLng(sourceLat, sourceLong);
                            LatLng toPointObj = new LatLng(destLat, destLong);
                            double k = 0.5;
                            double distanceBetweenTwoLatLon = SphericalUtil.computeDistanceBetween(fromPointObj, toPointObj);
                            double headingForTwoLatLon = SphericalUtil.computeHeading(fromPointObj, toPointObj);
                            LatLng computedDestinationCoordinates = SphericalUtil.computeOffset(fromPointObj, distanceBetweenTwoLatLon * 0.5, headingForTwoLatLon);
                            double x = (1 - k * k) * distanceBetweenTwoLatLon * 0.5 / (2 * k);
                            double r = (1 + k * k) * distanceBetweenTwoLatLon * 0.5 / (2 * k);
                            LatLng finalComputedOffset = SphericalUtil.computeOffset(computedDestinationCoordinates, x, headingForTwoLatLon > 40 ? headingForTwoLatLon + 90.0 : headingForTwoLatLon - 90.0);
                            double h1 = SphericalUtil.computeHeading(finalComputedOffset, fromPointObj);
                            double h2 = SphericalUtil.computeHeading(finalComputedOffset, toPointObj);
                            int numpoints = 50;
                            double step = (h2 - h1) / numpoints;
                            for (int i = 0; i < numpoints; i++) {
                                LatLng pi = SphericalUtil.computeOffset(finalComputedOffset, r, h1 + i * step);
                                polylineOptions.add(pi);
                            }
                        }

                        Polyline polyline = setRouteCustomTheme(polylineOptions, color, style, polylineWidth);

                        if(sourceMarker != null && !sourceMarker.equals("")) {
                            Bitmap sourceBitmap = constructBitmap(90, sourceMarker);
                            polyline.setStartCap(
                                    new CustomCap(
                                            BitmapDescriptorFactory.fromBitmap(sourceBitmap)
                                    )
                            );
                        }

                        if(destMarker != null && !destMarker.equals("")) {
                            Bitmap destBitmap = constructBitmap(90, destMarker);
                            polyline.setEndCap(
                                    new CustomCap(
                                            BitmapDescriptorFactory.fromBitmap(destBitmap)
                                    )
                            );
                        }

                        lines.add(polyline);

                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
    }

    private void getMapAsync(SupportMapFragment mapFragment, final String mapType, final String callback, final String pureScriptId, final float zoom){
        mapFragment.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(GoogleMap googleMap) {
                CommonJsInterface.this.googleMap = googleMap;
                googleMap.setMinZoomPreference(7.0f);
                googleMap.setMaxZoomPreference(googleMap.getMaxZoomLevel());
                markers = new JSONObject();
                markersElement.put(pureScriptId, markers);
                googleMap.setOnMarkerClickListener(new GoogleMap.OnMarkerClickListener() {
                    @Override
                    public boolean onMarkerClick(Marker marker) {
                        marker.hideInfoWindow();
                        return true;
                    }
                });
                try {
                    if (mapType.equals(LOCATE_ON_MAP)){
                        upsertMarker(LOCATE_ON_MAP, String.valueOf(lastLatitudeValue), String.valueOf(lastLongitudeValue), 160, 0.5f,0.5f);
                        CommonJsInterface.this.googleMap.setOnCameraMoveListener(new GoogleMap.OnCameraMoveListener() {
                            @Override
                            public void onCameraMove() {
                                try {
                                    double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                                    double lng =  (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                                    upsertMarker(LOCATE_ON_MAP, String.valueOf(lat), String.valueOf(lng),160, 0.5f,0.5f);
                                }catch (Exception e) {
                                    Log.i(LOG_TAG, "Marker creation error for ", e);
                                }
                            }
                        });
                        CommonJsInterface.this.googleMap.setOnCameraIdleListener(new GoogleMap.OnCameraIdleListener() {
                            @Override
                            public void onCameraIdle() {
                                double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                                double lng =  (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                                String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "LatLon", lat, lng);
                                Log.e(LOG_TAG, javascript);
                                dynamicUI.addJsToWebView(javascript);
                            }
                        });
                    }
                    if (lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) {
                        LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        CommonJsInterface.this.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, zoom));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                if(dynamicUI != null && callback != null){
                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "MAP", "READY", "LOADED");
                    Log.e(LOG_TAG, javascript);
                    dynamicUI.addJsToWebView(javascript);
                }
            }
        });
    } //NEW

    @JavascriptInterface
    public void exitLocateOnMap (String str){
        try {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    googleMap.setOnCameraMoveListener(null);
                    googleMap.setOnCameraIdleListener(null);
                }
            });
        } catch (Exception e) {
            Log.i(LOG_TAG, "LocateOnMap Exit Error for ", e);
        }
    }

    @JavascriptInterface
    public void locateOnMap (boolean goToCurrentLocation, final String lat, final String lon){
        try {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    if(goToCurrentLocation){
                        upsertMarker(CURRENT_LOCATION, String.valueOf(lastLatitudeValue), String.valueOf(lastLongitudeValue), 90, 0.5f,0.5f);
                    }else{
                        LatLng latLng = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                        upsertMarker(CURRENT_LOCATION, lat, lon, 90, 0.5f,0.5f);
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                    }
                    googleMap.setOnCameraMoveListener(new GoogleMap.OnCameraMoveListener() {
                        @Override
                        public void onCameraMove() {
                            try {
                                double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                                double lng =  (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                                upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng),160, 0.5f,0.5f);
                            }catch (Exception e) {
                                Log.i(LOG_TAG, "Marker creation error for ", e);
                            }
                        }
                    });
                    googleMap.setOnCameraIdleListener(new GoogleMap.OnCameraIdleListener() {
                        @Override
                        public void onCameraIdle() {
                            double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                            double lng =  (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                            if (storeMapCallBack != null){
                                String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeMapCallBack, "LatLon", lat, lng);
                                Log.e(LOG_TAG, javascript);
                                dynamicUI.addJsToWebView(javascript);
                            }
                        }
                    });
                    if ((lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) && goToCurrentLocation) {
                        LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        CommonJsInterface.this.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                    }else{
                        LatLng latLngObjMain = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                        CommonJsInterface.this.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                    }
                }
            });

        } catch (Exception e) {
            Log.i(LOG_TAG, "LocateOnMap error for ", e);
        }
    }

    @JavascriptInterface
    public void reallocateMapFragment(final String pureScriptId) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    SupportMapFragment mapFragment = (SupportMapFragment) ((FragmentActivity) activity).getSupportFragmentManager()
                            .findFragmentById(Integer.parseInt(pureScriptId));
                    if(mapFragment!=null){
                        mapFragment.getMapAsync(new OnMapReadyCallback() {
                            @Override
                            public void onMapReady(GoogleMap googleMap) {
                                CommonJsInterface.this.googleMap = googleMap;
                                markers = markersElement.get(pureScriptId);
                            }
                        });
                    }
                } catch (Exception e) {
                    Log.e("FAILED WHILE REALLOCATING", e.toString());
                }
            }
        });
    }

    private MarkerOptions makeMarkerObject(final String title, final double lat, final double lng, final int markerSize, final float anchorV, final float anchorV1) {
        try {
            MarkerOptions markerOptions = new MarkerOptions()
                    .position(new LatLng(lat, lng))
                    .title(title)
                    .anchor(anchorV, anchorV1);
            if(!title.equals(LOCATE_ON_MAP)){
                Bitmap smallMarker = constructBitmap(markerSize, title);
                markerOptions.icon(BitmapDescriptorFactory.fromBitmap(smallMarker));
            }
            return markerOptions;
        } catch (Exception e) {
            Log.e(LOG_TAG, "MARKER obj creation", e);
            return null;
        }
    }

    private Bitmap constructBitmap(final int markerSize, final String title){
        int imageID = context.getResources().getIdentifier(title, "drawable", activity.getPackageName());
        BitmapDrawable bitmapdraw = (BitmapDrawable) context.getResources().getDrawable(imageID);
        Bitmap b = bitmapdraw.getBitmap();
        //int markerWidth = markerSize;
        //if(title.equals(CURRENT_LOCATION)){
        //    markerWidth -= 50;
        //}
        float maximum = Math.max(b.getWidth(), b.getHeight());
        float minimum = Math.min(b.getWidth(), b.getHeight());
        float multiplier = markerSize/maximum;
        int markerWidth = Math.round(b.getWidth() * multiplier);
        int markerHeight = Math.round(b.getHeight() * multiplier);
        Log.i("real width and height of "+ title, String.valueOf(b.getWidth()) + " , " + String.valueOf(b.getHeight()));
        Log.i("after width and height of "+ title, String.valueOf(markerWidth) + " , " + String.valueOf(markerHeight));
        return Bitmap.createScaledBitmap(b, markerWidth, markerHeight, false);
    }
    private Bitmap getMarkerBitmapFromView(String locationName, String imageName) {

        View customMarkerView = ((LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(R.layout.marker_label_layout, null);
        TextView label = customMarkerView.findViewById(R.id.marker_text);
        if(locationName.equals("")){
            label.setVisibility(customMarkerView.GONE);
        }else{
            if (locationName.length() <= 27) {
                label.setText(locationName);
            } else {
                label.setText(locationName.substring(0, 17)+"...");
            }
        }
        ImageView pointer = customMarkerView.findViewById(R.id.pointer_img);
        try {
            if(imageName.equals("dest_marker") ){
                pointer.setImageDrawable(context.getResources().getDrawable(R.drawable.dest_marker));
            }else{
                pointer.setImageDrawable(context.getResources().getDrawable(R.drawable.src_marker));
            }
        }catch (Exception e){
            Log.e("Exception in rendering Image", e.toString());
        }
        customMarkerView.measure(View.MeasureSpec.UNSPECIFIED, View.MeasureSpec.UNSPECIFIED);
        customMarkerView.layout(0, 0, customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight());
        customMarkerView.buildDrawingCache();
        Bitmap returnedBitmap = Bitmap.createBitmap(customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight(),
                Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(returnedBitmap);
        canvas.drawColor(Color.WHITE, PorterDuff.Mode.SRC_IN);
        Drawable drawable = customMarkerView.getBackground();
        if (drawable != null)
            drawable.draw(canvas);
        customMarkerView.draw(canvas);
        return returnedBitmap;
    }

    @JavascriptInterface
    public void upsertMarker(final String title, final String lat, final String lng, final int markerSize, final float anchorV, final float anchorV1) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if(lat != null && lng != null){
                        double latitude = lat.equals("9.9") ? lastLatitudeValue : Double.parseDouble(lat);
                        double longitude = lat.equals("9.9") ? lastLatitudeValue : Double.parseDouble(lng);
                        LatLng latLngObj = new LatLng(latitude, longitude);
                        Marker markerObject;
                        if (markers.has(title)) {
                            markerObject = (Marker) markers.get(title);
                            markerObject.setPosition(latLngObj);
                            markerObject.setFlat(true);
                            markerObject.setVisible(true);
                            markerObject.hideInfoWindow();
                            Log.i(LOG_TAG, "Marker position updated for " + title);
                        } else {
                            MarkerOptions markerOptionsObj = makeMarkerObject(title, latitude, longitude, markerSize, anchorV, anchorV1);
                            if (markerOptionsObj != null && googleMap != null) {
                                markerObject = googleMap.addMarker(markerOptionsObj);
                                markers.put(title, markerObject);
                                markerObject.setPosition(latLngObj);
                                markerObject.setVisible(true);
                                markerObject.setFlat(true);
                                markerObject.hideInfoWindow();
                                if(title.equals("ic_customer_current_location")){
                                    userPositionMarker = markerObject;
                                }
                                Log.i(LOG_TAG, "New marker created and updated for " + title);
                            }
                        }
                    }
                } catch (Exception e) {
                    Log.i(LOG_TAG, "Marker creation error for " + title, e);
                }
            }
        });
    }

    @JavascriptInterface
    public void removeMarker(final String title) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if (markers.has(title)) {
                        Marker m = (Marker) markers.get(title);
                        m.setVisible(false);
                        Log.i(LOG_TAG, "Removed marker " + title);
                    }
                } catch (Exception e) {
                    Log.i(LOG_TAG, "Remove Marker error " + title, e);
                }
            }
        });
    }

    public Boolean setMapCustomTheme(String mapStyle) { // TODO Check for grey boxes and update the json for the same -- SHAILESH GAHLAWAT
        Boolean success = false;
        try {
            switch (mapStyle) {
                case "silver":
                    success = googleMap.setMapStyle(
                            MapStyleOptions.loadRawResourceStyle(
                                    context, R.raw.map_style_silver));
                    break;
                case "retro":
                    success = googleMap.setMapStyle(
                            MapStyleOptions.loadRawResourceStyle(
                                    context, R.raw.map_style_retro));
                    break;
                case "dark":
                    success = googleMap.setMapStyle(
                            MapStyleOptions.loadRawResourceStyle(
                                    context, R.raw.map_style_dark));
                    break;
                case "night":
                    success = googleMap.setMapStyle(
                            MapStyleOptions.loadRawResourceStyle(
                                    context, R.raw.map_style_night));
                    break;
                case "aubergine":
                    success = googleMap.setMapStyle(
                            MapStyleOptions.loadRawResourceStyle(
                                    context, R.raw.map_style_aubergine));
                    break;
                default:
                    success = googleMap.setMapStyle(
                            MapStyleOptions.loadRawResourceStyle(
                                    context, R.raw.map_style_retro));
                    break;
            }
            if (!success) {
                Log.e(LOG_TAG, "Style parsing failed.");
            }
        } catch (Resources.NotFoundException e) {
            Log.e(LOG_TAG, "Can't find style. Error: ", e);
        }
        return success;
    }

    @JavascriptInterface
    public void updateRoute (String json,  double currLat, double currLng) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if(googleMap!=null) {
                    try {
                        ArrayList<LatLng> points = getUpdatedPolyPoints(json,currLat,currLng);
                        Marker currMarker = (Marker) markers.get("ic_auto_map");
                        if (polylines != null) {
                            polylines.setEndCap(new ButtCap());
                            if (points.size() == 0) {
                                LatLng destination = new LatLng(currLat,currLng);
                                animateMarkerNew(destination,currMarker);
                                polylines.remove();
                                polylines = null;
                            }
                            else
                            {
                                PatternItem DASH = new Dash(1);
                                List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Arrays.asList(DASH);
                                polylines.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                                polylines.setPoints(points);
                                LatLng destination = points.get(points.size()-1);
                                animateMarkerNew(destination,currMarker);
                            }
                        }
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
    }

    private ArrayList<LatLng> getUpdatedPolyPoints (String json , double currLat, double currLng) throws JSONException {
        LatLng currPoint = new LatLng(currLat,currLng);
        ArrayList<LatLng> path = new ArrayList<>();
        ArrayList<LatLng> temp = new ArrayList<>();
        JSONObject jsonObject = null;
        jsonObject = new JSONObject(json);
        JSONArray coordinates = jsonObject.getJSONArray("points");
        JSONArray journeyCoordinates = jsonObject.getJSONArray("journeyCoordinates");
        JSONObject sourceCoordinates = (JSONObject) journeyCoordinates.get(0);
        JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length()-1);
        double sourceLat = sourceCoordinates.getDouble("lat");
        double sourceLong = sourceCoordinates.getDouble("lng");
        moveCamera(sourceLat, sourceLong, currLat, currLng, coordinates);
        for (int i = coordinates.length() -1 ; i >= 0   ; i--) {
            JSONObject coordinate = (JSONObject) coordinates.get(i);
            double lng = coordinate.getDouble("lng");
            double lat = coordinate.getDouble("lat");
            LatLng tempPoints = new LatLng(lat, lng);
            path.add(tempPoints);
        }
        int index = PolyUtil.locationIndexOnEdgeOrPath(currPoint,path,PolyUtil.isClosedPolygon(path),true,50.0);
        if (index == 0)
        {
            path.clear();
        }
        else {
            path.subList(index + 1, path.size()).clear();
        }
        return path;
    }

    @JavascriptInterface
    public boolean isCoordOnPath(String json , double currLat, double currLng) throws JSONException {
        LatLng currPoint = new LatLng(currLat,currLng);
        ArrayList<LatLng> path = new ArrayList<>();
        JSONObject jsonObject = null;
        jsonObject = new JSONObject(json);
        JSONArray coordinates = jsonObject.getJSONArray("points");
        for (int i = coordinates.length() -1 ; i >= 0   ; i--) {
            JSONObject coordinate = (JSONObject) coordinates.get(i);
            double lng = coordinate.getDouble("lng");
            double lat = coordinate.getDouble("lat");
            LatLng tempPoints = new LatLng(lat,lng);
            path.add(tempPoints);
        }
        boolean result = PolyUtil.isLocationOnPath(currPoint,path,true,50.0);
        return result;
    }



    private float bearingBetweenLocations(LatLng latLng1,LatLng latLng2) {
        double PI = 3.14159;
        double lat1 = latLng1.latitude * PI / 180;
        double long1 = latLng1.longitude * PI / 180;
        double lat2 = latLng2.latitude * PI / 180;
        double long2 = latLng2.longitude * PI / 180;
        double dLon = (long2 - long1);
        double y = Math.sin(dLon) * Math.cos(lat2);
        double x = Math.cos(lat1) * Math.sin(lat2) - Math.sin(lat1)
                * Math.cos(lat2) * Math.cos(dLon);
        double brng = Math.atan2(y, x);
        brng = Math.toDegrees(brng);
        brng = (brng + 360) % 360;
        return (float)brng;
    }

    private void animateMarkerNew(final LatLng destination, final Marker marker) {

        if (marker != null) {

            final LatLng startPosition = marker.getPosition();
            final LatLng endPosition = destination;

            final float startRotation = marker.getRotation();
            final LatLngInterpolatorNew latLngInterpolator = new LatLngInterpolatorNew.LinearFixed();

            ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1);
            valueAnimator.setDuration(3000); // duration 3 second
            valueAnimator.setInterpolator(new LinearInterpolator());
            valueAnimator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
                @Override
                public void onAnimationUpdate(ValueAnimator animation) {
                    try {
                        float v = animation.getAnimatedFraction();
                        LatLng newPosition = latLngInterpolator.interpolate(v, startPosition, endPosition);
//                        googleMap.moveCamera(CameraUpdateFactory.newCameraPosition(new CameraPosition.Builder()
//                                .target(newPosition)
//                                .zoom(15.5f)
//                                .build()
//                                ));
                        float rotation = bearingBetweenLocations(startPosition, endPosition);
                        if (rotation > 1.0)
                            marker.setRotation(rotation);
                        marker.setPosition(newPosition);
                        markers.put("ic_auto_map",marker);
                    } catch (Exception ex) {
                        //I don't care atm..
                    }
                }
            });
            valueAnimator.addListener(new AnimatorListenerAdapter() {
                @Override
                public void onAnimationEnd(Animator animation) {
                    super.onAnimationEnd(animation);
                }
            });
            valueAnimator.start();
        }
    }

    private interface LatLngInterpolatorNew {
        LatLng interpolate(float fraction, LatLng a, LatLng b);

        class LinearFixed implements LatLngInterpolatorNew {
            @Override
            public LatLng interpolate(float fraction, LatLng a, LatLng b) {
                double lat = (b.latitude - a.latitude) * fraction + a.latitude;
                double lngDelta = b.longitude - a.longitude;
                // Take the shortest path across the 180th meridian.
                if (Math.abs(lngDelta) > 180) {
                    lngDelta -= Math.signum(lngDelta) * 360;
                }
                double lng = lngDelta * fraction + a.longitude;
                return new LatLng(lat, lng);
            }
        }
    }
    @JavascriptInterface
    public void drawRoute(final String json, final String style, final String trackColor, final boolean isActual, final String sourceMarker, final String destMarker, final int polylineWidth, String type, String sourceName, String destinationName) {
//        ArrayList<Polyline> lines = new ArrayList<>();
//        polylines.add(lines);
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if(googleMap!=null) {
                    PolylineOptions polylineOptions = new PolylineOptions();
                    int color = Color.parseColor(trackColor);
                    try {
                        System.out.println("inside_drawRoute_try");
                        JSONObject jsonObject = new JSONObject(json);
                        JSONArray coordinates = jsonObject.getJSONArray("points");
                        JSONArray journeyCoordinates = jsonObject.getJSONArray("journeyCoordinates");
                        JSONObject sourceCoordinates = (JSONObject) journeyCoordinates.get(0);
                        JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length()-1);
                        double sourceLat = sourceCoordinates.getDouble("lat");
                        double sourceLong = sourceCoordinates.getDouble("lng");
                        double destLat = destCoordinates.getDouble("lat");
                        double destLong = destCoordinates.getDouble("lng");
                        if (sourceLat != 0.0 && sourceLong != 0.0 && destLat != 0.0 && destLong != 0.0)
                        {
                            moveCamera(sourceLat, sourceLong, destLat, destLong, coordinates);
                        }
                        if(coordinates.length() == 1){
                            JSONObject coordinate = (JSONObject) coordinates.get(0);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            upsertMarker("ic_auto_map",String.valueOf(lat), String.valueOf(lng), 90, 0.5f, 0.5f);
                            return;
                        }
                        if(isActual){
                            for (int i = coordinates.length() -1 ; i >= 0 ; i--) {
                                JSONObject coordinate = (JSONObject) coordinates.get(i);
                                double lng = coordinate.getDouble("lng");
                                double lat = coordinate.getDouble("lat");
                                polylineOptions.add(new LatLng(lat, lng));
                            }
                        }else{
                            LatLng fromPointObj = new LatLng(sourceLat, sourceLong);
                            LatLng toPointObj = new LatLng(destLat, destLong);
                            polylineOptions.add(fromPointObj);
                            polylineOptions.add(toPointObj);
//                            double k = 0.5;
//                            double distanceBetweenTwoLatLon = SphericalUtil.computeDistanceBetween(fromPointObj, toPointObj);
//                            double headingForTwoLatLon = SphericalUtil.computeHeading(fromPointObj, toPointObj);
//                            LatLng computedDestinationCoordinates = SphericalUtil.computeOffset(fromPointObj, distanceBetweenTwoLatLon * 0.5, headingForTwoLatLon);
//                            double x = (1 - k * k) * distanceBetweenTwoLatLon * 0.5 / (2 * k);
//                            double r = (1 + k * k) * distanceBetweenTwoLatLon * 0.5 / (2 * k);
//                            LatLng finalComputedOffset = SphericalUtil.computeOffset(computedDestinationCoordinates, x, headingForTwoLatLon > 40 ? headingForTwoLatLon + 90.0 : headingForTwoLatLon - 90.0);
//                            double h1 = SphericalUtil.computeHeading(finalComputedOffset, fromPointObj);
//                            double h2 = SphericalUtil.computeHeading(finalComputedOffset, toPointObj);
//                            int numpoints = 50;
//                            double step = (h2 - h1) / numpoints;
//                            for (int i = 0; i < numpoints; i++) {
//                                LatLng pi = SphericalUtil.computeOffset(finalComputedOffset, r, h1 + i * step);
//                                polylineOptions.add(pi);
//                            }
                        }

                        polylines = setRouteCustomTheme(polylineOptions, color, style, polylineWidth);
                        LatLng sourceLatLng = new LatLng(sourceLat, sourceLong);
                        LatLng destLatLng = new LatLng(destLat, destLong);
                        System.out.println("zxc src: " + sourceName);
                        System.out.println("zxc dest: " + destinationName);

                        if(destMarker != null && !destMarker.equals("")) {
                            List<LatLng> points = polylineOptions.getPoints();
                            LatLng dest = points.get(0);
                            MarkerOptions markerObj = new MarkerOptions()
                                    .title("marker")
                                    .position(dest)
                                    .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(destinationName, destMarker)));

                            Marker tempmarker = googleMap.addMarker(markerObj);
                            markers.put("marker", tempmarker);

                        }
                        if (type.equals("DRIVER_LOCATION_UPDATE"))
                        {
                            System.out.println("inside insert marker");
                            List<LatLng> points = polylineOptions.getPoints();
                            LatLng source = points.get(points.size() - 1);
                            upsertMarker("ic_auto_map",String.valueOf(source.latitude),String.valueOf(source.longitude), 90, 0.5f, 0.5f);
                            Marker currMarker = (Marker) markers.get("ic_auto_map");
                            int index = polylines.getPoints().size()-1;
                            float rotation = bearingBetweenLocations(polylines.getPoints().get(index), polylines.getPoints().get(index -1));
                            if (rotation > 1.0) currMarker.setRotation(rotation);
                            markers.put("ic_auto_map",currMarker);
                        } else if(sourceMarker != null && !sourceMarker.equals("")) {
                            System.out.println("sourcelatlong: " + sourceLatLng);
                            System.out.println("destlatlong: " + destLatLng);
                            List<LatLng> points = polylineOptions.getPoints();
                            LatLng source = points.get(points.size() - 1);
                            MarkerOptions markerObj = new MarkerOptions()
                                    .title("dest_marker")
                                    .position(source)
                                    .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(sourceName,sourceMarker)));
                            Marker tempmarker = googleMap.addMarker(markerObj);
                            markers.put("dest_marker", tempmarker);
                        }
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
    }
    @JavascriptInterface
    public void removeAllPolylines(String str) {
        removeMarker("ic_auto_map");
        removeMarker("marker");
        removeMarker("dest_marker");
                if (polylines != null) {
                    activity.runOnUiThread(
                            new Runnable() {
                                @Override
                                public void run() {
                                    polylines.remove();
                                    polylines = null;
                                }
                            }
                    );
                }
            }

    public Polyline setRouteCustomTheme(PolylineOptions options, int color, String style, final int width) {
        PatternItem DOT = new Dot();
        PatternItem GAP = new Gap(10);
        PatternItem DASH = new Dash(20);
        options.width(width);
        List<PatternItem> PATTERN_POLYLINE_DOTTED = Arrays.asList(GAP, DOT);
        List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Arrays.asList(DASH);
        options.color(color);
        switch (style) {
            case "DASH":
                options.pattern(PATTERN_POLYLINE_DOTTED_DASHED);
                break;
            case "DOT":
                options.pattern(PATTERN_POLYLINE_DOTTED);
                break;
            default:
                break;

        }
        return googleMap.addPolyline(options);
    }

    @JavascriptInterface
    public void animateCamera(final double lat, final double lng, final int zoom) {
        System.out.println("animateCamera " + lat + " " + lng);
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if (googleMap != null) {
                        LatLng latLngObj = new LatLng(lat, lng);
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLngObj, zoom));
                    }
                }catch (Exception e){
                    e.printStackTrace();
                    System.out.println("here in catch");
                }
            }
        });
    }

    @JavascriptInterface
    public void moveCamera(final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                double source_lat, source_lng, destination_lat, destination_lng;
                if (source_latitude <= destination_latitude) {
                    source_lat = source_latitude; // - 0.4*(destination_latitude-source_latitude);
                    destination_lat = destination_latitude; // + 0.1*(destination_latitude-source_latitude);
                } else {
                    source_lat = source_latitude;// + 0.1*(source_latitude-destination_latitude);
                    destination_lat = destination_latitude;// - 0.4*(source_latitude-destination_latitude);
                }
                if (source_longitude <= destination_longitude) {
                    source_lng = source_longitude;// - 1.15*(destination_longitude-source_longitude);
                    destination_lng = destination_longitude;// + 1.15*(destination_longitude-source_longitude);
                } else {
                    source_lng = source_longitude;// + 2.15*(source_longitude-destination_longitude);
                    destination_lng = destination_longitude;// - 2.15*(source_longitude-destination_longitude);
                }

                if(googleMap!=null) {
                    try {
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                        googleMap.setPadding(100, 150, 100, getScreenHeight()/2);
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds,0));
                        googleMap.setPadding(0, 0, 0, 0);
                    } catch (IllegalArgumentException e) {
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                    }
                    catch(Exception e){
                        System.out.println("In mmove camera in catch exception" + e);
                    }
                }
            }
        });
    }

    @JavascriptInterface
    public void moveCamera(final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude, final JSONArray json_coordinates) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                double source_lat, source_lng, destination_lat, destination_lng;

                Log.i("json_coordinates", String.valueOf(json_coordinates));
                ArrayList<Double> all_latitudes = new ArrayList<Double>();
                ArrayList<Double> all_longitudes = new ArrayList<Double>();
                for (int i = 0; i < json_coordinates.length(); i++) {
                    JSONObject each_json_coordinates = null;
                    try {
                        each_json_coordinates = (JSONObject) json_coordinates.get(i);
                        ArrayList<Double> each_coordinates = new ArrayList<Double>();
                        double lon = each_json_coordinates.getDouble("lng");
                        double lat = each_json_coordinates.getDouble("lat");
                        all_latitudes.add(lat);
                        all_longitudes.add(lon);
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
                Log.i("all_latitudes", String.valueOf(all_latitudes));
                Log.i("all_longitudes", String.valueOf(all_longitudes));
                double minimum_latitude = Collections.min(all_latitudes);
                double maximum_latitude = Collections.max(all_latitudes);
                double minimum_longitude = Collections.min(all_longitudes);
                double maximum_longitude = Collections.max(all_longitudes);
                Log.i("minimum_latitude", String.valueOf(minimum_latitude));
                Log.i("maximum_latitude", String.valueOf(maximum_latitude));

                if (source_latitude <= destination_latitude) {
                    source_lat = minimum_latitude - 1.3*(maximum_latitude - minimum_latitude);
                    destination_lat = maximum_latitude + 0.1*(maximum_latitude - minimum_latitude);
                } else {
                    source_lat = maximum_latitude + 0.1*(maximum_latitude - minimum_latitude);
                    destination_lat = minimum_latitude - 1.3*(maximum_latitude - minimum_latitude);
                }
                if (source_longitude <= destination_longitude) {
                    source_lng = minimum_longitude - 0.09*(maximum_longitude - minimum_longitude);
                    destination_lng = maximum_longitude + 0.09*(maximum_longitude - minimum_longitude);
                } else {
                    source_lng = maximum_longitude + 0.09*(maximum_longitude - minimum_longitude);
                    destination_lng = minimum_longitude - 0.09*(maximum_longitude - minimum_longitude);
                }

                Log.i("coordinates points", String.valueOf(json_coordinates));

                if(googleMap!=null) {
                    try {
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                    } catch (IllegalArgumentException e) {
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                    }
                    catch(Exception e){
                        System.out.println("In mmove camera in catch exception" + e);
                    }
                }
            }
        });
    }

    @JavascriptInterface
    public void currentPosition(String str) {
        System.out.println("Fetch Current Position");
        showLocationOnMap();
    }

    private void showLocationOnMap() {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
                    return;
                }
                client.getLastLocation().addOnSuccessListener(activity, new OnSuccessListener<Location>() {
                    @Override
                    public void onSuccess(Location location) {
                        if (location != null && googleMap!=null) {
                            Double lat = location.getLatitude();
                            Double lng = location.getLongitude();
                            lastLatitudeValue = lat;
                            lastLongitudeValue = lng;
                            LatLng latLng = new LatLng(location.getLatitude(), location.getLongitude());
                            if (userPositionMarker == null) {
                                upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng),160, 0.5f,0.5f); //TODO this function will be removed
                            } else {
                                userPositionMarker.setPosition(latLng);
                            }
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                        } else {
                            Log.e(LOG_TAG, "Current position not known4");
                        }
                    }
                }).addOnFailureListener(activity, new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                        Log.e(LOG_TAG, "Current position not known5");
                    }
                });
            }
        });
    }

    @JavascriptInterface
    public void uploadFile (){
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if ((ActivityCompat.checkSelfPermission(activity, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(activity, CAMERA) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(activity, READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)){
                    Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                    String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                    SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    sharedPref.edit().putString(context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp).apply();
                    Uri photoFile = FileProvider.getUriForFile(context.getApplicationContext(),context.getResources().getString(R.string.fileProviderPath), new File(context.getApplicationContext().getFilesDir(), "IMG_" + timeStamp+".jpg"));
                    takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
                    Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
                    chooseFromFile.setType("image/*");
                    Intent chooser = Intent.createChooser(takePicture, "Upload Image");
                    chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[] { chooseFromFile });
                    startActivityForResult(activity, chooser, IMAGE_CAPTURE_REQ_CODE, null);
                } else {
                    ActivityCompat.requestPermissions(activity, new String[]{CAMERA, WRITE_EXTERNAL_STORAGE, READ_EXTERNAL_STORAGE}, IMAGE_PERMISSION_REQ_CODE);
                }
            }
        });
    }
    @JavascriptInterface
    public void copyToClipboard (String inputText){
                ClipboardManager clipboard = (ClipboardManager) activity.getSystemService(Context.CLIPBOARD_SERVICE);
                ClipData clip = ClipData.newPlainText("Text", inputText);
                clipboard.setPrimaryClip(clip);
    }


    @JavascriptInterface
    public void  generatePDF (String str) throws JSONException {
        invoice = str;
        if ((ActivityCompat.checkSelfPermission(activity, READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(activity, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED))
        {
                    downloadPDF(str,activity,context);
        }
        else
        {
            ActivityCompat.requestPermissions(activity, new String[]{WRITE_EXTERNAL_STORAGE}, 67);
        }
    }

    public static void downloadPDF(String str, Activity activity, Context context) throws JSONException {
        JSONObject state = new JSONObject(str);
        JSONObject data = new JSONObject();
        JSONObject props = new JSONObject();
        data = state.optJSONObject("data");
        props= state.optJSONObject("props");
        int pageHeight = 1555;
        int pagewidth = 960;
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String userName =  sharedPref.getString("USER_NAME","__failed");
        JSONObject selectedItem = new JSONObject();
        selectedItem = data.getJSONObject("selectedItem");
        Bitmap logo = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_launcher);
        Bitmap src_icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_green_circle);
        Bitmap dest_icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_red_circle);
        Bitmap ic_line = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_line_pdf);
        Bitmap scaledBmp = null;
        PdfDocument pdfDocument = new PdfDocument();
        Paint image = new Paint();
        Paint text_16 = new Paint();
        Paint text_20 = new Paint();
        PdfDocument.PageInfo invoicePDF = new PdfDocument.PageInfo.Builder(pagewidth, pageHeight, 1).create();
        PdfDocument.Page myPage = pdfDocument.startPage(invoicePDF);
        Canvas canvas = myPage.getCanvas();
        text_16.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.NORMAL));
        text_16.setTextSize(15);
        text_16.setColor(ContextCompat.getColor(context, R.color.colorBodyText));
        text_20.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.NORMAL));
        text_20.setTextSize(20);
        text_20.setColor(ContextCompat.getColor(context, R.color.colorAccent));
        scaledBmp = Bitmap.createScaledBitmap(logo, 50, 50, false);
        canvas.drawBitmap(scaledBmp, 100, 82, image);
        text_16.setColor(ContextCompat.getColor(context, R.color.colorAccent));
        canvas.drawText(selectedItem.getString("date"), 786, 90, text_16);
        image.setColor(ContextCompat.getColor(context, R.color.colorBodyText));
        canvas.drawLine(100, 138, pagewidth - 100, 138, image);
        Paint text_24 = new Paint();
        text_24.setTypeface(Typeface.create(Typeface.SERIF, Typeface.NORMAL));
        text_24.setTextSize(24);
        canvas.drawText("Hey " + userName + ", here’s the invoice for your trip with us", 100, 166, text_20);
        text_20.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.BOLD));
        canvas.drawText("Total Amount", 100, 249, text_20);
        text_20.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("totalAmount"), pagewidth - 100, 249, text_20);
        text_20.setTextAlign(Paint.Align.LEFT);
        image.setColor(ContextCompat.getColor(context, R.color.colorAccent));
        canvas.drawLine(100, 305, pagewidth - 100, 305, image);
        text_16.setColor(ContextCompat.getColor(context, R.color.colorBodyText));
        text_16.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.NORMAL));
        canvas.drawText("Trip Charges", 102, 333, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("totalAmount"), pagewidth - 100, 333, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        image.setColor(ContextCompat.getColor(context, R.color.colorBodyText));
        canvas.drawLine(100, 381, pagewidth - 100, 381, image);
        text_16.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.BOLD));
        text_16.setColor(ContextCompat.getColor(context, R.color.colorAccent));
        canvas.drawText("Subtotal", 100, 409, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("totalAmount"), pagewidth - 100, 409, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        text_16.setColor(ContextCompat.getColor(context, R.color.colorBodyText));
        text_16.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.NORMAL));
        canvas.drawText("Promotion", 102, 449, text_16);
        text_16.setColor(ContextCompat.getColor(context, R.color.PrimaryBlue));
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText("-₹0", pagewidth - 100, 449, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        image.setColor(ContextCompat.getColor(context, R.color.colorBodyText));
        canvas.drawLine(100, 497, pagewidth - 100, 497, image);
        text_16.setColor(ContextCompat.getColor(context, R.color.colorBodyText));
        text_16.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.NORMAL));
        canvas.drawText("Final Amount (Before Tax)", 102, 525, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("totalAmount"), pagewidth - 100, 525, text_16);
//        text_16.setTextAlign(Paint.Align.LEFT);
//        canvas.drawText("GST", 102, 565, text_16);
//        text_16.setTextAlign(Paint.Align.RIGHT);
//        canvas.drawText("₹0", pagewidth-100, 565, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        image.setColor(ContextCompat.getColor(context, R.color.colorBodyText));
        canvas.drawLine(100, 613, pagewidth - 100, 613, image);
        text_16.setColor(ContextCompat.getColor(context, R.color.colorAccent));
        text_16.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.BOLD));
        canvas.drawText("Final Amount Paid", 102, 641, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("totalAmount"), pagewidth - 100, 641, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        text_20.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.BOLD));
        canvas.drawText("Payment Details", 100, 785, text_20);
        text_20.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("totalAmount"), pagewidth - 100, 785, text_20);
        text_20.setTextAlign(Paint.Align.LEFT);
        canvas.drawLine(100, 837, pagewidth - 100, 837, image);
        text_16.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.NORMAL));
        text_16.setColor(ContextCompat.getColor(context, R.color.colorAccent));
        canvas.drawText("Paid via", 102, 865, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText("Cash", pagewidth - 100, 865, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        canvas.drawText("Date & Time", 102, 905, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("date") + " " + selectedItem.getString("time"), pagewidth - 100, 905, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        text_20.setColor(ContextCompat.getColor(context, R.color.colorAccent));
        canvas.drawText("Trip Details", 100, 1015, text_20);
        canvas.drawLine(100, 1067, pagewidth - 100, 1067, image);
        canvas.drawText("Driver Name", 102, 1095, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("driverName"), pagewidth - 100, 1095, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        canvas.drawText("License Plate", 102, 1135, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("vehicleNumber"), pagewidth - 100, 1135, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        canvas.drawText("Trip ID", 102, 1175, text_16);
        text_16.setTextAlign(Paint.Align.RIGHT);
        canvas.drawText(selectedItem.getString("shortRideId"), pagewidth - 100, 1175, text_16);
        text_16.setTextAlign(Paint.Align.LEFT);
        scaledBmp = Bitmap.createScaledBitmap(src_icon, 10, 10, false);
        canvas.drawBitmap(scaledBmp, 100, 1267, image);
        scaledBmp = Bitmap.createScaledBitmap(ic_line, 1, 95, false);
        canvas.drawBitmap(scaledBmp, 105, 1275, image);
        scaledBmp = Bitmap.createScaledBitmap(dest_icon, 10, 10, false);
        canvas.drawBitmap(scaledBmp, 100, 1369, image);
        text_16.setTypeface(Typeface.create(Typeface.DEFAULT, Typeface.NORMAL));
        text_16.setColor(ContextCompat.getColor(context, R.color.colorAccent));
        canvas.drawText(selectedItem.getString("rideStartTime"), 130, 1275, text_16);
        canvas.drawText(selectedItem.getString("source"), 130, 1300, text_16);
        canvas.drawText(selectedItem.getString("rideEndTime"), 130, 1369, text_16);
        canvas.drawText(selectedItem.getString("destination"), 130, 1400, text_16);
        pdfDocument.finishPage(myPage);
        String fileName = "Invoice_Namma_Yatri_" + selectedItem.getString("rideId") + ".pdf";
        File file = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS), fileName);
        System.out.println("PDF");
        System.out.println(file.getPath());
        try {
            pdfDocument.writeTo(new FileOutputStream(file));
            Uri path = FileProvider.getUriForFile(context.getApplicationContext(),context.getResources().getString(R.string.fileProviderPath), file);
            Intent pdfOpenintent = new Intent(Intent.ACTION_VIEW);
            pdfOpenintent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_ACTIVITY_CLEAR_TOP);
//            pdfOpenintent.setFlags();
            pdfOpenintent.setDataAndType(path, "application/pdf");
            PendingIntent pendingIntent = PendingIntent.getActivity(context, 234567 , pdfOpenintent, 0);
            NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context,"General") ;
            mBuilder.setLargeIcon(logo);
                    mBuilder.setContentTitle("Invoice Downloaded")
                            .setSmallIcon((R.drawable.ic_launcher))
                    .setContentText("Invoice for your ride is downloaded!!!")
                    .setAutoCancel(true)
                    .setPriority(NotificationCompat.PRIORITY_DEFAULT);
                    mBuilder.setContentIntent(pendingIntent);
            NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
            notificationManager.notify(234567, mBuilder.build());
//            Toast.makeText(activity, "Invoice downloaded successfully.", Toast.LENGTH_SHORT).show();
        } catch (IOException e) {
            e.printStackTrace();
        }
        pdfDocument.close();

    }

    //###########################################################################
    //region: MAP FUNCTIONS Ends ------------------------------------------------
    //###########################################################################

    private class DatePickerLabels {
        private static final String MAXIMUM_PRESENT_DATE = "MAXIMUM_PRESENT_DATE";
        private static final String MINIMUM_EIGHTEEN_YEARS = "MINIMUM_EIGHTEEN_YEARS";
        private static final String MIN_EIGHTEEN_MAX_SIXTY_YEARS = "MIN_EIGHTEEN_MAX_SIXTY_YEARS";
        private static final String MAX_THIRTY_DAYS_FROM_CURRENT_DATE = "MAX_THIRTY_DAYS_FROM_CURRENT_DATE";
    }
}
