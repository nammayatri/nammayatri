/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility;

import android.Manifest;
import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.Activity;

import android.app.ActivityManager;
import android.app.AlertDialog;
import android.app.DatePickerDialog;
import android.app.PendingIntent;
import android.app.TimePickerDialog;
import android.content.ActivityNotFoundException;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.PixelFormat;
import android.graphics.PorterDuff;
import android.graphics.Rect;
import android.graphics.RectF;
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
import android.print.PrintAttributes;
import android.print.pdf.PrintedPdfDocument;
import android.provider.ContactsContract;
import android.provider.MediaStore;
import android.provider.Settings;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.ActionMode;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.PixelCopy;
import android.view.SurfaceView;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.animation.LinearInterpolator;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.webkit.JavascriptInterface;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.NumberPicker;
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

import androidx.core.text.HtmlCompat;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;
import androidx.work.Constraints;
import androidx.work.ExistingPeriodicWorkPolicy;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;

import com.airbnb.lottie.LottieAnimationView;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.common.api.ApiException;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.PendingResult;
import com.google.android.gms.common.api.ResolvableApiException;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.LocationSettingsRequest;
import com.google.android.gms.location.LocationSettingsResponse;
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
import com.google.android.gms.tasks.CancellationTokenSource;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
//import com.google.android.material.snackbar.Snackbar;
//import com.google.firebase.installations.FirebaseInstallations;
import com.google.android.libraries.places.api.model.AutocompleteSessionToken;
import com.google.android.play.core.review.ReviewInfo;
import com.google.android.play.core.review.ReviewManager;
import com.google.android.play.core.review.ReviewManagerFactory;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerFullScreenListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.options.IFramePlayerOptions;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.ui.DefaultPlayerUiController;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.URL;
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
import java.io.ByteArrayOutputStream;
import java.lang.Math;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.zip.Inflater;

import in.juspay.mobility.utils.LocationUpdateWorker;
import in.juspay.mobility.utils.CheckPermissionAutoStart;
import in.juspay.mobility.utils.CheckPermissionOverlay;
import in.juspay.mobility.utils.LocationUpdateService;
import in.juspay.mobility.utils.MediaPlayerView;
import in.juspay.mobility.utils.NotificationUtils;
import in.juspay.mobility.utils.OtpUtils;
import in.juspay.mobility.utils.mediaPlayer.DefaultMediaPlayerControl;
import in.juspay.mobility.utils.mediaPlayer.MediaPlayerControl;
import in.juspay.hypersdk.core.HyperFragment;
import in.juspay.hypersdk.core.JBridge;
import in.juspay.hypersdk.core.JuspayDuiHook;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.utils.network.JuspayHttpResponse;
import in.juspay.hypersdk.utils.network.NetUtils;
import in.juspay.hypersdk.core.DuiCallback;

import static android.Manifest.permission.ACCESS_BACKGROUND_LOCATION;
import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static androidx.core.app.ActivityCompat.startActivityForResult;
import static androidx.core.content.ContextCompat.getCodeCacheDir;
import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static android.content.Context.WINDOW_SERVICE;
import static android.content.Context.ACTIVITY_SERVICE;
import static android.view.View.LAYER_TYPE_SOFTWARE;
import java.net.URISyntaxException;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.content.pm.ResolveInfo;

public class CommonJsInterface extends JBridge implements in.juspay.hypersdk.core.JSI {


    private static final String LOG_TAG = "Beckn_JsInterface";
    private static final String LOCATE_ON_MAP = "LocateOnMap";
    private static final String DISCOUNT_END = "Discount End Date";
    private static final String DISCOUNT_START = "Discount Start Date";
    private static final String CURRENT_LOCATION = "ny_ic_customer_current_location";
    private static final String CURRENT_LOCATION_LATLON = "Current Location";
    public static final int LOCATION_PERMISSION_REQ_CODE = 1;
    public static final int STORAGE_PERMISSION = 67;
    public static float videoDuration = 0;
    private Activity activity;
    private JuspayServices juspayServices;
    private Context context;
    private DuiCallback dynamicUI;
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
    public static final int REQUEST_CONTACTS = 7;
    public static String phoneNumber;
    public static String invoice =null;
    public static String invoiceType =null;
    public static boolean permissionCheck = false;
    public static String storeCallBackPopUp = null;
    public static String storeOnResumeCallback = null;
    private LottieAnimationView animationView;
    public static YouTubePlayerView youTubePlayerView;
    public static YouTubePlayer youtubePlayer;
    private int distanceRemaining = -1;
    private static final int DATEPICKER_SPINNER_COUNT = 3;
    private static String storeMapCallBack = null;
    CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
    public static ArrayList<MediaPlayerView> audioPlayers = new ArrayList<>();
    public static String webViewCallBack = null;

    public CommonJsInterface(){
        super();
    }

    public CommonJsInterface(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super(juspayServices, activity, fragment);
        this.context = activity;
        this.dynamicUI = juspayServices.getDuiCallback();
        this.activity = activity;
        this.juspayServices = juspayServices;
        client = LocationServices.getFusedLocationProviderClient(context);
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
       this.dynamicUI = juspayServices.getDuiCallback();
   }

   @Override
    public void setActivity(Activity activity) {
        super.setActivity(activity);
        this.activity = activity;
        fetchLatLonAndUpdate();
    }

    @Override
    public void set(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super.set(juspayServices, activity, fragment);
        this.context = juspayServices.getContext();
        this.dynamicUI = juspayServices.getDuiCallback();
        this.activity = activity;
        this.juspayServices = juspayServices;
        client = LocationServices.getFusedLocationProviderClient(context);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        fetchLatLonAndUpdate();
    }

    @JavascriptInterface
    public void toast(String msg) {
        Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
    }

    private void fetchLatLonAndUpdate(){
        String lat = getKeyInNativeSharedPrefKeys("LAST_KNOWN_LAT");
        String lon = getKeyInNativeSharedPrefKeys("LAST_KNOWN_LON");
        lastLatitudeValue = lat != "__failed" ? Double.parseDouble(lat) : lastLatitudeValue;
        lastLongitudeValue = lon != "__failed" ? Double.parseDouble(lon) : lastLongitudeValue;
    }

    @JavascriptInterface
    public void setKeysInSharedPrefs(String key, String value) {
        KeyValueStore.write(juspayServices, key, value);
        setEnvInNativeSharedPrefKeys(key, value);
        if (MainActivity.getInstance().getResources().getString(R.string.service).equals(context.getResources().getString(R.string.nammayatripartner))){
            if (key.equals(context.getResources().getString(R.string.LANGUAGE_KEY))){
                updateLocaleResource(value);
            }
        }
    }

    public static void updateLocaleResource(String languageKey){
        Context context = MainActivity.getInstance().getApplicationContext();
        Locale locale;
        switch (languageKey){
            case "HI_IN" :
                locale = new Locale("hi");
                break;
            case "KN_IN" :
                locale = new Locale("kn");
                break;
            case "EN_US" :
                locale = new Locale("en");
                break;
            case "TA_IN" :
                locale = new Locale("ta");
                break;
            default:
                return;
        }
        Locale.setDefault(locale);
        Configuration configuration = context.getResources().getConfiguration();
        configuration.setLocale(locale);
        context.getResources().updateConfiguration(configuration,context.getResources().getDisplayMetrics());
    }

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

                        if (dynamicUI != null && (juspayServices.getDynamicUI() != null)) {
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

                        if (dynamicUI != null && (juspayServices.getDynamicUI() != null)) {
                            dynamicUI.addJsToWebView(javascript);
                        }
                    }
                } else {
                    String base64Data = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');", callback, "failure", base64Data, -1, Base64.encodeToString(url.getBytes(), Base64.NO_WRAP));

                    if (dynamicUI != null && (juspayServices.getDynamicUI() != null)) {
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

    static String storeCallB = null;
    @JavascriptInterface
    public void storeCallBackForNotification(String callback) {
        storeCallB = callback;
        storeCallBDriverLocationPermission = null;
        storeCallBOverlayPermission = null;
        storeCallBBatteryUsagePermission = null;
    }


    public static void callingStoreCall(DuiCallback dynamicUII, String notificationType){
        if (dynamicUII != null && storeCallB !=null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallB, notificationType);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void storeOnResumeCallback(String callback) {
        storeOnResumeCallback = callback;
    }

    public static void callOnResumeUpdateCallback(DuiCallback dynamicUII) {
        if (dynamicUII != null && storeOnResumeCallback != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s');", storeOnResumeCallback);
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

    public static void callingStoreCallBackPopUp (DuiCallback dynamicUII, JSONObject entity_payload){
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


    public static void callingStoreCallCustomer(DuiCallback dynamicUII, String notificationType){
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


    public static void callingStoreCallBackTime(DuiCallback dynamicUII, String time, String lat, String lng, Context context){
        if (dynamicUII != null && storeCallBTime != "TimeUpdate") {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
            storeCallBTime,time, lat, lng);
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
                                if (dynamicUI != null && (juspayServices.getDynamicUI() != null)) {
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
        i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
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
                        if (dynamicUI != null && (juspayServices.getDynamicUI() != null)) {
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

    private void reOrderSpinners(DatePickerDialog dialog, char[] dateOrder) {
        if(!dialog.isShowing()) {
            return;
        }

        final int yearId = Resources.getSystem().getIdentifier("year", "id", "android");
        final int monthId = Resources.getSystem().getIdentifier("month", "id", "android");
        final int dayId = Resources.getSystem().getIdentifier("day", "id", "android");
        final int layoutId = Resources.getSystem().getIdentifier("pickers", "id", "android");

        final NumberPicker yearSpinner = (NumberPicker) dialog.findViewById(yearId);
        final NumberPicker monthSpinner = (NumberPicker) dialog.findViewById(monthId);
        final NumberPicker daySpinner = (NumberPicker) dialog.findViewById(dayId);
        final LinearLayout layout = (LinearLayout) dialog.findViewById(layoutId);

        layout.removeAllViews();
        for (int i = 0; i < DATEPICKER_SPINNER_COUNT; i++) {
            switch (dateOrder[i]) {
                case 'y':
                    layout.addView(yearSpinner);
                    setImeOptions(yearSpinner, i);
                    break;
                case 'm':
                    layout.addView(monthSpinner);
                    setImeOptions(monthSpinner, i);
                    break;
                case 'd':
                    layout.addView(daySpinner);
                    setImeOptions(daySpinner, i);
                    break;
                default:
                    throw new IllegalArgumentException("Invalid DateOrder");
            }
        }
    }

    private void setImeOptions(NumberPicker spinner, int spinnerIndex) {
        final int imeOption;
        if (spinnerIndex < DATEPICKER_SPINNER_COUNT - 1) {
            imeOption = EditorInfo.IME_ACTION_NEXT;
        }
        else {
            imeOption = EditorInfo.IME_ACTION_DONE;
        }
        int idPickerInput = Resources.getSystem().getIdentifier("numberpicker_input", "id", "android");
        TextView input = (TextView) spinner.findViewById(idPickerInput);
        input.setImeOptions(imeOption);
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
                int datePickerTheme = AlertDialog.THEME_HOLO_LIGHT;
                if(Build.VERSION.SDK_INT <= Build.VERSION_CODES.N) datePickerTheme = 0;

                DatePickerDialog datePickerDialog = new DatePickerDialog(activity, datePickerTheme, new DatePickerDialog.OnDateSetListener() {
                    @Override
                    public void onDateSet(DatePicker datePicker, int year, int month, int date) {
                        if (dynamicUI != null && callback != null && juspayServices.getDynamicUI() != null) {
                            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d,%d,%d);",
                                callback, year, month, date);
                            dynamicUI.addJsToWebView(javascript);
                        }
                    }


                }, mYear, mMonth, mDate){

                    final int month = getContext().getResources().getIdentifier("month", "id", "android");
                    final String[] monthNumbers =
                    {
                            "Jan (01)",
                            "Feb (02)",
                            "Mar (03)",
                            "April (04)",
                            "May (05)",
                            "June (06)",
                            "July (07)",
                            "Aug (08)",
                            "Sept (09)",
                            "Oct (10)",
                            "Nov (11)",
                            "Dec (12)"
                    };

                    @Override
                    public void onDateChanged(@NonNull DatePicker view, int y, int m, int d) {
                        super.onDateChanged(view, y, m, d);
                        try {
                            if(Build.VERSION.SDK_INT > Build.VERSION_CODES.N) {
                                if (month != 0) {
                                    NumberPicker monthPicker = findViewById(month);
                                    if (monthPicker != null) {
                                        monthPicker.setDisplayedValues(monthNumbers);
                                    }
                                }
                            }
                        } catch (Exception e) {
                            Log.e(LOG_TAG,"Error in onDateChanged : " + e);
                        }
                    }

                    @Override
                    protected void onCreate(Bundle savedInstanceState)
                    {
                        super.onCreate(savedInstanceState);
                        try {
                            if(Build.VERSION.SDK_INT > Build.VERSION_CODES.N) {
                                if (month != 0) {
                                    NumberPicker monthPicker = findViewById(month);
                                    if (monthPicker != null) {
                                        monthPicker.setDisplayedValues(monthNumbers);
                                    }
                                }
                            }
                        } catch (Exception e){
                            Log.e(LOG_TAG,"Error in Date onCreate : " + e);
                        }
                    }
                };

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
                if(Build.VERSION.SDK_INT > Build.VERSION_CODES.N) datePickerDialog.setTitle(context.getString(R.string.select_date));
                else datePickerDialog.setTitle("");
                datePickerDialog.show();
                final char[] dateOrder =
                {
                    'd',
                    'm',
                    'y'
                };
                try {
                    if(Build.VERSION.SDK_INT > Build.VERSION_CODES.N) reOrderSpinners(datePickerDialog, dateOrder);
                } catch (Exception e) {
                    Log.e(LOG_TAG,"Error in reOrdering spinners : " + e);
                }
            }
        });
    }

    static String storeCallBInternetAction = "storeCallBackInternet";
    static String storeCallBackImageUpload = "imageUpload";
    static String storeCallBDriverLocationPermission = null;
    static String storeCallBOverlayPermission = null;
    static String storeCallBBatteryUsagePermission = null;


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

    public static void callingStoreCallBackBatteryUsagePermission(DuiCallback dynamicUII, String isPermission){
        if (dynamicUII != null && storeCallBBatteryUsagePermission!=null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBBatteryUsagePermission, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallBackDriverLocationPermission(DuiCallback dynamicUII, String isPermission){
        if (dynamicUII != null && storeCallBDriverLocationPermission!=null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBDriverLocationPermission, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallImageUpload(DuiCallback dynamicUII, String stringImage,String imageName){
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

    public static void callingStoreCallBackInternetAction(DuiCallback dynamicUII, String isPermission){
        if (dynamicUII != null && !storeCallBInternetAction.equals("storeCallBackInternet")) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
            storeCallBInternetAction, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallBackOverlayPermission(DuiCallback dynamicUII, String isPermission){
        if (dynamicUII != null && storeCallBOverlayPermission!=null) {
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
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                context.startActivity(intent);
            }
        }catch(ActivityNotFoundException e){
            e.printStackTrace();
            Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
            Uri uri = Uri.fromParts("package", getPackageName(), null);
            intent.setData(uri);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
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
        Intent locationUpdateService = new Intent(activity, LocationUpdateService.class);
        locationUpdateService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
        System.out.println( "In startLocationPollingAPI intent" + locationUpdateService);
        WorkManager mWorkManager ;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
        {
            context.getApplicationContext().startForegroundService(locationUpdateService);
        }
        else
        {
            context.startService(locationUpdateService);
        }
        mWorkManager = WorkManager.getInstance(context);
        Constraints constraints = new Constraints.Builder()
                .setRequiresDeviceIdle(true)
                .build();
        PeriodicWorkRequest mWorkRequest = new PeriodicWorkRequest.Builder(LocationUpdateWorker.class,13, TimeUnit.MINUTES).addTag(context.getString(R.string.location_update)).setConstraints(constraints).build();
        mWorkManager.enqueueUniquePeriodicWork(context.getString(R.string.location_update), ExistingPeriodicWorkPolicy.REPLACE,mWorkRequest);
    }

    @JavascriptInterface
    public void stopLocationPollingAPI() {
        Intent locationUpdateService = new Intent(activity, LocationUpdateService.class);
//        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
//        sharedPref.edit().putString("DRIVER_STATUS", "false").apply();
        activity.stopService(locationUpdateService);
        WorkManager mWorkManager = WorkManager.getInstance(context);
        mWorkManager.cancelAllWorkByTag(context.getString(R.string.location_update));
        Log.i(LOG_TAG, "stopLocationPollingAPI " );
    }

    //to check if works fine
    @JavascriptInterface
    public boolean isLocationPermissionEnabled() {
        return !(ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED);
    }

    @JavascriptInterface
    public boolean isOverlayPermissionEnabled() {
        if (NotificationUtils.overlayFeatureNotAvailable(context)){
            return true;
        }
        return Settings.canDrawOverlays(context);
    }

    @JavascriptInterface
    public boolean isLocationEnabled() {
        System.out.println("CommonJsInterface isLocationEnabled()");
        LocationManager locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        return locationManager != null && LocationManagerCompat.isLocationEnabled(locationManager);
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
        if (!isLocationPermissionEnabled()) {
            requestPermission();
        }
        resolvableLocationSettingsReq();
    }

    public void requestPermission() {
        try {
            ActivityCompat.requestPermissions(activity, new String[]{ACCESS_FINE_LOCATION}, LOCATION_PERMISSION_REQ_CODE);
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
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
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
    }

    private LocationCallback createLocCallBack (){
        return new LocationCallback() {
            @Override
            public void onLocationResult(@NonNull LocationResult locationResult) {
                super.onLocationResult(locationResult);
            }
        };
    }

    private LocationRequest createLocReq(int priority, long intervalMillis, long fastestIntervalMillis){
        LocationRequest locationRequest = LocationRequest.create()
                .setPriority(priority)
                .setInterval(intervalMillis)
                .setFastestInterval(fastestIntervalMillis);
        return locationRequest;
    }

    @SuppressLint("MissingPermission")
    private void updateLastKnownLocation(String callback, boolean animate){
        if (!isLocationPermissionEnabled()) return;

//        LocationRequest mLocationRequest =
//                createLocReq(Priority.PRIORITY_HIGH_ACCURACY, 1000, 500);
//        LocationCallback mLocationCallback =
//                createLocCallBack();

//        client.requestLocationUpdates(mLocationRequest, mLocationCallback, null);

        client.getCurrentLocation(Priority.PRIORITY_HIGH_ACCURACY, cancellationTokenSource.getToken())
                .addOnSuccessListener(activity, new OnSuccessListener<Location>() {
                    @Override
                    public void onSuccess(Location location) {
                        if (location != null) {
                            Double lat = location.getLatitude();
                            Double lng = location.getLongitude();
                            lastLatitudeValue = lat;
                            lastLongitudeValue = lng;
                            setEnvInNativeSharedPrefKeys("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                            setEnvInNativeSharedPrefKeys("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                            if (callback != null && dynamicUI != null && juspayServices.getDynamicUI() != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                                        callback, String.valueOf(lat), String.valueOf(lng));
                                dynamicUI.addJsToWebView(javascript);
                            }
                            if (animate && googleMap != null && lat!=null && lng != null){
                                LatLng latLng = new LatLng(lat, lng);
                                if (userPositionMarker == null) {
                                    upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng),160, 0.5f,0.9f); //TODO this function will be removed
                                } else {
                                    if (storeMapCallBack == null)
                                        userPositionMarker.setVisible(true);
                                    userPositionMarker.setPosition(latLng);
                                }
                                googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                            }
                        }
                        else getLastKnownLocationFromClientFallback(callback, animate);
                    }
                })
                .addOnFailureListener(activity, new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                        Log.e(LOG_TAG, "Current position not known");
                        getLastKnownLocationFromClientFallback(callback, animate);
                    }
                });

    }

    @SuppressLint("MissingPermission")
    private void getLastKnownLocationFromClientFallback(String callback, boolean animate){
        if (!isLocationPermissionEnabled()) return;
        client.getLastLocation()
                .addOnSuccessListener(activity, new OnSuccessListener<Location>() {
                    @Override
                    public void onSuccess(Location location) {
                        if (location != null) {
                            Double lat = location.getLatitude();
                            Double lng = location.getLongitude();
                            lastLatitudeValue = lat;
                            lastLongitudeValue = lng;
                            setEnvInNativeSharedPrefKeys("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                            setEnvInNativeSharedPrefKeys("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                            if (callback != null && dynamicUI != null && juspayServices.getDynamicUI() != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                                        callback, String.valueOf(lat), String.valueOf(lng));
                                dynamicUI.addJsToWebView(javascript);
                            }
                            if (animate && googleMap != null && lat!=null && lng != null){
                                LatLng latLng = new LatLng(lat, lng);
                                if (userPositionMarker == null) {
                                    upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng),160, 0.5f,0.9f); //TODO this function will be removed
                                } else {
                                    if (storeMapCallBack == null)
                                        userPositionMarker.setVisible(true);
                                    userPositionMarker.setPosition(latLng);
                                }
                                googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                            }
                        }
                    }
                })
                .addOnFailureListener(activity, new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                        Log.e(LOG_TAG, "Last and current position not known");
                    }
                });
    }

    @JavascriptInterface
    public void getLocationName(String latitude, String longitude, String defaultText, String callback) {
        if (!isLocationPermissionEnabled()) return;

        updateLastKnownLocation(null, false);

        if(defaultText.equals(CURRENT_LOCATION_LATLON)){
            latitude = String.valueOf(lastLatitudeValue);
            longitude = String.valueOf(lastLongitudeValue);
        }

        Geocoder geocoder = new Geocoder(activity, Locale.getDefault());
        StringBuilder returnedAddressStrBuilder;
        try {
            List<Address> addresses = geocoder.getFromLocation(Double.parseDouble(latitude), Double.parseDouble(longitude), 1);
            if (addresses != null && addresses.size() > 0) {
                returnedAddressStrBuilder = new StringBuilder();
                Address returnedAddress = addresses.get(0);
                for (int i = 0; i <= returnedAddress.getMaxAddressLineIndex(); i++) {
                    returnedAddressStrBuilder.append(returnedAddress.getAddressLine(i)).append(",");
                }
                Log.d(LOG_TAG, "getLocationName:" + returnedAddressStrBuilder);
            } else {
                returnedAddressStrBuilder = new StringBuilder(defaultText);
                Log.e(LOG_TAG, "Can't fetch current Address");
            }
            if (callback != null && dynamicUI != null && juspayServices.getDynamicUI() != null) {
                String returnedAddressStr = String.valueOf(returnedAddressStrBuilder);
                returnedAddressStr = returnedAddressStr.replaceAll("'", "");
                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                        callback, String.valueOf(latitude), String.valueOf(longitude), returnedAddressStr);
                Log.d(LOG_TAG, "getCurrent___Position___inside if" + latitude + longitude);
                dynamicUI.addJsToWebView(javascript);
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception occurred in getting Location Name " + e.getMessage());
            e.printStackTrace();
        }
    }


    @JavascriptInterface
    public void fetchAndUpdateCurrentLocation(String callback) {
        if(!isLocationPermissionEnabled()) return;
        updateLastKnownLocation(callback, true);
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
    public void initialWebViewSetUp(String callback, String id) {
        webViewCallBack = callback;
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                WebView webView= (WebView) activity.findViewById(Integer.parseInt(id));
                if(webView == null) return;
                webView.setWebViewClient(new WebViewClient(){
                    @Override
                    public boolean shouldOverrideUrlLoading(WebView view, String url) {

                        if (url.startsWith("intent://")) {
                            try {
                                Intent intent = Intent.parseUri(url, Intent.URI_INTENT_SCHEME);
                                if (intent != null) {
                                    PackageManager packageManager = context.getPackageManager();
                                    ResolveInfo info = packageManager.resolveActivity(intent, PackageManager.MATCH_DEFAULT_ONLY);
                                    if (info != null) {
                                        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                        context.startActivity(intent);
                                        return true;
                                    }
                                }
                            } catch (URISyntaxException e) {
                                e.printStackTrace();
                            }
                        }
                        return false;
                    }
                });
            }
        });
    }

    @JavascriptInterface
    public void goBackPrevWebPage(String id) {
        WebView webView = (WebView) activity.findViewById(Integer.parseInt(id));
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if(webView == null) return;
                if (webView.canGoBack()) {
                    webView.post(new Runnable() {
                        @Override
                        public void run() {
                            if (webView!=null){
                                webView.goBack();
                            }
                        }
                    });
                }else{
                    if (webViewCallBack != null && dynamicUI != null && juspayServices.getDynamicUI() != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                webViewCallBack, String.valueOf("TRUE"));
                        dynamicUI.addJsToWebView(javascript);
                    }
                }
            }
        });
    }

    @JavascriptInterface
    public String getCurrentLatLong() throws JSONException { // TODO:: TO BE DEPRECATED AS FUNCTION IS NOT IN USE
        JSONObject location = new JSONObject();
        location.put("lat",lastLatitudeValue);
        location.put("lng",lastLongitudeValue);
        return location.toString();
    }

    @JavascriptInterface
    public void initiateLocationServiceClient() {
        if(!isLocationPermissionEnabled()) return;
        resolvableLocationSettingsReq();
    }


    private void resolvableLocationSettingsReq(){
        LocationRequest locationRequest = createLocReq(Priority.PRIORITY_HIGH_ACCURACY, 1000, 500);

        LocationSettingsRequest.Builder lBuilder = new LocationSettingsRequest.Builder()
                .addLocationRequest(locationRequest)
                .setAlwaysShow(true);

        Task<LocationSettingsResponse> task =
                LocationServices.getSettingsClient(context).checkLocationSettings(lBuilder.build());

        task.addOnCompleteListener(new OnCompleteListener<LocationSettingsResponse>() {
            @Override
            public void onComplete(Task<LocationSettingsResponse> task) {
                try {
                    LocationSettingsResponse response = task.getResult(ApiException.class);
                } catch (ApiException exception) {
                    switch (exception.getStatusCode()) {
                        case LocationSettingsStatusCodes.RESOLUTION_REQUIRED:
                            try {
                                ResolvableApiException resolvable = (ResolvableApiException) exception;
                                resolvable.startResolutionForResult(activity,
                                        activity.getResources().getInteger(R.integer.LOCATION_RESOLUTION_REQUEST_CODE));
                            } catch (IntentSender.SendIntentException e) {
                                // Ignore the error.
                            } catch (ClassCastException e) {
                                // Ignore, should be an impossible error.
                            }
                            break;
                        case LocationSettingsStatusCodes.SETTINGS_CHANGE_UNAVAILABLE:
                            // Sadly this change is not available in this particular device :(
                            break;
                    }
                }
            }
        });
    }

    @JavascriptInterface
    public void getCurrentPosition(String callback) {
        if(!isLocationPermissionEnabled()) return;
        updateLastKnownLocation(callback, false);
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
    public String generateSessionToken(String str) {

        AutocompleteSessionToken token = AutocompleteSessionToken.newInstance();
        System.out.println("SESSION_TOKEN -> "+ token);
        String sessionToken  = token.toString();
        return sessionToken;
    }

    @JavascriptInterface
    public void renderBase64Image (String url, String id){
        String base64Image = getAPIResponse(url);
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if (!base64Image.equals("") && base64Image!=null && id!=null){
                        LinearLayout layout = activity.findViewById(Integer.parseInt(id));
                        byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                        Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);
                        ImageView imageView = new ImageView(context);
                        imageView.setImageBitmap(decodedByte);
                        imageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
                        imageView.setAdjustViewBounds(true);
                        imageView.setClipToOutline(true);
                        layout.removeAllViews();
                        layout.addView(imageView);
                    }
                } catch (Exception e){
                    e.printStackTrace();
                }
            }
        });
    }

    @JavascriptInterface
    public void startLottieProcess(String rawJson, String id, boolean repeat, float speed, String scaleType) {
        if (activity != null) activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    animationView = activity.findViewById(Integer.parseInt(id));
                    animationView.setAnimationFromJson(getJsonFromResources(rawJson));
                    animationView.loop(repeat);
                    animationView.setSpeed(speed);
                    animationView.playAnimation();
                    switch (scaleType) {
                        case "MATRIX" : animationView.setScaleType(ImageView.ScaleType.MATRIX); break;
                        case "FIT_XY" : animationView.setScaleType(ImageView.ScaleType.FIT_XY); break;
                        case "FIT_START" : animationView.setScaleType(ImageView.ScaleType.FIT_START); break;
                        case "FIT_END" : animationView.setScaleType(ImageView.ScaleType.FIT_END); break;
                        case "CENTER" : animationView.setScaleType(ImageView.ScaleType.CENTER); break;
                        case "CENTER_CROP" : animationView.setScaleType(ImageView.ScaleType.CENTER_CROP); break;
                        case "CENTER_INSIDE" : animationView.setScaleType(ImageView.ScaleType.CENTER_INSIDE); break;
                        default: animationView.setScaleType(ImageView.ScaleType.FIT_CENTER);break;
                    }
                } catch (Exception e) {
                    Log.d("TAG", "exception in startLottieAnimation" , e);
                }
            }
        });
    }

    /*
    * This function is deprecated on 12 Jan - 2023
    * Remove this function once it is not begin used.
    * */
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

    @JavascriptInterface
    public void storeCallBackLocateOnMap(String callback) {
        System.out.println("storeCallBackLocateOnMap" + callback);
        storeMapCallBack = callback;
    }
    @JavascriptInterface
    public void showMap(final String pureScriptId, boolean isEnableCurrentLocation, final String mapType, final float zoom, final String callback) {
        try {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                    FragmentManager supportFragmentManager = ((FragmentActivity) activity).getSupportFragmentManager();
                    FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                    fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                    fragmentTransaction.commitAllowingStateLoss();
                    if (mapFragment != null){
                        getMapAsync(mapFragment, isEnableCurrentLocation, mapType, callback, pureScriptId, zoom);
                    }
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
                            CommonJsInterface.this.googleMap.getUiSettings().setRotateGesturesEnabled(false);
                            googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                            markers = new JSONObject();
                            markersElement.put(pureScriptId, markers);
                            CommonJsInterface.this.googleMap.setOnMapLoadedCallback (new GoogleMap.OnMapLoadedCallback () {
                                @Override
                                public synchronized void onMapLoaded() {
                                    System.out.println("onMapLoaded");
                                    System.out.println(json);
                                    showRoute(json, routeType, "#323643", actualRoute, "ny_ic_dest_marker", "ny_ic_src_marker", 8);
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

                                                    if(dynamicUI != null && callback != null && juspayServices.getDynamicUI() != null){
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
                            polylineOptions.add(toPointObj);
                            polylineOptions.add(fromPointObj);
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

    private void getMapAsync(SupportMapFragment mapFragment, boolean isEnableCurrentLocation, final String mapType, final String callback, final String pureScriptId, final float zoom){
        mapFragment.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(GoogleMap googleMap) {
                CommonJsInterface.this.googleMap = googleMap;
                googleMap.setMinZoomPreference(7.0f);
                googleMap.setMaxZoomPreference(googleMap.getMaxZoomLevel());
                googleMap.getUiSettings().setRotateGesturesEnabled(false);
                googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
                    googleMap.setMyLocationEnabled(isEnableCurrentLocation);
                }
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
                        upsertMarker(LOCATE_ON_MAP, String.valueOf(lastLatitudeValue), String.valueOf(lastLongitudeValue), 160, 0.5f,0.9f);
                        CommonJsInterface.this.googleMap.setOnCameraMoveListener(new GoogleMap.OnCameraMoveListener() {
                            @Override
                            public void onCameraMove() {
                                try {
                                    double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                                    double lng =  (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                                    upsertMarker(LOCATE_ON_MAP, String.valueOf(lat), String.valueOf(lng),160, 0.5f,0.9f);
                                }catch (Exception e) {
                                    Log.i(LOG_TAG, "Marker creation error for ", e);
                                }
                            }
                        });
                        CommonJsInterface.this.googleMap.setOnCameraIdleListener(new GoogleMap.OnCameraIdleListener() {
                            @Override
                            public void onCameraIdle() {
                                if (dynamicUI != null && juspayServices.getDynamicUI() != null){
                                    double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                                    double lng =  (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "LatLon", lat, lng);
                                    Log.e(LOG_TAG, javascript);
                                    dynamicUI.addJsToWebView(javascript);
                                }
                            }
                        });
                    }
                    setMapCustomTheme("theme");
                    if (lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) {
                        LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        CommonJsInterface.this.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, zoom));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                if(dynamicUI != null && callback != null && juspayServices.getDynamicUI() != null){
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
            this.storeMapCallBack = null;
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
    public void enableMyLocation (boolean isEnableCurrentLocation ){
        try {
            activity.runOnUiThread((new Runnable() {
                @Override
                public void run() {
                    if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) == PackageManager.PERMISSION_GRANTED && CommonJsInterface.this.googleMap != null) {
                        CommonJsInterface.this.googleMap.setMyLocationEnabled(isEnableCurrentLocation);
                    }
                }
            }));
        } catch (Exception e){
            Log.i(LOG_TAG, "Enable My Location on GoogleMap error",e);
        }
    }

    @JavascriptInterface
    public void locateOnMap (boolean goToCurrentLocation, final String lat, final String lon){
        try {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    removeMarker("ny_ic_customer_current_location");
                    if(goToCurrentLocation){
                        LatLng latLng = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                    }else{
                        LatLng latLng = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                        googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                    }
                    googleMap.setOnCameraIdleListener(new GoogleMap.OnCameraIdleListener() {
                        @Override
                        public void onCameraIdle() {
                            double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                            double lng =  (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                            if (storeMapCallBack != null && dynamicUI!=null && juspayServices.getDynamicUI() != null){
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
                        googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
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
                                googleMap.getUiSettings().setRotateGesturesEnabled(false);
                                googleMap.getUiSettings().setMyLocationButtonEnabled(false);
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
            if(imageName.equals("ny_ic_dest_marker") ){
                pointer.setImageDrawable(context.getResources().getDrawable(R.drawable.ny_ic_dest_marker));
            }else{
                pointer.setImageDrawable(context.getResources().getDrawable(R.drawable.ny_ic_src_marker));
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
                                if(title.equals("ny_ic_customer_current_location")){
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
    public void addMediaPlayer (String viewID,String source) throws IOException {
        activity.runOnUiThread(new Runnable() {
           @Override
           public void run() {
               MediaPlayerView audioPlayer = new MediaPlayerView(context,activity);
                   try {
                       audioPlayer.inflateView(Integer.parseInt(viewID));
                       if (source.contains(".mp3")) {
                           audioPlayer.addAudioFileUrl(source);
                       } else {
                            Thread thread =  new Thread(new Runnable() {
                                @Override
                                public void run() {
                                    try {
                                       String base64 = getAPIResponse(source);
                                       byte decodedAudio[] = Base64.decode(base64,Base64.DEFAULT);
                                       File tempMp3 = File.createTempFile("audio_cache", "mp3", context.getCacheDir());
                                       tempMp3.deleteOnExit();
                                       FileOutputStream fos = new FileOutputStream(tempMp3);
                                       fos.write(decodedAudio);
                                       fos.close();
                                       FileInputStream fis = new FileInputStream(tempMp3);
                                       audioPlayer.addAudioFileInput(fis);
                                   } catch (Exception e) {

                                   }
                               }
                           });
                           thread.start();
                       }
                       audioPlayers.add(audioPlayer);
                   } catch (IOException e) {
                       e.printStackTrace();
                   }
               }
        });
    }

    @JavascriptInterface
    public void removeMediaPlayer () {
        for (MediaPlayerView audioPlayer : audioPlayers) {
            audioPlayer.resetListeners();
        }
        context.getCacheDir().delete();
        audioPlayers.removeAll(audioPlayers);
        DefaultMediaPlayerControl.mediaPlayer.reset();
    }

    @JavascriptInterface
    public void updateRoute (String json, String dest, String eta, String dummy) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if(googleMap!=null) {
                    try {
                        ArrayList<LatLng> path = new ArrayList<>();
                        JSONObject jsonObject = null;
                        jsonObject = new JSONObject(json);
                        JSONArray coordinates = jsonObject.getJSONArray("points");
                        for (int i = coordinates.length() -1 ; i >= 0   ; i--) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            LatLng tempPoint = new LatLng(lat, lng);
                            path.add(tempPoint);
                        }
                        Marker currMarker = (Marker) markers.get("ny_ic_auto_map");
                        Marker destMarker = (Marker) markers.get(dest);
                        destMarker.setIcon((BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(eta, dest))));
                        if (polylines != null) {
                            polylines.setEndCap(new ButtCap());
                            if (path.size() == 0) {
                                LatLng destination = destMarker.getPosition();
                                animateMarkerNew(destination,currMarker);
                                polylines.remove();
                                polylines = null;
                                currMarker.setAnchor(0.5f,0);
                                animateCamera(destMarker.getPosition().latitude,destMarker.getPosition().longitude,20.0f);
                            }
                            else
                            {
                                double destinationLat = path.get(0).latitude;
                                double destinationLon = path.get(0).longitude;
                                double sourceLat = path.get(path.size()-1).latitude;
                                double sourceLong = path.get(path.size()-1).longitude;
                                LatLng destination = path.get(path.size()-1);
                                animateMarkerNew(destination,currMarker);
                                PatternItem DASH = new Dash(1);
                                List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Arrays.asList(DASH);
                                polylines.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                                polylines.setPoints(path);
                                moveCamera(sourceLat, sourceLong, destinationLat, destinationLon, coordinates);
                            }
                        }
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
    }

    /*
     * This function is deprecated on 12 Jan - 2023
     * Remove this function once it is not begin used.
     */

    @JavascriptInterface
    public void updateRoute (String json,  double currLat, double currLng) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if(googleMap!=null) {
                    try {
                        ArrayList<LatLng> points = getUpdatedPolyPoints(json,currLat,currLng);
                        Marker currMarker = (Marker) markers.get("ny_ic_auto_map");
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

    /*
     * This function is deprecated on 12 Jan - 2023
     * Remove this function once it is not begin used.
     */

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
    public void shareTextMessage(String title, String message) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                Intent sendIntent = new Intent();
                sendIntent.setAction(Intent.ACTION_SEND);
                sendIntent.putExtra(Intent.EXTRA_TEXT, message);
                sendIntent.putExtra(Intent.EXTRA_TITLE, title);
                sendIntent.setType("text/plain");
                Intent shareIntent = Intent.createChooser(sendIntent, null);
                activity.startActivity(shareIntent);
            }
        });
    }

    @JavascriptInterface
    public void shareImageMessage(String message, String imageName) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                Intent sendIntent = new Intent();
                int image = context.getResources().getIdentifier(imageName, "drawable", context.getPackageName());
                BitmapDrawable bitmapDrawable = (BitmapDrawable) context.getResources().getDrawable(image);
                Bitmap bitmap = bitmapDrawable.getBitmap();
                Uri uri = Uri.parse(MediaStore.Images.Media.insertImage(context.getContentResolver() , bitmap , "qrCode",null));
                sendIntent.setAction(Intent.ACTION_SEND);
                sendIntent.putExtra(Intent.EXTRA_STREAM,uri);
                sendIntent.putExtra(Intent.EXTRA_TEXT, message);
                sendIntent.setType("image/*");
                Intent shareIntent = Intent.createChooser(sendIntent, null);
                activity.startActivity(shareIntent);
            }
        });
    }

    @JavascriptInterface
    public String isCoordOnPath(String json , double currLat, double currLng, int speed) throws JSONException {
        LatLng currPoint = new LatLng(currLat,currLng);
        ArrayList<LatLng> path = new ArrayList<>();
        JSONObject jsonObject = null;
        jsonObject = new JSONObject(json);
        JSONArray coordinates = jsonObject.getJSONArray("points");
        int eta = 0;
        int resultIndex;
        JSONObject result = new JSONObject();
        for (int i = coordinates.length() -1 ; i >= 0   ; i--) {
            JSONObject coordinate = (JSONObject) coordinates.get(i);
            double lng = coordinate.getDouble("lng");
            double lat = coordinate.getDouble("lat");
            LatLng tempPoints = new LatLng(lat,lng);
            path.add(tempPoints);
        }
        if (path.size() == 0)
        {
            result.put("points",new JSONArray());
            result.put("eta",0);
            result.put("distance",0);
            result.put("isInPath",false);
            return result.toString();
        }
        if (distanceRemaining > 300 || distanceRemaining == -1) {
            resultIndex = PolyUtil.locationIndexOnEdgeOrPath(currPoint, path, PolyUtil.isClosedPolygon(path), true, 30.0);
        } else {
            resultIndex = PolyUtil.locationIndexOnEdgeOrPath(currPoint, path, PolyUtil.isClosedPolygon(path), true, 50.0);
        }
        if (resultIndex == -1) {
            result.put("points",coordinates);
            result.put("eta",0);
            result.put("distance",0);
            result.put("isInPath",false);
        } else if (resultIndex == 0) {
            path.clear();
            result.put("points",new JSONArray());
            result.put("eta",0);
            result.put("distance",0);
            result.put("isInPath",true);
        } else if (resultIndex == (path.size()-2) || resultIndex == (path.size()-1)) {
            distanceRemaining = (int) SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            result.put("points",coordinates);
            result.put("eta",eta);
            result.put("distance",distanceRemaining);
            result.put("isInPath",true);
        } else {
            path.subList(resultIndex+2,path.size()).clear();
            distanceRemaining = (int)SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            JSONArray remainingPoints = new JSONArray();
            for (int i = path.size() - 1 ; i >= 0   ; i--) {
                LatLng point = path.get(i);
                JSONObject tempPoints = new JSONObject();
                tempPoints.put("lat",point.latitude);
                tempPoints.put("lng",point.longitude);
                remainingPoints.put(tempPoints);
            }
            result.put("points",remainingPoints);
            result.put("eta",eta);
            result.put("distance",distanceRemaining);
            result.put("isInPath",true);
        }
        return result.toString();
    }

    /*
     * This function is deprecated on 12 Jan - 2023
     * Remove this function once it is not begin used.
     */

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
            valueAnimator.setDuration(2000); // duration 3 second
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
                        markers.put("ny_ic_auto_map",marker);
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
    public void launchInAppRatingPopup(){
        ReviewManager manager = ReviewManagerFactory.create(context);
        Task<ReviewInfo> request = manager.requestReviewFlow();
        request.addOnCompleteListener(task -> {
            if (task.isSuccessful()) {
                // We can get the ReviewInfo object
                ReviewInfo reviewInfo = task.getResult();
                Task<Void> flow = manager.launchReviewFlow(activity, reviewInfo);
                flow.addOnCompleteListener(task1 -> {
                    // The flow has finished. The API does not indicate whether the user
                    // reviewed or not, or even whether the review dialog was shown.
                });
            } else {
                // There was some problem, log or handle the error code.
            }
        });
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
                            upsertMarker("ny_ic_auto_map",String.valueOf(lat), String.valueOf(lng), 90, 0.5f, 0.5f);
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
                            polylineOptions.add(toPointObj);
                            polylineOptions.add(fromPointObj);
                        }

                        polylines = setRouteCustomTheme(polylineOptions, color, style, polylineWidth);
                        LatLng sourceLatLng = new LatLng(sourceLat, sourceLong);
                        LatLng destLatLng = new LatLng(destLat, destLong);

                        if(destMarker != null && !destMarker.equals("")) {
                            List<LatLng> points = polylineOptions.getPoints();
                            LatLng dest = points.get(0);
                            MarkerOptions markerObj = new MarkerOptions()
                                    .title(destMarker)
                                    .position(dest)
                                    .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(destinationName, destMarker)));

                            Marker tempmarker = googleMap.addMarker(markerObj);
                            markers.put(destMarker, tempmarker);

                        }
                        if (type.equals("DRIVER_LOCATION_UPDATE"))
                        {
                            System.out.println("inside insert marker");
                            List<LatLng> points = polylineOptions.getPoints();
                            LatLng source = points.get(points.size() - 1);
                            upsertMarker("ny_ic_auto_map",String.valueOf(source.latitude),String.valueOf(source.longitude), 90, 0.5f, 0.5f);
                            Marker currMarker = (Marker) markers.get("ny_ic_auto_map");
                            int index = polylines.getPoints().size()-1;
                            float rotation = bearingBetweenLocations(polylines.getPoints().get(index), polylines.getPoints().get(index -1));
                            if (rotation > 1.0) currMarker.setRotation(rotation);
                            currMarker.setAnchor(0.5f,0.5f);
                            markers.put("ny_ic_auto_map",currMarker);
                        } else if(sourceMarker != null && !sourceMarker.equals("")) {
                            System.out.println("sourcelatlong: " + sourceLatLng);
                            System.out.println("destlatlong: " + destLatLng);
                            List<LatLng> points = polylineOptions.getPoints();
                            LatLng source = points.get(points.size() - 1);
                            MarkerOptions markerObj = new MarkerOptions()
                                    .title(sourceMarker)
                                    .position(source)
                                    .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(sourceName,sourceMarker)));
                            Marker tempmarker = googleMap.addMarker(markerObj);
                            markers.put(sourceMarker, tempmarker);
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
        removeMarker("ny_ic_auto_map");
        removeMarker("ny_ic_src_marker");
        removeMarker("ny_ic_dest_marker");
        activity.runOnUiThread(
                new Runnable() {
                    @Override
                    public void run() {
                        if (polylines != null) {
                            polylines.remove();
                            polylines = null;
                        }
                    }
                }
        );
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
    public void animateCamera(final double lat, final double lng, final float zoom) {
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
                        googleMap.setPadding(100, 200, 100, getScreenHeight()/2);
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
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 200));
                    } catch (IllegalArgumentException e) {
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 200));
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
                if(!isLocationPermissionEnabled()) return;
                updateLastKnownLocation(null, true);
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
    public void launchAppSettings(){
        Intent appSettingsIntent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
        appSettingsIntent.setData(Uri.fromParts("package", getPackageName(), null));
        appSettingsIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(appSettingsIntent);
    }

    @JavascriptInterface
    public void adjustViewWithKeyboard (String flag) {
        activity.runOnUiThread(() -> {
                if (flag.equals("true"))
                    activity.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_RESIZE);
                else
                    activity.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_NOTHING);
        });
    }

    @JavascriptInterface
    public void  generatePDF (String str, String format) throws JSONException {
        invoice = str;
        invoiceType = format;
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
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String userName =  sharedPref.getString("USER_NAME","__failed");
        JSONObject selectedItem = data.getJSONObject("selectedItem");
        JSONObject fares = selectedItem.optJSONObject("fareBreakUpList");
        if (fares == null) {
            fares = getFaresJson(selectedItem);
        }
        if (invoiceType.equals("OLD")){
            int pageHeight = 1555;
            int pagewidth = 960;
            Bitmap logo = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_launcher);
            Bitmap src_icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_green_circle);
            Bitmap dest_icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_red_circle);
            Bitmap ic_line = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_vertical_line);
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
            try {
                pdfDocument.writeTo(new FileOutputStream(file));
                Uri path = FileProvider.getUriForFile(context.getApplicationContext(), context.getResources().getString(R.string.fileProviderPath), file);
                Intent pdfOpenintent = new Intent(Intent.ACTION_VIEW);
                pdfOpenintent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_ACTIVITY_CLEAR_TOP);
                pdfOpenintent.setDataAndType(path, "application/pdf");
                PendingIntent pendingIntent = PendingIntent.getActivity(context, 234567, pdfOpenintent, PendingIntent.FLAG_IMMUTABLE);
                NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, "General");
                mBuilder.setLargeIcon(logo);
                mBuilder.setContentTitle("Invoice Downloaded")
                        .setSmallIcon((R.drawable.ny_ic_launcher))
                        .setContentText("Invoice for your ride is downloaded!!!")
                        .setAutoCancel(true)
                        .setPriority(NotificationCompat.PRIORITY_DEFAULT);
                mBuilder.setContentIntent(pendingIntent);
                NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
                notificationManager.notify(234567, mBuilder.build());
            } catch (IOException e) {
                e.printStackTrace();
            }
            pdfDocument.close();
        }
        else
        {
            int pageHeight = 1388;
            int pagewidth = 960;
            Bitmap logo = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_invoice_logo);
            Bitmap src_icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_green_circle);
            Bitmap dest_icon = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_red_circle);
            Bitmap ic_line = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_vertical_line);
            Bitmap scaledBmp = null;
            PdfDocument pdfDocument = new PdfDocument();
            PdfDocument.PageInfo invoicePDF = new PdfDocument.PageInfo.Builder(960, 1338, 1).create();
            PdfDocument.Page page = pdfDocument.startPage(invoicePDF);
            View content = getInvoiceLayout(data,selectedItem,fares,userName,context);
            content.measure(page.getCanvas().getWidth(),page.getCanvas().getHeight());
            content.layout(0,0,page.getCanvas().getWidth(),page.getCanvas().getHeight());
            content.draw(page.getCanvas());
            pdfDocument.finishPage(page);
            String fileNameformat = "NY_Ride_" + selectedItem.getString("date") + selectedItem.getString("rideStartTime") + ".pdf";
            String fileName = fileNameformat.replaceAll(":",".");
            File file = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS), fileName);
            try {
                pdfDocument.writeTo(new FileOutputStream(file));
                Uri path = FileProvider.getUriForFile(context.getApplicationContext(), context.getResources().getString(R.string.fileProviderPath), file);
                Intent pdfOpenintent = new Intent(Intent.ACTION_VIEW);
                pdfOpenintent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_ACTIVITY_CLEAR_TOP);
                pdfOpenintent.setDataAndType(path, "application/pdf");
                Bitmap notification_logo = BitmapFactory.decodeResource(context.getResources(), R.drawable.ny_ic_launcher);
                PendingIntent pendingIntent = PendingIntent.getActivity(context, 234567, pdfOpenintent, PendingIntent.FLAG_IMMUTABLE);
                NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, "General");
                mBuilder.setContentTitle("Invoice Downloaded")
                        .setSmallIcon((R.drawable.ny_ic_launcher))
                        .setContentText("Invoice for your ride is downloaded!!!")
                        .setAutoCancel(true)
                        .setPriority(NotificationCompat.PRIORITY_DEFAULT);
                mBuilder.setContentIntent(pendingIntent);
                NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
                notificationManager.notify(234567, mBuilder.build());
            } catch (IOException e) {
                e.printStackTrace();
            }
                pdfDocument.close();
        }

    }

    private static View getInvoiceLayout(JSONObject data, JSONObject selectedRide, JSONObject fares, String user, Context context) throws JSONException {
        View invoiceLayout = LayoutInflater.from(context).inflate(R.layout.invoice_template, null, false);
        TextView textView = invoiceLayout.findViewById(R.id.rideDate);
        textView.setText(selectedRide.getString("date"));
        textView = invoiceLayout.findViewById(R.id.userName);
        textView.setText(user.trim());
        textView = invoiceLayout.findViewById(R.id.paymentDetail);
        textView.setText(selectedRide.getString("totalAmount"));
        textView = invoiceLayout.findViewById(R.id.baseDistance);
        try {
            textView.setText(selectedRide.getString("baseDistance"));
        } catch (JSONException e) {
            textView.setText("");
        }
        textView = invoiceLayout.findViewById(R.id.baseFare);
        textView.setText(fares.getString("baseFare"));
        textView = invoiceLayout.findViewById(R.id.pickUpCharge);
        textView.setText(fares.getString("pickupCharges"));
        textView = invoiceLayout.findViewById(R.id.nominalFare);
        textView.setText(fares.getString("nominalFare"));
        textView = invoiceLayout.findViewById(R.id.waitingCharge);
        textView.setText(fares.getString("waitingCharges"));
        textView = invoiceLayout.findViewById(R.id.finalAmount);
        textView.setText(selectedRide.getString("totalAmount"));
        textView = invoiceLayout.findViewById(R.id.rideId);
        textView.setText(selectedRide.getString("shortRideId"));
        textView = invoiceLayout.findViewById(R.id.driverName);
        textView.setText(selectedRide.getString("driverName"));
        textView = invoiceLayout.findViewById(R.id.lincensePlate);
        textView.setText(selectedRide.getString("vehicleNumber"));
        textView = invoiceLayout.findViewById(R.id.rideStartTime);
        textView.setText(selectedRide.getString("rideStartTime"));
        textView = invoiceLayout.findViewById(R.id.source);
        textView.setText(selectedRide.getString("source"));
        textView = invoiceLayout.findViewById(R.id.rideEndTime);
        textView.setText(selectedRide.getString("rideEndTime"));
        textView = invoiceLayout.findViewById(R.id.destination);
        textView.setText(selectedRide.getString("destination"));
        return invoiceLayout;
    }

    // Added for Backward Compatibility
    private static JSONObject getFaresJson (JSONObject rideData) throws JSONException {
        JSONObject fares = new JSONObject();
        float  baseFare = Float.parseFloat(rideData.getString("totalAmount").substring(2)) - 10;
        fares.put("baseFare","₹ " + baseFare);
        fares.put("pickupCharges","₹ 10");
        fares.put("nominalFare","₹ 0");
        fares.put("waitingCharges","₹ 0");
        return fares;
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


    @JavascriptInterface
    public void setYoutubePlayer(String rawJson, final String playerId, String videoStatus){
        videoDuration = 0;
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if(videoStatus.equals("PAUSE"))   {
                        pauseYoutubeVideo();
                    }   else {
                        JSONObject json = new JSONObject(rawJson);
                        if (youTubePlayerView != null )
                            youTubePlayerView.release();
                            boolean showMenuButton = json.getBoolean("showMenuButton");
                            boolean showDuration = json.getBoolean("showDuration");
                            boolean setVideoTitle = json.getBoolean("setVideoTitle");
                            boolean showSeekBar = json.getBoolean("showSeekBar");
                            String videoTitle = json.getString("videoTitle");
                            String videoId = json.getString("videoId");
                            String videoType = "VIDEO";
                            if (json.has("videoType"))
                                {
                                    videoType = json.getString("videoType");
                                }
                            youTubePlayerView = new YouTubePlayerView(context);
                            LinearLayout layout = activity.findViewById(Integer.parseInt(playerId));
                            layout.addView(youTubePlayerView);
                            youTubePlayerView.setEnableAutomaticInitialization(false);
                            YouTubePlayerListener youTubePlayerListener = new AbstractYouTubePlayerListener() {
                                @Override
                                public void onReady(YouTubePlayer youTubePlayer) {
                                    try {
                                        youtubePlayer = youTubePlayer;
                                        DefaultPlayerUiController playerUiController = new DefaultPlayerUiController(youTubePlayerView, youTubePlayer);
                                        playerUiController.showMenuButton(showMenuButton);
                                        playerUiController.showDuration(showDuration);
                                        playerUiController.showSeekBar(showSeekBar);
                                        playerUiController.showFullscreenButton(true);
                                        if (setVideoTitle){
                                            playerUiController.setVideoTitle(videoTitle);
                                        }
                                        playerUiController.showYouTubeButton(false);
                                        youTubePlayerView.setCustomPlayerUi(playerUiController.getRootView());

                                        youTubePlayer.seekTo(videoDuration);
                                        youTubePlayer.loadVideo(videoId, 0);
                                        youTubePlayer.play();

                                    } catch (Exception e) {
                                        Log.e("error inside setYoutubePlayer onReady", String.valueOf(e));
                                    }

                                }
                                @Override
                                public void onCurrentSecond(@NonNull YouTubePlayer youTubePlayer, float second){
                                    videoDuration = second;
                                }
                            };

                        String finalVideoType = videoType;
                        youTubePlayerView.addFullScreenListener(new  YouTubePlayerFullScreenListener() {
                                @Override
                                public void onYouTubePlayerExitFullScreen() {
                                }

                                @Override
                                public void onYouTubePlayerEnterFullScreen() {
                                    Intent newIntent = new Intent(MainActivity.getInstance().getApplicationContext(), YoutubeVideoView.class );
                                    newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                    newIntent.putExtra("videoId", videoId);
                                    newIntent.putExtra("videoDuration", videoDuration);
                                    newIntent.putExtra("videoType", finalVideoType);
                                    MainActivity.getInstance().getApplicationContext().startActivity(newIntent);
                                }
                            });

                            IFramePlayerOptions options = new IFramePlayerOptions.Builder().controls(0).rel(0).build();
                            youTubePlayerView.initialize(youTubePlayerListener, options);
                        }
                    } catch (Exception e) {
                        Log.e("exception in setYoutubePlayer", String.valueOf(e));
                        }
                }
        });
    }

    @JavascriptInterface
    public void pauseYoutubeVideo(){
        if( youTubePlayerView != null) {
            youtubePlayer.pause();
        }
    }
    private String getAPIResponse(String url) {
        if (url.equals("") || url == null) return "";
        StringBuilder result = new StringBuilder();
        try {
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            connection.setRequestMethod("GET");
            connection.setRequestProperty("token",  getKeyInNativeSharedPrefKeys("REGISTERATION_TOKEN"));
            connection.connect();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302){
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                return "";
            }else {
                respReader = new InputStreamReader(connection.getInputStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                return result.toString();
            }
        } catch (Exception e) {
            e.printStackTrace();
            return "";
        }
    }

    @JavascriptInterface
    public void contactPermission() {
        try {
            if (ContextCompat.checkSelfPermission(MainActivity.getInstance(), Manifest.permission.READ_CONTACTS) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions(MainActivity.getInstance(), new String[]{Manifest.permission.READ_CONTACTS}, REQUEST_CONTACTS);
            } else {
                String contacts = MainActivity.getInstance().getPhoneContacts();
                if (MainActivity.getInstance().getJuspayServices().getDynamicUI() != null) {
                    contactsStoreCall(MainActivity.getInstance().getJuspayServices().getDuiCallback(), contacts);
                }
            }
        } catch (Exception e) {
            Log.e("error inside contactPermission", String.valueOf(e));
        }
    }


    static String storeCallBContact = null;
    @JavascriptInterface
    public void storeCallBackContacts(String callback) {
        storeCallBContact = callback;
    }

    public static void contactsStoreCall(DuiCallback dynamicUII, String contacts){
        if (dynamicUII != null && storeCallBContact != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
            storeCallBContact,contacts);
            dynamicUII.addJsToWebView(javascript);
        }
    }

}