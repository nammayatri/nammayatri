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

import android.app.AlarmManager;
import android.app.ActivityManager;
import android.app.AlertDialog;
import android.app.DatePickerDialog;
import android.app.PendingIntent;
import android.app.TimePickerDialog;
import android.content.ActivityNotFoundException;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Interpolator;
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
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.provider.MediaStore;
import android.provider.Settings;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.ActionMode;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.animation.LinearInterpolator;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.webkit.ConsoleMessage;
import android.webkit.JavascriptInterface;
import android.webkit.WebChromeClient;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.HorizontalScrollView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.NumberPicker;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.TimePicker;
import android.widget.Toast;
import android.os.Looper;
import android.view.View.OnClickListener;
import android.content.ContentValues;
import android.graphics.BitmapShader;
import android.graphics.Shader;
import android.graphics.Matrix;
import android.widget.Button;


import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.RequiresApi;
import androidx.browser.customtabs.CustomTabsIntent;
import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.core.location.LocationManagerCompat;
import androidx.camera.core.CameraSelector;
import androidx.camera.core.ImageAnalysis;
import androidx.camera.core.ImageCapture;
import androidx.camera.core.ImageCaptureException;
import androidx.camera.core.ImageProxy;
import androidx.camera.core.Preview;
import androidx.camera.lifecycle.ProcessCameraProvider;
import androidx.camera.view.PreviewView;

import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;
import androidx.viewpager2.widget.ViewPager2;
import androidx.work.Constraints;
import androidx.work.ExistingPeriodicWorkPolicy;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;
import androidx.lifecycle.LifecycleOwner;

import com.airbnb.lottie.LottieAnimationView;
import com.facebook.appevents.AppEventsConstants;
import com.google.android.gms.ads.identifier.AdvertisingIdClient;
import com.google.android.gms.auth.api.credentials.Credentials;
import com.google.android.gms.auth.api.credentials.CredentialsOptions;
import com.google.android.gms.auth.api.credentials.HintRequest;
import com.google.android.gms.common.api.ApiException;
import com.google.android.gms.common.api.ResolvableApiException;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.LocationSettingsRequest;
import com.google.android.gms.location.LocationSettingsResponse;
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
import com.clevertap.android.sdk.CleverTapAPI;
//import com.google.android.material.snackbar.Snackbar;
//import com.google.firebase.installations.FirebaseInstallations;
import com.google.android.libraries.places.api.model.AutocompleteSessionToken;
import com.google.android.play.core.review.ReviewInfo;
import com.google.android.play.core.review.ReviewManager;
import com.google.android.play.core.review.ReviewManagerFactory;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;
import com.google.maps.android.data.geojson.GeoJsonLayer;
import com.google.maps.android.data.geojson.GeoJsonPolygonStyle;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerFullScreenListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.options.IFramePlayerOptions;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.ui.DefaultPlayerUiController;
import com.google.common.util.concurrent.ListenableFuture;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
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
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import in.juspay.mobility.utils.AudioRecorder;
import in.juspay.mobility.utils.ChatBroadCastReceiver;
import in.juspay.mobility.utils.ChatService;
import in.juspay.mobility.utils.LocationUpdateWorker;
import in.juspay.mobility.utils.CheckPermissionAutoStart;
import in.juspay.mobility.utils.CheckPermissionOverlay;
import in.juspay.mobility.utils.LocationUpdateService;
import in.juspay.mobility.utils.MediaPlayerView;
import in.juspay.mobility.utils.MessageOverlayService;
import in.juspay.mobility.utils.NotificationUtils;
import in.juspay.mobility.utils.OtpUtils;
import in.juspay.mobility.utils.carousel.VPAdapter;
import in.juspay.mobility.utils.carousel.ViewPagerItem;
import in.juspay.mobility.utils.mediaPlayer.DefaultMediaPlayerControl;
import in.juspay.hypersdk.core.HyperFragment;
import in.juspay.hypersdk.core.JBridge;
import in.juspay.hypersdk.core.JuspayDuiHook;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.utils.network.JuspayHttpResponse;
import in.juspay.hypersdk.utils.network.NetUtils;
import in.juspay.hypersdk.core.DuiCallback;
import in.juspay.hypersdk.core.JSI;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static androidx.core.app.ActivityCompat.startActivityForResult;
import static androidx.core.app.ActivityCompat.startIntentSenderForResult;
import static androidx.core.content.ContextCompat.getCodeCacheDir;
import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static android.view.View.LAYER_TYPE_SOFTWARE;

import java.net.URISyntaxException;

import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.content.pm.ResolveInfo;

import com.facebook.appevents.AppEventsLogger;

public class CommonJsInterface extends JBridge implements in.juspay.hypersdk.core.JSI {

    private static SharedPreferences sharedPrefs;
    private static final String LOG_TAG = "Beckn_JsInterface";
    private static final String LOCATE_ON_MAP = "LocateOnMap";
    private static final String DISCOUNT_END = "Discount End Date";
    private static final String DISCOUNT_START = "Discount Start Date";
    private static final String CURRENT_LOCATION = "ny_ic_customer_current_location";
    private static final String CURRENT_LOCATION_LATLON = "Current Location";
    public static final int LOCATION_PERMISSION_REQ_CODE = 1;
    public static final int CREDENTIAL_PICKER_REQUEST = 74;
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
    private HashMap<String, JSONObject> markersElement = new HashMap<String, JSONObject>();// = new JSONObject();
    private JSONObject markers = new JSONObject();
    private double lastLatitudeValue;
    private double lastLongitudeValue;
    private int lastFocusedEditView;
    private Polyline polylines = null; //TODO Implement polylies using key-value pair
    private FirebaseAnalytics mFirebaseAnalytics;
    private static Calendar current = Calendar.getInstance();
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    private static final int IMAGE_PERMISSION_REQ_CODE_PROFILE = 1243;
    public static final int REQUEST_CALL = 8;
    public static final int REQUEST_CONTACTS = 7;
    public static String phoneNumber;
    public static String invoice = null;
    public static String invoiceType = null;
    public static boolean permissionCheck = false;
    public static String storeCallBackPopUp = null;
    public static String storeOnResumeCallback = null;
    private LottieAnimationView animationView;
    public static YouTubePlayerView youTubePlayerView;
    public static YouTubePlayer youtubePlayer;
    private static final int DATEPICKER_SPINNER_COUNT = 3;
    private static String storeMapCallBack = null;
    CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
    public static ArrayList<MediaPlayerView> audioPlayers = new ArrayList<>();
    public static String webViewCallBack = null;
    private AudioRecorder audioRecorder = null;
    public static int debounceAnimateCamera = 0;
    public static boolean isUploadPopupOpen = false;
    Toast toast = null;

    public static ArrayList<Marker> pickupPointsZoneMarkers = new ArrayList<Marker>();
    public static GeoJsonLayer layer;
    public static List<LatLng> zoneLatLngPoints = new ArrayList<LatLng>();
    private String regToken, baseUrl;
    private SharedPreferences sharedPref;
    private String zoneName = "";
    private float zoom = 17.0f;
    public static String detectPhoneNumbersCallBack = null;
    private PreviewView previewView;
    private ImageCapture imageCapture;
    private Button bCapture;
    public static Runnable cameraPermissionCallback;
    public static Boolean considerCameraOption = true;



    public CommonJsInterface() {
        super();
    }

    public CommonJsInterface(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super(juspayServices, activity, fragment);
        this.context = activity;
        this.context=context;
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
        sharedPref =  context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
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
        sharedPref =  context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        fetchLatLonAndUpdate();
    }

    @JavascriptInterface
    public void toast(String msg) {
        Toast.makeText(context, msg, Toast.LENGTH_LONG).show();
    }

    private void fetchLatLonAndUpdate() {
        String lat = getKeyInNativeSharedPrefKeys("LAST_KNOWN_LAT");
        String lon = getKeyInNativeSharedPrefKeys("LAST_KNOWN_LON");
        lastLatitudeValue = lat != "__failed" ? Double.parseDouble(lat) : lastLatitudeValue;
        lastLongitudeValue = lon != "__failed" ? Double.parseDouble(lon) : lastLongitudeValue;
    }

    @JavascriptInterface
    public void setKeysInSharedPrefs(String key, String value) {
        KeyValueStore.write(juspayServices, key, value);
        setEnvInNativeSharedPrefKeys(key, value);
        if (key.equals(context.getResources().getString(R.string.LANGUAGE_KEY))) {
            updateLocaleResource(value);
        }
    }

    public static void updateLocaleResource(String languageKey) {
        Context context = MainActivity.getInstance().getApplicationContext();
        Locale locale;
        switch (languageKey) {
            case "HI_IN":
                locale = new Locale("hi");
                break;
            case "KN_IN":
                locale = new Locale("kn");
                break;
            case "EN_US":
                locale = new Locale("en");
                break;
            case "TA_IN":
                locale = new Locale("ta");
                break;
            case "BN_IN" :
                locale = new Locale("bn");
                break;
            case "ML_IN" :
                locale = new Locale("ml");
                break;
            default:
                return;
        }
        Locale.setDefault(locale);
        Configuration configuration = context.getResources().getConfiguration();
        configuration.setLocale(locale);
        context.getResources().updateConfiguration(configuration, context.getResources().getDisplayMetrics());
    }

    @JavascriptInterface
    public void setEnvInNativeSharedPrefKeys(String key, String value) {
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
    public void addCarousel(String stringifyArray,String id){
        LinearLayout parentLayout = activity.findViewById(Integer.parseInt(id));
        if (activity == null || parentLayout == null ) return;
        activity.runOnUiThread(() -> {
            ViewPager2 viewPager2 = new ViewPager2(context);
            LinearLayout sliderDotsPanel= new LinearLayout(context);
            LinearLayout.LayoutParams sliderDotsPanelParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
            ViewGroup.LayoutParams scrollViewParams = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
            LinearLayout.LayoutParams linearLayoutParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT);
            LinearLayout linearLayout = new LinearLayout(context);
            linearLayout.setOrientation(LinearLayout.VERTICAL);
            linearLayout.setLayoutParams(linearLayoutParams);
            ScrollView scrollView = new ScrollView(context);
            scrollView.setLayoutParams(scrollViewParams);

            //adding data in array list
            ArrayList<ViewPagerItem> viewPagerItemArrayList = new ArrayList<>();
            try {
                JSONArray jsonArray = new JSONArray(stringifyArray);
                for (int i =0 ; i<jsonArray.length(); i++){
                    JSONObject jsonObject = jsonArray.getJSONObject(i);
                    int imageID = context.getResources().getIdentifier(jsonObject.getString("image"), "drawable", activity.getPackageName());
                    ViewPagerItem viewPagerItem = new ViewPagerItem(imageID, jsonObject.getString("title"), jsonObject.getString("description"));
                    viewPagerItemArrayList.add(viewPagerItem);
                }
            }catch (Exception e){
                Log.e(LOG_TAG, "Exception"+ e);
                return;
            }
            VPAdapter vpAdapter = new VPAdapter(viewPagerItemArrayList);
            viewPager2.setAdapter(vpAdapter);

            // setting the dots layout
            int dotsCount;
            ImageView[] dots;
            dotsCount = vpAdapter.getItemCount();
            dots = new ImageView[dotsCount];
            for(int i = 0; i < dotsCount; i++){
                dots[i] = new ImageView(context);
                dots[i].setImageDrawable(ContextCompat.getDrawable(context, R.drawable.carousel_dot_inactive));
                LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(28, 28);
                params.setMargins(14, 0, 14, 0);
                sliderDotsPanel.addView(dots[i], params);
                dots[0].setImageDrawable(ContextCompat.getDrawable(context, R.drawable.carousel_dot_active));
                int finalI = i;
                dots[i].setOnClickListener(new View.OnClickListener() {
                    @Override
                    public void onClick(View view) {
                        viewPager2.setCurrentItem(finalI);
                    }
                });
            }
            sliderDotsPanel.setLayoutParams(sliderDotsPanelParams);

            viewPager2.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
                @Override
                public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                    super.onPageScrolled(position, positionOffset, positionOffsetPixels);
                }
                @Override
                public void onPageSelected(int position) {
                    //setting active inactive dots
                    for(int i = 0; i< dotsCount; i++){
                        dots[i].setImageDrawable(ContextCompat.getDrawable(context, R.drawable.carousel_dot_inactive));
                    }
                    dots[position].setImageDrawable(ContextCompat.getDrawable(context, R.drawable.carousel_dot_active));
                    super.onPageSelected(position);
                }
                @Override
                public void onPageScrollStateChanged(int state) {
                    super.onPageScrollStateChanged(state);
                }
            });
            linearLayout.addView(viewPager2);
            linearLayout.setGravity(Gravity.BOTTOM);
            linearLayout.addView(sliderDotsPanel);
            linearLayout.setWeightSum(2);
            scrollView.addView(linearLayout);
            scrollView.setFillViewport(true);
            sliderDotsPanelParams.weight = 1;
            sliderDotsPanel.setGravity(Gravity.BOTTOM | Gravity.CENTER_HORIZONTAL);
            parentLayout.addView(scrollView);
        });
    }

    @JavascriptInterface
    public void detectPhoneNumbers(final String callback){
        detectPhoneNumbersCallBack = callback;
        HintRequest hintRequest = new HintRequest.Builder()
                .setPhoneNumberIdentifierSupported(true)
                .build();
        PendingIntent intent = Credentials.getClient(context).getHintPickerIntent(hintRequest);
        try {
            startIntentSenderForResult(activity,intent.getIntentSender(), CREDENTIAL_PICKER_REQUEST, null, 0, 0, 0,new Bundle());
        } catch (IntentSender.SendIntentException e) {
            e.printStackTrace();
        }
    }

    @JavascriptInterface
    public String getKeyInNativeSharedPrefKeys(String key) {
        SharedPreferences sharedPref = context.getSharedPreferences(
                context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        return sharedPref.getString(key, "__failed");
    }

    @JavascriptInterface
    public void removeKeysInSharedPrefs(String key) {
        KeyValueStore.remove(juspayServices, key);
        removeKeysInNativeSharedPrefs(key);
    }

    private void removeKeysInNativeSharedPrefs(String key) {
        if (!key.equals("")) {
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
            if (activity != null) activity.runOnUiThread(() -> {
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
                                    base64Data = Base64.encodeToString(payload.getBytes(), Base64.NO_WRAP);
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


    public static void callingStoreCall(DuiCallback dynamicUII, String notificationType) {
        if (dynamicUII != null && storeCallB != null) {
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

    public static void callingStoreCallBackPopUp(DuiCallback dynamicUII, JSONObject entity_payload) {
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


    public static void callingStoreCallCustomer(DuiCallback dynamicUII, String notificationType) {
        if (dynamicUII != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBCustomer, notificationType);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    static String storeCallBTime = "TimeUpdate";

    @JavascriptInterface
    public void storeCallBackTime(String callback) {
        storeCallBTime = callback;
    }


    public static void callingStoreCallBackTime(DuiCallback dynamicUII, String time, String lat, String lng, Context context) {
        if (dynamicUII != null && storeCallBTime != "TimeUpdate") {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                    storeCallBTime, time, lat, lng);
            System.out.println("time javascript " + javascript);
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
        activity.runOnUiThread(() -> {
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
        });
    }

    @JavascriptInterface
    public String getVersionName() {
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
    public int getVersionCode() {
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
    public String getManufacturerName() {
        return Build.MANUFACTURER;
    }

    @JavascriptInterface
    public int getAndroidVersion() {
        return Build.VERSION.SDK_INT;
    }

    @JavascriptInterface
    public String getPackageName() {
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
        activity.runOnUiThread(() -> FirebaseMessaging.getInstance().getToken()
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
                                callback, token);
                        if (dynamicUI != null && (juspayServices.getDynamicUI() != null)) {
                            dynamicUI.addJsToWebView(javascript);
                        }
                    }
                }));
    }

    @JavascriptInterface
    public void openWhatsAppSupport(String contactNumber) {
        String url = "https://api.whatsapp.com/send?phone=" + contactNumber;
        Intent i = new Intent(Intent.ACTION_VIEW);
        i.setData(Uri.parse(url));
        i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(i);
    }

    @JavascriptInterface
    public void loaderText(final String mainMsg, final String subMsg) {
        activity.runOnUiThread(() -> {
            TextView mainloaderText = activity.findViewById(R.id.loaderMainText);
            TextView subloaderText = activity.findViewById(R.id.loaderSubText);
            mainloaderText.setText(mainMsg);
            subloaderText.setText(subMsg);
        });
    }

    @JavascriptInterface
    public void timePicker(final String callback) {
        activity.runOnUiThread(() -> {
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
        });
    }

    private void reOrderSpinners(DatePickerDialog dialog, char[] dateOrder) {
        if (!dialog.isShowing()) {
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
        } else {
            imeOption = EditorInfo.IME_ACTION_DONE;
        }
        int idPickerInput = Resources.getSystem().getIdentifier("numberpicker_input", "id", "android");
        TextView input = (TextView) spinner.findViewById(idPickerInput);
        input.setImeOptions(imeOption);
    }

    @JavascriptInterface
    public void datePicker(final String callback, String label) {
        activity.runOnUiThread(() -> {
            final Calendar c = Calendar.getInstance();
            int mYear = c.get(Calendar.YEAR);
            int mMonth = c.get(Calendar.MONTH);
            int mDate = c.get(Calendar.DATE);
            int datePickerTheme = AlertDialog.THEME_HOLO_LIGHT;
            if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.N) datePickerTheme = 0;

            DatePickerDialog datePickerDialog = new DatePickerDialog(activity, datePickerTheme, new DatePickerDialog.OnDateSetListener() {
                @Override
                public void onDateSet(DatePicker datePicker, int year, int month, int date) {
                    if (dynamicUI != null && callback != null && juspayServices.getDynamicUI() != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d,%d,%d);",
                                callback, year, month, date);
                        dynamicUI.addJsToWebView(javascript);
                    }
                }


            }, mYear, mMonth, mDate) {

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
                        if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N) {
                            if (month != 0) {
                                NumberPicker monthPicker = findViewById(month);
                                if (monthPicker != null) {
                                    monthPicker.setDisplayedValues(monthNumbers);
                                }
                            }
                        }
                    } catch (Exception e) {
                        Log.e(LOG_TAG, "Error in onDateChanged : " + e);
                    }
                }

                @Override
                protected void onCreate(Bundle savedInstanceState) {
                    super.onCreate(savedInstanceState);
                    try {
                        if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N) {
                            if (month != 0) {
                                NumberPicker monthPicker = findViewById(month);
                                if (monthPicker != null) {
                                    monthPicker.setDisplayedValues(monthNumbers);
                                }
                            }
                        }
                    } catch (Exception e) {
                        Log.e(LOG_TAG, "Error in Date onCreate : " + e);
                    }
                }
            };

            switch (label) {
                case DatePickerLabels.MINIMUM_EIGHTEEN_YEARS:
                    Calendar maxDateDOB = Calendar.getInstance();
                    maxDateDOB.set(Calendar.DAY_OF_MONTH, mDate);
                    maxDateDOB.set(Calendar.MONTH, mMonth);
                    maxDateDOB.set(Calendar.YEAR, mYear - 18);
                    datePickerDialog.getDatePicker().setMaxDate(maxDateDOB.getTimeInMillis());
                    break;
                case DatePickerLabels.MAXIMUM_PRESENT_DATE:
                    datePickerDialog.getDatePicker().setMaxDate(System.currentTimeMillis() - 1000);
                    break;
            }
            if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N)
                datePickerDialog.setTitle(context.getString(R.string.select_date));
            else datePickerDialog.setTitle("");
            datePickerDialog.show();
            final char[] dateOrder =
                    {
                            'd',
                            'm',
                            'y'
                    };
            try {
                if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N)
                    reOrderSpinners(datePickerDialog, dateOrder);
            } catch (Exception e) {
                Log.e(LOG_TAG, "Error in reOrdering spinners : " + e);
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

    public static void callingStoreCallBackBatteryUsagePermission(DuiCallback dynamicUII, String isPermission) {
        if (dynamicUII != null && storeCallBBatteryUsagePermission != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBBatteryUsagePermission, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallBackDriverLocationPermission(DuiCallback dynamicUII, String isPermission) {
        if (dynamicUII != null && storeCallBDriverLocationPermission != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBDriverLocationPermission, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallImageUpload(DuiCallback dynamicUII, String stringImage, String imageName, String imagePath) {
        if (dynamicUII != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                    storeCallBackImageUpload, stringImage, imageName, imagePath);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallBackInternetAction(DuiCallback dynamicUII, String isPermission) {
        if (dynamicUII != null && !storeCallBInternetAction.equals("storeCallBackInternet")) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBInternetAction, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void callingStoreCallBackOverlayPermission(DuiCallback dynamicUII, String isPermission) {
        if (dynamicUII != null && storeCallBOverlayPermission != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBOverlayPermission, isPermission);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void requestBatteryPermission() {
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                Intent intent = new Intent(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
                Uri uri = Uri.fromParts("package", getPackageName(), null);
                intent.setData(uri);
                String packageName = context.getPackageName();
                PowerManager pm = (PowerManager) context.getSystemService(android.content.Context.POWER_SERVICE);
                if (pm.isIgnoringBatteryOptimizations(packageName)) {
                    intent.setAction(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
                } else {
                    intent.setAction(Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
                }
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                context.startActivity(intent);
            }
        } catch (ActivityNotFoundException e) {
            e.printStackTrace();
            Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
            Uri uri = Uri.fromParts("package", getPackageName(), null);
            intent.setData(uri);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            context.startActivity(intent);
        }
    }

    @JavascriptInterface
    public boolean isBatteryPermissionEnabled() {
        PowerManager powerManager = (PowerManager) activity.getSystemService(Context.POWER_SERVICE);
        return (powerManager.isIgnoringBatteryOptimizations(context.getPackageName()));
    }

    @JavascriptInterface
    public void disableActionEditText(final String id) {
        EditText editText = activity.findViewById(Integer.parseInt(id));
        if (editText != null) {
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
        System.out.println("In startLocationPollingAPI intent" + locationUpdateService);
        WorkManager mWorkManager;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.getApplicationContext().startForegroundService(locationUpdateService);
        } else {
            context.startService(locationUpdateService);
        }
        mWorkManager = WorkManager.getInstance(context);
        Constraints constraints = new Constraints.Builder()
                .setRequiresDeviceIdle(true)
                .build();
        PeriodicWorkRequest mWorkRequest = new PeriodicWorkRequest.Builder(LocationUpdateWorker.class, 13, TimeUnit.MINUTES).addTag(context.getString(R.string.location_update)).setConstraints(constraints).build();
        mWorkManager.enqueueUniquePeriodicWork(context.getString(R.string.location_update), ExistingPeriodicWorkPolicy.REPLACE, mWorkRequest);
    }

    @JavascriptInterface
    public void stopLocationPollingAPI() {
        Intent locationUpdateService = new Intent(activity, LocationUpdateService.class);
//        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
//        sharedPref.edit().putString("DRIVER_STATUS", "false").apply();
        activity.stopService(locationUpdateService);
        WorkManager mWorkManager = WorkManager.getInstance(context);
        mWorkManager.cancelAllWorkByTag(context.getString(R.string.location_update));
        Log.i(LOG_TAG, "stopLocationPollingAPI ");
    }

    //to check if works fine
    @JavascriptInterface
    public boolean isLocationPermissionEnabled() {
        return !(ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED);
    }

    @JavascriptInterface
    public boolean isOverlayPermissionEnabled() {
        if (NotificationUtils.overlayFeatureNotAvailable(context)) {
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
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in request permission", e);
        }
    }


    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.M)
    public void checkOverlayPermission() {
        System.out.println("CommonJsInterface checkOverlayPermission()");
        if (!Settings.canDrawOverlays(context)) {
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
                if (browserFragment != null) {
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
    public void metaLogEvent(String event) {
        try {
            AppEventsLogger logger = AppEventsLogger.newLogger(context);
            logger.logEvent(event);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error logging meta event : " + e);
        }
    }

    @JavascriptInterface
    public void firebaseLogEventWithParams(String event, String paramKey, String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey, paramValue);
        mFirebaseAnalytics.logEvent(event, params);
    }

    @JavascriptInterface
    public void firebaseLogEventWithTwoParams(String event, String paramKey1, String paramValue1, String paramKey2, String paramValue2) {
        Bundle params = new Bundle();
        params.putString(paramKey1, paramValue1);
        params.putString(paramKey2, paramValue2);
        mFirebaseAnalytics.logEvent(event, params);
    }

    @JavascriptInterface
    public void firebaseScreenNameLog(String screenName) {
        Bundle bundle = new Bundle();
        bundle.putString(FirebaseAnalytics.Param.SCREEN_NAME, screenName);
        bundle.putString(FirebaseAnalytics.Param.SCREEN_CLASS, "MainActivity");
        mFirebaseAnalytics.logEvent(FirebaseAnalytics.Event.SCREEN_VIEW, bundle);
    }

    @JavascriptInterface
    public void firebaseUserID(String id) {
        mFirebaseAnalytics.setUserId(id);
    }

    CleverTapAPI clevertapDefaultInstance = CleverTapAPI.getDefaultInstance(context);

    @JavascriptInterface
    public void setCleverTapUserData(String key, String value){
        HashMap<String, Object> profileUpdate = new HashMap<String, Object>();
        try {
            profileUpdate.put(key,value);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error sending user data: " + e);
        }

        if (clevertapDefaultInstance!=null) {
            clevertapDefaultInstance.onUserLogin(profileUpdate);
            sharedPrefs = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String fcmRegId = sharedPrefs.getString("FCM_TOKEN", "null");
            clevertapDefaultInstance.pushFcmRegistrationId(fcmRegId, true);
        }
    }

    @JavascriptInterface
    public void setCleverTapUserProp (String key, String value){
        HashMap<String, Object> profileUpdate = new HashMap<String, Object>();
        try {
            profileUpdate.put(key,value);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error sending user data: " + e);
        }
        if (clevertapDefaultInstance!=null)
            clevertapDefaultInstance.pushProfile(profileUpdate);
    }


    @JavascriptInterface
    public void cleverTapCustomEvent(String event){
        if (clevertapDefaultInstance!=null)
            clevertapDefaultInstance.pushEvent(event);
    }

    @JavascriptInterface
    public void cleverTapEvent(String event, String params){

        if (clevertapDefaultInstance!=null){
            Map<String, Object> resultMap = new HashMap<>();
            try {
                JSONArray jsonArray = new JSONArray(params);

                for (int i = 0; i < jsonArray.length(); i++) {
                    JSONObject jsonObject = jsonArray.getJSONObject(i);
                    String key = jsonObject.getString("key");
                    Object value = jsonObject.get("value");
                    resultMap.put(key, value);
                }
            } catch (JSONException e) {
                e.printStackTrace();
                return;
            }
            clevertapDefaultInstance.pushEvent(event,resultMap);}
    }

    @JavascriptInterface
    public void cleverTapCustomEventWithParams(String event, String paramKey, String paramValue){
        HashMap<String, Object> mapCustomEvent = new HashMap<String, Object>();
        mapCustomEvent.put(paramKey,paramValue);
        if (clevertapDefaultInstance!=null)
            clevertapDefaultInstance.pushEvent(event,mapCustomEvent);
    }

    @JavascriptInterface
    public void cleverTapSetLocation(){
        Location location = clevertapDefaultInstance.getLocation();
        clevertapDefaultInstance.setLocation(location);
    }

    @JavascriptInterface
    public void showDialer(String phoneNum, boolean call) {
        Intent intent = new Intent(call ? Intent.ACTION_CALL : Intent.ACTION_DIAL);
        intent.setData(Uri.parse("tel:" + phoneNum));
        if (call && ContextCompat.checkSelfPermission(activity, Manifest.permission.CALL_PHONE) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(activity, new String[]{Manifest.permission.CALL_PHONE}, REQUEST_CALL);
        } else {
            activity.startActivity(intent);
        }
    }

    @JavascriptInterface
    public void openNavigation(double slat, double slong, double dlat, double dlong) {
        try {
            if(sharedPref != null) sharedPref.edit().putString("MAPS_OPENED","true").apply();
            Uri gmmIntentUri = Uri.parse("google.navigation:q=" + String.valueOf(dlat) + "," + String.valueOf(dlong));
            Intent mapIntent = new Intent(Intent.ACTION_VIEW, gmmIntentUri);
            mapIntent.setPackage("com.google.android.apps.maps");
            activity.startActivity(mapIntent);
        } catch (Exception e) {
            setKeysInSharedPrefs("MAPS_OPENED","null");
            Log.e(LOG_TAG, "Can't open google maps", e);
        }
    }

    @JavascriptInterface
    public void openUrlInApp(String url) {
        activity.runOnUiThread(() -> {
            try {
                Intent httpIntent = new Intent(Intent.ACTION_VIEW);
                httpIntent.setData(Uri.parse(url));
                activity.startActivity(httpIntent);
            } catch (ActivityNotFoundException e) {
                Toast.makeText(context, context.getString(R.string.no_enabled_browser), Toast.LENGTH_LONG).show();
                firebaseLogEvent("exception_no_activity_found_for_intent");
            } catch (Exception e){
                firebaseLogEvent("exception_in_openUrlInApp");
            }
        });
    }

    @JavascriptInterface
    public void openUrlInMailApp(String mailId) {
        activity.runOnUiThread(() -> {
            try {
                Intent intent = new Intent(Intent.ACTION_SEND);
                String[] recipients = {mailId};
                intent.putExtra(Intent.EXTRA_EMAIL, recipients);
                intent.setType("text/html");
                intent.setPackage("com.google.android.gm");
                activity.startActivity(Intent.createChooser(intent, "Send mail"));
            } catch (Exception e) {
                Log.e(LOG_TAG, "Exception occurred while calling mail", e);
            }
        });
    }

    @JavascriptInterface
    public String getAAID() {
        try {
            AdvertisingIdClient.Info adInfo = AdvertisingIdClient.getAdvertisingIdInfo(context);
            String myId = adInfo != null ? adInfo.getId() : null;
            Log.i("UIDMY", myId);
            return myId;
        } catch (Exception e) {
            return "No ad id";
        }
    }

    @JavascriptInterface
    public void factoryResetApp() {
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

    private LocationCallback createLocCallBack() {
        return new LocationCallback() {
            @Override
            public void onLocationResult(@NonNull LocationResult locationResult) {
                super.onLocationResult(locationResult);
            }
        };
    }

    private LocationRequest createLocReq(int priority, long intervalMillis, long fastestIntervalMillis) {
        LocationRequest locationRequest = LocationRequest.create()
                .setPriority(priority)
                .setInterval(intervalMillis)
                .setFastestInterval(fastestIntervalMillis);
        return locationRequest;
    }

    @SuppressLint("MissingPermission")
    private void updateLastKnownLocation(String callback, boolean animate) {
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
                            if (animate && googleMap != null && lat != null && lng != null) {
                                LatLng latLng = new LatLng(lat, lng);
                                if (userPositionMarker == null) {
                                    upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f); //TODO this function will be removed
                                } else {
                                    if (storeMapCallBack == null)
                                        userPositionMarker.setVisible(true);
                                    userPositionMarker.setPosition(latLng);
                                }
                                googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLng, zoom));
                            }
                        } else getLastKnownLocationFromClientFallback(callback, animate);
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
    private void getLastKnownLocationFromClientFallback(String callback, boolean animate) {
        if (!isLocationPermissionEnabled()) return;
        if (client != null)
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
                                if (animate && googleMap != null && lat != null && lng != null) {
                                    LatLng latLng = new LatLng(lat, lng);
                                    if (userPositionMarker == null) {
                                        upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f); //TODO this function will be removed
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

        if (defaultText.equals(CURRENT_LOCATION_LATLON)) {
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
                        callback, latitude, longitude, returnedAddressStr);
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
        if (!isLocationPermissionEnabled()) return;
        updateLastKnownLocation(callback, true);
    }

    @JavascriptInterface
    public void requestKeyboardShow(final String id) {
        if (activity != null) {
            activity.runOnUiThread(() -> {
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
                    Log.e(LOG_TAG, "Keyboard Exception" + e.toString());
                }
            });
        }
    }

    @JavascriptInterface
    public void initialWebViewSetUp(String callback, String id) {
        webViewCallBack = callback;
        activity.runOnUiThread(() -> {
            WebView webView = (WebView) activity.findViewById(Integer.parseInt(id));
            if (webView == null) return;
            webView.setWebChromeClient(new WebChromeClient() {
                @Override
                public boolean onConsoleMessage(ConsoleMessage consoleMessage) {
                    String message = consoleMessage.message();
                    if (message.contains("Write permission denied")) {
                        // Handle the error here
                        ClipboardManager clipboard = (ClipboardManager) context.getSystemService(Context.CLIPBOARD_SERVICE);
                        ClipData clip = ClipData.newPlainText("MyLbl", "https://nammayatri.in/link/rider/SJ8D");
                        clipboard.setPrimaryClip(clip);
                    }
                    return super.onConsoleMessage(consoleMessage);
                }
            });
            webView.setWebViewClient(new WebViewClient() {
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
                    if (url.startsWith("tg:") || url.startsWith("https://www.facebook.com") || url.startsWith("https://www.twitter.com/") || url.startsWith("https://www.linkedin.com") || url.startsWith("https://api.whatsapp.com") || url.contains("YATRI.pdf") || url.startsWith("https://telegram.me/")) {
                        try {
                            CustomTabsIntent.Builder builder = new CustomTabsIntent.Builder();
                            CustomTabsIntent customTabsIntent = builder.build();
                            customTabsIntent.intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                            customTabsIntent.launchUrl(context, Uri.parse(url));
                            return true;
                        } catch (Exception e) {
                            if (toast != null) {
                                toast.cancel();
                            }
                            toast = Toast.makeText(context, "Looks like there is no app or web browser installed on your device", Toast.LENGTH_SHORT);
                            toast.show();
                            return true;
                        }
                    }
                    return false;
                }
            });
        });
    }

    @JavascriptInterface
    public void goBackPrevWebPage(String id) {
        WebView webView = (WebView) activity.findViewById(Integer.parseInt(id));
        activity.runOnUiThread(() -> {
            if (webView == null) return;
            if (webView.canGoBack()) {
                webView.post(() -> {
                    if (webView != null) {
                        webView.goBack();
                    }
                });
            } else {
                if (webViewCallBack != null && dynamicUI != null && juspayServices.getDynamicUI() != null) {
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                            webViewCallBack, String.valueOf("TRUE"));
                    dynamicUI.addJsToWebView(javascript);
                }
            }
        });
    }

    @JavascriptInterface
    public String getCurrentLatLong() throws JSONException { // TODO:: TO BE DEPRECATED AS FUNCTION IS NOT IN USE
        JSONObject location = new JSONObject();
        location.put("lat", lastLatitudeValue);
        location.put("lng", lastLongitudeValue);
        return location.toString();
    }

    @JavascriptInterface
    public void initiateLocationServiceClient() {
        if (!isLocationPermissionEnabled()) return;
        resolvableLocationSettingsReq();
    }


    private void resolvableLocationSettingsReq() {
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
        if (!isLocationPermissionEnabled()) return;
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
    public void previewImage(String base64Image) {
        activity.runOnUiThread(() -> {
            try {
                if (!base64Image.equals("") && base64Image != null) {
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
                    imagePreview.setMinimumHeight(screenHeight / 2);
                    imagePreview.setMinimumWidth(width);

                    ViewGroup.LayoutParams layoutParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT);
                    imagePreview.setLayoutParams(layoutParams);
                    builder.setView(imagePreview);
                    AlertDialog alertDialog = builder.create();
                    alertDialog.show();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public String generateSessionToken(String str) {

        AutocompleteSessionToken token = AutocompleteSessionToken.newInstance();
        System.out.println("SESSION_TOKEN -> " + token);
        String sessionToken = token.toString();
        return sessionToken;
    }

    @JavascriptInterface
    public void renderBase64Image(String url, String id, boolean fitCenter, boolean resize) {
        if (url.contains("http"))
            url = getAPIResponse(url);
        renderBase64ImageFile(url, id, fitCenter, resize);
    }

    @JavascriptInterface
    public void renderBase64ImageFile(String base64Image, String id, boolean fitCenter, boolean resize) {
        activity.runOnUiThread(() -> {
            try {
                if (!base64Image.equals("") && base64Image != null && id != null) {
                    LinearLayout layout = activity.findViewById(Integer.parseInt(id));
                    byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                    Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);
                    ImageView imageView = new ImageView(context);
                    if (resize) 
                    {
                    int cropTop = 170;    
                    int cropBottom = 0; 
                    int cropLeft = 700;  
                    int cropRight = 700; 
                    int newWidth = decodedByte.getWidth() - cropLeft - cropRight;
                    int newHeight = decodedByte.getHeight() - cropTop - cropBottom;
                    Bitmap croppedBitmap = Bitmap.createBitmap(decodedByte, cropLeft, cropTop, newWidth, newHeight);
                    float scaleWidth = ((float) (1500)) / croppedBitmap.getWidth();
                    Matrix matrix = new Matrix();
                    matrix.postScale(scaleWidth, 1);
                    Bitmap resizedBitmap = Bitmap.createBitmap(croppedBitmap, 0, 0, croppedBitmap.getWidth(), croppedBitmap.getHeight(), matrix, false);
                    imageView.setImageBitmap(croppedBitmap);
                    imageView.setRotation(90);
                    }
                    else
                    {
                     imageView.setImageBitmap(decodedByte);
                    }
                    if (fitCenter) 
                    {
                        imageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
                    } 
                    else 
                    {
                        imageView.setScaleType(ImageView.ScaleType.CENTER_CROP);
                    }
                    imageView.setAdjustViewBounds(true);
                    imageView.setClipToOutline(true);
                    layout.removeAllViews();
                    layout.addView(imageView);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

// if (!base64Image.equals("") && base64Image != null && id != null) {
//                 Toast.makeText(activity, id, Toast.LENGTH_SHORT).show();
//                 LinearLayout layout = activity.findViewById(Integer.parseInt(id));

//                 byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
//                 Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);

//                 int desiredHeight =  // Calculate your desired height after cropping
//                 int startY =  // Calculate the start Y position for cropping (from top)

//                 Bitmap croppedBitmap = Bitmap.createBitmap(decodedByte, 0, startY, decodedByte.getWidth(), desiredHeight);

//                 ImageView imageView = new ImageView(context);
//                 imageView.setImageBitmap(croppedBitmap);
                
//                 if (fitCenter) {
//                     imageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
//                 } else {
//                     imageView.setScaleType(ImageView.ScaleType.CENTER_CROP);
//                 }
                
//                 imageView.setAdjustViewBounds(true);
//                 imageView.setClipToOutline(true);
//                 imageView.setRotation(90);
                
//                 layout.removeAllViews();
//                 layout.addView(imageView);
//             }
    /*
     * This function is deprecated on 22 May - 2023
     * Added only for Backward Compatibility
     * Remove this function once it is not begin used.
     */

    @JavascriptInterface
    public void renderBase64Image(String url, String id) {
        String base64Image = getAPIResponse(url);
        if (activity != null) {
            activity.runOnUiThread(() -> {
                try {
                    if (!base64Image.equals("") && id != null) {
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
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
        }
    }


@JavascriptInterface
    public void renderCameraProfilePicture(String id) {
         if(activity!=null)
         {
        activity.runOnUiThread(() -> {
             if (isCameraPermissionGranted()) 
            {
            View profilePictureLayout = LayoutInflater.from(context).inflate(R.layout.validate_documents_preview, null, false);
            previewView = profilePictureLayout.findViewById(R.id.previewView);
            bCapture = profilePictureLayout.findViewById(R.id.bCapture);
            bCapture.setOnClickListener(view -> capturePhoto());
            ListenableFuture<ProcessCameraProvider> cameraProviderFuture = ProcessCameraProvider.getInstance(context);
            cameraProviderFuture.addListener(() -> {
            try {
                ProcessCameraProvider cameraProvider = cameraProviderFuture.get();
                startCameraX(cameraProvider);
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
                return ;
            }
            }, ContextCompat.getMainExecutor(activity));
             LinearLayout layout = activity.findViewById(Integer.parseInt(id));
             layout.removeAllViews();
             layout.addView(profilePictureLayout);
           } else 
              {
            requestCameraPermission(() -> renderCameraProfilePicture(id));   
              }    
            });
        }
    }

private boolean isCameraPermissionGranted() {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
        int cameraPermission = ContextCompat.checkSelfPermission(context, Manifest.permission.CAMERA);
        return cameraPermission == PackageManager.PERMISSION_GRANTED;
    }
    return true;
}

private void requestCameraPermission(Runnable callback) {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
        ActivityCompat.requestPermissions(activity, new String[]{Manifest.permission.CAMERA}, IMAGE_PERMISSION_REQ_CODE_PROFILE);
        cameraPermissionCallback = callback;
    }
}

@SuppressLint("RestrictedApi")
    private void startCameraX(ProcessCameraProvider cameraProvider) {
        cameraProvider.unbindAll();
        CameraSelector cameraSelector = new CameraSelector.Builder()
                .requireLensFacing(CameraSelector.LENS_FACING_BACK)
                .build();
        Preview preview = new Preview.Builder()
                .build();
        preview.setSurfaceProvider(previewView.getSurfaceProvider());
        imageCapture = new ImageCapture.Builder()
                .setCaptureMode(ImageCapture.CAPTURE_MODE_MINIMIZE_LATENCY)
                .build();
        ImageAnalysis imageAnalysis = new ImageAnalysis.Builder()
                .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
                .build();
        imageAnalysis.setAnalyzer(ContextCompat.getMainExecutor(activity), this::analyze);
        cameraProvider.bindToLifecycle((LifecycleOwner) activity, cameraSelector, preview, imageCapture);
    }

    public void analyze(@NonNull ImageProxy image) {
        Log.d("TAG", "analyze: got the frame at: " + image.getImageInfo().getTimestamp());
        image.close();
    }

private void capturePhoto() {
        long timestamp = System.currentTimeMillis();
        ContentValues contentValues = new ContentValues();
        contentValues.put(MediaStore.MediaColumns.DISPLAY_NAME, timestamp);
        contentValues.put(MediaStore.MediaColumns.MIME_TYPE, "image/jpeg");
        imageCapture.takePicture(
                new ImageCapture.OutputFileOptions.Builder(context.getContentResolver(), MediaStore.Images.Media.EXTERNAL_CONTENT_URI, contentValues).build(),
                ContextCompat.getMainExecutor(activity),
                new ImageCapture.OnImageSavedCallback() {
                    @Override
                    public void onImageSaved(@NonNull ImageCapture.OutputFileResults outputFileResults) {
                        Uri imageUri = outputFileResults.getSavedUri();
                        MainActivity.getInstance().encodeImageToBase64(null,imageUri);
                    }
                    @Override
                    public void onError(@NonNull ImageCaptureException exception) {
                        Toast.makeText(activity, "error", Toast.LENGTH_SHORT).show();
                    }
                }
        );
    }

    @JavascriptInterface
    public void setScaleType (String id, String imageUrl, String scaleType){
        if (activity == null) return;
        if (id != null || imageUrl != null){
            ImageView imageView = activity.findViewById(Integer.parseInt(id));
            try {
                URL url = new URL(imageUrl);
                Bitmap bitmap = BitmapFactory.decodeStream(url.openConnection().getInputStream());
                Handler mainLooper = new Handler(Looper.getMainLooper());
                mainLooper.post(() -> {
                    if (bitmap == null) return;
                    int calcHeight = (getScreenWidth() * bitmap.getHeight())/bitmap.getWidth();
                    imageView.getLayoutParams().height = calcHeight;
                    imageView.setScaleType(getScaleTypes(scaleType));
                    imageView.setImageBitmap(bitmap);
                    LinearLayout linearLayout = (LinearLayout) imageView.getParent();
                    linearLayout.removeAllViews();
                    linearLayout.addView(imageView);
                    imageView.setVisibility(View.VISIBLE);
                });
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    public ImageView.ScaleType getScaleTypes(String scale){
        switch (scale) {
            case "MATRIX" : return ImageView.ScaleType.MATRIX;
            case "FIT_XY" : return ImageView.ScaleType.FIT_XY;
            case "FIT_START" : return ImageView.ScaleType.FIT_START;
            case "FIT_END" : return ImageView.ScaleType.FIT_END;
            case "CENTER" : return ImageView.ScaleType.CENTER;
            case "CENTER_CROP" : return ImageView.ScaleType.CENTER_CROP;
            case "CENTER_INSIDE" : return ImageView.ScaleType.CENTER_INSIDE;
            default: return ImageView.ScaleType.FIT_CENTER;
        }
    }

    @JavascriptInterface
    public void startLottieProcess(String rawJson, String id, boolean repeat, float speed, String scaleType) {
        if (activity != null) activity.runOnUiThread(() -> {
            try {
                animationView = activity.findViewById(Integer.parseInt(id));
                animationView.setAnimationFromJson(getJsonFromResources(rawJson));
                animationView.loop(repeat);
                animationView.setSpeed(speed);
                animationView.playAnimation();
                animationView.setScaleType(getScaleTypes(scaleType));
            } catch (Exception e) {
                Log.d("TAG", "exception in startLottieAnimation", e);
            }
        });
    }

    private String getJsonFromResources(String rawJson) {
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
            activity.runOnUiThread(() -> {
                SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                FragmentManager supportFragmentManager = ((FragmentActivity) activity).getSupportFragmentManager();
                FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                fragmentTransaction.commitAllowingStateLoss();
                if (mapFragment != null) {
                    getMapAsync(mapFragment, isEnableCurrentLocation, mapType, callback, pureScriptId, zoom);
                }
            });
        } catch (Exception e) {
            Log.e("ADD_MARKER", e.toString());
        }
    } //NEW

    @JavascriptInterface
    public void mapSnapShot(final String pureScriptId, final String json, final String routeType, final boolean actualRoute, final String callback) {
        try {
            activity.runOnUiThread(() -> {
                SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                FragmentManager supportFragmentManager = ((FragmentActivity) activity).getSupportFragmentManager();
                FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                fragmentTransaction.commitAllowingStateLoss();
                mapFragment.getMapAsync(googleMap -> {
                    CommonJsInterface.this.googleMap = googleMap;
                    CommonJsInterface.this.googleMap.getUiSettings().setAllGesturesEnabled(false);
                    CommonJsInterface.this.googleMap.getUiSettings().setRotateGesturesEnabled(false);
                    googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                    markers = new JSONObject();
                    markersElement.put(pureScriptId, markers);
                    CommonJsInterface.this.googleMap.setOnMapLoadedCallback(new GoogleMap.OnMapLoadedCallback() {
                        @Override
                        public synchronized void onMapLoaded() {
                            System.out.println("onMapLoaded");
                            System.out.println(json);
                            showRoute(json, routeType, "#323643", actualRoute, "ny_ic_dest_marker", "ny_ic_src_marker", 8);
                            final Handler handler = new Handler();
                            handler.postDelayed(() -> {
                                GoogleMap.SnapshotReadyCallback callback2 = new GoogleMap.SnapshotReadyCallback() {
                                    Bitmap bitmap;

                                    @Override
                                    public void onSnapshotReady(Bitmap snapshot) {
                                        bitmap = snapshot;
                                        String encImage = "";
                                        try {
                                            ByteArrayOutputStream baos = new ByteArrayOutputStream();
                                            bitmap.compress(Bitmap.CompressFormat.JPEG, 80, baos);
                                            byte[] b = baos.toByteArray();
                                            encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                                        } catch (Exception e) {
                                            e.printStackTrace();
                                        }

                                        if (dynamicUI != null && callback != null && juspayServices.getDynamicUI() != null) {
                                            Log.i("callback encoded image 2", encImage);
                                            String javascript = String.format("window.callUICallback('%s','%s');", callback, encImage);
                                            Log.e(LOG_TAG, javascript);
                                            dynamicUI.addJsToWebView(javascript);
                                        }
                                    }
                                };
                                CommonJsInterface.this.googleMap.snapshot(callback2);
                            }, 2000);
                        }
                    });
                });
            });
        } catch (Exception e) {
            Log.e("ADD_MARKER", e.toString());
        }
    }

    @JavascriptInterface
    public void showRoute(final String json, final String style, final String trackColor, final boolean isActual, final String sourceMarker, final String destMarker, final int polylineWidth) {
        ArrayList<Polyline> lines = new ArrayList<>();
//        polylines.add(lines);
        activity.runOnUiThread(() -> {
            if (googleMap != null) {
                System.out.println("inside_showRoute");
                PolylineOptions polylineOptions = new PolylineOptions();
                int color = Color.parseColor(trackColor);
                try {
                    JSONObject jsonObject = new JSONObject(json);
                    JSONArray coordinates = jsonObject.getJSONArray("points");
                    JSONObject sourceCoordinates = (JSONObject) coordinates.get(0);
                    JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length() - 1);
                    double sourceLat = sourceCoordinates.getDouble("lat");
                    double sourceLong = sourceCoordinates.getDouble("lng");
                    double destLat = destCoordinates.getDouble("lat");
                    double destLong = destCoordinates.getDouble("lng");

                    double source_lat, source_lng, destination_lat, destination_lng;
                    if (sourceLat <= destLat) {
                        source_lat = sourceLat - 0.4 * (destLat - sourceLat);
                        destination_lat = destLat + 0.1 * (destLat - sourceLat);
                    } else {
                        source_lat = sourceLat + 0.1 * (sourceLat - destLat);
                        destination_lat = destLat - 0.4 * (sourceLat - destLat);
                    }
                    if (sourceLong <= destLong) {
                        source_lng = sourceLong - 0.09 * (destLong - sourceLong);
                        destination_lng = destLong + 0.09 * (destLong - sourceLong);
                    } else {
                        source_lng = sourceLong + 0.09 * (sourceLong - destLong);
                        destination_lng = destLong - 0.09 * (sourceLong - destLong);
                    }

                    if (googleMap != null) {
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
                        } catch (Exception e) {
                            System.out.println("In mmove camera in catch exception " + e);
                        }
                    }
//
                    if (isActual) {
                        for (int i = coordinates.length() - 1; i >= 0; i--) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            polylineOptions.add(new LatLng(lat, lng));
                        }
                    } else {
                        LatLng fromPointObj = new LatLng(sourceLat, sourceLong);
                        LatLng toPointObj = new LatLng(destLat, destLong);
                        polylineOptions.add(toPointObj);
                        polylineOptions.add(fromPointObj);
                    }

                    Polyline polyline = setRouteCustomTheme(polylineOptions, color, style, polylineWidth);

                    if (sourceMarker != null && !sourceMarker.equals("")) {
                        Bitmap sourceBitmap = constructBitmap(90, sourceMarker);
                        polyline.setStartCap(
                                new CustomCap(
                                        BitmapDescriptorFactory.fromBitmap(sourceBitmap)
                                )
                        );
                    }

                    if (destMarker != null && !destMarker.equals("")) {
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
        });
    }

    private void getMapAsync(SupportMapFragment mapFragment, boolean isEnableCurrentLocation, final String mapType, final String callback, final String pureScriptId, final float zoom) {
        mapFragment.getMapAsync(googleMap -> {
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
                if (mapType.equals(LOCATE_ON_MAP)) {
                    upsertMarker(LOCATE_ON_MAP, String.valueOf(lastLatitudeValue), String.valueOf(lastLongitudeValue), 160, 0.5f, 0.9f);
                    CommonJsInterface.this.googleMap.setOnCameraMoveListener(new GoogleMap.OnCameraMoveListener() {
                        @Override
                        public void onCameraMove() {
                            try {
                                double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                                double lng = (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                                upsertMarker(LOCATE_ON_MAP, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f);
                            } catch (Exception e) {
                                Log.i(LOG_TAG, "Marker creation error for ", e);
                            }
                        }
                    });
                    CommonJsInterface.this.googleMap.setOnCameraIdleListener(new GoogleMap.OnCameraIdleListener() {
                        @Override
                        public void onCameraIdle() {
                            if (dynamicUI != null && juspayServices.getDynamicUI() != null) {
                                double lat = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                                double lng = (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
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
            if (dynamicUI != null && callback != null && juspayServices.getDynamicUI() != null) {
                String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "MAP", "READY", "LOADED");
                Log.e(LOG_TAG, javascript);
                dynamicUI.addJsToWebView(javascript);
            }
        });
    } //NEW

    @JavascriptInterface
    public void exitLocateOnMap(String str) {
        try {
            this.storeMapCallBack = null;
            activity.runOnUiThread(() -> {
                for (Marker m : pickupPointsZoneMarkers) {
                    m.setVisible(false);
                }

                if(layer != null){
                    layer.removeLayerFromMap();
                }

                googleMap.setOnCameraMoveListener(null);
                googleMap.setOnCameraIdleListener(null);
            });
        } catch (Exception e) {
            Log.i(LOG_TAG, "LocateOnMap Exit Error for ", e);
        }
    }

    @JavascriptInterface
    public void enableMyLocation(boolean isEnableCurrentLocation) {
        try {
            activity.runOnUiThread((() -> {
                if (ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) == PackageManager.PERMISSION_GRANTED && CommonJsInterface.this.googleMap != null) {
                    CommonJsInterface.this.googleMap.setMyLocationEnabled(isEnableCurrentLocation);
                }
            }));
        } catch (Exception e) {
            Log.i(LOG_TAG, "Enable My Location on GoogleMap error", e);
        }
    }

    public JSONObject getNearestPoint(double lat, double lng, JSONArray path) throws JSONException {

        JSONObject jsonObject = new JSONObject();

        Location locationA = new Location("point A");
        locationA.setLatitude(lat);
        locationA.setLongitude(lng);

        double minDist = 10000000000.0;

        Location location = new Location("final point");

        for(int i=0;i<path.length();i++) {
            JSONObject a = path.getJSONObject(i);
            double px = (Double) a.get("lat");
            double py = (Double) a.get("lng");

            Location locationB = new Location("point B");
            locationB.setLatitude(px);
            locationB.setLongitude(py);

            float distance = locationA.distanceTo(locationB);

            if(distance<minDist){
                minDist = distance;
                location = locationB;
                zoneName = a.getString("place");
            }
        }
        jsonObject.put("place", zoneName);
        jsonObject.put("lat", location.getLatitude());
        jsonObject.put("long", location.getLongitude());
        return jsonObject;
    }

    public void drawMarkers(double lat , double lng, String name) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    MarkerOptions markerOptionsObj = new MarkerOptions()
                                                .title("")
                                                .position(new LatLng(lat,lng))
                                                .anchor(0.49f, 0.78f)
                                                .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(name,"ny_ic_zone_pickup_marker", null)));
                    Marker m = googleMap.addMarker(markerOptionsObj);
                    m.hideInfoWindow();
                    pickupPointsZoneMarkers.add(m);
                } catch (Exception e) {
                    Log.d("error on pickup markers", e.toString());
                }
            }
        });
    }

    @JavascriptInterface
    public void drawPolygon(String geoJson, String locationName) throws JSONException {

        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                System.out.println("Inside drawPolygon function");
                if(layer != null){
                    layer.removeLayerFromMap();
                }
                JSONObject geo = null;
                try {
                    geo = new JSONObject(geoJson);
                    PatternItem DASH = new Dash(20);
                    PatternItem GAP = new Gap(20);
                    List<PatternItem> PATTERN_POLYGON_ALPHA = Arrays.asList(GAP, DASH);
                    layer = new GeoJsonLayer(googleMap, geo);
                    GeoJsonPolygonStyle polyStyle = layer.getDefaultPolygonStyle();
                    polyStyle.setFillColor(Color.argb(25,0, 102, 255));
                    polyStyle.setStrokePattern(PATTERN_POLYGON_ALPHA);
                    polyStyle.setStrokeWidth(2);
                    polyStyle.setStrokeColor(Color.BLUE);
                    if (locationName.length() > 0){
                        zoom = 14.0f;
                        if (userPositionMarker == null) {
                            upsertMarker(CURRENT_LOCATION, String.valueOf(getKeyInNativeSharedPrefKeys("LAST_KNOWN_LAT")), String.valueOf( getKeyInNativeSharedPrefKeys("LAST_KNOWN_LON")),160, 0.5f,0.9f); //TODO this function will be removed
                        } else {
                        }
                        userPositionMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(locationName,CURRENT_LOCATION, null)));
                        userPositionMarker.setTitle("");
                        LatLng latLng = new LatLng(Double.valueOf(getKeyInNativeSharedPrefKeys("LAST_KNOWN_LAT")) , Double.valueOf( getKeyInNativeSharedPrefKeys("LAST_KNOWN_LON")));
                    }
                    layer.addLayerToMap();
//                    return;
                } catch (JSONException e) {
                    e.printStackTrace();
                }
//                return ;
            }
        });
//        return ;

    }

    @JavascriptInterface
    public void removeLabelFromMarker(){
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try{
                    zoom = 17.0f;
                    if(layer != null){
                        layer.removeLayerFromMap();
                    }
                    userPositionMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView("",CURRENT_LOCATION, null)));
                    userPositionMarker.setTitle("");
                }
                catch (Exception e){
                    e.printStackTrace();
                }
            }
        });

        return;
    }

    @JavascriptInterface
    public void locateOnMap (boolean goToCurrentLocation, final String lat, final String lon, String geoJson, String points){
        System.out.println("Inside locateOnMap" + geoJson);
        if (geoJson.equals("")){
            locateOnMap(goToCurrentLocation,lat,lon);
            return;
        }
        try {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    try {
                        drawPolygon(geoJson, "");
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
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
                            double lng = (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                            System.out.println("Inside OnCameraIdle");
                            ExecutorService executor = Executors.newSingleThreadExecutor();
                            Handler handler = new Handler(Looper.getMainLooper());
                            System.out.println("Inside OnCameraIdle");
                            executor.execute(() -> {
                                try {
                                    new Thread(new Runnable() {
                                        @Override
                                        public void run() {
                                            Boolean res =  isPointInside(lat, lng);
                                            System.out.println("Inside OnCameraIdle");
                                            handler.post(() -> {
                                                try {
                                                    if(res){
                                                        System.out.println("Inside OnCameraIdle"+res);
                                                        JSONArray zonePoints = new JSONArray(points);
                                                        System.out.println("Inside zonepoints" + zonePoints);
                                                        JSONObject nearestPickupPointObj = getNearestPoint(lat, lng, zonePoints);
                                                        Location nearestPickupPoint = new Location("");
                                                        nearestPickupPoint.setLatitude(nearestPickupPointObj.getDouble("lat"));
                                                        nearestPickupPoint.setLongitude(nearestPickupPointObj.getDouble("long"));

                                                        for (Marker m : pickupPointsZoneMarkers) {
                                                            m.setVisible(false);
                                                        }

                                                        for(int i=0;i<zonePoints.length();i++){
                                                            if(SphericalUtil.computeDistanceBetween(CommonJsInterface.this.googleMap.getCameraPosition().target, new LatLng((Double)zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng")))<=1){
                                                                drawMarkers((Double) zonePoints.getJSONObject(i).get("lat"),(Double) zonePoints.getJSONObject(i).get("lng"),  (String)zonePoints.getJSONObject(i).get("place"));
                                                                zoneName = (String)zonePoints.getJSONObject(i).get("place");
                                                            }
                                                            else{
                                                                drawMarkers((Double) zonePoints.getJSONObject(i).get("lat"),(Double) zonePoints.getJSONObject(i).get("lng"), "");
                                                            }
                                                        }

                                                        if(SphericalUtil.computeDistanceBetween(CommonJsInterface.this.googleMap.getCameraPosition().target, new LatLng(nearestPickupPoint.getLatitude(), nearestPickupPoint.getLongitude()))>1){
                                                            animateCamera(nearestPickupPoint.getLatitude(), nearestPickupPoint.getLongitude(),25.0f);
                                                        }
                                                    }
                                                    else {
                                                        for (Marker m : pickupPointsZoneMarkers) {
                                                            m.setVisible(false);
                                                        }
                                                    }
                                                    if (storeMapCallBack != null && dynamicUI!=null && juspayServices.getDynamicUI() != null){
                                                        String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeMapCallBack, zoneName, lat, lng);
                                                        Log.e(LOG_TAG, javascript);
                                                        dynamicUI.addJsToWebView(javascript);
                                                    }
                                                } catch (JSONException e) {
                                                    System.out.println("Exception " + e);
                                                }
                                                executor.shutdown();
                                            });
                                        }
                                    }).start();
                                } catch (Exception e) {
                                    Log.e ("api response error",e.toString());
                                }
                            });
//                            if (storeMapCallBack != null && dynamicUI!=null && juspayServices.getDynamicUI() != null){
//                                String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeMapCallBack, zoneName, lat, lng);
//                                Log.e(LOG_TAG, javascript);
//                                dynamicUI.addJsToWebView(javascript);
//                            }
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

    private Boolean isPointInside(Double lat, Double lng) {
        System.out.println("Inside isPOinteINside");
        StringBuilder result = new StringBuilder();
        sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        regToken = sharedPref.getString(context.getResources().getString(R.string.REGISTERATION_TOKEN), "null");
        baseUrl = sharedPref.getString("BASE_URL", "null");
        String version = sharedPref.getString("VERSION_NAME", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        System.out.println("BaseUrl" + baseUrl);
        try {
            String url = baseUrl + "/serviceability/origin";
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("token", regToken);
            connection.setRequestProperty("x-client-version", version);
            connection.setRequestProperty("x-device",deviceDetails);

            JSONObject payload = new JSONObject();

            JSONObject latLng = new JSONObject();
            latLng.put("lat", lat);
            latLng.put("lon", lng);
            payload.put("location", latLng);

            OutputStream stream = connection.getOutputStream();
            stream.write(payload.toString().getBytes());
            connection.connect();
            int respCode = connection.getResponseCode();
            System.out.println("Response Code ::" + respCode);
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                JSONObject errorPayload = new JSONObject(result.toString());
            } else {
                respReader = new InputStreamReader(connection.getInputStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                JSONObject res = new JSONObject(String.valueOf(result));
                System.out.println( res.getString("serviceable") + "my point result");
                return  true;
            }

            return false;
        } catch (Exception e) {
            return false;
        }
    }

    @JavascriptInterface
    public void locateOnMap(boolean goToCurrentLocation, final String lat, final String lon) {
        try {
            activity.runOnUiThread(() -> {
                removeMarker("ny_ic_customer_current_location");
                if (goToCurrentLocation) {
                    LatLng latLng = new LatLng(lastLatitudeValue, lastLongitudeValue);
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                } else {
                    LatLng latLng = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                    googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                }
                googleMap.setOnCameraIdleListener(new GoogleMap.OnCameraIdleListener() {
                    @Override
                    public void onCameraIdle() {
                        double lat1 = (CommonJsInterface.this.googleMap.getCameraPosition().target.latitude);
                        double lng = (CommonJsInterface.this.googleMap.getCameraPosition().target.longitude);
                        if (storeMapCallBack != null && dynamicUI != null && juspayServices.getDynamicUI() != null) {
                            String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeMapCallBack, "LatLon", lat1, lng);
                            Log.e(LOG_TAG, javascript);
                            dynamicUI.addJsToWebView(javascript);
                        }
                    }
                });
                if ((lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) && goToCurrentLocation) {
                    LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                    CommonJsInterface.this.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                } else {
                    LatLng latLngObjMain = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                    CommonJsInterface.this.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                    googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                }
            });

        } catch (Exception e) {
            Log.i(LOG_TAG, "LocateOnMap error for ", e);
        }
    }

    @JavascriptInterface
    public void reallocateMapFragment(final String pureScriptId) {
        activity.runOnUiThread(() -> {
            try {
                SupportMapFragment mapFragment = (SupportMapFragment) ((FragmentActivity) activity).getSupportFragmentManager()
                        .findFragmentById(Integer.parseInt(pureScriptId));
                if (mapFragment != null) {
                    mapFragment.getMapAsync(googleMap -> {
                        CommonJsInterface.this.googleMap = googleMap;
                        googleMap.getUiSettings().setRotateGesturesEnabled(false);
                        googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                        markers = markersElement.get(pureScriptId);
                    });
                }
            } catch (Exception e) {
                Log.e("FAILED WHILE REALLOCATING", e.toString());
            }
        });
    }

    private MarkerOptions makeMarkerObject(final String title, final double lat, final double lng, final int markerSize, final float anchorV, final float anchorV1) {
        try {
            MarkerOptions markerOptions = new MarkerOptions()
                    .position(new LatLng(lat, lng))
                    .title(title)
                    .anchor(anchorV, anchorV1);
            if (!title.equals(LOCATE_ON_MAP)) {
                Bitmap smallMarker = constructBitmap(markerSize, title);
                markerOptions.icon(BitmapDescriptorFactory.fromBitmap(smallMarker));
            }
            return markerOptions;
        } catch (Exception e) {
            Log.e(LOG_TAG, "MARKER obj creation", e);
            return null;
        }
    }

    private Bitmap constructBitmap(final int markerSize, final String title) {
        int imageID = context.getResources().getIdentifier(title, "drawable", activity.getPackageName());
        BitmapDrawable bitmapdraw = (BitmapDrawable) context.getResources().getDrawable(imageID);
        Bitmap b = bitmapdraw.getBitmap();
        //int markerWidth = markerSize;
        //if(title.equals(CURRENT_LOCATION)){
        //    markerWidth -= 50;
        //}
        float maximum = Math.max(b.getWidth(), b.getHeight());
        float minimum = Math.min(b.getWidth(), b.getHeight());
        float multiplier = markerSize / maximum;
        int markerWidth = Math.round(b.getWidth() * multiplier);
        int markerHeight = Math.round(b.getHeight() * multiplier);
        Log.i("real width and height of " + title, String.valueOf(b.getWidth()) + " , " + String.valueOf(b.getHeight()));
        Log.i("after width and height of " + title, String.valueOf(markerWidth) + " , " + String.valueOf(markerHeight));
        return Bitmap.createScaledBitmap(b, markerWidth, markerHeight, false);
    }

    private Bitmap getMarkerBitmapFromView(String locationName, String imageName, String specialLocationTagIcon) {
        View customMarkerView = ((LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate( imageName.equals("ny_ic_zone_pickup_marker") ? R.layout.zone_label_layout :  R.layout.marker_label_layout, null);
        View ImageAndTextView = customMarkerView.findViewById(R.id.zone_image_and_text);
        TextView label = customMarkerView.findViewById(R.id.marker_text);
        if (locationName.equals("")) {
            ImageAndTextView.setVisibility(View.GONE);
        }else {
            if (locationName.length() <= 27) {
                label.setText(locationName);
            } else {
                label.setText(locationName.substring(0, 17) + "...");
            }
        }

        try {
            if (specialLocationTagIcon != null) {
                ImageView specialTagImage = customMarkerView.findViewById(R.id.zone_image);
                specialTagImage.setVisibility(View.VISIBLE);
                int imageID = context.getResources().getIdentifier(specialLocationTagIcon, "drawable", activity.getPackageName());
                BitmapDrawable bitmapdraw = (BitmapDrawable) context.getResources().getDrawable(imageID);
                specialTagImage.setImageDrawable(bitmapdraw);
            }
        } catch (Exception e) {
            Log.e("Exception in rendering Image for special zone", e.toString());
        }

        ImageView pointer = customMarkerView.findViewById(R.id.pointer_img);
        try {
            if (imageName.equals("ny_ic_dest_marker")) {
                pointer.setImageDrawable(context.getResources().getDrawable(R.drawable.ny_ic_dest_marker));
            } else if(imageName.equals("ny_ic_zone_pickup_marker")){
                pointer.setImageDrawable(context.getResources().getDrawable(R.drawable.ny_ic_zone_pickup_marker));
            } else if(imageName.equals("ny_ic_customer_current_location")){
                pointer.setImageDrawable(context.getResources().getDrawable(R.drawable.ny_ic_customer_current_location));
                ViewGroup.LayoutParams layoutParams = (ViewGroup.LayoutParams) pointer.getLayoutParams();
                layoutParams.height = 160;
                layoutParams.width = 160;
                pointer.setLayoutParams(layoutParams);
            } else {
                pointer.setImageDrawable(context.getResources().getDrawable(R.drawable.ny_ic_src_marker));
            }
        } catch (Exception e) {
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
        activity.runOnUiThread(() -> {
            try {
                if (lat != null && lng != null) {
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
                            if (title.equals("ny_ic_customer_current_location")) {
                                userPositionMarker = markerObject;
                            }
                            Log.i(LOG_TAG, "New marker created and updated for " + title);
                        }
                    }
                }
            } catch (Exception e) {
                Log.i(LOG_TAG, "Marker creation error for " + title, e);
            }
        });
    }

    @JavascriptInterface
    public void removeMarker(final String title) {
        activity.runOnUiThread(() -> {
            try {
                if (markers.has(title)) {
                    Marker m = (Marker) markers.get(title);
                    m.setVisible(false);
                    Log.i(LOG_TAG, "Removed marker " + title);
                }
            } catch (Exception e) {
                Log.i(LOG_TAG, "Remove Marker error " + title, e);
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
    public void clearFocus(String id) {
        activity.runOnUiThread(() -> activity.findViewById(Integer.parseInt(id)).clearFocus());
    }

    @JavascriptInterface
    public String stopAudioRecording() {
        if (audioRecorder != null) {
            String res = audioRecorder.stopRecording();
            Log.d(LOG_TAG, "stopAudioRecording: " + res);
            audioRecorder = null;
            return res;
        }
        return null;
    }

    public boolean isMicrophonePermissionEnabled() {
        return ActivityCompat.checkSelfPermission(activity.getApplicationContext(), Manifest.permission.RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED;
    }

    @JavascriptInterface
    public boolean startAudioRecording() {
        if (isMicrophonePermissionEnabled()) {
            audioRecorder = new AudioRecorder();
            audioRecorder.startRecording();
            return true;
        } else {
            String[] permissions = {Manifest.permission.RECORD_AUDIO};
            ActivityCompat.requestPermissions(activity, permissions, AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION);
            return false;
        }
    }

    @JavascriptInterface
    public void addMediaPlayer(String viewID, String source) {
        activity.runOnUiThread(() -> {
            MediaPlayerView audioPlayer = new MediaPlayerView(context, activity);
            try {
                audioPlayer.inflateView(Integer.parseInt(viewID));
                if (source.contains(".mp3")) {
                    audioPlayer.addAudioFileUrl(source);
                } else {
                    Thread thread = new Thread(() -> {
                        try {
                            String base64 = getAPIResponse(source);
                            byte decodedAudio[] = Base64.decode(base64, Base64.DEFAULT);
                            File tempMp3 = File.createTempFile("audio_cache", "mp3", context.getCacheDir());
                            tempMp3.deleteOnExit();
                            FileOutputStream fos = new FileOutputStream(tempMp3);
                            fos.write(decodedAudio);
                            fos.close();
                            FileInputStream fis = new FileInputStream(tempMp3);
                            audioPlayer.addAudioFileInput(fis);
                        } catch (Exception e) {

                        }
                    });
                    thread.start();
                }
                audioPlayers.add(audioPlayer);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public String saveAudioFile(String source) throws IOException {
        File sourceFile = new File(source);
        FileInputStream fis = new FileInputStream(sourceFile);
        File destFile = new File(MainActivity.getInstance().getFilesDir().getAbsolutePath() + "final_audio_record.mp3");
        FileOutputStream fos = new FileOutputStream(destFile);
        int n;
        while ((n = fis.read()) != -1) {
            fos.write(n);
        }
        fis.close();
        fos.close();
        return destFile.getAbsolutePath();
    }

    @JavascriptInterface
    public void addMediaFile(String viewID, String source, String actionPlayerID, String playIcon, String pauseIcon, String timerID) throws IOException {
        Log.d(LOG_TAG, "addMediaFile: " + source);
        activity.runOnUiThread(() -> {
            MediaPlayerView audioPlayer;
            if (Integer.parseInt(actionPlayerID) != -1) {
                if (Integer.parseInt(timerID) != -1) {
                    audioPlayer = new MediaPlayerView(context, activity, Integer.parseInt(actionPlayerID), playIcon, pauseIcon, Integer.parseInt(timerID));
                    audioPlayer.setTimerId(Integer.parseInt(timerID));
                    audioPlayer.setTimerColorAndSize(Color.WHITE, 14);
                    audioPlayer.setVisualizerBarPlayedColor(Color.WHITE);
                } else {
                    audioPlayer = new MediaPlayerView(context, activity, Integer.parseInt(actionPlayerID), playIcon, pauseIcon);
                    audioPlayer.setTimerColorAndSize(Color.GRAY, 14);
                }
            } else {
                audioPlayer = new MediaPlayerView(context, activity);
            }
            try {
                audioPlayer.inflateView(Integer.parseInt(viewID));
                if (source.startsWith("http")) {
                    Thread thread = new Thread(() -> {
                        try {
                            String base64 = getAPIResponse(source);
                            byte decodedAudio[] = Base64.decode(base64, Base64.DEFAULT);
                            File tempMp3 = File.createTempFile("audio_cache", "mp3", context.getCacheDir());
                            tempMp3.deleteOnExit();
                            FileOutputStream fos = new FileOutputStream(tempMp3);
                            fos.write(decodedAudio);
                            fos.close();
                            FileInputStream fis = new FileInputStream(tempMp3);
                            audioPlayer.addAudioFileInput(fis);
                        } catch (Exception e) {

                        }
                    });
                    thread.start();
                } else {
                    File file = new File(source);
                    FileInputStream fis = new FileInputStream(file);
                    audioPlayer.addAudioFileInput(fis);
                }
                audioPlayers.add(audioPlayer);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public String uploadMultiPartData(String filePath, String uploadUrl, String fileType) throws IOException {
        String boundary = UUID.randomUUID().toString();

        URL url = new URL(uploadUrl);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("POST");
        connection.setDoOutput(true);
        connection.setUseCaches(false);
        connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);
        connection.setRequestProperty("token", getKeyInNativeSharedPrefKeys("REGISTERATION_TOKEN"));

        File file = new File(filePath);
        String fileName = file.getName();
        DataOutputStream outputStream = new DataOutputStream(connection.getOutputStream());

        outputStream.writeBytes("--" + boundary + "\r\n");
        outputStream.writeBytes(("Content-Disposition: form-data; name=\"file\"; filename=\"" + fileName + "\"" + "\r\n"));
        if (fileType.equals("Image"))
            outputStream.writeBytes("Content-Type: image/jpeg\r\n");
        else if (fileType.equals("Audio"))
            outputStream.writeBytes("Content-Type: audio/mpeg\r\n");
        outputStream.writeBytes("\r\n");

        FileInputStream fileInputStream = new FileInputStream(filePath);
        int bytesAvailable = fileInputStream.available();
        int maxBufferSize = 1024 * 1024;
        int bufferSize = Math.min(bytesAvailable, maxBufferSize);

        byte[] buffer = new byte[bufferSize];
        int bytesRead = fileInputStream.read(buffer, 0, bufferSize);
        while (bytesRead > 0) {
            outputStream.write(buffer, 0, bufferSize);
            bytesAvailable = fileInputStream.available();
            bufferSize = Math.min(bytesAvailable, maxBufferSize);
            bytesRead = fileInputStream.read(buffer, 0, bufferSize);
        }
        outputStream.writeBytes("\r\n");
        outputStream.writeBytes("--" + boundary + "\r\n");

        outputStream.writeBytes("Content-Disposition: form-data; name=\"fileType\"" + "\r\n");
        outputStream.writeBytes("Content-Type: application/json" + "\r\n");
        outputStream.writeBytes("\r\n");
        outputStream.writeBytes(fileType);
        outputStream.writeBytes("\r\n");
        outputStream.writeBytes("--" + boundary + "\r\n" + "--");

        int responseCode = connection.getResponseCode();
        String res = "";
        if (responseCode == 200) {
            StringBuilder s_buffer = new StringBuilder();
            InputStream is = new BufferedInputStream(connection.getInputStream());
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(is));
            String inputLine;
            while ((inputLine = bufferedReader.readLine()) != null) {
                s_buffer.append(inputLine);
            }
            res = s_buffer.toString();
            JsonObject jsonObject = JsonParser.parseString(res).getAsJsonObject();
            res = jsonObject.get("fileId").getAsString();
        } else {
            BufferedReader br = new BufferedReader(new InputStreamReader(connection.getErrorStream()));
            Toast.makeText(MainActivity.getInstance(), "Unable to upload image", Toast.LENGTH_SHORT).show();
        }
        return res;
    }

    @JavascriptInterface
    public void removeMediaPlayer() {
        for (MediaPlayerView audioPlayer : audioPlayers) {
            audioPlayer.resetListeners();
        }
        context.getCacheDir().delete();
        audioPlayers.removeAll(audioPlayers);
        DefaultMediaPlayerControl.mediaPlayer.reset();
    }

    @JavascriptInterface
    public void updateRoute(String json, String dest, String eta, String specialLocation) {
        activity.runOnUiThread(() -> {
            if (googleMap != null) {
                try {
                    ArrayList<LatLng> path = new ArrayList<>();
                    JSONObject jsonObject = null;
                    jsonObject = new JSONObject(json);
                    JSONArray coordinates = jsonObject.getJSONArray("points");
                    for (int i = coordinates.length() - 1; i >= 0; i--) {
                        JSONObject coordinate = (JSONObject) coordinates.get(i);
                        double lng = coordinate.getDouble("lng");
                        double lat = coordinate.getDouble("lat");
                        LatLng tempPoint = new LatLng(lat, lng);
                        path.add(tempPoint);
                    }
                    Marker currMarker = (Marker) markers.get("ic_vehicle_nav_on_map");
                    Marker destMarker = (Marker) markers.get(dest);
                    JSONObject specialLocationObject = new JSONObject(specialLocation);
                    String destinationSpecialTagIcon = specialLocationObject.getString("destSpecialTagIcon");

                    destMarker.setIcon((BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(eta, dest, destinationSpecialTagIcon.equals("") ? null : destinationSpecialTagIcon))));
                    if (polylines != null) {
                        polylines.setEndCap(new ButtCap());
                        if (path.size() == 0) {
                            LatLng destination = destMarker.getPosition();
                            animateMarkerNew(destination, currMarker);
                            polylines.remove();
                            polylines = null;
                            currMarker.setAnchor(0.5f, 0);
                            animateCamera(destMarker.getPosition().latitude, destMarker.getPosition().longitude, 17.0f);
                        } else {
                            double destinationLat = path.get(0).latitude;
                            double destinationLon = path.get(0).longitude;
                            double sourceLat = path.get(path.size() - 1).latitude;
                            double sourceLong = path.get(path.size() - 1).longitude;
                            LatLng destination = path.get(path.size() - 1);
                            animateMarkerNew(destination, currMarker);
                            PatternItem DASH = new Dash(1);
                            List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Arrays.asList(DASH);
                            polylines.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                            polylines.setPoints(path);
                            if (debounceAnimateCamera != 0) {
                                debounceAnimateCamera--;
                            } else {
                                moveCamera(sourceLat, sourceLong, destinationLat, destinationLon, coordinates);
                                debounceAnimateCamera = 10;
                            }
                        }
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @JavascriptInterface
    public void shareTextMessage(String title, String message) {
        activity.runOnUiThread(() -> {
            try{
                if (context != null){
                    Intent sendIntent = new Intent();
                    sendIntent.setAction(Intent.ACTION_SEND);
                    sendIntent.putExtra(Intent.EXTRA_TEXT, message);
                    sendIntent.putExtra(Intent.EXTRA_TITLE, title);
                    Bitmap thumbnailBitmap = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_launcher);
                    sendIntent.setType("text/plain");
                   if (thumbnailBitmap != null &&  Build.VERSION.SDK_INT > 28) {
                        Uri thumbnailUri = getImageUri(context, thumbnailBitmap);
                        ClipData clipData = ClipData.newUri(context.getContentResolver(), "ThumbnailImage" + System.currentTimeMillis(), thumbnailUri);
                        sendIntent.setClipData(clipData);
                   }
                    sendIntent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
                    Intent shareIntent = Intent.createChooser(sendIntent, null);
                    shareIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    context.startActivity(shareIntent);
                }
            }
            catch(Exception e) {
                firebaseLogEvent("exception_in_shareTextMessage");
            }

        });
    }

    private Uri getImageUri(Context context, Bitmap bitmap) {
        try {
            ByteArrayOutputStream bytes = new ByteArrayOutputStream();
            bitmap.compress(Bitmap.CompressFormat.PNG, 100, bytes);
            String path = MediaStore.Images.Media.insertImage(context.getContentResolver(), bitmap, "ThumbnailImage", null);
            return Uri.parse(path);
        } catch (Exception e) {
            return null;
        }
    }

    @JavascriptInterface
    public void shareImageMessage(String message, String imageName) {
        activity.runOnUiThread(() -> {
            Intent sendIntent = new Intent();
            int image = context.getResources().getIdentifier(imageName, "drawable", context.getPackageName());
            BitmapDrawable bitmapDrawable = (BitmapDrawable) context.getResources().getDrawable(image);
            Bitmap bitmap = bitmapDrawable.getBitmap();
            Uri uri = Uri.parse(MediaStore.Images.Media.insertImage(context.getContentResolver(), bitmap, "qrCode", null));
            sendIntent.setAction(Intent.ACTION_SEND);
            sendIntent.putExtra(Intent.EXTRA_STREAM, uri);
            sendIntent.putExtra(Intent.EXTRA_TEXT, message);
            sendIntent.setType("image/*");
            Intent shareIntent = Intent.createChooser(sendIntent, null);
            activity.startActivity(shareIntent);
        });
    }

    @JavascriptInterface
    public String isCoordOnPath(String json, double currLat, double currLng, int speed) throws JSONException {
        LatLng currPoint = new LatLng(currLat, currLng);
        ArrayList<LatLng> path = new ArrayList<>();
        JSONObject jsonObject = new JSONObject(json);
        int distanceRemaining;
        JSONArray coordinates = jsonObject.getJSONArray("points");
        int eta = 0;
        int resultIndex;
        JSONObject result = new JSONObject();
        for (int i = coordinates.length() - 1; i >= 0; i--) {
            JSONObject coordinate = (JSONObject) coordinates.get(i);
            double lng = coordinate.getDouble("lng");
            double lat = coordinate.getDouble("lat");
            LatLng tempPoints = new LatLng(lat, lng);
            path.add(tempPoints);
        }
        if (path.size() == 0) {
            result.put("points", new JSONArray());
            result.put("eta", 0);
            result.put("distance", 0);
            result.put("isInPath", false);
            return result.toString();
        }
        SharedPreferences sharedPref = context.getSharedPreferences(
                activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        double locationOnPathThres = Double.parseDouble(sharedPref.getString("ACCURACY_THRESHOLD", "30.0"));
        resultIndex = PolyUtil.locationIndexOnEdgeOrPath(currPoint, path, PolyUtil.isClosedPolygon(path), true, locationOnPathThres);
        if (resultIndex == -1) {
            result.put("points", coordinates);
            result.put("eta", 0);
            result.put("distance", 0);
            result.put("isInPath", false);
        } else if (resultIndex == 0) {
            path.clear();
            result.put("points", new JSONArray());
            result.put("eta", 0);
            result.put("distance", 0);
            result.put("isInPath", true);
        } else if (resultIndex == (path.size() - 2) || resultIndex == (path.size() - 1)) {
            distanceRemaining = (int) SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            result.put("points", coordinates);
            result.put("eta", eta);
            result.put("distance", distanceRemaining);
            result.put("isInPath", true);
        } else {
            path.subList(resultIndex + 2, path.size()).clear();
            distanceRemaining = (int) SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            JSONArray remainingPoints = new JSONArray();
            for (int i = path.size() - 1; i >= 0; i--) {
                LatLng point = path.get(i);
                JSONObject tempPoints = new JSONObject();
                tempPoints.put("lat", point.latitude);
                tempPoints.put("lng", point.longitude);
                remainingPoints.put(tempPoints);
            }
            result.put("points", remainingPoints);
            result.put("eta", eta);
            result.put("distance", distanceRemaining);
            result.put("isInPath", true);
        }
        return result.toString();
    }

    @SuppressLint("MissingPermission")
    @JavascriptInterface
    public void isMockLocation(String callback) {
        if (!isLocationPermissionEnabled()) return;
        try {
            client.getLastLocation()
                    .addOnSuccessListener(activity, location -> {
                        boolean isMock = false;
                        if (location == null) return;
                        if (Build.VERSION.SDK_INT <= 30) {
                            isMock = location.isFromMockProvider();
                            //methodName = "isFromMockProvider";
                        } else if (Build.VERSION.SDK_INT >= 31) {
                            isMock = location.isMock();
                            //methodName = "isMock";
                        }
                        if (callback != null && dynamicUI != null && juspayServices.getDynamicUI() != null) {
                            String js = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                    callback, isMock);
                            dynamicUI.addJsToWebView(js);
                        }
                    })
                    .addOnFailureListener(activity, e -> Log.e(LOG_TAG, "Last and current position not known"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private float bearingBetweenLocations(LatLng latLng1, LatLng latLng2) {
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
        return (float) brng;
    }

    private void animateMarkerNew(final LatLng destination, final Marker marker) {

        if (marker != null) {

            LatLng startPosition = marker.getPosition();
            LatLng endPosition = destination;


            ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1);
            valueAnimator.setDuration(2000); // duration 3 second
            valueAnimator.setInterpolator(new LinearInterpolator());
            valueAnimator.addUpdateListener(animation -> {
                try {
                    float v = animation.getAnimatedFraction();
                    LatLng newPosition = SphericalUtil.interpolate(startPosition, endPosition, v);
                    float rotation = bearingBetweenLocations(startPosition, endPosition);
                    if (rotation > 1.0)
                        marker.setRotation(rotation);
                    marker.setPosition(newPosition);
                    markers.put("ic_vehicle_nav_on_map",marker);
                } catch (Exception ex) {
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
    public void launchInAppRatingPopup() {
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
    public String getExtendedPath(String json) throws JSONException {
        ArrayList<LatLng> path = new ArrayList<>();
        ArrayList<LatLng> extendedPath = new ArrayList<>();
        JSONObject jsonObject = new JSONObject(json);
        JSONArray coordinates = jsonObject.getJSONArray("points");
        if (coordinates.length() <= 1) return json;
        SharedPreferences sharedPref = context.getSharedPreferences(
                activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        int pointsFactor = Integer.parseInt(sharedPref.getString("POINTS_FACTOR", "3"));

        for (int i = coordinates.length() - 1; i >= 0; i--) {
            JSONObject coordinate = (JSONObject) coordinates.get(i);
            double lng = coordinate.getDouble("lng");
            double lat = coordinate.getDouble("lat");
            LatLng tempPoints = new LatLng(lat, lng);
            path.add(tempPoints);
        }
        for (int i = 0, j = 1; i < path.size() - 1 && j <= path.size() - 1; i++, j++) {
            LatLng point1 = path.get(i);
            LatLng point2 = path.get(j);
            extendedPath.add(point1);
            double distanceBtw = SphericalUtil.computeDistanceBetween(point1, point2);
            int noOfPoints = (int) Math.ceil(distanceBtw / pointsFactor);
            float fraction = 1.0f / (noOfPoints + 1);
            for (int k = 1; k <= noOfPoints; k++) {
                LatLng point = getNewLatLng(fraction * k, point1, point2);
                extendedPath.add(point);
            }
        }
        extendedPath.add(path.get(path.size() - 1));
        JSONObject newPoints = new JSONObject();
        JSONArray remainingPoints = new JSONArray();
        for (int i = extendedPath.size() - 1; i >= 0; i--) {
            LatLng point = extendedPath.get(i);
            JSONObject tempPoints = new JSONObject();
            tempPoints.put("lat", point.latitude);
            tempPoints.put("lng", point.longitude);
            remainingPoints.put(tempPoints);
        }
        newPoints.put("points", remainingPoints);
        return newPoints.toString();
    }

    private LatLng getNewLatLng(float fraction, LatLng a, LatLng b) {
        double lat = (b.latitude - a.latitude) * fraction + a.latitude;
        double lngDelta = b.longitude - a.longitude;
        // Take the shortest path across the 180th meridian.
        if (Math.abs(lngDelta) > 180) {
            lngDelta -= Math.signum(lngDelta) * 360;
        }
        double lng = lngDelta * fraction + a.longitude;
        return new LatLng(lat, lng);
    }

    @JavascriptInterface
    public void drawRoute(final String json, final String style, final String trackColor, final boolean isActual, final String sourceMarker, final String destMarker, final int polylineWidth, String type, String sourceName, String destinationName, final String specialLocation) {
        activity.runOnUiThread(() -> {
            if (googleMap != null) {
                PolylineOptions polylineOptions = new PolylineOptions();
                int color = Color.parseColor(trackColor);
                try {
                    System.out.println("inside_drawRoute_try");
                    JSONObject jsonObject = new JSONObject(json);
                    JSONArray coordinates = jsonObject.getJSONArray("points");
                    if (coordinates.length() <= 1) {
                        JSONObject coordinate = (JSONObject) coordinates.get(0);
                        double lng = coordinate.getDouble("lng");
                        double lat = coordinate.getDouble("lat");
                        upsertMarker("ic_vehicle_nav_on_map",String.valueOf(lat), String.valueOf(lng), 90, 0.5f, 0.5f);
                        animateCamera(lat,lng,20.0f);
                        return;
                    }
                    JSONObject sourceCoordinates = (JSONObject) coordinates.get(0);
                    JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length()-1);
                    double sourceLong = sourceCoordinates.getDouble("lng");
                    double sourceLat = sourceCoordinates.getDouble("lat");
                    double destLat = destCoordinates.getDouble("lat");
                    double destLong = destCoordinates.getDouble("lng");
                    if (sourceLat != 0.0 && sourceLong != 0.0 && destLat != 0.0 && destLong != 0.0) {
                        moveCamera(sourceLat, sourceLong, destLat, destLong, coordinates);
                    }
                    if (isActual) {
                        for (int i = coordinates.length() - 1; i >= 0; i--) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            polylineOptions.add(new LatLng(lat, lng));
                        }
                    } else {
                        LatLng fromPointObj = new LatLng(sourceLat, sourceLong);
                        LatLng toPointObj = new LatLng(destLat, destLong);
                        polylineOptions.add(toPointObj);
                        polylineOptions.add(fromPointObj);
                    }

                    JSONObject specialLocationObject = new JSONObject(specialLocation);
                    String sourceSpecialTagIcon = specialLocationObject.getString("sourceSpecialTagIcon");
                    String destinationSpecialTagIcon = specialLocationObject.getString("destSpecialTagIcon");
                    polylines = setRouteCustomTheme(polylineOptions, color, style, polylineWidth);
                    LatLng sourceLatLng = new LatLng(sourceLat, sourceLong);
                    LatLng destLatLng = new LatLng(destLat, destLong);

                    if (destMarker != null && !destMarker.equals("")) {
                        List<LatLng> points = polylineOptions.getPoints();
                        LatLng dest = points.get(0);
                        MarkerOptions markerObj = new MarkerOptions()
                                .title(destMarker)
                                .position(dest)
                                .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(destinationName, destMarker, destinationSpecialTagIcon.equals("") ? null : destinationSpecialTagIcon)));

                        Marker tempmarker = googleMap.addMarker(markerObj);
                        markers.put(destMarker, tempmarker);

                    }
                    if((sourceMarker != null && !sourceMarker.equals(""))){
                        if (type.equals("DRIVER_LOCATION_UPDATE"))
                        {
                            List<LatLng> points = polylineOptions.getPoints();
                            LatLng source = points.get(points.size() - 1);
                            upsertMarker(sourceMarker,String.valueOf(source.latitude),String.valueOf(source.longitude), 90, 0.5f, 0.5f);
                            Marker currMarker = (Marker) markers.get(sourceMarker);
                            int index = polylines.getPoints().size()-1;
                            float rotation = (float) SphericalUtil.computeHeading(polylines.getPoints().get(index), polylines.getPoints().get(index -1));
                            if (rotation != 0.0) currMarker.setRotation(rotation);
                            currMarker.setAnchor(0.5f,0.5f);
                            markers.put(sourceMarker,currMarker);
                        } else {
                            List<LatLng> points = polylineOptions.getPoints();
                            LatLng source = points.get(points.size() - 1);
                            MarkerOptions markerObj = new MarkerOptions()
                                    .title(sourceMarker)
                                    .position(source)
                                    .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(sourceName,sourceMarker, sourceSpecialTagIcon.equals("") ? null : sourceSpecialTagIcon)));
                            Marker tempmarker = googleMap.addMarker(markerObj);
                            markers.put(sourceMarker, tempmarker);
                        }
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @JavascriptInterface
    public void updateRouteMarker(final String json) {
        if (activity != null) {
            activity.runOnUiThread(() -> {
                try {
                    if (googleMap != null) {
                        JSONObject jsonObject = new JSONObject(json);
                        JSONObject coor = (JSONObject) jsonObject.getJSONObject("locations");
                        JSONArray coordinates = coor.getJSONArray("points");
                        String sourceName = jsonObject.getString("sourceName");
                        String destinationName = jsonObject.getString("destName");
                        String sourceMarker = jsonObject.getString("sourceIcon");
                        String destinationMarker = jsonObject.getString("destIcon");
                        JSONObject specialLocationObject = jsonObject.getJSONObject("specialLocation");
                        String sourceTag = specialLocationObject.getString("sourceSpecialTagIcon");
                        String destinationTag = specialLocationObject.getString("destSpecialTagIcon");
                        if (coordinates.length() > 0) {
                            JSONObject sourceCoordinates = (JSONObject) coordinates.get(0);
                            JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length() - 1);
                            if (!sourceMarker.equals("") && (!sourceName.equals("") || !sourceTag.equals(""))) {
                                removeMarker(sourceMarker);
                                LatLng sourceLatLng = new LatLng(sourceCoordinates.getDouble("lat"), sourceCoordinates.getDouble("lng"));
                                MarkerOptions markerObj = new MarkerOptions()
                                        .title(sourceMarker)
                                        .position(sourceLatLng)
                                        .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(sourceName, sourceMarker, sourceTag.equals("") ? null : sourceTag)));
                                Marker marker = googleMap.addMarker(markerObj);
                                markers.put(sourceMarker, marker);
                            }
                            if (!destinationMarker.equals("") && (!destinationName.equals("") || !destinationTag.equals(""))) {
                                removeMarker(destinationMarker);
                                LatLng destinationLatLng = new LatLng(destCoordinates.getDouble("lat"), destCoordinates.getDouble("lng"));
                                MarkerOptions markerObj = new MarkerOptions()
                                        .title(destinationMarker)
                                        .position(destinationLatLng)
                                        .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(destinationName, destinationMarker, destinationTag.equals("") ? null : destinationTag)));
                                Marker marker = googleMap.addMarker(markerObj);
                                markers.put(destinationMarker, marker);
                            }
                        }
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            });
        }
    }

    @JavascriptInterface
    public void removeAllPolylines(String str) {
        removeMarker("ic_vehicle_nav_on_map");
        removeMarker("ny_ic_src_marker");
        removeMarker("ny_ic_dest_marker");
        activity.runOnUiThread(() -> {
                    if (polylines != null) {
                        polylines.remove();
                        polylines = null;
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
        activity.runOnUiThread(() -> {
            try {
                if (googleMap != null) {
                    LatLng latLngObj = new LatLng(lat, lng);
                    googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLngObj, zoom));
                }
            } catch (Exception e) {
                e.printStackTrace();

            }
        });
    }

    @JavascriptInterface
    public void moveCamera(final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude) {
        activity.runOnUiThread(() -> {
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

            if (googleMap != null) {
                try {
                    LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                    LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                    LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                    googleMap.setPadding(100, 200, 100, getScreenHeight() / 2);
                    googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                    googleMap.setPadding(0, 0, 0, 0);
                } catch (IllegalArgumentException e) {
                    LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                    LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                    LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                    googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                } catch (Exception e) {
                    System.out.println("In mmove camera in catch exception" + e);
                }
            }
        });
    }

    @JavascriptInterface
    public void moveCamera(final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude, final JSONArray json_coordinates) {
        activity.runOnUiThread(() -> {
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
                source_lat = minimum_latitude - 1.3 * (maximum_latitude - minimum_latitude);
                destination_lat = maximum_latitude + 0.1 * (maximum_latitude - minimum_latitude);
            } else {
                source_lat = maximum_latitude + 0.1 * (maximum_latitude - minimum_latitude);
                destination_lat = minimum_latitude - 1.3 * (maximum_latitude - minimum_latitude);
            }
            if (source_longitude <= destination_longitude) {
                source_lng = minimum_longitude - 0.09 * (maximum_longitude - minimum_longitude);
                destination_lng = maximum_longitude + 0.09 * (maximum_longitude - minimum_longitude);
            } else {
                source_lng = maximum_longitude + 0.09 * (maximum_longitude - minimum_longitude);
                destination_lng = minimum_longitude - 0.09 * (maximum_longitude - minimum_longitude);
            }

            Log.i("coordinates points", String.valueOf(json_coordinates));

            if (googleMap != null) {
                try {
                    LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                    LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                    LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                    if (json_coordinates.length() < 5) {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400));
                    } else {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150));
                    }
                } catch (IllegalArgumentException e) {
                    LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                    LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                    LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                    if (json_coordinates.length() < 5) {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400));
                    } else {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150));
                    }
                } catch (Exception e) {
                    System.out.println("In mmove camera in catch exception" + e);
                }
            }
        });
    }

    @JavascriptInterface
    public void currentPosition(String str) {
        System.out.println("Fetch Current Position");
        showLocationOnMap();
    }

    @JavascriptInterface
    public static void sendMessage(final String message){
        ChatService.sendMessage(message);
    }

    @JavascriptInterface
    public void scrollToEnd(final String id, final boolean isBottom){
       try {
           if (isBottom) {
               ScrollView scrollView = activity.findViewById(Integer.parseInt(id));
               if (scrollView != null) {
                   scrollView.fullScroll(View.FOCUS_DOWN);
               }
           }else {
               HorizontalScrollView horizontalScrollView = activity.findViewById(Integer.parseInt(id));
               if (horizontalScrollView != null) {
                   horizontalScrollView.fullScroll(View.FOCUS_RIGHT);
               }
           }
       } catch(Exception e) {
           Log.e(LOG_TAG,"Error in scroll to Bottom : " + e);
       }
    }

    @JavascriptInterface
    public void storeCallBackMessageUpdated(final String channelId, final String uuid, final String callback) {
        ChatService.storeCallBackMessage = callback;
        setKeysInSharedPrefs("CHAT_CHANNEL_ID", channelId);
        ChatService.chatChannelID = channelId;
        ChatService.chatUserId = uuid;
    }

    public static String storeCallBackOpenChatScreen = null;
    @JavascriptInterface
    public void storeCallBackOpenChatScreen(final String callback){
        storeCallBackOpenChatScreen = callback;
    }

    @JavascriptInterface
    public static void openChatScreen() {
        DuiCallback dynamicUII = MainActivity.getInstance().getJuspayServices().getDuiCallback();
        if (dynamicUII != null && storeCallBackOpenChatScreen != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s');", storeCallBackOpenChatScreen);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    public static void addDynamicView(DuiCallback dynamicUII) {
        ChatService.chatDynamicUI = dynamicUII;
    }

    @JavascriptInterface
    public void startChatListenerService() {
        try {
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String appState = sharedPref.getString("ACTIVITY_STATUS", "null");
            Intent chatListenerService = new Intent(context, ChatService.class);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S && appState.equals("onPause")) {
                AlarmManager manager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
                Intent alarmIntent = new Intent(context, ChatBroadCastReceiver.class);
                PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0, alarmIntent, PendingIntent.FLAG_IMMUTABLE);
                manager.setExact(AlarmManager.RTC_WAKEUP, System.currentTimeMillis(), pendingIntent);
            } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                context.startForegroundService(chatListenerService);
            } else {
                context.startService(chatListenerService);
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Failed to start ChatService : " + e);
        }
    }

    @JavascriptInterface
    public void stopChatListenerService() {
        try {
            Intent chatListenerService = new Intent(activity, ChatService.class);
            Intent overlayService = new Intent(activity, MessageOverlayService.class);
            activity.stopService(chatListenerService);
            activity.stopService(overlayService);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in stopChatListenerService : " + e);
        }
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

//    @JavascriptInterface
//    public void uploadFile (){
//        activity.runOnUiThread(new Runnable() {
//            @Override
//            public void run() {
//                if ((ActivityCompat.checkSelfPermission(activity, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(activity, CAMERA) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(activity, READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)){
//                    Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
//                    String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", new Locale("en","US")).format(new Date());
//                    SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
//                    sharedPref.edit().putString(context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp).apply();
//                    Uri photoFile = FileProvider.getUriForFile(context.getApplicationContext(),context.getApplicationInfo().packageName + ".fileProvider", new File(context.getApplicationContext().getFilesDir(), "IMG_" + timeStamp+".jpg"));
//                    takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
//                    Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
//                    chooseFromFile.setType("image/*");
//                    Intent chooser = Intent.createChooser(takePicture, "Upload Image");
//                    chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[] { chooseFromFile });
//                    startActivityForResult(activity, chooser, IMAGE_CAPTURE_REQ_CODE, null);
//                } else {
//                    ActivityCompat.requestPermissions(activity, new String[]{CAMERA, WRITE_EXTERNAL_STORAGE, READ_EXTERNAL_STORAGE}, IMAGE_PERMISSION_REQ_CODE);
//                }
//            }
//        });
//    }


    @JavascriptInterface
    public void uploadFile(boolean considerCamera) {
        if (!isUploadPopupOpen)
            activity.runOnUiThread(() -> {
                if ((ActivityCompat.checkSelfPermission(activity, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(activity, CAMERA) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(activity, READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)) {
                    Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                    String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                    SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    sharedPref.edit().putString(context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp).apply();
                    Uri photoFile = FileProvider.getUriForFile(context.getApplicationContext(),context.getApplicationInfo().packageName + ".fileProvider", new File(context.getApplicationContext().getFilesDir(), "IMG_" + timeStamp+".jpg"));
                    takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
                    Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
                    chooseFromFile.setType("image/*");
                    Intent chooser = Intent.createChooser(chooseFromFile, "Upload Image");
                    considerCameraOption = considerCamera;
                    if(considerCamera)
                    {
                    chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[]{takePicture});
                    }
                    isUploadPopupOpen = true;
                    startActivityForResult(activity, chooser, IMAGE_CAPTURE_REQ_CODE, null);
                } else {
                    ActivityCompat.requestPermissions(activity, new String[]{CAMERA, WRITE_EXTERNAL_STORAGE, READ_EXTERNAL_STORAGE}, IMAGE_PERMISSION_REQ_CODE);
                }
            });
    }

    @JavascriptInterface
    public void copyToClipboard(String inputText) {
        ClipboardManager clipboard = (ClipboardManager) activity.getSystemService(Context.CLIPBOARD_SERVICE);
        ClipData clip = ClipData.newPlainText("Text", inputText);
        clipboard.setPrimaryClip(clip);
    }

    @JavascriptInterface
    public void launchAppSettings() {
        Intent appSettingsIntent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
        appSettingsIntent.setData(Uri.fromParts("package", getPackageName(), null));
        appSettingsIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(appSettingsIntent);
    }

    @JavascriptInterface
    public void launchDateSettings() {
        try {
            context.startActivity(new Intent(android.provider.Settings.ACTION_DATE_SETTINGS).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK));
        }catch (ActivityNotFoundException e){
            context.startActivity(new Intent(android.provider.Settings.ACTION_SETTINGS).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK));
        }
    }
    
    @JavascriptInterface
    public void adjustViewWithKeyboard(String flag) {
        activity.runOnUiThread(() -> {
            if (flag.equals("true"))
                activity.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_RESIZE);
            else
                activity.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_NOTHING);
        });
    }

    @JavascriptInterface
    public void generatePDF(String str, String format) throws JSONException {
        invoice = str;
        invoiceType = format;
        if (android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.S) {
            if ((ActivityCompat.checkSelfPermission(activity, READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(activity, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)) {
                downloadPDF(str, activity, context);
            } else {
                ActivityCompat.requestPermissions(activity, new String[]{WRITE_EXTERNAL_STORAGE}, 67);
            }
        } else {
            downloadPDF(str, activity, context);
        }
    }

    public static void downloadPDF(String str, Activity activity, Context context) throws JSONException {
        JSONObject state = new JSONObject(str);
        JSONObject data = new JSONObject();
        JSONObject props = new JSONObject();
        data = state.optJSONObject("data");
        props = state.optJSONObject("props");
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String userName = sharedPref.getString("USER_NAME", "__failed");
        JSONObject selectedItem = data.getJSONObject("selectedItem");
        JSONArray fares = selectedItem.getJSONArray("faresList");
        if (invoiceType.equals("OLD")) {
            int pageHeight = 1555;
            int pagewidth = 960;
            Bitmap logo = BitmapFactory.decodeResource(context.getResources(), R.drawable.ic_launcher);
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
                Uri path = FileProvider.getUriForFile(context.getApplicationContext(), context.getApplicationInfo().packageName + ".fileProvider", file);
                Intent pdfOpenintent = new Intent(Intent.ACTION_VIEW);
                pdfOpenintent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_ACTIVITY_CLEAR_TOP);
                pdfOpenintent.setDataAndType(path, "application/pdf");
                PendingIntent pendingIntent = PendingIntent.getActivity(context, 234567, pdfOpenintent, PendingIntent.FLAG_IMMUTABLE);
                NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, "General");
                mBuilder.setLargeIcon(logo);
                mBuilder.setContentTitle("Invoice Downloaded")
                        .setSmallIcon((R.drawable.ic_launcher))
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
            PdfDocument pdfDocument = new PdfDocument();
            PdfDocument.PageInfo invoicePDF = new PdfDocument.PageInfo.Builder(960, 1338, 1).create();
            PdfDocument.Page page = pdfDocument.startPage(invoicePDF);
            View content = getInvoiceLayout(data, selectedItem, fares, userName, context);
            content.measure(page.getCanvas().getWidth(), page.getCanvas().getHeight());
            content.layout(0, 0, page.getCanvas().getWidth(), page.getCanvas().getHeight());
            content.draw(page.getCanvas());
            pdfDocument.finishPage(page);

            String fileNameformat = "";
            String serviceName = context.getResources().getString(R.string.service);
            if (serviceName.equals("jatrisaathi"))
            {
                fileNameformat = "JS_RIDE_";
            } else if (serviceName.equals("nammayatri"))
            {
                fileNameformat = "NY_RIDE_";
            }else{
                fileNameformat = "YATRI_RIDE_";
            }
            fileNameformat = fileNameformat + selectedItem.getString("date") + selectedItem.getString("rideStartTime") + ".pdf";
            String fileName = fileNameformat.replaceAll(":",".");
            try {
                File file = checkAndGetFileName(fileName);
                FileOutputStream fos = new FileOutputStream(file);
                pdfDocument.writeTo(fos);
                Uri path = FileProvider.getUriForFile(context.getApplicationContext(), context.getApplicationInfo().packageName + ".fileProvider", file);
                Intent pdfOpenintent = new Intent(Intent.ACTION_VIEW);
                pdfOpenintent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_ACTIVITY_CLEAR_TOP);
                pdfOpenintent.setDataAndType(path, "application/pdf");
                PendingIntent pendingIntent = PendingIntent.getActivity(context, 234567, pdfOpenintent, PendingIntent.FLAG_IMMUTABLE);
                NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, "General");
                mBuilder.setContentTitle("Invoice Downloaded")
                        .setSmallIcon((R.drawable.ic_launcher))
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

    private static View getInvoiceLayout(JSONObject data, JSONObject selectedRide, JSONArray fares, String user, Context context) throws JSONException {
        View invoiceLayout = LayoutInflater.from(context).inflate(R.layout.invoice_template, null, false);
        TextView textView = invoiceLayout.findViewById(R.id.rideDate);
        textView.setText(selectedRide.getString("date"));
        textView = invoiceLayout.findViewById(R.id.userName);
        textView.setText(user.trim());
        textView = invoiceLayout.findViewById(R.id.paymentDetail);
        textView.setText(selectedRide.getString("totalAmount"));

        LinearLayout fareBreakupElements = (LinearLayout) invoiceLayout.findViewById(R.id.fareBreakupElements);
        fareBreakupElements.setOrientation(LinearLayout.VERTICAL);

        try {
            for (int i = 0; i < fares.length(); i++) {

                LinearLayout.LayoutParams linearParams = new LinearLayout.LayoutParams(
                        LinearLayout.LayoutParams.MATCH_PARENT,
                        LinearLayout.LayoutParams.WRAP_CONTENT);
                LinearLayout linearLayout = new LinearLayout(context);
                linearLayout.setLayoutParams(linearParams);

                LinearLayout.LayoutParams linearParamsChild = new LinearLayout.LayoutParams(
                        LinearLayout.LayoutParams.MATCH_PARENT,
                        LinearLayout.LayoutParams.WRAP_CONTENT);
                LinearLayout linearLayoutChild = new LinearLayout(context);
                linearLayoutChild.setLayoutParams(linearParamsChild);
                linearParamsChild.weight = 1.0f;

                JSONObject fare = fares.getJSONObject(i);
                String value = fare.getString("price");
                String fareTypes = fare.getString("title");
                TextView textViewText = new TextView(context);
                textViewText.setTextSize(6);
                textViewText.setTextColor(Color.parseColor("#6D7280"));
                textViewText.setPadding(0, 0, 0, 10);
                Typeface typeface = Typeface.createFromAsset(context.getAssets(), "fonts/PlusJakartaSans-Regular.ttf");
                textViewText.setTypeface(typeface);
                textViewText.setText(fareTypes);
                linearLayout.addView(textViewText);
                linearLayout.addView(linearLayoutChild);

                TextView textViewPrice = new TextView(context);
                textViewPrice.setTextSize(6);
                textViewPrice.setPadding(0, 0, 0, 10);
                Typeface font = Typeface.createFromAsset(context.getAssets(), "fonts/PlusJakartaSans-Regular.ttf");
                textViewPrice.setTypeface(font);
                textViewPrice.setTextColor(Color.parseColor("#454545"));
                textViewPrice.setText("₹ " + value);
                linearLayout.addView(textViewPrice);

                fareBreakupElements.addView(linearLayout);

            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
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
        textView = invoiceLayout.findViewById(R.id.referenceText);
        textView.setText(selectedRide.getString("referenceString"));
        return invoiceLayout;
    }

    // Added for Backward Compatibility
    private static JSONObject getFaresJson(JSONObject rideData) throws JSONException {
        JSONObject fares = new JSONObject();
        float baseFare = Float.parseFloat(rideData.getString("totalAmount").substring(2)) - 10;
        fares.put("baseFare", "₹ " + baseFare);
        fares.put("pickupCharges", "₹ 10");
        fares.put("nominalFare", "₹ 0");
        fares.put("waitingCharges", "₹ 0");
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
    public void setYoutubePlayer(String rawJson, final String playerId, String videoStatus) {
        videoDuration = 0;
        activity.runOnUiThread(() -> {
            try {
                if (videoStatus.equals("PAUSE")) {
                    pauseYoutubeVideo();
                } else {
                    JSONObject json = new JSONObject(rawJson);
                    if (youTubePlayerView != null)
                        youTubePlayerView.release();
                    boolean showMenuButton = json.getBoolean("showMenuButton");
                    boolean showDuration = json.getBoolean("showDuration");
                    boolean setVideoTitle = json.getBoolean("setVideoTitle");
                    boolean showSeekBar = json.getBoolean("showSeekBar");
                    String videoTitle = json.getString("videoTitle");
                    String videoId = json.getString("videoId");
                    String videoType = "VIDEO";
                    if (json.has("videoType")) {
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
                                if (setVideoTitle) {
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
                        public void onCurrentSecond(@NonNull YouTubePlayer youTubePlayer, float second) {
                            videoDuration = second;
                        }
                    };

                    String finalVideoType = videoType;
                    youTubePlayerView.addFullScreenListener(new YouTubePlayerFullScreenListener() {
                        @Override
                        public void onYouTubePlayerExitFullScreen() {
                        }

                        @Override
                        public void onYouTubePlayerEnterFullScreen() {
                            Intent newIntent = new Intent(MainActivity.getInstance().getApplicationContext(), YoutubeVideoView.class);
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
        });
    }

    @JavascriptInterface
    public void pauseYoutubeVideo() {
        if (youTubePlayerView != null) {
            youtubePlayer.pause();
        }
    }

    private String getAPIResponse(String url) {
        if (url.equals("") || url == null) return "";
        StringBuilder result = new StringBuilder();
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        try {
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            connection.setRequestMethod("GET");
            connection.setRequestProperty("token", getKeyInNativeSharedPrefKeys("REGISTERATION_TOKEN"));
            connection.setRequestProperty("x-device",deviceDetails);
            connection.connect();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                return "";
            } else {
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

    public static void contactsStoreCall(DuiCallback dynamicUII, String contacts) {
        if (dynamicUII != null && storeCallBContact != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCallBContact, contacts);
            dynamicUII.addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void performHapticFeedback() {
        Vibrator vibrator = (Vibrator) context.getSystemService(Context.VIBRATOR_SERVICE);

        if (vibrator != null && vibrator.hasVibrator() && Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            VibrationEffect effect = VibrationEffect.createOneShot(50, VibrationEffect.DEFAULT_AMPLITUDE);
            vibrator.vibrate(effect);
        }
    }

    public static File checkAndGetFileName(String fileName) {
        File file = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS), fileName + ".pdf");
        int i = 1;
        while (file.exists()) {
            String updatedFile = fileName + "_(" + i + ")" + ".pdf";
            file = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS), updatedFile);
            i++;

        }
        return file;
    }

}