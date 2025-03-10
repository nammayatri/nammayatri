/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.common;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.POST_NOTIFICATIONS;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static android.app.Activity.RESULT_OK;
import static android.content.Context.MODE_PRIVATE;
import static android.graphics.Color.parseColor;
import static in.juspay.mobility.common.utils.Utils.captureImage;
import static in.juspay.mobility.common.utils.Utils.drawableToBitmap;
import static in.juspay.mobility.common.utils.Utils.encodeImageToBase64;
import static in.juspay.mobility.common.utils.Utils.getCircleOptionsFromJSON;

import android.Manifest;
import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ObjectAnimator;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.ActivityManager;
import android.app.AlertDialog;
import android.app.DatePickerDialog;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.TimePickerDialog;
import android.content.ActivityNotFoundException;
import android.content.BroadcastReceiver;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.Resources;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.PorterDuff;
import android.graphics.Rect;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.GradientDrawable;
import android.hardware.Sensor;
import android.hardware.SensorManager;
import android.icu.util.Calendar;
import android.location.Address;
import android.location.Geocoder;
import android.location.Location;
import android.location.LocationManager;
import android.media.AudioAttributes;
import android.media.RingtoneManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.Looper;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.provider.ContactsContract;
import android.provider.MediaStore;
import android.speech.SpeechRecognizer;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.view.WindowManager;
import android.view.animation.LinearInterpolator;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.webkit.ConsoleMessage;
import android.webkit.JavascriptInterface;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.DatePicker;
import android.widget.EditText;
import android.widget.HorizontalScrollView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.NumberPicker;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

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
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.airbnb.lottie.LottieAnimationView;
import com.google.android.gms.common.api.ApiException;
import com.google.android.gms.common.api.ResolvableApiException;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.LocationSettingsRequest;
import com.google.android.gms.location.LocationSettingsResponse;
import com.google.android.gms.location.LocationSettingsStatusCodes;
import com.google.android.gms.location.Priority;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.ButtCap;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Dot;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.GroundOverlay;
import com.google.android.gms.maps.model.GroundOverlayOptions;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.MapStyleOptions;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PatternItem;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.android.gms.tasks.CancellationTokenSource;
import com.google.android.gms.tasks.Task;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;
import com.google.maps.android.data.Feature;
import com.google.maps.android.data.Layer;
import com.google.maps.android.data.geojson.GeoJsonFeature;
import com.google.maps.android.data.geojson.GeoJsonLayer;
import com.google.maps.android.data.geojson.GeoJsonPolygonStyle;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Method;
import java.net.HttpURLConnection;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JsCallback;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.mobility.common.cropImage.CropImage;
import in.juspay.mobility.common.services.MobilityCallAPI;
import in.juspay.mobility.common.utils.CipherUtil;
import in.juspay.mobility.common.utils.Utils;

public class MobilityCommonBridge extends HyperBridge {

    public static final int REQUEST_CONTACTS = 7;
    public static final int LOCATION_PERMISSION_REQ_CODE = 1;
    public static final int BACKGROUND_LOCATION_REQ_CODE = 190;
    public static final int REQUEST_CALL = 8;
    protected static final int STORAGE_PERMISSION = 67;
    //Constants
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;
    private static final String LOCATE_ON_MAP = "LocateOnMap";
    private static final String HOTSPOTS = "hotspots";
    protected static final String CURRENT_LOCATION = "ny_ic_customer_current_location";
    protected static final String LOCATION_IS_FAR = "LocationIsFar";
    protected static Boolean editLocation = false;
    private static final String CURRENT_LOCATION_LATLON = "Current Location";
    protected static final int LOCATION_RESOLUTION_REQUEST_CODE = 21345;
    private static final int DATEPICKER_SPINNER_COUNT = 3;
    private static final int REQUEST_CODE_NOTIFICATION_PERMISSION = 10;
    private static final int PICK_CONTACT_REQUEST_CODE = 191;
    private static final int PICK_CONTACT_PERMISSION_REQUEST_CODE = 1993;
    //Maps
    protected HashMap<String, Marker> markers = new HashMap<>();
    protected GoogleMap googleMap;
    protected HashMap<String, Marker> zoneMarkers = new HashMap<>();
    protected static GeoJsonLayer layer;
    protected String regToken, baseUrl;
    protected String zoneName = "";
    protected float zoom = 17.0f;
    // CallBacks
    protected String storeLocateOnMapCallBack = null;
    protected String storeHotspotMapCallback = null;
    protected String storeCircleOnClickCallback = null;
    protected String storeEditLocationCallBack = null;
    protected String storeDashboardCallBack = null;
    private String storeImageUploadCallBack = null;
    private String storeUploadMultiPartCallBack = null;
    private String storePickContactCallBack = null;
    protected Marker userPositionMarker;
    private final UICallBacks callBack;
    private final FusedLocationProviderClient client;

    protected Hashtable<String, Hashtable<String, PolylineDataPoints>> polylinesByMapInstance = new Hashtable<>();
    protected HashMap<String, HashMap<String, Marker>> markersElement = new HashMap<>();
    protected HashMap<String, GoogleMap> googleMapInstance = new HashMap<>();
    //Location
    protected double lastLatitudeValue;
    protected double lastLongitudeValue;
    protected Location lastKnownLocation;
    //LOG_TAGS
    protected String MAPS = "MAPS";
    protected String LOCATION = "LOCATION";
    protected String UTILS = "UTILS";
    protected String OTHERS = "OTHERS";
    protected String DTUTILS = "DTUTILS";
    protected String OVERRIDE = "OVERRIDE";
    protected String CALLBACK = "CALLBACK";
    protected String NOTIFICATION = "NOTIFICATION";
    protected String LOG_TAG = MobilityCommonBridge.class.getSimpleName();
    private static String storeContactsCallBack = null;
    protected String phoneNumber;
    protected String invoice;
    CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
    private int lastFocusedEditView;
    private int lastFocusedEditText;
    private SpeechRecognizer speechRecognizer;

    private Queue<String> callbackQueue = new LinkedList<>();
    // Others
    private LottieAnimationView animationView;
    protected Method[] methods = null;
    private SensorManager sensorManager;
    private Sensor accelerometer;
    private ShakeDetector shakeDetector;

    protected class PolylineDataPoints {
        public Polyline polyline = null;
        public Polyline overlayPolylines = null;

        public void setPolyline(Polyline polyline) {
            this.polyline = polyline;
        }

        public void setOverlayPolylines(Polyline polyline) {
            this.overlayPolylines = polyline;
        }
    }

    protected boolean isAnimationNeeded = false;
    private PaymentPage paymentPage;

    protected Receivers receivers;
    private Integer debounceAnimateCameraCounter;
    private ViewTreeObserver.OnGlobalLayoutListener onGlobalLayoutListener;
    protected int animationDuration = 400;
    protected JSONObject locateOnMapConfig = null;
    protected String downloadLayout;
    protected int labelTextSize = 30;
    protected String labelId = "";
    protected float editPickupThreshold = 100;

    protected HashMap<String, CircleRippleEffect> circleRipples = new HashMap<>();
    protected HashMap<String, GroundOverlay> groundOverlays = new HashMap<>();


    private MediaPlayer mediaPlayer = null;
    private SpeechRecognition speechRecognition = null;
    private android.media.MediaPlayer audioPlayer;
    protected AppType app;
    protected MapUpdate mapUpdate = new MapUpdate();
    private MapRemoteConfig mapRemoteConfig;
    protected LocateOnMapManager locateOnMapManager = null;
    private static Hashtable<String, Hashtable<String, PolyLineAnimationTimers>> polylineAnimationTimers = new Hashtable<>();
    protected BridgeComponents bridgeComponents;

    public MobilityCommonBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        this.bridgeComponents = bridgeComponents;
        client = LocationServices.getFusedLocationProviderClient(bridgeComponents.getContext());
        if (isLocationPermissionEnabled()) {
            client.getCurrentLocation(Priority.PRIORITY_HIGH_ACCURACY, cancellationTokenSource.getToken())
                    .addOnSuccessListener(location -> {
                        JSONObject currLoc = new JSONObject();
                        try {
                            currLoc.put("lat", location.getLatitude());
                            currLoc.put("lon", location.getLongitude());
                            String command = String.format("window[\"onEvent'\"]('%s','%s')", "onLocationFetch", currLoc.toString());
                            callbackQueue.add(command);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    });
        }

        receivers = new Receivers(bridgeComponents);
        receivers.initReceiver();
        callBack = this::callImageUploadCallBack;
        Utils.registerCallback(callBack);
        fetchAndUpdateLastKnownLocation();
    }

    protected enum AppType {
        CONSUMER, PROVIDER
    }

    public enum AnimationType {
        FADE_IN, FADE_OUT, NONE;
    }

    public enum MapMode {
        NORMAL, SPECIAL_ZONE, HOTSPOT
    }

    public enum Theme {
        DARK, LIGHT
    }

    public static class MarkerConfig {
        String markerId = "";
        boolean showPointer = false;
        String pointerIcon = "";
        String primaryText = "";
        String secondaryText = "";
        MarkerImageConfig labelImage = new MarkerImageConfig("", 28, 28);
        MarkerImageConfig labelActionImage = new MarkerImageConfig("", 28, 28);
        MarkerActionImageConfig markerActionImage = new MarkerActionImageConfig("", 28, 28, "HORIZONTAL", "", new JSONObject(), new JSONObject(), new JSONObject());
        ;
        Theme theme = Theme.LIGHT;
        String markerCallback = "";
        String shortTitle = "";
        int labelMaxWidth = 300;
        int labelMaxLines = 1;
        int labelTextSize = 11;
        float rotation = 0;
        int markerIconSize = 160;
        AnimationType animationType = AnimationType.NONE;
        int animationDuration = 0;

        public void locationName(String primaryText, String secondaryText) {
            this.primaryText = primaryText;
            this.secondaryText = secondaryText;
        }

        public void setShortTitle(String shortTitle) {
            this.shortTitle = shortTitle;
        }

        public void setLabelMaxWidth(int labelMaxWidth) {
            this.labelMaxWidth = labelMaxWidth;
        }

        ;

        public void setLabelMaxLines(int labelMaxLines) {
            this.labelMaxLines = labelMaxLines;
        }

        ;

        public void setLabelTextSize(int labelTextSize) {
            if (labelTextSize != 0) {
                this.labelTextSize = labelTextSize;
            }
        }

        public void locationName(String primaryText) {
            this.primaryText = primaryText;
        }

        public void setPointer(String pointerIcon) {
            if (pointerIcon.equals(""))
                this.showPointer = false;
            this.pointerIcon = pointerIcon;
        }

        public void setLabelImage(String labelImage) {
            this.labelImage.image = labelImage;
        }

        public void setLabelActionImage(String labelActionImage) {
            this.labelActionImage.image = labelActionImage;
        }

        public void setRotation(float rotation) {
            this.rotation = rotation;
        }

        public void setMarkerId(String markerId) {
            this.markerId = markerId;
        }

        public void setMarkerAnimation(AnimationType animationType, int animationDuration) {
            this.animationType = animationType;
            this.animationDuration = animationDuration;
        }

        public void setLabelImageConfig(JSONObject labelImage) {
            if (labelImage != null) {
                this.labelImage.image = labelImage.optString("image", "");
                this.labelImage.height = labelImage.optInt("height", 28);
                this.labelImage.width = labelImage.optInt("width", 28);
            }
        }

        public void setLabelActionImageConfig(JSONObject actionImage) {
            if (actionImage != null) {
                this.labelActionImage.image = actionImage.optString("image", "");
                this.labelActionImage.height = actionImage.optInt("height", 28);
                this.labelActionImage.width = actionImage.optInt("width", 28);
            }
        }

        public void setTheme(String theme) {
            try {
                this.theme = Theme.valueOf(theme);
            } catch (Exception e) {
                this.theme = Theme.LIGHT;
            }
        }

        public void setMarkerCallback(String callback) {
            this.markerCallback = callback;
        }

        public void setMarkerIconSize(int markerSize) {
            this.markerIconSize = markerSize;
        }

        public void setMarkerActionImageConfig(JSONObject markerActionImage) {
            this.markerActionImage.image = markerActionImage.optString("image", "");
            this.markerActionImage.height = markerActionImage.optInt("height", 28);
            this.markerActionImage.width = markerActionImage.optInt("width", 28);
            this.markerActionImage.orientation = markerActionImage.optString("orientation", "VERTICAL");
            this.markerActionImage.background = markerActionImage.optString("background", "");
            this.markerActionImage.layoutMargin = getMarkerEdgeInsets(markerActionImage.optJSONObject("layoutMargin"));
            this.markerActionImage.padding = getMarkerEdgeInsets(markerActionImage.optJSONObject("padding"));
            this.markerActionImage.layoutPadding = getMarkerEdgeInsets(markerActionImage.optJSONObject("layoutPadding"));
        }

        public void setMarkerActionImage(String markerActionImage) {
            this.markerActionImage.image = markerActionImage;
        }
    }

    public static class MarkerImageConfig {
        String image;
        int height;
        int width;

        MarkerImageConfig(String image, int height, int width) {
            this.image = image;
            this.height = height;
            this.width = width;
        }
    }

    private static MarkerEdgeInsets getMarkerEdgeInsets(JSONObject config) {
        int left = config.optInt("left", -1);
        int top = config.optInt("top", -1);
        int right = config.optInt("right", -1);
        int bottom = config.optInt("bottom", -1);
        return new MarkerEdgeInsets(left, top, bottom, right);
    }

    public static class MarkerEdgeInsets {
        int left;
        int right;
        int bottom;
        int top;

        MarkerEdgeInsets(int left, int top, int right, int bottom) {
            this.left = left;
            this.right = right;
            this.top = top;
            this.bottom = bottom;
        }
    }

    public static class MarkerActionImageConfig {
        String image;
        int height;
        int width;
        String orientation;
        String background;
        MarkerEdgeInsets layoutMargin;
        MarkerEdgeInsets layoutPadding;
        MarkerEdgeInsets padding;

        MarkerActionImageConfig(String image, int height, int width, String orientation, String background, JSONObject layoutMargin, JSONObject layoutPadding, JSONObject padding) {
            this.image = image;
            this.height = height;
            this.width = width;
            this.orientation = orientation;
            this.background = background;
            this.layoutMargin = getMarkerEdgeInsets(layoutMargin);
            this.layoutPadding = getMarkerEdgeInsets(layoutPadding);
            this.padding = getMarkerEdgeInsets(padding);
        }
    }

    public Polyline getPolyLine(Boolean isOverlayPolyLine, PolylineDataPoints polylineData) {
        if (polylineData != null) {
            return isOverlayPolyLine ? polylineData.overlayPolylines : polylineData.polyline;
        } else {
            return null;
        }
    }

    public PolylineDataPoints getPolyLineDataByMapInstance(String mapKey, String polyLineKey) {
        Hashtable<String, PolylineDataPoints> polylines = polylinesByMapInstance.get(mapKey);
        if (polylines != null) {
            return polylines.get(polyLineKey);
        } else {
            return null;
        }
    }

    public void setPolyLineDataByMapInstance(String mapKey, String polyLineKey, PolylineDataPoints polyLineData) {
        Hashtable<String, PolylineDataPoints> polylines = polylinesByMapInstance.get(mapKey);
        if (polylines != null) {
            polylines.put(polyLineKey, polyLineData);
        } else {
            Hashtable<String, PolylineDataPoints> polylinesNew = new Hashtable<>();
            polylinesNew.put(polyLineKey, polyLineData);
            polylinesByMapInstance.put(mapKey, polylinesNew);
        }
    }

    public PolyLineAnimationTimers getPolyLineTimer(String mapKey, String polyLineKey) {
        Hashtable<String, PolyLineAnimationTimers> polyLineEntries = polylineAnimationTimers != null ? polylineAnimationTimers.get(mapKey) : null;
        PolyLineAnimationTimers polyLineAnimationTimer = polyLineEntries != null ? polyLineEntries.get(polyLineKey) : new PolyLineAnimationTimers();
        return polyLineAnimationTimer;
    }

    public void setPolylineAnimationTimers(String mapKey, String polyLineKey, PolyLineAnimationTimers polyLineTimer) {
        if (polylineAnimationTimers != null) {
            Hashtable<String, PolyLineAnimationTimers> polyLineEntries = polylineAnimationTimers.get(mapKey);
            if (polyLineEntries == null) {
                polyLineEntries = new Hashtable<>();
            }
            polyLineEntries.put(polyLineKey, polyLineTimer);
            polylineAnimationTimers.put(mapKey, polyLineEntries);
        }
    }


    protected class PolyLineAnimationTimers {
        Timer polyLineAnimationTimer = null;
        TimerTask polyAnimationTimerTask = null;

        public void intializePolyLineAnimationTimers(Timer polyLineAnimationTimer, TimerTask polyAnimationTimerTask) {
            this.polyLineAnimationTimer = polyLineAnimationTimer;
            this.polyAnimationTimerTask = polyAnimationTimerTask;
        }
    }

    public static class LocateOnMapManager {
        GeoJsonFeature focusedGeoJsonFeature = null;
        HashMap<String, GeoJsonFeature> gatesSpecialZone = new HashMap<>();
        boolean showZoneLabel = false;
        String locationName = null;
        String markerCallback = "";
        boolean enableMapClickListener = false;
        ArrayList<String> markerCallbackTags = new ArrayList<>();

        public void setFocusedGeoJsonFeature(GeoJsonFeature feature) {
            this.focusedGeoJsonFeature = feature;
        }

        public GeoJsonFeature getGateFeature(String gateName) {
            if (!this.gatesSpecialZone.isEmpty())
                return this.gatesSpecialZone.get(gateName);
            else {
                if (layer != null) {
                    Iterable<GeoJsonFeature> features = layer.getFeatures();
                    for (GeoJsonFeature geoJsonFeature : features) {
                        if (!geoJsonFeature.getProperty("name").equals("")) {
                            this.gatesSpecialZone.put(geoJsonFeature.getProperty("name"), geoJsonFeature);
                        }
                    }
                    return this.gatesSpecialZone.get(gateName);
                }
                return null;
            }
        }

        public void setShowZoneLabel(boolean showZoneLabel) {
            this.showZoneLabel = showZoneLabel;
        }

        public void setLocationName(String locationName) {
            this.locationName = locationName;
        }

        public void setMarkerCallback(String markerCallback, JSONArray callbackTags) {
            try {
                this.markerCallback = markerCallback;
                this.markerCallbackTags = new ArrayList<>();
                for (int i = 0; i < callbackTags.length(); i++) {
                    this.markerCallbackTags.add((String) callbackTags.get(i));
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        public void setEnableMapClickListener(boolean enable) {
            this.enableMapClickListener = enable;
        }
    }

    public MapRemoteConfig getMapRemoteConfig() {
        try {
            if (this.mapRemoteConfig == null) {
                mapRemoteConfig = new MapRemoteConfig();
                String mapConfig = getKeyInNativeSharedPrefKeys("MAP_REMOTE_CONFIG");
                mapRemoteConfig.fromJson(mapConfig);
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in getMapRemoteConfig", e);
        }
        return mapRemoteConfig;
    }

    private void fetchAndUpdateLastKnownLocation() {
        String lat = getKeyInNativeSharedPrefKeys("LAST_KNOWN_LAT");
        String lon = getKeyInNativeSharedPrefKeys("LAST_KNOWN_LON");
        lastLatitudeValue = !lat.equals("__failed") ? Double.parseDouble(lat) : lastLatitudeValue;
        lastLongitudeValue = !lon.equals("__failed") ? Double.parseDouble(lon) : lastLongitudeValue;
    }

    public static boolean isClassAvailable(String className) {
        try {
            Class.forName(className);
            return true;
        } catch (Exception ignored) {
        }
        return false;
    }

    @JavascriptInterface
    public void emptyCallbackQueue() {
        while (!callbackQueue.isEmpty()) {
            if (bridgeComponents.getJsCallback() != null) {
                bridgeComponents.getJsCallback().addJsToWebView(callbackQueue.poll());
            }
        }
    }

    @Override
    public void reset() {
        if (receivers != null) {
            receivers.deRegister();
            receivers = null;
        }
        polylinesByMapInstance = new Hashtable<>();
        googleMap = null;
        markers = new HashMap<>();
        zoneMarkers = new HashMap<>();
        layer = null;
        if (mediaPlayer != null) mediaPlayer.audioRecorder = null;
        clearAudioPlayer();
        if (onGlobalLayoutListener != null && bridgeComponents.getActivity() != null) {
            View parentView = bridgeComponents.getActivity().findViewById(android.R.id.content);
            if (parentView != null) {
                ViewTreeObserver observer = parentView.getRootView().getViewTreeObserver();
                if (observer != null && observer.isAlive()) {
                    observer.removeOnGlobalLayoutListener(onGlobalLayoutListener);
                }
            }
        }

        terminatePP();

        // CallBacks
        storeLocateOnMapCallBack = null;
        storeEditLocationCallBack = null;
        storeDashboardCallBack = null;
        userPositionMarker = null;
        storeImageUploadCallBack = null;
        if (mediaPlayer != null) mediaPlayer.audioPlayers = new ArrayList<>();
        Utils.deRegisterCallback(callBack);
    }

    // region Store and Trigger CallBack
    @JavascriptInterface
    public void storeCallBackInternetAction(String callback) {
        receivers.storeInternetActionCallBack = callback;
    }

    @JavascriptInterface
    public void storeCallBackDriverLocationPermission(String callback) {
        receivers.storeLocationCallBack = callback;
    }

    @JavascriptInterface
    public void storeCallBackLocateOnMap(String callback) {
        storeLocateOnMapCallBack = callback;
    }

    @JavascriptInterface
    public void storeCallbackHotspotMap(String callback) {
        storeHotspotMapCallback = callback;
    }

    @JavascriptInterface
    public void storeCallbackCircleOnClick(String callback) {
        storeCircleOnClickCallback = callback;
    }

    @JavascriptInterface
    public void storeCallBackImageUpload(String callback) {
        storeImageUploadCallBack = callback;
    }

    public void callImageUploadCallBack(String stringImage, String imageName, String imagePath) {
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                storeImageUploadCallBack, stringImage, imageName, imagePath);
        bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }

    @JavascriptInterface
    public void storeCallBackUploadMultiPartData(String callback) {
        storeUploadMultiPartCallBack = callback;
    }

    public void callUploadMultiPartCallBack(String fileType, String fileId) {
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                storeUploadMultiPartCallBack, fileType, fileId);
        bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }
    // endregion

    // region Location
    @JavascriptInterface
    public void currentPosition(final String zoomType) {
        Log.i(LOCATION, "Fetching Current Position");
        showLocationOnMap(zoomType);
    }

    @JavascriptInterface
    public void requestBackgroundLocation() {
        Activity activity = bridgeComponents.getActivity();
        if (activity != null && Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{Manifest.permission.ACCESS_BACKGROUND_LOCATION}, BACKGROUND_LOCATION_REQ_CODE);
        }
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
            if (bridgeComponents.getActivity() != null) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{ACCESS_FINE_LOCATION}, LOCATION_PERMISSION_REQ_CODE);
            }
        } catch (Exception e) {
            Log.e(LOCATION, "Exception in request permission", e);
        }
    }

    @JavascriptInterface
    public void requestNotificationPermission() {
        if (bridgeComponents.getActivity() != null && Build.VERSION.SDK_INT >= 33) {
            if (ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{POST_NOTIFICATIONS}, REQUEST_CODE_NOTIFICATION_PERMISSION);
            }
        }
    }

    @JavascriptInterface
    public boolean isLocationEnabled() {
        LocationManager locationManager = (LocationManager) bridgeComponents.getContext().getSystemService(Context.LOCATION_SERVICE);
        return locationManager != null && LocationManagerCompat.isLocationEnabled(locationManager);
    }

    @JavascriptInterface
    public String getLocationPermissionStatus() {
        if (bridgeComponents.getActivity() != null && ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_DENIED) {
            if (ActivityCompat.shouldShowRequestPermissionRationale(bridgeComponents.getActivity(), ACCESS_FINE_LOCATION)) {
                return "DISABLED";
            } else {
                return "DENIED";
            }
        } else {
            return "ENABLED";
        }
    }

    @JavascriptInterface
    public boolean isLocationPermissionEnabled() {
        return (ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.ACCESS_COARSE_LOCATION) == PackageManager.PERMISSION_GRANTED);
    }

    @JavascriptInterface
    public boolean isNotificationPermissionEnabled() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            return (ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED);
        } else {
            return true;
        }
    }

    @JavascriptInterface
    public void checkAndAskNotificationPermission() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU && !(ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED)) {
            try {
                if (bridgeComponents.getActivity() != null) {
                    ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{POST_NOTIFICATIONS}, REQUEST_CODE_NOTIFICATION_PERMISSION);
                }
            } catch (Exception e) {
                Log.e(NOTIFICATION, "Exception in request notification permission", e);
            }
        }
    }

    private void showLocationOnMap(final String zoomType) {
        ExecutorManager.runOnMainThread(() -> {
            if (!isLocationPermissionEnabled()) return;
            updateLastKnownLocation(null, true, zoomType, true);
        });
    }

    @JavascriptInterface
    public String getCurrentLatLong() throws JSONException { // TODO:: TO BE DEPRECATED AS FUNCTION IS NOT IN USE
        JSONObject location = new JSONObject();
        location.put("lat", lastLatitudeValue);
        location.put("lng", lastLongitudeValue);
        if (lastKnownLocation != null)
            location.put("ts", Utils.getUTCTimeStampFromMills(lastKnownLocation.getTime()));
        return location.toString();
    }

    @SuppressLint("MissingPermission")
    protected void updateLastKnownLocation(String callback, boolean animate, final String zoomType, boolean shouldFallBack) {
        if (!isLocationPermissionEnabled()) return;
        client.getCurrentLocation(Priority.PRIORITY_HIGH_ACCURACY, cancellationTokenSource.getToken())
                .addOnSuccessListener(location -> {
                    if (location != null) {
                        Double lat = location.getLatitude();
                        Double lng = location.getLongitude();
                        String ts = Utils.getUTCTimeStampFromMills(location.getTime());
                        lastLatitudeValue = lat;
                        lastLongitudeValue = lng;
                        lastKnownLocation = location;
                        setKeysInSharedPrefs("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                        setKeysInSharedPrefs("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                        setKeysInSharedPrefs("LAST_KNOWN_LOCATION_TS", ts);
                        if (callback != null) {
                            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                                    callback, lat, lng, ts);
                            bridgeComponents.getJsCallback().addJsToWebView(javascript);
                        }
                        if (animate && googleMap != null) {
                            LatLng latLng = new LatLng(lat, lng);
                            if (userPositionMarker == null) {
                                String marker = "";
                                if (!editLocation) {
                                    marker = CURRENT_LOCATION;
                                    upsertMarker(marker, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f); //TODO this function will be removed
                                }
                            } else {
                                if (storeLocateOnMapCallBack == null)
                                    userPositionMarker.setVisible(true);
                                userPositionMarker.setPosition(latLng);
                            }
                            try {
                                animateCamera(lat, lng, zoom, zoomType);
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        }
                    } else if (shouldFallBack) {
                        getLastKnownLocationFromClientFallback(callback, animate);
                    } else {
                        sendDefaultLocationCallback(callback);
                    }
                })
                .addOnFailureListener(e -> {
                    Log.e(LOCATION, "Current position not known");
                    if (shouldFallBack) {
                        getLastKnownLocationFromClientFallback(callback, animate);
                    } else {
                        sendDefaultLocationCallback(callback);
                    }
                });

    }

    private void sendDefaultLocationCallback(@Nullable String callback) {
        if (callback != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                    callback, "0.0", "0.0", Utils.getUTCTimeStampFromMills(System.currentTimeMillis()));
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }

    @SuppressLint("MissingPermission")
    private void getLastKnownLocationFromClientFallback(String callback, boolean animate) {
        if (!isLocationPermissionEnabled()) return;
        if (bridgeComponents.getActivity() != null) {
            if (client != null)
                client.getLastLocation()
                        .addOnSuccessListener(bridgeComponents.getActivity(), location -> {
                            if (location != null) {
                                Double lat = location.getLatitude();
                                Double lng = location.getLongitude();
                                String ts = Utils.getUTCTimeStampFromMills(location.getTime());
                                lastLatitudeValue = lat;
                                lastLongitudeValue = lng;
                                lastKnownLocation = location;
                                setKeysInSharedPrefs("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                                setKeysInSharedPrefs("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                                setKeysInSharedPrefs("LAST_KNOWN_LOCATION_TS", ts);
                                if (callback != null) {
                                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                                            callback, lat, lng, ts);
                                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                }
                                if (animate && googleMap != null) {
                                    LatLng latLng = new LatLng(lat, lng);
                                    if (userPositionMarker == null) {
                                        upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f); //TODO this function will be removed
                                    } else {
                                        if (storeLocateOnMapCallBack == null)
                                            userPositionMarker.setVisible(true);
                                        userPositionMarker.setPosition(latLng);
                                    }
                                    animateCamera(lat, lng, 17.0f, "ZOOM");
                                }
                            }
                        })
                        .addOnFailureListener(bridgeComponents.getActivity(), e -> Log.e(LOCATION, "Last and current position not known"));
        }
    }

    @JavascriptInterface
    public void drawCircleOnMap(String _payload) {
        try {
            JSONObject payload = new JSONObject(_payload);
            String circleId = payload.optString("circleId", "");
            double centerLat = payload.optDouble("centerLat", 0.0);
            double centerLon = payload.optDouble("centerLon", 0.0);
            boolean showPrimaryStrokeColor = payload.optBoolean("showPrimaryStrokeColor", true);
            CircleRippleEffectOptions config = setCircleConfigFromJSON(payload, showPrimaryStrokeColor);
            updateRippleCircleWithId(circleId, config, new LatLng(centerLat, centerLon));
        } catch (JSONException e) {
            Log.i(MAPS, "drawCircleOnMap error for ", e);
        }
    }

    protected void updateRippleCircleWithId(String id, CircleRippleEffectOptions config, LatLng source) {
        if (circleRipples != null) {
            CircleRippleEffect circle = circleRipples.get(id);
            if (circle == null) {
                CircleRippleEffect circleRippleEffect = new CircleRippleEffect(1, config);
                circleRippleEffect.draw(googleMap, source, bridgeComponents, storeCircleOnClickCallback);
                circleRipples.put(id, circleRippleEffect);
            } else {
                circle.updateCircle(config);
            }
        }
    }

    @JavascriptInterface
    public void locateOnMap(boolean goToCurrentLocation, final String lat, final String lon, float zoomLevel, JSONObject circleConfig) {
        try {
            System.out.println("Inside locateOnMap" + zoomLevel);
            ExecutorManager.runOnMainThread(() -> {
                removeMarker("ny_ic_customer_current_location");
                LatLng position = new LatLng(0.0, 0.0);
                final ImageView labelView = labelId.equals("") ? null : Objects.requireNonNull(bridgeComponents.getActivity()).findViewById(Integer.parseInt(labelId));
                boolean moveToCurrentPosition = goToCurrentLocation;
                try {
                    position = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                } catch (Exception e) {
                    e.printStackTrace();
                    moveToCurrentPosition = true;
                }
                if (moveToCurrentPosition) {
                    LatLng latLng = new LatLng(lastLatitudeValue, lastLongitudeValue);
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, zoomLevel));
                } else {
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(position, zoomLevel));
                    googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                }
                if (editLocation && googleMap != null) {
                    if (labelView != null)
                        labelView.setVisibility(View.INVISIBLE);
                    double distFromSource = SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, position);
                    String circleId = circleConfig.optString("circleId", "");
                    removeAllPolylines("");
                    CircleRippleEffectOptions options = setCircleConfigFromJSON(circleConfig, (distFromSource <= editPickupThreshold));
                    updateRippleCircleWithId(circleId, options, position);
                }
                if (googleMap != null) {
                    googleMap.setOnCameraMoveStartedListener(new GoogleMap.OnCameraMoveStartedListener() {
                        @Override
                        public void onCameraMoveStarted(int i) {
                            if (labelView != null)
                                labelView.setVisibility(View.INVISIBLE);
                        }
                    });
                    googleMap.setOnCameraIdleListener(() -> {
                        double latitude = googleMap.getCameraPosition().target.latitude;
                        double longitude = googleMap.getCameraPosition().target.longitude;
                        LatLng markerLatLng = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                        zoneName = "LatLon";
                        if (editLocation) {
                            double distance = SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, markerLatLng);
                            handleEditPickupMarkerListener(zoneName, distance, markerLatLng, labelView, circleConfig);
                        }
                        if (storeLocateOnMapCallBack != null && bridgeComponents != null) {
                            String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeLocateOnMapCallBack, zoneName, latitude, longitude);
                            bridgeComponents.getJsCallback().addJsToWebView(javascript);
                        }
                        if (storeHotspotMapCallback != null && bridgeComponents != null) {
                            LatLngBounds latLngBounds = googleMap.getProjection().getVisibleRegion().latLngBounds;
                            LatLng southwest = latLngBounds.southwest;
                            LatLng northeast = latLngBounds.northeast;
                            String javascript = String.format("window.callUICallback('%s','%s','%s','%s','%s');", storeHotspotMapCallback, southwest.latitude, northeast.latitude, northeast.longitude, southwest.longitude);
                            bridgeComponents.getJsCallback().addJsToWebView(javascript);
                        }
                    });
                }
                if ((lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) && moveToCurrentPosition) {
                    LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                    if (googleMap != null)
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, zoomLevel));
                } else {
                    if (googleMap != null) {
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(position, zoomLevel));
                        googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                    }
                }
            });

        } catch (Exception e) {
            Log.i(MAPS, "LocateOnMap error for ", e);
        }
    }

    public CircleRippleEffectOptions setCircleConfigFromJSON(JSONObject json, boolean showPrimaryStrokeColor) {
        float radius = (float) json.optDouble("radius", 0.0);
        String strokeColour = showPrimaryStrokeColor ? json.optString("primaryStrokeColor", "#FFFFFF") : json.optString("secondaryStrokeColor", "#FFFFFF");
        int fillColor = parseColor(json.optString("fillColor", "#FFFFFF"));
        int strokeWidth = json.optInt("strokeWidth", 0);
        String strokePattern = json.optString("strokePattern", "NORMAL");
        boolean isClickable = json.optBoolean("isCircleClickable", false);
        CircleRippleEffectOptions options = new CircleRippleEffectOptions();
        options.radius(radius);
        options.strokeWidth(strokeWidth);
        options.fromStrokeColor(strokeColour);
        options.fillColor(fillColor);
        options.strokePattern(strokePattern);
        options.isCircleClickable(isClickable);
        return options;
    }

    public void handleEditPickupMarkerListener(String key, double distFromSource, LatLng markerLatlng, ImageView labelView, JSONObject circleJSON) {
        removeMarker("ny_ic_src_marker");
        removeMarker("ny_ic_dest_marker");
        String primaryStrokeColor = circleJSON.optString("primaryStrokeColor", "#00FFFFFF");
        String secondaryStrokeColor = circleJSON.optString("secondaryStrokeColor", "#00FFFFFF");
        String circleId = circleJSON.optString("circleId", "");
        if (distFromSource >= editPickupThreshold) {
            CircleRippleEffectOptions config = new CircleRippleEffectOptions();
            config.fromStrokeColor(secondaryStrokeColor);
            config.radius(editPickupThreshold);
            updateRippleCircleWithId(circleId, config, markerLatlng);

            if (labelView != null) {
                zoneName += LOCATION_IS_FAR;
                MarkerConfig markerConfig = new MarkerConfig();
                markerConfig.locationName(bridgeComponents.getContext().getString(R.string.location_is_too_far), "");
                Bitmap bitmap = getMarkerBitmapFromView(null, true, MarkerType.NORMAL_MARKER, markerConfig);
                labelView.setImageBitmap(bitmap);
                labelView.setVisibility(View.VISIBLE);
            }
        } else {
            CircleRippleEffectOptions config = new CircleRippleEffectOptions();
            config.fromStrokeColor(primaryStrokeColor);
            config.radius(editPickupThreshold);
            updateRippleCircleWithId(circleId, config, markerLatlng);
        }
    }

    @JavascriptInterface
    public void locateOnMapV2(String _payload) {
        if (googleMap != null) {
            try {
                JSONObject payload = new JSONObject(_payload);
                boolean goToCurrentLocation = payload.optBoolean("goToCurrentLocation", false);
                String lat = payload.optString("lat", "0.0");
                String lon = payload.optString("lon", "0.0");
                String geoJson = payload.optString("geoJson", "");
                String points = payload.optString("points", "[]");
                float zoomLevel = (float) payload.optDouble("zoomLevel", 17.0);
                labelId = payload.optString("labelId", "");
                final ImageView labelView = payload.optString("labelId", "").equals("") ? null : Objects.requireNonNull(bridgeComponents.getActivity()).findViewById(Integer.parseInt(payload.getString("labelId")));
                String markerCallback = payload.optString("markerCallback", "");
                JSONArray markerCallbackForTags = payload.has("markerCallbackForTags") ? payload.getJSONArray("markerCallbackForTags") : new JSONArray();
                String locationName = payload.optString("locationName", "");
                JSONObject specialZoneMarkerConfig = payload.optJSONObject("specialZoneMarkerConfig");
                boolean navigateToNearestGate = payload.optBoolean("navigateToNearestGate", false);
                editPickupThreshold = (float) payload.optDouble("editPickUpThreshold", 100.0);
                editLocation = payload.optBoolean("editPickupLocation", false);
                String spotIcon = specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optString("spotIcon", "ny_ic_zone_pickup_marker_yellow") : "ny_ic_zone_pickup_marker_yellow";
                String selectedSpotIcon = specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optString("selectedSpotIcon", "ny_ic_selected_zone_pickup_marker_yellow") : "ny_ic_selected_zone_pickup_marker_yellow";
                String labelImage = specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optString("labelImage", "") : "";
                String theme = specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optString("theme", "LIGHT") : "LIGHT";
                boolean showZoneLabel = specialZoneMarkerConfig != null && specialZoneMarkerConfig.optBoolean("showZoneLabel", false);
                String labelActionImage = specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optString("labelActionImage", "") : "";
                LatLng markerLatlng = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                JSONObject circleConfigJSON = payload.optJSONObject("circleConfig");
                String circleId = circleConfigJSON.optString("circleId", "");
                List<List<LatLng>> polygonCoordinates = getPolygonCoordinates(geoJson);

                final JSONObject hotSpotConfig = locateOnMapConfig != null ? locateOnMapConfig.optJSONObject("hotSpotConfig") : null;
                final boolean enableHotSpot = hotSpotConfig != null && hotSpotConfig.optBoolean("enableHotSpot", false);

                if ((geoJson.equals("") && points.equals("[]") && enableHotSpot) || ((geoJson.equals("") || points.equals("[]")) && !enableHotSpot)) {
                    locateOnMap(goToCurrentLocation, lat, lon, zoomLevel, circleConfigJSON);
                    return;
                }
                ExecutorManager.runOnMainThread(new Runnable() {
                    final MapMode mapMode = points.equals("") ? MapMode.NORMAL : (geoJson.equals("") ? MapMode.HOTSPOT : MapMode.SPECIAL_ZONE);
                    final double goToNearestPointWithinRadius = hotSpotConfig != null ? hotSpotConfig.optDouble("goToNearestPointWithinRadius", 50.0) : 50.0;

                    @Override
                    public void run() {
                        try {
                            locateOnMapManager = new LocateOnMapManager();
                            locateOnMapManager.setShowZoneLabel(showZoneLabel);
                            locateOnMapManager.setLocationName(locationName);
                            locateOnMapManager.setMarkerCallback(markerCallback, markerCallbackForTags);
                            locateOnMapManager.setEnableMapClickListener(payload.optBoolean("enableMapClickListener", false));

                            if (googleMap != null && editLocation) {
                                removeAllPolylines("");
                                double distFromSource = SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, markerLatlng);
                                CircleRippleEffectOptions circleConfig = setCircleConfigFromJSON(circleConfigJSON, (distFromSource <= editPickupThreshold));
                                updateRippleCircleWithId(circleId, circleConfig, markerLatlng);
                            }
                            if (zoneMarkers != null) {
                                for (Map.Entry<String, Marker> marker : zoneMarkers.entrySet()) {
                                    Marker m = marker.getValue();
                                    m.setVisible(false);
                                }
                            }

                            if (layer != null)
                                layer.removeLayerFromMap();
                            removeMarker("ny_ic_customer_current_location");

                            JSONArray zonePoints = new JSONArray(points);
                            drawPolygon(geoJson, zonePoints, specialZoneMarkerConfig);

                            for (int i = 0; i < zonePoints.length(); i++) {
                                Double zoneMarkerLat = (Double) zonePoints.getJSONObject(i).get("lat");
                                Double zoneMarkerLon = (Double) zonePoints.getJSONObject(i).get("lng");
                                addZoneMarker(zoneMarkerLat, zoneMarkerLon, zoneMarkerLat + ":" + zoneMarkerLon, spotIcon);
                            }

                        } catch (JSONException e) {
                            e.printStackTrace();
                        }

                        googleMap.setOnCameraMoveStartedListener(new GoogleMap.OnCameraMoveStartedListener() {
                            @Override
                            public void onCameraMoveStarted(int i) {
                                Marker m = zoneMarkers.get("selectedGate");
                                if (m != null)
                                    m.setVisible(false);
                            }
                        });

                        googleMap.setOnMapLoadedCallback(() -> {
                            try {
                                JSONArray zonePoint = new JSONArray(points);
                                JSONObject nearestPoint = getNearestPoint(googleMap.getCameraPosition().target.latitude, googleMap.getCameraPosition().target.longitude, zonePoint);
                                if (mapMode.equals(MapMode.SPECIAL_ZONE)) {
                                    animateToFitPolygon(polygonCoordinates, goToCurrentLocation, nearestPoint.getDouble("lat"), nearestPoint.getDouble("long"), payload.optJSONObject("locateOnMapPadding"));

                                    if (showZoneLabel && locateOnMapManager != null) {
                                        String place = nearestPoint.optString("place", "");
                                        GeoJsonFeature feature = locateOnMapManager.getGateFeature(place);
                                        if (feature != null) {
                                            MarkerConfig markerConfig = new MarkerConfig();
                                            markerConfig.locationName(zoneName, locateOnMapManager.locationName);
                                            markerConfig.setLabelActionImage(labelActionImage);
                                            markerConfig.setLabelImage(labelImage);
                                            addZoneMarker(nearestPoint.getDouble("lat"), nearestPoint.getDouble("long"), "selectedZoneGate", selectedSpotIcon, markerConfig);
                                            changePolygonFocus(place);
                                        }
                                    }
                                } else if (mapMode.equals(MapMode.HOTSPOT) && SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng(nearestPoint.getDouble("lat"), nearestPoint.getDouble("long"))) < goToNearestPointWithinRadius) {
                                    animateCamera(nearestPoint.getDouble("lat"), nearestPoint.getDouble("long"), 20.0f, ZoomType.NO_ZOOM);
                                } else if (!lat.equals("0.0") && !lon.equals("0.0") && !editLocation) {
                                    animateCamera(Double.parseDouble(lat), Double.parseDouble(lon), zoomLevel, ZoomType.ZOOM);
                                }
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        });

                        googleMap.setOnCameraIdleListener(() -> {
                            try {
                                double lat1 = (googleMap.getCameraPosition().target.latitude);
                                double lng = (googleMap.getCameraPosition().target.longitude);
                                boolean isPointInsidePolygon = false;
                                boolean isOnSpot = false;
                                JSONArray zonePoints = new JSONArray(points);
                                JSONObject nearestPickupPointObj = getNearestPoint(lat1, lng, zonePoints);
                                double nearestPointLat = nearestPickupPointObj.getDouble("lat");
                                double nearestPointLng = nearestPickupPointObj.getDouble("long");
                                double nearestPointDistance = nearestPickupPointObj.getDouble("distance");
                                if (mapMode.equals(MapMode.SPECIAL_ZONE)) {
                                    isPointInsidePolygon = pointInsidePolygon(polygonCoordinates, lat1, lng);
                                    if (isPointInsidePolygon && navigateToNearestGate) {
                                        for (int i = 0; i < zonePoints.length(); i++) {
                                            if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng((Double) zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"))) <= 1) {
                                                zoneName = (String) zonePoints.getJSONObject(i).get("place");
                                                isOnSpot = true;
                                                Marker m = zoneMarkers.get("selectedGate");
                                                if (m != null)
                                                    m.setVisible(false);
                                                addZoneMarker((Double) zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"), "selectedGate", selectedSpotIcon);
                                                changePolygonFocus(zoneName);
                                                break;
                                            }
                                        }
                                        if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng(nearestPointLat, nearestPointLng)) > 1)
                                            animateCamera(nearestPointLat, nearestPointLng, 20.0f, ZoomType.NO_ZOOM);
                                    } else {
                                        zoneName = "LatLon";
                                    }
                                } else if (mapMode.equals(MapMode.HOTSPOT)) {
                                    for (int i = 0; i < zonePoints.length(); i++) {
                                        if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng((Double) zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"))) <= 1) {
                                            isOnSpot = true;
                                            Marker m = zoneMarkers.get("selectedGate");
                                            if (m != null)
                                                m.setVisible(false);
                                            addZoneMarker((Double) zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"), "selectedGate", selectedSpotIcon);
                                            break;
                                        }
                                    }
                                    if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng(nearestPointLat, nearestPointLng)) > 1 && nearestPointDistance <= goToNearestPointWithinRadius)
                                        animateCamera(nearestPointLat, nearestPointLng, 20.0f, ZoomType.NO_ZOOM);
                                    zoneName = "LatLon";
                                }
                                boolean sendCallback = storeLocateOnMapCallBack != null && ((mapMode.equals(MapMode.SPECIAL_ZONE) && (!isPointInsidePolygon || isOnSpot)) || (mapMode.equals(MapMode.HOTSPOT) && (isOnSpot || nearestPointDistance > goToNearestPointWithinRadius)));
                                if (sendCallback) {
                                    if (labelView != null && mapMode.equals(MapMode.SPECIAL_ZONE) && isOnSpot) {
                                        GeoJsonFeature gateGeoJsonFeature = locateOnMapManager.getGateFeature(zoneName);
                                        MarkerConfig markerConfig = new MarkerConfig();
                                        markerConfig.locationName(zoneName, locationName);
                                        markerConfig.setLabelImage(labelImage);
                                        markerConfig.setLabelMaxWidth(specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optInt("labelMaxWidth", 300) : 300);
                                        markerConfig.setTheme(theme);
                                        Bitmap bitmap = getMarkerBitmapFromView(null, true, MarkerType.SPECIAL_ZONE_MARKER, markerConfig);
                                        labelView.setImageBitmap(bitmap);
                                        labelView.setVisibility(View.VISIBLE);
                                    }
                                    if (editLocation) {
                                        double distFromSource = SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng(Double.parseDouble(lat), Double.parseDouble(lon)));
                                        handleEditPickupMarkerListener(zoneName, distFromSource, markerLatlng, labelView, circleConfigJSON);
                                    }
                                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeLocateOnMapCallBack, zoneName, lat1, lng);
                                    Log.e(CALLBACK, javascript);
                                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                }
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        });
                    }
                });
            } catch (Exception e) {
                Log.i(MAPS, "LocateOnMap error for ", e);
            }
        }
    }

    private void animateToFitPolygon(List<List<LatLng>> polygonCoordinates, boolean goToCurrentLocation, double lat, double lon, JSONObject locateOnMapPadding) {
        try {
            double x = 0.0, y = 0.0;
            double left = locateOnMapPadding.optDouble("left", 1.0);
            double top = locateOnMapPadding.optDouble("top", 1.0);
            double right = locateOnMapPadding.optDouble("right", 1.0);
            double bottom = locateOnMapPadding.optDouble("bottom", 1.0);
            List<LatLng> allCoordinates = new ArrayList<>();
            for (List<LatLng> innerList : polygonCoordinates)
                allCoordinates.addAll(innerList);

            ArrayList<LatLng> endPoints = getCoordinateEndPoint(allCoordinates);
            if (endPoints != null) {
                LatLng topLeft = endPoints.get(0);
                LatLng bottomRight = endPoints.get(1);

                x = bottomRight.longitude - topLeft.longitude;
                y = topLeft.latitude - bottomRight.latitude;

                double currentLat = goToCurrentLocation ? lastLatitudeValue : lat;
                double currentLon = goToCurrentLocation ? lastLongitudeValue : lon;

                LatLngBounds bounds = LatLngBounds.builder()
                        .include(new LatLng(currentLat - y / 2 * bottom, currentLon - x / 2 * left))
                        .include(new LatLng(currentLat + y / 2 * top, currentLon + x / 2 * right)).build();
                googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0), animationDuration, null);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private List<List<LatLng>> getPolygonCoordinates(String geoJson) {
        try {
            JSONObject jsonObject = new JSONObject(geoJson);
            List<List<LatLng>> polygonCoordinates = new ArrayList<>();

            for (int i = 0; i < jsonObject.getJSONArray("features").length(); i++) {
                JSONObject feature = jsonObject.getJSONArray("features").getJSONObject(i);
                JSONArray coordinates = feature.getJSONObject("geometry").getJSONArray("coordinates");
                String geometryType = feature.getJSONObject("geometry").getString("type");
                switch (geometryType) {
                    case "Point":
                        polygonCoordinates.add(Collections.singletonList(new LatLng(coordinates.getDouble(1), coordinates.getDouble(0))));
                        break;
                    case "Polygon":
                        JSONArray innerPolygon = coordinates.getJSONArray(0);
                        List<LatLng> coordinateLists = new ArrayList<>();
                        for (int j = 0; j < innerPolygon.length(); j++) {
                            JSONArray coordinate = (JSONArray) innerPolygon.get(j);
                            coordinateLists.add(new LatLng((Double) coordinate.get(1), (Double) coordinate.get(0)));
                        }
                        polygonCoordinates.add(coordinateLists);
                        break;
                    case "MultiPolygon":
                        JSONArray innerMultiPolygon = coordinates.getJSONArray(0).getJSONArray(0);
                        List<LatLng> coordinateList = new ArrayList<>();
                        for (int j = 0; j < innerMultiPolygon.length(); j++) {
                            JSONArray coordinate = (JSONArray) innerMultiPolygon.get(j);
                            coordinateList.add(new LatLng((Double) coordinate.get(1), (Double) coordinate.get(0)));
                        }
                        polygonCoordinates.add(coordinateList);
                        break;
                    default:
                        break;
                }
            }
            return polygonCoordinates;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    private ArrayList<LatLng> getCoordinateEndPoint(List<LatLng> allCoordinates) {
        try {
            ArrayList<Double> all_latitudes = new ArrayList<Double>();
            ArrayList<Double> all_longitudes = new ArrayList<Double>();
            for (int i = 0; i < allCoordinates.size(); i++) {
                LatLng coordinate = (LatLng) allCoordinates.get(i);
                double lat = coordinate.latitude;
                double lon = coordinate.longitude;
                all_latitudes.add(lat);
                all_longitudes.add(lon);
            }

            double minLat = Collections.min(all_latitudes);
            double maxLat = Collections.max(all_latitudes);
            double minLon = Collections.min(all_longitudes);
            double maxLon = Collections.max(all_longitudes);

            double left = minLon - 0.1 * (maxLon - minLon);
            double right = maxLon + 0.1 * (maxLon - minLon);
            double top = maxLat + 0.1 * (maxLat - minLat);
            double bottom = minLat - (maxLat - minLat);

            LatLng topLeft = new LatLng(top, left);
            LatLng bottomRight = new LatLng(bottom, right);

            ArrayList<LatLng> result = new ArrayList<>();
            result.add(topLeft);
            result.add(bottomRight);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    private Boolean pointInsidePolygon(List<List<LatLng>> polygonCoordinates, Double latitude, Double longitide) {
        try {
            double y = latitude;
            double x = longitide;
            boolean isInsidePolygon = false;
            for (int k = 0; k < polygonCoordinates.size(); k++) {
                List<LatLng> eachPolygonCoordinates = polygonCoordinates.get(k);
                boolean inside = false;
                for (int i = 0, j = eachPolygonCoordinates.size() - 1; i < eachPolygonCoordinates.size(); j = i++) {
                    LatLng point1 = eachPolygonCoordinates.get(i);
                    LatLng point2 = eachPolygonCoordinates.get(j);
                    double xi = (double) point1.longitude, yi = (double) point1.latitude;
                    double xj = (double) point2.longitude, yj = (double) point2.latitude;

                    boolean intersect = ((yi > y) != (yj > y)) && (x <= (xj - xi) * (y - yi) / (yj - yi) + xi);
                    isInsidePolygon = isInsidePolygon || intersect;
                    if (intersect)
                        inside = !inside;
                }
                isInsidePolygon = inside;
                if (inside)
                    break;
            }
            return isInsidePolygon;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    @JavascriptInterface
    public void drawPolygon(String geoJson, JSONArray zonePoints, JSONObject specialZoneMarkerConfig) {

        ExecutorManager.runOnMainThread(() -> {
            if (locateOnMapManager != null) {
                String labelActionImage = specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optString("labelActionImage", "") : "";
                String labelImage = specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optString("labelImage", "") : "";
                String selectedSpotIcon = specialZoneMarkerConfig != null ? specialZoneMarkerConfig.optString("selectedSpotIcon", "ny_ic_selected_zone_pickup_marker_yellow") : "ny_ic_selected_zone_pickup_marker_yellow";

                if (layer != null) {
                    layer.removeLayerFromMap();
                }
                if (googleMap != null) {
                    try {
                        JSONObject geo = new JSONObject(geoJson);
                        layer = new GeoJsonLayer(googleMap, geo);

                        layer.setOnFeatureClickListener(new Layer.OnFeatureClickListener() {
                            @Override
                            public void onFeatureClick(Feature feature) {
                                try {
                                    if (!feature.getProperty("name").equals("")) {
                                        GeoJsonFeature geoJsonFeature = (GeoJsonFeature) feature;
                                        changePolygonFocus(feature.getProperty("name"));
                                    }

                                    if (!feature.getProperty("name").equals("")) {
                                        String gateName = feature.getProperty("name");
                                        for (int i = 0; i < zonePoints.length(); i++) {
                                            zoneName = (String) zonePoints.getJSONObject(i).get("place");
                                            if (zoneName.equals(gateName)) {
                                                Double zoneMarkerLat = (Double) zonePoints.getJSONObject(i).get("lat");
                                                Double zoneMarkerLon = (Double) zonePoints.getJSONObject(i).get("lng");
                                                if (locateOnMapManager != null && locateOnMapManager.showZoneLabel) {
                                                    Marker m = zoneMarkers.get("selectedZoneGate");
                                                    if (m != null)
                                                        m.setVisible(false);
                                                    MarkerConfig markerConfig = new MarkerConfig();
                                                    markerConfig.locationName(zoneName, locateOnMapManager.locationName);
                                                    markerConfig.setLabelActionImage(labelActionImage);
                                                    markerConfig.setLabelImage(labelImage);
                                                    addZoneMarker(zoneMarkerLat, zoneMarkerLon, "selectedZoneGate", selectedSpotIcon, markerConfig);
                                                }
                                                animateCamera(zoneMarkerLat, zoneMarkerLon, 20.0f, ZoomType.NO_ZOOM);
                                            }
                                        }
                                    }
                                } catch (Exception e) {
                                    e.printStackTrace();
                                }

                            }
                        });

                        setPolygonStyle(null);
                        if (locateOnMapManager != null && locateOnMapManager.enableMapClickListener) {
                            googleMap.setOnMapClickListener(listener -> {
                                if (locateOnMapManager != null && locateOnMapManager.showZoneLabel) {
                                    Marker m = zoneMarkers.get("selectedZoneGate");
                                    if (m != null)
                                        m.setVisible(false);
                                    removePolygonFocus(locateOnMapManager.focusedGeoJsonFeature);
                                }
                            });
                        }
                        layer.addLayerToMap();
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
    }

    public void setPolygonStyle(GeoJsonFeature selectedGeoJsonFeature) {
        try {
            if (layer != null && locateOnMapManager != null) {
                Iterable<GeoJsonFeature> features = layer.getFeatures();
                List<PatternItem> dottedPattern = Arrays.asList(new Dash(12), new Gap(12));
                for (GeoJsonFeature geoJsonFeature : features) {
                    GeoJsonPolygonStyle polyStyle = new GeoJsonPolygonStyle();
                    polyStyle.setStrokeWidth(3);
                    if (!geoJsonFeature.getProperty("name").equals("")) {
                        if (geoJsonFeature.equals(selectedGeoJsonFeature)) {
                            polyStyle.setFillColor(Color.argb(75, 22, 150, 46));
                        } else {
                            polyStyle.setFillColor(Color.argb(35, 171, 226, 186));
                            polyStyle.setStrokePattern(dottedPattern);
                        }
                        polyStyle.setClickable(true);
                        polyStyle.setStrokeColor(Color.argb(100, 22, 150, 46));
                    } else {
                        polyStyle.setFillColor(Color.argb(15, 0, 102, 255));
                        polyStyle.setStrokeColor(Color.BLUE);
                        polyStyle.setStrokePattern(dottedPattern);
                        polyStyle.setClickable(false);
                    }
                    geoJsonFeature.setPolygonStyle(polyStyle);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void removePolygonFocus(GeoJsonFeature feature) {
        try {
            if (layer != null && locateOnMapManager != null && feature != null) {
                locateOnMapManager.setFocusedGeoJsonFeature(null);
                List<PatternItem> dottedPattern = Arrays.asList(new Dash(15), new Gap(15));
                GeoJsonPolygonStyle polygonStyle = new GeoJsonPolygonStyle();
                polygonStyle.setStrokeWidth(3);
                polygonStyle.setFillColor(Color.argb(35, 171, 226, 186));
                polygonStyle.setStrokePattern(dottedPattern);
                polygonStyle.setStrokeColor(Color.argb(100, 22, 150, 46));
                feature.setPolygonStyle(polygonStyle);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void changePolygonFocus(String name) {
        if (layer != null && locateOnMapManager != null) {
            Iterable<GeoJsonFeature> features = layer.getFeatures();
            GeoJsonFeature lastFocusedGeoJsonFeature = locateOnMapManager.focusedGeoJsonFeature;
            if (lastFocusedGeoJsonFeature != null) {
                if (lastFocusedGeoJsonFeature.getProperty("name").equals(name))
                    return;
                removePolygonFocus(lastFocusedGeoJsonFeature);
            }
            for (GeoJsonFeature geoJsonFeature : features) {
                if (geoJsonFeature.getProperty("name").equals(name)) {
                    locateOnMapManager.setFocusedGeoJsonFeature(geoJsonFeature);
                    GeoJsonPolygonStyle polygonStyle = new GeoJsonPolygonStyle();
                    polygonStyle.setStrokeWidth(3);
                    polygonStyle.setFillColor(Color.argb(75, 22, 150, 46));
                    polygonStyle.setStrokeColor(Color.argb(100, 22, 150, 46));
                    geoJsonFeature.setPolygonStyle(polygonStyle);
                    break;
                }
            }
        }
    }

    public void addZoneMarker(double lat, double lng, String name, String icon) {
        addZoneMarker(lat, lng, name, icon, new MarkerConfig());
    }

    public void addZoneMarker(double lat, double lng, String name, String icon, MarkerConfig markerConfig) {
        try {
            MarkerOptions markerOptionsObj = new MarkerOptions()
                    .title("")
                    .position(new LatLng(lat, lng))
                    .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(icon, false, MarkerType.SPECIAL_ZONE_MARKER, markerConfig)));
            if (!markerConfig.primaryText.equals(""))
                markerOptionsObj.anchor(markerOptionsObj.getAnchorU(), 0.88f);
            else
                markerOptionsObj.anchor(0.5f, 0.5f);
            Marker m = googleMap.addMarker(markerOptionsObj);
            if (m != null) {
                m.setTag(name);
                m.hideInfoWindow();
                zoneMarkers.put(name, m);
            }

            googleMap.setOnMarkerClickListener(marker -> {
                animateCamera(marker.getPosition().latitude, marker.getPosition().longitude, 20.0f, ZoomType.NO_ZOOM);
                if (locateOnMapManager != null && !locateOnMapManager.markerCallback.equals("") && locateOnMapManager.markerCallbackTags.contains(marker.getTag())) {
                    String js = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');", locateOnMapManager.markerCallback, marker.getTag(), marker.getPosition().latitude, marker.getPosition().longitude);
                    bridgeComponents.getJsCallback().addJsToWebView(js);
                }
                return true;
            });

        } catch (Exception e) {
            Log.d("error on pickup markers", e.toString());
        }
    }

    public JSONObject getNearestPoint(double lat, double lng, JSONArray path) throws JSONException {

        JSONObject jsonObject = new JSONObject();

        Location locationA = new Location("point A");
        locationA.setLatitude(lat);
        locationA.setLongitude(lng);

        double minDist = 10000000000.0;

        Location location = new Location("final point");

        for (int i = 0; i < path.length(); i++) {
            JSONObject a = path.getJSONObject(i);
            double px = (Double) a.get("lat");
            double py = (Double) a.get("lng");

            Location locationB = new Location("point B");
            locationB.setLatitude(px);
            locationB.setLongitude(py);

            float distance = locationA.distanceTo(locationB);

            if (distance < minDist) {
                minDist = distance;
                location = locationB;
                zoneName = a.has("place") ? a.getString("place") : "";
            }
        }
        return jsonObject.put("place", zoneName)
                .put("lat", location.getLatitude())
                .put("long", location.getLongitude())
                .put("distance", minDist);
    }

    @JavascriptInterface
    public void exitLocateOnMap(String str) {
        try {
            locateOnMapManager = null;
            storeLocateOnMapCallBack = null;
            editLocation = false;
            ExecutorManager.runOnMainThread(() -> {
                if (googleMap != null) {
                    googleMap.setOnCameraMoveListener(null);
                    googleMap.setOnCameraIdleListener(null);
                }
                if (zoneMarkers != null) {
                    for (Map.Entry<String, Marker> marker : zoneMarkers.entrySet()) {
                        Marker m = marker.getValue();
                        m.setVisible(false);
                    }
                }
                if (layer != null)
                    layer.removeLayerFromMap();
                removeAllCircles();
            });
        } catch (Exception e) {
            Log.i(MAPS, "LocateOnMap Exit Error for ", e);
        }
    }

    protected void removeAllCircles() {
        if (circleRipples != null) {
            ArrayList<String> circleKeys = new ArrayList<>();
            for (String i : circleRipples.keySet()) {
                circleKeys.add(i);
            }
            for (String circleKey : circleKeys) {
                CircleRippleEffect circle = circleRipples.get(circleKey);
                if (circle != null) {
                    circle.remove();
                }
                circleRipples.remove(circleKey);
            }
        }
    }

    protected void dottedLineFromCurrentPosition(String key, Double lat, Double lon, Boolean dottedLineVisible, Double dottedLineRange, String dottedLineColor, String gmapKey) {
        PolylineDataPoints polyline = getPolyLineDataByMapInstance(gmapKey, key);
        if (googleMap != null && dottedLineVisible) {
            if (polyline != null) {
                polyline.polyline.remove();
                polyline.setPolyline(null);
                setPolyLineDataByMapInstance(gmapKey, key, polyline);
            }
            if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng(lastLatitudeValue, lastLongitudeValue)) < dottedLineRange)
                drawDottedLine(key, googleMap.getCameraPosition().target.latitude, googleMap.getCameraPosition().target.longitude, lastLatitudeValue, lastLongitudeValue, dottedLineColor, gmapKey);
        }
    }

    private void drawDottedLine(String key, Double srcLat, Double srcLon, Double destLat, Double destLon, String color, String gmapKey) {
        if (googleMap != null) {
            PolylineOptions polylineOptions = new PolylineOptions();
            polylineOptions.add(new LatLng(srcLat, srcLon));
            polylineOptions.add(new LatLng(destLat, destLon));
            setPolyLineDataByMapInstance(gmapKey, key, setRouteCustomTheme(polylineOptions, Color.parseColor(color), "DOT", 8, null, googleMap, false, key, gmapKey));
        }
    }

    @JavascriptInterface
    public void initiateLocationServiceClient() {
        setKeyboardVisibilityListener();
        if (!isLocationPermissionEnabled()) return;
        resolvableLocationSettingsReq();
    }

    // Alternate way to get the keyboard status since there is no Keyboard Listener or Events in android
    private void setKeyboardVisibilityListener() {
        if (bridgeComponents.getActivity() != null && onGlobalLayoutListener == null) {
            try {
                final View parentView = bridgeComponents.getActivity().findViewById(android.R.id.content).getRootView();
                onGlobalLayoutListener = new ViewTreeObserver.OnGlobalLayoutListener() {

                    private boolean alreadyOpen;
                    private final Rect rect = new Rect();

                    @Override
                    public void onGlobalLayout() {
                        int defaultKeyboardHeightDP = 100;
                        int estimatedKeyboardDP = defaultKeyboardHeightDP + 48;
                        int estimatedKeyboardHeight = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, estimatedKeyboardDP, parentView.getResources().getDisplayMetrics());
                        parentView.getWindowVisibleDisplayFrame(rect);
                        int heightDiff = parentView.getRootView().getHeight() - (rect.bottom - rect.top);
                        boolean isShown = heightDiff >= estimatedKeyboardHeight;

                        if (isShown == alreadyOpen) {
                            return;
                        }
                        alreadyOpen = isShown;
                        try {
                            String encoded = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
                            String command = String.format("window[\"onEvent'\"]('%s',atob('%s'))", isShown ? "onKeyboardOpen" : "onKeyboardClose", encoded);
                            bridgeComponents.getJsCallback().addJsToWebView(command);
                        } catch (Exception e) {
                            Log.e("MobilityCommonBridge", "Error in onVisibilityChanged " + e);
                        }
                    }
                };
                parentView.getViewTreeObserver().addOnGlobalLayoutListener(onGlobalLayoutListener);
            } catch (Exception e) {
                Log.e("MobilityCommonBridge", "Error in setKeyboardVisibilityListener " + e);
            }
        }
    }

    private void resolvableLocationSettingsReq() {

        LocationRequest locationRequest = createLocReq();

        LocationSettingsRequest.Builder lBuilder = new LocationSettingsRequest.Builder()
                .addLocationRequest(locationRequest)
                .setAlwaysShow(true);

        Task<LocationSettingsResponse> task =
                LocationServices.getSettingsClient(bridgeComponents.getContext()).checkLocationSettings(lBuilder.build());

        task.addOnCompleteListener(task1 -> {
            try {
                task1.getResult(ApiException.class);
            } catch (ApiException exception) {
                switch (exception.getStatusCode()) {
                    case LocationSettingsStatusCodes.RESOLUTION_REQUIRED:
                        try {
                            if (bridgeComponents.getActivity() != null) {
                                ResolvableApiException resolvable = (ResolvableApiException) exception;
                                resolvable.startResolutionForResult(bridgeComponents.getActivity(), LOCATION_RESOLUTION_REQUEST_CODE);
                            }
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
        });
    }

    private LocationRequest createLocReq() {
        return new LocationRequest.Builder(Priority.PRIORITY_HIGH_ACCURACY)
                .setIntervalMillis(1000)
                .setMinUpdateIntervalMillis(500)
                .build();
    }

    @JavascriptInterface
    public void getCurrentPosition(String callback) {
        getCurrentPosition(callback, true);
    }

    @JavascriptInterface
    public void getCurrentPosition(String callback, boolean shouldFallBack) {
        if (!isLocationPermissionEnabled()) return;
        updateLastKnownLocation(callback, false, ZoomType.ZOOM, shouldFallBack);
    }

    @SuppressLint("MissingPermission")
    @JavascriptInterface
    public void isMockLocation(String callback) {
        Activity activity = bridgeComponents.getActivity();
        if (!isLocationPermissionEnabled()) return;
        if (client != null && activity != null) {
            client.getLastLocation()
                    .addOnSuccessListener(activity, location -> {
                        boolean isMock = false;
                        if (location != null) {
                            if (Build.VERSION.SDK_INT <= 30) {
                                isMock = location.isFromMockProvider();
                            } else {
                                isMock = location.isMock();
                            }
                        }
                        if (callback != null) {
                            String js = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                    callback, isMock);
                            bridgeComponents.getJsCallback().addJsToWebView(js);
                        }
                    })
                    .addOnFailureListener(activity, e -> {
                        Log.e(LOCATION, "Last and current position not known");
                        if (callback != null) {
                            String js = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                    callback, false);
                            bridgeComponents.getJsCallback().addJsToWebView(js);
                        }
                    });
        }
    }
    //endregion

    // region Maps
    @JavascriptInterface
    public void removeMarker(final String title) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (markers.containsKey(title)) {
                    Marker m = (Marker) markers.get(title);
                    m.setVisible(false);
                    m.remove();
                    markers.remove(title);
                    Log.i(MAPS, "Removed marker " + title);
                }
            } catch (Exception e) {
                Log.e(MAPS, "Remove Marker error " + title, e);
            }
        });
    }

    @JavascriptInterface
    public void removeAllMarkers() {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (markers != null) {
                    for (Map.Entry<String, Marker> marker : markers.entrySet()) {
                        Marker m = marker.getValue();
                        m.setVisible(false);
                        m.remove();
                    }
                    markers.clear();
                }
            } catch (Exception e) {
                Log.e(MAPS, "RemoveAllMarkers error ", e);
            }
        });
    }

    @JavascriptInterface
    public void animateCamera(final double lat, final double lng, final float zoom, final String zoomType) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (googleMap != null) {
                    LatLng latLngObj = new LatLng(lat, lng);
                    if (zoomType.equals(ZoomType.NO_ZOOM)) {
                        CameraPosition cameraPosition = new CameraPosition.Builder()
                                .target(latLngObj)
                                .zoom(googleMap.getCameraPosition().zoom)
                                .build();
                        googleMap.animateCamera(CameraUpdateFactory.newCameraPosition(cameraPosition), animationDuration, null);
                    } else {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLngObj, zoom), animationDuration, null);
                    }
                    Log.i(MAPS, "Animated Camera");
                }
            } catch (Exception e) {
                Log.e(MAPS, "Error while animating camera");
            }
        });
    }

    @SuppressLint("PotentialBehaviorOverride")
    @JavascriptInterface
    public void upsertMarkerLabel(String configString) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject config = new JSONObject(configString);
                String title = config.optString("title");
                String id = config.optString("id");
                String markerImage = config.optString("markerImage");
                String actionImage = config.optString("actionImage");
                String actionCallBack = config.optString("actionCallBack");
                JSONObject point = config.optJSONObject("position");
                LatLng position = null;
                MarkerConfig markerConfig = new MarkerConfig();
                markerConfig.locationName(title);
                markerConfig.setMarkerActionImage(actionImage);
                if (point != null) {
                    double lat = point.optDouble("lat", lastLatitudeValue);
                    double lon = point.optDouble("lng", lastLongitudeValue);
                    position = new LatLng(lat, lon);
                }
                if (markers.containsKey(id)) {
                    Marker existingMarker = (Marker) markers.get(id);
                    if (position != null) existingMarker.setPosition(position);
                    existingMarker.setVisible(true);
                    existingMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(null, markerImage.equals(""), MarkerType.NORMAL_MARKER, markerConfig)));
                } else {
                    MarkerOptions markerObj = new MarkerOptions();
                    if (position != null) markerObj.position(position);
                    markerObj.title("")
                            .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(null, markerImage.equals(""), MarkerType.NORMAL_MARKER, markerConfig)));
                    Marker marker = googleMap.addMarker(markerObj);
                    if (!actionCallBack.equals("")) {
                        if (marker != null) {
                            marker.setTag(actionCallBack);
                        }
                    }
                    markers.put(id, marker);
                }
            } catch (Exception e) {
                Log.e(MAPS, "Marker config parse error for " + configString, e);
            }
        });
    }

    public void animateCameraV2(final double lat, final double lng, final float zoom, final String zoomType, final String pureScriptID) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                GoogleMap gMap = (googleMapInstance.get(pureScriptID) != null) ? googleMapInstance.get(pureScriptID) : googleMap;
                if (gMap != null) {
                    LatLng latLngObj = new LatLng(lat, lng);
                    if (zoomType.equals(ZoomType.NO_ZOOM)) {
                        CameraPosition cameraPosition = new CameraPosition.Builder()
                                .target(latLngObj)
                                .zoom(googleMap.getCameraPosition().zoom)
                                .build();
                        gMap.animateCamera(CameraUpdateFactory.newCameraPosition(cameraPosition), animationDuration, null);
                    } else {
                        gMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLngObj, zoom), animationDuration, null);
                    }
                    Log.i(MAPS, "Animated Camera v2");
                }
            } catch (Exception e) {
                Log.e(MAPS, "Error while animating camera");
            }
        });
    }

    @JavascriptInterface
    public void showMarker(final String configString) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject config = new JSONObject(configString);
                JSONObject mConfig = config.optJSONObject("markerConfig");
                JSONObject animationConfig = mConfig.optJSONObject("animationConfig");
                String markerId = mConfig.optString("markerId", "");
                String pointerIcon = mConfig.optString("pointerIcon", "");
                String rotation = mConfig.optString("rotation", "0");
                String purescriptId = config.optString("purescriptId", "");
                MarkerConfig markerConfig = new MarkerConfig();
                markerConfig.setMarkerId(markerId);
                markerConfig.setPointer(pointerIcon);
                markerConfig.setRotation(Float.parseFloat(rotation));
                String lat = config.optString("lat", "9.9");
                String lng = config.optString("lng", "9.9");
                int markerSize = config.optInt("markerSize", 160);
                float anchorV = (float) config.optDouble("anchorV", 0.0);
                float anchorV1 = (float) config.optDouble("anchorV1", 0.0);
                String animationType = animationConfig.optString("animationType", "NONE");
                int animationDuration = animationConfig.optInt("animationDuration", 0);
                markerConfig.setMarkerAnimation(getAnimationType(animationType), animationDuration);
                upsertMarkerV2(markerConfig, lat, lng, markerSize, anchorV, anchorV1, purescriptId);
            } catch (Exception e) {
                Log.i(MAPS, "Marker creation error -> " + e);
            }
        });
    }

    public void upsertMarkerV2(MarkerConfig markerConfig, final String lat, final String lng, final int markerSize, final float anchorV, final float anchorV1, final String pureScripID) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                GoogleMap gMap = googleMapInstance.get(pureScripID);
                String title = markerConfig.markerId.equals("") ? markerConfig.pointerIcon : markerConfig.markerId;
                String imageName = markerConfig.pointerIcon;
                float rotation = markerConfig.rotation;
                float alpha = (markerConfig.animationType == AnimationType.FADE_IN) ? 0 : 1;
                System.out.println("marker ids" + title);
                if (lat != null && lng != null) {
                    double latitude = lat.equals("9.9") ? lastLatitudeValue : Double.parseDouble(lat);
                    double longitude = lng.equals("9.9") ? lastLongitudeValue : Double.parseDouble(lng);
                    LatLng latLngObj = new LatLng(latitude, longitude);
                    Marker markerObject;
                    if (markers.containsKey(title)) {
                        markerObject = (Marker) markers.get(title);
                        markerObject.setPosition(latLngObj);
                        markerObject.setFlat(true);
                        markerObject.setVisible(true);
                        markerObject.hideInfoWindow();
                        markerObject.getPosition();
                        markerObject.setAlpha(alpha);
                        Log.i(MAPS, "Marker position updated for " + title);
                    } else {
                        MarkerOptions markerOptionsObj = makeMarkerObject(imageName, latitude, longitude, markerSize, anchorV, anchorV1);
                        if (markerOptionsObj != null && gMap != null) {
                            markerObject = gMap.addMarker(markerOptionsObj);
                            markers.put(title, markerObject);
                            if (markerObject != null) {
                                markerObject.setPosition(latLngObj);
                                markerObject.setVisible(true);
                                markerObject.setFlat(true);
                                markerObject.hideInfoWindow();
                                markerObject.setRotation(rotation);
                                markerObject.setAlpha(alpha);
                            }
                            if (title.equals(CURRENT_LOCATION)) {
                                userPositionMarker = markerObject;
                            }
                            Log.i(MAPS, "New marker created and updated for " + title);
                        }
                    }
                    if (markerConfig.animationType != AnimationType.NONE) {
                        fadeInFadeOutMarker(markerConfig.animationType, title, markerConfig.animationDuration);
                    }
                }
            } catch (Exception e) {
                Log.i(MAPS, "Marker creation error " + e);
            }
        });
    }

    public AnimationType getAnimationType(String animationType) {
        switch (animationType) {
            case "FADE_IN":
                return AnimationType.FADE_IN;
            case "FADE_OUT":
                return AnimationType.FADE_OUT;
            case "NONE":
                return AnimationType.NONE;
        }
        return AnimationType.NONE;
    }

    @JavascriptInterface
    public void fadeInFadeOutMarker(final AnimationType animationType, final String markerId, int anmDuration) {
        ExecutorManager.runOnMainThread(() -> {
            Marker marker = markers.get(markerId);
            if (marker == null) return;
            switch (animationType) {
                case FADE_IN:
                    ObjectAnimator.ofFloat(marker, "alpha", 0f, 1f).setDuration(anmDuration * 1000).start();
                    break;
                case FADE_OUT:
                    Animator animator = ObjectAnimator.ofFloat(marker, "alpha", 1f, 0f);
                    animator.addListener(new Animator.AnimatorListener() {
                        @Override
                        public void onAnimationEnd(Animator animator) {
                            marker.setVisible(false);
                        }

                        @Override
                        public void onAnimationStart(Animator animator) {
                        }

                        @Override
                        public void onAnimationCancel(Animator animator) {
                        }

                        @Override
                        public void onAnimationRepeat(Animator animator) {
                        }
                    });
                    animator.setDuration(anmDuration * 1000).start();
                    break;
                case NONE:
                    break;
            }
        });
    }

    @JavascriptInterface
    public void upsertMarker(final String title, final String lat, final String lng, final int markerSize, final float anchorV, final float anchorV1) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (lat != null && lng != null) {
                    double latitude = lat.equals("9.9") ? lastLatitudeValue : Double.parseDouble(lat);
                    double longitude = lat.equals("9.9") ? lastLatitudeValue : Double.parseDouble(lng);
                    LatLng latLngObj = new LatLng(latitude, longitude);
                    Marker markerObject;
                    if (markers.containsKey(title)) {
                        markerObject = (Marker) markers.get(title);
                        markerObject.setPosition(latLngObj);
                        markerObject.setFlat(true);
                        markerObject.setVisible(true);
                        markerObject.hideInfoWindow();
                        markerObject.getPosition();
                        Log.i(MAPS, "Marker position updated for " + title);
                    } else {
                        MarkerOptions markerOptionsObj = makeMarkerObject(title, latitude, longitude, markerSize, anchorV, anchorV1);
                        if (markerOptionsObj != null && googleMap != null) {
                            markerObject = googleMap.addMarker(markerOptionsObj);
                            markers.put(title, markerObject);
                            if (markerObject != null) {
                                markerObject.setPosition(latLngObj);
                                markerObject.setVisible(!(storeLocateOnMapCallBack != null && title.equals(CURRENT_LOCATION)));
                                markerObject.setFlat(true);
                                markerObject.hideInfoWindow();
                            }
                            if (title.equals(CURRENT_LOCATION)) {
                                userPositionMarker = markerObject;
                            }
                            Log.i(MAPS, "New marker created and updated for " + title);
                        }
                    }

                }
            } catch (Exception e) {
                Log.i(MAPS, "Marker creation error for " + title, e);
            }
        });
    }

    @JavascriptInterface
    public String getLocationNameSDK(double latitude, double longitude) {
        GeoCoderHelper geoCoderHelper = new GeoCoderHelper(bridgeComponents.getContext());
        return geoCoderHelper.getLocName(latitude, longitude);
    }

    @JavascriptInterface
    public String getCoordinateFromAddress(String address) {
        GeoCoderHelper geoCoderHelper = new GeoCoderHelper(bridgeComponents.getContext());
        GeoCoderHelper.GeoCoordinate coordinate = geoCoderHelper.getGeoCoordinateFromAddress(address);
        if (coordinate != null) {
            JSONObject jsonObject = new JSONObject();
            try {
                jsonObject.put("latitude", coordinate.getLatitude());
                jsonObject.put("longitude", coordinate.getLongitude());
            } catch (JSONException e) {
                e.printStackTrace();
            }
            return jsonObject.toString();
        }
        return "NO_COORDINATE_FOUND";
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
            Log.e(MAPS, "MARKER obj creation", e);
            return null;
        }
    }

    protected Bitmap constructBitmap(final int markerSize, final String title) {
        @SuppressLint("DiscouragedApi") int imageID = bridgeComponents.getContext().getResources().getIdentifier(title, "drawable", bridgeComponents.getContext().getPackageName());
        @SuppressLint("UseCompatLoadingForDrawables") BitmapDrawable bitmapdraw = (BitmapDrawable) bridgeComponents.getContext().getResources().getDrawable(imageID);
        Bitmap b = bitmapdraw.getBitmap();
        float maximum = Math.max(b.getWidth(), b.getHeight());
        float multiplier = markerSize / maximum;
        int markerWidth = Math.round(b.getWidth() * multiplier);
        int markerHeight = Math.round(b.getHeight() * multiplier);
        Log.i(MAPS, "real width and height of " + title + b.getWidth() + " , " + b.getHeight());
        Log.i(MAPS, "after width and height of " + title + markerWidth + " , " + markerHeight);
        return Bitmap.createScaledBitmap(b, markerWidth, markerHeight, false);
    }

    @JavascriptInterface
    public void openNavigation(double slat, double slong, double dlat, double dlong) {
        try {
            setKeysInSharedPrefs("MAPS_OPENED", "true");
            String query = "google.navigation:q=%f,%f";
            String mapsQuery = String.format(Locale.ENGLISH, query, dlat, dlong);
            Uri mapsURI = Uri.parse(mapsQuery);
            Intent mapIntent = new Intent(Intent.ACTION_VIEW, mapsURI);
            mapIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            mapIntent.setPackage("com.google.android.apps.maps");
            bridgeComponents.getContext().startActivity(mapIntent);
        } catch (Exception e) {
            JuspayLogger.d(MAPS, "Unable to open navigation");
        }
    }

    @JavascriptInterface
    public void openNavigationWithQuery(double lat, double lon, String query, String packageName) {
        try {
            setKeysInSharedPrefs("MAPS_OPENED", "true");
            String mapsQuery = String.format(Locale.ENGLISH, query, lat, lon);
            Uri mapsURI = Uri.parse(mapsQuery);
            Intent mapIntent = new Intent(Intent.ACTION_VIEW, mapsURI);
            mapIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            mapIntent.setPackage(packageName);
            bridgeComponents.getContext().startActivity(mapIntent);
        } catch (ActivityNotFoundException e) {
            JuspayLogger.d(MAPS, "Trying Fallback");
            openNavigation(0.0, 0.0, lat, lon);
        } catch (Exception e) {
            JuspayLogger.d(MAPS, "Unable to open navigation");
        }
    }

    @JavascriptInterface
    public void getLocationName(String latitude, String longitude, String defaultText, String callback) {
        if (!isLocationPermissionEnabled()) return;

        updateLastKnownLocation(null, false, ZoomType.ZOOM, true);

        if (defaultText.equals(CURRENT_LOCATION_LATLON)) {
            latitude = String.valueOf(lastLatitudeValue);
            longitude = String.valueOf(lastLongitudeValue);
        }

        Geocoder geocoder = new Geocoder(bridgeComponents.getActivity(), Locale.getDefault());
        StringBuilder returnedAddressStrBuilder;
        try {
            List<Address> addresses = geocoder.getFromLocation(Double.parseDouble(latitude), Double.parseDouble(longitude), 1);
            if (addresses != null && addresses.size() > 0) {
                returnedAddressStrBuilder = new StringBuilder();
                Address returnedAddress = addresses.get(0);
                for (int i = 0; i <= returnedAddress.getMaxAddressLineIndex(); i++) {
                    returnedAddressStrBuilder.append(returnedAddress.getAddressLine(i)).append(",");
                }
                Log.d(MAPS, "getLocationName:" + returnedAddressStrBuilder);
            } else {
                returnedAddressStrBuilder = new StringBuilder(defaultText);
                Log.e(MAPS, "Can't fetch current Address");
            }
            if (callback != null) {
                String returnedAddressStr = String.valueOf(returnedAddressStrBuilder);
                returnedAddressStr = returnedAddressStr.replaceAll("'", "");
                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                        callback, latitude, longitude, returnedAddressStr);
                Log.d(MAPS, "getCurrent___Position___inside if" + latitude + longitude);
                bridgeComponents.getJsCallback().addJsToWebView(javascript);
            }
        } catch (Exception e) {
            Log.e(MAPS, "Exception occurred in getting Location Name " + e.getMessage());
            e.printStackTrace();
        }
    }

    @JavascriptInterface
    public void setMapPadding(final int left, final int top, final int right, final int bottom) {
        ExecutorManager.runOnMainThread(() -> {
            if (googleMap != null) {
                googleMap.setPadding(left, top, right, bottom);
            }
        });
    }


    @SuppressLint({"MissingPermission", "PotentialBehaviorOverride"})
    private void getMapAsync(SupportMapFragment mapFragment, boolean isEnableCurrentLocation, final String mapType, final String callback, final String pureScriptId, final float zoom) {
        if (bridgeComponents.getActivity() != null) {
            mapFragment.getMapAsync(googleMap -> {
                this.googleMap = googleMap;
                googleMapInstance.put(pureScriptId, googleMap);
                System.out.println("Inside getMapAsync " + googleMap);
                if (mapType.equals(HOTSPOTS)) {
                    googleMap.setMinZoomPreference(10.0f);
                    googleMap.setMaxZoomPreference(19.0f);
                } else {
                    googleMap.setMinZoomPreference(7.0f);
                    googleMap.setMaxZoomPreference(googleMap.getMaxZoomLevel());
                }
                googleMap.getUiSettings().setRotateGesturesEnabled(false);
                googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                if (isLocationPermissionEnabled()) {
                    googleMap.setMyLocationEnabled(isEnableCurrentLocation);
                }
                markersElement.put(pureScriptId, markers);
                googleMap.setOnMarkerClickListener(marker -> {
                    marker.hideInfoWindow();
                    return true;
                });
                try {
                    setMapCustomTheme();
                    if (lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) {
                        LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        this.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, zoom));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                if (callback != null) {
                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "MAP", "READY", "LOADED");
                    Log.e(MAPS, javascript);
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            });
        }
    }

    public MarkerConfig getMarkerConfigWithIdAndName(String markerName, String markerId) {
        MarkerConfig config = new MarkerConfig();
        config.setMarkerId(markerId);
        config.setPointer(markerName);
        return config;
    }

    public void setMapCustomTheme() { // TODO Check for grey boxes and update the json for the same -- SHAILESH GAHLAWAT
        boolean success;
        try {
            success = googleMap.setMapStyle(
                    MapStyleOptions.loadRawResourceStyle(
                            bridgeComponents.getContext(), R.raw.map_style_retro));
            if (!success) {
                Log.e(MAPS, "Style parsing failed.");
            }
        } catch (Resources.NotFoundException e) {
            Log.e(MAPS, "Can't find style. Error: ", e);
        }
    }

    @JavascriptInterface
    @SuppressLint("MissingPermission")
    public void enableMyLocation(boolean isEnableCurrentLocation) {
        try {
            ExecutorManager.runOnMainThread((() -> {
                if (googleMap != null && isLocationPermissionEnabled()) {
                    googleMap.setMyLocationEnabled(isEnableCurrentLocation);
                }
            }));
        } catch (Exception e) {
            Log.i(MAPS, "Enable My Location on GoogleMap error", e);
        }
    }

    @JavascriptInterface
    public void reallocateMapFragment(final String pureScriptId) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (bridgeComponents.getActivity() != null) {
                    SupportMapFragment mapFragment = (SupportMapFragment) ((FragmentActivity) bridgeComponents.getActivity()).getSupportFragmentManager()
                            .findFragmentById(Integer.parseInt(pureScriptId));
                    if (mapFragment != null) {
                        mapFragment.getMapAsync(googleMap -> {
                            this.googleMap = googleMap;
                            googleMap.getUiSettings().setRotateGesturesEnabled(false);
                            googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                            HashMap<String, Marker> markersByGmapInstance = markersElement.get(pureScriptId);
                            markers = markersByGmapInstance != null ? markersByGmapInstance : new HashMap<>();
                        });
                    }
                }
            } catch (Exception e) {
                Log.e("reallocateMapFragment", "FAILED WHILE REALLOCATING " + e);
            }
        });
    }

    @JavascriptInterface
    public void addRippleCircle(String config) {
        ExecutorManager.runOnBackgroundThread(() -> {
            int count = 1;
            String prefix = "ripples";
            CircleRippleEffectOptions options = new CircleRippleEffectOptions();
            LatLng point = new LatLng(lastLatitudeValue, lastLongitudeValue);
            try {
                JSONObject jsonConfig = new JSONObject(config);
                count = jsonConfig.optInt("count", 1);
                prefix = jsonConfig.optString("prefix", "ripples");
                JSONObject center = jsonConfig.optJSONObject("center");
                if (center != null) {
                    double lat = center.optDouble("lat", lastLatitudeValue);
                    double lon = center.optDouble("lng", lastLongitudeValue);
                    point = new LatLng(lat, lon);
                }
                getCircleOptionsFromJSON(jsonConfig, options);
            } catch (JSONException e) {
                Log.e(LOG_TAG, "Error parsing ripple config");
            }
            for (int i = 1; i <= count; i++) {
                CircleRippleEffect circleRippleEffect = new CircleRippleEffect(i,
                        options.clone()
                                .factor(i)
                                .fillColor(options.getFillColor())
                                .radius((i) * options.getRadius()));
                circleRippleEffect.draw(googleMap, point, bridgeComponents, null);
                circleRipples.put(prefix + "_" + i, circleRippleEffect);
            }
        });
    }

    @JavascriptInterface
    public void animateRippleCircle(String config) {
//        HandlerThread handler = new HandlerThread("CircleAnimation");
//        handler.start();
//        new Handler(handler.getLooper()).post(() -> TODO: Need to verify why animation is lagging if we run in Handler Thread.
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject jsonConfig = new JSONObject(config);
                int count = jsonConfig.optInt("count", 1);
                String prefix = jsonConfig.optString("prefix", "ripples");
                for (int i = 1; i <= count; i++) {
                    CircleRippleEffect circle = circleRipples.get(prefix + "_" + i);
                    if (circle != null) {
                        circle.startAnimation();
                    }
                }
            } catch (JSONException e) {
                Log.e(LOG_TAG, "Error parsing ripple config");
            }
        });
    }

    @JavascriptInterface
    public void removeRippleCircle(String config) {
        ExecutorManager.runOnBackgroundThread(() -> {
            try {
                JSONObject jsonConfig = new JSONObject(config);
                int count = jsonConfig.optInt("count", 1);
                String prefix = jsonConfig.optString("prefix", "ripples");
                for (int i = 1; i <= count; i++) {
                    String id = prefix + "_" + i;
                    CircleRippleEffect circle = circleRipples.get(id);
                    if (circle != null) {
                        circle.remove();
                    }
                    circleRipples.remove(id);
                }
            } catch (JSONException e) {
                Log.e(LOG_TAG, "Error parsing ripple config");
            }
        });
    }

    @JavascriptInterface
    public void updateRippleCirclePosition(String config) {
        ExecutorManager.runOnBackgroundThread(() -> {
            try {
                JSONObject jsonConfig = new JSONObject(config);
                int count = jsonConfig.optInt("count", 1);
                String prefix = jsonConfig.optString("prefix", "ripples");
                JSONObject center = jsonConfig.optJSONObject("center");
                LatLng point = new LatLng(lastLatitudeValue, lastLongitudeValue);
                if (center != null) {
                    double lat = center.optDouble("lat", lastLatitudeValue);
                    double lon = center.optDouble("lng", lastLongitudeValue);
                    point = new LatLng(lat, lon);
                }
                for (int i = 1; i <= count; i++) {
                    CircleRippleEffect circle = circleRipples.get(prefix + "_" + i);
                    if (circle != null) {
                        circle.updatePosition(point);
                    }
                }
            } catch (JSONException e) {
                Log.e(LOG_TAG, "Error parsing ripple config");
            }
        });
    }

    @JavascriptInterface
    public void addGroundOverlay(String config) {
        ExecutorManager.runOnBackgroundThread(() -> {
            try {
                JSONObject jsonConfig = new JSONObject(config);
                Context context = bridgeComponents.getContext();
                String id = jsonConfig.optString("id", "ground_overlay");
                int height = jsonConfig.optInt("height", 50);
                int width = jsonConfig.optInt("width", 50);
                String imageName = jsonConfig.optString("imageUrl", "");
                boolean fetchFromView = jsonConfig.optBoolean("fetchFromView", false);
                String viewID = jsonConfig.optString("viewID", "");
                JSONObject center = jsonConfig.optJSONObject("center");
                LatLng point = new LatLng(lastLatitudeValue, lastLongitudeValue);
                if (center != null) {
                    double lat = center.optDouble("lat", lastLatitudeValue);
                    double lon = center.optDouble("lng", lastLongitudeValue);
                    point = new LatLng(lat, lon);
                }
                Drawable drawable = null;
                if (bridgeComponents.getActivity() != null && fetchFromView) {
                    ImageView view = bridgeComponents.getActivity().findViewById(Integer.parseInt(viewID));
                    drawable = view.getDrawable();
                }
                GroundOverlayOptions options = new GroundOverlayOptions().position(point, width, height);
                if (drawable != null) {
                    options.image(BitmapDescriptorFactory.fromBitmap(drawableToBitmap(drawable)));
                } else {
                    options.image(BitmapDescriptorFactory.fromResource(context.getResources().getIdentifier(imageName, "drawable", context.getPackageName())));
                }
                ExecutorManager.runOnMainThread(() -> groundOverlays.put(id, googleMap.addGroundOverlay(options)));
            } catch (JSONException e) {
                Log.e(LOG_TAG, "Error parsing ripple config");
            }
        });
    }

    @JavascriptInterface
    public boolean isOverlayPresent(String id) {
        return groundOverlays.containsKey(id);
    }

    @JavascriptInterface
    public boolean isCirclePresent(String id) {
        return circleRipples.containsKey(id);
    }

    @JavascriptInterface
    public void removeGroundOverlay(String config) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject jsonConfig = new JSONObject(config);
                String id = jsonConfig.optString("id", "ground_overlay");
                GroundOverlay overlay = groundOverlays.get(id);
                if (overlay != null) {
                    overlay.remove();
                }
                groundOverlays.remove(id);
            } catch (JSONException e) {
                Log.e(LOG_TAG, "Error parsing ground Overlay config");
            }
        });
    }

    @JavascriptInterface
    public void updateGroundOverlay(String config) {
        ExecutorManager.runOnBackgroundThread(() -> {
            try {
                JSONObject jsonConfig = new JSONObject(config);
                String id = jsonConfig.optString("id", "ground_overlay");
                GroundOverlay overlay = groundOverlays.get(id);
                JSONObject center = jsonConfig.optJSONObject("center");
                LatLng point = new LatLng(lastLatitudeValue, lastLongitudeValue);
                if (center != null) {
                    double lat = center.optDouble("lat", lastLatitudeValue);
                    double lon = center.optDouble("lng", lastLongitudeValue);
                    point = new LatLng(lat, lon);
                }

                if (overlay != null) {
                    LatLng finalPoint = point;
                    ExecutorManager.runOnMainThread(() -> overlay.setPosition(finalPoint));
                }
            } catch (JSONException e) {
                Log.e(LOG_TAG, "Error parsing ripple config");
            }
        });
    }

    @JavascriptInterface
    public void clearMap() {
        ExecutorManager.runOnMainThread(() -> googleMap.clear());
        circleRipples.clear();
        groundOverlays.clear();
    }


    private void checkAndAnimatePolyline(final String polyLineKey, final int staticColor, final String style, final int polylineWidth, PolylineOptions polylineOptions, JSONObject mapRouteConfigObject, final GoogleMap gMap, String gmapKey) {
        try {
            isAnimationNeeded = mapRouteConfigObject != null && mapRouteConfigObject.optBoolean("isAnimation", false);
            PolyLineAnimationTimers polylineAnimationTimerInstance = getPolyLineTimer(gmapKey, polyLineKey);
            Timer polylineAnimationTimer = polylineAnimationTimerInstance != null ? polylineAnimationTimerInstance.polyLineAnimationTimer : null;
            TimerTask polylineTimerTask = polylineAnimationTimerInstance != null ? polylineAnimationTimerInstance.polyAnimationTimerTask : null;
            if (!isAnimationNeeded || Utils.getDeviceRAM() <= 4) {
                if (polylineAnimationTimer != null) {
                    polylineAnimationTimer.cancel();
                }
                if (polylineTimerTask != null) {
                    polylineTimerTask.cancel();
                }
                setPolyLineDataByMapInstance(gmapKey, polyLineKey, setRouteCustomTheme(polylineOptions, staticColor, style, polylineWidth, mapRouteConfigObject, gMap, false, polyLineKey, gmapKey));
                return;
            }
            JSONObject polylineAnimationConfigObject = mapRouteConfigObject.optJSONObject("polylineAnimationConfig");
            if (polylineAnimationConfigObject == null) {
                polylineAnimationConfigObject = new JSONObject();
            }

            int animateColor = Color.parseColor(polylineAnimationConfigObject.optString("color", "#D1D5DB"));
            setPolyLineDataByMapInstance(gmapKey, polyLineKey, setRouteCustomTheme(polylineOptions, animateColor, style, polylineWidth, mapRouteConfigObject, gMap, false, polyLineKey, gmapKey));
            PolylineOptions overlayPolylineOptions = new PolylineOptions();
            setPolyLineDataByMapInstance(gmapKey, polyLineKey, setRouteCustomTheme(overlayPolylineOptions, animateColor, style, polylineWidth, null, gMap, true, polyLineKey, gmapKey));

            if (polylineAnimationTimer != null) {
                polylineAnimationTimer.cancel();
            } else {
                polylineAnimationTimer = new Timer();
            }

            if (polylineTimerTask != null) {
                polylineTimerTask.cancel();
            }
            polylineTimerTask = new TimerTask() {
                private float drawDone = 0;

                @Override
                public void run() {
                    if (drawDone <= 28) {
                        drawDone += 2;
                    } else if (drawDone <= 66) {
                        drawDone += 4;
                    } else if (drawDone <= 98) {
                        drawDone += 2;
                    } else if (drawDone <= 200) {
                        drawDone += 2;
                    } else {
                        drawDone = 0;
                    }


                    if (drawDone >= 0 && drawDone <= 100) { // Drawing Phase
                        ExecutorManager.runOnMainThread(() -> {
                            try {
                                PolylineDataPoints polyDataPoints = getPolyLineDataByMapInstance(gmapKey, polyLineKey);
                                Polyline polyline = getPolyLine(false, polyDataPoints);
                                Polyline overlayPolylines = getPolyLine(true, polyDataPoints);
                                if (polyline != null) {
                                    List<LatLng> foregroundPoints = polyline.getPoints();
                                    Collections.reverse(foregroundPoints);

                                    int pointCount = foregroundPoints.size();
                                    int countToBeRemoved = (int) (pointCount * (drawDone / 100.0f));
                                    foregroundPoints.subList(0, countToBeRemoved).clear();

                                    if (polyline.getColor() != animateColor)
                                        polyline.setColor(animateColor);
                                    if (overlayPolylines != null) {
                                        if (overlayPolylines.getColor() != staticColor)
                                            overlayPolylines.setColor(staticColor);
                                        overlayPolylines.setPoints(foregroundPoints);
                                    }
                                }
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        });
                    } else if (drawDone > 100 && drawDone <= 200) { // Fading Phase
                        ExecutorManager.runOnMainThread(() -> {
                            try {
                                PolylineDataPoints polylineDataPoints = getPolyLineDataByMapInstance(gmapKey, polyLineKey);
                                Polyline polyline = getPolyLine(false, polylineDataPoints);
                                float alpha = (float) ((drawDone - 100.0) / 100.0);
                                final float[] fromColor = new float[3], toColor = new float[3], currColor = new float[3];
                                Color.colorToHSV(animateColor, fromColor);
                                Color.colorToHSV(staticColor, toColor);

                                currColor[0] = fromColor[0] + (toColor[0] - fromColor[0]) * alpha;
                                currColor[1] = fromColor[1] + (toColor[1] - fromColor[1]) * alpha;
                                currColor[2] = fromColor[2] + (toColor[2] - fromColor[2]) * alpha;

                                int newColor = Color.HSVToColor(currColor);
                                if (polyline != null && polyline.getColor() != newColor)
                                    polyline.setColor(newColor);
                                polylineDataPoints.setPolyline(polyline);
                                setPolyLineDataByMapInstance(gmapKey, polyLineKey, polylineDataPoints);

                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        });
                    }
                }
            };

            polylineAnimationTimer.schedule(polylineTimerTask, 200, 15);
            polylineAnimationTimerInstance.intializePolyLineAnimationTimers(polylineAnimationTimer, polylineTimerTask);
            setPolylineAnimationTimers(gmapKey, polyLineKey, polylineAnimationTimerInstance);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @JavascriptInterface
    public void drawRouteV2(final String drawRouteConfig) {
        onMapUpdate(null, null);
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject drawRouteConfigObject = new JSONObject(drawRouteConfig);
                String purescriptId = drawRouteConfigObject.optString("pureScriptID", "");
                JSONArray routesConfigs = drawRouteConfigObject.getJSONArray("routeConfigs");
                System.out.println(routesConfigs);
                for (int j = 0, size = routesConfigs.length(); j < size; j++) {
                    JSONObject drawRouteEntity = routesConfigs.getJSONObject(j);
                    String key = drawRouteEntity.optString("routeKey", "DEFAULT");
                    JSONObject locations = drawRouteEntity.getJSONObject("locations");
                    JSONArray coordinates = locations.getJSONArray("points");

                    JSONObject startMarkerConfig = new JSONObject(drawRouteEntity.getString("startMarkerConfig"));
                    JSONObject endMarkerConfig = new JSONObject(drawRouteEntity.getString("endMarkerConfig"));
                    JSONArray stopMarkerConfigs = new JSONArray(drawRouteEntity.getString("stopMarkerConfigs"));
                    Double sourceAnchorVD = startMarkerConfig.optDouble("anchorV", 0.5);
                    Double sourceAnchorUD = startMarkerConfig.optDouble("anchorU", 0.5);
                    Double destinationMarkerAnchorVD = endMarkerConfig.optDouble("anchorV", 0.5);
                    Double destinationMarkerAnchorUD = endMarkerConfig.optDouble("anchorU", 0.5);
                    float destinationAnchorV = destinationMarkerAnchorVD.floatValue();
                    float destinationAnchorU = destinationMarkerAnchorUD.floatValue();
                    float sourceAnchorV = sourceAnchorVD.floatValue();
                    float sourceAnchorU = sourceAnchorUD.floatValue();
                    String sourceIcon = startMarkerConfig.optString("pointerIcon", "");
                    String destIcon = endMarkerConfig.optString("pointerIcon", "");
                    String sourceIconId = startMarkerConfig.optString("markerId", "");
                    String destIconId = endMarkerConfig.optString("markerId", "");

                    JSONObject startLabelImageConfig = startMarkerConfig.optJSONObject("labelImage");
                    JSONObject endLabelImageConfig = endMarkerConfig.optJSONObject("labelImage");
                    JSONObject startActionImageConfig = startMarkerConfig.optJSONObject("labelActionImage");
                    JSONObject endActionImageConfig = endMarkerConfig.optJSONObject("labelActionImage");
                    String startShortTitle = startMarkerConfig.optString("shortTitle", "");
                    String endShortTitle = endMarkerConfig.optString("shortTitle", "");
                    String startTheme = startMarkerConfig.optString("theme", "LIGHT");
                    String endTheme = endMarkerConfig.optString("theme", "LIGHT");
                    JSONObject sourceMarkerActionImageConfig = startMarkerConfig.optJSONObject("actionImage");
                    JSONObject destMarkerActionImageConfig = endMarkerConfig.optJSONObject("actionImage");
                    String srcMarkerCallback = startMarkerConfig.optString("markerCallback", "");
                    String destMarkerCallback = endMarkerConfig.optString("markerCallback", "");
                    int polylineWidth = drawRouteEntity.optInt("routeWidth", 8);

                    String style = drawRouteEntity.optString("style", "LineString");
                    String trackColor = drawRouteEntity.optString("routeColor", "#000000");
                    String type = drawRouteEntity.optString("routeType", "DRIVER_LOCATION_UPDATE");

                    JSONObject mapRouteConfigObject = drawRouteEntity.optJSONObject("mapRouteConfig");


                    boolean isActual = drawRouteEntity.optBoolean("isActual", true);

                    GoogleMap gMap = googleMapInstance.get(purescriptId);

                    if (gMap != null) {
                        setPolyLineDataByMapInstance(purescriptId, key, new PolylineDataPoints());
                        PolylineOptions polylineOptions = new PolylineOptions();
                        int color = Color.parseColor(trackColor);


                        try {
                            if (coordinates.length() <= 1) {
                                JSONObject coordinate = (JSONObject) coordinates.get(0);
                                double lng = coordinate.getDouble("lng");
                                double lat = coordinate.getDouble("lat");
                                int vehicleSizeTagIcon = mapRouteConfigObject != null ? mapRouteConfigObject.getInt("vehicleSizeTagIcon") : 90;
                                MarkerConfig mConfig = getMarkerConfigWithIdAndName(sourceIcon, sourceIconId);
                                upsertMarkerV2(mConfig, String.valueOf(lat), String.valueOf(lng), vehicleSizeTagIcon, sourceAnchorU, sourceAnchorV, purescriptId);
                                animateCameraV2(lat, lng, 20.0f, ZoomType.ZOOM, purescriptId);
                                return;
                            }

                            JSONObject sourceCoordinates = (JSONObject) coordinates.get(0);
                            JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length() - 1);

                            double sourceLat = sourceCoordinates.getDouble("lat");
                            double sourceLong = sourceCoordinates.getDouble("lng");
                            double destLat = destCoordinates.getDouble("lat");
                            double destLong = destCoordinates.getDouble("lng");


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

                            if (sourceLat != 0.0 && sourceLong != 0.0 && destLat != 0.0 && destLong != 0.0) {
                                double destinationLat = destLat;
                                double destinationLong = destLong;
                                moveCameraV2(sourceLat, sourceLong, destinationLat, destinationLong, coordinates, purescriptId);
                            }
                            MarkerConfig markerConfig = new MarkerConfig();
                            checkAndAnimatePolyline(key, color, style, polylineWidth, polylineOptions, mapRouteConfigObject, gMap, purescriptId);
                            // Destination Marker
                            if (!destIcon.equals("")) {
                                List<LatLng> points = polylineOptions.getPoints();
                                LatLng dest = points.get(0);
                                MarkerConfig destMarkerConfig = new MarkerConfig();
                                Boolean useMarkerSize = endMarkerConfig.optBoolean("useMarkerSize", false);
                                Double destMarkerSize = useMarkerSize ? endMarkerConfig.optDouble("markerSize", 0.0) : 0.0;
                                if (destMarkerActionImageConfig != null)
                                    destMarkerConfig.locationName(endMarkerConfig.optString("primaryText", ""), endMarkerConfig.optString("secondaryText", ""));
                                else destMarkerConfig.locationName("");
                                destMarkerConfig.setLabelImageConfig(endLabelImageConfig);
                                destMarkerConfig.setLabelActionImageConfig(endActionImageConfig);
                                destMarkerConfig.setTheme(endTheme);
                                destMarkerConfig.setLabelMaxWidth(endMarkerConfig.optInt("labelMaxWidth", 300));
                                destMarkerConfig.setLabelMaxLines(endMarkerConfig.optInt("labelMaxLines", 1));
                                destMarkerConfig.setLabelTextSize(endMarkerConfig.optInt("labelTextSize", 11));
                                destMarkerConfig.setShortTitle(endShortTitle);
                                if (destMarkerActionImageConfig != null)
                                    destMarkerConfig.setMarkerActionImageConfig(destMarkerActionImageConfig);
                                destMarkerConfig.setMarkerIconSize(destMarkerSize.intValue());
                                MarkerOptions markerObj = new MarkerOptions()
                                        .title("")
                                        .position(dest)
                                        .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(destIcon, false, MarkerType.NORMAL_MARKER_V2, destMarkerConfig)))
                                        .anchor(destinationAnchorU, destinationAnchorV);

                                Marker tempmarker = gMap.addMarker(markerObj);
                                if (!destMarkerCallback.equals("")) {
                                    tempmarker.setTag(destMarkerCallback);
                                    gMap.setOnMarkerClickListener(currMarker -> {
                                        if (currMarker.getTag() != null) {
                                            String js = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');", currMarker.getTag(), currMarker.getTitle());
                                            bridgeComponents.getJsCallback().addJsToWebView(js);
                                            return true;
                                        }
                                        return false;
                                    });
                                }
                                markers.put(destIconId, tempmarker);

                            }

                            if (!sourceIcon.equals("")) {
                                List<LatLng> points = polylineOptions.getPoints();
                                LatLng source = points.get(points.size() - 1);
                                if (type.equals("DRIVER_LOCATION_UPDATE")) {
                                    Polyline polyline = getPolyLine(false, getPolyLineDataByMapInstance(purescriptId, key));
                                    int vehicleSizeTagIcon = mapRouteConfigObject != null ? mapRouteConfigObject.getInt("vehicleSizeTagIcon") : 90;
                                    MarkerConfig mConfig = getMarkerConfigWithIdAndName(sourceIcon, sourceIconId);
                                    upsertMarkerV2(mConfig, String.valueOf(source.latitude), String.valueOf(source.longitude), vehicleSizeTagIcon, sourceAnchorU, sourceAnchorV, purescriptId);
                                    Marker currMarker = (Marker) markers.get(sourceIconId);
                                    float rotation = 0.0f;
                                    if (polyline != null) {
                                        int index = polyline.getPoints().size() - 1;
                                        rotation = (float) SphericalUtil.computeHeading(polyline.getPoints().get(index), polyline.getPoints().get(index - 1));
                                    }
                                    if (rotation != 0.0 && currMarker != null)
                                        currMarker.setRotation(rotation);
                                    currMarker.setAnchor(sourceAnchorU, sourceAnchorV);
                                    markers.put(sourceIconId, currMarker);
                                } else {
                                    markerConfig.locationName(startMarkerConfig.optString("primaryText", ""), startMarkerConfig.optString("secondaryText", ""));
                                    markerConfig.setLabelImageConfig(startLabelImageConfig);
                                    markerConfig.setTheme(startTheme);
                                    markerConfig.setLabelActionImageConfig(startActionImageConfig);
                                    markerConfig.setLabelMaxWidth(startMarkerConfig.optInt("labelMaxWidth", 300));
                                    markerConfig.setLabelMaxLines(startMarkerConfig.optInt("labelMaxLines", 0));
                                    markerConfig.setLabelTextSize(startMarkerConfig.optInt("labelTextSize", 11));
                                    markerConfig.setShortTitle(startShortTitle);
                                    if (sourceMarkerActionImageConfig != null)
                                        markerConfig.setMarkerActionImageConfig(sourceMarkerActionImageConfig);
                                    MarkerOptions markerObj = new MarkerOptions()
                                            .title("")
                                            .position(source)
                                            .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(sourceIcon, false, MarkerType.NORMAL_MARKER, markerConfig)));
                                    Marker tempmarker = gMap.addMarker(markerObj);
                                    if (!srcMarkerCallback.equals("")) {
                                        tempmarker.setTag(srcMarkerCallback);
                                        gMap.setOnMarkerClickListener(currMarker -> {
                                            if (currMarker.getTag() != null) {
                                                String js = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');", currMarker.getTag(), currMarker.getTitle());
                                                bridgeComponents.getJsCallback().addJsToWebView(js);
                                                return true;
                                            }
                                            return false;
                                        });
                                    }
                                    markers.put(sourceIcon, tempmarker);
                                }
                            }

                            for (int i = 0; i < stopMarkerConfigs.length(); i++) {
                                JSONObject stopMarkerConfig = stopMarkerConfigs.optJSONObject(i);
                                if (stopMarkerConfig != null) {
                                    String stopIcon = stopMarkerConfig.optString("pointerIcon", "");
                                    String stopIconId = stopMarkerConfig.optString("markerId", "");
                                    if (!stopIcon.equals("")) {
                                        Double stopMarkerSize = stopMarkerConfig.optDouble("markerSize", 90);
                                        boolean useSourcePoint = stopMarkerConfig.optBoolean("useSourcePoint", false);
                                        boolean useDestPoints = stopMarkerConfig.optBoolean("useDestPoints", true);
                                        boolean usePosition = stopMarkerConfig.optBoolean("usePosition", false);
                                        JSONObject position = stopMarkerConfig.optJSONObject("position");
                                        Double lat = position != null ? position.optDouble("lat", 0.0) : 0.0;
                                        Double lng = position != null ? position.optDouble("lng", 0.0) : 0.0;
                                        Double stopMarkerAnchorVD = stopMarkerConfig.optDouble("anchorV", 1.0);
                                        Double stopMarkerAnchorUD = stopMarkerConfig.optDouble("anchorU", 0.5);
                                        JSONObject stopMarkerActionImageConfig = stopMarkerConfig.optJSONObject("actionImage");
                                        float stopMarkerAnchorV = stopMarkerAnchorVD.floatValue();
                                        float stopMarkerAnchorU = stopMarkerAnchorUD.floatValue();
                                        String stopShortTitle = stopMarkerConfig.optString("shortTitle", "");
                                        List<LatLng> points = polylineOptions.getPoints();
                                        LatLng source = useSourcePoint ? points.get(points.size() - 1) : useDestPoints ? points.get(0) : new LatLng(lat, lng);
                                        MarkerConfig mConfig = getMarkerConfigWithIdAndName(stopIcon, stopIconId);
                                        String primaryText = stopMarkerConfig.optString("primaryText", "");
                                        String secondaryText = stopMarkerConfig.optString("secondaryText", "");
                                        mConfig.locationName(primaryText, secondaryText);
                                        mConfig.setLabelImageConfig(startLabelImageConfig);
                                        mConfig.setTheme(endTheme);
                                        mConfig.setLabelActionImageConfig(startActionImageConfig);
                                        mConfig.setLabelMaxWidth(stopMarkerConfig.optInt("labelMaxWidth", 300));
                                        mConfig.setLabelMaxLines(stopMarkerConfig.optInt("labelMaxLines", 2));
                                        mConfig.setLabelTextSize(stopMarkerConfig.optInt("labelTextSize", 11));
                                        mConfig.setShortTitle(stopShortTitle);
                                        mConfig.setMarkerIconSize(stopMarkerSize.intValue());
                                        if (stopMarkerActionImageConfig != null)
                                            mConfig.setMarkerActionImageConfig(stopMarkerActionImageConfig);
                                        MarkerOptions markerObj = new MarkerOptions()
                                                .title("")
                                                .position(source)
                                                .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(stopIcon, false, MarkerType.STOP_ICON_MARKER, mConfig)))
                                                .anchor(stopMarkerAnchorU, stopMarkerAnchorV);
                                        Marker currMarker = gMap.addMarker(markerObj);
                                        markers.put(stopIcon, currMarker);
                                    }
                                }
                            }
                        } catch (JSONException e) {
                            System.out.println("Exception inside drawRouteV2" + e);
                            e.printStackTrace();
                        }
                    }
                }
            } catch (JSONException e) {
                e.printStackTrace();
            }
        });
    }


    @JavascriptInterface
    public void drawRoute(final String json, final String style, final String trackColor, final boolean isActual, final String sourceMarker, final String destMarker, final int polylineWidth, String type, final String mapRouteConfig) {
        onMapUpdate(null, null);
        ExecutorManager.runOnMainThread(() -> {
            if (googleMap != null && !editLocation) {
                String key = "DEFAULT";
                String mapKey = "DEFAULT";
                setPolyLineDataByMapInstance("DEFAULT", "DEFAULT", new PolylineDataPoints());
                PolylineOptions polylineOptions = new PolylineOptions();
                int color = Color.parseColor(trackColor);
                try {
                    JSONObject jsonObject = new JSONObject(json);
                    JSONArray coordinates = jsonObject.getJSONArray("points");
                    JSONObject mapRouteConfigObject = new JSONObject(mapRouteConfig);
                    JSONObject sourceMarkerConfig = new JSONObject(sourceMarker);
                    JSONObject destMarkerConfig = new JSONObject(destMarker);
                    String sourceIcon = sourceMarkerConfig.optString("pointerIcon", "");
                    String destinationIcon = destMarkerConfig.optString("pointerIcon", "");
                    String sourceMarkerId = sourceMarkerConfig.optString("markerId", "");
                    String destMarkerId = destMarkerConfig.optString("markerId", "");
                    if (coordinates.length() <= 1) {
                        JSONObject coordinate = (JSONObject) coordinates.get(0);
                        double lng = coordinate.getDouble("lng");
                        double lat = coordinate.getDouble("lat");
                        int vehicleSizeTagIcon = mapRouteConfigObject.getInt("vehicleSizeTagIcon");
                        upsertMarker(sourceIcon, String.valueOf(lat), String.valueOf(lng), vehicleSizeTagIcon, 0.5f, 0.5f);
                        animateCamera(lat, lng, 20.0f, ZoomType.ZOOM);
                        return;
                    }
                    JSONObject sourceCoordinates = (JSONObject) coordinates.get(0);
                    JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length() - 1);
                    double sourceLat = sourceCoordinates.getDouble("lat");
                    double sourceLong = sourceCoordinates.getDouble("lng");
                    double destLat = destCoordinates.getDouble("lat");
                    double destLong = destCoordinates.getDouble("lng");
                    boolean moveCamera = mapRouteConfigObject.optBoolean("autoZoom", true);
                    if (sourceLat != 0.0 && sourceLong != 0.0 && destLat != 0.0 && destLong != 0.0 && moveCamera) {
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
                    String sourceSpecialTagIcon = sourceMarkerConfig.getString("labelImage");
                    String destinationSpecialTagIcon = destMarkerConfig.getString("labelImage");
                    String destinationLabelActionImage = destMarkerConfig.getString("labelActionImage");
                    MarkerConfig markerConfig = new MarkerConfig();

                    checkAndAnimatePolyline(key, color, style, polylineWidth, polylineOptions, mapRouteConfigObject, googleMap, "DEFAULT");

                    if (!destinationIcon.equals("")) {
                        List<LatLng> points = polylineOptions.getPoints();
                        LatLng dest = points.get(0);
                        markerConfig.locationName(destMarkerConfig.optString("primaryText", ""), destMarkerConfig.optString("secondaryText", ""));
                        markerConfig.setLabelImage(destinationSpecialTagIcon);
                        markerConfig.setLabelActionImage(destinationLabelActionImage);
                        MarkerOptions markerObj = new MarkerOptions()
                                .title("")
                                .position(dest)
                                .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(destinationIcon, false, MarkerType.NORMAL_MARKER, markerConfig)));
                        Marker tempmarker = googleMap.addMarker(markerObj);
                        markers.put(destMarkerId, tempmarker);
                    }

                    if (!sourceIcon.equals("")) {
                        List<LatLng> points = polylineOptions.getPoints();
                        LatLng source = points.get(points.size() - 1);
                        Polyline polyline = getPolyLine(false, getPolyLineDataByMapInstance(mapKey, key));
                        ;
                        if (type.equals("DRIVER_LOCATION_UPDATE")) {
                            int vehicleSizeTagIcon = mapRouteConfigObject.getInt("vehicleSizeTagIcon");
                            upsertMarker(sourceIcon, String.valueOf(source.latitude), String.valueOf(source.longitude), vehicleSizeTagIcon, 0.5f, 0.5f);
                            Marker currMarker = (Marker) markers.get(sourceMarkerId);
                            int index = polyline.getPoints().size() - 1;
                            float rotation = (float) SphericalUtil.computeHeading(polyline.getPoints().get(index), polyline.getPoints().get(index - 1));
                            if (rotation != 0.0) currMarker.setRotation(rotation);
                            currMarker.setAnchor(0.5f, 0.5f);
                            markers.put(sourceMarkerId, currMarker);
                        } else {
                            markerConfig.setLabelImage(sourceSpecialTagIcon);
                            MarkerOptions markerObj = new MarkerOptions()
                                    .title("")
                                    .position(source)
                                    .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(sourceIcon, false, MarkerType.NORMAL_MARKER, markerConfig)));
                            Marker tempmarker = googleMap.addMarker(markerObj);
                            markers.put(sourceMarkerId, tempmarker);
                        }
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @JavascriptInterface
    public void updateMarker(final String markerConfigString) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (googleMap != null) {
                    JSONObject markerObject = new JSONObject(markerConfigString);
                    JSONObject position = markerObject.optJSONObject("position");
                    if (position != null && !(position.optDouble("lat", 0.0) == 0.0) && !(position.optDouble("lat", 0.0) == 0.0) && !markerObject.optString("pointerIcon").equals("")) {
                        removeMarker(markerObject.optString("pointerIcon", ""));
                        LatLng latLng = new LatLng(position.optDouble("lat", 0.0), position.optDouble("lng", 0.0));

                        MarkerConfig markerConfig = new MarkerConfig();
                        markerConfig.locationName(markerObject.optString("primaryText", ""), markerObject.optString("secondaryText", ""));
                        markerConfig.setLabelImage(markerObject.optString("labelImage"));
                        markerConfig.setLabelActionImage(markerObject.optString("labelActionImage"));

                        MarkerOptions markerObj = new MarkerOptions()
                                .title("")
                                .position(latLng)
                                .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(markerObject.optString("pointerIcon"), !markerObject.optBoolean("showPointer", false), MarkerType.NORMAL_MARKER, markerConfig)));
                        Marker marker = googleMap.addMarker(markerObj);
                        markers.put(markerObject.optString("markerId", ""), marker);
                    }
                }
            } catch (JSONException e) {
                e.printStackTrace();
            }
        });
    }


    @JavascriptInterface
    public void updateMarkerV2(final String markerConfigString) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (googleMap != null) {
                    JSONObject markerObject = new JSONObject(markerConfigString);
                    JSONObject position = markerObject.optJSONObject("position");
                    if (position != null && !(position.optDouble("lat", 0.0) == 0.0) && !(position.optDouble("lat", 0.0) == 0.0) && !markerObject.optString("pointerIcon").equals("")) {
                        removeMarker(markerObject.optString("pointerIcon", ""));
                        LatLng latLng = new LatLng(position.optDouble("lat", 0.0), position.optDouble("lng", 0.0));

                        MarkerConfig markerConfig = new MarkerConfig();
                        markerConfig.locationName(markerObject.optString("primaryText", ""), markerObject.optString("secondaryText", ""));
                        markerConfig.setLabelImageConfig(markerObject.optJSONObject("labelImage"));
                        markerConfig.setLabelActionImageConfig(markerObject.optJSONObject("labelActionImage"));
                        markerConfig.setTheme(markerObject.optString("theme", "LIGHT"));
                        markerConfig.setMarkerCallback(markerObject.optString("markerCallback", ""));
                        markerConfig.setLabelMaxWidth(markerObject.optInt("labelMaxWidth", 300));
                        markerConfig.setLabelMaxLines(markerObject.optInt("labelMaxLines", 1));
                        markerConfig.setLabelTextSize(markerObject.optInt("labelTextSize", 11));
                        markerConfig.setShortTitle(markerObject.optString("shortTitle", ""));
                        String markerTitle = markerObject.optString("pointerIcon", "");
                        MarkerOptions markerObj = new MarkerOptions()
                                .title(markerTitle)
                                .position(latLng)
                                .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(markerObject.optString("pointerIcon"), !markerObject.optBoolean("showPointer", false), MarkerType.NORMAL_MARKER, markerConfig)));
                        Marker marker = googleMap.addMarker(markerObj);
                        markers.put(markerTitle, marker);
                        if (marker != null && !markerConfig.markerCallback.equals("")) {
                            marker.setTag(markerConfig.markerCallback);
                            googleMap.setOnMarkerClickListener(m -> {
                                if (m.getTag() != null) {
                                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');", m.getTag(), m.getTitle());
                                    JsCallback jsCallback = bridgeComponents.getJsCallback();
                                    if (jsCallback != null) {
                                        jsCallback.addJsToWebView(javascript);
                                    }
                                }
                                return true;
                            });
                        }
                    }
                }
            } catch (JSONException e) {
                e.printStackTrace();
            }
        });
    }

    @SuppressLint({"UseCompatLoadingForDrawables", "SetTextI18n", "LongLogTag"})
    protected Bitmap getMarkerBitmapFromView(String pointerImage, boolean isInvisiblePointer, MarkerType markerType, MarkerConfig markerConfig) {
        Context context = bridgeComponents.getContext();
        @SuppressLint("InflateParams")

        String textColor = markerConfig.theme.equals(Theme.DARK) ? "#FFFFFF" : "#454545";
        String backgroundColor = markerConfig.theme.equals(Theme.DARK) ? "#454545" : "#FFFFFF";
        View customMarkerView = ((LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(context.getResources().getLayout(context.getResources().getIdentifier("marker_label_layout", "layout", context.getPackageName())), null);
        try {
            setMarkerBackground(backgroundColor, customMarkerView);
            setMarkerText(textColor, customMarkerView, markerConfig);
            setMarkerlabelImage(markerConfig.labelImage, customMarkerView);
            setMarkerPointerImage(pointerImage, isInvisiblePointer, markerType, customMarkerView, markerConfig);
            setMarkerActionImage(markerConfig.markerActionImage, markerConfig.primaryText, customMarkerView);
            setLabelImageAction(customMarkerView, markerConfig.labelActionImage);
        } catch (Exception e) {
            Log.e("getMarkerBitmapFromView", "Exception in rendering Image" + e);
        }
        LinearLayout main_label_layout = customMarkerView.findViewById(R.id.main_label_layout);
        if (main_label_layout != null && !markerConfig.labelActionImage.image.equals(""))
            main_label_layout.setPadding(main_label_layout.getPaddingLeft(), main_label_layout.getPaddingTop(), 2, main_label_layout.getPaddingBottom());
        customMarkerView.measure(View.MeasureSpec.UNSPECIFIED, View.MeasureSpec.UNSPECIFIED);
        customMarkerView.layout(0, 0, customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight());
        customMarkerView.buildDrawingCache();
        Bitmap returnedBitmap = Bitmap.createBitmap(customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight(), Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(returnedBitmap);
        canvas.drawColor(Color.WHITE, PorterDuff.Mode.SRC_IN);
        Drawable drawable = customMarkerView.getBackground();
        if (drawable != null)
            drawable.draw(canvas);
        customMarkerView.draw(canvas);
        return returnedBitmap;
    }

    @SuppressLint("JavascriptInterface")
    @JavascriptInterface
    public void onMapUpdate(String isIdleCallback, String isMovedCallback) {
        try {
            MapRemoteConfig mapRemoteConfig = getMapRemoteConfig();
            if (mapRemoteConfig.enableMapRecenter) {
                if (app == AppType.CONSUMER) {
                    ExecutorManager.runOnMainThread(() -> {
                        try {
                            JsCallback jsCallback = bridgeComponents.getJsCallback();
                            if (!mapUpdate.isIdleListenerActive) {
                                googleMap.setOnCameraIdleListener(() -> {
                                    if (isIdleCallback != null && jsCallback != null) {
                                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s');", isIdleCallback);
                                        jsCallback.addJsToWebView(javascript);
                                    }
                                    mapUpdate.mapRecenterHandler.removeCallbacksAndMessages(null);
                                    mapUpdate.mapRecenterHandler.postDelayed(() -> {
                                        if (mapUpdate.isGestureMovement)
                                            mapUpdate.isMapMoved = true;
                                        mapUpdate.isMapIdle = true;
                                    }, mapRemoteConfig.recenterDelay);
                                });
                                mapUpdate.isIdleListenerActive = true;
                            }
                            if (!mapUpdate.isMoveListenerActive) {
                                googleMap.setOnCameraMoveStartedListener(reason -> {
                                    if (isMovedCallback != null && jsCallback != null) {
                                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d);", isMovedCallback, reason);
                                        jsCallback.addJsToWebView(javascript);
                                    }
                                    if (reason == GoogleMap.OnCameraMoveStartedListener.REASON_GESTURE) {
                                        mapUpdate.isGestureMovement = true;
                                        mapUpdate.isMapIdle = false;
                                    } else {
                                        mapUpdate.isGestureMovement = false;
                                    }
                                    mapUpdate.mapRecenterHandler.removeCallbacksAndMessages(null);
                                });
                                mapUpdate.isMoveListenerActive = true;
                            }
                        } catch (Exception e) {
                            Log.e(LOG_TAG, "Error in onMapUpdate Executor " + e);
                        }
                    });
                }
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in onMapUpdate ", e);
        }
    }

    @JavascriptInterface
    public void removeOnMapUpdate() {
        try {
            if (googleMap != null) {
                if (mapUpdate.isIdleListenerActive) {
                    googleMap.setOnCameraIdleListener(null);
                    mapUpdate.isIdleListenerActive = false;
                }
                if (mapUpdate.isMoveListenerActive) {
                    googleMap.setOnCameraMoveStartedListener(null);
                    mapUpdate.isMoveListenerActive = false;
                }
                Log.e(LOG_TAG, "MapRecenter Removed");
            }
            mapUpdate.mapRecenterHandler.removeCallbacksAndMessages(null);
            mapUpdate.isMapMoved = false;
            mapUpdate.isMapIdle = true;
            mapUpdate.isGestureMovement = false;
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in removeOnMapUpdate " + e);
        }
    }

    private void setLabelImageAction(View customMarkerView, MarkerImageConfig labelActionImage) {
        try {
            if (labelActionImage != null && !labelActionImage.image.equals("")) {
                Context context = bridgeComponents.getContext();
                ImageView imageView = customMarkerView.findViewById(R.id.label_image_action);
                if (imageView != null) {
                    imageView.setVisibility(View.VISIBLE);
                    imageView.setImageDrawable(context.getResources().getDrawable(context.getResources().getIdentifier(labelActionImage.image, "drawable", context.getPackageName())));
                    int height = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, labelActionImage.height, context.getResources().getDisplayMetrics());
                    int width = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, labelActionImage.width, context.getResources().getDisplayMetrics());
                    LinearLayout.LayoutParams layoutParams = new LinearLayout.LayoutParams(width, height);
                    imageView.setLayoutParams(layoutParams);
                }
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in setLabelImageAction ", e);
        }
    }

    private void setMarkerActionImage(MarkerActionImageConfig actionImageConfig, String primaryText, View customMarkerView) {
        Context context = bridgeComponents.getContext();
        String actionImage = actionImageConfig.image;
        String orientation = actionImageConfig.orientation;
        String imageBackground = actionImageConfig.background;
        int height = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, actionImageConfig.height, context.getResources().getDisplayMetrics());
        int width = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, actionImageConfig.width, context.getResources().getDisplayMetrics());
        MarkerEdgeInsets layoutMargin = actionImageConfig.layoutMargin;
        MarkerEdgeInsets padding = actionImageConfig.padding;
        MarkerEdgeInsets layoutPadding = actionImageConfig.layoutPadding;
        LinearLayout mainLabelLayout = customMarkerView.findViewById(R.id.main_label_layout);
        if (orientation.equals("HORIZONTAL"))
            mainLabelLayout.setOrientation(LinearLayout.HORIZONTAL);
        else
            mainLabelLayout.setOrientation(LinearLayout.VERTICAL);

        if (layoutPadding.left != -1 && layoutPadding.top != -1 && layoutPadding.right != -1 && layoutPadding.bottom != -1)
            mainLabelLayout.setPadding(layoutPadding.left, layoutPadding.top, layoutPadding.right, layoutPadding.bottom);

        View ImageAndTextView = customMarkerView.findViewById(R.id.zone_image_and_text);
        if (layoutMargin.left != -1 && layoutMargin.top != -1 && layoutMargin.right != -1 && layoutMargin.bottom != -1) {
            LinearLayout.LayoutParams lp = (LinearLayout.LayoutParams) ImageAndTextView.getLayoutParams();
            lp.setMargins(layoutMargin.left, layoutMargin.top, layoutMargin.right, layoutMargin.bottom);
        }

        if (!actionImage.equals("")) {
            ImageView markerActionImage = customMarkerView.findViewById(R.id.marker_action_image);
            LinearLayout parent = customMarkerView.findViewById(R.id.marker_action_image_parent);
            if (padding.left != -1 && padding.top != -1 && padding.right != -1 && padding.bottom != -1) {
                markerActionImage.setPadding(padding.left, padding.top, padding.right, padding.bottom);
            }
            parent.setVisibility(View.VISIBLE);
            if (!imageBackground.equals("")) {
                parent.setLayoutParams(new LinearLayout.LayoutParams(
                        LinearLayout.LayoutParams.WRAP_CONTENT,
                        LinearLayout.LayoutParams.MATCH_PARENT
                ));
                parent.setBackground(context.getResources().getDrawable(context.getResources().getIdentifier(imageBackground, "drawable", context.getPackageName())));
                parent.setClickable(true);
            }
            if (height != 0 && width != 0) {
                ViewGroup.LayoutParams layoutParams = markerActionImage.getLayoutParams();
                layoutParams.height = height;
                layoutParams.width = width;
            }
            markerActionImage.setVisibility(View.VISIBLE);
            markerActionImage.setImageDrawable(context.getResources().getDrawable(context.getResources().getIdentifier(actionImage, "drawable", context.getPackageName())));
        }
        if (actionImage.equals("") && primaryText.equals("")) {
            View mainLableLayout = customMarkerView.findViewById(R.id.main_label_layout);
            mainLableLayout.setVisibility(View.GONE);
        }
    }

    private void setMarkerPointerImage(String pointerImage, Boolean isInvisiblePointer, MarkerType markerType, View customMarkerView, MarkerConfig markerConfig) {
        Context context = bridgeComponents.getContext();
        float markerRotation = markerConfig.rotation;
        int markerSize = markerConfig.markerIconSize;
        ImageView pointer = customMarkerView.findViewById(R.id.pointer_img);
        pointer.setRotation(markerRotation);
        if (pointerImage != null)
            pointer.setImageDrawable(context.getResources().getDrawable(context.getResources().getIdentifier(pointerImage, "drawable", context.getPackageName())));
        if (isInvisiblePointer)
            pointer.setVisibility(View.INVISIBLE);
        if (markerType.equals(MarkerType.SPECIAL_ZONE_MARKER)) {
            ViewGroup.LayoutParams layoutParams = pointer.getLayoutParams();
            layoutParams.height = 55;
            layoutParams.width = 55;
            pointer.setLayoutParams(layoutParams);
        } else if ((markerType.equals(MarkerType.STOP_ICON_MARKER) || markerType.equals(MarkerType.NORMAL_MARKER_V2)) && markerSize != 0) {
            ViewGroup.LayoutParams layoutParams = pointer.getLayoutParams();
            layoutParams.height = markerSize;
            layoutParams.width = markerSize;
            pointer.setLayoutParams(layoutParams);
        } else {
            if (pointerImage != null && pointerImage.equals("ny_ic_customer_current_location")) {
                ViewGroup.LayoutParams layoutParams = pointer.getLayoutParams();
                layoutParams.height = 160;
                layoutParams.width = 160;
                pointer.setImportantForAccessibility(2);
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P)
                    pointer.setAccessibilityHeading(false);
                pointer.setLayoutParams(layoutParams);
            }
        }
    }

    private void setMarkerlabelImage(MarkerImageConfig labelImageConfig, View customMarkerView) {
        try {
            Context context = bridgeComponents.getContext();
            if (labelImageConfig != null && !labelImageConfig.image.equals("")) {
                ImageView labelImage = customMarkerView.findViewById(R.id.zone_image);
                if (labelImage != null) {
                    labelImage.setVisibility(View.VISIBLE);
                    int imageID = context.getResources().getIdentifier(labelImageConfig.image, "drawable", bridgeComponents.getContext().getPackageName());
                    BitmapDrawable bitmap = (BitmapDrawable) context.getResources().getDrawable(imageID);
                    labelImage.setImageDrawable(bitmap);
                    int height = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, labelImageConfig.height, context.getResources().getDisplayMetrics());
                    int width = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, labelImageConfig.width, context.getResources().getDisplayMetrics());
                    LinearLayout.LayoutParams layoutParams = new LinearLayout.LayoutParams(width, height);
                    labelImage.setLayoutParams(layoutParams);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void setMarkerText(String textColor, View customMarkerView, MarkerConfig markerConfig) {
        try {
            View ImageAndTextView = customMarkerView.findViewById(R.id.zone_image_and_text);
            View labelTextViews = ImageAndTextView.findViewById(R.id.label_texts);
            String primaryText = markerConfig.primaryText;
            String shortTitle = markerConfig.shortTitle;
            String secondaryText = markerConfig.secondaryText;
            CustomTextView primaryTextView = labelTextViews.findViewById(R.id.primary_marker_text);
            CustomTextView secondaryTextView = labelTextViews.findViewById(R.id.secondary_marker_text);
            if (primaryText.equals("")) {
                ImageAndTextView.setVisibility(View.GONE);
            } else {
                String labelText = shortTitle != null && !shortTitle.equals("") ? shortTitle : primaryText;
                primaryTextView.setText(labelText);
                if (markerConfig.labelMaxWidth != 0)
                    primaryTextView.setMaxWidth(markerConfig.labelMaxWidth);
                if (secondaryText.equals(""))
                    primaryTextView.setMaxLines(markerConfig.labelMaxLines);

                primaryTextView.setTextSize(TypedValue.COMPLEX_UNIT_DIP, markerConfig.labelTextSize);
                primaryTextView.setImportantForAccessibility(TextView.IMPORTANT_FOR_ACCESSIBILITY_YES);
                primaryTextView.setContentDescription(primaryText);
                primaryTextView.setTextColor(Color.parseColor(textColor));
            }
            if (!secondaryText.equals("")) {
                secondaryTextView.setText(secondaryText);
                secondaryTextView.setImportantForAccessibility(TextView.IMPORTANT_FOR_ACCESSIBILITY_YES);
                secondaryTextView.setContentDescription(secondaryText);
                secondaryTextView.setVisibility(View.VISIBLE);
                if (markerConfig.labelMaxWidth != 0)
                    secondaryTextView.setMaxWidth(markerConfig.labelMaxWidth);
                secondaryTextView.setTextColor(Color.parseColor(textColor));
            } else {
                ViewGroup.LayoutParams labelTextsViewLayoutParams = labelTextViews.getLayoutParams();
                labelTextsViewLayoutParams.height = ViewGroup.LayoutParams.MATCH_PARENT;
                labelTextsViewLayoutParams.width = ViewGroup.LayoutParams.MATCH_PARENT;
                labelTextViews.setLayoutParams(labelTextsViewLayoutParams);
                ViewGroup.LayoutParams primaryTextViewLayoutParams = primaryTextView.getLayoutParams();
                primaryTextViewLayoutParams.height = ViewGroup.LayoutParams.MATCH_PARENT;
                primaryTextViewLayoutParams.width = ViewGroup.LayoutParams.MATCH_PARENT;
                primaryTextView.setLayoutParams(primaryTextViewLayoutParams);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    private void setMarkerBackground(String backgroundColor, View customMarkerView) {
        Context context = bridgeComponents.getContext();
        View background = customMarkerView.findViewById(R.id.main_label_layout);
        GradientDrawable drawable = (GradientDrawable) context.getResources().getDrawable(context.getResources().getIdentifier("ic_grey_border", "drawable", context.getPackageName()));
        drawable.setColor(Color.parseColor(backgroundColor));
        background.setBackground(drawable);
    }

    @JavascriptInterface
    public void removeAllPolylines(String removePolyLineConfig) {
        ExecutorManager.runOnMainThread(() -> {
            System.out.println("Inside remove all polylines" + removePolyLineConfig);
            JSONArray jsonMarkersArray = null;
            JSONObject jsonObject = null;
            JSONArray jsonPolylineArray = null;
            String jsonGMapKey = null;
            Hashtable<String, PolylineDataPoints> polylineData = null;
            ArrayList<String> mapkeys = new ArrayList<>();
            if (removePolyLineConfig.equals("")) {
                // TODO:: TO BE DEPRECATED AS THIS BLOCK OF CODE IS NOT IN USE
                removeMarker("ic_auto_nav_on_map");
                removeMarker("ny_ic_vehicle_nav_on_map");
                removeMarker("ny_ic_black_yellow_auto");
                removeMarker("ny_ic_bike_delivery_nav_on_map");
                removeMarker("ny_ic_src_marker");
                removeMarker("ny_ic_dest_marker");
                removeMarker("ny_ic_blue_marker");
                removeMarker("");
                if (polylinesByMapInstance != null) {
                    for (String i : polylinesByMapInstance.keySet()) {
                        mapkeys.add(i);
                    }
                }
            } else {
                try {
                    jsonObject = new JSONObject(removePolyLineConfig);
                    jsonMarkersArray = jsonObject.getJSONArray("markersToRemove");
                    jsonPolylineArray = jsonObject.getJSONArray("polyLinesToRemove");
                    jsonGMapKey = jsonObject.optString("gmapPursId", "DEFAULT");
                    mapkeys.add(jsonGMapKey);
                } catch (JSONException e) {
                    try {
                        jsonMarkersArray = new JSONArray(removePolyLineConfig);
                        if (jsonMarkersArray == null || (jsonMarkersArray != null && jsonMarkersArray.length() <= 0)) {
                            removeAllMarkers();
                        }
                        if (polylinesByMapInstance != null) {
                            for (String i : polylinesByMapInstance.keySet()) {
                                mapkeys.add(i);
                            }
                        }
                    } catch (JSONException k) {
                        k.printStackTrace();
                    }
                }
            }
            isAnimationNeeded = false;
            removeOnMapUpdate();

            for (int i = 0; jsonMarkersArray != null && i < jsonMarkersArray.length(); i++) {
                removeMarker(jsonMarkersArray.optString(i, ""));
            }
            for (int i = 0; jsonPolylineArray != null && jsonGMapKey != null && polylineData != null && i < jsonPolylineArray.length(); i++) {
                polylineData = polylinesByMapInstance.get(jsonGMapKey);
                if (polylineData != null) polylineData.remove(jsonPolylineArray.optString(i, ""));
            }

            if (polylinesByMapInstance != null && (jsonPolylineArray == null || (jsonPolylineArray != null && jsonPolylineArray.length() <= 0))) {
                for (String mapKey : mapkeys) {
                    polylineData = polylinesByMapInstance.get(mapKey);
                    if (polylineData != null) {
                        for (PolylineDataPoints i : polylineData.values()) {
                            if (i != null) {
                                Polyline polyline = i.polyline;
                                Polyline overlayPolylines = i.overlayPolylines;

                                if (polyline != null) {
                                    polyline.remove();
                                }
                                if (overlayPolylines != null) {
                                    overlayPolylines.remove();
                                }
                            }
                        }
                        polylineData.clear();
                        polylinesByMapInstance.put(mapKey, polylineData);
                    }
                }

            }
            if (polylinesByMapInstance != null && polylineData != null && jsonGMapKey != null) {
                polylinesByMapInstance.put(jsonGMapKey, polylineData);
            }

            for (String mapKey : mapkeys) {
                if (polylineAnimationTimers != null) {
                    Hashtable<String, PolyLineAnimationTimers> polylineAnimationTimerEntries = polylineAnimationTimers.get(mapKey);
                    if (polylineAnimationTimerEntries != null) {
                        for (PolyLineAnimationTimers polylineAnimationTimer : polylineAnimationTimerEntries.values()) {
                            if (polylineAnimationTimer != null) {
                                if (polylineAnimationTimer.polyLineAnimationTimer != null) {
                                    polylineAnimationTimer.polyLineAnimationTimer.cancel();
                                }
                                if (polylineAnimationTimer.polyAnimationTimerTask != null) {
                                    polylineAnimationTimer.polyAnimationTimerTask.cancel();
                                }
                            }
                        }
                        polylineAnimationTimerEntries.clear();
                        polylineAnimationTimers.remove(mapKey);
                    }
                }
            }
            removeAllCircles();
        });
    }

    @JavascriptInterface
    public void moveCamera(final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude, final JSONArray json_coordinates) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                double source_lat, source_lng, destination_lat, destination_lng;

                Log.i(MAPS, "json_coordinates" + json_coordinates);
                ArrayList<Double> all_latitudes = new ArrayList<>();
                ArrayList<Double> all_longitudes = new ArrayList<>();
                for (int i = 0; i < json_coordinates.length(); i++) {
                    JSONObject each_json_coordinates;
                    try {
                        each_json_coordinates = (JSONObject) json_coordinates.get(i);
                        double lon = each_json_coordinates.getDouble("lng");
                        double lat = each_json_coordinates.getDouble("lat");
                        all_latitudes.add(lat);
                        all_longitudes.add(lon);
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
                Log.i(MAPS, "all_latitudes" + (all_latitudes));
                Log.i(MAPS, "all_longitudes" + (all_longitudes));
                double minimum_latitude = Collections.min(all_latitudes);
                double maximum_latitude = Collections.max(all_latitudes);
                double minimum_longitude = Collections.min(all_longitudes);
                double maximum_longitude = Collections.max(all_longitudes);
                Log.i(MAPS, String.valueOf(minimum_latitude));
                Log.i(MAPS, String.valueOf(maximum_latitude));

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
                Log.i(MAPS, "Coordinates Points" + json_coordinates);
                if (googleMap != null) {
                    try {
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                        if (json_coordinates.length() < 5) {
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400), animationDuration, null);
                        } else {
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150), animationDuration, null);
                        }
                    } catch (IllegalArgumentException e) {
                        Log.i(MAPS, "Exception in Move camera" + e);
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                        if (json_coordinates.length() < 5) {
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400), animationDuration, null);
                        } else {
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150), animationDuration, null);
                        }
                    } catch (Exception e) {
                        Log.i(MAPS, "Exception in Move camera" + e);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    public PolylineDataPoints setRouteCustomTheme(PolylineOptions options, int color, String style, final int width, JSONObject mapRouteConfigObject, GoogleMap gMap, Boolean isOverLine, String key, String gmapKey) {
        PatternItem DOT = new Dot();
        PatternItem GAP = new Gap(10);
        options.width(width);
        List<PatternItem> PATTERN_POLYLINE_DOTTED = Arrays.asList(GAP, DOT);
        List<PatternItem> PATTERN_POLYLINE_DASHED_WITH_GAP = Collections.singletonList(new Dash(20));
        if (mapRouteConfigObject != null) {
            if (mapRouteConfigObject.has("dashUnit") && mapRouteConfigObject.has("gapUnit")) {
                int gap = mapRouteConfigObject.optInt("gapUnit", 0);
                int dash = mapRouteConfigObject.optInt("dashUnit", 1);
                PATTERN_POLYLINE_DASHED_WITH_GAP = Arrays.asList(new Dash(dash), new Gap(gap));
            }
        }
        options.color(color);
        switch (style) {
            case "DASH":
                options.pattern(PATTERN_POLYLINE_DASHED_WITH_GAP);
                break;
            case "DOT":
                options.pattern(PATTERN_POLYLINE_DOTTED);
                break;
            default:
                break;
        }
        Polyline polylineData = gMap != null ? gMap.addPolyline(options) : googleMap.addPolyline(options);
        PolylineDataPoints polylineDataPoints = getPolyLineDataByMapInstance(gmapKey, key);
        if (isOverLine) {
            polylineDataPoints.setOverlayPolylines(polylineData);
        } else {
            polylineDataPoints.setPolyline(polylineData);
        }
        return polylineDataPoints;
    }
    // endregion

    public void moveCameraV2(final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude, final JSONArray json_coordinates, final String puresciptID) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                double source_lat, source_lng, destination_lat, destination_lng;
                GoogleMap gMap = (googleMapInstance.get(puresciptID) != null) ? googleMapInstance.get(puresciptID) : googleMap;
                Log.i(MAPS, "json_coordinates" + json_coordinates);
                ArrayList<Double> all_latitudes = new ArrayList<>();
                ArrayList<Double> all_longitudes = new ArrayList<>();
                for (int i = 0; i < json_coordinates.length(); i++) {
                    JSONObject each_json_coordinates;
                    try {
                        each_json_coordinates = (JSONObject) json_coordinates.get(i);
                        double lon = each_json_coordinates.getDouble("lng");
                        double lat = each_json_coordinates.getDouble("lat");
                        all_latitudes.add(lat);
                        all_longitudes.add(lon);
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
                Log.i(MAPS, "all_latitudes" + (all_latitudes));
                Log.i(MAPS, "all_longitudes" + (all_longitudes));
                double minimum_latitude = Collections.min(all_latitudes);
                double maximum_latitude = Collections.max(all_latitudes);
                double minimum_longitude = Collections.min(all_longitudes);
                double maximum_longitude = Collections.max(all_longitudes);
                Log.i(MAPS, String.valueOf(minimum_latitude));
                Log.i(MAPS, String.valueOf(maximum_latitude));

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
                Log.i(MAPS, "Coordinates Points" + json_coordinates);
                if (gMap != null) {
                    try {
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                        if (json_coordinates.length() < 5) {
                            gMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400), animationDuration, null);
                        } else {
                            gMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150), animationDuration, null);
                        }
                    } catch (IllegalArgumentException e) {
                        Log.i(MAPS, "Exception in Move camera" + e);
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                        if (json_coordinates.length() < 5) {
                            gMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400), animationDuration, null);
                        } else {
                            gMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150), animationDuration, null);
                        }
                    } catch (Exception e) {
                        Log.i(MAPS, "Exception in Move camera" + e);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public String getDeviceID() {
        DeviceIdentifier androidIdentifier = new DeviceIdentifier();
        String deviceId = androidIdentifier.getDeviceId(bridgeComponents.getContext());
        return deviceId;
    }

    @JavascriptInterface
    public String getAndroidId() {
        String androidId = DeviceIdentifier.getAndroidId(bridgeComponents.getContext());
        if (DeviceIdentifier.isValid(androidId)) {
            return androidId;
        } else {
            return "NO_ANDROID_ID";
        }
    }

    @JavascriptInterface
    public void showMap(final String pureScriptId, boolean isEnableCurrentLocation, final String mapType, final float zoom, final String callback, final String mapConfig) {
        try {
            System.out.println("Inside showMap 123");
            ExecutorManager.runOnMainThread(() -> {
                if (bridgeComponents.getActivity() != null) {
                    try {
                        View view = bridgeComponents.getActivity().findViewById(Integer.parseInt(pureScriptId));
                        if (view == null) return;
                        JSONObject googleMapConfig = new JSONObject(mapConfig);
                        animationDuration = googleMapConfig.optInt("animationDuration", 400);
                        locateOnMapConfig = googleMapConfig.optJSONObject("locateOnMapConfig");
                        labelTextSize = googleMapConfig.optInt("labelTextSize", 30);
                        SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                        FragmentManager supportFragmentManager = ((FragmentActivity) bridgeComponents.getActivity()).getSupportFragmentManager();
                        FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                        fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                        fragmentTransaction.commitAllowingStateLoss();
                        getMapAsync(mapFragment, isEnableCurrentLocation, mapType, callback, pureScriptId, zoom);
                    } catch (Exception e) {
                        Log.e(MAPS, "Error in showMap " + e);
                        e.printStackTrace();
                    }
                }
            });
        } catch (Exception e) {
            Log.e("ADD_MARKER", e.toString());
        }
    }

    @JavascriptInterface
    public String isCoordOnPath(String json, double currLat, double currLng, int speed) throws JSONException {
        LatLng currPoint = new LatLng(currLat, currLng);
        ArrayList<LatLng> path = new ArrayList<>();
        JSONObject jsonObject = new JSONObject(json);
        int distanceRemaining;
        JSONArray coordinates = jsonObject.getJSONArray("points");
        int eta;
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
        double locationOnPathThres = Double.parseDouble(getKeysInSharedPref("ACCURACY_THRESHOLD").equals("__failed") ? "30.0" : getKeysInSharedPref("ACCURACY_THRESHOLD"));
        resultIndex = PolyUtil.locationIndexOnEdgeOrPath(currPoint, path, PolyUtil.isClosedPolygon(path), true, locationOnPathThres);
        if (resultIndex == -1) {
            result.put("points", coordinates);
            result.put("eta", 0);
            result.put("distance", 0);
            result.put("isInPath", false);
        } else if (resultIndex == (path.size() - 2) || resultIndex == (path.size() - 1)) {
            distanceRemaining = (int) SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            result.put("points", coordinates);
            result.put("eta", eta);
            result.put("distance", distanceRemaining);
            result.put("isInPath", true);
        } else if (resultIndex == 0) {
            path.clear();
            result.put("points", new JSONArray());
            result.put("eta", 0);
            result.put("distance", 0);
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

    @JavascriptInterface
    public void updateRoute(String _payload) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject payload = new JSONObject(_payload);
                String pureScriptID = payload.optString("pureScriptID", "");
                GoogleMap gMap = pureScriptID.isEmpty() ? googleMap : googleMapInstance.get(pureScriptID);
                if (gMap != null) {
                    try {
                        MapRemoteConfig mapRemoteConfig = getMapRemoteConfig();
                        float zoomLevel;
                        try {
                            zoomLevel = (float) mapRemoteConfig.zoomLevel;
                        } catch (Exception e) {
                            zoomLevel = 20.0f;
                        }
                        String json = payload.optString("json", "");
                        JSONObject destMarkerConfig = payload.optJSONObject("destMarkerConfig");
                        String dest = (destMarkerConfig != null) ? destMarkerConfig.optString("pointerIcon", "") : "";
                        String eta = payload.optString("eta", "");
                        String locationName = payload.optString("locationName", "");
                        String src = payload.optString("srcMarker", "");
                        String specialLocation = payload.optString("specialLocation", "");
                        String polylineKey = payload.optString("polylineKey", "DEFAULT");
                        JSONObject destMarkerActionImageConfig = (destMarkerConfig != null) ? destMarkerConfig.optJSONObject("actionImage") : null;
                        JSONObject specialLocationObject = new JSONObject(specialLocation);
                        int dashUnit = specialLocationObject.optInt("dashUnit", 1);
                        int gapUnit = specialLocationObject.optInt("gapUnit", 0);
                        boolean autoZoom = payload.optBoolean("autoZoom", true);
                        ArrayList<LatLng> path = new ArrayList<>();
                        JSONObject jsonObject = new JSONObject(json);
                        JSONArray coordinates = jsonObject.getJSONArray("points");
                        for (int i = coordinates.length() - 1; i >= 0; i--) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            LatLng tempPoint = new LatLng(lat, lng);
                            path.add(tempPoint);
                        }
                        Marker currMarker = (Marker) markers.get(src);
                        if (currMarker != null) {
                            currMarker.setTitle("Vehicle Icon On Map");
                            currMarker.hideInfoWindow();
                        }
                        Marker destMarker = (Marker) markers.get(dest);
                        String destinationSpecialTagIcon = specialLocationObject.getString("destSpecialTagIcon");
                        MarkerConfig markerConfig = new MarkerConfig();
                        markerConfig.locationName(locationName);
                        markerConfig.setLabelImage(destinationSpecialTagIcon);
                        if (destMarkerActionImageConfig != null)
                            markerConfig.setMarkerActionImageConfig(destMarkerActionImageConfig);
                        if (destMarker != null) {
                            destMarker.setIcon((BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(dest, false, MarkerType.NORMAL_MARKER, markerConfig))));
                            destMarker.setTitle("Driver is " + eta);
                        }
                        PolylineDataPoints polylineDataPoints = getPolyLineDataByMapInstance(pureScriptID, polylineKey);
                        Polyline polyline = getPolyLine(false, polylineDataPoints);
                        if (polyline != null) {
                            polyline.setEndCap(new ButtCap());
                            if (path.size() == 0) {
                                if (destMarker != null) {
                                    LatLng destination = destMarker.getPosition();
                                    animateMarkerNew(src, destination, currMarker, eta);
                                    Polyline overlayPolylines = getPolyLine(true, polylineDataPoints);
                                    if (overlayPolylines != null) {
                                        overlayPolylines.remove();
                                    }
                                    polyline.remove();
                                    if (polylineDataPoints != null) {
                                        polylineDataPoints.setPolyline(null);
                                        polylineDataPoints.setOverlayPolylines(null);
                                    }
                                    setPolyLineDataByMapInstance(pureScriptID, polylineKey, polylineDataPoints);
                                    if (currMarker != null) {
                                        currMarker.setAnchor(0.5f, 0);
                                    }
                                    mapUpdate.isMapMoved = false;
                                    mapUpdate.isMapIdle = true;
                                    animateCamera(destMarker.getPosition().latitude, destMarker.getPosition().longitude, zoomLevel, ZoomType.ZOOM);
                                }
                            } else {
                                double destinationLat = path.get(0).latitude;
                                double destinationLon = path.get(0).longitude;
                                double sourceLat = path.get(path.size() - 1).latitude;
                                double sourceLong = path.get(path.size() - 1).longitude;
                                LatLng destination = path.get(path.size() - 1);
                                animateMarkerNew(src, destination, currMarker, eta);
                                PatternItem dash = new Dash(dashUnit);
                                PatternItem gap = new Gap(gapUnit);
                                List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Arrays.asList(dash, gap);
                                polyline.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                                polyline.setPoints(path);
                                if (debounceAnimateCameraCounter == null)
                                    debounceAnimateCameraCounter = mapRemoteConfig.debounceAnimateCameraCounter;
                                if (autoZoom && mapUpdate.isMapIdle && (debounceAnimateCameraCounter <= 0 || mapUpdate.isMapMoved)) {
                                    moveCamera(sourceLat, sourceLong, destinationLat, destinationLon, coordinates);
                                    mapUpdate.isMapMoved = false;
                                    mapUpdate.isMapIdle = true;
                                    debounceAnimateCameraCounter = mapRemoteConfig.debounceAnimateCameraCounter;
                                } else {
                                    debounceAnimateCameraCounter--;
                                }
                            }
                        }
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        });
    }

    private void animateMarkerNew(String src, LatLng destination, final Marker marker, String infoLabelText) {
        if (marker != null) {
            LatLng startPosition = marker.getPosition();
            ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1);
            valueAnimator.setDuration(2000); // TODO :: get this value from Local Storage to maintain sync with PS
            valueAnimator.setInterpolator(new LinearInterpolator());
            valueAnimator.addUpdateListener(animation -> {
                try {
                    float v = animation.getAnimatedFraction();
                    LatLng newPosition = SphericalUtil.interpolate(startPosition, destination, v);
                    float rotation = bearingBetweenLocations(startPosition, destination);
                    if (rotation > 1.0) {
                        MarkerConfig markerConfig = new MarkerConfig();
                        markerConfig.locationName(infoLabelText);
                        markerConfig.setRotation(rotation);
                        marker.setRotation(0.0f);
                        marker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(src, false, MarkerType.NORMAL_MARKER, markerConfig)));
                    }
                    marker.setPosition(newPosition);
                    marker.setAnchor(0.5f, 0.7f);
                    markers.put(src, marker);
                } catch (Exception e) {
                    e.printStackTrace();
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

    @JavascriptInterface
    public String getExtendedPath(String json) throws JSONException {
        ArrayList<LatLng> path = new ArrayList<>();
        ArrayList<LatLng> extendedPath = new ArrayList<>();
        JSONObject jsonObject = new JSONObject(json);
        JSONArray coordinates = jsonObject.getJSONArray("points");
        if (coordinates.length() <= 1) return json;
        int pointsFactor = Integer.parseInt(getKeysInSharedPref("POINTS_FACTOR"));

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

    // region Shared Preference Utils
    @JavascriptInterface
    public void setKeysInSharedPrefs(String key, String value) {
        SharedPreferences sharedPref = bridgeComponents.getContext().getSharedPreferences(bridgeComponents.getSdkName(), MODE_PRIVATE);
        sharedPref.edit().putString(key, value).apply();
        if (key.equals(bridgeComponents.getContext().getString(R.string.LANGUAGE_KEY))) {
            Utils.updateLocaleResource(value, bridgeComponents.getContext());
        }
    }

    @JavascriptInterface
    public String getKeysInSharedPref(String key) {
        SharedPreferences sharedPref = bridgeComponents.getContext().getSharedPreferences(bridgeComponents.getSdkName(), MODE_PRIVATE);
        return sharedPref.getString(key, "__failed");
    }

    @JavascriptInterface
    public String getKeyInNativeSharedPrefKeys(String key) {
        return getKeysInSharedPref(key);
    }

    @JavascriptInterface
    public void setEnvInNativeSharedPrefKeys(String key, String value) {
        setKeysInSharedPrefs(key, value);
    }

    //endregion

    // region DATE / TIME UTILS

    @JavascriptInterface
    public void timePicker(final String callback, String label) {
        timePicker(callback, label, null);
    }

    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.N)
    public void timePicker(final String callback, String label, String defaultDate) {
        ExecutorManager.runOnMainThread(() -> {
            final Calendar c = Calendar.getInstance();
            int hour = c.get(Calendar.HOUR_OF_DAY);
            int minute = c.get(Calendar.MINUTE);
            Log.e(DTUTILS, "Time picker called");
            TimePickerDialog timePickerDialog = new TimePickerDialog(bridgeComponents.getActivity(), (timePicker, hourOfDay, minute1) -> {
                if (callback != null) {
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s', %d, %d,'%s');",
                            callback, hourOfDay, minute1, "SELECTED");
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            }, hour, minute, false);
            timePickerDialog.setOnCancelListener(var1 -> {
                if (callback != null) {
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s',%d,%d);",
                            callback, "CANCELLED", 0, 0);
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            });
            timePickerDialog.setOnDismissListener(var1 -> {
                if (callback != null) {
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s',%d,%d);",
                            callback, "DISMISSED", 0, 0);
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            });

            if (defaultDate != null && defaultDate != "") {
                try {
                    long defaultDateMillis = Long.parseLong(defaultDate);
                    Calendar defaultCa = Calendar.getInstance();
                    defaultCa.setTimeInMillis(defaultDateMillis);
                    timePickerDialog.updateTime(defaultCa.get(Calendar.HOUR_OF_DAY), defaultCa.get(Calendar.MINUTE));
                } catch (NumberFormatException e) {
                    Log.e(DTUTILS, "Invalid default time format: " + e);
                }
            }
            timePickerDialog.show();

        });
    }

    @SuppressLint("DiscouragedApi")
    private void reOrderSpinners(DatePickerDialog dialog, char[] dateOrder) {
        if (!dialog.isShowing()) {
            return;
        }

        final int yearId = Resources.getSystem().getIdentifier("year", "id", "android");
        final int monthId = Resources.getSystem().getIdentifier("month", "id", "android");
        final int dayId = Resources.getSystem().getIdentifier("day", "id", "android");
        final int layoutId = Resources.getSystem().getIdentifier("pickers", "id", "android");

        final NumberPicker yearSpinner = dialog.findViewById(yearId);
        final NumberPicker monthSpinner = dialog.findViewById(monthId);
        final NumberPicker daySpinner = dialog.findViewById(dayId);
        final LinearLayout layout = dialog.findViewById(layoutId);

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

    @SuppressLint("DiscouragedApi")
    private void setImeOptions(NumberPicker spinner, int spinnerIndex) {
        final int imeOption;
        if (spinnerIndex < DATEPICKER_SPINNER_COUNT - 1) {
            imeOption = EditorInfo.IME_ACTION_NEXT;
        } else {
            imeOption = EditorInfo.IME_ACTION_DONE;
        }
        int idPickerInput = Resources.getSystem().getIdentifier("numberpicker_input", "id", "android");
        TextView input = spinner.findViewById(idPickerInput);
        input.setImeOptions(imeOption);
    }

    @JavascriptInterface
    public void datePicker(final String callback, String label) {
        datePicker(callback, label, null);
    }

    @JavascriptInterface
    public void datePicker(final String callback, String label, String defaultDate) {
        datePicker(callback, label, defaultDate, null);
    }

    @JavascriptInterface
    public void datePicker(final String callback, String label, String defaultDate, String maxDate) {
        ExecutorManager.runOnMainThread(new Runnable() {
            @Override
            public void run() {
                final Calendar calendar = Calendar.getInstance();
                switch (label) {
                    case DatePickerLabels.MINIMUM_NEXT_DATE:
                        calendar.add(Calendar.DAY_OF_YEAR, 1);
                        break;
                }
                int mYear = calendar.get(Calendar.YEAR);
                int mMonth = calendar.get(Calendar.MONTH);
                int mDate = calendar.get(Calendar.DATE);
                int datePickerTheme = AlertDialog.THEME_HOLO_LIGHT;
                if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.N) datePickerTheme = 0;

                DatePickerDialog datePickerDialog = new DatePickerDialog(bridgeComponents.getActivity(), datePickerTheme, (datePicker, year, month, date) -> {
                    if (callback != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s',%d,%d,%d);",
                                callback, "SELECTED", year, month, date);
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
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
                        switch (label) {
                            case DatePickerLabels.MINIMUM_NEXT_DATE:
                                break;
                            default:
                                try {
                                    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N) {
                                        if (month != 0) {
                                            NumberPicker monthPicker = findViewById(month);
                                            if (monthPicker != null) {
                                                monthPicker.setDisplayedValues(monthNumbers);
                                            }
                                            if (maxDate != null && !maxDate.isEmpty() && !maxDate.equals("-1")) {
                                                long maxDateInMills = Long.parseLong(maxDate);
                                                this.getDatePicker().setMaxDate(maxDateInMills);
                                            }
                                        }
                                    }
                                } catch (Exception e) {
                                    Log.e(DTUTILS, "Error in onDateChanged : " + e);
                                }
                                break;
                        }
                    }

                    @Override
                    protected void onCreate(Bundle savedInstanceState) {
                        super.onCreate(savedInstanceState);
                        switch (label) {
                            case DatePickerLabels.MINIMUM_NEXT_DATE:
                                break;
                            default:
                                try {
                                    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N) {
                                        if (month != 0) {
                                            NumberPicker monthPicker = findViewById(month);
                                            if (monthPicker != null) {
                                                monthPicker.setDisplayedValues(monthNumbers);
                                            }
                                            if (maxDate != null && !maxDate.isEmpty() && !maxDate.equals("-1")) {
                                                long maxDateInMills = Long.parseLong(maxDate);
                                                this.getDatePicker().setMaxDate(maxDateInMills);
                                            }
                                        }
                                    }
                                } catch (Exception e) {
                                    Log.e(DTUTILS, "Error in Date onCreate : " + e);
                                }
                                break;
                        }
                    }
                };
                datePickerDialog.setOnCancelListener(var1 -> {
                    if (callback != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s', '%s',%d,%d,%d);",
                                callback, "CANCELLED", 0, 0, 0);
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                    }
                });
                datePickerDialog.setOnDismissListener(var1 -> {
                    if (callback != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s',%d,%d,%d);",
                                callback, "DISMISSED", 0, 0, 0);
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                    }
                });
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
                    case DatePickerLabels.MINIMUM_NEXT_DATE:
                        Calendar minDate = Calendar.getInstance();
                        minDate.set(Calendar.HOUR_OF_DAY, 0);
                        minDate.set(Calendar.MINUTE, 0);
                        minDate.set(Calendar.SECOND, 0);
                        minDate.set(Calendar.MILLISECOND, 0);
                        minDate.add(Calendar.DAY_OF_YEAR, 1);
                        datePickerDialog.getDatePicker().setMinDate(minDate.getTimeInMillis());
                        break;
                }
                if (maxDate != null && !maxDate.isEmpty() && !maxDate.equals("-1")) {
                    long maxDateInMills = Long.parseLong(maxDate);
                    datePickerDialog.getDatePicker().setMaxDate(maxDateInMills);
                }
                if (defaultDate != null && !defaultDate.isEmpty() && !defaultDate.equals("-1")) {
                    try {
                        long defaultDateMillis = Long.parseLong(defaultDate);
                        Calendar defaultCa = Calendar.getInstance();
                        defaultCa.setTimeInMillis(defaultDateMillis);
                        datePickerDialog.updateDate(defaultCa.get(Calendar.YEAR), defaultCa.get(Calendar.MONTH), defaultCa.get(Calendar.DAY_OF_MONTH));
                    } catch (NumberFormatException e) {
                        Log.e(DTUTILS, "Invalid default date format: " + e);
                    }
                }
                if (Build.VERSION.SDK_INT > Build.VERSION_CODES.N)
                    datePickerDialog.setTitle(bridgeComponents.getContext().getString(R.string.select_date));
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
                    Log.e(DTUTILS, "Error in reOrdering spinners : " + e);
                }
            }
        });
    }

    @JavascriptInterface
    public boolean isNetworkTimeEnabled() {
        try {
            String timeSettings = android.provider.Settings.Global.getString(
                    bridgeComponents.getContext().getContentResolver(),
                    android.provider.Settings.Global.AUTO_TIME);
            if (timeSettings != null) {
                return timeSettings.equals("1");
            }
            return true;
        } catch (Exception e) {
            bridgeComponents.getTrackerInterface().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.USER, UTILS, "Exception in isNetworkTimeEnabled", e);
            return true;
        }
    }
    //endregion

    // region OTHER UTILS
    @JavascriptInterface
    public String getVersionName() {
        PackageManager manager = bridgeComponents.getContext().getPackageManager();
        PackageInfo info = new PackageInfo();
        try {
            info = manager.getPackageInfo(bridgeComponents.getContext().getPackageName(), PackageManager.GET_ACTIVITIES);
        } catch (PackageManager.NameNotFoundException e) {
            Log.e(OTHERS, "Exception in get version name" + e);
        }
        return info.versionName;
    }

    @JavascriptInterface
    public String getLayoutBounds(String id) throws JSONException {
        Activity activity = bridgeComponents.getActivity();
        int height = 0;
        int width = 0;
        if (activity != null) {
            View view = activity.findViewById(Integer.parseInt(id));
            if (view != null) {
                height = view.getHeight();
                width = view.getWidth();
            }
        }
        JSONObject bounds = new JSONObject();
        bounds.put("height", pxToDp(height));
        bounds.put("width", pxToDp(width));
        return bounds.toString();
    }

    private int pxToDp(int px) {
        DisplayMetrics displayMetrics = bridgeComponents.getContext().getResources().getDisplayMetrics();
        return Math.round(px / (displayMetrics.xdpi / DisplayMetrics.DENSITY_DEFAULT));
    }
    //endregion

    @JavascriptInterface
    public int getVersionCode() {
        PackageManager manager = bridgeComponents.getContext().getPackageManager();
        PackageInfo info = new PackageInfo();
        try {
            info = manager.getPackageInfo(bridgeComponents.getContext().getPackageName(), PackageManager.GET_ACTIVITIES);
        } catch (PackageManager.NameNotFoundException e) {
            Log.e(OTHERS, "Exception in get version code" + e);
        }
        return info.versionCode;
    }

    @Deprecated
    @JavascriptInterface
    public void toggleLoader(final boolean visible) {
        ExecutorManager.runOnMainThread(() -> {
            if (bridgeComponents.getActivity() != null) {
                @SuppressLint("InflateParams") View loader = bridgeComponents.getActivity().getLayoutInflater().inflate(R.layout.loader, null);
                if (loader != null) {
                    if (visible) {
                        loader.setVisibility(View.VISIBLE);
                        loader.setOnClickListener(v -> { // Added this to prevent invisible touches through the loader
                            System.out.println("LOADER CLICKED");
                        });
                    } else {
                        loader.setVisibility(View.GONE);
                    }
                }
            }
        });
    }

    @JavascriptInterface
    public void loaderText(final String mainMsg, final String subMsg) {
        ExecutorManager.runOnMainThread(() -> {
            @SuppressLint("InflateParams") View loader = LayoutInflater.from(bridgeComponents.getContext()).inflate(R.layout.loader, null, false);
            TextView mainloaderText = loader.findViewById(R.id.loaderMainText);
            TextView subloaderText = loader.findViewById(R.id.loaderSubText);
            mainloaderText.setText(mainMsg);
            subloaderText.setText(subMsg);
        });
    }

    @JavascriptInterface
    public void hideKeyboardOnNavigation(boolean permission) {
        if (bridgeComponents.getActivity() != null) {
            View view = bridgeComponents.getActivity().getCurrentFocus();
            if (view == null) {
                view = new View(bridgeComponents.getActivity());
            }
            InputMethodManager imm = (InputMethodManager) bridgeComponents.getContext().getSystemService(Activity.INPUT_METHOD_SERVICE);
            imm.hideSoftInputFromWindow(view.getWindowToken(), 0);
        }
    }

    @SuppressLint("MissingPermission")
    @JavascriptInterface
    public boolean isNetworkAvailable() {
        ConnectivityManager connectivityManager = (ConnectivityManager) bridgeComponents.getContext().getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo activeNetworkInfo = null;
        if (connectivityManager != null) {
            activeNetworkInfo = connectivityManager.getActiveNetworkInfo();
        }
        return activeNetworkInfo != null && activeNetworkInfo.isConnected();
    }

    @SuppressLint("MissingPermission")
    @JavascriptInterface
    public boolean isInternetAvailable() {
        return ((ConnectivityManager) bridgeComponents.getContext().getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo() != null;
    }

    @JavascriptInterface
    public void showDialer(String phoneNum, boolean call) {
        Intent intent = new Intent(call ? Intent.ACTION_CALL : Intent.ACTION_DIAL);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        intent.setData(Uri.parse("tel:" + phoneNum));
        if (call && ContextCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.CALL_PHONE) != PackageManager.PERMISSION_GRANTED) {
            phoneNumber = phoneNum;
            ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{Manifest.permission.CALL_PHONE}, REQUEST_CALL);
        } else {
            bridgeComponents.getContext().startActivity(intent);
        }
    }

    @JavascriptInterface
    public void openUrlInApp(String url) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                Intent httpIntent = new Intent(Intent.ACTION_VIEW);
                httpIntent.setData(Uri.parse(url));
                httpIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                bridgeComponents.getContext().startActivity(httpIntent);
            } catch (ActivityNotFoundException e) {
                toast(bridgeComponents.getContext().getString(R.string.no_enabled_browser));
//                firebaseLogEvent("exception_no_activity_found_for_intent");
                Log.e(UTILS, "Exception occurred while calling WebView", e);
            } catch (Exception e) {
//                firebaseLogEvent("exception_in_openUrlInApp");
                Log.e(UTILS, "Exception occurred while calling WebView", e);
            }
        });
    }

    @JavascriptInterface
    public void requestKeyboardShow(final String id) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (bridgeComponents.getActivity() != null) {
                    int currentId = Integer.parseInt(id);
                    InputMethodManager inputMethodManager = (InputMethodManager) bridgeComponents.getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
                    View editText = bridgeComponents.getActivity().findViewById(currentId);
                    View prevEditText = null;
                    if (lastFocusedEditView != -1) {
                        prevEditText = bridgeComponents.getActivity().findViewById(lastFocusedEditView);
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
                Log.e(UTILS, "Keyboard Exception" + e);
            }
        });
    }

    @JavascriptInterface
    public void showKeyboard(final String id) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (bridgeComponents.getActivity() != null) {
                    int currentId = Integer.parseInt(id);
                    InputMethodManager inputMethodManager = (InputMethodManager) bridgeComponents.getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
                    EditText editText = bridgeComponents.getActivity().findViewById(currentId);
                    EditText prevEditText = null;
                    if (lastFocusedEditText != -1) {
                        prevEditText = bridgeComponents.getActivity().findViewById(lastFocusedEditText);
                    }
                    if (inputMethodManager != null && editText != null) {
                        if (prevEditText != null && lastFocusedEditText != currentId) {
                            prevEditText.clearFocus();
                            prevEditText.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
                            prevEditText.setImeOptions(EditorInfo.IME_ACTION_DONE);
                        }
                        editText.requestFocus();
                        editText.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
                        editText.setImeOptions(EditorInfo.IME_ACTION_DONE);
                        inputMethodManager.showSoftInput(editText, InputMethodManager.SHOW_IMPLICIT);
                    }
                    if (currentId != lastFocusedEditText) {
                        lastFocusedEditText = Integer.parseInt(id);
                    }
                }
            } catch (Exception e) {
                Log.e(UTILS, "Keyboard Exception" + e);
            }
        });
    }


    @JavascriptInterface
    public void goBackPrevWebPage(String id) {
        if (bridgeComponents.getActivity() != null) {
            WebView webView = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
            ExecutorManager.runOnMainThread(() -> {
                if (webView == null) return;
                if (webView.canGoBack()) {
                    webView.post(webView::goBack);
                } else {
                    if (storeDashboardCallBack != null) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                                storeDashboardCallBack, "TRUE");
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                    }
                }
            });
        }
    }

    @JavascriptInterface
    public void startLottieProcess(String configObj) {
        if (bridgeComponents.getActivity() != null) {
            ExecutorManager.runOnMainThread(() -> {
                try {
                    JSONObject jsonObject = new JSONObject(configObj);
                    float minProgress = Float.parseFloat(jsonObject.getString("minProgress"));
                    float maxProgress = Float.parseFloat(jsonObject.getString("maxProgress"));
                    float progress = (float) jsonObject.optDouble("progress", 0.0);
                    minProgress = minProgress >= maxProgress ? 0.0f : minProgress;
                    String scaleType = jsonObject.getString("scaleType");
                    boolean repeat = Boolean.parseBoolean(jsonObject.getString("repeat"));
                    int lottieId = Integer.parseInt(jsonObject.getString("lottieId"));
                    float speed = Float.parseFloat(jsonObject.getString("speed"));
                    String rawJson = jsonObject.getString("rawJson");
                    boolean forceToUseRemote = jsonObject.optBoolean("forceToUseRemote");

                    animationView = bridgeComponents.getActivity().findViewById(lottieId);
                    animationView.addAnimatorListener(new Animator.AnimatorListener() {
                        @Override
                        public void onAnimationEnd(Animator animation) {
                            if (repeat) {
                                animationView.setMinAndMaxProgress(0.0f, 1.0f);
                            }
                        }

                        @Override
                        public void onAnimationStart(@NonNull Animator animator) {
                        }

                        @Override
                        public void onAnimationCancel(@NonNull Animator animator) {
                        }

                        @Override
                        public void onAnimationRepeat(@NonNull Animator animator) {
                        }
                    });
                    if (rawJson.contains("http")) {
                        animationView.setFailureListener(throwable -> Log.d(UTILS, "Failure in setAnimationFromUrl", throwable));
                        animationView.setAnimationFromUrl(rawJson);
                        if (forceToUseRemote)
                            animationView.setCacheComposition(false);
                    } else {
                        animationView.setAnimationFromJson(getJsonFromResources(rawJson), null);
                    }
                    animationView.setRepeatCount(repeat ? ValueAnimator.INFINITE : 0);
                    animationView.setSpeed(speed);
                    animationView.setMinAndMaxProgress(minProgress, maxProgress);
                    animationView.setProgress(progress);
                    animationView.setScaleType(getScaleTypes(scaleType));
                    animationView.playAnimation();
                } catch (Exception e) {
                    Log.d(UTILS, "exception in startLottieProcess", e);
                }
            });
        }
    }

    @SuppressLint("DiscouragedApi")
    private String getJsonFromResources(String rawJson) throws IOException {
        Writer writer = new StringWriter();
        Context context = bridgeComponents.getContext();
        InputStream inputStreams;
        boolean isCheckAssets = (context.getResources().getIdentifier(rawJson, "raw", context.getPackageName()) == 0);
        if (isCheckAssets) {
            inputStreams = context.getAssets().open(rawJson + ".json");
            if (inputStreams == null) {
                inputStreams = context.getAssets().open("juspay/" + rawJson + ".json");
            }
        } else {
            inputStreams = context.getResources().openRawResource(context.getResources().getIdentifier(rawJson, "raw", context.getPackageName()));
        }
        char[] buffer = new char[1024];
        try {
            Reader reader = new BufferedReader(new InputStreamReader(inputStreams, StandardCharsets.UTF_8));
            int n;
            while ((n = reader.read(buffer)) != -1) {
                writer.write(buffer, 0, n);
            }
        } catch (Exception e) {
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

    @JavascriptInterface
    @SuppressLint("DiscouragedApi")
    public boolean isFilePresentDeep(String fileName) {
        Context context = bridgeComponents.getContext();
        InputStream inputStreams;
        String fileNameWithExe = fileName;
        int idx = fileName.lastIndexOf(".");
        fileName = idx == -1 ? fileName : fileName.substring(0, idx);
        boolean isCheckAssets = (context.getResources().getIdentifier(fileName, "raw", context.getPackageName()) == 0);
        if (isCheckAssets) {
            try {
                inputStreams = context.getAssets().open(fileNameWithExe);
            } catch (Exception e) {
                inputStreams = null;
            }
            if (inputStreams == null) {
                try {
                    inputStreams = context.getAssets().open("juspay/" + fileNameWithExe);
                } catch (Exception ignored) {
                }
            }
            return (inputStreams != null);
        } else {
            return true;
        }
    }

    @JavascriptInterface
    public void shareTextMessage(String title, String message) {
        ExecutorManager.runOnMainThread(() -> {
            Intent sendIntent = new Intent();
            Context context = bridgeComponents.getContext();
            sendIntent.setAction(Intent.ACTION_SEND);
            sendIntent.putExtra(Intent.EXTRA_TEXT, message);
            sendIntent.putExtra(Intent.EXTRA_TITLE, title);
            Bitmap thumbnailBitmap = BitmapFactory.decodeResource(context.getResources(), context.getResources().getIdentifier("ic_launcher", "drawable", context.getPackageName()));
            if (thumbnailBitmap != null && Build.VERSION.SDK_INT > 28) {
                Uri thumbnailUri = getImageUri(context, thumbnailBitmap);
                ClipData clipData = ClipData.newUri(context.getContentResolver(), "ThumbnailImage" + System.currentTimeMillis(), thumbnailUri);
                sendIntent.setClipData(clipData);
            }
            sendIntent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
            sendIntent.setType("text/plain");
            Intent shareIntent = Intent.createChooser(sendIntent, null);
            shareIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            context.startActivity(shareIntent);
        });
    }

    private Uri getImageUri(Context context, Bitmap bitmap) {
        try {
            ByteArrayOutputStream bytes = new ByteArrayOutputStream();
            bitmap.compress(Bitmap.CompressFormat.PNG, 100, bytes);
            String path = MediaStore.Images.Media.insertImage(context.getContentResolver(), bitmap, "ThumbnailImage" + System.currentTimeMillis(), null);
            return Uri.parse(path);
        } catch (Exception e) {
            return null;
        }
    }

    @JavascriptInterface
    public void shareImageMessage(String message, String image, String config) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject jsonObject = new JSONObject(config);
                if (bridgeComponents.getActivity() != null && jsonObject.has("viewId") && !jsonObject.getString("viewId").equals("")) {
                    Intent sendIntent = new Intent();
                    Context context = bridgeComponents.getContext();
                    Activity activity = bridgeComponents.getActivity();
                    @SuppressLint("InflateParams") View layoutToShare;
                    boolean isReferral = !jsonObject.has("code") || jsonObject.getBoolean("isReferral");
                    if (isReferral) {
                        layoutToShare = LayoutInflater.from(context).inflate(R.layout.referral_code, null, false);
                        if (jsonObject.has("code")) {
                            TextView code = layoutToShare.findViewById(R.id.code);
                            code.setText(jsonObject.getString("code"));
                        }

                        ImageView code_image = layoutToShare.findViewById(R.id.code_image);
                        ImageView qr_code_image = bridgeComponents.getActivity().findViewById(Integer.parseInt(jsonObject.getString("viewId")));
                        code_image.setImageDrawable(qr_code_image.getDrawable());

                        if (jsonObject.has("logoId")) {
                            ImageView logo_image = layoutToShare.findViewById(R.id.referral_logo);
                            ImageView logo_image_present = bridgeComponents.getActivity().findViewById(Integer.parseInt(jsonObject.getString("logoId")));
                            logo_image.setImageDrawable(logo_image_present.getDrawable());
                        }

                        layoutToShare.measure(360, 440);
                        layoutToShare.layout(0, 0, layoutToShare.getMeasuredWidth(), layoutToShare.getMeasuredHeight());

                    } else {
                        layoutToShare = activity.findViewById(Integer.parseInt(jsonObject.getString("viewId")));
                    }

                    Bitmap bitmap = Bitmap.createBitmap(layoutToShare.getMeasuredWidth(), layoutToShare.getMeasuredHeight(), Bitmap.Config.ARGB_8888);
                    Canvas canvas = new Canvas(bitmap);
                    layoutToShare.draw(canvas);
                    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
                    bitmap.compress(Bitmap.CompressFormat.JPEG, 100, bytes);


                    final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "US"));
                    f.setTimeZone(TimeZone.getTimeZone("IST"));
                    String imageDownloadTime = f.format(new Date());

                    String imageName = "IMG_" + imageDownloadTime;
                    String imageNameRemovedSpecial = imageName.replaceAll("[^a-zA-Z\\d]", "_");

                    String imagePath = MediaStore.Images.Media.insertImage(context.getContentResolver(), bitmap, imageNameRemovedSpecial + "", null);
                    Uri uri2 = Uri.parse(imagePath);

                    sendIntent.setAction(Intent.ACTION_SEND);
                    sendIntent.setType("image/*");
                    sendIntent.putExtra(Intent.EXTRA_STREAM, uri2);
                    sendIntent.putExtra(Intent.EXTRA_TEXT, message);
                    sendIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    sendIntent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
                    Intent shareIntent = Intent.createChooser(sendIntent, null);
                    shareIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    bridgeComponents.getContext().startActivity(shareIntent);
                }
            } catch (Exception e) {
                Toast.makeText(bridgeComponents.getContext(), "Something went wrong. Please try again later!", Toast.LENGTH_SHORT).show();
                Log.e("IMAGE_TEXT_SHARE", "Error in sharing the message");
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public void downloadLayoutAsImage(String qr) {
        downloadLayout = qr;
        if (bridgeComponents.getActivity() != null && checkAndAskStoragePermission()) {
            Context context = bridgeComponents.getContext();
            Activity activity = bridgeComponents.getActivity();
            View layoutView = activity.findViewById(Integer.parseInt(qr));

            Bitmap bitmap = Bitmap.createBitmap(layoutView.getWidth(), layoutView.getHeight(), Bitmap.Config.ARGB_8888);
            Canvas canvas = new Canvas(bitmap);
            layoutView.draw(canvas);

            File directory = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES);
            File imageFile = new File(directory, "QR.png");
            Uri path = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", imageFile);
            try {
                FileOutputStream fos = new FileOutputStream(imageFile);
                bitmap.compress(Bitmap.CompressFormat.PNG, 100, fos);
                fos.close();
                downloadLayout = null;
                showNotificationWithURI(path, activity.getString(R.string.qr_downloaded), context.getString(R.string.qr_for_your_vpa_is_downloaded), "image/png", "QR", "QR Download");
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public void showNotificationWithURI(Uri path, String toastMessage, String notificationContent, String dataType, String channelId, String channelDesc) {
        new Handler(Looper.getMainLooper()).post(() -> {
            toast(toastMessage);
            Context context = bridgeComponents.getContext();
            JuspayLogger.d(OTHERS, channelDesc + "inside Show Notification");
            Intent pdfOpenintent = new Intent(Intent.ACTION_VIEW);
            pdfOpenintent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_ACTIVITY_CLEAR_TOP);
            pdfOpenintent.setDataAndType(path, dataType);
            PendingIntent pendingIntent = PendingIntent.getActivity(context, 234567, pdfOpenintent, PendingIntent.FLAG_IMMUTABLE);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                NotificationChannel channel = new NotificationChannel(channelId, channelDesc, NotificationManager.IMPORTANCE_HIGH);
                channel.setDescription(channelDesc);
                NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
                AudioAttributes attributes = new AudioAttributes.Builder()
                        .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
                        .setUsage(AudioAttributes.USAGE_NOTIFICATION)
                        .build();
                channel.setSound(RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION), attributes);
                notificationManager.createNotificationChannel(channel);
            }
            NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, channelId);
            mBuilder.setContentTitle(toastMessage)
                    .setSmallIcon(bridgeComponents.getContext().getResources().getIdentifier((Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "ic_launcher_small_icon" : "ic_launcher", (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "drawable" : "mipmap", bridgeComponents.getContext().getPackageName()))
                    .setContentText(notificationContent)
                    .setAutoCancel(true)
                    .setSound(RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION))
                    .setPriority(NotificationCompat.PRIORITY_MAX);
            mBuilder.setContentIntent(pendingIntent);
            NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
            JuspayLogger.d(OTHERS, channelDesc + "notification is Created");
            if (ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
                JuspayLogger.d(OTHERS, channelDesc + "Notification permission is not given");
            } else {
                notificationManager.notify(234567, mBuilder.build());
                JuspayLogger.d(OTHERS, channelDesc + "notification is notified");
            }
        });
    }

    @JavascriptInterface
    public void adjustViewWithKeyboard(String flag) {
        ExecutorManager.runOnMainThread(() -> {
            if (bridgeComponents.getActivity() != null) {
                if (flag.equals("true"))
                    bridgeComponents.getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_RESIZE);
                else
                    bridgeComponents.getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_NOTHING);
            }
        });
    }

    @JavascriptInterface
    public void copyToClipboard(String inputText) {
        if (bridgeComponents.getActivity() != null) {
            ClipboardManager clipboard = (ClipboardManager) bridgeComponents.getActivity().getSystemService(Context.CLIPBOARD_SERVICE);
            ClipData clip = ClipData.newPlainText("Text", inputText);
            clipboard.setPrimaryClip(clip);
        }
    }

    @JavascriptInterface
    public void initialWebViewSetUp(String callback, String id) {
        storeDashboardCallBack = callback;
        Context context = bridgeComponents.getContext();
        Activity activity = bridgeComponents.getActivity();
        if (activity != null) {
            ExecutorManager.runOnMainThread(new Runnable() {
                @Override
                public void run() {
                    WebView webView = activity.findViewById(Integer.parseInt(id));
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
                                    Toast.makeText(context, "Looks like there is no app or web browser installed on your device", Toast.LENGTH_SHORT).show();
                                    return true;
                                }
                            }
                            return false;
                        }
                    });
                }
            });
        }
    }

    @JavascriptInterface
    public void minimizeApp() {
        Intent startMain = new Intent(Intent.ACTION_MAIN);
        startMain.addCategory(Intent.CATEGORY_HOME);
        startMain.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        bridgeComponents.getContext().startActivity(startMain);
    }

    @JavascriptInterface
    public void closeApp() {
        if (bridgeComponents.getActivity() != null) {
            bridgeComponents.getActivity().finish();
        }
    }

    @JavascriptInterface
    public void toast(String msg) {
        Toast.makeText(bridgeComponents.getContext(), msg, Toast.LENGTH_SHORT).show();
    }

    @JavascriptInterface
    public void scrollToEnd(final String id, final boolean isBottom) {
        try {
            if (bridgeComponents.getActivity() != null) {
                if (isBottom) {
                    ScrollView scrollView = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                    if (scrollView != null) {
                        scrollView.fullScroll(View.FOCUS_DOWN);
                    }
                } else {
                    HorizontalScrollView horizontalScrollView = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                    if (horizontalScrollView != null) {
                        horizontalScrollView.fullScroll(View.FOCUS_RIGHT);
                    }
                }
            }
        } catch (Exception e) {
            Log.e(OTHERS, "Error in scroll to Bottom : " + e);
        }
    }

    private static class DatePickerLabels {
        private static final String MAXIMUM_PRESENT_DATE = "MAXIMUM_PRESENT_DATE";
        private static final String MINIMUM_EIGHTEEN_YEARS = "MINIMUM_EIGHTEEN_YEARS";
        private static final String MIN_EIGHTEEN_MAX_SIXTY_YEARS = "MIN_EIGHTEEN_MAX_SIXTY_YEARS";
        private static final String MAX_THIRTY_DAYS_FROM_CURRENT_DATE = "MAX_THIRTY_DAYS_FROM_CURRENT_DATE";
        private static final String MINIMUM_PRESENT_DATE = "MINIMUM_PRESENT_DATE";
        private static final String MINIMUM_NEXT_DATE = "MINIMUM_NEXT_DATE";
    }

    public File checkAndGetFileName(String fileName) {
        File file = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS), fileName + ".pdf");
        int i = 1;
        while (file.exists()) {
            JuspayLogger.d(UTILS, "file already exists " + file.getName());
            String updatedFile = fileName + "_(" + i + ")" + ".pdf";
            file = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS), updatedFile);
            i++;

        }
        JuspayLogger.d(UTILS, "final File name " + file.getName());
        return file;
    }

    protected boolean checkAndAskStoragePermission() {
        if (Build.VERSION.SDK_INT < 30) {
            if (bridgeComponents.getActivity() != null && ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED || ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{READ_EXTERNAL_STORAGE, WRITE_EXTERNAL_STORAGE}, STORAGE_PERMISSION);
                return false;
            } else {
                return true;
            }
        }
        return true;
    }


    public ImageView.ScaleType getScaleTypes(String scale) {
        switch (scale) {
            case "MATRIX":
                return ImageView.ScaleType.MATRIX;
            case "FIT_XY":
                return ImageView.ScaleType.FIT_XY;
            case "FIT_START":
                return ImageView.ScaleType.FIT_START;
            case "FIT_END":
                return ImageView.ScaleType.FIT_END;
            case "CENTER":
                return ImageView.ScaleType.CENTER;
            case "CENTER_CROP":
                return ImageView.ScaleType.CENTER_CROP;
            case "CENTER_INSIDE":
                return ImageView.ScaleType.CENTER_INSIDE;
            default:
                return ImageView.ScaleType.FIT_CENTER;
        }
    }

    public static class ZoomType {
        public static final String NO_ZOOM = "NO_ZOOM";
        public static final String ZOOM = "ZOOM";
    }

    public enum MarkerType {
        SPECIAL_ZONE_MARKER,
        NORMAL_MARKER,
        NORMAL_MARKER_V2,
        STOP_ICON_MARKER,
    }

    @JavascriptInterface
    public void performHapticFeedback() {
        Vibrator vibrator = (Vibrator) bridgeComponents.getContext().getSystemService(Context.VIBRATOR_SERVICE);

        if (vibrator != null && vibrator.hasVibrator()) {
            VibrationEffect effect;
            if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
                effect = VibrationEffect.createOneShot(50, VibrationEffect.DEFAULT_AMPLITUDE);
                vibrator.vibrate(effect);
            }
        }
    }

    @JavascriptInterface
    public void horizontalScrollToPos(final String id, final String childId, final int focus) {
        if (bridgeComponents.getActivity() != null) {
            Activity activity = bridgeComponents.getActivity();
            HorizontalScrollView scrollView = activity.findViewById(Integer.parseInt(id));
            View child = activity.findViewById(Integer.parseInt(childId));
            int x = 0;
            int y = 0;
            if (child != null) {
                x = child.getLeft();
                y = child.getTop();
            }
            if (scrollView != null) {
                if (x == 0 && y == 0) {
                    scrollView.fullScroll(focus);
                } else {
                    scrollView.scrollTo(x, y);
                }
            }
        }
    }

    @JavascriptInterface
    public void clearStorageFile(String fileName) {
        SharedPreferences sharedPref = bridgeComponents.getContext().getSharedPreferences(fileName, MODE_PRIVATE);
        sharedPref.edit().clear().apply();
    }

    @JavascriptInterface
    public boolean isServiceRunning(String serviceClassName) {
        final ActivityManager activityManager = (ActivityManager) bridgeComponents.getContext().getSystemService(Context.ACTIVITY_SERVICE);
        final List<ActivityManager.RunningServiceInfo> services = activityManager.getRunningServices(Integer.MAX_VALUE);

        for (ActivityManager.RunningServiceInfo runningServiceInfo : services) {
            Log.i("SERVICE", (runningServiceInfo.service.getClassName()));
            if (runningServiceInfo.service.getClassName().equals(serviceClassName)) {
                return true;
            }
        }
        return false;
    }

    public static boolean isServiceRunning(Context context, String serviceClassName) {
        final ActivityManager activityManager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        final List<ActivityManager.RunningServiceInfo> services = activityManager.getRunningServices(Integer.MAX_VALUE);

        for (ActivityManager.RunningServiceInfo runningServiceInfo : services) {
            if (runningServiceInfo.service.getClassName().equals(serviceClassName)) {
                return true;
            }
        }
        return false;
    }

    @JavascriptInterface
    public void startServiceForClass(String serviceClassName) {
        try {
            Log.i("SERVICE", "starting the service for class - " + serviceClassName);
            Intent serviceIntent = new Intent(bridgeComponents.getContext(), Class.forName(serviceClassName));
            bridgeComponents.getContext().startService(serviceIntent);
        } catch (Exception e) {
            Log.i("SERVICE", "Error in starting the service for class - " + serviceClassName + " " + e);
        }
    }

    @JavascriptInterface
    public void stopServiceForClass(String serviceClassName) {
        try {
            Log.i("SERVICE", "stopping the service for class - " + serviceClassName);
            Intent serviceIntent = new Intent(bridgeComponents.getContext(), Class.forName(serviceClassName));
            bridgeComponents.getContext().stopService(serviceIntent);
        } catch (Exception e) {
            Log.i("SERVICE", "Error in stopping the service for class - " + serviceClassName + " " + e);
        }
    }

    @JavascriptInterface
    public void clearFocus(String id) {
        if (bridgeComponents.getActivity() != null) {
            ExecutorManager.runOnMainThread(() -> {
                View v = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                if (v != null) {
                    v.clearFocus();
                }
            });
        }
    }

    @JavascriptInterface
    public void uploadMultiPartData(String filePath, String uploadUrl, String fileType, String outputField, String formDataField) {
        try {
            String boundary = UUID.randomUUID().toString();

            URL url = new URL(uploadUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setDoOutput(true);
            connection.setUseCaches(false);
            connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);
            String token = KeyValueStore.read(bridgeComponents.getContext(), bridgeComponents.getSdkName(), "REGISTERATION_TOKEN", "__failed");
            connection.setRequestProperty("token", token);

            File file = new File(filePath);
            String fileName = file.getName();
            DataOutputStream outputStream = new DataOutputStream(connection.getOutputStream());

            outputStream.writeBytes("--" + boundary + "\r\n");
            outputStream.writeBytes(("Content-Disposition: form-data; name=\"" + formDataField + "\"; filename=\"" + fileName + "\"" + "\r\n"));
            if (fileType.equals("Image"))
                outputStream.writeBytes("Content-Type: image/jpeg\r\n");
            else if (fileType.equals("Audio"))
                outputStream.writeBytes("Content-Type: audio/mpeg\r\n");
            outputStream.writeBytes("\r\n");

            FileInputStream fileInputStream = new FileInputStream(file);
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
                JSONObject jsonObject;
                try {
                    jsonObject = new JSONObject(res);

                    res = jsonObject.getString(outputField);
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
            } else {
                Toast.makeText(bridgeComponents.getContext(), bridgeComponents.getContext().getString(R.string.unable_to_upload_media), Toast.LENGTH_SHORT).show();
            }
            callUploadMultiPartCallBack(fileType, res);
        } catch (Exception e) {
            Log.e("UPLOAD_MULTI_PART_DATA", "error in Upload file: " + e);
        }
    }

    // endregion

    // region PP - Utils

    @JavascriptInterface
    public void initiatePP(String bootData) {
        paymentPage = new PaymentPage(bridgeComponents);
        paymentPage.initiate(bootData);
    }

    @JavascriptInterface
    public void processPP(String payload) {
        if (paymentPage != null) {
            try {
                paymentPage.process(payload);
            } catch (Exception e) {
                Log.e(LOG_TAG, e.toString());
            }
        }
    }

    @JavascriptInterface
    public boolean onBackPressedPP() {
        return paymentPage != null && paymentPage.onBackPressed();
    }

    @JavascriptInterface
    public void terminatePP() {
        if (paymentPage != null) {
            paymentPage.terminate();
            paymentPage = null;
        }
    }

    @JavascriptInterface
    public boolean ppInitiateStatus() {
        if (paymentPage != null) {
            return paymentPage.initiateStatus();
        }
        return false;
    }

    // endregion

    protected static class Receivers {
        BroadcastReceiver gpsReceiver;
        BroadcastReceiver internetActionReceiver;
        BroadcastReceiver timeChangeCallback;
        String storeInternetActionCallBack = null;
        String storeLocationCallBack = null;
        BridgeComponents bridgeComponents;

        public Receivers(BridgeComponents bridgeComponents) {
            this.bridgeComponents = bridgeComponents;
        }

        public void initReceiver() {
            gpsReceiver = new BroadcastReceiver() {
                @Override
                public void onReceive(Context context, Intent intent) {
                    LocationManager locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
                    boolean isGpsEnabled = locationManager.isProviderEnabled(LocationManager.GPS_PROVIDER);
                    if (!isGpsEnabled) {
                        invokeOnEvent(bridgeComponents.getJsCallback(), "onLocationChanged");
                    } else {
                        callLocationCallBack(bridgeComponents.getJsCallback(), "true");
                    }
                }
            };
            internetActionReceiver = new BroadcastReceiver() {
                @Override
                public void onReceive(Context context, Intent intent) {
                    if (!isNetworkAvailable(bridgeComponents.getContext())) {
                        invokeOnEvent(bridgeComponents.getJsCallback(), "onInternetChanged");
                    } else {
                        callInternetActionCallBack(bridgeComponents.getJsCallback(), "true");
                    }
                }
            };
            timeChangeCallback = new BroadcastReceiver() {
                @Override
                public void onReceive(Context context, Intent intent) {
                    invokeOnEvent(bridgeComponents.getJsCallback(), "onTimeChanged");
                }
            };
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                bridgeComponents.getContext().registerReceiver(timeChangeCallback, new IntentFilter(Intent.ACTION_TIME_CHANGED), Context.RECEIVER_EXPORTED);
                bridgeComponents.getContext().registerReceiver(gpsReceiver, new IntentFilter(LocationManager.PROVIDERS_CHANGED_ACTION), Context.RECEIVER_EXPORTED);
                bridgeComponents.getContext().registerReceiver(internetActionReceiver, new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION), Context.RECEIVER_EXPORTED);
            } else {
                bridgeComponents.getContext().registerReceiver(timeChangeCallback, new IntentFilter(Intent.ACTION_TIME_CHANGED));
                bridgeComponents.getContext().registerReceiver(gpsReceiver, new IntentFilter(LocationManager.PROVIDERS_CHANGED_ACTION));
                bridgeComponents.getContext().registerReceiver(internetActionReceiver, new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION));
            }
        }

        protected void invokeOnEvent(@Nullable JsCallback callback, String event) {
            if (callback != null) {
                String encoded = Base64.encodeToString("{}".getBytes(), Base64.NO_WRAP);
                String command = String.format("window[\"onEvent'\"]('%s',atob('%s'))", event, encoded);
                callback.addJsToWebView(command);
            }
        }

        private boolean isNetworkAvailable(Context context) {
            ConnectivityManager connectivityManager
                    = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
            NetworkInfo activeNetworkInfo = connectivityManager != null ? connectivityManager.getActiveNetworkInfo() : null;
            return activeNetworkInfo != null && activeNetworkInfo.isConnected();
        }

        public void callInternetActionCallBack(JsCallback callback, String isPermission) {
            if (storeInternetActionCallBack != null) {
                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                        storeInternetActionCallBack, isPermission);
                callback.addJsToWebView(javascript);
            }
        }

        public void callLocationCallBack(JsCallback callback, String isPermission) {
            if (storeLocationCallBack != null) {
                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                        storeLocationCallBack, isPermission);
                callback.addJsToWebView(javascript);
            }
        }

        public void deRegister() {
            try {
                Context context = bridgeComponents.getContext().getApplicationContext();
                context.unregisterReceiver(gpsReceiver);
                context.unregisterReceiver(internetActionReceiver);
                context.unregisterReceiver(timeChangeCallback);
            } catch (Exception ignored) {
            }
        }
    }

    //region Image Rendering
    // Deprecated on 5-Jan-2024
    // Remove once it is not used
    // Use displayBase64Image instead
    @JavascriptInterface
    public void renderBase64Image(String url, String id, boolean fitCenter, String imgScaleType) {
        renderBase64ImageFile(url.contains("http") ? MobilityCallAPI.getInstance(bridgeComponents.getContext()).callAPI(url, MobilityCallAPI.getBaseHeaders(bridgeComponents.getContext()), null, "GET", false).getResponseBody() : url, id, fitCenter, imgScaleType);
    }

    // Deprecated on 5-Jan-2024
    // Remove once it is not used
    // Use displayBase64Image instead
    @JavascriptInterface
    public void renderBase64ImageFile(String base64Image, String id, boolean fitCenter, String imgScaleType) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (!base64Image.equals("") && id != null && bridgeComponents.getActivity() != null) {
                    LinearLayout layout = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                    if (layout != null) {
                        byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                        Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);
                        ImageView imageView = new ImageView(bridgeComponents.getContext());
                        ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(layout.getWidth(), layout.getHeight());
                        imageView.setLayoutParams(layoutParams);
                        imageView.setImageBitmap(decodedByte);
                        imageView.setScaleType(getScaleTypes(imgScaleType));
                        imageView.setAdjustViewBounds(true);
                        imageView.setClipToOutline(true);
                        layout.removeAllViews();
                        layout.addView(imageView);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public String convertAudioToBase64(String filePath) throws IOException {
        File file = new File(filePath);
        try (FileInputStream fileInputStream = new FileInputStream(file)) {
            byte[] fileBytes = new byte[(int) file.length()];
            fileInputStream.read(fileBytes);
            return Base64.encodeToString(fileBytes, Base64.DEFAULT);
        }
    }

    @JavascriptInterface
    public void encodeToBase64(String url, String callback) {
        ExecutorManager.runOnBackgroundThread(() -> {
            try {
                HttpURLConnection connection = MobilityCallAPI.callAPIConnection(url, MobilityCallAPI.getBaseHeaders(bridgeComponents.getContext()), null, "GET", false);
                Bitmap bm = BitmapFactory.decodeStream(MobilityCallAPI.getResponseStream(connection));
                ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
                bm.compress(Bitmap.CompressFormat.PNG, 90, byteArrayOutputStream);
                while (byteArrayOutputStream.toByteArray().length > 716800) { //reducing to 700KB
                    byteArrayOutputStream.reset();
                    bm.compress(Bitmap.CompressFormat.JPEG, 90, byteArrayOutputStream);
                }
                byte[] byteArray = byteArrayOutputStream.toByteArray();
                bridgeComponents.getJsCallback().addJsToWebView(String.format("window.callUICallback('%s','%s','%s')", callback, "SUCCESS", Base64.encodeToString(byteArray, Base64.DEFAULT).replace("\n", "")));
            } catch (Exception e) {
                bridgeComponents.getJsCallback().addJsToWebView(String.format("window.callUICallback('%s','%s','%s')", callback, "FAILED", ""));
            }
        });
    }

    @JavascriptInterface
    public void displayBase64Image(String configJSONString) {
        try {
            JSONObject configObj = new JSONObject(configJSONString);

            // parse params
            String source = configObj.optString("source", "");
            String id = configObj.optString("id", "");
            String scaleType = configObj.optString("scaleType", "CENTER_CROP");
            int inSampleSize = configObj.optInt("inSampleSize", 1);
            boolean adjustViewBounds = configObj.optBoolean("adjustViewBounds", true);

            // call api to get base64image
            String base64Image = source.startsWith("http") ? MobilityCallAPI.getInstance(bridgeComponents.getContext()).callAPI(source, MobilityCallAPI.getBaseHeaders(bridgeComponents.getContext()), null, "GET", false).getResponseBody() : source;

            // convert base64Image to imageView
            ExecutorManager.runOnMainThread(() -> {
                try {
                    if (!base64Image.equals("") && id != null && bridgeComponents.getActivity() != null) {
                        LinearLayout layout = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                        if (layout != null) {
                            byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                            BitmapFactory.Options options = new BitmapFactory.Options();
                            options.inSampleSize = inSampleSize;
                            Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length, options);

                            ImageView imageView = new ImageView(bridgeComponents.getContext());
                            ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(layout.getWidth(), layout.getHeight());
                            imageView.setLayoutParams(layoutParams);
                            imageView.setImageBitmap(decodedByte);
                            imageView.setScaleType(getScaleTypes(scaleType));
                            imageView.setAdjustViewBounds(adjustViewBounds);
                            imageView.setClipToOutline(true);

                            layout.removeAllViews();
                            layout.addView(imageView);
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    //region Audio Recorder

    // Deprecated on 16-Feb-24
    public void addMediaFile(String viewID, String source, String actionPlayerID, String playIcon, String pauseIcon, String timerID) {
        addMediaFile(viewID, source, actionPlayerID, playIcon, pauseIcon, timerID, false);
    }

    @JavascriptInterface
    public void addMediaFile(String viewID, String source, String actionPlayerID, String playIcon, String pauseIcon, String timerID, boolean autoPlay) {
        if (mediaPlayer == null) mediaPlayer = new MediaPlayer(bridgeComponents);
        mediaPlayer.addMediaFile(viewID, source, actionPlayerID, playIcon, pauseIcon, timerID, autoPlay);
    }

    // Deprecated on 16-Feb-24
    public void addMediaPlayer(String viewID, String source) {
        addMediaPlayer(viewID, source, false);
    }

    @JavascriptInterface
    public void addMediaPlayer(String viewID, String source, boolean autoPlay) {
        if (mediaPlayer == null) mediaPlayer = new MediaPlayer(bridgeComponents);
        mediaPlayer.addMediaPlayer(viewID, source, autoPlay);
    }

    @JavascriptInterface
    public void pauseMediaPlayer() {
        if (mediaPlayer == null) mediaPlayer = new MediaPlayer(bridgeComponents);
        mediaPlayer.pauseMediaPlayer();
    }

    @JavascriptInterface
    public void removeMediaPlayer() {
        if (mediaPlayer == null) mediaPlayer = new MediaPlayer(bridgeComponents);
        mediaPlayer.removeMediaPlayer();
    }

    @JavascriptInterface
    public boolean startAudioRecording(String fileName) {
        if (mediaPlayer == null) mediaPlayer = new MediaPlayer(bridgeComponents);
        return mediaPlayer.startAudioRecording(fileName);
    }

    @JavascriptInterface
    public String stopAudioRecording() {
        if (mediaPlayer == null) mediaPlayer = new MediaPlayer(bridgeComponents);
        return mediaPlayer.stopAudioRecording();
    }

    @JavascriptInterface
    public String saveAudioFile(String source) throws IOException {
        if (mediaPlayer == null) mediaPlayer = new MediaPlayer(bridgeComponents);
        return mediaPlayer.saveAudioFile(source);
    }

    @JavascriptInterface
    public void uploadFile(String ratio, boolean canChooseFromFile) {
        try {
            if (mediaPlayer == null) mediaPlayer = new MediaPlayer(bridgeComponents);
            JSONObject payload = new JSONObject(ratio);
            int imageAspectHeight = payload.optInt("imageAspectHeight", 0);
            int imageAspectWidth = payload.optInt("imageAspectWidth", 0);
            boolean showAccordingToAspectRatio = payload.optBoolean("showAccordingToAspectRatio", false);
            mediaPlayer.uploadFile(imageAspectHeight, imageAspectWidth, showAccordingToAspectRatio, canChooseFromFile);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @JavascriptInterface
    public void startAudioPlayer(String file, String callback) {
        int id = bridgeComponents.getContext().getResources().getIdentifier(file, "raw", bridgeComponents.getContext().getPackageName());
        if (audioPlayer != null) {
            audioPlayer.stop();
            audioPlayer = null;
        }
        try {
            if (id != 0) {
                Uri audioUri = Uri.parse("android.resource://" + bridgeComponents.getContext().getPackageName() + "/" + id);
                audioPlayer = android.media.MediaPlayer.create(bridgeComponents.getContext(), audioUri);
            } else {
                audioPlayer = new android.media.MediaPlayer();
                audioPlayer.setDataSource(file);
                audioPlayer.prepare();
            }
            audioPlayer.setOnPreparedListener((mediaPlayer) -> {
                mediaPlayer.start();
            });
            audioPlayer.setOnCompletionListener((mediaPlayer) -> {
                String js = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                        callback, "COMPLETED");
                bridgeComponents.getJsCallback().addJsToWebView(js);
            });
        } catch (Exception e) {
            Log.e(OTHERS, "Error in  startAudioPlayer : " + e);
            e.printStackTrace();
        }
    }

    @JavascriptInterface
    public void pauseAudioPlayer() {
        if (audioPlayer != null) {
            audioPlayer.pause();
        }
    }

    @JavascriptInterface
    public void clearAudioPlayer() {
        if (audioPlayer != null) {
            audioPlayer.stop();
            audioPlayer = null;
        }
    }


    @Override
    public boolean onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {
            case IMAGE_CAPTURE_REQ_CODE:
                if (resultCode == RESULT_OK) {
                    if (bridgeComponents.getActivity() != null) {
                        if (mediaPlayer != null) mediaPlayer.isUploadPopupOpen = false;
                        captureImage(data, bridgeComponents.getActivity(), bridgeComponents.getContext(), mediaPlayer.showToAspectRatio, mediaPlayer.height, mediaPlayer.width);
                    }
                } else {
                    if (mediaPlayer != null) mediaPlayer.isUploadPopupOpen = false;
                }
                break;
            case CropImage.CROP_IMAGE_ACTIVITY_REQUEST_CODE:
                if (resultCode == RESULT_OK) {
                    new Thread(() -> encodeImageToBase64(data, bridgeComponents.getContext(), null)).start();
                } else if (resultCode == CropImage.CROP_IMAGE_ACTIVITY_RESULT_ERROR_CODE) {
                    CropImage.ActivityResult result = CropImage.getActivityResult(data);
                    Log.e(OVERRIDE, result.getError().toString());
                }
                break;
            case PICK_CONTACT_REQUEST_CODE:
                if (resultCode == RESULT_OK) {
                    Uri contactUri = data.getData();

                    String[] projection = new String[]{
                            ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME,
                            ContactsContract.CommonDataKinds.Phone.NUMBER
                    };

                    try (Cursor cursor = bridgeComponents.getContext().getContentResolver().query(contactUri, projection, null, null, null)) {
                        if (cursor != null && cursor.moveToFirst()) {
                            String name = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME));
                            String phoneNumber = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.NUMBER));

                            Log.d("Contact Info", "Name: " + name + ", Phone Number: " + phoneNumber);
                            callPickContactCallBack(name, phoneNumber);
                        }
                    }
                }
                break;
        }
        return super.onActivityResult(requestCode, resultCode, data);
    }

    @JavascriptInterface
    public String fetchPackageName() {
        return bridgeComponents.getContext().getPackageName();
    }

    @Override
    public boolean onRequestPermissionResult(int requestCode, String[] permissions, int[] grantResults) {
        switch (requestCode) {
            case IMAGE_PERMISSION_REQ_CODE:
                Context context = bridgeComponents.getContext();
                if ((ActivityCompat.checkSelfPermission(context, CAMERA) == PackageManager.PERMISSION_GRANTED)) {
                    if (bridgeComponents.getActivity() != null) {
                        Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                        KeyValueStore.write(context, bridgeComponents.getSdkName(), context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp);
                        Uri photoFile = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", new File(context.getFilesDir(), "IMG_" + timeStamp + ".jpg"));
                        takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
                        Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
                        chooseFromFile.setType("image/*");
                        Intent chooser = Intent.createChooser(takePicture, context.getString(R.string.upload_image));
                        chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[]{chooseFromFile});
                        bridgeComponents.getActivity().startActivityForResult(chooser, IMAGE_CAPTURE_REQ_CODE, null);
                    }
                } else {
                    Toast.makeText(context, context.getString(R.string.please_allow_permission_to_capture_the_image), Toast.LENGTH_SHORT).show();
                }
                break;
            case MediaPlayerView.AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION:
                if (grantResults.length > 0 && grantResults[0] != PackageManager.PERMISSION_GRANTED) {
                    Toast.makeText(bridgeComponents.getContext(), "Permission Denied", Toast.LENGTH_SHORT).show();
                }
                break;
            case REQUEST_CONTACTS:
                boolean flag = ContextCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.READ_CONTACTS) == PackageManager.PERMISSION_GRANTED;
                String contacts;
                try {
                    if (flag) {
                        contacts = getPhoneContacts();
                    } else {
                        JSONArray flagArray = new JSONArray();
                        contacts = flagArray.toString();
                    }
                    contactsStoreCall(contacts);
                } catch (JSONException e) {
                    e.printStackTrace();
                }
                break;
            case PICK_CONTACT_PERMISSION_REQUEST_CODE:
                Intent contactPickerIntent = new Intent(Intent.ACTION_PICK, ContactsContract.CommonDataKinds.Phone.CONTENT_URI);
                bridgeComponents.getActivity().startActivityForResult(contactPickerIntent, PICK_CONTACT_REQUEST_CODE, null);
                break;
            default:
                break;
        }
        return super.onRequestPermissionResult(requestCode, permissions, grantResults);
    }

    //region Store and Trigger CallBack
    @JavascriptInterface
    public void contactPermission() {
        if (ContextCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.READ_CONTACTS) != PackageManager.PERMISSION_GRANTED) {
            if (bridgeComponents.getActivity() != null) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{Manifest.permission.READ_CONTACTS}, REQUEST_CONTACTS);
            }
        } else {
            ExecutorManager.runOnBackgroundThread(() -> {
                try {
                    contactsStoreCall(getPhoneContacts());
                } catch (Exception e) {
                    Log.e(UTILS, "Exception in Contact Permission" + e);
                }
            });
        }
    }

    public String getPhoneContacts() throws JSONException {
        ContentResolver contentResolver = bridgeComponents.getContext().getContentResolver();
        Uri uri = ContactsContract.CommonDataKinds.Phone.CONTENT_URI;
        JSONArray contacts = new JSONArray();
        String[] projection =
                {
                        ContactsContract.CommonDataKinds.Phone.NUMBER,
                        ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME
                };
        String selection = ContactsContract.Contacts.IN_VISIBLE_GROUP + " = '"
                + ("1") + "'";
        String sortOrder = ContactsContract.Contacts.DISPLAY_NAME
                + " COLLATE LOCALIZED ASC";
        try (Cursor cursor = contentResolver.query(uri, projection, selection, null, sortOrder)) {
            if (cursor.getCount() > 0) {
                while (cursor.moveToNext()) {
                    String contactNameStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME));
                    String contactStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.NUMBER));
                    JSONObject tempPoints = new JSONObject();
                    tempPoints.put("name", contactNameStr);
                    tempPoints.put("number", contactStr);
                    contacts.put(tempPoints);
                }
            }
        }
        JSONObject flagObject = new JSONObject();
        flagObject.put("name", "beckn_contacts_flag");
        flagObject.put("number", "true");
        contacts.put(flagObject);
        return contacts.toString();
    }

    public void contactsStoreCall(String contacts) {
        if (storeContactsCallBack != null) {
            String removedDoubleQuotes = contacts.replace("\\\"", "\\\\\"");
            String removedSingleQuote = removedDoubleQuotes.replace("'", "");
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeContactsCallBack, removedSingleQuote);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void storeCallBackContacts(String callback) {
        storeContactsCallBack = callback;
    }

    @JavascriptInterface
    public void initialiseShakeListener(String callback, String config) {
        try {
            JSONObject shakeListenerConfig = new JSONObject(config);
            sensorManager = (SensorManager) bridgeComponents.getContext().getSystemService(Context.SENSOR_SERVICE);
            accelerometer = sensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);
            sensorManager.registerListener(shakeDetector, accelerometer, SensorManager.SENSOR_DELAY_UI);
            float shakeAccelerationThreshold = (float) shakeListenerConfig.optDouble("shakeAccelerationThreshold", 5);
            int consecutiveShakeInterval = shakeListenerConfig.optInt("consecutiveShakeInterval", 500);
            int shakeCountResetTime = shakeListenerConfig.optInt("shakeCountResetTime", 3000);
            shakeDetector = new ShakeDetector(shakeAccelerationThreshold, consecutiveShakeInterval, shakeCountResetTime);
            shakeDetector.setOnShakeListener(new ShakeDetector.OnShakeListener() {
                @Override
                public void onShake(int count) {
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                            callback, count);
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            });
        } catch (Exception e) {
            Log.e(LOG_TAG, e.getMessage());
        }
    }

    @JavascriptInterface
    public void unregisterShakeListener() {
        sensorManager.unregisterListener(shakeDetector);
    }

    @JavascriptInterface
    public void registerShakeListener() {
        sensorManager.registerListener(shakeDetector, accelerometer, SensorManager.SENSOR_DELAY_UI);
    }

    @JavascriptInterface
    public String fetchFilesFromFolderPath(String path) {
        File[] files = FileUtils.getFilesInFolderPath(path);
        String[] filePaths = files != null ? new String[files.length] : new String[0];
        for (int i = 0; i < files.length; i++) {
            filePaths[i] = files[i].getAbsolutePath();
        }
        return Arrays.toString(filePaths);
    }

    public boolean requestUninstallPackage(String packageName) {
        try {
            Intent intent = new Intent(Intent.ACTION_DELETE);
            intent.setData(Uri.parse("package:" + packageName));
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            bridgeComponents.getContext().startActivity(intent);
            return true; // requested successfully
        } catch (Exception ignored) {
            return false;// request failed
        }
    }

    public boolean isPackageInstalled(String packageName) {
        PackageManager packageManager = bridgeComponents.getContext().getPackageManager();
        if (packageName == null || packageName.isEmpty()) {
            return false;
        }
        try {
            packageManager.getPackageInfo(packageName, 0);
            return true;
        } catch (PackageManager.NameNotFoundException e) {
            return false;
        }
    }


    @JavascriptInterface
    public boolean checkIfPointInsidePolygon(String geoJson, double latitude, double longitude) {
        List<List<LatLng>> polygonCoordinates = getPolygonCoordinates(geoJson);
        Boolean res = pointInsidePolygon(polygonCoordinates, latitude, longitude);
        return res;
    }

    @JavascriptInterface
    public String rsEncryption(String phoneNumber) {
        return CipherUtil.getInstance().encryptData(phoneNumber);
    }


    @JavascriptInterface
    public void storeCallBackPickContact(String callback) {
        storePickContactCallBack = callback;
    }

    public void callPickContactCallBack(String name, String phoneNumber) {
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                storePickContactCallBack, name, phoneNumber);
        bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }

    @JavascriptInterface
    public void pickContact() {
        if (ContextCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.READ_CONTACTS) != PackageManager.PERMISSION_GRANTED) {
            if (bridgeComponents.getActivity() != null) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{Manifest.permission.READ_CONTACTS}, PICK_CONTACT_PERMISSION_REQUEST_CODE);
            }
        } else {
            ExecutorManager.runOnMainThread(() -> {
                try {
                    Intent contactPickerIntent = new Intent(Intent.ACTION_PICK, ContactsContract.CommonDataKinds.Phone.CONTENT_URI);
                    bridgeComponents.getActivity().startActivityForResult(contactPickerIntent, PICK_CONTACT_REQUEST_CODE, null);
                } catch (Exception e) {
                    Log.e(UTILS, "Exception in Contact Permission" + e);
                }
            });
        }
    }

    @JavascriptInterface
    public void voiceToText(String callback) {
        new Handler(Looper.getMainLooper()).post(() -> {
            if (speechRecognition == null) {
                speechRecognition = new SpeechRecognition(bridgeComponents);
            }
            speechRecognition.requestPermissions();
            Intent intent = speechRecognition.setIntent();
            speechRecognition.setupSpeechRecognition(callback);
            speechRecognition.startListening(intent);
        });
    }

    @JavascriptInterface
    public void stopVoiceRecognition() {
        new Handler(Looper.getMainLooper()).post(() -> {
            if (speechRecognition != null) {
                speechRecognition.stopListening();
                speechRecognition = null;
            }
        });
    }

    @JavascriptInterface
    public void startVoiceRecognition() {
        new Handler(Looper.getMainLooper()).post(() -> {
            if (speechRecognition != null) {
                speechRecognition.requestPermissions();
                Intent intent = speechRecognition.setIntent();
                speechRecognition.startListening(intent);
            }
        });
    }

    @JavascriptInterface
    public void setupVoiceRecognitionView(String viewId) {
        Log.i(LOG_TAG, "setupVoiceRecognitionView is getting called");
        ExecutorManager.runOnMainThread(() -> {
            if (speechRecognition == null) {
                Log.i(LOG_TAG, "setupVoiceRecognitionView is created");
                speechRecognition = new SpeechRecognition(bridgeComponents);
            }
            speechRecognition.setupSpeechRecognitionView(viewId);
        });
    }

}

