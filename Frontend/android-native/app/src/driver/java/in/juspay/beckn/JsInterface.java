package in.juspay.mobility;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.DatePickerDialog;
import android.app.Notification;
import android.app.TimePickerDialog;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.location.Location;
import android.location.LocationManager;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Looper;
import android.util.Base64;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;
import android.webkit.JavascriptInterface;
import android.widget.DatePicker;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.TimePicker;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.location.LocationManagerCompat;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

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
import com.google.android.gms.location.LocationSettingsStatusCodes;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.MapStyleOptions;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
import com.google.android.libraries.places.api.Places;
import com.google.android.libraries.places.api.model.Place;
import com.google.android.libraries.places.widget.Autocomplete;
import com.google.android.libraries.places.widget.model.AutocompleteActivityMode;
import com.google.android.material.snackbar.Snackbar;
//import com.google.firebase.iid.FirebaseInstanceId;
//import com.google.firebase.iid.InstanceIdResult;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.MultiFormatWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.integration.android.IntentIntegrator;
import com.journeyapps.barcodescanner.BarcodeEncoder;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

import in.juspay.mobility.utils.NotificationUtils;
import in.juspay.hypersdk.HyperFragment;
import in.juspay.hypersdk.core.JBridge;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.utils.network.JuspayHttpResponse;
import in.juspay.hypersdk.utils.network.NetUtils;
import in.juspay.mystique.DynamicUI;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static androidx.constraintlayout.widget.Constraints.TAG;

public class JsInterface extends CommonJsInterface {

    private static final String LOG_TAG = "Beckn_JsInterface";
    private Activity activity;
    private JuspayServices juspayServices;
    private Context context;
    private DynamicUI dynamicUI;
    private GoogleMap googleMap;
    private static final int MAP_ZOOM_LEVEL = 12;
    private FusedLocationProviderClient client;
    private Marker userPositionMarker;

    public JsInterface(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super(activity, juspayServices, fragment);
        try {
            JSONObject headerObj = new JSONObject();
            headerObj.put("x-client-id", "nammayatripartner");
            boolean b = setAnalyticsHeader(headerObj.toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.context = activity;
        this.dynamicUI = juspayServices.getDynamicUI();
        this.activity = activity;
        this.juspayServices = juspayServices;
        // TODO :: Clarify with Daya
        //client = LocationServices.getFusedLocationProviderClient(context);
    }
}

