/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

 
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
import in.juspay.hypersdk.core.DuiCallback;
import in.juspay.hypersdk.core.HyperFragment;
import in.juspay.hypersdk.core.JBridge;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.utils.network.JuspayHttpResponse;
import in.juspay.hypersdk.utils.network.NetUtils;

import static android.Manifest.permission.ACCESS_FINE_LOCATION;
import static androidx.constraintlayout.widget.Constraints.TAG;

public class JsInterface extends CommonJsInterface implements in.juspay.hypersdk.core.JSI {

    private static final String LOG_TAG = "Beckn_JsInterface";
    private Activity activity;
    private JuspayServices juspayServices;
    private Context context;
    private DuiCallback dynamicUI;
    private GoogleMap googleMap;
    private static final int MAP_ZOOM_LEVEL = 12;
    private FusedLocationProviderClient client;
    private Marker userPositionMarker;


    public JsInterface(){
        super();
    }


    public JsInterface(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super(activity, juspayServices, fragment);
        try {
            JSONObject headerObj = new JSONObject();
            headerObj.put("x-client-id", "jatrisaathidriver");
            boolean b = setAnalyticsHeader(headerObj.toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.context = activity;
        this.dynamicUI = juspayServices.getDuiCallback();
        this.activity = activity;
        this.juspayServices = juspayServices;
        // TODO :: Clarify with Daya
        //client = LocationServices.getFusedLocationProviderClient(context);
    }


    @Override
    public void setActivity(Activity activity) {
        super.setActivity(activity);
        this.activity = activity;
    }

    @Override
    public void set(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super.set(activity, juspayServices, fragment);
        this.context = juspayServices.getContext();
        this.dynamicUI = juspayServices.getDuiCallback();
        this.activity = activity;
        this.juspayServices = juspayServices;
//        polylines = new ArrayList<>();
        try {
            JSONObject headerObj = new JSONObject();
            headerObj.put("x-client-id", "jatrisaathidriver");
            boolean b = setAnalyticsHeader(headerObj.toString());
            } catch (Exception e) {
            e.printStackTrace();
        }

    }

}

