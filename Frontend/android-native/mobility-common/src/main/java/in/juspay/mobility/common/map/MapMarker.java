package in.juspay.mobility.common.map;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.PorterDuff;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.LinearInterpolator;
import android.webkit.JavascriptInterface;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.maps.android.SphericalUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Locale;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.MobilityCommonBridge;
import in.juspay.mobility.common.R;

public class MapMarker {

    private Marker marker;
    private MarkerOptions markerOptions;

    public MapMarker(GoogleMap googleMap){
        markerOptions = new MarkerOptions();
        marker = googleMap.addMarker(markerOptions);
    }

    public void addMarker(GoogleMap googleMap, String config){
        try {
            JSONObject configObj = new JSONObject(config);


            if(configObj.has("title") && configObj.get("title") != ""){
            }

            String title = configObj.optString("title", "");
            double lat = configObj.optDouble("lat", 0.0);
            double lon = configObj.optDouble("lon", 0.0);
            int size = configObj.optInt("size", 0);
            boolean visible = configObj.optBoolean("visible", false);
            boolean flat = configObj.optBoolean("flat", false);
            boolean hideInfoWindow = configObj.optBoolean("hideInfoWindow", true);

            markerOptions.title(title)
                    .position(new LatLng(lat, lon))
                    .
        }catch (Exception e){

        }
    }
}

firstRide -> consumer
verifyOTP -> driverNY


