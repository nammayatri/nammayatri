package in.juspay.mobility.common.map;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.AnimatorSet;
import android.animation.ArgbEvaluator;
import android.animation.ValueAnimator;
import android.graphics.Color;
import android.graphics.Gainmap;
import android.util.Log;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.view.animation.AccelerateInterpolator;
import android.webkit.JavascriptInterface;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.ButtCap;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Dot;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PatternItem;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.maps.android.SphericalUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.MapRemoteConfig;
import in.juspay.mobility.common.MobilityCommonBridge;

public class Route {
    private Polyline polyline;
    private final PolylineOptions polylineOptions;

    private int gap;
    private int dash;
    private boolean isAnimationNeeded;
    private String LOG_TAG = "ROUTE_CLASS";


    public Route(GoogleMap googleMap){
        this.polylineOptions = new PolylineOptions();
        this.polyline = googleMap.addPolyline(this.polylineOptions);
        gap = 10;
        dash = 20;
        isAnimationNeeded = false;
    }

    public void drawRoute(GoogleMap googleMap, String config) {
        try {
            ExecutorManager.runOnMainThread(() -> {
                if (googleMap != null) {
                    try {
                        JSONObject configObj = new JSONObject(config);

                        JSONArray coordinates = configObj.getJSONArray("points");
                        String style = configObj.optString("style", "DOT");
                        int color = Color.parseColor(configObj.optString("color", ""));
                        int width = configObj.optInt("width", 1);
                        JSONObject animationConfig = configObj.getJSONObject("animationConfig");

                        for (int i = coordinates.length() - 1; i >= 0; i--) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            polylineOptions.add(new LatLng(lat, lng));
                        }

                        polylineOptions.width(width);
                        polylineOptions.color(color);

                        switch (style) {
                            case "DASH":
                                PatternItem DASH = new Dash(dash);
                                List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Collections.singletonList(DASH);
                                polylineOptions.pattern(PATTERN_POLYLINE_DOTTED_DASHED);
                                break;
                            case "DOT":
                                PatternItem DOT = new Dot();
                                PatternItem GAP = new Gap(gap);
                                List<PatternItem> PATTERN_POLYLINE_DOTTED = Arrays.asList(GAP, DOT);
                                polylineOptions.pattern(PATTERN_POLYLINE_DOTTED);
                                break;
                            default:
                                break;
                        }

                        polyline = googleMap.addPolyline(polylineOptions);

                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            });
        }catch(Exception e){
            e.printStackTrace();
        }
    }

    public void updateRoute(GoogleMap googleMap, String _payload) {
        ExecutorManager.runOnMainThread(() -> {
            if (googleMap != null) {
                try {
                    JSONObject configObj = new JSONObject(_payload);

                    ArrayList<LatLng> path = new ArrayList<>();
                    JSONArray coordinates = configObj.getJSONArray("points");

                    for (int i = coordinates.length() - 1; i >= 0; i--) {
                        JSONObject coordinate = (JSONObject) coordinates.get(i);
                        double lng = coordinate.getDouble("lng");
                        double lat = coordinate.getDouble("lat");
                        LatLng tempPoint = new LatLng(lat, lng);
                        path.add(tempPoint);
                    }

                    if (polyline != null) {
                        if (path.size() == 0) {
                            polyline.remove();
                        } else {
                            polyline.setPoints(path);
                        }
                    }
                } catch (JSONException e) {
                    Log.e(LOG_TAG, "Error in updateRoute", e);
                }
            }
        });
    }
}
