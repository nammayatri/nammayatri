package in.juspay.mobility.common.map;

import android.graphics.Color;
import android.util.Log;

import androidx.annotation.Nullable;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Dot;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.PatternItem;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.MapRemoteConfig;
import in.juspay.mobility.common.Utils;

public class Route {
    @Nullable private Polyline polyline;
    @Nullable private Polyline overlayPolyline;

    private final PolylineOptions polylineOptions;

    private final int gap;
    private final int dash;
    private final String ROUTE = "ROUTE_CLASS";

    private final HashSet<String> intermediateMarkers;
    private String startMarker;
    private String endMarker;

    private String style = "";
    private int width = 0;
    private int color = 0;

    public double startLat = 0.0, startLon = 0.0, endLat = 0.0, endLon = 0.0;
    public String nextRouteID = "";
    public String prevRouteID = "";

    public int maximumDebounceCounter;
    public int currentDebounceCounter = 0;

    @Nullable private Timer polylineAnimationTimer = null;


    public Route(BridgeComponents bridgeComponents){
        this.polylineOptions = new PolylineOptions();
        this.intermediateMarkers = new HashSet<>();
        MapRemoteConfig mapRemoteConfig=  new Map(bridgeComponents).getMapRemoteConfig();
        this.maximumDebounceCounter = mapRemoteConfig.debounceAnimateCameraCounter;
        gap = 10;
        dash = 20;
    }

    private void checkAndAnimatePolyline(GoogleMap googleMap, JSONObject configObj){
        try{
            boolean animation = configObj.optBoolean("animation", true);

            if(!animation && Utils.getDeviceRAM() <= 3){
                return ;
            }

            int animateColor = Color.parseColor(configObj.optString("color", "#D1D5DB"));

            PolylineOptions overlayPolylineOptions = new PolylineOptions();
            overlayPolylineOptions.color(animateColor);
            overlayPolylineOptions.pattern(getPattern(style));
            overlayPolylineOptions.width(width);
            overlayPolyline = googleMap.addPolyline(overlayPolylineOptions);

            if(polylineAnimationTimer!=null){
                polylineAnimationTimer.cancel();
            }

            this.polylineAnimationTimer = new Timer();

            polylineAnimationTimer.schedule(new TimerTask() {
                private float drawDone = 0;
                @Override
                public void run() {
                    if (drawDone <= 28) {
                        drawDone += 2;
                    } else if (drawDone <= 66) {
                        drawDone += 4;
                    }else if (drawDone <= 98){
                        drawDone += 2;
                    }else if (drawDone <= 200){
                        drawDone += 2;
                    }else {
                        drawDone = 0;
                    }


                    if(drawDone >= 0 && drawDone <= 100){ // Drawing Phase
                        ExecutorManager.runOnMainThread(() -> {
                            try{
                                if (polyline != null) {
                                    List<LatLng> foregroundPoints = polyline.getPoints();
                                    Collections.reverse(foregroundPoints);

                                    int pointCount = foregroundPoints.size();
                                    int countToBeRemoved = (int) (pointCount * (drawDone / 100.0f));
                                    foregroundPoints.subList(Math.max(0,foregroundPoints.size() - countToBeRemoved), foregroundPoints.size()).clear();

                                    if (polyline.getColor() != animateColor)
                                        polyline.setColor(animateColor);
                                    if (overlayPolyline != null) {
                                        if (overlayPolyline.getColor() != color)
                                            overlayPolyline.setColor(color);
                                        overlayPolyline.setPoints(foregroundPoints);
                                    }
                                }
                            }catch (Exception e){
                                e.printStackTrace();
                            }
                        });
                    }else if(drawDone > 100 && drawDone <= 200){ // Fading Phase
                        ExecutorManager.runOnMainThread(() -> {
                            try {
                                float alpha = (float) ((drawDone - 100.0) / 100.0);
                                final float[] fromColor = new float[3], toColor =   new float[3], currColor = new float[3];
                                Color.colorToHSV(animateColor, fromColor);
                                Color.colorToHSV(color, toColor);

                                currColor[0] = fromColor[0] + (toColor[0] - fromColor[0])*alpha;
                                currColor[1] = fromColor[1] + (toColor[1] - fromColor[1])*alpha;
                                currColor[2] = fromColor[2] + (toColor[2] - fromColor[2])*alpha;

                                int newColor = Color.HSVToColor(currColor);
                                if (polyline != null && polyline.getColor() != newColor)
                                    polyline.setColor(newColor);

                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        });
                    }
                }
            }, 200, 15);
        }catch (Exception e){
            e.printStackTrace();
        }
    }

    public void drawRoute(GoogleMap googleMap, MarkerHandler markerHandler, JSONObject configObj) {
        try {
            ExecutorManager.runOnMainThread(() -> {
                if (googleMap != null) {
                    try {
                        JSONObject pointsObj = configObj.getJSONObject("points");
                        JSONArray coordinates = pointsObj.getJSONArray("points");

                        boolean isStraightRoute = configObj.optBoolean("straightLine", false);

                        for (int i = 0; i < coordinates.length(); i++) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            if(i == 0){
                                startLat = lat;
                                startLon = lng;
                            }

                            if(i == coordinates.length()-1){
                                endLat = lat;
                                endLon = lng;
                            }

                            if(isStraightRoute && (i >= 1 && i <= coordinates.length() -2)){
                                continue;
                            }

                            polylineOptions.add(new LatLng(lat, lng));
                        }

                        this.style = configObj.optString("style", "LineString");
                        this.color = Color.parseColor(configObj.optString("color", ""));
                        this.width = configObj.optInt("width", 1);

                        polylineOptions.width(width);
                        polylineOptions.color(color);
                        polylineOptions.pattern(getPattern(style));

                        polyline = googleMap.addPolyline(polylineOptions);

                        JSONObject startMarkerConfig = configObj.getJSONObject("startMarker");
                        JSONObject endMarkerConfig = configObj.getJSONObject("endMarker");

                        startMarkerConfig.put("lat", startLat);
                        startMarkerConfig.put("lon", startLon);
                        endMarkerConfig.put("lat", endLat);
                        endMarkerConfig.put("lon", endLon);

                        if(startMarkerConfig.optBoolean("rotation", false) && coordinates.length() >= 2){
                            JSONObject startIconLocation = (JSONObject) coordinates.get(0);
                            double startIconLocationLng = startIconLocation.getDouble("lng");
                            double startIconLocationLat = startIconLocation.getDouble("lat");

                            JSONObject nextPointLocation = (JSONObject) coordinates.get(1);
                            double nextPointLocationLng = nextPointLocation.getDouble("lng");
                            double nextPointLocationLat = nextPointLocation.getDouble("lat");

                            startMarkerConfig.put("rotationDegree", (float) SphericalUtil.computeHeading(new LatLng(startIconLocationLat, startIconLocationLng), new LatLng(nextPointLocationLat, nextPointLocationLng)));
                        }

                        startMarker = markerHandler.addMarker(startMarkerConfig);
                        endMarker = markerHandler.addMarker(endMarkerConfig);

                        JSONArray intermediateMarkersConfig = configObj.optJSONArray("intermediateMarkers");
                        for(int i = 0; intermediateMarkersConfig!= null && i<intermediateMarkersConfig.length(); i++){
                            JSONObject intermediateMarkerConfig = intermediateMarkersConfig.getJSONObject(i);
                            intermediateMarkers.add(markerHandler.addMarker(intermediateMarkerConfig));
                        }

                        maximumDebounceCounter = configObj.optInt("debounceCounter", -1) != -1 ? maximumDebounceCounter : configObj.getInt("debounceCounter");
                        currentDebounceCounter = maximumDebounceCounter;

                        JSONObject animationConfig = configObj.getJSONObject("animationConfig");
                        checkAndAnimatePolyline(googleMap, animationConfig);
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            });
        }catch(Exception e){
            e.printStackTrace();
        }
    }

    public void updateRoute(GoogleMap googleMap, MarkerHandler markerHandler, JSONObject configObj) {
        ExecutorManager.runOnMainThread(() -> {
            if (googleMap != null) {
                try {
                    ArrayList<LatLng> path = new ArrayList<>();
                    JSONObject points = configObj.getJSONObject("points");
                    JSONArray coordinates = points.getJSONArray("points");

                    for (int i = 0; i < coordinates.length(); i++) {
                        JSONObject coordinate = (JSONObject) coordinates.get(i);
                        double lng = coordinate.getDouble("lng");
                        double lat = coordinate.getDouble("lat");
                        LatLng latLng = new LatLng(lat, lng);

                        if(i == 0){
                            startLat = lat;
                            startLon = lng;
                            markerHandler.moveMarker(startMarker, latLng, true);
                        }
                        if(i == coordinates.length() - 1){
                            endLat = lat;
                            endLon = lng;
                            markerHandler.moveMarker(endMarker, latLng, false);
                        }

                        path.add(latLng);
                    }

                    for(String intermediateMarkerId : intermediateMarkers) {
                        @Nullable LatLng intermediateMarkerPos = markerHandler.getCurrentPosition(intermediateMarkerId);

                        if (intermediateMarkerPos != null)
                            if (PolyUtil.locationIndexOnEdgeOrPath(intermediateMarkerPos, path, false, false, 1.0) == -1) {
                                markerHandler.removeMarker(intermediateMarkerId);
                            }
                    }

                    if (polyline != null) {
                        if (path.size() == 0) {
                            polyline.remove();
                            if(overlayPolyline != null)
                                overlayPolyline.remove();
                        } else {
                            polyline.setPoints(path);
                        }
                    }

                } catch (JSONException e) {
                    Log.e(ROUTE, "Error in updateRoute", e);
                }
            }
        });
    }



    public void removeRoute(MarkerHandler markerHandler){
        ExecutorManager.runOnMainThread(() -> {
            if (startMarker != null && !startMarker.equals("")) {
                markerHandler.removeMarker(startMarker);
            }
            if (endMarker != null && !endMarker.equals("")) {
                markerHandler.removeMarker(endMarker);
            }

            for (String intermediateMarker : intermediateMarkers) {
                markerHandler.removeMarker(intermediateMarker);
            }
            if (polyline != null) {
                polyline.remove();
                polyline = null;
            }
            if(overlayPolyline != null){
                overlayPolyline.remove();
                overlayPolyline = null;
            }
            if(polylineAnimationTimer!=null){
                polylineAnimationTimer.cancel();
            }
        });
    }

    public List<LatLng> getPoints(){
        return polyline == null ? new ArrayList<>() : polyline.getPoints();
    }

    private List<PatternItem> getPattern(String patternStyle){
        if (patternStyle.equals("DOT")) {
            PatternItem DOT = new Dot();
            PatternItem GAP = new Gap(gap);
            return Arrays.asList(GAP, DOT);
        }
        PatternItem DASH = new Dash(dash);
        return Collections.singletonList(DASH);
    }
}
