/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.customer;

import android.Manifest;
import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Color;
import android.graphics.pdf.PdfDocument;
import android.location.Location;
import android.net.Uri;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.provider.ContactsContract;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.animation.LinearInterpolator;
import android.webkit.JavascriptInterface;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.ButtCap;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PatternItem;
import com.google.android.gms.maps.model.PolylineOptions;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;
import com.google.maps.android.data.geojson.GeoJsonLayer;
import com.google.maps.android.data.geojson.GeoJsonPolygonStyle;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.callbacks.CallBack;
import in.juspay.mobility.common.MobilityCommonBridge;

public class MobilityCustomerBridge extends MobilityCommonBridge {

    public int debounceAnimateCameraCounter = 0;

    @Override
    public void reset() {
        super.reset();
    }

    // CallBacks Strings
    private static String storeContactsCallBack = null;
    private static String storeCustomerCallBack = null;
    public enum MapMode {
        NORMAL, SPECIAL_ZONE, HOTSPOT
    }

    public MobilityCustomerBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        if (isClassAvailable("in.juspay.mobility.app.callbacks.CallBack")) {
            CallBack callBack = new CallBack() {
                @Override
                public void customerCallBack(String notificationType, String notificationData) {
                    callingStoreCallCustomer(notificationType, notificationData);
                }

                @Override
                public void driverCallBack(String notificationType, String notificationData) {
                    Log.i(CALLBACK, "No Required");
                }

                @Override
                public void imageUploadCallBack(String encImage, String filename, String filePath) {
                    Log.i(CALLBACK, "No Required");
                }


                @Override
                public void chatCallBack(String message, String sentBy, String time, String len) {
                    Log.i(CALLBACK, "No Required");
                }

                @Override
                public void inAppCallBack(String onTapAction) {
                    Log.i(CALLBACK, "No Required");
                }

                @Override
                public void bundleUpdatedCallBack(String event, JSONObject desc) {
                    Log.i(CALLBACK, "No Required");
                }
            };
            NotificationUtils.registerCallback(callBack);
        }
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
    // endregion

    @JavascriptInterface
    public void storeCallBackCustomer(String callback) {
        storeCustomerCallBack = callback;
    }

    public void callingStoreCallCustomer(String notificationType, String notificationData) {
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                storeCustomerCallBack, notificationType, notificationData.replace("'",""));
        bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }

    @JavascriptInterface
    public void storeCallBackLocateOnMap(String callback) {
        storeLocateOnMapCallBack = callback;
    }

    //region Maps
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

    @JavascriptInterface
    public void updateRoute(String _payload) {
        ExecutorManager.runOnMainThread(() -> {
            if (googleMap != null) {
                try {
                    JSONObject payload = new JSONObject(_payload);
                    String json = payload.optString("json", "");
                    String dest = payload.optString("destMarker", "");
                    String eta = payload.optString("eta", "");
                    String src = payload.optString("srcMarker", "");
                    String specialLocation = payload.optString("specialLocation", "");
                    float zoomLevel = (float)payload.optDouble("zoomLevel", 17.0);
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
                    currMarker.setTitle("Vehicle Icon On Map");
                    Marker destMarker = (Marker) markers.get(dest);
                    JSONObject specialLocationObject = new JSONObject(specialLocation);
                    String destinationSpecialTagIcon = specialLocationObject.getString("destSpecialTagIcon");

                    destMarker.setIcon((BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(eta, dest, false,null, destinationSpecialTagIcon.equals("") ? null : destinationSpecialTagIcon, MarkerType.NORMAL_MARKER))));
                    destMarker.setTitle("Driver is " + eta);
                    if (polyline != null) {
                        polyline.setEndCap(new ButtCap());
                        if (path.size() == 0) {
                            LatLng destination = destMarker.getPosition();
                            animateMarkerNew(src, destination, currMarker);
                            polyline.remove();
                            if(overlayPolylines != null)
                                overlayPolylines.remove();
                            if(polylineAnimatorSet != null){
                                polylineAnimatorSet.cancel();
                            }
                            polyline = null;
                            currMarker.setAnchor(0.5f, 0);
                            animateCamera(destMarker.getPosition().latitude, destMarker.getPosition().longitude, zoomLevel, ZoomType.ZOOM);
                        } else {
                            double destinationLat = path.get(0).latitude;
                            double destinationLon = path.get(0).longitude;
                            double sourceLat = path.get(path.size() - 1).latitude;
                            double sourceLong = path.get(path.size() - 1).longitude;
                            LatLng destination = path.get(path.size() - 1);
                            animateMarkerNew(src, destination, currMarker);
                            PatternItem DASH = new Dash(1);
                            List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Collections.singletonList(DASH);
                            polyline.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                            polyline.setPoints(path);
                            if (debounceAnimateCameraCounter != 0) {
                                debounceAnimateCameraCounter--;
                            } else {
                                if (autoZoom) {
                                    moveCamera(sourceLat, sourceLong, destinationLat, destinationLon, coordinates);
                                    debounceAnimateCameraCounter = 10;
                                }
                            }
                        }
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    private void animateMarkerNew(String src, LatLng destination, final Marker marker) {
        if (marker != null) {

            LatLng startPosition = marker.getPosition();


            ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1);
            valueAnimator.setDuration(2000); // TODO :: get this value from Loacl Storage to maintain sync with PS
            valueAnimator.setInterpolator(new LinearInterpolator());
            valueAnimator.addUpdateListener(animation -> {
                try {
                    float v = animation.getAnimatedFraction();
                    LatLng newPosition = SphericalUtil.interpolate(startPosition, destination, v);
                    float rotation = bearingBetweenLocations(startPosition, destination);
                    if (rotation > 1.0)
                        marker.setRotation(rotation);
                    marker.setPosition(newPosition);
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
        jsonObject.put("place", zoneName);
        jsonObject.put("lat", location.getLatitude());
        jsonObject.put("long", location.getLongitude());
        jsonObject.put("distance", minDist);
        return jsonObject;
    }

    public void addZoneMarker(double lat, double lng, String name, String icon) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                MarkerOptions markerOptionsObj = new MarkerOptions()
                        .title("")
                        .position(new LatLng(lat, lng))
                        .anchor(0.5f, 0.5f)
                        .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView("", icon, false,null,null, MarkerType.SPECIAL_ZONE_MARKER)));
                Marker m = googleMap.addMarker(markerOptionsObj);
                if (m != null) {
                    m.hideInfoWindow();
                    zoneMarkers.put(name,m);
                }
            } catch (Exception e) {
                Log.d("error on pickup markers", e.toString());
            }
        });
    }

    @JavascriptInterface
    public void drawPolygon(String geoJson, String locationName) throws JSONException {

        ExecutorManager.runOnMainThread(() -> {
            if(layer != null){
                layer.removeLayerFromMap();
            }
            if (googleMap != null) {
                try {
                    JSONObject geo = new JSONObject(geoJson);
                    layer = new GeoJsonLayer(googleMap, geo);
                    GeoJsonPolygonStyle polyStyle = layer.getDefaultPolygonStyle();
                    polyStyle.setFillColor(Color.argb(25, 0, 102, 255));
                    polyStyle.setStrokeWidth(3);
                    polyStyle.setStrokeColor(Color.BLUE);
                    if (locationName.length() > 0) {
                        if (userPositionMarker == null) {
                            upsertMarker(CURRENT_LOCATION, String.valueOf(getKeyInNativeSharedPrefKeys("LAST_KNOWN_LAT")), String.valueOf(getKeyInNativeSharedPrefKeys("LAST_KNOWN_LON")), 160, 0.5f, 0.9f); //TODO this function will be removed
                        } else {
                            userPositionMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(locationName, CURRENT_LOCATION, false,null,null, MarkerType.NORMAL_MARKER)));
                            userPositionMarker.setTitle("");
                            LatLng latLng = new LatLng(Double.parseDouble(getKeyInNativeSharedPrefKeys("LAST_KNOWN_LAT")), Double.parseDouble(getKeyInNativeSharedPrefKeys("LAST_KNOWN_LON")));
                        }
                    }
                    layer.addLayerToMap();
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    @JavascriptInterface
    public void removeLabelFromMarker(float zoomLevel) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                zoom = zoomLevel;
                if (layer != null) {
                    layer.removeLayerFromMap();
                }
                if (userPositionMarker != null) {
                    userPositionMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView("", CURRENT_LOCATION, false,null,null, MarkerType.NORMAL_MARKER)));
                    userPositionMarker.setTitle("");
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public void locateOnMap (String _payload){
        try {
              for (Map.Entry<String, Marker> entry : zoneMarkers.entrySet()) {
                    String key = entry.getKey();
                    Marker marker = entry.getValue();
              }
              JSONObject payload = new JSONObject(_payload);
              boolean goToCurrentLocation = payload.optBoolean("goToCurrentLocation", false) ;
              String lat = payload.optString("lat", "0.0");
              String lon = payload.optString("lon", "0.0");
              String geoJson = payload.optString("geoJson", "");
              String points = payload.optString("points", "[]");
              float zoomLevel = (float) payload.optDouble("zoomLevel", 17.0);
              final TextView labelView = payload.optString("labelId", "").equals("") ? null : Objects.requireNonNull(bridgeComponents.getActivity()).findViewById(Integer.parseInt(payload.getString("labelId")));
              final JSONObject hotSpotConfig = locateOnMapConfig != null ? locateOnMapConfig.optJSONObject("hotSpotConfig") : null;
              final boolean enableHotSpot = hotSpotConfig != null && hotSpotConfig.optBoolean("enableHotSpot", false);
              if ((geoJson.equals("") && points.equals("[]") && enableHotSpot) || ((geoJson.equals("") || points.equals("[]")) && !enableHotSpot)){
                locateOnMap(goToCurrentLocation,lat,lon,zoomLevel);
                return;
              }
              ExecutorManager.runOnMainThread(new Runnable() {
                  final MapMode mapMode = points.equals("") ? MapMode.NORMAL : (geoJson.equals("") ? MapMode.HOTSPOT : MapMode.SPECIAL_ZONE);
                  final double goToNearestPointWithinRadius = hotSpotConfig != null ? hotSpotConfig.optDouble("goToNearestPointWithinRadius", 50.0) : 50.0;
                @Override
                public void run() {
                    try {
                        if (zoneMarkers != null) {
                            for (Map.Entry<String, Marker> set : zoneMarkers.entrySet()) {
                                Marker m = set.getValue();
                                m.setVisible(false);
                            }
                        }
                        if (layer != null)
                            layer.removeLayerFromMap();
                        drawPolygon(geoJson, "");
                        removeMarker("ny_ic_customer_current_location");
                        JSONArray zonePoints = new JSONArray(points);
                        for (int i = 0; i < zonePoints.length(); i++) {
                            Double zoneMarkerLat = (Double) zonePoints.getJSONObject(i).get("lat");
                            Double zoneMarkerLon = (Double) zonePoints.getJSONObject(i).get("lng");
                            addZoneMarker(zoneMarkerLat, zoneMarkerLon, zoneMarkerLat + ":" + zoneMarkerLon, "ny_ic_zone_pickup_marker_yellow");
                        }
                        JSONObject nearestPoint = getNearestPoint(googleMap.getCameraPosition().target.latitude, googleMap.getCameraPosition().target.longitude, zonePoints);
                        if (mapMode.equals(MapMode.SPECIAL_ZONE) || SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng(nearestPoint.getDouble("lat"), nearestPoint.getDouble("long"))) < goToNearestPointWithinRadius)
                            animateCamera(nearestPoint.getDouble("lat"), nearestPoint.getDouble("long"), 20.0f, ZoomType.NO_ZOOM);
                    } catch (JSONException e) {
                        e.printStackTrace();
                        e.printStackTrace();
                    }

                    googleMap.setOnCameraMoveStartedListener(new GoogleMap.OnCameraMoveStartedListener() {
                        @Override
                        public void onCameraMoveStarted(int i) {
                            if (labelView != null)
                                labelView.setVisibility(View.INVISIBLE);
                            Marker m = zoneMarkers.get("selectedGate");
                            if (m != null)
                                m.setVisible(false);
                        }
                    });

                    googleMap.setOnCameraIdleListener(() -> {
                        double lat1 = (googleMap.getCameraPosition().target.latitude);
                        double lng = (googleMap.getCameraPosition().target.longitude);
                        ExecutorService executor = Executors.newSingleThreadExecutor();
                        Handler handler = new Handler(Looper.getMainLooper());
                        executor.execute(() -> {
                            try {
                                new Thread(new Runnable() {
                                    @Override
                                    public void run() {
                                        handler.post(() -> {
                                            try {
                                                boolean isPointInsidePolygon = pointInsidePolygon(geoJson, lat1, lng);
                                                boolean isOnSpot = false;
                                                JSONArray zonePoints = new JSONArray(points);
                                                JSONObject nearestPickupPointObj = getNearestPoint(lat1, lng, zonePoints);
                                                double nearestPointLat = nearestPickupPointObj.getDouble("lat");
                                                double nearestPointLng = nearestPickupPointObj.getDouble("long");
                                                double nearestPointDistance = nearestPickupPointObj.getDouble("distance");
                                                if (mapMode.equals(MapMode.SPECIAL_ZONE)) {
                                                    if (isPointInsidePolygon) {
                                                        for (int i = 0; i < zonePoints.length(); i++) {
                                                            if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng((Double) zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"))) <= 1) {
                                                                zoneName = (String) zonePoints.getJSONObject(i).get("place");
                                                                isOnSpot = true;
                                                                Marker m = zoneMarkers.get("selectedGate");
                                                                if (m != null)
                                                                    m.setVisible(false);

                                                                addZoneMarker((Double)zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"), "selectedGate", "ny_ic_selected_zone_pickup_marker_yellow");
                                                            }
                                                        }
                                                        if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target,new LatLng(nearestPointLat, nearestPointLng)) > 1)
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
                                                            addZoneMarker((Double)zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"), "selectedGate", "ny_ic_selected_zone_pickup_marker_yellow");
                                                        }
                                                    }
                                                    if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng(nearestPointLat, nearestPointLng)) > 1 && nearestPointDistance <= goToNearestPointWithinRadius)
                                                        animateCamera(nearestPointLat, nearestPointLng, 20.0f, ZoomType.NO_ZOOM);
                                                    zoneName = "LatLon";
                                                }
                                                boolean sendCallback = storeLocateOnMapCallBack != null && ((mapMode.equals(MapMode.SPECIAL_ZONE) && (!isPointInsidePolygon || isOnSpot)) || (mapMode.equals(MapMode.HOTSPOT) && (isOnSpot || nearestPointDistance > goToNearestPointWithinRadius)));
                                                if (sendCallback) {
                                                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeLocateOnMapCallBack, zoneName, lat1, lng);
                                                    Log.e(CALLBACK, javascript);
                                                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                                }
                                            } catch (Exception e) {
                                                e.printStackTrace();
                                            }
                                            executor.shutdown();
                                        });
                                    }
                                }).start();
                            } catch (Exception e) {
                                Log.e ("api response error",e.toString());
                            }
                        });
                    });
                }
            });
        } catch (Exception e) {
            Log.i(MAPS, "LocateOnMap error for ", e);
        }
    }

    private Boolean pointInsidePolygon(String geoJson, Double latitude, Double longitide) {
        try {
            JSONObject geo = new JSONObject(geoJson);
            JSONArray coor = geo.getJSONArray("coordinates");
            JSONArray coor2 = (JSONArray) coor.get(0);
            JSONArray coor3 = (JSONArray) coor2.get(0);

            double y = latitude;
            double x = longitide;
            boolean inside = false;
            for (int i = 0, j = coor3.length() - 1; i < coor3.length(); j = i++) {
                JSONArray point1 = (JSONArray) coor3.get(i);
                JSONArray point2 = (JSONArray) coor3.get(j);
                double xi = (double) point1.get(0), yi = (double) point1.get(1);
                double xj = (double) point2.get(0), yj = (double) point2.get(1);

                boolean intersect = ((yi > y) != (yj > y)) && (x <= (xj - xi) * (y - yi) / (yj - yi) + xi);
                if (intersect) {
                    inside = !inside;
                }
            }

            return inside;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    private Boolean isServiceable(Double lat, Double lng) {
        StringBuilder result = new StringBuilder();
        regToken = getKeyInNativeSharedPrefKeys("REGISTERATION_TOKEN");
        baseUrl = getKeyInNativeSharedPrefKeys("BASE_URL");
        String version = getKeyInNativeSharedPrefKeys("VERSION_NAME");
        String deviceDetails = getKeyInNativeSharedPrefKeys("DEVICE_DETAILS");
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

    //endregion

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

    private ArrayList<LatLng> getCoordinateEndPoint(JSONArray coor) {
        try {
            ArrayList<Double> all_latitudes = new ArrayList<Double>();
            ArrayList<Double> all_longitudes = new ArrayList<Double>();
            for (int i = 0; i < coor.length(); i++) {
                JSONArray coordinate = (JSONArray) coor.get(i);
                double lat = (double) coordinate.get(1);
                double lon = (double) coordinate.get(0);
                all_latitudes.add(lat);
                all_longitudes.add(lon);
            }

            double minLat = Collections.min(all_latitudes);
            double maxLat = Collections.max(all_latitudes);
            double minLon = Collections.min(all_longitudes);
            double maxLon = Collections.max(all_longitudes);

            double left = minLon - 0.1*(maxLon - minLon);
            double right = maxLon + 0.1*(maxLon - minLon);
            double top = maxLat + 0.1*(maxLat - minLat);
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
    public void fetchAndUpdateCurrentLocation(String callback) {
        fetchAndUpdateCurrentLocation(callback,true);
    }

    @JavascriptInterface
    public void fetchAndUpdateCurrentLocation(String callback, boolean shouldFallback) {
        if (!isLocationPermissionEnabled()) return;
        updateLastKnownLocation(callback, true, ZoomType.ZOOM,shouldFallback);
    }

    //region Others
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

    @JavascriptInterface
    public void generatePDF(String str, String format) throws JSONException {
        invoice = str;
        if (checkAndAskStoragePermission()){
            downloadPDF(str, bridgeComponents.getContext());
        }
    }

    @SuppressLint("MissingPermission")
    public void downloadPDF(String str, Context context) throws JSONException {
        new Thread(() -> {
            try {
                JSONObject state = new JSONObject(str);
                JSONObject data = state.getJSONObject("data");
                String userName = getKeysInSharedPref("USER_NAME");
                String pdfHeading = data.getString("pdfHeading");
                JSONObject selectedItem = data.getJSONObject("selectedItem");
                JSONArray fares = selectedItem.getJSONArray("faresList");
                PdfDocument pdfDocument = new PdfDocument();
                PdfDocument.PageInfo invoicePDF = new PdfDocument.PageInfo.Builder(960, 1338, 1).create();
                PdfDocument.Page page = pdfDocument.startPage(invoicePDF);
                JuspayLogger.d(OTHERS, "PDF Document Created");
                View content = getInvoiceLayout(selectedItem, fares, userName, pdfHeading,  context);
                JuspayLogger.d(OTHERS, "PDF Layout inflated");
                content.measure(page.getCanvas().getWidth(), page.getCanvas().getHeight());
                content.layout(0, 0, page.getCanvas().getWidth(), page.getCanvas().getHeight());
                content.draw(page.getCanvas());
                pdfDocument.finishPage(page);
                JuspayLogger.d(OTHERS, "PDF Document canvas drawn");
                String fileNameformat;
                String serviceName = context.getResources().getString(R.string.service);
                String notificationHeading = context.getString(R.string.invoice_downloaded);
                String notificationSubheading = context.getString(R.string.invoice_for_your_ride_is_downloaded);
                System.out.println("service name ::" + serviceName);
                if (serviceName.equals("yatrisathiconsumer")) {
                    fileNameformat = "YS_RIDE_";
                } else if (serviceName.equals("nammayatriconsumer")) {
                    fileNameformat = "NY_RIDE_";
                    notificationHeading = context.getString(R.string.driver_receipt_downloaded);
                    notificationSubheading = context.getString(R.string.driver_receipt_for_your_ride_is_downloaded);
                } else {
                    fileNameformat = "YATRI_RIDE_";
                }
                fileNameformat = fileNameformat + selectedItem.getString("date") + selectedItem.getString("rideStartTime");
                String removedSpecial = fileNameformat.replaceAll("[^a-zA-Z\\d]", "_");
                JuspayLogger.d(OTHERS, "PDF Document name " + removedSpecial);
                try {
                    File file = checkAndGetFileName(removedSpecial);
                    JuspayLogger.d(OTHERS, "Available File name for PDF" + file.getName());
                    FileOutputStream fos = new FileOutputStream(file);
                    pdfDocument.writeTo(fos);
                    JuspayLogger.d(OTHERS, "PDF Document written to path " + file.getPath());
                    Uri path = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", file);
                    invoice = null;
                    showNotificationWithURI(path, notificationHeading, notificationSubheading, "application/pdf", "Invoice", "Invoice Download");
                } catch (IOException e) {
                    e.printStackTrace();
                }
                pdfDocument.close();
                JuspayLogger.d(OTHERS, "PDF Document closed ");
            } catch (Exception e) {
                JuspayLogger.e(OTHERS, e.toString());
            }
        }).start();
    }

    @SuppressLint("SetTextI18n")
    private View getInvoiceLayout(JSONObject selectedRide, JSONArray fares, String user, String heading, Context context) throws JSONException {
        JuspayLogger.d(OTHERS, "PDF Document inside inflate View");
        View invoiceLayout = LayoutInflater.from(context).inflate(R.layout.invoice_template, null, false);
        JuspayLogger.d(OTHERS, "PDF Document inflated View");
        TextView textView = invoiceLayout.findViewById(R.id.rideDate);
        textView.setText(selectedRide.getString("date"));
        textView = invoiceLayout.findViewById(R.id.userName);
        textView.setText(user.trim());
        textView = invoiceLayout.findViewById(R.id.paymentDetail);
        textView.setText(selectedRide.getString("totalAmount"));
        textView = invoiceLayout.findViewById(R.id.headingText);
        textView.setText(heading);
        LinearLayout fareBreakupElements = invoiceLayout.findViewById(R.id.fareBreakupElements);
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
                JuspayLogger.d(OTHERS, "PDF Document updating fares break ups" + fare);
                String value = fare.getString("price");
                String fareTypes = fare.getString("title");
                TextView textViewText = new TextView(context);
                textViewText.setTextSize(5);
                textViewText.setTextColor(Color.parseColor("#454545"));
                textViewText.setPadding(0, 0, 0, 10);
                textViewText.setText(fareTypes);
                linearLayout.addView(textViewText);
                linearLayout.addView(linearLayoutChild);

                TextView textViewPrice = new TextView(context);
                textViewPrice.setTextSize(5);
                textViewPrice.setPadding(0, 0, 0, 10);
                textViewPrice.setTextColor(Color.parseColor("#454545"));
                textViewPrice.setText(value);
                linearLayout.addView(textViewPrice);

                fareBreakupElements.addView(linearLayout);
                JuspayLogger.d(OTHERS, "PDF Document updated the fare " + fare + "in view");
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
        JuspayLogger.d(OTHERS, "PDF Document view updated and returning the view");
        return invoiceLayout;
    }

    @JavascriptInterface
    public int methodArgumentCount(String functionName) {
        try {
            methods = methods == null ? this.getClass().getMethods() : methods;
            for (Method m : methods) {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                    if (m.getName().equals(functionName)) {
                        return m.getParameterCount();
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }
    //endregion

    //region Override Functions
    @Override
    public boolean onActivityResult(int requestCode, int resultCode, Intent data) {
        return super.onActivityResult(requestCode, resultCode, data);
    }

    @Override
    public boolean onRequestPermissionResult(int requestCode, String[] permissions, int[] grantResults) {
        switch (requestCode) {
            case REQUEST_CALL:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    showDialer(phoneNumber, true);
                } else {
                    showDialer(phoneNumber, false);
                    toast("Permission Denied");
                }
                break;
            case LOCATION_PERMISSION_REQ_CODE:
                if (grantResults.length > 0 && grantResults[0] != PackageManager.PERMISSION_GRANTED) {
                    toast("Permission Denied");
                }
                break;
            case STORAGE_PERMISSION:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    if (invoice != null){
                        try {
                            JuspayLogger.d(OTHERS, "Storage Permission is granted. downloading  PDF");
                            downloadPDF(invoice, bridgeComponents.getContext());
                        } catch (JSONException e) {
                            e.printStackTrace();
                        }
                    } else if (downloadLayout != null) {
                        try {
                            JuspayLogger.d(OTHERS, "Storage Permission is granted. downloading  PDF");
                            downloadLayoutAsImage(downloadLayout);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                } else {
                    JuspayLogger.d(OTHERS, "Storage Permission is denied.");
                    toast("Permission Denied");
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
            default:
                break;
        }
        return super.onRequestPermissionResult(requestCode, permissions, grantResults);
    }
    //endregion
}
