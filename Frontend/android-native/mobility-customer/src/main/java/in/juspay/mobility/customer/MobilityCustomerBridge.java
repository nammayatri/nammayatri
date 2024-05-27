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
import android.net.Uri;
import android.os.Build;
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

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.ButtCap;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.PatternItem;
import com.google.android.gms.maps.model.Polyline;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;

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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.mobility.common.MapRemoteConfig;
import in.juspay.mobility.common.MobilityCommonBridge;

public class MobilityCustomerBridge extends MobilityCommonBridge {

    @Override
    public void reset() {
        super.reset();
    }

    // CallBacks Strings
    public enum MapMode {
        NORMAL, SPECIAL_ZONE, HOTSPOT
    }
    private Integer debounceAnimateCameraCounter;

    public MobilityCustomerBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        app = AppType.CONSUMER;
    }

    // endregion

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
            try {
                JSONObject payload = new JSONObject(_payload);
                String pureScriptID = payload.optString("pureScriptID","");
                GoogleMap gMap = pureScriptID.isEmpty() ? googleMap : googleMapInstance.get(pureScriptID);
                if (gMap != null) {
                    try {

                        MapRemoteConfig mapRemoteConfig = getMapRemoteConfig();
                        float zoomLevel;
                        try {
                            zoomLevel = (float) mapRemoteConfig.zoomLevel;
                        }catch (Exception e){
                            zoomLevel = 20.0f;
                        }

                        String json = payload.optString("json", "");
                        String dest = payload.optString("destMarker", "");
                        String eta = payload.optString("eta", "");
                        String src = payload.optString("srcMarker", "");
                        String specialLocation = payload.optString("specialLocation", "");
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
                        currMarker.setTitle("Vehicle Icon On Map");
                        Marker destMarker = (Marker) markers.get(dest);
                        String destinationSpecialTagIcon = specialLocationObject.getString("destSpecialTagIcon");
                        MarkerConfig markerConfig = new MarkerConfig();
                        markerConfig.locationName(eta);
                        markerConfig.setLabelImage(destinationSpecialTagIcon);
                        destMarker.setIcon((BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(dest, false, null, MarkerType.NORMAL_MARKER, markerConfig))));
                        destMarker.setTitle("Driver is " + eta);
                        if (polyline != null) {
                            polyline.setEndCap(new ButtCap());
                            if (path.isEmpty()) {
                                LatLng destination = destMarker.getPosition();
                                animateMarkerNew(src, destination, currMarker);
                                if(overlayPolylines != null) {
                                    overlayPolylines.remove();
                                    overlayPolylines = null;
                                }
                                polyline.remove();
                                polyline = null;
                                mapUpdate.isMapMoved = false;
                                mapUpdate.isMapIdle = true;
                                currMarker.setAnchor(0.5f, 0);
                                animateCamera(destMarker.getPosition().latitude, destMarker.getPosition().longitude, zoomLevel, ZoomType.ZOOM);
                            } else {
                                double destinationLat = path.get(0).latitude;
                                double destinationLon = path.get(0).longitude;
                                double sourceLat = path.get(path.size() - 1).latitude;
                                double sourceLong = path.get(path.size() - 1).longitude;
                                LatLng destination = path.get(path.size() - 1);
                                animateMarkerNew(src, destination, currMarker);
                                PatternItem dash = new Dash(dashUnit);
                                PatternItem gap = new Gap(gapUnit);
                                List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Arrays.asList(dash, gap);
                                polyline.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                                polyline.setPoints(path);
                                if(debounceAnimateCameraCounter == null) debounceAnimateCameraCounter = mapRemoteConfig.debounceAnimateCameraCounter;
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

    
    @JavascriptInterface
    public void updateRouteV2(String _payload) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject payload = new JSONObject(_payload);
                String pureScriptID = payload.optString("pureScriptID","");
                GoogleMap gMap = pureScriptID.isEmpty() ? googleMap : googleMapInstance.get(pureScriptID);
                if (gMap != null) {
                    try {
                    MapRemoteConfig mapRemoteConfig = getMapRemoteConfig();
                    float zoomLevel;
                    try {
                        zoomLevel = (float) mapRemoteConfig.zoomLevel;
                    }catch (Exception e){
                        zoomLevel = 20.0f;
                    }
                    String json = payload.optString("json", "");
                    String dest = payload.optString("destMarker", "");
                    String eta = payload.optString("eta", "");
                    String src = payload.optString("srcMarker", "");
                    String specialLocation = payload.optString("specialLocation", "");
                    String polylineKey = payload.optString("polylineKey", "DEFAULT");
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
                    currMarker.setTitle("Vehicle Icon On Map");
                    Marker destMarker = (Marker) markers.get(dest);
                    String destinationSpecialTagIcon = specialLocationObject.getString("destSpecialTagIcon");
                    MarkerConfig markerConfig = new MarkerConfig();
                    markerConfig.locationName(eta);
                    markerConfig.setLabelImage(destinationSpecialTagIcon);
                    destMarker.setIcon((BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(dest, false,null, MarkerType.NORMAL_MARKER, markerConfig))));
                    destMarker.setTitle("Driver is " + eta);
                    PolylineDataPoints polylineDataPoints = polylines.get(polylineKey);
                    Polyline polyline = getPolyLine(false, polylineDataPoints);
                    if (polyline != null) {
                        polyline.setEndCap(new ButtCap());
                        if (path.size() == 0) {
                            LatLng destination = destMarker.getPosition();
                            animateMarkerNew(src, destination, currMarker);
                            Polyline overlayPolylines = getPolyLine(true, polylineDataPoints);
                            if(overlayPolylines != null) {
                                overlayPolylines.remove();
                            }
                            polyline.remove();
                            polylineDataPoints.setPolyline(null);
                            polylineDataPoints.setOverlayPolylines(null);
                            polylines.put(polylineKey,polylineDataPoints);
                            currMarker.setAnchor(0.5f, 0);
                            mapUpdate.isMapMoved = false;
                            mapUpdate.isMapIdle = true;
                            animateCamera(destMarker.getPosition().latitude, destMarker.getPosition().longitude, zoomLevel, ZoomType.ZOOM);
                        } else {
                                double destinationLat = path.get(0).latitude;
                                double destinationLon = path.get(0).longitude;
                                double sourceLat = path.get(path.size() - 1).latitude;
                                double sourceLong = path.get(path.size() - 1).longitude;
                                LatLng destination = path.get(path.size() - 1);
                                animateMarkerNew(src, destination, currMarker);
                                PatternItem dash = new Dash(dashUnit);
                                PatternItem gap = new Gap(gapUnit);
                                List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Arrays.asList(dash, gap);
                                polyline.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                                polyline.setPoints(path);
                                if(debounceAnimateCameraCounter == null) debounceAnimateCameraCounter = mapRemoteConfig.debounceAnimateCameraCounter;
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

    @JavascriptInterface
    public void removeLabelFromMarker(float zoomLevel) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                zoom = zoomLevel;
                if (layer != null) {
                    layer.removeLayerFromMap();
                }
                if (userPositionMarker != null) {
                    userPositionMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(CURRENT_LOCATION, false,null, MarkerType.NORMAL_MARKER, new MarkerConfig())));
                    userPositionMarker.setTitle("");
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
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
        if(selectedRide.getString("destination").equals(""))
        {
            invoiceLayout.findViewById(R.id.destination).setVisibility(View.GONE);
            invoiceLayout.findViewById(R.id.rideEndTime).setVisibility(View.GONE);
            invoiceLayout.findViewById(R.id.destRedCircle).setVisibility(View.GONE);
            invoiceLayout.findViewById(R.id.dashedLine).setVisibility(View.GONE);
        }

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
            default:
                break;
        }
        return super.onRequestPermissionResult(requestCode, permissions, grantResults);
    }
    //endregion
}
