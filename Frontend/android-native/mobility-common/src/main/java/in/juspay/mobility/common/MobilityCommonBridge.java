package in.juspay.mobility.common;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.location.Location;
import android.util.Log;
import android.webkit.JavascriptInterface;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.Priority;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.tasks.CancellationTokenSource;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;

import org.json.JSONObject;

import java.util.Locale;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hypersdk.data.KeyValueStore;

public class MobilityCommonBridge extends HyperBridge {

    //Maps
    protected JSONObject markers = new JSONObject();
    protected GoogleMap googleMap;
    protected Polyline polyline = null;
    private Marker userPositionMarker;

    //Location
    protected double lastLatitudeValue;
    protected double lastLongitudeValue;
    CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();
    private FusedLocationProviderClient client;

    //LOG_TAGS
    protected String MAPS = "MAPS";
    protected String LOCATION = "LOCATION";
    protected String UTILS = "UTILS";

    //Constants
    private static final String LOCATE_ON_MAP = "LocateOnMap";
    private static final String CURRENT_LOCATION = "ny_ic_customer_current_location";

    // CallBacks
    protected static String storeLocateOnMapCallBack = null;

    public MobilityCommonBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
    }

    @Override
    public void reset() {

    }

    // LOCATION UTILS
    @JavascriptInterface
    public void currentPosition(String str) {
        Log.i(LOCATION, "Fetching Current");
        showLocationOnMap();
    }

    @JavascriptInterface
    public boolean isLocationPermissionEnabled() {
        return !(ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED);
    }

    private void showLocationOnMap() {
        ExecutorManager.runOnMainThread(new Runnable() {
            @Override
            public void run() {
                if (!isLocationPermissionEnabled()) return;
                updateLastKnownLocation(null, true);
            }
        });
    }


    @SuppressLint("MissingPermission")
    private void updateLastKnownLocation(String callback, boolean animate) {
        if (!isLocationPermissionEnabled()) return;

        client.getCurrentLocation(Priority.PRIORITY_HIGH_ACCURACY, cancellationTokenSource.getToken())
                .addOnSuccessListener(bridgeComponents.getActivity(), new OnSuccessListener<Location>() {
                    @Override
                    public void onSuccess(Location location) {
                        if (location != null) {
                            Double lat = location.getLatitude();
                            Double lng = location.getLongitude();
                            lastLatitudeValue = lat;
                            lastLongitudeValue = lng;
                            setKeysInSharedPrefs("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                            setKeysInSharedPrefs("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                            if (callback != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                                        callback, String.valueOf(lat), String.valueOf(lng));
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                            if (animate && googleMap != null && lat != null && lng != null) {
                                LatLng latLng = new LatLng(lat, lng);
                                if (userPositionMarker == null) {
                                    upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f); //TODO this function will be removed
                                } else {
                                    if (storeLocateOnMapCallBack == null)
                                        userPositionMarker.setVisible(true);
                                    userPositionMarker.setPosition(latLng);
                                }
                                googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                            }
                        } else getLastKnownLocationFromClientFallback(callback, animate);
                    }
                })
                .addOnFailureListener(bridgeComponents.getActivity(), new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                        Log.e(LOCATION, "Current position not known");
                        getLastKnownLocationFromClientFallback(callback, animate);
                    }
                });

    }

    @SuppressLint("MissingPermission")
    private void getLastKnownLocationFromClientFallback(String callback, boolean animate) {
        if (!isLocationPermissionEnabled()) return;
        client.getLastLocation()
                .addOnSuccessListener(bridgeComponents.getActivity(), new OnSuccessListener<Location>() {
                    @Override
                    public void onSuccess(Location location) {
                        if (location != null) {
                            Double lat = location.getLatitude();
                            Double lng = location.getLongitude();
                            lastLatitudeValue = lat;
                            lastLongitudeValue = lng;
                            setKeysInSharedPrefs("LAST_KNOWN_LAT", String.valueOf(lastLatitudeValue));
                            setKeysInSharedPrefs("LAST_KNOWN_LON", String.valueOf(lastLongitudeValue));
                            if (callback != null) {
                                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                                        callback, lat, lng);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                            if (animate && googleMap != null && lat != null && lng != null) {
                                LatLng latLng = new LatLng(lat, lng);
                                if (userPositionMarker == null) {
                                    upsertMarker(CURRENT_LOCATION, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f); //TODO this function will be removed
                                } else {
                                    if (storeLocateOnMapCallBack == null)
                                        userPositionMarker.setVisible(true);
                                    userPositionMarker.setPosition(latLng);
                                }
                                googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                            }
                        }
                    }
                })
                .addOnFailureListener(bridgeComponents.getActivity(), new OnFailureListener() {
                    @Override
                    public void onFailure(@NonNull Exception e) {
                        Log.e(LOCATION, "Last and current position not known");
                    }
                });
    }

    // MAP FUNCTIONS
    @JavascriptInterface
    public void removeMarker(final String title) {
        ExecutorManager.runOnMainThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if (markers.has(title)) {
                        Marker m = (Marker) markers.get(title);
                        m.setVisible(false);
                        Log.i(MAPS, "Removed marker " + title);
                    }
                } catch (Exception e) {
                    Log.e(MAPS, "Remove Marker error " + title, e);
                }
            }
        });
    }

    @JavascriptInterface
    public void animateCamera(final double lat, final double lng, final float zoom) {
        ExecutorManager.runOnMainThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if (googleMap != null) {
                        LatLng latLngObj = new LatLng(lat, lng);
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngZoom(latLngObj, zoom));
                        Log.i(MAPS, "Animated Camera");
                    }
                } catch (Exception e) {
                    Log.e(MAPS, "Error while animating camera");
                }
            }
        });
    }

    @JavascriptInterface
    public void upsertMarker(final String title, final String lat, final String lng, final int markerSize, final float anchorV, final float anchorV1) {
        ExecutorManager.runOnMainThread(new Runnable() {
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
                            Log.i(MAPS, "Marker position updated for " + title);
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
                                Log.i(MAPS, "New marker created and updated for " + title);
                            }
                        }
                    }
                } catch (Exception e) {
                    Log.i(MAPS, "Marker creation error for " + title, e);
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
            Log.e(MAPS, "MARKER obj creation", e);
            return null;
        }
    }

    private Bitmap constructBitmap(final int markerSize, final String title){
        int imageID = bridgeComponents.getContext().getResources().getIdentifier(title, "drawable", bridgeComponents.getActivity().getPackageName());
        BitmapDrawable bitmapdraw = (BitmapDrawable) bridgeComponents.getContext().getResources().getDrawable(imageID);
        Bitmap b = bitmapdraw.getBitmap();
        float maximum = Math.max(b.getWidth(), b.getHeight());
        float minimum = Math.min(b.getWidth(), b.getHeight());
        float multiplier = markerSize/maximum;
        int markerWidth = Math.round(b.getWidth() * multiplier);
        int markerHeight = Math.round(b.getHeight() * multiplier);
        Log.i("real width and height of "+ title, String.valueOf(b.getWidth()) + " , " + String.valueOf(b.getHeight()));
        Log.i("after width and height of "+ title, String.valueOf(markerWidth) + " , " + String.valueOf(markerHeight));
        return Bitmap.createScaledBitmap(b, markerWidth, markerHeight, false);
    }

    // SHARED PREFERENCES UTILS
    @JavascriptInterface
    public void setKeysInSharedPrefs(String key, String value) {
        KeyValueStore.write(bridgeComponents.getContext(), bridgeComponents.getSdkName(), key, value, false);
    }
}
