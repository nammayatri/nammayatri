package in.juspay.mobility.common.map;

import android.util.Log;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.MobilityCommonBridge;

public class Camera {

    public static int animationDuration = 400;
    private static final String CAMERA = "CAMERA";

    public static void animateCamera(GoogleMap googleMap, final double lat, final double lng, final float zoom, final String zoomType) {
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
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    /**
     * @Desc: camera focus the source and destination
     * */
    public static void moveCamera(GoogleMap googleMap, final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude, final ArrayList<LatLng> coordinates) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                double source_lat, source_lng, destination_lat, destination_lng;

                Log.i(CAMERA, "json_coordinates" + coordinates);
                ArrayList<Double> all_latitudes = new ArrayList<>();
                ArrayList<Double> all_longitudes = new ArrayList<>();
                for (int i = 0; i < coordinates.size(); i++) {
                    double lon = coordinates.get(i).longitude;
                    double lat = coordinates.get(i).latitude;
                    all_latitudes.add(lat);
                    all_longitudes.add(lon);
                }
                Log.i(CAMERA, "all_latitudes" + (all_latitudes));
                Log.i(CAMERA, "all_longitudes" + (all_longitudes));
                double minimum_latitude = Collections.min(all_latitudes);
                double maximum_latitude = Collections.max(all_latitudes);
                double minimum_longitude = Collections.min(all_longitudes);
                double maximum_longitude = Collections.max(all_longitudes);
                Log.i(CAMERA, String.valueOf(minimum_latitude));
                Log.i(CAMERA, String.valueOf(maximum_latitude));

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
                Log.i(CAMERA, "Coordinates Points" + coordinates);
                if (googleMap != null) {
                    try {
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                        if (coordinates.size() < 5) {
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400), animationDuration, null);
                        } else {
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150), animationDuration, null);
                        }
                    } catch (IllegalArgumentException e) {
                        Log.i(CAMERA, "Exception in Move camera" + e);
                        LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                        LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                        LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                        if (coordinates.size() < 5) {
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400), animationDuration, null);
                        } else {
                            googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150), animationDuration, null);
                        }
                    } catch (Exception e) {
                        Log.i(CAMERA, "Exception in Move camera" + e);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    public static class ZoomType {
        public static final String NO_ZOOM = "NO_ZOOM";
        public static final String ZOOM = "ZOOM";
    }
}
