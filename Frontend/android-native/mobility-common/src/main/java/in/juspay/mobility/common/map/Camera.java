package in.juspay.mobility.common.map;

import android.util.Log;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.maps.model.LatLng;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.MobilityCommonBridge;

public class Camera {

    public void animateCamera(GoogleMap googleMap, final double lat, final double lng, final float zoom, final String zoomType) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (googleMap != null) {
                    LatLng latLngObj = new LatLng(lat, lng);
                    if (zoomType.equals(MobilityCommonBridge.ZoomType.NO_ZOOM)) {
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
}
