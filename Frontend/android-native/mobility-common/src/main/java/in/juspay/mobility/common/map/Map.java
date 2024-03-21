package in.juspay.mobility.common.map;

import android.annotation.SuppressLint;
import android.util.Log;

import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.LatLng;

import org.json.JSONObject;

import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.MapRemoteConfig;
import in.juspay.mobility.common.MapUpdate;
import in.juspay.mobility.common.MobilityCommonBridge;



public class Map {

    public int animationDuration ;

    public GoogleMap googleMap;
    private BridgeComponents bridgeComponents;

    protected MapUpdate mapUpdate;
    private MapRemoteConfig mapRemoteConfig;

    private final String LOG_TAG = "MAP_CLASS";

    Map(GoogleMap googleMap, BridgeComponents bridgeComponents){
        this.googleMap = googleMap;
        this.bridgeComponents = bridgeComponents;
        this.mapUpdate = new MapUpdate();
    }

    public void showMap(final String pureScriptId, boolean isEnableCurrentLocation, final String mapType, final float zoom, final String callback, final String mapConfig) {
        try {
            ExecutorManager.runOnMainThread(() -> {
                if (bridgeComponents.getActivity() != null) {
                    try {
                        JSONObject googleMapConfig = new JSONObject(mapConfig);
                        animationDuration = googleMapConfig.optInt("animationDuration", 400);
                        locateOnMapConfig = googleMapConfig.optJSONObject("locateOnMapConfig");
                        labelTextSize = googleMapConfig.optInt("labelTextSize", 30);
                        SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                        FragmentManager supportFragmentManager = ((FragmentActivity) bridgeComponents.getActivity()).getSupportFragmentManager();
                        FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                        fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                        fragmentTransaction.commitAllowingStateLoss();
                        getMapAsync(mapFragment, isEnableCurrentLocation, mapType, callback, pureScriptId, zoom);
                    } catch (Exception e) {
                        Log.e(MAPS, "Error in showMap " + e);
                        e.printStackTrace();
                    }
                }
            });
        } catch (Exception e) {
            Log.e("ADD_MARKER", e.toString());
        }
    }

    @SuppressLint({"MissingPermission", "PotentialBehaviorOverride"})
    private void getMapAsync(SupportMapFragment mapFragment, boolean isEnableCurrentLocation, final String mapType, final String callback, final String pureScriptId, final float zoom) {
        if (bridgeComponents.getActivity() != null) {
            mapFragment.getMapAsync(googleMap -> {
                this.googleMap = googleMap;
                googleMap.setMinZoomPreference(7.0f);
                googleMap.setMaxZoomPreference(googleMap.getMaxZoomLevel());
                googleMap.getUiSettings().setRotateGesturesEnabled(false);
                googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                if (isLocationPermissionEnabled()) {
                    googleMap.setMyLocationEnabled(isEnableCurrentLocation);
                }
                markers = new JSONObject();
                markersElement.put(pureScriptId, markers);
                googleMap.setOnMarkerClickListener(marker -> {
                    marker.hideInfoWindow();
                    return true;
                });

                try {
                    if (mapType.equals(LOCATE_ON_MAP)) {
                        upsertMarker(LOCATE_ON_MAP, String.valueOf(lastLatitudeValue), String.valueOf(lastLongitudeValue), 160, 0.5f, 0.9f);
                        this.googleMap.setOnCameraMoveListener(() -> {
                            try {
                                double lat = (googleMap.getCameraPosition().target.latitude);
                                double lng = (googleMap.getCameraPosition().target.longitude);
                                upsertMarker(LOCATE_ON_MAP, String.valueOf(lat), String.valueOf(lng), 160, 0.5f, 0.9f);
                            } catch (Exception e) {
                                Log.i(MAPS, "Marker creation error for ", e);
                            }
                        });
                        this.googleMap.setOnCameraIdleListener(() -> {
                            if (callback != null) {
                                double lat = (googleMap.getCameraPosition().target.latitude);
                                double lng = (googleMap.getCameraPosition().target.longitude);
                                String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "LatLon", lat, lng);
                                Log.e(MAPS, javascript);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                        });
                    }
                    setMapCustomTheme();
                    if (lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) {
                        LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        this.googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, zoom));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                if (callback != null) {
                    String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", callback, "MAP", "READY", "LOADED");
                    Log.e(MAPS, javascript);
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
            });
        }
    }

    public void onMapUpdate(String isIdleCallback, String isMovedCallback) {
        try {
            MapRemoteConfig mapRemoteConfig = getMapRemoteConfig();
            if(mapRemoteConfig.enableMapRecenter) {
                if (app == MobilityCommonBridge.AppType.CONSUMER) {
                    ExecutorManager.runOnMainThread(() -> {
                        try {
                            if (!mapUpdate.isIdleListenerActive) {
                                googleMap.setOnCameraIdleListener(() -> {
                                    if (isIdleCallback != null) {
                                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s';", isIdleCallback);
                                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                    }
                                    mapUpdate.mapRecenterHandler.removeCallbacksAndMessages(null);
                                    mapUpdate.mapRecenterHandler.postDelayed(() -> {
                                        if(mapUpdate.isGestureMovement) mapUpdate.isMapMoved = true;
                                        mapUpdate.isMapIdle = true;
                                    }, mapRemoteConfig.recenterDelay);
                                });
                                mapUpdate.isIdleListenerActive = true;
                            }
                            if (!mapUpdate.isMoveListenerActive) {
                                googleMap.setOnCameraMoveStartedListener(reason -> {
                                    if(isMovedCallback != null){
                                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%d';", isIdleCallback, reason);
                                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                    }
                                    if (reason == GoogleMap.OnCameraMoveStartedListener.REASON_GESTURE) {
                                        mapUpdate.isGestureMovement = true;
                                        mapUpdate.isMapIdle = false;
                                    } else {
                                        mapUpdate.isGestureMovement = false;
                                    }
                                    mapUpdate.mapRecenterHandler.removeCallbacksAndMessages(null);
                                });
                                mapUpdate.isMoveListenerActive = true;
                            }
                        } catch (Exception e) {
                            Log.e(LOG_TAG, "Error in onMapUpdate Executor " + e);
                        }
                    });
                }
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in onMapUpdate ", e);
        }
    }

    public MapRemoteConfig getMapRemoteConfig() {
        try {
            if(this.mapRemoteConfig == null) {
                mapRemoteConfig = new MapRemoteConfig();
                String mapConfig = getKeyInNativeSharedPrefKeys("MAP_REMOTE_CONFIG");
                mapRemoteConfig.fromJson(mapConfig);
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in getMapRemoteConfig", e);
        }
        return mapRemoteConfig;
    }
}
