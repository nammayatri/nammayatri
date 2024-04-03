package in.juspay.mobility.common.map;


import android.util.Log;

import androidx.annotation.Nullable;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.mobility.common.MapRemoteConfig;
import in.juspay.mobility.common.utils.SharedPref;

// @TODO: move google map related functions here (ShowMap, PrepareMapAsync...)
public class Map {
    @Nullable private MapRemoteConfig mapRemoteConfig;
    private final SharedPref sharedPref;

    private final String LOG_TAG = "MAP_CLASS";

    public Map(BridgeComponents bridgeComponents){
        this.sharedPref = SharedPref.getSharedPref(bridgeComponents);
    }


    public MapRemoteConfig getMapRemoteConfig() {
        try {
            if(this.mapRemoteConfig == null) {
                mapRemoteConfig = new MapRemoteConfig();
                String mapConfig = sharedPref.getKeyInNativeSharedPrefKeys("MAP_REMOTE_CONFIG");
                mapRemoteConfig.fromJson(mapConfig);
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in getMapRemoteConfig", e);
        }
        return mapRemoteConfig;
    }
}
