package in.juspay.mobility.common;

import android.util.Log;
import org.json.JSONObject;

public class MapRemoteConfig {
    public long recenterDelay = 4000;
    public boolean enableMapRecenter = true;
    public int debounceAnimateCameraCounter = 5;
    public double zoomLevel = 20.0;

    public void fromJson(String json) {
        try {
            JSONObject config = new JSONObject(json);
            this.recenterDelay = config.getLong("recenter_delay");
            this.enableMapRecenter = config.getBoolean("enable_map_recenter");
            this.debounceAnimateCameraCounter = config.getInt("debounce_animate_camera_counter");
            this.zoomLevel = config.getDouble("zoom_level");
        } catch (Exception e) {
            Log.e("MapRemoteConfig","Error in fromJson ", e);
        }
    }
}
