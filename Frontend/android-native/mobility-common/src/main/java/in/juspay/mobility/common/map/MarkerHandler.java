package in.juspay.mobility.common.map;

import android.annotation.SuppressLint;

import androidx.annotation.Nullable;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.LatLng;

import org.json.JSONObject;

import java.util.HashMap;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;

public class MarkerHandler {
    private final HashMap<String, Marker> markers;
    @Nullable private final GoogleMap googleMap;
    private final BridgeComponents bridgeComponents;

    public MarkerHandler(@Nullable GoogleMap googleMap, BridgeComponents bridgeComponents){
        markers = new HashMap<>();
        this.googleMap = googleMap;
        this.bridgeComponents = bridgeComponents;
    }

    /*
    * @Desc: returns Marker obj related to id. if no marker found, it create new one.
    * */
    private Marker getOrDefaultMarkerObj(String id){
        if(! markers.containsKey(id)){
            markers.put(id, new Marker(id, bridgeComponents));
        }
        return markers.get(id);
    }

    /*
    * @desc: create and insert the marker at particular lat lon on google map
    * */
    public String addMarker(JSONObject config){
        try {
            String id = config.getString("id");
            Marker marker = getOrDefaultMarkerObj(id);
            marker.addMarker(googleMap, config);

            return id;
        }catch (Exception e){
            e.printStackTrace();
            return "";
        }
    }

    /*
    * @Desc: Move the particular marker to new Position
    * */
    public void moveMarker(String id, LatLng newPos, boolean movingAnimation){
        Marker marker = markers.get(id);
        if(marker != null)
            marker.moveMarker(newPos, movingAnimation);
    }

    /*
    * @Desc: Get the current position of the marker
    * */
    @Nullable public LatLng getCurrentPosition(String id){
        Marker marker = markers.get(id);
        return marker== null ? null : marker.getCurrentPosition();
    }

    /*
     * @Desc: remove marker related to the id
     * */
    public void removeMarker(String id){
        Marker marker = getOrDefaultMarkerObj(id);
        marker.removeMarker();
        markers.remove(id);
    }

    /*
     * @Desc: remove all markers related to particular google map instance
     * */
    public void removeAllMarkers(){
        for(HashMap.Entry<String, Marker> i : markers.entrySet()){
            i.getValue().removeMarker();
        }
        markers.clear();
    }

    /*
    * @Desc: Update the existing marker
    * */
    public void updateMarker(JSONObject config){
        try {
            String id = config.getString("id");
            Marker marker = markers.get(id);
            if(marker != null) {
                marker.removeMarker();
                marker.addMarker(googleMap, config);
            }
        }catch (Exception e){
            e.printStackTrace();
        }
    }

    /*
    * Helper Functions
    * */
    @SuppressLint("PotentialBehaviorOverride")
    public void setListeners(){
        if (googleMap != null) {
            googleMap.setOnMarkerClickListener(clickedMarkerObj -> {
                if (clickedMarkerObj.getTag() != null) {
                    Marker mapMarker = getOrDefaultMarkerObj((String) clickedMarkerObj.getTag());
                    String onClickCallback = mapMarker.getOnClickCallback();

                    if(!onClickCallback.equals("")) {
                        String js = String.format(Locale.ENGLISH, "window.callUICallback('%s');", clickedMarkerObj.getTag());
                        bridgeComponents.getJsCallback().addJsToWebView(js);
                    }
                }
                return true;
            });
        }
    }

}
