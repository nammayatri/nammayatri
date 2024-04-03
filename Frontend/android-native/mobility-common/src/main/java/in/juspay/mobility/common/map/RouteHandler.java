package in.juspay.mobility.common.map;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.LatLng;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;

public class RouteHandler {
    private final HashMap<String, Route> routes;
    private final GoogleMap googleMap;
    private final BridgeComponents bridgeComponents;

    public RouteHandler(GoogleMap googleMap, BridgeComponents bridgeComponents){
        this.googleMap = googleMap;
        this.routes = new HashMap<>();
        this.bridgeComponents = bridgeComponents;
    }

    private Route getOrDefaultRoute(String id){
        if(!routes.containsKey(id)){
            routes.put(id, new Route(bridgeComponents));
        }
        return routes.get(id);
    }

    /**
     * @Desc: Get the route based on ID and Calls "drawRoute"
     * */
    public String drawRoute(MarkerHandler markerHandler, JSONObject config){
        try {
            String id = config.getString("id");
            Route route = getOrDefaultRoute(id);
            route.drawRoute(googleMap, markerHandler, config);
            focusConnectedRoutes(route, true);
            return id;
        }catch (Exception e){
            e.printStackTrace();
            return "";
        }
    }

    /**
     * @Desc: Get the route based on ID and Calls "updateRote"
     * */
    public String updateRote(MarkerHandler markerHandler, JSONObject config){
        try {
            String id = config.getString("id");
            Route route = getOrDefaultRoute(id);
            route.updateRoute(googleMap, markerHandler, config);
            focusConnectedRoutes(route, false);
            return id;
        }catch (Exception e){
            e.printStackTrace();
            return "";
        }

    }

    /**
     * @Desc: Remove all routes
     * */
    public void removeAllRoutes(MarkerHandler markerHandler){
        for(HashMap.Entry<String, Route> i : routes.entrySet()){
            i.getValue().removeRoute(markerHandler);
        }
        routes.clear();
    }

    /*
    * @TODO: IMPROVE THE FOCUS LOGIC FOR MULTI ROUTES
    * */
    private void focusConnectedRoutes(Route route, boolean forcedFocus){
        ExecutorManager.runOnMainThread(() -> {
            double startLat = 0.0, startLon = 0.0, endLat = 0.0, endLon = 0.0;

            ArrayList<LatLng> allPoints = new ArrayList<>();
            int minCurrentDebounceCtr = Integer.MAX_VALUE;

            for (Route i = route; true; i = getOrDefaultRoute(i.nextRouteID)) {
                startLat = i.startLat;
                startLon = i.startLon;
                allPoints.addAll(i.getPoints());
                i.currentDebounceCounter--;
                minCurrentDebounceCtr = Math.min(i.currentDebounceCounter, minCurrentDebounceCtr);
                if (i.currentDebounceCounter <= 0) {
                    i.currentDebounceCounter = i.maximumDebounceCounter;
                }
                if (route.nextRouteID.equals(""))
                    break;
            }

            for (Route i = route; true; i = getOrDefaultRoute(i.prevRouteID)) {
                endLat = i.endLat;
                endLon = i.endLon;
                if (route.prevRouteID.equals(""))
                    break;
            }

            if (minCurrentDebounceCtr <= 0 || forcedFocus) {
                Camera.moveCamera(googleMap, startLat, startLon, endLat, endLon, allPoints);
            }
        });
    }

}
