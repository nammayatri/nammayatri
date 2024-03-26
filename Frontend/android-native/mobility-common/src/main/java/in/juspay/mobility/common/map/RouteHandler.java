package in.juspay.mobility.common.map;

import com.google.android.gms.maps.GoogleMap;

import org.json.JSONObject;

import java.util.HashMap;

public class RouteHandler {
    private final HashMap<String, Route> routes;
//    private GoogleMap googleMap;
    private final GoogleMap googleMap;

    RouteHandler(GoogleMap googleMap){
        this.googleMap = googleMap;
        this.routes = new HashMap<>();
    }

    private Route getRoute(String id){
        return routes.containsKey(id) ? routes.get(id) : routes.put(id, new Route(googleMap));
    }


    public void drawRoute(String routeId, String config){
        Route route = getRoute(routeId);
        route.drawRoute(googleMap, config);
    }

    public void updateRote(String routeId, String config){
        Route route = getRoute(routeId);
        route.updateRoute(googleMap, config);
    }
}
