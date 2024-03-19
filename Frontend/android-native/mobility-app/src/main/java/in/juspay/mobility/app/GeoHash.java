package in.juspay.mobility.app;

import com.google.android.gms.maps.model.LatLng;

import java.util.ArrayList;
import java.util.List;

public class GeoHash {
    private static final String BASE32 = "0123456789bcdefghjkmnpqrstuvwxyz";
    private static final int[] BITS = {16, 8, 4, 2, 1};

    public String encode(double latitude, double longitude, int precision) {
        double[] latRange = {-90.0, 90.0};
        double[] lonRange = {-180.0, 180.0};
        StringBuilder geohash = new StringBuilder();
        boolean isEven = true;
        int bit = 0;
        int ch = 0;

        while (geohash.length() < precision) {
            double mid;
            if (isEven) {
                mid = (lonRange[0] + lonRange[1]) / 2;
                if (longitude > mid) {
                    ch |= BITS[bit];
                    lonRange[0] = mid;
                } else
                    lonRange[1] = mid;
            } else {
                mid = (latRange[0] + latRange[1]) / 2;
                if (latitude > mid) {
                    ch |= BITS[bit];
                    latRange[0] = mid;
                } else
                    latRange[1] = mid;
            }

            isEven = !isEven;
            if (bit < 4)
                bit++;
            else {
                geohash.append(BASE32.charAt(ch));
                bit = 0;
                ch = 0;
            }
        }

        return geohash.toString();
    }

    public ArrayList<String> nearbyGeohashes(String geoHash, double radius) {
        LatLng currentPosition = decodeGeohash(geoHash);
        int precisionLevel = geoHash.length();
        ArrayList<String> geoHashes = new ArrayList<>();
        for (int bearing = 0; bearing < 360; bearing += 45) {
            LatLng position = calculateDestination(currentPosition.latitude, currentPosition.longitude,150.0, bearing);
            geoHashes.add(encode(position.latitude, position.longitude, precisionLevel));
        }
        return geoHashes;
    }

    private static LatLng calculateDestination(double currentLat, double currentLng, double distance, double bearing) {
        double angularDistance = distance / 6371000;
        double bearingRad = Math.toRadians(bearing);

        double lat1 = Math.toRadians(currentLat);
        double lon1 = Math.toRadians(currentLng);

        double lat2 = Math.asin(Math.sin(lat1) * Math.cos(angularDistance) +
                Math.cos(lat1) * Math.sin(angularDistance) * Math.cos(bearingRad));
        double lon2 = lon1 + Math.atan2(Math.sin(bearingRad) * Math.sin(angularDistance) * Math.cos(lat1),
                Math.cos(angularDistance) - Math.sin(lat1) * Math.sin(lat2));

        lon2 = (lon2 + 3 * Math.PI) % (2 * Math.PI) - Math.PI;

        return new LatLng(Math.toDegrees(lat2), Math.toDegrees(lon2));
    }

    public LatLng decodeGeohash(String geohash) {
        StringBuilder bits = new StringBuilder();
        for (char c : geohash.toCharArray()) {
            int i = BASE32.indexOf(c);
            bits.append(String.format("%5s", Integer.toBinaryString(i)).replace(' ', '0'));
        }

        boolean evenBit = true;
        double latMin = -90.0, latMax = 90.0;
        double lonMin = -180.0, lonMax = 180.0;

        for (int i = 0; i < bits.length(); i++) {
            char bit = bits.charAt(i);
            if (evenBit) {
                double lonMid = (lonMin + lonMax) / 2;
                if (bit == '1')
                    lonMin = lonMid;
                else
                    lonMax = lonMid;
            } else {
                double latMid = (latMin + latMax) / 2;
                if (bit == '1')
                    latMin = latMid;
                else
                    latMax = latMid;
            }
            evenBit = !evenBit;
        }

        return new LatLng((latMin + latMax) / 2, (lonMin + lonMax) / 2);
    }


}
