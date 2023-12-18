/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.common;

import android.content.Context;
import android.location.Address;
import android.location.Geocoder;
import android.util.Log;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.Locale;

public class GeoCoderHelper {
    Context context;
    Geocoder geocoder;

    String LOG_TAG = "GeoCoderHelper";

    public GeoCoderHelper(Context context){
        this.context = context;
        geocoder = new Geocoder(context, Locale.getDefault());
    }

    public class GeoCoordinate {

        private double latitude;
        private double longitude;

        public GeoCoordinate(double latitude, double longitude) {
            this.latitude = latitude;
            this.longitude = longitude;
        }

        public double getLatitude() {
            return latitude;
        }

        public double getLongitude() {
            return longitude;
        }
    }

    public String getLocName(double latitude, double longitude) {
        try {
            List<Address> addresses = geocoder.getFromLocation(latitude, longitude, 1);
            if ( geocoder.isPresent() && addresses != null && addresses.size() > 0) {
                Address address = addresses.get(0);
                return address.getAddressLine(0);
            } else {
                return "NO_LOCATION_FOUND";
            }
        } catch (Exception e) {
            e.printStackTrace();
            return "NO_LOCATION_FOUND";
        }
    }

    public GeoCoordinate getGeoCoordinateFromAddress(String address) {
        Geocoder geocoder = new Geocoder(context, Locale.getDefault());
        try {
            List<Address> addresses = geocoder.getFromLocationName(address, 1);
            if (addresses != null && addresses.size() > 0) {
                Log.d(LOG_TAG, "FromLocationName: " + addresses + " \n" + "GivenAddress: " + address);
                double latitude = addresses.get(0).getLatitude();
                double longitude = addresses.get(0).getLongitude();
                int decimalPlaces = 7;
                BigDecimal roundedValueLat = new BigDecimal(latitude).setScale(decimalPlaces, BigDecimal.ROUND_HALF_UP);
                BigDecimal roundedValueLon = new BigDecimal(longitude).setScale(decimalPlaces, BigDecimal.ROUND_HALF_UP);
                return new GeoCoordinate(roundedValueLat.doubleValue(), roundedValueLon.doubleValue());
            }
        } catch (IOException e) {
            e.printStackTrace();
            return new GeoCoordinate(0.0, 0.0);
        }
        return new GeoCoordinate(0.0, 0.0);
    }

}


