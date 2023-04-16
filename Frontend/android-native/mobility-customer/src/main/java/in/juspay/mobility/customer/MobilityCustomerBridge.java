package in.juspay.mobility.customer;

import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.PendingIntent;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.PorterDuff;
import android.graphics.drawable.Drawable;
import android.graphics.pdf.PdfDocument;
import android.net.Uri;
import android.os.Environment;
import android.provider.ContactsContract;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.animation.LinearInterpolator;
import android.webkit.JavascriptInterface;
import android.widget.ImageView;
import android.widget.TextView;
import android.Manifest;

import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.core.content.res.ResourcesCompat;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.ButtCap;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.PatternItem;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.common.MobilityCommonBridge;

public class MobilityCustomerBridge extends MobilityCommonBridge {

    public static BridgeComponents bridgeComponents;
    public String invoice;
    public static final int REQUEST_CONTACTS = 7;
    public static int debounceAnimateCameraCounter = 0;

    // CallBacks Strings
    private static String storeContactsCallBack = null;
    private static String storeOnResumeCallback = null;
    private static String storeCustomerCallBack = null;


    public MobilityCustomerBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        MobilityCustomerBridge.bridgeComponents = bridgeComponents;
    }

    // STORE_CALLBACKS AND CALL_CALLBACKS
    @JavascriptInterface
    public void contactPermission() {
        try {
            if (ContextCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.READ_CONTACTS) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{Manifest.permission.READ_CONTACTS}, REQUEST_CONTACTS);
            } else {
                    contactsStoreCall(getPhoneContacts());
            }
        } catch (Exception e) {
            Log.e(UTILS, "Exception in Contact Permission" + e);
        }
    }

    public static void contactsStoreCall(String contacts){
        if (storeContactsCallBack != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeContactsCallBack,contacts);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void storeCallBackContacts(String callback) {
        storeContactsCallBack = callback;
    }

    @JavascriptInterface
    public void storeOnResumeCallback(String callback) {
        storeOnResumeCallback = callback;
    }

    public static void callOnResumeUpdateCallback() {
        if (storeOnResumeCallback != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s');", storeOnResumeCallback);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void storeCallBackCustomer(String callback) {
        storeCustomerCallBack = callback;
    }

    public static void callingStoreCallCustomer(String notificationType){
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeCustomerCallBack,notificationType);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }

    @JavascriptInterface
    public void storeCallBackLocateOnMap(String callback) {
        storeLocateOnMapCallBack = callback;
    }

    //MAP FUNCTIONS
    @JavascriptInterface
    public String isCoordOnPath(String json , double currLat, double currLng, int speed) throws JSONException {
        LatLng currPoint = new LatLng(currLat,currLng);
        ArrayList<LatLng> path = new ArrayList<>();
        JSONObject jsonObject = new JSONObject(json);
        int distanceRemaining;
        JSONArray coordinates = jsonObject.getJSONArray("points");
        int eta = 0;
        int resultIndex;
        JSONObject result = new JSONObject();
        for (int i = coordinates.length() -1 ; i >= 0   ; i--) {
            JSONObject coordinate = (JSONObject) coordinates.get(i);
            double lng = coordinate.getDouble("lng");
            double lat = coordinate.getDouble("lat");
            LatLng tempPoints = new LatLng(lat,lng);
            path.add(tempPoints);
        }
        if (path.size() == 0)
        {
            result.put("points",new JSONArray());
            result.put("eta",0);
            result.put("distance",0);
            result.put("isInPath",false);
            return result.toString();
        }
        resultIndex = PolyUtil.locationIndexOnEdgeOrPath(currPoint, path, PolyUtil.isClosedPolygon(path), true, 20.0);
        if (resultIndex == -1) {
            result.put("points",coordinates);
            result.put("eta",0);
            result.put("distance",0);
            result.put("isInPath",false);
        } else if (resultIndex == 0) {
            path.clear();
            result.put("points",new JSONArray());
            result.put("eta",0);
            result.put("distance",0);
            result.put("isInPath",true);
        } else if (resultIndex == (path.size()-2) || resultIndex == (path.size()-1)) {
            distanceRemaining = (int) SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            result.put("points",coordinates);
            result.put("eta",eta);
            result.put("distance",distanceRemaining);
            result.put("isInPath",true);
        } else {
            path.subList(resultIndex+2,path.size()).clear();
            distanceRemaining = (int)SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            JSONArray remainingPoints = new JSONArray();
            for (int i = path.size() - 1 ; i >= 0   ; i--) {
                LatLng point = path.get(i);
                JSONObject tempPoints = new JSONObject();
                tempPoints.put("lat",point.latitude);
                tempPoints.put("lng",point.longitude);
                remainingPoints.put(tempPoints);
            }
            result.put("points",remainingPoints);
            result.put("eta",eta);
            result.put("distance",distanceRemaining);
            result.put("isInPath",true);
        }
        return result.toString();
    }

    @JavascriptInterface
    public void exitLocateOnMap (String str){
        try {
            this.storeLocateOnMapCallBack = null;
            ExecutorManager.runOnMainThread(new Runnable() {
                @Override
                public void run() {
                    if (googleMap != null) {
                        googleMap.setOnCameraMoveListener(null);
                        googleMap.setOnCameraIdleListener(null);
                    }
                }
            });
        } catch (Exception e) {
            Log.i(MAPS, "LocateOnMap Exit Error for ", e);
        }
    }

    @JavascriptInterface
    public void locateOnMap (boolean goToCurrentLocation, final String lat, final String lon){
        try {
            ExecutorManager.runOnMainThread(new Runnable() {
                @Override
                public void run() {
                    removeMarker("ny_ic_customer_current_location");
                    if(goToCurrentLocation){
                        LatLng latLng = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                    }else{
                        LatLng latLng = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                        googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                    }
                    googleMap.setOnCameraIdleListener(new GoogleMap.OnCameraIdleListener() {
                        @Override
                        public void onCameraIdle() {
                            double lat = (googleMap.getCameraPosition().target.latitude);
                            double lng =  (googleMap.getCameraPosition().target.longitude);
                            if (storeLocateOnMapCallBack != null){
                                String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeLocateOnMapCallBack, "LatLon", lat, lng);
                                bridgeComponents.getJsCallback().addJsToWebView(javascript);
                            }
                        }
                    });
                    if ((lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) && goToCurrentLocation) {
                        LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                        if (googleMap != null) googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                    }else{
                        LatLng latLngObjMain = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                        if (googleMap != null) {
                            googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                            googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                        }
                    }
                }
            });

        } catch (Exception e) {
            Log.i(MAPS, "LocateOnMap error for ", e);
        }
    }

    @JavascriptInterface
    public void updateRoute (String json, String dest, String eta) {
        ExecutorManager.runOnMainThread(new Runnable() {
            @Override
            public void run() {
                if(googleMap!=null) {
                    try {
                        ArrayList<LatLng> path = new ArrayList<>();
                        JSONObject jsonObject = null;
                        jsonObject = new JSONObject(json);
                        JSONArray coordinates = jsonObject.getJSONArray("points");
                        for (int i = coordinates.length() -1 ; i >= 0   ; i--) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            LatLng tempPoint = new LatLng(lat, lng);
                            path.add(tempPoint);
                        }
                        Marker currMarker = (Marker) markers.get("ny_ic_auto_map");
                        Marker destMarker = (Marker) markers.get(dest);
                        destMarker.setIcon((BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(eta, dest))));
                        if (polyline != null) {
                            polyline.setEndCap(new ButtCap());
                            if (path.size() == 0) {
                                LatLng destination = destMarker.getPosition();
                                animateMarkerNew(destination,currMarker);
                                polyline.remove();
                                polyline = null;
                                currMarker.setAnchor(0.5f,0);
                                animateCamera(destMarker.getPosition().latitude,destMarker.getPosition().longitude,17.0f);
                            }
                            else
                            {
                                double destinationLat = path.get(0).latitude;
                                double destinationLon = path.get(0).longitude;
                                double sourceLat = path.get(path.size()-1).latitude;
                                double sourceLong = path.get(path.size()-1).longitude;
                                LatLng destination = path.get(path.size()-1);
                                animateMarkerNew(destination,currMarker);
                                PatternItem DASH = new Dash(1);
                                List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Arrays.asList(DASH);
                                polyline.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                                polyline.setPoints(path);
                                if(debounceAnimateCameraCounter != 0) {
                                    debounceAnimateCameraCounter--;
                                } else{
                                    moveCamera(sourceLat, sourceLong, destinationLat, destinationLon, coordinates);
                                    debounceAnimateCameraCounter = 10;
                                }
                            }
                        }
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
    }

    private void animateMarkerNew(final LatLng destination, final Marker marker) {
        if (marker != null) {

            LatLng startPosition = marker.getPosition();
            LatLng endPosition = destination;


            ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1);
            valueAnimator.setDuration(2000); // TODO :: get this value from Loacl Storage to maintain sync with PS
            valueAnimator.setInterpolator(new LinearInterpolator());
            valueAnimator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
                @Override
                public void onAnimationUpdate(ValueAnimator animation) {
                    try {
                        float v = animation.getAnimatedFraction();
                        LatLng newPosition = SphericalUtil.interpolate(startPosition, endPosition,v);
                        float rotation = bearingBetweenLocations(startPosition, endPosition);
                        if (rotation > 1.0)
                            marker.setRotation(rotation);
                        marker.setPosition(newPosition);
                        markers.put("ny_ic_auto_map",marker);
                    } catch (Exception ex) {
                    }
                }
            });
            valueAnimator.addListener(new AnimatorListenerAdapter() {
                @Override
                public void onAnimationEnd(Animator animation) {
                    super.onAnimationEnd(animation);
                }
            });
            valueAnimator.start();
        }
    }

    private float bearingBetweenLocations(LatLng latLng1,LatLng latLng2) {
        double PI = 3.14159;
        double lat1 = latLng1.latitude * PI / 180;
        double long1 = latLng1.longitude * PI / 180;
        double lat2 = latLng2.latitude * PI / 180;
        double long2 = latLng2.longitude * PI / 180;
        double dLon = (long2 - long1);
        double y = Math.sin(dLon) * Math.cos(lat2);
        double x = Math.cos(lat1) * Math.sin(lat2) - Math.sin(lat1)
                * Math.cos(lat2) * Math.cos(dLon);
        double brng = Math.atan2(y, x);
        brng = Math.toDegrees(brng);
        brng = (brng + 360) % 360;
        return (float)brng;
    }

    @JavascriptInterface
    public void moveCamera(final double source_latitude, final double source_longitude, final double destination_latitude, final double destination_longitude, final JSONArray json_coordinates) {
        ExecutorManager.runOnMainThread(() -> {
            double source_lat, source_lng, destination_lat, destination_lng;

            Log.i(MAPS, "json_coordinates" + json_coordinates);
            ArrayList<Double> all_latitudes = new ArrayList();
            ArrayList<Double> all_longitudes = new ArrayList();
            for (int i = 0; i < json_coordinates.length(); i++) {
                JSONObject each_json_coordinates = null;
                try {
                    each_json_coordinates = (JSONObject) json_coordinates.get(i);
                    double lon = each_json_coordinates.getDouble("lng");
                    double lat = each_json_coordinates.getDouble("lat");
                    all_latitudes.add(lat);
                    all_longitudes.add(lon);
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
            Log.i(MAPS,"all_latitudes" + (all_latitudes));
            Log.i(MAPS,"all_longitudes" + (all_longitudes));
            double minimum_latitude = Collections.min(all_latitudes);
            double maximum_latitude = Collections.max(all_latitudes);
            double minimum_longitude = Collections.min(all_longitudes);
            double maximum_longitude = Collections.max(all_longitudes);
            Log.i(MAPS, String.valueOf(minimum_latitude));
            Log.i(MAPS, String.valueOf(maximum_latitude));

            if (source_latitude <= destination_latitude) {
                source_lat = minimum_latitude - 1.3*(maximum_latitude - minimum_latitude);
                destination_lat = maximum_latitude + 0.1*(maximum_latitude - minimum_latitude);
            } else {
                source_lat = maximum_latitude + 0.1*(maximum_latitude - minimum_latitude);
                destination_lat = minimum_latitude - 1.3*(maximum_latitude - minimum_latitude);
            }
            if (source_longitude <= destination_longitude) {
                source_lng = minimum_longitude - 0.09*(maximum_longitude - minimum_longitude);
                destination_lng = maximum_longitude + 0.09*(maximum_longitude - minimum_longitude);
            } else {
                source_lng = maximum_longitude + 0.09*(maximum_longitude - minimum_longitude);
                destination_lng = minimum_longitude - 0.09*(maximum_longitude - minimum_longitude);
            }
            Log.i(MAPS, "Coordinates Points" + json_coordinates);
            if(googleMap!=null) {
                try {
                    LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                    LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                    LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                    if(json_coordinates.length() < 5 ){
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400));
                    }else {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150));
                    }
                } catch (IllegalArgumentException e) {
                    Log.i(MAPS, "Exception in Move camera" + e);
                    LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                    LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                    LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                    if(json_coordinates.length() < 5 ){
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 400));
                    }else {
                        googleMap.animateCamera(CameraUpdateFactory.newLatLngBounds(bounds, 150));
                    }
                }
                catch(Exception e){
                    Log.i(MAPS, "Exception in Move camera" + e);
                }
            }
        });
    }


    // OTHERS
    public String getPhoneContacts() throws JSONException {
        ContentResolver contentResolver = bridgeComponents.getContext().getContentResolver();
        Uri uri = ContactsContract.CommonDataKinds.Phone.CONTENT_URI;
        Cursor cursor = contentResolver.query(uri,null,null,null,null);
        JSONArray contacts = new JSONArray();

        if(cursor.getCount()>0){
            while(cursor.moveToNext()){
                String contactNameStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME));
                String contactStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.NUMBER));
                String contactNumber = contactStr.replaceAll("[^0-9]", "");
                String contactName = contactNameStr.replaceAll("'","");
                JSONObject tempPoints = new JSONObject();
                tempPoints.put("name",contactName);
                tempPoints.put("number",contactNumber);
                contacts.put(tempPoints);
            }
        }

        JSONObject flagObject = new JSONObject();
        flagObject.put("name","beckn_contacts_flag");
        flagObject.put("number","true");
        contacts.put(flagObject);
        System.out.print("Contacts " + contacts);
        return contacts.toString();
    }


    @JavascriptInterface
    public void  generatePDF (String str, String format) throws JSONException {
        invoice = str;
        if ((ActivityCompat.checkSelfPermission(bridgeComponents.getActivity(), READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(bridgeComponents.getActivity(), WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED))
        {
            downloadPDF(str,bridgeComponents.getContext());
        }
        else
        {
            ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{WRITE_EXTERNAL_STORAGE}, 67);
        }
    }

    @SuppressLint("MissingPermission")
    public static void downloadPDF(String str, Context context) throws JSONException {
        JSONObject state = new JSONObject(str);
        JSONObject data = state.getJSONObject("data");
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.fileProviderPath), Context.MODE_PRIVATE);
        String userName =  sharedPref.getString("USER_NAME","__failed");
        JSONObject selectedItem = data.getJSONObject("selectedItem");
        JSONObject fares = selectedItem.optJSONObject("fareBreakUpList");
        PdfDocument pdfDocument = new PdfDocument();
        PdfDocument.PageInfo invoicePDF = new PdfDocument.PageInfo.Builder(960, 1338, 1).create();
        PdfDocument.Page page = pdfDocument.startPage(invoicePDF);
        View content = getInvoiceLayout(data,selectedItem,fares,userName,context);
        content.measure(page.getCanvas().getWidth(),page.getCanvas().getHeight());
        content.layout(0,0,page.getCanvas().getWidth(),page.getCanvas().getHeight());
        content.draw(page.getCanvas());
        pdfDocument.finishPage(page);
        String fileNameFormat = "NY_Ride_" + selectedItem.getString("date") + selectedItem.getString("rideStartTime") + ".pdf";
        String fileName = fileNameFormat.replaceAll(":",".");
        File file = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS), fileName);
        try {
            pdfDocument.writeTo(new FileOutputStream(file));
            Uri path = FileProvider.getUriForFile(context.getApplicationContext(), context.getResources().getString(R.string.fileProviderPath), file);
            Intent pdfOpenintent = new Intent(Intent.ACTION_VIEW);
            pdfOpenintent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_ACTIVITY_CLEAR_TOP);
            pdfOpenintent.setDataAndType(path, "application/pdf");
            PendingIntent pendingIntent = PendingIntent.getActivity(context, 234567, pdfOpenintent, PendingIntent.FLAG_IMMUTABLE);
            NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, "General");
            mBuilder.setContentTitle("Invoice Downloaded")
                    .setSmallIcon(R.drawable.ny_ic_launcher)
                    .setContentText("Invoice for your ride is downloaded!!!")
                    .setAutoCancel(true)
                    .setPriority(NotificationCompat.PRIORITY_DEFAULT);
            mBuilder.setContentIntent(pendingIntent);
            NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
            notificationManager.notify(234567, mBuilder.build());
        } catch (IOException e) {
            e.printStackTrace();
        }
        pdfDocument.close();
    }

    private static View getInvoiceLayout(JSONObject data, JSONObject selectedRide, JSONObject fares, String user, Context context) throws JSONException {
        @SuppressLint("InflateParams") View invoiceLayout = LayoutInflater.from(context).inflate(R.layout.invoice_template, null, false);
        TextView textView = invoiceLayout.findViewById(R.id.rideDate);
        textView.setText(selectedRide.getString("date"));
        textView = invoiceLayout.findViewById(R.id.userName);
        textView.setText(user.trim());
        textView = invoiceLayout.findViewById(R.id.paymentDetail);
        textView.setText(selectedRide.getString("totalAmount"));
        textView = invoiceLayout.findViewById(R.id.baseDistance);
        try {
            textView.setText(selectedRide.getString("baseDistance"));
        } catch (JSONException e) {
            textView.setText("");
        }
        textView = invoiceLayout.findViewById(R.id.baseFare);
        textView.setText(fares.getString("baseFare"));
        textView = invoiceLayout.findViewById(R.id.pickUpCharge);
        textView.setText(fares.getString("pickupCharges"));
        textView = invoiceLayout.findViewById(R.id.nominalFare);
        textView.setText(fares.getString("nominalFare"));
        textView = invoiceLayout.findViewById(R.id.waitingCharge);
        textView.setText(fares.getString("waitingCharges"));
        textView = invoiceLayout.findViewById(R.id.finalAmount);
        textView.setText(selectedRide.getString("totalAmount"));
        textView = invoiceLayout.findViewById(R.id.rideId);
        textView.setText(selectedRide.getString("shortRideId"));
        textView = invoiceLayout.findViewById(R.id.driverName);
        textView.setText(selectedRide.getString("driverName"));
        textView = invoiceLayout.findViewById(R.id.lincensePlate);
        textView.setText(selectedRide.getString("vehicleNumber"));
        textView = invoiceLayout.findViewById(R.id.rideStartTime);
        textView.setText(selectedRide.getString("rideStartTime"));
        textView = invoiceLayout.findViewById(R.id.source);
        textView.setText(selectedRide.getString("source"));
        textView = invoiceLayout.findViewById(R.id.rideEndTime);
        textView.setText(selectedRide.getString("rideEndTime"));
        textView = invoiceLayout.findViewById(R.id.destination);
        textView.setText(selectedRide.getString("destination"));
        return invoiceLayout;
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    private Bitmap getMarkerBitmapFromView(String locationName, String imageName) {
        Context context = bridgeComponents.getContext();
        View customMarkerView = ((LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(R.layout.marker_label_layout, null);
        TextView label = customMarkerView.findViewById(R.id.marker_text);
        if(locationName.equals("")){
            label.setVisibility(View.GONE);
        }else{
            if (locationName.length() <= 27) {
                label.setText(locationName);
            } else {
                label.setText(locationName.substring(0, 17)+"...");
            }
        }
        ImageView pointer = customMarkerView.findViewById(R.id.pointer_img);
        try {
            if(imageName.equals("ny_ic_dest_marker") ){
                pointer.setImageDrawable(ResourcesCompat.getDrawable(context.getResources(),R.drawable.ny_ic_dest_marker,context.getTheme()));
            }else{
                pointer.setImageDrawable(ResourcesCompat.getDrawable(context.getResources(),R.drawable.ny_ic_src_marker,context.getTheme()));
            }
        }catch (Exception e){
            Log.e("Exception in rendering Image", e.toString());
        }
        customMarkerView.measure(View.MeasureSpec.UNSPECIFIED, View.MeasureSpec.UNSPECIFIED);
        customMarkerView.layout(0, 0, customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight());
        customMarkerView.buildDrawingCache();
        Bitmap returnedBitmap = Bitmap.createBitmap(customMarkerView.getMeasuredWidth(), customMarkerView.getMeasuredHeight(),
                Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(returnedBitmap);
        canvas.drawColor(Color.WHITE, PorterDuff.Mode.SRC_IN);
        Drawable drawable = customMarkerView.getBackground();
        if (drawable != null)
            drawable.draw(canvas);
        customMarkerView.draw(canvas);
        return returnedBitmap;
    }
}
