/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.customer;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.graphics.pdf.PdfDocument;
import android.net.Uri;
import android.os.Build;
import android.view.LayoutInflater;
import android.view.View;
import android.view.animation.LinearInterpolator;
import android.webkit.JavascriptInterface;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.core.content.FileProvider;

import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.ButtCap;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PatternItem;
import com.google.android.gms.maps.model.Polyline;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.mobility.common.MapRemoteConfig;
import in.juspay.mobility.common.MobilityCommonBridge;
import android.view.accessibility.AccessibilityManager;

public class MobilityCustomerBridge extends MobilityCommonBridge {

    @Override
    public void reset() {
        super.reset();
    }

    // CallBacks Strings
    public enum MapMode {
        NORMAL, SPECIAL_ZONE, HOTSPOT
    }


    public MobilityCustomerBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        app = AppType.CONSUMER;
    }

    // endregion

    //region Maps




    @JavascriptInterface
    public void removeLabelFromMarker(float zoomLevel) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                zoom = zoomLevel;
                if (layer != null) {
                    layer.removeLayerFromMap();
                }
                if (userPositionMarker != null) {
                    userPositionMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(CURRENT_LOCATION, false, MarkerType.NORMAL_MARKER, new MarkerConfig())));
                    userPositionMarker.setTitle("");
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    //endregion

    @JavascriptInterface
    public void fetchAndUpdateCurrentLocation(String callback) {
        fetchAndUpdateCurrentLocation(callback,true);
    }

    @JavascriptInterface
    public void fetchAndUpdateCurrentLocation(String callback, boolean shouldFallback) {
        if (!isLocationPermissionEnabled()) return;
        updateLastKnownLocation(callback, true, ZoomType.ZOOM,shouldFallback);
    }

    //region Others

    @JavascriptInterface
    public void generatePDF(String str, String format) throws JSONException {
        invoice = str;
        if (checkAndAskStoragePermission()){
            downloadPDF(str, bridgeComponents.getContext());
        }
    }

    @SuppressLint("MissingPermission")
    public void downloadPDF(String str, Context context) throws JSONException {
        new Thread(() -> {
            try {
                JSONObject state = new JSONObject(str);
                JSONObject data = state.getJSONObject("data");
                String userName = getKeysInSharedPref("USER_NAME");
                String pdfHeading = data.getString("pdfHeading");
                JSONObject selectedItem = data.getJSONObject("selectedItem");
                JSONArray fares = selectedItem.getJSONArray("faresList");
                PdfDocument pdfDocument = new PdfDocument();
                PdfDocument.PageInfo invoicePDF = new PdfDocument.PageInfo.Builder(960, 1338, 1).create();
                PdfDocument.Page page = pdfDocument.startPage(invoicePDF);
                JuspayLogger.d(OTHERS, "PDF Document Created");
                View content = getInvoiceLayout(selectedItem, fares, userName, pdfHeading,  context);
                JuspayLogger.d(OTHERS, "PDF Layout inflated");
                content.measure(page.getCanvas().getWidth(), page.getCanvas().getHeight());
                content.layout(0, 0, page.getCanvas().getWidth(), page.getCanvas().getHeight());
                content.draw(page.getCanvas());
                pdfDocument.finishPage(page);
                JuspayLogger.d(OTHERS, "PDF Document canvas drawn");
                String fileNameformat;
                String serviceName = context.getResources().getString(R.string.service);
                String notificationHeading = context.getString(R.string.invoice_downloaded);
                String notificationSubheading = context.getString(R.string.invoice_for_your_ride_is_downloaded);
                System.out.println("service name ::" + serviceName);
                if (serviceName.equals("yatrisathiconsumer")) {
                    fileNameformat = "YS_RIDE_";
                } else if (serviceName.equals("nammayatriconsumer")) {
                    fileNameformat = "NY_RIDE_";
                    notificationHeading = context.getString(R.string.driver_receipt_downloaded);
                    notificationSubheading = context.getString(R.string.driver_receipt_for_your_ride_is_downloaded);
                } else {
                    fileNameformat = "YATRI_RIDE_";
                }
                fileNameformat = fileNameformat + selectedItem.getString("date") + selectedItem.getString("rideStartTime");
                String removedSpecial = fileNameformat.replaceAll("[^a-zA-Z\\d]", "_");
                JuspayLogger.d(OTHERS, "PDF Document name " + removedSpecial);
                try {
                    File file = checkAndGetFileName(removedSpecial);
                    JuspayLogger.d(OTHERS, "Available File name for PDF" + file.getName());
                    FileOutputStream fos = new FileOutputStream(file);
                    pdfDocument.writeTo(fos);
                    JuspayLogger.d(OTHERS, "PDF Document written to path " + file.getPath());
                    Uri path = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", file);
                    invoice = null;
                    showNotificationWithURI(path, notificationHeading, notificationSubheading, "application/pdf", "Invoice", "Invoice Download");
                } catch (IOException e) {
                    e.printStackTrace();
                }
                pdfDocument.close();
                JuspayLogger.d(OTHERS, "PDF Document closed ");
            } catch (Exception e) {
                JuspayLogger.e(OTHERS, e.toString());
            }
        }).start();
    }

    @SuppressLint("SetTextI18n")
    private View getInvoiceLayout(JSONObject selectedRide, JSONArray fares, String user, String heading, Context context) throws JSONException {
        JuspayLogger.d(OTHERS, "PDF Document inside inflate View");
        View invoiceLayout = LayoutInflater.from(context).inflate(R.layout.invoice_template, null, false);
        JuspayLogger.d(OTHERS, "PDF Document inflated View");
        TextView textView = invoiceLayout.findViewById(R.id.rideDate);
        textView.setText(selectedRide.getString("date"));
        textView = invoiceLayout.findViewById(R.id.userName);
        textView.setText(user.trim());
        textView = invoiceLayout.findViewById(R.id.paymentDetail);
        textView.setText(selectedRide.getString("totalAmount"));
        textView = invoiceLayout.findViewById(R.id.headingText);
        textView.setText(heading);
        LinearLayout fareBreakupElements = invoiceLayout.findViewById(R.id.fareBreakupElements);
        fareBreakupElements.setOrientation(LinearLayout.VERTICAL);

        try {
            for (int i = 0; i < fares.length(); i++) {
                LinearLayout.LayoutParams linearParams = new LinearLayout.LayoutParams(
                        LinearLayout.LayoutParams.MATCH_PARENT,
                        LinearLayout.LayoutParams.WRAP_CONTENT);
                LinearLayout linearLayout = new LinearLayout(context);
                linearLayout.setLayoutParams(linearParams);

                LinearLayout.LayoutParams linearParamsChild = new LinearLayout.LayoutParams(
                        LinearLayout.LayoutParams.MATCH_PARENT,
                        LinearLayout.LayoutParams.WRAP_CONTENT);
                LinearLayout linearLayoutChild = new LinearLayout(context);
                linearLayoutChild.setLayoutParams(linearParamsChild);
                linearParamsChild.weight = 1.0f;

                JSONObject fare = fares.getJSONObject(i);
                JuspayLogger.d(OTHERS, "PDF Document updating fares break ups" + fare);
                String value = fare.getString("price");
                String fareTypes = fare.getString("title");
                TextView textViewText = new TextView(context);
                textViewText.setTextSize(5);
                textViewText.setTextColor(Color.parseColor("#454545"));
                textViewText.setPadding(0, 0, 0, 10);
                textViewText.setText(fareTypes);
                linearLayout.addView(textViewText);
                linearLayout.addView(linearLayoutChild);

                TextView textViewPrice = new TextView(context);
                textViewPrice.setTextSize(5);
                textViewPrice.setPadding(0, 0, 0, 10);
                textViewPrice.setTextColor(Color.parseColor("#454545"));
                textViewPrice.setText(value);
                linearLayout.addView(textViewPrice);

                fareBreakupElements.addView(linearLayout);
                JuspayLogger.d(OTHERS, "PDF Document updated the fare " + fare + "in view");
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
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
        textView = invoiceLayout.findViewById(R.id.referenceText);
        textView.setText(selectedRide.getString("referenceString"));
        if(selectedRide.getString("destination").equals(""))
        {
            invoiceLayout.findViewById(R.id.destination).setVisibility(View.GONE);
            invoiceLayout.findViewById(R.id.rideEndTime).setVisibility(View.GONE);
            invoiceLayout.findViewById(R.id.destRedCircle).setVisibility(View.GONE);
            invoiceLayout.findViewById(R.id.dashedLine).setVisibility(View.GONE);
        }

        JuspayLogger.d(OTHERS, "PDF Document view updated and returning the view");
        return invoiceLayout;
    }

    @JavascriptInterface
    public int methodArgumentCount(String functionName) {
        try {
            methods = methods == null ? this.getClass().getMethods() : methods;
            for (Method m : methods) {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                    if (m.getName().equals(functionName)) {
                        return m.getParameterCount();
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }
    //endregion

    //region Override Functions
    @Override
    public boolean onActivityResult(int requestCode, int resultCode, Intent data) {
        return super.onActivityResult(requestCode, resultCode, data);
    }

    @Override
    public boolean onRequestPermissionResult(int requestCode, String[] permissions, int[] grantResults) {
        switch (requestCode) {
            case REQUEST_CALL:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    showDialer(phoneNumber, true);
                } else {
                    showDialer(phoneNumber, false);
                    toast("Permission Denied");
                }
                break;
            case LOCATION_PERMISSION_REQ_CODE:
                if (grantResults.length > 0 && grantResults[0] != PackageManager.PERMISSION_GRANTED) {
                    toast("Permission Denied");
                }
                break;
            case STORAGE_PERMISSION:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    if (invoice != null){
                        try {
                            JuspayLogger.d(OTHERS, "Storage Permission is granted. downloading  PDF");
                            downloadPDF(invoice, bridgeComponents.getContext());
                        } catch (JSONException e) {
                            e.printStackTrace();
                        }
                    } else if (downloadLayout != null) {
                        try {
                            JuspayLogger.d(OTHERS, "Storage Permission is granted. downloading  PDF");
                            downloadLayoutAsImage(downloadLayout);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                } else {
                    JuspayLogger.d(OTHERS, "Storage Permission is denied.");
                    toast("Permission Denied");
                }
                break;
            default:
                break;
        }
        return super.onRequestPermissionResult(requestCode, permissions, grantResults);
    }
    //endregion

    @JavascriptInterface
    public boolean isAccessibilityEnabled() {
        AccessibilityManager accessibilityManager = (AccessibilityManager) bridgeComponents.getContext().getSystemService(Context.ACCESSIBILITY_SERVICE);
        return accessibilityManager.isEnabled();
    }

    @JavascriptInterface
    public void updateMarkersOnRoute(String _payload) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                JSONObject payload = new JSONObject(_payload);
                String pureScriptID = payload.optString("pureScriptID", "");
                GoogleMap gMap = pureScriptID.isEmpty() ? googleMap : googleMapInstance.get(pureScriptID);
                String json = payload.optString("currentVehicleLocation");
                JSONObject currentVehicleLocation = payload.getJSONObject("currentVehicleLocation");
                double currentLng = currentVehicleLocation.getDouble("lng");
                double currentLat = currentVehicleLocation.getDouble("lat");
                LatLng currentLatLng = new LatLng(currentLat, currentLng);

                String eta = payload.optString("eta", "");
                JSONObject srcMarker = payload.getJSONObject("srcMarker");
                JSONObject srcHeaderArrowMarker = payload.getJSONObject("srcHeaderArrowMarker");
                String vehicleMarkerId = srcMarker.optString("markerId", "");
                String srcHeaderArrowMarkerId = srcHeaderArrowMarker.optString("markerId", "");
                double srcHeaderArrowSize = srcHeaderArrowMarker.optDouble("markerSize", 80.0);
                Marker marker = (Marker) markers.get(vehicleMarkerId);
                Marker markerHeaderArrow = (Marker) markers.get(srcHeaderArrowMarkerId);
                double vehicleRotationFromPrevLatLon = payload.optDouble("vehicleRotationFromPrevLatLon", -1.0);

                if (marker != null) {
                    String srcMarkerPointerIcon = srcMarker.optString("pointerIcon", "");

                    LatLng startPosition = marker.getPosition();
                    ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1);
                    valueAnimator.setDuration(2000);
                    valueAnimator.setInterpolator(new LinearInterpolator());
                    valueAnimator.addUpdateListener(animation -> {
                        try {
                            float v = animation.getAnimatedFraction();
                            LatLng newPosition = SphericalUtil.interpolate(startPosition, currentLatLng, v);
                            marker.setPosition(newPosition);
                            if (markerHeaderArrow != null){
                                String srcHeaderArrowMarkerIcon = srcHeaderArrowMarker.optString("pointerIcon", "");
                                float rotation = (vehicleRotationFromPrevLatLon == -1.0) ? bearingBetweenLocations(startPosition, currentLatLng) : (float) vehicleRotationFromPrevLatLon;
                                if (rotation > 1.0){
                                    MarkerConfig markerConfig = new MarkerConfig();
                                    markerConfig.setRotation(rotation);
                                    markerConfig.setMarkerIconSize((int) srcHeaderArrowSize);
                                    markerHeaderArrow.setRotation(0.0f);
                                    markerHeaderArrow.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(srcHeaderArrowMarkerIcon, false, MarkerType.NORMAL_MARKER_V2, markerConfig)));
                                }

                                markerHeaderArrow.setPosition(newPosition);
                                markerHeaderArrow.setAnchor(0.5f, 0.5f);
                                markers.put(srcHeaderArrowMarkerIcon, markerHeaderArrow);
                            }
                            marker.setAnchor(0.5f, 0.5f);
                        } catch (Exception e) {
                            e.printStackTrace();
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
                else {
                    String srcMarkerStr = payload.optString("srcMarker","");
                    showMarker(srcMarkerStr);
                }
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        });
    }

    @JavascriptInterface
    public boolean checkMarkerAvailable (String _payload) {
        try {
            JSONObject payload = new JSONObject(_payload);
            String markerId = payload.optString("markerId", "");
            return ((Marker) markers.get(markerId) != null);
        } catch (JSONException e) {
            return false;
        }
    }

    @JavascriptInterface
    public String getMarkerPosition (String _payload) {
        try {
            JSONObject payload = new JSONObject(_payload);
            String markerId = payload.optString("markerId", "");
            Marker marker = (Marker) markers.get(markerId);
            if(marker != null) {
                return (marker.getPosition().toString());
            }
            return "";
        } catch (JSONException e) {
            return "";
        }
    }
}
