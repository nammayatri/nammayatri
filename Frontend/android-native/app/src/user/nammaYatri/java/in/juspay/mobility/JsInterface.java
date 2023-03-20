/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility;

import android.app.Activity;
import android.app.DatePickerDialog;
import android.app.Notification;
import android.app.TimePickerDialog;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.webkit.JavascriptInterface;
import android.widget.DatePicker;
import android.widget.TimePicker;
import android.widget.Toast;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.model.Marker;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Calendar;
import java.util.Locale;

import in.juspay.mobility.utils.NotificationUtils;
import in.juspay.hypersdk.core.DuiCallback;
import in.juspay.hypersdk.core.HyperFragment;
import in.juspay.hypersdk.core.JuspayServices;

public class JsInterface extends CommonJsInterface implements in.juspay.hypersdk.core.JSI {

    private static final String LOG_TAG = "Beckn_JsInterface";
    private Activity activity;
    private JuspayServices juspayServices;
    private Context context;
    private DuiCallback dynamicUI;
    private GoogleMap googleMap;
    private Marker userPositionMarker;
    private FusedLocationProviderClient client;


    public JsInterface(){
        super();
    }

    public JsInterface(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super(activity, juspayServices, fragment);
        try {
            JSONObject headerObj = new JSONObject();
            headerObj.put("x-client-id", "nammayatri");
            boolean b = setAnalyticsHeader(headerObj.toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.context = activity;
        this.dynamicUI = juspayServices.getDuiCallback();
        this.activity = activity;
        this.juspayServices = juspayServices;
        client = LocationServices.getFusedLocationProviderClient(context);
    }


    @Override
    public void setActivity(Activity activity) {
        super.setActivity(activity);
        this.activity = activity;
    }

    @Override
    public void set(Activity activity, JuspayServices juspayServices, HyperFragment fragment) {
        super.set(activity, juspayServices, fragment);
        this.context = juspayServices.getContext();
        this.dynamicUI = juspayServices.getDuiCallback();
        this.activity = activity;
        this.juspayServices = juspayServices;
        try {
            JSONObject headerObj = new JSONObject();
            headerObj.put("x-client-id", "nammayatri");
            boolean b = setAnalyticsHeader(headerObj.toString());
            } catch (Exception e) {
                e.printStackTrace();
            }
//        polylines = new ArrayList<>();

    }

//     @JavascriptInterface
//     public void showDialer(String phoneNum) {
//         Intent intent = new Intent();
//         intent.setAction(Intent.ACTION_DIAL);
//         String phoneNumber;
//         phoneNumber = "tel:" + phoneNum;
//         intent.setData(Uri.parse(phoneNumber));
//         activity.startActivity(intent);
// //        Intent sendIntent = new Intent();
// //        sendIntent.setAction(Intent.ACTION_SEND);
// //        sendIntent.putExtra(Intent.EXTRA_TEXT, phoneNum);
// //        sendIntent.setType("text/plain");
// //
// //        Intent shareIntent = Intent.createChooser(sendIntent, null);
// //        activity.startActivity(shareIntent);
//     }
    @JavascriptInterface
    public void scheduleNotification(final String title, final String content, final String data, final int delay) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    Notification notification = NotificationUtils.createNotification(context, title, content, new JSONObject(data));
                    NotificationUtils.scheduleNotification(context, notification, delay);
                }catch (JSONException e){
                    Log.e("Notification", "data not a valid json: " + e.toString());
                }
            }
        });
    }

    @JavascriptInterface
    public void showNotification(final String title, final String content, final String data,final String imageUrl) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    NotificationUtils.showNotification(context, title, content, new JSONObject(data),imageUrl);
                }catch (JSONException e){
                    Log.e("Notification", "data not a valid json: " + e.toString());
                }

            }
        });
    }

    @JavascriptInterface
    public void hideKeyboardOnNavigation(boolean permission) {
        View view = this.activity.getCurrentFocus();
        if (view == null) {
            view = new View(this.activity);
        }
        InputMethodManager imm = (InputMethodManager) this.activity.getSystemService(Activity.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(activity.getWindow().getDecorView().getWindowToken(), 0);
    }

    @JavascriptInterface
    public void timePicker(final String callback) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                final Calendar c = Calendar.getInstance();
                int hour = c.get(Calendar.HOUR_OF_DAY);
                int minute = c.get(Calendar.MINUTE);
                Log.e(LOG_TAG,"Time picker called");
                TimePickerDialog timePickerDialog = new TimePickerDialog(activity, new TimePickerDialog.OnTimeSetListener() {
                    @Override
                    public void onTimeSet(TimePicker timePicker, int hourOfDay, int minute) {

                        Calendar datetime = Calendar.getInstance();
                        Calendar c = Calendar.getInstance();
                        datetime.set(Calendar.HOUR_OF_DAY, hourOfDay);
                        datetime.set(Calendar.MINUTE, minute);
                        Log.e(LOG_TAG, "Time picker called");
                        if (datetime.getTimeInMillis() >= c.getTimeInMillis()) {
                            //it's after current
                            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d,%d);",
                                    callback, hourOfDay, minute);

                            if (dynamicUI != null) {
                                dynamicUI.addJsToWebView(javascript);
                            } else {
                                //it's before current'
                                Toast.makeText(activity, "Invalid Time", Toast.LENGTH_LONG).show();
                            }


                        }
                    }
                }, hour, minute, false);
                timePickerDialog.show();
            }
        });
    }

    @JavascriptInterface
    public void datePicker(final String callback) {
        activity.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                final Calendar c = Calendar.getInstance();
                int mYear = c.get(Calendar.YEAR);
                int mMonth = c.get(Calendar.MONTH);
                int mDate = c.get(Calendar.DATE);
                Log.e(LOG_TAG,"Time picker called");
                DatePickerDialog datePickerDialog = new DatePickerDialog(activity, new DatePickerDialog.OnDateSetListener() {
                    @Override
                    public void onDateSet(DatePicker datePicker, int year, int month, int date) {
                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s',%d,%d,%d);",
                                callback, year, month + 1, date);

                        if (dynamicUI != null) {
                            dynamicUI.addJsToWebView(javascript);
                        }
                    }
                }, mYear, mMonth, mDate);
                datePickerDialog.show();
            }
        });
    }
}