package in.juspay.beckn.utils;

import android.annotation.SuppressLint;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.PixelFormat;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.util.Log;
import android.view.Gravity;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

import androidx.constraintlayout.widget.ConstraintLayout;

import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.InetAddress;
import java.net.Socket;
import java.net.URL;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.cert.X509Certificate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Calendar;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import in.juspay.beckn.R;
import in.juspay.hypersdk.core.JuspayServices;

public class AcceptRejectOverlaySheetService extends Service implements View.OnTouchListener {

    private static final String TAG = AcceptRejectOverlaySheetService.class.getSimpleName();
    private WindowManager windowManager;
    private int notificationID;
    final static int serviceId = 137630;
    private Button buttonAccept,buttonReject;
    TextView journeyStartsFrom, journeyEndsAt, estimatedPriceText, etaPickUpText, distancePickUpText;
    private Timer timer;
    private int checkForClick = 0 ;
    private JuspayServices juspayServices;
    private String caseId = null, baseUrl= null, shared_prefs_key = null ,base_env = null;
    private CountDownTimer mCountDownTimer;
    private long mTimeLeftInMillis = 5000;
    @SuppressLint("MissingPermission")
    private static FirebaseAnalytics mFirebaseAnalytics;
    private View floatyView;

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        System.out.println("in onStartCommand");
        if(intent!=null){
            try{
                shared_prefs_key = intent.getStringExtra("shared_prefs_key");
                base_env = intent.getStringExtra("base_env");
                if(shared_prefs_key != null){
                    caseId = intent.getStringExtra("caseId");
                    if (base_env.equals("null")){
                        baseUrl = "https://api.beckn.juspay.in/transport/v2";
                    }else{
                        baseUrl = base_env;
                    }
                    notificationID = intent.getIntExtra("notificationId",0);
                    getNotificationData(baseUrl,caseId,shared_prefs_key);
                    System.out.println("after calling latlongAPI");

                } else {
                    if(timer != null){
                        timer.cancel();
                    }
                    stopForeground(true);
                    stopSelf();
                }
            }catch (Exception e){
                Log.e(TAG, ""+e);
            }
        }else{
            System.out.println("In Catch");
        }
        return START_STICKY;
    }

    private void acceptRideApi( final String baseurl, final String caseId, final String shared_prefs_key){
        SharedPreferences sharedPref = this.getSharedPreferences(
                this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String version = sharedPref.getString("VERSION_NAME", "null");
        String adId = sharedPref.getString("AD_ID", "null");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() ->{
            StringBuilder result = new StringBuilder();
            try {
                String orderUrl = baseurl + "/driver/rideBooking/"+caseId+"/notification/respond";
                System.out.print("in acceptRideApi"+ orderUrl);
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("x-client-version", version);
                connection.setRequestProperty("x-client-adId", adId);
                connection.setRequestProperty("token", shared_prefs_key);
                connection.setDoOutput(true);

                JSONObject payload = new JSONObject();

                // payload.put("productInstanceId", caseId);
                payload.put("response", "ACCEPT");

                OutputStream stream = connection.getOutputStream();
                stream.write(payload.toString().getBytes());
                connection.connect();
                int respCode = connection.getResponseCode();
                InputStreamReader respReader;

                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    respReader = new InputStreamReader(connection.getErrorStream());
                    System.out.print("in error : "+ respReader);
                } else {
                    respReader = new InputStreamReader(connection.getInputStream());
                    System.out.print("in 200 : "+ respReader);
                }

                BufferedReader in = new BufferedReader(respReader);
                String inputLine;

                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                System.out.print("in result : "+ result.toString());
            } catch (Exception ignored) {
                System.out.println("Catch in acceptRideApi : " +ignored);
            }
            handler.post(()->{
                onDestroy();
                stopForeground(true);
                stopSelf();
                executor.shutdown();
            });
        });
    }

    private void rejectRideApi(final String baseurl, final String caseId, final String shared_prefs_key){
        System.out.print("in rejectRideApiCalled");
        SharedPreferences sharedPref = this.getSharedPreferences(
                this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String version = sharedPref.getString("VERSION_NAME", "null");
        String adId = sharedPref.getString("AD_ID", "null");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
            StringBuilder result = new StringBuilder();
            try {
                String orderUrl = baseurl + "/driver/rideBooking/"+caseId+"/notification/respond";
                System.out.print("in rejectRideApi");
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod("POST");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("x-client-version", version);
                connection.setRequestProperty("x-client-adId", adId);
                connection.setRequestProperty("token", shared_prefs_key);
                connection.setDoOutput(true);

                JSONObject payload = new JSONObject();

                // payload.put("productInstanceId", caseId);
                payload.put("response", "REJECT");

                OutputStream stream = connection.getOutputStream();
                stream.write(payload.toString().getBytes());
                connection.connect();
                int respCode = connection.getResponseCode();
                InputStreamReader respReader;

                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    respReader = new InputStreamReader(connection.getErrorStream());
                    System.out.print("in error : "+ respReader);
                } else {
                    respReader = new InputStreamReader(connection.getInputStream());
                    System.out.print("in 200 : "+ respReader);
                }

                BufferedReader in = new BufferedReader(respReader);
                String inputLine;

                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                System.out.print("in result : "+ result.toString());
            } catch (Exception ignored) {
                System.out.println("Catch in rejectRideApi : " +ignored);
            }
            handler.post(() -> {
                onDestroy();
                stopForeground(true);
                stopSelf();
                executor.shutdown();
            });
        });
    }

    private String getStandardTime(String dateStr) {
        SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
        df.setTimeZone(TimeZone.getTimeZone("UTC"));
        Date date = null;
        try {
            date = df.parse(dateStr);
        } catch (ParseException e) {
            e.printStackTrace();
        }
        df.setTimeZone(TimeZone.getDefault());
        String formattedDate = df.format(date);
        return formattedDate;
    }
    private void getNotificationData(final String baseurl, final String caseId, final String shared_prefs_key ){
        SharedPreferences sharedPref = this.getSharedPreferences(
                this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String version = sharedPref.getString("VERSION_NAME", "null");
        String adId = sharedPref.getString("AD_ID", "null");
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
            StringBuilder result = new StringBuilder();
            try {
                String orderUrl = baseurl + "/driver/rideBooking/" + caseId+"/notification";
                System.out.print(orderUrl);
                System.out.println(" in getNotificationData");
                HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
                if (connection instanceof HttpsURLConnection)
                    ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
                connection.setRequestMethod("GET");
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("x-client-version", version);
                connection.setRequestProperty("x-client-adId", adId);
                connection.setRequestProperty("token", shared_prefs_key);
                connection.connect();

                int respCode = connection.getResponseCode();
                InputStreamReader respReader;

                if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                    respReader = new InputStreamReader(connection.getErrorStream());
                    System.out.print("in error"+ respReader+ ", respcode: "+ respCode);
                } else {
                    respReader = new InputStreamReader(connection.getInputStream());
                    System.out.print("in 200"+ respReader);
                }

                BufferedReader in = new BufferedReader(respReader);
                String inputLine;

                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                System.out.print("in result getNotificationApi : "+ result.toString());
            } catch (Exception ignored) {
                System.out.println("Catch in getNotificationApi : " +ignored);
            }
            handler.post(() -> {
                System.out.println(" in onPostExecute : " +result);
                try{
                    JSONObject resp = new JSONObject(String.valueOf(result));
                    if (resp.has("rideRequest") )
                    {
                        if (resp.get("rideRequest")!=null)
                        {
                            System.out.println(" in ride request is not null : " +resp);
                            JSONObject rideData = new JSONObject(resp.get("rideRequest").toString());
                            if (rideData.has("notificationExpiryTime"))
                            {
                                System.out.println(" RIDEDATAAAAA " +rideData);
                                // Decoding address for pickUP and drop location
                                JSONObject addPickUpJSON = new JSONObject(rideData.get("pickupLoc").toString());
//                                JSONObject addressPickUpJSON = new JSONObject(addPickUpJSON.get("address").toString());
                                JSONObject addDropJSON = new JSONObject(rideData.get("dropLoc").toString());
//                                JSONObject addressDropJSON = new JSONObject(addDropJSON.get("address").toString());
                                String addressPickUp = decodeAddress(addPickUpJSON);
                                String addressDrop = decodeAddress(addDropJSON);
                                System.out.println(" in rideDATAA : " + addressPickUp + " -- and -- "+ addressDrop);

                                //Calculating time difference between current UTC and notificationExpireTime
                                final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                                System.out.println(" in SimpleDateFormat : " + f);
                                f.setTimeZone(TimeZone.getTimeZone("UTC"));
                                String getCurrTime = f.format(new Date());
                                System.out.println(" in getCurrTime : " + getCurrTime);
                                String getCurrTimeIst = getStandardTime (getCurrTime);
                                // System.out.println(" in getCurrTimeeeeeeee : " + getCurrTimeIst);
                                String getExpiryTime = rideData.get("notificationExpiryTime").toString();
                                // System.out.println(" in getExpiryTime : " + getExpiryTime);
                                String getExpiryTimeIst = getStandardTime (getExpiryTime);
                                // System.out.println(" in getExpiryTimeeeeeeee : " + getExpiryTimeIst);
                                String getPickupDistance = rideData.get("distanceToPickupLoc").toString();
                                String getEtaForPickup = rideData.get("etaForPickupLoc").toString();
                                String getEstPrice = rideData.get("estimatedTotalFare").toString();//estimatedTotalFare
                                System.out.println(" in getEstPrice : " + getEstPrice);
                                int calculatedTime = setExpireTimer(getExpiryTime,getCurrTime);
                                // System.out.println(" in calculatedTime : " + calculatedTime);
                                Date currentDeviceTime = Calendar.getInstance().getTime();
                                System.out.println(" get device currentDeviceTime : " + currentDeviceTime);
                                //Calling overlay
                                if(calculatedTime > 0){
                                    if(calculatedTime > 25 ){
                                        calculatedTime = 25;
                                    }
                                    JSONObject notificationPayload = new JSONObject();
                                    notificationPayload.put("notification_type", "NEW_RIDE_AVAILABLE");
                                    notificationPayload.put("calculated_time", calculatedTime);
                                    startForeground(serviceId, NotificationUtils.createNotification(getApplicationContext(), "Yatri Partner is running", "", notificationPayload));
                                    mTimeLeftInMillis = (calculatedTime)*1000;
                                    windowManager = (WindowManager) getSystemService(Context.WINDOW_SERVICE);//String priceEST, String pickupETA, String pickupDistanc
                                    addOverlayView(addressPickUp,addressDrop,getEstPrice,getEtaForPickup,getPickupDistance);
                                } else {
                                    Bundle params = new Bundle();
                                    mFirebaseAnalytics.logEvent("notification_expired",params);
                                }
                                firebaseLogEventWithParams("notification_received_at_ist" , "time" ,getCurrTimeIst);
                                firebaseLogEventWithParams("notification_expiry_time" , "time" ,getExpiryTimeIst);
                                firebaseLogEventWithParams("device_current_time" , "time" ,currentDeviceTime.toString());
                                firebaseLogEventWithParams("time_detail_ct_net_cdt" , "time" , getCurrTimeIst + " ..... " + getExpiryTimeIst + " ..... " + currentDeviceTime.toString());
                            }
                        }

                    }
                }
                catch(Exception e)
                {
                    System.out.println("Catch in getNotificationApi : " + e);
                }
                // TODO: check this.exception
                // TODO: do something with the feed
            });
        });
    }


    public void firebaseLogEventWithParams(String event,String paramKey,String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey,paramValue);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        mFirebaseAnalytics.logEvent(event, params);
    }

    private String decodeAddress(JSONObject object){
        try {
            String add1 = object.get("door").toString()+ ", "+ object.get("building").toString()+ ", "+ object.get("street").toString()+ ", "+ object.get("city").toString()+ ", "+ object.get("state").toString()+ ", "+ object.get("country").toString(); // + object.get("locality").toString()
            String add2 = object.get("building").toString()+ ", "+ object.get("street").toString()+ ", "+ object.get("city").toString()+ ", "+ object.get("state").toString()+ ", "+ object.get("country").toString(); // after street ", "+ object.get("locality").toString()+
            String add3 = object.get("street").toString()+ ", "+ object.get("city").toString()+ ", "+ object.get("state").toString()+ ", "+ object.get("country").toString();
            String add4 = object.get("city").toString()+ ", "+ object.get("state").toString()+ ", "+ object.get("country").toString();
            String add5 = object.get("city").toString()+ ", "+ object.get("state").toString()+ ", "+ object.get("country").toString();
            if(object.get("door").toString().equals("")  && object.get("building").toString().equals(" ") && object.get("street").toString().equals("")){ //&& object.get("locality").toString().equals(" ")
                return add5;
            }else if(object.get("door").toString().equals("")  && object.get("building").toString().equals(" ") && object.get("street").toString().equals("")){
                return add4;
            }else if(object.get("door").toString().equals("")  && object.get("building").toString().equals(" ")){
                return add3;
            }else if(object.get("door").toString().equals("")){
                return add2;
            }else{
                return add1;
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return "ERROR DECODING";
    }

    private int setExpireTimer(String expireTimeTemp, String currTimeTemp){
        String[] arrOfA = expireTimeTemp.split("T");
        String[] arrOfB = currTimeTemp.split("T");

        if(!arrOfA[0].equals(arrOfB[0])){
            return -1;
        }

        String[] timeTempExpire = arrOfA[1].split(":");
        String[] timeTempCurrent = arrOfB[1].split(":");

        timeTempExpire[2] = timeTempExpire[2].substring(0,2);
        timeTempCurrent[2] = timeTempCurrent[2].substring(0,2);

        int currTime = 0, expireTime = 0, calculate = 3600;

        for(int i = 0 ; i < timeTempCurrent.length;i++){

            currTime+= (Integer.parseInt(timeTempCurrent[i])*calculate);
            expireTime+= (Integer.parseInt(timeTempExpire[i])*calculate);
            calculate = calculate/60;

        }
        System.out.println("Time remaining and curr - expire: " + (expireTime-currTime)+" .... "+currTimeTemp+" .. "+expireTimeTemp);
        if ((expireTime-currTime) >= 5)
        {
            return expireTime-currTime - 5 ;
        }
        return 0;
    }

    private void startTimer(){
        mCountDownTimer = new CountDownTimer(mTimeLeftInMillis, 1000) {
            @Override
            public void onTick(long millisUntilFinished) {
                mTimeLeftInMillis = millisUntilFinished;
                updateCountDownText();
            }
            @Override
            public void onFinish() {
                if (checkForClick == 0) {
                    if(floatyView != null)
                    {
                        floatyView.setVisibility(View.GONE);
                    }
                    Bundle params = new Bundle();
                    // params.putString("status", "ignore");
                    mFirebaseAnalytics.logEvent("ride_ignored", params);

                    onDestroy();
                    stopForeground(true);
                    stopSelf();
                }
            }
        }.start();
    }

    private void updateCountDownText() {
        int seconds = (int) (mTimeLeftInMillis / 1000) % 60;
        if(seconds < 10){
            buttonAccept.setText("Accept in 0"+ seconds);
        }else {
            buttonAccept.setText("Accept in " + seconds);
        }
    }

    private void addOverlayView(String source, String destination , String priceEST, String pickupETA, String pickupDistance) {
        final WindowManager.LayoutParams params;
        // final Bundle logPayload = new Bundle();
        // final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        int layoutParamsType;

        // if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
            layoutParamsType = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
        }
        else {
            layoutParamsType = WindowManager.LayoutParams.TYPE_PHONE;
        }
        params = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.MATCH_PARENT,
                WindowManager.LayoutParams.WRAP_CONTENT,
                layoutParamsType,
                WindowManager.LayoutParams.FLAG_TURN_SCREEN_ON | WindowManager.LayoutParams.FLAG_DIM_BEHIND,
                PixelFormat.TRANSPARENT);
        params.dimAmount = 0.6f;
        params.gravity = Gravity.CENTER;

        ConstraintLayout interceptorLayout = new ConstraintLayout(this){
            public boolean dispatchKeyEvent(KeyEvent event) {
                if (event.getAction() == KeyEvent.ACTION_DOWN) {
                    if (event.getKeyCode() == KeyEvent.KEYCODE_BACK) {
                        Log.v(TAG, "BACK Button Pressed");
                        return true;
                    }
                }
                return super.dispatchKeyEvent(event);
            }
        };

        LayoutInflater inflater = ((LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE));

        if (inflater != null) {
            // logPayload.putString("screen_name","driver_pop_modal");
            // logPayload.putString("click_category","pop_up_click");
            floatyView = inflater.inflate(R.layout.activity_accept_reject_overlay_sheet, interceptorLayout);

            //starting timer
            mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
            startTimer();
            NotificationManager notificationManager = (NotificationManager)getSystemService(Context.NOTIFICATION_SERVICE ) ;

            //Button to accept ride
            buttonAccept = (Button) floatyView.findViewById(R.id.AcceptButton);
            buttonAccept .setOnClickListener(new View.OnClickListener(){
                @Override
                public void onClick(View v){
                    if(floatyView != null)
                    {
                        floatyView.setVisibility(View.GONE);
                    }
                    checkForClick = 1;
                    notificationManager.cancel(notificationID);
                    Bundle params = new Bundle();
                    // params.putString("status","accept");
                    // logPayload.putString("click_name","accept_click");
                    // sdkTracker.trackAction(PaymentConstants.SubCategory.Action.USER, PaymentConstants.LogLevel.INFO, "on_click", "value",logPayload);
                    acceptRideApi(baseUrl,caseId,shared_prefs_key);
                    mFirebaseAnalytics.logEvent("ride_accepted", params);
                }
            });

            //Button to reject ride
            buttonReject = (Button) floatyView.findViewById(R.id.RejectButton);
            buttonReject .setOnClickListener(new View.OnClickListener(){
                @Override
                public void onClick(View v){
                    if(floatyView != null)
                    {
                        floatyView.setVisibility(View.GONE);
                    }
                    checkForClick = 1;
                    notificationManager.cancel(notificationID);
                    Bundle params = new Bundle();
                    // params.putString("status","decline");
                    // logPayload.putString("click_name","decline_click");
                    // sdkTracker.trackAction(PaymentConstants.SubCategory.Action.USER, PaymentConstants.LogLevel.INFO, "on_click", "value",logPayload);
                    rejectRideApi(baseUrl,caseId,shared_prefs_key);
                    mFirebaseAnalytics.logEvent("ride_rejected", params);
                    Toast.makeText(getApplicationContext(),
                            "Ride Declined Sucessfully",
                            Toast.LENGTH_LONG)
                            .show();
                }
            });

            //set journeyStart text
            journeyStartsFrom = (TextView) floatyView.findViewById(R.id.journeyStarts);
            journeyStartsFrom.setText(source);

            //set journeyEnds text
            journeyEndsAt = (TextView) floatyView.findViewById(R.id.journeyEnds);
            journeyEndsAt.setText(destination);

            //set est. price text
            estimatedPriceText = (TextView) floatyView.findViewById(R.id.estPrice);
            // priceEST = priceEST.split("\\.")[0];
            double estimatedFinalPrice = Double.parseDouble(priceEST);
            estimatedPriceText.setText("₹ "+ Math.round(estimatedFinalPrice));

            //set distance to pickUp text
            distancePickUpText = (TextView) floatyView.findViewById(R.id.distanceToPickUp);
            pickupDistance = pickupDistance.split("\\.")[0];

            // This change needs to be tested
            int tempPickupDistance=Integer.parseInt(pickupDistance);
            System.out.println("tempPickUpDistance :- " +tempPickupDistance);
            if(tempPickupDistance > 999){
                pickupDistance = "" + (tempPickupDistance/1000);
                if(tempPickupDistance%1000 != 0){
                    pickupDistance += "."+ (Integer.toString(tempPickupDistance%1000)).charAt(0) + " Km";
                }
            }else{
                pickupDistance += " m";
            }
            System.out.println("pickupDistance :- " +pickupDistance);
            distancePickUpText.setText(pickupDistance);

            //set eta to location text
            etaPickUpText = (TextView) floatyView.findViewById(R.id.etaPickUp);
            pickupETA = pickupETA + " mins";
            etaPickUpText.setText(pickupETA);


            floatyView.setOnTouchListener(this);
            windowManager.addView(floatyView, params);
        }
        else {
            Log.e("SAW-example", "Layout Inflater Service is null; can't inflate and display R.layout.floating_view");
        }
    }

    @Override
    public void onDestroy() {

        super.onDestroy();

        if (floatyView != null) {
            windowManager.removeView(floatyView);
            floatyView = null;
        }
    }




    @Override
    public boolean onTouch(View view, MotionEvent motionEvent) {
        view.performClick();
        Log.v(TAG, "onTouch...");
        return true;
    }
}
