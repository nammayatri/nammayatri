/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.graphics.PixelFormat;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.os.Binder;
import android.os.Bundle;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.PowerManager;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.viewpager2.widget.ViewPager2;

import com.airbnb.lottie.LottieAnimationView;
import com.google.android.material.progressindicator.LinearProgressIndicator;
import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.net.URL;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;

public class OverlaySheetService extends Service implements View.OnTouchListener {

    private static final String TAG = OverlaySheetService.class.getSimpleName();
    private ViewPager2 viewPager;
    private ArrayList<SheetModel> sheetArrayList = new ArrayList<>();
    ExecutorService executor = Executors.newSingleThreadExecutor();
    private Timer countDownTimer;
    private WindowManager windowManager;
    private String regToken, baseUrl;
    private SharedPreferences sharedPref;
    private MediaPlayer mediaPlayer;
    private int currentVolume, time = 0, progressCompat = 0;
    private AudioManager audio;
    private View progressDialog, apiLoader, floatyView;
    private CountDownTimer rideStatusListener;
    private WindowManager.LayoutParams params;
    private FirebaseAnalytics mFirebaseAnalytics;
    private Boolean isRideAcceptedOrRejected = false;
    private TextView indicatorText1, indicatorText2, indicatorText3;
    private LinearProgressIndicator progressIndicator1, progressIndicator2, progressIndicator3;
    private ArrayList<TextView> indicatorTextList ;
    private ArrayList<LinearProgressIndicator> progressIndicatorsList ;
    private ArrayList<LinearLayout> indicatorList ;
    private Handler mainLooper = new Handler(Looper.getMainLooper());

    public class OverlayBinder extends Binder {
        public OverlaySheetService getService () {
            return OverlaySheetService.this;
        }
    }

    private SheetAdapter sheetAdapter = new SheetAdapter(sheetArrayList, viewPager, new SheetAdapter.OnItemClickListener() {
        @Override
        public void onViewHolderBind(SheetAdapter.SheetViewHolder holder, int position, ViewPager2 viewPager, List<Object> payloads) {
            SheetModel model = sheetArrayList.get(position);
            String x = payloads.size() > 0 ? (String) payloads.get(0) :"";
            switch (x) {
                case "inc" :
                    updateIndicators();
                    holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
                    updateIncreaseDecreaseButtons(holder, model);
                    return;
                case "time" :
                    updateAcceptButtonText(holder, model.getRideRequestPopupDelayDuration(), model.getStartTime(), getString(R.string.accept_offer));
                    updateProgressBars(true);
                    return;
            }

            holder.pickUpDistance.setText(model.getPickUpDistance()+" km ");
            holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
            holder.distanceToBeCovered.setText(model.getDistanceToBeCovered()+" km");
            holder.sourceArea.setText(model.getSourceArea());
            holder.sourceAddress.setText(model.getSourceAddress());
            holder.destinationArea.setText(model.getDestinationArea());
            holder.destinationAddress.setText(model.getDestinationAddress());
            holder.textIncPrice.setText(String.valueOf(model.getNegotiationUnit()));
            holder.textDecPrice.setText(String.valueOf(model.getNegotiationUnit()));
            if (model.getDriverMaxExtraFee() == 0) {
                holder.buttonIncreasePrice.setVisibility(View.GONE);
                holder.buttonDecreasePrice.setVisibility(View.GONE);
            }
            updateAcceptButtonText(holder, model.getRideRequestPopupDelayDuration(),model.getStartTime(), getString(R.string.accept_offer));
            updateIncreaseDecreaseButtons(holder, model);
            holder.reqButton.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    holder.reqButton.setClickable(false);
                    startApiLoader();
                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    Handler handler = new Handler(Looper.getMainLooper());
                    executor.execute(() -> {
                        try {
                            Boolean isApiSuccess = driverRespondApi(model.getSearchRequestId(), model.getOfferedPrice(), true, sheetArrayList.indexOf(model));
                            if (isApiSuccess){
                                holder.reqButton.setClickable(false);
                                updateSharedPreferences();
                                if (MainActivity.getInstance() != null) MainActivity.getInstance().triggerPopUP("ride_requested","RIDE_REQUESTED");
                                firebaseLogEvent("ride_accepted");
                                isRideAcceptedOrRejected = true;
                                handler.post(() -> {
                                    startLoader(model.getSearchRequestId());
                                    executor.shutdown();
                                });
                            }else{
                                handler.post(() -> {
//                                    cleanUp();
//                                    executor.shutdown();
                                    removeCard(position);
                                    if(apiLoader!= null) {
                                        windowManager.removeView(apiLoader);
                                        apiLoader = null;
                                    }
                                    if (sheetArrayList.size() > 0)
                                    {
                                        holder.reqButton.setClickable(true);
                                    }
                                    else
                                    {
                                        cleanUp();
                                        executor.shutdown();
                                    }
                                });
                            }
                        } catch (Exception e) {
                            firebaseLogEventWithParams("exception","request_button_click",e.toString());
                            cleanUp();
                        }
                    });
                }
            });

            holder.rejectButton.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    Handler handler = new Handler(Looper.getMainLooper());
                    executor.execute(() -> {
                        try {
                            new Thread(new Runnable() {
                                @Override
                                public void run() {
                                    driverRespondApi(model.getSearchRequestId(), model.getOfferedPrice(), false, sheetArrayList.indexOf(model));
                                }
                            }).start();
                            isRideAcceptedOrRejected = true;
                            holder.rejectButton.setClickable(false);
                            handler.post(() -> {
                                firebaseLogEvent("ride_declined");
                                removeCard(position);
                                executor.shutdown();
                                Toast.makeText(getApplicationContext(), getApplicationContext().getResources().getString(R.string.ride_rejected), Toast.LENGTH_SHORT).show();
                            });
                        } catch (Exception e) {
                            firebaseLogEventWithParams("exception","reject_button_click",e.toString());
                            System.out.println("reject exception: " + e);
                        }
                    });
                }
            });

            holder.buttonIncreasePrice.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    if (model.getOfferedPrice() <= model.getDriverMaxExtraFee() - model.getNegotiationUnit()) {
                        model.setUpdatedAmount(model.getUpdatedAmount() + model.getNegotiationUnit());
                        firebaseLogEvent("price_is_increased");
                        model.setOfferedPrice(model.getOfferedPrice()+model.getNegotiationUnit());
                        sheetAdapter.notifyItemChanged(position, "inc");
                        if (model.getOfferedPrice() == model.getDriverMaxExtraFee()){
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.post(new Runnable() {
                                @Override
                                public void run() {
                                    model.setButtonIncreasePriceAlpha(0.5f);
                                    model.setButtonIncreasePriceClickable(false);
                                    model.setButtonDecreasePriceAlpha(1.0f);
                                    model.setButtonDecreasePriceClickable(true);
                                }
                            });
                        }
                        else {
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.post(new Runnable() {
                                @Override
                                public void run() {
                                    model.setButtonDecreasePriceAlpha(1.0f);
                                    model.setButtonDecreasePriceClickable(true);
                                }
                            });
                        }
                    }
                }
            });
            holder.buttonDecreasePrice.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    if (model.getOfferedPrice() > 0) {
                        model.setUpdatedAmount(model.getUpdatedAmount() - model.getNegotiationUnit());
                        firebaseLogEvent("price_is_decreased");
                        model.setOfferedPrice(model.getOfferedPrice()-model.getNegotiationUnit());
                        sheetAdapter.notifyItemChanged(position,"inc");
                        if (model.getOfferedPrice() == 0){
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.post(new Runnable() {
                                @Override
                                public void run() {
                                    model.setButtonDecreasePriceAlpha(0.5f);
                                    model.setButtonDecreasePriceClickable(false);
                                    model.setButtonIncreasePriceAlpha(1.0f);
                                    model.setButtonIncreasePriceClickable(true);
                                }
                            });
                        }
                        else {
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.post(new Runnable() {
                                @Override
                                public void run() {
                                    model.setButtonIncreasePriceAlpha(1.0f);
                                    model.setButtonIncreasePriceClickable(true);
                                }
                            });
                        }
                    }
                }
            });
            viewPager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
                @Override
                public void onPageSelected(int position) {
                    super.onPageSelected(position);
                }

                @Override
                public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                    updateIndicators();
                    super.onPageScrolled(position, positionOffset, positionOffsetPixels);
                }

                @Override
                public void onPageScrollStateChanged(int state) {
                    updateIndicators();
                    super.onPageScrollStateChanged(state);
                }
            });
        }
    });

    private void updateIncreaseDecreaseButtons(SheetAdapter.SheetViewHolder holder, SheetModel model) {
        mainLooper.post(new Runnable() {
            @Override
            public void run() {
                holder.buttonDecreasePrice.setAlpha(model.getButtonDecreasePriceAlpha());
                holder.buttonDecreasePrice.setClickable(model.isButtonDecreasePriceClickable());
                holder.buttonIncreasePrice.setAlpha(model.getButtonIncreasePriceAlpha());
                holder.buttonIncreasePrice.setClickable(model.isButtonIncreasePriceClickable());
            }
        });
    }

    private void updateAcceptButtonText(SheetAdapter.SheetViewHolder holder, int rideRequestPopupDelayDuration, int startTime, String text) {
        if(rideRequestPopupDelayDuration > 0 && (time - startTime) < rideRequestPopupDelayDuration) {
            holder.reqButton.setText(text+" (" + (rideRequestPopupDelayDuration- (time - startTime)) +" )");
            holder.reqButton.setAlpha(0.5f);
            holder.reqButton.setClickable(false);    
            holder.rejectButton.setAlpha(0.5f);
            holder.rejectButton.setClickable(false);
        }else {
            holder.reqButton.setText(text);
            holder.reqButton.setAlpha(1.0f);
            holder.reqButton.setClickable(true);
            holder.rejectButton.setAlpha(1.0f);
            holder.rejectButton.setClickable(true);
        }      
    }

    private void removeCard (int position) {
        removeCard(position, false);
    }

    private void removeCard (int position, boolean isRemoved) {
        try{
            Handler handler = new Handler(Looper.getMainLooper());
            handler.post(new Runnable() {
                @Override
                public void run() {
                    if (!isRemoved) {
                        if (!(sheetArrayList.size() > position)) {
                            return;
                        }
                        if (position>=0 && position<sheetArrayList.size()){
                            sheetArrayList.remove(position);
                        }
                    }
                    sheetAdapter.updateSheetList(sheetArrayList);
                    sheetAdapter.notifyItemRemoved(position);
                    sheetAdapter.notifyItemRangeChanged(position, sheetArrayList.size());
                    updateIndicators();
                    updateProgressBars(false);
                    if(sheetArrayList.isEmpty()) {
                        cleanUp();
                    }
                }
            });
        }catch (Exception e){
            firebaseLogEventWithParams("exception","remove_card",e.toString());
            e.printStackTrace();
        }
    }

    private void cleanUp () {
        if(!isRideAcceptedOrRejected){
            firebaseLogEvent("ride_ignored");
        }
        try {
            countDownTimer.cancel();
            sheetAdapter.updateSheetList(new ArrayList<>());
            viewPager = null;
            if (rideStatusListener!=null){
                rideStatusListener.cancel();
                rideStatusListener = null;
            }

            if (floatyView !=null){
                if (floatyView.getParent()!=null){
                    windowManager.removeView(floatyView);
                }
            }

            if (progressDialog !=null){
                if (progressDialog.getParent()!=null){
                    windowManager.removeView(progressDialog);
                }
            }

            if (apiLoader !=null && apiLoader.getParent()!=null){
                windowManager.removeView(apiLoader);
            }
            floatyView = null;
            progressDialog = null;
            apiLoader = null;

            if (mediaPlayer != null) {
                mediaPlayer.pause();
            }
            time = 0;
            sheetArrayList.clear();
            NotificationUtils.binder = null;
            NotificationUtils.listData = new ArrayList<>();
            this.stopSelf();
        } catch(Exception e){
            firebaseLogEventWithParams("exception","clean_up",e.toString());
            Log.e("EXCEPTION", e.toString());
        }
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        try {
            if (mediaPlayer==null){
                mediaPlayer = MediaPlayer.create(getApplicationContext(), R.raw.allocation_request);
                mediaPlayer.setLooping(true);
                mediaPlayer.setOnPreparedListener(new MediaPlayer.OnPreparedListener() {
                    @Override
                    public void onPrepared(MediaPlayer mp) {
                        mediaPlayer.setWakeMode(getApplicationContext(), PowerManager.PARTIAL_WAKE_LOCK);
                    }
                });
                }
        } catch (Exception e) {
            firebaseLogEventWithParams("exception","on_bind",e.toString());
            e.printStackTrace();
        }
        return new OverlayBinder();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }


    public void addToList (Bundle rideRequestBundle) {
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
            try {
                if (sheetArrayList.size() >= 3 || findCardById(rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQUEST_ID)))) return;
                if (progressDialog==null || apiLoader == null ){
                    if (mediaPlayer!=null){
                        mediaPlayer.start();
                        increaseVolume();
                    }
                }
                handler.post(new Runnable() {
                    @Override
                    public void run() {
                        String searchRequestId  = rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQUEST_ID));
                        String searchRequestValidTill  = rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQ_VALID_TILL));
                        int baseFare  = rideRequestBundle.getInt(getResources().getString(R.string.BASE_FARE));
                        float distanceToPickup  = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_PICKUP));
                        float distanceTobeCovered = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_BE_COVERED));
                        String durationToPickup  = rideRequestBundle.getString(getResources().getString(R.string.DURATION_TO_PICKUP));
                        String addressPickUp  = rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_PICKUP));
                        String addressDrop  = rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_DROP));
                        String sourceArea = rideRequestBundle.getString("sourceArea");
                        String destinationArea = rideRequestBundle.getString("destinationArea");
                        int driverMaxExtraFee = rideRequestBundle.getInt("driverMaxExtraFee");
                        int driverMinExtraFee = rideRequestBundle.getInt("driverMinExtraFee");
                        int rideRequestPopupDelayDuration = rideRequestBundle.getInt("rideRequestPopupDelayDuration");
                        DecimalFormat df = new DecimalFormat();
                        df.setMaximumFractionDigits(2);

                        final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                        f.setTimeZone(TimeZone.getTimeZone("UTC"));
                        String getCurrTime = f.format(new Date());
                        int calculatedTime = calculateExpireTimer(searchRequestValidTill,getCurrTime);
                        if (sharedPref == null) sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                        int negotiationUnit = Integer.parseInt(sharedPref.getString("NEGOTIATION_UNIT", "10"));
                        int rideRequestedBuffer =  Integer.parseInt(sharedPref.getString("RIDE_REQUEST_BUFFER", "2"));
                        if (calculatedTime > rideRequestedBuffer){
                            calculatedTime -= rideRequestedBuffer;
                        }
                        SheetModel sheetModel = new SheetModel((df.format(distanceToPickup/1000)),
                                (df.format(distanceTobeCovered/1000)),
                                addressPickUp,
                                addressDrop,
                                baseFare,
                                calculatedTime,
                                searchRequestId,
                                destinationArea,
                                sourceArea,
                                time,
                                driverMinExtraFee,
                                driverMaxExtraFee,
                                rideRequestPopupDelayDuration,
                                negotiationUnit);
                        if (floatyView == null) {
                            startTimer();
                            showOverLayPopup();
                        }
                        sheetArrayList.add(sheetModel);
                        sheetAdapter.updateSheetList(sheetArrayList);
                        sheetAdapter.notifyItemInserted(sheetArrayList.indexOf(sheetModel));
                        updateIndicators();
                        updateProgressBars(false);
                    }
                });
            } catch (Exception e) {
                firebaseLogEventWithParams("exception","add_to_list",e.toString());
                e.printStackTrace();
            }
        });
    }

    public void removeCardById(String id){
        if (sheetArrayList!=null){
            if (sheetArrayList.size()>0){
                if ((sheetArrayList.size()==1  && rideStatusListener!=null)){
                    return;
                }
                for (int i = 0; i<sheetArrayList.size(); i++){
                    if (id.equals(sheetArrayList.get(i).getSearchRequestId())){
                        removeCard(i);
                        break;
                    }
                }
            }
        }
    }

    private void showOverLayPopup() {
        firebaseLogEvent("Overlay_is_popped_up");
        windowManager = (WindowManager) getSystemService(Context.WINDOW_SERVICE);
        int layoutParamsType;
        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
            layoutParamsType = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
        } else {
            layoutParamsType = WindowManager.LayoutParams.TYPE_PHONE;
        }

        params = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.MATCH_PARENT,
                WindowManager.LayoutParams.MATCH_PARENT,
                layoutParamsType,
                WindowManager.LayoutParams.FLAG_TURN_SCREEN_ON | WindowManager.LayoutParams.FLAG_DIM_BEHIND,
                PixelFormat.TRANSPARENT);
        params.dimAmount = 0.6f;
        params.gravity = Gravity.CENTER;
        params.screenOrientation = ActivityInfo.SCREEN_ORIENTATION_PORTRAIT;
        LayoutInflater inflater = ((LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE));
        floatyView = inflater.inflate(R.layout.viewpager_layout_view,null);
        progressDialog = inflater.inflate(R.layout.loading_screen_overlay, null);
        apiLoader = inflater.inflate(R.layout.api_loader, null);
        View dismissLoader = progressDialog.findViewById(R.id.loaderOverlay);
        dismissLoader.setOnClickListener(new View.OnClickListener() {
           @Override
           public void onClick(View view) {
               Handler handler = new Handler(Looper.getMainLooper());
               handler.post(new Runnable() {
                   @Override
                   public void run() {
                       cleanUp();
                   }
               });
           }
        });
        viewPager = floatyView.findViewById(R.id.view_pager);
        sheetAdapter.setViewPager(viewPager);
        viewPager.setAdapter(sheetAdapter);
        windowManager.addView(floatyView, params);
        setIndicatorClickListener();
    }

    private void startTimer() {
        TimerTask countUpTimerTask = new TimerTask() {
            @Override
            public void run() {
                time ++;
                new Handler(Looper.getMainLooper()).post(new Runnable() {
                    @Override
                    public void run() {
                        for (SheetModel model : sheetArrayList) {
                            int index = sheetArrayList.indexOf(model);
                            if(model.getReqExpiryTime() + model.getStartTime() - time < 1) {
                                removeCard(index, false);
                            } else {
                                sheetAdapter.notifyItemChanged(index, "time");
                            }
                        }
                    }
                });
            }
        };
        countDownTimer = new Timer();
        countDownTimer.scheduleAtFixedRate(countUpTimerTask, 1000, 1000);
    }

    public void firebaseLogEvent(String event) {
        Bundle params = new Bundle();
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        mFirebaseAnalytics.logEvent(event, params);
    }

    public void firebaseLogEventWithParams(String event,String paramKey,String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey,paramValue);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        mFirebaseAnalytics.logEvent(event, params);
    }

    private Boolean driverRespondApi(String searchRequestId, int offeredPrice, boolean isAccept, int slotNumber) {
        StringBuilder result = new StringBuilder();
        Handler handler = new Handler(Looper.getMainLooper());
        sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        regToken = sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null");
        baseUrl = sharedPref.getString("BASE_URL", "null");
        String bundle_version = sharedPref.getString("BUNDLE_VERSION", "null");
        String version = sharedPref.getString("VERSION_NAME", "null");
        try {
            String orderUrl = baseUrl + "/driver/searchRequest/quote/respond";
            HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("x-client-version", version);
            connection.setRequestProperty("token", regToken);
            connection.setRequestProperty("x-bundle-version", bundle_version);
            connection.setDoOutput(true);
            connection.setConnectTimeout(20000);
            connection.setReadTimeout(20000);
            JSONObject payload = new JSONObject();
            if (!isAccept || offeredPrice == 0) {
                payload.put(getResources().getString(R.string.OFFERED_FARE), null);
            } else {
                payload.put(getResources().getString(R.string.OFFERED_FARE), offeredPrice);
            }
            payload.put(getResources().getString(R.string.SEARCH_REQUEST_ID), searchRequestId);
            if (isAccept) payload.put("response", "Accept");
            else payload.put("response", "Reject");
            payload.put("slotNumber", slotNumber);
            OutputStream stream = connection.getOutputStream();
            stream.write(payload.toString().getBytes());
            connection.connect();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                JSONObject errorPayload = new JSONObject(result.toString());
                if (errorPayload.has(getResources().getString(R.string.ERROR_MESSAGE))){
                    handler.post(new Runnable() {
                        @Override
                        public void run() {
                            try {
                                Toast.makeText(getApplicationContext(), errorPayload.getString(getResources().getString(R.string.ERROR_MESSAGE)) , Toast.LENGTH_SHORT).show();
                            } catch (JSONException e) {
                                e.printStackTrace();
                            }
                        }
                    });
                }
            } else {
                //API Success
                return  true;
            }
            return false;
        } catch (SocketTimeoutException e){
            handler.post(new Runnable() {
                @Override
                public void run() {
                    Toast.makeText(getApplicationContext(), "Request Timeout" , Toast.LENGTH_SHORT).show();
                }
            });
            return false;
        } catch (Exception e) {
            firebaseLogEventWithParams("exception" , "driver_respond_api", e.toString());
            return false;
        }
    }

    @Override
    public void onDestroy() {
        if (floatyView != null) {
            try {
                windowManager.removeView(floatyView);
                mediaPlayer.release();
                mediaPlayer = null;
            } catch (Exception e) {
                e.printStackTrace();
            }
            floatyView = null;
        }
        super.onDestroy();
    }
    @Override
    public boolean onTouch(View view, MotionEvent motionEvent) {
        view.performClick();
        return true;
    }
    private int calculateExpireTimer(String expireTimeTemp, String currTimeTemp){
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
        if ((expireTime-currTime) >= 5)
        {
            return expireTime-currTime - 5 ;
        }
        return 0;
    }

    private void startLoader(String id) {
        countDownTimer.cancel();
        try {
            if (mediaPlayer != null) {
                if (mediaPlayer.isPlaying()){
                    mediaPlayer.pause();
                }
            }
            if (floatyView != null) {
                if (floatyView.getParent()!=null){
                    windowManager.removeView(floatyView);
                }
            }
            if(apiLoader!= null) {
                if (apiLoader.getParent()!=null){
                    windowManager.removeView(apiLoader);
                }
            }
            if (progressDialog !=null){
                windowManager.addView(progressDialog, params);
            }
            rideStatusListener = new CountDownTimer(getResources().getInteger(R.integer.LOADER_WAITING_TIME), 1000) {
                @Override
                public void onTick(long millisUntilFinished) {
                    sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    if (progressDialog!=null){
                        TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
                        loaderText.setText(getString(R.string.waiting_for_customer_response)+ " (" + String.valueOf(millisUntilFinished/1000) + ") ...");
                    }
                    if (sharedPref.getString(getResources().getString(R.string.RIDE_STATUS), "null").equals(getResources().getString(R.string.DRIVER_ASSIGNMENT))) {
                        sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                        showAcknowledgement(getString(R.string.DRIVER_ASSIGNMENT));
                    }
                    else if (sharedPref.getString(getString(R.string.CLEAR_FARE), "null").equals(id)){
                        showAcknowledgement(getString(R.string.CLEAR_FARE));
                    }
                }
                @Override
                public void onFinish() {
                    sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                    cleanUp();
                }
            }.start();
        } catch (Exception e) {
            firebaseLogEventWithParams("exception","start_loader",e.toString());
            cleanUp();
            e.printStackTrace();
        }
    }

    private void showAcknowledgement(String ackType) {
        Handler handler = new Handler(Looper.getMainLooper());
        String ackText = ackType.equals(getString(R.string.DRIVER_ASSIGNMENT)) ? getString(R.string.ride_assigned) : getString(R.string.ride_assigned_to_another_driver);
        int rawResource = ackType.equals(getString(R.string.DRIVER_ASSIGNMENT)) ? R.raw.ride_accepted_lottie : R.raw.accepted_by_another_driver_lottie;
        handler.post(new Runnable() {
            @Override
            public void run() {
                if (progressDialog!=null){
                    TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
                    LottieAnimationView lottieAnimationView = progressDialog.findViewById(R.id.lottie_view_waiting);
                    loaderText.setText(ackText);
                    lottieAnimationView.setAnimation(rawResource);
                    lottieAnimationView.setProgress(0);
                    lottieAnimationView.setSpeed(1.2f);
                    lottieAnimationView.playAnimation();
                    rideStatusListener.cancel();
                }
            }
        });
        handler.postDelayed(new Runnable() {
            @Override
            public void run() {
                cleanUp();
            }
        }, 1700);
    }

    private void startApiLoader() {
        try {
            if (mediaPlayer != null) {
                if (mediaPlayer.isPlaying()){
                    mediaPlayer.pause();
                }
            }
            if (apiLoader !=null){
                windowManager.addView(apiLoader, params);
            }
        } catch (Exception e) {
            firebaseLogEventWithParams("exception","start_api_loader",e.toString());
            e.printStackTrace();
        }
    }
    private void increaseVolume() {
        audio = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
        currentVolume = audio.getStreamVolume(AudioManager.STREAM_MUSIC);
        int maxVolume = audio.getStreamMaxVolume(AudioManager.STREAM_MUSIC);
        if (currentVolume / maxVolume < 0.7) {
            audio.setStreamVolume(AudioManager.STREAM_MUSIC,(int) (maxVolume * 0.9), AudioManager.ADJUST_SAME);
        }
    }

    private void updateIndicators(){
        mainLooper.post(new Runnable() {
            @Override
            public void run() {
                if (floatyView == null || viewPager == null || sheetArrayList == null) return;
                indicatorText1 = floatyView.findViewById(R.id.indicatorText1);
                indicatorText2 = floatyView.findViewById(R.id.indicatorText2);
                indicatorText3 = floatyView.findViewById(R.id.indicatorText3);
                progressIndicator1 = floatyView.findViewById(R.id.progress_indicator_1);
                progressIndicator2 = floatyView.findViewById(R.id.progress_indicator_2);
                progressIndicator3 = floatyView.findViewById(R.id.progress_indicator_3);
                indicatorTextList = new ArrayList<>(Arrays.asList(indicatorText1,indicatorText2,indicatorText3));
                progressIndicatorsList = new ArrayList<>(Arrays.asList(progressIndicator1,progressIndicator2,progressIndicator3));
                indicatorList = new ArrayList<>(Arrays.asList(
                        floatyView.findViewById(R.id.indicator1),
                        floatyView.findViewById(R.id.indicator2),
                        floatyView.findViewById(R.id.indicator3)));

                for (int  i =0; i<3; i++){
                    if (viewPager.getCurrentItem() == indicatorList.indexOf(indicatorList.get(i))){
                        indicatorList.get(i).setBackgroundColor(getColor(R.color.grey900));
                        progressIndicatorsList.get(i).setTrackColor(getColor(R.color.white));
                    }else {
                        indicatorList.get(i).setBackgroundColor(getColor(R.color.white));
                        progressIndicatorsList.get(i).setTrackColor(getColor(R.color.grey900));
                    }
                    if (i < sheetArrayList.size()){
                        indicatorTextList.get(i).setText("₹"+String.valueOf(sheetArrayList.get(i).getBaseFare() + sheetArrayList.get(i).getUpdatedAmount()));
                        progressIndicatorsList.get(i).setVisibility(View.VISIBLE);
                    } else {
                        indicatorTextList.get(i).setText("--");
                        progressIndicatorsList.get(i).setVisibility(View.GONE);
                    }
                }
            }
        });
    }

    private void setIndicatorClickListener() {
        if (viewPager == null || floatyView == null) return;
        indicatorList = new ArrayList<>(Arrays.asList(
                floatyView.findViewById(R.id.indicator1),
                floatyView.findViewById(R.id.indicator2),
                floatyView.findViewById(R.id.indicator3)));

        for (int i =0; i<3; i++){
            int finalI = i;
            indicatorList.get(i).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    mainLooper.post(new Runnable() {
                        @Override
                        public void run() {
                            if (viewPager == null) return;
                            viewPager.setCurrentItem(finalI);
                            if(!(finalI >= sheetArrayList.size() || finalI < 0)){ //index exists
                                firebaseLogEventWithParams("indicator_click", "index" , String.valueOf(finalI));
                            }
                        }
                    });
                }
            });
        }
    }

    private void updateProgressBars(boolean animated){
        if (floatyView == null || sheetArrayList == null ) return;
        progressIndicatorsList = new ArrayList<>(Arrays.asList(
                floatyView.findViewById(R.id.progress_indicator_1),
                floatyView.findViewById(R.id.progress_indicator_2),
                floatyView.findViewById(R.id.progress_indicator_3)));

        for (int i=0; i<sheetArrayList.size(); i++){
            progressCompat = sheetArrayList.get(i).getReqExpiryTime()  + sheetArrayList.get(i).getStartTime() - time;
            progressIndicatorsList.get(i).setProgressCompat(progressCompat*4, animated); // (100/maxExpiryTime)
            if (progressCompat <= 8){
                progressIndicatorsList.get(i).setIndicatorColor(getColor(R.color.red900));
            }else {
                progressIndicatorsList.get(i).setIndicatorColor(getColor(R.color.green900));
            }
        }
    }

    private boolean findCardById(String id){
        if (sheetArrayList != null){
            for (int i = 0; i<sheetArrayList.size(); i++){
                if (id.equals(sheetArrayList.get(i).getSearchRequestId())){
                    return true;
                }
            }
        }
        return false;
    }
    private void updateSharedPreferences() {
        Context context = getApplicationContext();
        SharedPreferences sharedPreferences = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        sharedPreferences.edit().putString(getString(R.string.LOCAL_STAGE),getString(R.string.RideRequested));
        SimpleDateFormat formatter = new SimpleDateFormat("EE MMM d y H:m:s ZZZ");
        String dateString = formatter.format(new Date());
        sharedPref.edit().putString(getString(R.string.RIDE_REQUEST_TIME), dateString).apply();
    }
}
