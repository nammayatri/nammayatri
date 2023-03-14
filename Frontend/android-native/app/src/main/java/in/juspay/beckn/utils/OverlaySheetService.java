package in.juspay.beckn.utils;

import android.animation.ValueAnimator;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Color;
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
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;
import android.view.animation.LinearInterpolator;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.viewpager2.widget.ViewPager2;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.beckn.R;

public class OverlaySheetService extends Service implements View.OnTouchListener {

    private static final String TAG = OverlaySheetService.class.getSimpleName();
    private ViewPager2 viewPager;
    private ArrayList<SheetModel> sheetArrayList = new ArrayList<>();
    private Timer countDownTimer;
    private ValueAnimator loaderAnimation;
    private View floatyView;
    private WindowManager windowManager;
    private String regToken, baseUrl;
    private double estimatedFare = 0;
    private SharedPreferences sharedPref;
    private MediaPlayer mediaPlayer;
    private int currentVolume;
    private ArrayList<View> loaders = new ArrayList();
    private int time = 0;
    private AudioManager audio;
    private View progressDialog;
    private CountDownTimer rideStatusListener;
    private WindowManager.LayoutParams params;


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
                    holder.basePrice.setText(model.getUpdatedAmount().toString());
                    return;
                case "time" :
                    holder.acceptRejTimer.setText("" + (model.getReqExpiryTime() + model.getStartTime() - time));
                    return;
            }

            if(!loaders.contains(holder.progressBar)) {
                loaders.add(holder.progressBar);
            }
            holder.pickUpDistance.setText(model.getPickUpDistance()+" km");
            holder.basePrice.setText(model.getBasePrice().toString());
            holder.distanceToBeCovered.setText(model.getDistanceToBeCovered()+" km");
            holder.sourceAddress.setText(model.getSourceAddress());
            holder.destinationArea.setText(model.getDestinationArea());
            holder.destinationAddress.setText(model.getDestinationAddress());
            holder.acceptRejTimer.setText("" + (model.getReqExpiryTime() + model.getStartTime() - time));
            holder.reqButton.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    Handler handler = new Handler(Looper.getMainLooper());
                    executor.execute(() -> {
                        try {
                            Boolean isApiSuccess = requestRideApi(model.getSearchRequestId(), model.getOfferedPrice());
                            if (isApiSuccess){
                                holder.reqButton.setClickable(false);
                                handler.post(() -> {
                                    startLoader();
                                    executor.shutdown();
                                });
                            }else{
                                handler.post(() -> {
                                    cleanUp();
                                    executor.shutdown();
                                });
                            }
                        } catch (Exception e) {
                            cleanUp();
                        }
                    });
                }
            });
            holder.rejButton.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Handler handler = new Handler(Looper.getMainLooper());
                    handler.post(new Runnable() {
                        @Override
                        public void run() {
                            System.out.println("reject button clicked");
                            holder.rejButton.setClickable(false);
                            removeCard(position);
                            Toast.makeText(getApplicationContext(), "Ride Rejected", Toast.LENGTH_SHORT).show();
                        }
                    });
                }
            });
            holder.buttonIncreasePrice.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    if (model.getOfferedPrice() <= 2) {
                        model.setUpdatedAmount(model.getUpdatedAmount() + 10.0f);
                        model.setOfferedPrice(model.getOfferedPrice()+1);
                        sheetAdapter.notifyItemChanged(position, "inc");
                        if (model.getOfferedPrice() == 3){
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.post(new Runnable() {
                                @Override
                                public void run() {
                                    holder.buttonIncreasePrice.setAlpha(0.5f);
                                    holder.buttonIncreasePrice.setClickable(false);
                                    holder.buttonDecreasePrice.setAlpha(1.0f);
                                    holder.buttonDecreasePrice.setClickable(true);
                                }
                            });
                        }
                        else {
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.post(new Runnable() {
                                @Override
                                public void run() {
                                    holder.buttonDecreasePrice.setAlpha(1.0f);
                                    holder.buttonDecreasePrice.setClickable(true);
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
                        model.setUpdatedAmount(model.getUpdatedAmount() - 10.0f);
                        model.setOfferedPrice(model.getOfferedPrice()-1);
                        sheetAdapter.notifyItemChanged(position,"inc");
                        if (model.getOfferedPrice() == 0){
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.post(new Runnable() {
                                @Override
                                public void run() {
                                    holder.buttonDecreasePrice.setAlpha(0.5f);
                                    holder.buttonDecreasePrice.setClickable(false);
                                    holder.buttonIncreasePrice.setAlpha(1.0f);
                                    holder.buttonIncreasePrice.setClickable(true);
                                }
                            });
                        }
                        else {
                            Handler handler = new Handler(Looper.getMainLooper());
                            handler.post(new Runnable() {
                                @Override
                                public void run() {
                                    holder.buttonIncreasePrice.setAlpha(1.0f);
                                    holder.buttonIncreasePrice.setClickable(true);
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
                    toggleSwipeImage(position);
                }
            });
        }
    });

    private void removeCard (int position) {
        removeCard(position, false);
    }

    private void removeCard (int position, boolean isRemoved) {
        try{
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
            if(sheetArrayList.isEmpty()) {
                cleanUp();
            }
        }catch (Exception e){
            e.printStackTrace();
        }
    }

    private void cleanUp () {
        try {
            countDownTimer.cancel();
            loaderAnimation.cancel();
            loaders = new ArrayList<>();
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

            floatyView = null;
            progressDialog = null;

            if (mediaPlayer != null) {
                mediaPlayer.pause();
            }
            time = 0;
            sheetArrayList.clear();
            NotificationUtils.binder = null;
            NotificationUtils.listData = new ArrayList<>();
        }catch(Exception e){
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
            audio = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
            currentVolume = audio.getStreamVolume(AudioManager.STREAM_MUSIC);
            int maxVolume = audio.getStreamMaxVolume(AudioManager.STREAM_MUSIC);
            if (currentVolume / maxVolume < 0.7) {
                audio.setStreamVolume(AudioManager.STREAM_MUSIC, (int) (maxVolume * 0.9), AudioManager.ADJUST_SAME);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return new OverlayBinder();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {

        return START_STICKY;
    }

    public void objAnimator () {
        loaderAnimation = ValueAnimator.ofInt(0, 360);
        loaderAnimation.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
            @Override
            public void onAnimationUpdate(ValueAnimator animation) {
                // Get a list of views and rotate
                int a = (int) animation.getAnimatedValue();
                for (Iterator<View> it = loaders.iterator(); it.hasNext(); ) {
                    View view = it.next();
                    view.setRotation(a);
                }
            }
        });
        loaderAnimation.setRepeatMode(ValueAnimator.RESTART);
        loaderAnimation.setRepeatCount(ValueAnimator.INFINITE);
        loaderAnimation.setDuration(4000);
        loaderAnimation.setInterpolator(new LinearInterpolator());
        loaderAnimation.start();
    }

    public void addToList (Bundle rideRequestBundle) {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Handler handler = new Handler(Looper.getMainLooper());
        executor.execute(() -> {
            try {
                if (progressDialog==null || progressDialog.getParent()==null){
                    if (mediaPlayer!=null){
                        mediaPlayer.seekTo(0);
                        mediaPlayer.start();
                    }
                }
                handler.post(new Runnable() {
                    @Override
                    public void run() {
                        String searchRequestId  = rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQUEST_ID));
                        String searchRequestValidTill  = rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQ_VALID_TILL));
                        double baseFare  = rideRequestBundle.getDouble(getResources().getString(R.string.BASE_FARE));
                        float roundOffBaseFare = (float) (Math.round(baseFare * 100.0) / 100.0);
                        float distanceToPickup  = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_PICKUP));
                        float distanceTobeCovered = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_BE_COVERED));
                        String durationToPickup  = rideRequestBundle.getString(getResources().getString(R.string.DURATION_TO_PICKUP));
                        String addressPickUp  = rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_PICKUP));
                        String addressDrop  = rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_DROP));
                        String destinationArea = rideRequestBundle.getString("destinationArea");
                        DecimalFormat df = new DecimalFormat();
                        df.setMaximumFractionDigits(2);

                        final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                        f.setTimeZone(TimeZone.getTimeZone("UTC"));
                        String getCurrTime = f.format(new Date());
                        int calculatedTime = calculateExpireTimer(searchRequestValidTill,getCurrTime);
                        if (calculatedTime >0){
                            if (calculatedTime >25){
                                calculatedTime = 25;
                            }
                        }
                        SheetModel sheetModel = new SheetModel((df.format(distanceToPickup/1000)), (df.format(distanceTobeCovered/1000)), addressPickUp, addressDrop, roundOffBaseFare, calculatedTime,searchRequestId,0, destinationArea);
                        sheetModel.setStartTime(time);
                        if (floatyView == null) {
                            startTimer();
                            showOverLayPopup();
                        }
                        sheetArrayList.add(sheetModel);
                        sheetAdapter.updateSheetList(sheetArrayList);
                        sheetAdapter.notifyItemInserted(sheetArrayList.indexOf(sheetModel));
                        toggleSwipeImage();
                    }
                });
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    private void showOverLayPopup() {
        windowManager = (WindowManager) getSystemService(Context.WINDOW_SERVICE);
        int layoutParamsType;
        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
            layoutParamsType = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
        } else {
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

        ConstraintLayout interceptorLayout = new ConstraintLayout(this) {
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
        floatyView = inflater.inflate(R.layout.viewpager_layout_view, interceptorLayout, false);
        progressDialog = inflater.inflate(R.layout.loading_screen_overlay, interceptorLayout, false);
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
    }

    private void startTimer() {
        objAnimator();
        TimerTask countUpTimerTask = new TimerTask() {
            @Override
            public void run() {
                time ++;
                new Handler(Looper.getMainLooper()).post(new Runnable() {
                    @Override
                    public void run() {
                        for (Iterator<SheetModel> it = sheetArrayList.iterator(); it.hasNext(); ) {
                            SheetModel model = it.next();
                            int index = sheetArrayList.indexOf(model);
                            if(model.getReqExpiryTime() + model.getStartTime() - time <= 0) {
                                it.remove();
                                removeCard(index, true);
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

    private Boolean requestRideApi(String searchRequestId, int offeredPrice) {
        StringBuilder result = new StringBuilder();
        Handler handler = new Handler(Looper.getMainLooper());
        sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        regToken = sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null");
        baseUrl = sharedPref.getString("BASE_URL", "null");
        String version = sharedPref.getString("VERSION_NAME", "null");
        try {
            String orderUrl = baseUrl + "/driver/searchRequest/quote/offer";
            HttpURLConnection connection = (HttpURLConnection) (new URL(orderUrl).openConnection());
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("x-client-version", version);
            connection.setRequestProperty("token", regToken);
            connection.setDoOutput(true);
            JSONObject payload = new JSONObject();
            if (offeredPrice == 0) {
                payload.put(getResources().getString(R.string.OFFERED_FARE), null);
            } else {
                payload.put(getResources().getString(R.string.OFFERED_FARE), (offeredPrice * 10));
            }
            payload.put(getResources().getString(R.string.SEARCH_REQUEST_ID), searchRequestId);
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
                                Toast.makeText(getApplicationContext(), errorPayload.getString(getResources().getString(R.string.ERROR_MESSAGE)) , Toast.LENGTH_LONG).show();
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
        } catch (Exception e) {
            handler.post(new Runnable() {
                @Override
                public void run() {
                    Toast.makeText(getApplicationContext(), getResources().getString(R.string.ERROR_OCCURRED) , Toast.LENGTH_SHORT).show();
                }
            });
            return false;
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
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
    private void toggleSwipeImage (int position){
        Handler handler = new Handler(Looper.getMainLooper());
        handler.post(new Runnable() {
            @Override
            public void run() {
                if(floatyView != null){
                    ImageView swipeIndicationImgLeft = floatyView.findViewById(R.id.swipe_left);
                    ImageView swipeIndicationImgRight = floatyView.findViewById(R.id.swipe_right);
                    if(sheetArrayList.size() > 1){
                        if(position == 0){
                            swipeIndicationImgRight.setVisibility(View.GONE);
                            swipeIndicationImgLeft.setVisibility(View.VISIBLE);
                        }else if(position+1 <= sheetArrayList.size()-1 && position-1 >= 0){
                            swipeIndicationImgRight.setVisibility(View.VISIBLE);
                            swipeIndicationImgLeft.setVisibility(View.VISIBLE);
                        }else if( position == sheetArrayList.size()-1 ){
                            swipeIndicationImgRight.setVisibility(View.VISIBLE);
                            swipeIndicationImgLeft.setVisibility(View.GONE);
                        }
                    }else{
                        swipeIndicationImgRight.setVisibility(View.GONE);
                        swipeIndicationImgLeft.setVisibility(View.GONE);
                    }

                }
            }
        });

    }
     private void toggleSwipeImage(){
         Handler handler = new Handler(Looper.getMainLooper());
         handler.post(new Runnable() {
             @Override
             public void run() {
                 if(floatyView != null){
                     ImageView swipeIndicationImg = floatyView.findViewById(R.id.swipe_left);
                     if (sheetArrayList.size() > 1) {
                         swipeIndicationImg.setVisibility(View.VISIBLE);
                     }
                 }
             }
         });
     }

    private void startLoader() {
        countDownTimer.cancel();
        sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        try {
            if (floatyView != null) {
                windowManager.removeView(floatyView);
            }
            if (progressDialog !=null){
                windowManager.addView(progressDialog, params);
            }
            if (mediaPlayer != null) {
                mediaPlayer.pause();
            }
            Handler handler = new Handler(Looper.getMainLooper());
            rideStatusListener = new CountDownTimer(getResources().getInteger(R.integer.LOADER_WAITING_TIME), 1000) {
                @Override
                public void onTick(long millisUntilFinished) {
                    if (sharedPref.getString(getResources().getString(R.string.RIDE_STATUS), "null").equals(getResources().getString(R.string.DRIVER_ASSIGNMENT))) {
                        handler.post(new Runnable() {
                            @Override
                            public void run() {
                                if (progressDialog!=null){
                                    TextView loaderText = progressDialog.findViewById(R.id.progress_bar_text);
                                    ProgressBar progressBarLoader = progressDialog.findViewById(R.id.loader_progressbar);
                                    loaderText.setTextColor(Color.parseColor("#53BB64"));
                                    loaderText.setText("Ride Assigned");
                                    loaderText.setTextSize(20);
                                    progressBarLoader.setVisibility(View.GONE);
                                    rideStatusListener.cancel();
                                    sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                                }
                            }
                        });
                        handler.postDelayed(new Runnable() {
                            @Override
                            public void run() {
                                cleanUp();
                            }
                        }, 1000);
                    }
                }
                @Override
                public void onFinish() {
                    sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                    cleanUp();
                }
            }.start();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

