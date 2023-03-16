/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;
import android.animation.ValueAnimator;
import android.content.Context;
import android.content.SharedPreferences;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.os.Bundle;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.Looper;
import android.os.PowerManager;
import android.view.View;
import android.view.animation.LinearInterpolator;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;
import com.airbnb.lottie.LottieAnimationView;
import com.google.android.material.progressindicator.LinearProgressIndicator;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;

public class RideRequestActivity extends AppCompatActivity {
    private int time = 0, progressCompat = 0;
    private ArrayList<SheetModel> sheetArrayList = new ArrayList<>();
    private ViewPager2 viewPager2;
    private Timer countDownTimer;
    private CountDownTimer rideStatusListener;
    private final Handler mainLooper = new Handler( Looper.getMainLooper());
    private static RideRequestActivity instance;
    private TextView indicatorText1, indicatorText2, indicatorText3;
    private LinearProgressIndicator progressIndicator1, progressIndicator2, progressIndicator3;
    private ArrayList<TextView> indicatorTextList ;
    private ArrayList<LinearProgressIndicator> progressIndicatorsList ;
    private ArrayList<LinearLayout> indicatorList ;
    private  RideRequestUtils rideRequestUtils = new RideRequestUtils();

    private final SheetAdapter sheetAdapter = new SheetAdapter(sheetArrayList, viewPager2, new SheetAdapter.OnItemClickListener() {
        @Override
        public void onViewHolderBind(SheetAdapter.SheetViewHolder holder, int position, ViewPager2 viewPager, List<Object> payloads) {
            SheetModel model = sheetArrayList.get(position);
            String x = payloads.size() > 0 ? (String) payloads.get(0) :"";
            switch (x) {
                case "inc" :
                    updateIndicators();
                    holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
                    return;
                case "time" :
                    updateProgressBars(true);
                    return;
            }

            holder.pickUpDistance.setText(model.getPickUpDistance()+" km " + getString(R.string.away));
            holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
            holder.distanceToBeCovered.setText(model.getDistanceToBeCovered()+" km");
            holder.sourceArea.setText(model.getSourceArea());
            holder.sourceAddress.setText(model.getSourceAddress());
            holder.destinationArea.setText(model.getDestinationArea());
            holder.destinationAddress.setText(model.getDestinationAddress());
            holder.acceptRejTimer.setText("" + (model.getReqExpiryTime() + model.getStartTime() - time));
            holder.reqButton.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    holder.reqButton.setClickable(false);
                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    executor.execute(() -> {
                        Boolean isApiSuccess = rideRequestUtils.driverRespondApi(model.getSearchRequestId(), model.getOfferedPrice(), true, RideRequestActivity.this, sheetArrayList.indexOf(model));
                        if (isApiSuccess){
                            mainLooper.post(() -> {
                                executor.shutdown();
                            });
                            startLoader(model.getSearchRequestId());
                        }
                    });
                }
            });
            holder.rejectButton.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    ExecutorService executor = Executors.newSingleThreadExecutor();
                    executor.execute(() -> {
                        new Thread(new Runnable() {
                            @Override
                            public void run() {
                                rideRequestUtils.driverRespondApi(model.getSearchRequestId(), model.getOfferedPrice(), false, RideRequestActivity.this, sheetArrayList.indexOf(model));
                            }
                        }).start();
                        holder.rejectButton.setClickable(false);
                        mainLooper.post(() -> {
                            removeCard(position);
                            executor.shutdown();
                            Toast.makeText(getApplicationContext(), getApplicationContext().getResources().getString(R.string.ride_rejected), Toast.LENGTH_SHORT).show();
                        });
                    });
                }
            });
            holder.buttonIncreasePrice.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    if (model.getOfferedPrice() <= model.getDriverMaxExtraFee() - model.getNegotiationUnit()) {
                        model.setUpdatedAmount(model.getUpdatedAmount() + model.getNegotiationUnit());
                        model.setOfferedPrice(model.getOfferedPrice()+model.getNegotiationUnit());
                        sheetAdapter.notifyItemChanged(position, "inc");
                        if (model.getOfferedPrice() == model.getDriverMaxExtraFee()){
                            mainLooper.post(new Runnable() {
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
                            mainLooper.post(new Runnable() {
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
                        model.setUpdatedAmount(model.getUpdatedAmount() - model.getNegotiationUnit());
                        model.setOfferedPrice(model.getOfferedPrice()-model.getNegotiationUnit());
                        sheetAdapter.notifyItemChanged(position,"inc");
                        if (model.getOfferedPrice() == 0){
                            mainLooper.post(new Runnable() {
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
                            mainLooper.post(new Runnable() {
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

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        instance = this;
        setContentView(R.layout.activity_ride_request);
        viewPager2 = findViewById(R.id.viewPager);
        sheetAdapter.setViewPager(viewPager2);
        viewPager2.setAdapter(sheetAdapter);
        if (getIntent() !=null){
            addToList(getIntent().getExtras());
        }
        setIndicatorClickListener();
    }


    public void addToList(Bundle rideRequestBundle){
        mainLooper.post(new Runnable() {
            @Override
            public void run() {
                if (rideRequestBundle == null || findCardById(rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQUEST_ID)))) return;
                String searchRequestValidTill  = rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQ_VALID_TILL));
                float distanceToPickup  = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_PICKUP));
                float distanceTobeCovered = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_BE_COVERED));
                DecimalFormat df = new DecimalFormat();
                df.setMaximumFractionDigits(2);
                final SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                simpleDateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
                String getCurrTime = simpleDateFormat.format(new Date());
                int calculatedTime = rideRequestUtils.calculateExpireTimer(searchRequestValidTill,getCurrTime);
                if (sheetArrayList.isEmpty()){
                    startTimer();
                }
                SharedPreferences sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                int negotiationUnit = Integer.parseInt(sharedPref.getString("NEGOTIATION_UNIT", "10"));
                SheetModel sheetModel = new SheetModel((df.format(distanceToPickup/1000)),
                        (df.format(distanceTobeCovered/1000)),
                        rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_PICKUP)),
                        rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_DROP)),
                        rideRequestBundle.getInt(getResources().getString(R.string.BASE_FARE)),
                        Math.min(calculatedTime, 25),
                        rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQUEST_ID)),
                        rideRequestBundle.getString("destinationArea"),
                        rideRequestBundle.getString("sourceArea"),
                        time,
                        rideRequestBundle.getInt("driverMinExtraFee"),
                        rideRequestBundle.getInt("driverMaxExtraFee"),
                        rideRequestBundle.getInt("rideRequestPopupDelayDuration"),
                        negotiationUnit);

                sheetArrayList.add(sheetModel);
                sheetAdapter.updateSheetList(sheetArrayList);
                sheetAdapter.notifyItemInserted(sheetArrayList.indexOf(sheetModel));
                updateIndicators();
                updateProgressBars(false);
            }
        });
    }

    private void startTimer() {
        TimerTask countUpTimerTask = new TimerTask() {
            @Override
            public void run() {
                time ++;
                mainLooper.post(new Runnable() {
                    @Override
                    public void run() {
                        for (SheetModel model : sheetArrayList) {
                            int index = sheetArrayList.indexOf(model);
                            if(model.getReqExpiryTime() + model.getStartTime() - time < 1) {
                                removeCard(index);
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

    private void startLoader(String id){
        countDownTimer.cancel();
        cancelSound();
        View progressDialog = findViewById(R.id.progress_loader);
        View viewPagerParentView = findViewById(R.id.view_pager_parent);
        mainLooper.post(new Runnable() {
            @Override
            public void run() {
                viewPagerParentView.setVisibility(View.GONE);
                progressDialog.setVisibility(View.VISIBLE);
                rideStatusListener = new CountDownTimer(getResources().getInteger(R.integer.LOADER_WAITING_TIME), 1000) {
                    @Override
                    public void onTick(long millisUntilFinished) {
                        SharedPreferences sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
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
                        SharedPreferences sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                        sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                        finish();
                    }
                }.start();
            }
        });
    }

    private void showAcknowledgement(String ackType) {
        String ackText = ackType.equals(getString(R.string.DRIVER_ASSIGNMENT)) ? getString(R.string.ride_assigned) : getString(R.string.ride_assigned_to_another_driver);
        int rawResource = ackType.equals(getString(R.string.DRIVER_ASSIGNMENT)) ? R.raw.ride_accepted_lottie : R.raw.accepted_by_another_driver_lottie;
        mainLooper.post(new Runnable() {
            @Override
            public void run() {
                View progressDialog = findViewById(R.id.progress_loader);
                TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
                LottieAnimationView lottieAnimationView = progressDialog.findViewById(R.id.lottie_view_waiting);
                loaderText.setText(ackText);
                lottieAnimationView.setAnimation(rawResource);
                lottieAnimationView.setProgress(0);
                lottieAnimationView.setSpeed(1.2f);
                lottieAnimationView.playAnimation();
                rideStatusListener.cancel();
            }
        });
        mainLooper.postDelayed(new Runnable() {
            @Override
            public void run() {
                finish();
            }
        }, 1700);
    }

    private void removeCard (int position) {
        mainLooper.post(new Runnable() {
            @Override
            public void run() {
                if (!(sheetArrayList.size() > position)) {
                    return;
                }
                if (position>=0 && position<sheetArrayList.size()){
                    sheetArrayList.remove(position);
                }
                sheetAdapter.updateSheetList(sheetArrayList);
                sheetAdapter.notifyItemRemoved(position);
                sheetAdapter.notifyItemRangeChanged(position, sheetArrayList.size());
                updateIndicators();
                updateProgressBars(true);
                if(sheetArrayList.isEmpty()) {
                    cancelSound();
                    finish();
                }
            }
        });
    }

    private void updateIndicators(){
        mainLooper.post(new Runnable() {
            @Override
            public void run() {
                if ( viewPager2 == null || sheetArrayList == null) return;
                indicatorText1 = findViewById(R.id.indicatorText1);
                indicatorText2 = findViewById(R.id.indicatorText2);
                indicatorText3 = findViewById(R.id.indicatorText3);
                progressIndicator1 = findViewById(R.id.progress_indicator_1);
                progressIndicator2 = findViewById(R.id.progress_indicator_2);
                progressIndicator3 = findViewById(R.id.progress_indicator_3);
                indicatorTextList = new ArrayList<>(Arrays.asList(indicatorText1,indicatorText2,indicatorText3));
                progressIndicatorsList = new ArrayList<>(Arrays.asList(progressIndicator1,progressIndicator2,progressIndicator3));
                indicatorList = new ArrayList<>(Arrays.asList(
                        findViewById(R.id.indicator1),
                        findViewById(R.id.indicator2),
                        findViewById(R.id.indicator3)));

                for (int  i =0; i<3; i++){
                    if (viewPager2.getCurrentItem() == indicatorList.indexOf(indicatorList.get(i))){
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
        if (viewPager2 == null ) return;
        indicatorList = new ArrayList<>(Arrays.asList(
                findViewById(R.id.indicator1),
                findViewById(R.id.indicator2),
                findViewById(R.id.indicator3)));

        for (int i =0; i<3; i++){
            int finalI = i;
            indicatorList.get(i).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    mainLooper.post(new Runnable() {
                        @Override
                        public void run() {
                            if (viewPager2 == null) return;
                            viewPager2.setCurrentItem(finalI);
                            if(!(finalI >= sheetArrayList.size() || finalI < 0)){ //index exists
                                rideRequestUtils.firebaseLogEventWithParams("indicator_click", "index" , String.valueOf(finalI), RideRequestActivity.this);
                            }
                        }
                    });
                }
            });
        }
    }

    private void updateProgressBars(boolean animated){
        if (sheetArrayList == null ) return;
        progressIndicatorsList = new ArrayList<>(Arrays.asList(
                findViewById(R.id.progress_indicator_1),
                findViewById(R.id.progress_indicator_2),
                findViewById(R.id.progress_indicator_3)));

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

    @Override
    protected void onDestroy() {
        instance = null;
        time = 0;
        sheetArrayList.clear();
        countDownTimer.cancel();
        cancelSound();
        NotificationUtils.lastRideReq.clear();
        rideRequestUtils.cancelRideReqNotification(this);
        super.onDestroy();
    }

    @Override
    protected void onPause() {
        NotificationUtils.lastRideReq.clear();
        super.onPause();
    }

    @Override
    protected void onResume() {
        super.onResume();
    }

    public static RideRequestActivity getInstance(){
        return instance;
    }

    private void cancelSound() {
        if (NotificationUtils.mediaPlayer!=null){
            if (NotificationUtils.mediaPlayer.isPlaying()){
                NotificationUtils.mediaPlayer.pause();
            }
        }
    }
}