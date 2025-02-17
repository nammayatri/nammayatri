/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import static in.juspay.mobility.app.NotificationUtils.DELIVERY;
import static in.juspay.mobility.app.NotificationUtils.NO_VARIANT;
import static in.juspay.mobility.app.NotificationUtils.RENTAL;
import static in.juspay.mobility.app.NotificationUtils.INTERCITY;
import static in.juspay.mobility.app.RideRequestUtils.firebaseLogEventWithParams;
import static in.juspay.mobility.app.RideRequestUtils.getRideRequestSound;
import static in.juspay.mobility.app.RideRequestUtils.getRideRequestSoundId;
import static in.juspay.mobility.app.RideRequestUtils.increaseVolume;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.ColorStateList;
import java.util.Locale;

import android.media.MediaPlayer;
import android.os.Bundle;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.Looper;
import android.os.PowerManager;
import android.util.Log;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.viewpager2.widget.ViewPager2;

import com.airbnb.lottie.LottieAnimationView;
import com.facebook.shimmer.ShimmerFrameLayout;
import com.google.android.material.progressindicator.LinearProgressIndicator;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.crashlytics.FirebaseCrashlytics;

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

public class RideRequestActivity extends AppCompatActivity {
    @SuppressLint("StaticFieldLeak")
    private static RideRequestActivity instance;
    Context context;
    private final Handler mainLooper = new Handler(Looper.getMainLooper());
    private final ArrayList<SheetModel> sheetArrayList = new ArrayList<>();
    private int time = 0;
    private ViewPager2 viewPager2;
    private Timer countDownTimer;
    private CountDownTimer rideStatusListener;
    private TextView indicatorText1, indicatorText2, indicatorText3, vehicleText1, vehicleText2, vehicleText3;
    private TextView tipBanner1, tipBanner2, tipBanner3;
    private ImageView tipBannerImage1, tipBannerImage2, tipBannerImage3;
    private ShimmerFrameLayout shimmerTip1, shimmerTip2, shimmerTip3;
    private LinearProgressIndicator progressIndicator1, progressIndicator2, progressIndicator3;
    private ArrayList<TextView> indicatorTextList, vehicleVariantList;
    private ArrayList<LinearProgressIndicator> progressIndicatorsList;
    private ArrayList<LinearLayout> indicatorList;
    private ArrayList<TextView> indicatorTipBannerList;
    private ArrayList<ImageView> indicatorTipBannerImageList;
    private ArrayList<LinearLayout> indicatorTipList;
    private ArrayList<ShimmerFrameLayout> shimmerTipList;
    private SharedPreferences sharedPref;
    private final MediaPlayer[] mediaPlayers = new MediaPlayer[3];
    private MediaPlayer currentMediaPlayer;
    private int currentMediaIndex = -1;
    int isMediaPlayerPrepared = 0;
    private String service = "";
    private String DUMMY_FROM_LOCATION = "dummyFromLocation";

    public static RideRequestActivity getInstance() {
        return instance;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        context = this;
        try {
            for (int i =0; i < 3; i++) {
                if (mediaPlayers[i] == null) {
                    mediaPlayers[i] = MediaPlayer.create(this, getRideRequestSound(this,i));
                    mediaPlayers[i].setLooping(true);
                    mediaPlayers[i].setOnPreparedListener(mp -> {
                        isMediaPlayerPrepared++;
                        mp.setWakeMode(this, PowerManager.PARTIAL_WAKE_LOCK);
                        if (isMediaPlayerPrepared == 3 && currentMediaIndex != -1 && (currentMediaPlayer == null || !currentMediaPlayer.isPlaying())) {
                            currentMediaPlayer = mediaPlayers[currentMediaIndex];
                            currentMediaPlayer.start();
                            if (sharedPref.getString("AUTO_INCREASE_VOL", "true").equals("true")){
                                increaseVolume(context);
                            }
                        }
                    });
                }
            }
        } catch (Exception e) {
            Exception exception = new Exception("Error in onBind " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            firebaseLogEventWithParams("exception_in_on_bind", "on_create", String.valueOf(e),this);
            e.printStackTrace();
        }
        service = getApplicationContext().getResources().getString(R.string.service);
        instance = this;
        setContentView(R.layout.activity_ride_request);
        viewPager2 = findViewById(R.id.view_pager);
        sheetAdapter.setViewPager(viewPager2);
        viewPager2.setAdapter(sheetAdapter);
        if (sharedPref == null)
            sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        if (getIntent() != null) {
            addToList(getIntent().getExtras());
        }
        View v = findViewById(R.id.indicator1);
    }

    public void addToList(Bundle rideRequestBundle) {
        if (rideRequestBundle == null || sheetArrayList == null || sheetArrayList.size() >= 3 || findCardById(rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQUEST_ID)))) return;
        mainLooper.post(() -> {
            if (findCardById(rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQUEST_ID))))
                return;
            String searchRequestValidTill = rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQ_VALID_TILL));
            float distanceToPickup = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_PICKUP));
            float distanceTobeCovered = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_BE_COVERED));
            int tollCharges = rideRequestBundle.getInt("tollCharges");
            String durationToPickup = rideRequestBundle.getString("durationToPickup");
            DecimalFormat df = new DecimalFormat();
            df.setMaximumFractionDigits(2);
            String getCurrTime = RideRequestUtils.getCurrentUTC();
            int calculatedTime = RideRequestUtils.calculateExpireTimer(searchRequestValidTill, getCurrTime);
            if (sheetArrayList.isEmpty()) {
                startTimer();
            }
            SharedPreferences sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            int negotiationUnit = rideRequestBundle.getInt("driverStepFeeWithCurrency", Integer.parseInt(sharedPref.getString( "NEGOTIATION_UNIT", "10")));
  
            double srcLat = rideRequestBundle.getDouble("srcLat");
            double srcLng = rideRequestBundle.getDouble("srcLng");
            double destLat = rideRequestBundle.getDouble("destLat");
            double destLng = rideRequestBundle.getDouble("destLng");
            boolean downgradeEnabled = rideRequestBundle.getBoolean("downgradeEnabled", false);
            int airConditioned = rideRequestBundle.getInt("airConditioned", -1);
            int ventilator = rideRequestBundle.getInt("ventilator", -1);
            String vehicleServiceTier = rideRequestBundle.getString("vehicleServiceTier", null);
            String rideStartTime = rideRequestBundle.getString("rideStartTime");
            String rideStartDate = rideRequestBundle.getString("rideStartDate");
            String rideDuration = String.format("%02d:%02d Hr", rideRequestBundle.getInt("rideDuration") / 3600 ,( rideRequestBundle.getInt("rideDuration") % 3600 ) / 60);
            String rideDistance = String.format("%d km", rideRequestBundle.getInt("rideDistance") / 1000);
            String notificationSource= rideRequestBundle.getString("notificationSource");
            int stops = rideRequestBundle.getInt("middleStopCount", 0);
            boolean roundTrip = rideRequestBundle.getBoolean("roundTrip");
            SheetModel sheetModel = new SheetModel((df.format(distanceToPickup / 1000)),
                    distanceTobeCovered,
                    tollCharges,
                    RideRequestUtils.calculateDp(durationToPickup, df),
                    rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_PICKUP)),
                    rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_DROP)),
                    rideRequestBundle.getInt(getResources().getString(R.string.BASE_FARE)),
                    Math.min(calculatedTime, 25),
                    rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQUEST_ID)),
                    rideRequestBundle.getString("destinationArea"),
                    rideRequestBundle.getString("sourceArea"),
                    rideRequestBundle.getString("currency"),
                    time,
                    rideRequestBundle.getInt("driverMinExtraFee"),
                    rideRequestBundle.getInt("driverMaxExtraFee"),
                    rideRequestBundle.getInt("rideRequestPopupDelayDuration"),
                    negotiationUnit,
                    rideRequestBundle.getInt("customerExtraFee"),
                    rideRequestBundle.getString("specialLocationTag"),
                    rideRequestBundle.getString("sourcePinCode"),
                    rideRequestBundle.getString("destinationPinCode"),
                    rideRequestBundle.getString("requestedVehicleVariant"),
                    rideRequestBundle.getBoolean("disabilityTag"),
                    rideRequestBundle.getBoolean("isTranslated"),
                    rideRequestBundle.getBoolean("gotoTag"),
                    rideRequestBundle.getInt("driverPickUpCharges"),
                    srcLat,
                    srcLng,
                    destLat,
                    destLng,
                    rideRequestBundle.getBoolean("specialZonePickup"),
                    rideRequestBundle.getInt("driverDefaultStepFee"),
                    downgradeEnabled,
                    airConditioned,
                    ventilator,
                    vehicleServiceTier,
                    rideRequestBundle.getString("rideProductType"),
                    rideDuration,
                    rideDistance,
                    rideStartTime,
                    rideStartDate,
                    notificationSource,
                    rideRequestBundle.getBoolean("isThirdPartyBooking"),
                    rideRequestBundle.getBoolean("isFavourite"),
                    rideRequestBundle.getDouble("parkingCharge"),
                    getCurrTime,
                    stops,
                    roundTrip
                    );
            if (currentMediaIndex == -1) {
                currentMediaIndex = getRideRequestSoundId(sheetModel.getRideProductType());
            }
            if (isMediaPlayerPrepared == 3 && (currentMediaPlayer == null || !currentMediaPlayer.isPlaying())) {
                currentMediaPlayer = mediaPlayers[currentMediaIndex];
                currentMediaPlayer.start();
                if (sharedPref.getString("AUTO_INCREASE_VOL", "true").equals("true")){
                    increaseVolume(context);
                }
            }
            sheetArrayList.add(sheetModel);
            sheetAdapter.updateSheetList(sheetArrayList);
            sheetAdapter.notifyItemInserted(sheetArrayList.indexOf(sheetModel));
            updateIndicators();
            updateProgressBars(false);
            RideRequestUtils.addRideReceivedEvent(null,rideRequestBundle,null,"ride_request_popped_in_activity", this);
        });
    }
    
    @SuppressLint("SetTextI18n")
    private void updateTagsView (SheetAdapter.SheetViewHolder holder, SheetModel model) {
        mainLooper.post(() -> {
            boolean showSpecialLocationTag = model.getSpecialZonePickup();
            String searchRequestId = model.getSearchRequestId();
//            boolean showVariant =  !model.getRequestedVehicleVariant().equals(NO_VARIANT) && model.isDowngradeEnabled() && RideRequestUtils.handleVariant(holder, model, this);
            if (model.getCustomerTip() > 0 || model.getDisabilityTag() || model.isGotoTag() || searchRequestId.equals(DUMMY_FROM_LOCATION) || showSpecialLocationTag || model.isFavourite() || model.getRoundTrip()) {
                holder.tagsBlock.setVisibility(View.VISIBLE);
                holder.accessibilityTag.setVisibility(model.getDisabilityTag() ? View.VISIBLE: View.GONE);
                if (showSpecialLocationTag && (model.getDriverDefaultStepFee() == model.getOfferedPrice())) {
                    holder.specialLocExtraTip.setText(model.getCurrency() + model.getDriverDefaultStepFee());
                    holder.specialLocExtraTip.setVisibility(View.VISIBLE);
                }else {
                    holder.specialLocExtraTip.setVisibility(View.GONE);
                }
                holder.customerTipTag.setVisibility(model.getCustomerTip() > 0 ? View.VISIBLE : View.GONE);
                holder.isFavouriteTag.setVisibility(model.isFavourite() ? View.VISIBLE : View.GONE);
                holder.specialLocTag.setVisibility(showSpecialLocationTag ? View.VISIBLE : View.GONE);
                holder.customerTipText.setText(sharedPref.getString("CURRENCY", "₹") + " " + model.getCustomerTip());
                holder.testRequestTag.setVisibility(searchRequestId.equals(DUMMY_FROM_LOCATION) ? View.VISIBLE : View.GONE);
                holder.gotoTag.setVisibility(model.isGotoTag() ? View.VISIBLE : View.GONE);
                holder.reqButton.setTextColor(model.isGotoTag() ? getColor(R.color.yellow900) : getColor(R.color.white));
                holder.reqButton.setBackgroundTintList(model.isGotoTag() ?
                        ColorStateList.valueOf(getColor(R.color.Black900)) :
                        ColorStateList.valueOf(getColor(R.color.green900)));
//                holder.rideTypeTag.setVisibility(showVariant ? View.VISIBLE : View.GONE);
                holder.stopsTag.setVisibility(model.getStops() > 0 ? View.VISIBLE : View.GONE);
                holder.stopsTagText.setText(getString(R.string.stops, model.getStops()));
                holder.roundTripRideTypeTag.setVisibility(model.getRoundTrip() ? View.VISIBLE : View.GONE);
            } else {
                holder.tagsBlock.setVisibility(View.GONE);
            }
        });
    }

    @SuppressLint("SetTextI18n")
    private void updateAcceptButtonText(SheetAdapter.SheetViewHolder holder, int rideRequestPopupDelayDuration, int startTime, String text) {
        if (rideRequestPopupDelayDuration > 0 && (time - startTime) < rideRequestPopupDelayDuration) {
            holder.reqButton.setText(text + " (" + (rideRequestPopupDelayDuration - (time - startTime)) + " )");
            holder.reqButton.setAlpha(0.5f);
            holder.reqButton.setClickable(false);
            holder.rejectButton.setAlpha(0.5f);
            holder.rejectButton.setClickable(false);
        } else {
            holder.reqButton.setText(text);
            holder.reqButton.setAlpha(1.0f);
            holder.reqButton.setClickable(true);
            holder.rejectButton.setAlpha(1.0f);
            holder.rejectButton.setClickable(true);
        }
    }

    private void updateIncreaseDecreaseButtons(SheetAdapter.SheetViewHolder holder, SheetModel model) {
        mainLooper.post(() -> {
            holder.buttonIncreasePrice.setVisibility(model.getDriverMaxExtraFee() == 0 ? View.GONE : View.VISIBLE);
            holder.buttonDecreasePrice.setVisibility(model.getDriverMaxExtraFee() == 0 ? View.GONE : View.VISIBLE);
            holder.buttonDecreasePrice.setAlpha(model.getButtonDecreasePriceAlpha());
            holder.buttonDecreasePrice.setClickable(model.isButtonDecreasePriceClickable());
            holder.buttonIncreasePrice.setAlpha(model.getButtonIncreasePriceAlpha());
            holder.buttonIncreasePrice.setClickable(model.isButtonIncreasePriceClickable());
        });
    }

    private final SheetAdapter sheetAdapter = new SheetAdapter(sheetArrayList, viewPager2, new SheetAdapter.OnItemClickListener() {
        @SuppressLint("SetTextI18n")
        @Override
        public void onViewHolderBind(SheetAdapter.SheetViewHolder holder, int position, ViewPager2 viewPager, List<Object> payloads) {
            SheetModel model = sheetArrayList.get(position);
            String x = !payloads.isEmpty() ? (String) payloads.get(0) : "";
            switch (x) {
                case "inc":
                    updateIndicators();
                    holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
                    holder.currency.setText(String.valueOf(model.getCurrency()));
                    updateIncreaseDecreaseButtons(holder, model);
                    RideRequestUtils.updateRateView(holder, model);
                    return;
                case "time":
                    updateAcceptButtonText(holder, model.getRideRequestPopupDelayDuration(), model.getStartTime(), (model.isGotoTag() ? getString(R.string.accept_goto) : getString(R.string.accept_offer)));
                    updateProgressBars(true);
                    return;
            }

            holder.pickUpDistance.setText(model.getPickUpDistance()+" km ");
            holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
            holder.distanceToBeCovered.setText(model.getDistanceToBeCovered() + " km");
            holder.tollTag.setVisibility(model.getTollCharges() > 0? View.VISIBLE : View.GONE);
            RideRequestUtils.handleDurationToPickup(holder, model, mainLooper, RideRequestActivity.this);
            holder.sourceArea.setText(model.getSourceArea());
            holder.sourceAddress.setText(model.getSourceAddress());
            holder.destinationArea.setText(model.getDestinationArea());
            holder.destinationAddress.setText(model.getDestinationAddress());

            if (!model.isTranslated()) RideRequestUtils.updateViewFromMlTranslation(holder, model, sharedPref, RideRequestActivity.this);

            holder.textIncPrice.setText(String.valueOf(model.getNegotiationUnit()));
            holder.textDecPrice.setText(String.valueOf(model.getNegotiationUnit()));
            holder.stopsInfo.setText(getString(model.getTollCharges() > 0 ? R.string.stops : R.string.stops_info, model.getStops()));
            holder.stopsInfo.setVisibility(model.getStops() > 0? View.VISIBLE : View.GONE);
            if(model.getSourcePinCode() != null &&  model.getSourcePinCode().trim().length()>0){
                holder.sourcePinCode.setText(model.getSourcePinCode().trim());
                holder.sourcePinCode.setVisibility(View.VISIBLE);
            }else{
                holder.sourceAddress.setMaxLines(2);
                holder.sourcePinCode.setVisibility(View.GONE);
            }
            if(model.getDestinationPinCode() != null && model.getDestinationPinCode().trim().length()>0){
                holder.destinationPinCode.setText(model.getDestinationPinCode());
                holder.destinationPinCode.setVisibility(View.VISIBLE);
            }else{
                holder.destinationAddress.setMaxLines(2);
                holder.destinationPinCode.setVisibility(View.GONE);
            }
            if (model.getspecialLocationTag() != null){
                RideRequestUtils.setSpecialZoneAttrs(holder, model.getspecialLocationTag(), RideRequestActivity.this);
            }

            updateAcceptButtonText(holder, model.getRideRequestPopupDelayDuration(), model.getStartTime(), model.isGotoTag() ? getString(R.string.accept_goto) : getString(R.string.accept_offer));
            RideRequestUtils.updateStepFeeAndButtonAlpha(holder, model, mainLooper);
            updateIncreaseDecreaseButtons(holder, model);
            updateTagsView(holder, model);
            RideRequestUtils.updateTierAndAC(holder, model, RideRequestActivity.this);
            RideRequestUtils.updateRateView(holder, model);
            RideRequestUtils.updateRentalView(holder, model, RideRequestActivity.this);
            RideRequestUtils.updateIntercityView(holder, model, RideRequestActivity.this);
            RideRequestUtils.updateExtraChargesString(holder, model, RideRequestActivity.this);
            String vehicleVariant = sharedPref.getString("VEHICLE_VARIANT", "");
            View progressDialog = findViewById(R.id.progress_loader);
            LottieAnimationView lottieAnimationView = progressDialog.findViewById(R.id.lottie_view_waiting);

            holder.reqButton.setOnClickListener(view -> {
                if (model.getSearchRequestId().equals(DUMMY_FROM_LOCATION)) {
                    respondDummyRequest();
                    removeCard(position);
                    return;
                }
                holder.reqButton.setClickable(false);
                // Deprecated as a generic loader is used irrespective of the vehicle variant
                // if (service.equals("yatriprovider") && vehicleVariant.equals("AUTO_RICKSHAW")){
                //     lottieAnimationView.setAnimation(R.raw.yatri_circular_loading_bar_auto);
                // }else if(service.equals("nammayatriprovider") && !vehicleVariant.equals("AUTO_RICKSHAW")){
                //     lottieAnimationView.setAnimation(R.raw.waiting_for_customer_lottie_cab);
                // }
                ExecutorService executor = Executors.newSingleThreadExecutor();
                executor.execute(() -> {
                    Boolean isApiSuccess = RideRequestUtils.driverRespondApi(model, true, RideRequestActivity.this, sheetArrayList.indexOf(model));
                    if (isApiSuccess) {
                        mainLooper.post(executor::shutdown);
                        startLoader(model.getSearchRequestId());
                    }
                });
            });
            holder.rejectButton.setOnClickListener(view -> {
                ExecutorService executor = Executors.newSingleThreadExecutor();
                executor.execute(() -> {
                    if (model.getSearchRequestId().equals(DUMMY_FROM_LOCATION)) {
                        respondDummyRequest();
                        removeCard(position);
                        return;
                    }
                    new Thread(() -> RideRequestUtils.driverRespondApi(model, false, RideRequestActivity.this, sheetArrayList.indexOf(model))).start();
                    holder.rejectButton.setClickable(false);
                    mainLooper.post(() -> {
                        removeCard(position);
                        executor.shutdown();
                        Toast.makeText(getApplicationContext(), getApplicationContext().getResources().getString(R.string.ride_rejected), Toast.LENGTH_SHORT).show();
                    });
                });
            });
            holder.buttonIncreasePrice.setOnClickListener(view -> {
                if (model.getOfferedPrice() <= model.getDriverMaxExtraFee() - model.getNegotiationUnit()) {
                    model.setUpdatedAmount(model.getUpdatedAmount() + model.getNegotiationUnit());
                    model.setOfferedPrice(model.getOfferedPrice() + model.getNegotiationUnit());
                    sheetAdapter.notifyItemChanged(position, "inc");
                    if (model.getOfferedPrice() == model.getDriverMaxExtraFee()) {
                        mainLooper.post(() -> {
                            model.setButtonIncreasePriceAlpha(0.5f);
                            model.setButtonIncreasePriceClickable(false);
                            model.setButtonDecreasePriceAlpha(1.0f);
                            model.setButtonDecreasePriceClickable(true);
                        });
                    } else {
                        mainLooper.post(() -> {
                            model.setButtonDecreasePriceAlpha(1.0f);
                            model.setButtonDecreasePriceClickable(true);
                        });
                    }
                }
            });
            holder.buttonDecreasePrice.setOnClickListener(view -> {
                if (model.getOfferedPrice() > 0) {
                    if (model.getSpecialZonePickup() && model.getOfferedPrice() >= model.getDriverDefaultStepFee()){
                        holder.specialLocExtraTip.setVisibility(View.GONE);
                    }
                    model.setUpdatedAmount(model.getUpdatedAmount() - model.getNegotiationUnit());
                    model.setOfferedPrice(model.getOfferedPrice() - model.getNegotiationUnit());
                    sheetAdapter.notifyItemChanged(position, "inc");
                    if (model.getOfferedPrice() == 0) {
                        mainLooper.post(() -> {
                            model.setButtonDecreasePriceAlpha(0.5f);
                            model.setButtonDecreasePriceClickable(false);
                            model.setButtonIncreasePriceAlpha(1.0f);
                            model.setButtonIncreasePriceClickable(true);
                        });
                    } else {
                        mainLooper.post(() -> {
                            model.setButtonIncreasePriceAlpha(1.0f);
                            model.setButtonIncreasePriceClickable(true);
                        });
                    }
                }
            });
            viewPager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
                @Override
                public void onPageSelected(int position) {
                    updateMediaPlayer(position);
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

    private void respondDummyRequest() {
        Handler handler = new Handler(Looper.getMainLooper());
        handler.post(() -> {
            Toast.makeText(getApplicationContext(), getString(R.string.test_request_successful), Toast.LENGTH_SHORT).show();
        });
    }

    private void startTimer() {
        TimerTask countUpTimerTask = new TimerTask() {
            @Override
            public void run() {
                time++;
                mainLooper.post(() -> {
                    for (SheetModel model : sheetArrayList) {
                        int index = sheetArrayList.indexOf(model);
                        if (model.getReqExpiryTime() + model.getStartTime() - time < 1) {
                            removeCard(index);
                        } else {
                            sheetAdapter.notifyItemChanged(index, "time");
                        }
                    }
                });
            }
        };
        countDownTimer = new Timer();
        countDownTimer.scheduleAtFixedRate(countUpTimerTask, 1000, 1000);
    }

    private void startLoader(String id) {
        if(countDownTimer != null) countDownTimer.cancel();
        for (MediaPlayer mediaPlayer : mediaPlayers) {
            mediaPlayer.release();
        }
        View progressDialog = findViewById(R.id.progress_loader);
        View viewPagerParentView = findViewById(R.id.view_pager_parent);
        mainLooper.post(() -> {
            viewPagerParentView.setVisibility(View.GONE);
            progressDialog.setVisibility(View.VISIBLE);
            rideStatusListener = new CountDownTimer(getResources().getInteger(R.integer.LOADER_WAITING_TIME), 1000) {
                @SuppressLint("SetTextI18n")
                @Override
                public void onTick(long millisUntilFinished) {
                    SharedPreferences sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
                    loaderText.setText(getString(R.string.waiting_for_customer_response) + " (" + (millisUntilFinished / 1000) + ") ...");
                    if (sharedPref.getString(getResources().getString(R.string.RIDE_STATUS), "null").equals(getResources().getString(R.string.DRIVER_ASSIGNMENT))) {
                        sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                        showAcknowledgement(getString(R.string.DRIVER_ASSIGNMENT));
                    } else if (sharedPref.getString(getString(R.string.CLEAR_FARE), "null").equals(id)) {
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
        });
    }

    private void showAcknowledgement(String ackType) {
        String ackText = ackType.equals(getString(R.string.DRIVER_ASSIGNMENT)) ? getString(R.string.ride_assigned) : getString(R.string.ride_assigned_to_another_driver);
        String key = getApplicationContext().getResources().getString(R.string.service);
        String vehicleVariant = sharedPref.getString("VEHICLE_VARIANT", null);
        View progressDialog = findViewById(R.id.progress_loader);
        int rawResource;
        if (ackType.equals(getString(R.string.DRIVER_ASSIGNMENT))){
            // Deprecated as a generic lottie loader is used irrespective of the vehicle variant or merchant
            // if (key != null && key.equals("yatriprovider") && vehicleVariant.equals("AUTO_RICKSHAW")) {
            //     rawResource = R.raw.yatri_auto_accepted_lottie;
            // }else if(key != null && key.equals("nammayatriprovider") && !vehicleVariant.equals("AUTO_RICKSHAW")){
            //     rawResource = R.raw.ride_accepted_lottie_cab;
            // }else
            rawResource = R.raw.ride_accepted_lottie;
        }
        else{
            // Deprecated as a generic lottie loader is used irrespective of the vehicle variant or merchant
            // if (key != null && key.equals("yatriprovider") && vehicleVariant.equals("AUTO_RICKSHAW")) {
            //     rawResource = R.raw.yatri_auto_declined;
            // }else if(key != null && key.equals("nammayatriprovider") && !vehicleVariant.equals("AUTO_RICKSHAW")){
            //     rawResource = R.raw.accepted_by_another_driver_lottie_cab;
            // }
            // else
            rawResource = R.raw.accepted_by_another_driver_lottie;
        }
        mainLooper.post(() -> {
            TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
            LottieAnimationView lottieAnimationView = progressDialog.findViewById(R.id.lottie_view_waiting);
            loaderText.setText(ackText);
            lottieAnimationView.setAnimation(rawResource);
            lottieAnimationView.setProgress(0);
            lottieAnimationView.setSpeed(1.2f);
            lottieAnimationView.playAnimation();
            if(rideStatusListener != null) rideStatusListener.cancel();
        });
        mainLooper.postDelayed(this::finish, 1700);
    }

    private void removeCard(int position) {
        mainLooper.post(() -> {
            if (!(sheetArrayList.size() > position)) {
                return;
            }
            if (position >= 0) {
                sheetArrayList.size();
                sheetArrayList.remove(position);
            }
            updateMediaPlayer(viewPager2.getCurrentItem());
            sheetAdapter.updateSheetList(sheetArrayList);
            sheetAdapter.notifyItemRemoved(position);
            sheetAdapter.notifyItemRangeChanged(position, sheetArrayList.size());
            updateIndicators();
            updateProgressBars(true);
            if (sheetArrayList.isEmpty()) {
                for (MediaPlayer mediaPlayer : mediaPlayers) {
                    mediaPlayer.release();
                }
                finish();
            }
        });
    }

    /*
     * To update the audio sound based on notification type when card changes are driver swipes the cards*/
    private void updateMediaPlayer(int position) {
        try {
            if (position < 0 || position >= sheetArrayList.size()) return;
            if (currentMediaPlayer != null && currentMediaPlayer.isPlaying()) {
                currentMediaPlayer.pause();
            }
            currentMediaIndex = getRideRequestSoundId(sheetArrayList.get(position).getRideProductType());
            currentMediaPlayer = mediaPlayers[currentMediaIndex];
            currentMediaPlayer.start();
        } catch (Exception e) {

        }
    }

    @SuppressLint("SetTextI18n")
    private void updateIndicators() {
        mainLooper.post(() -> {
            if (viewPager2 == null) return;
            if (indicatorText1 == null) indicatorText1 = findViewById(R.id.indicatorText1);
            if (indicatorText2 == null) indicatorText2 = findViewById(R.id.indicatorText2);
            if (indicatorText3 == null) indicatorText3 = findViewById(R.id.indicatorText3);
            if (vehicleText1 == null) vehicleText1 = findViewById(R.id.variant1);
            if (vehicleText2 == null) vehicleText2 = findViewById(R.id.variant2);
            if (vehicleText3 == null) vehicleText3 = findViewById(R.id.variant3);
            if (progressIndicator1 == null) progressIndicator1 = findViewById(R.id.progress_indicator_1);
            if (progressIndicator2 == null) progressIndicator2 = findViewById(R.id.progress_indicator_2);
            if (progressIndicator3 == null) progressIndicator3 = findViewById(R.id.progress_indicator_3);
            if (indicatorTextList == null) indicatorTextList = new ArrayList<>(Arrays.asList(indicatorText1, indicatorText2, indicatorText3));
            if (vehicleVariantList == null) vehicleVariantList = new ArrayList<>(Arrays.asList(vehicleText1, vehicleText2, vehicleText3));
            if (progressIndicatorsList == null) progressIndicatorsList = new ArrayList<>(Arrays.asList(progressIndicator1, progressIndicator2, progressIndicator3));
            if (indicatorList == null) {
                indicatorList = new ArrayList<>(Arrays.asList(
                        findViewById(R.id.indicator1),
                        findViewById(R.id.indicator2),
                        findViewById(R.id.indicator3)));
                setIndicatorClickListener();
            }
            if (indicatorTipList == null) indicatorTipList = new ArrayList<>(Arrays.asList(
                    findViewById(R.id.tip_indicator_0),
                    findViewById(R.id.tip_indicator_1),
                    findViewById(R.id.tip_indicator_2)));
            if (tipBanner1 == null) tipBanner1 = findViewById(R.id.tip_banner_view_0);
            if (tipBanner2 == null) tipBanner2 = findViewById(R.id.tip_banner_view_1);
            if (tipBanner3 == null) tipBanner3 = findViewById(R.id.tip_banner_view_2);
            if (indicatorTipBannerList == null) indicatorTipBannerList = new ArrayList<>(Arrays.asList(tipBanner1, tipBanner2, tipBanner3));
            if (tipBannerImage1 == null) tipBannerImage1 = findViewById(R.id.tip_banner_image_0);
            if (tipBannerImage2 == null) tipBannerImage2 = findViewById(R.id.tip_banner_image_1);
            if (tipBannerImage3 == null) tipBannerImage3 = findViewById(R.id.tip_banner_image_2);
            if (indicatorTipBannerImageList == null) indicatorTipBannerImageList = new ArrayList<>(Arrays.asList(tipBannerImage1, tipBannerImage2, tipBannerImage3));
            if (shimmerTip1 == null) shimmerTip1 = findViewById(R.id.shimmer_view_container_0);
            if (shimmerTip2 == null) shimmerTip2 = findViewById(R.id.shimmer_view_container_1);
            if (shimmerTip3 == null) shimmerTip3 = findViewById(R.id.shimmer_view_container_2);
            if (shimmerTipList == null) shimmerTipList = new ArrayList<>(Arrays.asList(shimmerTip1, shimmerTip2, shimmerTip3));
            for (int i = 0; i < 3; i++) {
                if (i < sheetArrayList.size()) {
                    shimmerTipList.get(i).setVisibility(View.VISIBLE);
                    updateTopBarBackground(i);
                    //handleIndicatorVariant(i); // Not needed @Rohit
                    vehicleVariantList.get(i).setVisibility(View.GONE);
                    indicatorTextList.get(i).setText(sharedPref.getString("CURRENCY", "₹") + (sheetArrayList.get(i).getBaseFare() + sheetArrayList.get(i).getUpdatedAmount()));
                    progressIndicatorsList.get(i).setVisibility(View.VISIBLE);
                    boolean isSpecialZone = sheetArrayList.get(i).getSpecialZonePickup();
                    if (viewPager2.getCurrentItem() == indicatorList.indexOf(indicatorList.get(i)) && sheetArrayList.get(i).getCustomerTip() > 0) {
                        indicatorList.get(i).setBackgroundColor(getColor(isSpecialZone ?  R.color.green100 : R.color.yellow200));
                    }
                    updateTopBar(i);
                } else {
                    indicatorTextList.get(i).setText("--");
                    indicatorList.get(i).setBackgroundColor(getColor(R.color.white));
                    shimmerTipList.get(i).setVisibility(View.GONE);
                    vehicleVariantList.get(i).setVisibility(View.GONE);
                    progressIndicatorsList.get(i).setVisibility(View.GONE);
                    indicatorTipBannerList.get(i).setVisibility(View.INVISIBLE);
                    indicatorTipBannerImageList.get(i).setVisibility(View.GONE);
                }
            }
        });
    }

    private void updateTopBarBackground(int i) {
        if (viewPager2.getCurrentItem() == indicatorList.indexOf(indicatorList.get(i))) {
            boolean isSpecialZone = sheetArrayList.get(i).getSpecialZonePickup();
            switch (sheetArrayList.get(i).getRideProductType()) {
                case RENTAL:
                    indicatorList.get(i).setBackgroundColor(getColor(R.color.turquoise10));
                    break;
                case INTERCITY:
                    indicatorList.get(i).setBackgroundColor(getColor(R.color.blue600));
                    break;
                case DELIVERY:
                    indicatorList.get(i).setBackgroundColor(getColor(R.color.white));
                    break;
                default:
                    indicatorList.get(i).setBackgroundColor(getColor(isSpecialZone ? R.color.green100 : R.color.grey900));
                    break;
            }
            progressIndicatorsList.get(i).setTrackColor(getColor(R.color.white));
            shimmerTipList.get(i).stopShimmer();
        } else {
            indicatorList.get(i).setBackgroundColor(getColor(R.color.white));
            progressIndicatorsList.get(i).setTrackColor(getColor(R.color.grey900));
            shimmerTipList.get(i).startShimmer();
        }
    }

    private void updateTopBar (int i){
        boolean isSpecialZone = sheetArrayList.get(i).getSpecialZonePickup();
        String rideProductType = sheetArrayList.get(i).getRideProductType();

        switch (rideProductType) {
            case RENTAL:
                indicatorTipBannerList.get(i).setVisibility(View.VISIBLE);
                indicatorTipBannerList.get(i).setText("Rental");
                indicatorTipBannerList.get(i).setTextColor(getColor(R.color.white));
                indicatorTipList.get(i).setBackground(getDrawable(R.drawable.rental_banner_rectangle));
                indicatorTipBannerImageList.get(i).setVisibility(View.GONE);
                break;
            case INTERCITY:
                indicatorTipBannerList.get(i).setVisibility(View.VISIBLE);
                indicatorTipBannerList.get(i).setText("Intercity");
                indicatorTipBannerList.get(i).setTextColor(getColor(R.color.white));
                indicatorTipList.get(i).setBackground(getDrawable(R.drawable.intercity_banner_rectangle));
                indicatorTipBannerImageList.get(i).setVisibility(View.GONE);
                break;
            case DELIVERY:
                indicatorTipBannerList.get(i).setVisibility(View.VISIBLE);
                indicatorTipBannerList.get(i).setText("Delivery");
                indicatorTipBannerList.get(i).setTextColor(getColor(R.color.Black800));
                indicatorTipList.get(i).setBackground(getDrawable(R.drawable.delivery_banner_rectangle));
                indicatorTipBannerImageList.get(i).setVisibility(View.VISIBLE);
                indicatorTipBannerImageList.get(i).setImageResource(R.drawable.ny_ic_delivery);
                break;
            default:
                if (sheetArrayList.get(i).getCustomerTip() > 0 || isSpecialZone || sheetArrayList.get(i).isFavourite()) {
                    indicatorTipBannerList.get(i).setVisibility(View.VISIBLE);
                    indicatorTipBannerList.get(i).setText(isSpecialZone? "Zone" : (sheetArrayList.get(i).getCustomerTip() > 0 ? "TIP" : "Favourite"));
                    indicatorTipBannerList.get(i).setTextColor(isSpecialZone ? getColor(R.color.white) : (sheetArrayList.get(i).getCustomerTip() > 0 ? getColor(R.color.black650) : getColor(R.color.white)));
                    indicatorTipList.get(i).setBackground(getDrawable(isSpecialZone ? R.drawable.zone_curve : (sheetArrayList.get(i).getCustomerTip() > 0 ? R.drawable.rectangle_9506 : R.drawable.blue_curve)));
                    indicatorTipBannerImageList.get(i).setVisibility(View.GONE);
                } else {
                    indicatorTipBannerList.get(i).setVisibility(View.INVISIBLE);
                    indicatorTipBannerImageList.get(i).setVisibility(View.GONE);
                }
        }
    }

    private void setIndicatorClickListener() {
        if (viewPager2 == null) return;
        for (int i = 0; i < 3; i++) {
            int finalI = i;
            indicatorList.get(i).setOnClickListener(view -> mainLooper.post(() -> {
                if (viewPager2 == null) return;
                viewPager2.setCurrentItem(finalI);
                if (!(finalI >= sheetArrayList.size())) { //index exists
                    firebaseLogEventWithParams("indicator_click", "index", String.valueOf(finalI), RideRequestActivity.this);
                }
            }));
        }
    }

    private void updateProgressBars(boolean animated) {
        if (sheetArrayList == null) return;
        try {    
        progressIndicatorsList = new ArrayList<>(Arrays.asList(
                findViewById(R.id.progress_indicator_1),
                findViewById(R.id.progress_indicator_2),
                findViewById(R.id.progress_indicator_3)));

        for (int i = 0; (i < sheetArrayList.size()) && (i < progressIndicatorsList.size()); i++) {
            int progressCompat = sheetArrayList.get(i).getReqExpiryTime() + sheetArrayList.get(i).getStartTime() - time;
            progressIndicatorsList.get(i).setProgressCompat(progressCompat * 4, animated); // (100/maxExpiryTime)
            if (progressCompat <= 8) {
                progressIndicatorsList.get(i).setIndicatorColor(getColor(R.color.red900));
            } else {
                progressIndicatorsList.get(i).setIndicatorColor(getColor(R.color.green900));
            }
        }
    }
     catch (Exception e) {
            // TODO: handle exception
        }
    }

    private boolean findCardById(String id) {
        try {
        if (sheetArrayList != null) {
            for (int i = 0; i < sheetArrayList.size(); i++) {
                if (id.equals(sheetArrayList.get(i).getSearchRequestId())) {
                    return true;
                }
            }
        }
        } catch (Exception e) {
            FirebaseAnalytics.getInstance(this).logEvent("Exception_in_findCardById", null);
            Log.e("RideRequestActivity", "Error in findCardById : " + e);
            return false;
        }
        return false;
    }


    public boolean removeCardById(String id){
        if (sheetArrayList.size()>0){
            for (int i = 0; i<sheetArrayList.size(); i++){
                if (id.equals(sheetArrayList.get(i).getSearchRequestId())){
                    removeCard(i);
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
        if(countDownTimer != null) countDownTimer.cancel();
        for (MediaPlayer mediaPlayer : mediaPlayers) {
            mediaPlayer.release();
        }
        NotificationUtils.lastRideReq.clear();
        RideRequestUtils.cancelRideReqNotification(this);
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
        RideRequestUtils.cancelRideReqNotification(this);
    }

}