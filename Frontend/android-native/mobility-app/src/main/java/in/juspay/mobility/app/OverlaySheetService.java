/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import static in.juspay.mobility.app.NotificationUtils.NO_VARIANT;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.res.ColorStateList;
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
import android.provider.Settings;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.viewpager2.widget.ViewPager2;

import com.airbnb.lottie.LottieAnimationView;
import com.facebook.shimmer.ShimmerFrameLayout;
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
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.hyper.bridge.HBridge;
import in.juspay.mobility.app.callbacks.CallBack;

import in.juspay.mobility.app.TranslatorMLKit;

public class OverlaySheetService extends Service implements View.OnTouchListener {

    private static final ArrayList<CallBack> callBack = new ArrayList<>();
    private final ArrayList<SheetModel> sheetArrayList = new ArrayList<>();
    private final Handler mainLooper = new Handler(Looper.getMainLooper());
    ExecutorService executor = Executors.newSingleThreadExecutor();
    private ViewPager2 viewPager;
    private Timer countDownTimer;
    private WindowManager windowManager;
    private SharedPreferences sharedPref;
    private MediaPlayer mediaPlayer;
    private int time = 0;
    private View progressDialog, apiLoader, floatyView;
    private CountDownTimer rideStatusListener;
    private WindowManager.LayoutParams params;
    private FirebaseAnalytics mFirebaseAnalytics;
    private Boolean isRideAcceptedOrRejected = false;
    private TextView indicatorText1, indicatorText2, indicatorText3, vehicleText1, vehicleText2, vehicleText3;
    private TextView indicatorTip1, indicatorTip2, indicatorTip3;
    private ShimmerFrameLayout shimmerTip1, shimmerTip2, shimmerTip3;
    private LinearProgressIndicator progressIndicator1, progressIndicator2, progressIndicator3;
    private ArrayList<TextView> indicatorTextList, vehicleVariantList;
    private ArrayList<LinearProgressIndicator> progressIndicatorsList;
    private ArrayList<LinearLayout> indicatorList;
    private ArrayList<TextView> tipsList;
    private ArrayList<ShimmerFrameLayout> shimmerTipList;
    private String key = "";

    @Override
    public void onCreate() {
        super.onCreate();
        key = getApplicationContext().getResources().getString(R.string.service);
    }

    public static void registerCallback(CallBack notificationCallback) {
        callBack.add(notificationCallback);
    }

    public static void deRegisterCallback(CallBack notificationCallback) {
        callBack.remove(notificationCallback);
    }

    @SuppressLint("SetTextI18n")
    private void updateTagsView (SheetAdapter.SheetViewHolder holder, SheetModel model) {
        mainLooper.post(() -> {
            String variant = model.getRequestedVehicleVariant();
            if (model.getCustomerTip() > 0 || model.getDisabilityTag() || model.isGotoTag() || (!variant.equals(NO_VARIANT) && key.equals("yatrisathiprovider"))) {
                String pickupChargesText = model.getCustomerTip() > 0 ?
                        getString(R.string.includes_pickup_charges_10) + " " + getString(R.string.and) + sharedPref.getString("CURRENCY", "₹") + " " + model.getCustomerTip() + " " + getString(R.string.tip) :
                        getString(R.string.includes_pickup_charges_10);
                holder.tagsBlock.setVisibility(View.VISIBLE);
                holder.accessibilityTag.setVisibility(model.getDisabilityTag() ? View.VISIBLE : View.GONE);
                holder.textIncludesCharges.setText(pickupChargesText);
                holder.customerTipText.setText(sharedPref.getString("CURRENCY", "₹") + " " + model.getCustomerTip() + " " + getString(R.string.tip));
                holder.customerTipTag.setVisibility(model.getCustomerTip() > 0 ? View.VISIBLE : View.GONE);
                holder.gotoTag.setVisibility(model.isGotoTag() ? View.VISIBLE : View.GONE);
                holder.reqButton.setTextColor(model.isGotoTag() ? getColor(R.color.yellow900) : getColor(R.color.white));
                holder.reqButton.setBackgroundTintList(model.isGotoTag() ?
                        ColorStateList.valueOf(getColor(R.color.Black900)) :
                        ColorStateList.valueOf(getColor(R.color.green900)));

                if (!variant.equals(NO_VARIANT) && key.equals("yatrisathiprovider")) {
                    if (Utils.getVariantType(variant).equals(Utils.VariantType.AC)) {
                        holder.rideTypeTag.setBackgroundResource(R.drawable.ic_ac_variant_tag);
                        holder.rideTypeTag.setVisibility(View.VISIBLE);
                    } else {
                        holder.rideTypeTag.setVisibility(View.VISIBLE);
                        holder.rideTypeTag.setBackgroundResource(R.drawable.ic_non_ac_variant_tag);
                        holder.rideTypeImage.setVisibility(View.GONE);
                    }
                    holder.rideTypeText.setText(variant);
                }
            } else {
                holder.tagsBlock.setVisibility(View.GONE);
            }
        });
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

    private final SheetAdapter sheetAdapter = new SheetAdapter(sheetArrayList, viewPager, new SheetAdapter.OnItemClickListener() {
        @SuppressLint("SetTextI18n")
        @Override
        public void onViewHolderBind(SheetAdapter.SheetViewHolder holder, int position, ViewPager2 viewPager, List<Object> payloads) {
            SheetModel model = sheetArrayList.get(position);
            String x = payloads.size() > 0 ? (String) payloads.get(0) : "";
            switch (x) {
                case "inc":
                    updateIndicators();
                    holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
                    holder.currency.setText(String.valueOf(model.getCurrency()));
                    updateIncreaseDecreaseButtons(holder, model);
                    return;
                case "time":
                    updateAcceptButtonText(holder, model.getRideRequestPopupDelayDuration(), model.getStartTime(), (model.isGotoTag() ? getString(R.string.accept_goto) : getString(R.string.accept_offer)));
                    updateProgressBars(true);
                    return;
            }

            holder.pickUpDistance.setText(model.getPickUpDistance() + " km ");
            holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
            holder.currency.setText(String.valueOf(model.getCurrency()));
            holder.distanceToBeCovered.setText(model.getDistanceToBeCovered() + " km");

            if( key.equals("yatrisathiprovider") ){
                holder.durationToPickup.setVisibility(View.VISIBLE);
                holder.durationToPickupImage.setVisibility(View.VISIBLE);
                holder.durationToPickup.setText(model.getDurationToPickup() + " min");
            } else {
                holder.durationToPickup.setVisibility(View.GONE);
                holder.durationToPickupImage.setVisibility(View.GONE);
            }

            holder.sourceArea.setText(model.getSourceArea());
            holder.sourceAddress.setText(model.getSourceAddress());
            holder.destinationArea.setText(model.getDestinationArea());
            holder.destinationAddress.setText(model.getDestinationAddress());
            holder.textIncPrice.setText(String.valueOf(model.getNegotiationUnit()));
            holder.textDecPrice.setText(String.valueOf(model.getNegotiationUnit()));
            if (model.getSourcePinCode() != null && model.getSourcePinCode().trim().length() > 0) {
                holder.sourcePinCode.setText(model.getSourcePinCode().trim());
                holder.sourcePinCode.setVisibility(View.VISIBLE);
            } else {
                holder.sourceAddress.setMaxLines(2);
                holder.sourcePinCode.setVisibility(View.GONE);
            }
            if (model.getDestinationPinCode() != null && model.getDestinationPinCode().trim().length() > 0) {
                holder.destinationPinCode.setText(model.getDestinationPinCode());
                holder.destinationPinCode.setVisibility(View.VISIBLE);
            } else {
                holder.destinationAddress.setMaxLines(2);
                holder.destinationPinCode.setVisibility(View.GONE);
            }
            if (model.getspecialLocationTag() != null) {
                RideRequestUtils.setSpecialZoneAttrs(holder, model.getspecialLocationTag(), OverlaySheetService.this);
            }
            sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String useMLKit = sharedPref.getString("USE_ML_TRANSLATE", "false");
            if (useMLKit.equals("false") && !model.isTranslated()) RideRequestUtils.updateViewFromMlTranslation(holder, model, sharedPref, OverlaySheetService.this);

            if (key.equals("yatrisathiprovider") || key.equals("yatriprovider")) {
                holder.textIncludesCharges.setVisibility(View.GONE);
            }
            updateAcceptButtonText(holder, model.getRideRequestPopupDelayDuration(), model.getStartTime(), model.isGotoTag() ? getString(R.string.accept_goto) : getString(R.string.accept_offer));
            updateIncreaseDecreaseButtons(holder, model);
            updateTagsView(holder, model);
            holder.reqButton.setOnClickListener(view -> {
                holder.reqButton.setClickable(false);
                if (key != null && key.equals("nammayatriprovider"))
                    startApiLoader();
                String vehicleVariant = sharedPref.getString("VEHICLE_VARIANT", null);
                if (key != null && key.equals("yatriprovider") && vehicleVariant.equals("AUTO_RICKSHAW")){
                    LottieAnimationView lottieAnimationView = progressDialog.findViewById(R.id.lottie_view_waiting);
                    lottieAnimationView.setAnimation(R.raw.yatri_circular_loading_bar_auto);
                }
                ExecutorService executor = Executors.newSingleThreadExecutor();
                Handler handler = new Handler(Looper.getMainLooper());
                executor.execute(() -> {
                    try {
                        Boolean isApiSuccess = driverRespondApi(model.getSearchRequestId(), model.getOfferedPrice(), true, sheetArrayList.indexOf(model));
                        if (isApiSuccess) {
                            holder.reqButton.setClickable(false);
                            updateSharedPreferences();
                            for (int i = 0; i < callBack.size(); i++) {
                                callBack.get(i).driverCallBack("RIDE_REQUESTED");
                            }
                            String logEvent = sharedPref.getString("DRIVER_STATUS_N", "null").equals("Silent") ? "silent_ride_accepted" : "ride_accepted";
                            firebaseLogEvent(logEvent);
                            isRideAcceptedOrRejected = true;
                            handler.post(() -> {
                                startLoader(model.getSearchRequestId());
                                executor.shutdown();
                            });
                        } else {
                            handler.post(() -> {
                                removeCard(position);
                                if (apiLoader != null && apiLoader.isAttachedToWindow()) {
                                    windowManager.removeView(apiLoader);
                                    apiLoader = null;
                                }
                                if (sheetArrayList.size() > 0) {
                                    holder.reqButton.setClickable(true);
                                } else {
                                    cleanUp();
                                    executor.shutdown();
                                }
                            });
                        }
                    } catch (Exception e) {
                        firebaseLogEventWithParams("exception", "request_button_click", e.toString());
                        cleanUp();
                    }
                });
            });

            holder.rejectButton.setOnClickListener(view -> {
                ExecutorService executor = Executors.newSingleThreadExecutor();
                Handler handler = new Handler(Looper.getMainLooper());
                executor.execute(() -> {
                    try {
                        new Thread(() -> driverRespondApi(model.getSearchRequestId(), model.getOfferedPrice(), false, sheetArrayList.indexOf(model))).start();
                        isRideAcceptedOrRejected = true;
                        holder.rejectButton.setClickable(false);
                        handler.post(() -> {
                            String logEvent = sharedPref.getString("DRIVER_STATUS_N", "null").equals("Silent") ? "silent_ride_declined" : "ride_declined";
                            Utils.logEvent (logEvent, getApplicationContext());
                            removeCard(position);
                            executor.shutdown();
                            Toast.makeText(getApplicationContext(), getString(R.string.ride_rejected), Toast.LENGTH_SHORT).show();
                        });
                    } catch (Exception e) {
                        firebaseLogEventWithParams("exception", "reject_button_click", e.toString());
                        System.out.println("reject exception: " + e);
                    }
                });
            });

            holder.buttonIncreasePrice.setOnClickListener(view -> {
                if (model.getOfferedPrice() <= model.getDriverMaxExtraFee() - model.getNegotiationUnit()) {
                    model.setUpdatedAmount(model.getUpdatedAmount() + model.getNegotiationUnit());
                    firebaseLogEvent("price_is_increased");
                    model.setOfferedPrice(model.getOfferedPrice() + model.getNegotiationUnit());
                    sheetAdapter.notifyItemChanged(position, "inc");
                    Handler handler = new Handler(Looper.getMainLooper());
                    if (model.getOfferedPrice() == model.getDriverMaxExtraFee()) {
                        handler.post(() -> {
                            model.setButtonIncreasePriceAlpha(0.5f);
                            model.setButtonIncreasePriceClickable(false);
                            model.setButtonDecreasePriceAlpha(1.0f);
                            model.setButtonDecreasePriceClickable(true);
                        });
                    } else {
                        handler.post(() -> {
                            model.setButtonDecreasePriceAlpha(1.0f);
                            model.setButtonDecreasePriceClickable(true);
                        });
                    }
                }
            });
            holder.buttonDecreasePrice.setOnClickListener(view -> {
                if (model.getOfferedPrice() > 0) {
                    model.setUpdatedAmount(model.getUpdatedAmount() - model.getNegotiationUnit());
                    firebaseLogEvent("price_is_decreased");
                    model.setOfferedPrice(model.getOfferedPrice() - model.getNegotiationUnit());
                    sheetAdapter.notifyItemChanged(position, "inc");
                    Handler handler = new Handler(Looper.getMainLooper());
                    if (model.getOfferedPrice() == 0) {
                        handler.post(() -> {
                            model.setButtonDecreasePriceAlpha(0.5f);
                            model.setButtonDecreasePriceClickable(false);
                            model.setButtonIncreasePriceAlpha(1.0f);
                            model.setButtonIncreasePriceClickable(true);
                        });
                    } else {
                        handler.post(() -> {
                            model.setButtonIncreasePriceAlpha(1.0f);
                            model.setButtonIncreasePriceClickable(true);
                        });
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

    private void removeCard(int position) {
        try {
            Handler handler = new Handler(Looper.getMainLooper());
            handler.post(() -> {
                if (!(sheetArrayList.size() > position)) {
                    return;
                }
                if (position >= 0) {
                    sheetArrayList.size();
                    sheetArrayList.remove(position);
                }
                sheetAdapter.updateSheetList(sheetArrayList);
                sheetAdapter.notifyItemRemoved(position);
                sheetAdapter.notifyItemRangeChanged(position, sheetArrayList.size());
                updateIndicators();
                updateProgressBars(false);
                if (sheetArrayList.isEmpty()) {
                    cleanUp();
                }
            });
        } catch (Exception e) {
            firebaseLogEventWithParams("exception", "remove_card", e.toString());
            e.printStackTrace();
        }
    }

    private void cleanUp() {
        if (!isRideAcceptedOrRejected) {
            firebaseLogEvent("ride_ignored");
        }
        try {
            callBack.clear();
            countDownTimer.cancel();
            sheetAdapter.updateSheetList(new ArrayList<>());
            viewPager = null;
            if (rideStatusListener != null) {
                rideStatusListener.cancel();
                rideStatusListener = null;
            }

            if (floatyView != null) {
                if (floatyView.getParent() != null) {
                    windowManager.removeView(floatyView);
                }
            }

            if (progressDialog != null && progressDialog.isAttachedToWindow()) {
                if (progressDialog.getParent() != null) {
                    windowManager.removeView(progressDialog);
                }
            }

            if (apiLoader != null && apiLoader.getParent() != null) {
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
        } catch (Exception e) {
            firebaseLogEventWithParams("exception", "clean_up", e.toString());
            Log.e("EXCEPTION", e.toString());
        }
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        try {
            if (mediaPlayer == null) {
                mediaPlayer = MediaPlayer.create(getApplicationContext(), R.raw.allocation_request);
                mediaPlayer.setLooping(true);
                mediaPlayer.setOnPreparedListener(mp -> mediaPlayer.setWakeMode(getApplicationContext(), PowerManager.PARTIAL_WAKE_LOCK));
            }
        } catch (Exception e) {
            firebaseLogEventWithParams("exception", "on_bind", e.toString());
            e.printStackTrace();
        }
        return new OverlayBinder();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    public void addToList(Bundle rideRequestBundle) {
        try {
            Handler handler = new Handler(Looper.getMainLooper());
            handler.postDelayed(() -> executor.execute(() -> {
                String searchRequestId = rideRequestBundle.getString("searchRequestId");
                boolean isClearedReq = MyFirebaseMessagingService.clearedRideRequest.containsKey(searchRequestId);
                boolean requestAlreadyPresent = findCardById(searchRequestId);
                if (sheetArrayList.size() >= 3 || requestAlreadyPresent || isClearedReq ){
                    if (isClearedReq) MyFirebaseMessagingService.clearedRideRequest.remove(searchRequestId);
                    return;
                }
                if (progressDialog == null || apiLoader == null) {
                    if (mediaPlayer != null) {
                        mediaPlayer.start();
                        increaseVolume();
                    }
                }
                handler.post(() -> {
                    String searchRequestValidTill = rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQ_VALID_TILL));
                    int baseFare = rideRequestBundle.getInt(getResources().getString(R.string.BASE_FARE));
                    String currency = rideRequestBundle.getString("currency");
                    float distanceToPickup = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_PICKUP));
                    float distanceTobeCovered = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_BE_COVERED));
                    String addressPickUp = rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_PICKUP));
                    String addressDrop = rideRequestBundle.getString(getResources().getString(R.string.ADDRESS_DROP));
                    String sourceArea = rideRequestBundle.getString("sourceArea");
                    String destinationArea = rideRequestBundle.getString("destinationArea");
                    int driverMaxExtraFee = rideRequestBundle.getInt("driverMaxExtraFee");
                    String durationToPickup = rideRequestBundle.getString("durationToPickup");
                    int driverMinExtraFee = rideRequestBundle.getInt("driverMinExtraFee");
                    int rideRequestPopupDelayDuration = rideRequestBundle.getInt("rideRequestPopupDelayDuration");
                    String specialLocationTag = rideRequestBundle.getString("specialLocationTag");
                    String sourcePinCode = rideRequestBundle.getString("sourcePinCode");
                    String destinationPinCode = rideRequestBundle.getString("destinationPinCode");
                    DecimalFormat df = new DecimalFormat("###.##", new DecimalFormatSymbols(new Locale("en", "us")));
                    String requestedVehicleVariant = rideRequestBundle.getString("requestedVehicleVariant");
                    Boolean disabilityTag = rideRequestBundle.getBoolean("disabilityTag");
                    Boolean isTranslated = rideRequestBundle.getBoolean("isTranslated");
                    df.setMaximumFractionDigits(2);
                    final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en", "us"));
                    f.setTimeZone(TimeZone.getTimeZone("UTC"));
                    String getCurrTime = f.format(new Date());
                    int calculatedTime = RideRequestUtils.calculateExpireTimer(searchRequestValidTill, getCurrTime);
                    if (sharedPref == null)
                        sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    int negotiationUnit = Integer.parseInt(sharedPref.getString( "NEGOTIATION_UNIT", "10"));
                    int rideRequestedBuffer = Integer.parseInt(sharedPref.getString("RIDE_REQUEST_BUFFER", "2"));
                    int customerExtraFee = rideRequestBundle.getInt("customerExtraFee");
                    boolean gotoTag = rideRequestBundle.getBoolean("gotoTag");
                    if (calculatedTime > rideRequestedBuffer) {
                        calculatedTime -= rideRequestedBuffer;

                    }
                    SheetModel sheetModel = new SheetModel((df.format(distanceToPickup / 1000)),
                            (df.format(distanceTobeCovered / 1000)),
                            (df.format(Integer.parseInt(durationToPickup)/ 60)),
                            addressPickUp,
                            addressDrop,
                            baseFare,
                            calculatedTime,
                            searchRequestId,
                            destinationArea,
                            sourceArea,
                            currency,
                            time,
                            driverMinExtraFee,
                            driverMaxExtraFee,
                            rideRequestPopupDelayDuration,
                            negotiationUnit,
                            customerExtraFee,
                            specialLocationTag,
                            sourcePinCode,
                            destinationPinCode,
                            requestedVehicleVariant,
                            disabilityTag,
                            isTranslated,
                            gotoTag);

                    if (floatyView == null) {
                        startTimer();
                        showOverLayPopup();
                    }
                    sheetArrayList.add(sheetModel);
                    sheetAdapter.updateSheetList(sheetArrayList);
                    sheetAdapter.notifyItemInserted(sheetArrayList.indexOf(sheetModel));
                    updateIndicators();
                    updateProgressBars(false);
                });
            }), (rideRequestBundle.getInt("keepHiddenForSeconds", 0) * 1000L));
        } catch (Exception e) {
            firebaseLogEventWithParams("exception", "add_to_list", e.toString());
            e.printStackTrace();
        }
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

    @SuppressLint("InflateParams")
    private void showOverLayPopup() {
        firebaseLogEvent("Overlay_is_popped_up");
        if (!Settings.canDrawOverlays(getApplicationContext())) return;
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
        LayoutInflater inflater = ((LayoutInflater) getApplicationContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE));
        floatyView = inflater.inflate(R.layout.viewpager_layout_view, null);
        TextView merchantLogo = floatyView.findViewById(R.id.merchantLogo);
        if (key != null && key.equals("yatrisathiprovider")){
        merchantLogo.setText("yatri\nsathi");
            ImageView merchantLogoIcon = (ImageView) floatyView.findViewById(R.id.merchantLogoIcon);
            LinearLayout.LayoutParams  layoutParams = new LinearLayout.LayoutParams(100,100);
            layoutParams.rightMargin = 12;
            merchantLogoIcon.setLayoutParams(layoutParams);
            merchantLogo.setLayoutParams(new LinearLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT , ViewGroup.LayoutParams.WRAP_CONTENT ));
        } else if (key != null && key.equals("yatriprovider")) {
            merchantLogo.setText("Yatri Driver");
        } else if (key != null && key.equals("passcultureprovider")) {
            merchantLogo.setText("Alliance Taxis");
        }
        progressDialog = inflater.inflate(R.layout.loading_screen_overlay, null);
        apiLoader = inflater.inflate(R.layout.api_loader, null);
        View dismissLoader = progressDialog.findViewById(R.id.loaderOverlay);
        dismissLoader.setOnClickListener(view -> {
            Handler handler = new Handler(Looper.getMainLooper());
            handler.post(this::cleanUp);
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
                time++;
                new Handler(Looper.getMainLooper()).post(() -> {
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

    public void firebaseLogEvent(String event) {
        Bundle params = new Bundle();
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        mFirebaseAnalytics.logEvent(event, params);
    }

    public void firebaseLogEventWithParams(String event, String paramKey, String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey, paramValue);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        mFirebaseAnalytics.logEvent(event, params);
    }

    private Boolean driverRespondApi(String searchRequestId, double offeredPrice, boolean isAccept, int slotNumber) {
        StringBuilder result = new StringBuilder();
        Handler handler = new Handler(Looper.getMainLooper());
        sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String regToken = sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null");
        String baseUrl = sharedPref.getString("BASE_URL", "null");
        String bundle_version = sharedPref.getString("BUNDLE_VERSION", "null");
        String version = sharedPref.getString("VERSION_NAME", "null");
        String sessionToken = sharedPref.getString("SESSION_ID", "null");
        String deviceDetails = sharedPref.getString("DEVICE_DETAILS", "null");
        String vehicleVariant = sharedPref.getString("VEHICLE_VARIANT", null);
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
            connection.setRequestProperty("session_id", sessionToken);
            connection.setRequestProperty("x-device", deviceDetails);
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
                if (errorPayload.has(getResources().getString(R.string.ERROR_MESSAGE))) {
                    handler.post(() -> {
                        try {
                            Toast.makeText(getApplicationContext(), errorPayload.getString(getString(R.string.ERROR_MESSAGE)), Toast.LENGTH_SHORT).show();
                        } catch (JSONException e) {
                            e.printStackTrace();
                        }
                    });
                }
            } else {
                //API Success
                return true;
            }
            return false;
        } catch (SocketTimeoutException e) {
            handler.post(() -> Toast.makeText(getApplicationContext(), "Request Timeout", Toast.LENGTH_SHORT).show());
            return false;
        } catch (Exception e) {
            firebaseLogEventWithParams("exception", "driver_respond_api", e.toString());
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

    private void startLoader(String id) {
        try {
            if (mediaPlayer != null) {
                if (mediaPlayer.isPlaying()) {
                    mediaPlayer.pause();
                }
            }
            if (floatyView != null) {
                if (floatyView.getParent() != null) {
                    windowManager.removeView(floatyView);
                }
            }
            if (apiLoader != null) {
                if (apiLoader.getParent() != null) {
                    windowManager.removeView(apiLoader);
                }
            }
            if (progressDialog != null) {
                windowManager.addView(progressDialog, params);
            }
            if (countDownTimer != null) {
                countDownTimer.cancel();
            }
            rideStatusListener = new CountDownTimer(getResources().getInteger(R.integer.LOADER_WAITING_TIME), 1000) {
                @SuppressLint("SetTextI18n")
                @Override
                public void onTick(long millisUntilFinished) {
                    sharedPref = getApplication().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    if (progressDialog != null) {
                        TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
                        loaderText.setText(getString(R.string.waiting_for_customer_response) + " (" + (millisUntilFinished / 1000) + ") ...");
                    }
                    if (sharedPref.getString(getResources().getString(R.string.RIDE_STATUS), "null").equals(getResources().getString(R.string.DRIVER_ASSIGNMENT))) {
                        sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                        showAcknowledgement(getString(R.string.DRIVER_ASSIGNMENT));
                    } else if (sharedPref.getString(getString(R.string.CLEAR_FARE), "null").equals(id)) {
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
            firebaseLogEventWithParams("exception", "start_loader", e.toString());
            cleanUp();
            e.printStackTrace();
        }
    }

    private void showAcknowledgement(String ackType) {
        Handler handler = new Handler(Looper.getMainLooper());
        String ackText = ackType.equals(getString(R.string.DRIVER_ASSIGNMENT)) ? getString(R.string.ride_assigned) : getString(R.string.ride_assigned_to_another_driver);
        String vehicleVariant = sharedPref.getString("VEHICLE_VARIANT", null);
        int rawResource;
        if (ackType.equals(getString(R.string.DRIVER_ASSIGNMENT))){
            if (key != null && key.equals("yatriprovider") && vehicleVariant.equals("AUTO_RICKSHAW")) {
                rawResource = R.raw.yatri_auto_accepted_lottie;
            }
            else
                rawResource = R.raw.ride_accepted_lottie;
        }
        else{
            if (key != null && key.equals("yatriprovider") && vehicleVariant.equals("AUTO_RICKSHAW")) {
                rawResource = R.raw.yatri_auto_declined;
            }
            else
                rawResource = R.raw.accepted_by_another_driver_lottie;
        }

        handler.post(() -> {
            if (progressDialog != null) {
                TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
                if (key != null && key.equals("yatrisathiprovider")) {
                    ImageView loader = progressDialog.findViewById(R.id.image_view_waiting);
                    loader.setImageResource(R.drawable.ic_ride_assigned);
                }
                LottieAnimationView lottieAnimationView = progressDialog.findViewById(R.id.lottie_view_waiting);
                loaderText.setText(ackText);
                if (lottieAnimationView.getVisibility() != View.GONE) {
                    lottieAnimationView.setAnimation(rawResource);
                    lottieAnimationView.setProgress(0);
                    lottieAnimationView.setSpeed(1.2f);
                    lottieAnimationView.playAnimation();
                }
                rideStatusListener.cancel();
            }
        });
        handler.postDelayed(this::cleanUp, 1700);
    }

    private void startApiLoader() {
        try {
            if (mediaPlayer != null) {
                if (mediaPlayer.isPlaying()) {
                    mediaPlayer.pause();
                }
            }
            if (apiLoader != null) {
                windowManager.addView(apiLoader, params);
            }
        } catch (Exception e) {
            firebaseLogEventWithParams("exception", "start_api_loader", e.toString());
            e.printStackTrace();
        }
    }

    private void increaseVolume() {
        AudioManager audio = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
        float currentVolume = (float) audio.getStreamVolume(AudioManager.STREAM_MUSIC);
        int maxVolume = audio.getStreamMaxVolume(AudioManager.STREAM_MUSIC);
        if (currentVolume / maxVolume < 0.7) {
            try{
                audio.setStreamVolume(AudioManager.STREAM_MUSIC, (int) (maxVolume * 0.9), AudioManager.ADJUST_SAME);
            }catch (Exception e){
                RideRequestUtils.firebaseLogEventWithParams("exception", "increase_volume", Objects.requireNonNull(e.getMessage()).substring(0, 40), this);
            }
        }
    }

    private void updateIndicators() {
        mainLooper.post(() -> {
            if (floatyView == null || viewPager == null) return;
            indicatorText1 = floatyView.findViewById(R.id.indicatorText1);
            indicatorText2 = floatyView.findViewById(R.id.indicatorText2);
            indicatorText3 = floatyView.findViewById(R.id.indicatorText3);
            vehicleText1 = floatyView.findViewById(R.id.variant1);
            vehicleText2 = floatyView.findViewById(R.id.variant2);
            vehicleText3 = floatyView.findViewById(R.id.variant3);
            progressIndicator1 = floatyView.findViewById(R.id.progress_indicator_1);
            progressIndicator2 = floatyView.findViewById(R.id.progress_indicator_2);
            progressIndicator3 = floatyView.findViewById(R.id.progress_indicator_3);
            indicatorTextList = new ArrayList<>(Arrays.asList(indicatorText1, indicatorText2, indicatorText3));
            vehicleVariantList = new ArrayList<>(Arrays.asList(vehicleText1, vehicleText2, vehicleText3));
            progressIndicatorsList = new ArrayList<>(Arrays.asList(progressIndicator1, progressIndicator2, progressIndicator3));
            indicatorList = new ArrayList<>(Arrays.asList(
                    floatyView.findViewById(R.id.indicator1),
                    floatyView.findViewById(R.id.indicator2),
                    floatyView.findViewById(R.id.indicator3)));
            indicatorTip1 = floatyView.findViewById(R.id.tip_view_0);
            indicatorTip2 = floatyView.findViewById(R.id.tip_view_1);
            indicatorTip3 = floatyView.findViewById(R.id.tip_view_2);
            shimmerTip1 = floatyView.findViewById(R.id.shimmer_view_container_0);
            shimmerTip2 = floatyView.findViewById(R.id.shimmer_view_container_1);
            shimmerTip3 = floatyView.findViewById(R.id.shimmer_view_container_2);
            tipsList = new ArrayList<>(Arrays.asList(indicatorTip1, indicatorTip2, indicatorTip3));
            shimmerTipList = new ArrayList<>(Arrays.asList(shimmerTip1, shimmerTip2, shimmerTip3));
            for (int i = 0; i < 3; i++) {
                if (viewPager.getCurrentItem() == indicatorList.indexOf(indicatorList.get(i))) {
                    indicatorList.get(i).setBackgroundColor(getColor(R.color.grey900));
                    progressIndicatorsList.get(i).setTrackColor(getColor(R.color.white));
                    shimmerTipList.get(i).stopShimmer();
                } else {
                    indicatorList.get(i).setBackgroundColor(getColor(R.color.white));
                    progressIndicatorsList.get(i).setTrackColor(getColor(R.color.grey900));
                    shimmerTipList.get(i).startShimmer();
                }
                if (i < sheetArrayList.size()) {
                    vehicleVariantList.get(i).setVisibility(View.VISIBLE);
                    String variant = sheetArrayList.get(i).getRequestedVehicleVariant();
                    if (variant == NotificationUtils.NO_VARIANT || key.equals("yatrisathiprovider")){
                        vehicleVariantList.get(i).setVisibility(View.GONE);
                    }else {
                        vehicleVariantList.get(i).setText(variant);
                        if (variant == "AC Taxi") {
                            vehicleVariantList.get(i).setTextColor(getColor(R.color.blue800));
                        } else if (variant == "Non AC") {
                            vehicleVariantList.get(i).setTextColor(getColor(R.color.orange900));
                        } else {
                            vehicleVariantList.get(i).setTextColor(getColor(R.color.Black800));
                        }
                        vehicleVariantList.get(i).setText(variant);
                    }
                    indicatorTextList.get(i).setText(sharedPref.getString("CURRENCY", "₹") + (sheetArrayList.get(i).getBaseFare() + sheetArrayList.get(i).getUpdatedAmount()));
                    progressIndicatorsList.get(i).setVisibility(View.VISIBLE);

                    if (viewPager.getCurrentItem() == indicatorList.indexOf(indicatorList.get(i)) && sheetArrayList.get(i).getCustomerTip() > 0) {
                        indicatorList.get(i).setBackgroundColor(Color.parseColor("#FEEBB9"));
                    }

                    if (sheetArrayList.get(i).getCustomerTip() > 0) {
                        tipsList.get(i).setVisibility(View.VISIBLE);
                    } else {
                        tipsList.get(i).setVisibility(View.INVISIBLE);
                    }
                } else {
                    indicatorTextList.get(i).setText("--");
                    vehicleVariantList.get(i).setVisibility(View.GONE);
                    progressIndicatorsList.get(i).setVisibility(View.GONE);
                    tipsList.get(i).setVisibility(View.INVISIBLE);
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

        for (int i = 0; i < 3; i++) {
            int finalI = i;
            indicatorList.get(i).setOnClickListener(view -> mainLooper.post(() -> {
                if (viewPager == null) return;
                viewPager.setCurrentItem(finalI);
                if (!(finalI >= sheetArrayList.size())) { //index exists
                    firebaseLogEventWithParams("indicator_click", "index", String.valueOf(finalI));
                }
            }));
        }
    }

    private void updateProgressBars(boolean animated) {
        try {
            if (floatyView == null) return;
            progressIndicatorsList = new ArrayList<>(Arrays.asList(
                    floatyView.findViewById(R.id.progress_indicator_1),
                    floatyView.findViewById(R.id.progress_indicator_2),
                    floatyView.findViewById(R.id.progress_indicator_3)));

            for (int i = 0; i < sheetArrayList.size(); i++) {
                int progressCompat = sheetArrayList.get(i).getReqExpiryTime() + sheetArrayList.get(i).getStartTime() - time;
                progressIndicatorsList.get(i).setProgressCompat(progressCompat * 4, animated); // (100/maxExpiryTime)
                if (progressCompat <= 8) {
                    progressIndicatorsList.get(i).setIndicatorColor(getColor(R.color.red900));
                } else {
                    progressIndicatorsList.get(i).setIndicatorColor(getColor(R.color.green900));
                }
            }
        } catch (Exception e) {
            mFirebaseAnalytics.logEvent("Exception_in_updateProgressBars", null);
            Log.e("OverlaySheetService", "Error in updateProgressBars " + e);
        }
    }

    private boolean findCardById(String id) {
        try {
            for (int i = 0; i < sheetArrayList.size(); i++) {
                if (id.equals(sheetArrayList.get(i).getSearchRequestId())) {
                    return true;
                }
            }
        } catch (Exception e) {
            mFirebaseAnalytics.logEvent("Exception_in_findCardById", null);
            Log.e("OverlaySheetService", "Error in findCardById " + e);
            return false;
        }
        return false;
    }

    private void updateSharedPreferences() {
        Context context = getApplicationContext();
        SharedPreferences sharedPreferences = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        sharedPreferences.edit().putString(getString(R.string.LOCAL_STAGE), getString(R.string.RideRequested)).apply();
        SimpleDateFormat formatter = new SimpleDateFormat("EE MMM d y H:m:s ZZZ", new Locale("en", "US"));
        String dateString = formatter.format(new Date());
        sharedPref.edit().putString(getString(R.string.RIDE_REQUEST_TIME), dateString).apply();
    }

    public class OverlayBinder extends Binder {
        public OverlaySheetService getService() {
            return OverlaySheetService.this;
        }
    }

}
