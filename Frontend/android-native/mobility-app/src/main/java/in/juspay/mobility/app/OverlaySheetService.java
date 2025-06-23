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
import static in.juspay.mobility.app.NotificationUtils.INTERCITY;
import static in.juspay.mobility.app.NotificationUtils.RENTAL;
import static in.juspay.mobility.app.RideRequestUtils.getRideRequestSound;
import static in.juspay.mobility.app.RideRequestUtils.getRideRequestSoundId;
import static in.juspay.mobility.app.RideRequestUtils.increaseVolume;

import android.annotation.SuppressLint;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.res.ColorStateList;
import android.graphics.Color;
import android.graphics.PixelFormat;
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
import com.google.firebase.crashlytics.FirebaseCrashlytics;

import org.json.JSONArray;
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
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;
import in.juspay.mobility.app.callbacks.CallBack;
import in.juspay.mobility.app.dataModel.VariantConfig;
import in.juspay.mobility.common.services.MobilityAPIResponse;
import in.juspay.mobility.common.services.MobilityCallAPI;
import in.juspay.mobility.common.services.TLSSocketFactory;

public class OverlaySheetService extends Service implements View.OnTouchListener {

    private static final String TAG = "OverlaySheetService";
    private static final String TAG_MEDIA_PLAYER = "OverlaySheetService:MediaPlayer";

    private static final int MAX_RIDE_REQUESTS = 6;

    private boolean isServiceDestroyed = false;

    private static final ArrayList<CallBack> callBack = new ArrayList<>();
    private final ArrayList<SheetModel> sheetArrayList = new ArrayList<>();
    private final Handler mainLooper = new Handler(Looper.getMainLooper());
    ExecutorService executor = Executors.newSingleThreadExecutor();
    @Nullable
    private ViewPager2 viewPager;

    private Timer countDownTimer;
    private WindowManager windowManager;
    private SharedPreferences sharedPref;
    private final MediaPlayer[] mediaPlayers = new MediaPlayer[3];
    @Nullable
    private volatile MediaPlayer currentMediaPlayer;
    private volatile int currentMediaIndex = -1;

    private int time = 0, retryAddViewCount = 10;;
    private View progressDialog, apiLoader, floatyView;
    private CountDownTimer rideStatusListener;
    private WindowManager.LayoutParams params;
    private FirebaseAnalytics mFirebaseAnalytics;
    private Boolean isRideAcceptedOrRejected = false;
    private TextView indicatorText1, indicatorText2, indicatorText3, indicatorText4, indicatorText5, indicatorText6, vehicleText1, vehicleText2, vehicleText3, vehicleText4, vehicleText5, vehicleText6;
    private TextView tipBanner1, tipBanner2, tipBanner3, tipBanner4, tipBanner5, tipBanner6;
    private ShimmerFrameLayout shimmerTip1, shimmerTip2, shimmerTip3, shimmerTip4, shimmerTip5, shimmerTip6;
    private LinearProgressIndicator progressIndicator1, progressIndicator2, progressIndicator3, progressIndicator4, progressIndicator5, progressIndicator6;
    private ArrayList<TextView> indicatorTextList;
    private ArrayList<LinearProgressIndicator> progressIndicatorsList;
    private ArrayList<LinearLayout> indicatorList;
    private ArrayList<TextView> indicatorTipBannerList;
    private ArrayList<LinearLayout> indicatorTipList;
    private ArrayList<ShimmerFrameLayout> shimmerTipList;
    private String key = "";
    private String DUMMY_FROM_LOCATION = "dummyFromLocation";
    private static MobilityRemoteConfigs remoteConfigs = new MobilityRemoteConfigs(false, true);
    private SheetModel modelForLogs;
    private final String BRAND_VIVO = "vivo";
    private final String NON_AC = "Non AC";
    private final String AC_TAXI = "AC Taxi";
    private Context context;
    private ExecutorService mediaPlayerExecutor = Executors.newSingleThreadExecutor();


    @Override
    public void onCreate() {
        super.onCreate();
        if (mediaPlayerExecutor.isTerminated() || mediaPlayerExecutor.isShutdown()) mediaPlayerExecutor = Executors.newSingleThreadExecutor();
        context = getApplicationContext();
        key = context.getResources().getString(R.string.service);
        isServiceDestroyed = false;
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
            boolean showSpecialLocationTag = model.getSpecialZonePickup();
            String searchRequestId = model.getSearchRequestId();
//            boolean showVariant =  !model.getRequestedVehicleVariant().equals(NO_VARIANT) && model.isDowngradeEnabled() && RideRequestUtils.handleVariant(holder, model, this);
            boolean updateTags = model.getCustomerTip() > 0 || model.getDisabilityTag() || searchRequestId.equals(DUMMY_FROM_LOCATION) || model.isGotoTag() || showSpecialLocationTag || model.isFavourite() || model.getStops() > 0 || model.getRoundTrip() || model.getCoinsForGoldTierRide() > 0 || model.getCongestionCharges() > 0 || model.getPetCharges() > 0;
            if (updateTags) {
                if (showSpecialLocationTag && (model.getDriverDefaultStepFee() == model.getOfferedPrice())) {
                    holder.specialLocExtraTip.setText(model.getCurrency() + model.getDriverDefaultStepFee());
                    holder.specialLocExtraTip.setVisibility(View.VISIBLE);
                } else {
                    holder.specialLocExtraTip.setVisibility(View.GONE);
                }
                holder.tagsBlock.setVisibility(View.VISIBLE);
                holder.pointsTagText.setText(model.getCoinsForGoldTierRide() + " " + getString(R.string.points));
                holder.pointsTag.setVisibility(model.getCoinsForGoldTierRide() > 0 ? View.VISIBLE : View.GONE);
                holder.accessibilityTag.setVisibility(model.getDisabilityTag() ? View.VISIBLE : View.GONE);
                holder.specialLocTag.setVisibility(showSpecialLocationTag ? View.VISIBLE : View.GONE);
                holder.customerTipText.setText(sharedPref.getString("CURRENCY", "₹") + " " + model.getCustomerTip() + " " + getString(R.string.tip));
                holder.customerTipTag.setVisibility(model.getCustomerTip() > 0 ? View.VISIBLE : View.GONE);
                holder.petTagText.setText(sharedPref.getString("CURRENCY", "₹") + " " + model.getPetCharges() + " " + getString(R.string.pet));
                holder.petTag.setVisibility(model.getPetCharges() > 0 ? View.VISIBLE : View.GONE);
                holder.congestionTagText.setText(model.getCongestionCharges() + " " + getString(R.string.extra));
                holder.congestionTag.setVisibility(model.getCongestionCharges() > 0 ? View.VISIBLE : View.GONE);
                holder.isFavouriteTag.setVisibility(model.isFavourite() ? View.VISIBLE : View.GONE);
                holder.testRequestTag.setVisibility(searchRequestId.equals(DUMMY_FROM_LOCATION) ? View.VISIBLE : View.GONE);
                holder.gotoTag.setVisibility(model.isGotoTag() ? View.VISIBLE : View.GONE);
                holder.reqButton.setTextColor(model.isGotoTag() ? getColor(R.color.yellow900) : getColor(R.color.white));
                holder.reqButton.setBackgroundTintList(model.isGotoTag() ?
                        ColorStateList.valueOf(getColor(R.color.Black900)) :
                        ColorStateList.valueOf(getColor(R.color.green900)));
                // Not required.
                holder.rideTypeTag.setVisibility(View.GONE);
                holder.stopsTag.setVisibility(model.getStops() > 0 ? View.VISIBLE : View.GONE);
                holder.stopsTagText.setText(getString(R.string.stops, model.getStops()));
                holder.roundTripRideTypeTag.setVisibility(model.getRoundTrip() ? View.VISIBLE : View.GONE);
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
            modelForLogs = model;
            String x = payloads.size() > 0 ? (String) payloads.get(0) : "";
            if (x.equals("inc")) {
                updateIndicators();
                holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
                holder.currency.setText(String.valueOf(model.getCurrency()));
                updateIncreaseDecreaseButtons(holder, model);
                RideRequestUtils.updateRateView(holder, model);
                return;
            }

            holder.pickUpDistance.setText(model.getPickUpDistance() + " km ");
            holder.baseFare.setText(String.valueOf(model.getBaseFare() + model.getUpdatedAmount()));
            holder.currency.setText(String.valueOf(model.getCurrency()));
            holder.distanceToBeCovered.setText(model.getDistanceToBeCovered() + " km");
            holder.tollTag.setVisibility(model.getTollCharges() > 0? View.VISIBLE : View.GONE);
            RideRequestUtils.handleDurationToPickup(holder, model, mainLooper, OverlaySheetService.this);
            holder.sourceArea.setText(model.getSourceArea());
            holder.sourceAddress.setText(model.getSourceAddress());
            holder.destinationArea.setText(model.getDestinationArea());
            holder.destinationAddress.setText(model.getDestinationAddress());
            holder.stopsInfo.setText(getString(model.getTollCharges() > 0 ? R.string.stops : R.string.stops_info , model.getStops()));
            holder.stopsInfo.setVisibility(model.getStops() > 0? View.VISIBLE : View.GONE);
            holder.textIncPrice.setText(String.valueOf(model.getNegotiationUnit()));
            holder.textDecPrice.setText(String.valueOf(model.getNegotiationUnit()));
            handlePinCode(model, holder);
            if (model.getspecialLocationTag() != null) {
                RideRequestUtils.setSpecialZoneAttrs(holder, model.getspecialLocationTag(), OverlaySheetService.this);
            }
            sharedPref = getApplication().getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String useMLKit = sharedPref.getString("USE_ML_TRANSLATE", "false");
            if (useMLKit.equals("false") && !model.isTranslated()) RideRequestUtils.updateViewFromMlTranslation(holder, model, sharedPref, OverlaySheetService.this);
            updateAcceptButtonText(holder, model.getRideRequestPopupDelayDuration(), model.getStartTime(), model.isGotoTag() ? getString(R.string.accept_goto) : getString(R.string.accept_offer));
            RideRequestUtils.updateStepFeeAndButtonAlpha(holder, model, mainLooper);
            updateIncreaseDecreaseButtons(holder, model);
            updateTagsView(holder, model);
            RideRequestUtils.updateRateView(holder, model);
            RideRequestUtils.updateTierAndAC(holder, model, OverlaySheetService.this);
            RideRequestUtils.updateRentalView(holder, model, OverlaySheetService.this);
            RideRequestUtils.updateIntercityView(holder, model, OverlaySheetService.this);
            RideRequestUtils.updateExtraChargesString(holder, model, OverlaySheetService.this);
            requestButtonClickListener(holder, model, position);
            rejectButtonClickListener(holder, model, position);
            increaseButtonClickListener(holder, model, position);
            decreaseButtonClickListener(holder, model, position);
            pagerCallbackListener();
        }
    });

    private void pagerCallbackListener() {
        viewPager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                updateMediaPlayer(position);
                NotificationUtils.firebaseLogEventWithParams(context,"multiple_ride_selected", "position", position + "");
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

    private void decreaseButtonClickListener(SheetAdapter.SheetViewHolder holder, SheetModel model, int position) {
        holder.buttonDecreasePrice.setOnClickListener(view -> {
            if (model.getOfferedPrice() > 0) {
                if (model.getSpecialZonePickup() && model.getOfferedPrice() >= model.getDriverDefaultStepFee()){
                    holder.specialLocExtraTip.setVisibility(View.GONE);
                }
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
    }

    private void increaseButtonClickListener(SheetAdapter.SheetViewHolder holder, SheetModel model, int position) {
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
    }

    private void rejectButtonClickListener(SheetAdapter.SheetViewHolder holder, SheetModel model, int position) {
        holder.rejectButton.setOnClickListener(view -> {
            ExecutorService executor = Executors.newSingleThreadExecutor();
            Handler handler = new Handler(Looper.getMainLooper());
            executor.execute(() -> {
                try {
                    if (model.getSearchRequestId().equals(DUMMY_FROM_LOCATION)){
                        respondDummyRequest();
                        removeCard(position);
                        return;
                    }
                    new Thread(() -> driverRespondApi(model, false, sheetArrayList.indexOf(model))).start();
                    isRideAcceptedOrRejected = true;
                    holder.rejectButton.setClickable(false);
                    handler.post(() -> {
                        String logEvent = sharedPref.getString("DRIVER_STATUS_N", "null").equals("Silent") ? "silent_ride_declined" : "ride_declined";
                        String event = sharedPref.getString("DRIVER_STATUS_N", "null").equals("Silent") ? "silent_ride_rejected" : "ride_rejected";
                        Utils.logEvent (logEvent, context);
                        RideRequestUtils.addRideReceivedEvent(null,null, modelForLogs, event, OverlaySheetService.this);
                        removeCard(position);
                        executor.shutdown();
                        Toast.makeText(context, getString(R.string.ride_rejected), Toast.LENGTH_SHORT).show();
                    });
                } catch (Exception e) {
                    Exception exception = new Exception("Error in RejectButton " + e);
                    FirebaseCrashlytics.getInstance().recordException(exception);
                    firebaseLogEventWithParams("exception_reject_button_click", "reject_button_click", String.valueOf(e));
                    System.out.println("reject exception: " + e);
                }
            });
        });
    }

    private void requestButtonClickListener(SheetAdapter.SheetViewHolder holder, SheetModel model, int position) {
        String vehicleVariant = sharedPref.getString("VEHICLE_VARIANT", null);
        LottieAnimationView lottieAnimationView = progressDialog.findViewById(R.id.lottie_view_waiting);
        holder.reqButton.setOnClickListener(view -> {
            if (model.getSearchRequestId().equals(DUMMY_FROM_LOCATION)) {
                respondDummyRequest();
                removeCard(position);
                return;
            }
            holder.reqButton.setClickable(false);
            if (key != null && key.equals("nammayatriprovider"))
                startApiLoader();
            // Deprecated as a generic loader is used irrespective of the vehicle variant
            // if (key != null && key.equals("yatriprovider") && vehicleVariant.equals("AUTO_RICKSHAW")){
            //     lottieAnimationView.setAnimation(R.raw.yatri_circular_loading_bar_auto);
            // }
            // else if(key != null && key.equals("nammayatriprovider") && !vehicleVariant.equals("AUTO_RICKSHAW")){
            //     lottieAnimationView.setAnimation(R.raw.waiting_for_customer_lottie_cab);
            // }

            ExecutorService executor = Executors.newSingleThreadExecutor();
            Handler handler = new Handler(Looper.getMainLooper());
            executor.execute(() -> {
                try {
                    Boolean isApiSuccess = driverRespondApi(model, true, sheetArrayList.indexOf(model));
                    if (isApiSuccess) {
                        holder.reqButton.setClickable(false);
                        updateSharedPreferences();
                        for (int i = 0; i < callBack.size(); i++) {
                            callBack.get(i).driverCallBack("RIDE_REQUESTED","{}");
                        }
                        String logEvent = sharedPref.getString("DRIVER_STATUS_N", "null").equals("Silent") ? "silent_ride_accepted" : "ride_accepted";
                        firebaseLogEvent(logEvent);
                        RideRequestUtils.addRideReceivedEvent(null,null, modelForLogs, logEvent, OverlaySheetService.this);
                        isRideAcceptedOrRejected = true;
                        handler.post(() -> {
                            startLoader(model.getSearchRequestId());
                            executor.shutdown();
                        });
                    } else {
                        handler.post(() -> {
                            removeCard(position);
                            if (apiLoader != null && apiLoader.isAttachedToWindow()) {
                                windowManager.removeViewImmediate(apiLoader);
                                apiLoader = null;
                            }
                            if (!sheetArrayList.isEmpty()) {
                                holder.reqButton.setClickable(true);
                            } else {
                                cleanUp();
                                executor.shutdown();
                            }
                        });
                    }
                } catch (Exception e) {
                    Exception exception = new Exception("Error in RequestButton " + e);
                    FirebaseCrashlytics.getInstance().recordException(exception);
                    firebaseLogEventWithParams("exception_request_button_click", "request_button_click", String.valueOf(e));
                    cleanUp();
                }
            });
        });
    }

    private void handlePinCode(SheetModel model, SheetAdapter.SheetViewHolder holder) {
        if (model.getSourcePinCode() != null && model.getSourcePinCode().trim().length() > 0) {
            holder.sourcePinCode.setText(model.getSourcePinCode().trim());
            holder.sourcePinCode.setVisibility(View.VISIBLE);
        } else if (remoteConfigs.hasKey("enable_pincode") && remoteConfigs.getBoolean("enable_pincode")) {
            new Thread(() -> {
                String pincode = RideRequestUtils.getPinCodeFromRR(model.getSrcLat(), model.getSrcLng(), OverlaySheetService.this);
                mainLooper.post(() -> {
                    if (holder != null && holder.sourcePinCode != null) {
                        if (pincode != null) {
                            holder.sourcePinCode.setText(pincode);
                            holder.sourcePinCode.setVisibility(View.VISIBLE);
                        } else {
                            holder.sourceAddress.setMaxLines(2);
                            holder.sourcePinCode.setVisibility(View.GONE);
                        }
                    }
                });
            }).start();
        }
        if (model.getDestinationPinCode() != null && model.getDestinationPinCode().trim().length() > 0) {
            holder.destinationPinCode.setText(model.getDestinationPinCode());
            holder.destinationPinCode.setVisibility(View.VISIBLE);
        } else if (remoteConfigs.hasKey("enable_pincode") && remoteConfigs.getBoolean("enable_pincode")) {
            new Thread(() -> {
                String pincode = RideRequestUtils.getPinCodeFromRR(model.getDestLat(), model.getDestLng(), OverlaySheetService.this);
                mainLooper.post(() -> {
                    if (holder != null && holder.sourcePinCode != null) {
                        if (pincode != null) {
                            holder.destinationPinCode.setText(pincode);
                            holder.destinationPinCode.setVisibility(View.VISIBLE);
                        } else {
                            holder.destinationAddress.setMaxLines(2);
                            holder.destinationPinCode.setVisibility(View.GONE);
                        }
                    }
                });
            }).start();
        }
    }

    private void respondDummyRequest() {
        Handler handler = new Handler(Looper.getMainLooper());
        handler.post(() -> {
            Toast.makeText(context, getString(R.string.test_request_successful), Toast.LENGTH_SHORT).show();
        });
    }

    private void removeCard(int position) {
        try {
            Handler handler = new Handler(Looper.getMainLooper());
            handler.post(() -> {
                if (!(sheetArrayList.size() > position)) {
                    return;
                }
                if (position >= 0) {
                    sheetArrayList.remove(position);
                }
                sheetAdapter.updateSheetList(sheetArrayList);
                sheetAdapter.notifyItemRemoved(position);
                sheetAdapter.notifyItemRangeChanged(position, sheetArrayList.size());
                updateIndicators();
                if (viewPager != null) {
                    updateMediaPlayer(viewPager.getCurrentItem());
                }
                updateProgressBars(false);
                if (sheetArrayList.isEmpty()) {
                    cleanUp();
                }
            });
        } catch (Exception e) {
            Exception exception = new Exception("Error in RemovingCard " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            firebaseLogEventWithParams("exception_in_remove_card", "remove_card", String.valueOf(e));
            e.printStackTrace();
        }
    }

    private void cleanUp() {
        if (!isRideAcceptedOrRejected) {
            firebaseLogEvent("ride_ignored");
            RideRequestUtils.addRideReceivedEvent(null,null, modelForLogs, "ride_ignored", this);
        }
        try {
            retryAddViewCount = 10;
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
                    windowManager.removeViewImmediate(floatyView);
                }
            }

            if (progressDialog != null && progressDialog.isAttachedToWindow()) {
                if (progressDialog.getParent() != null) {
                    windowManager.removeViewImmediate(progressDialog);
                }
            }

            if (apiLoader != null && apiLoader.getParent() != null) {
                windowManager.removeViewImmediate(apiLoader);
            }
            floatyView = null;
            progressDialog = null;
            apiLoader = null;

            for (MediaPlayer mediaPlayer: mediaPlayers) {
                if (mediaPlayer != null && mediaPlayer.isPlaying()) {
                    mediaPlayer.pause();
                }
            }
            currentMediaPlayer = null;
            currentMediaIndex = -1;
            time = 0;
            sheetArrayList.clear();
            NotificationUtils.binder = null;
            NotificationUtils.listData = new ArrayList<>();
            this.stopSelf();
        } catch (Exception e) {
            Exception exception = new Exception("Error in CleanUp " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            firebaseLogEventWithParams("exception_in_clean_up", "clean_up", String.valueOf(e));
            Log.e("EXCEPTION", e.toString());
        }
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        try {
            if (!mediaPlayerExecutor.isShutdown() && !mediaPlayerExecutor.isTerminated()) {
                mediaPlayerExecutor.execute(() -> {
                    for (int i =0; i < 3; i++) {
                        if (mediaPlayers[i] == null) {
                            mediaPlayers[i] = MediaPlayer.create(context, getRideRequestSound(context,i));
                            mediaPlayers[i].setLooping(true);
                            mediaPlayers[i].setOnPreparedListener(mp -> mp.setWakeMode(context, PowerManager.PARTIAL_WAKE_LOCK));
                        }
                    }
                });
            }
        } catch (Exception e) {
            Exception exception = new Exception("Error in onBind " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            firebaseLogEventWithParams("exception_in_on_bind", "on_bind", String.valueOf(e));
            e.printStackTrace();
        }
        return new OverlayBinder();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        isServiceDestroyed = false;
        if (mediaPlayerExecutor.isTerminated() || mediaPlayerExecutor.isShutdown()) mediaPlayerExecutor = Executors.newSingleThreadExecutor();
        return START_STICKY;
    }

    public void addToList(Bundle rideRequestBundle) {
        try {
            Handler handler = new Handler(Looper.getMainLooper());
            handler.postDelayed(() -> executor.execute(() -> {
                String searchRequestId = rideRequestBundle.getString("searchRequestId");
                boolean isClearedReq = MyFirebaseMessagingService.clearedRideRequest.containsKey(searchRequestId);
                boolean requestAlreadyPresent = findCardById(searchRequestId);
                if (sheetArrayList.size() >= MAX_RIDE_REQUESTS || requestAlreadyPresent || isClearedReq ){
                    if (isClearedReq) MyFirebaseMessagingService.clearedRideRequest.remove(searchRequestId);
                    String event = "ride_ignored_while_adding_to_list";
                    RideRequestUtils.addRideReceivedEvent(null, rideRequestBundle,null, event, this);
                    if (sheetArrayList.size() >= MAX_RIDE_REQUESTS) event = "ride_ignored_list_full"; else if (requestAlreadyPresent) event = "ride_ignored_already_present"; else if (isClearedReq) event = "ride_ignored_already_cleared";
                    firebaseLogEvent(event);
                    return;
                }
                handler.post(() -> {
                    String searchRequestValidTill = rideRequestBundle.getString(getResources().getString(R.string.SEARCH_REQ_VALID_TILL));
                    int baseFare = rideRequestBundle.getInt(getResources().getString(R.string.BASE_FARE));
                    String currency = rideRequestBundle.getString("currency");
                    float distanceToPickup = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_PICKUP));
                    float distanceTobeCovered = (float) rideRequestBundle.getInt(getResources().getString(R.string.DISTANCE_TO_BE_COVERED));
                    int tollCharges = rideRequestBundle.getInt("tollCharges");
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
                    int coinsRewardedOnGoldTierRide = rideRequestBundle.getInt("coinsRewardedOnGoldTierRide");
                    Boolean disabilityTag = rideRequestBundle.getBoolean("disabilityTag");
                    Boolean isTranslated = rideRequestBundle.getBoolean("isTranslated");
                    int driverPickUpCharges = rideRequestBundle.getInt("driverPickUpCharges");
                    int driverDefaultStepFee = rideRequestBundle.getInt("driverDefaultStepFee") ;
                    boolean specialZonePickup = rideRequestBundle.getBoolean("specialZonePickup");
                    df.setMaximumFractionDigits(2);
                    String getCurrTime = RideRequestUtils.getCurrentUTC();
                    int calculatedTime = RideRequestUtils.calculateExpireTimer(searchRequestValidTill, getCurrTime);
                    if (sharedPref == null)
                        sharedPref = getApplication().getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    int negotiationUnit = rideRequestBundle.getInt("driverStepFeeWithCurrency", Integer.parseInt(sharedPref.getString( "NEGOTIATION_UNIT", "10")));
                    int rideRequestedBuffer = Integer.parseInt(sharedPref.getString("RIDE_REQUEST_BUFFER", "2"));
                    int customerExtraFee = rideRequestBundle.getInt("customerExtraFee");
                    int congestionCharges = rideRequestBundle.getInt("congestionCharges");
                    int petCharges = rideRequestBundle.getInt("petCharges");
                    boolean gotoTag = rideRequestBundle.getBoolean("gotoTag");
                    double srcLat = rideRequestBundle.getDouble("srcLat");
                    double srcLng = rideRequestBundle.getDouble("srcLng");
                    double destLat = rideRequestBundle.getDouble("destLat");
                    double destLng = rideRequestBundle.getDouble("destLng");
                    boolean downgradeEnabled = rideRequestBundle.getBoolean("downgradeEnabled", false);
                    int airConditioned = rideRequestBundle.getInt("airConditioned", -1);
                    int ventilator = rideRequestBundle.getInt("ventilator",-1 );
                    String vehicleServiceTier = rideRequestBundle.getString("vehicleServiceTier", null);
                    String rideProductType = rideRequestBundle.getString("rideProductType");
                    String rideDuration = String.format("%02d:%02d hr", rideRequestBundle.getInt("rideDuration") / 3600 ,( rideRequestBundle.getInt("rideDuration") % 3600 ) / 60);
                    String rideDistance = String.format("%d km",rideRequestBundle.getInt("rideDistance") / 1000);
                    String rideStartTime = rideRequestBundle.getString("rideStartTime");
                    String rideStartDate= rideRequestBundle.getString("rideStartDate");
                    String notificationSource= rideRequestBundle.getString("notificationSource");
                    boolean isThirdPartyBooking = rideRequestBundle.getBoolean("isThirdPartyBooking");
                    Boolean isFavourite = rideRequestBundle.getBoolean("isFavourite");
                    double parkingCharge = rideRequestBundle.getDouble("parkingCharge", 0);
                    int stops = rideRequestBundle.getInt("middleStopCount", 0);
                    boolean roundTrip = rideRequestBundle.getBoolean("roundTrip");
                    if (calculatedTime > rideRequestedBuffer) {
                        calculatedTime -= rideRequestedBuffer;
                    }
                    SheetModel sheetModel = new SheetModel((df.format(distanceToPickup / 1000)),
                            distanceTobeCovered,
                            tollCharges,
                            RideRequestUtils.calculateDp(durationToPickup, df),
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
                            congestionCharges,
                            petCharges,
                            specialLocationTag,
                            sourcePinCode,
                            destinationPinCode,
                            requestedVehicleVariant,
                            coinsRewardedOnGoldTierRide,
                            disabilityTag,
                            isTranslated,
                            gotoTag,
                            driverPickUpCharges,
                            srcLat,
                            srcLng,
                            destLat,
                            destLng,
                            specialZonePickup,
                            driverDefaultStepFee,
                            downgradeEnabled,
                            airConditioned,
                            ventilator,
                            vehicleServiceTier,
                            rideProductType,
                            rideDuration,
                            rideDistance,
                            rideStartTime,
                            rideStartDate,
                            notificationSource,
                            isThirdPartyBooking,
                            isFavourite,
                            parkingCharge,
                            getCurrTime,
                            stops,
                            roundTrip
                    );

                    if (!mediaPlayerExecutor.isShutdown() && !mediaPlayerExecutor.isTerminated()) {
                        mediaPlayerExecutor.execute(() -> {
                            try {
                                if (currentMediaIndex == -1) {
                                    currentMediaIndex = getRideRequestSoundId(sheetModel.getRideProductType());
                                }
                                if (currentMediaPlayer == null || !currentMediaPlayer.isPlaying()) {
                                    currentMediaPlayer = mediaPlayers[currentMediaIndex];
                                    if(currentMediaPlayer != null && !currentMediaPlayer.isPlaying()) {
                                        currentMediaPlayer.start();
                                    }
                                    if (sharedPref == null) sharedPref = getApplication().getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                                    if (sharedPref.getString("AUTO_INCREASE_VOL", "true").equals("true")){
                                        increaseVolume(context);
                                    }
                                }
                            }catch (Exception e){
                                Log.e(TAG_MEDIA_PLAYER, e.toString());
                            }

                        });
                    }

                    if (floatyView == null) {
                        startTimer();
                        showOverLayPopup(rideRequestBundle);
                    }
                    sheetArrayList.add(sheetModel);
                    sheetAdapter.updateSheetList(sheetArrayList);
                    sheetAdapter.notifyItemInserted(sheetArrayList.indexOf(sheetModel));
                    updateIndicators();
                    updateProgressBars(false);
                    RideRequestUtils.addRideReceivedEvent(null,rideRequestBundle,null,"ride_request_popped", this);
                });
            }), (rideRequestBundle.getInt("keepHiddenForSeconds", 0) * 1000L));
        } catch (Exception e) {
            Exception exception = new Exception("Error in add_to_list " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            firebaseLogEventWithParams("exception_in_add_to_list", "add_to_list", String.valueOf(e));
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
    private void showOverLayPopup(Bundle rideRequestBundle) {
        if (!Settings.canDrawOverlays(context)) return;
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
        LayoutInflater inflater = ((LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE));
        floatyView = inflater.inflate(R.layout.viewpager_layout_view, null);
        TextView merchantLogo = floatyView.findViewById(R.id.merchantLogo);
        String appName = "";
        try{
            appName = getApplicationInfo().loadLabel(getPackageManager()).toString();
        }catch (Exception e){
            e.printStackTrace();
        }
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
        } else if (appName.contains("Mana")) {
            merchantLogo.setText("Mana Yatri");
        } else if (appName.contains("Kerala")) {
            merchantLogo.setText("Kerala Savaari");
        }

        progressDialog = inflater.inflate(R.layout.loading_screen_overlay, null);
        apiLoader = inflater.inflate(R.layout.api_loader, null);
        String vehicleVariant = sharedPref.getString("VEHICLE_VARIANT", "null");
        // Deprecated as a generic loader is used irrespective of the vehicle variant
        // LottieAnimationView apiLoaderlottieAnimationView = apiLoader.findViewById(R.id.lottie_view_waiting);
        // if(key != null && key.equals("nammayatriprovider") && !vehicleVariant.equals("AUTO_RICKSHAW")){
        //     apiLoaderlottieAnimationView.setAnimation(R.raw.ic_cab_vehicle_processing);
        // }
        View dismissLoader = progressDialog.findViewById(R.id.loaderOverlay);
        dismissLoader.setOnClickListener(view -> {
            Handler handler = new Handler(Looper.getMainLooper());
            handler.post(this::cleanUp);
        });
        viewPager = floatyView.findViewById(R.id.view_pager);
        sheetAdapter.setViewPager(viewPager);
        viewPager.setAdapter(sheetAdapter);
        if(remoteConfigs.hasKey("add_view_fallback") && remoteConfigs.getBoolean("add_view_fallback")) {
            addPagerLayoutToWindow();
        }else{
            windowManager.addView(floatyView, params);
        }
        setIndicatorClickListener();
        firebaseLogEvent("Overlay_is_popped_up");
        RideRequestUtils.addRideReceivedEvent(null, rideRequestBundle,null,"overlay_is_popped_up", this);
    }

    private void addPagerLayoutToWindow(){
        String deviceMan = android.os.Build.MANUFACTURER;
        if (!deviceMan.equals(BRAND_VIVO)){
            windowManager.addView(floatyView, params);
            return;
        }
        try {
            if (retryAddViewCount > 1) {
                retryAddViewCount --;
                windowManager.addView(floatyView, params);
            }else {
                firebaseLogEvent("not_able_to_add_view_to_window");
                cleanUp();
            }
        } catch (Exception e){
            Exception exception = new Exception("Error in addPagerLayoutToWindow " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            for (View windowView : new ArrayList<>(Arrays.asList(floatyView, progressDialog, apiLoader))) {
                if (windowView != null && windowView.isAttachedToWindow()) {
                    windowManager.removeViewImmediate(windowView);
                }
            }
            addPagerLayoutToWindow();
        }
    }

    private void startTimer() {
        TimerTask countUpTimerTask = new TimerTask() {
            @Override
            public void run() {
                time++;
                new Handler(Looper.getMainLooper()).post(() -> {
                    for (int index = 0; index < sheetArrayList.size(); index++){
                        SheetModel model = sheetArrayList.get(index);
                        if (model.getReqExpiryTime() + model.getStartTime() - time < 1) {
                            removeCard(index);
                        } else {
                            SheetAdapter.SheetViewHolder holder = sheetAdapter.getHolder(index);
                            if (holder != null) {
                                updateAcceptButtonText(holder, model.getRideRequestPopupDelayDuration(), model.getStartTime(), (model.isGotoTag() ? getString(R.string.accept_goto) : getString(R.string.accept_offer)));
                            }
                        }
                    }
                    updateProgressBars(true);
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

    private Boolean driverRespondApi(SheetModel model, boolean isAccept, int slotNumber) {
        String searchRequestId = model.getSearchRequestId();
        double offeredPrice = model.getOfferedPrice();
        String notificationSource = model.getNotificationSource();
        StringBuilder result = new StringBuilder();
        Handler handler = new Handler(Looper.getMainLooper());
        sharedPref = getApplication().getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
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
            payload.put("notificationSource", notificationSource);
            payload.put("renderedAt", model.getRenderedAt());
            payload.put("respondedAt", RideRequestUtils.getCurrentUTC());
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
                            Toast.makeText(context, errorPayload.getString(getString(R.string.ERROR_MESSAGE)), Toast.LENGTH_SHORT).show();
                        } catch (JSONException e) {
                            Exception exception = new Exception("Error in response respondAPI " + e);
                            FirebaseCrashlytics.getInstance().recordException(exception);
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
            Exception exception = new Exception("Error in SocketTimeOut " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            handler.post(() -> Toast.makeText(context, "Request Timeout", Toast.LENGTH_SHORT).show());
            return false;
        } catch (Exception e) {
            Exception exception = new Exception("Error in OverALLRespondAPI " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            firebaseLogEventWithParams("exception_in_driver_respond_api", "driver_respond_api", String.valueOf(e));
            return false;
        }
    }

    @Override
    public void onDestroy() {
        isServiceDestroyed = true;
        if (floatyView != null) {
            try {
                windowManager.removeViewImmediate(floatyView);
                for (MediaPlayer mediaPlayer: mediaPlayers) {
                    if (mediaPlayer != null) mediaPlayer.release();
                }
                currentMediaPlayer = null;
                currentMediaIndex = -1;
            } catch (Exception e) {
                e.printStackTrace();
            }
            floatyView = null;
        }
        mediaPlayerExecutor.shutdown();
        super.onDestroy();
    }

    @Override
    public boolean onTouch(View view, MotionEvent motionEvent) {
        view.performClick();
        return true;
    }

    private void startLoader(String id) {
        try {
            if (currentMediaPlayer != null) {
                if (currentMediaPlayer.isPlaying()) {
                    currentMediaPlayer.pause();
                }
            }
            if (floatyView != null) {
                if (floatyView.getParent() != null) {
                    windowManager.removeViewImmediate(floatyView);
                }
            }
            if (apiLoader != null) {
                if (apiLoader.getParent() != null) {
                    windowManager.removeViewImmediate(apiLoader);
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
                    sharedPref = getApplication().getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    if (progressDialog != null) {
                        TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
                        loaderText.setText(getString(R.string.waiting_for_customer_response) + " (" + (millisUntilFinished / 1000) + ") ...");
                    }
                    if (sharedPref.getString(getResources().getString(R.string.RIDE_STATUS), "null").equals(getResources().getString(R.string.DRIVER_ASSIGNMENT))) {
                        sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                        showAcknowledgement(getString(R.string.DRIVER_ASSIGNMENT));
                    } else if (sharedPref.getString(getString(R.string.CLEAR_FARE), "null").equals(id)) {
                        showAcknowledgement(getString(R.string.CLEAR_FARE));
                    } else {
                        boolean poolingCondition = remoteConfigs.hasKey("pool_for_ride") && remoteConfigs.getBoolean("pool_for_ride") && (((millisUntilFinished / 1000) % 3) == 0);
                        if (poolingCondition){
                            checkDriverRideList();
                        }
                    }
                }

                @Override
                public void onFinish() {
                    sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                    cleanUp();
                }
            }.start();
        } catch (Exception e) {
            Exception exception = new Exception("Error in StartLoader " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            firebaseLogEventWithParams("exception_in_start_loader", "start_loader", String.valueOf(e));
            cleanUp();
            e.printStackTrace();
        }
    }


    private void checkDriverRideList() {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        executor.execute(() ->
        {
            SharedPreferences sharedPref = getSharedPreferences(getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String baseUrl = sharedPref.getString("BASE_URL", "null");
            String orderUrl = baseUrl + "/driver/ride/list?limit=1&offset=0&onlyActive=true";
            try {
                MobilityCallAPI mobilityApiHandler = MobilityCallAPI.getInstance(context);
                Map<String, String> baseHeaders = mobilityApiHandler.getBaseHeaders(this);
                MobilityAPIResponse apiResponse = mobilityApiHandler.callAPI(orderUrl, baseHeaders, null, "GET", false);
                String apiResp = apiResponse.getResponseBody();
                JSONObject respOb = new JSONObject(apiResp);
                if (respOb.has("list")){
                    JSONArray jsonArray = respOb.getJSONArray("list");
                     int rideListLength = jsonArray.length();
                     if (rideListLength > 0) {
                         mainLooper.post(() -> {
                             sharedPref.edit().putString(getResources().getString(R.string.IS_RIDE_ACTIVE), "true").apply();
                             sharedPref.edit().putString(getResources().getString(R.string.RIDE_STATUS), "null").apply();
                             showAcknowledgement(getString(R.string.DRIVER_ASSIGNMENT));
                             RideRequestUtils.openApplication(this);
                             NotificationUtils.updateLocationUpdateDisAndFreq(NotificationUtils.DRIVER_ASSIGNMENT, sharedPref);
                         });
                     }
                }
                executor.shutdown();
            } catch (Exception error) {
                Exception exception = new Exception("Exception in checkDriverRideList " + error);
                FirebaseCrashlytics.getInstance().recordException(exception);
            }
        });
    }

    private void showAcknowledgement(String ackType) {
        Handler handler = new Handler(Looper.getMainLooper());
        String ackText = ackType.equals(getString(R.string.DRIVER_ASSIGNMENT)) ? getString(R.string.ride_assigned) : getString(R.string.ride_assigned_to_another_driver);
        String vehicleVariant = sharedPref.getString("VEHICLE_VARIANT", null);
        int rawResource;
        if (ackType.equals(getString(R.string.DRIVER_ASSIGNMENT))){
            // Deprecated as a generic lottie loader is used irrespective of the vehicle variant or merchant
            // if (key != null && key.equals("yatriprovider") && vehicleVariant.equals("AUTO_RICKSHAW")) {
            //     rawResource = R.raw.yatri_auto_accepted_lottie;
            // }
            // else if(key != null && key.equals("nammayatriprovider") && !vehicleVariant.equals("AUTO_RICKSHAW")){
            //     rawResource = R.raw.ride_accepted_lottie_cab;
            // }
            // else
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

        handler.post(() -> {
            if (progressDialog != null) {
                TextView loaderText = progressDialog.findViewById(R.id.text_waiting_for_customer);
                LottieAnimationView lottieAnimationView = progressDialog.findViewById(R.id.lottie_view_waiting);
                loaderText.setText(ackText);
                if (lottieAnimationView.getVisibility() != View.GONE) {
                    lottieAnimationView.setAnimation(rawResource);
                    lottieAnimationView.setProgress(0);
                    lottieAnimationView.setSpeed(1.2f);
                    lottieAnimationView.playAnimation();
                }
                if (rideStatusListener != null) rideStatusListener.cancel();
            }
        });
        handler.postDelayed(this::cleanUp, 1700);
    }

    private void startApiLoader() {
        try {
            if (currentMediaPlayer != null) {
                if (currentMediaPlayer.isPlaying()) {
                    currentMediaPlayer.pause();
                }
            }
            if (apiLoader != null) {
                windowManager.addView(apiLoader, params);
            }
        } catch (Exception e) {
            Exception exception = new Exception("Error in startAPILoader " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            firebaseLogEventWithParams("exception_in_start_api_loader", "start_api_loader", String.valueOf(e));
            e.printStackTrace();
        }
    }

    public boolean getIsServiceDestroyed() {
        return isServiceDestroyed;
    }

    private void updateIndicators() {
        mainLooper.post(() -> {
            if (floatyView == null || viewPager == null) return;
            indicatorText1 = floatyView.findViewById(R.id.indicatorText1);
            indicatorText2 = floatyView.findViewById(R.id.indicatorText2);
            indicatorText3 = floatyView.findViewById(R.id.indicatorText3);
            indicatorText4 = floatyView.findViewById(R.id.indicatorText4);
            indicatorText5 = floatyView.findViewById(R.id.indicatorText5);
            indicatorText6 = floatyView.findViewById(R.id.indicatorText6);
            progressIndicator1 = floatyView.findViewById(R.id.progress_indicator_1);
            progressIndicator2 = floatyView.findViewById(R.id.progress_indicator_2);
            progressIndicator3 = floatyView.findViewById(R.id.progress_indicator_3);
            progressIndicator4 = floatyView.findViewById(R.id.progress_indicator_4);
            progressIndicator5 = floatyView.findViewById(R.id.progress_indicator_5);
            progressIndicator6 = floatyView.findViewById(R.id.progress_indicator_6);
            indicatorTextList = new ArrayList<>(Arrays.asList(indicatorText1, indicatorText2, indicatorText3, indicatorText4, indicatorText5, indicatorText6));
            progressIndicatorsList = new ArrayList<>(Arrays.asList(progressIndicator1, progressIndicator2, progressIndicator3, progressIndicator4, progressIndicator5, progressIndicator6));
            indicatorList = new ArrayList<>(Arrays.asList(
                    floatyView.findViewById(R.id.indicator1),
                    floatyView.findViewById(R.id.indicator2),
                    floatyView.findViewById(R.id.indicator3),
                    floatyView.findViewById(R.id.indicator4),
                    floatyView.findViewById(R.id.indicator5),
                    floatyView.findViewById(R.id.indicator6)));
            indicatorTipList = new ArrayList<>(Arrays.asList(
                    floatyView.findViewById(R.id.tip_indicator_0),
                    floatyView.findViewById(R.id.tip_indicator_1),
                    floatyView.findViewById(R.id.tip_indicator_2),
                    floatyView.findViewById(R.id.tip_indicator_3),
                    floatyView.findViewById(R.id.tip_indicator_4),
                    floatyView.findViewById(R.id.tip_indicator_5)));
            tipBanner1 = floatyView.findViewById(R.id.tip_banner_view_0);
            tipBanner2 = floatyView.findViewById(R.id.tip_banner_view_1);
            tipBanner3 = floatyView.findViewById(R.id.tip_banner_view_2);
            tipBanner4 = floatyView.findViewById(R.id.tip_banner_view_3);
            tipBanner5 = floatyView.findViewById(R.id.tip_banner_view_4);
            tipBanner6 = floatyView.findViewById(R.id.tip_banner_view_5);
            indicatorTipBannerList = new ArrayList<>(Arrays.asList(tipBanner1, tipBanner2, tipBanner3, tipBanner4, tipBanner5, tipBanner6));
            shimmerTip1 = floatyView.findViewById(R.id.shimmer_view_container_0);
            shimmerTip2 = floatyView.findViewById(R.id.shimmer_view_container_1);
            shimmerTip3 = floatyView.findViewById(R.id.shimmer_view_container_2);
            shimmerTip4 = floatyView.findViewById(R.id.shimmer_view_container_3);
            shimmerTip5 = floatyView.findViewById(R.id.shimmer_view_container_4);
            shimmerTip6 = floatyView.findViewById(R.id.shimmer_view_container_5);
            shimmerTipList = new ArrayList<>(Arrays.asList(shimmerTip1, shimmerTip2, shimmerTip3, shimmerTip4, shimmerTip5, shimmerTip6));
            for (int i = 0; i < MAX_RIDE_REQUESTS; i++) {
                if (i < sheetArrayList.size()) {
                    shimmerTipList.get(i).setVisibility(View.VISIBLE);
                    updateTopBarBackground(i);
                    indicatorTextList.get(i).setText(sharedPref.getString("CURRENCY", "₹") + (sheetArrayList.get(i).getBaseFare() + sheetArrayList.get(i).getUpdatedAmount()));
                    progressIndicatorsList.get(i).setVisibility(View.VISIBLE);
                    boolean isSpecialZone = sheetArrayList.get(i).getSpecialZonePickup();
                    updateTopBar(i);
                } else {
                    indicatorTextList.get(i).setText("--");
                    indicatorList.get(i).setBackgroundColor(getColor(R.color.white));
                    indicatorTipBannerList.get(i).setText("Favourite");
                    shimmerTipList.get(i).setVisibility(View.INVISIBLE);
                    progressIndicatorsList.get(i).setVisibility(View.GONE);
                    indicatorTipBannerList.get(i).setVisibility(View.INVISIBLE);
                }
            }
        });
    }

    private void updateTopBarBackground(int i) {
        if (viewPager == null) return;
        if (viewPager.getCurrentItem() == indicatorList.indexOf(indicatorList.get(i))) {
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
                default: {
                    indicatorList.get(i).setBackgroundColor(getColor(isSpecialZone ? R.color.green100 : sheetArrayList.get(i).getCustomerTip() > 0 ? R.color.yellow200 : sheetArrayList.get(i).getCongestionCharges() > 0 ? R.color.orange100 : R.color.grey900));
                    break;
                }
            }
            progressIndicatorsList.get(i).setTrackColor(getColor(R.color.white));
        } else {
            indicatorList.get(i).setBackgroundColor(getColor(R.color.white));
            progressIndicatorsList.get(i).setTrackColor(getColor(R.color.grey900));
        }
    }

    private void updateTopBar (int i){
        boolean isSpecialZone = sheetArrayList.get(i).getSpecialZonePickup();
        String rideProductType = sheetArrayList.get(i).getRideProductType();
        int minWidth = indicatorList.get(i).getWidth();
        if (minWidth > 0) indicatorTipList.get(i).setMinimumWidth((int)(minWidth * 0.8));
        float cornerRadii = 30.0f;
        switch (rideProductType) {
            case RENTAL:
                indicatorTipBannerList.get(i).setVisibility(View.VISIBLE);
                indicatorTipBannerList.get(i).setText("Rental");
                indicatorTipBannerList.get(i).setTextColor(getColor(R.color.white));
                shimmerTipList.get(i).setBackground(DrawableUtil.createRoundedDrawable(getColor(R.color.turquoise), cornerRadii));

                break;
            case INTERCITY:
                indicatorTipBannerList.get(i).setVisibility(View.VISIBLE);
                indicatorTipBannerList.get(i).setText("Intercity");
                indicatorTipBannerList.get(i).setTextColor(getColor(R.color.white));
                shimmerTipList.get(i).setBackground(DrawableUtil.createRoundedDrawable(getColor(R.color.blue800), cornerRadii));

                break;
            case DELIVERY:
                indicatorTipBannerList.get(i).setVisibility(View.VISIBLE);
                indicatorTipBannerList.get(i).setText("Delivery");
                indicatorTipBannerList.get(i).setTextColor(getColor(R.color.Black800));
                shimmerTipList.get(i).setBackground(DrawableUtil.createRoundedDrawable(Color.parseColor("#FEEBB9"), cornerRadii));
                break;
            default:
                if (sheetArrayList.get(i).getCustomerTip() > 0 || sheetArrayList.get(i).getCongestionCharges() > 0 || isSpecialZone || sheetArrayList.get(i).isFavourite()) {
                    indicatorTipBannerList.get(i).setVisibility(View.VISIBLE);
                    indicatorTipBannerList.get(i).setText(isSpecialZone? "Zone" : (sheetArrayList.get(i).getCustomerTip() > 0 ? "TIP" : sheetArrayList.get(i).getCongestionCharges() > 0 ? sharedPref.getString("CURRENCY", "₹") + sharedPref.getString("CURRENCY", "₹") : "Favourite"));
                    indicatorTipBannerList.get(i).setTextColor(isSpecialZone ? getColor(R.color.white) : (sheetArrayList.get(i).getCustomerTip() > 0 ? getColor(R.color.black650) : getColor(R.color.white)));
                    shimmerTipList.get(i).setBackground(isSpecialZone ? DrawableUtil.createRoundedDrawable(Color.parseColor("#53BB6F"), cornerRadii) : DrawableUtil.createRoundedDrawable(sheetArrayList.get(i).getCustomerTip() > 0 ? getColor(R.color.yellow900) : sheetArrayList.get(i).getCongestionCharges() > 0 ? getColor(R.color.orange900) : getColor(R.color.blue800), cornerRadii));
                } else {
                    indicatorTipBannerList.get(i).setText("Favourite");
                    shimmerTipList.get(i).setVisibility(View.INVISIBLE);
                    indicatorTipBannerList.get(i).setVisibility(View.INVISIBLE);
                }
        }
    }

    private void setIndicatorClickListener() {
        if (viewPager == null || floatyView == null) return;
        indicatorList = new ArrayList<>(Arrays.asList(
                floatyView.findViewById(R.id.indicator1),
                floatyView.findViewById(R.id.indicator2),
                floatyView.findViewById(R.id.indicator3),
                floatyView.findViewById(R.id.indicator4),
                floatyView.findViewById(R.id.indicator5),
                floatyView.findViewById(R.id.indicator6)));

        for (int i = 0; i < MAX_RIDE_REQUESTS; i++) {
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

    /*
    * To update the audio sound based on notification type when card changes are driver swipes the cards*/
    private void updateMediaPlayer(int position) {
        if (!mediaPlayerExecutor.isTerminated() && !mediaPlayerExecutor.isShutdown()) {
            mediaPlayerExecutor.execute(() -> {
                try {
                    if (position < 0 || position >= sheetArrayList.size()) return;
                    int index = getRideRequestSoundId(sheetArrayList.get(position).getRideProductType());
                    if (index == currentMediaIndex) return;
                    if (currentMediaPlayer != null && currentMediaPlayer.isPlaying()) {
                        currentMediaPlayer.pause();
                    }
                    currentMediaIndex = index;
                    currentMediaPlayer = mediaPlayers[currentMediaIndex];
                    if (currentMediaPlayer != null && !currentMediaPlayer.isPlaying()) {
                        currentMediaPlayer.start();
                    }
                } catch (Exception e) {
                    Log.e(TAG_MEDIA_PLAYER, e.toString());
                }
            });
        }
    }

    private void updateProgressBars(boolean animated) {
        try {
            if (floatyView == null) return;
            if (progressIndicatorsList == null)
                progressIndicatorsList = new ArrayList<>(Arrays.asList(
                        floatyView.findViewById(R.id.progress_indicator_1),
                        floatyView.findViewById(R.id.progress_indicator_2),
                        floatyView.findViewById(R.id.progress_indicator_3),
                        floatyView.findViewById(R.id.progress_indicator_4),
                        floatyView.findViewById(R.id.progress_indicator_5),
                        floatyView.findViewById(R.id.progress_indicator_6)));

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
            Exception exception = new Exception("Error in UpdateProgress " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
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
            Exception exception = new Exception("Error in findCardById " + e);
            FirebaseCrashlytics.getInstance().recordException(exception);
            mFirebaseAnalytics.logEvent("Exception_in_findCardById", null);
            Log.e("OverlaySheetService", "Error in findCardById " + e);
            return false;
        }
        return false;
    }

    private void updateSharedPreferences() {
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
