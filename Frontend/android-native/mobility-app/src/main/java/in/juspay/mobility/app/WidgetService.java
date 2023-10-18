/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.animation.ValueAnimator;
import android.app.ActivityManager;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.PixelFormat;
import android.media.MediaPlayer;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.provider.Settings;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.LinearInterpolator;
import android.view.animation.RotateAnimation;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.core.content.res.ResourcesCompat;
import androidx.interpolator.view.animation.FastOutLinearInInterpolator;

import com.google.android.material.progressindicator.LinearProgressIndicator;
import com.google.firebase.analytics.FirebaseAnalytics;

import org.json.JSONObject;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Queue;
import java.util.TimeZone;


public class WidgetService extends Service {
    private View widgetView;
    private WindowManager windowManager;
    private ImageView imageClose;
    private float height, width;
    private String widgetMessage;
    private int calculatedTime = 0;
    private JSONObject entity_payload, data;
    private boolean isRemovingInProcess = false;
    private final Queue<Intent> rideRequestQueue = new LinkedList<>();

    private final Bundle params = new Bundle();

    private static FirebaseAnalytics mFirebaseAnalytics;
    private int LAYOUT_FLAG;

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(getApplicationContext());
        String intentMessage = intent != null && intent.hasExtra(getResources().getString(R.string.WIDGET_MESSAGE)) ? intent.getStringExtra(getResources().getString(R.string.WIDGET_MESSAGE)) : null;
        if (intent != null && calculatedTime == 0 && intentMessage == null) {
            showSilentNotification(intent);
        } else {
            if (intentMessage != null && intentMessage.equals("CLEAR_FARE") && silentRideRequest != null && progressBar != null && dismissRequest != null && handler != null) {
                removeViewAndRequest(0);
            } else if (isRemovingInProcess) {
                rideRequestQueue.offer(intent);
            } else if (intentMessage != null && !intentMessage.equals("CLEAR_FARE")) {
                addMessageToWidget(intent);
            }
        }
        return START_STICKY;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        addWidgetToWindowManager();
    }

    private View silentRideRequest, dismissRequest, floatingWidget;

    private LinearProgressIndicator progressBar;

    private Handler handler;

    private float mAngleToRotate;

    private void showSilentNotification(Intent intent) {
        if(widgetView == null) return;
        try {
            // Fetch TextView for fare and distanceToPickup
            TextView fareTextView = widgetView.findViewById(R.id.ride_fare);
            TextView distanceTextView = widgetView.findViewById(R.id.distance_to_pickup);

            // Get Current Time in UTC
            final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", new Locale("en"));
            f.setTimeZone(TimeZone.getTimeZone("UTC"));
            String getCurrTime = f.format(new Date());


            // Fetch data from intent
            if (intent != null) {
                entity_payload = new JSONObject(intent.getStringExtra("payload"));
                String dataBuilder = intent.hasExtra("data") ? intent.getStringExtra("data") : null;
                data = dataBuilder != null ? new JSONObject(dataBuilder) : null;
            }


            if (entity_payload != null && entity_payload.has("baseFare")) {
                System.out.println("PAYLOAD + PAYLIAD " + entity_payload); // TODO:: REMOVE
                MediaPlayer mediaPlayer = MediaPlayer.create(getApplicationContext(), R.raw.silent_mode_notification_sound);
                if (mediaPlayer != null)
                    mediaPlayer.start();
                // Fetch data from entity_payload
                int fare = entity_payload.getInt("baseFare");
                int distanceToPickup = entity_payload.getInt("distanceToPickup");
                String searchRequestValidTill = entity_payload.getString("searchRequestValidTill");


                calculatedTime = calculateExpireTimer(searchRequestValidTill, getCurrTime);
                calculatedTime = Math.min(calculatedTime, 30);
                calculatedTime = calculatedTime < 0 ? 20 : calculatedTime;
                DecimalFormat df = new DecimalFormat();
                df.setMaximumFractionDigits(2);

                // Update text for fare and distanceToPickup
                SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                String fareText = sharedPref.getString("CURRENCY", "₹") + fare;
                fareTextView.setText(fareText);
                String distanceText;
                if (distanceToPickup > 1000) {
                    distanceText = (df.format(distanceToPickup / 1000)) + " km pickup";
                } else {
                    distanceText = distanceToPickup + " m pickup";
                }
                distanceTextView.setText(distanceText);

                silentRideRequest = widgetView.findViewById(R.id.silent_ride_request_background);
                if (silentRideRequest != null){
                    // Add silentRideRequest view
                    silentRideRequest.setVisibility(View.VISIBLE);

                    // Start Slide-in animation
                    silentRideRequest.setTranslationX(-1500);
                    silentRideRequest.animate().translationX(0)
                            .setInterpolator(new FastOutLinearInInterpolator())
                            .setDuration(600)
                            .start();

                    //onClick Listener for rideRequest
                    silentRideRequest.setOnClickListener(view -> {
                        if (data != null && entity_payload != null) {
                            NotificationUtils.showAllocationNotification(getApplicationContext(), data, entity_payload);
                            silentRideRequest.setVisibility(View.GONE);
                            progressBar.setVisibility(View.GONE);
                            dismissRequest.setVisibility(View.GONE);
                            calculatedTime = 0;
                        }
                    });
                }

                dismissRequest = widgetView.findViewById(R.id.dismiss_silent_reqID);
                if (dismissRequest != null) {
                    dismissRequest.setVisibility(View.VISIBLE);

                    dismissRequest.setTranslationX(-1500);
                    dismissRequest.animate().translationX(0)
                            .setInterpolator(new FastOutLinearInInterpolator())
                            .setDuration(600)
                            .start();
                }

                progressBar = widgetView.findViewById(R.id.silent_progress_indicator);
                if (progressBar != null) {
                    //Start progress bar :-
                    progressBar.setVisibility(View.VISIBLE);
                    progressBar.setIndicatorColor(getColor(R.color.green900));

                    progressBar.setTranslationX(-1500);
                    progressBar.animate().translationX(0)
                            .setInterpolator(new FastOutLinearInInterpolator())
                            .setDuration(600)
                            .start();
                }


                // Animate the floating widget
                floatingWidget = widgetView.findViewById(R.id.floating_logo);
                mAngleToRotate = 360f;
                rotationAnimation(floatingWidget, 0.0f, mAngleToRotate);
                widgetView.post(() -> {
                    try {
                    int calculatedWidth = (widgetView.getWidth() / 100) * 85;
                    int[] ar = new int[100];
                    for (int i = ar.length - 1, j = 1; i >= 0 && j <= ar.length; i--, j++) {
                        ar[i] = (calculatedWidth / 100) * j;
                    }

                    ValueAnimator anim = ValueAnimator.ofInt(ar);
                    anim.addUpdateListener(valueAnimator -> {
                        if (progressBar != null) {
                            int val = (Integer) valueAnimator.getAnimatedValue();
                            ViewGroup.LayoutParams layoutParams = progressBar.getLayoutParams();
                            if (val < calculatedWidth / 3 && val > calculatedWidth / 5) {
                                progressBar.setIndicatorColor(getColor(R.color.yellow900));
                            } else if (val < calculatedWidth / 5) {
                                progressBar.setIndicatorColor(getColor(R.color.red900));
                            } else {
                                progressBar.setIndicatorColor(getColor(R.color.green900));
                            }
                            layoutParams.width = val;
                            progressBar.setLayoutParams(layoutParams);
                        } else {
                            anim.removeAllUpdateListeners();
                            anim.end();
                        }
                    });
                    anim.setDuration((calculatedTime + 1) * 1000L);
                    anim.start();
                    } catch (Exception e) {
                        Log.e("WidgetService", "Error in showSilentNotification " + e);
                        mFirebaseAnalytics.logEvent("Exception_in_showSilentNotification",null);
                    }
                });


                //Revert Animation
                handler = new Handler();
                removeViewAndRequest(calculatedTime * 1000);

                // Adding dismiss button on widget
                dismissRequest.setOnClickListener(view -> {
                    if(silentRideRequest != null) silentRideRequest.setVisibility(View.GONE);
                    if(progressBar != null) progressBar.setVisibility(View.GONE);
                    if(dismissRequest != null) dismissRequest.setVisibility(View.GONE);
                    silentRideRequest = null;
                    progressBar = null;
                    handler.removeCallbacksAndMessages(null);
                    calculatedTime = 0;
                });
                mFirebaseAnalytics = FirebaseAnalytics.getInstance(getApplicationContext());
                mFirebaseAnalytics.logEvent("ny_silent_ride_request", params);
            }
        } catch (Exception e) {
            e.printStackTrace();
            calculatedTime = 0;
        }
    }

    private void removeViewAndRequest(int delayInMilliSeconds) {
        handler.postDelayed(() -> {
            isRemovingInProcess = true;
            if (silentRideRequest != null) {
                silentRideRequest.animate().translationX(-1500)
                        .setInterpolator(new FastOutLinearInInterpolator())
                        .setDuration(600)
                        .start();
            }
            if (progressBar != null) {
                progressBar.animate().translationX(-1500)
                        .setInterpolator(new FastOutLinearInInterpolator())
                        .setDuration(600)
                        .start();
            }

            rotationAnimation(floatingWidget, mAngleToRotate, 0.0f);

            handler.postDelayed(() -> {
                if (silentRideRequest != null && progressBar != null && dismissRequest != null) {
                    silentRideRequest.setVisibility(View.GONE);
                    progressBar.setVisibility(View.GONE);
                    dismissRequest.setVisibility(View.GONE);
                    silentRideRequest = null;
                    progressBar = null;
                    dismissRequest = null;
                    calculatedTime = 0;
                    isRemovingInProcess = false;
                    handler.removeCallbacksAndMessages(null);
                    if (rideRequestQueue.size() > 0 && rideRequestQueue.peek() != null) {
                        showSilentNotification(rideRequestQueue.poll());
                    }
                }
            }, 700);
        }, delayInMilliSeconds);
    }

    private void rotationAnimation(View view, float start, float end) {
        if (view != null) {
            RotateAnimation wheelRotation = new RotateAnimation(start, end, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f);
            wheelRotation.setDuration(600);
            wheelRotation.setInterpolator(getApplicationContext(), android.R.interpolator.accelerate_decelerate);
            view.startAnimation(wheelRotation);
        }
    }

    private int calculateExpireTimer(String expireTimeTemp, String currTimeTemp) {
        String[] arrOfA = expireTimeTemp.split("T");
        String[] arrOfB = currTimeTemp.split("T");
        if (!arrOfA[0].equals(arrOfB[0])) {
            return -1;
        }
        String[] timeTempExpire = arrOfA[1].split(":");
        String[] timeTempCurrent = arrOfB[1].split(":");
        timeTempExpire[2] = timeTempExpire[2].substring(0, 2);
        timeTempCurrent[2] = timeTempCurrent[2].substring(0, 2);
        int currTime = 0, expireTime = 0, calculate = 3600;
        for (int i = 0; i < timeTempCurrent.length; i++) {
            currTime += (Integer.parseInt(timeTempCurrent[i]) * calculate);
            expireTime += (Integer.parseInt(timeTempExpire[i]) * calculate);
            calculate = calculate / 60;
        }
        if ((expireTime - currTime) >= 5) {
            return expireTime - currTime - 5;
        }
        return 0;
    }

    private void addMessageToWidget(Intent intent) {
        LinearLayout messageView;
        TextView messageTextView;
        TextView messageHeaderView;
        String widgetMessageHeader = null;
        if (intent != null) {
            widgetMessage = intent.getStringExtra(getResources().getString(R.string.WIDGET_MESSAGE));
            widgetMessageHeader = intent.getStringExtra("sentBy");
        }
        if (widgetMessage != null) {
            if (windowManager != null) {
                messageView = widgetView.findViewById(R.id.message_view_right);
                messageHeaderView = widgetView.findViewById(R.id.messageTextView_right_header);
                messageTextView = widgetView.findViewById(R.id.messageTextView_right);
                messageView.setVisibility(View.VISIBLE);
                messageTextView.setText(widgetMessage);
                if (widgetMessageHeader != null && messageHeaderView != null) {
                    messageTextView.setVisibility(View.VISIBLE);
                    messageHeaderView.setText(widgetMessageHeader);
                    messageHeaderView.setVisibility(View.VISIBLE);
                } else {
                    if (messageHeaderView != null) {
                        messageHeaderView.setVisibility(View.GONE);
                    }
                }
                Handler handler = new Handler();
                handler.postDelayed(() -> {
                    Animation aniFade = AnimationUtils.loadAnimation(getApplicationContext(), R.anim.fade_out);
                    messageView.startAnimation(aniFade);
                    handler.postDelayed(() -> messageView.setVisibility(View.GONE), getResources().getInteger(R.integer.WIDGET_MESSAGE_ANIMATION_DURATION));
                }, getResources().getInteger(R.integer.DURATION_OF_SHOWING_MESSAGE));
            }
        }
    }

    private void addWidgetToWindowManager() {
        if (!Settings.canDrawOverlays(this)) return;
        float scale = getResources().getDisplayMetrics().density;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            LAYOUT_FLAG = WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY;
        } else {
            LAYOUT_FLAG = WindowManager.LayoutParams.TYPE_PHONE;
        }
        //inflating widgetView
        windowManager = (WindowManager) getSystemService(WINDOW_SERVICE);
        widgetView = LayoutInflater.from(this).inflate(R.layout.floating_widget_layout,null);
        DisplayMetrics dm = new DisplayMetrics();
        windowManager.getDefaultDisplay().getRealMetrics(dm);
        WindowManager.LayoutParams widgetLayoutParams = new WindowManager.LayoutParams(WindowManager.LayoutParams.WRAP_CONTENT, WindowManager.LayoutParams.WRAP_CONTENT, LAYOUT_FLAG, WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE, PixelFormat.TRANSLUCENT);

        //initial Position
        widgetLayoutParams.gravity = Gravity.TOP | Gravity.START;
        widgetLayoutParams.x = 16;
        widgetLayoutParams.y = dm.heightPixels / 4;

        //layout params for close button
        WindowManager.LayoutParams closeImageParams = new WindowManager.LayoutParams(WindowManager.LayoutParams.MATCH_PARENT, (int) (50 * scale + 0.5f), LAYOUT_FLAG, WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE, PixelFormat.TRANSLUCENT);
        closeImageParams.gravity = Gravity.BOTTOM | Gravity.CENTER;

        imageClose = new ImageView(this);
        try {
            imageClose.setImageResource(R.drawable.ny_ic_close_transparent);
        } catch (Exception e) {
            Log.e("Exception in rendering Image", e.toString());
        }
        imageClose.setPadding(0, 0, 0, (int) (10 * scale + 0.5f));
        imageClose.setVisibility(View.INVISIBLE);
        imageClose.setBackground(ResourcesCompat.getDrawable(getResources(), R.drawable.widget_close_gradient, null));
        windowManager.addView(imageClose, closeImageParams);
        windowManager.addView(widgetView, widgetLayoutParams);
        widgetView.setVisibility(View.VISIBLE);
        height = dm.heightPixels;
        width = dm.widthPixels;


        //dragMovement
        widgetView.setOnTouchListener(new View.OnTouchListener() {
            int initialX, initialY;
            float initialTouchX, initialTouchY;
            boolean isCloseEnabled = false;
            long actionDownTime;

            @Override
            public boolean onTouch(View view, MotionEvent motionEvent) {
                try {
                    view.performClick();
                    SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    switch (motionEvent.getAction()) {
                        case MotionEvent.ACTION_DOWN:
                            imageClose.setVisibility(View.GONE);
                            actionDownTime = Calendar.getInstance().getTimeInMillis();
                            initialX = widgetLayoutParams.x;
                            initialY = widgetLayoutParams.y;

                            //get touch position location
                            initialTouchX = motionEvent.getRawX();
                            initialTouchY = motionEvent.getRawY();
                            return true;

                        case MotionEvent.ACTION_UP:
                            imageClose.setVisibility(View.GONE);

                            if (isCloseEnabled) {
                                stopSelf();
                            } else {
                                ValueAnimator valueAnimator = ValueAnimator.ofFloat(widgetLayoutParams.x, 0);
                                valueAnimator.setDuration(getResources().getInteger(R.integer.WIDGET_CORNER_ANIMATION_DURATION));
                                valueAnimator.addUpdateListener(animation -> {
                                    try {
                                        widgetLayoutParams.x = Math.round((Float) animation.getAnimatedValue());
                                        windowManager.updateViewLayout(widgetView, widgetLayoutParams);
                                    } catch (Exception e) {
                                        e.printStackTrace();
                                    }
                                });
                                valueAnimator.start();

                                //click definition
                                if (Math.abs(initialTouchX - motionEvent.getRawX()) < 5 && Math.abs(initialTouchY - motionEvent.getRawY()) < 5) {
                                    long ramBasedTime = getTimeBasedOnRamSize();
                                    if (sharedPref.getString("MAPS_OPENED", "null").equals("true") || sharedPref.getString("IS_RIDE_ACTIVE", "null").equals("true")) {
                                        Handler mainLooper = new Handler(Looper.getMainLooper());
                                        minimizeApp();
                                        View loaderView = getLoaderView();
                                        mainLooper.postDelayed(() -> {
                                            loaderView.setVisibility(View.GONE);
                                            openMainActivity();
                                            }, ramBasedTime );
                                    } else {
                                        openMainActivity();
                                    }
                                }
                            }
                            return true;

                        case MotionEvent.ACTION_MOVE:
                            if (Calendar.getInstance().getTimeInMillis() - actionDownTime > 200) {
                                if (sharedPref.getString("DRIVER_STATUS_N", "null").equals("Offline")) {
                                    imageClose.setVisibility(View.VISIBLE);
                                }
                            }
                            widgetLayoutParams.x = initialX + (int) (motionEvent.getRawX() - initialTouchX);
                            widgetLayoutParams.y = initialY + (int) (motionEvent.getRawY() - initialTouchY);
                            windowManager.updateViewLayout(widgetView, widgetLayoutParams);

                            if (widgetLayoutParams.y > height * 0.85 && widgetLayoutParams.x > width * 0.30 && widgetLayoutParams.x < width * 0.55) {
                                imageClose.setImageResource(R.drawable.ny_ic_close_filled_round);
                                Vibrator vibrator = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);
                                if (isCloseEnabled) {
                                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                                        vibrator.vibrate(VibrationEffect.createOneShot(50, VibrationEffect.DEFAULT_AMPLITUDE));
                                    } else {
                                        //deprecated in API 26
                                        vibrator.vibrate(500);
                                    }
                                }
                                if (sharedPref.getString("DRIVER_STATUS_N", "null").equals("Offline")) {
                                    isCloseEnabled = true;
                                }
                            } else {
                                imageClose.setImageResource(R.drawable.ny_ic_close_transparent);
                                isCloseEnabled = false;
                            }
                            return true;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return false;
            }
        });
    }

    private View getLoaderView() {
        View loaderView = LayoutInflater.from(getApplicationContext()).inflate(R.layout.loader_mini, null);
        WindowManager.LayoutParams lp = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.WRAP_CONTENT,
                WindowManager.LayoutParams.WRAP_CONTENT,
                LAYOUT_FLAG,
                WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE,
                PixelFormat.TRANSLUCENT);
        ProgressBar loader = loaderView.findViewById(R.id.loader);
        loaderView.setVisibility(View.VISIBLE);
        windowManager.addView(loaderView, lp);
        RotateAnimation rotateAnimation = new RotateAnimation(0, 360,
                Animation.RELATIVE_TO_SELF, 0.5f,
                Animation.RELATIVE_TO_SELF, 0.5f);
        rotateAnimation.setInterpolator(new LinearInterpolator());
        rotateAnimation.setDuration(1000);
        rotateAnimation.setRepeatCount(Animation.INFINITE);
        loader.startAnimation(rotateAnimation);
        return loaderView;
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
        if (widgetView != null) {
            windowManager.removeView(widgetView);
            widgetView = null;
        }
        if (imageClose != null) {
            windowManager.removeView(imageClose);
            imageClose = null;
        }
        rideRequestQueue.clear();
    }

    private long getTimeBasedOnRamSize() {
        ActivityManager.MemoryInfo mi = new ActivityManager.MemoryInfo();
        ActivityManager activityManager = (ActivityManager) getSystemService(ACTIVITY_SERVICE);
        activityManager.getMemoryInfo(mi);
        double totalRamInGB = ((double) mi.totalMem) / 1024 / 1024 / 1024;
        long timeInMilliseconds;
        if (totalRamInGB >= 7) {
            timeInMilliseconds = 1000;
        } else {
            double ramSizeDifference = 7 - totalRamInGB;
            double timeDifference = ramSizeDifference * 500;
            timeInMilliseconds = (long) (1000 + timeDifference);
        }

        return timeInMilliseconds;
    }

    private void openMainActivity() {
        Intent intent = getPackageManager().getLaunchIntentForPackage(getPackageName());
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
        getApplicationContext().startActivity(intent);
        stopSelf();
    }

    public void minimizeApp() {
        Intent minimizeIntent = new Intent(Intent.ACTION_MAIN);
        minimizeIntent.addCategory(Intent.CATEGORY_HOME);
        minimizeIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        this.startActivity(minimizeIntent);
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}
