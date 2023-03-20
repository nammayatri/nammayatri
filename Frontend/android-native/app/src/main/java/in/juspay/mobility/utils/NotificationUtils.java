/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import android.app.ActivityManager;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.media.AudioAttributes;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.provider.Settings;
import android.util.Log;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.Random;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import org.json.JSONException;
import org.json.JSONObject;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.TimeZone;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;

import com.google.firebase.analytics.FirebaseAnalytics;
import javax.net.ssl.HttpsURLConnection;

public class NotificationUtils extends AppCompatActivity {

    private static final String TAG = "NotificationUtils";
    public static final String DRIVER_HAS_REACHED = "DRIVER_HAS_REACHED";
    public static final String NEW_RIDE_AVAILABLE = "NEW_RIDE_AVAILABLE";
    public static final String CLEARED_FARE = "CLEARED_FARE";
    public static final String CANCELLED_SEARCH_REQUEST = "CANCELLED_SEARCH_REQUEST";
    public static final String TRIP_STARTED = "TRIP_STARTED";
    public static final String CANCELLED_PRODUCT = "CANCELLED_PRODUCT";
    public static final String DRIVER_ASSIGNMENT = "DRIVER_ASSIGNMENT";
    public static final String TRIP_FINISHED = "TRIP_FINISHED";
    public static String CHANNEL_ID = "General";
    public static String FLOATING_NOTIFICATION = "FLOATING_NOTIFICATION";
    public static Uri soundUri = null;
    public static OverlaySheetService.OverlayBinder binder;
    public static ArrayList<Bundle> listData = new ArrayList();
    private static RideRequestUtils rideRequestUtils = new RideRequestUtils();
    private static FirebaseAnalytics mFirebaseAnalytics;
    static Random rand = new Random();
    public static int notificationId = rand.nextInt(1000000);
    private static int smallIcon = R.drawable.ny_ic_launcher;
    public static MediaPlayer mediaPlayer;
    private static AudioManager audio;
    public static Bundle lastRideReq = new Bundle();

    public interface NotificationCallback {
        public void triggerPop(String id, String type);

        public void triggerPopUp(String id, String type);

        public void callFlowCustomer(String notificationType);

        public void triggerAllocationPopUp(String id, String type, JSONObject entity_payload);
    }

    private static ArrayList<NotificationCallback> notificationCallback = new ArrayList<>();

    public static void registerCallback(NotificationCallback notificationCallback) {
        NotificationUtils.notificationCallback.add(notificationCallback);
    }

    public static void deRegisterCallback(NotificationCallback notificationCallback) {
        NotificationUtils.notificationCallback.remove(notificationCallback);
    }

    public static void createNotificationChannel(Context context, String channelId) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            // channelId FLOATING_NOTIFICATION is having IMPORTANCE_DEFAULT, all other id's are having IMPORTANCE_HIGH
            NotificationChannel channel = new NotificationChannel(channelId, channelId, channelId.equals(FLOATING_NOTIFICATION) ? NotificationManager.IMPORTANCE_DEFAULT : NotificationManager.IMPORTANCE_HIGH);
            switch (channelId) {
                case TRIP_STARTED:
                    soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.notify_otp_sound);
                    break;
                case CANCELLED_PRODUCT:
                    soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.cancel_notification_sound);
                    break;
                case DRIVER_HAS_REACHED:
                    soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.driver_arrived);
                    break;
                default:
                    soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                    break;
            }

            AudioAttributes attributes = new AudioAttributes.Builder()
                    .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
                    .setUsage(AudioAttributes.USAGE_NOTIFICATION)
                    .build();
            channel.setSound(soundUri, attributes);
            context.getSystemService(NotificationManager.class).createNotificationChannel(channel);
        }
    }

    public static void showNotification(Context context, String title, String msg, JSONObject data, String imageUrl) throws JSONException {
        Intent mainActivityIntent = new Intent(context, MainActivity.class);
        mainActivityIntent.putExtra("NOTIFICATION_DATA", data.toString());
        mainActivityIntent.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent pendingIntent = PendingIntent.getActivity(context, notificationId, mainActivityIntent, PendingIntent.FLAG_IMMUTABLE);
        String notificationType = data.getString("notification_type");
        String channelId = CHANNEL_ID;
        Uri notificationSound = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
        String key = context.getString(R.string.service);
        Bundle params = new Bundle();
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
        switch (notificationType) {
            case TRIP_STARTED:
                channelId = TRIP_STARTED;
                notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.notify_otp_sound);
                mFirebaseAnalytics.logEvent(key.equals("nammayatri") ? "ny_user_ride_started" : "ride_started", params);
                break;
            case CANCELLED_PRODUCT:
                channelId = CANCELLED_PRODUCT;
                notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.cancel_notification_sound);
                if (key.equals("nammayatripartner") && msg.contains("Customer had to cancel your ride")) {
                    startMediaPlayer(context, R.raw.ride_cancelled_media);
                } else {
                    startMediaPlayer(context, R.raw.cancel_notification_sound);
                }
                mFirebaseAnalytics.logEvent(key.equals("nammayatri") ? "ny_user_ride_cancelled" : "ride_cancelled", params);
                break;
            case DRIVER_ASSIGNMENT:
                channelId = FLOATING_NOTIFICATION;
                if (key.equals(context.getString(R.string.nammayatripartner))) {
                    startMediaPlayer(context, R.raw.ride_assigned);
                }
                mFirebaseAnalytics.logEvent(key.equals("nammayatri") ? "ny_user_ride_assigned" : "driver_assigned", params);
                break;
            case TRIP_FINISHED:
                mFirebaseAnalytics.logEvent(key.equals("nammayatri") ? "ny_user_ride_completed" : "ride_completed", params);
                break;
            default:
                channelId = FLOATING_NOTIFICATION;
                notificationSound = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                break;
        }

        NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, channelId);
        mBuilder.setSmallIcon(smallIcon)
                .setContentTitle(title)
                .setContentText(msg)
                .setAutoCancel(true)
                .setPriority(NotificationManager.IMPORTANCE_MAX)
                .setContentIntent(pendingIntent)
                .setChannelId(channelId)
                .setSound(notificationSound);

        Bitmap bitmapFromUrl = getBitmapFromUrl(imageUrl);
        if (imageUrl != null && bitmapFromUrl != null) {
            mBuilder.setLargeIcon(bitmapFromUrl)
                    .setStyle(new NotificationCompat.BigPictureStyle()
                            .bigPicture(bitmapFromUrl)
                            .bigLargeIcon(null));
        }
        NotificationManagerCompat.from(context).notify(notificationId, mBuilder.build());

        notificationId++;
        if (DRIVER_ASSIGNMENT.equals(notificationType) || CANCELLED_PRODUCT.equals(notificationType)) {
            for (int i = 0; i < notificationCallback.size(); i++) {
                notificationCallback.get(i).triggerPop((data.getString("entity_ids")), (data.getString("notification_type")));
            }
        }
        if (key.equals("nammayatri") &&
                (notificationType.equals(TRIP_FINISHED)
                        || notificationType.equals(DRIVER_ASSIGNMENT)
                        || notificationType.equals(CANCELLED_PRODUCT)
                        || notificationType.equals(TRIP_STARTED))) {
            for (int i = 0; i < notificationCallback.size(); i++) {
                notificationCallback.get(i).callFlowCustomer(notificationType);
            }
        }
    }

    public static void showAllocationNotification(Context context, JSONObject data, JSONObject entity_payload) {
        try {
            String notificationType = data.getString("notification_type");
            mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
            switch (notificationType) {
                case NEW_RIDE_AVAILABLE:
                    Bundle params = new Bundle();
                    mFirebaseAnalytics.logEvent("ride_request_received", params);
                    Intent overlaySheetService = new Intent(context, OverlaySheetService.class);
                    SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    overlaySheetService.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    Bundle sheetData = new Bundle();
                    String expiryTime = "";
                    String searchRequestId = "";
                    try {
                        JSONObject addressPickUp = new JSONObject(entity_payload.get("fromLocation").toString());
                        JSONObject addressDrop = new JSONObject(entity_payload.get("toLocation").toString());
                        sheetData.putString("searchRequestId", entity_payload.getString("searchRequestId"));
                        sheetData.putString("searchRequestValidTill", entity_payload.getString("searchRequestValidTill"));
                        sheetData.putInt("baseFare", entity_payload.getInt("baseFare"));
                        sheetData.putInt("distanceToPickup", entity_payload.getInt("distanceToPickup"));
                        sheetData.putString("durationToPickup", entity_payload.getString("durationToPickup"));
                        sheetData.putInt("distanceTobeCovered", entity_payload.getInt("distance"));
                        sheetData.putString("sourceArea", addressPickUp.getString("area"));
                        sheetData.putString("destinationArea", addressDrop.getString("area"));
                        sheetData.putString("addressPickUp", addressPickUp.getString("full_address"));
                        sheetData.putString("addressDrop", addressDrop.getString("full_address"));
                        sheetData.putInt("driverMinExtraFee", entity_payload.has("driverMinExtraFee") ? entity_payload.getInt("driverMinExtraFee") : 10);
                        sheetData.putInt("driverMaxExtraFee", entity_payload.has("driverMaxExtraFee") ? entity_payload.getInt("driverMaxExtraFee") : 20);
                        sheetData.putInt("rideRequestPopupDelayDuration", entity_payload.has("rideRequestPopupDelayDuration") ? entity_payload.getInt("rideRequestPopupDelayDuration") : 0);
                        expiryTime = entity_payload.getString("searchRequestValidTill");
                        searchRequestId = entity_payload.getString("searchRequestId");
                    } catch (Exception e) {
                        Bundle overlayExceptionParams = new Bundle();
                        overlayExceptionParams.putString("search_request_id", searchRequestId);
                        overlayExceptionParams.putString("driver_id", sharedPref.getString("DRIVER_ID", "null"));
                        mFirebaseAnalytics.logEvent("exception_parsing_overlay_data", overlayExceptionParams);
                        Log.e(TAG, "Exception" + e);
                    }
                    final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                    f.setTimeZone(TimeZone.getTimeZone("IST"));
                    String currTime = f.format(new Date());
                    boolean rideReqExpired = (rideRequestUtils.calculateExpireTimer(expiryTime, currTime)) <= 1;
                    Log.e(TAG, "TimeDifference : " + (rideRequestUtils.calculateExpireTimer(expiryTime, currTime)));
                    if (rideRequestUtils.calculateExpireTimer(expiryTime, currTime) > 2) { // if valid for more than 2 seconds then only show
                        if (Settings.canDrawOverlays(context)) {
                            if (binder == null) {
                                context.startService(overlaySheetService);
                                listData.add(sheetData);
                            } else {
                                new Handler(Looper.getMainLooper()).post(new Runnable() {
                                    @Override
                                    public void run() {
                                        if (binder != null) {
                                            binder.getService().addToList(sheetData);
                                        }
                                    }
                                });
                            }
                            context.bindService(overlaySheetService, new ServiceConnection() {
                                @Override
                                public void onServiceConnected(ComponentName name, IBinder service) {
                                    if (service instanceof OverlaySheetService.OverlayBinder) {
                                        new Handler(Looper.getMainLooper()).post(new Runnable() {
                                            @Override
                                            public void run() {
                                                binder = (OverlaySheetService.OverlayBinder) service;
                                                ArrayList x = listData;
                                                listData = new ArrayList<>();
                                                for (Iterator<Bundle> it = x.iterator(); it.hasNext(); ) {
                                                    Bundle item = it.next();
                                                    binder.getService().addToList(item);
                                                }
                                            }
                                        });
                                    }
                                }

                                @Override
                                public void onServiceDisconnected(ComponentName name) {
                                    binder = null;
                                }
                            }, BIND_AUTO_CREATE);
                        } else if (overlayFeatureNotAvailable(context)) {
                            try {
                                mFirebaseAnalytics.logEvent("low_ram_device", params);
                                if (RideRequestActivity.getInstance() == null) {
                                    Intent intent = new Intent(context.getApplicationContext(), RideRequestActivity.class);
                                    intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
                                    intent.putExtras(sheetData);
                                    context.getApplicationContext().startActivity(intent);
                                } else {
                                    RideRequestActivity.getInstance().addToList(sheetData);
                                }
                                lastRideReq = new Bundle();
                                lastRideReq.putAll(sheetData);
                                lastRideReq.putBoolean("rideReqExpired", rideReqExpired);
                                startMediaPlayer(context, R.raw.allocation_request);
                                rideRequestUtils.createRideRequestNotification(context);
                            } catch (Exception e) {
                                params.putString("exception", e.toString());
                                mFirebaseAnalytics.logEvent("exception_in_opening_ride_req_activity", params);
                            }
                        }

                        Log.i("notificationCallback_size", Integer.toString(notificationCallback.size()));
                        Bundle overlayPermissionParams = new Bundle();
                        overlayPermissionParams.putString("search_request_id", searchRequestId);
                        overlayPermissionParams.putString("driver_id", sharedPref.getString("DRIVER_ID", "null"));
                        mFirebaseAnalytics.logEvent("no_overlay_permission", overlayPermissionParams);

                        for (int i = 0; i < notificationCallback.size(); i++) {
                            notificationCallback.get(i).triggerAllocationPopUp(data.getString("entity_ids"), data.getString("notification_type"), entity_payload);
                        }

                    } else {
                        System.out.println("expired notification" + " <> " + currTime + " <> " + expiryTime + " <> " + String.valueOf(rideRequestUtils.calculateExpireTimer(expiryTime, currTime)) + " <> " + searchRequestId + " <> " + sharedPref.getString("DRIVER_ID", "null"));
                        Bundle overlayParams = new Bundle();
                        overlayParams.putString("current_time", currTime);
                        overlayParams.putString("expiry_time", expiryTime);
                        overlayParams.putString("time_difference", String.valueOf(rideRequestUtils.calculateExpireTimer(expiryTime, currTime)));
                        overlayParams.putString("search_request_id", searchRequestId);
                        overlayParams.putString("driver_id", sharedPref.getString("DRIVER_ID", "null"));
                        mFirebaseAnalytics.logEvent("overlay_popup_expired", overlayParams);
                    }
                    break;

                case CLEARED_FARE:
                case CANCELLED_SEARCH_REQUEST:
                    if (binder != null) {
                        binder.getService().removeCardById(data.getString(context.getString(R.string.entity_ids)));
                    }
                    break;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static Bitmap getBitmapFromUrl(String imageUrl) {
        try {
            URL url = new URL(imageUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setDoInput(true);
            connection.connect();
            InputStream input = connection.getInputStream();
            return BitmapFactory.decodeStream(input);
        } catch (Exception e) {
            Log.e("Exception", "Error in getting notification image: " + e.getLocalizedMessage());
            return null;
        }
    }

    private static void startMediaPlayer(Context context, int mediaFile) {
        if (mediaPlayer != null) {
            mediaPlayer.stop();
            mediaPlayer = null;
        }
        audio = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
        audio.setStreamVolume(AudioManager.STREAM_MUSIC, (int) audio.getStreamMaxVolume(AudioManager.STREAM_MUSIC), AudioManager.ADJUST_SAME);
        mediaPlayer = MediaPlayer.create(context, mediaFile);
        mediaPlayer.start();
    }

    public static boolean overlayFeatureNotAvailable(Context context) {
        ActivityManager activityManager = (ActivityManager) context.getSystemService(ACTIVITY_SERVICE);
        return activityManager.isLowRamDevice();
    }
}