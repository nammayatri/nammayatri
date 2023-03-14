package in.juspay.mobility.utils;

import android.annotation.SuppressLint;
import android.app.AlarmManager;
import android.app.Notification;
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
import android.os.SystemClock;
import android.provider.Settings;
import android.util.Log;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.Random;

import in.juspay.hypersdk.core.JuspayServices;

import androidx.annotation.RequiresApi;
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

    public static String CHANNEL_ID = "General";
    public static String FLOATING_NOTIFICATION = "FLOATING_NOTIFICATION";
    public static String ALLOCATION_TYPE = "NEW_RIDE_AVAILABLE";
    public static String TRIP_CHANNEL_ID = "TRIP_STARTED";
    public static String CANCELLED_PRODUCT = "CANCELLED_PRODUCT";
    public static String DRIVER_ASSIGNMENT = "DRIVER_ASSIGNMENT";
    public static String TRIP_FINISHED = "TRIP_FINISHED" ;
    public static String RINGING_CHANNEL_ID = "RINGING_ALERT";
    public static String REALLOCATE_PRODUCT = "REALLOCATE_PRODUCT";
    public static Uri soundUri = null;
    private static boolean hasStartedService = false;
    public static OverlaySheetService.OverlayBinder binder;
    public static ArrayList<Bundle> listData = new ArrayList();

    @SuppressLint("MissingPermission")
    private static FirebaseAnalytics mFirebaseAnalytics;

    private static JuspayServices juspayServices;
    static Random rand = new Random();
    public static int notificationId = rand.nextInt(1000000);
    private static int smallIcon = R.drawable.ic_yatri_notification;
    private static MediaPlayer mediaPlayer;
    private static AudioManager audio;

    public interface NotificationCallback{
        public void triggerPop(String id,String type);
        public void triggerPopUp(String id,String type);
        public void callFlowCustomer(String notificationType);
        public void triggerAllocationPopUp(String id, String type, JSONObject entity_payload);
    }
    private static ArrayList<NotificationCallback> notificationCallback = new ArrayList<>();
    public  static void registerCallback(NotificationCallback notificationCallback)
    {
        NotificationUtils.notificationCallback.add(notificationCallback);
    }
    public static void deRegisterCallback(NotificationCallback notificationCallback)
    {
        NotificationUtils.notificationCallback.remove(notificationCallback);
    }
    public static void scheduleNotification (Context context, Notification notification , int delay) {
        Intent notificationIntent = new Intent(context, NotificationReciever.class );
        notificationIntent.putExtra(NotificationReciever.NOTIFICATION_ID, notificationId);
        notificationId++;
        notificationIntent.putExtra(NotificationReciever.NOTIFICATION, notification);
        PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0, notificationIntent, PendingIntent.FLAG_UPDATE_CURRENT);
        long futureInMillis = SystemClock.elapsedRealtime() + delay;
        AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
        assert alarmManager != null;
        alarmManager.set(AlarmManager.ELAPSED_REALTIME_WAKEUP, futureInMillis, pendingIntent);
    }

    public static Notification createNotification (Context context, String title, String msg, JSONObject data) throws JSONException {
        Bitmap bigIcon = BitmapFactory.decodeResource(context.getResources(), R.mipmap.ic_launcher);
        Intent intent= new Intent(context, MainActivity.class);
        System.out.println("Create notification"+data.toString());
        intent.putExtra("NOTIFICATION_DATA", data.toString());
//        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
        intent.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent pendingIntent = PendingIntent.getActivity(context, notificationId , intent, 0);
        String channelId = FLOATING_NOTIFICATION;
        Uri notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.allocation_request);
        if (data.has("notification_type")){
            String notificationType = new String(data.getString("notification_type"));
            if (ALLOCATION_TYPE.equals(notificationType)){
                System.out.println("created:- "+ notificationType);
                channelId = RINGING_CHANNEL_ID;
            } else if (TRIP_CHANNEL_ID.equals(notificationType) ) {
                System.out.println("created:- "+ notificationType);
                channelId = notificationType;
            }
        }
        System.out.println("created:- "+ channelId);
        Log.e("Beckn_LOG","CRETENOTIFICTION");
        NotificationCompat.Builder builder = new NotificationCompat.Builder(context, channelId)
                .setLargeIcon(bigIcon)
                .setSmallIcon(smallIcon)
                .setContentTitle(title)
                .setContentText(msg)
                .setAutoCancel(true)
                .setContentIntent(pendingIntent)
                .setChannelId(channelId)
                .setPriority(NotificationManager.IMPORTANCE_MAX);
        if (data.has("notification_type")){
            String notificationType = new String(data.getString("notification_type"));
            int calculatedTime = 20;
            if(data.has("calculated_time")){
                calculatedTime = data.getInt("calculated_time");
                if (calculatedTime < 0){
                    calculatedTime = 0;
                }
            }
            if (ALLOCATION_TYPE.equals(notificationType)){
                builder.setSound(notificationSound);
                builder.setTimeoutAfter(calculatedTime*1000);
            }
        }

        notificationId ++;
        return builder.build() ;
    }

    public static void showAllocationNotification (Context context, String title, String msg, JSONObject data, String imageUrl, JSONObject entity_payload){
        try{
            String notificationType = new String(data.getString("notification_type"));
            String channelId;
            if (ALLOCATION_TYPE.equals(notificationType)) {
                System.out.println("showNotification:- "+ notificationType);
                channelId = RINGING_CHANNEL_ID;
            }
            if (ALLOCATION_TYPE.equals(notificationType)) {
                System.out.println("In_if_in_notification before");
                Bundle params = new Bundle();
                mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                mFirebaseAnalytics.logEvent("ride_request_received",params);
                //Recieved Notification && checking for permission if overlay permission is given, if not then it will redirect to give permission

                Intent svcT = new Intent(context, OverlaySheetService.class);
                SharedPreferences sharedPref = context.getSharedPreferences(
                        context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                String token = sharedPref.getString("RegistrationToken", "null");
                String env = sharedPref.getString("ENV", "null");
                svcT.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                System.out.println("Call Service before");
                Bundle sheetData = new Bundle();
                String expiryTime = "";
                try{
                    JSONObject addressPickUp = new JSONObject(entity_payload.get("fromLocation").toString());
                    JSONObject addressDrop = new JSONObject(entity_payload.get("toLocation").toString());
                    sheetData.putString("searchRequestId", entity_payload.getString("searchRequestId"));
                    sheetData.putString("searchRequestValidTill", entity_payload.getString("searchRequestValidTill"));
                    sheetData.putDouble("baseFare", entity_payload.getDouble("baseFare"));
                    sheetData.putInt("distanceToPickup", entity_payload.getInt("distanceToPickup"));
                    sheetData.putString("durationToPickup", entity_payload.getString("durationToPickup"));
                    sheetData.putInt("distanceTobeCovered", entity_payload.getInt("distance"));
                    sheetData.putString("addressPickUp", addressPickUp.getString("full_address"));
                    expiryTime = entity_payload.getString("searchRequestValidTill");
                    System.out.println(String.valueOf(entity_payload));
                    if(addressDrop.getString("area").equals("")) {
                        sheetData.putString("addressDrop", addressDrop.getString("full_address"));
                        sheetData.putString("destinationArea", "");
                    }
                    else {
                        sheetData.putString("destinationArea", addressDrop.getString("area"));
                        sheetData.putString("addressDrop", addressDrop.getString("full_address"));
                    }
                }catch (Exception e){
                    Log.e(TAG,"Exception" + e);
                }
                final SimpleDateFormat f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
                f.setTimeZone(TimeZone.getTimeZone("IST"));
                String currTime = f.format(new Date());
                System.out.println("TimeDifference : " + (calculateTimeDifference(expiryTime, currTime)));
                if (calculateTimeDifference(expiryTime, currTime)>0){
                    if(checkPermission(context)){
                        //Starting OverlaySheetService
                        if(binder == null) {
                            listData.add(sheetData);
                        } else {
                            new Handler(Looper.getMainLooper()).post(new Runnable() {
                                @Override
                                public void run() {
                                    if (binder !=null){
                                        binder.getService().addToList(sheetData);
                                    }
                                }
                            });
                        }
                        context.bindService(svcT, new ServiceConnection() {
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
                    } else {
                        Log.i("notificationCallback_size", Integer.toString(notificationCallback.size()));
                        if(ALLOCATION_TYPE.equals(notificationType)){
                            for(int i =0;i<notificationCallback.size();i++){
                                notificationCallback.get(i).triggerAllocationPopUp(data.getString("entity_ids"), data.getString("notification_type"), entity_payload);
                            }
                        }
                    }
                }
                notificationId ++;
            }

        }catch (Exception e){
            e.printStackTrace();
        }
    }

    @SuppressLint("MissingPermission")
    @RequiresApi(api = Build.VERSION_CODES.M)
    public static void showNotification(Context context, String title, String msg, JSONObject data, String imageUrl ){
        try {
            Log.e(TAG, "SHOWNOTIFICATION MESSAGE");
            Bitmap bigIcon = BitmapFactory.decodeResource(context.getResources(), R.mipmap.ic_launcher);
            Bitmap bitmap = null;
            if (imageUrl != null)
            {
                 bitmap = getBitmapfromUrl(imageUrl);
            }
            Intent intent= new Intent(context, MainActivity.class);
            System.out.println("Notificationn Utils Data"+ data.toString());
            System.out.println("Notificationn111"+data.getString("notification_type"));
            System.out.println("Notificationn222"+(data.getString("entity_ids")));
            System.out.println("imageUrl"+imageUrl);
            intent.putExtra("NOTIFICATION_DATA", data.toString());
//            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
            intent.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
            PendingIntent pendingIntent = PendingIntent.getActivity(context, notificationId , intent, 0);
            String notificationType = new String(data.getString("notification_type"));
            String channelId;
            String key = context.getString(R.string.service);
            System.out.println("key"+key);
            if (ALLOCATION_TYPE.equals(notificationType)) {
                System.out.println("showNotification:- "+ notificationType);
                channelId = RINGING_CHANNEL_ID;
            } else if (TRIP_CHANNEL_ID.equals(notificationType) ) {
                System.out.println("showNotification:- "+ notificationType);
                channelId = notificationType;
            }else if (CANCELLED_PRODUCT.equals(notificationType) ) {
                System.out.println("showNotification:- "+ notificationType);
                channelId = notificationType;
            } else{
                System.out.println("showNotification:- "+ notificationType);
                channelId = FLOATING_NOTIFICATION;
            }
            System.out.println("showNotification:-> "+ channelId);
            Intent svc = new Intent(context, AcceptRejectOverlaySheet.class);
            PendingIntent pendingIntentPopUp = PendingIntent.getActivity(context, notificationId , svc, 0);
            if (ALLOCATION_TYPE.equals(notificationType)) {
                //Recieved Notification && checking for permission if overlay permission is given, if not then it will redirect to give permission
//                if(checkPermission(context)){
//                    Intent notificationIntent = new Intent(context, MainActivityService.class);
//                    notificationIntent.setAction("some string");
//
//                    context.sendBroadcast(notificationIntent);
                    SharedPreferences sharedPref = context.getSharedPreferences(
                            context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                    String token = sharedPref.getString("RegistrationToken", "null");
                    String baseEnv = sharedPref.getString("ENV", "null");
                    svc.putExtra("caseId",data.getString("entity_ids"));
                    svc.putExtra("notificationId",notificationId);
                    svc.putExtra("shared_prefs_key", token);
                    svc.putExtra("base_env", baseEnv);
                    svc.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    pendingIntentPopUp = PendingIntent.getActivity(context, notificationId , svc, PendingIntent.FLAG_UPDATE_CURRENT);
//                }
                System.out.println("In_if_in_notification after");
            }

            NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, channelId);
            if (imageUrl != null){
                mBuilder.setLargeIcon(bitmap)
                        .setSmallIcon(smallIcon)
                        .setContentTitle(title)
                        .setContentText(msg)
                        .setAutoCancel(true)
                        .setContentIntent(pendingIntent)
                        .setPriority(NotificationManager.IMPORTANCE_MAX)
                        .setChannelId(channelId)
                        .setStyle(
                                new NotificationCompat.BigPictureStyle()
                                        .bigPicture(bitmap)
                                        .bigLargeIcon(null));
            }
            else {
                mBuilder.setSmallIcon(smallIcon)
                        .setContentTitle(title)
                        .setContentText(msg)
                        .setAutoCancel(true)
                        .setPriority(NotificationManager.IMPORTANCE_MAX)
                        .setContentIntent(pendingIntent)
                        .setChannelId(channelId);
            }
            if(pendingIntentPopUp != null && ALLOCATION_TYPE.equals(notificationType)){
                mBuilder.addAction(0,"View",pendingIntentPopUp);
            }
           if(Build.VERSION.SDK_INT < Build.VERSION_CODES.O) {
               System.out.println("Default sound");
               Uri notificationSound;
               if (notificationType.equals(ALLOCATION_TYPE)) {
                   notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.allocation_request);
               } else if (notificationType.equals(TRIP_CHANNEL_ID)){
                   notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.notify_otp_sound);
                   mBuilder.setSound(notificationSound);
               }else if (notificationType.equals(CANCELLED_PRODUCT)){
                notificationSound = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.cancel_notification_sound);
                   mBuilder.setSound(notificationSound);
                }else{
                   notificationSound = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
               }
               System.out.println("Default sound"+ notificationSound);
           }
            NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
            System.out.println("In clean notification before notify");

            if (notificationType.equals(ALLOCATION_TYPE))
                {
//                    mBuilder.setTimeoutAfter(20000);
                    System.out.println("In clean notification if");
                }
            notificationManager.notify(notificationId, mBuilder.build());
            if (DRIVER_ASSIGNMENT.equals(notificationType)){
                if (mediaPlayer != null){
                    mediaPlayer.stop();
                    mediaPlayer = null;
                }
                audio = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
                if (key.equals("nammayatripartner")) {
                    audio.setStreamVolume(AudioManager.STREAM_MUSIC, (int) audio.getStreamMaxVolume(AudioManager.STREAM_MUSIC), AudioManager.ADJUST_SAME);
                }
                mediaPlayer = MediaPlayer.create(context, R.raw.ride_assigned);
                mediaPlayer.start();
            }
            if (TRIP_CHANNEL_ID.equals(notificationType) ) {
                Bundle params = new Bundle();
                mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                mFirebaseAnalytics.logEvent("ride_started",params);
            }
            if (TRIP_FINISHED.equals(notificationType) ) {
                Bundle params = new Bundle();
                mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                mFirebaseAnalytics.logEvent("ride_completed",params);
            }
            if (CANCELLED_PRODUCT.equals(notificationType) ) {
                Bundle params = new Bundle();
                mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                mFirebaseAnalytics.logEvent("ride_cancelled",params);
            }
            if (DRIVER_ASSIGNMENT.equals(notificationType) ) {
                Bundle params = new Bundle();
                mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);
                mFirebaseAnalytics.logEvent("driver_assigned",params);
            }
            notificationId ++;
            if(DRIVER_ASSIGNMENT.equals(notificationType) || CANCELLED_PRODUCT.equals(notificationType))
            {

                for(int i =0;i<notificationCallback.size();i++)
                {
//                    CommonJsInterface.callingStoreCall();
                    notificationCallback.get(i).triggerPop((data.getString("entity_ids")),(data.getString("notification_type")));
                }
            }
            if((TRIP_FINISHED.equals(notificationType) || DRIVER_ASSIGNMENT.equals(notificationType) || REALLOCATE_PRODUCT.equals(notificationType) || CANCELLED_PRODUCT.equals(notificationType) || TRIP_CHANNEL_ID.equals(notificationType)) && (key.equals("nammayatri")))
            {

                for(int i =0;i<notificationCallback.size();i++)
                {
                    System.out.println("notificationCallback" + notificationType);
                    notificationCallback.get(i).callFlowCustomer(notificationType);
                }
            }
        }catch (Exception e){
            e.printStackTrace();
        }
    }

    private static Bitmap getBitmapfromUrl(String imageUrl) {
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
            Log.e("awesome", "Error in getting notification image: " + e.getLocalizedMessage());
            return null;
        }
    }

    @RequiresApi(api = Build.VERSION_CODES.M)
    private static boolean checkPermission(Context context){
        if (Settings.canDrawOverlays(context)) {
            return true;
        }
        else {
            return false;
            //return checkDrawOverlayPermission(context);
            //return checkPermission(context);
        }
    }

    @RequiresApi(api = Build.VERSION_CODES.M)
    private static boolean checkDrawOverlayPermission(Context context) {
        System.out.println("Call permission function");
        System.out.println("Current thread" + Thread.currentThread()+ " , main thread - "+ Looper.getMainLooper().getThread());
        if (!Settings.canDrawOverlays(context)) {
            Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, Uri.parse("package:" + context.getPackageName()));
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_NEW_TASK);

            System.out.println("Call permission before");
            context.startActivity(intent);
            //checkDrawOverlayPermission(context);
            System.out.println("Call permission after recurrison");
        }
        return !Settings.canDrawOverlays(context);
    }


    public static void showBigNotification(Context context, String title, String msg){
        try{
            Bitmap bigIcon = BitmapFactory.decodeResource(context.getResources(), R.mipmap.ic_launcher);
            Intent intent = new Intent(context, MainActivity.class);
//            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
            intent.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
            PendingIntent pendingIntent = PendingIntent.getActivity(context, 0 , intent, 0);

            NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context,CHANNEL_ID)
                    .setLargeIcon(bigIcon)
                    .setSmallIcon(smallIcon)
                    .setContentTitle(title)
                    .setContentText(msg)
                    .setAutoCancel(true)
                    .setContentIntent(pendingIntent)
                    .setPriority(NotificationCompat.PRIORITY_DEFAULT);

            NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);

            notificationManager.notify(notificationId, mBuilder.build());
            notificationId ++;
        }catch (Exception e){
            e.printStackTrace();
        }
    }

    public static void showBigImageNotification(Context context, String title, String msg, String url) {
        try {
            Bitmap bitmap = getBitmapFromURL(url);

            Intent intent = new Intent(context, MainActivity.class);
//            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
            intent.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
            PendingIntent pendingIntent = PendingIntent.getActivity(context, 0, intent, 0);

            NotificationCompat.BigPictureStyle bigPictureStyle = new NotificationCompat.BigPictureStyle()
                    .bigPicture(bitmap);
            if(msg != null && !msg.trim().isEmpty()){
                bigPictureStyle.setSummaryText(msg);
            }
            if(title != null && !title.trim().isEmpty()){
                bigPictureStyle.setBigContentTitle(title);
            }
            Notification notification = new NotificationCompat.Builder(context, CHANNEL_ID)
                    .setLargeIcon(bitmap)
                    .setSmallIcon(smallIcon)
                    .setStyle(bigPictureStyle)
                    .setContentIntent(pendingIntent)
                    .build();

            NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
            notificationManager.notify(notificationId, notification);
            notificationId++;
        }catch (Exception e){
            e.printStackTrace();
        }
    }

    public static void createNotificationChannel(Context context, String channel_Id){
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O){
                CharSequence name = channel_Id;
                String description = "Important Notification";
                int importance = NotificationManager.IMPORTANCE_HIGH;
                if(channel_Id.equals("FLOATING_NOTIFICATION")){
                    importance = NotificationManager.IMPORTANCE_DEFAULT;
                }
                NotificationChannel channel = new NotificationChannel(channel_Id, name, importance);
                channel.setDescription(description);
                System.out.println("Channel"+ channel_Id);
                if (channel_Id.equals(RINGING_CHANNEL_ID)) {
                    soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.allocation_request);
                } else if (channel_Id.equals(TRIP_CHANNEL_ID)){
                    soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.notify_otp_sound);
                }else if (channel_Id.equals(CANCELLED_PRODUCT)){
                    soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.cancel_notification_sound);
                }else{
                    soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                }
                System.out.println("Channel"+ soundUri);
                AudioAttributes attributes = new AudioAttributes.Builder()
                        .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
                        .setUsage(AudioAttributes.USAGE_NOTIFICATION)
                        .build();
                channel.setSound(soundUri, attributes);

                NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
                if(notificationManager != null){
                    notificationManager.createNotificationChannel(channel);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static Bitmap getBitmapFromURL(String src) {
        try {
            URL url = new URL(src);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setDoInput(true);
            connection.connect();
            InputStream input = connection.getInputStream();
            return BitmapFactory.decodeStream(input);
        } catch (Exception e) {
            return null;
        }
    }

    public static String decodeAddress(JSONObject object){
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

    private static int calculateTimeDifference(String expireTimeTemp, String currTimeTemp){
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
        if ((expireTime-currTime) >= 5) {
            return expireTime-currTime - 5 ;
        }
        return 0;
    }
}