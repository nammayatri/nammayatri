package in.juspay.mobility.common.data;

import android.app.NotificationChannelGroup;
import android.app.NotificationManager;
import android.content.Context;
import android.media.AudioAttributes;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Build;

public class NotificationChannelData{
    public final String channelId;
    public final int importance;
    public final String description;
    public final Uri soundUri;
    public final String name;
    public final String groupId;

    public NotificationChannelData(String channelId, String name, String description, String groupId, int importance, Uri soundUri){
        this.channelId = channelId;
        this.name = name;
        this.description = description;
        this.groupId = groupId;
        this.importance = importance;
        this.soundUri = soundUri;
    }
}


    private void initNotificationChannel() {
        new Thread(() -> {
            NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                try {
                    notificationManager.deleteNotificationChannel("RINGING_ALERT");
                    notificationManager.deleteNotificationChannel("TRIP_STARTED");
                    notificationManager.deleteNotificationChannel("General");
                    notificationManager.deleteNotificationChannel("FLOATING_NOTIFICATION");
                } catch(Exception e) {
                    System.out.println("Notification Channel doesn't exists");
                }
            }


            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                NotificationChannelGroup safetyGroup = new NotificationChannelGroup("1_safety", "Enhanced Safety");
                NotificationChannelGroup rideRelatedGroup = new NotificationChannelGroup("2_ride_related", "Essential - Ride related");
                NotificationChannelGroup serviceGroup = new NotificationChannelGroup("3_services", "Services");
                NotificationChannelGroup promotionalGroup = new NotificationChannelGroup("4_promotional", "Promotional");

                if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.P){
                    safetyGroup.setDescription("Notifications related to Safety");
                    rideRelatedGroup.setDescription("Notifications related to ride starts, end");
                    serviceGroup.setDescription("Notifications related to Services");
                    promotionalGroup.setDescription("Notifications related to promotional");
                }

                notificationManager.createNotificationChannelGroup(safetyGroup);
                notificationManager.createNotificationChannelGroup(rideRelatedGroup);
                notificationManager.createNotificationChannelGroup(serviceGroup);
                notificationManager.createNotificationChannelGroup(promotionalGroup);
            }


            NotificationUtils.createNotificationChannel(MainActivity.this, NotificationUtils.DRIVER_QUOTE_INCOMING);
            NotificationUtils.createNotificationChannel(MainActivity.this, NotificationUtils.DRIVER_ASSIGNMENT);
            NotificationUtils.createNotificationChannel(MainActivity.this, NotificationUtils.REALLOCATE_PRODUCT);
            NotificationUtils.createNotificationChannel(MainActivity.this, NotificationUtils.GENERAL_NOTIFICATION);
            NotificationUtils.createNotificationChannel(MainActivity.this, NotificationUtils.RIDE_STARTED);
            NotificationUtils.createNotificationChannel(MainActivity.this, NotificationUtils.CANCELLED_PRODUCT);
            NotificationUtils.createNotificationChannel(MainActivity.this, NotificationUtils.DRIVER_HAS_REACHED);
            NotificationUtils.createNotificationChannel(MainActivity.this, NotificationUtils.TRIP_FINISHED);
            NotificationUtils.createNotificationChannel(MainActivity.this, MyFirebaseMessagingService.NotificationTypes.SOS_TRIGGERED);
            NotificationUtils.createNotificationChannel(MainActivity.this, MyFirebaseMessagingService.NotificationTypes.SOS_RESOLVED);
        }).start();
    }



    public static void createNotificationChannel(Context context, String channel_Id) {
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                String description = "Important Notification";
                int importance = NotificationManager.IMPORTANCE_HIGH;
                if (channel_Id.equals("FLOATING_NOTIFICATION")) {
                    importance = NotificationManager.IMPORTANCE_DEFAULT;
                }
                android.app.NotificationChannel channel = new android.app.NotificationChannel(channel_Id, channel_Id, importance);
                channel.setDescription(description);
                switch (channel_Id) {
                    case RIDE_STARTED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.ride_started);
                        channel.setName("Ride Started");
                        channel.setDescription("Used to notify when ride has started");
                        break;
                    case CANCELLED_PRODUCT:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.cancel_notification_sound);
                        channel.setName("Ride Cancelled");
                        channel.setDescription("Used to notify when ride is cancelled");
                        break;
                    case DRIVER_HAS_REACHED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.driver_arrived);
                        channel.setName("Driver Arrived");
                        channel.setDescription("Used to notify when driver has arrived");
                        break;
                    case TRIP_FINISHED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.ride_completed_talkback);
                        channel.setName("Ride Completed");
                        channel.setDescription("Used to notify when ride is completed");
                        break;
                    case MyFirebaseMessagingService.NotificationTypes.SOS_TRIGGERED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.ny_ic_sos_danger);
                        channel.setName("SOS Triggered");
                        channel.setDescription("Used to alert when SOS is triggered by emergency contact");
                        break;
                    case MyFirebaseMessagingService.NotificationTypes.SOS_RESOLVED:
                        soundUri = Uri.parse("android.resource://" + context.getPackageName() + "/" + R.raw.ny_ic_sos_safe);
                        channel.setName("SOS Resolved");
                        channel.setDescription("Used to alert when SOS is resolved by emergency contact");
                        break;
                    case DRIVER_QUOTE_INCOMING:
                        soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                        channel.setName("Driver Quote Incoming");
                        channel.setDescription("Driver quote related Notifications");
                        break;
                    case DRIVER_ASSIGNMENT:
                        soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                        channel.setName("Driver Assignment");
                        channel.setDescription("Driver Assignment related Notifications");
                        break;
                    case REALLOCATE_PRODUCT:
                        soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                        channel.setName("Ride Reallocation");
                        channel.setDescription("Ride Reallocation related Notifications");
                        break;
                    default:
                        soundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
                        channel.setName("Other ride related");
                        channel.setDescription("Other ride related Notifications");
                }


                AudioAttributes attributes = new AudioAttributes.Builder()
                        .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
                        .setUsage(AudioAttributes.USAGE_NOTIFICATION)
                        .build();
                channel.setSound(soundUri, attributes);


                switch (channel_Id) {
                    case RIDE_STARTED:
                    case TRIP_FINISHED:
                    case DRIVER_HAS_REACHED:
                    case CANCELLED_PRODUCT:
                    case DRIVER_QUOTE_INCOMING:
                    case DRIVER_ASSIGNMENT:
                    case REALLOCATE_PRODUCT:
                        channel.setGroup("2_ride_related");
                        break;
                    case MyFirebaseMessagingService.NotificationTypes.SOS_TRIGGERED:
                    case MyFirebaseMessagingService.NotificationTypes.SOS_RESOLVED:
                        channel.setGroup("1_safety");
                        break;
                }

                NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
                if (notificationManager != null) {
                    notificationManager.createNotificationChannel(channel);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }