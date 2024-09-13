package in.juspay.mobility.app.services;

import android.Manifest;
import android.annotation.TargetApi;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ServiceInfo;
import android.graphics.Color;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.telecom.PhoneAccount;
import android.telecom.PhoneAccountHandle;
import android.telecom.TelecomManager;
import android.telecom.VideoProfile;
import android.util.Log;

import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.ProcessLifecycleOwner;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import com.twilio.voice.Call;
import com.twilio.voice.CallInvite;

import in.juspay.mobility.app.NotificationProxyActivity;
import in.juspay.mobility.app.R;

public class IncomingCallNotificationService  extends Service {

    private static final String TAG = IncomingCallNotificationService.class.getSimpleName();

    private PhoneAccountHandle phoneAccountHandle;
    private TelecomManager telecomManager;

    @Override
    public void onCreate() {
        super.onCreate();
        // register telecom account info

        System.out.println("Inside oncreate of ");
        Context appContext = this.getApplicationContext();
        String appName = "Connection Service";
        System.out.println("Inside oncreate of ");
        phoneAccountHandle =
                new PhoneAccountHandle(new ComponentName(appContext, VoiceConnectionService.class),
                        appName);
        telecomManager = (TelecomManager) appContext.getSystemService(TELECOM_SERVICE);
        PhoneAccount phoneAccount = new PhoneAccount.Builder(phoneAccountHandle, appName)
                .setCapabilities(PhoneAccount.CAPABILITY_CALL_PROVIDER)
                .setCapabilities(PhoneAccount.CAPABILITY_SELF_MANAGED)
                .build();
        telecomManager.registerPhoneAccount(phoneAccount);
    }
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        System.out.println("Inside onstartCOmmand of IncomingCallNotificationService" );
        String action = intent.getAction();
        System.out.println("Inside onstartCOmmand of IncomingCallNotificationService" + action);
        if (action != null) {
            CallInvite callInvite = intent.getParcelableExtra(String.valueOf(R.string.INCOMING_CALL_INVITE));
            int notificationId = intent.getIntExtra(String.valueOf(R.string.INCOMING_CALL_NOTIFICATION_ID), 0);
            if (action.equals(String.valueOf(R.string.ACTION_INCOMING_CALL))) {
                    handleIncomingCall(intent, callInvite, notificationId);
                } else if (action.equals(String.valueOf(R.string.ACTION_OUTGOING_CALL))){
                    handleOutgoingCall(intent);
                }
                else if (action.equals(String.valueOf(R.string.ACTION_ACCEPT))) {
                    System.out.println("INisde accept call ");
                    accept(callInvite, notificationId);
                } else if (action.equals(String.valueOf(R.string.ACTION_REJECT))) {
                System.out.println("Inside reject call");
                    assert callInvite != null;
                    reject(callInvite);
                } else if (action.equals(String.valueOf(R.string.ACTION_CANCEL_CALL))) {
                    handleCancelledCall(intent);
                }
            }
            return START_NOT_STICKY;
        }

        @Override
        public IBinder onBind(Intent intent) {
            return null;
        }

        private Notification createNotification(int notificationId, int channelImportance) {
            Intent intent = new Intent(this, NotificationProxyActivity.class);
            intent.setAction(String.valueOf(R.string.ACTION_INCOMING_CALL_NOTIFICATION));
            intent.putExtra(String.valueOf(R.string.INCOMING_CALL_NOTIFICATION_ID), notificationId);
//            intent.putExtra(String.valueOf(R.string.INCOMING_CALL_INVITE), callInvite);
            intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);

            PendingIntent pendingIntent =
                    PendingIntent.getActivity(this, notificationId, intent, PendingIntent.FLAG_IMMUTABLE);
            /*
             * Pass the notification id and call sid to use as an identifier to cancel the
             * notification later
             */
            Bundle extras = new Bundle();
//            extras.putString(String.valueOf(R.string.CALL_SID_KEY), callInvite.getCallSid());

            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                return buildNotification("ABCD" + " is calling.",
                        pendingIntent,
                        extras,
                        notificationId,
                        createChannel(channelImportance));
            } else {
                //noinspection deprecation
                return new NotificationCompat.Builder(this)
                        .setSmallIcon(R.drawable.ny_ic_call)
                        .setContentTitle("CHecking")
                        .setContentText("ABCD" + " is calling.")
                        .setAutoCancel(true)
                        .setExtras(extras)
                        .setContentIntent(pendingIntent)
                        .setGroup("test_app_notification")
                        .setPriority(Notification.PRIORITY_HIGH)
                        .setCategory(Notification.CATEGORY_CALL)
                        .setColor(Color.rgb(214, 10, 37)).build();
            }
        }

        /**
         * Build a notification.
         *
         * @param text          the text of the notification
         * @param pendingIntent the body, pending intent for the notification
         * @param extras        extras passed with the notification
         * @return the builder
         */
        @TargetApi(Build.VERSION_CODES.O)
        private Notification buildNotification(String text, PendingIntent pendingIntent, Bundle extras,
                                               int notificationId,
                                               String channelId) {
            Intent rejectIntent = new Intent(getApplicationContext(), NotificationProxyActivity.class);
            rejectIntent.setAction(String.valueOf(R.string.ACTION_REJECT));
            // rejectIntent.putExtra(String.valueOf(R.string.INCOMING_CALL_INVITE), callInvite);
            rejectIntent.putExtra(String.valueOf(R.string.INCOMING_CALL_NOTIFICATION_ID), notificationId);
            PendingIntent piRejectIntent = PendingIntent.getService(getApplicationContext(), notificationId, rejectIntent, PendingIntent.FLAG_IMMUTABLE);

            Intent acceptIntent = new Intent(getApplicationContext(), NotificationProxyActivity.class);
            acceptIntent.setAction(String.valueOf(R.string.ACTION_ACCEPT));
            // acceptIntent.putExtra(String.valueOf(R.string.INCOMING_CALL_INVITE), callInvite);
            acceptIntent.putExtra(String.valueOf(R.string.INCOMING_CALL_NOTIFICATION_ID), notificationId);
            acceptIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            PendingIntent piAcceptIntent = PendingIntent.getActivity(getApplicationContext(), notificationId, acceptIntent, PendingIntent.FLAG_IMMUTABLE);

            Notification.Builder builder =
                    new Notification.Builder(getApplicationContext(), channelId)
                            .setSmallIcon(R.drawable.ny_ic_call)
                            .setContentTitle("Incoming Call")
                            .setContentText(text)
                            .setCategory(Notification.CATEGORY_CALL)
                            .setExtras(extras)
                            .setPriority(Notification.PRIORITY_HIGH)
                            .setAutoCancel(true)
                            .addAction(android.R.drawable.ic_menu_delete, getString(R.string.decline), piRejectIntent)
                            .addAction(android.R.drawable.ic_menu_call, "Answer", piAcceptIntent)
                            .setFullScreenIntent(pendingIntent, true);

            return builder.build();
        }

        @TargetApi(Build.VERSION_CODES.O)
        private String createChannel(int channelImportance) {
            NotificationChannel callInviteChannel = new NotificationChannel(String.valueOf(R.string.VOICE_CHANNEL_HIGH_IMPORTANCE),
                    "Primary Voice Channel", NotificationManager.IMPORTANCE_HIGH);
            String channelId = String.valueOf(R.string.VOICE_CHANNEL_HIGH_IMPORTANCE);

            if (channelImportance == NotificationManager.IMPORTANCE_LOW) {
                callInviteChannel = new NotificationChannel(String.valueOf(R.string.VOICE_CHANNEL_LOW_IMPORTANCE),
                        "Primary Voice Channel", NotificationManager.IMPORTANCE_LOW);
                channelId = String.valueOf(R.string.VOICE_CHANNEL_LOW_IMPORTANCE);
            }
            callInviteChannel.setLightColor(Color.GREEN);
            callInviteChannel.setLockscreenVisibility(Notification.VISIBILITY_PRIVATE);
            NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
            notificationManager.createNotificationChannel(callInviteChannel);

            return channelId;
        }

        private void accept(CallInvite callInvite, int notificationId) {
            endForeground();
        }

        private void reject(CallInvite callInvite) {
            endForeground();
            callInvite.reject(getApplicationContext());
        }

        private void handleCancelledCall(Intent intent) {
            endForeground();
            LocalBroadcastManager.getInstance(this).sendBroadcast(intent);
        }

        private void handleIncomingCall(Intent intent, CallInvite callInvite, int notificationId) {
            LocalBroadcastManager.getInstance(this).sendBroadcast(intent);

            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                setCallInProgressNotification(notificationId);
            }
        }

    private void handleOutgoingCall(Intent intent) {
        // place a call with the telecom subsystem
        System.out.println("Inisde handleOutgoing Call");
        final Bundle extra = intent.getExtras();
        System.out.println("Inisde handleOutgoing Call"+ extra);
        if (null != extra) {
            Bundle callInfo = new Bundle();
            final Uri recipient = Uri.fromParts(PhoneAccount.SCHEME_TEL, "abcd", null) ;//extra.getParcelable(String.valueOf(in.juspay.mobility.app.R.string.OUTGOING_CALL_RECIPIENT));
            System.out.println("printing the URI recipient" + recipient);
            final int permissionsState =
                    ActivityCompat.checkSelfPermission(this,
                            Manifest.permission.MANAGE_OWN_CALLS);
            callInfo.putParcelable(TelecomManager.EXTRA_OUTGOING_CALL_EXTRAS, extra);
            callInfo.putParcelable(TelecomManager.EXTRA_PHONE_ACCOUNT_HANDLE, phoneAccountHandle);
            callInfo.putInt(TelecomManager.EXTRA_START_CALL_WITH_VIDEO_STATE, VideoProfile.STATE_AUDIO_ONLY);
            if (permissionsState == PackageManager.PERMISSION_GRANTED) {
                telecomManager.placeCall(recipient, callInfo);
            }
            setCallInProgressNotification(122345);


        }
    }

    

        private void endForeground() {
            stopForeground(true);
        }

        @TargetApi(Build.VERSION_CODES.O)
        private void setCallInProgressNotification(int notificationId) {
            if (isAppVisible()) {
                Log.i(TAG, "setCallInProgressNotification - app is visible.");
                startForegroundService(notificationId, createNotification( notificationId, NotificationManager.IMPORTANCE_HIGH));
            } else {
                Log.i(TAG, "setCallInProgressNotification - app is NOT visible.");
                startForeground(notificationId, createNotification( notificationId, NotificationManager.IMPORTANCE_HIGH));
            }
        }

        @TargetApi(Build.VERSION_CODES.O)
        private void startForegroundService(final int id, final Notification notification) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                startForeground(id, notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_MICROPHONE);
            } else {
                startForeground(id, notification);
            }
        }

        private boolean isAppVisible() {
            return ProcessLifecycleOwner
                    .get()
                    .getLifecycle()
                    .getCurrentState()
                    .isAtLeast(Lifecycle.State.STARTED);
        }
    }


