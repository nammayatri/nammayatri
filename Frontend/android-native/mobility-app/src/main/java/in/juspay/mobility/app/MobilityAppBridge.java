package in.juspay.mobility.app;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.webkit.JavascriptInterface;

import com.facebook.appevents.AppEventsLogger;
import com.google.android.gms.tasks.Task;
import com.google.android.play.core.review.ReviewInfo;
import com.google.android.play.core.review.ReviewManager;
import com.google.android.play.core.review.ReviewManagerFactory;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessaging;

import java.util.ArrayList;
import java.util.Locale;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.mobility.app.callbacks.CallBack;

public class MobilityAppBridge extends HyperBridge {

    // Log Tags
    private static final String CHATS = "CHATS";
    private static final String META_LOG = "META_LOG";
    private static final String CALLBACK = "CALLBACK";

    private static FirebaseAnalytics mFirebaseAnalytics;
    protected static String storeChatMessageCallBack = null;
    public static String storeCallBackOpenChatScreen = null;
    private static final ArrayList<SendMessageCallBack> sendMessageCallBacks = new ArrayList<>();
    CallBack callBack = new CallBack() {
        @Override
        public void customerCallBack(String notificationType) {
            Log.i(CALLBACK,"Not required");
        }

        @Override
        public void driverCallBack(String notificationType) {
            Log.i(CALLBACK,"Not required");
        }

        @Override
        public void imageUploadCallBack(String encImage, String filename, String filePath) {
            Log.i(CALLBACK,"Not required");
        }

        @Override
        public void internetCallBack(String isPermission) {
            Log.i(CALLBACK,"Not required");
        }

        @Override
        public void chatCallBack(String message, String sentBy, String dateFormatted, String len) {
            callChatMessageCallBack(message,sentBy,dateFormatted,len);
        }
        @Override
        public void inAppCallBack(String inAppCallBack) {
            callInAppNotificationCallBack(inAppCallBack);
        }
    };

    public MobilityAppBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(bridgeComponents.getContext());
        ChatService.registerCallback(callBack);
        InAppNotification.registerCallback(callBack);
    }

    @JavascriptInterface
    public void factoryResetApp() {
        if (bridgeComponents.getActivity() != null) {
            final PackageManager pm = bridgeComponents.getActivity().getPackageManager();
            final Intent intent = pm.getLaunchIntentForPackage(bridgeComponents.getActivity().getPackageName());
            bridgeComponents.getActivity().finishAffinity(); // Finishes all activities.
            bridgeComponents.getContext().startActivity(intent);    // Start the launch activity
        }
    }

    @Override
    public void reset() {
        ChatService.deRegisterCallback(callBack);
        InAppNotification.deRegisterCallBack(callBack);
    }

    // region Store And Trigger CallBack
    @JavascriptInterface
    public void storeCallBackMessageUpdated(final String channelId, final String uuid, final String callback) {
        storeChatMessageCallBack = callback;
        KeyValueStore.write(bridgeComponents.getContext(), bridgeComponents.getSdkName(), "CHAT_CHANNEL_ID", channelId); // Update the Local Storage Value
        ChatService.chatChannelID = channelId;
        ChatService.chatUserId = uuid;
    }

    public void callChatMessageCallBack(String message, String sentBy, String dateFormatted, String len) {
        if (storeChatMessageCallBack != null) {
            String javascript = String.format("window.callUICallback(\"%s\",\"%s\",\"%s\",\"%s\",\"%s\");", storeChatMessageCallBack, message, sentBy, dateFormatted,len);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }
    public void callInAppNotificationCallBack(String onTapAction) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback(\"%s\");", onTapAction);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }
    // endregion

    @JavascriptInterface
    public void firebaseLogEvent(String event) {
        Bundle params = new Bundle();
        mFirebaseAnalytics.logEvent(event, params);
    }

    @JavascriptInterface
    public void firebaseLogEventWithParams(String event, String paramKey, String paramValue) {
        Bundle params = new Bundle();
        params.putString(paramKey, paramValue);
        mFirebaseAnalytics.logEvent(event, params);
    }

    @JavascriptInterface
    public void firebaseLogEventWithTwoParams(String event, String paramKey1, String paramValue1, String paramKey2, String paramValue2) {
        Bundle params = new Bundle();
        params.putString(paramKey1, paramValue1);
        params.putString(paramKey2, paramValue2);
        mFirebaseAnalytics.logEvent(event, params);
    }

    @JavascriptInterface
    public void firebaseUserID(String id) {
        mFirebaseAnalytics.setUserId(id);
    }

    @JavascriptInterface
    public void setFCMToken(final String callback) {
        ExecutorManager.runOnMainThread(() -> FirebaseMessaging.getInstance().getToken()
                .addOnCompleteListener(task -> {
                    if (!task.isSuccessful()) {
                        return;
                    }
                    // Get new FCM registration token
                    String token = task.getResult();
                    KeyValueStore.write(bridgeComponents.getContext(), bridgeComponents.getSdkName(), "FCM_TOKEN", token);
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');", callback, token);
                    if (callback != null) {
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                    }
                }));
    }

    @JavascriptInterface
    public void metaLogEvent(String event){
        try {
            AppEventsLogger logger = AppEventsLogger.newLogger(bridgeComponents.getContext());
            logger.logEvent(event);
        } catch (Exception e) {
            Log.e(META_LOG, "Error logging meta event : " + e);
        }
    }
    
    @JavascriptInterface
    public void launchInAppRatingPopup() {
        ReviewManager manager = ReviewManagerFactory.create(bridgeComponents.getContext());
        Task<ReviewInfo> request = manager.requestReviewFlow();
        request.addOnCompleteListener(task -> {
            if (task.isSuccessful() && bridgeComponents.getActivity() != null) {
                // We can get the ReviewInfo object
                ReviewInfo reviewInfo = task.getResult();
                Task<Void> flow = manager.launchReviewFlow(bridgeComponents.getActivity(), reviewInfo);
                flow.addOnCompleteListener(task1 -> {
                    // The flow has finished. The API does not indicate whether the user
                    // reviewed or not, or even whether the review dialog was shown.
                });
            }

        });
    }
    // endregion

    //region Chat Utiils
    @JavascriptInterface
    public static void sendMessage(final String message) {
        for (SendMessageCallBack sendMessageCallBack : sendMessageCallBacks) {
            sendMessageCallBack.sendMessage(message);
        }
    }

    @JavascriptInterface
    public void storeCallBackOpenChatScreen(final String callback){
        storeCallBackOpenChatScreen = callback;
    }

    @JavascriptInterface
    public void startChatListenerService() {
        try {
            Context context = bridgeComponents.getContext();
            SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String appState = sharedPref.getString("ACTIVITY_STATUS", "null");
            Intent chatListenerService = new Intent(context, ChatService.class);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S && appState.equals("onPause")) {
                AlarmManager manager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
                Intent alarmIntent = new Intent(context, ChatBroadCastReceiver.class);
                PendingIntent pendingIntent = PendingIntent.getBroadcast(context, 0, alarmIntent, PendingIntent.FLAG_IMMUTABLE);
                manager.setExact(AlarmManager.RTC_WAKEUP, System.currentTimeMillis(), pendingIntent);
            } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                context.startForegroundService(chatListenerService);
            } else {
                context.startService(chatListenerService);
            }
        } catch (Exception e) {
            Log.e(CHATS, "Failed to start ChatService : " + e);
        }
    }

    @JavascriptInterface
    public void stopChatListenerService() {
        try {
            Intent chatListenerService = new Intent(bridgeComponents.getContext(), ChatService.class);
            bridgeComponents.getContext().stopService(chatListenerService);
        } catch (Exception e) {
            Log.e(CHATS, "Failed to stop ChatService : " + e);
        }
    }

    public interface SendMessageCallBack {
        void sendMessage(String message);
    }

    public static void registerSendMessageCallBack(SendMessageCallBack callBack){
        sendMessageCallBacks.add(callBack);
    }

    public static void deRegisterSendMessageCallBack(SendMessageCallBack callBack){
        sendMessageCallBacks.remove(callBack);
    }
    // endregion
}
