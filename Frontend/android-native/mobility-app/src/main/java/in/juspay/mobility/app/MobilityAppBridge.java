package in.juspay.mobility.app;

import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.RECORD_AUDIO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static android.app.Activity.RESULT_OK;
import static androidx.core.app.ActivityCompat.startIntentSenderForResult;

import android.app.Activity;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.drawable.GradientDrawable;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.location.Location;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.text.Html;
import android.provider.MediaStore;
import android.util.Base64;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.viewpager2.widget.ViewPager2;

import com.clevertap.android.sdk.CleverTapAPI;
import com.facebook.appevents.AppEventsLogger;
import com.google.android.gms.auth.api.credentials.Credential;
import com.google.android.gms.auth.api.credentials.Credentials;
import com.google.android.gms.auth.api.credentials.HintRequest;
import com.google.android.gms.tasks.Task;
import com.google.android.play.core.review.ReviewInfo;
import com.google.android.play.core.review.ReviewManager;
import com.google.android.play.core.review.ReviewManagerFactory;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.firebase.perf.FirebasePerformance;
import com.google.firebase.perf.metrics.Trace;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerFullScreenListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.options.IFramePlayerOptions;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.ui.DefaultPlayerUiController;
import com.theartofdev.edmodo.cropper.CropImage;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.mobility.app.callbacks.CallBack;
import in.juspay.mobility.app.carousel.VPAdapter;
import in.juspay.mobility.app.carousel.ViewPagerItem;
import in.juspay.mobility.app.mediaPlayer.DefaultMediaPlayerControl;

public class MobilityAppBridge extends HyperBridge {

    // Log Tags
    private static final String LOG_TAG = "MobilityAppBridge";
    private static final String CHATS = "CHATS";
    private static final String META_LOG = "META_LOG";
    private static final String CALLBACK = "CALLBACK";
    private static final String UTILS = "UTILS";
    private static final String OVERRIDE = "OVERRIDE";

    private ArrayList<ViewPagerItem> viewPagerItemArrayList = new ArrayList<>();
    private static FirebaseAnalytics mFirebaseAnalytics;
    CleverTapAPI clevertapDefaultInstance;
    protected static String storeChatMessageCallBack = null;
    public static String storeCallBackOpenChatScreen = null;
    public static String storeDetectPhoneNumbersCallBack = null;
    private String storeImageUploadCallBack = null;
    private String storeUploadMultiPartCallBack = null;


    // Permission request Code
    private static final int CREDENTIAL_PICKER_REQUEST = 74;
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;

    private ArrayList<MediaPlayerView> audioPlayers = new ArrayList<>();
    private AudioRecorder audioRecorder = null;
    private boolean isUploadPopupOpen = false;

    public static YouTubePlayerView youTubePlayerView;
    public static YouTubePlayer youtubePlayer;
    public static float videoDuration = 0;

    protected HashMap<String, Trace> traceElements;

    private static final ArrayList<SendMessageCallBack> sendMessageCallBacks = new ArrayList<>();
    CallBack callBack = new CallBack() {
        @Override
        public void customerCallBack(String notificationType) {
            Log.i(CALLBACK, "Not required");
        }

        @Override
        public void driverCallBack(String notificationType) {
            Log.i(CALLBACK, "Not required");
        }

        @Override
        public void imageUploadCallBack(String encImage, String filename, String filePath) {
            callImageUploadCallBack(encImage, filename, filePath);
        }

        @Override
        public void chatCallBack(String message, String sentBy, String dateFormatted, String len) {
            callChatMessageCallBack(message, sentBy, dateFormatted, len);
        }

        @Override
        public void inAppCallBack(String inAppCallBack) {
            callInAppNotificationCallBack(inAppCallBack);
        }

        @Override
        public void bundleUpdatedCallBack(String event, JSONObject payload) {
            String command = String.format("window[\"onEvent'\"]('%s','%s')", event, payload.toString());
            bridgeComponents.getJsCallback().addJsToWebView(command);
        }
    };

    public MobilityAppBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        mFirebaseAnalytics = FirebaseAnalytics.getInstance(bridgeComponents.getContext());
        ChatService.registerCallback(callBack);
        InAppNotification.registerCallback(callBack);
        RemoteAssetsDownloader.registerCallback(callBack);
        traceElements = new HashMap<>();
        Utils.registerCallback(callBack);
        clevertapDefaultInstance = CleverTapAPI.getDefaultInstance(bridgeComponents.getContext());
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
        RemoteAssetsDownloader.deRegisterCallback(callBack);
        Utils.deRegisterCallback(callBack);
        storeImageUploadCallBack = null;
        audioPlayers = new ArrayList<>();
        audioRecorder = null;
        isUploadPopupOpen = false;
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
            String javascript = String.format("window.callUICallback(\"%s\",\"%s\",\"%s\",\"%s\",\"%s\");", storeChatMessageCallBack, message, sentBy, dateFormatted, len);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void removeChatMessageCallback() {
        storeChatMessageCallBack = null;
    }

    public void callInAppNotificationCallBack(String onTapAction) {
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback(\"%s\");", onTapAction);
        bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }

    @JavascriptInterface
    public void storeCallBackImageUpload(String callback) {
        storeImageUploadCallBack = callback;
    }

    public void callImageUploadCallBack(String stringImage, String imageName, String imagePath) {
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                storeImageUploadCallBack, stringImage, imageName, imagePath);
        bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }

    @JavascriptInterface
    public void storeCallBackUploadMultiPartData(String callback){
        storeUploadMultiPartCallBack = callback;
    }

    public void callUploadMultiPartCallBack(String fileType, String fileId) {
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s');",
                storeUploadMultiPartCallBack, fileType, fileId);
        bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }
    // endregion

    @JavascriptInterface
    public static void firebaseLogEvent(String event) {
        Bundle params = new Bundle();
        mFirebaseAnalytics.logEvent(event, params);
    }

    @JavascriptInterface
    public void startTracingPerf(String attribute){
        Trace myTrace = FirebasePerformance.getInstance().newTrace(attribute);
        myTrace.start();
        traceElements.put(attribute, myTrace);
    }

    @JavascriptInterface
    public void stopTracingPerf(String attribute){
        if(traceElements.containsKey(attribute)) {
            Trace myTrace = traceElements.get(attribute);
            myTrace.stop();
            traceElements.remove(attribute);
        }
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
    public void metaLogEvent(String event) {
        try {
            AppEventsLogger logger = AppEventsLogger.newLogger(bridgeComponents.getContext());
            logger.logEvent(event);
        } catch (Exception e) {
            Log.e(META_LOG, "Error in metaLogEvent " + e);
        }
    }

    @JavascriptInterface
    public void metaLogEventWithParams(String event, String paramKey, String paramValue) {
        try {
            Bundle params = new Bundle();
            params.putString(paramKey, paramValue);
            AppEventsLogger logger = AppEventsLogger.newLogger(bridgeComponents.getContext());
            logger.logEvent(event,params);
        } catch (Exception e) {
            Log.e(META_LOG, "Error in metaLogEventWithParams : " + e);
        }
    }

    @JavascriptInterface
    public void metaLogEventWithTwoParams(String event, String paramKey1, String paramValue1, String paramKey2, String paramValue2) {
        try {
            Bundle params = new Bundle();
            params.putString(paramKey1, paramValue1);
            params.putString(paramKey2, paramValue2);
            AppEventsLogger logger = AppEventsLogger.newLogger(bridgeComponents.getContext());
            logger.logEvent(event,params);
        } catch (Exception e) {
            Log.e(META_LOG, "Error in metaLogEventWithTwoParams : " + e);
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
    public void storeCallBackOpenChatScreen(final String callback) {
        storeCallBackOpenChatScreen = callback;
    }

    @JavascriptInterface
    public void removeCallBackOpenChatScreen() {
        storeCallBackOpenChatScreen = null;
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
            Intent overlayService = new Intent(bridgeComponents.getContext(), MessageOverlayService.class);
            bridgeComponents.getContext().stopService(chatListenerService);
            bridgeComponents.getContext().stopService(overlayService);
        } catch (Exception e) {
            Log.e(CHATS, "Failed to stop ChatService : " + e);
        }
    }

    public interface SendMessageCallBack {
        void sendMessage(String message);
    }

    public static void registerSendMessageCallBack(SendMessageCallBack callBack) {
        sendMessageCallBacks.add(callBack);
    }

    public static void deRegisterSendMessageCallBack(SendMessageCallBack callBack) {
        sendMessageCallBacks.remove(callBack);
    }
    // endregion

    

    @JavascriptInterface
    public void addCarouselWithVideo(String stringifyArray, String id) {
            viewPagerItemArrayList.clear();
            Activity activity = bridgeComponents.getActivity();
            Context context = bridgeComponents.getContext();
            LinearLayout parentLayout = null;
            if (activity != null) {
                parentLayout = activity.findViewById(Integer.parseInt(id));
            }
            if (activity == null || parentLayout == null) return;
            LinearLayout finalParentLayout = parentLayout;
            activity.runOnUiThread(() -> {

                ViewPager2 viewPager2 = new ViewPager2(context);

                LinearLayout sliderDotsPanel = new LinearLayout(context);
                LinearLayout.LayoutParams sliderDotsPanelParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);

                ViewGroup.LayoutParams scrollViewParams = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, finalParentLayout.getHeight() );
                ScrollView scrollView = new ScrollView(context);
                scrollView.setLayoutParams(scrollViewParams);

                LinearLayout.LayoutParams linearLayoutParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, finalParentLayout.getHeight());
                LinearLayout linearLayout = new LinearLayout(context);
                linearLayout.setOrientation(LinearLayout.VERTICAL);
                linearLayout.setLayoutParams(linearLayoutParams);

                //adding data in array list

                try {
                    JSONObject jsonData = new JSONObject(stringifyArray);
                    linearLayout.setGravity( Utils.getGravity(jsonData.optString("gravity" ,"CENTER")));
                    JSONArray jsonArray = jsonData.getJSONArray("carouselData");
                    for (int i = 0; i < jsonArray.length(); i++) {
                        JSONObject jsonObject = jsonArray.getJSONObject(i);
                        JSONObject imageConfig = jsonObject.getJSONObject("imageConfig");
                        JSONObject titleConfig = jsonObject.getJSONObject("titleConfig");
                        JSONObject descriptionConfig = jsonObject.getJSONObject("descriptionConfig");
                        String contentType = jsonObject.getString("contentType");
                        JSONObject videoData = jsonObject.getJSONObject("youtubeConfig");
                        int carouselGravity = jsonObject.optInt("gravity",Gravity.CENTER);
                        int imageID = in.juspay.mobility.app.Utils.getResIdentifier(context,imageConfig.getString("image"), "drawable");
                        ViewPagerItem viewPagerItem = new ViewPagerItem(imageID, imageConfig, descriptionConfig, titleConfig, contentType, videoData, carouselGravity);
                        viewPagerItemArrayList.add(viewPagerItem);
                    }
                } catch (Exception e) {
                    Log.e(UTILS, "Exception" + e);
                    return;
                }
                viewPager2.setAdapter(vpAdapter);

                // setting the dots layout
                int dotsCount;
                ImageView[] dots;
                dotsCount = vpAdapter.getItemCount();
                dots = new ImageView[dotsCount];
                for (int i = 0; i < dotsCount; i++) {
                    dots[i] = new ImageView(context);
                    dots[i].setImageDrawable(ContextCompat.getDrawable(context, in.juspay.mobility.app.R.drawable.carousel_dot_inactive));
                    LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(28, 28);
                    params.setMargins(14, 0, 14, 0);
                    sliderDotsPanel.addView(dots[i], params);
                    dots[0].setImageDrawable(ContextCompat.getDrawable(context, in.juspay.mobility.app.R.drawable.carousel_dot_active));
                    int finalI = i;
                    dots[i].setOnClickListener(view -> viewPager2.setCurrentItem(finalI));
                }
                sliderDotsPanel.setLayoutParams(sliderDotsPanelParams);
                viewPager2.setLayoutParams(new ViewGroup.LayoutParams(finalParentLayout.getWidth(), finalParentLayout.getHeight() - 50));

                viewPager2.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
                    @Override
                    public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                        super.onPageScrolled(position, positionOffset, positionOffsetPixels);
                    }

                    @Override
                    public void onPageSelected(int position) {
                        //setting active inactive dots
                        for (int i = 0; i < dotsCount; i++) {
                            dots[i].setImageDrawable(ContextCompat.getDrawable(context, in.juspay.mobility.app.R.drawable.carousel_dot_inactive));
                            dots[i].setContentDescription("Page Indicator, Page "  +( i+1) + " : Swipe or Tap To Go To Next Page\"");
                        }
                        dots[position].setImageDrawable(ContextCompat.getDrawable(context, in.juspay.mobility.app.R.drawable.carousel_dot_active));
                        super.onPageSelected(position);
                    }

                    @Override
                    public void onPageScrollStateChanged(int state) {
                        super.onPageScrollStateChanged(state);
                    }
                });
                linearLayout.addView(viewPager2);

                linearLayout.addView(sliderDotsPanel);
                scrollView.addView(linearLayout);
                sliderDotsPanel.setGravity(Gravity.BOTTOM | Gravity.CENTER_HORIZONTAL);
                finalParentLayout.addView(scrollView);
            });
    }

    // Deprecated on 30 OCT 2023.
    @JavascriptInterface
    public void addCarousel(String stringifyArray, String id) {
        Activity activity = bridgeComponents.getActivity();
        Context context = bridgeComponents.getContext();
        LinearLayout parentLayout = null;
        if (activity != null) {
            parentLayout = activity.findViewById(Integer.parseInt(id));
        }
        if (activity == null || parentLayout == null) return;
        LinearLayout finalParentLayout = parentLayout;
        activity.runOnUiThread(() -> {
            ViewPager2 viewPager2 = new ViewPager2(context);
            LinearLayout sliderDotsPanel = new LinearLayout(context);
            LinearLayout.LayoutParams sliderDotsPanelParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
            ViewGroup.LayoutParams scrollViewParams = new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);
            LinearLayout.LayoutParams linearLayoutParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT);
            LinearLayout linearLayout = new LinearLayout(context);
            linearLayout.setOrientation(LinearLayout.VERTICAL);
            linearLayout.setLayoutParams(linearLayoutParams);
            ScrollView scrollView = new ScrollView(context);
            scrollView.setLayoutParams(scrollViewParams);

            //adding data in array list
            ArrayList<ViewPagerItem> viewPagerItemArrayList = new ArrayList<>();
            try {
                JSONArray jsonArray = new JSONArray(stringifyArray);
                for (int i = 0; i < jsonArray.length(); i++) {
                    JSONObject jsonObject = jsonArray.getJSONObject(i);
                    int imageID = Utils.getResIdentifier(context, jsonObject.getString("image"), "drawable");
                    ViewPagerItem viewPagerItem = new ViewPagerItem(imageID, jsonObject.getString("title"), jsonObject.getString("description"));
                    viewPagerItemArrayList.add(viewPagerItem);
                }
            } catch (Exception e) {
                Log.e(UTILS, "Exception" + e);
                return;
            }
            VPAdapter vpAdapter = new VPAdapter(viewPagerItemArrayList);
            viewPager2.setAdapter(vpAdapter);

            // setting the dots layout
            int dotsCount;
            ImageView[] dots;
            dotsCount = vpAdapter.getItemCount();
            dots = new ImageView[dotsCount];
            for (int i = 0; i < dotsCount; i++) {
                dots[i] = new ImageView(context);
                dots[i].setImageDrawable(ContextCompat.getDrawable(context, R.drawable.carousel_dot_inactive));
                LinearLayout.LayoutParams params = new LinearLayout.LayoutParams(28, 28);
                params.setMargins(14, 0, 14, 0);
                sliderDotsPanel.addView(dots[i], params);
                dots[0].setImageDrawable(ContextCompat.getDrawable(context, R.drawable.carousel_dot_active));
                int finalI = i;
                dots[i].setOnClickListener(view -> viewPager2.setCurrentItem(finalI));
            }
            sliderDotsPanel.setLayoutParams(sliderDotsPanelParams);

            viewPager2.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
                @Override
                public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                    super.onPageScrolled(position, positionOffset, positionOffsetPixels);
                }

                @Override
                public void onPageSelected(int position) {
                    //setting active inactive dots
                    for (int i = 0; i < dotsCount; i++) {
                        dots[i].setImageDrawable(ContextCompat.getDrawable(context, R.drawable.carousel_dot_inactive));
                        dots[i].setContentDescription("Page Indicator, Page " + (i + 1) + " : Swipe or Tap To Go To Next Page\"");
                    }
                    dots[position].setImageDrawable(ContextCompat.getDrawable(context, R.drawable.carousel_dot_active));
                    super.onPageSelected(position);
                }

                @Override
                public void onPageScrollStateChanged(int state) {
                    super.onPageScrollStateChanged(state);
                }
            });
            linearLayout.addView(viewPager2);
            linearLayout.setGravity(Gravity.BOTTOM);
            linearLayout.addView(sliderDotsPanel);
            linearLayout.setWeightSum(2);
            scrollView.addView(linearLayout);
            scrollView.setFillViewport(true);
            sliderDotsPanelParams.weight = 1;
            sliderDotsPanel.setGravity(Gravity.BOTTOM | Gravity.CENTER_HORIZONTAL);
            finalParentLayout.addView(scrollView);
        });
    }

    @JavascriptInterface
    public void setYoutubePlayer(String rawJson, final String playerId, String videoStatus) {
        if (bridgeComponents.getActivity() != null) {
            videoDuration = 0;
            ExecutorManager.runOnMainThread(() -> {
                try {
                    if (videoStatus.equals("PAUSE")) {
                        pauseYoutubeVideo();
                    } else {
                        JSONObject json = new JSONObject(rawJson);
                        if (youTubePlayerView != null)
                            youTubePlayerView.release();
                        boolean showMenuButton = json.getBoolean("showMenuButton");
                        boolean showDuration = json.getBoolean("showDuration");
                        boolean setVideoTitle = json.getBoolean("setVideoTitle");
                        boolean showSeekBar = json.getBoolean("showSeekBar");
                        String videoTitle = json.getString("videoTitle");
                        String videoId = json.getString("videoId");
                        String videoType = "VIDEO";
                        if (json.has("videoType")) {
                            videoType = json.getString("videoType");
                        }
                        youTubePlayerView = new YouTubePlayerView(bridgeComponents.getContext());
                        LinearLayout layout = bridgeComponents.getActivity().findViewById(Integer.parseInt(playerId));
                        layout.addView(youTubePlayerView);
                        youTubePlayerView.setEnableAutomaticInitialization(false);
                        YouTubePlayerListener youTubePlayerListener = new AbstractYouTubePlayerListener() {
                            @Override
                            public void onReady(@NonNull YouTubePlayer youTubePlayer) {
                                try {
                                    youtubePlayer = youTubePlayer;
                                    DefaultPlayerUiController playerUiController = new DefaultPlayerUiController(youTubePlayerView, youTubePlayer);
                                    playerUiController.showMenuButton(showMenuButton);
                                    playerUiController.showDuration(showDuration);
                                    playerUiController.showSeekBar(showSeekBar);
                                    playerUiController.showFullscreenButton(true);
                                    if (setVideoTitle) {
                                        playerUiController.setVideoTitle(videoTitle);
                                    }
                                    playerUiController.showYouTubeButton(false);
                                    youTubePlayerView.setCustomPlayerUi(playerUiController.getRootView());

                                    youTubePlayer.seekTo(videoDuration);
                                    youTubePlayer.loadVideo(videoId, 0);
                                    youTubePlayer.play();

                                } catch (Exception e) {
                                    Log.e("error inside setYoutubePlayer onReady", String.valueOf(e));
                                }
                            }

                            @Override
                            public void onCurrentSecond(@NonNull YouTubePlayer youTubePlayer, float second) {
                                videoDuration = second;
                            }
                        };

                        String finalVideoType = videoType;
                        youTubePlayerView.addFullScreenListener(new YouTubePlayerFullScreenListener() {
                            @Override
                            public void onYouTubePlayerExitFullScreen() {
                            }

                            @Override
                            public void onYouTubePlayerEnterFullScreen() {
                                Intent newIntent = new Intent(bridgeComponents.getContext(), YoutubeVideoView.class);
                                newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                newIntent.putExtra("videoId", videoId);
                                newIntent.putExtra("videoDuration", videoDuration);
                                newIntent.putExtra("videoType", finalVideoType);
                                bridgeComponents.getContext().startActivity(newIntent);
                            }
                        });

                        IFramePlayerOptions options = new IFramePlayerOptions.Builder().controls(0).rel(0).build();
                        youTubePlayerView.initialize(youTubePlayerListener, options);
                    }
                } catch (Exception e) {
                    Log.e("exception in setYoutubePlayer", String.valueOf(e));
                }
            });
        }
    }
    
    private final VPAdapter vpAdapter = new VPAdapter(viewPagerItemArrayList, bridgeComponents.getContext(), new VPAdapter.VPAdapterListener(){

        @Override
        public void onViewHolderBind(VPAdapter.ViewHolder holder, int position, Context context) {
            ViewPagerItem viewPagerItem = viewPagerItemArrayList.get(position);
            JSONObject margin = viewPagerItem.getDescriptionMargin();
            JSONObject titleMargin = viewPagerItem.getTitleMargin();
            String titleGravity = viewPagerItem.getTitleGravity();
            int carouselGravity = viewPagerItem.getCarouselGravity();
            String descriptionGravity = viewPagerItem.getDescriptionGravity();

            float density = (Resources.getSystem().getDisplayMetrics().density);
            // imageView Config ------------------------------------------
            if ((viewPagerItem.getContentType()).equals("IMAGE")){
                holder.imageView.setImageResource(viewPagerItem.getImageID());
                holder.imageView.getLayoutParams().height = (int) (viewPagerItem.getImageHeight() * density);
                GradientDrawable gradientDrawable = new GradientDrawable();
                gradientDrawable.setShape(GradientDrawable.RECTANGLE);
                holder.imageView.setVisibility(View.VISIBLE);
                gradientDrawable.setCornerRadii(new float[] {20, 20, 20, 20, 0,0,0,0});
                gradientDrawable.setColor(Color.parseColor(viewPagerItem.getImageBgColor()));
                holder.imageView.setBackground(gradientDrawable);
                holder.video.setVisibility(View.GONE);
            }
            else {
                vpAdapter.embedYoutubeVideo(context, (viewPagerItem.getVideoData()).toString(), "PLAY", holder.video);
                holder.video.setVisibility(View.VISIBLE);
                holder.imageView.setVisibility(View.GONE);
            }

            // Heading text Config ------------------------------------------
            holder.tvHeading.setTextSize(viewPagerItem.getTitleTextSize());
            holder.tvHeading.setTextColor(Color.parseColor(viewPagerItem.getTitleColor()));
            holder.tvHeading.setText(viewPagerItem.getTitleText());
            ViewGroup.MarginLayoutParams titleLayoutParams = (ViewGroup.MarginLayoutParams) holder.tvHeading.getLayoutParams();
            titleLayoutParams.setMargins(titleMargin.optInt("left", 0), titleMargin.optInt("top", 0), titleMargin.optInt("right", 0), titleMargin.optInt("bottom", 0));
            holder.tvHeading.setLayoutParams(titleLayoutParams);
            holder.tvHeading.setGravity(Utils.getGravity(titleGravity));

            // Description text Config ---------------------------------------
            holder.tvDesc.setText(Html.fromHtml(viewPagerItem.getDescriptionText()));
            if(viewPagerItem.getDescriptionText().equals(""))
            {
                holder.tvDesc.setVisibility(View.GONE);
            }
            holder.tvDesc.setTextSize(viewPagerItem.getDescriptionTextSize());
            holder.tvDesc.setTextColor(Color.parseColor(viewPagerItem.getDescriptionColor()));
            ViewGroup.MarginLayoutParams descLayoutParams = (ViewGroup.MarginLayoutParams) holder.tvDesc.getLayoutParams();
            descLayoutParams.setMargins(margin.optInt("left", 0) * 2, margin.optInt("top", 0), margin.optInt("right", 0), margin.optInt("bottom", 0));
            holder.tvDesc.setLayoutParams(descLayoutParams);
            holder.tvDesc.setGravity(Utils.getGravity(descriptionGravity));
            holder.parentLinearLayout.setGravity(carouselGravity);
                    }
                });
                
    @JavascriptInterface
    public void pauseYoutubeVideo() {
        if(vpAdapter != null)
            vpAdapter.pauseYoutubeVideo();
        if ( youtubePlayer != null) {
            youtubePlayer.pause();
        }
        youTubePlayerView = null;
    }

    @JavascriptInterface
    public void detectPhoneNumbers(final String callback) {
        storeDetectPhoneNumbersCallBack = callback;
        HintRequest hintRequest = new HintRequest.Builder()
                .setPhoneNumberIdentifierSupported(true)
                .build();
        PendingIntent intent = Credentials.getClient(bridgeComponents.getContext()).getHintPickerIntent(hintRequest);
        try {
            if (bridgeComponents.getActivity() != null) {
                startIntentSenderForResult(bridgeComponents.getActivity(), intent.getIntentSender(), CREDENTIAL_PICKER_REQUEST, null, 0, 0, 0, new Bundle());
            }
        } catch (IntentSender.SendIntentException e) {
            e.printStackTrace();
        }
    }

    @JavascriptInterface
    public void cleverTapEvent(String event, String params) {
        if (clevertapDefaultInstance != null) {
            Map<String, Object> resultMap = new HashMap<>();
            try {
                JSONArray jsonArray = new JSONArray(params);

                for (int i = 0; i < jsonArray.length(); i++) {
                    JSONObject jsonObject = jsonArray.getJSONObject(i);
                    String key = jsonObject.getString("key");
                    Object value = jsonObject.get("value");
                    resultMap.put(key, value);
                }
            } catch (JSONException e) {
                e.printStackTrace();
                return;
            }
            clevertapDefaultInstance.pushEvent(event, resultMap);
        }
    }

    @JavascriptInterface
    public void setCleverTapUserData(String key, String value) {
        HashMap<String, Object> profileUpdate = new HashMap<>();
        try {
            profileUpdate.put(key, value);
        } catch (Exception e) {
            Log.e(UTILS, "Error sending user data: " + e);
        }

        if (clevertapDefaultInstance != null) {
            clevertapDefaultInstance.onUserLogin(profileUpdate);
            SharedPreferences sharedPrefs = bridgeComponents.getContext().getSharedPreferences(bridgeComponents.getContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String fcmRegId = sharedPrefs.getString("FCM_TOKEN", "null");
            clevertapDefaultInstance.pushFcmRegistrationId(fcmRegId, true);
        }
    }

    @JavascriptInterface
    public void setCleverTapUserProp(String key, String value) {
        HashMap<String, Object> propertiesUpdate = new HashMap<>();
        try {
            propertiesUpdate.put(key, value);
        } catch (Exception e) {
            Log.e(UTILS, "Error sending user data: " + e);
        }
        if (clevertapDefaultInstance != null)
            clevertapDefaultInstance.pushProfile(propertiesUpdate);
    }

    @JavascriptInterface
    public void setCleverTapUserMultipleProp(String arr) {
        if (clevertapDefaultInstance == null)
            clevertapDefaultInstance = CleverTapAPI.getDefaultInstance(bridgeComponents.getContext());

        Map<String, Object> propertiesUpdate = new HashMap<>();
        try {
            JSONArray jsonArray = new JSONArray(arr);

            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject jsonObject = jsonArray.getJSONObject(i);
                String key = jsonObject.getString("key");
                Object value = jsonObject.get("value");
                propertiesUpdate.put(key, value);
            }
        } catch (JSONException e) {
            e.printStackTrace();
            return;
        }
        clevertapDefaultInstance.pushProfile(propertiesUpdate);
    }


    @JavascriptInterface
    public void cleverTapCustomEvent(String event) {
        if (clevertapDefaultInstance != null)
            clevertapDefaultInstance.pushEvent(event);
    }

    @JavascriptInterface
    public void cleverTapCustomEventWithParams(String event, String paramKey, String paramValue) {
        HashMap<String, Object> mapCustomEvent = new HashMap<>();
        mapCustomEvent.put(paramKey, paramValue);
        if (clevertapDefaultInstance != null)
            clevertapDefaultInstance.pushEvent(event, mapCustomEvent);
    }

    @JavascriptInterface
    public void cleverTapSetLocation() {
        Location location = clevertapDefaultInstance.getLocation();
        clevertapDefaultInstance.setLocation(location);
    }

    @JavascriptInterface
    public void openWhatsAppSupport(String contactNumber) {
        String url = "https://api.whatsapp.com/send?phone=" + contactNumber;
        Intent intent = new Intent(Intent.ACTION_VIEW);
        intent.setData(Uri.parse(url));
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        bridgeComponents.getContext().startActivity(intent);
    }

    //region Image Rendering

    @JavascriptInterface
    public void renderBase64Image(String url, String id, boolean fitCenter, String imgScaleType) {
        if (url.contains("http"))
            url = getAPIResponse(url);
        renderBase64ImageFile(url, id, fitCenter, imgScaleType);
    }

    @JavascriptInterface
    public void renderBase64ImageFile(String base64Image, String id, boolean fitCenter, String imgScaleType) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (!base64Image.equals("") && id != null && bridgeComponents.getActivity() != null) {
                    LinearLayout layout = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                    if (layout != null){
                        byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                        Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);
                        ImageView imageView = new ImageView(bridgeComponents.getContext());
                        ViewGroup.LayoutParams layoutParams = new ViewGroup.LayoutParams(layout.getWidth(),layout.getHeight());
                        imageView.setLayoutParams(layoutParams);
                        imageView.setImageBitmap(decodedByte);
                        imageView.setScaleType(getScaleTypes(imgScaleType));
                        imageView.setAdjustViewBounds(true);
                        imageView.setClipToOutline(true);
                        layout.removeAllViews();
                        layout.addView(imageView);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    public ImageView.ScaleType getScaleTypes(String scale) {
        switch (scale) {
            case "MATRIX":
                return ImageView.ScaleType.MATRIX;
            case "FIT_XY":
                return ImageView.ScaleType.FIT_XY;
            case "FIT_START":
                return ImageView.ScaleType.FIT_START;
            case "FIT_END":
                return ImageView.ScaleType.FIT_END;
            case "CENTER":
                return ImageView.ScaleType.CENTER;
            case "CENTER_CROP":
                return ImageView.ScaleType.CENTER_CROP;
            case "CENTER_INSIDE":
                return ImageView.ScaleType.CENTER_INSIDE;
            default:
                return ImageView.ScaleType.FIT_CENTER;
        }
    }

    //endregion

    //region Media Player

    @JavascriptInterface
    public void addMediaFile(String viewID, String source, String actionPlayerID, String playIcon, String pauseIcon, String timerID) {
        Log.d(LOG_TAG, "addMediaFile: " + source);
        Context context = bridgeComponents.getContext();
        Activity activity = bridgeComponents.getActivity();
        ExecutorManager.runOnMainThread(() -> {
            MediaPlayerView audioPlayer;
            if (Integer.parseInt(actionPlayerID) != -1) {
                if (Integer.parseInt(timerID) != -1) {
                    audioPlayer = new MediaPlayerView(context, activity, Integer.parseInt(actionPlayerID), playIcon, pauseIcon, Integer.parseInt(timerID));
                    audioPlayer.setTimerColorAndSize(Color.WHITE, 14);
                    audioPlayer.setVisualizerBarPlayedColor(Color.WHITE);
                } else {
                    audioPlayer = new MediaPlayerView(context, activity, Integer.parseInt(actionPlayerID), playIcon, pauseIcon);
                    audioPlayer.setTimerColorAndSize(Color.GRAY, 14);
                }
            } else {
                audioPlayer = new MediaPlayerView(context, activity);
            }
            try {
                audioPlayer.inflateView(Integer.parseInt(viewID));
                if (source.startsWith("http")) {
                    Thread thread = new Thread(() -> {
                        try {
                            String base64 = getAPIResponse(source);
                            byte[] decodedAudio = Base64.decode(base64, Base64.DEFAULT);
                            File tempMp3 = File.createTempFile("audio_cache", source.substring(source.length()-3), context.getCacheDir());
                            tempMp3.deleteOnExit();
                            FileOutputStream fos = new FileOutputStream(tempMp3);
                            fos.write(decodedAudio);
                            fos.close();
                            FileInputStream fis = new FileInputStream(tempMp3);
                            audioPlayer.addAudioFileInput(fis);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    });
                    thread.start();
                } else {
                    File file = new File(source);
                    FileInputStream fis = new FileInputStream(file);
                    audioPlayer.addAudioFileInput(fis);
                }
                audioPlayers.add(audioPlayer);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public void addMediaPlayer(String viewID, String source) {
        ExecutorManager.runOnMainThread(() -> {
            MediaPlayerView audioPlayer = new MediaPlayerView(bridgeComponents.getContext(), bridgeComponents.getActivity());
            try {
                audioPlayer.inflateView(Integer.parseInt(viewID));
                if (source.contains(".mp3")) {
                    audioPlayer.addAudioFileUrl(source);
                } else {
                    Thread thread = new Thread(() -> {
                        try {
                            String base64 = getAPIResponse(source);
                            byte[] decodedAudio = Base64.decode(base64, Base64.DEFAULT);
                            File tempMp3 = File.createTempFile("audio_cache", "mp3", bridgeComponents.getContext().getCacheDir());
                            tempMp3.deleteOnExit();
                            FileOutputStream fos = new FileOutputStream(tempMp3);
                            fos.write(decodedAudio);
                            fos.close();
                            FileInputStream fis = new FileInputStream(tempMp3);
                            audioPlayer.addAudioFileInput(fis);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    });
                    thread.start();
                }
                audioPlayers.add(audioPlayer);
            } catch (IOException e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public void pauseMediaPlayer() {
        if (DefaultMediaPlayerControl.mediaPlayer.isPlaying()) {
            DefaultMediaPlayerControl.mediaPlayer.pause();
        }
        for (MediaPlayerView audioPlayer : audioPlayers) {
            audioPlayer.onPause(audioPlayer.getPlayer());
        }
    }

    @JavascriptInterface
    public void removeMediaPlayer() {
        try {
            if (audioPlayers != null) {
                for (MediaPlayerView audioPlayer : audioPlayers) {
                    audioPlayer.resetListeners();
                }
                bridgeComponents.getContext().getCacheDir().delete();
                audioPlayers.clear();
                DefaultMediaPlayerControl.mediaPlayer.reset();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //endregion

    //region Audio Recorder

    @JavascriptInterface
    public boolean startAudioRecording() {
        if (isMicrophonePermissionEnabled()) {
            audioRecorder = new AudioRecorder();
            audioRecorder.startRecording(bridgeComponents.getContext());
            System.out.println("Started recording");
            return true;
        } else {
            if (bridgeComponents.getActivity() != null) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{RECORD_AUDIO}, AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION);
            }
            return false;
        }
    }

    public boolean isMicrophonePermissionEnabled() {
        return ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED;
    }

    @JavascriptInterface
    public String stopAudioRecording() {
        if (audioRecorder != null) {
            String res = audioRecorder.stopRecording();
            Log.d(LOG_TAG, "stopAudioRecording: " + res);
            audioRecorder = null;
            return res;
        }
        return null;
    }

    @JavascriptInterface
    public String saveAudioFile(String source) throws IOException {
        File sourceFile = new File(source);
        FileInputStream fis = new FileInputStream(sourceFile);
        File destFile = new File(bridgeComponents.getContext().getFilesDir().getAbsolutePath() + "/final_audio_record.mp3");
        FileOutputStream fos = new FileOutputStream(destFile);
        int n;
        while ((n = fis.read()) != -1) {
            fos.write(n);
        }
        fis.close();
        fos.close();
        return destFile.getAbsolutePath();
    }

    //endregion

    //region Utils

    @JavascriptInterface
    public void clearFocus(String id) {
        if (bridgeComponents.getActivity() != null) {
            ExecutorManager.runOnMainThread(() -> bridgeComponents.getActivity().findViewById(Integer.parseInt(id)).clearFocus());
        }
    }

    @JavascriptInterface
    public void uploadMultiPartData(String filePath, String uploadUrl, String fileType) throws IOException {
        String boundary = UUID.randomUUID().toString();

        URL url = new URL(uploadUrl);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("POST");
        connection.setDoOutput(true);
        connection.setUseCaches(false);
        connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);
        String token = KeyValueStore.read(bridgeComponents.getContext(), bridgeComponents.getSdkName(),"REGISTERATION_TOKEN", "__failed" );
        connection.setRequestProperty("token", token);

        File file = new File(filePath);
        String fileName = file.getName();
        DataOutputStream outputStream = new DataOutputStream(connection.getOutputStream());

        outputStream.writeBytes("--" + boundary + "\r\n");
        outputStream.writeBytes(("Content-Disposition: form-data; name=\"file\"; filename=\"" + fileName + "\"" + "\r\n"));
        if (fileType.equals("Image"))
            outputStream.writeBytes("Content-Type: image/jpeg\r\n");
        else if (fileType.equals("Audio"))
            outputStream.writeBytes("Content-Type: audio/mpeg\r\n");
        outputStream.writeBytes("\r\n");

        FileInputStream fileInputStream = new FileInputStream(file);
        int bytesAvailable = fileInputStream.available();
        int maxBufferSize = 1024 * 1024;
        int bufferSize = Math.min(bytesAvailable, maxBufferSize);

        byte[] buffer = new byte[bufferSize];
        int bytesRead = fileInputStream.read(buffer, 0, bufferSize);
        while (bytesRead > 0) {
            outputStream.write(buffer, 0, bufferSize);
            bytesAvailable = fileInputStream.available();
            bufferSize = Math.min(bytesAvailable, maxBufferSize);
            bytesRead = fileInputStream.read(buffer, 0, bufferSize);
        }
        outputStream.writeBytes("\r\n");
        outputStream.writeBytes("--" + boundary + "\r\n");

        outputStream.writeBytes("Content-Disposition: form-data; name=\"fileType\"" + "\r\n");
        outputStream.writeBytes("Content-Type: application/json" + "\r\n");
        outputStream.writeBytes("\r\n");
        outputStream.writeBytes(fileType);
        outputStream.writeBytes("\r\n");
        outputStream.writeBytes("--" + boundary + "\r\n" + "--");

        int responseCode = connection.getResponseCode();
        String res = "";
        if (responseCode == 200) {
            StringBuilder s_buffer = new StringBuilder();
            InputStream is = new BufferedInputStream(connection.getInputStream());
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(is));
            String inputLine;
            while ((inputLine = bufferedReader.readLine()) != null) {
                s_buffer.append(inputLine);
            }
            res = s_buffer.toString();
            JSONObject jsonObject;
            try {
                jsonObject = new JSONObject(res);
                res = jsonObject.getString("fileId");
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        } else {
            Toast.makeText(bridgeComponents.getContext(), "Unable to upload image", Toast.LENGTH_SHORT).show();
        }
        callUploadMultiPartCallBack(fileType, res);
    }

    private String getAPIResponse(String url) {
        StringBuilder result = new StringBuilder();
        try {
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            connection.setRequestMethod("GET");
            String token = KeyValueStore.read(bridgeComponents.getContext(), bridgeComponents.getSdkName(),"REGISTERATION_TOKEN", "__failed" );
            connection.setRequestProperty("token", token);
            String deviceDetail = KeyValueStore.read(bridgeComponents.getContext(), bridgeComponents.getSdkName(),"DEVICE_DETAILS", "__failed" );
            connection.setRequestProperty("x-device", deviceDetail);
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
                return "";
            } else {
                respReader = new InputStreamReader(connection.getInputStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                return result.toString();
            }
        } catch (Exception e) {
            e.printStackTrace();
            return "";
        }
    }


    @JavascriptInterface
    public void uploadFile() { // TODO : need to handle thr storage permission
        if (!isUploadPopupOpen) {
            ExecutorManager.runOnMainThread(() -> {
                Context context = bridgeComponents.getContext();
                if ((ActivityCompat.checkSelfPermission(context.getApplicationContext(), CAMERA) == PackageManager.PERMISSION_GRANTED)) {
                    if (bridgeComponents.getActivity() != null) {
                        Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                        KeyValueStore.write(context, bridgeComponents.getSdkName(), context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp);
                        Uri photoFile = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", new File(context.getFilesDir(), "IMG_" + timeStamp + ".jpg"));
                        takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
                        Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
                        chooseFromFile.setType("image/*");
                        Intent chooser = Intent.createChooser(takePicture, context.getString(in.juspay.mobility.app.R.string.upload_image));
                        chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[]{chooseFromFile});
                        isUploadPopupOpen = true;
                        bridgeComponents.getActivity().startActivityForResult(chooser, IMAGE_CAPTURE_REQ_CODE, null);
                    }
                } else {
                    if (bridgeComponents.getActivity() != null) {
                        ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{CAMERA, READ_EXTERNAL_STORAGE, WRITE_EXTERNAL_STORAGE}, IMAGE_PERMISSION_REQ_CODE);
                    }
                }
            });
        }
    }

    //endregion

    @Override
    public boolean onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {
            case IMAGE_CAPTURE_REQ_CODE:
                if (resultCode == RESULT_OK) {
                    if (bridgeComponents.getActivity() != null) {
                        isUploadPopupOpen = false;
                        Utils.captureImage(data, bridgeComponents.getActivity(), bridgeComponents.getContext());
                    }
                } else {
                    isUploadPopupOpen = false;
                }
                break;
            case CropImage.CROP_IMAGE_ACTIVITY_REQUEST_CODE:
                if (resultCode == RESULT_OK) {
                    new Thread(() -> Utils.encodeImageToBase64(data, bridgeComponents.getContext())).start();
                } else if (resultCode == CropImage.CROP_IMAGE_ACTIVITY_RESULT_ERROR_CODE) {
                    CropImage.ActivityResult result = CropImage.getActivityResult(data);
                    Log.e(OVERRIDE, result.getError().toString());
                }
                break;
            case CREDENTIAL_PICKER_REQUEST:
                if (resultCode == RESULT_OK) {
                    Credential credentials = data.getParcelableExtra(Credential.EXTRA_KEY);
                    String selectedNumber = credentials.getId().substring(3);
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                            storeDetectPhoneNumbersCallBack, selectedNumber); //mobile_number
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                }
                break;
        }
        return super.onActivityResult(requestCode, resultCode, data);
    }

    @Override
    public boolean onRequestPermissionResult(int requestCode, String[] permissions, int[] grantResults) {
        switch (requestCode) {
            case IMAGE_PERMISSION_REQ_CODE:
                Context context = bridgeComponents.getContext();
                if ((ActivityCompat.checkSelfPermission(context, CAMERA) == PackageManager.PERMISSION_GRANTED)) {
                    if (bridgeComponents.getActivity() != null) {
                        Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                        String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", Locale.getDefault()).format(new Date());
                        KeyValueStore.write(context, bridgeComponents.getSdkName(), context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp);
                        Uri photoFile = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", new File(context.getFilesDir(), "IMG_" + timeStamp + ".jpg"));
                        takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
                        Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
                        chooseFromFile.setType("image/*");
                        Intent chooser = Intent.createChooser(takePicture, context.getString(in.juspay.mobility.app.R.string.upload_image));
                        chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[]{chooseFromFile});
                        bridgeComponents.getActivity().startActivityForResult(chooser, IMAGE_CAPTURE_REQ_CODE, null);
                    }
                } else {
                    Toast.makeText(context, context.getString(in.juspay.mobility.app.R.string.please_allow_permission_to_capture_the_image), Toast.LENGTH_SHORT).show();
                }
                break;
            case AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION:
                if (grantResults.length > 0 && grantResults[0] != PackageManager.PERMISSION_GRANTED) {
                    Toast.makeText(bridgeComponents.getContext(), "Permission Denied", Toast.LENGTH_SHORT).show();
                }
                break;
            default:
                break;
        }
        return super.onRequestPermissionResult(requestCode, permissions, grantResults);
    }

}