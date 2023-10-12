package in.juspay.mobility.driver;

import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.RECORD_AUDIO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static android.app.Activity.RESULT_OK;
import static android.content.Context.WINDOW_SERVICE;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.net.Uri;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.os.PowerManager;
import android.provider.MediaStore;
import android.provider.Settings;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.ActionMode;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.webkit.JavascriptInterface;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;
import androidx.core.content.FileProvider;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;
import androidx.work.Constraints;
import androidx.work.ExistingPeriodicWorkPolicy;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.CustomCap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;
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
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.mobility.app.AudioRecorder;
import in.juspay.mobility.app.CheckPermissionOverlay;
import in.juspay.mobility.app.LocationUpdateService;
import in.juspay.mobility.app.LocationUpdateWorker;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.OverlaySheetService;
import in.juspay.mobility.app.TranslatorMLKit;
import in.juspay.mobility.app.Utils;
import in.juspay.mobility.app.callbacks.CallBack;
import in.juspay.mobility.common.MobilityCommonBridge;
import in.juspay.mobility.driver.mediaPlayer.DefaultMediaPlayerControl;

public class MobilityDriverBridge extends MobilityCommonBridge {

    private static final String LOG_TAG = "MobilityDriverBridge";

    // Constants
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;

    // Media Utils
    public static YouTubePlayerView youTubePlayerView;
    public static YouTubePlayer youtubePlayer;
    public static float videoDuration = 0;
    public static ArrayList<MediaPlayerView> audioPlayers = new ArrayList<>();
    private AudioRecorder audioRecorder = null;

    // Others
    public static boolean isUploadPopupOpen = false;

    // CallBacks
    private String storeDriverCallBack = null;
    private String storeUpdateTimeCallBack = null;
    private String storeImageUploadCallBack = null;
    private CallBack callBack;
    private LocationUpdateService.UpdateTimeCallback locationCallback;

    public MobilityDriverBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        registerCallBacks();
    }

    //region Store and Trigger CallBack
    @JavascriptInterface
    public void storeCallBackForNotification(String callback) {
        storeDriverCallBack = callback;
    }

    public void callDriverNotificationCallBack(String notificationType) {
        if (storeDriverCallBack != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeDriverCallBack, notificationType);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void translateString(String callback, String toTranslate)
    {
        String lang = getKeysInSharedPref("LANGUAGE_KEY");
        TranslatorMLKit translator = new TranslatorMLKit("en", lang, bridgeComponents.getContext());
        translator.translateStringWithCallback(toTranslate, callback, bridgeComponents);
    }

    @JavascriptInterface
    public void storeCallBackTime(String callback) {
        storeUpdateTimeCallBack = callback;
    }

    public void callUpdateTimeCallBack(String time, String lat, String lng) {
        if (storeUpdateTimeCallBack != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s','%s','%s');",
                    storeUpdateTimeCallBack, time, lat, lng);
            Log.d(CALLBACK, javascript);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
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

    public void registerCallBacks() {
        if (isClassAvailable("in.juspay.mobility.app.callbacks.CallBack")) {
            callBack = new CallBack() {
                @Override
                public void customerCallBack(String notificationType) {
                    Log.i(OTHERS, "No Customer CallBack Required");
                }

                @Override
                public void driverCallBack(String notificationType) {
                    callDriverNotificationCallBack(notificationType);
                }

                @Override
                public void imageUploadCallBack(String encImage, String filename, String filePath) {
                    callImageUploadCallBack(encImage, filename, filePath);
                }

                @Override
                public void chatCallBack(String message, String sentBy, String time, String len) {
                    Log.i(OTHERS, "No Required");
                }

                @Override
                public void inAppCallBack(String onTapAction) {
                    Log.i(OTHERS, "No Required");
                }
            };
            NotificationUtils.registerCallback(callBack);
            Utils.registerCallback(callBack);
        }
        if (isClassAvailable("in.juspay.mobility.app.LocationUpdateService")) {
            locationCallback = this::callUpdateTimeCallBack;
            LocationUpdateService.registerCallback(locationCallback);
        }
        if (isClassAvailable("in.juspay.mobility.app.OverlaySheetService")) {
            OverlaySheetService.registerCallback(callBack);
        }
    }

    public void onDestroy() {
        Log.e("onDestroy","onDestroy");
        NotificationUtils.deRegisterCallback(callBack);
        Utils.deRegisterCallback(callBack);
        DefaultMediaPlayerControl.mediaPlayer.reset();
        if (isClassAvailable("in.juspay.mobility.app.OverlaySheetService")) {
            OverlaySheetService.deRegisterCallback(callBack);
        }
        if (isClassAvailable("in.juspay.mobility.app.LocationUpdateService")) {
            LocationUpdateService.deRegisterCallback(locationCallback);
        }
        // Clearing all static variables
        // Media Utils
        youTubePlayerView = null;
        youtubePlayer = null;
        videoDuration = 0;
        audioPlayers = new ArrayList<>();
        audioRecorder = null;

        // Others
        isUploadPopupOpen = false;

        // CallBacks
        storeDriverCallBack = null;
        storeUpdateTimeCallBack = null;
        storeImageUploadCallBack = null;
        callBack = null;
    }
    //endregion

    //region Location
    @JavascriptInterface
    public void startLocationPollingAPI() {
        if (isClassAvailable("in.juspay.mobility.app.LocationUpdateService")) {
            Intent locationUpdateService = new Intent(bridgeComponents.getContext(), LocationUpdateService.class);
            locationUpdateService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
            WorkManager mWorkManager;
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                bridgeComponents.getContext().getApplicationContext().startForegroundService(locationUpdateService);
            } else {
                bridgeComponents.getContext().startService(locationUpdateService);
            }
            mWorkManager = WorkManager.getInstance(bridgeComponents.getContext());
            Constraints constraints = new Constraints.Builder()
                    .setRequiresDeviceIdle(true)
                    .build();
            PeriodicWorkRequest mWorkRequest = new PeriodicWorkRequest.Builder(LocationUpdateWorker.class, 13, TimeUnit.MINUTES).addTag(bridgeComponents.getContext().getString(R.string.location_update)).setConstraints(constraints).build();
            mWorkManager.enqueueUniquePeriodicWork(bridgeComponents.getContext().getString(R.string.location_update), ExistingPeriodicWorkPolicy.UPDATE, mWorkRequest);
            Log.i(LOCATION, "Start Location Polling");
        }
    }

    @JavascriptInterface
    public void stopLocationPollingAPI() {
        Intent locationUpdateService = new Intent(bridgeComponents.getContext(), LocationUpdateService.class);
        bridgeComponents.getContext().stopService(locationUpdateService);
        WorkManager mWorkManager = WorkManager.getInstance(bridgeComponents.getContext());
        mWorkManager.cancelAllWorkByTag(bridgeComponents.getContext().getString(R.string.location_update));
        Log.i(LOCATION, "Stop Location Update Polling");
    }

    //endregion

    //region Media Utils
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

    @JavascriptInterface
    public void pauseYoutubeVideo() {
        if (youTubePlayerView != null) {
            youtubePlayer.pause();
        }
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
    public void previewImage(String base64Image) {
        Activity activity = bridgeComponents.getActivity();
        if (activity != null) {
            ExecutorManager.runOnMainThread(() -> {
                try {
                    if (!base64Image.equals("")) {
                        byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                        Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);

                        AlertDialog.Builder builder = new AlertDialog.Builder(activity);
                        builder.setCancelable(true);
                        ImageView imagePreview = new ImageView(activity);
                        imagePreview.setImageBitmap(decodedByte);

                        DisplayMetrics displayMetrics = new DisplayMetrics();
                        activity.getWindowManager().getDefaultDisplay().getMetrics(displayMetrics);
                        int screenHeight = displayMetrics.heightPixels;
                        int width = displayMetrics.widthPixels;
                        imagePreview.setMinimumHeight(screenHeight / 2);
                        imagePreview.setMinimumWidth(width);

                        ViewGroup.LayoutParams layoutParams = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.MATCH_PARENT);
                        imagePreview.setLayoutParams(layoutParams);
                        builder.setView(imagePreview);
                        AlertDialog alertDialog = builder.create();
                        alertDialog.show();
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
        }
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

    /*
     * This function is deprecated on 22 May - 2023
     * Added only for Backward Compatibility
     * Remove this function once it is not begin used.
     */

    @JavascriptInterface
    public void renderBase64Image(String url, String id) {
        String base64Image = getAPIResponse(url);
        if (bridgeComponents.getActivity() != null) {
            ExecutorManager.runOnMainThread(() -> {
                try {
                    if (!base64Image.equals("") && id != null) {
                        LinearLayout layout = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
                        byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                        Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);
                        ImageView imageView = new ImageView(bridgeComponents.getContext());
                        imageView.setImageBitmap(decodedByte);
                        imageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
                        imageView.setAdjustViewBounds(true);
                        imageView.setClipToOutline(true);
                        layout.removeAllViews();
                        layout.addView(imageView);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
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

    @Override
    public void reset() {
        onDestroy();
        super.reset();
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
                        setKeysInSharedPrefs(context.getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp);
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

    public boolean isMicrophonePermissionEnabled() {
        return ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED;
    }

    @JavascriptInterface
    public boolean startAudioRecording() {
        if (isMicrophonePermissionEnabled()) {
            audioRecorder = new AudioRecorder();
            audioRecorder.startRecording(bridgeComponents.getContext());
            return true;
        } else {
            if (bridgeComponents.getActivity() != null) {
                ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{RECORD_AUDIO}, AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION);
            }
            return false;
        }
    }

    @JavascriptInterface
    public String saveAudioFile(String source) throws IOException {
        File sourceFile = new File(source);
        FileInputStream fis = new FileInputStream(sourceFile);
        File destFile = new File(bridgeComponents.getContext().getFilesDir().getAbsolutePath() + "final_audio_record.mp3");
        FileOutputStream fos = new FileOutputStream(destFile);
        int n;
        while ((n = fis.read()) != -1) {
            fos.write(n);
        }
        fis.close();
        fos.close();
        return destFile.getAbsolutePath();
    }

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
                            File tempMp3 = File.createTempFile("audio_cache", "mp3", context.getCacheDir());
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
    //endregion

    //region Permission Utils
    @SuppressLint("BatteryLife")
    @JavascriptInterface
    public void requestBatteryPermission() {
        try {
            Intent intent = new Intent(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
            Uri uri = Uri.fromParts("package", bridgeComponents.getContext().getPackageName(), null);
            intent.setData(uri);
            String packageName = bridgeComponents.getContext().getPackageName();
            PowerManager pm = (PowerManager) bridgeComponents.getContext().getSystemService(Context.POWER_SERVICE);
            if (pm.isIgnoringBatteryOptimizations(packageName)) {
                intent.setAction(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
            } else {
                intent.setAction(Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
            }
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            bridgeComponents.getContext().startActivity(intent);
        } catch (ActivityNotFoundException e) {
            e.printStackTrace();
            Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
            Uri uri = Uri.fromParts("package", bridgeComponents.getContext().getPackageName(), null);
            intent.setData(uri);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            bridgeComponents.getContext().startActivity(intent);
        }
    }

    @JavascriptInterface
    public boolean isBatteryPermissionEnabled() {
        PowerManager powerManager = (PowerManager) bridgeComponents.getContext().getSystemService(Context.POWER_SERVICE);
        return (powerManager.isIgnoringBatteryOptimizations(bridgeComponents.getContext().getPackageName()));
    }

    @JavascriptInterface
    public boolean isOverlayPermissionEnabled() {
        if (NotificationUtils.overlayFeatureNotAvailable(bridgeComponents.getContext())) {
            return true;
        }
        return Settings.canDrawOverlays(bridgeComponents.getContext());
    }

    @JavascriptInterface
    public void checkOverlayPermission() {
        System.out.println("CommonJsInterface checkOverlayPermission()");
        if (!Settings.canDrawOverlays(bridgeComponents.getContext())) {
            requestOverlayPermission();
            System.out.print("After request permission");
        }
    }

    @JavascriptInterface
    public void requestAutoStartPermission() {
        CheckPermissionAutoStart.getInstance().getAutoStartPermission(bridgeComponents.getContext());
    }

    @JavascriptInterface
    public void requestOverlayPermission() {
        try {
            Intent intent = new Intent(bridgeComponents.getContext(), CheckPermissionOverlay.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            bridgeComponents.getContext().startActivity(intent);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in request permission", e);
        }
    }

    //endregion

    // region Maps
    @JavascriptInterface
    public void mapSnapShot(final String pureScriptId, final String json, final String routeType, final boolean actualRoute, final String callback) {
        try {
            if (bridgeComponents.getActivity() != null) {
                ExecutorManager.runOnMainThread(() -> {
                    try {
                        SupportMapFragment mapFragment = SupportMapFragment.newInstance();
                        FragmentManager supportFragmentManager = ((FragmentActivity) bridgeComponents.getActivity()).getSupportFragmentManager();
                        FragmentTransaction fragmentTransaction = supportFragmentManager.beginTransaction();
                        fragmentTransaction.add(Integer.parseInt(pureScriptId), mapFragment);
                        fragmentTransaction.commitAllowingStateLoss();
                        mapFragment.getMapAsync(googleMap -> {
                            this.googleMap = googleMap;
                            googleMap.getUiSettings().setAllGesturesEnabled(false);
                            googleMap.getUiSettings().setRotateGesturesEnabled(false);
                            googleMap.getUiSettings().setMyLocationButtonEnabled(false);
                            markers = new JSONObject();
                            markersElement.put(pureScriptId, markers);
                            this.googleMap.setOnMapLoadedCallback(new GoogleMap.OnMapLoadedCallback() {
                                @Override
                                public synchronized void onMapLoaded() {
                                    showRoute(json, routeType, "#323643", actualRoute, "ny_ic_dest_marker", "ny_ic_src_marker", 8);
                                    final Handler handler = new Handler();
                                    handler.postDelayed(() -> {
                                        GoogleMap.SnapshotReadyCallback callback2 = new GoogleMap.SnapshotReadyCallback() {
                                            Bitmap bitmap;

                                            @Override
                                            public void onSnapshotReady(Bitmap snapshot) {
                                                bitmap = snapshot;
                                                String encImage = "";
                                                try {
                                                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
                                                    bitmap.compress(Bitmap.CompressFormat.JPEG, 80, baos);
                                                    byte[] b = baos.toByteArray();
                                                    encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                                                } catch (Exception e) {
                                                    e.printStackTrace();
                                                }

                                                if (callback != null) {
                                                    Log.i(MAPS, encImage);
                                                    String javascript = String.format("window.callUICallback('%s','%s');", callback, encImage);
                                                    Log.e(LOG_TAG, javascript);
                                                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                                }
                                            }
                                        };
                                        googleMap.snapshot(callback2);
                                    }, 2000);
                                }
                            });
                        });
                    } 
                    catch (Exception e) {
                        Log.e(LOG_TAG, "Error in mapSnapShot " + e);
                    }
                });
            }
        } catch (Exception e) {
            Log.e("ADD_MARKER", e.toString());
        }
    }


    @JavascriptInterface
    public void showRoute(final String json, final String style, final String trackColor, final boolean isActual, final String sourceMarker, final String destMarker, final int polylineWidth) {
        ExecutorManager.runOnMainThread(() -> {
            if (googleMap != null) {
                Log.i(MAPS, "Show Route");
                PolylineOptions polylineOptions = new PolylineOptions();
                int color = Color.parseColor(trackColor);
                try {
                    JSONObject jsonObject = new JSONObject(json);
                    JSONArray coordinates = jsonObject.getJSONArray("points");
                    JSONObject sourceCoordinates = (JSONObject) coordinates.get(0);
                    JSONObject destCoordinates = (JSONObject) coordinates.get(coordinates.length() - 1);
                    double sourceLat = sourceCoordinates.getDouble("lat");
                    double sourceLong = sourceCoordinates.getDouble("lng");
                    double destLat = destCoordinates.getDouble("lat");
                    double destLong = destCoordinates.getDouble("lng");

                    double source_lat, source_lng, destination_lat, destination_lng;
                    if (sourceLat <= destLat) {
                        source_lat = sourceLat - 0.4 * (destLat - sourceLat);
                        destination_lat = destLat + 0.1 * (destLat - sourceLat);
                    } else {
                        source_lat = sourceLat + 0.1 * (sourceLat - destLat);
                        destination_lat = destLat - 0.4 * (sourceLat - destLat);
                    }
                    if (sourceLong <= destLong) {
                        source_lng = sourceLong - 0.09 * (destLong - sourceLong);
                        destination_lng = destLong + 0.09 * (destLong - sourceLong);
                    } else {
                        source_lng = sourceLong + 0.09 * (sourceLong - destLong);
                        destination_lng = destLong - 0.09 * (sourceLong - destLong);
                    }

                    if (googleMap != null) {
                        try {
                            LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                            LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                            LatLngBounds bounds = LatLngBounds.builder().include(pickupLatLng).include(destinationLatLng).build();
                            googleMap.moveCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                        } catch (IllegalArgumentException e) {
                            LatLng pickupLatLng = new LatLng(source_lat, source_lng);
                            LatLng destinationLatLng = new LatLng(destination_lat, destination_lng);
                            LatLngBounds bounds = LatLngBounds.builder().include(destinationLatLng).include(pickupLatLng).build();
                            googleMap.moveCamera(CameraUpdateFactory.newLatLngBounds(bounds, 0));
                        } catch (Exception e) {
                            System.out.println("In mmove camera in catch exception " + e);
                        }
                    }
                    if (isActual) {
                        for (int i = coordinates.length() - 1; i >= 0; i--) {
                            JSONObject coordinate = (JSONObject) coordinates.get(i);
                            double lng = coordinate.getDouble("lng");
                            double lat = coordinate.getDouble("lat");
                            polylineOptions.add(new LatLng(lat, lng));
                        }
                    } else {
                        LatLng fromPointObj = new LatLng(sourceLat, sourceLong);
                        LatLng toPointObj = new LatLng(destLat, destLong);
                        polylineOptions.add(toPointObj);
                        polylineOptions.add(fromPointObj);
                    }
                    Polyline polyline = setRouteCustomTheme(polylineOptions, color, style, polylineWidth);

                    if (sourceMarker != null && !sourceMarker.equals("")) {
                        Bitmap sourceBitmap = constructBitmap(90, sourceMarker);
                        polyline.setStartCap(
                                new CustomCap(
                                        BitmapDescriptorFactory.fromBitmap(sourceBitmap)
                                )
                        );
                    }

                    if (destMarker != null && !destMarker.equals("")) {
                        Bitmap destBitmap = constructBitmap(90, destMarker);
                        polyline.setEndCap(
                                new CustomCap(
                                        BitmapDescriptorFactory.fromBitmap(destBitmap)
                                )
                        );
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        });
    }
    //endregion

    //region Others
    private String getAPIResponse(String url) {
        if (url.equals("")) return "";
        StringBuilder result = new StringBuilder();
        try {
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            connection.setRequestMethod("GET");
            connection.setRequestProperty("token", getKeysInSharedPref("REGISTERATION_TOKEN"));
            connection.setRequestProperty("x-device", getKeysInSharedPref("DEVICE_DETAILS"));
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
    public void launchAppSettings() {
        Context context = bridgeComponents.getContext();
        Intent appSettingsIntent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
        appSettingsIntent.setData(Uri.fromParts("package", context.getPackageName(), null));
        appSettingsIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        context.startActivity(appSettingsIntent);
    }

    @JavascriptInterface
    public void launchDateSettings() {
        try {
            bridgeComponents.getContext().startActivity(new Intent(android.provider.Settings.ACTION_DATE_SETTINGS).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK));
        }catch (ActivityNotFoundException e){
            bridgeComponents.getContext().startActivity(new Intent(android.provider.Settings.ACTION_SETTINGS).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK));
        }
    }

    @JavascriptInterface
    public void disableActionEditText(final String id) {
        if (bridgeComponents.getActivity() != null) {
            EditText editText = bridgeComponents.getActivity().findViewById(Integer.parseInt(id));
            if (editText != null) {
                editText.setCustomSelectionActionModeCallback(new ActionMode.Callback() {
                    @Override
                    public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
                        return false;
                    }

                    @Override
                    public void onDestroyActionMode(ActionMode mode) {
                    }

                    @Override
                    public boolean onCreateActionMode(ActionMode mode, Menu menu) {
                        return false;
                    }

                    @Override
                    public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
                        return false;
                    }
                });
                editText.setLongClickable(false);
                editText.setTextIsSelectable(false);
            }
        }
    }

    @JavascriptInterface
    public void clearFocus(String id) {
        if (bridgeComponents.getActivity() != null) {
            ExecutorManager.runOnMainThread(() -> bridgeComponents.getActivity().findViewById(Integer.parseInt(id)).clearFocus());
        }
    }

    @JavascriptInterface
    public String uploadMultiPartData(String filePath, String uploadUrl, String fileType) throws IOException {
        String boundary = UUID.randomUUID().toString();

        URL url = new URL(uploadUrl);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("POST");
        connection.setDoOutput(true);
        connection.setUseCaches(false);
        connection.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);
        connection.setRequestProperty("token", getKeyInNativeSharedPrefKeys("REGISTERATION_TOKEN"));

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
        return res;
    }

    @JavascriptInterface
    public void setScaleType(String id, String imageUrl, String scaleType) {
        Activity activity = bridgeComponents.getActivity();
        if (activity == null) return;
        if (id != null) {
            ImageView imageView = activity.findViewById(Integer.parseInt(id));
            try {
                URL url = new URL(imageUrl);
                Bitmap bitmap = BitmapFactory.decodeStream(url.openConnection().getInputStream());
                Handler mainLooper = new Handler(Looper.getMainLooper());
                mainLooper.post(() -> {
                    if (bitmap == null) return;
                    imageView.getLayoutParams().height = (getScreenWidth() * bitmap.getHeight()) / bitmap.getWidth();
                    imageView.setScaleType(getScaleTypes(scaleType));
                    imageView.setImageBitmap(bitmap);
                    LinearLayout linearLayout = (LinearLayout) imageView.getParent();
                    linearLayout.removeAllViews();
                    linearLayout.addView(imageView);
                    imageView.setVisibility(View.VISIBLE);
                });
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    public int getScreenWidth() {
        DisplayMetrics dm = new DisplayMetrics();
        ((WindowManager) bridgeComponents.getContext().getSystemService(WINDOW_SERVICE)).getDefaultDisplay().getRealMetrics(dm);
        return dm.widthPixels;
    }

    @JavascriptInterface
    public int methodArgumentCount(String functionName) {
        try {
            methods = methods == null ? this.getClass().getMethods() : methods;
            for (Method m : methods) {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                    if (m.getName().equals(functionName)) {
                        return m.getParameterCount();
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }
    // endregion

    //region Override Funtions
    @Override
    public boolean onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {
            case IMAGE_CAPTURE_REQ_CODE:
                isUploadPopupOpen = false;
                if (resultCode == RESULT_OK) {
                    if (bridgeComponents.getActivity() != null) {
                        Utils.captureImage(data, bridgeComponents.getActivity(), bridgeComponents.getContext());
                    }
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
                        setKeysInSharedPrefs(context.getResources().getString(in.juspay.mobility.app.R.string.TIME_STAMP_FILE_UPLOAD), timeStamp);
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
            case REQUEST_CALL:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    showDialer(phoneNumber, false);
                } else {
                    toast("Permission Denied");
                }
                break;
            case LOCATION_PERMISSION_REQ_CODE:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    Log.i(OVERRIDE, "Location Permission Granted");
                } else {
                    toast("Permission Denied");
                }
                break;
            case STORAGE_PERMISSION:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED && downloadLayout != null) {
                    try {
                        JuspayLogger.d(OTHERS, "Storage Permission is granted. downloading  PDF");
                        downloadLayoutAsImage(downloadLayout);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                } else {
                    JuspayLogger.d(OTHERS, "Storage Permission is denied.");
                    toast("Permission Denied");
                }
                break;
            case AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION:
                if (grantResults.length > 0 && grantResults[0] != PackageManager.PERMISSION_GRANTED) {
                    Toast.makeText(bridgeComponents.getContext(), "Permission Denied", Toast.LENGTH_SHORT).show();
                }
                break;
        }
        return super.onRequestPermissionResult(requestCode, permissions, grantResults);
    }
    //endregion
}


