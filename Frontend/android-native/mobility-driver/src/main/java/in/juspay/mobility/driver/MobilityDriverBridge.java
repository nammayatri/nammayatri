package in.juspay.mobility.driver;

import static android.Manifest.permission.CAMERA;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.RECORD_AUDIO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;
import static android.app.Activity.RESULT_OK;
import static android.content.Context.WINDOW_SERVICE;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import com.google.common.io.BaseEncoding;
import com.google.common.util.concurrent.ListenableFuture;
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
import android.view.LayoutInflater;
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
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;
import androidx.work.Constraints;
import androidx.work.ExistingPeriodicWorkPolicy;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;
import androidx.camera.core.CameraSelector;
import androidx.camera.core.ImageAnalysis;
import androidx.camera.core.ImageCapture;
import androidx.camera.core.ImageCaptureException;
import androidx.camera.core.ImageProxy;
import androidx.camera.core.Preview;
import androidx.camera.lifecycle.ProcessCameraProvider;
import androidx.camera.view.PreviewView;
import androidx.lifecycle.LifecycleOwner;

import org.apache.commons.math3.analysis.UnivariateFunction;
import org.apache.commons.math3.analysis.solvers.BrentSolver;

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

import org.apache.commons.math3.special.Gamma;
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.app.AudioRecorder;
import in.juspay.mobility.app.CheckPermissionOverlay;
import in.juspay.mobility.app.LocationUpdateService;
import in.juspay.mobility.app.LocationUpdateWorker;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.OverlaySheetService;
import in.juspay.mobility.app.Utils;
import in.juspay.mobility.app.callbacks.CallBack;
import in.juspay.mobility.common.MobilityCommonBridge;
import in.juspay.mobility.driver.mediaPlayer.DefaultMediaPlayerControl;

import android.os.Looper;
import android.view.View.OnClickListener;
import android.content.ContentValues;
import android.graphics.BitmapShader;
import android.graphics.Shader;
import android.graphics.Matrix;
import android.widget.Button;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;

public class MobilityDriverBridge extends MobilityCommonBridge {

    private static final String LOG_TAG = "MobilityDriverBridge";

    // Constants
    private static final int IMAGE_CAPTURE_REQ_CODE = 101;
    private static final int IMAGE_PERMISSION_REQ_CODE = 4997;
    private static final int IMAGE_PERMISSION_REQ_CODE_PROFILE = 1243;

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

    private PreviewView previewView;
    private ImageCapture imageCapture;
    private Button bCapture;
    public static Runnable cameraPermissionCallback;

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
                    new Thread(() -> Utils.encodeImageToBase64(data, bridgeComponents.getContext(), null)).start();
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
        Context context = bridgeComponents.getContext();
        switch (requestCode) {
            case IMAGE_PERMISSION_REQ_CODE:
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
            case AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION:
                if (grantResults.length > 0 && grantResults[0] != PackageManager.PERMISSION_GRANTED) {
                    Toast.makeText(bridgeComponents.getContext(), "Permission Denied", Toast.LENGTH_SHORT).show();
                }
                break;
            case IMAGE_PERMISSION_REQ_CODE_PROFILE:
              if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
              if (cameraPermissionCallback != null)
               {
                cameraPermissionCallback.run();
                cameraPermissionCallback = null;
               }
               } 
               else 
                {
                    Toast.makeText(context, R.string.need_permission_to_access_the_camera, Toast.LENGTH_SHORT).show();
                    callImageUploadCallBack( "", "", "");
                }
                break;
        }
        return super.onRequestPermissionResult(requestCode, permissions, grantResults);
    }
    // endRegion

  // driver Profile functions

    @JavascriptInterface
    public void renderBase64ImageCircular(String url, String id) {
    String base64Image = url;
        Activity activity = bridgeComponents.getActivity();
        Context context = bridgeComponents.getContext();
     if (activity != null) {
    activity.runOnUiThread(() -> {
       
            if (!base64Image.equals("") && base64Image != null && id != null) {
                LinearLayout layout = activity.findViewById(Integer.parseInt(id));
                if(layout!=null)
                {
                byte[]  decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);
                ImageView imageView = new ImageView(context);
                BitmapDrawable bitmapDrawable = new BitmapDrawable(context.getResources(), decodedByte);
                Drawable circularDrawable = createCircularDrawable(bitmapDrawable);
                imageView.setImageDrawable(circularDrawable);
                imageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
                imageView.setAdjustViewBounds(true);
                imageView.setClipToOutline(true);
                imageView.setRotation(270);
                layout.removeAllViews();
                layout.addView(imageView);
                }
            }
    });
     }
}

private Drawable createCircularDrawable(BitmapDrawable bitmapDrawable) {
    Context context = bridgeComponents.getContext();
    Bitmap bitmap = bitmapDrawable.getBitmap();
    Bitmap circularBitmap = Bitmap.createBitmap(bitmap.getWidth(), bitmap.getHeight(), Bitmap.Config.ARGB_8888);
    Canvas canvas = new Canvas(circularBitmap);
    Paint paint = new Paint();
    paint.setAntiAlias(true);
    BitmapShader shader = new BitmapShader(bitmap, Shader.TileMode.CLAMP, Shader.TileMode.CLAMP);
    Matrix matrix = new Matrix();
    matrix.setTranslate((bitmap.getWidth() - canvas.getWidth()) / 2f, (bitmap.getHeight() - canvas.getHeight()) / 2f);
    shader.setLocalMatrix(matrix);
    paint.setShader(shader);
    float radius = Math.min(bitmap.getWidth(), bitmap.getHeight()) / 2f;
    canvas.drawCircle(canvas.getWidth() / 2f, canvas.getHeight() / 2f, radius, paint);
    return new BitmapDrawable(context.getResources(), circularBitmap);
}

@JavascriptInterface
    public void renderCameraProfilePicture(String id) {
    Activity activity = bridgeComponents.getActivity();
    Context context = bridgeComponents.getContext();
         if(activity!=null)
         {
        activity.runOnUiThread(() -> {
             if (isCameraPermissionGranted()) 
            {
            View profilePictureLayout = LayoutInflater.from(context).inflate(R.layout.profile_picture_camera_preview, null, false);
            previewView = profilePictureLayout.findViewById(R.id.previewView);
            bCapture = profilePictureLayout.findViewById(R.id.bCapture);
            bCapture.setOnClickListener(view -> capturePhoto());
            ListenableFuture<ProcessCameraProvider> cameraProviderFuture = ProcessCameraProvider.getInstance(context);
            cameraProviderFuture.addListener(() -> {
            try {
                ProcessCameraProvider cameraProvider = cameraProviderFuture.get();
                startCameraX(cameraProvider);
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
                return ;
            }
            }, ContextCompat.getMainExecutor(activity));
             LinearLayout layout = activity.findViewById(Integer.parseInt(id));
             layout.removeAllViews();
             layout.addView(profilePictureLayout);
           } else 
              {
            requestCameraPermission(() -> renderCameraProfilePicture(id));   
              }    
            });
        }
    }

private boolean isCameraPermissionGranted() {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
        Context context = bridgeComponents.getContext();
        int cameraPermission = ContextCompat.checkSelfPermission(context, Manifest.permission.CAMERA);
        return cameraPermission == PackageManager.PERMISSION_GRANTED;
    }
    return true;
}

private void requestCameraPermission(Runnable callback) {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
        Activity activity = bridgeComponents.getActivity();
        ActivityCompat.requestPermissions(activity, new String[]{Manifest.permission.CAMERA}, IMAGE_PERMISSION_REQ_CODE_PROFILE);
        cameraPermissionCallback = callback;
    }
}

@SuppressLint("RestrictedApi")
    private void startCameraX(ProcessCameraProvider cameraProvider) {
        Activity activity = bridgeComponents.getActivity();
        cameraProvider.unbindAll();
        CameraSelector cameraSelector = new CameraSelector.Builder()
                .requireLensFacing(CameraSelector.LENS_FACING_FRONT)
                .build();
        Preview preview = new Preview.Builder()
                .build();
        preview.setSurfaceProvider(previewView.getSurfaceProvider());
        imageCapture = new ImageCapture.Builder()
                .setCaptureMode(ImageCapture.CAPTURE_MODE_MINIMIZE_LATENCY)
                .build();
        ImageAnalysis imageAnalysis = new ImageAnalysis.Builder()
                .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
                .build();
        imageAnalysis.setAnalyzer(ContextCompat.getMainExecutor(activity), this::analyze);
        cameraProvider.bindToLifecycle((LifecycleOwner) activity, cameraSelector, preview, imageCapture);
    }

    public void analyze(@NonNull ImageProxy image) {
        Log.d("TAG", "analyze: got the frame at: " + image.getImageInfo().getTimestamp());
        image.close();
    }

private void capturePhoto() {
        Activity activity = bridgeComponents.getActivity();
        Context context = bridgeComponents.getContext();
        long timestamp = System.currentTimeMillis();
        ContentValues contentValues = new ContentValues();
        contentValues.put(MediaStore.MediaColumns.DISPLAY_NAME, timestamp);
        contentValues.put(MediaStore.MediaColumns.MIME_TYPE, "image/jpeg");
        imageCapture.takePicture(
                new ImageCapture.OutputFileOptions.Builder(context.getContentResolver(), MediaStore.Images.Media.EXTERNAL_CONTENT_URI, contentValues).build(),
                ContextCompat.getMainExecutor(activity),
                new ImageCapture.OnImageSavedCallback() {
                    @Override
                    public void onImageSaved(@NonNull ImageCapture.OutputFileResults outputFileResults) {
                        Uri imageUri = outputFileResults.getSavedUri();
                        Utils.encodeImageToBase64(null,bridgeComponents.getContext(),imageUri);
                    }
                    @Override
                    public void onError(@NonNull ImageCaptureException exception) {
                        Toast.makeText(activity, "error", Toast.LENGTH_SHORT).show();
                    }
                }
        );
    }
   // endRegions

    // getting Brisque Array 

    private static double[] flattenFeatureList(List<double[]> featuresList) {
        int totalFeatures = featuresList.stream().mapToInt(array -> array.length).sum();
        double[] flattenedFeatures = new double[totalFeatures];

        int index = 0;
        for (double[] features : featuresList) {
            for (double feature : features) {
                flattenedFeatures[index++] = feature;
            }
        }

        return flattenedFeatures;
    }


    public static double[][] calculateLocalDeviation(double[][] image, double[][] localMean, double[][] kernel) {
        int imageRows = image.length;
        int imageCols = image[0].length;
        int kernelSize = kernel.length;
        int halfKernelSize = kernelSize / 2;

        double[][] squaredImage = new double[imageRows][imageCols];
        double[][] squaredSigma = new double[imageRows][imageCols];
        double[][] localDeviation = new double[imageRows][imageCols];

        for (int i = 0; i < imageRows; i++) {
            for (int j = 0; j < imageCols; j++) {
                squaredImage[i][j] = image[i][j] * image[i][j];
            }
        }

        squaredSigma = convolve2D(squaredImage, kernel);

        for (int i = 0; i < imageRows; i++) {
            for (int j = 0; j < imageCols; j++) {
                localDeviation[i][j] = Math.sqrt(Math.abs(localMean[i][j] * localMean[i][j] - squaredSigma[i][j]));
            }
        }

        return localDeviation;
    }


     public static double[][] convolve2D(double[][] image, double[][] kernel) {
        int imageRows = image.length;
        int imageCols = image[0].length;
        int kernelSize = kernel.length;
        int halfKernelSize = kernelSize / 2;

        double[][] result = new double[imageRows][imageCols];

        for (int i = 0; i < imageRows; i++) {
            for (int j = 0; j < imageCols; j++) {
                double sum = 0;

                for (int m = -halfKernelSize; m <= halfKernelSize; m++) {
                    for (int n = -halfKernelSize; n <= halfKernelSize; n++) {
                        int row = i + m;
                        int col = j + n;

                        if (row >= 0 && row < imageRows && col >= 0 && col < imageCols) {
                            sum += image[row][col] * kernel[m + halfKernelSize][n + halfKernelSize];
                        }
                    }
                }

                result[i][j] = sum;
            }
        }

        return result;
    }

    public static double[][] generateGaussianKernel2D(int n, double sigma) {
        int halfN = n / 2;

        double[][] gaussianKernel = new double[n][n];
        double sum = 0.0;

        for (int y = 0; y < n; y++) {
            for (int x = 0; x < n; x++) {
                int offsetY = y - halfN;
                int offsetX = x - halfN;

                double exponent = -(offsetX * offsetX + offsetY * offsetY) / (2.0 * sigma * sigma);
                gaussianKernel[y][x] = (1.0 / (2.0 * Math.PI * sigma * sigma)) * Math.exp(exponent);

                sum += gaussianKernel[y][x];
            }
        }

        // Normalize the kernel
        for (int y = 0; y < n; y++) {
            for (int x = 0; x < n; x++) {
                gaussianKernel[y][x] /= sum;
            }
        }

        return gaussianKernel;
    }

  
       public static double[][] calculateMSCNCoefficients(double[][] image, int kernelSize, double sigma) {
        double C = 1.0 / 255.0;
        double[][] kernel = generateGaussianKernel2D(kernelSize, sigma);
        double[][] localMean = convolve2D(image, kernel);
        double[][] localVar = calculateLocalDeviation(image, localMean, kernel);

        int rows = image.length;
        int cols = image[0].length;
        double[][] mscnCoefficients = new double[rows][cols];

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                mscnCoefficients[i][j] = (image[i][j] - localMean[i][j]) / (localVar[i][j] + C);
            }
        }

        return mscnCoefficients;
    }


    public static Map<String, double[][]> calculatePairProductCoefficients(double[][] mscnCoefficients) {
        int rows = mscnCoefficients.length;
        int cols = mscnCoefficients[0].length;

        double[][] horizontal = new double[rows][cols - 1];
        double[][] vertical = new double[rows - 1][cols];
        double[][] mainDiagonal = new double[rows - 1][cols - 1];
        double[][] secondaryDiagonal = new double[rows - 1][cols - 1];

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols - 1; j++) {
                horizontal[i][j] = mscnCoefficients[i][j] * mscnCoefficients[i][j + 1];
            }
        }

        for (int i = 0; i < rows - 1; i++) {
            for (int j = 0; j < cols; j++) {
                vertical[i][j] = mscnCoefficients[i][j] * mscnCoefficients[i + 1][j];
            }
        }

        for (int i = 0; i < rows - 1; i++) {
            for (int j = 0; j < cols - 1; j++) {
                mainDiagonal[i][j] = mscnCoefficients[i][j] * mscnCoefficients[i + 1][j + 1];
                secondaryDiagonal[i][j] = mscnCoefficients[i + 1][j] * mscnCoefficients[i][j + 1];
            }
        }

        Map<String, double[][]> coefficients = new LinkedHashMap<>();
        coefficients.put("mscn", mscnCoefficients);
        coefficients.put("horizontal", horizontal);
        coefficients.put("vertical", vertical);
        coefficients.put("main_diagonal", mainDiagonal);
        coefficients.put("secondary_diagonal", secondaryDiagonal);

        return coefficients;
    }



public static double[] asymmetricGeneralizedGaussianFit(double[][] x) {
        double alpha = estimateAlpha(x);
        double sigmaL = estimateSigma(x, alpha, z -> z < 0);
        double sigmaR = estimateSigma(x, alpha, z -> z >= 0);
        double[] meanResult = estimateMean(alpha, sigmaL, sigmaR);

        return new double[]{alpha, meanResult[0], sigmaL, sigmaR};
    }

    private static double estimateAlpha(double[][] x) {
        double rHat = estimateRHat(x);
        double gamma = estimateGamma(x);
        double rHatModified = estimateRHatModified(rHat, gamma);

        BrentSolver solver = new BrentSolver();
        UnivariateFunction equation = alpha -> estimatePhi(alpha) - rHatModified;
        return solver.solve(100, equation, 0.1, 5.0);
    }

    private static double estimateRHat(double[][] x) {
        double sumAbsX = 0.0;
        double sumXSquare = 0.0;
        int size = 0;

        for (double[] row : x) {
            for (double value : row) {
                double absValue = Math.abs(value);
                sumAbsX += absValue;
                sumXSquare += value * value;
                size++;
            }
        }

        return (sumAbsX / size) * (sumAbsX / size) / (sumXSquare / size);
    }

    private static double estimateGamma(double[][] x) {
        double leftSquares = meanSquaresSum(x, z -> z < 0);
        double rightSquares = meanSquaresSum(x, z -> z >= 0);
        return Math.sqrt(leftSquares) / Math.sqrt(rightSquares);
    }

    private static double estimatePhi(double alpha) {
        double numerator = Gamma.gamma(2 / alpha) * Gamma.gamma(2 / alpha);
        double denominator = Gamma.gamma(1 / alpha) * Gamma.gamma(3 / alpha);
        return numerator / denominator;
    }

    private static double estimateSigma(double[][] x, double alpha, Filter filter) {
        double sumFiltered = 0.0;
        int countFiltered = 0;

        for (double[] row : x) {
            for (double value : row) {
                if (filter.shouldInclude(value)) {
                    sumFiltered += value * value;
                    countFiltered++;
                }
            }
        }

        return Math.sqrt(sumFiltered / countFiltered);
    }

    private static double[] estimateMean(double alpha, double sigmaL, double sigmaR) {
        double constant = Math.sqrt(Gamma.gamma(1 / alpha) / Gamma.gamma(3 / alpha));
        double mean = (sigmaR - sigmaL) * constant * (Gamma.gamma(2 / alpha) / Gamma.gamma(1 / alpha));
        return new double[]{mean};
    }

    private static double meanSquaresSum(double[][] x, Filter filter) {
        double sumFiltered = 0.0;
        int countFiltered = 0;

        for (double[] row : x) {
            for (double value : row) {
                if (filter.shouldInclude(value)) {
                    sumFiltered += value * value;
                    countFiltered++;
                }
            }
        }

        return sumFiltered / countFiltered;
    }

    private interface Filter {
        boolean shouldInclude(double value);
    }

    private static class LessThanFilter implements Filter {
        @Override
        public boolean shouldInclude(double value) {
            return value < 0;
        }
    }

    private static class GreaterThanOrEqualFilter implements Filter {
        @Override
        public boolean shouldInclude(double value) {
            return value >= 0;
        }
    }

    private static double estimateRHatModified(double rHat, double gamma) {
        double numerator = (gamma * gamma * gamma + 1) * (gamma + 1);
        double denominator = (gamma * gamma + 1) * (gamma * gamma + 1);
        return rHat * numerator / denominator;
    }


  public static double[] calculateBrisqueFeatures(double[][] image, int kernelSize, double sigma) {
        double[][] mscnCoefficients = calculateMSCNCoefficients(image, kernelSize, sigma);
        Map<String, double[][]> coefficients = calculatePairProductCoefficients(mscnCoefficients);

        List<double[]> featuresList = new ArrayList<>();
        for (Map.Entry<String, double[][]> entry : coefficients.entrySet()) {
            String coefficientsName = entry.getKey();
            double[][] coeffArray = entry.getValue();

            double[] currentFeatures = calculateFeatures(coefficientsName, coeffArray);
            featuresList.add(currentFeatures);
        }

        return flattenFeatureList(featuresList);
     }


    private static double[] calculateFeatures(String coefficientsName, double[][] coefficients) {
        double[] features;

        double[] asymmetricGeneralizedGaussianFitResult = asymmetricGeneralizedGaussianFit(coefficients);
        double alpha = asymmetricGeneralizedGaussianFitResult[0];
        double mean = asymmetricGeneralizedGaussianFitResult[1];
        double sigmaL = asymmetricGeneralizedGaussianFitResult[2];
        double sigmaR = asymmetricGeneralizedGaussianFitResult[3];

        if (coefficientsName.equals("mscn")) {
            double var = (sigmaL * sigmaL + sigmaR * sigmaR) / 2;
            features = new double[]{alpha, var};
        } else {
            double[] additionalFeatures = new double[]{alpha, mean, sigmaL * sigmaL, sigmaR * sigmaR};
            features = additionalFeatures;
        }

        return features;
    }



    private static double[] scaleFeatures(double[] features, double[] min, double[] max) {
        double[] scaledFeatures = new double[features.length];
        for (int i = 0; i < features.length; i++) {
            scaledFeatures[i] = -1 + (2.0 / (max[i] - min[i])) * (features[i] - min[i]);
        }
        return scaledFeatures;
    }
    
    public static double[] convertBytesToDoubles(byte[] byteArray) {
        double[] doubleArray = new double[byteArray.length];

        for (int i = 0; i < byteArray.length; i++) {
            doubleArray[i] = (double) byteArray[i];
        }

        return doubleArray;
    }
    
    
    public static double[][] downscaleImage(double[][] grayImage, int targetWidth, int targetHeight) {
        int originalWidth = grayImage[0].length;
        int originalHeight = grayImage.length;

        Bitmap inputBitmap = Bitmap.createBitmap(originalWidth, originalHeight, Bitmap.Config.ARGB_8888);
        for (int y = 0; y < originalHeight; y++) {
            for (int x = 0; x < originalWidth; x++) {
                int grayValue = (int) (grayImage[y][x] * 255.0);
                int pixel = Color.rgb(grayValue, grayValue, grayValue);
                inputBitmap.setPixel(x, y, pixel);
            }
        }

        Bitmap resizedBitmap = Bitmap.createScaledBitmap(inputBitmap, targetWidth, targetHeight, false);

        double[][] resizedGrayImage = new double[targetHeight][targetWidth];
        for (int y = 0; y < targetHeight; y++) {
            for (int x = 0; x < targetWidth; x++) {
                int pixel = resizedBitmap.getPixel(x, y) & 0xFF;
                resizedGrayImage[y][x] = pixel / 255.0;
            }
        }

        return resizedGrayImage;
    }


    double[][] convertImageTo2dGrayImage(Bitmap image) {
        int width = image.getWidth();
        int height = image.getHeight();
        double[][] grayscaleArray = new double[height][width];

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                int pixelColor = image.getPixel(x, y);
                int red = (pixelColor >> 16) & 0xFF;
                int green = (pixelColor >> 8) & 0xFF;
                int blue = pixelColor & 0xFF;

                double gray = 0.299 * red + 0.587 * green + 0.114 * blue;
                grayscaleArray[y][x] = gray;
            }
        }

        return grayscaleArray;
    }


    @JavascriptInterface
    public void checkImageQuality(String base64Image, String base64ImageOrg) {
       
       
       
        if (base64ImageOrg.startsWith("data:image")) {
                base64ImageOrg = base64ImageOrg.substring(base64ImageOrg.indexOf(",") + 1);
            }
           byte[] binaryData = BaseEncoding.base64().decode(base64ImageOrg);
           int width=0,height=0;
        
          Bitmap image = BitmapFactory.decodeByteArray(binaryData, 0, binaryData.length);

           
            if (image != null) {
                 width = image.getWidth();
                 height = image.getHeight();
            }
      

       double[][] grayImage = convertImageTo2dGrayImage(image);
    
       double[] brisqueFeatures = calculateBrisqueFeatures(grayImage, 7, 7.0 / 6.0);

       double[][] downscaledImage = downscaleImage(grayImage, height / 2, width / 2);
       
       double[] downscaleBrisqueFeatures = calculateBrisqueFeatures(downscaledImage, 7, 7.0 / 6.0);


       double[] concatenatedFeatures = new double[brisqueFeatures.length + downscaleBrisqueFeatures.length];

       System.arraycopy(brisqueFeatures, 0, concatenatedFeatures, 0, brisqueFeatures.length);

       System.arraycopy(downscaleBrisqueFeatures, 0, concatenatedFeatures, brisqueFeatures.length, downscaleBrisqueFeatures.length);

      for (int i =0; i< concatenatedFeatures.length; i++ )
      {
          System.out.println(concatenatedFeatures[i]);
          System.out.println(" ");
      }

       //return concatenatedFeatures;
       
    }
    //endregion

}


