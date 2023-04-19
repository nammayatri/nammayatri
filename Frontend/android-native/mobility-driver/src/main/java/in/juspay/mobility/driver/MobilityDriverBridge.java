package in.juspay.mobility.driver;

import android.app.AlertDialog;
import android.app.DatePickerDialog;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.Build;
import android.os.PowerManager;
import android.provider.Settings;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.webkit.JavascriptInterface;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.NumberPicker;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;

import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerFullScreenListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.options.IFramePlayerOptions;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.ui.DefaultPlayerUiController;

import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.appcommon.GpsListeningService;
import in.juspay.mobility.common.MobilityCommonBridge;
import in.juspay.mobility.driver.mediaPlayer.DefaultMediaPlayerControl;

public class MobilityDriverBridge extends MobilityCommonBridge {

    private static final String LOG_TAG = "MobilityDriverBridge";
    private static final int DATEPICKER_SPINNER_COUNT = 3;
    public static YouTubePlayerView youTubePlayerView;
    public static YouTubePlayer youtubePlayer;
    public static float videoDuration = 0;
    public static ArrayList<MediaPlayerView> audioPlayers = new ArrayList<>();

    public MobilityDriverBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
//        GpsListeningService.startLocationService();
    }

    @JavascriptInterface
    public void pauseYoutubeVideo(){
        if( youTubePlayerView != null) {
            youtubePlayer.pause();
        }
    }

    public void updateLocaleResource(String languageKey) {
        Context context = bridgeComponents.getContext();
        Locale locale;
        switch (languageKey) {
            case "HI_IN":
                locale = new Locale("hi");
                break;
            case "KN_IN":
                locale = new Locale("kn");
                break;
            case "EN_US":
                locale = new Locale("en");
                break;
            case "TA_IN":
                locale = new Locale("ta");
                break;
            default:
                return;
        }
        Locale.setDefault(locale);
        Configuration configuration = context.getResources().getConfiguration();
        configuration.setLocale(locale);
        context.getResources().updateConfiguration(configuration, context.getResources().getDisplayMetrics());
    }

    @JavascriptInterface
    public void setYoutubePlayer(String rawJson, final String playerId, String videoStatus){
        videoDuration = 0;
        bridgeComponents.getActivity().runOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    if(videoStatus.equals("PAUSE"))   {
                        pauseYoutubeVideo();
                    }   else {
                        JSONObject json = new JSONObject(rawJson);
                        if (youTubePlayerView != null )
                            youTubePlayerView.release();
                        boolean showMenuButton = json.getBoolean("showMenuButton");
                        boolean showDuration = json.getBoolean("showDuration");
                        boolean setVideoTitle = json.getBoolean("setVideoTitle");
                        boolean showSeekBar = json.getBoolean("showSeekBar");
                        String videoTitle = json.getString("videoTitle");
                        String videoId = json.getString("videoId");
                        String videoType = "VIDEO";
                        if (json.has("videoType"))
                        {
                            videoType = json.getString("videoType");
                        }
                        youTubePlayerView = new YouTubePlayerView(bridgeComponents.getContext());
                        LinearLayout layout = bridgeComponents.getActivity().findViewById(Integer.parseInt(playerId));
                        layout.addView(youTubePlayerView);
                        youTubePlayerView.setEnableAutomaticInitialization(false);
                        YouTubePlayerListener youTubePlayerListener = new AbstractYouTubePlayerListener() {
                            @Override
                            public void onReady(YouTubePlayer youTubePlayer) {
                                try {
                                    youtubePlayer = youTubePlayer;
                                    DefaultPlayerUiController playerUiController = new DefaultPlayerUiController(youTubePlayerView, youTubePlayer);
                                    playerUiController.showMenuButton(showMenuButton);
                                    playerUiController.showDuration(showDuration);
                                    playerUiController.showSeekBar(showSeekBar);
                                    playerUiController.showFullscreenButton(true);
                                    if (setVideoTitle){
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
                            public void onCurrentSecond(@NonNull YouTubePlayer youTubePlayer, float second){
                                videoDuration = second;
                            }
                        };

                        String finalVideoType = videoType;
                        youTubePlayerView.addFullScreenListener(new  YouTubePlayerFullScreenListener() {
                            @Override
                            public void onYouTubePlayerExitFullScreen() {
                            }

                            @Override
                            public void onYouTubePlayerEnterFullScreen() {
                                Intent newIntent = new Intent(bridgeComponents.getContext(), YoutubeVideoView.class );
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
            }
        });
    }

    private void reOrderSpinners(DatePickerDialog dialog, char[] dateOrder) {
        if(!dialog.isShowing()) {
            return;
        }

        final int yearId = Resources.getSystem().getIdentifier("year", "id", "android");
        final int monthId = Resources.getSystem().getIdentifier("month", "id", "android");
        final int dayId = Resources.getSystem().getIdentifier("day", "id", "android");
        final int layoutId = Resources.getSystem().getIdentifier("pickers", "id", "android");

        final NumberPicker yearSpinner = (NumberPicker) dialog.findViewById(yearId);
        final NumberPicker monthSpinner = (NumberPicker) dialog.findViewById(monthId);
        final NumberPicker daySpinner = (NumberPicker) dialog.findViewById(dayId);
        final LinearLayout layout = (LinearLayout) dialog.findViewById(layoutId);

        layout.removeAllViews();
        for (int i = 0; i < DATEPICKER_SPINNER_COUNT; i++) {
            switch (dateOrder[i]) {
                case 'y':
                    layout.addView(yearSpinner);
                    setImeOptions(yearSpinner, i);
                    break;
                case 'm':
                    layout.addView(monthSpinner);
                    setImeOptions(monthSpinner, i);
                    break;
                case 'd':
                    layout.addView(daySpinner);
                    setImeOptions(daySpinner, i);
                    break;
                default:
                    throw new IllegalArgumentException("Invalid DateOrder");
            }
        }
    }


    private void setImeOptions(NumberPicker spinner, int spinnerIndex) {
        final int imeOption;
        if (spinnerIndex < DATEPICKER_SPINNER_COUNT - 1) {
            imeOption = EditorInfo.IME_ACTION_NEXT;
        }
        else {
            imeOption = EditorInfo.IME_ACTION_DONE;
        }
        int idPickerInput = Resources.getSystem().getIdentifier("numberpicker_input", "id", "android");
        TextView input = (TextView) spinner.findViewById(idPickerInput);
        input.setImeOptions(imeOption);
    }

    @JavascriptInterface
    public void requestBatteryPermission(){
        try {
            if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                Intent intent = new Intent(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
                Uri uri = Uri.fromParts("package", bridgeComponents.getContext().getPackageName(), null);
                intent.setData(uri);
                String packageName = bridgeComponents.getContext().getPackageName();
                PowerManager pm = (PowerManager) bridgeComponents.getContext().getSystemService(android.content.Context.POWER_SERVICE);
                if (pm.isIgnoringBatteryOptimizations(packageName)) {
                    intent.setAction(Settings.ACTION_IGNORE_BATTERY_OPTIMIZATION_SETTINGS);
                }
                else {
                    intent.setAction(Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
                }
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                bridgeComponents.getContext().startActivity(intent);
            }
        }catch(ActivityNotFoundException e){
            e.printStackTrace();
            Intent intent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
            Uri uri = Uri.fromParts("package", bridgeComponents.getContext().getPackageName(), null);
            intent.setData(uri);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            bridgeComponents.getContext().startActivity(intent);
        }
    }

    @JavascriptInterface
    public boolean isBatteryPermissionEnabled(){
        PowerManager powerManager = (PowerManager) bridgeComponents.getContext().getSystemService(Context.POWER_SERVICE);
        return (powerManager.isIgnoringBatteryOptimizations(bridgeComponents.getContext().getPackageName()));
    }

    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.M)
    public void checkOverlayPermission(){
        System.out.println("CommonJsInterface checkOverlayPermission()");
        if(!Settings.canDrawOverlays(bridgeComponents.getContext())){
            requestOverlayPermission();
            System.out.print("After request permission");
        }
    }

    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.M)
    public void requestAutoStartPermission() {
        CheckPermissionAutoStart.getInstance().getAutoStartPermission(bridgeComponents.getContext());
    }

    @JavascriptInterface
    @RequiresApi(api = Build.VERSION_CODES.M)
    public void requestOverlayPermission() {
        try {
            Intent intent = new Intent(bridgeComponents.getContext(), CheckPermissionOverlay.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            bridgeComponents.getContext().startActivity(intent);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in request permission", e);
        }
    }


    @JavascriptInterface
    public void previewImage ( String base64Image){
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (!base64Image.equals("") && base64Image!=null){
                    byte[] decodedString = Base64.decode(base64Image, Base64.DEFAULT);
                    Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);

                    AlertDialog.Builder builder = new AlertDialog.Builder(bridgeComponents.getContext());
                    builder.setCancelable(true);
                    ImageView imagePreview = new ImageView(bridgeComponents.getContext());
                    imagePreview.setImageBitmap(decodedByte);

                    DisplayMetrics displayMetrics = new DisplayMetrics();
                    bridgeComponents.getActivity().getWindowManager().getDefaultDisplay().getMetrics(displayMetrics);
                    int screenHeight = displayMetrics.heightPixels;
                    int width = displayMetrics.widthPixels;
                    imagePreview.setMinimumHeight(screenHeight/2);
                    imagePreview.setMinimumWidth(width);

                    ViewGroup.LayoutParams layoutParams= new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT,LinearLayout.LayoutParams.MATCH_PARENT);
                    imagePreview.setLayoutParams(layoutParams);
                    builder.setView(imagePreview);
                    AlertDialog alertDialog = builder.create();
                    alertDialog.show();
                }
            } catch (Exception e){
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public void addMediaPlayer (String viewID,String source) throws IOException {
        ExecutorManager.runOnMainThread(() -> {
            MediaPlayerView audioPlayer = new MediaPlayerView(bridgeComponents.getContext(),bridgeComponents.getActivity());
            try {
                audioPlayer.inflateView(Integer.parseInt(viewID));
                if (source.contains(".mp3")) {
                    audioPlayer.addAudioFileUrl(source);
                } else {
                    Thread thread =  new Thread(new Runnable() {
                        @Override
                        public void run() {
                            try {
                                String base64 = getAPIResponse(source);
                                byte decodedAudio[] = Base64.decode(base64,Base64.DEFAULT);
                                File tempMp3 = File.createTempFile("audio_cache", "mp3", bridgeComponents.getContext().getCacheDir());
                                tempMp3.deleteOnExit();
                                FileOutputStream fos = new FileOutputStream(tempMp3);
                                fos.write(decodedAudio);
                                fos.close();
                                FileInputStream fis = new FileInputStream(tempMp3);
                                audioPlayer.addAudioFileInput(fis);
                            } catch (Exception e) {

                            }
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

    private String getAPIResponse(String url) {
        if (url.equals("") || url == null) return "";
        StringBuilder result = new StringBuilder();
        try {
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            connection.setRequestMethod("GET");
            connection.setRequestProperty("token",  getKeyInNativeSharedPrefKeys("REGISTERATION_TOKEN"));
            connection.connect();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302){
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                return "";
            }else {
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
    public void removeMediaPlayer () {
        for (MediaPlayerView audioPlayer : audioPlayers) {
            audioPlayer.resetListeners();
        }
        bridgeComponents.getContext().getCacheDir().delete();
        audioPlayers.removeAll(audioPlayers);
        DefaultMediaPlayerControl.mediaPlayer.reset();
    }
}


