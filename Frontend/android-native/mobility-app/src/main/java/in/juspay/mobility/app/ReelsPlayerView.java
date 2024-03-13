package in.juspay.mobility.app;

import android.content.Context;
import android.media.AudioAttributes;
import android.media.AudioFocusRequest;
import android.media.AudioManager;
import android.os.Bundle;
import android.util.Log;
import android.view.WindowManager;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;


import java.util.Locale;

import in.juspay.mobility.app.reels.ReelController;

public class ReelsPlayerView extends AppCompatActivity {

   private ReelController reelController;
   private AudioManager audioManager;
   private Bundle bundle;
   private AudioFocusRequest mFocusRequest;
   private AudioManager.OnAudioFocusChangeListener onAudioFocusChangeListener;

   private boolean isAudioFocusLossed = false;
   private boolean isAudioFocusGranted = false;

   private boolean isActivityVisible = true;
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        bundle = getIntent().getExtras();
        setContentView(R.layout.reels_player_view);
        getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        audioManager  = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
        if(!isAudioFocusGranted){
            getAudioFocus();
        }
        initializeReelController();
    }

    @Override
    protected void onDestroy() {
        Log.i("NEW_ACTIVITY", "destroyed");
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s', %s, %s);",
                reelController.callback, "ACTION", "DESTROY_REEL", reelController.getCurrentReelVideoConfig(), null);

        reelController.sendJsCallback(javascript);

        try{
            reelController.stopAndReleaseExoplayers();
        } catch(Exception e){
            Log.e("REEL_ERROR", "error in stopping and releasing exoplayers");
        }
        abandonAudioFocus();
        ReelController.deRegisterCallbacks();
        super.onDestroy();
    }

    @Override
    protected void onPause() {
        try{
            reelController.pauseExoplayers(false, true);
        }catch(Exception e){
            Log.e("REEL_ON_PAUSE", e.toString());
        }
        reelController.sendPauseActivityCallback();
        isActivityVisible = false;
        super.onPause();
    }

    @Override
    protected void onResume() {
        try {
            reelController.resumeExoplayer();
        }catch (Exception e){
            Log.e("REEL_ON_RESUME", e.toString());
        }
        reelController.sendResumeActivityCallback();
        isActivityVisible = true;
        if(!isAudioFocusGranted) {
            getAudioFocus();
        }
        super.onResume();
    }

    protected void getAudioFocus(){

        Log.i("REEL_AUDIO_FOCUS", "Getting the audio focus for reels");
        AudioAttributes mPlaybackAttributes = new AudioAttributes.Builder()
                .setUsage(AudioAttributes.USAGE_MEDIA)
                .setContentType(AudioAttributes.CONTENT_TYPE_SPEECH)
                .build();

        int audioManagerResult;

        this.onAudioFocusChangeListener = focusChange -> {
            Log.i("REEL_AUDIO_FOCUS_CHANGED", focusChange + "");
                    if(!isAudioFocusGranted && isActivityVisible) return;
                    switch(focusChange){
                        case AudioManager.AUDIOFOCUS_GAIN:
                            isAudioFocusLossed = false;
                            reelController.resumeExoplayer();
                            break;
                        case AudioManager.AUDIOFOCUS_LOSS:
                            Log.i("REEL","isAudioFocusLossed changed changeListener " + isAudioFocusLossed);
                            isAudioFocusLossed = true;
                            reelController.pauseExoplayers(false, true);
                            break;
                    }
        };

        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
            this.mFocusRequest = new AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN)
                    .setAudioAttributes(mPlaybackAttributes)
                    .setAcceptsDelayedFocusGain(true)
                    .setWillPauseWhenDucked(true)
                    .setOnAudioFocusChangeListener(this.onAudioFocusChangeListener)
                    .build();

            audioManagerResult = audioManager.requestAudioFocus(this.mFocusRequest);

        }else{
            audioManagerResult = audioManager.requestAudioFocus(this.onAudioFocusChangeListener, AudioManager.STREAM_MUSIC, AudioManager.AUDIOFOCUS_GAIN);
        }

        if (audioManagerResult == AudioManager.AUDIOFOCUS_REQUEST_GRANTED) {
               isAudioFocusGranted = true;
        }else{
                abandonAudioFocus();
        }
    }

    private void abandonAudioFocus(){
        Log.i("REEL_AUDIO_FOCUS", "Abandoning the audio focus for reels");
        try{
                if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O){
                    audioManager.abandonAudioFocusRequest(mFocusRequest);
                }else{
                    audioManager.abandonAudioFocus(onAudioFocusChangeListener);
                }
                isAudioFocusGranted = false;
        }catch(Exception e){
            Log.e("REEL_AUDIO_FOCUS", e.toString());
        }
    }

    private void initializeReelController(){
        reelController = new ReelController(this, this, () -> {
                if(!isAudioFocusGranted){
                    isAudioFocusGranted = true;
                    getAudioFocus();
                }
        }, () -> {
            Log.i("REEL","isAudioFocusLossed changed abandon " + isAudioFocusLossed);
                    isAudioFocusLossed = false;
                    abandonAudioFocus();
        });

        if (bundle != null) {
            String jsonData = bundle.getString("reelsJSONData");
            int index = bundle.getInt("index");
            String callback = bundle.getString("callback");
            reelController.initializeReelsView(jsonData, index, callback );
        }
    }


}


