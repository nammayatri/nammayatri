package in.juspay.mobility.app.reels;

import android.app.Activity;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.widget.LinearLayout;

import android.widget.RelativeLayout;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.viewpager2.widget.ViewPager2;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Locale;
import java.util.Objects;

import in.juspay.mobility.app.R;

public class ReelController {

    private final Context context;
    private final Activity activity;
    private final ArrayList<ReelViewPagerItem> reelViewPagerItemArrayList = new ArrayList<>();

    private final ViewPager2 reelViewPager;

    public String callback;

    private ExoplayerItem currentExoplayerPlaying = null;

    private final ReelViewAdapter reelViewAdapter;
    private final ArrayList<ExoplayerItem> exoplayerItems = new ArrayList<>();

    private final OnPauseExoplayerListener onPauseExoplayerListener;

    public static ArrayList<ReelControllerCallback> callbackList = new ArrayList<>();

    public static ReelActivityInterface reelActivityInterface ;

    public interface ReelControllerCallback {
        void sendJsCallBack(String javascript);
    }

    public interface ReelActivityInterface {
        void onPauseCallback();
        void onResumeCallback();
    }

    public static void registerReelActivity (ReelActivityInterface reelActivityIn){
        reelActivityInterface = reelActivityIn;
    }

    public static void registerCallback(ReelControllerCallback callback){
        callbackList.add(callback);
    }

    public static void deRegisterCallbacks(){
        callbackList.clear();
    }

    public ReelController(Context context, Activity activity, OnResumeExoplayerListener onResumeExoplayerListener, OnPauseExoplayerListener onPauseExoplayerListener){
        this.context = context;
        this.activity = activity;
        reelViewPager = new ViewPager2(context);
        reelViewPager.setOrientation(ViewPager2.ORIENTATION_VERTICAL);
        this.onPauseExoplayerListener = onPauseExoplayerListener;
        reelViewAdapter = new ReelViewAdapter(reelViewPagerItemArrayList, context, exoplayerItems::add, new ReelViewAdapter.ReelViewAdapterInterface() {
            @Nullable
            @Override
            public ExoplayerItem getCurrentExoplayerPlaying() {
                return currentExoplayerPlaying;
            }

            @Override
            public ViewPager2 getReelViewPager() {
                return reelViewPager;
            }

            @Override
            public void sendJsCallbackFromAdapter(String javascript) {
                sendJsCallback(javascript);
            }

            @Nullable
            @Override
            public String getCallback() {
                return callback;
            }

            @Override
            public void abandonAudioFocus() {
                onPauseExoplayerListener.abandonAudioFocusWhilePausingExoplayer();
            }

            @Override
            public void getAudioFocus() {
                onResumeExoplayerListener.getAudioFocusToResumeExoplayer();
            }

            @Override
            public ReelViewPagerItem getCurrentReelViewPagerItem() {
                return reelViewPagerItemArrayList.get(reelViewPager.getCurrentItem());
            }

            @Override
            public void setCurrentReelViewPagerItem(int position) {
                reelViewPager.setCurrentItem(position);
            }

        });
    }



    public JSONObject getCurrentReelVideoConfig() {
        try{
            return reelViewPagerItemArrayList.get(currentExoplayerPlaying.position).reelVideoConfig;
        } catch(Exception e){
            return null;
        }
    }


    public interface OnResumeExoplayerListener{
        void getAudioFocusToResumeExoplayer();
    }

    public interface OnPauseExoplayerListener{
        void abandonAudioFocusWhilePausingExoplayer();
    }

    public void initializeReelsView(String stringifyJsonData, int index, String callback){

        try{
            JSONObject jsonData = new JSONObject(stringifyJsonData);
            this.callback = callback;
            JSONArray jsonArray = jsonData.getJSONArray("reelData");
            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject jsonObject = jsonArray.getJSONObject(i);
                ReelViewPagerItem reelViewPagerItem = new ReelViewPagerItem(jsonObject);
                reelViewPagerItemArrayList.add(reelViewPagerItem);
            }

            Collections.rotate(reelViewPagerItemArrayList, index * -1);
            reelViewPager.setAdapter(reelViewAdapter);
            reelViewAdapter.notifyItemRangeInserted(0, jsonArray.length());
            reelViewAdapter.setReelTitleConfig(jsonData.optJSONObject("descriptionConfig") == null ? new JSONObject() : jsonData.optJSONObject("titleConfig"));
            reelViewAdapter.setReelDescriptionConfig(jsonData.optJSONObject("descriptionConfig") == null ? new JSONObject() : jsonData.optJSONObject("descriptionConfig"));
            reelViewAdapter.setReelExtraConfig(jsonData.optJSONObject("reelExtraConfig") == null ? new JSONObject() : Objects.requireNonNull(jsonData.optJSONObject("reelExtraConfig")));

        } catch(JSONException e){
            Log.e("REELS_VIEW_ACTIVITY", "error in json while initializing the reelsView" + e);
        }

        RelativeLayout finalReelParentLayout = activity.findViewById(R.id.reels_player_view_ll);
        LinearLayout backPressImageButton = activity.findViewById(R.id.reel_backpress_button);

        backPressImageButton.setOnClickListener(v -> activity.finish());


        reelViewPager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                super.onPageScrolled(position, positionOffset, positionOffsetPixels);
            }


            @Override
            public void onPageScrollStateChanged(int state) {
                super.onPageScrollStateChanged(state);
                Log.e("REEL_SCROLL_STATE_CHANGED", "" + state);
            }

            @Override
            public void onPageSelected(int position) {
                // add a callback here to trigger action on page callback change
                if (callback != null) {
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s', %s, %s);",
                            callback, "CURRENT_POSITION", reelViewPagerItemArrayList.get(position).getReelViewPagerItemId(), reelViewPagerItemArrayList.get(position).reelVideoConfig, null);

                    sendJsCallback(javascript);
                }

                Log.e("PAGE_SELECTED_CHANGED", "" + reelViewPagerItemArrayList.get(position).getReelViewPagerItemId());

                pauseExoplayers(true, false);
                try {
                    currentExoplayerPlaying = exoplayerItems.get(position);
                    exoplayerItems.get(position).exoPlayer.setPlayWhenReady(false);
                    exoplayerItems.get(position).exoPlayer.seekTo(0);
                    currentExoplayerPlaying.reelSeekBar.setProgress(0);
                    currentExoplayerPlaying.reelPauseButton.setVisibility(View.GONE);
                    if(currentExoplayerPlaying.scrollViewExpanded){
                        currentExoplayerPlaying.reelInfoView.performClick();
                        currentExoplayerPlaying.scrollViewExpanded = false;
                    }
                    exoplayerItems.get(position).exoPlayer.play();
                } catch(IndexOutOfBoundsException e){
                    Thread repeatThread = new Thread() {
                        public void run() {
                            int restartAttemptCount = 0;
                            boolean contentLoaded = false;
                            while (restartAttemptCount < 5 && !contentLoaded) {
                                restartAttemptCount++;
                                try{
                                    currentExoplayerPlaying = exoplayerItems.get(position);
                                    ((Activity) context).runOnUiThread(() -> {
                                        exoplayerItems.get(position).exoPlayer.setPlayWhenReady(false);
                                        exoplayerItems.get(position).exoPlayer.seekTo(0);
                                        currentExoplayerPlaying.reelSeekBar.setProgress(0);
                                        exoplayerItems.get(position).exoPlayer.play();
                                    });

                                    currentExoplayerPlaying.reelPauseButton.setVisibility(View.GONE);
                                    if(currentExoplayerPlaying.scrollViewExpanded){
                                        currentExoplayerPlaying.reelInfoView.performClick();
                                        currentExoplayerPlaying.scrollViewExpanded = false;
                                    }
                                    contentLoaded = true;

                                }
                                catch(Exception e)
                                {
                                    Log.e("REEL_RESTARTING_THREAD", "Restarting attempt" + " " + restartAttemptCount + " " + e);
                                }

                                try {
                                    Thread.sleep(1000);
                                } catch (InterruptedException e) {
                                    e.printStackTrace();
                                }

                                if(restartAttemptCount == 4){
                                    Toast.makeText(context, "Something went wrong. Please try again later!", Toast.LENGTH_SHORT).show();
                                    ((Activity) context).finish();
                                }

                            }
                        }
                    };

                    repeatThread.start();
                }  catch (Exception e) {
                    Log.e("REEL_PLAYBACK_ERROR", e.toString());
                    Toast.makeText(context, "Something went wrong. Please try again later!", Toast.LENGTH_SHORT).show();
                    ((Activity) context).finish();
                }

                super.onPageSelected(position);
            }
        });

        finalReelParentLayout.addView(reelViewPager);
    }

    public void stopAndReleaseExoplayers(){
        try {

            reelViewAdapter.removeCallbacks();
            for (ExoplayerItem exoplayerItem : exoplayerItems) {
                if (exoplayerItem.exoPlayer != null) {
                    exoplayerItem.exoPlayer.pause();
                    exoplayerItem.exoPlayer.release();
                }
            }
            currentExoplayerPlaying = null;
            exoplayerItems.clear();
            reelViewPagerItemArrayList.clear();
            reelViewAdapter.handler.removeCallbacks(reelViewAdapter.updatePercentageVideoCompletedRunnable);
        } catch (Exception e) {
            Log.e("REEL", "error in releasing the exoplayers" + e);
        }
    }

    public void resumeExoplayer() {
        if (currentExoplayerPlaying != null) {
            try {
                if(!currentExoplayerPlaying.exoPlayer.isPlaying()) {
                    currentExoplayerPlaying.exoPlayer.play();
                }
                currentExoplayerPlaying.reelPauseButton.setVisibility(View.GONE);
            } catch (Exception e) {
                Log.e("REEL", "error in resuming the current exoplayerPlaying" + e);
            }
        }
    }

    public void pauseExoplayers(Boolean playFromStart, boolean abandonAudioManager) {

        try {
            for (ExoplayerItem exoplayerItem : exoplayerItems) {
                if (exoplayerItem.exoPlayer != null) {
                    exoplayerItem.exoPlayer.pause();
                    exoplayerItem.reelPauseButton.setVisibility(View.VISIBLE);
                    if (playFromStart) {
                        exoplayerItem.exoPlayer.seekTo(0);
                    }
                }
            }

            if (abandonAudioManager) {
                onPauseExoplayerListener.abandonAudioFocusWhilePausingExoplayer();
            }
        } catch (Exception e) {
            Log.e("REEL", "error in pausing the exoplayers" + e);
        }

    }

    public void sendJsCallback(String javaScript){
        for(ReelControllerCallback callback : callbackList){
            callback.sendJsCallBack(javaScript);
        }
    }

    public void sendPauseActivityCallback(){
        if(reelActivityInterface != null){
            reelActivityInterface.onPauseCallback();
        }
    }

    public void sendResumeActivityCallback(){
        if(reelActivityInterface != null){
            reelActivityInterface.onResumeCallback();
        }
    }

}
