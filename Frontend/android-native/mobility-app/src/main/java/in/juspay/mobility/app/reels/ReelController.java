package in.juspay.mobility.app.reels;

import android.app.Activity;
import android.content.Context;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;

import androidx.media3.exoplayer.ExoPlayer;
import androidx.viewpager2.widget.ViewPager2;

import com.google.android.material.progressindicator.LinearProgressIndicator;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.mobility.app.MobilityAppBridge;
import in.juspay.mobility.app.R;

public class ReelController {

    private Context context;
    private Activity activity;
    private final ArrayList<ReelViewPagerItem> reelViewPagerItemArrayList = new ArrayList<>();

    public static ViewPager2 reelViewPager;

    public static String callback;

    public static ExoPlayer currentExoplayerPlaying = null;

    public static LinearProgressIndicator currentHorizontalProgressBar = null;

    public static BridgeComponents bridgeComponentsInternal = null;

    private ReelViewAdapter reelViewAdapter;

    private static ArrayList<ExoplayerItem> exoplayerItems = new ArrayList<>();

    public ReelController(Context context){
        this.context = context;
        this.activity = (Activity) context;
        reelViewPager = new ViewPager2(context);
        reelViewPager.setOrientation(ViewPager2.ORIENTATION_VERTICAL);

        reelViewAdapter = new ReelViewAdapter(reelViewPagerItemArrayList, context, bridgeComponentsInternal, new ReelViewAdapter.OnVideoPreparedListener() {
            @Override
            public void onVideoPrepared(ExoplayerItem exoplayerItem) {
                exoplayerItems.add(exoplayerItem);
            }
        });
    }

    public void initializeReelsView(String stringifyJsonData, int index, String callback){

        try{
            JSONObject jsonData = new JSONObject(stringifyJsonData);
            this.callback = callback;
            JSONObject reelTitleConfig = jsonData.getJSONObject("titleConfig");
            JSONObject reelDescriptionConfig = jsonData.getJSONObject("descriptionConfig");
            JSONArray jsonArray = jsonData.getJSONArray("reelData");
            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject jsonObject = jsonArray.getJSONObject(i);
                ReelViewPagerItem reelViewPagerItem = new ReelViewPagerItem(jsonObject);
                reelViewPagerItemArrayList.add(reelViewPagerItem);
            }

            Collections.rotate(reelViewPagerItemArrayList, index * -1);
            reelViewPager.setAdapter(reelViewAdapter);
            reelViewAdapter.notifyItemRangeInserted(0, jsonArray.length());
            reelViewAdapter.setReelTitleConfig(reelTitleConfig);
            reelViewAdapter.setReelDescriptionConfig(reelDescriptionConfig);
        } catch(JSONException e){
            Log.e("REELS_VIEW_ACTIVITY", "error in json while initializing the reelsView" + e);
        }

        RelativeLayout finalReelParentLayout = activity.findViewById(R.id.reels_player_view_ll);
        LinearLayout backPressImageButton = activity.findViewById(R.id.reel_backpress_button);

        backPressImageButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                activity.finish();
            }
        });


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
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s');",
                            callback, "CURRENT_POSITION", reelViewPagerItemArrayList.get(position).reelVideoConfig.toString());

                    bridgeComponentsInternal.getJsCallback().addJsToWebView(javascript);
                }

                Log.e("PAGE_SELECTED_CHANGED", "" + reelViewPagerItemArrayList.get(position).getReelViewPagerItemId());

                pauseExoplayers(true);

                try {
                    currentExoplayerPlaying = exoplayerItems.get(position).exoPlayer;
                    currentHorizontalProgressBar = exoplayerItems.get(position).horizontalProgressBar;
                    exoplayerItems.get(position).exoPlayer.setPlayWhenReady(false);
                    exoplayerItems.get(position).exoPlayer.seekTo(0);
                    currentHorizontalProgressBar.setProgress(0);
                    exoplayerItems.get(position).exoPlayer.play();
                    Log.i("REELS", position + "");
                } catch (Exception e) {
                    Log.e("REEL_PLAYBACK_ERROR", e.toString());
                }

                super.onPageSelected(position);
            }
        });

        finalReelParentLayout.addView(reelViewPager);
    }

    public void stopAndReleaseExoplayers(){
        try {

            for (ExoplayerItem exoplayerItem : exoplayerItems) {
                if (exoplayerItem.exoPlayer != null) {
                    exoplayerItem.exoPlayer.pause();
                    exoplayerItem.exoPlayer.release();
                }
            }
            currentExoplayerPlaying = null;
            exoplayerItems.clear();
            reelViewAdapter.handler.removeCallbacks(reelViewAdapter.updatePercentageVideoCompletedRunnable);
        } catch (Exception e) {
            Log.e("REEL", "error in releasing the exoplayers" + e);
        }
    }

    public void resumeExoplayer() {
        if (currentExoplayerPlaying != null) {
            try {
                currentExoplayerPlaying.play();
            } catch (Exception e) {
                Log.e("REEL", "error in resuming the current exoplayerPlaying" + e);
            }
        }
    }

    public void pauseExoplayers(Boolean playFromStart) {
        try {
            for (ExoplayerItem exoplayerItem : exoplayerItems) {
                if (exoplayerItem.exoPlayer != null) {
                    exoplayerItem.exoPlayer.pause();
                    if (playFromStart) {
                        exoplayerItem.exoPlayer.seekTo(0);
                    }
                }
            }
        } catch (Exception e) {
            Log.e("REEL", "error in pausing the exoplayers" + e);
        }

    }

    public static void initializeBridgeComponents (BridgeComponents bridgeComponents){
        bridgeComponentsInternal = bridgeComponents;
    }

}
