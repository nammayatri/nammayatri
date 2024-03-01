package in.juspay.mobility.app.reels;


import android.animation.Animator;
import android.animation.AnimatorSet;
import android.animation.ObjectAnimator;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.GradientDrawable;
import android.net.Uri;
import android.os.Build;
import android.os.Handler;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.SeekBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.OptIn;
import androidx.media3.common.MediaItem;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.ui.PlayerView;
import androidx.recyclerview.widget.RecyclerView;
import androidx.viewpager2.widget.ViewPager2;

import com.bumptech.glide.Glide;
import com.bumptech.glide.load.engine.DiskCacheStrategy;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Locale;

import in.juspay.mobility.app.R;

public class ReelViewAdapter extends RecyclerView.Adapter<ReelViewAdapter.ViewHolder> {

    ArrayList <ReelViewPagerItem> reelViewPagerItemArrayList;
    public Context context;
    static MediaSource mediaSource;
    public JSONObject reelTitleConfig, reelDescriptionConfig, reelExtraConfig;
    OnVideoPreparedListener videoPreparedListener;
    ReelViewAdapterInterface reelViewAdapterInterface;

    Handler handler;

    Runnable updatePercentageVideoCompletedRunnable;

    public ReelViewAdapter(ArrayList<ReelViewPagerItem> reelViewPagerItemArrayList,  Context context, OnVideoPreparedListener videoPreparedListener, ReelViewAdapterInterface reelViewAdapterInterface) {
        this.context = context;
        this.reelViewPagerItemArrayList = reelViewPagerItemArrayList;
        this.videoPreparedListener = videoPreparedListener;
        this.reelViewAdapterInterface = reelViewAdapterInterface;
        this.handler = new Handler();

        updatePercentageVideoCompletedRunnable = new Runnable() { // the handler to get the current Video playing percentage
            @Override
            public void run() {
                try{
                    getPercentageVideoCompleted();
                }catch(Exception e){
                    Log.e("REEL_UPDATE", e.toString());
                }
                handler.postDelayed(this, 1000);
            }
        };
        handler.postDelayed(updatePercentageVideoCompletedRunnable, 1000);

        Log.i("REELS", "initialized the data");
    }

    public void removeCallbacks(){
        handler.removeCallbacks(updatePercentageVideoCompletedRunnable, null);
    }

    public void getPercentageVideoCompleted(){

        ExoplayerItem currentExoplayerPlaying = reelViewAdapterInterface.getCurrentExoplayerPlaying();
        if(currentExoplayerPlaying == null) {
            Log.i("REEL_DATA_ERROR", "currentExoplayer is null in getPercentageVideoCompleted");
            return;
        }
        long currentPosition = currentExoplayerPlaying.exoPlayer.getCurrentPosition();
        long duration = currentExoplayerPlaying.exoPlayer.getDuration();
        int percentageCompleted = (int) ((currentPosition * 100) / duration);

        if (currentExoplayerPlaying.reelSeekBar != null) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
                currentExoplayerPlaying.reelSeekBar.setProgress(percentageCompleted, true);
            }else{
                currentExoplayerPlaying.reelSeekBar.setProgress(percentageCompleted);
            }
        }
        sendVideoPercentageCompletedJsCallBack(percentageCompleted, currentExoplayerPlaying);
    }

    private void sendVideoPercentageCompletedJsCallBack(int percentageCompleted, ExoplayerItem currentExoplayerPlaying){
        boolean sendCallbackAfterEverySecondEnabled = currentExoplayerPlaying.reelThresholdConfig.has("sendCallbackAfterEverySecondEnabled") && currentExoplayerPlaying.reelThresholdConfig.optBoolean("sendCallbackAfterEverySecondEnabled", false);

        String callback = reelViewAdapterInterface.getCallback();

        JSONObject reelVideoConfig = (reelViewAdapterInterface.getCurrentReelViewPagerItem() == null) ? null : reelViewAdapterInterface.getCurrentReelViewPagerItem().reelVideoConfig;

        if(sendCallbackAfterEverySecondEnabled) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s', %s, %s);",
                    callback, "CURRENT_VIDEO_PERCENTAGE_COMPLETED", percentageCompleted + "", reelVideoConfig, null);
            reelViewAdapterInterface.sendJsCallbackFromAdapter(javascript);
            return;
        }

        if(currentExoplayerPlaying.reelThresholdConfig.has("isThresholdEnabled") && currentExoplayerPlaying.reelThresholdConfig.optBoolean("isThresholdEnabled", false)){
            boolean startThresholdEnabled = currentExoplayerPlaying.reelThresholdConfig.has("isStartThresholdEnabled") && currentExoplayerPlaying.reelThresholdConfig.optBoolean("isStartThresholdEnabled", false);
            boolean endThresholdEnabled = currentExoplayerPlaying.reelThresholdConfig.has("isEndThresholdEnabled") && currentExoplayerPlaying.reelThresholdConfig.optBoolean("isEndThresholdEnabled", false);
            if(startThresholdEnabled){
                int startThreshold = currentExoplayerPlaying.reelThresholdConfig.has("startThreshold") ? currentExoplayerPlaying.reelThresholdConfig.optInt("startThreshold", 5) : 5;
                if(!currentExoplayerPlaying.isStartThresholdCrossed && percentageCompleted  > startThreshold){
                    currentExoplayerPlaying.isStartThresholdCrossed = true;


                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s', %s, %s);",
                            callback, "CURRENT_VIDEO_START_THRESHOLD_CROSSED", percentageCompleted + "", reelVideoConfig, null);

                    reelViewAdapterInterface.sendJsCallbackFromAdapter(javascript);
                }
            }

            if(endThresholdEnabled){
                int endThreshold = currentExoplayerPlaying.reelThresholdConfig.has("endThreshold") ? currentExoplayerPlaying.reelThresholdConfig.optInt("endThreshold", 80) : 80;
                if(!currentExoplayerPlaying.isEndThresholdCrossed && percentageCompleted  > endThreshold){
                    currentExoplayerPlaying.isEndThresholdCrossed = true;
                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s', %s, %s);",
                            callback, "CURRENT_VIDEO_END_THRESHOLD_CROSSED", percentageCompleted + "",reelVideoConfig, null);
                    reelViewAdapterInterface.sendJsCallbackFromAdapter(javascript);
                }
            }
        }
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.reel_view_pager_item, parent, false);
        Log.i("REELS", "initialized the data for onCreateViewHolder");
        return new ViewHolder(view, context, videoPreparedListener, reelViewAdapterInterface, reelTitleConfig, reelDescriptionConfig, reelExtraConfig);
    }

    @OptIn(markerClass = UnstableApi.class) @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.setReelsData(reelViewPagerItemArrayList.get(position), context, position, this.getItemCount());
    }

    @Override
    public int getItemCount() {
        return reelViewPagerItemArrayList.size();
    }

    public void setReelTitleConfig(JSONObject reelTitleConfig) {
        this.reelTitleConfig = reelTitleConfig;
    }

    public void setReelDescriptionConfig(JSONObject reelDescriptionConfig) {
        this.reelDescriptionConfig = reelDescriptionConfig;
    }

    public void setReelExtraConfig(JSONObject reelExtraConfig) {
        Log.i("Reel Extra Config", reelExtraConfig.toString());
        this.reelExtraConfig = reelExtraConfig;
    }

    public interface OnVideoPreparedListener {
        void onVideoPrepared(ExoplayerItem exoplayerItem);
    }

    public interface ReelViewAdapterInterface {
        @Nullable
        ExoplayerItem getCurrentExoplayerPlaying();

        void sendJsCallbackFromAdapter(String javascript);

        String getCallback();

        @Nullable
        ViewPager2 getReelViewPager();

        void abandonAudioFocus();

        void getAudioFocus();

        @Nullable
        ReelViewPagerItem getCurrentReelViewPagerItem();

        void setCurrentReelViewPagerItem(int position);

    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        ImageView thumbnailImageView;
        LinearLayout reelPauseButtonClickArea, videoProgressBar, reelInfoView, reelBottomButtonContainer, showGradient, reelSideButtonContainer;

        PlayerView reelVideoView;
        OnVideoPreparedListener videoPreparedListener;
        ReelViewAdapterInterface reelViewAdapterInterface;
        TextView reelTitleView, reelDescriptionView;

        ExoPlayer exoPlayer;
        JSONObject reelTitleConfig, reelDescriptionConfig, reelExtraConfig;
        JSONArray bottomButtonConfig, sideButtonConfig;
        View reelItemView;

        ScrollView reelInfoScrollView;
        ImageView reelPauseButton;
        Context context;
        int totalViewItems = 0;
        boolean scrollViewExpanded = false;
        SeekBar reelSeekBar;



        public ViewHolder (@NonNull View reelItemView, Context context, OnVideoPreparedListener videoPreparedListener, ReelViewAdapterInterface reelViewAdapterInterface, JSONObject reelTitleConfig, JSONObject reelDescriptionConfig, JSONObject reelExtraConfig) {
            super(reelItemView);
            this.reelItemView = reelItemView;
            this.context = context;
            thumbnailImageView = reelItemView.findViewById(R.id.reelThumbnailImageView);
            videoProgressBar = reelItemView.findViewById(R.id.reelVideoProgressBar);
            reelVideoView = reelItemView.findViewById(R.id.reelVideoView);
            reelTitleView = reelItemView.findViewById(R.id.reelTitleView);
            reelDescriptionView = reelItemView.findViewById(R.id.reelDescriptionView);
            reelInfoScrollView = reelItemView.findViewById(R.id.reelInfoScrollView);
            reelBottomButtonContainer = reelItemView.findViewById(R.id.reelBottomButtonContainer);
            reelSideButtonContainer = reelItemView.findViewById(R.id.reelSideButtonContainer);
            reelInfoView = reelItemView.findViewById(R.id.reelInfoView);
            showGradient = reelItemView.findViewById(R.id.reel_gradient_background);
            this.videoPreparedListener = videoPreparedListener;
            this.reelTitleConfig = reelTitleConfig;
            this.reelDescriptionConfig = reelDescriptionConfig;
            this.reelExtraConfig = reelExtraConfig;
            reelPauseButton = reelItemView.findViewById(R.id.reelPauseButton);
            reelPauseButtonClickArea = reelItemView.findViewById(R.id.reelPauseButtonClickView);
            this.reelViewAdapterInterface = reelViewAdapterInterface;
            reelSeekBar = reelItemView.findViewById(R.id.reels_player_view_seekbar);
        }

        public LinearLayout generateButtons(ReelViewPagerItem reelViewPagerItem, JSONArray buttonConfigDataArray, boolean isSideButton) throws JSONException { // generating the buttons for reels

            LinearLayout.LayoutParams layoutParamsWithMatchParentWidth = new LinearLayout.LayoutParams( LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
            LinearLayout.LayoutParams layoutParamsWithWrapContentWidth = new LinearLayout.LayoutParams( LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
            LinearLayout parentLayout = new LinearLayout(context);
            parentLayout.setOrientation(LinearLayout.VERTICAL);
            for(int i = 0 ; i < buttonConfigDataArray.length(); i++){
                JSONArray eachRowButtonConfig = buttonConfigDataArray.getJSONArray(i);

                LinearLayout childLayout = new LinearLayout(context);
                layoutParamsWithMatchParentWidth.setMargins(0, isSideButton ? 50 : 20, 0, 0);
                childLayout.setLayoutParams(layoutParamsWithMatchParentWidth);
                childLayout.setOrientation(LinearLayout.HORIZONTAL);

                for(int j = 0 ; j < eachRowButtonConfig.length(); j++){

                    // creating button layout
                    JSONObject buttonConfig = eachRowButtonConfig.getJSONObject(j);
                    LinearLayout button = new LinearLayout(context);
                    layoutParamsWithWrapContentWidth.setMargins(0, 0, isSideButton ? 0 : 15, 0);
                    button.setLayoutParams(layoutParamsWithWrapContentWidth);
                    button.setOrientation(LinearLayout.HORIZONTAL);
                    button.setGravity(Gravity.CENTER);
                    GradientDrawable grad = new GradientDrawable();
                    grad.setCornerRadius(1f * buttonConfig.optInt("cornerRadius", 50) * (isSideButton ? 2 : 1));
                    grad.setColor(Color.parseColor(buttonConfig.isNull("buttonColor") ? "#4DFFFFFF" : buttonConfig.optString("buttonColor","#4DFFFFFF" )));
                    button.setBackground(grad);
                    if(!isSideButton) button.setPadding(30, 15, 30, 15);
                    else button.setPadding(25, 25, 25, 25);
                    button.setOnClickListener(v -> {
                        try{

                            // todo : toggle between active and inactive images

                            boolean toDestroyActivity = false;
                            JSONArray buttonActions = new JSONArray(buttonConfig.isNull("actions") ? "[]" : buttonConfig.optString("actions", "[]"));
                            for(int i1 = 0; i1 < buttonActions.length() ; i1++){
                                String action = buttonActions.getString(i1);
                                if (action.equals("DESTROY_REEL")){toDestroyActivity = true; }
                                else{
                                    String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s', %s, %s);",
                                            reelViewAdapterInterface.getCallback(), "ACTION", action, reelViewPagerItem.reelVideoConfig, buttonConfig);
                                    reelViewAdapterInterface.sendJsCallbackFromAdapter(javascript);
                                }
                            }
                            if(toDestroyActivity || buttonActions.length() == 0){
                                Log.i("DESTROY ACTION IS ACCOMPLISHED", "DESTROYING");
                                ((Activity) context).finish();
                            }

                        }catch(Exception e){
                            Log.i("REELS_ERROR", "Exception occurred in fetching actions" + e);
                            ((Activity) context).finish();
                        }
                    });

                    // prefix Image
                    ImageView prefixImage = getImageView(buttonConfig.isNull("prefixImage") ? "" : buttonConfig.optString("prefixImage", ""),
                            buttonConfig.optInt("prefixImageWidth", 40),
                            buttonConfig.optInt("prefixImageHeight", 40),
                            0, 0, 10, 0);

                    if(prefixImage != null) button.addView(prefixImage);


                    TextView tv = new TextView(context);
                    tv.setText(buttonConfig.isNull("text") ? "Go Back" : buttonConfig.optString("text","Go Back" ));
                    tv.setTextSize(buttonConfig.optInt("textSize",15));
                    tv.setTextColor(Color.parseColor(buttonConfig.isNull("textColor") ? "#ffffff" : buttonConfig.optString("textColor","#ffffff" )));
                    button.addView(tv);

                    //suffix image
                    ImageView suffixImage = getImageView(buttonConfig.isNull("suffixImage") ? "" : buttonConfig.optString("suffixImage", ""),
                            buttonConfig.optInt("suffixImageWidth", 40),
                            buttonConfig.optInt("suffixImageHeight", 40),
                            10, 0, 0, 0);
                    if(suffixImage != null) button.addView(suffixImage);

                    // handling the case for side image buttons
                    ImageView inActiveImage = getImageView(buttonConfig.isNull("inActiveIndex") ? "" : buttonConfig.optString("inActiveIndex", ""),
                            buttonConfig.optInt("inActiveIndexWidth", 40),
                            buttonConfig.optInt("inActiveIndexHeight", 40),
                            10, 10, 10, 10);
                    if(inActiveImage != null) button.addView(inActiveImage);

                    childLayout.addView(button);

                }
                parentLayout.addView(childLayout);
            }
            return parentLayout;

        }

        public ImageView getImageView(String imageUrl, int imageWidth, int imageHeight, int MarginLeft, int MarginTop, int MarginRight, int MarginBottom){
            if(imageUrl.equals("")) return null;
            else{
                LinearLayout.LayoutParams imageViewLayoutParams = new LinearLayout.LayoutParams(imageWidth, imageHeight);
                imageViewLayoutParams.setMargins(MarginLeft, MarginTop, MarginRight, MarginBottom);
                ImageView imageView = new ImageView(context);
                imageView.setLayoutParams(imageViewLayoutParams);
                Glide.with(context)
                        .load(imageUrl)
                        .into(imageView);
                return imageView;
            }
        }

        @SuppressLint("ClickableViewAccessibility")
        @UnstableApi
        void setReelsData(ReelViewPagerItem reelViewPagerItem, Context context, int currentPosition, int totalItems){
            // setting thumbnail image
            Glide.with(context)
                    .load(reelViewPagerItem.getThumbnailImageUrl())
                    .error(R.drawable.black_background)
                    .fallback(R.drawable.black_background)
                    .diskCacheStrategy(DiskCacheStrategy.ALL)
                    .into(thumbnailImageView);

            this.totalViewItems = totalItems;
            this.reelPauseButton.setVisibility(View.GONE);
            //setting progressBar
            reelSeekBar.setVisibility(this.reelExtraConfig.optBoolean("progressBarVisible", false) ? View.VISIBLE : View.GONE);
            reelSeekBar.getProgressDrawable().setColorFilter(Color.parseColor(this.reelExtraConfig.isNull("progressBarColor") ? "#FFFFFF" : this.reelExtraConfig.optString("progressBarColor", "")), android.graphics.PorterDuff.Mode.SRC_IN);
            reelSeekBar.getThumb().mutate().setAlpha(0);
            reelSeekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
                @Override
                public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                    if(fromUser){
                        ExoplayerItem currentExoplayerPlaying = reelViewAdapterInterface.getCurrentExoplayerPlaying();
                        if(currentExoplayerPlaying != null){
                            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
                                currentExoplayerPlaying.reelSeekBar.setProgress(progress, true);
                            }else{
                                currentExoplayerPlaying.reelSeekBar.setProgress(progress);
                            }

                            float currentDuration = ((progress + 1)/100.f) * (currentExoplayerPlaying.exoPlayer.getDuration());
                            currentExoplayerPlaying.exoPlayer.seekTo((long)currentDuration);
                        }
                    }
                }

                @Override
                public void onStartTrackingTouch(SeekBar seekBar) {
                    reelSeekBar.getThumb().mutate().setAlpha(255);
                }

                @Override
                public void onStopTrackingTouch(SeekBar seekBar) {
                    reelSeekBar.getThumb().mutate().setAlpha(0);
                }
            });

            reelSeekBar.setOnTouchListener((view, motionEvent) -> !reelExtraConfig.optBoolean("seekEnabled", false));

            reelVideoView.setVisibility(View.GONE);
            thumbnailImageView.setVisibility(View.VISIBLE);

            reelTitleView.setText(reelViewPagerItem.getTitle());
            reelDescriptionView.setVisibility(reelViewPagerItem.getTitle().equals("") || reelViewPagerItem.getTitle() == null ? View.GONE : View.VISIBLE);
            reelDescriptionView.setText(reelViewPagerItem.getDescription());
            reelDescriptionView.setVisibility(reelViewPagerItem.getDescription().equals("") || reelViewPagerItem.getDescription() == null ? View.GONE : View.VISIBLE);

            try{
                bottomButtonConfig = new JSONArray(reelViewPagerItem.getBottomButtonConfig());
                sideButtonConfig = new JSONArray(reelViewPagerItem.getSideButtonConfig());
                reelTitleView.setTextSize((float) reelTitleConfig.optInt("size", 22));
                reelTitleView.setTextColor(Color.parseColor(reelTitleConfig.isNull("color") ? "#FFFFFF" : reelTitleConfig.optString("color", "#FFFFFF")));
                int titleMaxLines = reelTitleConfig.has("maxLines") ? reelTitleConfig.optInt("maxLines", 3) : 3;
                reelTitleView.setMaxLines(titleMaxLines);

                reelDescriptionView.setTextSize((float) reelDescriptionConfig.optInt("size", 22));
                reelDescriptionView.setTextColor(Color.parseColor(reelDescriptionConfig.isNull("color") ? "#FFFFFF" : reelDescriptionConfig.optString("color", "#FFFFFF")));
                int descriptionMaxLines = reelDescriptionConfig.has("maxLines") ? reelDescriptionConfig.optInt("maxLines", 2) : 2;
                reelDescriptionView.setMaxLines(descriptionMaxLines);


                reelInfoScrollView.setVerticalScrollBarEnabled(false);
                reelInfoScrollView.setHorizontalScrollBarEnabled(false);

                FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
                reelInfoView.setLayoutParams(layoutParams);

                reelInfoView.post(() -> {
                    reelInfoScrollView.setLayoutParams(new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, reelInfoView.getHeight()));
                    RelativeLayout.LayoutParams shadowLayoutParams = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, reelInfoView.getHeight() + 400);
                    shadowLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
                    showGradient.setLayoutParams(shadowLayoutParams);
                });

                reelPauseButtonClickArea.setOnClickListener(v -> {
                    ExoplayerItem currentExoplayerPlaying = reelViewAdapterInterface.getCurrentExoplayerPlaying();
                    if(currentExoplayerPlaying != null){
                        if(currentExoplayerPlaying.exoPlayer.isPlaying()){
                            currentExoplayerPlaying.exoPlayer.pause();
                            reelViewAdapterInterface.abandonAudioFocus();
                            reelPauseButton.setVisibility(View.VISIBLE);
                        }else{
                            reelViewAdapterInterface.getAudioFocus();
                            currentExoplayerPlaying.exoPlayer.play();
                            reelPauseButton.setVisibility(View.GONE);
                        }
                    }
                    else{
                        Log.i("REEL_DATA_ERROR", "current exoplayer is null in setReelsData");

                    }
                });

                reelInfoView.setOnClickListener(new View.OnClickListener() {
                    int previousHeight = 250;

                    @Override
                    public void onClick(View v) {
                        ExoplayerItem currentExoplayerPlaying = reelViewAdapterInterface.getCurrentExoplayerPlaying();
                        if(currentExoplayerPlaying == null) return;
                        if(currentExoplayerPlaying.scrollViewExpanded){ //compress the view
                            ValueAnimator animator = ValueAnimator.ofInt(reelInfoScrollView.getHeight(), previousHeight);
                            animator.addUpdateListener(animation -> {
                                int animatedValue = (int) animation.getAnimatedValue();
                                LinearLayout.LayoutParams layoutParams3 = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, animatedValue);
                                reelInfoScrollView.setLayoutParams(layoutParams3);

                                RelativeLayout.LayoutParams layoutParams2 = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, animatedValue + 400);
                                layoutParams2.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
                                showGradient.setLayoutParams(layoutParams2);
                            });

                            animator.addListener(new Animator.AnimatorListener() {
                                @Override
                                public void onAnimationStart(@NonNull Animator animation) {

                                }

                                @Override
                                public void onAnimationEnd(@NonNull Animator animation) {
                                    reelDescriptionView.setMaxLines(descriptionMaxLines);
                                }

                                @Override
                                public void onAnimationCancel(@NonNull Animator animation) {

                                }

                                @Override
                                public void onAnimationRepeat(@NonNull Animator animation) {

                                }
                            });

                            animator.setDuration(300);
                            animator.start();

                            currentExoplayerPlaying.scrollViewExpanded = false;
                        }else{ // expand the view

                            previousHeight = reelInfoView.getHeight();

                            reelDescriptionView.setMaxLines(100);
                            reelDescriptionView.post(() -> {
                                ValueAnimator animator = ValueAnimator.ofInt(previousHeight, Math.min(reelInfoView.getHeight(), 700));
                                animator.addUpdateListener(animation -> {
                                    int animatedValue = (int) animation.getAnimatedValue();
                                    LinearLayout.LayoutParams layoutParams3 = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, animatedValue);
                                    reelInfoScrollView.setLayoutParams(layoutParams3);

                                    RelativeLayout.LayoutParams layoutParams2 = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, animatedValue + (400 * bottomButtonConfig.length()) );
                                    layoutParams2.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
                                    showGradient.setLayoutParams(layoutParams2);
                                });


                                animator.setDuration(300);
                                animator.start();
                                currentExoplayerPlaying.scrollViewExpanded = true;
                            });

                        }
                    }
                });


                // add dynamic button at the bottom
                LinearLayout bottomButtons = generateButtons(reelViewPagerItem, bottomButtonConfig, false);
                reelBottomButtonContainer.addView(bottomButtons);

                // add dynamic button at the side
                LinearLayout sideButtons = generateButtons(reelViewPagerItem, sideButtonConfig, true);
                reelSideButtonContainer.addView(sideButtons);
            }
            catch(Exception e){
                Log.e("REEL", e.toString());
                Toast.makeText(context, "Something went wrong. Please try again later!", Toast.LENGTH_SHORT).show();
                ((Activity) context).finish();
            }

            exoPlayer = new ExoPlayer.Builder(context).build();
            videoPreparedListener.onVideoPrepared(new ExoplayerItem(getAbsoluteAdapterPosition(), exoPlayer, reelSeekBar,  reelPauseButton, reelViewPagerItem.getThresholdConfig(), scrollViewExpanded, reelInfoView));

            boolean isLastItem = currentPosition == totalItems - 1;
            exoPlayer.setRepeatMode(Player.REPEAT_MODE_ONE);
            exoPlayer.addListener(new Player.Listener() {

                @Override
                public void onPlayerError(@NonNull PlaybackException error) {
                    error.printStackTrace();
                    Log.e("REEL_EXOPLAYER_PLAYER_ERROR", "Error occurred during reels playback");
                    ExoplayerItem currentExoplayerPlaying = reelViewAdapterInterface.getCurrentExoplayerPlaying();
                    if(currentExoplayerPlaying == null) {
                        Toast.makeText(context, "Something went wrong. Please try again later!", Toast.LENGTH_SHORT).show();
                        ((Activity) context).finish();
                    }else{
                        Toast.makeText(context, "Something went wrong. Please try again!", Toast.LENGTH_SHORT).show();
                        currentExoplayerPlaying.exoPlayer.pause();
                        currentExoplayerPlaying.exoPlayer.seekTo(0);
                        reelPauseButton.setVisibility(View.VISIBLE);
                    }
                }

                @Override
                public void onPositionDiscontinuity(@NonNull Player.PositionInfo oldPosition, @NonNull Player.PositionInfo newPosition, int reason) {
                    Player.Listener.super.onPositionDiscontinuity(oldPosition, newPosition, reason);
                    if(reason == Player.DISCONTINUITY_REASON_AUTO_TRANSITION){
                        Log.i("REELS", "StateEnded");
                        // check if autoSwipeToNextItem is enabled or not
                        ExoplayerItem currentExoplayerPlaying = reelViewAdapterInterface.getCurrentExoplayerPlaying();

                        if((reelExtraConfig.has("autoSwipeToNext") && reelExtraConfig.optBoolean("autoSwipeToNext", false)) && !isLastItem){
                            reelViewAdapterInterface.setCurrentReelViewPagerItem((currentPosition + 1) % totalItems);
                            return;
                        }

                        if(currentExoplayerPlaying == null) return;
                        if(!(currentExoplayerPlaying.reelThresholdConfig.has("isThresholdEnabled") && currentExoplayerPlaying.reelThresholdConfig.optBoolean("isThresholdEnabled", false)) &&
                                !(currentExoplayerPlaying.reelThresholdConfig.has("sendCallbackAfterEverySecondEnabled") && currentExoplayerPlaying.reelThresholdConfig.optBoolean("sendCallbackAfterEverySecondEnabled", false))) {
                            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s', %s, %s);",
                                    reelViewAdapterInterface.getCallback(), "CURRENT_VIDEO_COMPLETED_100", 100 + "", reelViewPagerItem.reelVideoConfig, null);
                            reelViewAdapterInterface.sendJsCallbackFromAdapter(javascript);
                        }

                        if((reelExtraConfig.has("bounceAnimationEnabled") && reelExtraConfig.optBoolean("bounceAnimationEnabled", false)) && !isLastItem){
                            int bounceAnimationCount = reelExtraConfig.has("bounceAnimationCount") ? reelExtraConfig.optInt("bounceAnimationCount", 2): 2;
                            int bounceAnimationDuration = reelExtraConfig.has("bounceAnimationDuration") ? reelExtraConfig.optInt("bounceAnimationDuration", 400): 400;
                            playBounceAnimationOnOnView(reelViewAdapterInterface.getReelViewPager(), bounceAnimationCount, bounceAnimationDuration);

                        }

                        if(currentExoplayerPlaying.reelSeekBar != null)
                            currentExoplayerPlaying.reelSeekBar.setProgress(0);
                        currentExoplayerPlaying.isEndThresholdCrossed = false;
                        currentExoplayerPlaying.isStartThresholdCrossed = false;
                    }

                }

                @Override
                public void onPlaybackStateChanged(int playbackState) {
                    switch (playbackState){
                        case Player.STATE_BUFFERING:
                            videoProgressBar.setVisibility(View.VISIBLE);
                            break;
                        case Player.STATE_READY:
                            videoProgressBar.setVisibility(View.GONE);
                            thumbnailImageView.setVisibility(View.GONE);
                            reelVideoView.setVisibility(View.VISIBLE);
                        default:
                            break;
                    }
                }
            });

            reelVideoView.setPlayer(exoPlayer);
            exoPlayer.seekTo(0);

            DataSource.Factory dataSourceFactory = new DefaultDataSource.Factory(context);
            mediaSource = new ProgressiveMediaSource.Factory(dataSourceFactory).createMediaSource(MediaItem.fromUri(Uri.parse(reelViewPagerItem.getVideoUrl())));
            exoPlayer.setMediaSource(mediaSource);
            exoPlayer.prepare();
            exoPlayer.setPlayWhenReady(false);

        }

    }

    private static void playBounceAnimationOnOnView( View view, int bounceAnimationCount, int bounceAnimationDuration){
        final int totalAnimationCount = bounceAnimationCount;
        ObjectAnimator upAnimator = ObjectAnimator.ofFloat(view, View.TRANSLATION_Y, 0f, -150f );
        upAnimator.setDuration(bounceAnimationDuration);

        ObjectAnimator downAnimator = ObjectAnimator.ofFloat(view, View.TRANSLATION_Y, -150f, 0);
        downAnimator.setDuration(bounceAnimationDuration);

        AnimatorSet animatorSet = new AnimatorSet();
        animatorSet.playSequentially(upAnimator, downAnimator);

        animatorSet.addListener(new Animator.AnimatorListener() {
            int loopCount = 0;

            @Override
            public void onAnimationStart(Animator animation) {
            }

            @Override
            public void onAnimationEnd(Animator animation) {
                if (++loopCount < totalAnimationCount) {
                    animatorSet.start();
                }
            }

            @Override
            public void onAnimationCancel(Animator animation) {
            }

            @Override
            public void onAnimationRepeat(Animator animation) {
            }
        });

        animatorSet.start();

    }

}
