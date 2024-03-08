package in.juspay.mobility.app.reels;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
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
import android.os.Looper;
import android.provider.MediaStore;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.view.animation.Animation;
import android.view.animation.AnimationSet;
import android.view.animation.BounceInterpolator;
import android.view.animation.LinearInterpolator;
import android.view.animation.TranslateAnimation;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import androidx.annotation.NonNull;
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
import com.bumptech.glide.load.Options;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.request.RequestOptions;

import com.google.android.material.datepicker.MaterialStyledDatePickerDialog;
import com.google.android.material.progressindicator.LinearProgressIndicator;


import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.mobility.app.MobilityAppBridge;
import in.juspay.mobility.app.R;

public class ReelViewAdapter extends RecyclerView.Adapter<ReelViewAdapter.ViewHolder> {

    ArrayList <ReelViewPagerItem> reelViewPagerItemArrayList;
    public Context context;
    static MediaSource mediaSource;
    public JSONObject reelTitleConfig, reelDescriptionConfig;
    OnVideoPreparedListener videoPreparedListener;

    Handler handler;

    BridgeComponents bridgeComponents;
    Runnable updatePercentageVideoCompletedRunnable;

    public ReelViewAdapter(ArrayList<ReelViewPagerItem> reelViewPagerItemArrayList,  Context context, BridgeComponents bridgeComponents, OnVideoPreparedListener videoPreparedListener) {
        this.context = context;
        this.reelViewPagerItemArrayList = reelViewPagerItemArrayList;
        this.bridgeComponents = bridgeComponents;
        this.videoPreparedListener = videoPreparedListener;
        this.handler = new Handler();

        updatePercentageVideoCompletedRunnable = new Runnable() { // the handler to get the current Video playing percentage
            @Override
            public void run() {
                getPercentageVideoCompleted();
                handler.postDelayed(this, 1000);
            }
        };
        handler.postDelayed(updatePercentageVideoCompletedRunnable, 1000);
        Log.i("REELS", "initialized the data");
    }

    public void getPercentageVideoCompleted(){
        if (ReelController.currentExoplayerPlaying != null){
            long currentPosition = ReelController.currentExoplayerPlaying.getCurrentPosition();
            long duration = ReelController.currentExoplayerPlaying.getDuration();
            int percentageCompleted = (int) ((currentPosition * 100) / duration);
            if(percentageCompleted < 97) {
                ReelController.currentHorizontalProgressBar.setProgressCompat(percentageCompleted, true);
            }
        }
    }

    public void setBridgeComponents(BridgeComponents bridgeComponents){
        this.bridgeComponents = bridgeComponents;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.reel_view_pager_item, parent, false);
        Log.i("REELS", "initialized the data for onCreateViewHolder");
        return new ViewHolder(view, context, videoPreparedListener, reelTitleConfig, reelDescriptionConfig);
    }

    @OptIn(markerClass = UnstableApi.class) @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.setReelsData(reelViewPagerItemArrayList.get(position), context);
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

    public interface OnVideoPreparedListener {
       public void onVideoPrepared(ExoplayerItem exoplayerItem);
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        ImageView thumbnailImageView;
        LinearProgressIndicator reelHorizontalProgressBar ;
        LinearLayout videoProgressBar, reelInfoView, reelBottomButtonContainer, showGradient, reelSideButtonContainer;

        PlayerView reelVideoView;
        OnVideoPreparedListener videoPreparedListener;
        TextView reelTitleView, reelDescriptionView;

        ExoPlayer exoPlayer;
        JSONObject reelTitleConfig, reelDescriptionConfig;
        JSONArray bottomButtonConfig, sideButtonConfig;
        View reelItemView;

        ScrollView reelInfoScrollView;
        Context context;
        boolean scrollViewExpanded = false;



        public ViewHolder (@NonNull View reelItemView, Context context, OnVideoPreparedListener videoPreparedListener, JSONObject reelTitleConfig, JSONObject reelDescriptionConfig) {
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
            this.reelHorizontalProgressBar = reelItemView.findViewById(R.id.reels_player_view_progress);
        }

        public LinearLayout generateButtons(JSONArray buttonConfigDataArray, boolean isSideButton) throws JSONException { // generating the buttons for reels

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
                    grad.setColor(Color.parseColor(buttonConfig.optString("buttonColor","#4D000000" )));
                    button.setBackground(grad);
                    if(!isSideButton) button.setPadding(30, 15, 30, 15);
                    else button.setPadding(25, 25, 25, 25);

                    button.setOnClickListener(new View.OnClickListener() {
                        @Override
                        public void onClick(View v) {
                            try{

                                // todo : toggle between active and inactive images

                                boolean toDestroyActivity = false;
                                JSONArray buttonActions = buttonConfig.optJSONArray("actions");
                                for(int i = 0; i < buttonActions.length() ; i++){
                                    String action = buttonActions.getString(i);
                                    if (action.equals("DESTROY")){toDestroyActivity = true; }
                                    else{
                                        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s', '%s');",
                                                ReelController.callback, "ACTION", action);
                                        ReelController.bridgeComponentsInternal.getJsCallback().addJsToWebView(javascript);
                                    }
                                }
                                if(toDestroyActivity){
                                    Log.i("ACTION IS ACCOMPLISHED", "DESTROYING");
                                    ((Activity) context).finish();
                                }

                            }catch(Exception e){
                                Log.i("REELS_ERROR", "Exception occurred in fetching actions");
                                ((Activity) context).finish();
                            }
                        }
                    });


                    // prefix Image
                    ImageView prefixImage = getImageView(buttonConfig.optString("prefixImage", ""),
                            buttonConfig.optInt("prefixImageWidth", 40),
                            buttonConfig.optInt("prefixImageHeight", 40),
                            0, 0, 10, 0);

                    if(prefixImage != null) button.addView(prefixImage);


                    TextView tv = new TextView(context);
                    tv.setText(buttonConfig.optString("text","Go Back" ));
                    tv.setTextSize(buttonConfig.optInt("textSize",15));
                    tv.setTextColor(Color.parseColor(buttonConfig.optString("textColor","#ffffff" )));
                    button.addView(tv);

                    //suffix image
                    ImageView suffixImage = getImageView(buttonConfig.optString("suffixImage", ""),
                            buttonConfig.optInt("suffixImageWidth", 40),
                            buttonConfig.optInt("suffixImageHeight", 40),
                            10, 0, 0, 0);
                    if(suffixImage != null) button.addView(suffixImage);

                    // handling the case for side image buttons
                    ImageView inActiveImage = getImageView(buttonConfig.optString("inActiveIndex", ""),
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

        @UnstableApi
        void setReelsData(ReelViewPagerItem reelViewPagerItem, Context context){
            // setting thumbnail image
            Glide.with(context)
            .load(reelViewPagerItem.getThumbnailImageUrl())
            .error(R.drawable.black_background)
            .fallback(R.drawable.black_background)
            .diskCacheStrategy(DiskCacheStrategy.ALL)
            .into(thumbnailImageView);

            //setting progressBar
            videoProgressBar.setVisibility(View.VISIBLE);
            reelVideoView.setVisibility(View.GONE);
            thumbnailImageView.setVisibility(View.VISIBLE);

            reelTitleView.setText(reelViewPagerItem.getTitle());
            reelDescriptionView.setText(reelViewPagerItem.getDescription());

            try{
                bottomButtonConfig = new JSONArray(reelViewPagerItem.getBottomButtonConfig());
                sideButtonConfig = new JSONArray(reelViewPagerItem.getSideButtonConfig());
                reelTitleView.setTextSize(Float.valueOf(reelTitleConfig.has("size") ? reelTitleConfig.getInt("size") : 22));
                reelTitleView.setTextColor(Color.parseColor(reelTitleConfig.has("color") ? reelTitleConfig.getString("color") : "#FFFFFF"));
                int titleMaxLines = reelTitleConfig.has("maxLines") ? reelTitleConfig.getInt("maxLines") : 3;
                reelTitleView.setMaxLines(titleMaxLines);

                reelDescriptionView.setTextSize(Float.valueOf(reelDescriptionConfig.has("size") ? reelDescriptionConfig.getInt("size") : 22));
                reelDescriptionView.setTextColor(Color.parseColor(reelDescriptionConfig.has("color") ? reelDescriptionConfig.getString("color") : "#FFFFFF"));
                int descriptionMaxLines = reelDescriptionConfig.has("maxLines") ? reelDescriptionConfig.getInt("maxLines") : 2;
                reelDescriptionView.setMaxLines(descriptionMaxLines);


                reelInfoScrollView.setVerticalScrollBarEnabled(false);
                reelInfoScrollView.setHorizontalScrollBarEnabled(false);

                FrameLayout.LayoutParams layoutParams = new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT);
                reelInfoView.setLayoutParams(layoutParams);

                reelInfoView.post(new Runnable() {
                    @Override
                    public void run() {
                        reelInfoScrollView.setLayoutParams(new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, reelInfoView.getHeight()));
                        RelativeLayout.LayoutParams shadowLayoutParams = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, reelInfoView.getHeight() + 400);
                        shadowLayoutParams.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
                        showGradient.setLayoutParams(shadowLayoutParams);
                    }
                });


                reelInfoView.setOnClickListener(new View.OnClickListener() {
                    int previousHeight = 250;

                    @Override
                    public void onClick(View v) {
                        if(scrollViewExpanded){ //compress the view
                            ValueAnimator animator = ValueAnimator.ofInt(reelInfoScrollView.getHeight(), previousHeight);
                            animator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
                                @Override
                                public void onAnimationUpdate(ValueAnimator animation) {
                                    int animatedValue = (int) animation.getAnimatedValue();
                                    LinearLayout.LayoutParams layoutParams3 = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, animatedValue);
                                    reelInfoScrollView.setLayoutParams(layoutParams3);

                                    RelativeLayout.LayoutParams layoutParams2 = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, animatedValue + 400);
                                    layoutParams2.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
                                    showGradient.setLayoutParams(layoutParams2);
                                }

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

                            scrollViewExpanded = false;
                        }else{ // expand the view

                            previousHeight = reelInfoView.getHeight();

                            reelDescriptionView.setMaxLines(100);
                            reelDescriptionView.post(new Runnable() {
                                @Override
                                public void run() {
                                    ValueAnimator animator = ValueAnimator.ofInt(previousHeight, reelInfoView.getHeight() > 700 ? 700 : reelInfoView.getHeight() );
                                    animator.addUpdateListener(new ValueAnimator.AnimatorUpdateListener() {
                                        @Override
                                        public void onAnimationUpdate(ValueAnimator animation) {
                                            int animatedValue = (int) animation.getAnimatedValue();
                                            LinearLayout.LayoutParams layoutParams3 = new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, animatedValue);
                                            reelInfoScrollView.setLayoutParams(layoutParams3);

                                            RelativeLayout.LayoutParams layoutParams2 = new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, animatedValue + (400 * bottomButtonConfig.length()) );
                                            layoutParams2.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM);
                                            showGradient.setLayoutParams(layoutParams2);
                                        }
                                    });


                                    animator.setDuration(300);
                                    animator.start();
                                    scrollViewExpanded = true;
                                }
                            });

                        }
                    }
                });


                // add dynamic button at the bottom
                LinearLayout bottomButtons = generateButtons(bottomButtonConfig, false);
                reelBottomButtonContainer.addView(bottomButtons);

                // add dynamic button at the side
                LinearLayout sideButtons = generateButtons(sideButtonConfig, true);
                reelSideButtonContainer.addView(sideButtons);
            }
            catch(Exception e){
                Log.e("REEL", e.toString());
            }

            exoPlayer = new ExoPlayer.Builder(context).build();
            exoPlayer.setRepeatMode(Player.REPEAT_MODE_ONE);
            exoPlayer.addListener(new Player.Listener() {

                @Override
                public void onPlayerError(PlaybackException error) {
                    error.printStackTrace();
                    Log.e("REEL_EXOPLAYER_PLAYER_ERROR", "Error occurred during reels playback");
                }


                @Override
                public void onPositionDiscontinuity(Player.PositionInfo oldPosition, Player.PositionInfo newPosition, int reason) {
                    Player.Listener.super.onPositionDiscontinuity(oldPosition, newPosition, reason);
                    if(reason == Player.DISCONTINUITY_REASON_AUTO_TRANSITION){
                        Log.i("REELS", "StateEnded");
                        playBounceAnimationOnOnView(ReelController.reelViewPager);
                        ReelController.currentHorizontalProgressBar.setProgress(0);
                    }

                }

                @Override
                public void onPlaybackStateChanged(int playbackState) {
                    Log.i("This is the state", playbackState + "");
                    switch (playbackState){
                        case Player.STATE_BUFFERING:
                            videoProgressBar.setVisibility(View.VISIBLE);
                            break;
                        case Player.STATE_READY:
                            videoProgressBar.setVisibility(View.GONE);
                            thumbnailImageView.setVisibility(View.GONE);
                            reelVideoView.setVisibility(View.VISIBLE);
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
            videoPreparedListener.onVideoPrepared(new ExoplayerItem(getAbsoluteAdapterPosition(), exoPlayer, reelHorizontalProgressBar));

        }

    }

    private static void playBounceAnimationOnOnView( View view){
        final int totalAnimationCount = 2;
        final long animationDuration = 400L; // in milliseconds

        ObjectAnimator upAnimator = ObjectAnimator.ofFloat(view, View.TRANSLATION_Y, 0f, -150f );
        upAnimator.setDuration(animationDuration);

        ObjectAnimator downAnimator = ObjectAnimator.ofFloat(view, View.TRANSLATION_Y, -150f, 0);
        downAnimator.setDuration(animationDuration);

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
