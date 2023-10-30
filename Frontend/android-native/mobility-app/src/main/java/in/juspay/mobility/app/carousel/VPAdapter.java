package in.juspay.mobility.app.carousel;

import android.content.Context;
import android.content.Intent;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.drawable.GradientDrawable;
import android.text.Html;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerFullScreenListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.options.IFramePlayerOptions;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.ui.DefaultPlayerUiController;

import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.app.R;
import in.juspay.mobility.app.SheetAdapter;
import in.juspay.mobility.app.Utils;
import in.juspay.mobility.app.YoutubeVideoView;
import okhttp3.internal.Util;

public class VPAdapter extends RecyclerView.Adapter<VPAdapter.ViewHolder> {
    ArrayList<ViewPagerItem> viewPagerItemArrayList;
    public static float videoDuration = 0;

    private VPAdapterListener listener;
    public static YouTubePlayerView youTubePlayerView ;
    public static YouTubePlayer youtubePlayer;
    public Context context ;
    public VPAdapter(ArrayList<ViewPagerItem> viewPagerItemArrayList, Context context, VPAdapterListener listener ) {
        this.viewPagerItemArrayList = viewPagerItemArrayList;
        this.context = context;
        this.listener = listener;
    }
    public VPAdapter(ArrayList<ViewPagerItem> viewPagerItemArrayList) {
        this.viewPagerItemArrayList = viewPagerItemArrayList;
    }

    public interface VPAdapterListener{
        void onViewHolderBind(ViewHolder holder, int position, Context context);
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.viewpager_item,parent,false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        if (listener != null) {
            listener.onViewHolderBind(holder, position, context);
        } else {
            ViewPagerItem viewPagerItem = viewPagerItemArrayList.get(position);
            holder.imageView.setImageResource(viewPagerItem.imageID);
            holder.imageView.getLayoutParams().height = (Resources.getSystem().getDisplayMetrics().heightPixels)/3;
            holder.tvHeading.setText(viewPagerItem.getTitleText());
            holder.tvDesc.setText(viewPagerItem.getDescriptionText());
        }

    }
    @Override
    public int getItemCount() {
        return viewPagerItemArrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{
        public ImageView imageView;
        public LinearLayout video;
        public LinearLayout parentLinearLayout;
        public TextView tvHeading;
        public TextView tvDesc;
        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            imageView = itemView.findViewById(R.id.carouselImageView);
            tvHeading = itemView.findViewById(R.id.tvHeading);
            tvDesc = itemView.findViewById(R.id.tvDesc);
            video = itemView.findViewById(R.id.videoViewLinearLayout);
            parentLinearLayout = itemView.findViewById(R.id.parentLinearLayout);
        }
    }

    public void embedYoutubeVideo(Context context, String rawJson, String videoStatus, LinearLayout video) {
            videoDuration = 0;
            ExecutorManager.runOnMainThread(() -> {
                try {
                    if (videoStatus.equals("PAUSE")) {
                        pauseYoutubeVideo();
                    } else {
                        JSONObject json = new JSONObject(rawJson);
                        boolean showMenuButton = json.getBoolean("showMenuButton");
                        boolean showDuration = json.getBoolean("showDuration");
                        boolean setVideoTitle = json.getBoolean("setVideoTitle");
                        boolean showSeekBar = json.getBoolean("showSeekBar");
                        String videoTitle = json.getString("videoTitle");
                        String videoId = json.getString("videoId");
                        String videoType = "VIDEO";
                        int videoHeight = json.getInt("videoHeight");
                        if (json.has("videoType")) {
                            videoType = json.getString("videoType");
                        }
                        youTubePlayerView = new YouTubePlayerView(context);
                        LinearLayout layout = video ;
                        layout.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT , videoHeight));
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
                                    Log.e("error inside embedYoutubeVideo onReady", String.valueOf(e));
                                }
                            }

                            @Override
                            public void onCurrentSecond(@NonNull YouTubePlayer youTubePlayer, float second) {
                                videoDuration = second;
                            }
                        };
                        if (videoHeight != 0 )
                        {   ViewGroup.LayoutParams layoutParams = youTubePlayerView.getLayoutParams();
                            layoutParams.height = videoHeight;
                            youTubePlayerView.setLayoutParams(layoutParams);
                            youTubePlayerView.setMinimumHeight(700);
                        }

                        String finalVideoType = videoType;
                        youTubePlayerView.addFullScreenListener(new YouTubePlayerFullScreenListener() {
                            @Override
                            public void onYouTubePlayerExitFullScreen() {
                                if (videoHeight != 0 )
                                    {   ViewGroup.LayoutParams layoutParams = youTubePlayerView.getLayoutParams();
                                        layoutParams.height = videoHeight;
                                        youTubePlayerView.setLayoutParams(layoutParams);
                                        youTubePlayerView.setMinimumHeight(700);
                                    }
                            }

                            @Override
                            public void onYouTubePlayerEnterFullScreen() {
                                Intent newIntent = new Intent(context, YoutubeVideoView.class);
                                newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                newIntent.putExtra("videoId", videoId);
                                newIntent.putExtra("videoDuration", videoDuration);
                                newIntent.putExtra("videoType", finalVideoType);
                                context.startActivity(newIntent);
                            }
                        });

                        IFramePlayerOptions options = new IFramePlayerOptions.Builder().controls(0).rel(0).build();
                        youTubePlayerView.initialize(youTubePlayerListener, options);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
    }

    public void pauseYoutubeVideo() {
        if (youTubePlayerView != null) {
            if(youtubePlayer != null)
                youtubePlayer.pause();
            youTubePlayerView = null;
        }
    }

    

}