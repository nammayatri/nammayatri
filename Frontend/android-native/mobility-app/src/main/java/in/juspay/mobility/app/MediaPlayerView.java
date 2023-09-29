/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.app.Activity;
import android.content.Context;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.Nullable;

import java.io.FileInputStream;
import java.io.IOException;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.app.mediaPlayer.DefaultMediaPlayerControl;
import in.juspay.mobility.app.mediaPlayer.MediaPlayerControl;
import in.juspay.mobility.app.mediaPlayer.MediaPlayerOnCompleteListener;
import in.juspay.mobility.app.mediaPlayer.MediaPlayerOnDurationListener;
import in.juspay.mobility.app.mediaPlayer.MediaPlayerOnPauseListener;
import in.juspay.mobility.app.mediaPlayer.MediaPlayerOnPlayListener;
import in.juspay.mobility.app.mediaPlayer.MediaPlayerOnPreparedListener;

public class MediaPlayerView extends FrameLayout implements MediaPlayerOnPlayListener,
        MediaPlayerOnCompleteListener,
        MediaPlayerOnDurationListener,
        MediaPlayerOnPauseListener,
        MediaPlayerOnPreparedListener,
        OnTaskCompleteListener {

    protected final Context context;
    protected MediaPlayerControl player = new DefaultMediaPlayerControl();
    protected int layout = R.layout.sounwave_view;
    private SoundVisualizerBarView visualizerBar;
    private TextView timer;
    private ImageView actionButton;
    private ProgressBar progressLoader;
    private Activity activity;
    private int frameID = -1;
    private int timerID = -1;
    private String playIcon = "ic_play";
    private String pauseIcon = "ic_pause";

    public MediaPlayerView(Context context, Activity activity) {
        super(context);
        this.context = context;
        this.activity = activity;
        init(context);
    }

    public MediaPlayerView(Context context, Activity activity, int frameID, String playIcon, String pauseIcon) {
        super(context);
        this.context = context;
        this.activity = activity;
        this.frameID = frameID;
        this.playIcon = playIcon;
        this.pauseIcon = pauseIcon;
        init(context);
    }

    public MediaPlayerView(Context context, Activity activity, int frameID, String playIcon, String pauseIcon, int timerID) {
        super(context);
        this.context = context;
        this.activity = activity;
        this.frameID = frameID;
        this.playIcon = playIcon;
        this.pauseIcon = pauseIcon;
        this.timerID = timerID;
        init(context);
    }

    public MediaPlayerView(Activity activity, Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        this.context = context;
        this.activity = activity;

        init(context);
    }

    public MediaPlayerView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.context = context;

        init(context);
    }

    public void addAudioFileUri(final Uri audioFileUri) throws IOException {
        player.setAudioSource(context, audioFileUri);
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    try {
                        visualizerBar.updateVisualizer(audioFileUri);
                    } catch (Exception e) {

                    }
                }
            });
        }
    }

    public void addAudioFileUrl(String audioFileUrl) throws IOException {
        player.setAudioSource(audioFileUrl);
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    try {
                        visualizerBar.updateVisualizer(audioFileUrl);
                    } catch (Exception e) {

                    }
                }
            });
        }
    }

    public void addAudioFileInput(FileInputStream file) throws IOException {
        player.setAudioSource(file);
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    visualizerBar.updateVisualizer(file);
                }
            });
        }
        onTaskComplete();
    }

    public void updateVisualizer(Uri audioFileUrl) throws IOException {
        visualizerBar.updateVisualizer(audioFileUrl);
    }

    public MediaPlayerControl getPlayer() {
        return player;
    }

    protected void init(final Context context) {
        View view = LayoutInflater.from(context).inflate(layout, this);

        player.setOnCompleteListener(this)
                .setOnDurationListener(this)
                .setOnPauseListener(this)
                .setOnPlayListener(this)
                .setOnPrepariedListener(this);

        visualizerBar = view.findViewById(R.id.vSoundBar);
        timer = view.findViewById(R.id.vTimer);
        progressLoader = view.findViewById(R.id.audio_loader);
        actionButton = view.findViewById(R.id.vActionButton);
        visualizerBar.setOnTaskCompleteListener(this);
        if (frameID != -1) {
            FrameLayout loader = view.findViewById(R.id.vLoaderPlayer);
            ProgressBar progressBar = view.findViewById(R.id.audio_loader);
            progressBar.setBackgroundColor(Color.TRANSPARENT);
            ((ViewGroup) loader.getParent()).removeView(loader);
            LinearLayout linearLayout = activity.findViewById(frameID);
            linearLayout.removeAllViews();
            linearLayout.addView(loader);
            actionButton.setImageDrawable(getDrawable(context, playIcon));
            actionButton.getLayoutParams().height = linearLayout.getLayoutParams().height;
            actionButton.getLayoutParams().width = linearLayout.getLayoutParams().width;
        } else {
            int marginInPixels = (int) (8 * getResources().getDisplayMetrics().density + 0.5f);
            ViewGroup.MarginLayoutParams actionLayoutParams = (ViewGroup.MarginLayoutParams) actionButton.getLayoutParams();
            actionButton.setLayoutParams(actionLayoutParams);
            actionLayoutParams.setMargins(marginInPixels, 0, marginInPixels, 0);
        }
        if (timerID != -1) {
            TextView timer = view.findViewById(R.id.vTimer);
            ((ViewGroup) timer.getParent()).removeView(timer);
            LinearLayout linearLayout = activity.findViewById(timerID);
            linearLayout.removeAllViews();
            linearLayout.addView(timer);
        } else {
            TextView timer = view.findViewById(R.id.vTimer);
            ViewGroup.MarginLayoutParams timerParams = (ViewGroup.MarginLayoutParams) timer.getLayoutParams();
            int marginInPixels = (int) (8 * getResources().getDisplayMetrics().density + 0.5f);
            timerParams.setMargins(marginInPixels, 0, marginInPixels, 0);
            timer.setLayoutParams(timerParams);
        }
        actionButton.setOnClickListener(runner -> player.toggle());
    }

    public void setTimerColorAndSize(int color, float size) {
        timer.setTextSize(size);
        timer.setTextColor(color);
    }

    public void setVisualizerBarPlayedColor(int color) {
        visualizerBar.setNonPlayedStateColor(color);
    }

    public void inflateView(int id) throws IOException {
        if (activity != null) {
            LinearLayout linearLayout = activity.findViewById(id);
            if (linearLayout != null) {
                linearLayout.addView(this);
            }
        }
    }

    public void resetListeners() {
        player.setOnCompleteListener(null)
                .setOnDurationListener(null)
                .setOnPauseListener(null)
                .setOnPrepariedListener(null)
                .setOnPlayListener(null);
        visualizerBar.setOnTaskCompleteListener(null);
        visualizerBar.cancelTask();
        ExecutorManager.runOnMainThread(() -> {
            if (getParent() instanceof ViewGroup) {
                ((ViewGroup) getParent()).removeView(MediaPlayerView.this);
            }
        });
        player.stop();
    }

    @Override
    public void onComplete(MediaPlayerControl player) {
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    actionButton.setImageDrawable(getDrawable(context, playIcon));
                    visualizerBar.updatePlayerPercent(0);
                    timer.setText(getTime(player.getDuration()));
                }
            });
        }
    }

    @Override
    public void onDurationProgress(MediaPlayerControl player, Long duration, Long currentTimestamp) {
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    visualizerBar.updatePlayerPercent(currentTimestamp / (float) duration);
                    timer.setText(getTime(Math.max(0, duration - currentTimestamp)));
                }
            });
        }
    }

    @Override
    public void onPause(MediaPlayerControl player) {
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    actionButton.setImageDrawable(getDrawable(context, playIcon));
                }
            });
        }
    }

    @Override
    public void onPlay(MediaPlayerControl player) {
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    actionButton.setImageDrawable(getDrawable(context, pauseIcon));
                }
            });
        }
    }

    public Drawable getDrawable(Context context, String name) {
        Resources resources = context.getResources();
        final int resourceId = resources.getIdentifier(name, "drawable",
                context.getPackageName());
        return resources.getDrawable(resourceId);
    }

    @Override
    public void onPrepared(MediaPlayerControl player) {
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    timer.setText(getTime(player.getDuration()));
                    actionButton.setClickable(true);
                }
            });
        }
    }

    public String getTime(long milliseconds) {
        long minutes = (milliseconds / 1000) / 60;
        long seconds = (milliseconds / 1000) % 60;
        return (minutes + ":" + String.format("%02d", seconds));
    }

    @Override
    public void onTaskComplete() {
        if (activity != null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    actionButton.setVisibility(VISIBLE);
                    progressLoader.setVisibility(GONE);
                }
            });
        }
    }
}
