/* 
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.utils;

import android.app.Activity;
import android.content.Context;
import android.media.MediaPlayer;
import android.net.Uri;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

import in.juspay.mobility.CommonJsInterface;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;
import in.juspay.mobility.utils.mediaPlayer.DefaultMediaPlayerControl;
import in.juspay.mobility.utils.mediaPlayer.MediaPlayerControl;
import in.juspay.mobility.utils.mediaPlayer.MediaPlayerOnCompleteListener;
import in.juspay.mobility.utils.mediaPlayer.MediaPlayerOnDurationListener;
import in.juspay.mobility.utils.mediaPlayer.MediaPlayerOnPauseListener;
import in.juspay.mobility.utils.mediaPlayer.MediaPlayerOnPlayListener;
import in.juspay.mobility.utils.mediaPlayer.MediaPlayerOnPreparedListener;
import in.juspay.hypersdk.core.DuiCallback;

public class MediaPlayerView extends FrameLayout implements MediaPlayerOnPlayListener,
        MediaPlayerOnCompleteListener,
        MediaPlayerOnDurationListener,
        MediaPlayerOnPauseListener,
        MediaPlayerOnPreparedListener,
        OnTaskCompleteListener{

    protected final Context context;
    protected MediaPlayerControl player = new DefaultMediaPlayerControl();
    protected int layout = R.layout.sounwave_view;
    private SoundVisualizerBarView visualizerBar;
    private TextView timer;
    private ImageView actionButton;
    private ProgressBar progressLoader;
    private Activity activity;

    public MediaPlayerView(Context context, Activity activity) {
        super(context);
        this.context = context;
        this.activity = activity;
        init(context);
    }

    public MediaPlayerView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        this.context = context;

        init(context);
    }

    public MediaPlayerView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        this.context = context;

        init(context);
    }

    public void addAudioFileUri(final Uri audioFileUri) throws IOException {
        player.setAudioSource(context, audioFileUri);
        if (activity != null){
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
        if (activity != null){
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
        if (activity != null){
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

    public MediaPlayerControl getPlayer (){
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
        actionButton.setOnClickListener(runner -> player.toggle());
    }



    public void inflateView (int id) throws IOException {
        try {
            LinearLayout linearLayout = MainActivity.getInstance().findViewById(id);
            linearLayout.addView(this);
        } catch (Exception e) {

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
        player.stop();
    }

    @Override
    public void onComplete(MediaPlayerControl player) {
        if (activity!=null) {
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    actionButton.setImageDrawable(ContextCompat.getDrawable(context, R.drawable.ic_play));
                    visualizerBar.updatePlayerPercent(0);
                    timer.setText("00.00");
                }
            });
        }
    }

    @Override
    public void onDurationProgress(MediaPlayerControl player, Long duration, Long currentTimestamp) {
        if (activity != null){
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    visualizerBar.updatePlayerPercent(currentTimestamp / (float) duration);
                    timer.setText(getTime(duration - currentTimestamp));
                }
            });
        }
    }

    @Override
    public void onPause(MediaPlayerControl player) {
        if (activity != null){
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    actionButton.setImageDrawable(ContextCompat.getDrawable(context, R.drawable.ic_play));
                }
            });
        }
    }

    @Override
    public void onPlay(MediaPlayerControl player) {
        if (activity != null){
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    actionButton.setImageDrawable(ContextCompat.getDrawable(context, R.drawable.ic_pause));
                }
            });
        }
    }

    @Override
    public void onPrepared(MediaPlayerControl player) {
        if (activity != null){
            activity.runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    timer.setText(getTime(player.getDuration()));
                    actionButton.setClickable(true);
                }
            });
        }
    }
    public String getTime (long milliseconds) {
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
