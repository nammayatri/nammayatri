package in.juspay.mobility.app.reels;


import android.widget.ProgressBar;

import androidx.media3.exoplayer.ExoPlayer;

import com.google.android.material.progressindicator.LinearProgressIndicator;

public class ExoplayerItem {
    public ExoPlayer exoPlayer;
    public int position;

    LinearProgressIndicator horizontalProgressBar;

    public ExoplayerItem(int position, ExoPlayer exoplayer, LinearProgressIndicator horizontalProgressBar){
        this.exoPlayer = exoplayer;
        this.position = position;
        this.horizontalProgressBar = horizontalProgressBar;
    }

}
