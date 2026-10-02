package in.juspay.mobility.app.reels;



import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.SeekBar;

import androidx.media3.exoplayer.ExoPlayer;
import com.google.android.material.progressindicator.LinearProgressIndicator;
import org.json.JSONObject;

public class ExoplayerItem {
    public ExoPlayer exoPlayer;
    public int position;
    Boolean isStartThresholdCrossed = false, isEndThresholdCrossed = false, scrollViewExpanded;
    ImageView reelPauseButton ;

    JSONObject reelThresholdConfig;

    LinearLayout reelInfoView;

    SeekBar reelSeekBar;

    public ExoplayerItem(int position, ExoPlayer exoplayer, SeekBar reelSeekBar, ImageView reelPauseButton, JSONObject reelThresholdConfig, Boolean scrollViewExpanded, LinearLayout reelInfoView){
        this.exoPlayer = exoplayer;
        this.position = position;
        this.reelThresholdConfig = reelThresholdConfig;
        this.reelPauseButton = reelPauseButton;
        this.scrollViewExpanded = scrollViewExpanded;
        this.reelInfoView = reelInfoView;
        this.reelSeekBar = reelSeekBar;
    }

}
