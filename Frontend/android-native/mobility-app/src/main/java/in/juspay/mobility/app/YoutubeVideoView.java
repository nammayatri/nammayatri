package in.juspay.mobility.app;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;

import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;


public class YoutubeVideoView extends AppCompatActivity {
    float duration;
    YouTubePlayerView youTubePlayerView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bundle newBundle = getIntent().getExtras();
        supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);
        if (newBundle != null && newBundle.getString("videoType").equals("VIDEO")) {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        } else {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        }

        setContentView(R.layout.youtube_video_view);

        youTubePlayerView = findViewById(R.id.idVideoView);
        getLifecycle().addObserver(youTubePlayerView);
        youTubePlayerView.enterFullScreen();

        youTubePlayerView.addYouTubePlayerListener(new AbstractYouTubePlayerListener() {
            @Override
            public void onReady(@NonNull YouTubePlayer youTubePlayer) {
                if (newBundle != null) {
                    youTubePlayer.loadVideo(newBundle.getString("videoId"), newBundle.getFloat("videoDuration"));
                }
            }

            @Override
            public void onCurrentSecond(@NonNull YouTubePlayer youTubePlayer, float second) {
                super.onCurrentSecond(youTubePlayer, second);
                duration = second;
            }

        });


    }

    @Override
    protected void onDestroy() {
        if (youTubePlayerView != null) {
            youTubePlayerView.release();
        }
        super.onDestroy();
        if(MobilityAppBridge.youtubePlayer != null) {
            MobilityAppBridge.youTubePlayerView.exitFullScreen();
            MobilityAppBridge.youtubePlayer.seekTo(duration);
            MobilityAppBridge.youtubePlayer.play();
        }
    }
}