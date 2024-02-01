package in.juspay.mobility.app;

import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.viewpager2.widget.ViewPager2;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.mobility.app.reels.ExoplayerItem;
import in.juspay.mobility.app.reels.ReelController;
import in.juspay.mobility.app.reels.ReelViewAdapter;
import in.juspay.mobility.app.reels.ReelViewPagerItem;

public class ReelsPlayerView extends AppCompatActivity {

   private ReelController reelController;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        getWindow().getDecorView().setSystemUiVisibility( View.SYSTEM_UI_FLAG_HIDE_NAVIGATION | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY);
        Bundle bundle = getIntent().getExtras();
        setContentView(R.layout.reels_player_view);

       reelController = new ReelController(this);

        if (bundle != null) {
                String jsonData = bundle.getString("reelsJSONData");
                int index = bundle.getInt("index");
                String callback = bundle.getString("callback");
                reelController.initializeReelsView(jsonData, index, callback );
        }
    }


    @Override
    protected void onDestroy() {
        Log.i("NEW_ACTIVITY", "destroyed");
        reelController.stopAndReleaseExoplayers();
        super.onDestroy();
    }

    @Override
    protected void onPause() {
        reelController.pauseExoplayers(false);
        super.onPause();
    }

    @Override
    protected void onResume() {
        reelController.resumeExoplayer();
        super.onResume();
    }

}


