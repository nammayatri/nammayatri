package in.juspay.mobility.dynamicfeature;

import android.content.Intent;
import android.os.Bundle;
import androidx.appcompat.app.AppCompatActivity;
import androidx.annotation.Nullable;
import com.finternet.sdk.Callback;
import com.finternet.sdk.GullakCore;
import com.finternet.sdk.GullakSDKResponse;
import com.google.firebase.FirebaseApp;
import com.google.gson.Gson;

public class DynamicActivity extends AppCompatActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_resource);

        Intent intent = getIntent();
        String repeatUserLoginToken = intent.getStringExtra("token");
        FirebaseApp.getApps(this);


        String response = GullakCore.startGullak(DynamicActivity.this, repeatUserLoginToken, getApplicationContext().getPackageName(), new Callback() {
            @Override
            public void onResponse(GullakSDKResponse s) {
                Gson gson = new Gson();
                String responseJson = gson.toJson(s);
                Intent resultIntent = new Intent();
                resultIntent.putExtra("responseJson", responseJson);
                if (intent.getStringExtra("cbIdentifier")!=null) resultIntent.putExtra("cbIdentifier", intent.getStringExtra("cbIdentifier"));
                setResult(RESULT_OK, resultIntent);
                finish();
            }
        });
    }
}