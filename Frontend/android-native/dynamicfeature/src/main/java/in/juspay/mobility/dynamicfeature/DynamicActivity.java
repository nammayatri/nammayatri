package in.juspay.mobility.dynamicfeature;

import android.app.Application;
import android.content.Intent;
import android.os.Bundle;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.annotation.Nullable;

import com.facebook.react.PackageList;
import com.facebook.react.ReactNativeHost;
import com.facebook.react.ReactPackage;
import com.facebook.react.defaults.DefaultReactNativeHost;
import com.finternet.sdk.Callback;
import com.finternet.sdk.GullakCore;
import com.finternet.sdk.GullakSDKResponse;
import com.finternet.sdk.MyEventPackage;
import com.google.android.play.core.splitcompat.SplitCompat;
import com.google.firebase.FirebaseApp;
import com.google.gson.Gson;
import com.google.firebase.crashlytics.FirebaseCrashlytics;

import java.util.List;

import in.juspay.mobility.BuildConfig;

public class DynamicActivity extends AppCompatActivity {
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        SplitCompat.installActivity(this);
        setContentView(R.layout.activity_resource);
        try{
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
        }catch(Exception e){
            Toast.makeText(this, "Error occurred", Toast.LENGTH_SHORT).show();
            FirebaseCrashlytics.getInstance().recordException(e);
            finish();
        }
    }

    public static ReactNativeHost createReactNativeHost(Application app) {
        return new DefaultReactNativeHost(app) {
            @Override
            public boolean getUseDeveloperSupport() {
                return BuildConfig.DEBUG;
            }


            @Override
            protected List<ReactPackage> getPackages() {
                @SuppressWarnings("UnnecessaryLocalVariable")
                List<ReactPackage> packages = new PackageList(this).getPackages();
                packages.add(new MyEventPackage());
                return packages;
            }


            @Override
            protected String getJSMainModuleName() {
                return "index";
            }


            @Override
            protected boolean isNewArchEnabled() {
                return false;
            }


            @Override
            protected Boolean isHermesEnabled() {
                return false;
            }
        };
    }

}