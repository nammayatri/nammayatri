package in.juspay.mobility;

import android.util.Log;

import com.facebook.soloader.SoLoader;
import com.google.android.play.core.splitcompat.SplitCompatApplication;

import in.juspay.mobility.sdk.core.QuickJSRuntime;

//import in.juspay.hypersdk.core.JavascriptEngine;

public class MainApplicationProvider extends SplitCompatApplication {


    @Override
    public void onCreate() {
        super.onCreate();
        Log.e("TEST", "onCreate -> " + System.currentTimeMillis());
        QuickJSRuntime.initiateRuntime(new QuickJSRuntime.QuickJSRuntimeCallback() {
            @Override
            public void onDone() {
            }
        });
//        SoLoader.init(this,false);
//        JavascriptEngine.createIsolate(this);
    }
}
