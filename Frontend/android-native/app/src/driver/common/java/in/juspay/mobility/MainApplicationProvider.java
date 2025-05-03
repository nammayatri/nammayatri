package in.juspay.mobility;

import com.facebook.soloader.SoLoader;
import com.google.android.play.core.splitcompat.SplitCompatApplication;

//import in.juspay.hypersdk.core.JavascriptEngine;

public class MainApplicationProvider extends SplitCompatApplication {


    @Override
    public void onCreate() {
        super.onCreate();
        SoLoader.init(this,false);
//        JavascriptEngine.createIsolate(this);
    }
}
