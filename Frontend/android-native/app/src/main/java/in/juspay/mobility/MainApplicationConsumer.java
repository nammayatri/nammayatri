package in.juspay.mobility;

import android.app.Application;

import com.facebook.react.ReactApplication;
import com.facebook.react.ReactNativeHost;
import com.google.android.play.core.splitcompat.SplitCompatApplication;

import java.lang.reflect.Method;

public class MainApplicationConsumer extends Application {

    @Override
    public void onCreate() {
        super.onCreate();
        System.out.println("Hyper Service " + MobilityServiceHolder.getInstance(this).getHyperService());
        System.out.println("Hyper Service isInitialised " + MobilityServiceHolder.getInstance(this).getHyperService().isInitialised());
        MobilityServiceHolder.getInstance(this).initiate(this);
    }
}
