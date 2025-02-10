package in.juspay.mobility;

import android.app.Application;

import com.facebook.react.ReactApplication;
import com.facebook.react.ReactNativeHost;
import com.google.android.play.core.splitcompat.SplitCompatApplication;

import java.lang.reflect.Method;
public class MainApplicationProvider extends SplitCompatApplication implements ReactApplication {

    @Override
    public ReactNativeHost getReactNativeHost() {
        try {
            Class<?> clazz = Class.forName("in.juspay.mobility.dynamicfeature.DynamicActivity");
            Class<?>[] paramTypes = { Application.class };
            Method method = clazz.getMethod("createReactNativeHost", paramTypes);
            Object[] args = { this };
            return (ReactNativeHost) method.invoke(null, this);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }


    @Override
    public void onCreate() {
        super.onCreate();
        System.out.println("Hyper Service " + MobilityServiceHolder.getInstance(this).getHyperService());
        System.out.println("Hyper Service isInitialised " + MobilityServiceHolder.getInstance(this).getHyperService().isInitialised());
        MobilityServiceHolder.getInstance(this).initiate(this);
    }
}
