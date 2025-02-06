package in.juspay.mobility;

import android.app.Activity;
import android.content.Context;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;

import java.util.ArrayList;
import java.util.List;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.services.HyperServices;
import in.juspay.services.TenantParams;

public class MobilityServices extends HyperServices {


    public MobilityServices(@NonNull Context context) {
        super(context, new TenantParams() {
            @Override
            public String getBootLoaderEndpoint() {
                return "null";
            }

            @Nullable
            @Override
            public String getBaseContent() {
                return null;
            }

            @NonNull
            @Override
            public String getNamespace() {
                return "";
            }

            @NonNull
            @Override
            public List<Class<? extends HyperBridge>> getBridgeClasses() {
                return new ArrayList<>();
            }
        });
    }

}