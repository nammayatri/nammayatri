package in.juspay.hypersdk.services;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONException;
import org.json.JSONObject;

import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.JuspayCoreLib;
import in.juspay.hypersdk.core.JuspayServices;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.core.SdkTracker;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.utils.IntegrationUtils;

/**
 * Service to provide sdk config to any class in the sdk
 * This class, manages reading the config
 * Caching config, to be supplied in cases where fileProviderService is not yet up
 * Supply latest config to all classes, so that config, is present only in one point in memory
 * This is currently designed to be a pull based model
 */
public class SdkConfigService {
    private static final String sdkConfigLocation = "sdk_config.json";

    @NonNull
    private final JuspayServices juspayServices;
    @NonNull
    private JSONObject sdkConfig;

    public SdkConfigService(@NonNull JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
        try {
            sdkConfig = new JSONObject(KeyValueStore.read(juspayServices, sdkConfigLocation, "{}"));
        } catch (JSONException e) {
            sdkConfig = new JSONObject();
            juspayServices.getSdkTracker().trackException(LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, PaymentConstants.SDK_CONFIG, "Exception while parsing sdk config", e);
        }
    }

    /**
     * Function used to update config from file.
     * This is also responsible to cache the latest config to local-store
     */
    public void renewConfig(@NonNull Context context) {
        try {
            String rawConfig = juspayServices.getFileProviderService().readFromFile(context, sdkConfigLocation);
            sdkConfig = new JSONObject(rawConfig);
            KeyValueStore.write(juspayServices, sdkConfigLocation, rawConfig);
        } catch (JSONException e) {
            juspayServices.getSdkTracker().trackException(LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, PaymentConstants.SDK_CONFIG, "Exception while parsing renewed sdk config", e);
        }
    }

    /**
     * Function to get latest sdk config.
     *
     * @return the sdk configuration
     */
    @NonNull
    public JSONObject getSdkConfig() {
        return sdkConfig;
    }

    /**
     * Function to get sdk config from local storage
     * This is to be used only in cases when caller has no way to get access to a Juspay Services object
     * In any other case please use the member function to get sdkConfig
     */
    @Nullable
    public static JSONObject getCachedSdkConfig() {
        try {
            if (JuspayCoreLib.getApplicationContext() != null) {
                return new JSONObject(KeyValueStore.read(JuspayCoreLib.getApplicationContext(), IntegrationUtils.getSdkInfo(JuspayCoreLib.getApplicationContext()).getSdkName(), sdkConfigLocation, "{}"));
            }
        } catch (JSONException e) {
            SdkTracker.trackBootException(LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, PaymentConstants.SDK_CONFIG, "Exception while parsing cached sdk config", e);
        }
        return null;
    }
}
