package in.juspay.mobility.sdk.core;

import android.content.Context;

import androidx.annotation.NonNull;

import org.json.JSONObject;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.hyper.core.JuspayCoreLib;
import in.juspay.mobility.sdk.R;
import in.juspay.mobility.sdk.utils.network.NetUtils;

public class PrefetchServices {
    private static final String LOG_TAG = "PrefetchServices";

    public static void preFetch(@NonNull final Context context, @NonNull final JSONObject bundle) {
        JuspayCoreLib.setApplicationContext(context.getApplicationContext());
        NetUtils.setApplicationHeaders(context);
        try {
            bundle.put("pre_fetch", "true");
            bundle.put("use_local_assets", bundle.optBoolean("useLocalAssets", context.getResources().getBoolean(R.bool.use_local_assets)));
            JuspayServices juspayServices = new JuspayServices(context, null);
            juspayServices.setPrefetch(true);
            juspayServices.setBundleParameter(bundle);

            ExecutorManager.runOnMainThread(() -> juspayServices.initiate(() -> {}));
        } catch (Exception e) {
            SdkTracker.trackAndLogBootException(LOG_TAG, LogCategory.LIFECYCLE, LogSubCategory.LifeCycle.HYPER_SDK, Labels.HyperSdk.PREFETCH, "Exception happened in PREFETCH", e);
        }
    }
}
