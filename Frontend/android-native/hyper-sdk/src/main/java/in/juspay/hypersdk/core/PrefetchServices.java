package in.juspay.hypersdk.core;

import android.content.Context;

import androidx.annotation.NonNull;

import org.json.JSONObject;

import in.juspay.hyper.constants.Labels;
import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayCoreLib;
import in.juspay.hypersdk.R;
import in.juspay.hypersdk.utils.network.NetUtils;

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
