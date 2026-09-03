package in.juspay.mobility;

import static android.content.Context.MODE_PRIVATE;
import static in.juspay.mobility.MainActivity.getService;
import static in.juspay.mobility.Utils.getInnerPayload;
import in.juspay.mobility.app.R;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.util.Pair;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebViewClient;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.util.LinkedList;
import java.util.Queue;
import java.util.UUID;

import in.juspay.hypersdk.core.MerchantViewType;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.data.KeyValueStore;
import in.juspay.hypersdk.ui.HyperPaymentsCallback;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;

public class MobilityServiceHolder {
    private static MobilityServiceHolder instance;
    private static JSONObject initiatePayload = new JSONObject();
    private static long initiateTime = 0;
    private MobilityServices hyperService;
    private final Queue<Pair<JSONObject, JuspayResponseHandler>> queue = new LinkedList<>();
    @Nullable
    private HyperPaymentsCallbackAdapter adapter = null;

    MobilityServiceHolder(Context context) {
        hyperService = new MobilityServices(context);
    }

    public static MobilityServiceHolder getInstance(Context context) {
        if (instance == null || instance.hyperService == null) {
            Log.i("APP_PERF", "ON_CREATE_HYPER_SERVICE : " + System.currentTimeMillis());
            instance = new MobilityServiceHolder(context);
            // Backward Compatibility
            boolean isUpdated = Boolean.parseBoolean(KeyValueStore.read(context,context.getString(in.juspay.mobility.app.R.string.preference_file_key),"isUpdated","false"));
            if (!isUpdated) {
                try {
                    File deleteFile = new File(context.getDir("juspay",MODE_PRIVATE), "v1-assets_downloader_godel_2.1.33.jsa");
                    if (deleteFile.exists()) deleteFile.delete();
                    deleteFile = new File(context.getDir("juspay",MODE_PRIVATE), "v1-index_bundle_godel_2.1.33.jsa");
                    if (deleteFile.exists()) deleteFile.delete();
                    KeyValueStore.write(context,context.getString(in.juspay.mobility.app.R.string.preference_file_key),"isUpdated","true");
                } catch (Exception e) {
                    Log.e("MobilityServiceHolder",e.toString());
                }
            }
            Log.i("APP_PERF", "ON_CREATE_HYPER_END : " + System.currentTimeMillis());
        }
        return instance;
    }

    public static JSONObject getInitiatePayload() {
        return initiatePayload;
    }

    public boolean isInitialized() {
        return hyperService.isInitialised();
    }

    public long getInitiateTime() {
        return initiateTime;
    }

    public void initiate(Context context) {
        if (!hyperService.isInitialised()) {
            initiatePayload = new JSONObject();
            JSONObject payload = new JSONObject();

            try {
                initiatePayload.put("requestId", UUID.randomUUID());
                initiatePayload.put("service", getService());
                initiatePayload.put("betaAssets", false);
                getInnerPayload(payload, "initiate", context);
                initiatePayload.put(PaymentConstants.PAYLOAD, payload);
            } catch (JSONException e) {
                e.printStackTrace();
            }
            Log.i("APP_PERF", "ON_INITIATE_START : " + System.currentTimeMillis());
            initiateTime = System.currentTimeMillis();
            hyperService.initiate(initiatePayload, new HyperPaymentsCallback() {
                @Override
                public void onStartWaitingDialogCreated(@Nullable View view) {
                    if (adapter != null) {
                        adapter.onStartWaitingDialogCreated(view);
                    }
                }

                @Override
                public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                    Log.i("APP_PERF", "ON_INITIATE_END : " + System.currentTimeMillis());
                    Log.i("OnEvent", "OnEvent : " + jsonObject);
                    if (adapter == null) {
                        queue.add(new Pair<>(jsonObject, juspayResponseHandler));
                    } else {
                        adapter.onEvent(jsonObject, juspayResponseHandler);
                    }
                }

                @Nullable
                @Override
                public View getMerchantView(ViewGroup viewGroup, MerchantViewType merchantViewType) {
                    return null;
                }

                @Nullable
                @Override
                public WebViewClient createJuspaySafeWebViewClient() {
                    return null;
                }
            });
        }
    }

    public void process(FragmentActivity activity, ViewGroup view, JSONObject jsonObject) {
        if (hyperService.isInitialised()) {
            hyperService.process(activity, view, jsonObject);
        }
    }

    public void process(JSONObject jsonObject) {
        if (hyperService.isInitialised()) {
            hyperService.process(jsonObject);
        }
    }

    public void setCallbackAdapter(HyperPaymentsCallbackAdapter adapter) {
        if (this.adapter == null) {
            while (!queue.isEmpty()) {
                Pair<JSONObject, JuspayResponseHandler> pair = queue.poll();
                if (pair != null) adapter.onEvent(pair.first, pair.second);
            }
        }
        this.adapter = adapter;
    }

    public void terminate() {
        adapter = null;
        hyperService.terminate();
        instance = null;
        hyperService = null;
    }

    public boolean bindToService(Context context, Activity activity) {
        if (MobilityServiceHolder.canCacheApp(context)) {
            hyperService.resetActivity((FragmentActivity) activity);
            terminate();
            MobilityServiceHolder.getInstance(context).initiate(context);
            return true;
        } else {
            terminate();
            return false;
        }
    }

    public static void leaveMeAlone() {
        instance = null;
    }

    public static void catchMeBack(MobilityServiceHolder instance) {
        MobilityServiceHolder.instance = instance;
        MobilityServiceHolder.instance.queue.clear();
    }

    public static boolean canCacheApp(Context context) {
        String s = KeyValueStore.read(context, context.getString(R.string.preference_file_key), "APP_CACHING_CONFIG", "{}");
        boolean isCachingEnabled = false;
        try {
            JSONObject config = new JSONObject(s);
            isCachingEnabled = config.optBoolean(KeyValueStore.read(context, context.getString(R.string.preference_file_key), "DRIVER_LOCATION", "").toLowerCase(),false);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return isCachingEnabled;
    }

    public void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        hyperService.onActivityResult(requestCode, resultCode, data);
    }

    public boolean onBackPressed() {
        return hyperService.onBackPressed();
    }

    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        hyperService.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }


}
