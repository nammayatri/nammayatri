package in.juspay.hypersdk.core;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;

import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import in.juspay.hyper.core.JuspayLogger;

public class CustomtabResult extends Activity {

    public static final String CUSTOMTAB_RESULT = "customtab-result";
    private static final String LOG_TAG = "CustomtabResult";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Intent data = new Intent(CUSTOMTAB_RESULT);

        try {
            data.putExtra("response", getIntent().getDataString());
        } catch (Exception e) {
            JuspayLogger.e(LOG_TAG, "Couldn't find data from url", e);
        }

        data.putExtra("status", "SUCCESS");
        LocalBroadcastManager.getInstance(this).sendBroadcast(data);
        startActivity(new Intent(this, CustomtabActivity.class));
        finish();
    }
}
