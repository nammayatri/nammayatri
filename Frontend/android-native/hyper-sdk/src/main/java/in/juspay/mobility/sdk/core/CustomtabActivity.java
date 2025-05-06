package in.juspay.mobility.sdk.core;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
import android.os.Bundle;
import android.os.Parcelable;

import androidx.browser.customtabs.CustomTabsIntent;
import androidx.browser.customtabs.CustomTabsService;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import java.util.ArrayList;
import java.util.List;

import in.juspay.mobility.sdk.hyper.core.JuspayLogger;

/**
 * Created by kiran.puppala on 5/17/18.
 */

public class CustomtabActivity extends Activity {
    public static final String CUSTOMTAB_RESULT = "customtab-result";
    private static final String LOG_TAG = "CustomtabActivity";

    private Boolean isFirstResume = true;

    /**
     * Check chrome app is installed or not
     */
    public static boolean isChromeInstalled(ArrayList<ResolveInfo> list) {
        for (ResolveInfo info : list) {
            if (info.activityInfo.packageName.equals("com.android.chrome")) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns a list of packages that support Custom Tabs.
     */
    public static ArrayList<ResolveInfo> getCustomTabsPackages(Context context, String url) {
        PackageManager pm = context.getPackageManager();
        // Get default VIEW intent handler.
        Intent activityIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));

        // Get all apps that can handle VIEW intents.
        List<ResolveInfo> resolvedActivityList;

        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.M) {
            resolvedActivityList = pm.queryIntentActivities(activityIntent, PackageManager.MATCH_ALL);
        } else {
            resolvedActivityList = pm.queryIntentActivities(activityIntent, 0);
        }

        ArrayList<ResolveInfo> packagesSupportingCustomTabs = new ArrayList<>();

        for (ResolveInfo info : resolvedActivityList) {
            Intent serviceIntent = new Intent();
            serviceIntent.setAction(CustomTabsService.ACTION_CUSTOM_TABS_CONNECTION);
            serviceIntent.setPackage(info.activityInfo.packageName);

            // Check if this package also resolves the Custom Tabs service.
            if (pm.resolveService(serviceIntent, 0) != null) {
                packagesSupportingCustomTabs.add(info);
            }
        }

        return packagesSupportingCustomTabs;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (getIntent() != null) {
            Uri uri = getIntent().getData();

            if (uri != null && uri.getHost() != null) {
                finish();
            } else {
                final Bundle extras = getIntent().getExtras();
                final String url = extras == null ? null : extras.getString("url");
                if (url != null) {
                    loadUrl(url);
                } else {
                    finish();
                }
            }
        } else {
            finish();
        }

    }

    public void loadUrl(String url) {
        CustomTabsIntent.Builder builder = new CustomTabsIntent.Builder();
        CustomTabsIntent customTabsIntent = builder.build();
        customTabsIntent.intent.setData(Uri.parse(url));

        try {
            ArrayList<ResolveInfo> appInfo = getCustomTabsPackages(getBaseContext(), url);

            if (appInfo.size() > 0) {
                if (isChromeInstalled(appInfo)) {
                    customTabsIntent.intent.setPackage("com.android.chrome");///If chrome installed launch in chrome
                    customTabsIntent.launchUrl(CustomtabActivity.this, Uri.parse(url));
                } else {
                    launchIntentChooser(appInfo, url); //Else launch app chooser which supports cct
                }
            } else {
                launchInBrowser(url); //If no apps found for cct, launch in browser
            }
        } catch (Exception e) { //Handle all the exceptions
            JuspayLogger.e(LOG_TAG, "Exception in customtab activity", e);
        }

    }

    /**
     * Shows browser list to load url
     ***/
    private void launchInBrowser(String url) {
        Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
        startActivity(intent);
    }

    /***
     * Launch Intent Chooser with list of apps given
     */
    public void launchIntentChooser(ArrayList<ResolveInfo> list, String url) {
        List<Intent> targetedShareIntents = new ArrayList<>();

        for (ResolveInfo item : list) {
            Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
            intent.setPackage(item.activityInfo.packageName);
            targetedShareIntents.add(intent);
        }

        Intent chooserIntent = Intent.createChooser(targetedShareIntents.remove(0), "Select app");
        chooserIntent.putExtra(Intent.EXTRA_INITIAL_INTENTS, targetedShareIntents.toArray(new Parcelable[0]));
        startActivity(chooserIntent);
    }

    @Override
    public void onResume() {
        super.onResume();
        if (!isFirstResume) {
            Intent data = new Intent(CUSTOMTAB_RESULT);
            data.putExtra("status", "CANCELLED");
            LocalBroadcastManager.getInstance(this).sendBroadcast(data);
            finish();
        } else {
            isFirstResume = false;
        }
    }
}
