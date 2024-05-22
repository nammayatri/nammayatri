/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app.services;

import android.app.Activity;
import android.content.Context;
import android.content.IntentSender;
import android.graphics.Color;
import android.util.Log;
import java.util.Locale;

import com.google.android.gms.tasks.Task;
import com.google.android.material.snackbar.Snackbar;
import com.google.android.play.core.appupdate.AppUpdateInfo;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.appupdate.AppUpdateManagerFactory;
import com.google.android.play.core.install.InstallStateUpdatedListener;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.InstallStatus;
import com.google.android.play.core.install.model.UpdateAvailability;

import javax.annotation.Nullable;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;

public class MobilityAppUpdate {

    private final AppUpdateManager appUpdateManager;
    private static MobilityAppUpdate mobilityAppUpdate;
    private static int updateType;
    @Nullable
    private AppUpdateInfo updateInfo;
    private static final int REQUEST_CODE_UPDATE_APP = 587;
    private static final String LOG_TAG = "MobilityAppUpdate";

    private MobilityAppUpdate(Context context) {
        appUpdateManager = AppUpdateManagerFactory.create(context);
    }

    public static MobilityAppUpdate getInstance(Context context) {
        if (mobilityAppUpdate == null) {
            mobilityAppUpdate = new MobilityAppUpdate(context);
        }
        return mobilityAppUpdate;
    }

    public void checkAndUpdateApp(MobilityRemoteConfigs remoteConfigs, Context context) {
        // Returns an intent object that you use to check for an update.
        Task<AppUpdateInfo> appUpdateInfoTask = appUpdateManager.getAppUpdateInfo();

        if(remoteConfigs.hasKey("force_update")) {
            updateType = remoteConfigs.getBoolean("force_update") ? AppUpdateType.IMMEDIATE : AppUpdateType.FLEXIBLE;
        }else{
            updateType = AppUpdateType.FLEXIBLE;
        }

        InstallStateUpdatedListener listener = state -> {
            if (state.installStatus() == InstallStatus.DOWNLOADED) {
                popupSnackbarForCompleteUpdate(context);
            }
        };
        appUpdateManager.registerListener(listener);

        appUpdateInfoTask.addOnSuccessListener(appUpdateInfo -> {
            updateInfo = appUpdateInfo;
            if (appUpdateInfo.installStatus() == InstallStatus.DOWNLOADED) {
                appUpdateManager.unregisterListener(listener);
                appUpdateManager.completeUpdate();
                return;
            }
            if (appUpdateInfo.updateAvailability() == UpdateAvailability.UPDATE_AVAILABLE
                    && appUpdateInfo.isUpdateTypeAllowed(updateType)) {
                Log.d(LOG_TAG, "Inside update");
                try {
                    appUpdateManager.startUpdateFlowForResult(
                            // Pass the intent that is returned by 'getAppUpdateInfo()'.
                            appUpdateInfo,
                            // Or 'AppUpdateType.FLEXIBLE' for flexible updates.
                            updateType,
                            // The current activity making the update request.
                            (Activity) context,
                            // Include a request code to later monitor this update request.
                            REQUEST_CODE_UPDATE_APP
                    );
                } catch (IntentSender.SendIntentException e) {
                    Log.e("Error in updating app",e.toString());
                }
                Log.d(LOG_TAG, "Update available");
            } else {
                appUpdateManager.unregisterListener(listener);
                Log.d(LOG_TAG, "No Update available");
            }
        });
    }

    private void popupSnackbarForCompleteUpdate(Context context) {
        try{
            Snackbar snackbar =
                    Snackbar.make(
                            ((Activity) context).findViewById(android.R.id.content),
                            "An update has just been downloaded.",
                            Snackbar.LENGTH_INDEFINITE);
            snackbar.setAction("RESTART", view -> appUpdateManager.completeUpdate());
            snackbar.setActionTextColor(Color.parseColor("#FCC32C"));
            snackbar.show();
        }catch (Exception e){
            Log.e("Exception in snack bar",e.toString());
        }
    }

    public boolean checkForUpdates() {
        if (updateInfo != null) {
            return updateInfo.updateAvailability() == UpdateAvailability.UPDATE_AVAILABLE;
        } else {
            return false;
        }
    }

    public void startFlexibleUpdate(BridgeComponents bridgeComponents, String callback) {
        Context context = bridgeComponents.getContext();
        //Returns an intent object
        Task<AppUpdateInfo> appUpdateInfoTask = appUpdateManager.getAppUpdateInfo();

        InstallStateUpdatedListener listener = state -> {
            if (state.installStatus() == InstallStatus.DOWNLOADED) {
                popupSnackbarForCompleteUpdate(context);
            }
        };

        appUpdateManager.registerListener(listener);

        appUpdateInfoTask.addOnSuccessListener(appUpdateInfo -> {
            if (appUpdateInfo.installStatus() == InstallStatus.DOWNLOADED) {
                appUpdateManager.unregisterListener(listener);
                appUpdateManager.completeUpdate();
            }
             Log.d(LOG_TAG, "inside update");
            try {
                appUpdateManager.startUpdateFlowForResult(
                        // Pass the intent that is returned by 'getAppUpdateInfo()'.
                        appUpdateInfo,
                        // Or 'AppUpdateType.FLEXIBLE' for flexible updates.
                        AppUpdateType.FLEXIBLE,
                        // The current activity making the update request.
                        (Activity) context,
                        // Include a request code to later monitor this update request.
                        REQUEST_CODE_UPDATE_APP
                );
                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    callback, "Success");
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
            }
            catch (IntentSender.SendIntentException e) {
                Log.e(LOG_TAG, "Error starting update", e);
                String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    callback, "Failure");
                    bridgeComponents.getJsCallback().addJsToWebView(javascript);
            }
        });
    }
}
