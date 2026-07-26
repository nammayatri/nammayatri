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

import com.google.android.gms.tasks.Task;
import com.google.android.material.snackbar.Snackbar;
import com.google.android.play.core.appupdate.AppUpdateInfo;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.appupdate.AppUpdateManagerFactory;
import com.google.android.play.core.install.InstallStateUpdatedListener;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.InstallStatus;
import com.google.android.play.core.install.model.UpdateAvailability;

import java.lang.ref.WeakReference;

import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;

public class MobilityAppUpdate {

    private final AppUpdateManager appUpdateManager;
    private static int updateType;


    private static final int REQUEST_CODE_UPDATE_APP = 587;
    private final String LOG_TAG = "MobilityAppUpdate";

    WeakReference<Activity> activity;

    public MobilityAppUpdate(Activity activity) {
        this.activity = new WeakReference<>(activity);
        appUpdateManager = AppUpdateManagerFactory.create(activity.getApplicationContext());
    }

    public void checkAndUpdateApp(MobilityRemoteConfigs remoteConfigs) {
        // Returns an intent object that you use to check for an update.
        Task<AppUpdateInfo> appUpdateInfoTask = appUpdateManager.getAppUpdateInfo();

        if(remoteConfigs.hasKey("force_update")) {
            updateType = remoteConfigs.getBoolean("force_update") ? AppUpdateType.IMMEDIATE : AppUpdateType.FLEXIBLE;
        }else{
            updateType = AppUpdateType.FLEXIBLE;
        }

        InstallStateUpdatedListener listener = state -> {
            if (state.installStatus() == InstallStatus.DOWNLOADED) {
                popupSnackbarForCompleteUpdate();
            }
        };
        appUpdateManager.registerListener(listener);

        appUpdateInfoTask.addOnSuccessListener(appUpdateInfo -> {
            if (appUpdateInfo.installStatus() == InstallStatus.DOWNLOADED) {
                appUpdateManager.unregisterListener(listener);
                appUpdateManager.completeUpdate();
                return;
            }
            if (appUpdateInfo.updateAvailability() == UpdateAvailability.UPDATE_AVAILABLE
                    && appUpdateInfo.isUpdateTypeAllowed(updateType) && this.activity.get() != null) {
                Log.d(LOG_TAG, "Inside update");
                try {
                    appUpdateManager.startUpdateFlowForResult(
                            // Pass the intent that is returned by 'getAppUpdateInfo()'.
                            appUpdateInfo,
                            // Or 'AppUpdateType.FLEXIBLE' for flexible updates.
                            updateType,
                            // The current activity making the update request.
                            this.activity.get(),
                            // Include a request code to later monitor this update request.
                            REQUEST_CODE_UPDATE_APP
                    );
                } catch (IntentSender.SendIntentException e) {
                    e.printStackTrace();
                }
                Log.d(LOG_TAG, "Update available");
            } else {
                appUpdateManager.unregisterListener(listener);
                Log.d(LOG_TAG, "No Update available");
            }
        });
    }

    private void popupSnackbarForCompleteUpdate() {
        if (this.activity.get() != null) {
            try{
                Snackbar snackbar =
                        Snackbar.make(
                                (this.activity.get()).findViewById(android.R.id.content),
                                "An update has just been downloaded.",
                                Snackbar.LENGTH_INDEFINITE);
                snackbar.setAction("RESTART", view -> appUpdateManager.completeUpdate());
                snackbar.setActionTextColor(Color.parseColor("#FCC32C"));
                snackbar.show();
            }catch (Exception e){
                e.printStackTrace();
            }
        }
    }
}
