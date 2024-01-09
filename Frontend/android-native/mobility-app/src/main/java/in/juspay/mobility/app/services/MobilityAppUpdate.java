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
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.gms.tasks.Task;
import com.google.android.material.snackbar.Snackbar;
import com.google.android.play.core.appupdate.AppUpdateInfo;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.appupdate.AppUpdateManagerFactory;
import com.google.android.play.core.install.InstallStateUpdatedListener;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.InstallStatus;
import com.google.android.play.core.install.model.UpdateAvailability;
import com.google.j2objc.annotations.Weak;

import java.lang.ref.WeakReference;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.mobility.app.R;
import in.juspay.mobility.app.RemoteConfigs.MobilityRemoteConfigs;

public class MobilityAppUpdate {

    @NonNull
    private final AppUpdateManager appUpdateManager;
    @Nullable
    private WeakReference<BridgeComponents> bridgeComponents;
    private int updateType;


    private static final int REQUEST_CODE_UPDATE_APP = 587;
    private final String LOG_TAG = MobilityAppUpdate.class.getSimpleName();

    Context context;

    public MobilityAppUpdate(Context context) {
        this.context =context;
        appUpdateManager = AppUpdateManagerFactory.create(context);
    }

    public void checkAndUpdateApp(MobilityRemoteConfigs remoteConfigs, String callback, boolean autoStart) {
        // Returns an intent object that you use to check for an update.
        Task<AppUpdateInfo> appUpdateInfoTask = appUpdateManager.getAppUpdateInfo();

        if(remoteConfigs.hasKey("force_update")) {
            updateType = remoteConfigs.getBoolean("force_update") ? AppUpdateType.IMMEDIATE : AppUpdateType.FLEXIBLE;
        }else{
            updateType = AppUpdateType.FLEXIBLE;
        }
        InstallStateUpdatedListener listener;
        if (autoStart) {
            listener = state -> {
                if (state.installStatus() == InstallStatus.DOWNLOADED) {
                    popupSnackbarForCompleteUpdate();
                }
            };
            appUpdateManager.registerListener(listener);
        }
        InstallStateUpdatedListener finalListener = listener;
        appUpdateInfoTask.addOnSuccessListener(appUpdateInfo -> {
            if (appUpdateInfo.installStatus() == InstallStatus.DOWNLOADED) {
                if (finalListener != null) {
                    appUpdateManager.unregisterListener(finalListener);
                    appUpdateManager.completeUpdate();
                }
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
                            (Activity) this.context,
                            // Include a request code to later monitor this update request.
                            REQUEST_CODE_UPDATE_APP
                    );
                } catch (IntentSender.SendIntentException e) {
                    e.printStackTrace();
                }
                Log.d(LOG_TAG, "Update available");
            } else {
                appUpdateManager.unregisterListener(finalListener);
                Log.d(LOG_TAG, "No Update available");
            }
        });
    }

    private void popupSnackbarForCompleteUpdate() {
        try{
            Snackbar snackbar =
                    Snackbar.make(
                            ((Activity) context).findViewById(R.id.content),
                            "An update has just been downloaded.",
                            Snackbar.LENGTH_INDEFINITE);
            snackbar.setAction("RESTART", view -> appUpdateManager.completeUpdate());
            snackbar.show();
        }catch (Exception e){
            e.printStackTrace();
        }
    }

    public void isUpdateAvailable(MobilityRemoteConfigs remoteConfigs, BridgeComponents bridgeComponents, String callBack) {
        this.bridgeComponents = new WeakReference<>(bridgeComponents);

    }
}
