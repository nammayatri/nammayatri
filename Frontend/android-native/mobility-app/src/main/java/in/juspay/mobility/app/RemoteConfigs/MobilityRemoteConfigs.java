/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app.RemoteConfigs;

import android.util.Log;

import androidx.annotation.NonNull;

import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.Task;
import com.google.firebase.remoteconfig.ConfigUpdate;
import com.google.firebase.remoteconfig.ConfigUpdateListener;
import com.google.firebase.remoteconfig.FirebaseRemoteConfig;
import com.google.firebase.remoteconfig.FirebaseRemoteConfigException;
import com.google.firebase.remoteconfig.FirebaseRemoteConfigSettings;

import in.juspay.mobility.app.R;

public class MobilityRemoteConfigs {

    private String LOG_TAG = "MobilityRemoteConfigs";

    private FirebaseRemoteConfig mFirebaseRemoteConfig;
    private FirebaseRemoteConfigSettings configSettings;

    public MobilityRemoteConfigs() {
        mFirebaseRemoteConfig = FirebaseRemoteConfig.getInstance();
        configSettings = new FirebaseRemoteConfigSettings.Builder()
                .build();
        mFirebaseRemoteConfig.setDefaultsAsync(R.xml.remote_config_defaults);
        fetchRemoteConfigs();
        realTimeConfigListner(); // TODO:: Remove before release
        Log.d(LOG_TAG, "CONSTRUCTOR");
    }

    public boolean getBoolean(String key) {
        return mFirebaseRemoteConfig.getBoolean(key);
    }

    public double getDouble(String key) {
        return mFirebaseRemoteConfig.getDouble(key);
    }

    public long getLong(String key) {
        return mFirebaseRemoteConfig.getLong(key);
    }

    public String getString(String key) {
        return mFirebaseRemoteConfig.getString(key);
    }

    public void fetchRemoteConfigs() {
        mFirebaseRemoteConfig.setConfigSettingsAsync(configSettings);
        mFirebaseRemoteConfig
                .fetchAndActivate()
                .addOnCompleteListener(task -> {
                    if (task.isSuccessful()) {
                        boolean updated = task.getResult();
                        Log.d(LOG_TAG, "Config params updated: " + updated);
                    } else {
                        Log.d(LOG_TAG, "Config params failed: " + task.getException());
                    }
                });
    }

    public void realTimeConfigListner() {
        mFirebaseRemoteConfig.addOnConfigUpdateListener(new ConfigUpdateListener() {
            @Override
            public void onUpdate(ConfigUpdate configUpdate) {
                Log.d(LOG_TAG, "Updated keys: " + configUpdate.getUpdatedKeys());

                mFirebaseRemoteConfig.activate().addOnCompleteListener(new OnCompleteListener() {
                    @Override
                    public void onComplete(@NonNull Task task) {
                        Log.d(LOG_TAG, "Config params activate " + task);
//                        displayWelcomeMessage();
                    }
                });
            }
            @Override
            public void onError(FirebaseRemoteConfigException error) {
                Log.w(LOG_TAG, "Config update error with code: " + error.getCode(), error);
            }
        });
    }


}
