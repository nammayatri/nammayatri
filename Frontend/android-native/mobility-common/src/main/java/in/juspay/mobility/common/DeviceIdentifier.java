/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.common;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.media.MediaDrm;
import android.media.UnsupportedSchemeException;
import android.os.Build;
import android.provider.Settings;
import android.util.Log;
import java.util.UUID;
public class DeviceIdentifier {

    private static final String TAG = "DeviceIdentifier";
    private static final String WIDEVINE_UUID_STRING = "edef8ba9-79d6-4ace-a3c8-27dcd51d21ed";
    private static final String DEVICE_ID = "DEVICE_ID";

    public String getDeviceId(Context context) {
        String mediaDRM = getMediaDRM();
        if(isValid(mediaDRM)) return mediaDRM + '$' + getDeviceModel();
        String androidId = getAndroidId(context);
        if(isValid(androidId)) return androidId + '$' + "ANDROID_ID";
        return getFallbackUUID(context);
    }

    public static boolean isValid(String id) {
        return  id != null && !id.isEmpty() && !id.equals("null") && !id.equals("__failed") && !id.equals("(null)");
    }

    private String getFallbackUUID(Context context) {
        try {
            SharedPreferences sharedPreferences = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
            String uuid = sharedPreferences.getString(context.getString(R.string.preference_file_key), null);
            if(!isValid(uuid)) {
                uuid = UUID.randomUUID().toString();
                SharedPreferences.Editor editor = sharedPreferences.edit();
                editor.putString(DEVICE_ID, uuid);
                editor.apply();
            }
            return uuid + '$' + "RANDOM_UUID";
        } catch (Exception e) {
            return "NO_DEVICE_ID";
        }
    }
    @SuppressLint("HardwareIds")
    public static String getAndroidId(Context context) {
        if(context != null) return Settings.Secure.getString(context.getContentResolver(), Settings.Secure.ANDROID_ID);
        else return null;
    }

    private String getMediaDRM() {
        String deviceId = null;
        UUID widevineUUID = UUID.fromString(WIDEVINE_UUID_STRING);

        MediaDrm mediaDrm = null;
        try {
            mediaDrm = new MediaDrm(widevineUUID);
            byte[] deviceUniqueId = mediaDrm.getPropertyByteArray(MediaDrm.PROPERTY_DEVICE_UNIQUE_ID);
            deviceId = bytesToHex(deviceUniqueId);
        } catch (UnsupportedSchemeException e) {
            Log.e(TAG, "Unsupported DRM scheme", e);
        } catch (Exception e) {
            Log.e(TAG, "Failed to get DRM ID", e);
        } finally {
            if (mediaDrm != null) {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
                    mediaDrm.close();
                } else {
                    mediaDrm.release();
                }
            }
        }
        return deviceId;
    }

    private String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    private String getDeviceModel() {
        String model = Build.MODEL;
        return model != null ? model.replaceAll(" " ,"") : "UnknownModel";
    }
}
