/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import com.huawei.hms.push.RemoteMessage;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.annotation.Nullable;
import com.clevertap.android.sdk.CleverTapAPI;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Iterator;

public class HmsMessageService extends com.huawei.hms.push.HmsMessageService {

    @Override
    public void onNewToken(@Nullable String token, @Nullable Bundle bundle) {
        CleverTapAPI cleverTapAPIInstance = CleverTapAPI.getDefaultInstance(getApplicationContext());
        super.onNewToken(token, bundle);
        if(token!=null){
            cleverTapAPIInstance.pushHuaweiRegistrationId(token, true);
        }
    }

    @Override
    public void onMessageReceived(RemoteMessage remoteMessage) {
        try {
            String ctData = remoteMessage.getData();
            Bundle extras = stringToBundle(ctData);
            CleverTapAPI.createNotification(getApplicationContext(),extras);
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onMessageSent(String msgId) {
        // Handle message sent confirmation
    }

    @Override
    public void onSendError(String msgId, Exception e) {
        // Handle message send error
    }


    public static Bundle stringToBundle (String content) throws JSONException {

        Bundle bundle = new Bundle();

        if (!TextUtils.isEmpty(content)) {
            JSONObject jsonObject = new JSONObject(content);
            Iterator iter = jsonObject.keys();
            while (((Iterator<?>) iter).hasNext()) {
                String key = (String) iter.next();
                String value = jsonObject.getString(key);
                bundle.putString(key, value);
            }
        }

        return bundle;
    }
}
