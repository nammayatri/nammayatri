/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.content.Context;
import android.os.Bundle;

import com.clevertap.android.sdk.CleverTapAPI;
import com.clevertap.android.sdk.Logger;
import com.xiaomi.mipush.sdk.MiPushCommandMessage;
import com.xiaomi.mipush.sdk.MiPushMessage;
import com.xiaomi.mipush.sdk.PushMessageReceiver;
import android.text.TextUtils;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Iterator;

public class XiaomiMessageReceiver extends PushMessageReceiver {
    private static final String LOG_TAG = "CleverTapXiaomi";

    @Override
    public void onReceivePassThroughMessage(Context context, MiPushMessage miPushMessage) {

        Logger.d(LOG_TAG,"onReceivePassThroughMessage is called");
        try {
            String ctData = miPushMessage.getContent();
            Bundle extras = stringToBundle(ctData);
            CleverTapAPI.createNotification(context,extras);

        } catch (JSONException e) {
            e.printStackTrace();
        }
    }


    @Override
    public void onNotificationMessageArrived(Context context, MiPushMessage miPushMessage) {
        super.onNotificationMessageArrived(context, miPushMessage);
        Logger.d(LOG_TAG,"onNotificationMessageArrived is called");
    }

    @Override
    public void onReceiveRegisterResult(Context context, MiPushCommandMessage miPushCommandMessage) {
        super.onReceiveRegisterResult(context, miPushCommandMessage);
        Logger.d(LOG_TAG,"onReceiveRegisterResult is called");
    }

    public static Bundle stringToBundle (String content) throws JSONException {

        Bundle bundle = new Bundle();

        if (!TextUtils.isEmpty(content)) {
            JSONObject jsonObject = new JSONObject(content);
            Iterator iter = jsonObject.keys();
            while (iter.hasNext()) {
                String key = (String) iter.next();
                String value = jsonObject.getString(key);
                bundle.putString(key, value);
            }
        }

        return bundle;
    }
}
