/*
 * Copyright 2022-23, Juspay
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 * is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package in.juspay.mobility.utils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.widget.Toast;

import in.juspay.mobility.MainActivity;

public class MainActivityService extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        Intent notificatinStart = new Intent(context, MainActivity.class);
        notificatinStart.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        System.out.println("It is in MainActivityService");
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            context.startActivity(notificatinStart);
        }else
        {
            Toast.makeText(context, "Service restarted else", Toast.LENGTH_SHORT).show();
            context.startActivity(notificatinStart);
        }
    }
}
