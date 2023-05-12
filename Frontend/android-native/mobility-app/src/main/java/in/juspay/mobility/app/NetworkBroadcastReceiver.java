/*
 * Copyright 2022-23, Juspay
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 * is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 * the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package in.juspay.mobility.app;


import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;

import java.util.ArrayList;

public class NetworkBroadcastReceiver extends BroadcastReceiver {
    Context context;
    private static final ArrayList<CallBack> callBack = new ArrayList<>();
    private static final ArrayList<ProcessCallBack> processCallBack = new ArrayList<>();
    public static void registerCallback(CallBack notificationCallback)
    {
        callBack.add(notificationCallback);
    }
    public static void deRegisterCallback(CallBack notificationCallback)
    {
        callBack.remove(notificationCallback);
    }
    public static void registerProcessCallback(ProcessCallBack notificationCallback)
    {
        processCallBack.add(notificationCallback);
    }
    public static void deRegisterProcessCallback(ProcessCallBack notificationCallback)
    {
        processCallBack.remove(notificationCallback);
    }

    public interface ProcessCallBack{
        void callProcess(String flag,String type);
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        this.context = context;
        if (context!=null){
            if(!isNetworkAvailable()){
                for(int i = 0; i< processCallBack.size(); i++) {
                    processCallBack.get(i).callProcess("true","INTERNET_ACTION");
                }
            }else {
                try {
                    for(int i = 0; i< callBack.size(); i++) {
                        callBack.get(i).internetCallBack("true");
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
    }

    private boolean isNetworkAvailable() {
        ConnectivityManager connectivityManager
                = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo activeNetworkInfo = connectivityManager != null ? connectivityManager.getActiveNetworkInfo() : null;
        return activeNetworkInfo != null && activeNetworkInfo.isConnected();
    }
}
