package in.juspay.beckn.utils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;

import in.juspay.beckn.CommonJsInterface;
import in.juspay.beckn.MainActivity;

public class NetworkBroadcastReceiver extends BroadcastReceiver {
    Context context;

    @Override
    public void onReceive(Context context, Intent intent) {
        this.context = context;
        if (context!=null){
            if(!isNetworkAvailable()){
                MainActivity.getInstance().triggerPopUPMain("true","INTERNET_ACTION");
            }else {
                try {
                    CommonJsInterface.callingStoreCallBackInternetAction(MainActivity.getInstance().getJuspayServices().getDynamicUI(), "true");
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
