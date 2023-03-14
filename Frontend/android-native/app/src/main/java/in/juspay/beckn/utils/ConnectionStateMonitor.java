package in.juspay.beckn.utils;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.ConnectivityManager.NetworkCallback;
import android.net.Network;
import android.net.NetworkCapabilities;
import android.net.NetworkRequest;
import android.util.Log;

public class ConnectionStateMonitor extends NetworkCallback {
    private final NetworkRequest networkRequest;
    private final CallBack onAvailable;
    private final CallBack onLost;
    private String LOG_TAG = "Network";

    public interface CallBack {
        void runCallback();
    }

    public ConnectionStateMonitor(CallBack onAvailable, CallBack onLost) {
        networkRequest = new NetworkRequest.Builder().addTransportType(NetworkCapabilities.TRANSPORT_CELLULAR).addTransportType(NetworkCapabilities.TRANSPORT_WIFI).build();
        this.onAvailable = onAvailable;
        this.onLost = onLost;
    }

    public void enable(Context context) {
        ConnectivityManager connectivityManager = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        connectivityManager.registerNetworkCallback(networkRequest , this);
    }

    public void disable(Context context) {
        ConnectivityManager connectivityManager = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        connectivityManager.unregisterNetworkCallback(this);
    }

    @Override
    public void onAvailable(Network network) {
        super.onAvailable(network);
        if(onAvailable != null)
            onAvailable.runCallback();
        Log.i(LOG_TAG,"onAvailable");
    }

    @Override
    public void onLost(Network network) {
        super.onLost(network);
        if(onLost != null)
            onLost.runCallback();
        Log.i(LOG_TAG,"onLost");
    }
}
