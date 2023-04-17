package in.juspay.mobility.utils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.util.Log;

public class ChatBroadCastReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        Intent chatListenerService = new Intent(context, ChatService.class);
        chatListenerService.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK );
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
            {
                context.startForegroundService(chatListenerService);
            }
            else
            {
                context.startService(chatListenerService);
            }
        } catch (Exception e) {
            Log.e("ChatBroadCastReceiver", "Failed to start ChatService : " + e);
        }
    }
}
