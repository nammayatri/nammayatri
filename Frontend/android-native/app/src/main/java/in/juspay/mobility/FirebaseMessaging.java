package in.juspay.mobility;


import androidx.annotation.NonNull;

import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;

import in.juspay.mobility.app.MyFirebaseMessagingService;

public class FirebaseMessaging extends FirebaseMessagingService {
    @Override
    public void onMessageReceived(@NonNull RemoteMessage message) {
        super.onMessageReceived(message);
        MyFirebaseMessagingService.onMessageReceived(getApplicationContext(),message);
    }

    @Override
    public void onNewToken(@NonNull String token) {
        super.onNewToken(token);
        MyFirebaseMessagingService.onNewToken(getApplicationContext(), token);
    }
}
