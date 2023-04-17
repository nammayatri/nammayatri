package in.juspay.mobility.utils;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.media.RingtoneManager;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.provider.Settings;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.Task;
import com.google.firebase.FirebaseApp;
import com.google.firebase.auth.AuthResult;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentChange;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.ListenerRegistration;
import com.google.firebase.firestore.Query;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import in.juspay.hypersdk.core.DuiCallback;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;

public class ChatService extends Service {
    private static Context context;
    private ListenerRegistration chatListener;
    final int chatNotificationId = 18012023;
    final int serviceNotificationID = 19012023;
    private static boolean isChatServiceRunning = false;
    private static SharedPreferences sharedPrefs;
    public static FirebaseFirestore firestoreInstance;
    public static FirebaseAuth firebaseAuth;
    public static DuiCallback chatDynamicUI;
    private static final String LOG_TAG = "mobility_ChatService";
    public static String chatChannelID;
    public static String chatUserId;
    public static String storeCallBackMessage;
    public static boolean sessionCreated = false;
    public static boolean shouldNotify = true;
    private static ArrayList<Message> messages = new ArrayList<>();
    private Handler handler = new Handler();
    private String merchant = null;
    @Override
    public void onCreate() {
        super.onCreate();
        context = getApplicationContext();
        firestoreInstance = FirebaseFirestore.getInstance();
        firebaseAuth = FirebaseAuth.getInstance();
        merchant = getApplicationContext().getResources().getString(R.string.service);
        sharedPrefs = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        if(sharedPrefs != null) chatChannelID = sharedPrefs.getString("CHAT_CHANNEL_ID", "");
        isSessionCreated();
        if (!isChatServiceRunning){
            this.startForeground(serviceNotificationID, createNotification());
            FirebaseUser user = firebaseAuth.getCurrentUser();
            if(user != null) {
                startChatService();
            } else {
                signInAnonymously();
            }
        }

    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        handleMessages(messages);
        return  START_STICKY;
    }

    private void isSessionCreated() {
        try{
            firestoreInstance.collection("Chats").document(chatChannelID).get().addOnCompleteListener(
                    (task -> {
                        if(task.isSuccessful()){
                            if(task.getResult().exists()){
                                sessionCreated = true;
                            }
                        }
                    })
            );
        } catch (Exception error){
            Log.e(LOG_TAG,"Error in isSessionCreated :: " + error);
        }

   }

   private void startChatService() {
       handler.postDelayed(() -> addChatListener(), 1000);
   }

    private void signInAnonymously(){
        try {
            firebaseAuth.signInAnonymously()
                    .addOnCompleteListener(new OnCompleteListener<AuthResult>() {
                        @Override
                        public void onComplete(@NonNull Task<AuthResult> task) {
                            if (task.isSuccessful()) {
                                startChatService();
                                Log.d(LOG_TAG, "signInAnonymously:success");
                            } else {
                                Log.w(LOG_TAG, "signInAnonymously:failure", task.getException());
                            }
                        }
                    });
        } catch (Exception e){
            Log.e(LOG_TAG,"Error in signInAnonymously" + e);
        }
    }

    private void handleMessages(ArrayList<Message> messages) {
        int count = 1;
        isChatServiceRunning = false;
        int messageSize = messages.size();
        for (Message message: messages) {
            shouldNotify = true;
            if(count == messageSize && !isChatServiceRunning){
                isChatServiceRunning = true;
                if(count == messageSize && count != 1){
                    shouldNotify = false;
                }
            }
            count++;
            handleMessage(message);
        }
    }

    private void addChatListener() {
        if(chatListener != null) return;
        try {
            if (FirebaseApp.getApps(context).size() == 0) {
                FirebaseApp.initializeApp(context);
            }
        }
        catch (Exception e){
            Log.e(LOG_TAG,"Exception in Firebase Initialization" + e);
        }
        try {
            if(firestoreInstance != null && chatChannelID != null){
                chatListener = firestoreInstance.collection("Chats").document(chatChannelID).collection("messages").orderBy("timestamp", Query.Direction.ASCENDING).addSnapshotListener(
                        (snapshots, e) -> {
                            if (e != null) {
                                Log.e(LOG_TAG, "listen:error", e);
                                return;
                            }
                            int count = 1;
                            if(snapshots == null) return;
                            for (DocumentChange dc : snapshots.getDocumentChanges()) {
                                switch (dc.getType()) {
                                    case ADDED:
                                        Map<String, Object> newMessageMap = dc.getDocument().getData();
                                        Message newMessage = createMessageObj(newMessageMap);
                                        messages.add(newMessage);
                                        shouldNotify = true;
                                        if(count == (snapshots.getDocumentChanges()).size() && !isChatServiceRunning){
                                            isChatServiceRunning = true;
                                            if(count == (snapshots.getDocumentChanges()).size() && count != 1){
                                                shouldNotify = false;
                                            }
                                        }
                                        count++;
                                        handleMessage(newMessage);
                                        break;
                                    case MODIFIED:
                                        break;
                                    case REMOVED:
                                        break;
                                }
                            }
                        }
                );
            }else{
                Log.d(LOG_TAG,"Document path cannot be empty, chatChannelId is empty");
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in listening to messages");
        }

    }

    public static void sendMessage(final String message) {
        HashMap<String,Object> _newMessage = new HashMap<>();
        _newMessage.put("message", message);
        _newMessage.put("sentBy", chatUserId);
        _newMessage.put("timestamp",System.currentTimeMillis());
        if(sharedPrefs !=  null) chatChannelID = sharedPrefs.getString("CHAT_CHANNEL_ID", null);
        if(chatChannelID != null){
            try {
                if (!sessionCreated){
                    HashMap<String, Object> _timestamp = new HashMap<>();
                    _timestamp.put("timestamp", System.currentTimeMillis());
                    firestoreInstance.collection("Chats").document(chatChannelID).set(_timestamp).addOnCompleteListener(
                            (res) -> {
                                if(res.isSuccessful()){
                                    sessionCreated = true;
                                }
                            }
                    );
                }
                firestoreInstance.collection("Chats").document(chatChannelID).collection("messages").add(_newMessage);
            } catch (Exception e){
                Log.e(LOG_TAG, "Error in sending a message" + e);
            }
        }else{
            Log.d(LOG_TAG,"Document path cannot be empty, chatChannelId is empty");
        }

    }

    private void handleMessage(Message newMessage){
        String _dateFormatted = newMessage.timestamp;
        String _message = newMessage.message;
        String _sentBy = newMessage.sentBy;
        String appState = null;
        if(sharedPrefs != null) appState = sharedPrefs.getString("ACTIVITY_STATUS", "null");
        String sentBy = "";
        if (_sentBy.equals("Driver")) sentBy = "nammayatripartner";
        else sentBy = "nammayatri";
        if(appState.equals("onDestroy") || appState.equals("onPause")){
            if(!(merchant.equals(sentBy)) && isChatServiceRunning && shouldNotify){
                if(merchant.equals("nammayatripartner")) startWidgetService(_message, _sentBy);
                else createChatNotification(_sentBy, _message);
            }
        }
        if(appState.equals("onPause") || appState.equals("onResume")){
            try{
                String javascript = String.format("window.callUICallback(\"%s\",\"%s\",\"%s\",\"%s\");", storeCallBackMessage, _message, _sentBy, _dateFormatted);
                if(chatDynamicUI != null){
                    chatDynamicUI.addJsToWebView(javascript);
                }
            } catch (Exception err) {
                Log.e(LOG_TAG,"Error sending the message to jbridge : " + err);
            }
        }
    }

    private void startWidgetService(String widgetMessage, String sentBy){
        Intent widgetService = new Intent(getApplicationContext(), WidgetService.class);
        if (merchant.equals(getString(R.string.nammayatripartner)) && Settings.canDrawOverlays(getApplicationContext())  && !sharedPrefs.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPrefs.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPrefs.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            widgetService.putExtra(getResources().getString(R.string.WIDGET_MESSAGE),widgetMessage);
            widgetService.putExtra("sentBy",(sentBy + " :-"));
            try{
                startService(widgetService);
            }catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private Notification createNotification() {
        createNotificationChannel();
        Intent notificationIntent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 10, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        String contentText;
        if(merchant.equals("nammayatripartner")){
            contentText = getString(R.string.you_can_now_chat_with_customer);
        } else {
            contentText = getString(R.string.you_can_now_chat_with_driver);
        }
        NotificationCompat.Builder notification =
                new NotificationCompat.Builder(this, "Message")
                        .setContentTitle(getString(R.string.chatting_is_enabled))
                        .setContentText(contentText)
                        .setSmallIcon(R.drawable.ny_ic_launcher)
                        .setPriority(NotificationCompat.PRIORITY_MIN)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent);
        return notification.build();
    }

    private void createChatNotification(String sentBy, String message) {
        createChatNotificationChannel();
        Intent notificationIntent = new Intent(this, MainActivity.class);
        notificationIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP|Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, chatNotificationId, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        Notification notification =
                new NotificationCompat.Builder(this, "MessageUpdates")
                        .setContentTitle(sentBy)
                        .setAutoCancel(true)
                        .setContentText(message)
                        .setSmallIcon(R.drawable.ny_ic_launcher)
                        .setDefaults(Notification.DEFAULT_ALL)
                        .setPriority(NotificationCompat.PRIORITY_HIGH)
                        .setContentIntent(pendingIntent)
                        .setSound(RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION))
                        .build();

        NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
        notificationManager.notify(chatNotificationId, notification);
        return;
    }
    private void createChatNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = "MessageUpdates" ;
            String description = "Chat Notification Channel";
            NotificationChannel channel = new NotificationChannel("MessageUpdates", name, NotificationManager.IMPORTANCE_HIGH);
            channel.setDescription(description);
            NotificationManager notificationManager = getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }
    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = "Message" ;
            String description = "Service Notification Channel";
            NotificationChannel channel = new NotificationChannel("Message", name, NotificationManager.IMPORTANCE_MIN);
            channel.setDescription(description);
            NotificationManager notificationManager = getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }
    private String getChatDate(Long time){
        Date date;
        try {
            date = new Date(time);
        } catch (Exception error){
            date = new Date((Long) System.currentTimeMillis());
        }
        /*   add this to date format if date is needed ---> dd MMM yyyy   */
        DateFormat dateFormat = new SimpleDateFormat("h:mm a", Locale.ENGLISH);
        String dateFormatted = dateFormat.format(date);
        return dateFormatted;
    }

    class Message {
        public Message(String message, String sentBy, String timestamp) {
            this.message = message;
            this.sentBy = sentBy;
            this.timestamp = timestamp;
        }
        String message;
        String sentBy;
        String timestamp;
    }

    private Message createMessageObj(Map<String, Object> map){
        String timestamp = getChatDate((Long) map.get("timestamp"));
        String message = (String) map.get("message");
        String sentBy = (String) map.get("sentBy");
        Message messageObj = new Message(message, sentBy, timestamp);
        return messageObj;
    }
    
    @Override
    public void onDestroy() {
        NotificationManager notificationManager = (NotificationManager)getSystemService(Context.NOTIFICATION_SERVICE);
        notificationManager.cancel(chatNotificationId);
        isChatServiceRunning = false;
        shouldNotify = true;
        stopForeground(true);
        if(chatListener != null) chatListener.remove();
        messages.clear();
        stopSelf();
        super.onDestroy();
    }
    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}
