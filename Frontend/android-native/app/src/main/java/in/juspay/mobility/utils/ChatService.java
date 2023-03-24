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
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

import com.google.firebase.FirebaseApp;
import com.google.firebase.firestore.DocumentChange;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.ListenerRegistration;
import com.google.firebase.firestore.Query;

import org.json.JSONException;
import org.json.JSONObject;

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
    final int chatNotificationId = 19012023;
    private static boolean isChatServiceRunning = false;
    private static SharedPreferences sharedPrefs;
    public static FirebaseFirestore firestoreInstance;
    public static DuiCallback chatDynamicUI;
    private static final String LOG_TAG = "Beckn_ChatService";
    public static String chatChannelID;
    public static String chatUserId;
    public static String storeCallBackMessage;
    private static ArrayList<Message> messages = new ArrayList<>();
    private Handler handler = new Handler();
    @Override
    public void onCreate() {
        super.onCreate();
        context = getApplicationContext();
        firestoreInstance = FirebaseFirestore.getInstance();
        sharedPrefs = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        if (!isChatServiceRunning){
            this.startForeground(chatNotificationId, createNotification());
            isChatServiceRunning = true;
            handler.postDelayed(()-> addChatListener(),1000);
        }
    }
    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        for (Message message: messages) {
            handleMessage(message);
        }
        return  START_STICKY;
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
        chatChannelID = sharedPrefs.getString("CHAT_CHANNEL_ID", null);
        if(firestoreInstance != null && chatChannelID != null){
            chatListener = firestoreInstance.collection("Chats").document(chatChannelID).collection("messages").orderBy("timestamp", Query.Direction.ASCENDING).addSnapshotListener(
                    (snapshots, e) -> {
                        if (e != null) {
                            Log.e(LOG_TAG, "listen:error", e);
                            return;
                        }
                        for (DocumentChange dc : snapshots.getDocumentChanges()) {
                            switch (dc.getType()) {
                                case ADDED:
                                    Map<String, Object> newMessageMap = dc.getDocument().getData();
                                    Message newMessage = createMessageObj(newMessageMap);
                                    messages.add(newMessage);
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
    }

    public static void sendMessage(final String message) {
        HashMap<String,Object> _newMessage = new HashMap<>();
        _newMessage.put("message", message);
        _newMessage.put("sentBy", chatUserId);
        _newMessage.put("timestamp", System.currentTimeMillis());
        chatChannelID = sharedPrefs.getString("CHAT_CHANNEL_ID", null);
        if(chatChannelID != null){
            firestoreInstance.collection("Chats").document(chatChannelID).collection("messages").add(_newMessage);
        }else{
            Log.d(LOG_TAG,"Document path cannot be empty, chatChannelId is empty");
        }

    }

    private void handleMessage(Message newMessage){
        String _dateFormatted = newMessage.timestamp;
        String _message = newMessage.message;
        String _sentBy = newMessage.sentBy;
        String appState = sharedPrefs.getString("ACTIVITY_STATUS", "null");
        String sentBy = "";
        if (_sentBy.equals("Driver")) sentBy = "nammayatripartner";
        else sentBy = "nammayatri";
        if(appState.equals("onDestroy") || appState.equals("onPause")){
            if(!((context.getResources().getString(R.string.service)).equals(sentBy))){
                if(context.getResources().getString(R.string.service).equals("nammayatripartner")) startWidgetService(_message, _sentBy);
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
                Log.e(LOG_TAG,"Error sending the message to jbridge" + err);
            }
        }
    }

    private void startWidgetService(String widgetMessage, String sentBy){
        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        Intent widgetService = new Intent(getApplicationContext(), WidgetService.class);
        if (getResources().getString(R.string.service).equals(getString(R.string.nammayatripartner)) && Settings.canDrawOverlays(getApplicationContext())  && !sharedPref.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPref.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
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
        if(context.getResources().getString(R.string.service).equals("nammayatripartner")){
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
        JSONObject payload = new JSONObject();
        try{
            payload.put("notification_type", "CHAT_MESSAGE");
        } catch (JSONException e) {
            Log.e(LOG_TAG,"Error in adding data to jsonObject");
        }
        notificationIntent.putExtra("NOTIFICATION_DATA", payload.toString());
        notificationIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP|Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 18012023, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
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
        notificationManager.notify(18012023, notification);
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
        String message;
        String sentBy;
        String timestamp;
    }

    private Message createMessageObj(Map<String, Object> map){
        Message message = new Message();
        String timestamp = getChatDate((Long) map.get("timestamp"));
        message.message = (String) map.get("message");
        message.sentBy = (String) map.get("sentBy");
        message.timestamp = timestamp;
        return message;
    }
    
    @Override
    public void onDestroy() {
        super.onDestroy();
        NotificationManager notificationManager = (NotificationManager)getSystemService(Context.NOTIFICATION_SERVICE);
        notificationManager.cancel(18012023);
        isChatServiceRunning = false;
        stopForeground(true);
        if(chatListener != null) chatListener.remove();
        messages.clear();
        stopSelf();
    }
    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}

/*   Required for future usage  */

//    private void getUserFCM() {
//        fcmListener = firestoreInstance.collection("DeviceInfo").document(chatChannelID).addSnapshotListener(
//                (snapshot,err) -> {
//                    if (err != null) {
//                        Log.w(TAG, "listen:error", err);
//                        return;
//                    }
//                    if (snapshot != null && snapshot.exists()) {
//                        userFCM = (String) snapshot.getData().get("fcm_token");
//                    } else {
//                        Log.d(TAG, "Current data: null");
//                    }
//                }
//        );
//    }

/*   Required for future usage  */

//    private static void sendFCM(String message) throws IOException, NoSuchAlgorithmException, KeyManagementException, JSONException {
//        String url = "https://fcm.googleapis.com/fcm/send";
//        HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
//        if (connection instanceof HttpsURLConnection)
//            ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
//        connection.setRequestMethod("POST");
//        connection.setRequestProperty("Content-Type", "application/json");
//        connection.setRequestProperty("Authorization", "key=");
//        connection.setDoOutput(true);
//
//        JSONObject notificationJson = new JSONObject();
//        notificationJson.put("to", userFCM);
//        notificationJson.put("priority", "high");
//
//        JSONObject notification = new JSONObject();
//        notification.put("title", "Driver");
//        notification.put("body",message);
//
//        JSONObject data = new JSONObject();
//        data.put("notification_type", "NEW_MESSAGE");
//
//        notificationJson.put("notification",notification);
//        notificationJson.put("data",data);
//
//        OutputStream stream = connection.getOutputStream();
//        stream.write(notificationJson.toString().getBytes());
//        connection.connect();
//        int respCode = connection.getResponseCode();
//        InputStreamReader respReader;
//
//        if ((respCode < 200 || respCode >= 300) && respCode != 302) {
//            respReader = new InputStreamReader(connection.getErrorStream());
//            System.out.print("Error sending FCM" + respReader);
//        } else {
//            respReader = new InputStreamReader(connection.getInputStream());
//            System.out.print("Success" + respReader);
//        }
//    }