package in.juspay.mobility.utils;

import static in.juspay.mobility.utils.NotificationUtils.startMediaPlayer;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.provider.Settings;
import android.util.Log;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import com.google.firebase.FirebaseApp;
import com.google.firebase.auth.FirebaseAuth;
import com.google.firebase.auth.FirebaseUser;
import com.google.firebase.firestore.DocumentChange;
import com.google.firebase.firestore.FirebaseFirestore;
import com.google.firebase.firestore.ListenerRegistration;
import com.google.firebase.firestore.Query;
import org.json.JSONObject;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import javax.net.ssl.HttpsURLConnection;
import in.juspay.hypersdk.core.DuiCallback;
import in.juspay.mobility.CommonJsInterface;
import in.juspay.mobility.MainActivity;
import in.juspay.mobility.R;
import in.juspay.mobility.BuildConfig;

public class ChatService extends Service {
    private static Context context;
    private ListenerRegistration chatListener;
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
    private static final ArrayList<Message> messages = new ArrayList<>();
    private final Handler handler = new Handler();
    private static String merchant = null;
    private static String baseUrl;
    static Random random = new Random();
    String merchantType = BuildConfig.MERCHANT_TYPE;
    final MessageOverlay messageOverlay = new MessageOverlay();
    @Override
    public void onCreate() {
        super.onCreate();
        context = getApplicationContext();
        firestoreInstance = FirebaseFirestore.getInstance();
        firebaseAuth = FirebaseAuth.getInstance();
        merchant = getApplicationContext().getResources().getString(R.string.service);
        sharedPrefs = this.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        if(sharedPrefs != null) {
            chatChannelID = sharedPrefs.getString("CHAT_CHANNEL_ID", "");
            baseUrl = sharedPrefs.getString("BASE_URL", "null");
        }
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
        handler.postDelayed(this::handleMessages, 1000);
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
                    .addOnCompleteListener(task -> {
                        if (task.isSuccessful()) {
                            startChatService();
                            Log.d(LOG_TAG, "signInAnonymously:success");
                        } else {
                            Log.w(LOG_TAG, "signInAnonymously:failure", task.getException());
                        }
                    });
        } catch (Exception e){
            Log.e(LOG_TAG,"Error in signInAnonymously" + e);
        }
    }

    private void handleMessages() {
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
            handleMessage(message, String.valueOf(messageSize - 1));
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
                                        handleMessage(newMessage, String.valueOf(snapshots.size() - 1));
                                        break;
                                    case MODIFIED:
                                        break;
                                    case REMOVED:
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
                if(BuildConfig.MERCHANT_TYPE.equals("DRIVER")) sendFCM(message);
            } catch (Exception e){
                Log.e(LOG_TAG, "Error in sending a message" + e);
            }
        }else{
            Log.d(LOG_TAG,"Document path cannot be empty, chatChannelId is empty");
        }
    }

    private void handleMessage(Message newMessage, String len) {
        try  {
            String _dateFormatted = newMessage.timestamp;
            String _message = newMessage.message;
            String _sentBy = newMessage.sentBy;
            String appState = null;
            String stage = "HomeScreen";
            if(sharedPrefs != null) appState = sharedPrefs.getString("ACTIVITY_STATUS", "null");
            if(sharedPrefs != null) stage = sharedPrefs.getString("LOCAL_STAGE", "null");
            String sentBy;
            if (_sentBy.equals("Driver")) {
                String appName = getApplicationContext().getResources().getString(R.string.app_name);
                if (appName.equals("Yatri Partner"))
                    sentBy = "yatripartner";
                else if(appName.equals("Yatri Sathi Driver"))
                    sentBy = "jatrisaathidriver";
                else
                    sentBy = "nammayatripartner";
            } else {
                String appName = getApplicationContext().getResources().getString(R.string.app_name);
                if (appName.equals("Yatri"))
                    sentBy = "yatri";
                else if(appName.equals("Yatri Sathi"))
                    sentBy = "jatrisaathi";
                else
                    sentBy = "nammayatri";
            }
            if(appState.equals("onPause") || appState.equals("onResume")){
                try{
                    String javascript = String.format("window.callUICallback(\"%s\",\"%s\",\"%s\",\"%s\",\"%s\");", storeCallBackMessage, _message, _sentBy, _dateFormatted, len);
                    if(chatDynamicUI != null){
                        chatDynamicUI.addJsToWebView(javascript);
                    }
                } catch (Exception err) {
                    Log.e(LOG_TAG,"Error sending the message to jbridge : " + err);
                }
            }
            String message =  getMessageFromKey(_message);
            if(!(merchant.equals(sentBy)) && isChatServiceRunning && shouldNotify && message != "" && merchantType.equals("DRIVER")){
                if(appState.equals("onDestroy") || appState.equals("onPause")){
                    if(NotificationUtils.overlayFeatureNotAvailable(context)) NotificationUtils.createChatNotification(_sentBy, message, context);
                    else startOverlayService(message, _dateFormatted);
                }else if (appState.equals("onResume") && !(stage.equals("ChatWithCustomer"))) {
                    String notificationId = String.valueOf(random.nextInt(1000000));
                    MainActivity.showInAppNotification(_sentBy, message, CommonJsInterface.storeCallBackOpenChatScreen,"", "", "", "", notificationId, 5000, context);
                }
            }
        } catch (Exception e) {
            Log.e(LOG_TAG,"Error in handleMessage : " + e);
        }
    }

    private void startOverlayService(String message, String timestamp){
        if ((merchantType.equals("DRIVER")) && Settings.canDrawOverlays(getApplicationContext())  && !sharedPrefs.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPrefs.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPrefs.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            try{
                messageOverlay.showMessageOverlay(message, timestamp, context);
                startMediaPlayer(context, R.raw.new_message, false);
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
        if(merchantType.equals("DRIVER")){
            contentText = getString(R.string.you_can_now_chat_with_customer);
        } else {
            contentText = getString(R.string.you_can_now_chat_with_driver);
        }
        NotificationCompat.Builder notification =
                new NotificationCompat.Builder(this, "Message")
                        .setContentTitle(getString(R.string.chatting_is_enabled))
                        .setContentText(contentText)
                        .setSmallIcon(R.drawable.ic_launcher)
                        .setPriority(NotificationCompat.PRIORITY_MIN)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent);
        return notification.build();
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
            date = new Date(System.currentTimeMillis());
        }
        /*   add this to date format if date is needed ---> dd MMM yyyy   */
        DateFormat dateFormat = new SimpleDateFormat("h:mm a", Locale.ENGLISH);
        return dateFormat.format(date);
    }

    static class Message {
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
        return new Message(message, sentBy, timestamp);
    }

    private static void sendFCM(String message) {
        try {
            String url = baseUrl + "/onMessage";
            StringBuilder result = new StringBuilder();
            String regToken = sharedPrefs.getString("REGISTERATION_TOKEN", "null");
            String bundle_version = sharedPrefs.getString("BUNDLE_VERSION", "null");
            String version = sharedPrefs.getString("VERSION_NAME", "null");
            String sessionToken = sharedPrefs.getString("SESSION_ID", "null");
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            if (connection instanceof HttpsURLConnection)
                ((HttpsURLConnection) connection).setSSLSocketFactory(new TLSSocketFactory());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("x-client-version", version);
            connection.setRequestProperty("token", regToken);
            connection.setRequestProperty("x-bundle-version", bundle_version);
            connection.setRequestProperty("session_id", sessionToken);
            connection.setConnectTimeout(20000);
            connection.setReadTimeout(20000);
            connection.setDoOutput(true);

            JSONObject payload = new JSONObject();
            payload.put("message", message);
            payload.put("rideId", chatChannelID);

            OutputStream stream = connection.getOutputStream();
            stream.write(payload.toString().getBytes());
            connection.connect();
            int respCode = connection.getResponseCode();
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                Log.e(LOG_TAG,"Error in calling send FCM API" + result);
            }
        } catch (Exception e){
            Log.e(LOG_TAG,"Error in sending FCM" + e);
        }
    }

    public static String getMessageFromKey(String key) {
        try {
            String suggestionsStr = sharedPrefs.getString("SUGGESTIONS_DEFINITIONS", "null");
            JSONObject suggestions = new JSONObject(suggestionsStr);
            String language = sharedPrefs.getString("LANGUAGE_KEY", "null");
            if(suggestions.has(key)) {
                JSONObject message = suggestions.getJSONObject(key);
                if(message.has(language.toLowerCase())) return message.getString(language.toLowerCase());
                else return message.getString("en_us");
            } else {
                return key;
            }
        } catch (Exception e) {
            Log.e(LOG_TAG,"Error in getMessageFromKey : " + e);
            return "";
        }
    }

    @Override
    public void onDestroy() {
        isChatServiceRunning = false;
        shouldNotify = true;
        sessionCreated = false;
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
