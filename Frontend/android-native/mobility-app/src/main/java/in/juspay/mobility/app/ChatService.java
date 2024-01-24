/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package in.juspay.mobility.app;

import static in.juspay.mobility.app.NotificationUtils.startMediaPlayer;
import static android.graphics.Color.rgb;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
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
import java.util.TimeZone;

import javax.net.ssl.HttpsURLConnection;

import in.juspay.mobility.app.callbacks.CallBack;
import in.juspay.mobility.app.callbacks.ShowNotificationCallBack;

public class ChatService extends Service {
    private Context context;
    private static final ArrayList<CallBack> callBack = new ArrayList<>();
    private static final ArrayList<ShowNotificationCallBack> inAppCallBacks = new ArrayList<>();
    private ListenerRegistration chatListener;
    final int serviceNotificationID = 19012023;
    private static boolean isChatServiceRunning = false;
    private SharedPreferences sharedPrefs;
    public FirebaseFirestore firestoreInstance;
    public FirebaseAuth firebaseAuth;
    private static final String LOG_TAG = "mobility_ChatService";
    public static String chatChannelID;
    public static String chatUserId;
    public boolean sessionCreated = false;
    public boolean shouldNotify = true;
    private final ArrayList<Message> messages = new ArrayList<>();
    private String merchant = null;
    private String baseUrl;
    static Random random = new Random();
    String merchantType = "";
    MobilityAppBridge.SendMessageCallBack sendMessageCallBack;
    MessageOverlayService.SendMessageCallBack sendMessageCallBackOverlay;

    public static void registerCallback(CallBack notificationCallback) {
        callBack.add(notificationCallback);
    }

    public static void deRegisterCallback(CallBack notificationCallback) {
        callBack.remove(notificationCallback);
    }

    public static void registerInAppCallback(ShowNotificationCallBack notificationCallback) {
        inAppCallBacks.add(notificationCallback);
    }

    public static void deRegisterInAppCallback(ShowNotificationCallBack notificationCallback) {
        inAppCallBacks.remove(notificationCallback);
    }


    @Override
    public void onCreate() {
        super.onCreate();
        context = getApplicationContext();
        firestoreInstance = FirebaseFirestore.getInstance();
        firebaseAuth = FirebaseAuth.getInstance();
        merchant = getApplicationContext().getResources().getString(R.string.service);
        merchantType = merchant.contains("partner") || merchant.contains("driver") || merchant.contains("provider") ? "DRIVER" : "USER";
        sharedPrefs = context.getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        this.startForeground(serviceNotificationID, createNotification());
        if (sharedPrefs != null) {
            chatChannelID = sharedPrefs.getString("CHAT_CHANNEL_ID", "");
            baseUrl = sharedPrefs.getString("BASE_URL", "null");
        }
        isSessionCreated();
        sendMessageCallBack = this::sendMessages;
        MobilityAppBridge.registerSendMessageCallBack(sendMessageCallBack);
        sendMessageCallBackOverlay = this::sendMessages;
        MessageOverlayService.registerSendMessageCallBack(sendMessageCallBackOverlay);
        if (!isChatServiceRunning) {
            this.startForeground(serviceNotificationID, createNotification());
            FirebaseUser user = firebaseAuth.getCurrentUser();
            if (user != null) {
                startChatService();
            } else {
                signInAnonymously();
            }
        }

    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        this.startForeground(serviceNotificationID, createNotification());
        handleMessages();
        return START_STICKY;
    }

    private void isSessionCreated() {
        try {
            firestoreInstance.collection("Chats").document(chatChannelID).get().addOnCompleteListener(
                    (task -> {
                        if (task.isSuccessful()) {
                            if (task.getResult().exists()) {
                                sessionCreated = true;
                            }
                        }
                    })
            );
        } catch (Exception error) {
            Log.e(LOG_TAG, "Error in isSessionCreated :: " + error);
        }

    }

    private void startChatService() {
        addChatListener();
    }

    private void signInAnonymously() {
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
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in signInAnonymously" + e);
        }
    }

    private void handleMessages() {
        int count = 1;
        isChatServiceRunning = false;
        int messageSize = messages.size();
        for (Message message : messages) {
            shouldNotify = true;
            if (count == messageSize && !isChatServiceRunning) {
                isChatServiceRunning = true;
                if (count != 1) {
                    shouldNotify = false;
                }
            }
            count++;
            handleMessage(message, String.valueOf(messageSize - 1));
        }
    }

    private void addChatListener() {
        if (chatListener != null) return;
        try {
            if (FirebaseApp.getApps(context).size() == 0) {
                FirebaseApp.initializeApp(context);
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in Firebase Initialization" + e);
        }
        try {
            if (firestoreInstance != null && chatChannelID != null) {
                chatListener = firestoreInstance.collection("Chats").document(chatChannelID).collection("messages").orderBy("timestamp", Query.Direction.ASCENDING).addSnapshotListener(
                        (snapshots, e) -> {
                            if (e != null) {
                                Log.e(LOG_TAG, "listen:error", e);
                                return;
                            }
                            int count = 1;
                            if (snapshots == null) return;
                            for (DocumentChange dc : snapshots.getDocumentChanges()) {
                                switch (dc.getType()) {
                                    case ADDED:
                                        Map<String, Object> newMessageMap = dc.getDocument().getData();
                                        Message newMessage = createMessageObj(newMessageMap);
                                        messages.add(newMessage);
                                        shouldNotify = true;
                                        if (count == (snapshots.getDocumentChanges()).size() && !isChatServiceRunning) {
                                            isChatServiceRunning = true;
                                            if (count == (snapshots.getDocumentChanges()).size() && count != 1) {
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
            } else {
                Log.d(LOG_TAG, "Document path cannot be empty, chatChannelId is empty");
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in listening to messages " + e);
        }

    }

    public void sendMessages(final String message) {
        HashMap<String, Object> _newMessage = new HashMap<>();
        _newMessage.put("message", message);
        _newMessage.put("sentBy", chatUserId);
        _newMessage.put("timestamp", System.currentTimeMillis());
        if (sharedPrefs != null) chatChannelID = sharedPrefs.getString("CHAT_CHANNEL_ID", null);
        if (chatChannelID != null) {
            try {
                if (!sessionCreated) {
                    HashMap<String, Object> _timestamp = new HashMap<>();
                    _timestamp.put("timestamp", System.currentTimeMillis());
                    firestoreInstance.collection("Chats").document(chatChannelID).set(_timestamp).addOnCompleteListener(
                            (res) -> {
                                if (res.isSuccessful()) {
                                    sessionCreated = true;
                                }
                            }
                    );
                }
                firestoreInstance.collection("Chats").document(chatChannelID).collection("messages").add(_newMessage);
                if (merchantType.equals("DRIVER")) {
                    String decodedMessage = getMessageFromKey(message, "EN_US");
                    if(!decodedMessage.equals("")) sendFCM(decodedMessage);
                }
            } catch (Exception e) {
                Log.e(LOG_TAG, "Error in sending a message" + e);
            }
        } else {
            Log.d(LOG_TAG, "Document path cannot be empty, chatChannelId is empty");
        }
    }

    private void handleMessage(Message newMessage, String len) {
        try {
            String _dateFormatted = newMessage.timestamp;
            String _message = newMessage.message;
            String _sentBy = newMessage.sentBy;
            String appState = "";
            String stage = "HomeScreen";
            if (sharedPrefs != null) appState = sharedPrefs.getString("ACTIVITY_STATUS", "null");
            if (sharedPrefs != null) stage = sharedPrefs.getString("LOCAL_STAGE", "null");
            String sentBy;
            String appName = getApplicationContext().getResources().getString(R.string.app_name);
            if (_sentBy.equals("Driver")) {
                if (appName.equals("Yatri Driver"))
                    sentBy = "yatriprovider";
                else if(appName.equals("Yatri Sathi Driver"))
                    sentBy = "yatrisathiprovider";
                else
                    sentBy = "nammayatriprovider";
            } else {
                if (appName.equals("Yatri"))
                    sentBy = "yatriconsumer";
                else if(appName.equals("Yatri Sathi"))
                    sentBy = "yatrisathiconsumer";
                else
                    sentBy = "nammayatri";
            }
            if (appState.equals("onPause") || appState.equals("onResume")) {
                try {
                    for (int i = 0; i < callBack.size(); i++) {
                        callBack.get(i).chatCallBack(_message, _sentBy, _dateFormatted, len);
                    }
                } catch (Exception err) {
                    Log.e(LOG_TAG, "Error sending the message to jbridge : " + err);
                }
            }
            String message = getMessageFromKey(_message, null);
            if (!(merchant.equals(sentBy)) && isChatServiceRunning && shouldNotify && !message.equals("") && merchantType.equals("DRIVER")) {
                if (appState.equals("onDestroy") || appState.equals("onPause")) {
                    if (NotificationUtils.overlayFeatureNotAvailable(context))
                        NotificationUtils.createChatNotification(_sentBy, message, context);
                    else startOverlayService(message, _dateFormatted);
                } else if (appState.equals("onResume") && merchantType.equals("DRIVER") && !(stage.equals("ChatWithCustomer"))) {
                    String notificationId = String.valueOf(random.nextInt(1000000));
                    for (ShowNotificationCallBack inAppCallBack : inAppCallBacks) {
                        inAppCallBack.showInAppNotification("From " + getCustomerApp(appName) + " " +  _sentBy, message, MobilityAppBridge.storeCallBackOpenChatScreen, "", "", "", "", notificationId, 5000, context);
                    }
                }
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in handleMessage : " + e);
        }
    }

    private String getCustomerApp(String driverApp) {
        switch (driverApp) {
            case "Yatri Driver" :
                return "Yatri";
            case "Yatri Sathi Driver" :
                return "Yatri Sathi";
            case "Namma Yatri Partner":
                return "Namma Yatri";
            default: return "";
        }
    }

    private void startOverlayService(String message, String timestamp) {
        if ((merchantType.equals("DRIVER")) && Settings.canDrawOverlays(getApplicationContext()) && !sharedPrefs.getString(getResources().getString(R.string.REGISTERATION_TOKEN), "null").equals("null") && (sharedPrefs.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onPause") || sharedPrefs.getString(getResources().getString(R.string.ACTIVITY_STATUS), "null").equals("onDestroy"))) {
            try {
                SimpleDateFormat inputFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", Locale.ENGLISH);
                inputFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
                SimpleDateFormat finalOutputFormat = new SimpleDateFormat("h:mm a", Locale.ENGLISH);
                finalOutputFormat.setTimeZone(TimeZone.getTimeZone("Asia/Kolkata"));
                Date date = inputFormat.parse(timestamp);
                String finalOutputDate = date !=null ? finalOutputFormat.format(date) : timestamp;
                Intent intent = new Intent(context, MessageOverlayService.class);
                intent.putExtra("message", message);
                intent.putExtra("timestamp", finalOutputDate);
                context.startService(intent);
                startMediaPlayer(context, R.raw.new_message, false);
            } catch (Exception e) {
                Log.e(LOG_TAG, "Error in startOverlayService : " + e);
                e.printStackTrace();
            }
        }
    }

    private Notification createNotification() {
        createNotificationChannel();
        Intent notificationIntent = getPackageManager().getLaunchIntentForPackage(getPackageName());
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 10, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        String contentText;
        if (merchantType.equals("DRIVER")) {
            contentText = getString(R.string.you_can_now_chat_with_customer);
        } else {
            contentText = getString(R.string.you_can_now_chat_with_driver);
        }
        NotificationCompat.Builder notification =
                new NotificationCompat.Builder(this, "Message")
                        .setContentTitle(getString(R.string.chatting_is_enabled))
                        .setContentText(contentText)
                        .setSmallIcon(Utils.getResIdentifier(context, (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "ic_launcher_small_icon" : "ny_ic_launcher", (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "drawable" : "mipmap"))
                        .setPriority(NotificationCompat.PRIORITY_MIN)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent);
        return notification.build();
    }

    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = "Message";
            String description = "Service Notification Channel";
            NotificationChannel channel = new NotificationChannel("Message", name, NotificationManager.IMPORTANCE_MIN);
            channel.setDescription(description);
            NotificationManager notificationManager = getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }

    private String getChatDate(Long time) {
        Date date;
        try {
            date = new Date(time);
        } catch (Exception error) {
            date = new Date(System.currentTimeMillis());
        }
        /*   add this to date format if date is needed ---> dd MMM yyyy   */
        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.sss'Z'", Locale.ENGLISH);
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
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

    private Message createMessageObj(Map<String, Object> map) {
        String timestamp = getChatDate((Long) map.get("timestamp"));
        String message = (String) map.get("message");
        String sentBy = (String) map.get("sentBy");
        return new Message(message, sentBy, timestamp);
    }

    private void sendFCM(String message) {
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
                Log.e(LOG_TAG, "Error in calling send FCM API" + result);
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in sending FCM" + e);
        }
    }

    private String getMessageFromKey(String key, @Nullable String lang) {
        try {
            String suggestionsStr = sharedPrefs.getString("SUGGESTIONS_DEFINITIONS", "null");
            JSONObject suggestions = new JSONObject(suggestionsStr);
            String language = lang == null ? sharedPrefs.getString("LANGUAGE_KEY", "null") : lang;
            if (suggestions.has(key)) {
                JSONObject message = suggestions.getJSONObject(key);
                if (message.has(language.toLowerCase())) return message.getString(language.toLowerCase());
                else return message.getString("en_us");
            } else {
                return key;
            }
        } catch (Exception e) {
            Log.e(LOG_TAG, "Error in getMessageFromKey : " + e);
            return "";
        }
    }

    @Override
    public void onDestroy() {
        isChatServiceRunning = false;
        shouldNotify = true;
        sessionCreated = false;
        MobilityAppBridge.deRegisterSendMessageCallBack(sendMessageCallBack);
        MessageOverlayService.deRegisterSendMessageCallBack(sendMessageCallBackOverlay);
        stopForeground(true);
        if (chatListener != null) chatListener.remove();
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
