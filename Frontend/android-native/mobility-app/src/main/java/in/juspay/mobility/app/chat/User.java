package in.juspay.mobility.app.chat;

import com.google.firebase.firestore.ListenerRegistration;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

public class User {
    private String channelId;
    private String name;
    private ListenerRegistration listener;
    private String id;
    private final JSONArray messages;

    public User (String channelId, String name, String id) {
        this.channelId = channelId;
        this.id = id;
        this.name = name;
        this.messages = new JSONArray();
    }

    public String getChannelId() {
        return channelId;
    }

    public void setChannelId(String channelId) {
        this.channelId = channelId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public ListenerRegistration getListener() {
        return listener;
    }

    public void setListener(ListenerRegistration listener) {
        this.listener = listener;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void addMessage (Map<String, Object> map) {
        messages.put(createMessageObj(map));
    }

    public JSONArray getMessages() {
        return  messages;
    }

    public static class Message {
        String message, sentBy, timestamp, name;
        public Message(String message, String sentBy, String timestamp, String name) {
            this.message = message;
            this.sentBy = sentBy;
            this.timestamp = timestamp;
            this.name = name;
        }

        public JSONObject toJSON() {
            JSONObject json = new JSONObject();
            try {
                json.put("message",this.message);
                json.put("name",this.name);
                json.put("sentBy",this.sentBy);
                json.put("timestamp",this.timestamp);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
            return json;
        }
    }

    private JSONObject createMessageObj(Map<String, Object> map) {
        return new JSONObject(map);
    }

//    private String getChatDate(Long time) {
//        Date date;
//        try {
//            date = new Date(time);
//        } catch (Exception error) {
//            date = new Date(System.currentTimeMillis());
//        }
//        /*   add this to date format if date is needed ---> dd MMM yyyy   */
//        DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.sss'Z'", Locale.ENGLISH);
//        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
//        return dateFormat.format(date);
//    }
}
