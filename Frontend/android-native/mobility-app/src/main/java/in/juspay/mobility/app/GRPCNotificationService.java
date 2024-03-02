package in.juspay.mobility.app;
import static io.grpc.Metadata.ASCII_STRING_MARSHALLER;

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
import android.util.Log;

import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;

import com.google.android.gms.location.LocationServices;

import java.util.Map;
import java.util.Timer;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import io.grpc.CallOptions;
import io.grpc.Channel;
import io.grpc.ClientCall;
import io.grpc.ClientInterceptor;
import io.grpc.ConnectivityState;
import io.grpc.ForwardingClientCall;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.Metadata;
import io.grpc.MethodDescriptor;
import io.grpc.stub.StreamObserver;

interface ErrorListener {
    void onError(Throwable t);
}

public class GRPCNotificationService extends Service implements ErrorListener {
    private static NotificationGrpc.NotificationStub asyncStub;
    private ManagedChannel channel;
    private String token;
    private Context context;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        this.context = getApplicationContext();
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        initialize();
//        this.startForeground(50051, createNotification());
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        stopForeground(true);
        closeChannel();
        stopSelf();
    }

    /* Initialize channel and connection for bi-directional notification streaming */
    private void initialize() {
        channel = ManagedChannelBuilder.forAddress("beta.beckn.uat.juspay.net", 50051)
                .intercept(new GRPCNotificationHeaderInterceptor(token))
                .keepAliveTime(20, TimeUnit.SECONDS)
                .keepAliveTimeout(5, TimeUnit.SECONDS)
                .keepAliveWithoutCalls(true)
                .maxRetryAttempts(10)
                .enableRetry()
                .build();
        asyncStub = NotificationGrpc.newStub(channel);
        startGRPCNotificationService();
    }

    /* Creating channel for sticky notification */
    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel("SEARCH_REQUEST_NOTIFICATION", LOCATION_SERVICE, NotificationManager.IMPORTANCE_MIN);
            channel.setDescription("LISTENING_FOR_NOTIFICATIONS");
            NotificationManager notificationManager = getSystemService(NotificationManager.class);
            notificationManager.createNotificationChannel(channel);
        }
    }

    /* returns notification for foreground services */
    private Notification createNotification() {
        createNotificationChannel();
        Intent notificationIntent = getPackageManager().getLaunchIntentForPackage(context.getPackageName());
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 10, notificationIntent, PendingIntent.FLAG_IMMUTABLE);
        NotificationCompat.Builder notification =
                new NotificationCompat.Builder(this, "SEARCH_REQUEST_NOTIFICATION")
                        .setContentTitle("Listening")
                        .setContentText(getString(R.string.your_location_is_being_updated))
                        .setSmallIcon(Utils.getResIdentifier(context, (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) ? "ic_launcher_small_icon" : "ny_ic_launcher", "drawable"))
                        .setPriority(NotificationCompat.PRIORITY_MIN)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent);
        return notification.build();
    }

    // This method starts the GRPC Notification Service
    private void startGRPCNotificationService() {
        GRPCNotificationResponseObserver notificationResponseObserver = new GRPCNotificationResponseObserver(this);
        StreamObserver<NotificationAck> notificationRequestObserver = asyncStub.streamPayload(notificationResponseObserver);
        notificationResponseObserver.startGRPCNotification(notificationRequestObserver);
    }

    // Method to close the gRPC channel
    private void closeChannel() {
        if (channel != null && !channel.isShutdown()) {
            channel.shutdownNow();
        }
    }

    // Override the finalize() method to ensure cleanup when the object is garbage collected
    @Override
    protected void finalize() throws Throwable {
        try {
            closeChannel();
        } finally {
            super.finalize();
        }
    }

    @Override
    public void onError(Throwable t) {
        Log.e("GRPC", "[Retrying]");
        closeChannel();
        initialize();
    }
}

/***
 * GRPCNotificationResponseObserver
 * This class is responsible to observe the stream of the bidirectional communication happening between client application and GRPC server
 * Implements StreamObserver class as the duplex communication will be in stream
 * ***/
class GRPCNotificationResponseObserver implements StreamObserver<NotificationPayload> {
    private StreamObserver<NotificationAck> notificationRequestObserver;
    private final ErrorListener errorListener;

    public GRPCNotificationResponseObserver(ErrorListener errorListener) {
        this.errorListener = errorListener;
    }

    /***
     * This method is responsible for initiating the connection to the server ( initially the acknowledgement will be sent for a random notification id )'''
     * ***/
    public void startGRPCNotification( StreamObserver<NotificationAck> notificationRequestObserver){
        this.notificationRequestObserver = notificationRequestObserver;
        Log.i("GRPC", "[Started]");
        this.notificationRequestObserver.onNext(NotificationAck.newBuilder().setId("").build());
    }

    @Override
    public void onNext(NotificationPayload value) {
        Log.i("GRPC", "[Message] : " + value.toString());
        /***
         * Here we receive the message, we need to process the data here.
         */
        this.notificationRequestObserver.onNext(NotificationAck.newBuilder().setId(value.getId()).build());
    }

    @Override
    public void onError(Throwable t) {
        Log.e("GRPC", "[Error] : " + t.toString());
        errorListener.onError(t);
    }

    @Override
    public void onCompleted() {
        Log.e("GRPC", "[Completed]");
    }
}

/***
 * GRPCNotificationHeaderInterceptor
 * This class is responsible for modifying the message ( here adding header in the message ) before sending the message to the server in duplex communication
 * ***/
class GRPCNotificationHeaderInterceptor implements ClientInterceptor {
    final private String token;
    GRPCNotificationHeaderInterceptor(String token) {
        this.token = token;
    }

    @Override
    public <ReqT, RespT> ClientCall<ReqT, RespT> interceptCall(MethodDescriptor<ReqT, RespT> method, CallOptions callOptions, Channel next) {
        return new ForwardingClientCall.SimpleForwardingClientCall<ReqT, RespT>(next.newCall(method, callOptions)) {

            @Override
            public void start(Listener<RespT> responseListener, Metadata headers) {
                headers.put(Metadata.Key.of("token", ASCII_STRING_MARSHALLER), token);
                super.start(responseListener, headers);
            }
        };
    }
}
