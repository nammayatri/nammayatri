package in.juspay.mobility.app;
import static io.grpc.Metadata.ASCII_STRING_MARSHALLER;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.IBinder;
import android.util.Log;

import androidx.annotation.Nullable;

import java.util.concurrent.TimeUnit;

import io.grpc.CallOptions;
import io.grpc.Channel;
import io.grpc.ClientCall;
import io.grpc.ClientInterceptor;
import io.grpc.ForwardingClientCall;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.Metadata;
import io.grpc.MethodDescriptor;
import io.grpc.stub.StreamObserver;

public class GRPCNotificationService extends Service {
    private static NotificationGrpc.NotificationStub asyncStub;
    private ManagedChannel channel;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    // Construct Managed Notification Channel
    GRPCNotificationService() {
        Context context = getApplicationContext();
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        
        channel = ManagedChannelBuilder.forAddress("beta.beckn.uat.juspay.net", 50051)
                .intercept(new GRPCNotificationHeaderInterceptor(token))
                .keepAliveTime(15, TimeUnit.SECONDS)
                .build();
        asyncStub = NotificationGrpc.newStub(channel);
    }

    // This method starts the GRPC Notification Service
    public static void startGRPCNotificationService() throws Exception {
        GRPCNotificationResponseObserver notificationResponseObserver = new GRPCNotificationResponseObserver();
        StreamObserver<NotificationAck> notificationRequestObserver = asyncStub.streamPayload(notificationResponseObserver);
        notificationResponseObserver.startGRPCNotification(notificationRequestObserver);
    }

    // Method to close the gRPC channel
    public void closeChannel() {
        if (channel != null && !channel.isShutdown()) {
            try {
                channel.shutdown().awaitTermination(5, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
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
}

/***
 * GRPCNotificationResponseObserver
 * This class is responsible to observe the stream of the bidirectional communication happening between client application and GRPC server
 * Implements StreamObserver class as the duplex communication will be in stream
 * ***/
class GRPCNotificationResponseObserver implements StreamObserver<NotificationPayload> {
    private StreamObserver<NotificationAck> notificationRequestObserver;

    /***
     *startGRPCNotification - this method is responsible for initiating the connection to the server ( initially the acknowledgement will be sent for a random notification id )'''
     * ***/
    public void startGRPCNotification( StreamObserver<NotificationAck> notificationRequestObserver){
        this.notificationRequestObserver = notificationRequestObserver;
        Log.i("GRPC", "[Started]");
        this.notificationRequestObserver.onNext(NotificationAck.newBuilder().setId("").build());
    }

    @Override
    public void onNext(NotificationPayload value) {
        Log.i("GRPC", "[Message] : " + value.toString());
        // here we receive the message
        // we need to process the data here.
        this.notificationRequestObserver.onNext(NotificationAck.newBuilder().setId(value.getId()).build());
    }

    @Override
    public void onError(Throwable t) {
        Log.e("GRPC", "[Error] : " + t.toString());
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
    private String token;
    GRPCNotificationHeaderInterceptor(String token) {
        token = token;
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
