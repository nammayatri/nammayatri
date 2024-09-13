package in.juspay.mobility.app.services;

import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.telecom.CallAudioState;
import android.telecom.Connection;
import android.telecom.ConnectionRequest;
import android.telecom.ConnectionService;
import android.telecom.DisconnectCause;
import android.telecom.PhoneAccountHandle;
import android.telecom.TelecomManager;
import android.util.Log;

import androidx.localbroadcastmanager.content.LocalBroadcastManager;
import android.os.Handler;

import in.juspay.mobility.app.R;

public class VoiceConnectionService extends ConnectionService {
    private static final String TAG = "VoiceConnectionService";
    private static Connection activeConnection;

    public static final String ACTION_DISCONNECT_CALL = "ACTION_DISCONNECT_CALL";
    public static final String ACTION_DTMF_SEND = "ACTION_DTMF_SEND";
    public static final String DTMF = "DTMF";
    private static final int PERMISSIONS_ALL = 100;
    public static Connection getConnection() {
        return activeConnection;
    }

    public static void releaseConnection() {
        if (null != activeConnection) {
            activeConnection.destroy();
            activeConnection = null;
        }
    }

    @Override
    public Connection onCreateIncomingConnection(PhoneAccountHandle connectionManagerPhoneAccount, ConnectionRequest request) {
        Connection incomingCallConnection = createConnection(request);
        incomingCallConnection.setRinging();
        return incomingCallConnection;
    }

    @Override
    public Connection onCreateOutgoingConnection(PhoneAccountHandle connectionManagerPhoneAccount,
                                                 ConnectionRequest request) {
        Connection outgoingCallConnection = createConnection(request);
        outgoingCallConnection.setDialing();
        return outgoingCallConnection;
    }

    private Connection createConnection(ConnectionRequest request) {
        activeConnection = new Connection() {

            @Override
            public void onStateChanged(int state) {
                if (state == Connection.STATE_DIALING) {
                    final Handler handler = new Handler();
                    handler.post(() -> sendCallRequestToActivity(String.valueOf(R.string.ACTION_OUTGOING_CALL)));
                }
            }

            @Override
            public void onCallAudioStateChanged(CallAudioState state) {
                Log.d(TAG, "onCallAudioStateChanged called, current state is " + state);
            }

            @Override
            public void onPlayDtmfTone(char c) {
                Log.d(TAG, "onPlayDtmfTone called with DTMF " + c);
                Bundle extras = new Bundle();
                extras.putString(DTMF, Character.toString(c));
//                activeConnection.setExtras(exFMObilitras);
                final Handler handler = new Handler();
                handler.post(() -> sendCallRequestToActivity(ACTION_DTMF_SEND));
            }

            @Override
            public void onDisconnect() {
                super.onDisconnect();
                activeConnection.setDisconnected(new DisconnectCause(DisconnectCause.LOCAL));
                releaseConnection();
                final Handler handler = new Handler();
                handler.post(() -> sendCallRequestToActivity(ACTION_DISCONNECT_CALL));
            }

            @Override
            public void onSeparate() {
                super.onSeparate();
            }

            @Override
            public void onAbort() {
                super.onAbort();
                activeConnection.setDisconnected(new DisconnectCause(DisconnectCause.CANCELED));
                releaseConnection();
            }

            @Override
            public void onAnswer() {
                super.onAnswer();
            }

            @Override
            public void onReject() {
                super.onReject();
            }

            @Override
            public void onPostDialContinue(boolean proceed) {
                super.onPostDialContinue(true);
            }
        };
        // setup the origin of the caller
        final Uri recipient = request.getExtras().getParcelable(String.valueOf(R.string.OUTGOING_CALL_RECIPIENT));
        if (null != recipient) {
            activeConnection.setAddress(recipient, TelecomManager.PRESENTATION_ALLOWED);
        } else {
            activeConnection.setAddress(request.getAddress(), TelecomManager.PRESENTATION_ALLOWED);
        }
        // self managed isn't available before version O
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N_MR1) {
            activeConnection.setConnectionProperties(Connection.PROPERTY_SELF_MANAGED);
        }
        // set mute capability (for DTMF support?)
        activeConnection.setConnectionCapabilities(Connection.CAPABILITY_MUTE);
        return activeConnection;
    }

    /*
     * Send call request to the VoiceConnectionServiceActivity
     */
    private void sendCallRequestToActivity(String action) {
        Intent intent = new Intent(action);
        Bundle extras = new Bundle();
        if (action.equals(String.valueOf(R.string.ACTION_OUTGOING_CALL))) {
            Uri address = activeConnection.getAddress();
            extras.putParcelable(String.valueOf(R.string.OUTGOING_CALL_RECIPIENT), address);
            intent.putExtras(extras);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_SINGLE_TOP);
            LocalBroadcastManager.getInstance(this).sendBroadcast(intent);
        } else if (action.equals(ACTION_DISCONNECT_CALL)) {
            extras.putInt("Reason", DisconnectCause.LOCAL);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_SINGLE_TOP);
            intent.putExtras(extras);
            LocalBroadcastManager.getInstance(this).sendBroadcast(intent);
        } else if (action.equals(ACTION_DTMF_SEND)) {
            String d = activeConnection.getExtras().getString(DTMF);
            extras.putString(DTMF, activeConnection.getExtras().getString(DTMF));
            intent.putExtras(extras);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_SINGLE_TOP);
            LocalBroadcastManager.getInstance(this).sendBroadcast(intent);
        }
    }


//    protected static void selectAudioDevice(VoiceActivity.AudioDevices audioDevice) {
//        if (activeConnection != null) {
//            if (audioDevice == VoiceActivity.AudioDevices.Speaker) {
//                activeConnection.setAudioRoute(CallAudioState.ROUTE_SPEAKER);
//            } else if (audioDevice == VoiceActivity.AudioDevices.Earpiece) {
//                activeConnection.setAudioRoute(CallAudioState.ROUTE_EARPIECE);
//            } else if (audioDevice == VoiceActivity.AudioDevices.Headset) {
//                activeConnection.setAudioRoute(CallAudioState.ROUTE_WIRED_HEADSET);
//            } else if (audioDevice == VoiceActivity.AudioDevices.Bluetooth) {
//                activeConnection.setAudioRoute(CallAudioState.ROUTE_BLUETOOTH);
//            }
//        }
//    }
}
