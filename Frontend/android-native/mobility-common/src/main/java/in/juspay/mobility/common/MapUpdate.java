package in.juspay.mobility.common;

import android.os.Handler;
import android.os.Looper;

public class MapUpdate {
    public boolean isIdleListenerActive = false;
    public boolean isMoveListenerActive = false;
    public boolean isMapMoved = false;
    public boolean isGestureMovement = false;
    public Handler mapRecenterHandler = new Handler(Looper.getMainLooper());
    public boolean isMapIdle = true;
}
