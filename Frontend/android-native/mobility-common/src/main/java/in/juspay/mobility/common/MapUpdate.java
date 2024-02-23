package in.juspay.mobility.common;

import android.os.Handler;

public class MapUpdate {
    public boolean isIdleListenerActive = false;
    public boolean isMoveListenerActive = false;
    public boolean isMapMoved = false;
    public boolean isGestureMovement = false;
    public Handler mapRecenterHandler = new Handler();
    public boolean isMapIdle = true;
}
