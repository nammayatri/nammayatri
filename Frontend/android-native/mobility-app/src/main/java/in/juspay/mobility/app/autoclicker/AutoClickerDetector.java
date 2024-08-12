package in.juspay.mobility.app.autoclicker;
import android.accessibilityservice.AccessibilityServiceInfo;
import android.content.Context;
import android.view.MotionEvent;
import android.view.accessibility.AccessibilityManager;

import java.util.ArrayList;
import java.util.List;

public class AutoClickerDetector {

    Context context;

    public AutoClickerDetector (Context context){
        this.context = context;
    }

    private static boolean isDetectingSimilarGestures = false;
    private static Touch gestureStart = null;
    private static final List<Gesture> recentGestures = new ArrayList<>();
    private static final int HISTORY_SIZE = 20;
    private static final int similarCountThreshold = 1;
    private static final int clearHistoryOnRecentNSimilarCount = 2;
    private static final int durationForSimilarGestures = 50;
    private static final float differenceOfCoordinatesForSimilarGestures = 1.0f;

    public static boolean isAccessibilityServiceEnabled(Context context) {
        AccessibilityManager am = (AccessibilityManager) context.getSystemService(Context.ACCESSIBILITY_SERVICE);
        List<AccessibilityServiceInfo> enabledServices = am.getEnabledAccessibilityServiceList(AccessibilityServiceInfo.FEEDBACK_ALL_MASK);
        return !enabledServices.isEmpty();
    }

    public static boolean isDetectingSimilarGestures() {
        return isDetectingSimilarGestures;
    }

    public static void recordEvent(MotionEvent event) {
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                gestureStart = new Touch(event.getRawX(), event.getRawY(), System.currentTimeMillis());
                break;
            case MotionEvent.ACTION_UP:
                if (gestureStart != null) {
                    Touch gestureEnd = new Touch(event.getRawX(), event.getRawY(), System.currentTimeMillis());
                    recentGestures.add(new Gesture(gestureStart, gestureEnd));
                    trimGestureHistory();
                    checkSimilarGestures();
                }
                gestureStart = null;
                break;
        }
    }

    private static void trimGestureHistory() {
        int recentCount = recentGestures.size();
        // if last latest n gestures are not same then clear all gestures
        if (recentCount >= clearHistoryOnRecentNSimilarCount) {
            Gesture lastGesture = recentGestures.get(recentCount - 1);
            Gesture secondLastGesture = recentGestures.get(recentCount - 2);
            if (!lastGesture.isSimilar(secondLastGesture)) {
                // clear all except FirstN
                clearAllExceptFirstN(similarCountThreshold);
                isDetectingSimilarGestures = false;
            }
        }

        // clear excessive items
        while (recentGestures.size() > HISTORY_SIZE) {
            recentGestures.remove(0);
        }
    }

    private static void clearAllExceptFirstN(int n) {
        // Ensure we don't try to keep more items than are in the list
        if (recentGestures.size() > n) {
            List<Gesture> firstNGestures = new ArrayList<>(recentGestures.subList(0, n));
            recentGestures.clear();
            recentGestures.addAll(firstNGestures);
        }
    }

    private static void checkSimilarGestures() {
        int recentCount = recentGestures.size();

        // Iterate from the last gesture to the first
        for (int i = recentCount - 1; i >= 0; i--) {
            Gesture searchGesture = recentGestures.get(i);
            int similarCount = 0;

            // Compare the current gesture with all the previous gestures
            for (int j = recentCount - 1; j >= 0; j--) {
                if (i != j && searchGesture.isSimilar(recentGestures.get(j))) {
                    similarCount++;
                }
            }

            // If there is more than n similar gestures, set the detection flag to true
            if (similarCount >= similarCountThreshold) {
                isDetectingSimilarGestures = true;
                return;
            }
        }

        // If no similar gestures are found, set the detection flag to false
        isDetectingSimilarGestures = false;
    }


    private static class Touch {
        final float x; // x coordinate of touch
        final float y; // y coordinate of touch
        final long time; // timestamp

        Touch(float x, float y, long time) {
            this.x = x;
            this.y = y;
            this.time = time;
        }

        boolean isSimilar(Touch other) {
            return Math.abs(this.x - other.x) < differenceOfCoordinatesForSimilarGestures && Math.abs(this.y - other.y) < differenceOfCoordinatesForSimilarGestures;
        }
    }

    private static class Gesture {
        final Touch start;
        final Touch end;
        final long duration;

        Gesture(Touch start, Touch end) { // it contains touch start, touch end and duration of the touch
            this.start = start;
            this.end = end;
            this.duration = end.time - start.time;
        }

        boolean isSimilar(Gesture other) {
            return this.start.isSimilar(other.start) &&
                    this.end.isSimilar(other.end) &&
                    Math.abs(this.duration - other.duration) < durationForSimilarGestures;
        }
    }
}
