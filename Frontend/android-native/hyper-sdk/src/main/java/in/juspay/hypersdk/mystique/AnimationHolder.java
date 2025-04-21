package in.juspay.hypersdk.mystique;

import android.animation.Animator;
import android.animation.ObjectAnimator;
import android.animation.PropertyValuesHolder;
import android.animation.ValueAnimator;
import android.util.Property;
import android.view.View;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.view.animation.AccelerateInterpolator;
import android.view.animation.BounceInterpolator;
import android.view.animation.DecelerateInterpolator;
import android.view.animation.Interpolator;
import android.view.animation.LinearInterpolator;
import android.view.animation.PathInterpolator;

import androidx.annotation.NonNull;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.WeakHashMap;

import in.juspay.hypersdk.core.DuiCallback;

public class AnimationHolder {
    private final static String NAME = "name";

    private final float density;
    private final DuiCallback duiCallback;
    private final WeakHashMap<View, CallbackHolder> callbackHashMap;
    private final WeakHashMap<View, HashMap<String, InlineAnimation>> animatorHashMap;

    public AnimationHolder(DuiCallback duiCallback, float density) {
        this.density = density;
        this.duiCallback = duiCallback;
        this.animatorHashMap = new WeakHashMap<>();
        this.callbackHashMap = new WeakHashMap<>();
    }

    private boolean hasOneKeyAtleast(JSONObject object, String... keys) {
        for (String key : keys) {
            if (object.has(key)) {
                return true;
            }
        }
        return false;
    }

    private float getFloat(JSONObject object, String name, float def, float multiplier) {
        try {
            return (float) (multiplier * object.getDouble(name));
        } catch (JSONException e) {
            return def;
        }
    }

    private String getString(JSONObject object, String name, String def) {
        try {
            return object.getString(name);
        } catch (JSONException e) {
            return def;
        }
    }

    private ArrayList<String> getJSONKeys(JSONObject object) {
        Iterator<String> keyIter = object.keys();
        ArrayList<String> keys = new ArrayList<>();
        while (keyIter.hasNext()) {
            keys.add(keyIter.next());
        }
        return keys;
    }

    private JSONObject getJSONObject(JSONArray array, int i) {
        try {
            return array.getJSONObject(i);
        } catch (JSONException e) {
            return null;
        }
    }

    private Boolean toResetAnimation(JSONObject object) {
        if (object.has("resetAnimation")) {
            try {
                return object.getBoolean("resetAnimation");
            } catch (JSONException e) {
                return false;
            }
        }
        return false;
    }

    public void applyAnimation(Object object, JSONArray animation, JSONObject properties) {
        if ((object instanceof View)) {
            assertView(object);
            View view = (View) object;
            updateViewCallbacks(view, properties);
            setupAnimation(view, animation, toResetAnimation(properties));
        }
    }

    private void setupAnimation(View view, JSONArray animations, Boolean resetAnimation) {
        HashMap<String, InlineAnimation> appliedAnimations = animatorHashMap.get(view);
        if (appliedAnimations == null) {
            appliedAnimations = new HashMap<>();
            animatorHashMap.put(view, appliedAnimations);
        }
        HashMap<String, Boolean> seen = new HashMap<>();
        for (int i = 0; i < animations.length(); i++) {
            JSONObject animation = getJSONObject(animations, i);
            if (animation == null) {
                continue;
            }
            String name = getString(animation, NAME, "");
            if (appliedAnimations.containsKey(name)) {
                InlineAnimation inlineAnimation = appliedAnimations.get(name);
                if (inlineAnimation != null) inlineAnimation.update(animation, resetAnimation);
                else startNewAnimation(view, animation, appliedAnimations, name);
            } else startNewAnimation(view, animation, appliedAnimations, name);
            seen.put(name, true);
        }
        List<String> appliedKeys = new ArrayList<>(appliedAnimations.keySet());
        for (String name : appliedKeys) {
            if (!seen.containsKey(name)) {
                InlineAnimation inlineAnimation = appliedAnimations.get(name);
                if (inlineAnimation != null) inlineAnimation.remove();
                appliedAnimations.remove(name);
            }
        }
    }

    private void startNewAnimation(View view, JSONObject animation, HashMap<String, InlineAnimation> appliedAnimations, String name) {
        InlineAnimation inlineAnimation = new InlineAnimation(animation, view);
        inlineAnimation.start();
        appliedAnimations.put(name, inlineAnimation);
    }

    private void updateViewCallbacks(View view, JSONObject properties) {
        CallbackHolder holder = callbackHashMap.get(view);
        if (holder == null) {
            holder = new CallbackHolder();
        }
        holder.updateCallbacks(properties);
        callbackHashMap.put(view, holder);
    }

    private void assertView(Object object) {
        if (!(object instanceof View)) {
            throw new Error("Instance object is not a view");
        }
    }

    class InlineAnimation {
        private final static String TAG = "tag";
        private final static String INTERPOLATOR = "interpolator";
        private final static String DELAY = "delay";
        private final static String DURATION = "duration";
        private final static String REPEAT_MODE = "repeatMode";
        private final static String REPEAT_COUNT = "repeatCount";

        private final static String FROM_X = "fromX";
        private final static String FROM_Y = "fromY";
        private final static String TO_X = "toX";
        private final static String TO_Y = "toY";

        private final static String FROM_SCALE_X = "fromScaleX";
        private final static String FROM_SCALE_Y = "fromScaleY";
        private final static String TO_SCALE_X = "toScaleX";
        private final static String TO_SCALE_Y = "toScaleY";

        private final static String FROM_ROTATION = "fromRotation";
        private final static String FROM_ROTATION_X = "fromRotationX";
        private final static String FROM_ROTATION_Y = "fromRotationY";
        private final static String TO_ROTATION = "toRotation";
        private final static String TO_ROTATION_X = "toRotationX";
        private final static String TO_ROTATION_Y = "toRotationY";

        private final static String FROM_ALPHA = "fromAlpha";
        private final static String TO_ALPHA = "toAlpha";


        private final WeakReference<View> viewRef;
        private JSONObject properties, newProperties;
        private ObjectAnimator animator;
        private ArrayList<PropertyValuesHolder> holders = new ArrayList<>();

        public InlineAnimation(JSONObject properties, View view) {
            this.viewRef = new WeakReference<>(view);
            this.properties = properties;
        }

        public String getName() {
            return getString(properties, NAME, "");
        }

        public String getTag() {
            return getString(properties, TAG, "untagged");
        }

        public void update(JSONObject newProperties, Boolean resetAnimation) {
            if (!resetAnimation && this.isSame(newProperties)) {
                return;
            }
            this.stop();
            this.newProperties = newProperties;
            this.resetAnimation();
            this.newProperties = null;

            this.properties = newProperties;
            this.start();
        }

        public void start() {
            createAnimator();
            setEventListeners();
            animator.start();
        }

        public void stop() {
            if (animator.isRunning()) {
                animator.cancel();
            }
        }

        public void remove() {
            this.stop();
            this.resetAnimation();
        }

        private boolean isSame(JSONObject newProperties) {
            List<String> keys = getJSONKeys(properties);
            List<String> newKeys = getJSONKeys(newProperties);
            for (String newKey : newKeys) {
                if (!keys.contains(newKey)) {
                    return false;
                }
                if (!getString(properties, newKey, "").equals(getString(newProperties, newKey, null))) {
                    return false;
                }
                keys.remove(newKey);
            }
            return (keys.size() == 0);
        }

        private void setEventListeners() {
            if (duiCallback == null) {
                return;
            }
            final CallbackHolder holder = callbackHashMap.get(viewRef.get());
            if (holder != null && (holder.getOnEnd() != null || holder.getOnStart() != null)) {
                animator.addListener(new Animator.AnimatorListener() {
                    @Override
                    public void onAnimationStart(@NonNull Animator animator) {
                        if (holder.getOnStart() != null) {
                            duiCallback.addJsToWebView("window.callUICallback('" + holder.getOnStart() + "','" + getTag() + "');");
                        }
                    }

                    @Override
                    public void onAnimationEnd(@NonNull Animator animator) {
                        if (holder.getOnEnd() != null) {
                            duiCallback.addJsToWebView("window.callUICallback('" + holder.getOnEnd() + "','" + getTag() + "');");
                        }
                        animator.removeListener(this);
                    }

                    @Override
                    public void onAnimationCancel(@NonNull Animator animator) {

                    }

                    @Override
                    public void onAnimationRepeat(@NonNull Animator animator) {

                    }
                });
            }
        }

        private void createAnimator() {
            if (viewRef.get() == null) {
                return;
            }
            View view = viewRef.get();
            holders = new ArrayList<>();
            animator = new ObjectAnimator();
            animator.setTarget(view);
            animator.setInterpolator(getInterpolator());
            animator.setDuration((int) getFloat(properties, DURATION, 0, 1.0f));
            animator.setStartDelay((int) getFloat(properties, DELAY, 0, 1.0f));
            int count = (int) getFloat(properties, REPEAT_COUNT, 0, 1.0f);
            animator.setRepeatCount(count);
            if (properties.has(REPEAT_MODE)) {
                boolean isReverse = "reverse".equals(getString(properties, REPEAT_MODE, null));
                animator.setRepeatMode(isReverse ? ValueAnimator.REVERSE : ValueAnimator.RESTART);
            }

            createPropertyHolder(View.ALPHA, view.getAlpha(), FROM_ALPHA, TO_ALPHA);
            createPropertyHolder(View.ROTATION, view.getRotation(), FROM_ROTATION, TO_ROTATION);
            createPropertyHolder(View.ROTATION_X, view.getRotationX(), FROM_ROTATION_X, TO_ROTATION_X);
            createPropertyHolder(View.ROTATION_Y, view.getRotationY(), FROM_ROTATION_Y, TO_ROTATION_Y);
            createPropertyHolder(View.SCALE_X, view.getScaleX(), FROM_SCALE_X, TO_SCALE_X);
            createPropertyHolder(View.SCALE_Y, view.getScaleY(), FROM_SCALE_Y, TO_SCALE_Y);
            createPropertyHolder(View.TRANSLATION_X, view.getTranslationX(), FROM_X, TO_X);
            createPropertyHolder(View.TRANSLATION_Y, view.getTranslationY(), FROM_Y, TO_Y);

            PropertyValuesHolder[] propertyValuesHolders = new PropertyValuesHolder[holders.size()];
            for (int i = 0; i < holders.size(); i++) {
                propertyValuesHolders[i] = holders.get(i);
            }
            animator.setValues(propertyValuesHolders);
        }

        private Interpolator getInterpolator() {
            String type = getString(properties, INTERPOLATOR, "linear");
            switch (type) {
                case "bounce":
                    return new BounceInterpolator();
                case "easein":
                    return new AccelerateInterpolator();
                case "easeout":
                    return new DecelerateInterpolator();
                case "easeinout":
                    return new AccelerateDecelerateInterpolator();
            }
            if (type.contains(",")) {
                String[] tokens = type.split(",");
                float[] values = new float[]{0, 0, 0, 0};
                for (int i = 0; i < tokens.length; i++) {
                    values[i] = Float.parseFloat(tokens[i]);
                }
                return new PathInterpolator(values[0], values[1], values[2], values[3]);

            }
            return new LinearInterpolator();
        }

        private void resetAnimation() {
            resetProperty(View.ALPHA, 1, FROM_ALPHA, TO_ALPHA);
            resetProperty(View.ROTATION, 0, FROM_ROTATION, TO_ROTATION);
            resetProperty(View.ROTATION_X, 0, FROM_ROTATION_X, TO_ROTATION_X);
            resetProperty(View.ROTATION_Y, 0, FROM_ROTATION_Y, TO_ROTATION_Y);
            resetProperty(View.SCALE_X, 1, FROM_SCALE_X, TO_SCALE_X);
            resetProperty(View.SCALE_Y, 1, FROM_SCALE_Y, TO_SCALE_Y);
            resetProperty(View.TRANSLATION_X, 0, FROM_X, TO_X);
            resetProperty(View.TRANSLATION_Y, 0, FROM_Y, TO_Y);
        }

        private void resetProperty(Property<View, Float> property, float def, String... keys) {
            if (!hasOneKeyAtleast(properties, keys)) {
                return;
            }
            if (newProperties != null && hasOneKeyAtleast(newProperties, keys)) {
                return;
            }
            property.set(viewRef.get(), def);
        }

        private void createPropertyHolder(Property<View, Float> property, float def, String... keys) {
            if (!hasOneKeyAtleast(properties, keys)) {
                return;
            }
            float scaleWithDensity = (property == View.TRANSLATION_Y || property == View.TRANSLATION_X) ? density : 1;
            float[] floatValues = new float[keys.length];
            for (int i = 0; i < keys.length; i++) {
                floatValues[i] = getFloat(properties, keys[i], def, scaleWithDensity);
            }
            holders.add(PropertyValuesHolder.ofFloat(property, floatValues));
        }
    }

    class CallbackHolder {
        private final static String ON_ANIMATION_END = "onAnimationEnd";
        private final static String ON_ANIMATION_START = "onAnimationStart";
        private final static String ON_ANIMATION_UPDATE = "onAnimationUpdate";

        private String onStart, onUpdate, onEnd;

        void updateCallbacks(JSONObject object) {
            if (object == null) {
                return;
            }
            onEnd = getString(object, ON_ANIMATION_END, onEnd);
            onStart = getString(object, ON_ANIMATION_START, onStart);
            onUpdate = getString(object, ON_ANIMATION_UPDATE, onUpdate);
        }

        public String getOnStart() {
            return onStart;
        }

        public String getOnUpdate() {
            return onUpdate;
        }

        public String getOnEnd() {
            return onEnd;
        }
    }
}
