/*
 * Copyright (c) 2012-2017 "JUSPAY Technologies"
 * JUSPAY Technologies Pvt. Ltd. [https://www.juspay.in]
 *
 * This file is part of JUSPAY Platform.
 *
 * JUSPAY Platform is free software: you can redistribute it and/or modify
 * it for only educational purposes under the terms of the GNU Affero General
 * Public License (GNU AGPL) as published by the Free Software Foundation,
 * either version 3 of the License, or (at your option) any later version.
 * For Enterprise/Commerical licenses, contact <info@juspay.in>.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  The end user will
 * be liable for all damages without limitation, which is caused by the
 * ABUSE of the LICENSED SOFTWARE and shall INDEMNIFY JUSPAY for such
 * damages, claims, cost, including reasonable attorney fee claimed on Juspay.
 * The end user has NO right to claim any indemnification based on its use
 * of Licensed Software. See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/agpl.html>.
 */

package in.juspay.mobility.sdk.core;

import android.animation.Animator;
import android.animation.ObjectAnimator;
import android.animation.PropertyValuesHolder;
import android.animation.TimeInterpolator;
import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.SurfaceTexture;
import android.media.MediaPlayer;
import android.net.Uri;
import android.text.Editable;
import android.text.InputFilter;
import android.text.TextWatcher;
import android.util.DisplayMetrics;
import android.util.Pair;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.Surface;
import android.view.TextureView;
import android.view.View;
import android.view.animation.AccelerateDecelerateInterpolator;
import android.view.animation.AccelerateInterpolator;
import android.view.animation.BounceInterpolator;
import android.view.animation.DecelerateInterpolator;
import android.view.animation.LinearInterpolator;
import android.view.animation.PathInterpolator;
import android.widget.CalendarView;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.PopupMenu;
import android.widget.SeekBar;

import androidx.annotation.Keep;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;
import androidx.viewpager2.widget.ViewPager2;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.hyperlottie.LottieAnimation;
import in.juspay.mobility.sdk.mystique.AccordionLayout;
import in.juspay.mobility.sdk.mystique.AnimationHolder;
import in.juspay.mobility.sdk.mystique.BottomSheetLayout;
import in.juspay.mobility.sdk.mystique.ListAdapter;
import in.juspay.mobility.sdk.mystique.OnPageChange;
import in.juspay.mobility.sdk.mystique.OnScroll;
import in.juspay.mobility.sdk.mystique.SecureActionCallback;
import in.juspay.mobility.sdk.mystique.SwypeLayout;
import in.juspay.mobility.sdk.mystique.SwypeScroll;
import in.juspay.mobility.sdk.mystique.VPAdapter;

/**
 * Created by naman_juspay on 9/11/16.
 */

public class InflateView {
    private static final String LOG_TAG = InflateView.class.getName();

    private static final Map<Class<?>, Class<?>> PRIMITIVE_TYPES = new Hashtable<>();

    private static final String FUNCTION_ARG_START = ":";
    private static final String FUNCTION_ARG_SPLIT = ",";
    private static final String KEYWORD_SPLIT = "->";
    private static final String ARG_TYPE_SPLIT = "_";
    private static final String SETTER_EQUALS = "=";

    private static final Pattern FUNCTION_ARG_SPLIT_ESCAPE = Pattern.compile("(?<!\\\\)" + Pattern.quote(","));
    private static final Pattern COMMAND_SPLIT = Pattern.compile("(?<!\\\\)" + Pattern.quote(";"));

    static {
        PRIMITIVE_TYPES.put(Boolean.class, boolean.class);
        PRIMITIVE_TYPES.put(Character.class, char.class);
        PRIMITIVE_TYPES.put(Byte.class, byte.class);
        PRIMITIVE_TYPES.put(Short.class, short.class);
        PRIMITIVE_TYPES.put(Integer.class, int.class);
        PRIMITIVE_TYPES.put(Long.class, long.class);
        PRIMITIVE_TYPES.put(Float.class, float.class);
        PRIMITIVE_TYPES.put(Double.class, double.class);
        PRIMITIVE_TYPES.put(Void.class, void.class);
    }

    @NonNull
    private final HashMap<Cmd, Method> functionCache = new HashMap<>();


    private float swipeStartX, swipeEndX;
    private float swipeStartY, swipeEndY;


    @Nullable
    private PopupMenu popUpMenu;

    @NonNull
    protected HashMap<String, Object> state = new HashMap<>();

    @Nullable
    private String currViewId = "-1";
    @Nullable
    private String lastCommand = "";
    @Nullable
    private String currView = "";
    @Nullable
    private String fileOrigin = "";

    private boolean useAppContext = false;

    @NonNull
    protected final DynamicUI dynamicUI;
    @NonNull
    private final AnimationHolder animationHolder;
    @NonNull
    private final DuiCallback duiCallback;
    @Nullable
    private LottieAnimation lottieAnimation = null;

    InflateView(@NonNull final DynamicUI dynamicUI) {
        this.dynamicUI = dynamicUI;
        this.duiCallback = new DuiCallback() {
            @Override
            public DuiLogger getLogger() {
                return dynamicUI.getLogger();
            }

            @Override
            public void addJsToWebView(@NonNull String js) {
                dynamicUI.addJsToWebView(js);
            }

            @Override
            public InflateView getInflateView() {
                return dynamicUI.getInflateView();
            }
        };
        state.put("duiObj", dynamicUI);
        animationHolder = new AnimationHolder(duiCallback, dynamicUI.getAppContext().getResources().getDisplayMetrics().density);
        if (PaymentUtils.isClassAvailable("in.juspay.hyperlottie.LottieAnimation")) {
            lottieAnimation = new LottieAnimation(dynamicUI.getAppContext(), dynamicUI, dynamicUI.getBridgeComponents().getFileProviderInterface());
        }
    }

    public static boolean isWrappedPrimitiveType(Class<?> clazz) {
        return PRIMITIVE_TYPES.containsKey(clazz);
    }

    public void putInState(String key, Object o) {
        state.put(key, o);
    }

    @NonNull
    public HashMap<String, Object> getState() {
        return state;
    }

    public void resetState() {
        state = new HashMap<>();
    }

    public String getErrorDetails() {
        return currViewId + " - " + currView + "-" + fileOrigin + " - " + lastCommand;
    }

    @SuppressWarnings("unchecked")
    public <T> T getStateValFromKey(String key) {
        return (T) state.get(key);
    }

    @NonNull
    @Keep
    @SuppressWarnings("unused") // Called from JS
    public Boolean containsInState(String key) {
        return state.containsKey(key);
    }

    private Object[] parseArguments(String arguments, boolean useApplContext) {
        if (arguments == null || arguments.trim().equals("")) {
            return new Object[0];
        }
        List<Object> newArgs = new ArrayList<>();
        String[] splitArgs;
        String[] argOccurance = arguments.split(ARG_TYPE_SPLIT);

        if (indexOf(arguments, FUNCTION_ARG_SPLIT, 0) == -1) {
            newArgs.add(getValue(arguments, useApplContext));
        } else if (argOccurance.length == 2) {
            newArgs.add(getValue(arguments, useApplContext));
        } else {
            splitArgs = FUNCTION_ARG_SPLIT_ESCAPE.split(arguments);

            for (String arg : splitArgs) {
                newArgs.add(getValue(arg, useApplContext));
            }
        }

        return newArgs.toArray();
    }

    private Class<?>[] parseTypeArguments(String arguments) {
        // TODO: refactor this method like parseArguments is
        if (arguments == null) {
            return null;
        }

        // special case for animation sending string arrays to not split on ','
        String[] argOccurrence = arguments.split(ARG_TYPE_SPLIT);

        if (indexOf(arguments, FUNCTION_ARG_SPLIT, 0) != -1 && argOccurrence.length != 2) {
            String[] splitArgs = FUNCTION_ARG_SPLIT_ESCAPE.split(arguments);

            if (splitArgs.length > 1) {
                Class<?>[] classArray = new Class[splitArgs.length];

                for (int i = 0; i < splitArgs.length; i++) {
                    classArray[i] = getClassType(splitArgs[i]);
                }

                return classArray;
            }
        }

        return (Class<?>[]) new Class[]{getClassType(arguments)};
    }

    private Context getContext() {
        if (useAppContext) {
            return dynamicUI.getAppContext();
        } else {
            return dynamicUI.getActivity();
        }
    }

    @SuppressWarnings("unchecked")
    private <Any> Any getClassType(String toConvert) {
        if (toConvert != null) {
            String[] toParse = substr(toConvert, ARG_TYPE_SPLIT);
            String type = toParse[0];
            switch (type) {
                case "i":
                case "dp":
                    return (Any) int.class;
                case "b":
                    return (Any) boolean.class;
                case "cs":
                case "strget":
                    return (Any) CharSequence.class;
                case "f":
                    return (Any) float.class;
                case "sp":
                    return (Any) Float.class;
                case "l":
                    return (Any) long.class;
                case "get":
                    Object result = state.get(toParse[1]);

                    if (result != null) {
                        return (Any) result.getClass();
                    } else {
                        dynamicUI.getLogger().e("WARNING", " isNull : fn__getClassType - " + toConvert + " " + getErrorDetails());
                        dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__getClassType - " + toConvert + " " + getErrorDetails());
                    }

                case "dpf":
                    return (Any) float.class;
                case "ctx":
                    return (Any) Context.class;
                case "s":
                    return (Any) String.class;
                case "null":
                    return null;
                default:
                    try {
                        return (Any) getClassName(type);
                    } catch (ClassNotFoundException e) {
                        dynamicUI.getLogger().e("WARNING", " no class with name " + type + " : fn__getClassType - " + toConvert + " " + getErrorDetails());
                        dynamicUI.getErrorCallback().onError("WARNING", " no class with name " + type + " : fn__getClassType - " + toConvert + " " + getErrorDetails());
                    }
            }
        } else {
            dynamicUI.getLogger().e("WARNING", " isNull : fn__getClassType -  toConvert" + " " + getErrorDetails());
            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__getClassType -  toConvert" + " " + getErrorDetails());
        }

        return (Any) String.class; // maybe default could be null
    }

    Class<?> getClassName(String className) throws ClassNotFoundException {
        switch (className) {
            case "in.juspay.mystique.SwypeLayout":
                return SwypeLayout.class;
            case "in.juspay.mystique.SwypeScroll":
                return SwypeScroll.class;
            case "in.juspay.mystique.AccordionLayout":
                return AccordionLayout.class;
            case "in.juspay.mystique.BottomSheetLayout":
                return BottomSheetLayout.class;
            default:
                return Class.forName(className);
        }
    }

    // converting strings to the required types
    @SuppressWarnings("unchecked")
    private <Any> Any getValue(String toConvert, boolean useApplContext) {
        String value = null;

        if (toConvert != null) {
            dynamicUI.getLogger().d("getValue!", toConvert);

            String[] toParse = substr(toConvert, ARG_TYPE_SPLIT);
            String type = toParse[0];
            value = toParse[1];

            if (value.indexOf('\\') != -1 && value.contains(";"))
                value = value.replace("\\\\;", ";");

            if (value.indexOf('\\') != -1 && value.contains("_"))
                value = value.replace("\\\\_", "_");

            if (value.indexOf('\\') != -1 && value.contains(":"))
                value = value.replace("\\\\:", ":");

            if (value.indexOf('\\') != -1 && value.contains(","))
                value = value.replace("\\\\,", ",");

            if (value.indexOf('\\') != -1 && value.contains("="))
                value = value.replace("\\\\=", "=");

            return getValueNew(type, value);
        } else {
            dynamicUI.getLogger().e("WARNING", " isNull : fn__getValue - value" + " " + getErrorDetails());
            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__getValue - value" + " " + getErrorDetails());
        }

        return (Any) value;
    }

    public int dpToPx(int dp) {
        if (dp > 0) {
            DisplayMetrics displayMetrics = dynamicUI.getAppContext().getResources().getDisplayMetrics();
            return Math.round(dp * displayMetrics.density);
        } else {
            return 0;
        }
    }

    public float dpToPx(float dp) {
        if (dp > 0) {
            DisplayMetrics displayMetrics = dynamicUI.getAppContext().getResources().getDisplayMetrics();
            return Math.round(dp * displayMetrics.density);
        } else {
            return 0;
        }
    }

    private int indexOf(String s, String pattern, int startIndex) {
        int index = s.substring(startIndex).indexOf(pattern);
        if (index != -1 && index != 0 && index < s.length()) {
            if (s.charAt(index + startIndex - 1) == '\\') {
                return indexOf(s, pattern, index + startIndex + pattern.length());
            }
        }
        if (index == -1) {
            return index;
        } else {
            return index + startIndex;
        }
    }

    private String[] substr(String toSubStr, String pattern) {
        int index = indexOf(toSubStr, pattern, 0);

        if (index == -1) {
            return new String[]{toSubStr};
        }

        String[] parts = new String[2];
        parts[0] = toSubStr.substring(0, index);
        parts[1] = toSubStr.substring(index + pattern.length());

        return parts;
    }


    private Object runCommand(Object instance, Object result, String command, boolean useApplContext) throws Exception {
        lastCommand = command;
        String argsDetails = null;

        if (indexOf(command, KEYWORD_SPLIT, 0) != -1) {
            String keyword = substr(command, KEYWORD_SPLIT)[0];
            String key = null;

            String classMethodDetails;
            String methodOrClassName;

            // get_key
            if (indexOf(keyword, ARG_TYPE_SPLIT, 0) != -1 && keyword.startsWith("get")) {
                String[] parts = substr(keyword, ARG_TYPE_SPLIT);
                key = parts[1];
                keyword = parts[0];
            }

            if (indexOf(command, FUNCTION_ARG_START, 0) != -1) {
                classMethodDetails = substr(command, KEYWORD_SPLIT)[1];
                String[] parts = substr(classMethodDetails, FUNCTION_ARG_START);
                methodOrClassName = parts[0];
                argsDetails = parts[1];
            } else {
                classMethodDetails = substr(command, KEYWORD_SPLIT)[1];
                methodOrClassName = classMethodDetails;
            }

            Method toInvoke;

            switch (keyword) {
                case "infl":
                    toInvoke = findMethodInClass(this.getClass(), classMethodDetails);

                    if (argsDetails != null) {
                        if (toInvoke != null) {
                            result = toInvoke.invoke(this, parseArguments(argsDetails, useApplContext));
                        } else {
                            dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - infl  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - infl  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                        }
                    } else {
                        if (toInvoke != null) {
                            result = toInvoke.invoke(this);
                        } else {
                            dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - infl classMethodDetails  " + classMethodDetails + " " + getErrorDetails());
                            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - infl classMethodDetails  " + classMethodDetails + " " + getErrorDetails());
                        }
                    }
                    break;

                case "this":
                    toInvoke = findMethodInClass(instance.getClass(), classMethodDetails);

                    if (argsDetails != null) {
                        if (toInvoke != null) {
                            result = toInvoke.invoke(instance, parseArguments(argsDetails, useApplContext));
                        } else {
                            dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - classMethodDetails  " + classMethodDetails + " " + getErrorDetails());
                            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - classMethodDetails  " + classMethodDetails + " " + getErrorDetails());
                        }
                    } else {
                        if (toInvoke != null) {
                            result = toInvoke.invoke(instance);
                        } else {
                            dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - this  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - this  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                        }
                    }
                    break;
                case "parent":
                    toInvoke = findMethodInClass(instance.getClass(), classMethodDetails);

                    if (argsDetails != null) {
                        if (toInvoke != null) {
                            result = toInvoke.invoke(instance, parseArguments(argsDetails, useApplContext));
                        } else {
                            dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - parent  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - parent  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                        }
                    } else {
                        if (toInvoke != null) {
                            result = toInvoke.invoke(instance);
                        } else {
                            dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - parent  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - parent classMethodDetails  " + classMethodDetails + " " + getErrorDetails());
                        }
                    }

                    break;
                case "ctx":
                    Context ctx = dynamicUI.getActivity();
                    if (ctx == null) {
                        ctx = dynamicUI.getAppContext();
                    }
                    toInvoke = findMethodInClass(ctx.getClass(), classMethodDetails);

                    if (argsDetails != null) {
                        if (toInvoke != null) {
                            result = toInvoke.invoke(ctx, parseArguments(argsDetails, useApplContext));
                        } else {
                            dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - ctx  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - ctx  classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                        }
                    } else {
                        if (toInvoke != null) {
                            result = toInvoke.invoke(ctx);
                        } else {
                            dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - ctx classMethodDetails  " + classMethodDetails + " " + getErrorDetails());
                            dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - ctx classMethodDetails  " + classMethodDetails + " " + getErrorDetails());
                        }
                    }
                    break;
                case "get":
//                    dynamicUI.getLogger().d(LOG_TAG, "getting " + key);
                    if (key != null) {
                        Object toRunOn = state.get(key);

                        if (indexOf(methodOrClassName, ARG_TYPE_SPLIT, 0) == -1 && toRunOn != null) {
                            toInvoke = findMethodInClass(toRunOn.getClass(), classMethodDetails);

                            if (argsDetails != null) {
                                if (toInvoke != null) {
                                    result = toInvoke.invoke(toRunOn, parseArguments(argsDetails, useApplContext));
                                } else {
                                    dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - get classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                                    dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - get classMethodDetails " + classMethodDetails + " " + getErrorDetails());
                                }
                            } else {
                                if (toInvoke != null) {
                                    result = toInvoke.invoke(toRunOn);
                                } else {
                                    dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - get classMethodDetails : " + classMethodDetails + " " + getErrorDetails());
                                    dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - get classMethodDetails : " + classMethodDetails + " " + getErrorDetails());
                                }
                            }
                        } else {
                            if (toRunOn != null) {
                                String fieldName = substr(methodOrClassName, ARG_TYPE_SPLIT)[1];
                                return findAndSetField(state.get(key), fieldName, substr(classMethodDetails, FUNCTION_ARG_START)[1], useApplContext);
                            } else {
                                dynamicUI.getLogger().e("WARNING", " isNull : fn__runCommand - get_" + key + " is null" + " " + getErrorDetails());
                                dynamicUI.getErrorCallback().onError("WARNING", " isNull : fn__runCommand - get_" + key + " is null" + " " + getErrorDetails());
                            }
                        }
                    }
                    break;
                default:
                    // var_fieldName
                    if (indexOf(methodOrClassName, "var_", 0) != -1) {
                        String fieldName = substr(methodOrClassName, ARG_TYPE_SPLIT)[1];
                        Field fieldToChange = getClassName(methodOrClassName).getDeclaredField(fieldName);
                        fieldToChange.setAccessible(true);
                        fieldToChange.set(null, getValue(substr(classMethodDetails, FUNCTION_ARG_START)[1], useApplContext));
                    } else {
                        // class->new:arguments
                        if (classMethodDetails.equals("new") || substr(classMethodDetails, FUNCTION_ARG_START)[0].equals("new")) {
                            if (argsDetails != null) {
                                if (keyword.equals("in.juspay.hypersdk.mystique.DuiInvocationHandler")) {
                                    parseArguments(argsDetails, useApplContext);
                                    // result = new DuiInvocationHandler(args[0], this.dynamicUI);
                                } else {
                                    Class<?>[] argClass = parseTypeArguments(argsDetails);
                                    Constructor<?>[] constructors = getClassName(keyword).getConstructors();
                                    for (Constructor<?> constructor : constructors) {
                                        if (constructor.getParameterTypes().length == getArgsLength(argsDetails) && matchTypes(constructor.getParameterTypes(), argClass)) {
                                            result = constructor.newInstance(parseArguments(argsDetails, useApplContext));
                                            break;
                                        }
                                    }
                                }
//                                result = getClassName(keyword).getConstructor(parseTypeArguments(argsDetails)).newInstance(parseArguments(argsDetails));
                            } else {
                                result = getClassName(keyword).newInstance();
                            }
                        } else {
                            // static function in class
                            toInvoke = findMethodInClass(getClassName(keyword), classMethodDetails);
                            if (toInvoke != null) {
                                if (toInvoke.getName().equals("forName")) {
                                    result = getClassName((String) getValue(argsDetails, useApplContext));
                                } else {
                                    if (argsDetails != null) {
                                        result = toInvoke.invoke(null, parseArguments(argsDetails, useApplContext));
                                    } else {
                                        result = toInvoke.invoke(null);
                                    }
                                }
                            }
                        }
                    }
                    break;
            }
        } else {
            // just run it on the instance
            if (result == null) {
                if (indexOf(command, FUNCTION_ARG_START, 0) != -1) {
                    argsDetails = substr(command, FUNCTION_ARG_START)[1];
                    result = findMethodInClass(instance.getClass(), command).invoke(instance, parseArguments(argsDetails, useApplContext));
                } else {
                    result = findMethodInClass(instance.getClass(), command).invoke(instance);
                }
            } else {

                if (indexOf(command, FUNCTION_ARG_START, 0) != -1) {
                    argsDetails = substr(command, FUNCTION_ARG_START)[1];
                    result = findMethodInClass(result.getClass(), command).invoke(result, parseArguments(argsDetails, useApplContext));
                } else {
                    result = findMethodInClass(result.getClass(), command).invoke(result);
                }
            }
        }
        return result;
    }

    public Object parseAndRunPipe(Object instance, String toParse, boolean useApplContext) throws Exception {
        String[] commands = COMMAND_SPLIT.split(toParse);
        Object result = null;
        for (String command : commands) {
            if (!command.equals("")) {
                if (indexOf(command, SETTER_EQUALS, 0) != -1) {
                    String[] parts = substr(command, SETTER_EQUALS);
                    String setter = parts[0];
                    String stateName = substr(setter, ARG_TYPE_SPLIT)[1];
                    Object output = runCommand(instance, result, parts[1], useApplContext);
                    state.put(stateName, output);
                    dynamicUI.getLogger().d(LOG_TAG, "setting " + stateName + " to " + output);
                } else {
                    result = runCommand(instance, result, command, useApplContext);
                }
            }
        }
        // remove previous keys maybe
        return instance;
    }

    @Nullable
    @Keep // will be used by reflection
    @SuppressWarnings("unused")
    public Class<?> createPrimitiveClass(String toCovertTo) {
        switch (toCovertTo) {
            case "b":
                return boolean.class;
            case "c":
                return char.class;
            case "d":
                return double.class;
            case "f":
                return float.class;
            case "i":
                return int.class;
            case "l":
                return long.class;
            case "s":
                return short.class;
            case "v":
                return void.class;
            case "by":
                return byte.class;
        }
        return null;
    }

    @Keep
    @SuppressWarnings("unused")// used in javascript
    public void convertAndStoreArray(ArrayList<?> arr, Class<?> toConvertTo, String stateKey, boolean primitive) {
        int length = arr.size();

        if (primitive) {
            toConvertTo = PRIMITIVE_TYPES.get(toConvertTo);
        }

        if (toConvertTo != null) {
            Object newArr = Array.newInstance(toConvertTo, length);
            for (int i = 0; i < length; i++) {
                Array.set(newArr, i, arr.get(i));
            }
            state.put(stateKey, newArr);
        }
    }

    private int getArgsLength(String args) {
        return FUNCTION_ARG_SPLIT_ESCAPE.split(args).length;
    }

    protected boolean matchTypes(Class<?>[] methodClassTypes, Class<?>[] arguments) {
        for (int i = 0; i < methodClassTypes.length; i++) {
            // if real function accepts Object type and our type is non primitive we should consider it as true
            if (arguments[i] != null && methodClassTypes[i] != null && !(methodClassTypes[i].equals(Object.class) && !arguments[i].isPrimitive())) {
                if (!methodClassTypes[i].equals(arguments[i])) {
                    if (methodClassTypes[i].isPrimitive() && !arguments[i].isArray()) { // isArray check as for spinners it is reaching inside - root cause not found yet
                        Class<?> properClass;
                        try {
                            Field type = arguments[i].getField("TYPE");
                            properClass = (Class<?>) type.get(null);

                            if (properClass != null && !properClass.equals(methodClassTypes[i])) {
                                return false;
                            }
                        } catch (NoSuchFieldException e) {
                            return false;
                        } catch (Exception e) {
                            return true;
                        }
                    } else if (methodClassTypes[i].equals(ClassLoader.class)) { // special case added for proxy class Loader
                        if (arguments[i].getName().equals("dalvik.system.PathClassLoader")) {
                            return true;
                        }
                    } else if (!(methodClassTypes[i].equals(arguments[i])) && !methodClassTypes[i].isAssignableFrom(arguments[i])) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private Method tryExactMatch(Class<?> c, String functionName, Class<?>[] args) throws Exception {
        return c.getMethod(functionName, args);
    }

    private Method trySingleArgumentDeepMatch(Class<?> c, String functionName, Class<?> arg) {

        // First check if the argument is a primitive type wrapped in an object
        if (isWrappedPrimitiveType(arg)) {
            try {
                return c.getMethod(functionName, PRIMITIVE_TYPES.get(arg));
            } catch (
                    NoSuchMethodException e) {/* continue to match super classes.. yeah even for wrapped types */}
        }

        // Check if any of arg's super classes / super interfaces match the method
        do {
            for (Class<?> iface : arg.getInterfaces()) {
                try {
                    return c.getMethod(functionName, iface);
                } catch (NoSuchMethodException e) { /* continue loop */ }
            }

            try {
                return c.getMethod(functionName, arg);
            } catch (NoSuchMethodException e) { /* continue loop */ }
        } while ((arg = arg.getSuperclass()) != null);

        dynamicUI.getLogger().e(LOG_TAG, "Never reach here");
        return null;
    }

    // Try to find a matching function were any argument can be a super type of args in given specification
    private Method tryMultiAgrumentDeepMatch(Class<?> c, String functionName, Class<?>[] arguments) {
        if ("undefined".equals(functionName)) {
            return null;
        }

        dynamicUI.getLogger().d(LOG_TAG, "tryMultiAgrumentDeepMatch reached. Beware slow function.. " + c.toString() + " : " + functionName + " : " + arguments.length);

        Method[] methodList = c.getMethods(); // This call is very CPU intensive!!

        for (Method inheritedMethod : methodList) {
            if (inheritedMethod.getName().equals(functionName)) {
                if (inheritedMethod.getParameterTypes().length == arguments.length && matchTypes(inheritedMethod.getParameterTypes(), arguments)) {
                    return inheritedMethod;
                }
            }
        }

        return null;
    }

    private Method findMethodInClass(Class<?> c, String methodSignature) throws Exception {
        if (c == null) {
            return null;
        }
//        dynamicUI.getLogger().d(LOG_TAG, "findMethodInClass(): " + c.getName() + ", signature: " + methodSignature);

        String functionName;
        String arguments = null;

        if (indexOf(methodSignature, FUNCTION_ARG_START, 0) != -1) {
            String[] methodDetails = substr(methodSignature, FUNCTION_ARG_START);
            functionName = methodDetails[0];
            arguments = methodDetails[1];
        } else {
            functionName = methodSignature;
        }

        Class<?>[] args = arguments != null ? parseTypeArguments(arguments) : null;
        Cmd cmd = new Cmd(c, functionName, args);
        return findMethodWithCmd(cmd);

    }

    private Object findAndSetField(Object instance, String fieldName, String arg, boolean useApplContext) throws IllegalAccessException {
        Field fieldToSet = null;
        try {
            fieldToSet = instance.getClass().getField(fieldName);
        } catch (NoSuchFieldException e) {
            Field[] fieldsList = instance.getClass().getFields();
            for (Field currentField : fieldsList) {
                if (currentField.getName().equals(fieldName)) {
                    fieldToSet = currentField;
                }
            }
        }
        if (fieldToSet != null) {
            fieldToSet.set(instance, getValue(arg, useApplContext));
        } else {
            dynamicUI.getLogger().d(LOG_TAG, "Couldn't set field for " + fieldName);
        }
        return instance;
    }

    public void setCurrViewId(@Nullable String id) {
        this.currViewId = id;
    }

    public void setCurrView(@Nullable String viewType) {
        this.currView = viewType;
    }

    public void setFileOrigin(@Nullable String filename) {
        this.fileOrigin = filename;
    }

    public void parseKeys(String key, final JSONObject properties, Object instance, boolean useApplContext) {
        try {
            if ("inlineAnimation".equals(key) && !Thread.currentThread().getName().equals("PreRenderThread")) {
                this.animationHolder.applyAnimation(instance, new JSONArray(properties.getString(key)), properties);
                return;
            }
            if ("lottieAnimation".equals(key)) {
                if ((this.lottieAnimation != null)) {
                    this.lottieAnimation.applyAnimation(instance, new JSONArray(properties.getString(key)));
                }
                return;
            }

            if("retryCount".equals(key)){
                final String js = properties.getString("retryCount");
                final int retryCount = Integer.parseInt(js);
                if(instance instanceof ListView){
                    ListView listView = (ListView) instance;
                    ListAdapter listAdapter = (ListAdapter) listView.getAdapter();
                    listAdapter.setMaxRetryImageTaskCount(retryCount);
                }else if(instance instanceof ViewPager2){
                    ViewPager2 viewPager2 = (ViewPager2) instance;
                    VPAdapter vpAdapter = (VPAdapter) viewPager2.getAdapter();
                    vpAdapter.setMaxRetryImageTaskCount(retryCount);
                }
            }

            if ("listItem".equals(key) && properties.has("listData")) {
                if (dynamicUI.getActivity() == null) {
                    dynamicUI.getLogger().e("Missing Activity", "listData, it is not  activity, it is applicationContext");
                    return;
                }
                if (instance instanceof ListView) {
                    ListView listView = (ListView) instance;
                    listView.setDivider(null);
                    Renderer renderer = dynamicUI.getAndroidInterface().getRenderer();
                    JSONObject listItem = new JSONObject(properties.getString("listItem"));
                    JSONArray rowData = new JSONArray(properties.getString("listData"));
                    JSONObject itemView = listItem.getJSONObject("itemView");
                    JSONArray holderViews = listItem.getJSONArray("holderViews");
                    ListAdapter adapter = new ListAdapter(dynamicUI.getActivity(), renderer, itemView, holderViews, rowData, duiCallback);
                    listView.setAdapter(adapter);
                } else if (instance instanceof ViewPager2) {
                    ViewPager2 viewpager = (ViewPager2) instance;
                    Renderer renderer = dynamicUI.getAndroidInterface().getRenderer();
                    JSONObject listItem = new JSONObject(properties.getString("listItem"));
                    JSONArray rowData = new JSONArray(properties.getString("listData"));
                    JSONObject itemView = listItem.getJSONObject("itemView");
                    JSONArray holderViews = listItem.getJSONArray("holderViews");
                    VPAdapter adapter = new VPAdapter(dynamicUI.getActivity(), renderer, itemView, holderViews, rowData, duiCallback);
                    viewpager.setAdapter(adapter);
                }
                return;
            }
            if ("listData".equals(key)) {

                if (instance instanceof ListView) {
                    ListView listView = (ListView) instance;
                    JSONArray rowData = new JSONArray(properties.getString("listData"));
                    if (listView.getAdapter() instanceof ListAdapter) {
                        ListAdapter adapter = (ListAdapter) listView.getAdapter();
                        adapter.updateRowData(rowData);
                        adapter.notifyDataSetChanged();
                    }
                } else if (instance instanceof ViewPager2) {
                    ViewPager2 viewPager2 = (ViewPager2) instance;
                    JSONArray rowData = new JSONArray(properties.getString("listData"));
                    if (viewPager2.getAdapter() instanceof VPAdapter) {
                        VPAdapter adapter = (VPAdapter) viewPager2.getAdapter();
                        adapter.updateRowData(rowData);
                        adapter.notifyDataSetChanged();
                    }
                    return;
                }
            }
            if ("currentItem".equals(key)) {
                if (instance instanceof ViewPager2) {
                    ViewPager2 viewPager2 = (ViewPager2) instance;

                    try {
                        int index = properties.getInt("currentItem");
                        if (viewPager2.getAdapter() instanceof VPAdapter) {
                            VPAdapter adapter = (VPAdapter) viewPager2.getAdapter();
                            JSONArray rowData = adapter.getRowData();
                            viewPager2.postDelayed(() -> {
                                if (index <= rowData.length() - 1 && viewPager2.getCurrentItem() != index) {
                                    viewPager2.setCurrentItem(index);
                                }
                            }, 50);
                        }
                    } catch (Exception ignored) {

                    }
                    return;
                }
            }

            if (key.equals("pattern")) {
                Method method = instance.getClass().getMethod("setFilters", InputFilter[].class);
                String pattern = properties.getString("pattern");
                String[] patternArr = pattern.split(",");
                final String regex = patternArr[0];
                int patternLength;

                if (patternArr.length == 1) {
                    patternLength = 10000;
                } else {
                    patternLength = Integer.parseInt(patternArr[1].trim());
                }

                InputFilter filter = (source, start, end, dest, dstart, dend) -> {
                    for (int i = start; i < end; ++i) {
                        if (!Pattern.compile(regex).matcher(String.valueOf(source.charAt(i))).matches()) {
                            return dest.subSequence(dstart, dend);
                        }
                    }
                    return null;
                };
                InputFilter[] inpf = new InputFilter[]{filter, new InputFilter.LengthFilter(patternLength)};
                method.invoke(instance, new Object[]{inpf});
            }

            if (key.equals("inputFilter")) {
                Method method = instance.getClass().getMethod("setFilters", InputFilter[].class);
                String regex = properties.getString("inputFilter");

                InputFilter filter = (source, start, end, dest, dStart, dEnd) -> {
                    String input = dest.subSequence(0, dStart).toString() +
                            source.subSequence(start, end) +
                            dest.subSequence(dEnd, dest.length());

                    Pattern pattern = Pattern.compile(regex);
                    Matcher matcher = pattern.matcher(input);

                    if (!matcher.matches() && !matcher.hitEnd()) {
                        if (source.equals("") && dStart != dEnd) {
                            return dest.subSequence(dStart, dEnd);
                        }
                        return "";
                    }
                    return null;
                };
                InputFilter[] inputFilters = new InputFilter[]{filter};
                method.invoke(instance, new Object[]{inputFilters});
            }

            if (key.equals("onKeyUp")) {
                final String js = properties.getString("onKeyUp");
                Method onClickMethod = instance.getClass().getMethod("setOnKeyListener", View.OnKeyListener.class);
                onClickMethod.invoke(instance, (View.OnKeyListener) (view, i, k) -> {
                    dynamicUI.addJsToWebView("window.callUICallback('" + js + "','" + i + "');");
                    return false;
                });
            }

            if (key.equals("onLongPress")) {
                final String js = properties.getString("onLongPress");
                Method onClickMethod = instance.getClass().getMethod("setOnLongClickListener", View.OnLongClickListener.class);
                onClickMethod.invoke(instance, (View.OnLongClickListener) view -> {
                    dynamicUI.addJsToWebView("window.callUICallback('" + js + "');");
                    return false;
                });
            }

            if (key.equals("source") && instance instanceof TextureView) {
                TextureView loader = (TextureView) instance;
                final Context context = dynamicUI.getAppContext();
                String packageName = context.getPackageName();
                String fileName = properties.getString("source");
                int resId = context.getResources().getIdentifier(fileName, "raw", packageName);
                // TODO :: add asset support (PICAF-12004)
                String vPath = "android.resource://" + packageName + "/raw/" + resId;
                final Uri uri = Uri.parse(vPath);
                final MediaPlayer mMediaPlayer = new MediaPlayer();
                loader.setSurfaceTextureListener(new TextureView.SurfaceTextureListener() {
                    private boolean isDrawn = false;

                    @Override
                    public void onSurfaceTextureAvailable(@NonNull SurfaceTexture surfaceTexture, int width, int height) {
                        // TODO :: find out how width and height params play out (PICAF-12003)
                        if (isDrawn) {
                            return;
                        }
                        try {
                            isDrawn = true;
                            mMediaPlayer.reset();
                            mMediaPlayer.setDataSource(context, uri);
                            mMediaPlayer.setSurface(new Surface(surfaceTexture));
                            mMediaPlayer.prepareAsync();
                            if (properties.optBoolean("autoloop", false)) {
                                mMediaPlayer.setLooping(true);
                            }
                            mMediaPlayer.setOnPreparedListener(mp -> mMediaPlayer.start());
                        } catch (Exception e) {
                            dynamicUI.getLogger().e("TextureView", "Exception in TextureView: " + e);
                        }
                    }

                    @Override
                    public void onSurfaceTextureSizeChanged(@NonNull SurfaceTexture surfaceTexture, int width, int height) {
                        // TODO :: figure if this concerns us (PICAF-12003)
                    }

                    @Override
                    public boolean onSurfaceTextureDestroyed(@NonNull SurfaceTexture surfaceTexture) {
                        try {
                            mMediaPlayer.stop();
                            mMediaPlayer.release();
                        } catch (Exception e) {
                            // Stop throws illegal state exception. if called twice for the same media player.
                            // Exception ignored
                        }
                        return true;
                    }

                    @Override
                    public void onSurfaceTextureUpdated(@NonNull SurfaceTexture surfaceTexture) {
                        // TODO :: figure if this concerns us (PICAF-12003)
                    }
                });
            }

            if (key.equals("onClick")) {
                final String js = properties.getString("onClick");
                Method onClickMethod = instance.getClass().getMethod("setOnClickListener", View.OnClickListener.class);
                onClickMethod.invoke(instance, (View.OnClickListener) v -> dynamicUI.invokeFunctionInJS("callUICallback",js));
            }

            if (key.equals("onScroll")) {
                try {
                    final String js = properties.getString("onScroll");
                    if (instance instanceof ListView) {
                        OnScroll scroll;
                        if (((ListView) instance).getTag() instanceof OnScroll) {
                            scroll = ((OnScroll) ((ListView) instance).getTag());

                        } else {
                            scroll = new OnScroll(duiCallback);
                        }
                        scroll.setScrollCallback(js);
                        ((ListView) instance).setOnScrollListener(scroll);
                        ((ListView) instance).setTag(scroll);
                    }

                } catch (Exception e) {
                    System.out.println("Exception occured in onScroll:" + e);
                }
            }
            if (key.equals("onScrollStateChange")) {
                try {
                    final String js = properties.getString("onScrollStateChange");

                    if (instance instanceof ListView) {
                        OnScroll scroll;
                        if (((ListView) instance).getTag() instanceof OnScroll) {
                            scroll = ((OnScroll) ((ListView) instance).getTag());

                        } else {
                            scroll = new OnScroll(duiCallback);
                        }
                        scroll.setScrollChangeCallback(js);
                        ((ListView) instance).setOnScrollListener(scroll);
                        ((ListView) instance).setTag(scroll);


                    }
                } catch (Exception e) {
                    System.out.println("Exception occured in onScrollStateChange :" + e);
                }
            }

            if (key.equals("onRefresh")) {
                try {
                    final String js = properties.getString("onRefresh");
                    Method onClickMethod = instance.getClass().getMethod("setOnRefreshListener", SwipeRefreshLayout.OnRefreshListener.class);
                    onClickMethod.invoke(instance, (SwipeRefreshLayout.OnRefreshListener) () -> dynamicUI.addJsToWebView("window.callUICallback('" + js + "');"));
                } catch (Exception e) {
                    System.out.println("Exception occured  :" + e);
                }
            }


            if (key.equals("onItemClick")) {
                if (!(instance instanceof ListView)) {
                    return;
                }
                final ListView listView = (ListView) instance;
                final String js = properties.getString("onItemClick");
                listView.setOnItemClickListener((adapterView, view, i, l) -> dynamicUI.addJsToWebView("window.callUICallback('" + js + "'," + i + ");"));
            }

            if (key.equals("onChange")) {
                if (properties.has("separator")) {
                    separatorTextChange(properties, instance);
                } else {
                    normalTextChange(properties, instance);
                }

            }


            if (key.equals("onFocus")) {
                Method onFocusChangeMethod = instance.getClass().getMethod("setOnFocusChangeListener", View.OnFocusChangeListener.class);
                final String js = properties.getString("onFocus");
                onFocusChangeMethod.invoke(instance, (View.OnFocusChangeListener) (v, hasFocus) -> dynamicUI.addJsToWebView("window.callUICallback('" + js + "','" + hasFocus + "');"));
            }

            if (key.equals("onTouch")) {
                final String jsFunc = properties.getString("onTouch");
                Method onTouchMethod = instance.getClass().getMethod("setOnTouchListener", View.OnTouchListener.class);
                onTouchMethod.invoke(instance, new View.OnTouchListener() {

                    @SuppressLint("ClickableViewAccessibility")
                    @Override
                    public boolean onTouch(View v, MotionEvent event) {
                        String js = "window.callUICallback('" + jsFunc + "','" + event.getX() + "','" + event.getY() + "');";
                        dynamicUI.addJsToWebView(js);
                        return false;
                    }
                });
            }

            if (key.equals("onDateChange")) {
                final String jsFunc = properties.getString("onDateChange");
                Method onTouchMethod = instance.getClass().getMethod("setOnDateChangeListener", CalendarView.OnDateChangeListener.class);
                onTouchMethod.invoke(instance, (CalendarView.OnDateChangeListener) (view, year, month, dayOfMonth) -> {
                    String js = "window.callUICallback('" + jsFunc + "','" + year + "','" + month + "','" + dayOfMonth + "');";
                    dynamicUI.addJsToWebView(js);
                });
            }

            if (key.equals("onPageSelected")) {
                try {
                    final String js = properties.getString("onPageSelected");

                    if (instance instanceof ViewPager2) {
                        OnPageChange pageChange;
                        if (((ViewPager2) instance).getTag() instanceof OnPageChange) {
                            pageChange = ((OnPageChange) ((ViewPager2) instance).getTag());

                        } else {
                            pageChange = new OnPageChange(duiCallback);
                        }
                        pageChange.setOnPageSelectedCallBack(js);
                        ((ViewPager2) instance).setTag(pageChange);
                        ((ViewPager2) instance).unregisterOnPageChangeCallback(pageChange);
                        ((ViewPager2) instance).registerOnPageChangeCallback(pageChange);
                    }
                } catch (Exception e) {
                    System.out.println("Exception occured in onPageSelected :" + e);
                }
            }

            if (key.equals("onPageScrolled")) {
                try {
                    final String js = properties.getString("onPageScrolled");

                    if (instance instanceof ViewPager2) {
                        OnPageChange pageChange;
                        if (((ViewPager2) instance).getTag() instanceof OnPageChange) {
                            pageChange = ((OnPageChange) ((ViewPager2) instance).getTag());

                        } else {
                            pageChange = new OnPageChange(duiCallback);
                        }
                        pageChange.setOnPageScrolledCallBack(js);
                        ((ViewPager2) instance).setTag(pageChange);
                        ((ViewPager2) instance).unregisterOnPageChangeCallback(pageChange);
                        ((ViewPager2) instance).registerOnPageChangeCallback(pageChange);
                    }
                } catch (Exception e) {
                    System.out.println("Exception occured in onPageScrolled :" + e);
                }
            }

            if (key.equals("onPageScrollStateChanged")) {
                try {
                    final String js = properties.getString("onPageScrollStateChanged");

                    if (instance instanceof ViewPager2) {
                        OnPageChange pageChange;
                        if (((ViewPager2) instance).getTag() instanceof OnPageChange) {
                            pageChange = ((OnPageChange) ((ViewPager2) instance).getTag());

                        } else {
                            pageChange = new OnPageChange(duiCallback);
                        }
                        pageChange.setOnPageScrollStateChangedCallBack(js);
                        ((ViewPager2) instance).setTag(pageChange);
                        ((ViewPager2) instance).unregisterOnPageChangeCallback(pageChange);
                        ((ViewPager2) instance).registerOnPageChangeCallback(pageChange);
                    }
                } catch (Exception e) {
                    System.out.println("Exception occured in onPageScrollStateChanged :" + e);
                }
            }

            if (key.equals("onSwipe")) {
                final String jsFunc = properties.getString("onSwipe");
                Method onTouchMethod = instance.getClass().getMethod("setOnTouchListener", View.OnTouchListener.class);
                onTouchMethod.invoke(instance, new View.OnTouchListener() {

                    @SuppressLint("ClickableViewAccessibility")
                    @Override
                    public boolean onTouch(View v, MotionEvent event) {
                        float THRESHOLD = 100;
                        String swipeType = "0";
                        switch (event.getAction()) {
                            case MotionEvent.ACTION_DOWN: {
                                swipeStartX = event.getX();
                                swipeStartY = event.getY();
                                break;
                            }
                            case MotionEvent.ACTION_UP: {
                                swipeEndX = event.getX();
                                swipeEndY = event.getY();
                                float delX = swipeEndX - swipeStartX;
                                float delY = swipeEndY - swipeStartY;
                                float angle = (float) Math.toDegrees(Math.atan2(delY, delX));
                                angle = (angle < 0) ? angle + 360f : angle;
                                if ((angle >= 45 && angle <= 135) || (angle >= 225 && angle <= 315)) {
                                    if (swipeEndY - swipeStartY > THRESHOLD) {
                                        swipeType = "2";
                                    } else if (swipeStartY - swipeEndY > THRESHOLD) {
                                        swipeType = "-2";
                                    }
                                } else {
                                    if (swipeEndX - swipeStartX > THRESHOLD) {
                                        swipeType = "1";
                                    } else if (swipeStartX - swipeEndX > THRESHOLD) {
                                        swipeType = "-1";
                                    }
                                }
                                break;
                            }
                        }

                        String js = "window.callUICallback('" + jsFunc + "','" + swipeType + "');";
                        dynamicUI.addJsToWebView(js);

                        return true;
                    }
                });
            }

            if (key.equals("popupMenu")) {
                if (dynamicUI.getActivity() == null) {
                    dynamicUI.getLogger().e("Missing Activity", "popupMenu, it is not  activity, it is applicationContext");
                    return;
                }
                // split on , with escaped ,
                String[] popupMenu = properties.getString("popupMenu").split(FUNCTION_ARG_SPLIT_ESCAPE.toString());

                final String callbackName = properties.getString("onMenuItemClick");
                popUpMenu = new PopupMenu(dynamicUI.getActivity(), (View) instance);
                for (int i = 0; i < popupMenu.length; i++) {
                    if (popupMenu[i].contains("\\") && popupMenu[i].contains(",")) {
                        popupMenu[i] = popupMenu[i].replace("\\\\,", ",");
                    }
                    popUpMenu.getMenu().add(Menu.NONE, i, Menu.NONE, popupMenu[i]);
                }
                popUpMenu.setOnMenuItemClickListener(item -> {
                    dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "', '" + item.getItemId() + "');");
                    return true;
                });

                final PopupMenu finalPopup = popUpMenu;
                ((View) instance).setOnClickListener(v -> finalPopup.show());
            }

            if (key.equals("onSeekBarChanged")) {
                final String jsFunc = properties.getString("onSeekBarChanged");
                Method setOnSeekBarChangeListener = instance.getClass().getMethod("setOnSeekBarChangeListener", SeekBar.OnSeekBarChangeListener.class);
                setOnSeekBarChangeListener.invoke(instance, new SeekBar.OnSeekBarChangeListener() {

                    @Override
                    public void onProgressChanged(SeekBar seekBar, int i, boolean b) {
                        String js = "window.callUICallback('" + jsFunc + "', 'PROGRESS_CHANGED', '" + i + "','" + b + "');";
                        dynamicUI.addJsToWebView(js);
                    }

                    @Override
                    public void onStartTrackingTouch(SeekBar seekBar) {
                        String js = "window.callUICallback('" + jsFunc + "', 'START_TRACKING_TOUCH');";
                        dynamicUI.addJsToWebView(js);
                    }

                    @Override
                    public void onStopTrackingTouch(SeekBar seekBar) {
                        String js = "window.callUICallback('" + jsFunc + "', 'STOP_TRACKING_TOUCH');";
                        dynamicUI.addJsToWebView(js);
                    }
                });
            }

            if (key.equals("runInUI")) {
                String value = properties.getString(key);
                instance = parseAndRunPipe(instance, value, useApplContext);
            }

            if ("onStateChanged".equals(key)) {
                if (instance instanceof BottomSheetLayout) {
                    ((BottomSheetLayout) instance).setStateChangeCallback(duiCallback, properties.getString(key));
                }
            }

            if ("onSlide".equals(key)) {
                if (instance instanceof BottomSheetLayout) {
                    ((BottomSheetLayout) instance).setSlideCallback(duiCallback, properties.getString(key));
                }
            }

            if (key.equals("animation")) {
                handleAnimation(instance, new JSONArray(properties.getString("animation")));
            }


            if (key.equals("afterRender")) {
                String id = properties.getString("id");
                String js = "javascript:window.callUICallback('" + properties.getString("afterRender") + "', '" + id + "');";
                dynamicUI.addJsToWebView(js);
            }

            if (key.equals("feedback")) {
                // for clickFeedBack
                String id = properties.getString("id");
                String js = "javascript:window.callUICallback('" + properties.getString("feedback") + "', '" + id + "', '" + "feedback" + "');";
                dynamicUI.addJsToWebView(js);
            }

            if (key.equals("secureEdit")) {
                // for secure fields
                if (instance instanceof EditText) {
                    JSONArray arguments = new JSONArray(properties.getString("secureEdit"));
                    boolean disableCopy = false, disableCut = false, disableShare = false, disablePaste = false;
                    for (int i = 0; i < arguments.length(); i++) {
                        String arg = arguments.get(i).toString();
                        switch (arg) {
                            case "copy":
                                disableCopy = true;
                                break;
                            case "paste":
                                disablePaste = true;
                                break;
                            case "cut":
                                disableCut = true;
                                break;
                            case "share":
                                disableShare = true;
                                break;
                        }
                    }
                    ((EditText) instance).setCustomSelectionActionModeCallback(new SecureActionCallback(disableCopy, disableCut, disableShare, disablePaste));
                }
            }

        } catch (Exception e) {
            dynamicUI.getErrorCallback().onException("WARNING", " excep: fn__parseKeys  - " + getErrorDetails(), e);
        }
    }

    public void handleAnimation(Object instance, final JSONArray animObj) throws JSONException {
        JSONObject currAnimJSON, currProp;
        JSONArray props;
        String animId;
        String onEndId;
        float from, to;
        Pair<Integer, ObjectAnimator> viewToObjAnim;
        for (int i = 0; i < animObj.length(); i++) {
            currAnimJSON = animObj.getJSONObject(i);
            props = new JSONArray(currAnimJSON.getString("props"));
            animId = currAnimJSON.has("id") ? currAnimJSON.getString("id") : "";
            onEndId = currAnimJSON.has("onEnd") ? currAnimJSON.getString("onEnd") : "";
            PropertyValuesHolder[] propsArr = new PropertyValuesHolder[props.length()];
            String propName;
            for (int j = 0; j < props.length(); j++) {
                currProp = props.getJSONObject(j);
                from = (float) currProp.getDouble("from");
                to = (float) currProp.getDouble("to");
                propName = currProp.getString("prop");
                propsArr[j] = PropertyValuesHolder.ofFloat(propName, from, to);
            }
            final ObjectAnimator objAnim = getAnimator(instance, propsArr, currAnimJSON);
            viewToObjAnim = new Pair<>(((View) instance).getId(), objAnim);
            state.put("M_anim_" + animId, viewToObjAnim);
            if (currAnimJSON.has("onEnd")) {
                final String finalOnEndId = "M_anim_" + onEndId;
                objAnim.addListener(new Animator.AnimatorListener() {
                    @Override
                    public void onAnimationStart(@NonNull Animator animator) {

                    }

                    @Override
                    public void onAnimationEnd(@NonNull Animator animator) {
                        if (state.containsKey(finalOnEndId)) {
                            ObjectAnimator objAnimToStart = ((Pair<String, ObjectAnimator>) state.get(finalOnEndId)).second;
                            if (objAnimToStart != null && objAnimToStart != objAnim) {
                                objAnimToStart.start();
                            }
                        }
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
    }

    @SuppressWarnings("unchecked")
    public Pair<String, ObjectAnimator> findAnimationById(String id) {
        String animId = "M_anim_" + id;
        if (state.containsKey(animId)) {
            return (Pair<String, ObjectAnimator>) state.get(animId);
        }
        return null;
    }

    private ObjectAnimator getAnimator(Object instance, PropertyValuesHolder[] propsArr, final JSONObject animObj) throws JSONException {
        int repeatCount = 0;
        float duration = 0, delay = 0;
        boolean startImmediate;
        String ease = "linear";

        duration = animObj.has("duration") ? (float) animObj.getDouble("duration") : duration;
        delay = animObj.has("delay") ? (float) animObj.getDouble("delay") : delay;
        repeatCount = animObj.has("repeatCount") ? animObj.getInt("repeatCount") : repeatCount;
        startImmediate = animObj.has("startImmediate") && animObj.getBoolean("startImmediate");
        ease = animObj.has("easing") ? animObj.getString("easing") : ease;

        ObjectAnimator objAnim = ObjectAnimator.ofPropertyValuesHolder(instance, propsArr);
        objAnim.setDuration((long) duration);
        objAnim.setStartDelay((long) delay);
        objAnim.setRepeatCount(repeatCount);
        objAnim.setInterpolator(getEasing(ease));
        if (startImmediate) {
            objAnim.start();
        }
        return objAnim;

    }

    private TimeInterpolator getEasing(String ease) {
        switch (ease) {
            case "ease-in":
                return new AccelerateInterpolator();
            case "ease-out":
                return new DecelerateInterpolator();
            case "ease-in-out":
                return new AccelerateDecelerateInterpolator();
            case "bounce":
                return new BounceInterpolator();
            case "linear":
                return new LinearInterpolator();
            default:
                try {
                    if (ease.contains("[")) {
                        String type = ease.substring(0, ease.indexOf("["));
                        JSONArray consts = new JSONArray(ease.substring(ease.indexOf("[")));
                        float[] constants = new float[consts.length()];
                        for (int i = 0; i < constants.length; i++) {
                            constants[i] = (float) consts.getDouble(i);
                        }
                        return getCustomEasing(type, constants);
                    }
                } catch (JSONException ignored) {
                }
                return new LinearInterpolator();
        }
    }

    private TimeInterpolator getCustomEasing(String type, final float[] params) {
        switch (type) {
            case "bezier":
                return new PathInterpolator(params[0], params[1], params[2], params[3]);
            case "spring":
                return x -> (float) (Math.pow(2, -10 * x) * Math.sin((2 * Math.PI / params[0]) * (x - params[0] / 4))) + 1;
        }
        return new LinearInterpolator();
    }

    public void dismissPopUp() {
        ExecutorManager.runOnMainThread(() -> {
            if (popUpMenu != null) {
                popUpMenu.dismiss();
            }
        });
    }

    private void normalTextChange(JSONObject properties, Object instance) throws Exception {
        Method onChangeMethod = instance.getClass().getMethod("addTextChangedListener", TextWatcher.class);
        final String js = properties.getString("onChange");
        onChangeMethod.invoke(instance, new TextWatcher() {
            private String previousText;

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                previousText = s.toString();

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                String str;
                try {
                    str = URLEncoder.encode(s + "", "UTF-8");
                    str = "decodeURIComponent('" + str + "')";
                    str = str.replace("+", "%20");
                } catch (UnsupportedEncodingException e) {
                    str = "'" + s + "'";
                    //Ignored will never happen
                }
                if (!previousText.equals(s.toString())) {
                    dynamicUI.addJsToWebView("window.callUICallback('" + js + "', " + str + ");");
                }
            }

            @Override
            public void afterTextChanged(Editable s) {

            }
        });
    }

    private void separatorTextChange(final JSONObject properties, Object instance) throws Exception {
        final Method onChangeMethod = instance.getClass().getMethod("addTextChangedListener", TextWatcher.class);
        final EditText cardField = (EditText) instance;
        final String js = properties.getString("onChange");

        onChangeMethod.invoke(instance, new TextWatcher() {
            private static final int TOTAL_SYMBOLS = 26; // size of pattern 0000-0000-0000-0000
            private static final int TOTAL_DIGITS = 21; // max numbers of digits in pattern: 0000 x 4

            private final int DIVIDER_POSITION = properties.getInt("separatorRepeat"); // means divider position is every 4th symbol beginning with 0
            private final int DIVIDER_MODULO = DIVIDER_POSITION + 1; // means divider position is every 5th symbol beginning with 1
            private final char DIVIDER = properties.getString("separator").charAt(0);

            private String previousText;
            private boolean executeTextChange = true;

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                previousText = s.toString();
            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                if (!previousText.equals(s.toString()) && executeTextChange) {
                    dynamicUI.addJsToWebView("window.callUICallback('" + js + "', '" + s + "');");
                }
            }

            @Override
            public void afterTextChanged(Editable s) {
                if (s.length() == 0) {
                    return;
                }

                if (cardField.isFocused() && !previousText.equals(s.toString()) && executeTextChange) {
                    boolean isDelete = previousText.length() > s.length();
                    InputFilter[] filters = s.getFilters(); // save filters
                    s.setFilters(new InputFilter[]{});

                    int curPos = cardField.getSelectionStart();
                    executeTextChange = false;

                    if ((curPos + 1) % DIVIDER_MODULO == 0 && isDelete) {
                        s.delete(curPos - 1, curPos);
                    }
                    if (!isInputCorrect(s)) {

                        s.replace(0, s.length(), buildCorrectString(getDigitArray(s), s.length()));

                        if (s.length() > 0 && DIVIDER == s.charAt(s.length() - 1) && isDelete) {
                            s.delete(s.length() - 1, s.length());
                        }

                    }
                    if (curPos != 0 && curPos % DIVIDER_MODULO == 0 && s.length() > curPos && !isDelete) {
                        cardField.setSelection(curPos + 1);
                    }


                    executeTextChange = true;
                    s.setFilters(filters);
                }
            }

            private boolean isInputCorrect(Editable s) {
                boolean isCorrect = s.length() <= TOTAL_SYMBOLS; // check size of entered string

                for (int i = 0; i < s.length(); i++) { // check that every element is right
                    if (i > 0 && (i + 1) % DIVIDER_MODULO == 0) {
                        isCorrect &= DIVIDER == s.charAt(i);
                    } else {
                        isCorrect &= Character.isDigit(s.charAt(i));
                    }
                }

                return isCorrect;
            }

            private String buildCorrectString(char[] digits, int len) {
                final StringBuilder formatted = new StringBuilder();

                for (int i = 0; i < digits.length; i++) {
                    if (digits[i] != 0) {
                        formatted.append(digits[i]);

                        if ((i > 0) && (i < (len - 1)) && (((i + 1) % DIVIDER_POSITION) == 0)) {
                            formatted.append(DIVIDER);
                        }
                    }
                }

                return formatted.toString();
            }

            private char[] getDigitArray(final Editable s) {
                char[] digits = new char[TOTAL_DIGITS];
                int index = 0;

                for (int i = 0; i < s.length() && index < TOTAL_DIGITS; i++) {
                    char current = s.charAt(i);

                    if (Character.isDigit(current)) {
                        digits[index] = current;
                        index++;
                    }
                }

                return digits;
            }
        });
    }

    public void setUseAppContext(boolean useAppContext) {
        this.useAppContext = useAppContext;
    }

    public boolean getUseAppContext() {
        return this.useAppContext;
    }

    public DynamicUI getDUI() {
        return this.dynamicUI;
    }

    protected static class Cmd {
        private final Class<?> clazz;
        private final String functionName;
        private final Class<?>[] args;

        public Cmd(Class<?> clazz, String functionName, Class<?>[] args) {
            this.clazz = clazz;
            this.functionName = functionName;
            this.args = args;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Cmd cmd = (Cmd) o;

            if (!clazz.equals(cmd.clazz)) return false;
            if (!functionName.equals(cmd.functionName)) return false;
            // Probably incorrect - comparing Object[] arrays with Arrays.equals
            return Arrays.equals(args, cmd.args);
        }

        @Override
        public int hashCode() {
            int result = clazz.hashCode();
            result = 31 * result + functionName.hashCode();
            result = 31 * result + (args != null ? Arrays.hashCode(args) : 0);
            return result;
        }
    }

    protected Method findMethodWithCmd(Cmd cmd) throws Exception {

        if (functionCache.containsKey(cmd)) {
            return functionCache.get(cmd);
        } else {
            Method m;
            try {
                m = tryExactMatch(cmd.clazz, cmd.functionName, cmd.args);
            } catch (NoSuchMethodException e) {
                if (cmd.args != null && cmd.args.length == 1)
                    m = trySingleArgumentDeepMatch(cmd.clazz, cmd.functionName, cmd.args[0]);
                else
                    m = tryMultiAgrumentDeepMatch(cmd.clazz, cmd.functionName, cmd.args); // Last fallback.. consumes CPU!
            }
            functionCache.put(cmd, m);
            return m;
        }
    }


    @SuppressWarnings("unchecked")
    protected <Any> Any getValueNew(String type, String value) {
        switch (type) {

            case "infl":
                return (Any) this;
            case "i":
                return ((Any) (Integer) Integer.parseInt(value));
            case "b":
                return (Any) (Boolean) Boolean.parseBoolean(value);
            case "f":
                return (Any) (Float) Float.parseFloat(value);
            case "s":
                return (Any) value;
            case "sp":
                return (Any) (Float) (Float.parseFloat(value) * dynamicUI.getAppContext().getResources().getDisplayMetrics().scaledDensity);
            case "dp":
                return (Any) (Integer) dpToPx(Integer.parseInt(value));
            case "dpf":
                return (Any) (Float) dpToPx(Float.parseFloat(value));
            case "l":
                return (Any) (Long) Long.parseLong(value);
            case "get":
                return (Any) state.get(value);
            case "ctx":
                if (dynamicUI.getActivity() != null && !useAppContext) {
                    return (Any) dynamicUI.getActivity();
                } else {
                    return (Any) dynamicUI.getAppContext();
                }
            case "null":
                return null;
            case "strget":
                return (Any) (state.get(value) + "");
        }
        return (Any) value;
    }
}
