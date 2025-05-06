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
import android.content.Context;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Rect;
import android.os.Build;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.util.Pair;
import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowMetrics;
import android.view.inputmethod.InputMethodManager;
import android.webkit.JavascriptInterface;
import android.widget.ImageView;
import android.widget.PopupMenu;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.core.view.ViewCompat;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import in.juspay.mobility.sdk.hyper.core.ExecutorManager;

/**
 * Created by sahebjot on 4/10/16.
 */

final class AndroidInterface {

    private String state;
    private final DynamicUI dynamicUI;

    private final Map<String, PendingAddScreenMapItem> pendingAddScreenMap = new HashMap<>();
    private final Set<String> onGoingPrepareScreenSet = new HashSet<>();

    //Never make it public - Should not be accessible outside this package
    AndroidInterface(@NonNull DynamicUI dynamicUI) {
        this.dynamicUI = dynamicUI;
    }

    public Renderer getRenderer() {
        return dynamicUI.getRenderer();
    }

    @Deprecated
    @JavascriptInterface
    public void Render(final String ui, final String callbackName) {
        Log.d("DynamicUI", "Method Android.Render is deprecated. Use Android.render() instead");
        render(ui, callbackName, null);
    }

    @JavascriptInterface
    public void render(final String ui, final String callbackName) {
        render(ui, callbackName, "true", null);
    }

    @Deprecated
    @JavascriptInterface
    public void Render(final String ui, final String callbackName, final String shouldFlush) {
        Log.d("DynamicUI", "Method Android.Render is deprecated. Use Android.render() instead");
        render(ui, callbackName, shouldFlush, null);
    }

    @JavascriptInterface
    public void render(final String ui, final String callbackName, final String shouldFlush) {
        render(ui, callbackName, shouldFlush, null);
    }

    @JavascriptInterface
    public void render(final String ui, final String callbackName, final String shouldFlush, final String namespace) {
        try {
            final JSONObject jsonUI = new JSONObject(ui);
            if (dynamicUI.getContainer(namespace) != null) {
                ExecutorManager.runOnMainThread(() -> {
                    try {
                        dynamicUI.getRenderer().renderUI(jsonUI, dynamicUI.getContainer(namespace), Boolean.parseBoolean(shouldFlush), namespace);
                        if (callbackName != null) {
                            dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'success');");
                        }
                    } catch (Exception e) {
                        dynamicUI.getLogger().e("ERROR", " excep: fn__Render  - " + dynamicUI.getRenderer().getErrorDetails());
                        dynamicUI.getErrorCallback().onException("ERROR", " excep: fn__Render  - " + dynamicUI.getRenderer().getErrorDetails(), e);
                        if (callbackName != null) {
                            dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'failure');");
                        }
                    }
                });
            } else {
                dynamicUI.getLogger().e("missing_container", "render, it is not activity, it is applicationContext/ no container");
                dynamicUI.getErrorCallback().onError("ERROR", " excep: fn__Render  - missing_container - " + dynamicUI.getRenderer().getErrorDetails());
                if (callbackName != null) {
                    dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'failure');");
                }
            }
        } catch (JSONException e) {
            dynamicUI.getLogger().e("JSONERROR", "fn__render - " + dynamicUI.getRenderer().getErrorDetails() + " - " + ui);
        }
    }

    @JavascriptInterface
    public void dismissPopUp() {
        dynamicUI.getRenderer().dismissPopUp();
    }

    @JavascriptInterface
    public void throwError(String error) {
        dynamicUI.getLogger().e("throwError", error);
    }

    @JavascriptInterface
    public void addViewToParent(final String parentId, final String ui, final int index, final String callbackName, final boolean replaceChild) {
        addViewToParent(parentId, ui, index, callbackName, replaceChild, null);
    }

    @JavascriptInterface
    public void addViewToParent(final String parentId, final String ui, final int index, final String callbackName, final boolean replaceChild, final String namespace) {
        try {
            final JSONObject uiObj = new JSONObject(ui);

            ExecutorManager.runOnMainThread(() -> {
                try {
                    dynamicUI.getRenderer().addViewToParent(parentId, uiObj, index, replaceChild, namespace);
                    if (callbackName != null) {
                        dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "','" + "success" + "');");
                    }
                } catch (Exception e) {
                    dynamicUI.getLogger().e("ERROR", " excep: fn__addViewToParent  - " + dynamicUI.getRenderer().getErrorDetails());
                    dynamicUI.getErrorCallback().onException("ERROR", " excep: fn__addViewToParent  - " + dynamicUI.getRenderer().getErrorDetails(), e);
                    if (callbackName != null) {
                        dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "','" + "failure" + "');");
                    }
                }
            });
        } catch (JSONException e) {
            dynamicUI.getLogger().e("JSONERROR", "Error while parsing " + ui);
        }
    }

    public static class PreRenderThread extends Thread {
        public PreRenderThread(Runnable runnable) {
            super(runnable);
            setName("PreRenderThread");
        }
    }

    @JavascriptInterface
    public void prepareAndStoreView(final String screenName, final String ui, final String callbackName) {
        // TODO :: Change to accept as namespace
        try {
            final JSONObject uiObject = new JSONObject(ui);
            PreRenderThread thread = new PreRenderThread(() -> {
                try {
                    setPrepareScreenTaskStatus(screenName, true);
                    dynamicUI.getRenderer().prepareAndStoreView(screenName, uiObject);
                    if (callbackName != null) {
                        dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "','success');");
                    }
                } catch (Exception e) {
                    dynamicUI.getLogger().e("ERROR", " excep: fn__prepareAndStoreView  - " + dynamicUI.getRenderer().getErrorDetails());
                    dynamicUI.getErrorCallback().onException("ERROR", " excep: fn__prepareAndStoreView  - " + dynamicUI.getRenderer().getErrorDetails(), e);
                    if (callbackName != null) {
                        dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "','" + "failure" + "');");
                    }
                }
                setPrepareScreenTaskStatus(screenName, false);
                processPendingAddScreen(screenName);
            });
            thread.start();
        } catch (JSONException e) {
            dynamicUI.getLogger().e("JSONERROR", "Error while parsing " + ui);
        }

    }

    @JavascriptInterface
    public void addStoredViewToParent(final String parentId, final String screenName, final int index, final String callbackName, final boolean replaceChild, final String runInUIprop) {
        addStoredViewToParent(parentId, screenName, index, callbackName, replaceChild, runInUIprop, null);
    }

    @JavascriptInterface
    public void addStoredViewToParent(final String parentId, final String screenName, final int index, final String callbackName, final boolean replaceChild, final String runInUIprop, final String namespace) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (onGoingPrepareScreenSet.contains(screenName)) {
                    // Seems like Prepare is ongoing for the screen we are trying to add
                    // Add this AddScreen to Map so that it is executed after it Prepare is finished
                    pendingAddScreenMap.put(screenName, new PendingAddScreenMapItem(parentId, screenName, index, callbackName, replaceChild, runInUIprop));
                } else {
                    //Prepare Screen is not ongoing. So lets add it.
                    dynamicUI.getRenderer().addStoredViewToParent(parentId, screenName, index, replaceChild, namespace);
                    InflateView inflateView = new InflateJSON(dynamicUI);
                    inflateView.setUseAppContext(true);
                    dynamicUI.getRenderer().parseAndRunPipe(dynamicUI.getAppContext(), runInUIprop, "", "", inflateView.getUseAppContext());

                    if (callbackName != null) {
                        dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "','" + "success" + "');");
                    }
                }
            } catch (Exception e) {
                dynamicUI.getLogger().e("ERROR", " excep: fn__addStoredViewToParent  - " + dynamicUI.getRenderer().getErrorDetails());
                dynamicUI.getErrorCallback().onException("ERROR", " excep: fn__addStoredViewToParent  - " + dynamicUI.getRenderer().getErrorDetails(), e);
            }
        });
    }

    @JavascriptInterface
    public void addViewToParent(final String parentId, final String ui, final int index, final String callbackName) {
        addViewToParent(parentId, ui, index, callbackName, null);
    }

    @JavascriptInterface
    public void addViewToParent(final String parentId, final String ui, final int index, final String callbackName, final String namespace) {
        addViewToParent(parentId, ui, index, callbackName, false, namespace);
    }

    private int findChildIndex(int viewId, ViewGroup parent) {
        int childCount = parent.getChildCount();
        for (int i = 0; i < childCount; i++) {
            if (parent.getChildAt(i).getId() == viewId) {
                return i;
            }
        }
        return -1;
    }


    private boolean replaceViewImpl(View viewToReplaceWith, View viewToReplace) {
        boolean success = false;
        ViewGroup parent = (ViewGroup) viewToReplace.getParent();
        int childIndex = findChildIndex(viewToReplace.getId(), parent);
        if (childIndex != -1) {
            success = true;
            parent.removeViewAt(childIndex);
            parent.addView(viewToReplaceWith, childIndex);
        }
        return success;
    }

    @JavascriptInterface
    public void replaceView(final String viewToReplaceWithStr, final int viewId) {
        replaceView(viewToReplaceWithStr, viewId, null);
    }

    @JavascriptInterface
    public void replaceView(final String viewToReplaceWithStr, final int viewId, final String namespace) {
        try {
            final JSONObject viewToReplaceWithJSON = new JSONObject(viewToReplaceWithStr);
            ExecutorManager.runOnMainThread(() -> {
                try {
                    ViewGroup container = dynamicUI.getContainer(namespace);
                    if (container != null) {
                        View viewToReplaceWith = dynamicUI.getRenderer().createView(viewToReplaceWithJSON);
                        View viewToReplace = container.findViewById(viewId);
                        if (viewToReplace != null) {
                            if (viewToReplace instanceof ViewGroup) {
                                int childrenCount = ((ViewGroup) viewToReplace).getChildCount();
                                for (int i = 0; i < childrenCount; i++) {
                                    View child = ((ViewGroup) viewToReplace).getChildAt(0);
                                    if (child != null) {
                                        ((ViewGroup) viewToReplace).removeViewAt(0);
                                        ((ViewGroup) viewToReplaceWith).addView(child, i);
                                    }
                                }
                            }

                            if (replaceViewImpl(viewToReplaceWith, viewToReplace)) {
                                // optimize for cases of invalidate
                                viewToReplaceWith.requestLayout();
                            }
                        }
                    } else {
                        dynamicUI.getLogger().e("missing_container", "replaceView, no container");
                    }
                } catch (JSONException e) {
                    // IGNORED
                } catch (Exception e) {
                    dynamicUI.getLogger().e(e.getLocalizedMessage(), "excep: fn__replaceView - Error while replaceView " + e);
                }
            });
        } catch (JSONException e) {
            dynamicUI.getLogger().e("JSON_ERROR", "fn__replaceView - " + dynamicUI.getRenderer().getErrorDetails() + " - " + viewToReplaceWithStr);
        }
    }

    @JavascriptInterface
    public void moveView(final String id, final String idx) {
        moveView(id, idx, null);
    }

    @JavascriptInterface
    public void moveView(final String id, final String idx, final String namespace) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                ViewGroup container = dynamicUI.getContainer(namespace);
                if (container != null) {
                    View viewToMove = container.findViewById(Integer.parseInt(id));
                    ViewGroup parentGroup = (ViewGroup) viewToMove.getParent();
                    parentGroup.removeView(viewToMove);
                    parentGroup.addView(viewToMove, Integer.parseInt(idx));
                } else {
                    dynamicUI.getLogger().e("missing_container", "moveView, no container");
                }
            } catch (Exception e) {
                dynamicUI.getLogger().e("ERROR", " fn__moveView - " + dynamicUI.getRenderer().getErrorDetails());
            }
        });
    }

    @JavascriptInterface
    public void removeView(final int id) {
        removeView(id, null);
    }

    @JavascriptInterface
    public void removeView(final int id, final String namespace) {
        ExecutorManager.runOnMainThread(() -> {
            ViewGroup container = dynamicUI.getContainer(namespace);
            if (container != null) {
                View childView = container.findViewById(id);
                if (childView == null) {
                    return;
                }
                ViewGroup parentView = (ViewGroup) childView.getParent();
                parentView.removeView(childView);
            } else {
                dynamicUI.getLogger().e("missing_container", "removeView, no container");
            }
        });
    }

    @JavascriptInterface
    public void runInUI(final String toRun, final String callbackName, final String lineNo, final String fileName) {
        ExecutorManager.runOnMainThread(() -> {
            try {

                InflateView inflateView = new InflateJSON(dynamicUI);
                if (dynamicUI.getActivity() != null) {
                    inflateView.setUseAppContext(false);
                    dynamicUI.getRenderer().parseAndRunPipe(dynamicUI.getActivity(), toRun, lineNo, fileName, inflateView.getUseAppContext());
                } else {
                    inflateView.setUseAppContext(true);
                    dynamicUI.getRenderer().parseAndRunPipe(dynamicUI.getAppContext(), toRun, lineNo, fileName, inflateView.getUseAppContext());
                }
                if (callbackName != null) {
                    dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'success');");
                }
            } catch (Exception e) {
                dynamicUI.getLogger().e("ERROR", " excep: fn__runInUI  - " + dynamicUI.getRenderer().getErrorDetails());
                dynamicUI.getErrorCallback().onException("ERROR", " excep: fn__runInUI  - " + dynamicUI.getRenderer().getErrorDetails(), e);

                if (callbackName != null) {
                    dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'failure');");
                }
            }
        });
    }

    // backward compat
    @JavascriptInterface
    public void runInUI(final String toRun, final String callbackName) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                InflateView inflateView = new InflateJSON(dynamicUI);
                if (dynamicUI.getActivity() != null) {
                    inflateView.setUseAppContext(false);
                    dynamicUI.getRenderer().parseAndRunPipe(dynamicUI.getActivity(), toRun, "", "", inflateView.getUseAppContext());
                } else {
                    inflateView.setUseAppContext(true);
                    dynamicUI.getRenderer().parseAndRunPipe(dynamicUI.getAppContext(), toRun, "", "", inflateView.getUseAppContext());
                }
                if (callbackName != null) {
                    dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'success');");
                }
            } catch (Exception e) {
                String errName = e.getClass().getName();
                dynamicUI.getLogger().e("ERROR", " excep: fn__runInUI  - " + errName + " - " + dynamicUI.getRenderer().getErrorDetails());
                dynamicUI.getErrorCallback().onError("ERROR", " excep: fn__runInUI  - " + errName + " - " + dynamicUI.getRenderer().getErrorDetails());

                if (callbackName != null) {
                    dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'failure');");
                }
            }
        });
    }

    @JavascriptInterface
    public void updateProperties(final String json) {
        updateProperties(json, null);
    }

    @NonNull
    private String getJSONResult(String json) throws Exception {
        JSONArray cmds = new JSONArray(json);
        InflateJSON inflateView = new InflateJSON(dynamicUI);
        inflateView.setUseAppContext(dynamicUI.getActivity() == null);
        Object result = inflateView.runJSON(null, cmds, inflateView.getUseAppContext(), null);
        if (result != null) {
            return result.toString();
        }
        return "_null_";
    }

    @JavascriptInterface
    public void runCmdsInBg(final String json, final String callback) {
        ExecutorManager.runOnBackgroundThread(() -> runJSONWithCallback(json, callback));
    }

    private void runJSONWithCallback(final String json, final String callback) {
        try {
            String result = getJSONResult(json);
            if (callback != null) {
                String decodeUtfWithTryJs = String.format("window.callUICallback('%s',%s);", callback, dynamicUI.encodeUtfAndWrapDecode(result, "ERROR"));
                dynamicUI.addJsToWebView(decodeUtfWithTryJs);
            }
        } catch (Exception e) {
            dynamicUI.getLogger().e("ERROR", " excep: fn__runInUIJSON  - " + dynamicUI.getRenderer().getErrorDetails());
            dynamicUI.getErrorCallback().onException("ERROR", " excep: fn__runInUIJSON  - " + dynamicUI.getRenderer().getErrorDetails(), e);

            if (callback != null) {
                dynamicUI.addJsToWebView("window.callUICallbackJSON(" + callback + ",'failure');");
            }
        }
    }

    @JavascriptInterface
    public void runCmdsInUI(final String json, final String callback) {
        ExecutorManager.runOnMainThread(() -> runJSONWithCallback(json, callback));
    }

    @JavascriptInterface
    @NonNull
    public String runCmds(final String json) {
        try {
            return getJSONResult(json);
        } catch (Exception e) {
            dynamicUI.getLogger().e("ERROR", " excep: fn__runInUIJSON  - " + dynamicUI.getRenderer().getErrorDetails());
            dynamicUI.getErrorCallback().onException("ERROR", " excep: fn__runInUIJSON  - " + dynamicUI.getRenderer().getErrorDetails(), e);
        }
        return "__failure__";
    }

    @JavascriptInterface
    public void updateProperties(final String json, final String namespace) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                ViewGroup container = dynamicUI.getContainer(namespace);
                if (container != null) {
                    JSONObject object = new JSONObject(json);
                    View view = container.findViewById(object.getInt("id"));
                    object.remove("id");
                    InflateView inflateView = new InflateJSON(dynamicUI);
                    inflateView.setUseAppContext(true);
                    for (Iterator<String> it = object.keys(); it.hasNext(); ) {
                        String key = it.next();
                        dynamicUI.getRenderer().getInflateView().parseKeys(key, object, view, inflateView.getUseAppContext());
                    }
                } else {
                    dynamicUI.getLogger().e("missing_container", "updateProperties, no container");
                }
            } catch (Exception e) {
                dynamicUI.getLogger().e(e.getLocalizedMessage(), "excep: fn__updateProperties- Error while updateProperties " + e);
            }
        });
    }

    @JavascriptInterface
    public void run(final String toRun, final String callbackName) {
        try {
            InflateView inflateView = new InflateJSON(dynamicUI);
            if (dynamicUI.getActivity() != null) {
                inflateView.setUseAppContext(false);
                dynamicUI.getRenderer().parseAndRunPipe(dynamicUI.getActivity(), toRun, "", "", inflateView.getUseAppContext());
            } else {
                inflateView.setUseAppContext(true);
                dynamicUI.getRenderer().parseAndRunPipe(dynamicUI.getAppContext(), toRun, "", "", inflateView.getUseAppContext());
            }
            if (callbackName != null) {
                dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'success');");
            }
        } catch (Exception e) {
            String errName = e.getClass().getName();
            dynamicUI.getLogger().e("runInUI", errName);
            dynamicUI.getErrorCallback().onError("runInUI", errName + " - " + dynamicUI.getRenderer().getErrorDetails());
            if (callbackName != null) {
                dynamicUI.addJsToWebView("window.callUICallback(" + callbackName + ",'failure');");
            }
        }
    }

    @JavascriptInterface
    public void saveState(String state) {
        this.state = state;
    }

    @JavascriptInterface
    public String getState() {
        if (this.state != null) {
            return this.state;
        } else {
            return "{}";
        }
    }

    @JavascriptInterface
    public void setState(String state) {
        this.state = state;
    }

    @JavascriptInterface
    public void setImage(final int id, final String base64ImageString, final String namespace) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                ViewGroup container = dynamicUI.getContainer(namespace);
                if (container != null) {
                    ImageView image = container.findViewById(id);
                    byte[] decodedString = Base64.decode(base64ImageString, Base64.DEFAULT);
                    Bitmap decodedByte = BitmapFactory.decodeByteArray(decodedString, 0, decodedString.length);
                    image.setImageBitmap(decodedByte);
                } else {
                    dynamicUI.getLogger().e("missing_container", "setImage, no container");
                }

            } catch (Exception e) {
                dynamicUI.getLogger().e("ERROR", " excep: fn__setImage  - " + dynamicUI.getRenderer().getErrorDetails());
                dynamicUI.getErrorCallback().onException("ERROR", " excep: fn__setImage  - " + dynamicUI.getRenderer().getErrorDetails(), e);
            }
        });
    }

    @JavascriptInterface
    public String fetchData(String key) {
        return dynamicUI.getAppContext().getSharedPreferences("DUI", Context.MODE_PRIVATE).getString(key, "null");
    }

    @JavascriptInterface
    public void saveData(String key, String value) {
        dynamicUI.getAppContext().getSharedPreferences("DUI", Context.MODE_PRIVATE).edit().putString(key, value).apply();
    }

    @JavascriptInterface
    public String getScreenDimensions() {
        JSONObject response = new JSONObject();
        JSONObject viewportDimensions = new JSONObject();
        DisplayMetrics displaymetrics = new DisplayMetrics();
        DisplayMetrics metrics = new DisplayMetrics();
        Rect rectangle = new Rect();
        int screenHeight = 0;
        try {
            if (dynamicUI.getActivity() != null) {
                dynamicUI.getActivity().getWindow().getDecorView().getWindowVisibleDisplayFrame(rectangle);
                dynamicUI.getActivity().getWindowManager().getDefaultDisplay().getMetrics(displaymetrics);

                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                    WindowMetrics windowMetrics = dynamicUI.getActivity().getWindowManager().getCurrentWindowMetrics();
                    screenHeight = windowMetrics.getBounds().height();
                } else {
                    dynamicUI.getActivity().getWindowManager().getDefaultDisplay().getRealMetrics(metrics);
                    screenHeight = metrics.heightPixels;
                }
            } else {
                displaymetrics = Resources.getSystem().getDisplayMetrics();
            }
            response.put("width", displaymetrics.widthPixels);
            response.put("height", displaymetrics.heightPixels);
            response.put("screenHeight", screenHeight);

            viewportDimensions.put("top", rectangle.top);
            viewportDimensions.put("bottom", rectangle.bottom);
            viewportDimensions.put("left", rectangle.left);
            viewportDimensions.put("right", rectangle.right);
            response.put("viewportDimensions", viewportDimensions);
        } catch (JSONException e) {
            dynamicUI.getLogger().e("JSON_EXCEPTION", e.toString());
        }
        return response.toString();
  }

    @JavascriptInterface
    public void toggleKeyboard(final int id, final String type, final String namespace) {
        if (dynamicUI.getActivity() != null) {
            ExecutorManager.runOnMainThread(() -> {
                ViewGroup container = dynamicUI.getContainer(namespace);
                if (container != null) {
                    View view = container.findViewById(id);
                    InputMethodManager inputMethodManager = (InputMethodManager) dynamicUI.getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);

                    if (type.equals("show")) {
                        inputMethodManager.showSoftInput(view, InputMethodManager.SHOW_IMPLICIT);
                    } else {
                        inputMethodManager.hideSoftInputFromWindow(view.getWindowToken(), 0);
                    }
                } else {
                    dynamicUI.getLogger().e("missing_container", "removeView, no container");
                }
            });
        } else {
            dynamicUI.getLogger().e("Missing Activity", "toggleKeyboard, it is not  activity, it is applicationContext");
        }
    }

    @JavascriptInterface
    public void generateUIElement(final String type, final int id, final String[] elements, final String callbackName) {
        generateUIElement(type, id, elements, callbackName, null);
    }

    @JavascriptInterface
    public void generateUIElement(final String type, final int id, final String[] elements, final String callbackName, final String namespace) {
        ExecutorManager.runOnMainThread(() -> {
            ViewGroup container = dynamicUI.getContainer(namespace);
            if (container != null) {
                if (type.equals("PopupMenu")) {
                    View view = container.findViewById(id);
                    view.setOnClickListener(v -> showPopup(v, elements, callbackName));
                }
            } else {
                dynamicUI.getLogger().e("missing_container", "render, no container");
            }
        });
    }

    public void showPopup(View v, String[] elements, final String callbackName) {
        if (dynamicUI.getActivity() != null) {
            PopupMenu popup = new PopupMenu(dynamicUI.getActivity(), v);

            for (int i = 0; i < elements.length; i++) {
                popup.getMenu().add(Menu.NONE, i, Menu.NONE, elements[i]);
            }

            popup.setOnMenuItemClickListener(item -> {
                dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "', '" + item.getItemId() + "');");
                Toast.makeText(dynamicUI.getActivity(), "You Clicked : " + item.getTitle(), Toast.LENGTH_SHORT).show();
                return true;
            });
            popup.show();
        } else {
            dynamicUI.getLogger().e("Missing Activity", "showPopup, it is not  activity, it is applicationContext");
        }
    }

    @JavascriptInterface
    public String getInternalStorageBaseFilePath() {
        return dynamicUI.getAppContext().getDir("juspay", Context.MODE_PRIVATE).getAbsolutePath();
    }

    @JavascriptInterface
    public boolean isFilePresent(String filePath) {
        File file = new File(filePath);
        return file.exists();
    }

    @Deprecated
    @JavascriptInterface
    public void showLoading() {
        Log.d("DynamicUI", "Android.showLoading() method is deprecated. This method does nothing.");
    }

    @JavascriptInterface
    public String getNewID() {
        return String.valueOf(ViewCompat.generateViewId());
    }

    @JavascriptInterface
    public void startAnim(final String animId) {
        startAnim(animId, null);
    }

    @JavascriptInterface
    public void startAnim(final String animId, final String callbackName) {
        final Pair<String, ObjectAnimator> animState = dynamicUI.getRenderer().getInflateView().findAnimationById(animId);
        ExecutorManager.runOnMainThread(() -> {
            try {
                if (animState != null) {
                    if (animState.second != null) {
                        animState.second.start();
                        animState.second.addListener(new Animator.AnimatorListener() {
                            @Override
                            public void onAnimationStart(@NonNull Animator animator) {

                            }

                            @Override
                            public void onAnimationEnd(@NonNull Animator animator) {
                                if (callbackName != null && !callbackName.isEmpty()) {
                                    dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "', '" + animId + "');");
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
            } catch (Exception e) {
                dynamicUI.getLogger().e("JSONERROR", "Error parsing json for animation with id " + animId);
            }
        });
    }

    @JavascriptInterface
    public void cancelAnim(final String animId, final String callbackName) {
        final Pair<String, ObjectAnimator> animState = dynamicUI.getRenderer().getInflateView().getStateValFromKey("M_anim_" + animId);
        final ObjectAnimator objAnim = animState.second;
        ExecutorManager.runOnMainThread(() -> {
            try {
                objAnim.cancel();
                float curVal = (float) objAnim.getAnimatedValue();
                dynamicUI.addJsToWebView("window.callUICallback('" + callbackName + "', '" + curVal + "');");
            } catch (Exception e) {
                dynamicUI.getLogger().e("JSONERROR", "Error parsing json for animation with id " + animId);
            }
        });
    }

    @JavascriptInterface
    public void updateAnim(final int viewId, final String animationStr) {
        updateAnim(viewId, animationStr, null);
    }

    @JavascriptInterface
    public void updateAnim(final int viewId, final String animationStr, final String namespace) {
        try {
            final JSONObject animObj = new JSONObject(animationStr);
            final JSONArray animArr = new JSONArray();
            animArr.put(animObj);
            ExecutorManager.runOnMainThread(() -> {
                try {
                    ViewGroup container = dynamicUI.getContainer(namespace);
                    if (container != null) {
                        View aView = container.findViewById(viewId);
                        dynamicUI.getRenderer().getInflateView().handleAnimation(aView, animArr);
                    } else {
                        dynamicUI.getLogger().e("missing_container", "updateAnim, no container");
                    }
                } catch (Exception e) {
                    dynamicUI.getLogger().e("ERROR", "updateAnim: View doesn't exist for id -" + viewId);
                }
            });
        } catch (JSONException e) {
            dynamicUI.getLogger().e("JSONERROR", "Error parsing json for animation string " + animationStr);
        }
    }

    @JavascriptInterface
    public int dpToPx(int dp) {
        if (dp > 0) {
            DisplayMetrics displayMetrics = dynamicUI.getAppContext().getResources().getDisplayMetrics();
            return Math.round(dp * displayMetrics.density);
        } else {
            return 0;
        }
    }

    @JavascriptInterface
    public String setFragmentContainer(int id, String namespace) {
        ViewGroup container = dynamicUI.getContainer(namespace);
        if (container != null) {
            View a = container.findViewById(id);
            if (a instanceof ViewGroup) {
                return dynamicUI.addToContainerList((ViewGroup) a);
            }
        }
        return "__failed";
    }

    private final static class PendingAddScreenMapItem {
        String parentId;
        String screenName;
        int index;
        String callbackName;
        boolean replaceChild;
        String runInUIprop;

        PendingAddScreenMapItem(String parentId, String screenName, int index, String callbackName, boolean replaceChild, String runInUIprop) {
            this.parentId = parentId;
            this.screenName = screenName;
            this.index = index;
            this.callbackName = callbackName;
            this.replaceChild = replaceChild;
            this.runInUIprop = runInUIprop;
        }
    }

    void setPrepareScreenTaskStatus(final String screenName, final boolean isRunning) {
        ExecutorManager.runOnMainThread(() -> {
            if (isRunning) {
                onGoingPrepareScreenSet.add(screenName);
            } else {
                onGoingPrepareScreenSet.remove(screenName);
            }
        });
    }

    void processPendingAddScreen(final String screenName) {
        ExecutorManager.runOnMainThread(() -> {
            PendingAddScreenMapItem item = pendingAddScreenMap.get(screenName);
            if (item != null) {
                pendingAddScreenMap.remove(screenName);
                addStoredViewToParent(item.parentId, item.screenName, item.index, item.callbackName, item.replaceChild, item.runInUIprop, null);
            }
        });
    }

    @JavascriptInterface
    public String addToContainerList(int id, String namespace) {
        ViewGroup container = dynamicUI.getContainer(namespace);
        if (container != null) {
            if (container.findViewById(id) instanceof ViewGroup)
                return dynamicUI.addToContainerList((ViewGroup) container.findViewById(id));
        }
        return "__failed";
    }
}
