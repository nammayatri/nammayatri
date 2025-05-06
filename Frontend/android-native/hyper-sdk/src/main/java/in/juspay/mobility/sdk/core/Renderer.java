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

import android.content.Context;
import android.view.TextureView;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RadioButton;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.google.android.flexbox.FlexboxLayout;

import org.json.JSONArray;
import org.json.JSONObject;

import java.lang.reflect.Constructor;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import in.juspay.mobility.sdk.hyper.core.ExecutorManager;
import in.juspay.mobility.sdk.services.SdkConfigService;


/**
 * Created by sahebjot on 4/10/16.
 */

public class Renderer {
    @NonNull
    private final HashMap<String, View> prevView;
    @NonNull
    private final HashMap<String, ViewGroup> container;
    @NonNull
    private final DynamicUI dynamicUI;
    private int viewCacheCapacity;

    @NonNull
    ConcurrentHashMap<String, List<View>> viewCache = new ConcurrentHashMap<>();

    //Never make it public - Should not be accessible outside this package
    Renderer(@NonNull DynamicUI dynamicUI) {
        this.dynamicUI = dynamicUI;
        this.container = new HashMap<>();
        this.prevView = new HashMap<>();
        try {
            this.viewCacheCapacity = SdkConfigService.getCachedSdkConfig().getJSONObject("uiFeatures").getJSONObject("nbListItemCaching").getInt("bgCacheCapacity");
        } catch (Exception e) {
            this.viewCacheCapacity = 4;
        }
        try {
            initCache();
        } catch (Exception e) {
            dynamicUI.getLogger().e("Error while initializing cache", e.toString());
        }
    }

    public void dismissPopUp() {
        dynamicUI.getInflateView().dismissPopUp();
    }

    public void renderUI(final JSONObject jsonUI, ViewGroup container, String namespace) throws Exception {
        renderUI(jsonUI, container, true, namespace);
    }

    private void initCache() {
        String[] classes = {"android.widget.RelativeLayout", "android.widget.LinearLayout", "android.widget.ImageView", "android.widget.ScrollView", "android.widget.TextView", "android.widget.EditText", "android.widget.FrameLayout"};
        ExecutorManager.runOnBackgroundThread(() -> {
            List<View> views = Collections.synchronizedList(new ArrayList<>());
            for (String className : classes) {
                try {
                    for (int i = 0; i < viewCacheCapacity; i++) {
                        Class<?> clazz = Class.forName(className);
                        Constructor<?> ctor = clazz.getConstructor(Context.class);
                        View view = (View) ctor.newInstance(Renderer.this.dynamicUI.getAppContext());
                        views.add(view);
                    }
                    viewCache.put(className, views);
                    views = Collections.synchronizedList(new ArrayList<>());
                } catch (Exception e) {
                    dynamicUI.getLogger().e("Error while initializing cache in function", e.toString());
                }
            }
        });

    }

    public View getCachedViewFor(String className) {
        List<View> views = viewCache.get(className);
        if (views == null) {
            return null;
        }
        int size = views.size();
        if (size == 0) {
            replenishCache(className);
            return null;
        }
        View view = views.remove(0);
        if (size < viewCacheCapacity) {
            replenishCache(className);
        }
        return view;
    }

    public void replenishCache(final String className) {
        ExecutorManager.runOnBackgroundThread(() -> {
            List<View> views = viewCache.get(className);
            if (views == null) {
                views = Collections.synchronizedList(new ArrayList<>());
                viewCache.put(className, views);
            }
            if (views.size() < viewCacheCapacity) {
                try {
                    Class<?> clazz = Class.forName(className);
                    Constructor<?> constructor = clazz.getConstructor(Context.class);
                    Context context = dynamicUI.getActivity() != null ? dynamicUI.getActivity() : dynamicUI.getAppContext();
                    View view = (View) constructor.newInstance(context);
                    views.add(view);
                } catch (Exception ignored) {
                }
            }
        });
    }

    public void renderUI(JSONObject jsonUI, ViewGroup container, boolean shouldFlush, String namespace) throws Exception {
        if (namespace == null) {
            namespace = "default";
        }
        this.container.put(namespace, container);
        Queue<RenderTreeNode> queue = new LinkedList<>();
        InflateView inflateView = new InflateJSON(dynamicUI);
        inflateView.setUseAppContext(true);
        View newView = createAllNodesAndReturnRoot(jsonUI, queue, inflateView.getUseAppContext());

        if (shouldFlush && prevView.get(namespace) != newView) {
            removeViewFromContainer(prevView.get(namespace), namespace);
        }

        addViewFromRenderTreeNodeQueue(queue);
        render(newView, namespace);
        prevView.put(namespace, newView);
    }

    private static class RenderTreeNode {
        ViewGroup parent;
        View itself;

        RenderTreeNode(ViewGroup parent, View itself) {
            this.parent = parent;
            this.itself = itself;
        }
    }

    private void addViewFromRenderTreeNodeQueue(Queue<RenderTreeNode> queue) {
        while (!queue.isEmpty()) {
            RenderTreeNode currentNode = queue.poll();
            if (currentNode != null) {
                currentNode.parent.addView(currentNode.itself);
            }
        }
    }

    public void addViewToParent(String parentStrId, JSONObject ui, int index, boolean replaceChild, final String namespace) throws Exception {
        int parentId = dynamicUI.getAppContext().getResources().getIdentifier(parentStrId, "id", dynamicUI.getAppContext().getPackageName());
        ViewGroup container = dynamicUI.getContainer(namespace);
        if (index >= 0 && container != null) {
            ViewGroup parentView = container.findViewById(parentId);
            if (replaceChild) {
                parentView.removeAllViews();
            }

            /*
             * We will create all the elements (objects of Views) first and then
             * add it all together to make a tree. Then we will add the new
             * sub tree to the parent given.
             */
            Queue<RenderTreeNode> queue = new LinkedList<>();
            InflateView inflateView = new InflateJSON(dynamicUI);
            inflateView.setUseAppContext(true);
            View newSubRoot = createAllNodesAndReturnRoot(ui, queue, inflateView.getUseAppContext());

            addViewFromRenderTreeNodeQueue(queue);

            parentView.addView(newSubRoot, index);
        } else {
            if (container == null) {
                dynamicUI.getLogger().e("Missing Container", "addViewToParent, InflateView, it is not  activity, it is applicationContext");
            }
            if (ui.has("props")) {
                setCurrentNodeDetails(ui.getString("type"), ui.getJSONObject("props"));
            }
            dynamicUI.getErrorCallback().onError("ERROR", " isNull : fn__addViewToParent - negative index " + getErrorDetails());
        }
    }

    public void prepareAndStoreView(String screenName, JSONObject ui) throws Exception {
        InflateView inflateView = new InflateJSON(dynamicUI);
        inflateView.setUseAppContext(true);
        View newSubRoot = createNodesAndReturnRoot(ui, inflateView);
        dynamicUI.addToScreenMap(screenName, newSubRoot);
    }

    public void addStoredViewToParent(String parentStrId, String screenName, int index, boolean replaceChild, final String namespace) {
        int parentId = dynamicUI.getAppContext().getResources().getIdentifier(parentStrId, "id", dynamicUI.getAppContext().getPackageName());
        if (index >= 0) {
            ViewGroup container = dynamicUI.getContainer(namespace);
            if (container == null) {
                dynamicUI.getErrorCallback().onError("ERROR", " isNull : fn__addViewToParent - container null " + getErrorDetails());
                return;
            }
            ViewGroup parentView = container.findViewById(parentId);
            if (replaceChild) {
                parentView.removeAllViews();
            }

            View newSubRoot = (View) dynamicUI.getViewFromScreenName(screenName);
            if (newSubRoot != null) {
                if (newSubRoot.getParent() != null) {
                    ((ViewGroup) newSubRoot.getParent()).removeView(newSubRoot); // <- fix
                }
                parentView.addView(newSubRoot, index);
            } else {
                dynamicUI.getErrorCallback().onError("ERROR", " isNull : fn__addViewToParent - child null " + getErrorDetails());
            }
        } else {
            dynamicUI.getErrorCallback().onError("ERROR", " isNull : fn__addViewToParent - negative index " + getErrorDetails());
        }
    }

    private View createNodesAndReturnRoot(JSONObject ui, InflateView inflateView) throws Exception {
        String viewType = ui.getString("type");
        JSONObject properties = ui.getJSONObject("props");

        if (ui.has("props")) {
            setCurrentNodeDetails(viewType, properties);
        }

        Object instance = getNewInstanceFromClassName(viewType);
        Iterator<String> keys = properties.keys();
        String key;

        while (keys.hasNext()) {
            key = keys.next();
            inflateView.parseKeys(key, properties, instance, inflateView.getUseAppContext());
        }

        JSONArray children;
        JSONObject child;
        children = ui.getJSONArray("children");
        if (children.length() != 0) {
            for (int i = 0; i < children.length(); i++) {
                child = children.getJSONObject(i);
                View childView = createNodesAndReturnRoot(child, inflateView);
                ((ViewGroup) instance).addView(childView);
            }
        }

        return (View) instance;
    }

    private View createAllNodesAndReturnRoot(JSONObject ui, Queue<RenderTreeNode> queue, boolean useApplCtx) throws Exception {
        String viewType = ui.getString("type");
        JSONObject properties = ui.getJSONObject("props");

        if (ui.has("props")) {
            setCurrentNodeDetails(viewType, properties);
        }

        Object instance = getNewInstanceFromClassName(viewType);
        Iterator<String> keys = properties.keys();
        String key;

        while (keys.hasNext()) {
            key = keys.next();
            dynamicUI.getInflateView().parseKeys(key, properties, instance, useApplCtx);
        }

        JSONArray children;
        JSONObject child;
        children = ui.getJSONArray("children");
        if (children.length() != 0) {
            for (int i = 0; i < children.length(); i++) {
                child = children.getJSONObject(i);
                View childView = createAllNodesAndReturnRoot(child, queue, useApplCtx);
                queue.add(new RenderTreeNode((ViewGroup) instance, childView));
            }
        }

        return (View) instance;
    }

    public void setCurrentNodeDetails(String viewType, JSONObject properties) throws Exception {
        dynamicUI.getInflateView().setCurrView(viewType);

        if (properties.has("node_id")) {
            String viewId = properties.getString("node_id");
            dynamicUI.getInflateView().setCurrViewId(viewId);
        }

        if (properties.has("__filename")) {
            String __filename = properties.getString("__filename");
            dynamicUI.getInflateView().setFileOrigin(__filename);
        }
    }

    private Object getNewInstanceFromClassName(String clz) throws Exception {
        Context context = dynamicUI.getAppContext();
        View view = getCachedViewFor(clz);
        if (view != null) {
            return view;
        }
        switch (clz) {
            case "android.widget.RelativeLayout":
                return new RelativeLayout(context);
            case "android.widget.LinearLayout":
                return new LinearLayout(context);
            case "android.widget.FrameLayout":
                return new FrameLayout(context);
            case "android.widget.ScrollView":
                return new ScrollView(context);
            case "android.widget.TextView":
                return new TextView(context);
            case "android.widget.Button":
                return new Button(context);
            case "android.widget.EditText":
                return new EditText(context);
            case "android.widget.ImageView":
                return new ImageView(context);
            case "android.widget.ImageButton":
                return new ImageButton(context);
            case "android.widget.CheckBox":
                return new CheckBox(context);
            case "android.widget.RadioButton":
                return new RadioButton(context);
            case "android.view.TextureView":
                return new TextureView(context);
            case "androidx.swiperefreshlayout.widget.SwipeRefreshLayout":
                return new SwipeRefreshLayout(context);
            case "com.google.android.flexbox.FlexBoxLayout":
                return new FlexboxLayout(context);
            default:
                Class<?> cls = getInflateView().getClassName(clz);
                Constructor<?> ct = cls.getConstructor(Context.class);
                Object instance;
                instance = ct.newInstance(context);
                return instance;
        }
    }

    public View createView(JSONObject ui) throws Exception {
        String viewType = ui.getString("type");
        JSONObject properties = ui.getJSONObject("props");

        if (ui.has("props")) {
            setCurrentNodeDetails(viewType, properties);
        }

        Object instance = getNewInstanceFromClassName(viewType);
        Iterator<String> keys = properties.keys();
        String key;
        InflateView inflateView = new InflateJSON(dynamicUI);
        inflateView.setUseAppContext(true);
        while (keys.hasNext()) {
            key = keys.next();
            dynamicUI.getInflateView().parseKeys(key, properties, instance, inflateView.getUseAppContext());
        }

        JSONArray children;
        JSONObject child;
        children = ui.getJSONArray("children");
        if (children.length() != 0) {
            for (int i = 0; i < children.length(); i++) {
                child = children.getJSONObject(i);
                View childView = createView(child);
                if (childView != null) {
                    ((ViewGroup) instance).addView(childView);
                }
            }
        }

        return (View) instance;
    }

    private void render(final View viewInstance, String namespace) {
        ViewGroup container = this.container.get(namespace);
        if (viewInstance != null && container != null) {
            container.addView(viewInstance);
        } else {
            dynamicUI.getErrorCallback().onError("ERROR", " isNull : fn__Render -  instance null " + getErrorDetails());
        }
    }

    private void removeViewFromContainer(final View toRemove, String namespace) {
        ViewGroup container = this.container.get(namespace);
        if (container != null) {
            int index = container.indexOfChild(toRemove);
            container.removeViewAt(index);
        }
    }

    public String getErrorDetails() {
        return dynamicUI.getInflateView().getErrorDetails();
    }

    public void parseAndRunPipe(Object instance, String toParse, String lineNo, String fileName, boolean useApplCtx) throws Exception {
        dynamicUI.getInflateView().setCurrView("modifyDom");
        dynamicUI.getInflateView().setCurrViewId("");
        dynamicUI.getInflateView().setFileOrigin("ln: " + lineNo + " " + fileName);
        dynamicUI.getInflateView().parseAndRunPipe(instance, toParse, useApplCtx);
    }

    public InflateView getInflateView() {
        return dynamicUI.getInflateView();
    }
}
