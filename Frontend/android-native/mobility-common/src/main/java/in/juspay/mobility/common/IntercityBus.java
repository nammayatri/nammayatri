package in.juspay.mobility.common;

import android.webkit.JavascriptInterface;
import android.webkit.WebView;

import java.util.Objects;

import in.juspay.hyper.core.BridgeComponents;

public class IntercityBus {
    String redBusPermissionCallback;
    String pSPermissionRequestCallback;
    private BridgeComponents bridgeComponents;


    @JavascriptInterface
    public void requestMobileNumberPermission(String cbFunction){
        redBusPermissionCallback = cbFunction;
        String triggerEvent = String.format("window.callUICallback(%s)", pSPermissionRequestCallback);
        Objects.requireNonNull(bridgeComponents.getJsCallback()).addJsToWebView(triggerEvent);
    }

    public void setBridgeComponents(BridgeComponents bridgeComponents){
        this.bridgeComponents = bridgeComponents;
    }

    public void sentNumberToIntercityBus(String phoneNumber, String webViewId){
        WebView webView = bridgeComponents.getActivity().findViewById(Integer.parseInt(webViewId));
        webView.evaluateJavascript(redBusPermissionCallback + "(" + phoneNumber + ")", null);
    }

    public void setPSPermissionRequestCallback(String cb){
        pSPermissionRequestCallback = cb;
    }
}
