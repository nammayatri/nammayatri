package in.juspay.mobility;

import static in.juspay.hypersdk.core.PaymentUtils.isClassAvailable;

import android.content.Context;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.mobility.app.MobilityAppBridge;
import in.juspay.mobility.customer.MobilityCustomerBridge;
import in.juspay.mobility.driver.MobilityDriverBridge;
import in.juspay.services.HyperServices;
import in.juspay.services.TenantParams;

public class MobilityServices extends HyperServices {

    public MobilityServices(@NonNull Context context) {

        super(context,new TenantParams() {
            @NonNull
            @Override
            public String getBootLoaderEndpoint() {
                return "https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperos/nammayatriconsumer/v1-boot_loader.zip";
            }

            @NonNull
            @Override
            public String getBaseContent() {
                return "<html>\n" +
                        "<head>\n" +
                        "    <title>Mobility App</title>\n" +
                        "</head>\n" +
                        "<body>\n" +
                        "</body>\n" +
                        "<script type=\"text/javascript\">\n" +
                        // We call window.bootLoad() in initiate. This allows to use the web view on demand.
                        "window.bootLoad = function(){\n" +
                        "    window.DUIGatekeeper = JBridge;\n" +
                        "    var headID = document.getElementsByTagName(\"head\")[0];\n" +
                        "    var newScript = document.createElement('script');\n" +
                        "    newScript.type = 'text/javascript';\n" +
                        "    newScript.id = 'boot_loader';\n" +
                        "    var bundleLoadStart = Date.now();\n" +
                        "    var logViaTracker = function() {\n" +
                        "       var bundleLoadEnd = Date.now();\n" +
                        "       window.__osStart = Date.now();\n" +
                        "       var loadLatency = bundleLoadEnd - bundleLoadStart;\n" +
                        "       var obj = {};\n" +
                        "       obj[\"os_bundle_load\"] = {\"bundle_load_start\":bundleLoadStart,\"bundle_load_end\":bundleLoadEnd,\"bundle_load_latency\":loadLatency};\n" +
                        "       window.__osBundleLoadLogLine = obj;\n" +
                        "    }\n" +
                        "    window.onerror = function (message, src, lno, cno, err) {\n" +
                        "       console.log('ERROR WHILE LOADING SCRIPT');\n"+
                        "       const errorObj = {};\n"+
                        "       errorObj.message = typeof message === 'string' ? message : '';\n"+
                        "       errorObj.source = typeof src === 'string' ? src : '';\n"+
                        "       errorObj.lineNo = typeof lno === 'number' ? lno : -1;\n"+
                        "       errorObj.columnNo = typeof cno === 'number' ? cno : -1;\n"+
                        "       if (typeof err === 'object') {\n"+
                        "           errorObj.stackTrace = typeof err.stack === 'string' ? err.stack : '';\n"+
                        "       }\n"+
                        "       window.scriptError = errorObj;\n"+
                        "       var args = JSON.stringify({ app: \"in.juspay.hyperos\", serializedError: JSON.stringify(errorObj)});\n" +
                        "       JBridge.runInJuspayBrowser(\"onScriptError\", args, \"\");\n"+
                        "    };\n" +
                        "    var loadBundle = function () {\n" +
                        "       newScript.innerHTML = JBridge.loadFileInDUI('v1-boot_loader.jsa');\n" +
                        "       headID.appendChild(newScript);\n" +
                        "       logViaTracker();\n" +
                        "    }\n" +
                        "    loadBundle();\n" +
                        "    setTimeout(function () {\n" +
                        "       if (typeof window.onMerchantEvent !== 'function') {\n" +
                        "           loadBundle();\n" +
                        "           var retryObj = {'retry_tried': 'true'};\n" +
                        "           try {\n" +
                        "               window.Analytics._trackLifeCycle('hypersdk')('info')('bundle_load_retry')(retryObj)();\n" +
                        "           } catch (e) {}\n" +
                        "       }\n" +
                        "    }, 1000);\n" +
                        "}\n" +
                        // We call onHtmlReady so that the edge case where web view is not present in initiate is avoided
                        "JBridge.runInJuspayBrowser(\"onHtmlReady\", \"{}\", \"\");\n " +
                        "</script>\n" +
                        "</html>";
            }

            @NonNull
            @Override
            public String getNamespace() {
                return "mobility";
            }

            @NonNull
            @Override
            public List<Class<? extends HyperBridge>> getBridgeClasses() {
                ArrayList<Class<? extends HyperBridge>> bridgeLists = new ArrayList<>();
                if (isClassAvailable("in.juspay.mobility.app.MobilityCustomerBridge")) {
                    bridgeLists.add(MobilityCustomerBridge.class);
                }
                if (isClassAvailable("in.juspay.mobility.app.MobilityDriverBridge")) {
                    bridgeLists.add(MobilityDriverBridge.class);
                }
                if (isClassAvailable("in.juspay.mobility.app.MobilityAppBridge")) {
                    bridgeLists.add(MobilityAppBridge.class);
                }
                return bridgeLists;
            }
        });
    }

}
