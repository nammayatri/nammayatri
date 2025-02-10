package in.juspay.mobility;

import android.app.Activity;
import android.content.Context;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentActivity;

import java.util.ArrayList;
import java.util.List;

import in.juspay.hyper.bridge.HyperBridge;
import in.juspay.services.HyperServices;
import in.juspay.services.TenantParams;

public class MobilityServices extends HyperServices {


    public MobilityServices(@NonNull Context context) {
        super(context, new TenantParams() {
            @Override
            public String getBootLoaderEndpoint() {
                return null;
            }

            @NonNull
            @Override
            public String getBaseContent() {
                return "<html>\n" +
                        "\n" +
                        "<head>\n" +
                        "    <title>MOBILITY-RELEASE</title>\n" +
                        "</head>\n" +
                        "\n" +
                        "<body>\n" +
                        "</body>\n" +
                        "<script type=\"text/javascript\">\n" +
                        "    window.__OS = \"ANDROID\";\n" +
                        "    window.JBridge = top.JBridge;\n" +
                        "    window.Android = top.Android;\n" +
                        "    window.DUIGatekeeper = JBridge;\n" +
                        "    window.JBridge.setAnalyticsHeader(JSON.stringify({ \"x-client-id\": \"mobility\" }));\n" +
                        "</script>\n" +
                        "\n" +
                        "<script type=\"text/javascript\">\n" +
                        "    let headID = document.getElementsByTagName(\"head\")[0];\n" +
                        "    window.prevTimeStamp = Date.now();\n" +
                        "   window.isDev =  " +  BuildConfig.flavor.equals("dev") + ";" +
                        "    window.assetDownloadDuration = Date.now();\n" +
                        "    let jsFile = JBridge.loadFileInDUI('v1-assets_downloader.jsa'); // update index_bundle.js to share apk\n" +
                        "    window.assetDownloadDurationEnd = Date.now();\n" +
                        "    let newScript = document.createElement('script');\n" +
                        "    newScript.type = 'text/javascript';\n" +
                        "    newScript.id = 'mystique';\n" +
                        "    newScript.innerHTML = jsFile;\n" +
                        "    headID.appendChild(newScript);\n" +
                        "\n" +
                        "</script>\n" +
                        "\n" +
                        "</html>\n";
            }

            @NonNull
            @Override
            public String getNamespace() {
                return "";
            }

            @NonNull
            @Override
            public List<Class<? extends HyperBridge>> getBridgeClasses() {
                return new ArrayList<>();
            }
        });
    }

}







