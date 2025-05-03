//package in.juspay.hypersdk.core;
//
//import android.content.Context;
//import android.util.Log;
//
//import androidx.annotation.NonNull;
//import androidx.annotation.Nullable;
//import androidx.javascriptengine.JavaScriptIsolate;
//import androidx.javascriptengine.JavaScriptSandbox;
//
//import com.google.common.util.concurrent.ListenableFuture;
//
//import java.util.concurrent.ExecutionException;
//
//public class JavascriptEngine implements JSEngine {
//
//    Context mContext;
//    static ListenableFuture<JavaScriptSandbox> jsSandboxFuture;
//
//    @Nullable
//    JavaScriptIsolate jsIsolate;
//
//    public static void createIsolate(Context mContext) {
//        jsSandboxFuture = JavaScriptSandbox.createConnectedInstanceAsync(mContext);
//    }
//
//    public JavascriptEngine(Context mContext) {
//        this.mContext = mContext;
//        try {
//            this.jsIsolate = jsSandboxFuture.get().createIsolate();
//        } catch (ExecutionException | InterruptedException e) {
//            System.out.println("Exception while creating isolate");
//        }
//    }
//
//    @Override
//    public void addJavascriptInterface(Object object, String name) {
////        jsIsolate.
//    }
//
//    @Override
//    public void evaluateJavascript(String js) {
//        if (jsIsolate != null) {
//            jsIsolate.evaluateJavaScriptAsync(js);
//        }
//    }
//
//    @Override
//    public void loadDataWithBaseURL(String baseUrl, String data, String mimeType, String encoding, String historyUrl) {
//
//    }
//}
