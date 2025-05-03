package in.juspay.hypersdk.core;


public interface JSEngine {
    void addJavascriptInterface(Object object, String name);
    void evaluateJavascript(String js);
    void loadDataWithBaseURL( String baseUrl, String data,
                         String mimeType, String encoding, String historyUrl);
}
