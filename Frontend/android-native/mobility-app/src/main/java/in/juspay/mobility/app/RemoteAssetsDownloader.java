/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility.app;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Handler;
import android.os.IBinder;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebViewClient;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;


import androidx.annotation.Nullable;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.UUID;

import in.juspay.hypersdk.core.MerchantViewType;
import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallback;
import in.juspay.mobility.app.callbacks.CallBack;
import in.juspay.services.HyperServices;


public class RemoteAssetsDownloader extends Service {
    HyperServices hyperServices;
    private static ArrayList<CallBack> callBack = new ArrayList<>();

    public static void registerCallback(CallBack notificationCallback) {
        callBack.add(notificationCallback);
    }

    public static void deRegisterCallback(CallBack notificationCallback) {
        callBack.remove(notificationCallback);
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public int onStartCommand(@Nullable Intent intent, int flags, int startId) {
        if (intent != null){
            JSONObject payload = new JSONObject();
            JSONObject outerPayload = new JSONObject();
            String merchantType = intent.getStringExtra("merchantType");
            String bundleType = intent.getStringExtra("bundleType");

            if (bundleType.equals("FCM")) {
                return updateBundle(intent, null, getApplicationContext());
            }

            if (hyperServices == null) {
                hyperServices = new HyperServices(this);
            }
            if (merchantType != null && !hyperServices.isInitialised()) {
                try {
                    payload.put("clientId", getResources().getString(R.string.client_id));
                    payload.put("merchantId", getResources().getString(R.string.merchant_id));
                    payload.put("action", "initiate");
                    payload.put("logLevel", 1);
                    payload.put("isBootable", false);
                    payload.put("bundleTimeOut", -1);
                    payload.put(PaymentConstants.ENV, "prod");
                    outerPayload.put("requestId", UUID.randomUUID());
                    outerPayload.put("service", getService(merchantType));
                    outerPayload.put("betaAssets", false);
                    outerPayload.put(PaymentConstants.PAYLOAD, payload);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                hyperServices.initiate(outerPayload, new HyperPaymentsCallback() {
                    @Override
                    public void onStartWaitingDialogCreated(@Nullable View view) {

                    }

                    @Override
                    public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                        String event = jsonObject.optString("event");
                        System.out.println("onEvent -> " + jsonObject);
                        if (event.equals("terminate")) {
                            hyperServices.terminate();
                            onDestroy();
                        }
                    }

                    @Nullable
                    @Override
                    public View getMerchantView(ViewGroup viewGroup, MerchantViewType merchantViewType) {
                        return null;
                    }

                    @Nullable
                    @Override
                    public WebViewClient createJuspaySafeWebViewClient() {
                        return null;
                    }
                });
            }
        }
        return START_NOT_STICKY;
    }

    @Override
    public void onDestroy() {
        stopSelf();
        callBack = new ArrayList<>();
        hyperServices = null;
        super.onDestroy();

    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
    public String getService(String service) {
        if (service.equals("USER")) {
            return "in.yatri.consumer";
        } else {
            return "in.yatri.provider";
        }
    }

    public static String appendSdkNameAndVersion(String file, Context context) {
        final int dotPosition = file.lastIndexOf('.');
        int versionId = context.getResources().getIdentifier("godel_version", "string", context.getPackageName());
        String version = context.getString(versionId);
        int nameId = context.getResources().getIdentifier("godel_app_name", "string", context.getPackageName());
        String name = context.getString(nameId);

        if (dotPosition > 0 && dotPosition < file.length() - 1) {
            String fileName = file.substring(0, dotPosition);
            String extension = file.substring(dotPosition);

            return fileName + "_" + name + "_" + version + extension;
        } else {
            return file + "_" + name + "_" + version;
        }
    }

    public int updateBundle(Intent intent, JSONObject data, Context context){
        try {
            JSONObject payload ;
            if(intent != null){ payload= new JSONObject(intent.getStringExtra("payload"));}
            else if(data != null){payload = data;}
            else return START_NOT_STICKY;
            JSONArray urls = payload.has("urls") ? payload.getJSONArray("urls"):new JSONArray();
            for (int i = 0; i < urls.length(); i++) {
                String url = urls.getJSONObject(i).has("url") ? urls.getJSONObject(i).getString("url") : "";
                String fileName = "", key = "";
                int index = url.lastIndexOf("/");
                if (index != -1) {
                    fileName = url.substring(index + 1);
                    key = fileName;
                    fileName = appendSdkNameAndVersion(fileName, context);
                }
                String path = urls.getJSONObject(i).has("path") ? urls.getJSONObject(i).getString("path").trim() : null;
                String finalPath;

                Boolean removeHash = urls.getJSONObject(i).has("removeHash") && urls.getJSONObject(i).getBoolean("removeHash");
                if (path != null &&  !(path.length() ==0)) {
                    char lastChar = path.charAt(path.length() - 1);
                    if(lastChar == '/'){
                        finalPath = path+fileName;
                    }else{
                        finalPath = path+'/'+fileName;
                    }
                } else {
                    finalPath = fileName;
                }
                if(finalPath!=null){
                    downloadFile(url, finalPath, key, removeHash, context);
                }
            }
            String description = payload.has("description") ? payload.getString("description") : "";
            String title = payload.has("title") ? payload.getString("title") : "";
            String image = payload.has("image") ? payload.getString("image") : "";
            JSONObject popUpData = new JSONObject();
            popUpData.put("description",description);
            popUpData.put("title",title);
            popUpData.put("image",image);
            for (CallBack cb : callBack) {
                cb.bundleUpdatedCallBack("onBundleUpdated", popUpData);
            }
        } catch (JSONException | InterruptedException e) {
            throw new RuntimeException(e);
        }
        Handler handler = new Handler();
        handler.postDelayed(this::onDestroy, 3000);
        return START_NOT_STICKY;
    }

    public void downloadFile(String fileUrl, String filePath, String key, Boolean removeHash, Context context) throws InterruptedException {
        Thread downloadThread = new Thread(() -> {
            try {
                URL url = new URL(fileUrl);
                HttpURLConnection connection = (HttpURLConnection) url.openConnection();
                connection.setRequestMethod("GET");
                int responseCode = connection.getResponseCode();

                if (responseCode == HttpURLConnection.HTTP_OK) {
                    FileOutputStream outputStream = new FileOutputStream(new File(context.getDir("juspay", Context.MODE_PRIVATE), filePath));
                    InputStream inputStream = connection.getInputStream();
                    byte[] buffer = new byte[4096];
                    int bytesRead;
                    while ((bytesRead = inputStream.read(buffer)) != -1) {
                        outputStream.write(buffer, 0, bytesRead);
                    }
                    inputStream.close();
                    outputStream.close();
                    if(removeHash){
                       SharedPreferences sharedPrefs = context.getApplicationContext().getSharedPreferences(context.getApplicationContext().getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                       String assetMetaData = sharedPrefs.getString("asset_metadata.json", "__failed");
                       if(!assetMetaData.equals("__failed")){
                           JSONObject metaData = new JSONObject(assetMetaData);
                           metaData.remove(key);
                           SharedPreferences.Editor editor = sharedPrefs.edit();
                           editor.putString("asset_metadata.json",metaData.toString());
                           editor.apply();
                       }
                    }

                } else {
                    System.out.println("Failed to download file. Response code: " + responseCode);
                }
                connection.disconnect();
            } catch (IOException e) {
                e.printStackTrace();
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        });
        downloadThread.start();
        downloadThread.join();
    }


}