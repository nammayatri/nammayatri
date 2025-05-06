package in.juspay.mobility.sdk.services;

import static in.juspay.mobility.sdk.core.PaymentConstants.ATTR_HASH_IN_DISK;
import static in.juspay.mobility.sdk.services.ServiceConstants.ASSET_METADATA_FILE_NAME;
import static in.juspay.mobility.sdk.services.ServiceConstants.ATTR_LAST_CHECKED;
import static in.juspay.mobility.sdk.services.ServiceConstants.ATTR_ZIPHASH_IN_DISK;
import static in.juspay.mobility.sdk.services.ServiceConstants.DEF_REMOTE_ASSET_TTL;
import static in.juspay.mobility.sdk.services.ServiceConstants.KEY_REMOTE_ASSET_TTL;

import android.content.Context;
import android.os.AsyncTask;
import android.os.Build;
import android.util.Base64;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.lang.ref.WeakReference;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.util.HashMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogLevel;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.core.JuspayServices;
import in.juspay.mobility.sdk.core.SdkTracker;
import in.juspay.mobility.sdk.data.KeyValueStore;
import in.juspay.mobility.sdk.data.SessionInfo;
import in.juspay.mobility.sdk.security.EncryptionHelper;
import in.juspay.mobility.sdk.utils.network.NetUtils;
import in.juspay.mobility.sdk.utils.network.SessionizedNetUtils;

import dalvik.system.ZipPathValidator;

/**
 * Utility functions that deal with downloading of hot-pushed assets.
 * <p>
 * <em>This file contains the critical functions that are
 * required by the Juspay SDKs to run, and care has to be taken to modify. Please stop if you do not know where you are
 * going.</em>
 *
 * @author Veera Manohara Subbiah [veera.subbiah@juspay.in]
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @since 26/04/2017
 */
public class RemoteAssetService {
    private static final String LOG_TAG = "RemoteAssetService";

    private final String sdkName;
    private JSONObject assetMetadata;
    @NonNull
    private final JuspayServices juspayServices;
    private static final JSONArray fileDownloadTimes = new JSONArray();

    public RemoteAssetService(@NonNull JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
        this.sdkName = juspayServices.getSdkInfo().getSdkName();
    }

    private long getAssetTtl() {
        return Long.parseLong(KeyValueStore.read(juspayServices.getContext(), sdkName, KEY_REMOTE_ASSET_TTL, String.valueOf(DEF_REMOTE_ASSET_TTL)));
    }

    public boolean getContent(@NonNull Context context, String location) throws Exception {
        return getContent(context, location, getAssetTtl());
    }

    private byte[] download(String sourceHash, String location) {
        //HTTP GET to download latest Copy
        HashMap<String, String> queryParam = new HashMap<>();

        queryParam.put("ts", String.valueOf(System.currentTimeMillis()));
        queryParam.put("If-None-Match", sourceHash);
        queryParam.put("Accept-Encoding", "gzip");

        juspayServices.sdkDebug(LOG_TAG, "START fetching content from: " + location);

        byte[] newText = null;

        try {
            NetUtils netUtils = new SessionizedNetUtils(this.juspayServices.getSessionInfo(), 0, 0, false);
            newText = netUtils.fetchIfModified(location, queryParam);
        } catch (Exception e) {
            juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "Error While Downloading File", e);
        }

        return newText;
    }

    private byte[] unZipAndVerify(@NonNull Context context, byte[] newText, String fileName) {
        final FileProviderService fileProviderService = juspayServices.getFileProviderService();
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        // If zip file, unzip it  and check for signature
        if (newText != null && fileName.contains(".zip")) {
            //Open Stream to unzip downloaded bytes
            try (InputStream downloadedStream = new ByteArrayInputStream(newText)) {
                try (ZipInputStream zin = new ZipInputStream(downloadedStream)) {
                    ZipEntry distinctFiles;

                    // From UPSIDE_DOWN_CAKE ZipPathValidator checks if ZipInputStream.getNextEntry() has any ".." in the filePath or if the filePath starts with "/" and throws a ZipException if it has any of them, so calling clearCallback skips this validation.
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.UPSIDE_DOWN_CAKE) {
                        ZipPathValidator.clearCallback();
                    }
                    try {
                        byte[] fileToBeVerified = null;
                        byte[] signature = null;
                        while ((distinctFiles = zin.getNextEntry()) != null) {
                            ByteArrayOutputStream bufout = new ByteArrayOutputStream();
                            byte[] buffer = new byte[1024];
                            int read;
                            while ((read = zin.read(buffer)) != -1) {
                                bufout.write(buffer, 0, read);
                            }
                            zin.closeEntry();
                            bufout.close();
                            if (distinctFiles.getName().contains("signature")) {
                                signature = Base64.decode(bufout.toByteArray(), Base64.NO_WRAP);
                            } else if (distinctFiles.getName().contains(".jsa") || (fileName.contains("certificate") && distinctFiles.getName().contains(".zip"))) {
                                fileToBeVerified = bufout.toByteArray();
                            }
                        }

                        try (ObjectInputStream keyIn = new ObjectInputStream(new ByteArrayInputStream(fileProviderService.getAssetFileAsByte(context, "remoteAssetPublicKey")))) {
                            //Access Public Key
                            PublicKey pubkey = (PublicKey) keyIn.readObject();

                            //Verify Algorithm
                            Signature verifyalg = Signature.getInstance("DSA");
                            verifyalg.initVerify(pubkey);

                            //Verify Signature
                            verifyalg.update(fileToBeVerified);

                            if (!verifyalg.verify(signature)) {
                                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.REMOTE_ASSET_SERVICE, "signature_not_verified", fileName);
                                return null;
                            } else {
                                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.REMOTE_ASSET_SERVICE, "signature_verified", fileName);
                                return fileToBeVerified;
                            }
                        } catch (ClassNotFoundException e) {
                            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "Exception while Reading Public Key", e);
                        } catch (NoSuchAlgorithmException e) {
                            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "DSA Algorithm not found", e);
                        } catch (InvalidKeyException e) {
                            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "Key Used was Invalid", e);
                        } catch (SignatureException e) {
                            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "Exception while matching Signature for file", e);
                        }
                    } catch (Exception e) {
                        sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "Exception while verifying Signature", e);
                    }
                }
            } catch (IOException e) {
                sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "IOException while verifying Signature", e);
            }
            return null;
        }
        return newText;
    }

    private String decideAndUpdateInternalStorage(@NonNull Context context, byte[] newText, String sourceHash, String unzippedFileName) {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        final FileProviderService fileProviderService = juspayServices.getFileProviderService();
        boolean isFileUpdated = false;

        String newHash = EncryptionHelper.md5(newText);

        if (newHash == null) {
            newHash = "";
        }

        juspayServices.sdkDebug(LOG_TAG, "hashInDisk: " + sourceHash);
        juspayServices.sdkDebug(LOG_TAG, "newHash: " + newHash);

        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.REMOTE_ASSET_SERVICE, "remote_asset_service_update_hash", "Hash of used file '" + unzippedFileName + "' is now " + newHash);
        if (sourceHash != null && sourceHash.equals(newHash)) {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.REMOTE_ASSET_SERVICE, "remote_asset_service_compare_hash", "Remote hash is same as disk hash. Not updating asset '" + unzippedFileName + "'");
        } else {
            sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.REMOTE_ASSET_SERVICE, "remote_asset_service_compare_hash", "Remote hash differs from disk hash. Updating asset '" + unzippedFileName + "'");
            isFileUpdated = fileProviderService.updateFile(context, unzippedFileName, newText);
        }

        return isFileUpdated ? newHash : null;
    }

    private String unzipAndUpdateInternalStorage(@NonNull Context context, byte[] certificates) throws IOException {
        final FileProviderService fileProviderService = juspayServices.getFileProviderService();

        //Open Stream to unzip downloaded bytes
        String newHash = EncryptionHelper.md5(certificates);

        if (newHash == null) {
            newHash = "";
        }

        try (ZipInputStream zin = new ZipInputStream(new ByteArrayInputStream(certificates))) {
            ZipEntry distinctFiles;
            while ((distinctFiles = zin.getNextEntry()) != null) {
                String unzippedFileName = distinctFiles.getName();
                if (distinctFiles.isDirectory()) {
                    continue;
                }

                try (ByteArrayOutputStream buffer = new ByteArrayOutputStream()) {
                    for (int c = zin.read(); c != -1; c = zin.read()) {
                        buffer.write(c);
                    }
                    fileProviderService.updateCertificate(context, unzippedFileName, buffer.toByteArray());
                }
            }
        }
        return newHash;
    }

    private boolean getContent(@NonNull Context context, String location, long ttl) throws Exception {
        return getContent(context, location, null, ttl);
    }

    private boolean getContent(@NonNull Context context, String location, String fileName, long ttl) throws Exception {
        final SessionInfo sessionInfo = juspayServices.getSessionInfo();
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        final FileProviderService fileProviderService = juspayServices.getFileProviderService();

        int index = location.lastIndexOf("/");

        if (fileName == null) {
            fileName = location.substring(index + 1);
        }

        String unzippedFileName = fileName.replace(".zip", ".jsa");
        JSONObject assetMetadata = getMetadata(unzippedFileName);
        String sourceHash = "", zipHash = "";

        boolean fileModified = false;

        if (null != assetMetadata.getString(ATTR_LAST_CHECKED)) {
            sourceHash = assetMetadata.getString(ATTR_HASH_IN_DISK);
            zipHash = assetMetadata.getString(ATTR_ZIPHASH_IN_DISK);
        } else {
            if (!fileName.contains(".zip")) {
                try (FileInputStream fileInputStream = new FileInputStream(fileProviderService.readFromFile(context, fileName, false))) {
                    sourceHash = EncryptionHelper.md5(fileInputStream);
                }
            }
        }

        byte[] newText = download(zipHash, location);

        if (newText != null) {
            fileModified = true;
            zipHash = EncryptionHelper.md5(newText);
        }

        newText = unZipAndVerify(context, newText, fileName);

        if (newText == null) {
            if (!fileModified) {
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.REMOTE_ASSET_SERVICE, "remote_asset_service_etag_match", "ETAG matches for '" + fileName + "'. Not downloading from " + location);
                return false;
            }

            newText = EncryptionHelper.v1Encrypt(fileProviderService.readFromFile(context, unzippedFileName, false).getBytes());
        }
        if (newText != null) {
            juspayServices.sdkDebug(LOG_TAG, "DONE fetching content from: " + location);
            juspayServices.sdkDebug(LOG_TAG, "Text: " + new String(newText));
        }

        String newHash = decideAndUpdateInternalStorage(context, newText, sourceHash, unzippedFileName);

        if (newHash != null) {
            assetMetadata.put(ATTR_LAST_CHECKED, System.currentTimeMillis());
            assetMetadata.put(ATTR_HASH_IN_DISK, newHash);
            assetMetadata.put(ATTR_ZIPHASH_IN_DISK, zipHash);

            setMetadata(unzippedFileName, assetMetadata);
        }
        return true;
    }

    public void renewFile(@NonNull Context context, final String location, final String callback, final String fileName, final long startTime) {
        renewFile(context, location, callback, getAssetTtl(), fileName, startTime);
    }

    public void renewFile(@NonNull Context context, final String location, final String callback, final long startTime) {
        renewFile(context, location, callback, getAssetTtl(), null, startTime);
    }

    public void renewFile(@NonNull Context context, final String location, @Nullable final String callback, final long ttlInMilliSeconds, final String fileName, final long startTime) {
        juspayServices.sdkDebug(LOG_TAG, "Looking to renew file: " + location);
        new AssetDownloadTask(context, location, fileName, callback, ttlInMilliSeconds, this, startTime).executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR);
    }

    private void updateCertificates(@NonNull Context context, String location, long ttl) throws JSONException, IOException {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();

        JSONObject assetMetadata = getMetadata(location);
        int index = location.lastIndexOf("/");
        String fileName = location.substring(index + 1);
        String sourceHash = "";
        String newHash = "";
        String zipHash = "";
        boolean fileModified = false;

        if (null != assetMetadata.getString(ATTR_LAST_CHECKED)) {
            sourceHash = assetMetadata.getString(ATTR_HASH_IN_DISK);
            zipHash = assetMetadata.getString(ATTR_ZIPHASH_IN_DISK);
        }

        byte[] certificates = download(zipHash, location);
        if (certificates != null) {
            fileModified = true;
            zipHash = EncryptionHelper.md5(certificates);
        }

        certificates = unZipAndVerify(context, certificates, fileName);

        juspayServices.sdkDebug(LOG_TAG, "DONE fetching content from: " + location);
        juspayServices.sdkDebug(LOG_TAG, "hashInDisk: " + sourceHash);
        juspayServices.sdkDebug(LOG_TAG, "newHash: " + newHash);

        sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.REMOTE_ASSET_SERVICE, "remote_asset_service_update_hash", "Hash of used file '" + fileName + "' is now " + newHash);

        if (certificates == null) {
            if (!fileModified) {
                sdkTracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.INFO, Labels.System.REMOTE_ASSET_SERVICE, "remote_asset_service_etag_match", "ETAG matches for '" + fileName + "'. Not downloading from " + location);
            }
        } else {
            newHash = unzipAndUpdateInternalStorage(context, certificates);
            assetMetadata.put(ATTR_LAST_CHECKED, System.currentTimeMillis());
            assetMetadata.put(ATTR_HASH_IN_DISK, newHash);
            assetMetadata.put(ATTR_ZIPHASH_IN_DISK, zipHash);
            setMetadata(location, assetMetadata);
        }
    }

    public synchronized JSONObject getMetadata(String location) throws JSONException {
        final SdkTracker sdkTracker = juspayServices.getSdkTracker();
        try {
            assetMetadata = new JSONObject(KeyValueStore.read(juspayServices.getContext(), sdkName, ASSET_METADATA_FILE_NAME, "{}"));
        } catch (JSONException e) {
            sdkTracker.trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "Exception trying to read from KeyStore: " + ASSET_METADATA_FILE_NAME, e);
            throw new RuntimeException("Unexpected internal error.", e);
        }

        juspayServices.sdkDebug(LOG_TAG, "assetMetadata: " + assetMetadata);

        if (!assetMetadata.has(location)) {
            assetMetadata.put(location, new JSONObject());
            ((JSONObject) assetMetadata.get(location)).put(ATTR_LAST_CHECKED, 0);
            ((JSONObject) assetMetadata.get(location)).put(ATTR_HASH_IN_DISK, "");
            ((JSONObject) assetMetadata.get(location)).put(ATTR_ZIPHASH_IN_DISK, "");
        }

        return (JSONObject) assetMetadata.get(location);
    }

    private synchronized void setMetadata(String location, JSONObject assetMetadata) throws JSONException {
        if (this.assetMetadata == null) {
            getMetadata(location);
        }

        this.assetMetadata.put(location, assetMetadata);
        KeyValueStore.write(juspayServices.getContext(), sdkName, ASSET_METADATA_FILE_NAME, this.assetMetadata.toString());
    }

    public synchronized void resetMetadata(String location) throws JSONException {
        if (assetMetadata == null) {
            getMetadata(location);
        }

        assetMetadata.remove(location);
        KeyValueStore.write(juspayServices.getContext(), sdkName, ASSET_METADATA_FILE_NAME, assetMetadata.toString());
    }

    /**
     * Class for the async task that downloads assets from the server.
     *
     * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
     * @since 09/12/2019
     */
    private static class AssetDownloadTask extends AsyncTask<Void, Void, Boolean> {
        private String location;
        private String fileName;
        private String callback;
        private long ttlInMilliSeconds;
        private final WeakReference<Context> contextWeakReference;
        private long renewFileStartTime;
        private RemoteAssetService remoteAssetService;

        AssetDownloadTask(@NonNull Context context, String location, String fileName, String callback, long ttlInMilliSeconds, RemoteAssetService remoteAssetService, long time) {
            this.location = location;
            this.fileName = fileName;
            this.callback = callback;
            this.ttlInMilliSeconds = ttlInMilliSeconds;
            this.remoteAssetService = remoteAssetService;
            this.contextWeakReference = new WeakReference<>(context);
            this.renewFileStartTime = time;
        }

        @Override
        protected Boolean doInBackground(Void... voids) {
            Context context = contextWeakReference.get();
            if (context != null) {
                try {
                    if (!location.contains("certificates")) {
                        return remoteAssetService.getContent(context, location, fileName, ttlInMilliSeconds);
                    } else {
                        remoteAssetService.updateCertificates(context, location, ttlInMilliSeconds);
                    }
                } catch (Exception e) {
                    remoteAssetService.juspayServices
                            .getSdkTracker()
                            .trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.REMOTE_ASSET_SERVICE, "Could not renew file " + location + ": " + e.getMessage(), e);
                }
            }

            return false;
        }

        @Override
        protected void onPostExecute(Boolean downloaded) {
            super.onPostExecute(downloaded);

            long renewFileEndTime = System.currentTimeMillis();
            long totalFileDownloadTime = renewFileEndTime - this.renewFileStartTime;
            JSONObject fileDownLoadTimeObj = new JSONObject();
            try {
                fileDownLoadTimeObj.put("startTime", renewFileStartTime);
                fileDownLoadTimeObj.put("endTime", renewFileEndTime);
                fileDownLoadTimeObj.put("totalTime", totalFileDownloadTime);
                fileDownLoadTimeObj.put("fileName", this.fileName);
            } catch (JSONException ignored) {
            }
            fileDownloadTimes.put(fileDownLoadTimeObj);
            if (callback != null) {
                String commandFmt = "window.callUICallback('%s', '%b', '%s', '%s');";
                String command = String.format(commandFmt, callback, downloaded, location, remoteAssetService.juspayServices.getFileProviderService().appendSdkNameAndVersion(fileName));
                remoteAssetService.juspayServices.sdkDebug(LOG_TAG, command);
                remoteAssetService.juspayServices.addJsToWebView(command);
            }
        }
    }

    public JSONArray getFileDownloadTimes() {
        return fileDownloadTimes;
    }
}
