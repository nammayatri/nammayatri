package in.juspay.mobility.sdk.services;

import android.content.Context;
import android.os.Environment;

import androidx.annotation.NonNull;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import in.juspay.mobility.sdk.hyper.constants.Labels;
import in.juspay.mobility.sdk.hyper.constants.LogCategory;
import in.juspay.mobility.sdk.hyper.constants.LogLevel;
import in.juspay.mobility.sdk.hyper.constants.LogSubCategory;
import in.juspay.mobility.sdk.hyper.core.FileProviderInterface;
import in.juspay.mobility.sdk.hyper.core.JuspayLogger;
import in.juspay.mobility.sdk.R;
import in.juspay.mobility.sdk.core.JuspayServices;
import in.juspay.mobility.sdk.core.PaymentConstants;
import in.juspay.mobility.sdk.core.SdkTracker;
import in.juspay.mobility.sdk.data.KeyValueStore;
import in.juspay.mobility.sdk.data.SdkInfo;
import in.juspay.mobility.sdk.security.EncryptionHelper;

/**
 * A class that contains helper methods for files.
 *
 * @author Sahil Dave [sahil.dave@juspay.in]
 * @author Sri Harsha Chilakapati [sri.harsha@juspay.in]
 * @author Dayanidhi D [dayanidhi.d@juspay.in]
 * @since 14/03/2017
 */
public class FileProviderService implements FileProviderInterface {
    private static final String LOG_TAG = "FileProviderService";

    @NonNull
    private final Map<String, String> fileCache = new HashMap<>();
    @NonNull
    private final List<String> fileCacheWhiteList = new ArrayList<>();
    @NonNull
    private final JuspayServices juspayServices;
    private final boolean shouldCheckInternalAssets = true;

    public FileProviderService(@NonNull JuspayServices juspayServices) {
        this.juspayServices = juspayServices;
    }

    public void addToFileCacheWhiteList(String fileName) {
        fileCacheWhiteList.add(fileName);
    }

    @Override
    @NonNull
    public String readFromFile(@NonNull Context context, @NonNull String fileName) {
        return readFromFile(context, fileName, true);
    }

    @Override
    public void renewFile(@NonNull String endPoint, @NonNull String fileName, long startTime) {
        juspayServices.getRemoteAssetService().renewFile(juspayServices.getContext(), endPoint, null, fileName, startTime);
    }

    public String readFromFile(@NonNull Context context, String fileName, boolean useCache) {
        String data = null;
        if (useCache) {
            data = readFromCache(fileName);
        }

        if (data == null && shouldCheckInternalAssets) {
            data = readFromInternalStorage(context, fileName);
        }

        if (data == null) {
            data = readFromAssets(context, fileName);
        }

        if (fileCacheWhiteList.contains(fileName) && data != null) {
            cacheFile(fileName, data);
        }

        return data == null ? "" : data;
    }

    public String readFromCache(String fileName) {
        if (isFileCached(fileName)) {
            final String returnData = fileCache.get(fileName);

            juspayServices.sdkDebug(LOG_TAG, "Returning cached value of the file: " + fileName);
            juspayServices.sdkDebug(LOG_TAG, "Cached: " + returnData);

            return returnData;
        }

        return null;
    }

    private String readFromInternalStorage(@NonNull Context context, String realFileName) {
        String fileName = appendSdkNameAndVersion(realFileName);

        if (juspayServices.getSdkInfo().usesLocalAssets()) {
            return null;
        }

        StringBuilder returnString = new StringBuilder();

        SdkTracker tracker = juspayServices.getSdkTracker();
        try {
            if (fileName.endsWith("jsa")) {
                byte[] returnData = decryptGunzipInternalStorage(context, fileName);
                if (returnData != null) {
                    return new String(returnData);
                }
                tracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.WARNING, Labels.System.FILE_PROVIDER_SERVICE, "readFromInternalStorage", "Returning null from internal storage for " + fileName);
                return null;
            }

            try (FileInputStream fis = new FileInputStream(getFileFromInternalStorage(context, fileName))) {
                try (InputStreamReader isr = new InputStreamReader(fis)) {
                    try (BufferedReader input = new BufferedReader(isr)) {
                        int num;
                        while ((num = input.read()) != -1) {
                            returnString.append((char) num);
                        }
                    }
                }
                tracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.DEBUG, Labels.System.FILE_PROVIDER_SERVICE, "readFromInternalStorage", "Returning the file content without decryption for " + fileName);
                return returnString.toString();
            }
        } catch (Exception e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "read from internal storage failed", e);
        }

        return null;
    }

    private String readFromAssets(@NonNull Context context, String fileName) {
        final SdkTracker tracker = juspayServices.getSdkTracker();

        try {
            byte[] encrypted = getAssetFileAsByte(context, fileName);

            if (fileName.endsWith("jsa")) {
                //  get decrypted data
                juspayServices.sdkDebug(LOG_TAG, "Read JSA Asset file " + fileName + " with encrypted hash - " + EncryptionHelper.md5(encrypted));
                byte[] decrypted = EncryptionHelper.decryptThenGunzip(encrypted, context.getResources().getString(R.string.juspay_encryption_version));

                return new String(decrypted);
            } else {
                juspayServices.sdkDebug(LOG_TAG, "Done reading " + fileName + " from assets");
                return new String(encrypted);
            }
        } catch (Exception e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Exception trying to read from file: " + fileName, e);
            return null;
        }
    }

    private void cacheFile(String fileName, String fromInternalStorage) {
        fileCache.put(fileName, fromInternalStorage);
        juspayServices.sdkDebug(LOG_TAG, "Caching file: " + fileName);
    }

    private boolean isFileCached(String fileName) {
        return fileCache.containsKey(fileName);
    }

    @SuppressWarnings("UnusedReturnValue")
    public boolean updateFile(@NonNull Context context, String fileName, byte[] content) {
        return writeToFile(context, fileName, content, false);
    }

    @SuppressWarnings("UnusedReturnValue")
    public boolean updateCertificate(@NonNull Context context, String fileName, byte[] content) {
        return writeToFile(context, fileName, content, true);
    }

    private void updateFallback(@NonNull Context context, String realFileName, String fileName) {
        if (fileName.endsWith("jsa") && isFilePresent(context, fileName)) {
            juspayServices.sdkDebug(LOG_TAG, "updateFallback: starting" + fileName + "  " + realFileName);
            try {
                byte[] returnData = decryptGunzipInternalStorage(context, fileName);
                String hash = EncryptionHelper.md5(returnData);

                String bh = KeyValueStore.read(juspayServices, PaymentConstants.JP_BLOCKED_HASH, "{}"); // read the blocked hash
                JSONObject blocked_hashes = new JSONObject(bh);
                juspayServices.sdkDebug(LOG_TAG, "updateFallback: got the blocked hash");

                if (blocked_hashes.has(realFileName)) {
                    juspayServices.sdkDebug(LOG_TAG, "updateFallback: got the file name " + realFileName);
                    JSONObject blocked_hash = blocked_hashes.getJSONObject(realFileName);

                    if (!(blocked_hash.has("latest_hash") && blocked_hash.getString("latest_hash").equals(hash))) {
                        // if there is no blocked hash or  if it is there it is not equivalent to the current hash then
                        juspayServices.sdkDebug(LOG_TAG, "updateFallback: wonderful.. copying to the fallback");
                        copyFile(context, fileName, "fb/" + fileName);

                        blocked_hash.remove("latest_hash");
                        blocked_hashes.put(realFileName, blocked_hash);
                        KeyValueStore.write(juspayServices, PaymentConstants.JP_BLOCKED_HASH, blocked_hashes.toString());

                        juspayServices.sdkDebug(LOG_TAG, "updateFallback: file copied");
                    }
                } else {
                    // if there is no blocked hash at all then
                    copyFile(context, fileName, "fb/" + fileName);

                    juspayServices.sdkDebug(LOG_TAG, "updateFallback: we didn;t get the file name from blocked hash " + fileName);
                    juspayServices.sdkDebug(LOG_TAG, "updateFallback: wonderful.. copying to the fallback");
                    juspayServices.sdkDebug(LOG_TAG, "updateFallback: file copied");
                }
            } catch (FileNotFoundException e) {
                juspayServices.getSdkTracker().trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.HyperSdk.AUTO_FALLBACK, "File not found: " + fileName, e);
            } catch (Exception e) {
                juspayServices.getSdkTracker().trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.HyperSdk.AUTO_FALLBACK, "Exception: " + fileName, e);
            }
        }
    }

    private void copyFile(@NonNull Context context, String inputFileName, String outputFileName) {
        try {
            //create output directory if it doesn't exist
            createJuspayDir(context);
            createRequiredDir(context, outputFileName);

            juspayServices.sdkDebug(LOG_TAG, "copyFile: " + inputFileName + "   " + outputFileName);

            try (InputStream in = new FileInputStream(getFileFromInternalStorage(context, inputFileName))) {
                try (OutputStream out = new FileOutputStream(getFileFromInternalStorage(context, outputFileName))) {
                    byte[] buffer = new byte[1024];
                    int read;
                    while ((read = in.read(buffer)) != -1) {
                        out.write(buffer, 0, read);
                    }
                    // write the output file (You have now copied the file)
                    out.flush();
                }
            }

        } catch (FileNotFoundException e) {
            juspayServices.getSdkTracker().trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "File not found: " + inputFileName, e);
        } catch (Exception e) {
            juspayServices.getSdkTracker().trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Exception: " + inputFileName, e);
        }
    }

    private boolean writeToFile(@NonNull Context context, String realFileName, byte[] content, boolean isCertificate) {
        String fileName = appendSdkNameAndVersion(realFileName);
        updateFallback(context, realFileName, fileName);
        deleteFileFromCache(fileName);
        final SdkTracker tracker = juspayServices.getSdkTracker();

        try {
            createJuspayDir(context);
            createRequiredDir(context, fileName);

            if (isCertificate) {
                createCertDir(context);
            }
            try (FileOutputStream fos = new FileOutputStream(getFileFromInternalStorage(context, fileName))) {
                fos.write(content);
            }
            return true;
        } catch (FileNotFoundException e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "File not found: " + fileName, e);
        } catch (IOException e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "IOException: " + fileName, e);
        } catch (Exception e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Exception: " + fileName, e);
        }

        return false;
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private void createRequiredDir(@NonNull Context context, String fileName) {

        if (fileName.contains("/")) {
            File file = new File(context.getDir("juspay", Context.MODE_PRIVATE), fileName.substring(0, fileName.lastIndexOf("/")));

            if (!file.exists()) {
                file.mkdirs();
            }
        }
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private void createJuspayDir(@NonNull Context context) {

        File file = new File(context.getCacheDir(), "juspay");

        if (!file.exists()) {
            file.mkdir();
        }
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private void createCertDir(@NonNull Context context) {

        File file = new File(context.getDir("juspay", Context.MODE_PRIVATE), "certificates_v1");

        if (!file.exists()) {
            file.mkdir();
        }
    }

    public boolean isFilePresent(@NonNull Context context, String fileName) {
        if (shouldCheckInternalAssets) {
            File file = new File(context.getDir("juspay", Context.MODE_PRIVATE), appendSdkNameAndVersion(fileName));

            if (file.exists()) {
                return true;
            }
        }

        try (InputStream is = context.getResources().getAssets().open("juspay/" + fileName)) {
            return is != null;
        } catch (IOException e) {
            return false;
        }
    }

    public String appendSdkNameAndVersion(String file) {
        final SdkInfo sdkInfo = juspayServices.getSdkInfo();
        final int dotPosition = file.lastIndexOf('.');

        if (dotPosition > 0 && dotPosition < file.length() - 1) {
            String fileName = file.substring(0, dotPosition);
            String extension = file.substring(dotPosition);

            return fileName + "_" + sdkInfo.getSdkName() + "_" + sdkInfo.getSdkVersion() + extension;
        } else {
            return file + "_" + sdkInfo.getSdkName() + "_" + sdkInfo.getSdkVersion();
        }
    }

    private File getFileFromInternalStorage(@NonNull Context context, String fileName) {

        juspayServices.sdkDebug(LOG_TAG, "Context while reading Internal Storage :" + context);
        juspayServices.sdkDebug(LOG_TAG, "Getting file from internal storage. Filename: " + fileName);

        return new File(context.getDir("juspay", Context.MODE_PRIVATE), fileName);
    }

    public byte[] getInternalStorageFileAsByte(Context context, String fileName) throws FileNotFoundException {
        final SdkTracker tracker = juspayServices.getSdkTracker();
        final RemoteAssetService remoteAssetService = juspayServices.getRemoteAssetService();

        try {
            try (ByteArrayOutputStream ous = new ByteArrayOutputStream()) {
                try (InputStream ios = new FileInputStream(getFileFromInternalStorage(context, fileName))) {
                    readFromInputStream(ous, ios);
                }
                return ous.toByteArray();
            }
        } catch (FileNotFoundException e) {
            juspayServices.sdkDebug(LOG_TAG, "File not found " + fileName);

            try {
                remoteAssetService.resetMetadata(fileName.replace(".zip", ".jsa"));
            } catch (JSONException e1) {
                tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Couldn't reset " + fileName, e);
            }

            throw e;
        } catch (IOException e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Could not read " + fileName, e);
            deleteFileFromInternalStorage(context, fileName);

            throw new RuntimeException(e);
        } catch (Exception e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Exception: Could not read " + fileName, e);
            deleteFileFromInternalStorage(context, fileName);

            throw new RuntimeException(e);
        }
    }

    public byte[] getAssetFileAsByte(@NonNull Context context, String fileName) {
        final SdkTracker tracker = juspayServices.getSdkTracker();

        try {
            try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
                try (InputStream is = context.getResources().getAssets().open("juspay/" + fileName)) {
                    readFromInputStream(bos, is);
                }
                return bos.toByteArray();
            }
        } catch (FileNotFoundException e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Could not read " + fileName, e);
            throw new RuntimeException(e);
        } catch (IOException e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Could not read " + fileName, e);
            deleteFileFromInternalStorage(context, fileName);

            throw new RuntimeException(e);
        } catch (Exception e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Exception: Could not read " + fileName, e);
            deleteFileFromInternalStorage(context, fileName);
        }

        return new byte[]{};
    }

    private void readFromInputStream(ByteArrayOutputStream bos, InputStream is) throws IOException {
        byte[] buffer = new byte[4096];
        int read;

        while ((read = is.read(buffer)) != -1) {
            bos.write(buffer, 0, read);
        }
    }

    @SuppressWarnings("UnusedReturnValue")
    public boolean deleteFileFromInternalStorage(Context context, String fileName) {
        final SdkTracker tracker = juspayServices.getSdkTracker();
        final RemoteAssetService remoteAssetService = juspayServices.getRemoteAssetService();

        File corruptedFile = getFileFromInternalStorage(context, fileName);

        if (corruptedFile.exists()) {
            juspayServices.sdkDebug(LOG_TAG, "Deleting " + fileName + " from internal storage");
            JuspayLogger.e(LOG_TAG, "FILE CORRUPTED. DISABLING SDK");
            tracker.trackAction(LogSubCategory.Action.SYSTEM, LogLevel.WARNING, Labels.System.FILE_PROVIDER_SERVICE, "file_corrupted", fileName);

            try {
                remoteAssetService.resetMetadata(fileName.replace(".zip", ".jsa"));
            } catch (Exception e) {
                tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Error while resetting etag", e);
            }

            return corruptedFile.delete();
        } else {
            JuspayLogger.e(LOG_TAG, fileName + " not found");
            return false;
        }
    }

    private void deleteFileFromCache(String fileName) {
        if (isFileCached(fileName)) {
            fileCache.remove(fileName);
        }
    }

    @SuppressWarnings("unused")
    public byte[] decryptGunzipAssetFile(@NonNull Context context, String fileName) {
        final SdkTracker tracker = juspayServices.getSdkTracker();

        byte[] encrypted = new byte[0];
        try {
            encrypted = getAssetFileAsByte(context, fileName);
        } catch (Exception e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Exception in reading " + fileName + " from assets", e);
        }
        return EncryptionHelper.decryptThenGunzip(encrypted, context.getResources().getString(R.string.juspay_encryption_version));
    }

    public byte[] decryptGunzipInternalStorage(@NonNull Context context, String fileName) throws FileNotFoundException {
        final SdkTracker tracker = juspayServices.getSdkTracker();

        byte[] encrypted;
        try {
            encrypted = getInternalStorageFileAsByte(context, fileName);
            juspayServices.sdkDebug(LOG_TAG, "Read Encrypted file from internalStorage - " + fileName
                    + " with encrypted hash - " + EncryptionHelper.md5(encrypted));
            return EncryptionHelper.decryptThenGunzip(encrypted, context.getResources().getString(R.string.juspay_encryption_version));
        } catch (FileNotFoundException e) {
            juspayServices.sdkDebug(LOG_TAG, "No File to decrypt in internal storage: " + fileName);
            throw e;
        } catch (Exception e) {
            tracker.trackException(LogCategory.ACTION, LogSubCategory.Action.SYSTEM, Labels.System.FILE_PROVIDER_SERVICE, "Exception in reading " + fileName + " from internal storage", e);
            return null;
        }
    }

    public String writeFileToDisk(@NonNull Context context, String data, String fileName) {
        try {
            File externalFilesDir = context.getExternalFilesDirs(Environment.DIRECTORY_DOWNLOADS)[0];
            File path = new File(externalFilesDir.getAbsolutePath());

            //noinspection ResultOfMethodCallIgnored
            path.mkdirs();
            File file = new File(path, fileName);
            //noinspection ResultOfMethodCallIgnored
            file.createNewFile();
            if (file.exists()) {
                FileWriter fileWriter = new FileWriter(file);
                fileWriter.write(data);
                fileWriter.flush();
                fileWriter.close();
                JSONObject respData = new JSONObject();
                respData.put("error", "false");
                respData.put("data", path);
                return respData.toString();
            } else {
                String error = "Exception in creating the file";
                JuspayLogger.d(LOG_TAG, error);
                return String.format("{\"error\":\"true\",\"data\":\"%s\"}", "unknown_error::" + error);
            }
        } catch (Exception e) {
            JuspayLogger.d(LOG_TAG, "Exception in downloading the file :" + e);
            return String.format("{\"error\":\"true\",\"data\":\"%s\"}", "unknown_error::" + e);
        }
    }
}
