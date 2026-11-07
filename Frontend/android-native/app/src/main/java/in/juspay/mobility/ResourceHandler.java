package in.juspay.mobility;

import android.annotation.SuppressLint;
import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import in.juspay.hyper.constants.LogCategory;
import in.juspay.hyper.constants.LogSubCategory;
import in.juspay.hypersdk.core.JuspayServices;

public class ResourceHandler {

    @NonNull
    private final Context context;
    @NonNull
    private final JuspayServices juspayServices;

    private final String LOG_TAG = this.getClass().getSimpleName();

    public ResourceHandler(@NonNull Context context) {
        this.context = context;
        this.juspayServices = new JuspayServices(context,null);
    }

    protected String getFromAssets (String fileName) {
        return juspayServices.getFileProviderService().readFromFile(context,fileName);
    }

    @SuppressLint("DiscouragedApi")
    protected boolean isResourcePresent (String type, String fileName) {
        return (context.getResources().getIdentifier(fileName, type, context.getPackageName()) != 0);
    }


    /**
     * Make sure the file exist before calling this function by isResourcePresent
     * @param fileName Resource file name
     * @return raw file in String format
     */
    @SuppressLint("DiscouragedApi")
    @Nullable
    protected String getRawResource (String fileName) {
        int id = context.getResources().getIdentifier(fileName, "raw", context.getPackageName());
        try {
            InputStream inputStream = context.getResources().openRawResource(id);
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            readFromInputStream(byteArrayOutputStream,inputStream);
            return byteArrayOutputStream.toString();
        } catch (Exception e) {
            juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, "GET_JSON_RESOURCE", "Exception while reading " + fileName + " from raw", e);
        }
        return null;
    }

    @SuppressLint("DiscouragedApi")
    @Nullable
    protected InputStream getRawResourceStream (String fileName) {
        int id = context.getResources().getIdentifier(fileName, "raw", context.getPackageName());
        try {
            return context.getResources().openRawResource(id);
        } catch (Exception e) {
            juspayServices.getSdkTracker().trackAndLogException(LOG_TAG, LogCategory.ACTION, LogSubCategory.Action.SYSTEM, "GET_JSON_RESOURCE", "Exception while reading " + fileName + " from raw" , e);
        }
        return null;
    }


    private void readFromInputStream(ByteArrayOutputStream bos, InputStream is) throws IOException {
        byte[] buffer = new byte[4096];
        int read;

        while ((read = is.read(buffer)) != -1) {
            bos.write(buffer, 0, read);
        }
    }


    /**
     * close should be called after the resource operations are done.
     */
    public void close () {
        this.juspayServices.terminate();
    }

}
