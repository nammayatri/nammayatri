package in.juspay.mobility.common;

import android.content.Context;

import java.io.File;

public class FileUtils {

        public static File[] getFilesInFolder(Context context, String folderName) {
            File dir = new File(context.getFilesDir(), folderName);
    
            if (dir.exists() && dir.isDirectory()) {
                return dir.listFiles();
            } else {
                return null;
            }
        }

        public static File[] getFilesInFolderPath(String folderPath) {
            File dir = new File(folderPath);

            if (dir.exists() && dir.isDirectory()) {
                return dir.listFiles();
            } else {
                return null;
        }

    }
}
