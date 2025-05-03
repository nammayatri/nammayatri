package in.juspay.hypersdk.core;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull; // Use androidx annotations if possible
import androidx.annotation.Nullable;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Utility class for saving and reading byte arrays to/from the application's cache directory.
 */
public class CacheUtils {

    private static final String TAG = "CacheUtils"; // Tag for logging

    /**
     * Saves a byte array to a file in the application's internal cache directory.
     *
     * Note: Files in the cache directory may be deleted by the system when
     * storage space is needed. Do not rely on this for persistent storage.
     *
     * IMPORTANT: This performs file I/O and should NOT be called on the main thread.
     * Use a background thread (e.g., ExecutorService, AsyncTask - though deprecated, RxJava).
     *
     * @param context  The application or activity context. Cannot be null.
     * @param filename A unique name for the cache file (e.g., "user_avatar_data", "config_cache.bin").
     *                 Should not be null or empty. Avoid path separators ('/').
     * @param data     The byte array to save. Cannot be null.
     * @return true if the file was saved successfully, false otherwise.
     */
    public static boolean saveBytesToCache(@NonNull Context context, @NonNull String filename, @NonNull byte[] data) {
        if (filename.isEmpty()) {
            Log.e(TAG, "Filename cannot be empty.");
            return false;
        }

        File cacheDir = context.getCacheDir();
        if (cacheDir == null) {
            Log.e(TAG, "Could not get cache directory.");
            return false; // Should not happen normally
        }

        File file = new File(cacheDir, filename);

        // Use try-with-resources to ensure the stream is closed automatically
        try (OutputStream outputStream = new FileOutputStream(file)) {
            outputStream.write(data);
            Log.d(TAG, "Successfully saved " + data.length + " bytes to cache file: " + file.getAbsolutePath());
            return true; // Indicate success
        } catch (IOException e) {
            Log.e(TAG, "Error saving bytes to cache file: " + file.getAbsolutePath(), e);
            // Optional: Attempt to delete partially written file on error
            if (file.exists()) {
                file.delete();
            }
            return false; // Indicate failure
        } catch (Exception e) {
            // Catch other potential exceptions (e.g., SecurityException)
            Log.e(TAG, "Unexpected error saving to cache file: " + file.getAbsolutePath(), e);
            if (file.exists()) {
                file.delete();
            }
            return false; // Indicate failure
        }
    }


    /**
     * Reads a byte array from a file in the application's internal cache directory.
     *
     * IMPORTANT: This performs file I/O and should NOT be called on the main thread.
     *
     * @param context The application or activity context. Cannot be null.
     * @param filename The unique name of the cache file used when saving. Should not be null or empty.
     * @return The byte array if the file exists and was read successfully, null otherwise.
     */
    @Nullable // Indicates the method can return null
    public static byte[] readBytesFromCache(@NonNull Context context, @NonNull String filename) {
        if (filename.isEmpty()) {
            Log.e(TAG, "Filename cannot be empty.");
            return null;
        }

        File cacheDir = context.getCacheDir();
        if (cacheDir == null) {
            Log.e(TAG, "Could not get cache directory.");
            return null;
        }

        File file = new File(cacheDir, filename);

        if (!file.exists() || !file.isFile()) {
            Log.w(TAG, "Cache file not found or is not a file: " + file.getAbsolutePath());
            return null;
        }

        // Check file size before allocating byte array (optional but good practice)
        long fileSize = file.length();
        if (fileSize > Integer.MAX_VALUE) {
            Log.e(TAG, "Cache file is too large to read into a byte array: " + fileSize + " bytes");
            return null;
        }

        // Use try-with-resources for FileInputStream
        // Use ByteArrayOutputStream to read data efficiently
        try (InputStream inputStream = new FileInputStream(file);
             ByteArrayOutputStream byteOutputStream = new ByteArrayOutputStream((int) fileSize)) { // Initialize with expected size

            byte[] buffer = new byte[4096]; // Or another reasonable buffer size
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                byteOutputStream.write(buffer, 0, bytesRead);
            }

            byte[] result = byteOutputStream.toByteArray();
            Log.d(TAG, "Successfully read " + result.length + " bytes from cache file: " + file.getAbsolutePath());
            return result;

        } catch (IOException e) {
            Log.e(TAG, "Error reading bytes from cache file: " + file.getAbsolutePath(), e);
            return null;
        } catch (OutOfMemoryError e) {
            Log.e(TAG, "Out of memory reading cache file: " + file.getAbsolutePath(), e);
            return null; // Handle OOM specifically if file size check wasn't sufficient
        } catch (Exception e) {
            Log.e(TAG, "Unexpected error reading from cache file: " + file.getAbsolutePath(), e);
            return null;
        }
    }


    // --- Example Usage (using a simple ExecutorService) ---
    /*

    import java.util.concurrent.ExecutorService;
    import java.util.concurrent.Executors;
    import android.os.Handler;
    import android.os.Looper;

    // Inside your Activity or Fragment

    private final ExecutorService executor = Executors.newSingleThreadExecutor(); // Manage lifecycle appropriately
    private final Handler mainHandler = new Handler(Looper.getMainLooper());

    private void cacheSomeDataInBackground(Context context, byte[] myData) {
        executor.execute(() -> {
            // Background work
            boolean success = CacheUtils.saveBytesToCache(context.getApplicationContext(), "my_cached_data.bin", myData);

            // Post result back to the main thread
            mainHandler.post(() -> {
                if (success) {
                    Log.i("MyApp", "Data successfully cached.");
                    // Update UI or state
                } else {
                    Log.e("MyApp", "Failed to cache data.");
                    // Show error message
                }
            });
        });
    }

    private void loadCachedDataInBackground(Context context) {
        executor.execute(() -> {
             // Background work
            byte[] cachedData = CacheUtils.readBytesFromCache(context.getApplicationContext(), "my_cached_data.bin");

            // Post result back to the main thread
            mainHandler.post(() -> {
                if (cachedData != null) {
                    Log.i("MyApp", "Loaded " + cachedData.length + " bytes from cache.");
                    // processMyData(cachedData);
                } else {
                    Log.w("MyApp", "Data not found in cache.");
                    // fetchFreshData();
                }
            });
        });
    }

    // Remember to shut down the executor when your component is destroyed
    @Override
    protected void onDestroy() {
        super.onDestroy();
        executor.shutdown(); // Or shutdownNow() depending on needs
    }

    */

}