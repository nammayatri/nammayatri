package in.juspay.mobility.utils;

import android.content.Context;
import dalvik.system.DexFile;
import java.io.FileReader;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Enumeration;
import in.juspay.mobility.utils.FindDebugger;
import in.juspay.mobility.utils.FindEmulator;
import android.content.pm.ApplicationInfo;
import java.io.File;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import android.os.Build;
import android.content.pm.PackageManager;
import java.util.List;
import java.util.Arrays;
import android.util.Log;
import android.app.ActivityManager;

public class RootDetection {
    public static boolean basicRootCheck() {
        return checkRootMethod1() || checkRootMethod2() || checkRootMethod3();
    }

    private static boolean checkRootMethod1() {
        String buildTags = android.os.Build.TAGS;
        return  buildTags == null || ((buildTags != null) && buildTags.contains("test-keys"));
    }

    private static boolean checkRootMethod2() {
        String[] paths = {  
                "/system/app/Superuser.apk",
                "/system/app/UnRoot.apk",
                "/system/app/Nakup.apk",
                "/sbin/su",
                "/system/bin/su",
                "/system/xbin/su",
                "/data/local/xbin/su",
                "/data/local/bin/su",
                "/system/sd/xbin/su",
                "/system/bin/failsafe/su",
                "/data/local/su",
                "/su/bin/su"
            };
            
        for (String path : paths) {
            if (new File(path).exists()) return true;
        }

        for(String pathDir : System.getenv("PATH").split(":")){
            if(new File(pathDir, "su").exists()) {
                return true;
            }
        }
        
        return false;
    }

    private static boolean checkRootMethod3() {
        Process process = null;
        try {
            process = Runtime.getRuntime().exec(new String[] { "/system/xbin/which", "su" });
            BufferedReader in = new BufferedReader(new InputStreamReader(process.getInputStream()));
            if (in.readLine() != null) return true;
            return false;
        } catch (Throwable t) {
            return false;
        } finally {
            if (process != null) process.destroy();
        }
    }
    
    public boolean isQEmuEnvDetected(Context context) {
        if (FindEmulator.hasKnownDeviceId(context)
        || FindEmulator.hasKnownImsi(context)
        || FindEmulator.hasEmulatorBuild(context)
        || FindEmulator.hasKnownPhoneNumber(context) || FindEmulator.hasPipes()
        || FindEmulator.hasQEmuDrivers() || FindEmulator.hasEmulatorAdb()
        || FindEmulator.hasQEmuFiles()
        || FindEmulator.hasGenyFiles()) {
            return true;
        } else {
            return false;
        }
    }
    
    public boolean isEmulator() {
        return (Build.BRAND.startsWith("generic") && Build.DEVICE.startsWith("generic"))
                || Build.FINGERPRINT.startsWith("generic")
                || Build.FINGERPRINT.startsWith("unknown")
                || Build.HARDWARE.contains("goldfish")
                || Build.HARDWARE.contains("ranchu")
                || Build.MODEL.contains("google_sdk")
                || Build.MODEL.contains("Emulator")
                || Build.MODEL.contains("Android SDK built for x86")
                || Build.MANUFACTURER.contains("Genymotion")
                || Build.PRODUCT.contains("sdk_google")
                || Build.PRODUCT.contains("google_sdk")
                || Build.PRODUCT.contains("sdk")
                || Build.PRODUCT.contains("sdk_x86")
                || Build.PRODUCT.contains("sdk_gphone64_arm64")
                || Build.PRODUCT.contains("vbox86p")
                || Build.PRODUCT.contains("emulator")
                || Build.PRODUCT.contains("simulator");
    }

    public boolean hookDetected(Context context) {
        PackageManager packageManager = context.getPackageManager();
        List<ApplicationInfo> applicationInfoList = packageManager.getInstalledApplications(PackageManager.GET_META_DATA);
        String[] dangerousPackages = {"de.robv.android.xposed.installer", "com.saurik.substrate", "de.robv.android.xposed"};

        if (applicationInfoList != null) {
            for (ApplicationInfo applicationInfo : applicationInfoList) {
                if (Arrays.asList(dangerousPackages).contains(applicationInfo.packageName)) {
                    return true;
                }
            }
        }

        return advancedHookDetection(context);
    }

    public boolean checkJarFiles() {
        try {
            HashSet<String> libraries = new HashSet();
            String mapsFilename = "/proc/" + android.os.Process.myPid() + "/maps";
            BufferedReader reader = new BufferedReader(new FileReader(mapsFilename));
            String line;
            while((line = reader.readLine()) != null) {
                if (line.endsWith(".so") || line.endsWith(".jar")) {
                    int n = line.lastIndexOf(" ");
                    libraries.add(line.substring(n + 1));
                }
            }
            for (String library : libraries) {
                if(library.contains("com.saurik.substrate")) {
                    Log.wtf("HookDetection", "Substrate shared object found: " + library);
                    return true;
                }
                if(library.contains("XposedBridge.jar")) {
                    Log.wtf("HookDetection", "Xposed JAR found: " + library);
                    return true;
                }
            }
            reader.close();
        }
        catch (Exception e) {
            Log.wtf("HookDetection", e.toString());
        }
        return false;
    }

    public boolean checkNativeFunction(Context activity) {
        PackageManager packageManager = activity.getPackageManager();
        List<ApplicationInfo> applicationInfoList = packageManager.getInstalledApplications(PackageManager.GET_META_DATA);
        for (ApplicationInfo applicationInfo : applicationInfoList) {
            if (applicationInfo.processName.equals("com.example.hookdetection")) {
                HashSet<String> classes = new HashSet();
                DexFile dex;
                try {
                    dex = new DexFile(applicationInfo.sourceDir);
                    Enumeration entries = dex.entries();
                    while (entries.hasMoreElements()) {
                        String entry = (String) entries.nextElement();
                        classes.add(entry);
                    }
                    dex.close();
                } catch (Exception e) {
                    Log.e("HookDetection", e.toString());
                }
                for (String className : classes) {
                    if (className.startsWith("com.example.hookdetection")) {
                        try {
                            Class clazz = Class.forName(className);
                            for (Method method : clazz.getDeclaredMethods()) {
                                if (Modifier.isNative(method.getModifiers())) {
                                    Log.wtf("HookDetection", "Native function found (could be hooked by Substrate or Xposed): " + clazz.getCanonicalName() + "->" + method.getName());
                                    return true;
                                }
                            }
                        } catch (ClassNotFoundException e) {
                            Log.wtf("HookDetection", e.toString());
                        }
                    }
                }
            }
        }

        return false;
    }

    public boolean isDebugged() {

        boolean tracer = false;
        try {
            tracer = FindDebugger.hasTracerPid();
        } catch (Exception exception) {
            exception.printStackTrace();
        }
 
        if (FindDebugger.isBeingDebugged() || tracer) {
            return true;
        } else {
            return false;
        }
     }

    public boolean checkActivityProcessors(Context context) {
        ActivityManager activityManager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        List<ActivityManager.RunningServiceInfo> runningServices = activityManager.getRunningServices(300);

        if (runningServices != null) {
            for (int i = 0; i < runningServices.size(); ++i) {
                if (runningServices.get(i).process.contains("fridaserver")) {
                    return true;
                }
                if (runningServices.get(i).process.contains("supersu")) {
                    return true;
                }
                if (runningServices.get(i).process.contains("superuser")) {
                    return true;
                }
            }
        }

        return false;
    }

    public boolean advancedHookDetection(Context context) {
        try {
            throw new Exception();
        } catch (Exception e) {
            int zygoteInitCallCount = 0;
            for (StackTraceElement stackTraceElement : e.getStackTrace()) {
                if (stackTraceElement.getClassName().equals("com.android.internal.os.ZygoteInit")) {
                    zygoteInitCallCount++;
                    if (zygoteInitCallCount == 2) {
                        return true;
                    }
                }

                if (stackTraceElement.getClassName().equals("com.saurik.substrate.MS$2") &&
                        stackTraceElement.getMethodName().equals("invoked")) {
                    return true;
                }

                if (stackTraceElement.getClassName().equals("de.robv.android.xposed.XposedBridge") &&
                        stackTraceElement.getMethodName().equals("main")) {
                    return true;
                }

                if (stackTraceElement.getClassName().equals("de.robv.android.xposed.XposedBridge") &&
                        stackTraceElement.getMethodName().equals("handleHookedMethod")) {
                    return true;
                }
            }
        }

        return checkActivityProcessors(context);
    }
}
