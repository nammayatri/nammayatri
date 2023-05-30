package in.juspay.mobility.utils;

import android.content.Context;
import android.os.Build;
import android.telephony.TelephonyManager;
import android.util.Log;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import in.juspay.mobility.utils.Property;
import in.juspay.mobility.utils.FindDebugger;

/**
 * Class used to determine functionality specific to the Android QEmu.
 *
 * @author tstrazzere
 */
public class FindEmulator {

    // Need to check the format of these
    // Android emulator support up to 16 concurrent emulator
    // The console of the first emulator instance running on a given
    // machine uses console port 5554
    // Subsequent instances use port numbers increasing by two
    private static String[] known_numbers = {
            "15555215554", // Default emulator phone numbers + VirusTotal
            "15555215556", "15555215558", "15555215560", "15555215562", "15555215564", "15555215566",
            "15555215568", "15555215570", "15555215572", "15555215574", "15555215576", "15555215578",
            "15555215580", "15555215582", "15555215584",};
    private static String[] known_device_ids = {"000000000000000", // Default emulator id
            "e21833235b6eef10", // VirusTotal id
            "012345678912345"};
    private static String[] known_imsi_ids = {"310260000000000" // Default imsi id
    };
    private static String[] known_pipes = {"/dev/socket/qemud", "/dev/qemu_pipe"};
    private static String[] known_files = {"/system/lib/libc_malloc_debug_qemu.so", "/sys/qemu_trace",
            "/system/bin/qemu-props"};
    private static String[] known_geny_files = {"/dev/socket/genyd", "/dev/socket/baseband_genyd"};
    private static String[] known_qemu_drivers = {"goldfish"};
    /**
     * Known props, in the format of [property name, value to seek] if value to seek is null, then it is assumed that
     * the existence of this property (anything not null) indicates the QEmu environment.
     */
    private static Property[] known_props = {new Property("init.svc.qemud", null),
            new Property("init.svc.qemu-props", null), new Property("qemu.hw.mainkeys", null),
            new Property("qemu.sf.fake_camera", null), new Property("qemu.sf.lcd_density", null),
            new Property("ro.bootloader", "unknown"), new Property("ro.bootmode", "unknown"),
            new Property("ro.hardware", "goldfish"), new Property("ro.kernel.android.qemud", null),
            new Property("ro.kernel.qemu.gles", null), new Property("ro.kernel.qemu", "1"),
            new Property("ro.product.device", "generic"), new Property("ro.product.model", "sdk"),
            new Property("ro.product.name", "sdk"),
            // Need to double check that an "empty" string ("") returns null
            new Property("ro.serialno", null)};
    /**
     * The "known" props have the potential for false-positiving due to interesting (see: poorly) made Chinese
     * devices/odd ROMs. Keeping this threshold low will result in better QEmu detection with possible side affects.
     */
    private static int MIN_PROPERTIES_THRESHOLD = 0x5;

    static {
        // This is only valid for arm, so gate it
        for(String abi : Build.SUPPORTED_ABIS) {
            if(abi.equalsIgnoreCase("armeabi-v7a")) {
                // System.loadLibrary("anti");
                break;
            }
        }
    }

    /**
     * Check the existence of known pipes used by the Android QEmu environment.
     *
     * @return {@code true} if any pipes where found to exist or {@code false} if not.
     */
    public static boolean hasPipes() {
        for (String pipe : known_pipes) {
            File qemu_socket = new File(pipe);
            if (qemu_socket.exists()) {
                return true;
            }
        }

        return false;
    }

    /**
     * Check the existence of known files used by the Android QEmu environment.
     *
     * @return {@code true} if any files where found to exist or {@code false} if not.
     */
    public static boolean hasQEmuFiles() {
        for (String pipe : known_files) {
            File qemu_file = new File(pipe);
            if (qemu_file.exists()) {
                return true;
            }
        }

        return false;
    }

    /**
     * Check the existence of known files used by the Genymotion environment.
     *
     * @return {@code true} if any files where found to exist or {@code false} if not.
     */
    public static boolean hasGenyFiles() {
        for (String file : known_geny_files) {
            File geny_file = new File(file);
            if (geny_file.exists()) {
                return true;
            }
        }

        return false;
    }

    /**
     * Reads in the driver file, then checks a list for known QEmu drivers.
     *
     * @return {@code true} if any known drivers where found to exist or {@code false} if not.
     */
    public static boolean hasQEmuDrivers() {
        for (File drivers_file : new File[]{new File("/proc/tty/drivers"), new File("/proc/cpuinfo")}) {
            if (drivers_file.exists() && drivers_file.canRead()) {
                // We don't care to read much past things since info we care about should be inside here
                byte[] data = new byte[1024];
                try {
                    InputStream is = new FileInputStream(drivers_file);
                    is.read(data);
                    is.close();
                } catch (Exception exception) {
                    exception.printStackTrace();
                }

                String driver_data = new String(data);
                for (String known_qemu_driver : FindEmulator.known_qemu_drivers) {
                    if (driver_data.indexOf(known_qemu_driver) != -1) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    public static boolean hasKnownPhoneNumber(Context context) {
        TelephonyManager telephonyManager = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);

        try {
            String phoneNumber = telephonyManager.getLine1Number();
            for (String number : known_numbers) {
                if (number.equalsIgnoreCase(phoneNumber)) {
                    return true;
                }

            }
        } catch( SecurityException exception) {
            log("Unable to request getLine1Number, failing open :" + exception.toString());
        }

        return false;
    }

    public static boolean hasKnownDeviceId(Context context) {
        TelephonyManager telephonyManager = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);

        try {
            String deviceId = telephonyManager.getDeviceId();
            for (String known_deviceId : known_device_ids) {
                if (known_deviceId.equalsIgnoreCase(deviceId)) {
                    return true;
                }
            }
        } catch( SecurityException exception) {
            log("Unable to request getDeviceId, failing open :" + exception.toString());
        }

        return false;
    }

    public static boolean hasKnownImsi(Context context) {
        TelephonyManager telephonyManager = (TelephonyManager) context.getSystemService(Context.TELEPHONY_SERVICE);

        try {
            String imsi = telephonyManager.getSubscriberId();

            for (String known_imsi : known_imsi_ids) {
                if (known_imsi.equalsIgnoreCase(imsi)) {
                    return true;
                }
            }
        } catch( SecurityException exception) {
            log("Unable to request getSubscriberId, failing open :" + exception.toString());
        }

        return false;
    }

    public static boolean hasEmulatorBuild(Context context) {
        String BOARD = android.os.Build.BOARD; // The name of the underlying board, like "unknown".
        // This appears to occur often on real hardware... that's sad
        // String BOOTLOADER = android.os.Build.BOOTLOADER; // The system bootloader version number.
        String BRAND = android.os.Build.BRAND; // The brand (e.g., carrier) the software is customized for, if any.
        // "generic"
        String DEVICE = android.os.Build.DEVICE; // The name of the industrial design. "generic"
        String HARDWARE = android.os.Build.HARDWARE; // The name of the hardware (from the kernel command line or
        // /proc). "goldfish"
        String MODEL = android.os.Build.MODEL; // The end-user-visible name for the end product. "sdk"
        String PRODUCT = android.os.Build.PRODUCT; // The name of the overall product.
        if ((BOARD.compareTo("unknown") == 0) /* || (BOOTLOADER.compareTo("unknown") == 0) */
                || (BRAND.compareTo("generic") == 0) || (DEVICE.compareTo("generic") == 0)
                || (MODEL.compareTo("sdk") == 0) || (PRODUCT.compareTo("sdk") == 0)
                || (HARDWARE.compareTo("goldfish") == 0)) {
            return true;
        }
        return false;
    }

    public static boolean isOperatorNameAndroid(Context paramContext) {
        String szOperatorName = ((TelephonyManager) paramContext.getSystemService(Context.TELEPHONY_SERVICE)).getNetworkOperatorName();
        boolean isAndroid = szOperatorName.equalsIgnoreCase("android");
        return isAndroid;
    }

    public native static int qemuBkpt();

    public static boolean checkQemuBreakpoint() {
        boolean hit_breakpoint = false;

        // Potentially you may want to see if this is a specific value
        int result = qemuBkpt();

        if (result > 0) {
            hit_breakpoint = true;
        }

        return hit_breakpoint;
    }

    public static boolean hasEmulatorAdb() {
        try {
            return FindDebugger.hasAdbInEmulator();
        } catch (Exception exception) {
            exception.printStackTrace();
            return false;
        }
    }

    /**
     * Will query specific system properties to try and fingerprint a QEmu environment. A minimum threshold must be met
     * in order to prevent false positives.
     *
     * @param context A {link Context} object for the Android application.
     * @return {@code true} if enough properties where found to exist or {@code false} if not.
     */
    // public boolean hasQEmuProps(Context context) {
    //     int found_props = 0;

    //     for (Property property : known_props) {
    //         String property_value = Utilities.getProp(context, property.name);
    //         // See if we expected just a non-null
    //         if ((property.seek_value == null) && (property_value != null)) {
    //             found_props++;
    //         }
    //         // See if we expected a value to seek
    //         if ((property.seek_value != null) && (property_value.indexOf(property.seek_value) != -1)) {
    //             found_props++;
    //         }

    //     }

    //     if (found_props >= MIN_PROPERTIES_THRESHOLD) {
    //         return true;
    //     }

    //     return false;
    // }


    public static void log(String msg) {
        Log.v("AntiEmu:FindEmulator", msg);
    }
}