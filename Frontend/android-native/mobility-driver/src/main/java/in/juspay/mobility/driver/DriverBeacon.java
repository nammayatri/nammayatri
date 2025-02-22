package in.juspay.mobility.driver;

import android.Manifest;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.le.AdvertiseData;
import android.bluetooth.le.AdvertisingSet;
import android.bluetooth.le.AdvertisingSetCallback;
import android.bluetooth.le.AdvertisingSetParameters;
import android.bluetooth.le.BluetoothLeAdvertiser;
import android.bluetooth.le.PeriodicAdvertisingParameters;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.provider.Settings;
import android.util.Log;
import android.widget.Toast;

import androidx.core.app.ActivityCompat;


public class DriverBeacon {
    public boolean isAdvertising = false;               // Keeps track of whether beacon is currently advertising
    public boolean isPeriodicallyAdvertising = false;   // keeps track of whether handler is periodically advertising
    private AdvertisingSet advertisingSet = null;
    public BluetoothAdapter bluetoothAdapter = null;
    public BluetoothLeAdvertiser bluetoothLeAdvertiser = null;

    public String uuid;                                 // keeps track of the uuid when starting beacon

    private Handler handler = new Handler(Looper.getMainLooper());
    private final Context context;

    // Permissions required for each possible version
    private final String[] permissions = (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S)?
            new String[]{
                    Manifest.permission.BLUETOOTH_ADVERTISE,
                    Manifest.permission.BLUETOOTH_SCAN,
                    Manifest.permission.BLUETOOTH_CONNECT,
                    Manifest.permission.BLUETOOTH_ADMIN
            }:
            new String[]{
                    Manifest.permission.BLUETOOTH_ADMIN,
                    Manifest.permission.ACCESS_FINE_LOCATION
    };

    // Callback for the advertising
    private AdvertisingSetCallback setCallback = new AdvertisingSetCallback() {
        @Override
        public void onAdvertisingSetStarted(AdvertisingSet advertisingSet, int txPower, int status) {
            if (status == AdvertisingSetCallback.ADVERTISE_SUCCESS) {
                Log.i("AdvertisingSet", "Advertising started successfully");
                DriverBeacon.this.advertisingSet = advertisingSet;
                isAdvertising = true;
            } else {
                Log.e("AdvertisingSet", "Falied to start advertising: " + status);
            }
        }
        @Override
        public void onAdvertisingSetStopped(AdvertisingSet advertisingSet) {
            Log.i("AdvertisingSet", "Advertising stopped.");
            isAdvertising = false;
        }
    };

    // When initialized, it requires the context for many of the Toast notifs to work.
    // This simply ensures all requirements to run it have bene met and creates a bluetooth adapter
    public DriverBeacon(Context context) {
        this.context = context;
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) {
            Toast.makeText(this.context, "Android versions is too old " + Build.VERSION.SDK_INT, Toast.LENGTH_LONG).show();
            Log.i("BTinit", "${Build.VERSION.SDK_INT}");
            return;
        }

        // Create actual BLE advertiser
        bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
        bluetoothLeAdvertiser = bluetoothAdapter.getBluetoothLeAdvertiser();

        if (!bluetoothAdapter.isEnabled()) {
            promptEnableBluetooth();
            Log.e("BTinit", "Bluetooth is off or not supported; cannot stop advertising");
            throw new IllegalStateException("Bluetooth must be enabled to use DriverBeacon.");
        }

        if (!hasPermissions()) {
            throw new IllegalStateException("Required permissions not granted.");
        }
    }

    // When provided with the data, this starts the beacon
    public void startBeacon(String uuid, int major, int minor) {
        this.uuid = uuid;
        if (!bluetoothAdapter.isEnabled()) {
            Log.e("iBeacon", "Bluetooth is off or not supported; cannot start advertising");
            throw new IllegalStateException("Bluetooth needs to be on to use this application");
        }

        byte[] beaconData = createIBeaconData(
                this.uuid,
                major,
                minor
        );

        // Creates advertiseData which is then sent out as advertisement from beacon
        AdvertiseData advertiseData = new AdvertiseData.Builder()
                .addManufacturerData(0x4C00, beaconData)
                .build();

        // Set up advertising parameters
        AdvertisingSetParameters params = new AdvertisingSetParameters.Builder()
                .setConnectable(false)
                .setScannable(true)
                .setInterval(AdvertisingSetParameters.INTERVAL_MEDIUM) // sends message every 400 ms
                .setTxPowerLevel(AdvertisingSetParameters.TX_POWER_HIGH) // High power Tx for better ranging
                .build();

        bluetoothLeAdvertiser.startAdvertisingSet(params, advertiseData, null, null, null, setCallback);

        isAdvertising = true;
    }

    // Stops any running beacon.
    // Currently throws an error if no beacon is running, but I can change that if required. (with the right logic this shouldn't even be possible anyway)
    public void stopBeacon() {
        if (!bluetoothAdapter.isEnabled()) {
            Log.e("iBeacon", "Bluetooth is off or not supported; cannot stop advertising");
            throw new IllegalStateException("Bluetooth is not enabled.");
        }

        // if not advertising, throw error
        if (advertisingSet == null) {
            throw new IllegalStateException("Can't stop running beacon without first starting.");
        }

        bluetoothLeAdvertiser.stopAdvertisingSet(setCallback);
        advertisingSet = null;
        isAdvertising = false;

    }

    // Once advertising, can be used to update major and minor values in the currently advertising data
    // Same error thrown as stopBeacon() if no beacon currently running
    // major and minor can probably be hardcoded as necessary on higher level function/class to correspond to required functionality
    public void updateBeacon(int major, int minor) {
        byte[] beaconData = createIBeaconData(
            uuid,
            major,
            minor
        );

        // Creates new advertiseData which is then sent out as advertisement from beacon
        AdvertiseData advertiseData = new AdvertiseData.Builder()
            .addManufacturerData(0x4C00, beaconData) // 0x4C00 is Apple's Manufacturer ID
            .build();

        // If not advertising, throw error
        if (advertisingSet == null) {
            throw new IllegalStateException("Can't stop running beacon without first starting.");
        }

        // updates the currently advertising data
        advertisingSet.setAdvertisingData(advertiseData);

    }

    // Periodically starts and stops beacon advertising based on the advertisingInterval (ms) provided
    public void startPeriodicAdvertising(String uuid, int major, int minor, long advertisingInterval) {
        if(!hasPermissions()) {
            Log.e("BLEBeacon", "Required permissions are not granted");
            return;
        }
        startBeacon(uuid, major, minor);

        // Runnable that periodically turns the beacon on and off
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                if (isAdvertising) {
                    stopBeacon();
                } else {
                    startBeacon(uuid, major, minor);
                }
                handler.postDelayed(this, advertisingInterval);
            }
        };

        handler.post(runnable);

        isPeriodicallyAdvertising = true;
    }

    // Fully stops periodic advertising
    public void stopPeriodicAdvertising() {
        handler.removeCallbacksAndMessages(null);
        stopBeacon();
        isPeriodicallyAdvertising = false;
    }


    // Creates data with iBeacon packet config to advertise
    private byte[] createIBeaconData(
            String uuid,
            int major,
            int minor
    ) {
        // Converting uuid into byte array from String
        String uuidWithoutDashes = uuid.replace("-", "");
        byte[] uuidBytes = new byte[16];
        for (int i = 0; i < uuidBytes.length; i++) {
            uuidBytes[i] = (byte) Integer.parseInt(uuidWithoutDashes.substring(i * 2, i * 2 + 2), 16);
        }

        byte[] majorBytes = new byte[] {(byte) ((major >> 8) & 0xFF), (byte) (major & 0xFF)};
        byte[] minorBytes = new byte[] {(byte) ((minor >> 8) & 0xFF), (byte) (minor & 0xFF)};

        // Creating actual byte array
        byte[] iBeaconBytes = new byte[23];
        iBeaconBytes[0] = 0x02;
        iBeaconBytes[1] = 0x15;
        System.arraycopy(uuidBytes, 0, iBeaconBytes, 2, uuidBytes.length);
        System.arraycopy(majorBytes, 0, iBeaconBytes, 18, majorBytes.length);
        System.arraycopy(minorBytes, 0, iBeaconBytes, 20, minorBytes.length);
        iBeaconBytes[22] = (byte) (-59);
        return iBeaconBytes;
    }

    // Takes user to bluetooth in settings to turn on if necessary
    private void promptEnableBluetooth() {
        Toast.makeText(this.context, "Please enable bluetooth to start advertising", Toast.LENGTH_LONG).show();
        Intent intent = new Intent(Settings.ACTION_BLUETOOTH_SETTINGS);
        context.startActivity(intent);
    }

    // Checks if all necessary permissions are provided
    private boolean hasPermissions() {
        for (String permission: permissions) {
            if (ActivityCompat.checkSelfPermission(context, permission) != PackageManager.PERMISSION_GRANTED)
                return false;
        }
        return true;
    }
}