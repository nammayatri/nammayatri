package in.juspay.mobility.driver;

import android.bluetooth.le.AdvertisingSet;
import android.widget.TextView;

public class DriverBeacon {
    public boolean isAdvertsing = false;
    public boolean isPeriodicallyAdvertising = false;
    private AdvertisingSet advertisingSet = null;

    private boolean currentlyMajor = false;
    private boolean firstEntry = true;

}

//private var currentlyMajor = false
//private var firstTime = true
//private var selectedBus: String? = null
//private var uuid: String? = null
//private val busHash: HashMap<String, String> = hashMapOf(
//        "TN 015 0123" to "27B2F751-228D-4BEA-8A06-F8ADC74388E6".lowercase(),
//        "KA 321 3210" to "ACA22A9D-06B2-4789-9669-9313B2F5605A".lowercase()
//    )
//
//
//private val handler = Handler(Looper.getMainLooper())
//private val advertisingInterval: Long = 10000
//private val updateInterval: Long = 5000
//
//private val callback = object : AdvertisingSetCallback() {
//    override fun onAdvertisingSetStarted(advertisingSet: AdvertisingSet?, txPower: Int, status: Int) {
//        if (status == AdvertisingSetCallback.ADVERTISE_SUCCESS) {
//            Log.i("AdvertisingSet", "Advertising started successfully")
//            this@MainActivity.advertisingSet = advertisingSet
//                    isAdvertising = true
//        } else {
//            Log.e("AdvertisingSet", "Failed to start advertising: $status")
//        }
//    }
//
//    override fun onAdvertisingSetStopped(advertisingSet: AdvertisingSet?) {
//        Log.i("AdvertisingSet", "Advertising stopped")
//        isAdvertising = false
//    }
//}
//
//@RequiresApi(Build.VERSION_CODES.O)
//override fun onCreate(savedInstanceState: Bundle?) {
//    super.onCreate(savedInstanceState)
//
//    setContentView(R.layout.activity_main)
//    supportActionBar!!.hide()
//
//    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) {
//        Toast.makeText(this, "Android versions is too old " + Build.VERSION.SDK_INT, Toast.LENGTH_LONG).show()
//        Log.i("version", "${Build.VERSION.SDK_INT}")
//    }
//
//    bluetoothAdapter = BluetoothAdapter.getDefaultAdapter()
//    bluetoothLeAdvertiser = bluetoothAdapter.bluetoothLeAdvertiser
//
//    if (!bluetoothAdapter.isEnabled) {
//        promptEnableBluetooth()
//        Log.e("iBeacon", "Bluetooth is off or not supported; cannot stop advertising")
//        return
//    }
//
//    statusTextView = findViewById(R.id.statusTextView)
//    startButton = findViewById(R.id.startButton)
//    stopButton = findViewById(R.id.stopButton)
//    radioGroup = findViewById(R.id.busSelect)
//
//    radioGroup.clearCheck()
//
//    radioGroup.setOnCheckedChangeListener { _, checkedId ->
//            radioButton = findViewById(checkedId)
//
//        Toast.makeText(
//                this@MainActivity,
//        "Selected Bus is : " + radioButton.text,
//                Toast.LENGTH_SHORT
//            ).show()
//    }
//
//    if (!hasPermissions()) {
//        requestPermissions()
//    }
//
//    startButton.setOnClickListener {
//        if (radioGroup.checkedRadioButtonId == -1) {
//            Toast.makeText(this, "Please choose a bus.", Toast.LENGTH_LONG).show()
//        }
//        else if (!isPeriodicallyAdvertising) {
//            startPeriodicAdvertising()
//        }
//    }
//
//    stopButton.setOnClickListener {
//        if (isPeriodicallyAdvertising) {
//            stopPeriodicAdvertising()
//        }
//    }
//}
//
//@RequiresApi(Build.VERSION_CODES.O)
//private fun startBeacon() {
//    if (!bluetoothAdapter.isEnabled) {
//        Log.e("iBeacon", "Bluetooth is off or not supported; cannot start advertising")
//        return
//    }
//
//    val beaconData = createIBeaconData(
//            uuid = uuid!!,
//            major = 1,
//            minor = 59
//        )
//
//    val advertiseData = AdvertiseData.Builder()
//            .addManufacturerData(0x4C00, beaconData) // 0x4C00 is Apple's Manufacturer ID
//            .build()
//
//    val params = AdvertisingSetParameters.Builder()
//            .setConnectable(false)
//            .setScannable(true)
//            .setInterval(AdvertisingSetParameters.INTERVAL_LOW)
//            .setTxPowerLevel(AdvertisingSetParameters.TX_POWER_HIGH)
//            .build()
//
//    bluetoothLeAdvertiser.startAdvertisingSet(params, advertiseData, null, null, null, callback)
//    statusTextView.text = getString(R.string.update_advertising, selectedBus, uuid, 1, 59)
//}
//
//private fun stopBeacon() {
//    if (!bluetoothAdapter.isEnabled) {
//        Log.e("iBeacon", "Bluetooth is off or not supported; cannot stop advertising")
//        return
//    }
//    advertisingSet?.let {
//        bluetoothLeAdvertiser.stopAdvertisingSet(callback)
//        advertisingSet = null
//    }
//    isAdvertising = false
//    statusTextView.text = "Status: Paused advertising"
//}
//
//private fun createIBeaconData(
//        uuid: String,
//        major: Int,
//        minor: Int,
//        ): ByteArray {
//    val uuidBytes = uuid.replace("-", "")
//            .chunked(2)
//            .map {it.toInt(16).toByte()}
//            .toByteArray()
//    val majorBytes = byteArrayOf((major shr 8).toByte(), major.toByte())
//    val minorBytes = byteArrayOf((minor shr 8).toByte(), minor.toByte())
//    return byteArrayOf(
//            0x02, 0x15
//    ) + uuidBytes + majorBytes + minorBytes + byteArrayOf((-59).toByte())
//}
//
//private fun updateBeacon() {
//    if (firstTime) {
//        firstTime = false
//        return
//    }
//    Log.i("Update", "updating")
//    val major: Int
//    val minor: Int
//    if (!currentlyMajor) {
//        major = 59
//        minor = 1
//        currentlyMajor = true
//    } else {
//        major = 1
//        minor = 59
//        currentlyMajor = false
//    }
//
//    val beaconData = createIBeaconData(
//            uuid = uuid!!,
//            major = major,
//            minor = minor
//        )
//
//    val advertiseData = AdvertiseData.Builder()
//            .addManufacturerData(0x4C00, beaconData) // 0x4C00 is Apple's Manufacturer ID
//            .build()
//
//    advertisingSet?.setAdvertisingData(advertiseData)
//
//    statusTextView.text = getString(R.string.update_advertising, selectedBus, uuid, major, minor)
//}
//
//private fun startPeriodicAdvertising() {
//    if(!hasPermissions()) {
//        Log.e("BLEBeacon", "Required permissions are not granted")
//        return
//    }
//
//    val checkedButton = radioGroup.checkedRadioButtonId
//    if (checkedButton == -1) {
//        Toast.makeText(this, "Please choose a bus", Toast.LENGTH_LONG).show()
//        return
//    }
//    selectedBus = findViewById<RadioButton>(checkedButton).text.toString()
//    uuid = busHash[selectedBus] ?: return
//
//            startBeacon()
//
//    val runnable = object: Runnable {
//        @RequiresApi(Build.VERSION_CODES.O)
//        override fun run() {
//            handler.postDelayed(this, advertisingInterval)
//            updateBeacon()
//        }
//    }
//
//    handler.post(runnable)
//
//    isPeriodicallyAdvertising = true
//    startButton.visibility = View.GONE
//    stopButton.visibility = View.VISIBLE
//    radioGroup.visibility = View.GONE
//}
//
//private fun stopPeriodicAdvertising() {
//    handler.removeCallbacksAndMessages(null)
//    stopBeacon()
//    isPeriodicallyAdvertising = false
//    statusTextView.text = getString(R.string.stop_advertising)
//    startButton.visibility = View.VISIBLE
//    stopButton.visibility = View.GONE
//    radioGroup.visibility = View.VISIBLE
//}
//
//private fun hasPermissions(): Boolean {
//    val permissions = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
//        arrayOf(
//                Manifest.permission.BLUETOOTH_ADVERTISE,
//                Manifest.permission.BLUETOOTH_SCAN,
//                Manifest.permission.BLUETOOTH_CONNECT,
//                Manifest.permission.BLUETOOTH_ADMIN
//        )
//    } else {
//        arrayOf(
//                Manifest.permission.BLUETOOTH_ADMIN,
//                Manifest.permission.ACCESS_FINE_LOCATION
//        )
//    }
//
//    return permissions.all {
//        ActivityCompat.checkSelfPermission(this, it) == PackageManager.PERMISSION_GRANTED
//    }
//}
//
//private fun promptEnableBluetooth() {
//    Toast.makeText(this, "Please enable bluetooth to start advertising", Toast.LENGTH_LONG).show()
//    startActivity(Intent(Settings.ACTION_BLUETOOTH_SETTINGS))
//}
//
//private fun requestPermissions() {
//    val permissions = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
//        arrayOf(
//                Manifest.permission.BLUETOOTH_ADVERTISE,
//                Manifest.permission.BLUETOOTH_SCAN,
//                Manifest.permission.BLUETOOTH_CONNECT
//        )
//    } else {
//        arrayOf(
//                Manifest.permission.BLUETOOTH_ADMIN,
//                Manifest.permission.ACCESS_FINE_LOCATION
//        )
//    }
//
//    ActivityCompat.requestPermissions(this, permissions, 1)
//}
//
//override fun onRequestPermissionsResult(
//        requestCode: Int,
//        permissions: Array<out String>,
//        grantResults: IntArray
//) {
//    super.onRequestPermissionsResult(requestCode, permissions, grantResults)
//}
//}
