package `in`.juspay.hypersdk.lifecycle

internal interface RequestPermissionResult {
    fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<String>,
        grantResults: IntArray
    )
}
