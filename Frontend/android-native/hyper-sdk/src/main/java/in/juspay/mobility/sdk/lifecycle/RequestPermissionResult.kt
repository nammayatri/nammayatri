package `in`.juspay.mobility.sdk.lifecycle

internal interface RequestPermissionResult {
    fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<String>,
        grantResults: IntArray
    )
}
