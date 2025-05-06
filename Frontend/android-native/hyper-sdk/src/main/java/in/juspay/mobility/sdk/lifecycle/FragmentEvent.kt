package `in`.juspay.mobility.sdk.lifecycle

internal enum class FragmentEvent(val key: String) {
    ON_PAUSE("onPause"),
    ON_RESUME("onResume"),
    ON_STOP("onStop"),
    ON_DESTROY("onDestroy"),
    ON_SAVED_STATE_INSTANCE("OnSavedStateInstance"),
    ON_ACTIVITY_RESULT("onActivityResult"),
    ON_REQUEST_PERMISSION_RESULT("onRequestPermissionResult"),
    ON_ATTACH("onAttach")
}
