package `in`.juspay.hypersdk.lifecycle

internal interface EventListener {
    fun onEvent(payload: String, subscribedFragment: HyperFragment)
}
