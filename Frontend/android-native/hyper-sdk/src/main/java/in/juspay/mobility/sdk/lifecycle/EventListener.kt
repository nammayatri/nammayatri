package `in`.juspay.mobility.sdk.lifecycle

internal interface EventListener {
    fun onEvent(payload: String, subscribedFragment: HyperFragment)
}
