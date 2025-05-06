package `in`.juspay.mobility.sdk.hyper.core

interface CallbackInvoker {
    fun invokeCallbackInDUIWebview(methodName: String?, argumentJson: String?)
    fun invokeFnInDUIWebview(command: String)
}
