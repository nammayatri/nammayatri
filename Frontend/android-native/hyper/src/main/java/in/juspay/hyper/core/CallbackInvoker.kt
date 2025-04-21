package `in`.juspay.hyper.core

interface CallbackInvoker {
    fun invokeCallbackInDUIWebview(methodName: String?, argumentJson: String?)
    fun invokeFnInDUIWebview(command: String)
}
