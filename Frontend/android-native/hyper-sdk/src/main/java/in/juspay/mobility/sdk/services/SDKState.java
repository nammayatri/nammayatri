package in.juspay.mobility.sdk.services;

enum SDKState {
    // object instantiated
    INSTANTIATED,
    // initiate called
    INITIATE_STARTED,
    // initiate finished
    INITIATE_COMPLETED,
    // terminate called
    TERMINATED
}
