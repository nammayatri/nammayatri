package in.juspay.services;

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
