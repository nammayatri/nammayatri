let common = ../rider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes = defaultOutput._apiRelatedTypes ++ "/RideBooking"
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/RideBooking"
          , _servantApi = defaultOutput._servantApi ++ "/RideBooking"
          , _domainHandler = common.outputPrefixRiderApp ++ "Domain/Action/UI"
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/RideBooking"
          }

let clientFunction =
      Some "RiderPlatformClient.RiderApp.RideBooking.callRiderApp"

in      common.defaultConfigs
    //  { _output = outputPath, _clientFunction = clientFunction }
