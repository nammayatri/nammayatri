let common = ../provider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes = defaultOutput._apiRelatedTypes ++ "/RideBooking"
          , _servantApi = defaultOutput._servantApi ++ "/RideBooking"
          , _domainHandler = common.outputPrefixDriverApp ++ "Domain/Action/UI"
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/RideBooking"
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/RideBooking"
          }

let clientFunction =
      Some
        "ProviderPlatformClient.DynamicOfferDriver.RideBooking.callDriverOfferBPP"

in      common.defaultConfigs
    //  { _output = outputPath, _clientFunction = clientFunction }
