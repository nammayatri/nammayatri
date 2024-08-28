let common = ../provider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "RideBooking"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
              defaultOutput._apiRelatedTypes ++ "/" ++ folderName
          , _servantApi = defaultOutput._servantApi ++ "/" ++ folderName
          , _domainHandler = defaultOutput._domainHandler ++ "/" ++ folderName
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ folderName
          }

let clientFunction =
      Some
        "ProviderPlatformClient.DynamicOfferDriver.RideBooking.callDriverOfferBPP"

in      common.defaultConfigs
    //  { _output = outputPath
        , _clientFunction = clientFunction
        , _folderName = Some folderName
        }
