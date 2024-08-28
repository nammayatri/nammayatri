let common = ../rider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "RideBooking"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
              defaultOutput._apiRelatedTypes ++ "/" ++ folderName
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ folderName
          , _servantApi = defaultOutput._servantApi ++ "/" ++ folderName
          , _domainHandler = common.outputPrefixRiderApp ++ "/" ++ folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ folderName
          }

let clientFunction =
      Some "RiderPlatformClient.RiderApp.RideBooking.callRiderApp"

in      common.defaultConfigs
    //  { _output = outputPath
        , _clientFunction = clientFunction
        , _folderName = Some folderName
        }
