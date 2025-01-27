let common = ../provider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "Fleet"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
              defaultOutput._apiRelatedTypes ++ "/" ++ folderName
          , _extraApiRelatedTypes =
              defaultOutput._extraApiRelatedTypes ++ "/" ++ folderName
          , _servantApi = defaultOutput._servantApi ++ "/" ++ folderName
          , _domainHandler = defaultOutput._domainHandler ++ "/" ++ folderName
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ folderName
          , _servantApiClient =
              defaultOutput._servantApiClient ++ "/" ++ folderName
          }

let serverName = Some "DRIVER_OFFER_BPP_MANAGEMENT"

in      common.defaultConfigs
    //  { _output = outputPath
        , _serverName = serverName
        , _folderName = Some folderName
        }
