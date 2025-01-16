let common = ../rider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "Management"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
              defaultOutput._apiRelatedTypes ++ "/" ++ folderName
          , _extraApiRelatedTypes =
              defaultOutput._extraApiRelatedTypes ++ "/" ++ folderName
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ folderName
          , _servantApi = defaultOutput._servantApi ++ "/" ++ folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ folderName
          , _servantApiClient =
              defaultOutput._servantApiClient ++ "/" ++ folderName
          }

let serverName = Some "APP_BACKEND_MANAGEMENT"

in      common.defaultConfigs
    //  { _output = outputPath
        , _serverName = serverName
        , _folderName = Some folderName
        }
