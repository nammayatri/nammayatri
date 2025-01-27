let common = ../rider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "RideBooking"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
                  common.outputPrefixRiderAppReadOnly
              ++  "API/Types/Dashboard/"
              ++  folderName
          , _extraApiRelatedTypes =
                  common.outputPrefixRiderApp
              ++  "API/Types/Dashboard/"
              ++  folderName
              ++  "/OrphanInstances"
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ folderName
          , _servantApi = defaultOutput._servantApi ++ "/" ++ folderName
          , _domainHandler = defaultOutput._domainHandler ++ "/" ++ folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ folderName
          , _servantApiClient =
              defaultOutput._servantApiClient ++ "/" ++ folderName
          }

let serverName = Some "APP_BACKEND"

in      common.defaultConfigs
    //  { _output = outputPath
        , _serverName = serverName
        , _folderName = Some folderName
        , _packageMapping =
          [ { _1 = common.GeneratorType.API_TYPES, _2 = "rider-app" }
          , { _1 = common.GeneratorType.SERVANT_API, _2 = "rider-app" }
          , { _1 = common.GeneratorType.DOMAIN_HANDLER, _2 = "rider-app" }
          ]
        }
