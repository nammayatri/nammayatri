let common = ../../unified-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let serverFolderName = "Rider"

let folderName = "Management"

let fullFolderName = serverFolderName ++ "/" ++ folderName

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
                  common.outputPrefixRiderAppReadOnly
              ++  "API/Types/UnifiedDashboard/"
              ++  folderName
          , _extraApiRelatedTypes =
                  common.outputPrefixRiderApp
              ++  "API/Types/UnifiedDashboard/"
              ++  folderName
              ++  "/OrphanInstances"
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ fullFolderName
          , _servantApi =
                  common.outputPrefixRiderAppReadOnly
              ++  "API/Action/UnifiedDashboard/"
              ++  folderName
          , _domainHandler =
                  common.outputPrefixRiderApp
              ++  "Domain/Action/UnifiedDashboard/"
              ++  folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ fullFolderName
          , _servantApiClient =
              defaultOutput._servantApiClient ++ "/" ++ fullFolderName
          }

let serverName = Some "APP_BACKEND_MANAGEMENT"

in      common.defaultConfigs
    //  { _output = outputPath
        , _serverName = serverName
        , _folderName = Some folderName
        , _defaultImports = common.mkDefaultImports "rider-app"
        , _packageMapping =
          [ { _1 = common.GeneratorType.API_TYPES, _2 = "rider-app" }
          , { _1 = common.GeneratorType.SERVANT_API, _2 = "rider-app" }
          , { _1 = common.GeneratorType.DOMAIN_HANDLER, _2 = "rider-app" }
          , { _1 = common.GeneratorType.API_TREE_COMMON, _2 = "rider-app" }
          , { _1 = common.GeneratorType.SERVANT_API_DASHBOARD
            , _2 = "unified-dashboard"
            }
          , { _1 = common.GeneratorType.API_TREE_CLIENT
            , _2 = "unified-dashboard"
            }
          ]
        }
