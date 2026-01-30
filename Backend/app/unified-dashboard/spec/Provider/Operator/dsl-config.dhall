let common = ../../unified-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let serverFolderName = "Provider"

let folderName = "Operator"

let fullFolderName = serverFolderName ++ "/" ++ folderName

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
                  common.outputPrefixDriverAppReadOnly
              ++  "API/Types/UnifiedDashboard/"
              ++  folderName
          , _extraApiRelatedTypes =
                  common.outputPrefixDriverApp
              ++  "API/Types/UnifiedDashboard/"
              ++  folderName
              ++  "/OrphanInstances"
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ fullFolderName
          , _servantApi =
                  common.outputPrefixDriverAppReadOnly
              ++  "API/Action/UnifiedDashboard/"
              ++  folderName
          , _domainHandler =
                  common.outputPrefixDriverApp
              ++  "Domain/Action/UnifiedDashboard/"
              ++  folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ fullFolderName
          , _servantApiClient =
              defaultOutput._servantApiClient ++ "/" ++ fullFolderName
          }

let serverName = Some "DRIVER_OFFER_BPP_MANAGEMENT"

in      common.defaultConfigs
    //  { _output = outputPath
        , _serverName = serverName
        , _folderName = Some folderName
        , _defaultImports = common.mkDefaultImports "dynamic-offer-driver-app"
        , _packageMapping =
          [ { _1 = common.GeneratorType.API_TYPES
            , _2 = "dynamic-offer-driver-app"
            }
          , { _1 = common.GeneratorType.SERVANT_API
            , _2 = "dynamic-offer-driver-app"
            }
          , { _1 = common.GeneratorType.DOMAIN_HANDLER
            , _2 = "dynamic-offer-driver-app"
            }
          , { _1 = common.GeneratorType.API_TREE_COMMON
            , _2 = "dynamic-offer-driver-app"
            }
          , { _1 = common.GeneratorType.SERVANT_API_DASHBOARD
            , _2 = "unified-dashboard"
            }
          , { _1 = common.GeneratorType.API_TREE_CLIENT
            , _2 = "unified-dashboard"
            }
          ]
        }
