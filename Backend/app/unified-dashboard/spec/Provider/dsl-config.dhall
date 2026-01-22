let common = ../unified-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "Provider"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
              common.outputPrefixDriverAppReadOnly ++ "API/Types/" ++ folderName
          , _extraApiRelatedTypes =
                  common.outputPrefixDriverApp
              ++  "API/Types/"
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

let serverName = Some "DRIVER_OFFER_BPP_MANAGEMENT"

in      common.defaultConfigs
    //  { _output = outputPath
        , _serverName = serverName
        , _folderName = Some folderName
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
