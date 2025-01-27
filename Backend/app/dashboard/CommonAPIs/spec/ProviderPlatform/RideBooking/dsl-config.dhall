let common = ../provider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "RideBooking"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
                  common.outputPrefixDriverAppReadOnly
              ++  "API/Types/Dashboard/"
              ++  folderName
          , _extraApiRelatedTypes =
                  common.outputPrefixDriverApp
              ++  "API/Types/Dashboard/"
              ++  folderName
              ++  "/OrphanInstances"
          , _servantApi = defaultOutput._servantApi ++ "/" ++ folderName
          , _domainHandler = defaultOutput._domainHandler ++ "/" ++ folderName
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ folderName
          , _servantApiClient =
              defaultOutput._servantApiClient ++ "/" ++ folderName
          }

let serverName = Some "DRIVER_OFFER_BPP"

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
          , { _1 = common.GeneratorType.API_TREE_CLIENT
            , _2 = "provider-dashboard"
            }
          , { _1 = common.GeneratorType.SERVANT_API_DASHBOARD
            , _2 = "provider-dashboard"
            }
          ]
        }
