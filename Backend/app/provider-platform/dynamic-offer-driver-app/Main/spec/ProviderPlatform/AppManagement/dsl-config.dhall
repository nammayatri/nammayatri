let common = ../dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "AppManagement"

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
          , _servantApiDashboardAuth =
              defaultOutput._servantApiDashboardAuth ++ "/" ++ folderName
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
          ]
        }
