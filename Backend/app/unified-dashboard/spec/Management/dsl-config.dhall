let common = ../unified-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "Management"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
              common.outputPrefixDashboardReadOnly ++ "API/Types/" ++ folderName
          , _extraApiRelatedTypes =
                  common.outputPrefixDashboard
              ++  "API/Types/"
              ++  folderName
              ++  "/OrphanInstances"
          , _extraApiRelatedCommonTypes =
              common.outputPrefixCommonApis ++ folderName ++ "/Common"
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ folderName
          , _servantApi =
              defaultOutput._servantApiDashboard ++ "/" ++ folderName
          , _domainHandler =
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
        , _generate =
          [ common.GeneratorType.DOMAIN_TYPE
          , common.GeneratorType.BEAM_TABLE
          , common.GeneratorType.BEAM_QUERIES
          , common.GeneratorType.CACHED_QUERIES
          , common.GeneratorType.DOMAIN_HANDLER_DASHBOARD
          , common.GeneratorType.API_TYPES
          , common.GeneratorType.SERVANT_API_DASHBOARD
          , common.GeneratorType.API_TREE_DASHBOARD
          , common.GeneratorType.API_TREE_COMMON
          , common.GeneratorType.SQL
          ]
        , _defaultImports = common.mkDefaultImports "dynamic-offer-driver-app"
        , _importsMapping =
          [ { _1 = "Dashboard.Common", _2 = None Text }
          , { _1 = "SharedLogic.Transaction"
            , _2 = Some "Domain.Action.Management.Transaction"
            }
          , { _1 = "Domain.Types.Transaction", _2 = None Text }
          ]
        , _packageMapping =
          [ { _1 = common.GeneratorType.API_TYPES, _2 = "unified-dashboard" }
          , { _1 = common.GeneratorType.SERVANT_API, _2 = "unified-dashboard" }
          , { _1 = common.GeneratorType.DOMAIN_HANDLER
            , _2 = "unified-dashboard"
            }
          , { _1 = common.GeneratorType.API_TREE_COMMON
            , _2 = "unified-dashboard"
            }
          , { _1 = common.GeneratorType.SERVANT_API_DASHBOARD
            , _2 = "unified-dashboard"
            }
          ]
        }
