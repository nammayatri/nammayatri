let rootDir = env:GIT_ROOT_PATH

let sharedServicesReadOnly =
      rootDir ++ "/Backend/lib/shared-services/src-read-only/"

let sharedServicesSrc = rootDir ++ "/Backend/lib/shared-services/src/"

let driverAppReadOnly =
          rootDir
      ++  "/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/"

let outputPath =
      { _apiRelatedTypes =
          sharedServicesReadOnly ++ "API/Types/ProviderPlatform/Safety"
      , _extraApiRelatedTypes = ""
      , _extraApiRelatedCommonTypes = sharedServicesSrc ++ "Safety/Common"
      , _beamQueries = ""
      , _extraBeamQueries = ""
      , _cachedQueries = ""
      , _extraCachedQueries = ""
      , _beamTable = ""
      , _domainHandler = sharedServicesSrc ++ "Safety/Domain/Handler"
      , _domainHandlerDashboard = ""
      , _domainType = ""
      , _servantApi = driverAppReadOnly ++ "API/Action/UI/Safety"
      , _servantApiDashboard = ""
      , _servantApiClient = ""
      , _sql = [] : List { _1 : Text, _2 : Text }
      , _purescriptFrontend = ""
      }

let GeneratorType =
      < SERVANT_API
      | SERVANT_API_DASHBOARD
      | API_TREE
      | API_TREE_DASHBOARD
      | API_TREE_COMMON
      | API_TREE_CLIENT
      | API_TYPES
      | DOMAIN_HANDLER
      | DOMAIN_HANDLER_DASHBOARD
      | BEAM_QUERIES
      | CACHED_QUERIES
      | BEAM_TABLE
      | DOMAIN_TYPE
      | SQL
      | PURE_SCRIPT_FRONTEND
      >

let ImportType = < SIMPLE | QUALIFIED >

let PackageImport =
      { _importType : ImportType
      , _importPackageName : Text
      , _importModuleName : Text
      }

let DefaultImports =
      { _qualifiedImports : List Text
      , _simpleImports : List Text
      , _packageImports : List PackageImport
      , _generationType : GeneratorType
      }

let defaultTypeImportMapper =
      [ { _1 = "Text", _2 = "Kernel.Prelude" }
      , { _1 = "Maybe", _2 = "Kernel.Prelude" }
      , { _1 = "Double", _2 = "Kernel.Prelude" }
      , { _1 = "TimeOfDay", _2 = "Kernel.Prelude" }
      , { _1 = "Day", _2 = "Data.Time.Calendar" }
      , { _1 = "Int", _2 = "Kernel.Prelude" }
      , { _1 = "Bool", _2 = "Kernel.Prelude" }
      , { _1 = "Id", _2 = "Kernel.Types.Id" }
      , { _1 = "ShortId", _2 = "Kernel.Types.Id" }
      , { _1 = "UTCTime", _2 = "Kernel.Prelude" }
      , { _1 = "Meters", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecMeters", _2 = "Kernel.Types.Common" }
      , { _1 = "Kilometers", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecMoney", _2 = "Kernel.Types.Common" }
      , { _1 = "Seconds", _2 = "Kernel.Types.Common" }
      , { _1 = "Distance", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecDistance", _2 = "Kernel.Types.Common" }
      , { _1 = "DistanceUnit", _2 = "Kernel.Types.Common" }
      , { _1 = "Money", _2 = "Kernel.Types.Common" }
      ]

let defaultImports =
      [ { _simpleImports =
          [ "EulerHS.Prelude", "Servant", "Tools.Auth", "Kernel.Utils.Common" ]
        , _qualifiedImports =
          [ "Kernel.Prelude", "Control.Lens", "Environment", "Kernel.Types.Id" ]
        , _packageImports =
              [ { _importType = ImportType.QUALIFIED
                , _importPackageName = "dynamic-offer-driver-app"
                , _importModuleName = "Environment"
                }
              ]
            : List PackageImport
        , _generationType = GeneratorType.SERVANT_API
        }
      , { _simpleImports =
          [ "EulerHS.Prelude hiding (id)"
          , "Servant"
          , "Tools.Auth"
          , "Data.OpenApi (ToSchema)"
          ]
        , _qualifiedImports = [ "Kernel.Prelude", "Kernel.Types.Id" ]
        , _packageImports = [] : List PackageImport
        , _generationType = GeneratorType.API_TYPES
        }
      , { _simpleImports =
          [ "EulerHS.Prelude hiding (id)"
          , "Servant"
          , "Tools.Auth"
          , "Data.OpenApi (ToSchema)"
          ]
        , _qualifiedImports = [ "Kernel.Prelude", "Kernel.Types.Id" ]
        , _packageImports = [] : List PackageImport
        , _generationType = GeneratorType.DOMAIN_HANDLER
        }
      ]

let ApiKind = < UI | DASHBOARD >

let ClientName = < OPERATIONS | FLEET | RIDE_BOOKING >

in  { _output = outputPath
    , _storageConfig =
      { _sqlTypeMapper = [] : List { _1 : Text, _2 : Text }
      , _extraDefaultFields = [] : List { _1 : Text, _2 : Text }
      , _defaultCachedQueryKeyPrefix = ""
      , _cacheFlowType = None Text
      }
    , _defaultImports = defaultImports
    , _defaultTypeImportMapper = defaultTypeImportMapper
    , _generate =
      [ GeneratorType.DOMAIN_HANDLER
      , GeneratorType.SERVANT_API
      , GeneratorType.API_TYPES
      ]
    , _packageMapping = [] : List { _1 : GeneratorType, _2 : Text }
    , _apiKind = ApiKind.UI
    , _serverName = None Text
    , _folderName = None Text
    , _migrationParams =
        [] : List { _migrationName : Text, _migrationParam : Optional Text }
    , _endpointPrefix = None Text
    , _apiDashboardPrefix = None Text
    , _serverNameTypePrefix = None Text
    }
