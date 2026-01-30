let rootDir = env:GIT_ROOT_PATH

let outputPrefixDashboardReadOnly =
      rootDir ++ "/Backend/app/unified-dashboard/src-read-only/"

let outputPrefixDashboard = rootDir ++ "/Backend/app/unified-dashboard/src/"

let outputPrefixCommonApisReadOnly =
      rootDir ++ "/Backend/app/unified-dashboard/src-read-only/API/Types/"

let outputPrefixCommonApis = rootDir ++ "/Backend/app/unified-dashboard/src/"

let outputPrefixDriverAppReadOnly =
          rootDir
      ++  "/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/"

let outputPrefixDriverApp =
          rootDir
      ++  "/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/"

let outputPrefixRiderAppReadOnly =
      rootDir ++ "/Backend/app/rider-platform/rider-app/Main/src-read-only/"

let outputPrefixRiderApp =
      rootDir ++ "/Backend/app/rider-platform/rider-app/Main/src/"

let migrationPath =
      rootDir ++ "/Backend/dev/migrations-read-only/unified-dashboard/"

let outputPath =
      { _apiRelatedTypes = outputPrefixCommonApisReadOnly
      , _extraApiRelatedTypes = outputPrefixCommonApis ++ "/Extra"
      , _extraApiRelatedCommonTypes = outputPrefixCommonApis ++ "/Common"
      , _beamQueries = outputPrefixDashboardReadOnly ++ "Storage/Queries"
      , _extraBeamQueries = outputPrefixDashboard ++ "Storage/Queries/"
      , _cachedQueries =
          outputPrefixDashboardReadOnly ++ "Storage/CachedQueries"
      , _extraCachedQueries = outputPrefixDashboard ++ "Storage/CachedQueries"
      , _beamTable = outputPrefixDashboardReadOnly ++ "Storage/Beam"
      , _domainHandlerDashboard = outputPrefixDashboard ++ "Domain/Action"
      , _domainType = outputPrefixDashboardReadOnly ++ "Domain/Types"
      , _servantApiDashboard = outputPrefixDashboardReadOnly ++ "API/Action"
      , _servantApiClient = outputPrefixDashboardReadOnly ++ "API/Client"
      , _sql = [ { _1 = migrationPath, _2 = "atlas_dashboard" } ]
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
      , { _1 = "String", _2 = "Kernel.Prelude" }
      , { _1 = "Maybe", _2 = "Kernel.Prelude" }
      , { _1 = "Double", _2 = "Kernel.Prelude" }
      , { _1 = "Float", _2 = "Kernel.Prelude" }
      , { _1 = "TimeOfDay", _2 = "Kernel.Prelude" }
      , { _1 = "Day", _2 = "Data.Time.Calendar" }
      , { _1 = "Int", _2 = "Kernel.Prelude" }
      , { _1 = "Integer", _2 = "Kernel.Prelude" }
      , { _1 = "Bool", _2 = "Kernel.Prelude" }
      , { _1 = "NonEmpty", _2 = "Kernel.Prelude" }
      , { _1 = "Either", _2 = "Kernel.Prelude" }
      , { _1 = "Id", _2 = "Kernel.Types.Id" }
      , { _1 = "ShortId", _2 = "Kernel.Types.Id" }
      , { _1 = "UTCTime", _2 = "Kernel.Prelude" }
      , { _1 = "FilePath", _2 = "Kernel.Prelude" }
      , { _1 = "Meters", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecMeters", _2 = "Kernel.Types.Common" }
      , { _1 = "Kilometers", _2 = "Kernel.Types.Common" }
      , { _1 = "Money", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecMoney", _2 = "Kernel.Types.Common" }
      , { _1 = "Seconds", _2 = "Kernel.Types.Common" }
      , { _1 = "Minutes", _2 = "Kernel.Types.Common" }
      , { _1 = "Centesimal", _2 = "Kernel.Types.Common" }
      , { _1 = "Currency", _2 = "Kernel.Types.Common" }
      , { _1 = "Price", _2 = "Kernel.Types.Common" }
      , { _1 = "PriceAPIEntity", _2 = "Kernel.Types.Common" }
      , { _1 = "Distance", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecDistance", _2 = "Kernel.Types.Common" }
      , { _1 = "DistanceUnit", _2 = "Kernel.Types.Common" }
      , { _1 = "ServerName", _2 = "Domain.Types.AccessMatrix" }
      , { _1 = "APISuccess", _2 = "Kernel.Types.APISuccess" }
      , { _1 = "Merchant", _2 = "Domain.Types.Merchant" }
      ]

let extraDefaultFields =
      [ { _1 = "merchantId", _2 = "Maybe (Id Merchant)" }
      , { _1 = "createdAt", _2 = "UTCTime" }
      , { _1 = "updatedAt", _2 = "UTCTime" }
      ]

let sqlMapper =
      [ { _1 = "\\[Text\\]", _2 = "text[]" }
      , { _1 = "Text", _2 = "text" }
      , { _1 = "\\[Id ", _2 = "text[]" }
      , { _1 = "Id ", _2 = "character varying(36)" }
      , { _1 = "\\[ShortId ", _2 = "text[]" }
      , { _1 = "ShortId ", _2 = "character varying(36)" }
      , { _1 = "Int", _2 = "integer" }
      , { _1 = "Double", _2 = "double precision" }
      , { _1 = "HighPrecMoney", _2 = "double precision" }
      , { _1 = "Money", _2 = "integer" }
      , { _1 = "Bool", _2 = "boolean" }
      , { _1 = "UTCTime", _2 = "timestamp with time zone" }
      , { _1 = "TimeOfDay", _2 = "time without time zone" }
      , { _1 = "Day", _2 = "date" }
      , { _1 = "Seconds", _2 = "integer" }
      , { _1 = "Kilometers", _2 = "integer" }
      , { _1 = "Meters", _2 = "integer" }
      , { _1 = "Currency", _2 = "character varying(255)" }
      , { _1 = "HighPrecDistance", _2 = "double precision" }
      , { _1 = "DistanceUnit", _2 = "character varying(255)" }
      ]

let mkDefaultImports =
      \(serverPackageName : Text) ->
        [ { _simpleImports =
            [ "EulerHS.Prelude"
            , "Servant"
            , "Tools.Auth"
            , "Kernel.Utils.Common"
            ]
          , _qualifiedImports =
            [ "Domain.Types.Person"
            , "Kernel.Prelude"
            , "Control.Lens"
            , "Domain.Types.Merchant"
            , "Environment"
            , "Kernel.Types.Id"
            , "Kernel.Types.Beckn.Context"
            ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.SERVANT_API
          }
        , { _simpleImports =
            [ "EulerHS.Prelude hiding (sortOn)"
            , "Servant"
            , "Tools.Auth.Api"
            , "Kernel.Utils.Common hiding (INFO)"
            , "Storage.Beam.CommonInstances ()"
            ]
          , _qualifiedImports =
            [ "Domain.Types.Person"
            , "Kernel.Prelude"
            , "Control.Lens"
            , "Domain.Types.Merchant"
            , "Environment"
            , "Kernel.Types.Id"
            , "Kernel.Types.Beckn.Context"
            ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.SERVANT_API_DASHBOARD
          }
        , { _simpleImports =
            [ "EulerHS.Prelude"
            , "Tools.Auth.Api"
            , "Tools.Auth.Merchant"
            , "Kernel.Utils.Common"
            , "Storage.Beam.CommonInstances ()"
            ]
          , _qualifiedImports =
            [ "Kernel.Prelude"
            , "Domain.Types.Person"
            , "Domain.Types.Merchant"
            , "Environment"
            , "Kernel.Types.Id"
            , "Kernel.Types.Beckn.Context"
            ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.DOMAIN_HANDLER_DASHBOARD
          }
        , { _simpleImports =
            [ "EulerHS.Prelude hiding (id, state)"
            , "Servant"
            , "Data.OpenApi (ToSchema)"
            , "Servant.Client"
            , "Kernel.Types.Common"
            ]
          , _qualifiedImports =
            [ "Kernel.Prelude"
            , "Domain.Types.Person"
            , "Domain.Types.Merchant"
            , "Environment"
            , "Kernel.Types.Id"
            , "EulerHS.Types"
            , "Kernel.Types.APISuccess"
            , "Kernel.Types.HideSecrets"
            , "Kernel.Storage.Esqueleto"
            , "Data.Singletons.TH"
            ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.API_TYPES
          }
        , { _simpleImports = [ "Servant" ]
          , _qualifiedImports =
            [ "Domain.Types.Merchant"
            , "Environment"
            , "Kernel.Types.Beckn.Context"
            , "Kernel.Types.Id"
            ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.API_TREE
          }
        , { _simpleImports = [ "Servant" ]
          , _qualifiedImports =
            [ "Domain.Types.Merchant"
            , "Environment"
            , "Kernel.Types.Beckn.Context"
            , "Kernel.Types.Id"
            ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.API_TREE_DASHBOARD
          }
        , { _simpleImports = [ "EulerHS.Prelude", "Data.OpenApi (ToSchema)" ]
          , _qualifiedImports =
            [ "Kernel.Storage.Esqueleto"
            , "Text.Show"
            , "Text.Read"
            , "Dashboard.Common"
            , "Data.List"
            , "Data.Singletons.TH"
            ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.API_TREE_COMMON
          }
        , { _simpleImports = [ "Kernel.Prelude", "Servant" ]
          , _qualifiedImports =
            [ "Kernel.Types.Beckn.City"
            , "Domain.Types.Merchant"
            , "Domain.Types.AccessMatrix"
            , "Tools.Auth.Merchant"
            , "Tools.Client"
            ]
          , _packageImports =
            [ { _importType = ImportType.QUALIFIED
              , _importPackageName = serverPackageName
              , _importModuleName = "API.UnifiedDashboard"
              }
            ]
          , _generationType = GeneratorType.API_TREE_CLIENT
          }
        , { _simpleImports =
            [ "EulerHS.Prelude hiding (id)"
            , "Servant"
            , "Tools.Auth"
            , "Data.OpenApi (ToSchema)"
            ]
          , _qualifiedImports =
            [ "Kernel.Prelude"
            , "Domain.Types.Person"
            , "Domain.Types.Merchant"
            , "Environment"
            , "Kernel.Types.Id"
            , "Kernel.Types.Beckn.Context"
            ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.DOMAIN_HANDLER
          }
        , { _simpleImports = [ "Data.Aeson", "Kernel.Utils.Dhall" ] : List Text
          , _qualifiedImports = [ "!Tools.Beam.UtilsTH" ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.DOMAIN_TYPE
          }
        , { _simpleImports =
            [ "Kernel.Prelude"
            , "Tools.Beam.UtilsTH"
            , "Kernel.External.Encryption"
            ]
          , _qualifiedImports = [ "Database.Beam as B" ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.BEAM_TABLE
          }
        , { _simpleImports =
            [ "Kernel.Beam.Functions"
            , "Kernel.Prelude"
            , "Kernel.External.Encryption"
            , "Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)"
            , "Kernel.Types.Error"
            ]
          , _qualifiedImports = [ "Sequelize as Se" ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.BEAM_QUERIES
          }
        , { _simpleImports = [ "Kernel.Prelude", "Kernel.Utils.Common" ]
          , _qualifiedImports = [ "Kernel.Storage.Hedis as Hedis" ]
          , _packageImports = [] : List PackageImport
          , _generationType = GeneratorType.CACHED_QUERIES
          }
        ]

let ApiKind = < UI | DASHBOARD >

let ClientName = < OPERATIONS | FLEET | RIDE_BOOKING >

let defaultConfigs =
      { _output = outputPath
      , _storageConfig =
        { _sqlTypeMapper = sqlMapper
        , _extraDefaultFields = extraDefaultFields
        , _defaultCachedQueryKeyPrefix = "unifiedDashboard"
        }
      , _defaultTypeImportMapper = defaultTypeImportMapper
      , _generate =
        [ GeneratorType.DOMAIN_TYPE
        , GeneratorType.BEAM_TABLE
        , GeneratorType.BEAM_QUERIES
        , GeneratorType.CACHED_QUERIES
        , GeneratorType.DOMAIN_HANDLER
        , GeneratorType.DOMAIN_HANDLER_DASHBOARD
        , GeneratorType.API_TYPES
        , GeneratorType.SERVANT_API
        , GeneratorType.SERVANT_API_DASHBOARD
        , GeneratorType.API_TREE
        , GeneratorType.API_TREE_DASHBOARD
        , GeneratorType.API_TREE_COMMON
        , GeneratorType.API_TREE_CLIENT
        , GeneratorType.SQL
        ]
      , _packageMapping = [] : List { _1 : GeneratorType, _2 : Text }
      , _apiKind = ApiKind.DASHBOARD
      , _serverName = None Text
      , _endpointPrefix = Some "Unified"
      , _folderName = None Text
      , _apiDashboardPrefix = Some "API.UnifiedDashboard"
      , _serverNameTypePrefix = Some "Domain.Types.AccessMatrix"
      , _migrationParams =
          [] : List { _migrationName : Text, _migrationParam : Optional Text }
      }

in  { defaultConfigs
    , ClientName
    , outputPrefixDriverApp
    , outputPrefixDriverAppReadOnly
    , outputPrefixRiderApp
    , outputPrefixRiderAppReadOnly
    , GeneratorType
    , outputPath
    , outputPrefixDashboardReadOnly
    , outputPrefixDashboard
    , outputPrefixCommonApisReadOnly
    , outputPrefixCommonApis
    , migrationPath
    , mkDefaultImports
    }
