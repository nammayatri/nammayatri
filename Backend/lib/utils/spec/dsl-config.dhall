let rootDir = env:GIT_ROOT_PATH

let outputPrefixReadOnly = rootDir ++ "/Backend/lib/utils/src-read-only/"

let outputPrefix = rootDir ++ "/Backend/lib/utils/src/"

let driverMigrationPath =
      rootDir ++ "/Backend/dev/migrations-read-only/dynamic-offer-driver-app/"

let riderMigrationPath =
      rootDir ++ "/Backend/dev/migrations-read-only/rider-app/"

let outputPath =
      { _apiRelatedTypes = outputPrefixReadOnly ++ "Lib/Utils/API/Types"
      , _extraApiRelatedTypes = ""
      , _extraApiRelatedCommonTypes = ""
      , _beamQueries = outputPrefixReadOnly ++ "Lib/Utils/Storage/Queries"
      , _extraBeamQueries = outputPrefix ++ "Lib/Utils/Storage/Queries/"
      , _cachedQueries =
          outputPrefixReadOnly ++ "Lib/Utils/Storage/CachedQueries"
      , _extraCachedQueries = outputPrefix ++ "Lib/Utils/Storage/CachedQueries/"
      , _beamTable = outputPrefixReadOnly ++ "Lib/Utils/Storage/Beam"
      , _domainHandler = outputPrefix ++ "Lib/Utils/Domain/Action"
      , _domainHandlerDashboard = ""
      , _domainType = outputPrefixReadOnly ++ "Lib/Utils/Types"
      , _servantApi = outputPrefixReadOnly ++ "Lib/Utils/API/Action"
      , _servantApiDashboard = ""
      , _servantApiClient = ""
      , _sql =
        [ { _1 = driverMigrationPath, _2 = "atlas_driver_offer_bpp" }
        , { _1 = riderMigrationPath, _2 = "atlas_app" }
        ]
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
      , { _1 = "Money", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecMoney", _2 = "Kernel.Types.Common" }
      , { _1 = "Seconds", _2 = "Kernel.Types.Common" }
      , { _1 = "Currency", _2 = "Kernel.Types.Common" }
      , { _1 = "Price", _2 = "Kernel.Types.Common" }
      , { _1 = "PriceAPIEntity", _2 = "Kernel.Types.Common" }
      , { _1 = "Distance", _2 = "Kernel.Types.Common" }
      , { _1 = "HighPrecDistance", _2 = "Kernel.Types.Common" }
      , { _1 = "DistanceUnit", _2 = "Kernel.Types.Common" }
      ]

let extraDefaultFields =
      [ { _1 = "createdAt", _2 = "UTCTime" }
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
      , { _1 = "Hours", _2 = "integer" }
      , { _1 = "Kilometers", _2 = "integer" }
      , { _1 = "Meters", _2 = "integer" }
      , { _1 = "Currency", _2 = "character varying(255)" }
      , { _1 = "HighPrecDistance", _2 = "double precision" }
      , { _1 = "DistanceUnit", _2 = "character varying(255)" }
      ]

let defaultImports =
      [ { _simpleImports =
          [ "EulerHS.Prelude", "Servant", "Tools.Auth", "Kernel.Utils.Common" ]
        , _qualifiedImports =
          [ "Kernel.Prelude", "Control.Lens", "Environment", "Kernel.Types.Id" ]
        , _packageImports = [] : List PackageImport
        , _generationType = GeneratorType.SERVANT_API
        }
      , { _simpleImports =
          [ "EulerHS.Prelude hiding (id)"
          , "Servant"
          , "Tools.Auth"
          , "Data.OpenApi (ToSchema)"
          ]
        , _qualifiedImports =
          [ "Kernel.Prelude", "Environment", "Kernel.Types.Id" ]
        , _packageImports = [] : List PackageImport
        , _generationType = GeneratorType.API_TYPES
        }
      , { _simpleImports =
          [ "EulerHS.Prelude hiding (id)"
          , "Servant"
          , "Tools.Auth"
          , "Data.OpenApi (ToSchema)"
          ]
        , _qualifiedImports =
          [ "Kernel.Prelude", "Environment", "Kernel.Types.Id" ]
        , _packageImports = [] : List PackageImport
        , _generationType = GeneratorType.DOMAIN_HANDLER
        }
      , { _simpleImports = [ "Data.Aeson" ] : List Text
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

in  { _output = outputPath
    , _storageConfig =
      { _sqlTypeMapper = sqlMapper
      , _extraDefaultFields = extraDefaultFields
      , _defaultCachedQueryKeyPrefix = "utils"
      }
    , _defaultImports = defaultImports
    , _defaultTypeImportMapper = defaultTypeImportMapper
    , _generate =
      [ GeneratorType.DOMAIN_TYPE
      , GeneratorType.BEAM_TABLE
      , GeneratorType.BEAM_QUERIES
      , GeneratorType.DOMAIN_HANDLER
      , GeneratorType.SERVANT_API
      , GeneratorType.API_TYPES
      , GeneratorType.SQL
      , GeneratorType.CACHED_QUERIES
      ]
    , _packageMapping = [] : List { _1 : GeneratorType, _2 : Text }
    , _apiKind = ApiKind.UI
    , _serverName = None Text
    , _folderName = None Text
    , _migrationParams =
        [] : List { _migrationName : Text, _migrationParam : Optional Text }
    , _endpointPrefix = None Text
    }
