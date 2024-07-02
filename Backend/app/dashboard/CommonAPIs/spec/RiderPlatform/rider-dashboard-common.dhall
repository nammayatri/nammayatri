let rootDir = env:GIT_ROOT_PATH

let outputPrefixDashboardReadOnly =
      rootDir ++ "/Backend/app/dashboard/rider-dashboard/src-read-only/"

let outputPrefixDashboard =
      rootDir ++ "/Backend/app/dashboard/rider-dashboard/src/"

let outputPrefixCommonApisReadOnly =
      rootDir ++ "/Backend/app/dashboard/CommonAPIs/src-read-only/"

let outputPrefixCommonApis = rootDir ++ "/Backend/app/dashboard/CommonAPIs/src/"

let outputPrefixRiderAppReadOnly =
      rootDir ++ "/Backend/app/rider-platform/rider-app/Main/src-read-only/"

let outputPrefixRiderApp =
      rootDir ++ "/Backend/app/rider-platform/rider-app/Main/src/"

let migrationPath =
      rootDir ++ "/Backend/dev/migrations-read-only/rider-dashboard/"

let outputPath =
      { _apiRelatedTypes =
          outputPrefixCommonApisReadOnly ++ "API/Types/RiderPlatform"
      , _extraApiRelatedTypes =
          outputPrefixCommonApis ++ "Dashboard/RiderPlatform"
      , _beamQueries = outputPrefixDashboardReadOnly ++ "Storage/Queries"
      , _extraBeamQueries = outputPrefixDashboardReadOnly ++ "Storage/Queries"
      , _cachedQueries =
          outputPrefixDashboardReadOnly ++ "Storage/CachedQueries"
      , _extraCachedQueries =
          outputPrefixDashboardReadOnly ++ "Storage/CachedQueries"
      , _beamTable = outputPrefixDashboardReadOnly ++ "Storage/Beam"
      , _domainHandler = outputPrefixRiderApp ++ "Domain/Action/Dashboard"
      , _domainHandlerDashboard =
          outputPrefixDashboard ++ "Domain/Action/RiderPlatform"
      , _domainType = outputPrefixDashboardReadOnly ++ "Domain/Types"
      , _servantApi = outputPrefixRiderAppReadOnly ++ "API/Action/Dashboard"
      , _servantApiDashboard =
          outputPrefixDashboardReadOnly ++ "API/Action/RiderPlatform"
      , _sql = [ { _1 = migrationPath, _2 = "atlas_driver_offer_bpp" } ]
      , _purescriptFrontend = ""
      }

let GeneratorType =
      < SERVANT_API
      | SERVANT_API_DASHBOARD
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
      , { _1 = "NonEmpty", _2 = "Kernel.Prelude" }
      , { _1 = "Id", _2 = "Kernel.Types.Id" }
      , { _1 = "ShortId", _2 = "Kernel.Types.Id" }
      , { _1 = "UTCTime", _2 = "Kernel.Prelude" }
      , { _1 = "BaseUrl", _2 = "Kernel.Prelude" }
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
      , { _1 = "APISuccess", _2 = "Kernel.Types.APISuccess" }
      ]

let extraDefaultFields =
      [ { _1 = "merchantId", _2 = "Maybe (Id Merchant)" }
      , { _1 = "merchantOperatingCityId"
        , _2 = "Maybe (Id MerchantOperatingCity)"
        }
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

let defaultImports =
      [ { _simpleImports =
          [ "EulerHS.Prelude", "Servant", "Tools.Auth", "Kernel.Utils.Common" ]
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
          [ "EulerHS.Prelude"
          , "Servant"
          , "Tools.Auth.Api"
          , "Kernel.Utils.Common"
          , "Storage.Beam.CommonInstances ()"
          ]
        , _qualifiedImports =
          [ "Domain.Types.Person"
          , "Kernel.Prelude"
          , "Control.Lens"
          , "Kernel.Types.Id"
          , "Kernel.Types.Beckn.Context"
          , "RiderPlatformClient.DynamicOfferDriver"
          ]
        , _packageImports =
          [ { _importType = ImportType.QUALIFIED
            , _importPackageName = "lib-dashboard"
            , _importModuleName = "Domain.Types.Merchant"
            }
          , { _importType = ImportType.QUALIFIED
            , _importPackageName = "lib-dashboard"
            , _importModuleName = "Environment"
            }
          ]
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
          [ "Domain.Types.Person"
          , "Kernel.Prelude"
          , "Kernel.Types.Id"
          , "Kernel.Types.Beckn.Context"
          , "SharedLogic.Transaction"
          ]
        , _packageImports =
          [ { _importType = ImportType.QUALIFIED
            , _importPackageName = "lib-dashboard"
            , _importModuleName = "Domain.Types.Merchant"
            }
          , { _importType = ImportType.QUALIFIED
            , _importPackageName = "lib-dashboard"
            , _importModuleName = "Environment"
            }
          ]
        , _generationType = GeneratorType.DOMAIN_HANDLER_DASHBOARD
        }
      , { _simpleImports =
          [ "EulerHS.Prelude hiding (id)"
          , "Servant"
          , "Data.OpenApi (ToSchema)"
          , "Servant.Client"
          ]
        , _qualifiedImports =
          [ "Kernel.Prelude"
          , "Domain.Types.Person"
          , "Domain.Types.Merchant"
          , "Environment"
          , "Kernel.Types.Id"
          , "EulerHS.Types"
          , "Kernel.Types.APISuccess"
          ]
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

let ClientName = < OPERATIONS | FLEET | RIDE_BOOKING >

let ApiKind = < UI | DASHBOARD >

let defaultConfigs =
      { _output = outputPath
      , _storageConfig =
        { _sqlTypeMapper = sqlMapper
        , _extraDefaultFields = extraDefaultFields
        , _defaultCachedQueryKeyPrefix = "riderDashboard"
        }
      , _defaultImports = defaultImports
      , _defaultTypeImportMapper = defaultTypeImportMapper
      , _generate =
        [ GeneratorType.DOMAIN_HANDLER
        , GeneratorType.DOMAIN_HANDLER_DASHBOARD
        , GeneratorType.API_TYPES
        , GeneratorType.SERVANT_API
        , GeneratorType.SERVANT_API_DASHBOARD
        ]
      , _apiKind = ApiKind.DASHBOARD
      , _clientFunction = None
      }

in  { defaultConfigs, ClientName, outputPrefixRiderApp }
