let rootDir = env:GIT_ROOT_PATH

let outputPrefixReadOnly =
      rootDir ++ "/Backend/lib/shared-services/src-read-only/"

let outputPrefix = rootDir ++ "/Backend/lib/shared-services/src/"

let ridermigrationPath =
      rootDir ++ "/Backend/dev/migrations-read-only/rider-app/"

let drivermigrationPath =
      rootDir ++ "/Backend/dev/migrations-read-only/dynamic-offer-driver-app/"

let sqlOutputPath =
      [ { _1 = ridermigrationPath, _2 = "atlas_app" }
      , { _1 = drivermigrationPath, _2 = "atlas_driver_offer_bpp" }
      ]

let outputPath =
      { _apiRelatedTypes = outputPrefixReadOnly ++ "API/Types/UI"
      , _beamQueries = outputPrefixReadOnly ++ "Storage/Queries"
      , _extraBeamQueries = outputPrefix ++ "Storage/Queries/"
      , _beamTable = outputPrefixReadOnly ++ "Storage/Beam"
      , _domainHandler = outputPrefix ++ "Domain/Action/UI"
      , _domainType = outputPrefixReadOnly ++ "Domain/Types"
      , _servantApi = outputPrefixReadOnly ++ "API/Action/UI"
      , _sql = sqlOutputPath
      , _purescriptFrontend = ""
      }

let GeneratorType =
      < SERVANT_API
      | API_TYPES
      | DOMAIN_HANDLER
      | BEAM_QUERIES
      | BEAM_TABLE
      | DOMAIN_TYPE
      | SQL
      | PURE_SCRIPT_FRONTEND
      >

let DefaultImports =
      { _qualifiedImports : List Text
      , _simpleImports : List Text
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
      ]

let extraDefaultFields =
      [ { _1 = "merchantId", _2 = "Maybe (Id IssueManagement.Common.Merchant)" }
      , { _1 = "merchantOperatingCityId"
        , _2 = "Maybe (Id IssueManagement.Common.MerchantOperatingCity)"
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
          ]
        , _generationType = GeneratorType.SERVANT_API
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
          ]
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
          ]
        , _generationType = GeneratorType.DOMAIN_HANDLER
        }
      , { _simpleImports = [] : List Text
        , _qualifiedImports = [ "!Kernel.Beam.Lib.UtilsTH" ]
        , _generationType = GeneratorType.DOMAIN_TYPE
        }
      , { _simpleImports =
          [ "Kernel.Prelude"
          , "Kernel.Beam.Lib.UtilsTH"
          , "Kernel.External.Encryption"
          ]
        , _qualifiedImports = [ "Database.Beam as B" ]
        , _generationType = GeneratorType.BEAM_TABLE
        }
      , { _simpleImports =
          [ "Kernel.Beam.Functions"
          , "Kernel.Prelude"
          , "Kernel.External.Encryption"
          , "Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)"
          , "Kernel.Types.Error"
          , "Kernel.Beam.Lib.UtilsTH (HasSchemaName)"
          ]
        , _qualifiedImports = [ "Sequelize as Se" ]
        , _generationType = GeneratorType.BEAM_QUERIES
        }
      ]

in  { _output = outputPath
    , _storageConfig =
      { _sqlTypeMapper = sqlMapper, _extraDefaultFields = extraDefaultFields }
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
      ]
    }
