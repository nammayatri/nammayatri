let dslInputPathPrefix = "Backend/app/alchemist/spec"

let haskellOutputPathPrefix = "app"

let sqlOutputPathPrefix = "dev/migrations-read-only"

let rideAppName = "rider-app"

let driverAppName = "dynamic-offer-driver-app"

let riderAppPath = "rider-platform/${rideAppName}/Main"

let driverAppPath = "provider-platform/${driverAppName}/Main"

let readOnlySrcFolder = "src-read-only"

let srcFolder = "src"

in  { riderApp =
      { inputFileConfigs =
        { storageYaml = "${dslInputPathPrefix}/${riderAppPath}/Storage"
        , apiYaml = "${dslInputPathPrefix}/${riderAppPath}/API"
        }
      , outputFileConfigs =
        { beamTableOutputFilePath =
            "${haskellOutputPathPrefix}/${riderAppPath}/${readOnlySrcFolder}/Storage/Beam"
        , beamQueriesOutputFilePath =
            "${haskellOutputPathPrefix}/${riderAppPath}/${readOnlySrcFolder}/Storage/Queries"
        , domainTypeOutputFilePath =
            "${haskellOutputPathPrefix}/${riderAppPath}/${readOnlySrcFolder}/Domain/Types"
        , sqlOutputFilePath = "${sqlOutputPathPrefix}/${rideAppName}"
        , servantAPIOutputFilePath =
            "${haskellOutputPathPrefix}/${riderAppPath}/${readOnlySrcFolder}/API/Action/UI"
        , apiTypesOutputFilePath =
            "${haskellOutputPathPrefix}/${riderAppPath}/${readOnlySrcFolder}/API/Types/UI"
        , domainHandlerOutputFilePath =
            "${haskellOutputPathPrefix}/${riderAppPath}/${srcFolder}/Domain/Action/UI"
        }
      }
    , driverApp =
      { inputFileConfigs =
        { storageYaml = "${dslInputPathPrefix}/${driverAppPath}/Storage"
        , apiYaml = "${dslInputPathPrefix}/${riderAppPath}/API"
        }
      , outputFileConfigs =
        { beamTableOutputFilePath =
            "${haskellOutputPathPrefix}/${driverAppPath}/${readOnlySrcFolder}/Storage/Beam"
        , beamQueriesOutputFilePath =
            "${haskellOutputPathPrefix}/${driverAppPath}/${readOnlySrcFolder}/Storage/Queries"
        , domainTypeOutputFilePath =
            "${haskellOutputPathPrefix}/${driverAppPath}/${readOnlySrcFolder}/Domain/Types"
        , sqlOutputFilePath = "${sqlOutputPathPrefix}/${rideAppName}"
        , servantAPIOutputFilePath =
            "${haskellOutputPathPrefix}/${driverAppPath}/${readOnlySrcFolder}/API/Action/UI"
        , apiTypesOutputFilePath =
            "${haskellOutputPathPrefix}/${driverAppPath}/${readOnlySrcFolder}/API/Types/UI"
        , domainHandlerOutputFilePath =
            "${haskellOutputPathPrefix}/${driverAppPath}/${srcFolder}/Domain/Action/UI"
        }
      }
    }
