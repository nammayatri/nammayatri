let sqlOutputPathPrefix = "Backend/dev/migrations-read-only"

let readOnlySrcFolder = "src-read-only"

let srcFolder = "src"

let rideAppName = "rider-app"

let driverAppName = "dynamic-offer-driver-app"

let riderAppPath = "Backend/app/rider-platform/${rideAppName}/Main"

let driverAppPath = "Backend/app/provider-platform/${driverAppName}/Main"

in  { riderApp =
      { inputFileConfigs =
        { storageYaml = "${riderAppPath}/spec/Storage"
        , apiYaml = "${riderAppPath}/spec/API"
        }
      , outputFileConfigs =
        { beamTableOutputFilePath =
            "${riderAppPath}/${readOnlySrcFolder}/Storage/Beam"
        , beamQueriesOutputFilePath =
            "${riderAppPath}/${readOnlySrcFolder}/Storage/Queries"
        , domainTypeOutputFilePath =
            "${riderAppPath}/${readOnlySrcFolder}/Domain/Types"
        , sqlOutputFilePath = "${sqlOutputPathPrefix}/${rideAppName}"
        , servantAPIOutputFilePath =
            "${riderAppPath}/${readOnlySrcFolder}/API/Action/UI"
        , apiTypesOutputFilePath =
            "${riderAppPath}/${readOnlySrcFolder}/API/Types/UI"
        , domainHandlerOutputFilePath =
            "${riderAppPath}/${srcFolder}/Domain/Action/UI"
        }
      }
    , driverApp =
      { inputFileConfigs =
        { storageYaml = "${driverAppPath}/Storage"
        , apiYaml = "${riderAppPath}/API"
        }
      , outputFileConfigs =
        { beamTableOutputFilePath =
            "${driverAppPath}/${readOnlySrcFolder}/Storage/Beam"
        , beamQueriesOutputFilePath =
            "${driverAppPath}/${readOnlySrcFolder}/Storage/Queries"
        , domainTypeOutputFilePath =
            "${driverAppPath}/${readOnlySrcFolder}/Domain/Types"
        , sqlOutputFilePath = "${sqlOutputPathPrefix}/${rideAppName}"
        , servantAPIOutputFilePath =
            "${driverAppPath}/${readOnlySrcFolder}/API/Action/UI"
        , apiTypesOutputFilePath =
            "${driverAppPath}/${readOnlySrcFolder}/API/Types/UI"
        , domainHandlerOutputFilePath =
            "${driverAppPath}/${srcFolder}/Domain/Action/UI"
        }
      }
    }
