let rootDir = env:GIT_ROOT_PATH

let common = ../issue-management-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "IssueManagement"

let outputPrefixCommonApisReadOnly =
      rootDir ++ "/Backend/lib/shared-services/src-read-only/"

let outputPrefixCommonApis = rootDir ++ "/Backend/lib/shared-services/src/"

let outputPrefixAppReadOnly =
      rootDir ++ "/Backend/app/rider-platform/rider-app/Main/src-read-only/"

let outputPrefixApp =
      rootDir ++ "/Backend/app/rider-platform/rider-app/Main/src/"

let migrationPath = rootDir ++ "/Backend/dev/migrations-read-only/dashboard/"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
                  outputPrefixCommonApisReadOnly
              ++  "API/Types/RiderPlatform/"
              ++  folderName
          , _extraApiRelatedTypes = ""
          , _extraApiRelatedCommonTypes =
              outputPrefixCommonApis ++ "IssueManagement/Common/Dashboard"
          , _domainHandler =
              outputPrefixApp ++ "Domain/Action/Dashboard/" ++ folderName
          , _domainHandlerDashboard = ""
          , _domainType = ""
          , _servantApi = ""
          , _servantApiDashboardAuth =
                  outputPrefixAppReadOnly
              ++  "API/Action/DashboardAuth/"
              ++  folderName
          , _servantApiDashboard = ""
          , _servantApiClient = ""
          , _sql = [ { _1 = migrationPath, _2 = "atlas_dashboard" } ]
          }

let defaultStorageConfig = common.defaultConfigs._storageConfig

let storageConfig =
          defaultStorageConfig
      //  { _defaultCachedQueryKeyPrefix = "riderDashboard" }

let defaultImports = common.mkDefaultImports "rider-app"

let serverName = Some "APP_BACKEND_MANAGEMENT"

in      common.defaultConfigs
    //  { _output = outputPath
        , _storageConfig = storageConfig
        , _defaultImports = defaultImports
        , _serverName = serverName
        , _endpointPrefix = Some "Rider"
        , _folderName = Some folderName
        }
