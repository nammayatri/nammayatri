let rootDir = env:GIT_ROOT_PATH

let common = ../issue-management-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "IssueManagement"

let outputPrefixCommonApisReadOnly =
      rootDir ++ "/Backend/lib/shared-services/src-read-only/"

let outputPrefixCommonApis = rootDir ++ "/Backend/lib/shared-services/src/"

let outputPrefixAppReadOnly =
          rootDir
      ++  "/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/"

let outputPrefixApp =
          rootDir
      ++  "/Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/"

let migrationPath = rootDir ++ "/Backend/dev/migrations-read-only/dashboard/"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
                  outputPrefixCommonApisReadOnly
              ++  "API/Types/ProviderPlatform/"
              ++  folderName
          , _extraApiRelatedTypes = ""
          , _extraApiRelatedCommonTypes =
              outputPrefixCommonApis ++ "IssueManagement/Common/Dashboard"
          , _domainHandler =
              outputPrefixApp ++ "Domain/Action/Dashboard/" ++ folderName
          , _domainHandlerDashboard = ""
          , _domainType = ""
          , _servantApi =
              outputPrefixAppReadOnly ++ "API/Action/Dashboard/" ++ folderName
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
      //  { _defaultCachedQueryKeyPrefix = "providerDashboard" }

let defaultImports = common.mkDefaultImports "dynamic-offer-driver-app"

let serverName = Some "DRIVER_OFFER_BPP_MANAGEMENT"

in      common.defaultConfigs
    //  { _output = outputPath
        , _storageConfig = storageConfig
        , _defaultImports = defaultImports
        , _serverName = serverName
        , _endpointPrefix = Some "Provider"
        , _folderName = Some folderName
        }
