let common = ../provider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "Management"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
              defaultOutput._apiRelatedTypes ++ "/" ++ folderName
          , _extraApiRelatedTypes =
              defaultOutput._extraApiRelatedTypes ++ "/" ++ folderName
          , _domainHandler = defaultOutput._domainHandler ++ "/" ++ folderName
          , _domainHandlerDashboard =
              defaultOutput._domainHandlerDashboard ++ "/" ++ folderName
          , _servantApi = defaultOutput._servantApi ++ "/" ++ folderName
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/" ++ folderName
          }

let clientFunction =
      Some
        "ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations"

in      common.defaultConfigs
    //  { _output = outputPath
        , _clientFunction = clientFunction
        , _folderName = Some folderName
        }
