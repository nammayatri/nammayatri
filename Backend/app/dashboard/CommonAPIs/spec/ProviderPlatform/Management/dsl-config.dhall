let common = ../provider-dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes = defaultOutput._apiRelatedTypes ++ "/Management"
          , _servantApi = defaultOutput._servantApi ++ "/Management"
          , _servantApiDashboard =
              defaultOutput._servantApiDashboard ++ "/Management"
          }

let clientMapper =
      [ { _1 = common.ClientName.OPERATIONS
        , _2 =
            "ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations"
        }
      ]

in      common.defaultConfigs
    //  { _output = outputPath, _clientMapper = clientMapper }
