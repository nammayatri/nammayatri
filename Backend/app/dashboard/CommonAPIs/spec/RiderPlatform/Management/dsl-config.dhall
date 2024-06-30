let common = ../rider-dashboard-common.dhall

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
        , _2 = "RiderPlatformClient.RiderApp.Operations.callRiderAppOperations"
        }
      ]

in      common.defaultConfigs
    //  { _output = outputPath, _clientMapper = clientMapper }
