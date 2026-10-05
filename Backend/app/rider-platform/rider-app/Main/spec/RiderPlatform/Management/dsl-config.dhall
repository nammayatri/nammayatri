let common = ../dashboard-common.dhall

let defaultOutput = common.defaultConfigs._output

let folderName = "Management"

let outputPath =
          defaultOutput
      //  { _apiRelatedTypes =
              defaultOutput._apiRelatedTypes ++ "/" ++ folderName
          , _extraApiRelatedTypes =
              defaultOutput._extraApiRelatedTypes ++ "/" ++ folderName
          , _servantApi = defaultOutput._servantApi ++ "/" ++ folderName
          , _servantApiDashboardAuth =
              defaultOutput._servantApiDashboardAuth ++ "/" ++ folderName
          }

let serverName = Some "APP_BACKEND_MANAGEMENT"

in      common.defaultConfigs
    //  { _output = outputPath
        , _serverName = serverName
        , _folderName = Some folderName
        , -- Marks the generators whose output lands INSIDE rider-app, so an
          -- `importPackageOverrides: <Mod>: "rider-app"` entry emits `"this"`
          -- there (self-package imports are illegal).
          -- Mirrors RideBooking/ and AppManagement/.
          -- NOTE: unlike RideBooking/ and AppManagement/, API_TYPES is NOT
          -- mapped here: this folder's api types are generated into CommonAPIs
          -- (dashboard-helper-api), not rider-app, so mapping them would make
          -- the generated client import them from the wrong package.
          _packageMapping =
          [ { _1 = common.GeneratorType.SERVANT_API, _2 = "rider-app" }
          , { _1 = common.GeneratorType.DOMAIN_HANDLER, _2 = "rider-app" }
          ]
        }
