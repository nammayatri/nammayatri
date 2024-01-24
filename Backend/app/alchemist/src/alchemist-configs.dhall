let sqlOutputPathPrefix = "Backend/dev/migrations-read-only"

let readOnlySrcFolder = "src-read-only"

let srcFolder = "src"

let rideAppName = "rider-app"

let driverAppName = "dynamic-offer-driver-app"

let riderAppPath = "Backend/app/rider-platform/${rideAppName}/Main"

let driverAppPath = "Backend/app/provider-platform/${driverAppName}/Main"

in  { api =
      { inputPath = "${riderAppPath}/spec/API"
      , outputPath = "${riderAppPath}/${readOnlySrcFolder}"
      , enabled = True
      , output = {}
      }
    , storage =
      { inputPath = "${riderAppPath}/spec/Storage"
      , outputPath = "${riderAppPath}/${readOnlySrcFolder}"
      , sqlOutputPath = "${sqlOutputPathPrefix}/${rideAppName}"
      , enabled = True
      }
    }
