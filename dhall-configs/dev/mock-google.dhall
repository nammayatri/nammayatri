let common = ./common.dhall

let useRealGoogle = Some common.googleCfg

let doNotUseRealGoogle =
      None { googleMapsUrl : Text, googleRoadsUrl : Text, googleKey : Text }

in  { port = +8019
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/mock-google.log" }
    , graceTerminationPeriod = +90
    , mockDataPath = "./app/mocks/google/mock-data/"
    , googleCfg = doNotUseRealGoogle
    }
