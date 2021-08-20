let common = ./common.dhall

in

{ port = +8020
, serverUrl = "http://localhost:8020/"
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
}
