let common = ./common.dhall

in

{ port = +8020
, serverUrl = "https://api.sandbox.beckn.juspay.in/latest/registry"
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
}
