let common = ./common.dhall

let domain = < MOBILITY
              | FINAL_MILE_DELIVERY
              | LOCAL_RETAIL
              | FOOD_AND_BEVERAGE
              | HEALTHCARE
              | METRO
              | PARKING
              >

let type = < BAP
            | BPP
            | BG
            | LREG
            | CREG
            | RREG
            >

let credRegistry =
  [ { shortOrgId = "mobility-app"
    , uniqueKeyId = "mobility-app-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8013/cab/v1"
    , _type = type.BAP
    , domain = domain.MOBILITY
    }
  , { shortOrgId = "fmd-test-app"
    , uniqueKeyId = "fmd-test-app-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8019/v1"
    , _type = type.BPP
    , domain = domain.FINAL_MILE_DELIVERY
    }
  , { shortOrgId = "JUSPAY.BG.1"
    , uniqueKeyId = "juspay-bg-1-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8015/v1"
    , _type = type.BG
    , domain = domain.MOBILITY
    }
  , { shortOrgId = "JUSPAY.FMD.UAT.1"
    , uniqueKeyId = "juspay-fmd-1-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8018/v1"
    , _type = type.BPP
    , domain = domain.FINAL_MILE_DELIVERY
    }
  , { shortOrgId = "JUSPAY.MOBILITY.APP.UAT.1"
    , uniqueKeyId = "juspay-mobility-bap-1-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8013/cab/v1"
    , _type = type.BAP
    , domain = domain.MOBILITY
    }
  , { shortOrgId = "JUSPAY.MOBILITY.APP.UAT.2"
    , uniqueKeyId = "juspay-mobility-bap-1-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8013/metro/v1"
    , _type = type.BAP
    , domain = domain.METRO
    }
  , { shortOrgId = "JUSPAY.MOBILITY.APP.UAT.3"
    , uniqueKeyId = "juspay-mobility-bap-1-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8022/beckn" -- CHECK ME
    , _type = type.BAP
    , domain = domain.PARKING
    }
  , { shortOrgId = "JUSPAY.MOBILITY.PROVIDER.UAT.1"
    , uniqueKeyId = "juspay-mobility-bpp-1-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f"
    , _type = type.BPP
    , domain = domain.MOBILITY
    }
  , { shortOrgId = "another-test-cabs"
    , uniqueKeyId = "juspay-mobility-bpp-1-key"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "http://localhost:8014/v1/e1f37274-f0aa-4bb3-93a0-2476349487b7"
    , _type = type.BPP
    , domain = domain.MOBILITY
    }
  , { shortOrgId = "NSDL.BG.1"
    , uniqueKeyId = "nsdl_bg_1"
    , signPubKey = "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="
    , url = "https://gateway-1.beckn.nsdl.co.in/v1"
    , _type = type.BG
    , domain = domain.MOBILITY
    }
  , { shortOrgId = "metro-bpp"
    , uniqueKeyId = "metro-bpp-key"
    , signPubKey = "OGfSqt352PXRfdd+pLXo3eLLd96iL8dcbireMynl5A4="
    , url = "http://localhost:8000"
    , _type = type.BPP
    , domain = domain.METRO
    }
  ]

in

{ port = +8020
, credRegistry = credRegistry
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-registry.log"}
}
