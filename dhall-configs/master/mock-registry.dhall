let common = ./common.dhall

let credRegistry =
  [
  -- Gateway
    { shortOrgId = "api.sandbox.beckn.juspay.in/latest/gateway/v1"
    , uniqueKeyId = "22"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/gateway/v1"
    }
  , { shortOrgId = "nsdl.co.in"
    , uniqueKeyId = "21"
    , signPubKey = "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="
    , url = "https://gateway-1.beckn.nsdl.co.in/v1"
    }

  -- FMD
  -- BAP
  , { shortOrgId = "JUSPAY.MOCK.FMD.BAP.DEV"
    , uniqueKeyId = "juspay-mock-fmd-bap-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "BECKN.FMD.BAP.DEV"
    , uniqueKeyId = "beckn-fmd-bap-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "SHOPX.FMD.BAP.DEV"
    , uniqueKeyId = "shopx-fmd-bap-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "PEPPO.FMD.BAP.DEV"
    , uniqueKeyId = "peppo-fmd-bap-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "PEPPO.LOCAL.FMD.BAP.DEV"
    , uniqueKeyId = "peppo-local-fmd-bap-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "PEPPO.DEV.FMD.BAP.DEV"
    , uniqueKeyId = "peppo-dev-fmd-bap-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "JUSPAY.FMD.BAP.DEV"
    , uniqueKeyId = "juspay-local-fmd-bap-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  -- BPP
  , { shortOrgId = "BECKN.FMD.BPP.DEV"
    , uniqueKeyId = "beckn-fmd-bpp-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "JUSPAY.MOCK.FMD.BPP.DEV"
    , uniqueKeyId = "juspay-mock-fmd-bpp-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "JUSPAY.DUNZO.FMD.BPP.DEV"
    , uniqueKeyId = "juspay-dunzo-fmd-bpp-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }
  , { shortOrgId = "LIGHTENING.LOGISTICS.FMD.BPP.DEV"
    , uniqueKeyId = "lightening-logistics-fmd-bpp-key-dev"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "FIXME"
    }

  -- Mobility
  -- BAP
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bap/cab/v1"
    , uniqueKeyId = "19"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bap/cab/v1"
    }
  -- BPP
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/565db72a-04d4-4211-90ae-c956461397b2"
    , uniqueKeyId = "12"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/565db72a-04d4-4211-90ae-c956461397b2"
    }
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/a45f243f-9915-4842-b78b-6d718844a48d"
    , uniqueKeyId = "13"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/a45f243f-9915-4842-b78b-6d718844a48d"
    }
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/fb6ee235-8cf5-4f8f-aba2-40de1fa733d1"
    , uniqueKeyId = "14"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/fb6ee235-8cf5-4f8f-aba2-40de1fa733d1"
    }
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/c26001a5-8a20-4e77-bebd-f9d7fce618bc"
    , uniqueKeyId = "15"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/c26001a5-8a20-4e77-bebd-f9d7fce618bc"
    }
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/384786e9-63e1-4f00-bbd9-40480387907d"
    , uniqueKeyId = "16"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/384786e9-63e1-4f00-bbd9-40480387907d"
    }
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/dc46e80a-99d7-4f96-9949-2c045106b081"
    , uniqueKeyId = "17"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/dc46e80a-99d7-4f96-9949-2c045106b081"
    }
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/092ef105-6fe6-4eab-9c6f-e8a57b51e1af"
    , uniqueKeyId = "18"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/092ef105-6fe6-4eab-9c6f-e8a57b51e1af"
    }

  -- Metro
  -- BAP
  , { shortOrgId = "api.sandbox.beckn.juspay.in/latest/bap/metro/v1"
    , uniqueKeyId = "20"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/latest/bap/metro/v1"
    }
  -- BPP
  , { shortOrgId = "api.sandbox.beckn.juspay.in/bpp/metro/v1"
    , uniqueKeyId = "25"
    , signPubKey = "OGfSqt352PXRfdd+pLXo3eLLd96iL8dcbireMynl5A4="
    , url = "https://api.sandbox.beckn.juspay.in/bpp/metro/v1"
    }

  -- Parking
  -- BAP
  , { shortOrgId = "api.sandbox.beckn.juspay.in/dev/bap/parking/v1"
    , uniqueKeyId = "56"
    , signPubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    , url = "https://api.sandbox.beckn.juspay.in/dev/bap/parking/v1"
    }
  -- BPP
  , { shortOrgId = "shop.pinpark.co.in"
    , uniqueKeyId = "shop.pinpark.co.in"
    , signPubKey = "uglwgKDEm+MePsU//L6BAgB5SPb29YMZn/jTWPNFL6c="
    , url = "http://shop.pinpark.co.in" -- FIXME
    }
  ]

in

{ port = +8020
, credRegistry = credRegistry
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-registry.log"}
}
