{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | JSON instances for the generated PartnerAuth API types. The generated types
-- (src-read-only) only derive Generic/Show/ToSchema; Servant '[JSON] needs
-- FromJSON/ToJSON. Default (camelCase) encoding matches the PWA contract
-- ({ token } / { isValid, sessionToken, mobileNumber, name }).
module API.Types.UI.PartnerAuthExtra
  ( module API.Types.UI.PartnerAuth,
    module API.Types.UI.PartnerAuthExtra,
  )
where

import API.Types.UI.PartnerAuth
import Data.Aeson

instance FromJSON PartnerSessionReq where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PartnerSessionReq where
  toJSON = genericToJSON defaultOptions

instance FromJSON PartnerSessionRes where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON PartnerSessionRes where
  toJSON = genericToJSON defaultOptions
