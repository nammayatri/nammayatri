{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.CachedQuote where

import Data.Aeson
import qualified Domain.Types.FRFSQuote as Quote
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data CachedQuoteD = CachedQuoteD
  { price :: Price,
    stationsJson :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
