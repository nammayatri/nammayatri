{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

-- import qualified Domain.Types.SearchRequest
-- import qualified Domain.Types.Location
-- import qualified Kernel.Types.Price
-- import qualified Kernel.Types.Distance
-- import qualified Kernel.Types.Time

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude

-- import qualified Domain.Types.YudhishthiraTH as T

data EnumType = Enum1T | Enum2T
  deriving (Generic, Show, ToJSON)

data TagData = TagData
  { searchRequest :: DSR.SearchRequest,
    area :: Text,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    enumTest :: EnumType
  }
  deriving (Generic, Show, ToJSON)

data EndRideTagData = EndRideTagData
  { ride :: DRide.Ride,
    booking :: SRB.Booking
  }
  deriving (Generic, Show, FromJSON, ToJSON)
