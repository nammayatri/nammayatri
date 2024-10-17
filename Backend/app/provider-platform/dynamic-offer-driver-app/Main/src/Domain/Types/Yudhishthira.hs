{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

import qualified Data.Aeson as A
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types.Application as YA
import qualified Lib.Yudhishthira.Types.Common as YTC
import qualified Lib.Yudhishthira.TypesTH as YTH

data TagData = TagData
  { searchRequest :: DSR.SearchRequest,
    area :: Text,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data EndRideTagData = EndRideTagData
  { ride :: DRide.Ride,
    booking :: SRB.Booking
  }
  deriving (Generic, Show, FromJSON, ToJSON)

$(YTH.generateGenericDefault ''TagData)
$(YTH.generateGenericDefault ''EndRideTagData)

instance YTC.LogicInputLink YA.ApplicationEvent where
  getLogicInputDef a =
    case a of
      YA.Search -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @TagData)
      YA.RideEnd -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @EndRideTagData)
      _ -> Nothing
