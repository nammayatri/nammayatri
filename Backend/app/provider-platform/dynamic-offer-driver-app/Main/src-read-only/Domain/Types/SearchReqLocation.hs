{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchReqLocation where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.External.Maps.HasCoordinates
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.GenericPretty
import qualified Tools.Beam.UtilsTH

data SearchReqLocation = SearchReqLocation
  { area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    extras :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    full_address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.SearchReqLocation.SearchReqLocation,
    instructions :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show, Kernel.Utils.GenericPretty.PrettyShow, Kernel.External.Maps.HasCoordinates.HasCoordinates)
