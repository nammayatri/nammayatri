module Domain.Types.Tickets.TicketPlace where

import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import Kernel.Types.Id

data TicketPlace = TicketPlace
  { id :: Id TicketPlace,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    name :: Text,
    description :: Maybe Text,
    lat :: Maybe Double,
    lon :: Maybe Double,
    gallery :: [Text],
    openTimings :: Maybe TimeOfDay,
    closeTimings :: Maybe TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
