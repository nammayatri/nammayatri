{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.TicketBooking where

import Control.Lens.Operators
import Data.Aeson
import Data.OpenApi
import qualified Data.Text as T
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

allTicketBookingStatus :: [BookingStatus]
allTicketBookingStatus = [minBound .. maxBound]

data BookingStatus = Pending | Failed | Booked | Cancelled deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, ToJSON, FromJSON)

instance ToSchema BookingStatus where
  declareNamedSchema proxy = do
    genericDeclareNamedSchema customSchemaOptions proxy
    where
      customSchemaOptions = defaultSchemaOptions {datatypeNameModifier = const "TicketBookingStatus"}

instance ToParamSchema BookingStatus where
  toParamSchema _ =
    mempty
      & title ?~ "TicketBookingStatus"
      & type_ ?~ OpenApiString
      & enum_
        ?~ map (String . T.pack . show) allTicketBookingStatus

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BookingStatus)

$(mkHttpInstancesForEnum ''BookingStatus)
