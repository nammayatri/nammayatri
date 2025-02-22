{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.TicketBooking where

import Control.Lens.Operators
import Data.Aeson
import Data.OpenApi
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

allTicketBookingStatus :: [BookingStatus]
allTicketBookingStatus = [minBound .. maxBound]

data BookingStatus = Pending | Failed | Booked | Cancelled | RefundInitiated deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, ToJSON, FromJSON)

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
