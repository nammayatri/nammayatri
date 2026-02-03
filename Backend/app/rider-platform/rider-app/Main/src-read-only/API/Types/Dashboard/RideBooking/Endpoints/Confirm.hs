{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Confirm where

import qualified "this" API.UI.Confirm
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.Extra.MerchantPaymentMethod
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Quote
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Payment.Interface
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("confirm" :> PostConfirmRideSearchQuotesConfirmHelper)

type PostConfirmRideSearchQuotes =
  ( "rideSearch" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "quotes"
      :> Capture
           "quoteId"
           (Kernel.Types.Id.Id Domain.Types.Quote.Quote)
      :> "confirm"
      :> QueryParam
           "paymentMethodId"
           Kernel.External.Payment.Interface.PaymentMethodId
      :> QueryParam
           "paymentInstrument"
           Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument
      :> QueryParam
           "isAdvancedBookingEnabled"
           Kernel.Prelude.Bool
      :> Post
           '[JSON]
           API.UI.Confirm.ConfirmRes
  )

type PostConfirmRideSearchQuotesConfirmHelper =
  ( "rideSearch" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> "quotes"
      :> Capture
           "quoteId"
           (Kernel.Types.Id.Id Domain.Types.Quote.Quote)
      :> "confirm"
      :> QueryParam "agentId" Kernel.Prelude.Text
      :> QueryParam
           "paymentMethodId"
           Kernel.External.Payment.Interface.PaymentMethodId
      :> QueryParam
           "paymentInstrument"
           Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument
      :> QueryParam
           "isAdvancedBookingEnabled"
           Kernel.Prelude.Bool
      :> QueryParam
           "requiresPaymentBeforeConfirm"
           Kernel.Prelude.Bool
      :> Post
           '[JSON]
           API.UI.Confirm.ConfirmRes
  )

newtype ConfirmAPIs = ConfirmAPIs {postConfirmRideSearchQuotes :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Quote.Quote -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.External.Payment.Interface.PaymentMethodId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient API.UI.Confirm.ConfirmRes}

mkConfirmAPIs :: (Client EulerHS.Types.EulerClient API -> ConfirmAPIs)
mkConfirmAPIs confirmClient = (ConfirmAPIs {..})
  where
    postConfirmRideSearchQuotes = confirmClient

data ConfirmUserActionType
  = POST_CONFIRM_RIDE_SEARCH_QUOTES
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON ConfirmUserActionType where
  toJSON POST_CONFIRM_RIDE_SEARCH_QUOTES = Data.Aeson.String "POST_CONFIRM_RIDE_SEARCH_QUOTES"

instance FromJSON ConfirmUserActionType where
  parseJSON (Data.Aeson.String "POST_CONFIRM_RIDE_SEARCH_QUOTES") = pure POST_CONFIRM_RIDE_SEARCH_QUOTES
  parseJSON _ = fail "POST_CONFIRM_RIDE_SEARCH_QUOTES expected"

$(Data.Singletons.TH.genSingletons [''ConfirmUserActionType])
