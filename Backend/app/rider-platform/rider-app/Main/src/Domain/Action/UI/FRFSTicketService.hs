{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.FRFSBookingStatusAPIRes
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteAPI
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearchAPIReq
import qualified Domain.Types.FRFSSearchAPIRes
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatusAPIRes
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getFrfsStations :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow [Domain.Types.Station.Station]
getFrfsStations = error "Logic yet to be decided"

postFrfsSearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Domain.Types.FRFSSearchAPIReq.FRFSSearchAPIReq -> Environment.Flow Domain.Types.FRFSSearchAPIRes.FRFSSearchAPIRes
postFrfsSearch = error "Logic yet to be decided"

getFrfsSearchQuote :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [Domain.Types.FRFSQuoteAPI.FRFSQuoteAPI]
getFrfsSearchQuote = error "Logic yet to be decided"

postFrfsQuoteConfirm :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Environment.Flow Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm = error "Logic yet to be decided"

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Environment.Flow Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.Flow Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus = error "Logic yet to be decided"

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) Environment.Flow [Domain.Types.FRFSBookingStatusAPIRes.FRFSBookingStatusAPIRes]
getFrfsBookingList = error "Logic yet to be decided"
