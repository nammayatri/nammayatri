{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DSearch
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Station as Station
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.Queries.FRFSSearch as SearchQ
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Station as QS
import qualified Storage.Queries.Station as QStation
import Tools.Auth
import Tools.Error

getFrfsStations :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Station.FRFSVehicleType -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
getFrfsStations (mbPersonId, merchantId_) vehicleType_ = do
  personId_ <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  _ <- B.runInReplica $ QP.findById personId_ >>= fromMaybeM (PersonNotFound personId_.getId)
  _ <- B.runInReplica $ CQM.findById merchantId_ >>= fromMaybeM (MerchantNotFound merchantId_.getId)
  stations <- B.runInReplica $ QS.getTicketPlacesByVehicleType vehicleType_
  return $
    map
      ( \Station.Station {..} ->
          FRFSTicketService.FRFSStationAPI
            { color = Nothing,
              stationType = Nothing,
              ..
            }
      )
      stations

postFrfsSearch :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Station.FRFSVehicleType -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearch (personId, merchantId) mVehicleType req = do
  fromStation <- QStation.findByStationCode req.from >>= fromMaybeM (InvalidRequest "Invalid from station code")
  toStation <- QStation.findByStationCode req.from >>= fromMaybeM (InvalidRequest "Invalid from station code")

  searchReqId <- generateGUID
  now <- getCurrentTime

  let searchReq =
        DSearch.FRFSSearch
          { id = searchReqId,
            fromStationId = fromStation.id.getId,
            quantity = req.quantity,
            toStationId = toStation.id.getId,
            vehicleType = vehicleType,
            merchantId = Just merchantId,
            merchantOperatingCityId = Nothing,
            createdAt = now,
            updatedAt = now
          }
  SearchQ.create searchReq
  return $ FRFSSearchAPIRes searchReqId

getFrfsSearchQuote :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
getFrfsSearchQuote = error "Logic yet to be decided"

postFrfsQuoteConfirm :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm = error "Logic yet to be decided"

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus = error "Logic yet to be decided"

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList = error "Logic yet to be decided"
