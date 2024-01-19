{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DFRFSSearch
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
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Station as QS
import qualified Storage.Queries.Station as QStation
import Tools.Auth
import Tools.Error

getFrfsStations :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Station.FRFSVehicleType -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
getFrfsStations _ vehicleType_ = do
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

postFrfsSearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Station.FRFSVehicleType -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearch (_, merchantId) vehicleType_ FRFSSearchAPIReq {..} = do
  _ <- QStation.findById fromStationId >>= fromMaybeM (InvalidRequest "Invalid from station id")
  _ <- QStation.findById toStationId >>= fromMaybeM (InvalidRequest "Invalid to station id")

  searchReqId <- generateGUID
  now <- getCurrentTime

  let searchReq =
        DFRFSSearch.FRFSSearch
          { id = searchReqId,
            vehicleType = vehicleType_,
            merchantId = Just merchantId,
            merchantOperatingCityId = Nothing,
            createdAt = now,
            updatedAt = now,
            ..
          }
  QFRFSSearch.create searchReq
  return $ FRFSSearchAPIRes searchReqId

getFrfsSearchQuote :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
getFrfsSearchQuote = error "Logic yet to be decided"

-- _ <- QFRFSSearch.findById searchId >>= fromMaybeM (InvalidRequest "Invalid search id")
-- quotes <- B.runInReplica $ QFRFSQuote.findBySearchId searchId
-- mapM (\DFRFSQuote{..} -> do
--   trips <- B.runInReplica $ QFRFSTrip.findAllByQuoteId id
--   let trips' = sortBy (\tripA tripB -> compare tripA.stopSequence tripB.stopSequence) trips
--   map ()
--   return $ FRFSTicketService.FRFSQuoteAPIRes
--     {
--       quoteId = id,
--       ..
--     }) quotes

-- -- let vehicleType_ = searchReq.vehicleType
-- -- stations <- B.runInReplica $ QS.getTicketPlacesByMerchantOperatingCityIdAndVehicleType (DFRFSSearch.merchantOperatingCityId searchReq) vehicleType_
-- -- let stationIds = map (Kernel.Types.Id.getId . Station.id) stations
-- quotes <- B.runInReplica $ QFRFSQuote.findBySearchId searchId
-- return $
--   map
--     ( \DFRFSQuote.FRFSQuote {..} ->
--         FRFSTicketService.FRFSQuoteAPIRes
--           { stations = filter (\s -> Station.id s `elem` stationIds) stations,
--             quoteId = id,
--             ..
--           }
--     )
--     quotes

postFrfsQuoteConfirm :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm = error "Logic yet to be decided"

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus = error "Logic yet to be decided"

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList = error "Logic yet to be decided"
