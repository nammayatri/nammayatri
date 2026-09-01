{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Booking
  ( ConfirmAdvSeatBookingReq (..),
    confirmAdvSeatBooking,
    GetPickupPointsReq (..),
    getPickupPointsCached,
    AddBlockSeatsReq (..),
    GetTotalFareReq (..),
    getPickupPoints,
    addBlockSeats,
    getTotalFare,
  )
where

import qualified Data.Text as T
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Extra.IntegratedBPPConfig (TNSTCConfig)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Client (callTnstc)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Types
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import qualified Text.XML as XML
import Text.XML.Writer (ToXML (..), XML, element, elementA)

type TnstcFlow m r = (MonadFlow m, EncFlow m r, Metrics.CoreMetrics m, HasField "requestId" r (Maybe Text))

fmtDate :: Day -> Text
fmtDate = T.pack . formatTime defaultTimeLocale "%d/%m/%Y"

op :: Text -> XML.Name
op n = XML.Name n (Just setcNamespace) (Just "com")

arg0 :: XML.Name
arg0 = XML.Name "arg0" Nothing Nothing

el :: Text -> Text -> XML
el n v = elementA (XML.Name n Nothing Nothing) ([] :: [(XML.Name, Text)]) (v :: Text)

data GetPickupPointsReq = GetPickupPointsReq
  { rqppCounterCode :: Text,
    rqppJourneyDate :: Day,
    rqppServiceId :: Text,
    rqppPlaceId :: Text,
    rqppUserName :: Text
  }

instance ToXML GetPickupPointsReq where
  toXML req =
    element (op "GetAllServicePickupPointsByServiceID") $
      element arg0 $ do
        el "counterCode" req.rqppCounterCode
        el "franchiseeUser" "false"
        el "journeyDate" (fmtDate req.rqppJourneyDate)
        el "serviceID" req.rqppServiceId
        el "startPlaceID" req.rqppPlaceId
        el "userName" req.rqppUserName

data AddBlockSeatsReq = AddBlockSeatsReq
  { rqbsClassId :: Text,
    rqbsCounterCode :: Text,
    rqbsCreatedBy :: Text,
    rqbsEndPlaceId :: Text,
    rqbsJourneyDate :: Day,
    rqbsLayoutId :: Text,
    rqbsSeatNumbers :: [Text],
    rqbsServiceId :: Text,
    rqbsStartPlaceId :: Text,
    rqbsTotalAdults :: Int,
    rqbsUserName :: Text,
    rqbsWsRefNo :: Text
  }

instance ToXML AddBlockSeatsReq where
  toXML req =
    element (op "AddBlockSeats") $
      element arg0 $ do
        el "classID" req.rqbsClassId
        el "counterCode" req.rqbsCounterCode
        el "createdBy" req.rqbsCreatedBy
        el "endPlaceID" req.rqbsEndPlaceId
        el "franchiseeUser" "false"
        el "journeyDate" (fmtDate req.rqbsJourneyDate)
        el "layoutID" req.rqbsLayoutId
        forM_ req.rqbsSeatNumbers (el "seatNumber")
        el "serviceID" req.rqbsServiceId
        el "startPlaceID" req.rqbsStartPlaceId
        el "totalNumberOfAdults" (show req.rqbsTotalAdults)
        el "userName" req.rqbsUserName
        el "WSRefNo" req.rqbsWsRefNo

data GetTotalFareReq = GetTotalFareReq
  { rqtfAdultMale :: Int,
    rqtfAdultFemale :: Int,
    rqtfChildMale :: Int,
    rqtfChildFemale :: Int,
    rqtfClassId :: Text,
    rqtfConcessionTypeId :: Text,
    rqtfCounterCode :: Text,
    rqtfCreatedBy :: Text,
    rqtfEndPlaceCode :: Text,
    rqtfEndPlaceId :: Text,
    rqtfJourneyDate :: Day,
    rqtfPickupPointDropOffId :: Text,
    rqtfPickupPointPlaceId :: Text,
    rqtfSeatBlockIds :: [Text],
    rqtfSeatNumbers :: [Text],
    rqtfServiceId :: Text,
    rqtfStartPlaceCode :: Text,
    rqtfStartPlaceId :: Text,
    rqtfUserName :: Text,
    rqtfWsRefNo :: Text
  }

instance ToXML GetTotalFareReq where
  toXML req =
    element (op "GetTotalFareDetailsOfTicket") $ do
      element arg0 $ do
        el "adultFemale" (show req.rqtfAdultFemale)
        el "adultMale" (show req.rqtfAdultMale)
        el "childFemale" (show req.rqtfChildFemale)
        el "childMale" (show req.rqtfChildMale)
        el "classID" req.rqtfClassId
        el "concessionTypeId" req.rqtfConcessionTypeId
        el "counterCode" req.rqtfCounterCode
        el "createdBy" req.rqtfCreatedBy
        el "endPlaceCode" req.rqtfEndPlaceCode
        el "endPlaceID" req.rqtfEndPlaceId
        el "franchiseeUser" "false"
        el "journeyDate" (fmtDate req.rqtfJourneyDate)
        el "pickupPointDropOffId" req.rqtfPickupPointDropOffId
        el "pickupPointPlaceId" req.rqtfPickupPointPlaceId
        forM_ req.rqtfSeatBlockIds (el "seatBlockIds")
        forM_ req.rqtfSeatNumbers (el "seatNumber")
        el "serviceID" req.rqtfServiceId
        el "startPlaceCode" req.rqtfStartPlaceCode
        el "startPlaceID" req.rqtfStartPlaceId
        el "totalNumberOfAdults" (show (req.rqtfAdultMale + req.rqtfAdultFemale))
        el "totalNumberOfChild" (show (req.rqtfChildMale + req.rqtfChildFemale))
        el "totalNumberOfSeats" (show (length req.rqtfSeatNumbers))
        el "userName" req.rqtfUserName
        el "WSRefNo" req.rqtfWsRefNo
      elementA (XML.Name "arg1" Nothing Nothing) ([] :: [(XML.Name, Text)]) ("O" :: Text)

-- | ConfirmAdvSeatBooking. Field set follows the vendor's working sample; only serviceID,
-- createdBy, totalFare, journeyDate, startPlaceID, WSRefNo and addnlAge are actually
-- mandatory (verified by blanking each against staging), but the fee components are sent
-- as returned because TNSTC stores them verbatim and never recomputes them.
data ConfirmAdvSeatBookingReq = ConfirmAdvSeatBookingReq
  { rqcAdultOrChild :: Text,
    rqcAddnlAdultOrChilds :: [Text],
    rqcAdultMale :: Int,
    rqcAdultFemale :: Int,
    rqcChildMale :: Int,
    rqcChildFemale :: Int,
    rqcAge :: Text,
    rqcGender :: Text,
    rqcPassengerName :: Text,
    rqcAddnlPassengerNames :: [Text],
    rqcAddnlAges :: [Text],
    rqcAddnlGenders :: [Text],
    rqcEmailId :: Text,
    rqcPhoneNumber :: Text,
    rqcBasicFare :: Text,
    rqcTotalFare :: Text,
    rqcClassId :: Text,
    rqcConcessionTypeId :: Text,
    rqcCounterCode :: Text,
    rqcCreatedBy :: Text,
    rqcEndPlaceCode :: Text,
    rqcEndPlaceId :: Text,
    rqcJourneyDate :: Day,
    rqcPickupPointDropOffId :: Text,
    rqcPickupPointPlaceId :: Text,
    rqcPickupPointTime :: Text,
    rqcPickupPointDropOffTime :: Text,
    rqcSeatBlockIds :: [Text],
    rqcSeatNumbers :: [Text],
    rqcServiceId :: Text,
    rqcStartPlaceCode :: Text,
    rqcStartPlaceId :: Text,
    rqcUserName :: Text,
    rqcWsRefNo :: Text
  }

instance ToXML ConfirmAdvSeatBookingReq where
  toXML req =
    element (op "ConfirmAdvSeatBooking") $ do
      element arg0 $ do
        forM_ req.rqcAddnlAdultOrChilds (el "addnlAdultOrChild")
        forM_ req.rqcAddnlAges (el "addnlAge")
        forM_ req.rqcAddnlGenders (el "addnlGender")
        forM_ req.rqcAddnlPassengerNames (el "addnlPasngrName")
        el "adultFemale" (show req.rqcAdultFemale)
        el "adultMale" (show req.rqcAdultMale)
        el "adultOrChild" req.rqcAdultOrChild
        el "advanceOrCurrentBooking" "Y"
        el "age" req.rqcAge
        el "basicFare" req.rqcBasicFare
        el "childFemale" (show req.rqcChildFemale)
        el "childMale" (show req.rqcChildMale)
        el "classID" req.rqcClassId
        el "concessionTypeId" req.rqcConcessionTypeId
        el "counterCode" req.rqcCounterCode
        el "createdBy" req.rqcCreatedBy
        el "emailId" req.rqcEmailId
        el "endPlaceCode" req.rqcEndPlaceCode
        el "endPlaceID" req.rqcEndPlaceId
        el "gender" req.rqcGender
        el "journeyDate" (fmtDate req.rqcJourneyDate)
        el "onewayOrReturnTrip" "O"
        el "passengerName" req.rqcPassengerName
        el "phoneNumber" req.rqcPhoneNumber
        el "pickupPointDropOffId" req.rqcPickupPointDropOffId
        el "pickupPointDropOffTime" req.rqcPickupPointDropOffTime
        el "pickupPointPlaceId" req.rqcPickupPointPlaceId
        el "pickupPointTime" req.rqcPickupPointTime
        el "returnServiceID" "0"
        forM_ req.rqcSeatBlockIds (el "seatBlockIds")
        forM_ req.rqcSeatNumbers (const (el "seatIDs" ""))
        forM_ req.rqcSeatNumbers (el "seatNumber")
        el "serviceID" req.rqcServiceId
        el "startPlaceCode" req.rqcStartPlaceCode
        el "startPlaceID" req.rqcStartPlaceId
        el "ticketNumber" "1"
        el "totalFare" req.rqcTotalFare
        el "userName" req.rqcUserName
        el "WSRefNo" req.rqcWsRefNo
      element (XML.Name "arg1" Nothing Nothing) $ do
        el "counterCode" req.rqcCounterCode
        el "createdBy" req.rqcCreatedBy
        el "franchiseeUser" "false"
        el "idProofLookupId" "166"
        el "idProofRefernce" ""
        el "userName" req.rqcUserName

confirmAdvSeatBooking :: TnstcFlow m r => TNSTCConfig -> ConfirmAdvSeatBookingReq -> m TnstcBookingResult
confirmAdvSeatBooking config req = callTnstc config "ConfirmAdvSeatBooking" req parseBookingResult

-- | Boarding points for a service are fixed for the day, so they are cached for an hour and
-- the vendor call becomes the cache-miss path. /seats fetches them first, which means confirm
-- resolving the rider's chosen point is normally a cache hit rather than a fourth SOAP round
-- trip on an already slow path.
getPickupPointsCached :: (TnstcFlow m r, CacheFlow m r) => TNSTCConfig -> Text -> GetPickupPointsReq -> m [TnstcPickupPoint]
getPickupPointsCached config cacheScope req =
  IM.withInMemCache ["tnstcPickupPoints", cacheScope, req.rqppServiceId, fmtDate req.rqppJourneyDate, req.rqppPlaceId] 3600 $
    getPickupPoints config req

getPickupPoints :: TnstcFlow m r => TNSTCConfig -> GetPickupPointsReq -> m [TnstcPickupPoint]
getPickupPoints config req = callTnstc config "GetAllServicePickupPointsByServiceID" req parsePickupPoints

addBlockSeats :: TnstcFlow m r => TNSTCConfig -> AddBlockSeatsReq -> m TnstcBlockResult
addBlockSeats config req = callTnstc config "AddBlockSeats" req parseBlockResult

getTotalFare :: TnstcFlow m r => TNSTCConfig -> GetTotalFareReq -> m TnstcFareResult
getTotalFare config req = callTnstc config "GetTotalFareDetailsOfTicket" req parseFareResult
