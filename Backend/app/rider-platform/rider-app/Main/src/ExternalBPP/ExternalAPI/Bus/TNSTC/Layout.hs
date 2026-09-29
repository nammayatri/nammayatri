{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Layout
  ( GetConcessionTypesReq (..),
    GetServiceSeatDetailsReq (..),
    getConcessionTypes,
    getAddressPlaceList,
    getIdProofTypes,
    getServiceSeatDetails,
  )
where

import Data.Time (Day)
import Domain.Types.Extra.IntegratedBPPConfig (TNSTCConfig)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Client (TnstcFlow, arg0, callTnstc, el, fmtDate, op)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Types
import Kernel.Prelude
import qualified Kernel.Storage.InMem as IM
import Kernel.Utils.Common
import Text.XML.Writer (ToXML (..), XML, element)

data GetConcessionTypesReq = GetConcessionTypesReq
  { rqctClassId :: Text,
    rqctCounterCode :: Maybe Text,
    rqctEndPlaceId :: Text,
    rqctJourneyDate :: Day,
    rqctSeatNumbers :: [Text],
    rqctServiceId :: Text,
    rqctStartPlaceId :: Text,
    rqctTotalSeats :: Int,
    rqctUserName :: Text
  }

instance ToXML GetConcessionTypesReq where
  toXML req =
    element (op "GetAllConcessionTypesByServiceID") $
      element arg0 $ do
        el "classID" req.rqctClassId
        whenJust req.rqctCounterCode (el "counterCode")
        el "endPlaceID" req.rqctEndPlaceId
        el "franchiseeUser" "false"
        el "journeyDate" (fmtDate req.rqctJourneyDate)
        forM_ req.rqctSeatNumbers (el "seatNumber")
        el "serviceID" req.rqctServiceId
        el "startPlaceID" req.rqctStartPlaceId
        el "totalNumberOfSeats" (show req.rqctTotalSeats)
        el "userName" req.rqctUserName

data GetServiceSeatDetailsReq = GetServiceSeatDetailsReq
  { rqssCounterCode :: Maybe Text,
    rqssEndPlaceId :: Text,
    rqssJourneyDate :: Day,
    rqssServiceClass :: Text,
    rqssServiceId :: Text,
    rqssStartPlaceId :: Text,
    rqssSingleLady :: Bool,
    rqssUserName :: Text
  }

instance ToXML GetServiceSeatDetailsReq where
  toXML req =
    element (op "GetServiceSeatDetails") $
      element arg0 $ do
        whenJust req.rqssCounterCode (el "counterCode")
        el "endPlaceID" req.rqssEndPlaceId
        el "journeyDate" (fmtDate req.rqssJourneyDate)
        el "serviceClass" req.rqssServiceClass
        el "serviceID" req.rqssServiceId
        el "startPlaceID" req.rqssStartPlaceId
        when req.rqssSingleLady $ do
          el "totFemales" "1"
          el "totMales" "0"
        el "userName" req.rqssUserName

-- | The whole place master (569 rows). Takes no arg0 at all -- sending one is an
-- unmarshalling error. Used for the placeID -> stateCode mapping.
data GetAddressPlaceListReq = GetAddressPlaceListReq

instance ToXML GetAddressPlaceListReq where
  toXML _ = element (op "GetAddressPlaceList") (pure () :: XML)

-- | GetActivelookUpValues(IDPROOF, ONLINE_BOOKING) -> the ID proof types valid for online
-- booking. Tiny, global and changes ~never, so it is cached in-process for a day: with several
-- pods that is one vendor call per pod per day, and Redis would add a hop to every /seats for a
-- few hundred bytes.
data GetLookupValuesReq = GetLookupValuesReq
  { rqlvType :: Text,
    rqlvContext :: Text
  }

instance ToXML GetLookupValuesReq where
  toXML req =
    element (op "GetActivelookUpValues") $ do
      el "arg0" req.rqlvType
      el "arg1" req.rqlvContext

getIdProofTypes :: (TnstcFlow m r, CacheFlow m r) => TNSTCConfig -> Text -> m [TnstcLookupValue]
getIdProofTypes config cacheScope =
  IM.withInMemCache ["tnstcIdProofTypes", cacheScope] 86400 $
    callTnstc config "GetActivelookUpValues" (GetLookupValuesReq "IDPROOF" "ONLINE_BOOKING") parseLookupValues

getAddressPlaceList :: TnstcFlow m r => TNSTCConfig -> m [TnstcPlace]
getAddressPlaceList config = callTnstc config "GetAddressPlaceList" GetAddressPlaceListReq parsePlaces

getServiceSeatDetails :: TnstcFlow m r => TNSTCConfig -> GetServiceSeatDetailsReq -> m TnstcSeatSets
getServiceSeatDetails config req = callTnstc config "GetServiceSeatDetails" req parseSeatSets

getConcessionTypes :: TnstcFlow m r => TNSTCConfig -> GetConcessionTypesReq -> m [TnstcConcessionType]
getConcessionTypes config req = callTnstc config "GetAllConcessionTypesByServiceID" req parseConcessionTypes
