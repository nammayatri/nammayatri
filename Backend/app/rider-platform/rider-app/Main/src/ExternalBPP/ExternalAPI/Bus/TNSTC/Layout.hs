{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Layout
  ( GetConcessionTypesReq (..),
    GetServiceSeatDetailsReq (..),
    getConcessionTypes,
    getAddressPlaceList,
    getServiceSeatDetails,
  )
where

import qualified Data.Text as T
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Extra.IntegratedBPPConfig (TNSTCConfig)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Client (callTnstc)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Types
import Kernel.Prelude
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

data GetConcessionTypesReq = GetConcessionTypesReq
  { rqctClassId :: Text,
    rqctCounterCode :: Text,
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
        el "counterCode" req.rqctCounterCode
        el "endPlaceID" req.rqctEndPlaceId
        el "franchiseeUser" "false"
        el "journeyDate" (fmtDate req.rqctJourneyDate)
        forM_ req.rqctSeatNumbers (el "seatNumber")
        el "serviceID" req.rqctServiceId
        el "startPlaceID" req.rqctStartPlaceId
        el "totalNumberOfSeats" (show req.rqctTotalSeats)
        el "userName" req.rqctUserName

data GetServiceSeatDetailsReq = GetServiceSeatDetailsReq
  { rqssCounterCode :: Text,
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
        el "counterCode" req.rqssCounterCode
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

getAddressPlaceList :: TnstcFlow m r => TNSTCConfig -> m [TnstcPlace]
getAddressPlaceList config = callTnstc config "GetAddressPlaceList" GetAddressPlaceListReq parsePlaces

getServiceSeatDetails :: TnstcFlow m r => TNSTCConfig -> GetServiceSeatDetailsReq -> m TnstcSeatSets
getServiceSeatDetails config req = callTnstc config "GetServiceSeatDetails" req parseSeatSets

getConcessionTypes :: TnstcFlow m r => TNSTCConfig -> GetConcessionTypesReq -> m [TnstcConcessionType]
getConcessionTypes config req = callTnstc config "GetAllConcessionTypesByServiceID" req parseConcessionTypes
