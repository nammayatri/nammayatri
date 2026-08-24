{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Services
  ( GetAvailableServiceDetailsReq (..),
    getAvailableServiceDetails,
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
import Text.XML.Writer (ToXML (..), element, elementA)

data GetAvailableServiceDetailsReq = GetAvailableServiceDetailsReq
  { rqStartPlaceId :: Text,
    rqEndPlaceId :: Text,
    rqJourneyDate :: Day,
    rqCounterCode :: Text,
    rqTotalSeats :: Int,
    rqUserName :: Text,
    rqUserId :: Text
  }

formatJourneyDate :: Day -> Text
formatJourneyDate = T.pack . formatTime defaultTimeLocale "%d/%m/%Y"

instance ToXML GetAvailableServiceDetailsReq where
  toXML req =
    element (nm "GetAvailableServiceDetails") $
      element (XML.Name "arg0" Nothing Nothing) $ do
        el "counterCode" req.rqCounterCode
        el "endPlaceID" req.rqEndPlaceId
        el "journeyDate" (formatJourneyDate req.rqJourneyDate)
        el "journeyFromTime" "00:00"
        el "journeyToTime" "23:59"
        el "serviceClass" "0"
        el "startPlaceID" req.rqStartPlaceId
        el "totFemales" "0"
        el "totMales" (show req.rqTotalSeats)
        el "userID" req.rqUserId
        el "userName" req.rqUserName
    where
      nm n = XML.Name n (Just setcNamespace) (Just "com")
      el n v = elementA (XML.Name n Nothing Nothing) ([] :: [(XML.Name, Text)]) (v :: Text)

getAvailableServiceDetails ::
  ( MonadFlow m,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasField "requestId" r (Maybe Text)
  ) =>
  TNSTCConfig ->
  GetAvailableServiceDetailsReq ->
  m [TnstcServiceVO]
getAvailableServiceDetails config req =
  callTnstc config "GetAvailableServiceDetails" req parseAvailableServices
