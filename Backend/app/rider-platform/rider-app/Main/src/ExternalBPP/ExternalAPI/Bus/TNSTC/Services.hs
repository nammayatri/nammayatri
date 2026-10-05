{-# LANGUAGE OverloadedStrings #-}

module ExternalBPP.ExternalAPI.Bus.TNSTC.Services
  ( GetAvailableServiceDetailsReq (..),
    getAvailableServiceDetails,
  )
where

import Data.Time (Day)
import Domain.Types.Extra.IntegratedBPPConfig (TNSTCConfig)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Client (TnstcFlow, arg0, callTnstc, el, fmtDate, op)
import ExternalBPP.ExternalAPI.Bus.TNSTC.Types
import Kernel.Prelude
import Text.XML.Writer (ToXML (..), element)

data GetAvailableServiceDetailsReq = GetAvailableServiceDetailsReq
  { rqStartPlaceId :: Text,
    rqEndPlaceId :: Text,
    rqJourneyDate :: Day,
    rqCounterCode :: Maybe Text,
    rqTotalSeats :: Int,
    rqUserName :: Text,
    rqUserId :: Text
  }

instance ToXML GetAvailableServiceDetailsReq where
  toXML req =
    element (op "GetAvailableServiceDetails") $
      element arg0 $ do
        whenJust req.rqCounterCode (el "counterCode")
        el "endPlaceID" req.rqEndPlaceId
        el "journeyDate" (fmtDate req.rqJourneyDate)
        el "journeyFromTime" "00:00"
        el "journeyToTime" "23:59"
        el "serviceClass" "0"
        el "startPlaceID" req.rqStartPlaceId
        el "totFemales" "0"
        el "totMales" (show req.rqTotalSeats)
        el "userID" req.rqUserId
        el "userName" req.rqUserName

getAvailableServiceDetails ::
  TnstcFlow m r =>
  TNSTCConfig ->
  GetAvailableServiceDetailsReq ->
  m [TnstcServiceVO]
getAvailableServiceDetails config req =
  callTnstc config "GetAvailableServiceDetails" req parseAvailableServices
