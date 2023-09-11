{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Track
  ( TrackBuildReq (..),
    buildTrackReq,
  )
where

import qualified Beckn.Types.Core.Taxi.Track as Track
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantConfigNew as DMC
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Id
import Kernel.Utils.Common

data TrackBuildReq = TrackBuildReq
  { bppBookingId :: Id DBooking.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    bppId :: Text,
    bppUrl :: BaseUrl,
    transactionId :: Text,
    merchant :: DM.Merchant,
    merchantConfig :: DMC.MerchantConfigNew
  }

buildTrackReq ::
  ( MonadFlow m,
    HedisFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  TrackBuildReq ->
  m (BecknReq Track.TrackMessage)
buildTrackReq res = do
  messageId <- generateGUID
  Redis.setExp (key messageId) res.bppRideId 1800 --30 mins
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  context <- buildTaxiContext Context.TRACK messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.merchantConfig.city res.merchantConfig.country False
  pure $ BecknReq context $ mkTrackMessage res
  where
    key messageId = "Track:bppRideId:" <> messageId

mkTrackMessage :: TrackBuildReq -> Track.TrackMessage
mkTrackMessage res = Track.TrackMessage res.bppBookingId.getId
