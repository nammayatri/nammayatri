{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.ExotelEndRide
  ( API,
    handler,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.UI.ExotelEndRide as DExotelEndRide
import qualified Domain.Action.UI.Ride.EndRide as EndRide
import Domain.Types.Exophone (ExophoneType (..))
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import Kernel.Types.Id (getId)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.Queries.PersonExtra as QPerson
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error

type API = CallBasedEndRideAPI

handler :: FlowServer API
handler = callBasedEndRidelHandler

-------- Direct call (Exotel) APIs
type CallBasedEndRideAPI =
  "exotel"
    :> "ride"
    :> ( "end"
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> Get '[JSON] DExotelEndRide.AckResp
       )

callBasedEndRidelHandler :: FlowServer CallBasedEndRideAPI
callBasedEndRidelHandler = callBasedEndRide

callBasedEndRide :: Text -> Text -> FlowHandler DExotelEndRide.AckResp
callBasedEndRide callFrom_ callTo_ = withFlowHandlerAPI . ActorInfo.withRequestIdActorInfo $ do
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  mobileNumberHash <- getDbHash callFrom
  endRideCandidates <- filter (\e -> e.exophoneType == END_RIDE) <$> CQExophone.findAllByPhone callTo
  tentativeExophone <- case endRideCandidates of
    (x : _) -> pure x
    [] -> Kernel.Utils.Common.throwError (ExophoneDoesNotExist callTo)
  driver <- runInReplica (QPerson.findByMobileNumberAndMerchantAndRole "+91" mobileNumberHash tentativeExophone.merchantId DP.DRIVER) >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  activeRide <- runInReplica (QRideLite.getCurrentActiveByDriverIdLite driver.id) >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  let exophone = fromMaybe tentativeExophone (find (\e -> Just e.merchantOperatingCityId == activeRide.merchantOperatingCityId) endRideCandidates)
  shandle <- EndRide.buildEndRideHandle exophone.merchantId exophone.merchantOperatingCityId Nothing False
  DExotelEndRide.callBasedEndRide shandle exophone.merchantId mobileNumberHash callFrom
  where
    dropFirstZero = T.dropWhile (== '0')
