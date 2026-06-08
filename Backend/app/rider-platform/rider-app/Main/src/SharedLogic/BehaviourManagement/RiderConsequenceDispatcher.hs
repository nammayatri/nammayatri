{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.RiderConsequenceDispatcher
  ( RiderDispatchContext (..),
    handleConsequences,
    handleCommunications,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.CommunicationEngine.Parser as CMParser
import qualified Lib.CommunicationEngine.Types as CMT
import qualified Lib.ConsequenceEngine.Parser as CEParser
import qualified Lib.ConsequenceEngine.Types as CET

data RiderDispatchContext = RiderDispatchContext
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    bookingId :: Text,
    rideId :: Maybe Text
  }

handleConsequences ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  RiderDispatchContext ->
  Id DP.Person ->
  [CET.ConsequenceDirective] ->
  m ()
handleConsequences ctx riderId directives = do
  let (actions, errors) = CEParser.parseDirectives directives
  unless (null errors) $
    logError $ "Rider consequence parse errors for " <> riderId.getId <> ": " <> show errors
  forM_ actions $ dispatchConsequence ctx riderId

dispatchConsequence ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  RiderDispatchContext ->
  Id DP.Person ->
  CET.ConsequenceAction ->
  m ()
dispatchConsequence ctx riderId = \case
  CET.NoAction -> pure ()
  CET.GrantCoupon params -> do
    logInfo $
      "GRANT_COUPON for rider "
        <> riderId.getId
        <> " booking="
        <> ctx.bookingId
        <> " offerId="
        <> show params.offerId
        <> " coupon="
        <> show params.couponCode
  CET.AwardCoins params ->
    logWarning $ "AWARD_COINS ignored for rider " <> riderId.getId <> ": " <> show params
  CET.AwardCash params ->
    logWarning $ "AWARD_CASH ignored for rider " <> riderId.getId <> ": " <> show params
  other ->
    logInfo $ "Rider consequence not implemented: " <> show other <> " for rider " <> riderId.getId

handleCommunications ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  [CMT.CommunicationDirective] ->
  m ()
handleCommunications riderId directives = do
  let (actions, errors) = CMParser.parseDirectives directives
  unless (null errors) $
    logError $ "Rider communication parse errors for " <> riderId.getId <> ": " <> show errors
  forM_ actions $ \action ->
    logInfo $ "Rider communication for " <> riderId.getId <> ": " <> show action
