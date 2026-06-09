{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Rewards where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardOffer as DReward
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DCoins
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.CachedQueries.RewardOffer as QRewardOffer
import qualified Storage.Queries.Person as QPerson
import Tools.Error

data RewardOfferResp = RewardOfferResp
  { id :: Text,
    title :: Text,
    description :: Maybe Text,
    imageUrl :: Maybe Text,
    displayOrder :: Int,
    milestoneTarget :: Maybe Int,
    triggerEvent :: Text,
    requiredTags :: [Text],
    logicDomain :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data RewardOfferProgressResp = RewardOfferProgressResp
  { offerId :: Text,
    milestoneTarget :: Maybe Int,
    currentProgress :: Int,
    isCompleted :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getRewardOffers ::
  (EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m [RewardOfferResp]
getRewardOffers (driverId, _, merchantOpCityId) = do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let driverTagTexts = [raw | LYT.TagNameValueExpiry raw <- fromMaybe [] person.driverTag]
  offers <- QRewardOffer.findAllActiveByMerchantOpCityId merchantOpCityId
  let visibleOffers =
        filter
          ( \o ->
              o.entityType == DReward.DRIVER
                && (null o.requiredTags || any (`elem` driverTagTexts) o.requiredTags)
          )
          offers
  pure $ map mkRewardOfferResp visibleOffers

getRewardOfferProgress ::
  (EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Id DReward.RewardOffer ->
  m RewardOfferProgressResp
getRewardOfferProgress (driverId, _, _) offerId = do
  offer <- QRewardOffer.findById offerId >>= fromMaybeM (InvalidRequest "Reward offer not found")
  currentProgress <-
    case offer.triggerEvent of
      DReward.EndRide -> DCoins.getValidRideCountByDriverIdKey driverId >>= pure . fromMaybe 0
      _ -> pure 0
  let isCompleted = maybe False (currentProgress >=) offer.milestoneTarget
  pure
    RewardOfferProgressResp
      { offerId = offer.id.getId,
        milestoneTarget = offer.milestoneTarget,
        currentProgress,
        isCompleted
      }

mkRewardOfferResp :: DReward.RewardOffer -> RewardOfferResp
mkRewardOfferResp DReward.RewardOffer {..} =
  RewardOfferResp
    { id = id.getId,
      title,
      description,
      imageUrl,
      displayOrder,
      milestoneTarget,
      triggerEvent = show triggerEvent,
      requiredTags,
      logicDomain
    }
