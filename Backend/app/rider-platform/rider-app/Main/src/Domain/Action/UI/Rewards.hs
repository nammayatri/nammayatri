module Domain.Action.UI.Rewards where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RewardOffer as DReward
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
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

getRewardOffers ::
  (EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant) ->
  m [RewardOfferResp]
getRewardOffers (riderId, _) = do
  person <- QPerson.findById riderId >>= fromMaybeM (PersonDoesNotExist riderId.getId)
  let merchantOpCityId = person.merchantOperatingCityId
  let riderTagTexts = [raw | LYT.TagNameValueExpiry raw <- fromMaybe [] person.customerNammaTags]
  offers <- QRewardOffer.findAllActiveByMerchantOpCityId merchantOpCityId
  let visibleOffers =
        filter
          ( \o ->
              o.entityType == DReward.RIDER
                && (null o.requiredTags || any (`elem` riderTagTexts) o.requiredTags)
          )
          offers
  pure $ map mkRewardOfferResp visibleOffers

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
