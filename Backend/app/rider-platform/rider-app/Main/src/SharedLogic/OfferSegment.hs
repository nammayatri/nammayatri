{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.OfferSegment
  ( OfferSegmentInput (..),
    DimensionSummary (..),
    getPersonOfferSegment,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonUsageStats as DPUS
import Kernel.External.Encryption (EncFlow, decrypt)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.TypesTH as YTH
import qualified SharedLogic.Utils as SLUtils
import qualified Storage.Queries.PersonUsageStats as QPersonUsageStats
import qualified Tools.DynamicLogic as TDL

data DimensionSummary = DimensionSummary
  { vehicleType :: Maybe Text,
    vehicleServiceTierType :: Maybe Text,
    productType :: Text,
    passTypeId :: Maybe Text,
    ticketCount :: Int,
    purchaseCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data OfferSegmentInput = OfferSegmentInput
  { ticketCount :: Int,
    purchaseCount :: Int,
    ticketPurchaseCount :: Int,
    passPurchaseCount :: Int,
    hasEverPurchased :: Bool,
    daysSinceLastPurchase :: Maybe Int,
    dimensions :: [DimensionSummary]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

$(YTH.generateGenericDefault ''OfferSegmentInput)

newtype OfferSegmentResp = OfferSegmentResp {segment :: Maybe Text}
  deriving (Generic, Show, FromJSON)

getPersonOfferSegment ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Person.Person ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe Text)
getPersonOfferSegment person merchantOperatingCityId = do
  now <- getCurrentTime
  (logics, _) <- TDL.getAppDynamicLogic (cast merchantOperatingCityId) LYT.FRFS_OFFER_SEGMENT_POLICY now Nothing Nothing
  if null logics
    then do
      logInfo "OfferSegment: no FRFS_OFFER_SEGMENT_POLICY configured"
      pure Nothing
    else do
      input <- buildInput person now
      result <- LYTU.runLogics logics input
      unless (null result.errors) $
        logError $ "OfferSegment: rule errors: " <> show result.errors
      case A.fromJSON result.result of
        A.Success (resp :: OfferSegmentResp) -> pure resp.segment
        A.Error err -> do
          logError $ "OfferSegment: could not parse logic result: " <> show err
          pure Nothing

buildInput ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Person.Person ->
  UTCTime ->
  m OfferSegmentInput
buildInput person now = do
  mbStaticId <- forM person.mobileNumber $ \encPhone ->
    SLUtils.getPureStaticCustomerId person <$> decrypt encPhone
  rows <- case mbStaticId of
    Just _ -> QPersonUsageStats.findAllByStaticPersonId mbStaticId
    Nothing -> QPersonUsageStats.findAllByPersonId person.id
  let staleRows = filter (\row -> row.personId /= person.id) rows
  unless (null staleRows) $
    fork "repoint person usage stats" $
      forM_ staleRows $ \row -> QPersonUsageStats.updatePersonIdById person.id row.id
  pure $ summarize now rows

summarize :: UTCTime -> [DPUS.PersonUsageStats] -> OfferSegmentInput
summarize now rows =
  OfferSegmentInput
    { ticketCount = sum (map (fromMaybe 0 . (.ticketCount)) rows),
      purchaseCount = totalPurchases,
      ticketPurchaseCount = purchasesOf DPUS.TICKET,
      passPurchaseCount = purchasesOf DPUS.PASS,
      hasEverPurchased = totalPurchases > 0,
      daysSinceLastPurchase = daysSince <$> mbLastPurchasedAt,
      dimensions = map toDimension rows
    }
  where
    totalPurchases = sum (map (.purchaseCount) rows)
    purchasesOf p = sum (map (.purchaseCount) (filter (\row -> row.productType == p) rows))
    mbLastPurchasedAt = case map (.lastPurchasedAt) rows of
      [] -> Nothing
      ts -> Just (maximum ts)
    daysSince t = floor (diffUTCTime now t / 86400) :: Int
    toDimension :: DPUS.PersonUsageStats -> DimensionSummary
    toDimension row =
      DimensionSummary
        { vehicleType = show <$> row.vehicleType,
          vehicleServiceTierType = show <$> row.vehicleServiceTierType,
          productType = show row.productType,
          passTypeId = (.getId) <$> row.passTypeId,
          ticketCount = fromMaybe 0 row.ticketCount,
          purchaseCount = row.purchaseCount
        }
