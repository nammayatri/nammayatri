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
    CurrentPurchase (..),
    OfferSegmentContext (..),
    DimensionSummary (..),
    OfferSegmentResp (..),
    ticketContext,
    passContext,
    getPersonOfferSegment,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Aeson as A
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassType as PassType
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonPTStats as DPUS
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.TypesTH as YTH
import qualified SharedLogic.Utils as SLUtils
import qualified Storage.Queries.PersonPTStats as QPersonPTStats
import qualified Tools.DynamicLogic as TDL

-- | Counters for one (vehicleType, serviceTier, productType, passType) row.
data DimensionSummary = DimensionSummary
  { vehicleType :: Maybe Text,
    vehicleServiceTierType :: Maybe Text,
    productType :: Text,
    passTypeId :: Maybe Text,
    ticketCount :: Int,
    purchaseCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OfferSegmentContext = OfferSegmentContext
  { productType :: Maybe DPUS.FRFSProductType,
    vehicleType :: Maybe Spec.VehicleCategory,
    vehicleServiceTierType :: Maybe Spec.ServiceTierType,
    passTypeId :: Maybe (Id PassType.PassType)
  }
  deriving (Show, Generic)

ticketContext :: Maybe Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> OfferSegmentContext
ticketContext vehicleType vehicleServiceTierType =
  OfferSegmentContext
    { productType = Just DPUS.TICKET,
      vehicleType = vehicleType,
      vehicleServiceTierType = vehicleServiceTierType,
      passTypeId = Nothing
    }

passContext :: Id PassType.PassType -> OfferSegmentContext
passContext passTypeId =
  OfferSegmentContext
    { productType = Just DPUS.PASS,
      vehicleType = Nothing,
      vehicleServiceTierType = Nothing,
      passTypeId = Just passTypeId
    }

data CurrentPurchase = CurrentPurchase
  { productType :: Maybe Text,
    vehicleType :: Maybe Text,
    vehicleServiceTierType :: Maybe Text,
    passTypeId :: Maybe Text,
    ticketCount :: Int,
    purchaseCount :: Int,
    daysSinceLastPurchase :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data OfferSegmentInput = OfferSegmentInput
  { ticketCount :: Int,
    purchaseCount :: Int,
    ticketPurchaseCount :: Int,
    passPurchaseCount :: Int,
    hasEverPurchased :: Bool,
    daysSinceLastPurchase :: Maybe Int,
    current :: CurrentPurchase,
    dimensions :: [DimensionSummary]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(YTH.generateGenericDefault ''OfferSegmentInput)

newtype OfferSegmentResp = OfferSegmentResp {segment :: Maybe Text}
  deriving (Generic, Show, FromJSON)

getPersonOfferSegment ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Person.Person ->
  Id DMOC.MerchantOperatingCity ->
  OfferSegmentContext ->
  m (Maybe Text)
getPersonOfferSegment person merchantOperatingCityId ctx = do
  segmentResult <- withTryCatch "getPersonOfferSegment" $ do
    now <- getCurrentTime
    (logics, _) <- TDL.getAppDynamicLogic (cast merchantOperatingCityId) LYT.FRFS_OFFER_SEGMENT_POLICY now Nothing Nothing
    if null logics
      then pure Nothing
      else do
        rows <- fetchUsageRows person
        let input = mkInput now rows ctx
        result <- LYTU.runLogics logics input
        unless (null result.errors) $
          logError $ "OfferSegment: rule errors: " <> show result.errors
        case A.fromJSON result.result of
          A.Success (resp :: OfferSegmentResp) -> pure resp.segment
          A.Error err -> do
            logError $ "OfferSegment: could not parse logic result: " <> show err
            pure Nothing
  case segmentResult of
    Right segment -> pure segment
    Left err -> do
      logError $ "OfferSegment: resolve failed for person " <> person.id.getId <> ", left unsegmented: " <> show err
      pure Nothing

fetchUsageRows ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Person.Person ->
  m [DPUS.PersonPTStats]
fetchUsageRows person = do
  mbStaticPersonId <-
    forM person.mobileNumber $ \encPhone ->
      SLUtils.getPureStaticCustomerId person <$> decrypt encPhone
  case mbStaticPersonId of
    Nothing -> QPersonPTStats.findAllByPersonId person.id
    Just staticPersonId ->
      QPersonPTStats.findAllByStaticPersonId staticPersonId >>= \case
        [] -> adoptRowsKeyedOnPersonId staticPersonId
        rows -> repointStalePersonIds rows
  where
    adoptRowsKeyedOnPersonId staticPersonId = do
      rows <- QPersonPTStats.findAllByPersonId person.id
      unless (null rows) $
        fork "adopt backfilled person pt stats" $
          forM_ rows $ \row -> QPersonPTStats.updateStaticPersonIdById staticPersonId row.id
      pure rows

    repointStalePersonIds rows = do
      let staleRows = filter (\row -> row.personId /= person.id) rows
      unless (null staleRows) $
        fork "repoint person pt stats" $
          forM_ staleRows $ \row -> QPersonPTStats.updatePersonIdById person.id row.id
      pure rows

mkInput :: UTCTime -> [DPUS.PersonPTStats] -> OfferSegmentContext -> OfferSegmentInput
mkInput now rows ctx =
  OfferSegmentInput
    { ticketCount = sum (map (fromMaybe 0 . (.ticketCount)) rows),
      purchaseCount = totalPurchases,
      ticketPurchaseCount = purchasesOf DPUS.TICKET,
      passPurchaseCount = purchasesOf DPUS.PASS,
      hasEverPurchased = totalPurchases > 0,
      daysSinceLastPurchase = daysSince <$> mbLastPurchasedAt,
      current = mkCurrent,
      dimensions = map toDimension rows
    }
  where
    mkCurrent =
      CurrentPurchase
        { productType = show <$> ctx.productType,
          vehicleType = show <$> ctx.vehicleType,
          vehicleServiceTierType = show <$> ctx.vehicleServiceTierType,
          passTypeId = (.getId) <$> ctx.passTypeId,
          ticketCount = maybe 0 (fromMaybe 0 . (.ticketCount)) mbCurrentRow,
          purchaseCount = maybe 0 (.purchaseCount) mbCurrentRow,
          daysSinceLastPurchase = case mbCurrentRow of
            Just row | row.purchaseCount > 0 -> Just (daysSince row.lastPurchasedAt)
            _ -> Nothing
        }
    mbCurrentRow = listToMaybe (filter matchesCurrent rows)
    matchesCurrent row =
      Just row.productType == ctx.productType
        && row.vehicleType == ctx.vehicleType
        && row.vehicleServiceTierType == ctx.vehicleServiceTierType
        && row.passTypeId == ctx.passTypeId
    totalPurchases = sum (map (.purchaseCount) rows)
    purchasesOf p = sum (map (.purchaseCount) (filter (\row -> row.productType == p) rows))
    mbLastPurchasedAt = case map (.lastPurchasedAt) (filter ((> 0) . (.purchaseCount)) rows) of
      [] -> Nothing
      ts -> Just (maximum ts)
    daysSince t = floor (diffUTCTime now t / 86400) :: Int
    toDimension :: DPUS.PersonPTStats -> DimensionSummary
    toDimension row =
      DimensionSummary
        { vehicleType = show <$> row.vehicleType,
          vehicleServiceTierType = show <$> row.vehicleServiceTierType,
          productType = show row.productType,
          passTypeId = (.getId) <$> row.passTypeId,
          ticketCount = fromMaybe 0 row.ticketCount,
          purchaseCount = row.purchaseCount
        }
