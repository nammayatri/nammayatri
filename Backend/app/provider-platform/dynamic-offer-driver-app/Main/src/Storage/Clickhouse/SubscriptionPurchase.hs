{-# OPTIONS_GHC -Wno-orphans #-}
module Storage.Clickhouse.SubscriptionPurchase where

import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import qualified Data.Set as Set

data SubscriptionPurchaseT f = SubscriptionPurchaseT
  { ownerId :: C f Text,
    ownerType :: C f DSP.SubscriptionOwnerType,
    status :: C f DSP.SubscriptionPurchaseStatus
  }
  deriving (Generic)

deriving instance Show SubscriptionPurchase

subscriptionPurchaseTTable :: SubscriptionPurchaseT (FieldModification SubscriptionPurchaseT)
subscriptionPurchaseTTable =
  SubscriptionPurchaseT
    { ownerId = "owner_id",
      ownerType = "owner_type",
      status = "status"
    }

type SubscriptionPurchase = SubscriptionPurchaseT Identity

instance CH.ClickhouseValue DSP.SubscriptionOwnerType
instance CH.ClickhouseValue DSP.SubscriptionPurchaseStatus

$(TH.mkClickhouseInstances ''SubscriptionPurchaseT 'SELECT_FINAL_MODIFIER)

findActiveDistinctOwnersByOwnerIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  DSP.SubscriptionOwnerType ->
  m Int
findActiveDistinctOwnersByOwnerIds ownerIds ownerType = do
  if null ownerIds
    then pure 0
    else do
      res <-
        CH.findAll $
          CH.select_ (\sub -> CH.notGrouped sub.ownerId) $
            CH.filter_
              ( \sub ->
                  sub.ownerId `CH.in_` ownerIds
                    CH.&&. sub.ownerType CH.==. ownerType
                    CH.&&. sub.status CH.==. DSP.ACTIVE
              )
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE subscriptionPurchaseTTable)
      pure $ Set.size (Set.fromList res)
