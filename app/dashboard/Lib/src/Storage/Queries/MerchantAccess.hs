{-# LANGUAGE TypeApplications #-}

module Storage.Queries.MerchantAccess where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DAccess
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Merchant
import Storage.Tabular.MerchantAccess

create :: DAccess.MerchantAccess -> SqlDB ()
create = Esq.create

findByPersonIdAndMerchantId ::
  (Transactionable m) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  m (Maybe DAccess.MerchantAccess)
findByPersonIdAndMerchantId personId merchantId = findOne $ do
  merchantAccess <- from $ table @MerchantAccessT
  where_ $
    merchantAccess ^. MerchantAccessPersonId ==. val (toKey personId)
      &&. merchantAccess ^. MerchantAccessMerchantId ==. val (toKey merchantId)
  return merchantAccess

findAllByPersonId ::
  (Transactionable m) =>
  Id DP.Person ->
  m [DMerchant.Merchant]
findAllByPersonId personId = findAll $ do
  (merchantAccess :& merchant) <-
    from $
      table @MerchantAccessT
        `innerJoin` table @MerchantT
          `Esq.on` ( \(merchantAccess :& merchant) ->
                       merchantAccess ^. MerchantAccessMerchantId ==. merchant ^. MerchantTId
                   )
  where_ $
    merchantAccess ^. MerchantAccessPersonId ==. val (toKey personId)
  return merchant

deleteById :: Id DAccess.MerchantAccess -> SqlDB ()
deleteById = Esq.deleteByKey @MerchantAccessT
