{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.PlanDetails where

import Control.Monad
import qualified Domain.Types.PlanDetails as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common (decodeFromText, encodeToText)
import Kernel.Utils.Error
import Storage.Tabular.Merchant (MerchantTId)
import Tools.Error

derivePersistField "Domain.PaymentMode"
derivePersistField "Domain.Frequency"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PlanDetailsT sql=plan_details
      id Text
      paymentMode Domain.PaymentMode
      frequency Domain.Frequency
      merchantId MerchantTId
      planCriteriaConfigJSON Text sql=plan_criteria_config_json
      city Text
      Primary id
      deriving Generic
    |]

instance TEntityKey PlanDetailsT where
  type DomainKey PlanDetailsT = Id Domain.PlanDetails
  fromKey (PlanDetailsTKey _id) = Id _id
  toKey (Id id) = PlanDetailsTKey id

instance FromTType PlanDetailsT Domain.PlanDetails where
  fromTType PlanDetailsT {..} = do
    planCriteriaConfig <-
      maybe (throwError $ InternalError "Unable to decode PlanDetailsT.planCriteriaConfig") return $
        Domain.PlanCriteriaConfig <$> decodeFromText planCriteriaConfigJSON
    return $
      Domain.PlanDetails
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType PlanDetailsT Domain.PlanDetails where
  toTType Domain.PlanDetails {..} = do
    let planCriteriaConfigJSON = getConfigJSON planCriteriaConfig
    PlanDetailsT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }

getConfigJSON :: Domain.PlanCriteriaConfig -> Text
getConfigJSON = \case
  Domain.PlanCriteriaConfig cfg -> encodeToText cfg
