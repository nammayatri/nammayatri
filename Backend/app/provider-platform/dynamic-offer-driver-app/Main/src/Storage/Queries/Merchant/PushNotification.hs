{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.PushNotification
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.Merchant.PushNotification
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.PushNotification as BeamMPN

findByMerchantIdPNKeyLangaugeUdf :: MonadFlow m => Id Merchant -> PushNotificationKey -> Language -> Maybe Text -> m (Maybe PushNotification)
findByMerchantIdPNKeyLangaugeUdf id pnKey language udf1 =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamMPN.merchantId $ Se.Eq (getId id),
          Se.Is BeamMPN.pushNotificationKey $ Se.Eq pnKey,
          Se.Is BeamMPN.language $ Se.Eq language,
          Se.Is BeamMPN.udf1 $ Se.Eq udf1
        ]
    ]

instance FromTType' BeamMPN.PushNotification PushNotification where
  fromTType' BeamMPN.PushNotificationT {..} = do
    pure $
      Just
        PushNotification
          { merchantId = Id merchantId,
            ..
          }

instance ToTType' BeamMPN.PushNotification PushNotification where
  toTType' PushNotification {..} = do
    BeamMPN.PushNotificationT
      { BeamMPN.merchantId = getId merchantId,
        BeamMPN.pushNotificationKey = pushNotificationKey,
        BeamMPN.language = language,
        BeamMPN.udf1 = udf1,
        BeamMPN.notificationSubType = notificationSubType,
        BeamMPN.icon = icon,
        BeamMPN.body = body,
        BeamMPN.title = title
      }
