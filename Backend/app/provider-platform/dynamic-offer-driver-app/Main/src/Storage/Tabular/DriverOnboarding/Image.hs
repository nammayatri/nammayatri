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

module Storage.Tabular.DriverOnboarding.Image where

import qualified Domain.Types.DriverOnboarding.Error as Domain
import qualified Domain.Types.DriverOnboarding.Image as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.ImageType"
derivePersistField "Domain.DriverOnboardingError"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    ImageT sql=image
      id Text
      personId PersonTId
      merchantId MerchantTId
      s3Path Text
      imageType Domain.ImageType
      isValid Bool
      failureReason Domain.DriverOnboardingError Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey ImageT where
  type DomainKey ImageT = Id Domain.Image
  fromKey (ImageTKey _id) = Id _id
  toKey (Id id) = ImageTKey id

instance FromTType ImageT Domain.Image where
  fromTType ImageT {..} = do
    return $
      Domain.Image
        { id = Id id,
          merchantId = fromKey merchantId,
          personId = fromKey personId,
          ..
        }

instance ToTType ImageT Domain.Image where
  toTType Domain.Image {..} =
    ImageT
      { id = getId id,
        merchantId = toKey merchantId,
        personId = toKey personId,
        ..
      }
