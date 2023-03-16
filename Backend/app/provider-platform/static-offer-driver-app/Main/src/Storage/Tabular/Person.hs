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

module Storage.Tabular.Person where

import qualified Domain.Types.Person as Domain
import Kernel.External.Encryption (DbHash, Encrypted (..), EncryptedHashed (..))
import Kernel.External.FCM.Types (FCMRecipientToken)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Centesimal
import Kernel.Types.Id
import Kernel.Utils.Version
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "Domain.Role"
derivePersistField "Domain.Gender"
derivePersistField "Domain.IdentifierType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PersonT sql=person
      id Text
      firstName Text
      middleName Text Maybe
      lastName Text Maybe
      role Domain.Role
      gender Domain.Gender
      identifierType Domain.IdentifierType
      email Text Maybe
      mobileNumberEncrypted Text Maybe
      mobileNumberHash DbHash Maybe
      mobileCountryCode Text Maybe
      passwordHash DbHash Maybe
      identifier Text Maybe
      rating Centesimal Maybe
      isNew Bool
      merchantId MerchantTId
      deviceToken FCMRecipientToken Maybe
      description Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      bundleVersion Text Maybe
      clientVersion Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey PersonT where
  type DomainKey PersonT = Id Domain.Person
  fromKey (PersonTKey _id) = Id _id
  toKey (Id id) = PersonTKey id

instance FromTType PersonT Domain.Person where
  fromTType PersonT {..} = do
    bundleVersion' <- forM bundleVersion readVersion
    clientVersion' <- forM clientVersion readVersion
    return $
      Domain.Person
        { id = Id id,
          mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
          merchantId = fromKey merchantId,
          bundleVersion = bundleVersion',
          clientVersion = clientVersion',
          ..
        }

instance ToTType PersonT Domain.Person where
  toTType Domain.Person {..} =
    PersonT
      { id = getId id,
        mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        mobileNumberHash = mobileNumber <&> (.hash),
        merchantId = toKey merchantId,
        bundleVersion = versionToText <$> bundleVersion,
        clientVersion = versionToText <$> clientVersion,
        ..
      }
