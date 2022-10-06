{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.IdfyVerification where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverOnboarding.IdfyVerification as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Storage.Tabular.DriverOnboarding.Image as ImageT
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    IdfyVerificationT sql=idfy_verification
      id Text
      driverId PersonTId
      documentImageId1 ImageT.ImageTId
      documentImageId2 ImageT.ImageTId Maybe
      requestId Text
      docType Image.ImageType
      status Text
      idfyResponse Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      Unique IdfyVerificationRequestId
      deriving Generic
    |]

instance TEntityKey IdfyVerificationT where
  type DomainKey IdfyVerificationT = Id Domain.IdfyVerification
  fromKey (IdfyVerificationTKey _id) = Id _id
  toKey (Id id) = IdfyVerificationTKey id

instance TType IdfyVerificationT Domain.IdfyVerification where
  fromTType IdfyVerificationT {..} = do
    return $
      Domain.IdfyVerification
        { id = Id id,
          driverId = fromKey driverId,
          documentImageId1 = fromKey documentImageId1,
          documentImageId2 = fromKey <$> documentImageId2,
          ..
        }

  toTType Domain.IdfyVerification {..} =
    IdfyVerificationT
      { id = getId id,
        driverId = toKey driverId,
        documentImageId1 = toKey documentImageId1,
        documentImageId2 = toKey <$> documentImageId2,
        ..
      }
