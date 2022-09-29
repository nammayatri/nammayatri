{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverOnboarding.Image where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.DriverOnboarding.Image as Domain
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.ImageType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    ImageT sql=image
      id Text
      personId PersonTId
      organizationId OrganizationTId
      s3Path Text
      imageType Domain.ImageType
      isValid Bool
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey ImageT where
  type DomainKey ImageT = Id Domain.Image
  fromKey (ImageTKey _id) = Id _id
  toKey (Id id) = ImageTKey id

instance TType ImageT Domain.Image where
  fromTType ImageT {..} = do
    return $
      Domain.Image
        { id = Id id,
          organizationId = fromKey organizationId,
          personId = fromKey personId,
          ..
        }

  toTType Domain.Image {..} =
    ImageT
      { id = getId id,
        organizationId = toKey organizationId,
        personId = toKey personId,
        ..
      }
