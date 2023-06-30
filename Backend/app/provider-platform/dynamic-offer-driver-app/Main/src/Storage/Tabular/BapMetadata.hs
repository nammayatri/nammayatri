{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.BapMetadata where

import qualified Domain.Types.BapMetadata as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BapMetadataT sql=bap_metadata
      id Text
      name Text
      logoUrl Text
      Primary id
      deriving Generic
    |]

instance TEntityKey BapMetadataT where
  type DomainKey BapMetadataT = Id Domain.BapMetadata
  fromKey (BapMetadataTKey _id) = Id _id
  toKey (Id id) = BapMetadataTKey id

instance FromTType BapMetadataT Domain.BapMetadata where
  fromTType BapMetadataT {..} = do
    logoUrl_ <- parseBaseUrl logoUrl
    return $
      Domain.BapMetadata
        { id = Id id,
          logoUrl = logoUrl_,
          ..
        }

instance ToTType BapMetadataT Domain.BapMetadata where
  toTType Domain.BapMetadata {..} =
    BapMetadataT
      { id = getId id,
        logoUrl = showBaseUrl logoUrl,
        ..
      }
