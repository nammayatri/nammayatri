{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SubscriberMetadata where

import qualified Domain.Types.SubscriberMetadata as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SubscriberMetadataT sql=subscriber_metadata
      id Text
      name Text
      logoUrl Text
      Primary id
      deriving Generic
    |]

instance TEntityKey SubscriberMetadataT where
  type DomainKey SubscriberMetadataT = Id Domain.SubscriberMetadata
  fromKey (SubscriberMetadataTKey _id) = Id _id
  toKey (Id id) = SubscriberMetadataTKey id

instance FromTType SubscriberMetadataT Domain.SubscriberMetadata where
  fromTType SubscriberMetadataT {..} = do
    logoUrl_ <- parseBaseUrl logoUrl
    return $
      Domain.SubscriberMetadata
        { id = Id id,
          logoUrl = logoUrl_,
          ..
        }

instance ToTType SubscriberMetadataT Domain.SubscriberMetadata where
  toTType Domain.SubscriberMetadata {..} =
    SubscriberMetadataT
      { id = getId id,
        logoUrl = showBaseUrl logoUrl,
        ..
      }
