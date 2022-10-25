{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Organization where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Geofencing (GeoRestriction)
import qualified Beckn.Types.Geofencing as Geo
import Beckn.Types.Id
import qualified Domain.Types.Organization as Domain

derivePersistField "Domain.OrganizationType"
derivePersistField "Domain.OrganizationDomain"
derivePersistField "Domain.Status"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OrganizationT sql=organization
      id Text
      name Text
      description Text Maybe
      shortId Text
      uniqueKeyId Text
      mobileNumber Text Maybe
      mobileCountryCode Text Maybe
      gstin Text Maybe
      orgType Domain.OrganizationType sql=type
      domain Domain.OrganizationDomain Maybe
      fromTime UTCTime Maybe
      toTime UTCTime Maybe
      headCount Int Maybe
      status Domain.Status
      verified Bool
      enabled Bool
      createdAt UTCTime
      updatedAt UTCTime
      info Text Maybe
      originRestriction GeoRestriction
      destinationRestriction GeoRestriction
      Primary id
      Unique OrganizationShortId
      deriving Generic
    |]

instance TEntityKey OrganizationT where
  type DomainKey OrganizationT = Id Domain.Organization
  fromKey (OrganizationTKey _id) = Id _id
  toKey (Id id) = OrganizationTKey id

instance TType OrganizationT Domain.Organization where
  fromTType OrganizationT {..} = do
    return $
      Domain.Organization
        { id = Id id,
          shortId = ShortId shortId,
          _type = orgType,
          geofencingConfig =
            Geo.GeofencingConfig
              { origin = originRestriction,
                destination = destinationRestriction
              },
          ..
        }
  toTType Domain.Organization {..} =
    OrganizationT
      { id = getId id,
        shortId = getShortId shortId,
        orgType = _type,
        originRestriction = geofencingConfig.origin,
        destinationRestriction = geofencingConfig.destination,
        ..
      }
