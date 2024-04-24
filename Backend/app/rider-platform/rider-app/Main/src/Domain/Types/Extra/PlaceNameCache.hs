{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.PlaceNameCache where

import Database.Beam.Backend
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForList)

-- Extra code goes here --
data AddressResp = AddressResp
  { longName :: Text,
    shortName :: Text,
    types :: [Text]
  }
  deriving stock (Generic, Show, Read, Ord, Eq)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AddressResp where
  sqlValueSyntax = autoSqlValueSyntax

$(mkBeamInstancesForList ''AddressResp)
