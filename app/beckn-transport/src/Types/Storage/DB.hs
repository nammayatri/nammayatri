{-# LANGUAGE DeriveAnyClass #-}

module Types.Storage.DB where

import           EulerHS.Prelude                       hiding (id)

import qualified Types.Storage.Organization            as Organization

import qualified Database.Beam                         as B

data TransporterDb f =
  TransporterDb
    { 
    _organization :: f (B.TableEntity Organization.OrganizationT)
    }
  deriving (Generic, B.Database be)

transporterDb :: B.DatabaseSettings be TransporterDb
transporterDb =
  B.defaultDbSettings `B.withDbModification`
  B.dbModification
    { _organization = Organization.fieldEMod
    }
