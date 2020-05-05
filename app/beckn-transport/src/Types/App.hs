{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types.App where

import           Beckn.Utils.TH
import           Data.Swagger
import           Database.Beam.Backend.SQL     (FromBackendRow,
                                                HasSqlValueSyntax)
import           Database.Beam.MySQL           (MySQL, MysqlValueSyntax)
import           Database.Beam.MySQL.FromField
import qualified EulerHS.Interpreters          as I
import qualified EulerHS.Language              as L
import           EulerHS.Prelude
import qualified EulerHS.Runtime               as R
import           Servant
import           Servant.Swagger
-- App Types
data Env =
  Env
    { runTime :: R.FlowRuntime
    }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)

type FlowServer api = ServerT api (ReaderT Env (ExceptT ServerError IO))

newtype CustomerId =
  CustomerId
    { _getCustomerId :: Text
    }
  deriving  (Generic, Show)

deriveIdentifierInstances ''CustomerId

newtype OrganizationId =
  OrganizationId
    { _getOrganizationId :: Text
    }
  deriving  (Generic, Show)

deriveIdentifierInstances ''OrganizationId

type Limit = Int

type Offset = Int

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a