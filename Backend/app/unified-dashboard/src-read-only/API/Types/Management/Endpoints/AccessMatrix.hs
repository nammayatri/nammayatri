{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Management.Endpoints.AccessMatrix where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Merchant
import qualified Domain.Types.Role
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AccessMatrixAPIEntity = AccessMatrixAPIEntity {accessMatrix :: [AccessMatrixRowAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AccessMatrixItemAPIEntity = AccessMatrixItemAPIEntity
  { additionalUserActions :: Kernel.Prelude.Maybe Data.Text.Text,
    serverName :: Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName,
    userActionType :: Domain.Types.AccessMatrix.UserActionType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AccessMatrixRowAPIEntity = AccessMatrixRowAPIEntity {accessMatrixRow :: [AccessMatrixItemAPIEntity], role :: Domain.Types.Role.RoleAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantCityList = MerchantCityList {cityList :: [Kernel.Types.Beckn.Context.City], merchantId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = (GetAccessMatrix :<|> GetMerchantWithCityList)

type GetAccessMatrix =
  ( "accessMatrix" :> QueryParam "limit" Kernel.Prelude.Integer :> QueryParam "offset" Kernel.Prelude.Integer
      :> QueryParam
           "roleId"
           (Kernel.Types.Id.Id Domain.Types.Role.Role)
      :> Get '[JSON] AccessMatrixAPIEntity
  )

type GetMerchantWithCityList = ("accessMatrix" :> "merchantWithCityList" :> Get '[JSON] [MerchantCityList])

data AccessMatrixAPIs = AccessMatrixAPIs
  { getAccessMatrix :: Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Role.Role) -> EulerHS.Types.EulerClient AccessMatrixAPIEntity,
    getMerchantWithCityList :: EulerHS.Types.EulerClient [MerchantCityList]
  }

mkAccessMatrixAPIs :: (Client EulerHS.Types.EulerClient API -> AccessMatrixAPIs)
mkAccessMatrixAPIs accessMatrixClient = (AccessMatrixAPIs {..})
  where
    getAccessMatrix :<|> getMerchantWithCityList = accessMatrixClient

data AccessMatrixUserActionType
  = GET_ACCESS_MATRIX
  | GET_MERCHANT_WITH_CITY_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''AccessMatrixUserActionType])
