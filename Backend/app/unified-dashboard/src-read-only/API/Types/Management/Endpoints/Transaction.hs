{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Management.Endpoints.Transaction where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Transaction
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data ListTransactionResp = ListTransactionResp {list :: [TransactionAPIEntity], summary :: Kernel.External.Verification.SafetyPortal.Types.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RequestorAPIEntity = RequestorAPIEntity
  { email :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    lastName :: Data.Text.Text,
    mobileCountryCode :: Data.Text.Text,
    mobileNumber :: Data.Text.Text,
    registeredAt :: Kernel.Prelude.UTCTime,
    verified :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransactionAPIEntity = TransactionAPIEntity
  { commonDriverId :: Kernel.Prelude.Maybe Data.Text.Text,
    commonRideId :: Kernel.Prelude.Maybe Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    endpoint :: Domain.Types.AccessMatrix.UserActionType,
    id :: Kernel.Types.Id.Id Domain.Types.Transaction.Transaction,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    request :: Kernel.Prelude.Maybe Data.Text.Text,
    requestor :: RequestorAPIEntity,
    response :: Kernel.Prelude.Maybe Data.Text.Text,
    responseError :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("/transaction" :> GetTransactionList)

type GetTransactionList =
  ( "list" :> QueryParam "searchString" Data.Text.Text :> QueryParam "limit" Kernel.Prelude.Integer :> QueryParam "offset" Kernel.Prelude.Integer
      :> QueryParam
           "requestorId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam "driverId" Data.Text.Text
      :> QueryParam
           "rideId"
           Data.Text.Text
      :> QueryParam
           "endpoint"
           Domain.Types.AccessMatrix.UserActionType
      :> Get
           ('[JSON])
           ListTransactionResp
  )

newtype TransactionAPIs = TransactionAPIs {getTransactionList :: (Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Domain.Types.AccessMatrix.UserActionType) -> EulerHS.Types.EulerClient ListTransactionResp)}

mkTransactionAPIs :: (Client EulerHS.Types.EulerClient API -> TransactionAPIs)
mkTransactionAPIs transactionClient = (TransactionAPIs {..})
  where
    getTransactionList = transactionClient

data TransactionUserActionType
  = GET_TRANSACTION_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON TransactionUserActionType where
  toJSON (GET_TRANSACTION_LIST) = Data.Aeson.String "GET_TRANSACTION_LIST"

instance FromJSON TransactionUserActionType where
  parseJSON (Data.Aeson.String "GET_TRANSACTION_LIST") = pure GET_TRANSACTION_LIST
  parseJSON _ = fail "GET_TRANSACTION_LIST expected"

$(Data.Singletons.TH.genSingletons [(''TransactionUserActionType)])
