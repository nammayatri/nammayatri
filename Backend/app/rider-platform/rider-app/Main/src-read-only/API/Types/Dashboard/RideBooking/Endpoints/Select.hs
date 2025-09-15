{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Select where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Select
import qualified "this" Domain.Types.Estimate
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client
import qualified SharedLogic.Cancel

type API = ("select" :> (PostSelectEstimate :<|> GetSelectQuotes :<|> GetSelectResult :<|> PostSelectCancelSearch))

type PostSelectEstimate =
  ( "estimate" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> Capture
           "estimateId"
           (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate)
      :> "select"
      :> ReqBody ('[JSON]) Domain.Action.UI.Select.DSelectReq
      :> Post
           ('[JSON])
           Domain.Action.UI.Select.MultimodalSelectRes
  )

type GetSelectQuotes =
  ( Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Capture "estimateId" (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate) :> "quotes"
      :> Get
           ('[JSON])
           Domain.Action.UI.Select.SelectListRes
  )

type GetSelectResult =
  ( Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Capture "estimateId" (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate) :> "result"
      :> Get
           ('[JSON])
           Domain.Action.UI.Select.QuotesResultResponse
  )

type PostSelectCancelSearch =
  ( Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> Capture
           "estimateId"
           (Kernel.Types.Id.Id Domain.Types.Estimate.Estimate)
      :> "cancel"
      :> Post ('[JSON]) SharedLogic.Cancel.CancelAPIResponse
  )

data SelectAPIs = SelectAPIs
  { postSelectEstimate :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> Domain.Action.UI.Select.DSelectReq -> EulerHS.Types.EulerClient Domain.Action.UI.Select.MultimodalSelectRes),
    getSelectQuotes :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> EulerHS.Types.EulerClient Domain.Action.UI.Select.SelectListRes),
    getSelectResult :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> EulerHS.Types.EulerClient Domain.Action.UI.Select.QuotesResultResponse),
    postSelectCancelSearch :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Estimate.Estimate -> EulerHS.Types.EulerClient SharedLogic.Cancel.CancelAPIResponse)
  }

mkSelectAPIs :: (Client EulerHS.Types.EulerClient API -> SelectAPIs)
mkSelectAPIs selectClient = (SelectAPIs {..})
  where
    postSelectEstimate :<|> getSelectQuotes :<|> getSelectResult :<|> postSelectCancelSearch = selectClient

data SelectUserActionType
  = POST_SELECT_ESTIMATE
  | GET_SELECT_QUOTES
  | GET_SELECT_RESULT
  | POST_SELECT_CANCEL_SEARCH
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''SelectUserActionType)])
