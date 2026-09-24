{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Quote where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Quote
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.SearchRequest
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("quote" :> GetQuoteResult)

type GetQuoteResult =
  ( Capture "searchId" (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest)
      :> Capture
           "customerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> "result"
      :> Get '[JSON] Domain.Action.UI.Quote.GetQuotesRes
  )

newtype QuoteAPIs = QuoteAPIs {getQuoteResult :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient Domain.Action.UI.Quote.GetQuotesRes}

mkQuoteAPIs :: (Client EulerHS.Types.EulerClient API -> QuoteAPIs)
mkQuoteAPIs quoteClient = (QuoteAPIs {..})
  where
    getQuoteResult = quoteClient

data QuoteUserActionType
  = GET_QUOTE_RESULT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON QuoteUserActionType where
  toJSON GET_QUOTE_RESULT = Data.Aeson.String "GET_QUOTE_RESULT"

instance FromJSON QuoteUserActionType where
  parseJSON (Data.Aeson.String "GET_QUOTE_RESULT") = pure GET_QUOTE_RESULT
  parseJSON _ = fail "GET_QUOTE_RESULT expected"

$(Data.Singletons.TH.genSingletons [''QuoteUserActionType])
