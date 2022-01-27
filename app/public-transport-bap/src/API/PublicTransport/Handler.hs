module API.PublicTransport.Handler where

import API.PublicTransport.Search.Handler as Search
import qualified API.PublicTransport.Types as PublicTransport
import App.Types

handler :: FlowServer PublicTransport.API
handler =
  Search.handler