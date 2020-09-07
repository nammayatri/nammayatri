module Beckn.Utils.Mock
  ( noSearchResultId,
    serviceUnavailableId,
    fmdErrorFlowId,
    allFmdErrorFlowIds,
  )
where

import Beckn.Types.Core.FmdError
import qualified Data.Map as M
import EulerHS.Prelude

noSearchResultId :: Text
noSearchResultId = "err-0001-no-search-results"

serviceUnavailableId :: Text
serviceUnavailableId = "err-0002-service-unavailable"

fmdErrorFlowId :: FmdError -> Text
fmdErrorFlowId err = "search-error-fmd-" <> fmdErrorBareCodeText err

allFmdErrorFlowIds :: Map Text FmdError
allFmdErrorFlowIds =
  M.fromList $
    allFmdErrors <&> \err -> (fmdErrorFlowId err, err)
