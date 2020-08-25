module Beckn.Utils.Mock
  ( noSearchResultId,
    fmdErrorFlowId,
    allFmdErrorFlowIds,
  )
where

import Beckn.Types.Core.FmdError
import qualified Data.Map as M
import EulerHS.Prelude

noSearchResultId :: Text
noSearchResultId = "err-0001-no-search-results"

fmdErrorFlowId :: FmdError -> Text
fmdErrorFlowId err = "search-error-fmd-" <> fmdErrorBareCodeText err

allFmdErrorFlowIds :: Map Text FmdError
allFmdErrorFlowIds =
  M.fromList $
    allFmdErrors <&> \err -> (fmdErrorFlowId err, err)
