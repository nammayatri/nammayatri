{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.NammaTag
  ( postNammaTagTagCreate,
    postNammaTagQueryCreate,
    postNammaTagAppDynamicLogicVerify,
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.ChakraQueries
import Servant
import Tools.Auth

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.CreateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagCreate _merchantShortId _opCity req = do error "Logic yet to be decided" req

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate _merchantShortId _opCity req = do error "Logic yet to be decided" req

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Lib.Yudhishthira.Types.AppDynamicLogicReq -> Environment.Flow Lib.Yudhishthira.Types.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify _merchantShortId _opCity req = do error "Logic yet to be decided" req
