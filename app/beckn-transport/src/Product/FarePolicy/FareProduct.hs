{-# LANGUAGE DerivingVia #-}

module Product.FarePolicy.FareProduct where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Domain.Types.FarePolicy.FareProduct
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy.FareProduct as SFareProduct
import Types.API.FarePolicy.FareProduct
import Types.Error
import Utils.Common

listFareProducts :: SP.Person -> FlowHandler ListFareProductsRes
listFareProducts person = withFlowHandlerAPI $ do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  fareProducts <- SFareProduct.findEnabledByOrgId orgId
  pure $ ListFareProductsRes $ makeFareProductAPIEntity <$> fareProducts

updateFareProduct :: SP.Person -> UpdateFareProductReq -> FlowHandler APISuccess
updateFareProduct person updReq = withFlowHandlerAPI $ do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  Esq.runTransaction $
    if updReq.enabled
      then SFareProduct.upsertFareProduct orgId updReq.fareProductType
      else SFareProduct.deleteFareProduct orgId updReq.fareProductType
  pure Success
