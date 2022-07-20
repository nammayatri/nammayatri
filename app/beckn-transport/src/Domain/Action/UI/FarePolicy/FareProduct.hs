{-# LANGUAGE DerivingVia #-}

module Domain.Action.UI.FarePolicy.FareProduct
  ( ListFareProductsRes (..),
    UpdateFareProductReq (..),
    listFareProducts,
    updateFareProduct,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Domain.Types.FarePolicy.FareProduct
import qualified Domain.Types.Person as SP
import qualified Storage.Queries.FarePolicy.FareProduct as SFareProduct
import Types.Error
import Utils.Common

newtype ListFareProductsRes = ListFareProductsRes
  { list :: [FareProductAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema, ToJSON, FromJSON)

data UpdateFareProductReq = UpdateFareProductReq
  { enabled :: Bool,
    fareProductType :: FareProductType
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

listFareProducts :: (EsqDBFlow m r) => SP.Person -> m ListFareProductsRes
listFareProducts person = do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  fareProducts <- SFareProduct.findEnabledByOrgId orgId
  pure $ ListFareProductsRes $ makeFareProductAPIEntity <$> fareProducts

updateFareProduct :: (EsqDBFlow m r) => SP.Person -> UpdateFareProductReq -> m APISuccess
updateFareProduct person updReq = do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  Esq.runTransaction $
    if updReq.enabled
      then SFareProduct.upsertFareProduct orgId updReq.fareProductType
      else SFareProduct.deleteFareProduct orgId updReq.fareProductType
  pure Success
