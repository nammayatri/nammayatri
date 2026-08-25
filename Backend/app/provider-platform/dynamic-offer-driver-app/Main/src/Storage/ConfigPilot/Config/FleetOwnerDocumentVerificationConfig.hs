{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..), projectDriverConfigs) where

import Data.List (nub, nubBy, sortOn)
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig as DT
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.DocumentVerificationConfig as SQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as SQ
import qualified Storage.Queries.DocumentVerificationConfig as QDVC

data FleetOwnerDocumentVerificationConfigDimensions = FleetOwnerDocumentVerificationConfigDimensions
  { merchantOperatingCityId :: Text,
    documentType :: Maybe DVC.DocumentType,
    role :: Maybe [DP.Role]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'FleetOwnerDocumentVerificationConfig where
  type DimensionsFor 'FleetOwnerDocumentVerificationConfig = FleetOwnerDocumentVerificationConfigDimensions
  configTypeValue = FleetOwnerDocumentVerificationConfig
  sConfigType = SFleetOwnerDocumentVerificationConfig

instance ConfigDimensions FleetOwnerDocumentVerificationConfigDimensions where
  type ConfigTypeOf FleetOwnerDocumentVerificationConfigDimensions = 'FleetOwnerDocumentVerificationConfig
  type ConfigValueTypeOf FleetOwnerDocumentVerificationConfigDimensions = [DT.FleetOwnerDocumentVerificationConfig]
  getConfigType _ = FleetOwnerDocumentVerificationConfig
  getConfigList a =
    LCP.resolveConfigList
      a
      (LYT.DRIVER_CONFIG FleetOwnerDocumentVerificationConfig)
      (Id a.merchantOperatingCityId)
      fetch
      [ LCP.DimMatcher (.documentType) (Just . (.documentType)) (==),
        LCP.DimMatcher (.role) (Just . (.role)) rolesOverlap
      ]
      Nothing
    where
      fetch = do
        let cityId = Id a.merchantOperatingCityId
        fleetCfgs <- SQ.findAllByMerchantOpCityId cityId (Just [])
        if null fleetCfgs
          then projectDriverConfigs <$> SQDVC.findAllByMerchantOpCityId cityId (Just [])
          else pure fleetCfgs
  configFallback a =
    Just $ do
      let cityId = Id a.merchantOperatingCityId
      fleetCfgs <- SQ.findByDimensions cityId Nothing
      fmap (filter matchesDims) $
        if null fleetCfgs
          then projectDriverConfigs <$> QDVC.findByDimensions cityId Nothing Nothing
          else pure fleetCfgs
    where
      matchesDims cfg =
        maybe True (== cfg.documentType) a.documentType
          && maybe True (`rolesOverlap` cfg.role) a.role

rolesOverlap :: [DP.Role] -> [DP.Role] -> Bool
rolesOverlap asked have = any (`elem` have) asked

projectDriverConfigs :: [DVC.DocumentVerificationConfig] -> [DT.FleetOwnerDocumentVerificationConfig]
projectDriverConfigs = map mkFleetConfig . collapse . filter ((== Just DVC.Fleet) . (.documentCategory))
  where
    -- DVC is keyed by vehicleCategory and FVC is not, so several rows can share a documentType.
    -- Keep one per documentType, lowest `order` first -- getOneConfig throws if two survive.
    collapse = nubBy (\x y -> x.documentType == y.documentType) . sortOn (\c -> (c.order, show c.vehicleCategory :: Text))
    mkFleetConfig DVC.DocumentVerificationConfig {..} =
      DT.FleetOwnerDocumentVerificationConfig {role = nub (fromMaybe [] rolesAllowedToUploadDocument), ..}
