{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicElement where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Lib.Yudhishthira.Types.AppDynamicLogicElement
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicElement as Beam
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import qualified Data.Aeson
import qualified Kernel.Types.Id
import qualified Data.String.Conversions



instance FromTType' Beam.AppDynamicLogicElement Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement
    where fromTType' (Beam.AppDynamicLogicElementT {..}) = do pure $ Just Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement{description = description,
                                                                                                                                               domain = domain,
                                                                                                                                               logic = ((Kernel.Prelude.fromMaybe Data.Aeson.Null . Data.Aeson.decode . Data.String.Conversions.cs)) logic,
                                                                                                                                               merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                                                               order = order,
                                                                                                                                               patchedElement = ((\val -> (Data.Aeson.decode . Data.String.Conversions.cs) =<< val)) patchedElement,
                                                                                                                                               version = version,
                                                                                                                                               createdAt = createdAt,
                                                                                                                                               updatedAt = updatedAt}
instance ToTType' Beam.AppDynamicLogicElement Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement
    where toTType' (Lib.Yudhishthira.Types.AppDynamicLogicElement.AppDynamicLogicElement {..}) = do Beam.AppDynamicLogicElementT{Beam.description = description,
                                                                                                                                 Beam.domain = domain,
                                                                                                                                 Beam.logic = ((Data.String.Conversions.cs . Data.Aeson.encode)) logic,
                                                                                                                                 Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                                                                 Beam.order = order,
                                                                                                                                 Beam.patchedElement = (fmap (Data.String.Conversions.cs . Data.Aeson.encode)) patchedElement,
                                                                                                                                 Beam.version = version,
                                                                                                                                 Beam.createdAt = createdAt,
                                                                                                                                 Beam.updatedAt = updatedAt}



