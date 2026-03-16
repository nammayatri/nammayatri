{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Transformers.JourneyLeg where

import Control.Applicative ((<|>))
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLegMapping as DJLM
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, fromMaybeM)

getIsDeleted :: MonadFlow m => Maybe DJLM.JourneyLegMapping -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> m (Kernel.Prelude.Maybe Kernel.Prelude.Bool)
getIsDeleted mbJourneyLegMapping isDeleted = return $ (mbJourneyLegMapping <&> (.isDeleted)) <|> isDeleted

getJourneyId :: MonadFlow m => Maybe DJLM.JourneyLegMapping -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
getJourneyId mbJourneyLegMapping journeyId = do
  let mbJourneyId = (mbJourneyLegMapping <&> (.journeyId)) <|> (Kernel.Types.Id.Id <$> journeyId)
  mbJourneyId & fromMaybeM (InternalError $ "Jouney Id not found for journeyLeg")

getSequenceNumber :: MonadFlow m => Maybe DJLM.JourneyLegMapping -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> m Kernel.Prelude.Int
getSequenceNumber mbJourneyLegMapping sequenceNumber = do
  let mbSequenceNumber = (mbJourneyLegMapping <&> (.sequenceNumber)) <|> sequenceNumber
  mbSequenceNumber & fromMaybeM (InternalError $ "Leg order not found for journeyLeg")
