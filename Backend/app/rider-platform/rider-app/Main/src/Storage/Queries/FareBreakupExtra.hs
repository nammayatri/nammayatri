module Storage.Queries.FareBreakupExtra where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Tuple.Extra as Tuple
import Domain.Types.Booking
import Domain.Types.FareBreakup as DFB
import qualified EulerHS.Prelude as EP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, addPrice)
import qualified Sequelize as Se
import qualified Storage.Beam.FareBreakup as BeamFB
import Storage.Queries.OrphanInstances.FareBreakup ()

mergeByTitleAndCreateMany :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [FareBreakup] -> m ()
mergeByTitleAndCreateMany fbs = do
  let mergeCongestionChargeTo = bool DFB.BASE_FARE DFB.DISTANCE_FARE $ any (\fb -> getTitle fb == DFB.DISTANCE_FARE) fbs
      mergePairs = [(DFB.CONGESTION_CHARGE, mergeCongestionChargeTo)]
  mergedFareBreakups <- mergeFareBreakups fbs mergePairs
  traverse_ create mergedFareBreakups
  where
    {- Takes a list of fareBreakups and a list of pairs of breakup titles to be merged (first element is merged into second element) and returns a list of fareBreakups with the merged titles.
       | In case of duplicate first elements of pair, the last pair will be considered.
       | If the given pair's first element is not present in the given list of FareBreakup, it is ignored.
       | If the given pair's second element is not present in the given list of FareBreakup, first element isn't removed from the list -}
    mergeFareBreakups :: (MonadFlow m) => [FareBreakup] -> [(FareBreakupTitle, FareBreakupTitle)] -> m [FareBreakup]
    mergeFareBreakups fareBreakups mergePairs = do
      let mergerHashMap = HMS.fromList mergePairs
          (toBeMergedFBList, mergeIntoFBList) = partitionMaybe (\fb -> (fb,) <$> HMS.lookup (getTitle fb) mergerHashMap) fareBreakups
          mergeIntoHashMap = HMS.fromList $ map (getTitle Tuple.&&& EP.id) mergeIntoFBList
      finalFBHashMap :: HMS.HashMap FareBreakupTitle FareBreakup <-
        foldlM
          ( \hmap (fb, mergeIntoFBTitle) ->
              case HMS.lookup mergeIntoFBTitle hmap of
                Just mergeIntoFB -> do
                  mergedFB <- mergeInto fb mergeIntoFB
                  pure $ HMS.insert mergeIntoFBTitle mergedFB hmap
                Nothing -> do
                  let title = getTitle fb
                  pure $ HMS.insert title fb hmap
          )
          mergeIntoHashMap
          toBeMergedFBList
      pure $ HMS.elems finalFBHashMap
      where
        -- TODO:: Move to shared kernel
        partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
        partitionMaybe f =
          foldr'
            ( \x (ys, zs) ->
                case f x of
                  Just y -> (y : ys, zs)
                  Nothing -> (ys, x : zs)
            )
            ([], [])

        mergeInto :: (MonadFlow m) => FareBreakup -> FareBreakup -> m FareBreakup
        mergeInto fb2 fb1 = do
          finalPrice <- addPrice fb1.amount fb2.amount
          pure $ fb1 {amount = finalPrice}

    getTitle :: FareBreakup -> FareBreakupTitle
    getTitle = fromMaybe DFB.PLATFORM_FEE . (.title) -- As we will always have title in new breakups, so default case will never be used.
    create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (DFB.FareBreakup -> m ())
    create = createWithKV

findAllByBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> m [FareBreakup]
findAllByBookingId bookingId = findAllWithKVAndConditionalDB [Se.Is BeamFB.bookingId $ Se.Eq $ getId bookingId] Nothing

deleteAllByBookingId :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> m ()
deleteAllByBookingId bookingId = deleteWithKV [Se.Is BeamFB.bookingId $ Se.Eq $ getId bookingId]

findAllByEntityIdAndEntityTypeInKV :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> FareBreakupEntityType -> m [FareBreakup]
findAllByEntityIdAndEntityTypeInKV entityId entityType =
  findAllWithKVAndConditionalDB
    [Se.And [Se.Is BeamFB.bookingId $ Se.Eq entityId, Se.Is BeamFB.entityType $ Se.Eq entityType]]
    Nothing
