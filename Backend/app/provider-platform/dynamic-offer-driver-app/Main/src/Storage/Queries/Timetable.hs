{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.Timetable where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time.LocalTime (LocalTime (..))
import qualified Database.Esqueleto.Experimental as Ex
import qualified Database.Esqueleto.Internal.Internal as EI
import Database.Persist (unFieldNameDB)
import Database.Persist.SqlBackend
import qualified Domain.Types.Booking as DBooking
import Domain.Types.TimeRange (TimeRange)
import Domain.Types.Timetable (Timetable)
import qualified Domain.Types.Timetable as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.RecurringBooking (fullRecurringBookingT)
import Storage.Tabular.FarePolicy
import Storage.Tabular.RecurringBooking
import Storage.Tabular.Timetable as STimetable

findAllUnscheduledAndActiveDuring :: Transactionable m => TimeRange -> m [Id Timetable]
findAllUnscheduledAndActiveDuring timeRange =
  Esq.findAll $ do
    tt <- allUnscheduledAndActiveDuring timeRange
    pure $ tt ^. TimetableTId

allUnscheduledAndActiveDuring :: TimeRange -> SqlQuery (SqlExpr (Entity TimetableT))
allUnscheduledAndActiveDuring timeRange = do
  tt <- from $ table @TimetableT
  where_ $
    tt ^. TimetableStatus ==. val Domain.Active
      &&. Esq.isNothing (tt ^. TimetableBookingId)
      &&. tt ^. TimetablePickupTime >=. val (localTimeOfDay timeRange.start)
      &&. tt ^. TimetablePickupTime <=. val (localTimeOfDay timeRange.end)
      &&. tt ^. TimetablePickupDate >=. val (localDay timeRange.start)
      &&. tt ^. TimetablePickupDate <=. val (localDay timeRange.end)
  pure tt

findUpcomingBooking :: Transactionable m => Id Timetable -> m (Maybe Domain.UpcomingBooking)
findUpcomingBooking ttId = Esq.buildDType $
  fmap (fmap $ extractSolidType @Domain.UpcomingBooking) $
    Esq.findOne' $ do
      (tt :& (rbt :& fromLocT :& toLocT) :& fpt) <-
        from $
          table @TimetableT
            `innerJoin` fullRecurringBookingT
            `Esq.on` (\(tt :& (rbt :& _ :& _)) -> tt ^. TimetableRecurringBookingId ==. rbt ^. RecurringBookingTId)
            `innerJoin` table @FarePolicyT
            `Esq.on` (\(_ :& (rbt :& _ :& _) :& fpt) -> rbt ^. RecurringBookingFarePolicyId ==. fpt ^. FarePolicyTId)

      where_ $ tt ^. TimetableTId ==. val (toKey ttId)
      pure (tt, (rbt, fromLocT, toLocT), fpt)

updateTimetableWithBookingId :: Id Timetable -> Id DBooking.Booking -> SqlDB ()
updateTimetableWithBookingId timetableId bookingId =
  Esq.update $ \tt -> do
    set
      tt
      [ TimetableBookingId =. just (val $ toKey bookingId)
      ]
    where_ $ tt ^. TimetableTId ==. val (toKey timetableId)

-- NOTE: This was copied from esqueleto 3.5.2.3 need to update version
values :: (EI.ToSomeValues a, Ex.ToAliasReference a, Ex.ToAlias a) => NE.NonEmpty a -> Ex.From a
values exprs = Ex.From $ do
  ident <- EI.newIdentFor $ EI.DBName "vq"
  alias <- Ex.toAlias $ NE.head exprs
  ref <- Ex.toAliasReference ident alias
  let aliasIdents =
        mapMaybe
          ( \someVal -> case someVal of
              EI.SomeValue (EI.ERaw aliasMeta _) -> EI.sqlExprMetaAlias aliasMeta
          )
          $ EI.toSomeValues ref
  pure (ref, const $ mkExpr ident aliasIdents)
  where
    someValueToSql :: EI.IdentInfo -> EI.SomeValue -> (TLB.Builder, [PersistValue])
    someValueToSql info (EI.SomeValue expr) = EI.materializeExpr info expr

    mkValuesRowSql :: EI.IdentInfo -> [EI.SomeValue] -> (TLB.Builder, [PersistValue])
    mkValuesRowSql info vs =
      let materialized = someValueToSql info <$> vs
          valsSql = TLB.toLazyText . fst <$> materialized
          params = concatMap snd materialized
       in (TLB.fromLazyText $ "(" <> TL.intercalate "," valsSql <> ")", params)

    -- (VALUES (v11, v12,..), (v21, v22,..)) as "vq"("v1", "v2",..)
    mkExpr :: EI.Ident -> [EI.Ident] -> EI.IdentInfo -> (TLB.Builder, [PersistValue])
    mkExpr valsIdent colIdents info =
      let materialized = mkValuesRowSql info . EI.toSomeValues <$> NE.toList exprs
          (valsSql, params) =
            ( TL.intercalate "," $ map (TLB.toLazyText . fst) materialized,
              concatMap snd materialized
            )
          colsAliases = TL.intercalate "," (map (TLB.toLazyText . EI.useIdent info) colIdents)
       in ( "(VALUES " <> TLB.fromLazyText valsSql <> ") AS "
              <> EI.useIdent info valsIdent
              <> "("
              <> TLB.fromLazyText colsAliases
              <> ")",
            params
          )

insertTimetables :: [Timetable] -> SqlDB ()
insertTimetables timetables = do
  SqlDB $
    lift $
      insertManyOnConflict
        (fmap toTType timetables)
        UniqueBookingPickupDate
        DoNothing

insertManyOnConflict ::
  forall a backend m unique.
  ( PersistEntity a,
    EI.KnowResult unique ~ Unique a,
    EI.FinalResult unique,
    MonadIO m,
    BackendCompatible SqlBackend backend
  ) =>
  [a] ->
  unique ->
  OnConflict a ->
  ReaderT backend m ()
insertManyOnConflict [] _ _ =
  pure ()
insertManyOnConflict valuesToInsert conflictKey onConflict = do
  sqlBackend <- asks projectBackend
  let info = (sqlBackend, EI.initialIdentState)

      -- Since we don't actually ever look at anything other than the type of the
      -- SqlExpr argument in the sqlInsertInto call we can use laziness to
      -- summon a value with the correct type
      --
      -- NOTE: do not try to force this value as it will cause infinite recursion
      insertionProxy = insertionProxy :: SqlExpr (EI.Insertion a)

      insertIntoFragment = EI.sqlInsertInto info insertionProxy

      (valuesBuilders, valuesValues) = unzip $ fmap mkSqlEntityFragment valuesToInsert

      valuesFragment =
        ( "VALUES " <> EI.uncommas valuesBuilders <> "\n",
          concat valuesValues
        )

      uniqueFieldName =
        EI.uncommas $
          toList $
            TLB.fromText . flip getEscapedRawName sqlBackend . unFieldNameDB . snd
              <$> persistUniqueToFieldNames (EI.finalR conflictKey)

      onConflictFragment =
        ("ON CONFLICT (" <> uniqueFieldName <> ") ", []) <> renderOnConflict onConflict

      (queryBuilder, queryValues) =
        mconcat [insertIntoFragment, valuesFragment, onConflictFragment]
  rawExecute (TL.toStrict $ TLB.toLazyText queryBuilder) queryValues

mkSqlEntityFragment :: (PersistEntity a) => a -> (TLB.Builder, [PersistValue])
mkSqlEntityFragment entityVal =
  let vals = fmap toPersistValue $ toPersistFields entityVal
      builder = EI.parens $ EI.uncommas $ replicate (length vals) "?"
   in (builder, vals)

renderOnConflict :: OnConflict a -> (TLB.Builder, [PersistValue])
renderOnConflict onConflict =
  case onConflict of
    DoNothing -> ("DO NOTHING", [])

data OnConflict a
  = DoNothing
