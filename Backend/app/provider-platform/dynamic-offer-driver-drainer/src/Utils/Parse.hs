{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Utils.Parse where

import Data.Aeson as A
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Aeson.Types (Parser, emptyArray)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import EulerHS.KVConnector.Types (MeshMeta (..))
import qualified EulerHS.KVConnector.Types as EKT
import EulerHS.Language as EL
import EulerHS.Prelude
import Sequelize
import Text.Casing (camel)
import Prelude (read)

parseStreamEntryId :: ByteString -> EL.KVDBStreamEntryID
parseStreamEntryId bs =
  -- "number-number" is redis entry id invariant
  let (ms, sq) =
        bimap (read . T.unpack) (read . T.unpack)
          . splitOn "-"
          . TE.decodeUtf8With TE.lenientDecode
          $ bs
   in EL.KVDBStreamEntryID ms sq
  where
    splitOn dilim = (\tup -> (fst tup, T.drop 1 $ snd tup)) . T.breakOn dilim

parseUpdateValue :: A.Value -> Parser (Text, A.Value)
parseUpdateValue = A.withObject "update key value pair" $ \o ->
  (,) <$> (o .: "value0") <*> (o .: "value1")

parseDeleteCommandValues ::
  forall be table. (Model be table, MeshMeta be table) => A.Value -> Parser (Where be table)
parseDeleteCommandValues = A.withObject "DBDeleteCommand: Where clause object" $ \o -> do
  w <- o .: "value0"
  if w == ("where" :: Text)
    then decodeWhere =<< o .: "value1"
    else fail "Expected where clause"

parseUpdateCommandValues :: forall be table. (Model be table, MeshMeta be table) => A.Value -> Parser ([Set be table], Where be table)
parseUpdateCommandValues = A.withArray "DBUpdateCommand" $ \v -> do
  case V.toList v of
    [updVals, whereClauseObj] -> liftA2 (,) (parseUpdateSetClause updVals) (parseWhereValuePairs whereClauseObj)
    _ -> fail "Expected UpdateCommand updateVals and whereVals"
  where
    parseUpdateSetClause updVals = (parseSetClause @be @table) =<< parseUpdateValuePairs updVals
    parseUpdateValuePairs o = traverse parseUpdateValue =<< parseJSON o

parseWhereValuePairs :: forall be table. (Model be table, MeshMeta be table) => A.Value -> Parser (Where be table)
parseWhereValuePairs = A.withArray "Where clause list" $ \v -> do
  case V.toList v of
    [whereObj] ->
      ( A.withObject "Where clause object" $ \o -> do
          w <- o .: "value0"
          if w == ("where" :: Text)
            then decodeWhere =<< o .: "value1"
            else fail "Expected where clause"
      )
        whereObj
    _ -> fail "Expected where clause as a list with single element"

decodeWhere ::
  forall be table.
  (Model be table, MeshMeta be table) =>
  A.Value ->
  Parser [Clause be table]
decodeWhere whereClauseObj = (\x -> pure [x]) =<< decodeClause whereClauseObj

decodeClause ::
  forall be table.
  (Model be table, MeshMeta be table) =>
  A.Value ->
  Parser (Clause be table)
decodeClause = foldWhere'
  where
    foldWhere' :: forall b tbl. (Model b tbl, MeshMeta b tbl) => A.Value -> Parser (Clause b tbl)
    foldWhere' obj = case obj of
      (A.Object km) ->
        if AKM.member (AesonKey.fromText "$and") km
          then
            ( A.withArray "DBUpdateCommand And" $ \v -> do
                clauses <- mapM foldWhere' (V.toList v)
                return $ And clauses
            )
              (fromMaybe emptyArray $ AKM.lookup (AesonKey.fromText "$and") km)
          else
            if AKM.member (AesonKey.fromText "$or") km
              then
                ( A.withArray "DBUpdateCommand Or" $ \v -> do
                    clauses <- mapM foldWhere' (V.toList v)
                    return $ Or clauses
                )
                  (fromMaybe emptyArray $ AKM.lookup (AesonKey.fromText "$or") km)
              else decodeTerm obj
      _ -> fail "unable to decode"

decodeTerm :: forall be table. (Model be table, MeshMeta be table) => A.Value -> Parser (Clause be table)
decodeTerm = \case
  A.Object hm -> do
    (key, val) <- getSingleKeyValue hm
    let keyCamel = (T.pack . camel . T.unpack . AesonKey.toText) key
    case val of
      Just (A.Object obj) -> do
        (operation, mValue) <- getSingleKeyValue obj
        case (operation, mValue) of
          ("$in", Just value) -> do
            case value of -- TODO Use actual In clause here
              A.Array vecList -> do
                let inList = V.toList vecList
                clauseList <- mapM (\v -> (parseFieldAndGetClause @be @table) v keyCamel) inList
                return $ Or $ map (\(EKT.TermWrap col fieldValue) -> Is col (Eq fieldValue)) clauseList
              _ -> fail "Expecting list - Decoding failed at term $in"
          ("$gt", Just value) -> do
            (EKT.TermWrap column fieldValue) <- (parseFieldAndGetClause @be @table) value keyCamel
            -- (column, fieldValue) <- getFieldAndValue value keyCamel
            return $ Is column (GreaterThan fieldValue)
          ("$gte", Just value) -> do
            (EKT.TermWrap column fieldValue) <- (parseFieldAndGetClause @be @table) value keyCamel
            return $ Is column (GreaterThanOrEq fieldValue)
          ("$lt", Just value) -> do
            (EKT.TermWrap column fieldValue) <- (parseFieldAndGetClause @be @table) value keyCamel
            return $ Is column (LessThan fieldValue)
          ("$lte", Just value) -> do
            (EKT.TermWrap column fieldValue) <- (parseFieldAndGetClause @be @table) value keyCamel
            return $ Is column (LessThanOrEq fieldValue)
          ("$notIn", Just value) -> do
            -- (\(Is col term) -> Is col (Not term)) <$> decodeTerm (A.object [keyCamel A..= A.object ["$in" A..= value]])
            case value of -- TODO Use actual Not In
              A.Array vecList -> do
                let inList = V.toList vecList
                clauseList <- mapM (\v -> (parseFieldAndGetClause @be @table) v keyCamel) inList
                return $ And $ map (\(EKT.TermWrap col fieldValue) -> Is col (Not $ Eq fieldValue)) clauseList
              _ -> fail "Expecting an Array - Error decoding term"
          ("$ne", Just value) -> do
            (EKT.TermWrap column fieldValue) <- (parseFieldAndGetClause @be @table) value keyCamel
            return $ Is column (Not $ Eq fieldValue)
          ("$not", Just value) -> (\(Is col term) -> Is col (Not term)) <$> decodeTerm (A.object [(AesonKey.fromText keyCamel) A..= value])
          _ -> fail "Expecting term constructor - Error decoding term"
      Just value -> do
        -- Eq case {"id","1234"}
        (EKT.TermWrap column fieldValue) <- (parseFieldAndGetClause @be @table) value keyCamel
        return $ Is column (Eq fieldValue)
      _ -> fail "Expecting term object - Error decoding term"
  _ -> fail "Expecting Clause object - Error decoding Clause"
  where
    getSingleKeyValue km = case AKM.keys km of
      [k] -> return (k, AKM.lookup k km)
      _ -> fail "Unable to decode term - Expecting object with single key"
