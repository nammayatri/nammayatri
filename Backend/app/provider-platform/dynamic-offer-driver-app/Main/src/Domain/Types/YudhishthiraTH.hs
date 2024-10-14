{-# LANGUAGE TypeApplications #-}

module Domain.Types.YudhishthiraTH (generateAllDefault, AllDefaults (..)) where

import Control.Monad
import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Data.Time
import Kernel.Prelude hiding (Type)
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Language.Haskell.TH
import qualified Servant.Client.Core

class AllDefaults a where
  getAllDefaults :: Proxy a -> Maybe a -> [a]

_id :: a -> a
_id a = a

getFieldDefaultValues :: Name -> Exp -> Q (Exp, [Dec])
getFieldDefaultValues fieldTypeName defaultValues = do
  let resultFromPrimitives = go
      proxyN = mkName "Proxy"
  if resultFromPrimitives == ListE []
    then do
      let nestedIns = []
      -- nestedIns <- generateAllDefault fieldTypeName
      let funcCall = AppE (AppE (VarE 'getAllDefaults) (AppTypeE (ConE proxyN) (ConT fieldTypeName))) defaultValues
      return (funcCall, nestedIns)
    else return (resultFromPrimitives, [])
  where
    go
      | fieldTypeName == ''Text = ListE [LitE (StringL "defaultText")]
      | fieldTypeName == ''String = ListE [LitE (StringL "defaultText")]
      | fieldTypeName == ''Int = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Integer = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Double = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''Float = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''HighPrecMoney = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''Meters = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Seconds = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Rational = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''Bool = ListE [ConE 'True]
      | fieldTypeName == ''UTCTime = ListE [AppE (AppE (ConE 'Data.Time.UTCTime) (AppE (VarE 'read) (LitE (StringL "2000-10-10")))) (LitE (IntegerL 0))]
      | fieldTypeName == ''BaseUrl = ListE [AppE (AppE (AppE (AppE (ConE 'BaseUrl) (ConE 'Servant.Client.Core.Https)) (LitE (StringL "example.com"))) (LitE (IntegerL 443))) (LitE (StringL "/this"))]
      | fieldTypeName == ''Kernel.Types.Id.Id = ListE [LitE (StringL "random-1000-4000-8000-uuid")]
      | otherwise = ListE []

-- below just to ease the process a little
generateAllDefault :: Name -> [(String, [String])] -> Q [Dec]
generateAllDefault typeName overrides = do
  res <- reify typeName
  result <- case res of
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> generateAllDefaultForRecords overrides typeName conName fields
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) []) -> generateAllDefaultForRecords overrides typeName conName fields
    TyConI (DataD _ _ _ _ enums _) -> generateAllDefaultForEnums overrides typeName enums
    TyConI (NewtypeD _ _ _ _ enums _) -> generateAllDefaultForEnums overrides typeName [enums]
    TyConI (TySynD _n _ _enums) -> generateAllDefaultForEnums overrides typeName []
    _ -> fail $ "can't generate AllDefaults for type: " ++ show res
  -- runIO $ putStrLn (pprint result)
  pure result

generateAllDefaultForRecords :: [(String, [String])] -> Name -> Name -> [VarBangType] -> Q [Dec]
generateAllDefaultForRecords overrides typeName conName fields = do
  let baseVar = mkName "base"
  let typeProxy = mkName "_typeProxy"
  fieldCombinations <- forM fields $ \(fieldName, _, fieldType) -> do
    let fieldName' = DT.unpack $ DL.reverse (DT.splitOn "." $ show fieldName) DL.!! 0
    let fieldNameVar = mkName $ fieldName' <> "Var"
    let valueFromBaseInput = AppE (VarE 'maybeToList) (UInfixE (VarE fieldName) (VarE 'fmap) (VarE baseVar))
    if elem fieldName' (map fst overrides)
      then pure $ (BindS (VarP fieldNameVar) $ foldl' (\acc (fn, fvs) -> if fn == fieldName' then UInfixE acc (VarE '(<>)) (ListE $ map (\fv -> AppE (VarE 'read) (LitE (StringL fv))) fvs) else acc) (ListE []) overrides, [])
      else do
        (fieldValues, fieldGetAllInstance) <-
          case fieldType of
            AppT (ConT maybeName) (AppT (ConT typeOfConstructor) (ConT _rawFieldType)) | maybeName == ''Maybe && typeOfConstructor == ''Kernel.Types.Id.Id -> do
              fieldAllDefaults <- getFieldDefaultValues typeOfConstructor (ConE 'Nothing)
              let fieldDefaultValues = fst fieldAllDefaults
              let allValues = AppE (AppE (VarE 'map) (ConE 'Just)) fieldDefaultValues
              pure $ (AppE (VarE 'DL.nub) allValues, snd fieldAllDefaults)
            AppT (ConT maybeName) (AppT ListT (ConT rawFieldType)) | maybeName == ''Maybe -> do
              fieldAllDefaults <- getFieldDefaultValues rawFieldType (UInfixE (VarE fieldName) (VarE '(=<<)) (VarE baseVar))
              let fieldDefaultValues = fst fieldAllDefaults
              let nothingAndDefaultValue = InfixE (Just $ ListE [AppE (ConE 'Just) (ListE [])]) (VarE '(<>)) (Just valueFromBaseInput)
              let allValues = InfixE (Just nothingAndDefaultValue) (VarE '(<>)) (Just $ ListE [AppE (ConE 'Just) fieldDefaultValues])
              pure $ (AppE (VarE 'DL.nub) allValues, snd fieldAllDefaults)
            AppT (ConT maybeName) (ConT rawFieldType) | maybeName == ''Maybe -> do
              fieldAllDefaults <- getFieldDefaultValues rawFieldType (UInfixE (VarE fieldName) (VarE '(=<<)) (VarE baseVar))
              let fieldDefaultValues = fst fieldAllDefaults
              let nothingAndDefaultValue = InfixE (Just $ ListE [ConE 'Nothing]) (VarE '(<>)) (Just valueFromBaseInput)
              let allValues = InfixE (Just nothingAndDefaultValue) (VarE '(<>)) (Just $ AppE (AppE (VarE 'map) (ConE 'Just)) fieldDefaultValues)
              pure $ (AppE (VarE 'DL.nub) allValues, snd fieldAllDefaults)
            AppT (ConT typeOfConstructor) (ConT _rawFieldType) | typeOfConstructor == ''Kernel.Types.Id.Id -> do
              fieldAllDefaults <- getFieldDefaultValues typeOfConstructor (ConE 'Nothing)
              let fieldDefaultValues = fst fieldAllDefaults
              pure $ (AppE (VarE 'DL.nub) fieldDefaultValues, snd fieldAllDefaults)
            AppT ListT (ConT rawFieldType) -> do
              fieldAllDefaults <- getFieldDefaultValues rawFieldType (ConE 'Nothing)
              let fieldDefaultValues = fst fieldAllDefaults
              let nothingAndDefaultValue = InfixE (Just $ ListE [ListE []]) (VarE '(<>)) (Just valueFromBaseInput)
              let allValues = InfixE (Just nothingAndDefaultValue) (VarE '(<>)) (Just $ ListE [fieldDefaultValues])
              pure $ (AppE (VarE 'DL.nub) allValues, snd fieldAllDefaults)
            ConT rawFieldType -> do
              fieldAllDefaults <- getFieldDefaultValues rawFieldType (UInfixE (VarE fieldName) (VarE 'fmap) (VarE baseVar))
              let fieldDefaultValues = fst fieldAllDefaults
              pure $ (UInfixE fieldDefaultValues (VarE '(<>)) valueFromBaseInput, snd fieldAllDefaults)
            a@(_) -> do
              runIO $ putStrLn $ "non supported type: " ++ show a
              fail "Not Supported currently, to add support for your case check this: https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/Language-Haskell-TH.html#g:22"
        pure $ (BindS (VarP fieldNameVar) fieldValues, fieldGetAllInstance)

  let mkTypeValue acc fieldCombination = do
        case fst fieldCombination of
          BindS (VarP fieldNameVar) _ -> pure $ AppE acc (VarE fieldNameVar)
          _ -> fail $ show typeName <> ", this shouldn't have happened, generated from previous step. something is wrong with the type :)"

  almostBody <- foldlM mkTypeValue (ConE conName) fieldCombinations
  let allDec = foldl' (\acc fieldCombination -> acc <> (snd fieldCombination)) [] fieldCombinations
  let allfieldDefaultsList = map fst fieldCombinations

  let body = DoE Nothing (allfieldDefaultsList ++ [NoBindS (AppE (VarE 'return) almostBody)])

  let finalBody = UInfixE (AppE (VarE 'take) (LitE (IntegerL 10000000000000000))) (VarE '($)) body

  runIO $ putStrLn $ (pprint finalBody)

  let functionDec = [FunD 'getAllDefaults [Clause [VarP typeProxy, VarP baseVar] (NormalB finalBody) []]]
  let instanceDec = [InstanceD Nothing [] (AppT (ConT ''AllDefaults) (ConT typeName)) functionDec]

  return $ DL.nub $ instanceDec <> allDec

generateAllDefaultForEnums :: [(String, [String])] -> Name -> [Con] -> Q [Dec]
generateAllDefaultForEnums overrides enumName enums = do
  let baseVar = mkName "_base"
  let typeProxy = mkName "_typeProxy"

  body <-
    if not $ null overrides
      then pure $ foldl' (\accD (_, fvs) -> UInfixE accD (VarE '(<>)) (ListE $ map (\fv -> AppE (VarE 'read) (LitE (StringL fv))) fvs)) (ListE []) overrides
      else do
        foldlF enums (ListE []) $ \acc enum -> do
          fmap (UInfixE acc (VarE '(<>))) $ do
            case enum of
              NormalC en [(_, ConT wrappedType)] -> do
                runIO $ putStrLn $ (show enumName) ++ "   " ++ (show enums)
                (fieldValues, _) <- getFieldDefaultValues wrappedType (ConE 'Nothing)
                pure $ AppE (AppE (VarE 'map) (ConE en)) fieldValues
              NormalC en [(_, AppT (ConT typeWrapper) (ConT wrappedType))] | typeWrapper == ''Maybe -> do
                fieldAllDefaults <- getFieldDefaultValues wrappedType (ConE 'Nothing)
                let fieldValues = fst fieldAllDefaults
                pure $ (AppE (AppE (VarE 'map) (UInfixE (ConE en) (VarE '(.)) (ConE 'Just))) fieldValues)
              NormalC en [(_, AppT (ConT typeWrapper) (ConT _))] | typeWrapper == ''Kernel.Types.Id.Id -> do
                fieldAllDefaults <- getFieldDefaultValues typeWrapper (ConE 'Nothing)
                let fieldValues = fst fieldAllDefaults
                pure $ (AppE (AppE (VarE 'map) (UInfixE (ConE en) (VarE '(.)) (ConE 'Kernel.Types.Id.Id))) fieldValues)
              NormalC en [(_, ConT enumInnerValType), (_, AppT (ConT typeWrapper) (ConT wrappedType))] | typeWrapper == ''Maybe -> do
                enumInnerDefaultVals <- getFieldDefaultValues enumInnerValType (ConE 'Nothing)
                fieldAllDefaults <- getFieldDefaultValues wrappedType (ConE 'Nothing)
                let fstLis = mkName "a"
                    sndLis = mkName "b"
                    almostBody = TupE [Just (VarE fstLis), Just (VarE sndLis)]
                    allCombinations = DoE Nothing [BindS (VarP fstLis) (fst enumInnerDefaultVals), BindS (VarP sndLis) (fst fieldAllDefaults), NoBindS (AppE (VarE 'return) almostBody)]
                    tupFst = mkName "x"
                    tupSnd = mkName "y"
                pure $ (AppE (AppE (VarE 'map) (LamE [TupP [VarP tupFst, VarP tupSnd]] (UInfixE (AppE (ConE en) (VarE tupFst)) (VarE '($)) (AppE (ConE 'Just) (VarE tupSnd))))) allCombinations)
              NormalC en _ -> pure $ ListE [ConE en]
              _ -> fail $ "expected enum, but got -> " <> show enum
  let functionDec = [FunD 'getAllDefaults [Clause [VarP typeProxy, VarP baseVar] (NormalB body) []]]
  let instanceDec = [InstanceD Nothing [] (AppT (ConT ''AllDefaults) (ConT enumName)) functionDec]
  -- runIO $ putStrLn $ (pprint instanceDec) ++ "\n\n" ++ (show enums)
  return instanceDec
  where
    foldlF arrV acc fn = foldlM fn acc arrV
