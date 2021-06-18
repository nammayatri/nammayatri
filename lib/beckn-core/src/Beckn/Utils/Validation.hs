module Beckn.Utils.Validation where

import Beckn.Types.Error.API
import Beckn.Types.Flow
import Beckn.Types.Validation.Predicate
import Beckn.Utils.Error.Throwing
import qualified Data.Aeson.Types as A
import Data.Either.Validation
import qualified Data.Text as T
import EulerHS.Prelude hiding (pred)
import qualified GHC.Generics

type Validate a = a -> Validation [Text] ()

validate ::
  (Predicate a p, ShowablePredicate p) =>
  Text ->
  a ->
  p ->
  Validation [Text] ()
validate name a pred =
  unless (pFun pred a) . Failure $ [pShow pred name]

validateMaybe ::
  (Predicate a p, ShowablePredicate p) =>
  Text ->
  Maybe a ->
  p ->
  Validation [Text] ()
validateMaybe name mbField pred =
  whenJust mbField (\field -> validate name field pred)

validate' ::
  Text ->
  a ->
  (a -> Bool) ->
  (Text -> Text) ->
  Validation [Text] ()
validate' name a predFun predShow =
  unless (predFun a) . Failure $ [predShow name]

runValidationFromJson :: Text -> Validate a -> a -> A.Parser a
runValidationFromJson name validation a =
  case validation a of
    Success _ -> pure a
    Failure errs -> fail $ show $ validationErrorsToText name errs

runRequestValidation :: Text -> Validate a -> a -> FlowR r a
runRequestValidation name validation a =
  case validation a of
    Success _ -> pure a
    Failure errs -> throwError $ InvalidRequest $ validationErrorsToText name errs

validationErrorsToText :: Text -> [Text] -> Text
validationErrorsToText name errs =
  name
    <> " validation failure, the following expectations are not met: "
    <> T.intercalate ", " errs

genericParseJsonWithValidation ::
  ( Generic c,
    A.GFromJSON A.Zero (GHC.Generics.Rep c)
  ) =>
  Text ->
  Validate c ->
  A.Value ->
  A.Parser c
genericParseJsonWithValidation name val =
  genericParseJSON A.defaultOptions
    >=> runValidationFromJson name val
