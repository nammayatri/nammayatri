# Data validation
Roughly speaking, there are two types of validation.

## Pure validation
Pure validation is mostly validating requests that come from type forms. Checking that some field matches some regex does not require anything except the data itself, thus it is pure.

Let's say we have type
```haskell
data SomeRequest = SomeRequest
  { field1 :: Text
  , field2 :: Maybe Int
  , field3 :: SubType
  }

data SubType = SubType
  { field4 :: Text
  }
```
and we need to check that `field1` is not empty string and `field2` is greater than 0 if it is `Just`. Validation function would look something like this:
```haskell
validateSomeRequest :: Validate SomeRequest
validateSomeRequest SomeRequest {..} =
     validateField "field1" field1 NotEmpty
  *> validateField "field2" field2 (InMaybe $ GreaterThan 0)
  *> validateObject "field3" field3 validateSubType
```
To run a validation function use `runRequestValidation`.

### Predicates
A predicate is a function that returns a boolean value. Validation functions are natural predicates. All predicates are stored in `Beckn.Types.Predicates`. Our validation predicates are defined as separate types (often polymorphic) that have instance of `Predicate` which defines the predicate function itself and instance of `ShowablePredicate` which defines how that predicate is going to be rendered to a human readable string. For matching against regex we use [Kleene](https://hackage.haskell.org/package/kleene)'s `RE Char` type directly.

## Business logic validation
When pure validation is not enough, we do it right in the main flow, usually with a list of `unless` statements. `unless` is better than `when` because it specifies an assertion, not counter-assertion. And it is better than bare `if` expression because it doesn't require unnecessary indentation. So, instead of
```haskell
f userId req = do
  entity <- getEntity req.id
  if entity.user_id == userId
    then do
      ...
    else throwError AccessDenied
```
and
```haskell
f userId req = do
  entity <- getEntity req.id
  when (entity.user_id /= userId) $ throwError AccessDenied
  ...
```
use
```haskell
f userId req = do
  entity <- getEntity req.id
  unless (entity.user_id == userId) $ throwError AccessDenied
  ...
```
