# Error handling
BPP and BAP combine two types of endpoints:

* Beckn-spec actions for communicating with BAP, BPP and BG: `search`, `on_search`, `confirm`, `on_confirm`, etc.
* Endpoints for mobile apps: Yatri partner app (BPP) and Yatri customer app (BAP)

Beckn-spec endpoints require errors to be returned in one format, mobile endpoints returns error in other format. Each endpoint is "signed" with special function, that catches exception and converts them to desired format: `withFlowHandlerBecknAPI` and `withFlowHandlerAPI`.

All exception make up a hierarchy, to create a new exception one should bind it to this hierarchy tree with `instanceExceptionWithParent` TH macro.

`BaseError` is the root of all exceptions, it has only a message we want to print which can be set in `IsBaseError` type class (all child errors inherit it). These are raw errors, in context of endpoints they are interpreted as 500 HTTP errors, in context of services (e.g. allocation service) they can be used as service-specific errors.

`HTTPError` is a child of `BaseErrors`. These have error codes (upper snake case word), http code and custom headers. `HTTPError`s also have to be `APIError`s and `BecknAPIError`s. This is done so errors can be generic, like some db error, but they could have different representations in mobile apps API and in beckn API.

## Throwing errors

To throw an exception use `throwError`

To unwrap a `Maybe` value use `fromMaybeM`

To unwrap an `Either` value use `fromEitherM`

## Adding errors

Where to put an error? There's no yet consensus on this. Few options are possible:

* `Beckn.Types.Error` — for generic errors
* `Types.Error` — for generic per project
* In the module right near the function where it is thrown — for very specific errors that wouldn't be used anywhere else

When creating an error you need to put some additional info in it, try to avoid generic `Text` fields. For example, instead of
```haskell
data MissingHeader = MissingHeader Text

instance IsBaseError HeaderError where
  toMessage (MissingHeader msg) = Just msg

f headerName = throwError . MissingHeader $ "Header " <> headerName <> " is missing"
```
use more specific type `HeaderName` and define message generation in typeclass instance:
```haskell
data MissingHeader = MissingHeader HeaderName

instance IsBaseError HeaderError where
  toMessage (MissingHeader headerName) = Just $ "Header " <> headerName <> " is missing"

f headerName = throwError . MissingHeader $ headerName
```

## Unwrapping external API calls errors

When making an API call there can be two types of errors: transport protocol error and API error. You can get either a "connection refused" or a properly formatted json with some error code. To distinguish these two, use `callApiExtractingApiError` and `callApiUnwrappingApiError`. For an API error type you would need to specify `FromResponse` instance which defines how exactly an error type is extracted from an HTTP response. Use `callOwnAPI` to parse an error as `APIError` and use `callBecknAPI`/`callBecknAPI'` to parse an error as a beckn-spec error.
