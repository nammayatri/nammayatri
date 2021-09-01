# Architecture
There are few layers in this project

## Product layer
Product layer is the topmost layer, all endpoints and business logic belongs to it. Endpoints return type `FlowHandler` which you can construct with `withFlowHandlerBecknAPI` and `withFlowHandlerAPI`. For the distinction between these two see [Error handling](./error-handling.md)

Under the hood we use [EulerHS](https://bitbucket.org/juspay/euler-hs/src/master/)' `Flow` type. But all of our code in this layer is written polymorphically in `m` with some constraints. These constraints can be:
* `MonadReader r m` is used for accessing environment. Each app has different environment, all of the library function should work in any environment. To constraint an environment currently we use `HasField` and our custom `HasFields`.
* `Log` is an mtl-style logging interface
* EulerHS' `MonadFlow` is an interface for EulerHS' `Flow` type
* `Forkable` is a custom typeclass to support forking
* `MonadThrow` and `MonadCatch` for throwing and catching
* `MonadGuid` is used for id's generation
* `MonadTime` is used to get time
* `MonadClock` is used to get system's clock time. It is used for metrics mostly
* `Beckn.Types.App.MonadFlow` is a combination of many generic common constraints

As well as mtl-style classes we use handle/service pattern in our business logic. There are two types of handler pattern: creating a handle per each "module" and pass them to the code that uses it; and creating one handle with only the dependencies that the code uses. We use the second approach

There might arise a question what should one choose to implement some "module": mtl-style typeclass or handle pattern? For common generic things (like logging, accessing time, or some generic metrics) we use typeclasses, for business logic we use handle pattern. For some tasks it is enough to have some `HasField` constraint on an environment to make it work in different apps.

## Queries layer
Queries layer is the DB access layer. It is fairly primitive, consisting of DB types and access functions. All reading (SQL's `SELECT`) queries should return `Maybe` value that would be unwrapped somewhere in Product layer. All writing queries (SQL's `INSERT`, `UPDATE` and  `DELETE`) should be written in `SqlDB` monad and run with `runSqlDBTransaction` function. As you can guess from the name it is needed to support transactions.

Under the hood we use [Beam](https://bitbucket.org/juspay/euler-hs/src/master) as a DB adapter. There's also one [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) query and there are plans to move to [Esqueleto](https://hackage.haskell.org/package/esqueleto) + [Persistent](https://hackage.haskell.org/package/persistent)

