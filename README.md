# avail

`avail` is a companion to monad transformers that allows you to impose effect constraints on *concrete monads*. Specifically, instead of writing

```haskell
myApp :: (MonadWriter Log m, MonadState Store m, MonadReader Env m) => m ()
```

it allows you to write

```haskell
myApp :: Effs '[MonadWriter Log, MonadState Store, MonadReader Env] => App ()
```

where `App` is a specific, concrete monad stack.

## Introduction

Current effects libraries all have one principle of effect restriction: an effect can be used in a monad if it can be interpreted in terms of the monad. This works well with a polymorphic monad type, but a polymorphic type is unfriendly to compiler optimization. In contrast, a concrete monad can be easily optimized, but if we fix a monad that supplies all the effects we need, we can no longer restrict what effects each function can use.

`avail` solves this problem with the [*phantom constraint pattern*](https://xn--i2r.xn--rhqv96g/2021/09/14/redundant-constraints/). We use a newtype wrapper `M` to screen out the user from directly manipulating the underlying monad, and let user to implement interpretations of effects in terms of other more primitive effects. In any function, the user can use an effect only if:

- The effect is implemented for the monad, and
- The *effect constraint* `Eff e` is available in the context.

The second requirement decouples the availability of effects from the monad implementation. At last, we use a function `runM` to clear the constraints and restore the underlying monad. A typical example looks like this:

```haskell
import Avail
import Control.Monad.Reader
import Control.Monad.State

type App = M (ReaderT Int (State Bool))

testParity :: Effs '[MonadReader Int, MonadState Bool] => App ()
testParity = do
  num <- ask @Int
  put (even num)

example :: IO ()
example = do
  print $ runM @'[MonadReader Int, MonadState Bool] testParity
    & (`runReaderT` 2)
    & (`execState` False)
  print $ runM @'[MonadReader Int, MonadState Bool] testParity
    & (`runReaderT` 3)
    & (`execState` False)
```

Through microbenchmarks and tests in some applications, the performance of using `avail` is at least on par with, and often better than, using the polymorphic counterparts.

## Making `avail` work with new typeclasses

`avail` already comes with support of all `mtl`, `exceptions`, `unliftio`, `monad-control` and [`capability`](https://hackage.haskell.org/package/capability) typeclasses. To add support for your own typeclass, for example:

```haskell
class MonadOvO m where
  ...
```

You can use the Template Haskell utilities in the `Avail.Derive` module.

```haskell
import Avail.Derive
avail [t| MonadOvO |]
```

There may be other more complicated cases, such as dependencies between classes and multi-param classes:

```haskell
class (MonadOvO m, MonadQwQ m) => MonadUwU r m where
  ...
```

`avail` gets you covered:

```haskell
import Avail.Derive
with1 \r -> avail'
  [ [t| MonadOvO |]
  , [t| MonadQwQ |]
  ] [t| MonadUwU $r |]
```

## Limitations

- Running effects:
  Because effect constraints in `avail` are detached from the monad structure, they cannot be run on a one-by-one basis. Practically, one can only run all effects and obtain the underlying concrete monad at once via `runM`. This means there is no exact equivalent to `runReaderT`, `runExceptT` etc on the `M` monad.

  If your application can entirely run on a single transformer stack (in particular, `ReaderT IO`, but also other transformer stacks), this is a non-issue because there will be no need to run effects one-by-one. For some other scenarios, there are some solutions that may be used solve this issue:

  - `local` is an almost identical substitute to `runReaderT` without destructing the monad.
  - Similarly, `tryError` is a substitute to `runExceptT`.
  - To simulate `runStateT`, simply set the value before the action and get the value after it.
  - `listen` is a very close analog to `runWriterT`.

## Where is `availability`?

The old `availability` is abandoned due to its attempt at addressing two problems at once (capability *and* availability management), and may introduce confusion to new users on the choice between `capability` and `availability`. The new `avail` library focuses on availability management and acts not as a standalone effects library, but an alternative effect management layer that is used together with `mtl` or `capability` (which also reduces preformance overhead).

The old `availability` can be found at [`re-xyr/availability-old`](https://github.com/re-xyr/availability-old).
