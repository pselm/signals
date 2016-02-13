
-- | Tasks make it easy to describe asynchronous operations that may fail,
-- | like HTTP requests or writing to a database.
-- |
-- | This is implemented on top of Purescript's `Aff` type. The main difference
-- | is that `Task` has a polymorphically-typed error channel, whereas
-- | the error channel for `Aff` can only represent a `String`.

module Elm.Task
    ( module Virtual
    , Task, TaskE, toAff
    , TaskCallback, makeTask
    , succeed, fail
    , mapError, onError
    , toMaybe, fromMaybe
    , toResult, fromResult
    , spawn, sleep
    , ThreadID
    ) where


-- For re-export

import Prelude (map) as Virtual
import Elm.Apply (andMap, map2, map3, map4, map5) as Virtual
import Elm.Bind (andThen) as Virtual
import Data.Traversable (sequence) as Virtual


-- Internal

import Control.Monad.Aff (Aff, makeAff, forkAff, later')
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Error.Class (throwError)
import Prelude (Unit, unit, map, pure, (<<<), (>>=), const, ($))
import Data.Either (Either(..), either)
import Elm.Result (Result(..))
import Elm.Maybe (Maybe(..))
import Data.Int (round)


-- Duplicate from Elm.Time to avoid circular dependency
type Time = Number


-- | Represents asynchronous effects that may fail. It is useful for stuff like
-- | HTTP.
-- | 
-- | For example, maybe we have a task with the type (`Task String User`). This means
-- | that when we perform the task, it will either fail with a `String` message or
-- | succeed with a `User`. So this could represent a task that is asking a server
-- | for a certain user.
-- |
-- | Implemented in terms of Purescript's `Aff` type, with `ExceptT` layered on top
-- | in order to provide for a polymorphically-typed error channel.
type Task x a = forall e. ExceptT x (Aff e) a


-- | Equivalent to a `Task`, but with the effect types specified.
type TaskE e x a = ExceptT x (Aff e) a


-- | Takes a `Task` and unwraps the underlying `Aff`.
-- | 
-- | Note that you can use "do notation" directly with the `Task` type -- you
-- | don't have to unwrap it first. Essentially, you only need to unwrap the
-- | `Task` if you need to interact with the `Aff` type.
toAff :: forall eff x a. Task x a -> Aff eff (Either x a)
toAff = runExceptT


-- | A task that succeeds immediately when run.
-- | 
-- |     succeed 42    -- results in 42
-- | 
-- | Equivalent to Purescript's `pure`.
succeed :: forall x a. a -> Task x a
succeed = pure


-- | A task that fails immediately when run.
-- |
-- |     fail "file not found" : Task String a
-- |
-- | Equivalent to Purescript's `throwError`.
fail :: forall x a. x -> Task x a
fail = throwError


-- | A callback for error and success, to be used when constructing a `Task`
-- | via `makeTask`.
type TaskCallback e x a = ((x -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit)


-- | Creates a `Task` from a function that accepts error and success callbacks.
-- | To create a `Task` using the foreign function interface (FFI), you would typically
-- | pass this function to the FFI as a parameter. Then, you can do your computation,
-- | and callback with the first parameter to indicate failure or the second callback
-- | to indicate success.
-- |
-- | Note that the return value you get from this function can be used as if it were
-- | a `Task x a` ... it's just a bit tricky to ignore the effects types in this case.
makeTask :: forall e x a. TaskCallback e x a -> TaskE e x a
makeTask cb =
    ExceptT (
        -- Everything that `Task` does is "success" from the point of view of `Aff`, since
        -- `Aff` can only fail with a genuine Javascript exception. So, the function we
        -- give to makeAff just goes Left or Right on the success side.
        makeAff \error success ->
            cb (success <<< Left) (success <<< Right)
    )


-- ERRORS

-- | Recover from a failure in a task. If the given task fails, we use the
-- | callback to recover.
-- | 
-- |     fail "file not found" `onError` (\msg -> succeed 42) -- succeed 42
-- |     succeed 9 `onError` (\msg -> succeed 42)             -- succeed 9
-- |
-- | Like Purescript's `catchError`, but with a different signature.
onError :: forall x y a. Task x a -> (x -> Task y a) -> Task y a
onError task handler =
    -- This is equivalent to `catchError`, but that doesn't work by itself,
    -- because it would need a signature of
    --
    -- `forall x a. Task x a -> (x -> Task x a) -> Task x a`
    ExceptT (
        runExceptT task >>=
            either
                (runExceptT <<< handler)
                (pure <<< Right)
    )


-- | Transform the error value. This can be useful if you need a bunch of error
-- | types to match up.
-- | 
-- |     type Error = Http Http.Error | WebGL WebGL.Error
-- | 
-- |     getResources : Task Error Resource
-- |     getResources =
-- |       sequence [ mapError Http serverTask, mapError WebGL textureTask ]
-- |
-- | Equivalent to Purescript's `withExceptT`.
mapError :: forall x y a. (x -> y) -> Task x a -> Task y a
mapError = withExceptT


-- | Helps with handling failure. Instead of having a task fail with some value
-- | of type `x` it promotes the failure to a `Nothing` and turns all successes into
-- | `Just` something.
-- | 
-- |     toMaybe (fail "file not found") == succeed Nothing
-- |     toMaybe (succeed 42)            == succeed (Just 42)
-- | 
-- | This means you can handle the error with the `Maybe` module instead.
toMaybe :: forall x y a. Task x a -> Task y (Maybe a)
toMaybe task =
    map Just task `onError` (\_ -> succeed Nothing)


-- | If you are chaining together a bunch of tasks, it may be useful to treat
-- | a maybe value like a task.
-- | 
-- |     fromMaybe "file not found" Nothing   == fail "file not found"
-- |     fromMaybe "file not found" (Just 42) == succeed 42
fromMaybe :: forall x a. x -> Maybe a -> Task x a
fromMaybe default maybe =
    case maybe of
        Just value -> succeed value
        Nothing -> fail default


-- | Helps with handling failure. Instead of having a task fail with some value
-- | of type `x` it promotes the failure to an `Err` and turns all successes into
-- | `Ok` something.
-- | 
-- |     toResult (fail "file not found") == succeed (Err "file not found")
-- |     toResult (succeed 42)            == succeed (Ok 42)
-- | 
-- | This means you can handle the error with the `Result` module instead.
toResult :: forall x y a. Task x a -> Task y (Result x a)
toResult task =
    map Ok task `onError` (\msg -> succeed (Err msg))


-- | If you are chaining together a bunch of tasks, it may be useful to treat
-- | a result like a task.
-- | 
-- |     fromResult (Err "file not found") == fail "file not found"
-- |     fromResult (Ok 42)                == succeed 42
fromResult :: forall x a. Result x a -> Task x a
fromResult result =
    case result of
        Ok value -> succeed value
        Err msg -> fail msg


-- THREADS

-- | Abstract type that uniquely identifies a thread.
newtype ThreadID = ThreadID Int


-- | Run a task on a separate thread. This lets you start working with basic
-- | concurrency. In the following example, `task1` and `task2` will be interleaved.
-- | If `task1` makes a long HTTP request, we can hop over to `task2` and do some
-- | work there.
-- | 
-- |     spawn task1 `andThen` \_ -> task2
spawn :: forall x y a. Task x a -> Task y ThreadID
spawn task =
    ExceptT (
        -- Since nothing uses the ThreadID at the moment, we'll just make one up ...
        -- this may need to change in the future ...
        map
            (const (Right (ThreadID 1)))
            (forkAff $ runExceptT task)
    )


-- | Make a thread sleep for a certain amount of time. The following example
-- | sleeps for 1 second and then succeeds with 42.
-- | 
-- |     sleep 1000 `andThen` \_ -> succeed 42
sleep :: forall x. Time -> Task x Unit
sleep time =
    ExceptT $ later' (round time) (pure (Right unit))

