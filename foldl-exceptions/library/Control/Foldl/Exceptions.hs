{- |

When 'IO' is the monad of a monadic fold, each step in the evaluation of the
fold might throw an exception, thus causing the entire evaluation to fail. This
module provides some functions for capturing the results from /part of/ a fold
even if some of the steps did not succeed.

This module uses 'try' from the @safe-exceptions@ package and will not catch
async exceptions.

-}

module Control.Foldl.Exceptions
  (
  -- * Stopping at the first exception
    exHalt_, exHalt

  -- * Continuing past failed steps
  , exSkip_, exSkip

  ) where

-- foldl
import Control.Foldl

-- safe-exceptions
import Control.Exception.Safe (Exception, MonadCatch, SomeException, try)

{- |

Performs the steps of a fold up until the first step that throws an exception,
then produces the result obtained thus far.

==== Example

>>> import Control.Exception
>>> f x = if x < 10 then return x else throw Overflow
>>> xs = [1, 2, 500, 4] :: [Integer]

Since @f 500@ produces an exception, the following fold fails:

>>> fold1 = premapM f (generalize list)
>>> foldM fold1 xs
*** Exception: arithmetic overflow

By applying 'untilFirstException', we can produce a new fold that returns
the intermediate result at the point where the exception occurs.

>>> fold2 = exHalt_ fold1
>>> foldM fold2 xs
[1,2]

-}

exHalt_ :: forall m a b.
    (Monad m, MonadCatch m) => FoldM m a b -> FoldM m a b

exHalt_ f = fmap snd (exHalt @SomeException f)

{- |

Performs the steps of a fold up until the first step that throws an exception,
then produces a tuple containing:

  1. The exception that was thrown, if any.
  2. The result obtained thus far.

The first type parameter lets you specify what type of exception to catch. Any
other type of exception will still cause the entire fold's evaluation to fail.

==== Example

>>> import Control.Exception
>>> f x = if x < 10 then return x else throw Overflow
>>> xs = [1, 2, 500, 4] :: [Integer]
>>> fold1 = premapM f (generalize list)

>>> :set -XTypeApplications
>>> fold2 = exHalt @ArithException fold1

>>> foldM fold2 xs
(Just arithmetic overflow,[1,2])

-}

exHalt :: forall e m a b.
    (Exception e, Monad m, MonadCatch m) =>
    FoldM m a b -> FoldM m a (Maybe e, b)

exHalt (FoldM step begin done) = FoldM step' begin' done'
  where
    begin' =
      do
        y <- begin
        return (Nothing, y)

    step' x'@(Just _, _) _ = return x'
    step' (Nothing, x1) a =
      do
        x2Either <- try (step x1 a)
        case x2Either of
            Left e   -> return (Just e, x1)
            Right x2 -> return (Nothing, x2)

    done' (eMaybe, x) =
      do
        b <- done x
        return (eMaybe, b)

{- |

Perform only steps of a fold that succeed, ignoring any steps that fail.

==== Example

>>> import Control.Exception
>>> f x = if x < 10 then return x else throw Overflow
>>> xs = [1, 2, 500, 4] :: [Integer]

Since @f 500@ produces an exception, the following fold fails:

>>> fold1 = premapM f (generalize list)
>>> foldM fold1 xs
*** Exception: arithmetic overflow

By applying 'exSkip_', we can produce a new fold that produces a result from all
steps that /don't/ fail:

>>> fold2 = exSkip_ fold1
>>> foldM fold2 xs
[1,2,4]

-}

exSkip_ :: forall m a b.
    (Monad m, MonadCatch m) => FoldM m a b -> FoldM m a b

exSkip_ f = fmap snd (exSkip @SomeException f)

{- |

Perform only steps of a fold that succeed, collecting the exceptions thrown from
each step that fail. Produces a tuple containing:

  1. A list of any exceptions thrown.
  2. The result obtained from the steps that succeeded.

The first type parameter lets you specify what type of exception to catch. Any
other type of exception will still cause the entire fold's evaluation to fail.

==== Example

>>> import Control.Exception
>>> f x = if x < 10 then return x else throw Overflow
>>> xs = [1, 2, 500, 4] :: [Integer]

Since @f 500@ produces an exception, the following fold fails:

>>> fold1 = premapM f (generalize list)
>>> foldM fold1 xs
*** Exception: arithmetic overflow

By applying 'exSkip', we can produce a new fold that produces a result from all
steps that /don't/ fail:

>>> :set -XTypeApplications
>>> fold2 = exSkip @ArithException fold1
>>> foldM fold2 xs
([arithmetic overflow],[1,2,4])

-}

exSkip :: forall e m a b.
    (Exception e, Monad m, MonadCatch m) =>
    FoldM m a b -> FoldM m a ([e], b)

exSkip (FoldM step begin done) = FoldM step' begin' done'
  where
    begin' =
      do
        y <- begin
        return (id, y)

    step' (es, x1) a =
      do
        x2Either <- try (step x1 a)
        case x2Either of
            Left e   -> return (es . (e :), x1)
            Right x2 -> return (es, x2)

    done' (es, x) =
      do
        b <- done x
        return (es [], b)
