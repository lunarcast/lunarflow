module Lunarflow.ErrorStack where

import Prelude
import Data.Debug (class Debug, genericDebug)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.String (joinWith)
import Lunarflow.String (indent)
import Run (Run)
import Run.Except as Except

-- | Error stack with a stack of extra data on top of it.
data ErrorStack a e
  = While a (ErrorStack a e)
  | Errored e

-- | Either type which uses an error stack as an error.
type EitherStacked a e r
  = Either (ErrorStack a e) r

-- | The Except functor but using an error stack.
type EXCEPT_STACKED a e
  = Except.EXCEPT (ErrorStack a e)

-- | Add extra data to possible errors. 
while ::
  forall a e r.
  a ->
  Run
    ( except :: EXCEPT_STACKED a e
    , except :: EXCEPT_STACKED a e
    | r
    )
    ~> Run ( except :: EXCEPT_STACKED a e | r )
while data' = Except.catch $ While data' >>> Except.throw

-- | Throw an error
throw :: forall a e t r. e -> Run ( except :: EXCEPT_STACKED a e | r ) t
throw = Errored >>> Except.throw

------------ Typeclass instances
derive instance genericErrorStack :: Generic (ErrorStack a e) _

instance debugErrorStack :: (Debug a, Debug e) => Debug (ErrorStack a e) where
  debug a = genericDebug a

instance showErrroStack :: (Show e) => Show (ErrorStack String e) where
  show (While data' error) =
    joinWith "\n"
      [ show error
      , indent 4 $ "while " <> data'
      ]
  show (Errored e) = show e
else instance showErrorStack :: (Show a, Show e) => Show (ErrorStack a e) where
  show = go >>> show
    where
    go (While data' inner) = While (show data') (go inner)

    go (Errored e) = Errored e
