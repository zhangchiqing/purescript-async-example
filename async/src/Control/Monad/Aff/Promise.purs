{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Control.Monad.Aff.Promise
  ( defer
  , wait
  , Promise
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Aff (Aff, forkAll)
import Control.Monad.Aff.AVar (AVAR, makeVar', makeVar, takeVar, putVar, modifyVar)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Fork (class MonadFork, fork)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)

import Data.Foldable (foldl)
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)

newtype Promise a = Promise (∀ eff. Aff (avar ∷ AVAR | eff) a)

-- | Forks an asynchronous computation, returning a Promise that will block
-- | until completed. Promise results are shared among consumers (one-to-many
-- | resolution).
defer
  ∷ ∀ m eff e a
  . (Affable (avar ∷ AVAR | eff) m, MonadFork e m)
  ⇒ m a
  → m (Promise a)
defer run = do
  cell ← fromAff $ makeVar' Nothing
  consumers ← fromAff $ makeVar' mempty
  fork do
    res ← run
    fromAff do
      fns ← takeVar consumers
      modifyVar (const (Just res)) cell
      putVar consumers mempty
      forkAll (foldl (\xs f → f res : xs) mempty fns)
  pure $ Promise do
    res ← takeVar cell
    putVar cell res
    case res of
      Just a → pure a
      Nothing → do
        res' ← makeVar
        modifyVar (putVar res' : _) consumers
        takeVar res'

-- | Blocks until a Promise is resolved. If the Promise has already resolved,
-- | then it will yield immediately.
wait
  ∷ ∀ m eff a
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ Promise a → m a
wait (Promise run) = fromAff run

instance functorPromise ∷ Functor Promise where
  map f (Promise run) = Promise (f <$> run)

instance applyPromise ∷ Apply Promise where
  apply = ap

instance applicativePromise ∷ Applicative Promise where
  pure = Promise <<< pure

instance bindPromise ∷ Bind Promise where
  bind (Promise run) k = Promise (run >>= k >>> wait)

instance monadPromise ∷ Monad Promise

instance semigroupPromise ∷ (Semigroup a) ⇒ Semigroup (Promise a) where
  append = lift2 append

instance monoidPromise ∷ (Monoid a) ⇒ Monoid (Promise a) where
  mempty = pure mempty

instance monadRecPromise ∷ MonadRec Promise where
  tailRecM k a = Promise (tailRecM (wait <<< k) a)
