module Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.ST
import Control.Monad.Aff.AVar
import Data.Monoid (mempty)
import Data.Maybe
import Data.Either
import Control.Parallel (sequential, parallel)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration
import Control.Monad.Aff.Promise (defer, wait)

type A = String
type B = String
type C = String
type D = String
type E = String

type AffConsole e a = Aff (console :: CONSOLE, now :: NOW | e) a

getA :: forall e. String -> AffConsole e A
getA a = do
  liftEff $ log "getA"
  a <- later' 2000 $ pure "A"
  liftEff $ log "finish getA"
  pure a

getB :: forall e. String -> AffConsole e B
getB b = do
  liftEff $ log "getB"
  b <- later' 1000 $ pure "B"
  liftEff $ log "finish getB"
  pure b

getCWithAB :: forall e. A -> B -> A -> AffConsole e C
getCWithAB a b c = do
  liftEff $ log "getCWithAB"
  c <- later' 1000 $ pure $ a <> b <> "C"
  liftEff $ log "finish getCWithAB"
  pure c

getDWithB :: forall e. B -> AffConsole e D
getDWithB b = do
  liftEff $ log "getDWithB"
  d <- later' 3000 $ pure $ b <> "D"
  liftEff $ log "finish getDWithB"
  pure d

getEWithCD :: forall e. C -> D -> AffConsole e E
getEWithCD c d = do
  liftEff $ log "getEWithCD"
  e <- later' 2000 $ pure $ c <> d <> "E"
  liftEff $ log "finish getEWithCD"
  pure e


--             0    1    2    3    4    5    6    7
--getA         |--------->
--getB         |---->
--getCWithAB             |---->
--getDWithB         |-------------->
--getEWithCD                       |-------->
--                                           Done

-- The above chart is the async requests flow that I want.
-- I want to send `getA` and `getB` together.
-- When both of them are finished, the results of them will be used to send `getCWithAB`.
-- When `getB` is finished, its result will be used to send `getDWithB`
-- When both `getCWithAB` and `getDWithB` are finished, the results of them will be used to send `getEWithCD`
-- When `getEWithCD` is finished, return the result of E.
--
-- I expect the log to be
-- ```
-- * Build successful.
-- getA
-- getB
-- finish getB
-- getDWithB
-- finish getA
-- getCWithAB
-- finish getCWithAB
-- finish getDWithB
-- getEWithCD
-- finish getEWithCD
-- ABCBDE
-- ```

main = launchAff do
  aA <- defer $ getA "a"
  bA <- defer $ getB "b"

  let cAA = sequential $ getCWithAB <$> parallel (wait aA) <*> parallel (wait bA) <*> parallel (wait aA)
  let dA = wait bA >>= getDWithB
  cA <- cAA
  let eAA = sequential $ getEWithCD <$> parallel cA <*> parallel dA
  eA <- eAA
  e <- eA
  liftEff $ log e
