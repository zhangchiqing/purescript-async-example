module Main where

import Prelude (bind, pure, (<$>), ($), (<>), (<*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff
import Data.String (joinWith)
import Data.Tuple (Tuple(..), snd, fst)
import Data.Traversable (traverseDefault)
import Control.Parallel (sequential, parallel)
import Service
import Tweet
import User

getUser :: forall e. UserId -> Aff e User
getUser userId = sequential $ makeUser <$> pure userId <*> parallel usernameA <*> parallel photoA
  where usernameA = getUserName userId
        photoA = getUserPhoto userId

getTweet :: forall e. Tweet -> Aff e (Tuple String User)
getTweet tweet = sequential $ Tuple <$> pure message <*> parallel userA
  where userId = selectUserId tweet
        message = selectMessage tweet
        userA = getUser userId

getTweetByUserId :: forall e. UserId -> Aff e (Array (Tuple String User))
getTweetByUserId userId = do
  tweets <- getTweets userId
  traverseDefault getTweet tweets

showTupledTweet :: Tuple String User -> String
showTupledTweet t = fst t <> " " <> showUser (snd t)

type MainAff e = Eff (err :: EXCEPTION, console :: CONSOLE | e) (Canceler (console :: CONSOLE | e))

main :: forall e. MainAff e
main = launchAff do
  tweets <- getTweetByUserId theId
  liftEff $ log $ joinWith " , " $ showTupledTweet <$> tweets
  where theId = "1"
