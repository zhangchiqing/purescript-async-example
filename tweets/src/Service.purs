module Service (
  getTweets
, getUserName
, getUserPhoto
) where

import Prelude (bind, pure, (<$>), ($), (<>), (<*>))
import Control.Monad.Aff
import Tweet
import User

getTweets :: forall e. UserId -> Aff e (Array Tweet)
getTweets userId = pure [
  { userId: "1", msg: "Hello world" },
  { userId: "2", msg: "PureScript Aff is awesome" },
  { userId: "2", msg: "PureScript is a strongly typed programming language" },
  { userId: "3", msg: "PureScript Compiler is written in Haskell." }
]

getUserName :: forall e. UserId -> Aff e String
getUserName "1" = pure "Leo"
getUserName "2" = pure "Mr. Pure"
getUserName _ = pure "Anonymous"

getUserPhoto :: forall e. UserId -> Aff e String
getUserPhoto "1" = pure ":)"
getUserPhoto "2" = pure ":("
getUserPhoto _ = pure ":|"

