module Tweet where

import Prelude (bind, pure, (<$>), ($), (<>), (<*>))
import User

type Tweet = { userId :: UserId, msg :: String }

selectUserId :: Tweet -> UserId
selectUserId t = t.userId

selectMessage :: Tweet -> String
selectMessage t = t.msg

showTweet :: Tweet -> String
showTweet t = "userId: " <> t.userId <> ", msg: " <> t.msg

