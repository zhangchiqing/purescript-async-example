module User where

import Prelude (bind, pure, (<$>), ($), (<>), (<*>))

type UserId = String
type User = { id :: UserId , name :: String , photo :: String }

makeUser :: UserId -> String -> String -> User
makeUser id name photo = { id: id, name: name, photo: photo }

showUser :: User -> String
showUser u = "(user :| id: " <> u.id <> " name: " <> u.name <> " photo: " <> u.photo <> ")"
