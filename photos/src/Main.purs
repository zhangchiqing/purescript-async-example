module Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff.Future (defer', wait)
import Control.Monad.Aff (later', Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Parallel (parTraverse)
import Data.String (joinWith)
import Data.Traversable (traverse)

type AffConsole e a = Aff (console :: CONSOLE | e) a

newtype Token = Token String
newtype Secret = Secret String
newtype User = User String
newtype Photo = Photo String

instance showUser :: Show User where
  show (User u) = u

instance showPhoto :: Show Photo where
  show (Photo p) = p

getToken :: forall e. AffConsole e Token
getToken = do
  liftEff $ log "getToken"
  t <- later' 1000 $ pure $ Token "Token"
  pure t

getSecret :: forall e. AffConsole e Secret
getSecret = do
  liftEff $ log "getSecret"
  s <- later' 1000 $ pure $ Secret "Secret"
  pure s

getUsers :: forall e. Token -> Secret -> AffConsole e (Array User)
getUsers t s = do
  liftEff $ log "getUsers"
  users <- later' 1000 $ pure [User "A", User "B", User "C"]
  pure users

getPhotoByTokenAndUser :: forall e. Token -> User -> AffConsole e Photo
getPhotoByTokenAndUser t u = do
  liftEff $ log $ "getPhotoByTokenAndUser " <> show u
  users <- later' 1000 $ pure $ photo u
  pure users
  where
    photo :: User -> Photo
    photo (User "A") = Photo ":)"
    photo (User "B") = Photo ":D"
    photo (User "C") = Photo ":/"
    photo _   = Photo ":-|"

sendEmailWithPhotos :: forall e. Array Photo -> AffConsole e Unit
sendEmailWithPhotos ps = do
  liftEff $ log "sendEmailWithPhotos"
  later' 1000 $ pure unit

getPhotosByTokenAndUsers :: forall e. Token -> Array User -> AffConsole e (Array Photo)
getPhotosByTokenAndUsers t = parTraverse $ getPhotoByTokenAndUser t

getPhotosByTokenAndUsersSequentially :: forall e. Token -> Array User -> AffConsole e (Array Photo)
getPhotosByTokenAndUsersSequentially t = traverse $ getPhotoByTokenAndUser t

showPhotos :: Array Photo -> String
showPhotos ps = joinWith " , " $ show <$> ps

getPhotos :: forall e. AffConsole (avar :: AVAR | e) (Array Photo)
getPhotos = do
  tokenP <- defer' $ getToken
  secretP <- defer' $ getSecret
  usersP <- defer' $ join $ getUsers <$> wait tokenP <*> wait secretP
  photosP <- defer' $ join $ getPhotosByTokenAndUsers <$> wait tokenP <*> wait usersP
  sentP <- defer' $ join $ sendEmailWithPhotos <$> wait photosP
  wait photosP <* wait sentP

main :: forall eff. AffConsole (avar :: AVAR | eff) Unit
main = do
  photos <- getPhotos
  liftEff $ log $ showPhotos photos
