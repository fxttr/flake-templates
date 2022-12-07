{-# LANGUAGE OverloadedStrings #-}

module Users.Login.API() where

import Servant
import Control.Monad.Reader
import Users.Login.Types
import Users.Login.Domain.Models
import Servant.Auth.Server
import Data.Password.Bcrypt

_err401 :: ServerError
_err401 = err401 { errBody = "Error logging user in, please verify credentials" }

getAuthenticatedCookies :: (MonadIO m) => UserLoginAPIAction AuthenticatedUser m
getAuthenticatedCookies cookieSettings jwtSettings user = do
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
  
  case mApplyCookies of
    Nothing           -> throwError $ _err401
    Just applyCookies -> return $ applyCookies NoContent

checkLoginPassword :: (MonadIO m) => CheckPassword m
checkLoginPassword login (SavedUser username password) = do
  let pass = mkPassword $ loginPassword login
  let hashedPass = password
  let checkedPassword = checkPassword pass hashedPass
  case checkedPassword of
    PasswordCheckFail -> return $ Left (SpecificErr WrongPassword)
    PasswordCheckSuccess -> return $ Right $ AuthenticatedUser username 