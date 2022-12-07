{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Users.Login.Domain.Models (Login, AuthenticatedUser, SavedUser, ErrGetUserByLogin, GetSavedUserByLogin, CheckPassword, Authorized) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Data.Password.Bcrypt (PasswordHash, Bcrypt)
import Data.Text (Text)
import Servant.Auth ( Auth, Cookie, JWT )
import Common.Model ( Err )

type Password = Text
type Username = Text
type EMail = Text
type HashedPassword = PasswordHash Bcrypt

newtype AuthenticatedUser = AuthenticatedUser Username
  deriving (Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

type GetSavedUserByLogin m = Login -> m (Either (Err ErrGetUserByLogin) SavedUser)
type CheckPassword m = Login -> SavedUser -> m (Either (Err ErrGetUserByLogin) AuthenticatedUser)
type Authorized = Auth '[Cookie, JWT] AuthenticatedUser

data Login = Login {
    email :: !EMail,
    tenantId :: !Int,
    password :: !Password
} deriving (Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

data SavedUser = SavedUser !Username !HashedPassword
  deriving (Show, Generic)

data ErrGetUserByLogin
  = UserDoesNotExist
  | ReplicatedUser
  | WrongPassword
  deriving (Show, Eq)