{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Users.Types (UserAPI) where

import Servant (Get, JSON, (:>), type (:<|>))
import Users.Login.Types (UserLoginAPI)
import Users.Register.Types (UserRegisterAPI)
import Users.Domain.Models ( User )

type UserBaseAPI = "users" :> Get '[JSON] [User]

type UserAPI = UserBaseAPI :<|> UserLoginAPI :<|> UserRegisterAPI