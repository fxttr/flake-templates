{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.Login.Types (UserLoginAPI, UserLoginAPIAction) where

import Servant (Post, JSON, (:>), ReqBody, StdMethod (POST), HasServer (ServerT), ServerError (errBody), err401, NoContent)
import Users.Login.Domain.Models (Login)
import Servant.API (Headers, Header, Verb)
import Servant.Auth.Server (SetCookie, CookieSettings, JWTSettings)
import Control.Monad.Reader (MonadIO, liftIO)
import Config (AppT)

type AcceptHeader returnContent =
    Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
         returnContent

type UserLoginAPI = "users" :> "login" :> ReqBody '[JSON] Login :> Verb 'POST 204 '[JSON] ()


type RegistrationAppT m = AppT m (AcceptHeader NoContent)

type UserLoginAPIAction actionPayload m = CookieSettings -> JWTSettings -> actionPayload -> RegistrationAppT m