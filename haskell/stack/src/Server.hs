{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server (mkApp) where

import Config
    ( AppT(runApp), Config(configPool, configJWTSettings) )
import Control.Monad.Reader (MonadIO, runReaderT)
import Database.Persist.Postgresql (ConnectionPool)
import Network.Wai ( Application )
import Servant
    ( serveWithContext,
      type (:<|>)((:<|>)),
      IsSecure(NotSecure),
      HasServer(hoistServerWithContext, ServerT),
      Server,
      Context((:.), EmptyContext),
      Handler(Handler), Proxy (Proxy), FromHttpApiData (..) )
import Servant.Auth.Server
    ( defaultCookieSettings,
      CookieSettings(cookieIsSecure, cookieSameSite, cookieXsrfSetting),
      JWTSettings,
      SameSite(SameSiteStrict) )
import Users.Types ( UserAPI )
import Tenants.Types (TenantAPI)

instance FromHttpApiData SortBy where parseUrlPiece = parseBoundedTextData

type API = UserAPI :<|> TenantAPI

api :: Proxy API
api = Proxy

context :: Proxy '[CookieSettings, JWTSettings]
context = Proxy

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

appToServer :: Config -> Server API
appToServer cfg = hoistServerWithContext api context (convertApp cfg) $ server (configPool cfg) (configJWTSettings cfg)

-- Creates server definition for API, which is the one that adds the logic to the typed specification
server :: (MonadIO m) => ConnectionPool -> JWTSettings -> ServerT API (AppT m)
server pool jwtSettings = do
  UserAPI :<|> authenticationServer defaultCookieSettings jwtSettings pool

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings {cookieIsSecure = NotSecure, cookieSameSite = SameSiteStrict, cookieXsrfSetting = Nothing}

-- Creates IO application from API definition and serrver
mkApp :: Config -> Application
mkApp config = do
  let ctx = cookieSettings :. cookieSettings :. configJWTSettings config :. EmptyContext
  serveWithContext api ctx (appToServer config)