{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Concurrent (ThreadId)
import Control.Exception (throwIO)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger (monadLoggerLog), MonadLoggerIO (askLoggerIO))
import Control.Monad.Metrics (Metrics, MonadMetrics (getMetrics))
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, createPostgresqlPool)
import Katip (Katip (getLogEnv, localLogEnv), KatipT (KatipT), LogEnv, Severity (InfoS), logMsg, runKatipT)
import Katip.Core (Katip)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant (ServerError)
import Servant.Auth.Server (JWTSettings)
import System.Environment (lookupEnv)
import Logger (adapt, defaultLogEnv)

data Environment = Development | Staging | Production deriving (Eq, Show, Read)

data Config = Config
  { configPool :: !ConnectionPool,
    configEnv :: !Environment,
    configMetrics :: !Metrics,
    configEkgServer :: !ThreadId,
    configJWTSettings :: !JWTSettings,
    configLogEnv :: !LogEnv,
    configPort :: !Port
  }

newtype AppT m a = AppT
  { runApp :: ReaderT Config (ExceptT ServerError m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Config,
      MonadError ServerError,
      MonadIO
    )

type App = AppT IO

instance Monad m => MonadMetrics (AppT m) where
  getMetrics = asks configMetrics

instance MonadIO m => Katip (AppT m) where
  getLogEnv = asks configLogEnv
  localLogEnv = error "not implemented"

instance MonadIO m => MonadLogger (AppT m) where
  monadLoggerLog = adapt logMsg

instance MonadIO m => MonadLogger (KatipT m) where
  monadLoggerLog = adapt logMsg

instance Katip IO where
  getLogEnv = defaultLogEnv
  localLogEnv _ io = io

instance MonadIO m => MonadLoggerIO (KatipT m) where
  askLoggerIO = KatipT $ return $ adapt logMsg

setLogger :: Environment -> Middleware
setLogger Staging = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
  logMsg "web" InfoS "todo: received some request"
  liftIO $ app req respond

makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Staging env = runKatipT env $ createPostgresqlPool (connStr "") (envPool Staging)
makePool Development env = runKatipT env $ createPostgresqlPool (connStr "") (envPool Development)
makePool Production env = do
  pool <- runMaybeT $ do
    let keys =
          [ "host=",
            "port=",
            "user=",
            "password=",
            "dbname="
          ]
        envs =
          [ "PGHOST",
            "PGPORT",
            "PGUSER",
            "PGPASS",
            "PGDATABASE"
          ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
    lift $ runKatipT env $ createPostgresqlPool prodStr (envPool Production)
  case pool of
    Nothing -> throwIO (userError "Database Configuration not present in environment.")
    Just a -> return a

envPool :: Environment -> Int
envPool Staging = 1
envPool Development = 1
envPool Production = 8

connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=testdb" <> sfx <> " user=admin password=pass port=5432"