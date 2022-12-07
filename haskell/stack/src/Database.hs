{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database(share, doMigrations, runDb) where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.Postgresql
    ( runMigration,
      runSqlPool,
      BackendKey(SqlBackendKey),
      SqlPersistT )
import Database.Persist.TH
    ( mkMigrate, mkPersist, persistManyFileWith, share, sqlSettings )
import Database.Persist.Quasi ( lowerCaseSettings )
import Data.Text (Text)
import Say ( say )

import Data.Password.Bcrypt (Bcrypt, PasswordHash(..))
import Data.Password.Instances ()

import Config (Config, configPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistManyFileWith lowerCaseSettings [
      "config/models/User.persistentmodels",
      "config/models/Tenant.persistentmodels",
      "config/models/Contact.persistentmodels",
      "config/models/Customer.persistentmodels",
      "config/models/Milestone.persistentmodels",
      "config/models/Project.persistentmodels"
    ]
  )

doMigrations :: SqlPersistT IO ()
doMigrations = do
  liftIO $ say "Database.doMigrations"
  runMigration migrateAll
  liftIO $ say "Migrations ran successfuly"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool