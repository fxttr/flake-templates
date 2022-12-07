{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tenants.Types (TenantAPI) where

import Tenants.Model (Tenant)
import Servant ((:>), Get, JSON)

type TenantAPI = "tenants" :> Get '[JSON] [Tenant]