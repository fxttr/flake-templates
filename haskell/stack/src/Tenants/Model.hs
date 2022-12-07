 module Tenants.Model (Tenant) where

data Tenant = Tenant {
    name :: String,
    tenantId :: String
}