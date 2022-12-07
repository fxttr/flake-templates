module Users.Domain.Models (User) where

import Tenants.Model(Tenant)

data User = User {
    firstName :: String,
    lastName :: String,
    email :: String,
    tenant :: Tenant
}