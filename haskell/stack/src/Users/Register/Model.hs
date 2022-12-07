module Users.Register.Model (Register) where

data Register = Register {
    firstName :: String,
    lastName :: String,
    email :: String,
    tenantId :: Int,
    password :: String
}