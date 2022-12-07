{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Users.Register.Types (UserRegisterAPI) where
    
import Servant (ReqBody, JSON, (:>), Post)
import Users.Register.Model (Register)
import Common.Model (Status)

type UserRegisterAPI = "users" :> "register" :> ReqBody '[JSON] Register :> Post '[JSON] Status
