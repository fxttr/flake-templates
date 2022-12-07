module Users.Persistent () where
    
import Users.Login.Domain.Models

data UserRepository m = UserRepository
  { getUserByLogin :: GetSavedUserByLogin m,
    createUserBySignup :: CreateUserBySignup m
  }