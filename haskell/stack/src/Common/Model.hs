module Common.Model (Status, Err, ErrorMessage) where
    
import Data.Text (Text)

type ErrorMessage = Text

data Err a = AnyErr | SpecificErr a deriving (Show, Eq)

data Status = Status {
    code :: !Integer,
    reason :: !String
}