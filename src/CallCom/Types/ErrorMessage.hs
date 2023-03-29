{-# LANGUAGE DeriveGeneric #-}


module CallCom.Types.ErrorMessage (ErrorMessage (ErrorMessage)) where


import Data.Text (Text)
import GHC.Generics (Generic)


newtype ErrorMessage = ErrorMessage { unErrorMessage :: Text }
  deriving Generic
