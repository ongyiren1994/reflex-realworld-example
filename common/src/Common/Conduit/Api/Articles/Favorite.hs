{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.Articles.Favorite where

import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..))
import           GHC.Generics                           (Generic)
import Data.Text

data Favorite = Favorite
  { slug :: Text
  , bool :: Bool
  } deriving Show

deriving instance Generic Favorite
deriving instance ToJSON Favorite
deriving instance FromJSON Favorite