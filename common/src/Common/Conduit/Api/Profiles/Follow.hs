{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common.Conduit.Api.Profiles.Follow where

import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..))
import           GHC.Generics                           (Generic)
import Data.Text

data Follow = Follow
  { username :: Text
  , bool :: Bool
  } deriving Show

deriving instance Generic Follow
deriving instance ToJSON Follow
deriving instance FromJSON Follow