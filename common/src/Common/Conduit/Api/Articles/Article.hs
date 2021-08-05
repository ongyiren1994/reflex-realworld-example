{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
module Common.Conduit.Api.Articles.Article where

import Common.Conduit.Api.Profiles (Profile)
import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Set                    (Set)
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import GHC.Generics                (Generic)

data Article = Article
  { id             :: Integer
  , slug           :: Text
  , title          :: Text
  , description    :: Text
  , body           :: Text
  , tagList        :: Set Text
  , createdAt      :: UTCTime
  , updatedAt      :: UTCTime
  , favorited      :: Bool
  , favoritesCount :: Integer
  , author         :: Profile
  } deriving Show

deriving instance Generic Article
deriving instance ToJSON Article
deriving instance FromJSON Article
