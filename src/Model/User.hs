{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving,
TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
module Model.User where

import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.URI (URI)

import Database.Persist
import Database.Persist.TH
import Model.Fields

share [mkPersist sqlSettings, mkMigrate "migrateUsers"] [persistLowerCase|
User
    domain Text
    display_name Text
    -- Type? / ByteString?
    password_digest Text Maybe
    -- Type?
    locale Text
    url URI
    UniqueDomain domain
    deriving Show
|]

instance ToJSON (UserGeneric backend) where
    toJSON u = object ["display_name" .= userDisplay_name u,
                       "domain" .= userDomain u,
                       "locale" .= userLocale u,
                       "url" .= userUrl u
                       ]
instance FromJSON (UserGeneric backend) where
    parseJSON (Object u) = User <$>
                                u .: "domain" <*>
                                u .: "display_name" <*>
                                u .:? "password_digest" <*>
                                u .: "locale" <*>
                                u .: "url"
    parseJSON _ = mzero
