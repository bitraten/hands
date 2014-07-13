{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving,
TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts,
FlexibleInstances #-}
module Model.Post where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.URI (URI)

import Database.Persist
import Database.Persist.TH
import Model.Fields
import Model.User

share [mkPersist sqlSettings, mkMigrate "migratePost"] [persistLowerCase|
Post json
    slug Text
    -- UserDomain
    domain Text
    body Text
    -- Blaze.Html ?
    body_html Text
    -- Tag
    tags [Text]
    published_at UTCTime
    -- Guid
    guid Text
    edited_at UTCTime
    url URI
    -- Guid
    referenced_guid Text Maybe
    UniqueGuid guid
    deriving Show
|]
