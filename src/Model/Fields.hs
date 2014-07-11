{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving,
TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}
module Model.Fields where

import Control.Applicative (pure)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Text (pack, unpack)
import Network.URI (URI, uriToString, parseURI)

import Database.Persist
import Database.Persist.Sql (PersistFieldSql, sqlType)

uriToText uri = pack $ uriToString id uri ""

instance PersistField URI where
    toPersistValue = PersistText . uriToText 
    fromPersistValue (PersistText v) = case (parseURI $ unpack v) of
                            Just uri -> Right uri
                            Nothing  -> Left "Not a valid URI"

instance PersistFieldSql URI where
    sqlType _ = SqlString

instance ToJSON URI where
    toJSON = String . uriToText

instance FromJSON URI where
    parseJSON (String u) = pure $ fromJust $ parseURI $ unpack u
    parseJSON _ = fail "could not parse"
