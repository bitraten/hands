{-# LANGUAGE OverloadedStrings #-}
module Handler.Posts where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Class
import Database.Persist.Sql
import Model.Post
import Text.Blaze.Html
import Util
import qualified View.Posts
import Web.Spock

index :: WebAction ()
index = do
    host <- hostname
    posts <- fmap (map entityVal) $ runSQL $ loadPosts host
    let title = "index"
    myBlaze title $ View.Posts.index posts

show :: T.Text -> WebAction ()
show slug = do
    host <- hostname
    post <- runSQL $ loadPost host slug
    let title = "Post"
    case post of
        Just p  -> myBlaze title . View.Posts.show $ entityVal p
        _       -> show404

-- TODO Multiple Tags
tagged tag = undefined

loadPost :: T.Text -> T.Text -> SqlPersistM (Maybe (Entity Post))
loadPost host slug = selectFirst [PostSlug ==. slug, PostDomain ==. host] []

loadPosts :: T.Text -> SqlPersistM [(Entity Post)]
loadPosts host = selectList [PostDomain ==. host] []
