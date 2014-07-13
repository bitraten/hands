{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Handler.Posts as Posts
import Model.Post
import Model.User
import Control.Monad.Logger
import Data.Monoid (mempty)
import Database.Persist.Sqlite hiding (get) 
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static
import Util
import Web.Spock
import Web.Spock.Auth


main :: IO ()
main = do pool <- createSqlitePool "hands.db" 5
          runNoLoggingT $ runSqlPool (runMigration migratePost) pool
          spock 3000 sessCfg (PCConduitPool pool) () $ do
            middleware (staticPolicy $ addBase "public")
            blogHandlers
          where
            sessCfg = authSessCfg (AuthCfg (5 * 60 * 60) ())

blogHandlers :: WebApp
blogHandlers = do      
        get "/" $ Posts.index $ HtmlRequested ()
        get "/posts" $ do
            setStatus movedPermanently301
            setHeader "Location" "/"
        get "/posts.json" $
               Posts.index $ JsonRequested ()
        get "/user.json" $ do
            host <- hostname
            user <- runSQL $ selectFirst [UserDomain ==. host] []
            case user of
                Just u  -> json $ entityVal u
                _       -> text host
        get "/{slug:^[a-z]{3}[0-9]{3}(\\.[a-z]+)?$}" $
            do  Just slug <- param "slug"
                hostname >>= ($ requestedFormat slug) . Posts.show
        get "/tag/:tag" $
            do  Just tag <- param "tag"
--                blaze $ Posts.tagged tag
                text tag
