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
          runNoLoggingT $ runSqlPool (runMigration (migratePost >> migrateUsers)) pool
          spock 3000 sessCfg (PCConduitPool pool) () $ do
            middleware (staticPolicy $ addBase "public")
            blogHandlers
          where
            sessCfg = authSessCfg (AuthCfg (5 * 60 * 60) ())

blogHandlers :: WebApp
blogHandlers = do      
        get "/" $ requireUser $ Posts.index $ HtmlRequested ()
        get "/posts" $ do
            setStatus movedPermanently301
            setHeader "Location" "/"
        get "/posts.json" $
              requireUser $ Posts.index $ JsonRequested ()
        get "/user.json" $ requireUser json
        get "/{slug:^[a-z]{3}[0-9]{3}(\\.[a-z]+)?$}" $
            do  Just slug <- param "slug"
                requireUser $ Posts.show $ requestedFormat slug
        get "/tag/:tag" $
            do  Just tag <- param "tag"
--                blaze $ Posts.tagged tag
                text tag

requireUser :: (User -> WebAction ()) -> WebAction ()
                        -- Maybe chaining?
requireUser action = do Just host <- header "Host"
                        user <- loadUser host
                        case user of
                            Just u -> action u
                            -- redirect?
                            Nothing -> text "Hostname Error."
