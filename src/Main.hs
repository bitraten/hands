{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Handler.Posts as Posts
import Model.Post
import Control.Monad.Logger
import Data.Monoid (mempty)
import Database.Persist.Sqlite hiding (get) 
import Network.Wai.Middleware.Static
import Util
import Web.Spock
import Web.Spock.Auth


main :: IO ()
main = do pool <- createSqlitePool "hands.db" 5
          runNoLoggingT $ runSqlPool (runMigration migratePost) pool
          spock 3000 sessCfg (PCConduitPool pool) () $ do
            middleware (staticPolicy mempty)
            blogHandlers
--            apiHandlers
          where
            sessCfg = authSessCfg (AuthCfg (5 * 60 * 60) ())

blogHandlers :: WebApp
blogHandlers = do      
        get "/" Posts.index
        get "/:slug" $
            do  Just slug <- param "slug"
                Posts.show slug
        get "/tag/:tag" $
            do  Just tag <- param "tag"
--                blaze $ Posts.tagged tag
                text tag

apiHandlers :: WebApp
apiHandlers = undefined
