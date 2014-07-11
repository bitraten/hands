{-# LANGUAGE OverloadedStrings #-}                                         
module Util where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import Database.Persist.Sqlite hiding (get)
import Network.HTTP.Types.Status
import Text.Blaze.Html5 (Html)
import qualified View.Application
import qualified View.Error
import Web.Spock
import Web.Spock.Auth

type WebApp = SpockM Connection (VisitorSession () ()) () ()
type WebAction a = SpockAction Connection (VisitorSession () ()) () a

runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn

hostname :: WebAction T.Text
hostname = do
    host <- header "Host"
    case (host) of
        Just h -> return h
        Nothing -> return T.empty

myBlaze :: Html -> Html -> WebAction ()
myBlaze t c  = blaze $ View.Application.html t c

show404 :: WebAction ()
show404 = do
    setStatus notFound404
    myBlaze "Error" $ View.Error.show "Not Found."
