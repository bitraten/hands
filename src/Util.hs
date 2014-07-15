{-# LANGUAGE OverloadedStrings #-}                                         
module Util where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import Database.Persist.Sqlite hiding (get)
import Model.User
import Network.HTTP.Types.Status
import System.FilePath.Posix (splitExtension)
import Text.Blaze.Html5 (Html)
import qualified View.Application
import qualified View.Error
import Web.Spock
import Web.Spock.Auth

type WebApp = SpockM Connection (VisitorSession () ()) () ()
type WebAction a = SpockAction Connection (VisitorSession () ()) () a

data RequestedFormat a =  HtmlRequested a | JsonRequested a | MdRequested a
                        | OtherRequested a

runSQL :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> WebAction a
runSQL action =
    runQuery $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn

loadUser :: T.Text -> WebAction (Maybe User)
loadUser host = fmap (fmap entityVal) $ runSQL
                    $ selectFirst [UserDomain ==. host] []

requestedFormat :: T.Text -> RequestedFormat T.Text
requestedFormat text = case (splitExtension $ T.unpack text) of
                    (name, ".json")  -> JsonRequested $ T.pack name
                    (name, ".md")    -> MdRequested $ T.pack name
                    (name, "")       -> HtmlRequested $ T.pack name
                    _                -> OtherRequested text

myBlaze :: User -> Html -> Html -> WebAction ()
myBlaze = ((blaze .) .) . View.Application.html

show404 :: User -> WebAction ()
show404 user = do
    setStatus notFound404
    myBlaze user "Error" $ View.Error.show "Not Found."
