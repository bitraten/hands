{-# LANGUAGE OverloadedStrings #-}
module View.Error where

import Data.Text
import Text.Blaze (toValue)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

show :: Text -> Html
show error = do
    p $ toHtml error
