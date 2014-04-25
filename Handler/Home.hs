{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Crud

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
handleHomeR :: Handler Html
handleHomeR = do
    {-
    (fForm, fIdMay) <- addWidget
    let _ = fIdMay :: Maybe FaqId
    -}

    faqs <- runDB $ selectList ([]::[Filter Faq]) []
    (rWid, xs) <- readWidget
    let _ = rWid :: WidgetT App Handler ()
    let _ = xs :: [Entity Faq]

    defaultLayout $ do
        $(widgetFile "homepage")
