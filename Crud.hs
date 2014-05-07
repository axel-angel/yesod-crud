{-# LANGUAGE OverloadedStrings #-}
module Crud where

import Prelude
import Model
import Yesod
--import Yesod.Form
import Data.Text (Text)
--import Text.Shakespeare.I18N
import Control.Applicative

readWidget :: (MonadHandler m, m ~ HandlerT site IO,
        YesodPersist site,
        PersistQuery (YesodPersistBackend site (HandlerT site IO)),
        PersistMonadBackend (YesodPersistBackend site (HandlerT site IO)) ~ PersistEntityBackend val,
        PersistEntity val,
        Show val)
    => m (WidgetT site IO (), [Entity val])
readWidget = do
    xs <- runDB $ selectList [] []

    let widget = [whamlet|
        $forall Entity eId e <- xs
            <ul>
                <li>#{show eId}
                <li>#{show e}
    |]

    return (widget, xs)

{- EntityForm -}
class EntityForm a where
    toAForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
        Maybe a -> AForm m a

instance EntityForm Faq where
    toAForm entityMay = Faq
        <$> fieldForm FaqName entityMay
        <*> fieldForm FaqContent entityMay
        <*> fieldForm FaqOrder entityMay

{- Utils -}
fieldForm :: (RenderMessage site FormMessage, MonadHandler m,
           HandlerSite m ~ site, ToField t, EntityFieldsForm a) =>
    EntityField a t -> Maybe a -> AForm m t
fieldForm field entityMay = areq toField
    (fieldSettingsLabel $ fieldLabel field)
    (fieldValue field <$> entityMay)

{- EntityFieldsForm -}
class EntityFieldsForm a where
    fieldValue :: EntityField a t -> a -> t

    -- FIXME: generalize to RenderMessage site msg
    fieldLabel :: EntityField a t -> Text

instance EntityFieldsForm Faq where
    fieldValue FaqId = undefined -- FIXME
    fieldValue FaqName = faqName
    fieldValue FaqContent = faqContent
    fieldValue FaqOrder = faqOrder

    -- to move into an user-definable instance
    fieldLabel FaqId = ("Id" :: Text)
    fieldLabel FaqName = ("Name" :: Text)
    fieldLabel FaqContent = ("Content" :: Text)
    fieldLabel FaqOrder = ("Order" :: Text)

{- ToField -}
class ToField a where
    toField :: Monad m => RenderMessage (HandlerSite m) FormMessage =>
        Field m a

instance ToField Text where
    toField = textField

instance ToField Int where
    toField = intField
