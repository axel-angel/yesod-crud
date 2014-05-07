{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module Crud where

import Prelude
import Model
import Yesod
--import Yesod.Form
import Data.Text (Text)
--import Text.Shakespeare.I18N
import Control.Applicative
import Text.Blaze (ToMarkup)

viewWidget :: (MonadHandler m, m ~ HandlerT site IO,
        YesodPersist site,
        PersistQuery (YesodPersistBackend site (HandlerT site IO)),
        PersistMonadBackend (YesodPersistBackend site (HandlerT site IO)) ~ PersistEntityBackend val,
        PersistEntity val,
        Show val)
    => m (WidgetT site IO (), [Entity val])
viewWidget = do
    xs <- runDB $ selectList [] []
    let fields = tableFields :: [TableFieldHtml Faq]

    let widget = [whamlet|
        <table>
            <thead>
                $forall TableFieldHtml f <- fields
                    <th>#{fieldLabel f}
            <tbody>
                $forall Entity eId e <- xs
                    <tr>
                        <td>#{show eId}
                        <td>#{show e}
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

{- DataWrapper for tableFields -}
data TableFieldHtml a = forall b c. (c ~ EntityField a b, ToMarkup b) => TableFieldHtml c

{- EntityFieldsForm -}
class EntityFieldsForm a where
    fieldValue :: EntityField a t -> a -> t

    -- FIXME: generalize to RenderMessage site msg
    fieldLabel :: EntityField a t -> Text

    tableFields :: [TableFieldHtml a]

instance EntityFieldsForm Faq where
    fieldValue FaqId = undefined -- FIXME
    fieldValue FaqName = faqName
    fieldValue FaqContent = faqContent
    fieldValue FaqOrder = faqOrder

    -- to move into an user-definable instance
    fieldLabel FaqId = "Id" :: Text
    fieldLabel FaqName = "Name" :: Text
    fieldLabel FaqContent = "Content" :: Text
    fieldLabel FaqOrder = "Order" :: Text

    tableFields = [TableFieldHtml FaqId, TableFieldHtml FaqName, TableFieldHtml FaqContent, TableFieldHtml FaqOrder]

{- ToField -}
class ToField a where
    toField :: Monad m => RenderMessage (HandlerSite m) FormMessage =>
        Field m a

instance ToField Text where
    toField = textField

instance ToField Int where
    toField = intField
