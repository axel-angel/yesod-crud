{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Crud where

import Prelude
import Model
import Yesod
--import Yesod.Form
import Data.Text (Text)
--import Text.Shakespeare.I18N
import Control.Applicative

import Language.Haskell.TH
import Data.Char (toUpper)
import Control.Monad (when)


class EntityForm a where
    toAForm :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
        Maybe a -> AForm m a

fieldForm :: (RenderMessage site FormMessage, MonadHandler m,
           HandlerSite m ~ site, ToField t,
           EntityFieldsForm a, EntityFieldLabeled a) =>
    EntityField a t -> Maybe a -> AForm m t
fieldForm field entityMay = areq toField
    (fieldSettingsLabel $ fieldLabel field)
    (fieldValue field <$> entityMay)


class EntityFieldsForm a where
    fieldValue :: EntityField a t -> a -> t


class EntityFieldLabeled a where
    -- FIXME: generalize to RenderMessage site msg
    fieldLabel :: EntityField a t -> Text

-- FIXME: user should generate it with TH or easily overriden
instance EntityFieldLabeled Faq where
    fieldLabel FaqId = "Id" :: Text
    fieldLabel FaqName = "Name" :: Text
    fieldLabel FaqContent = "Content" :: Text
    fieldLabel FaqOrder = "Order" :: Text

class ToField a where
    toField :: Monad m => RenderMessage (HandlerSite m) FormMessage =>
        Field m a

instance ToField Text where
    toField = textField

instance ToField Int where
    toField = intField


derivePersistForm :: String -> Q [Dec]
derivePersistForm s = do
    sInfo <- reify $ mkName s
    let cons = case sInfo of
         TyConI (DataD _ _ _ (c:_) _) -> case c of
                  RecC _ names -> map (\(n, _, _) -> n) names
                  _ -> []
         _ -> []

    when (length cons == 0) $ reportError $ "Expected data constructor: " ++ s

    let iFF typ = InstanceD [] (ConT ''EntityFieldsForm `AppT` typ)
    return
        [ iFF (ConT $ mkName s)
            [ FunD 'fieldValue $
                (flip map) cons $ \accName ->
                    let field [] = []
                        field (x:xs) = toUpper x : xs
                        fieName = mkName . field $ nameBase accName
                    in Clause [ConP fieName []] (NormalB $ VarE accName) []
            ]
        ]
