{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.CloudEnv where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
--import Text.Julius (RawJS (..))

-- The datatype we wish to receive from the form
--data CloudEnv = CloudEnv
--    { envName        :: Text
--    , envDescription :: Text
--    , envUrl         :: Text
--    }
--  deriving Show

--
getCloudEnvR :: Handler Html
getCloudEnvR = do
    maybeUserId <- maybeAuthId
    (formWidget, formEnctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (cloudEnvForm maybeUserId)
    let submission = Nothing :: Maybe cloudEnvForm
        handlerName = "getCloudEnvR" :: Text
    allCloudEnv <- runDB $ getAllCloudEnv

    defaultLayout $ do
--      let (cloudenvFormId, cloudenvTextareaId, cloudenvListId) = cloudenvIds
--      aDomId <- newIdent
        setTitle "Welcome To Quito!"
--      $(widgetFile "homepage")

--cloudEnvForm :: Form
--cloudEnvForm = renderBootstrap3 BootstrapBasicForm 
--cloudEnvForm :: Html -> MForm Handler (FormResult CloudEnv, Widget)
cloudEnvForm :: UserId -> AForm Handler (CloudEnv)
cloudEnvForm uid = CloudEnv
    <$> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
    <*> areq urlField "Module URL" Nothing
    <*> areq hiddenField "UserId" (Just (Just uid))
--  <$> fileAFormReq "Choose a file"
--  <*> areq textField textSettings Nothing
--  -- Add attributes like the placeholder and CSS classes.
--  where textSettings = FieldSettings
--          { fsLabel = "What's on the file?"
--          , fsTooltip = Nothing
--          , fsId = Nothing
--          , fsName = Nothing
--          , fsAttrs =
--              [ ("class", "form-control")
--              , ("placeholder", "File description")
--              ]
--          }

postCloudEnvR :: Handler Value
postCloudEnvR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    cloudenv <- (requireJsonBody :: Handler CloudEnv)

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    --let cloudenv' = cloudenv { cloudEnvUserId = maybeCurrentUserId }

    --insertedCloudEnv <- runDB $ insertEntity cloudenv'
    insertedCloudEnv <- runDB $ insertEntity cloudenv
    returnJson insertedCloudEnv

-- retrieve all Cloud Env
getAllCloudEnv :: DB [Entity CloudEnv]
getAllCloudEnv = selectList [] [Asc CloudEnvId]
