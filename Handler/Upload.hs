{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Upload where

import Import
import qualified Data.Text.IO as T
import Yesod.Core.Types

getUploadR :: Handler Html
getUploadR = do
    (formWidget, formEnctype) <- generateFormPost uploadForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getUploadR" :: Text
    defaultLayout $ do
      addStylesheet $ StaticR css_dropzone_css
      addScript $ StaticR js_dropzone_js
      aDomId <- newIdent
      setTitle "Welcome To Yesod!"
      $(widgetFile "uploadpage")

postUploadR :: Handler Html
postUploadR = do
     -- dropzone generates <input type="file" name="file" />

     fileInfo <- runInputPost $ ireq fileField "file"
     let FileInfo fName fContentType fSourceRaw fMove = fileInfo

     defaultLayout $ do
       addScript $ StaticR js_dropzone_js
       aDomId <- newIdent
       lift $ putStr "Submission ="
       lift $ T.putStr fName
       lift $ fMove "/tmp/thamer"

       setTitle "Welcome To Yesod!"
       $(widgetFile "uploadpage")

uploadForm :: Form (FileInfo, Text)
uploadForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
