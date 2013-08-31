{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Upload where

import Import
import Data.Text (unpack)
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

displayUploadPage :: Handler Html
displayUploadPage = defaultLayout $ do
  setTitle "Upload page"
  $(widgetFile "uploadpage")

copyFileToGallery :: Text -> ([Char] -> IO ()) -> HandlerT App IO Html
copyFileToGallery fName fMove = do
     lift $ fMove ("/tmp/upload/" ++ (unpack fName))
     displayUploadPage

postUploadR :: Handler Html
postUploadR = do
     -- dropzone generates <input type="file" name="file" />

     fileInfo <- runInputPost $ ireq fileField "file"
     let FileInfo fName fContentType _ fMove = fileInfo

     case (unpack fContentType) of
       "image/png" -> copyFileToGallery fName fMove
       "image/jpeg" -> copyFileToGallery fName fMove
       _ -> invalidArgs ["Only PNG, JPEG and Zip files are accepted"]

uploadForm :: Form (FileInfo, Text)
uploadForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
