{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Upload where

import Data.Text (unpack)
import Yesod.Core.Types

import Import



getUploadR :: Handler Html
getUploadR = do
    (formWidget, formEnctype) <- generateFormPost uploadForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getUploadR" :: Text
    defaultLayout $ do
      addStylesheet $ StaticR css_dropzone_css
      addScript $ StaticR js_dropzone_js
      aDomId <- newIdent
      setTitle "Upload page"
      $(widgetFile "uploadpage")

displayUploadPage :: Handler Html
displayUploadPage = defaultLayout $ do
  setTitle "Upload page"
  $(widgetFile "uploadpage")

copyFileToGallery :: Text -> ([Char] -> IO ()) -> HandlerT App IO Html
copyFileToGallery fName fMove = do
    extra <- getExtra
    let galleryPath = extraGalleryPath extra
    let path = (unpack galleryPath) ++ "/" ++ (unpack fName)
    lift $ fMove $ path
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
