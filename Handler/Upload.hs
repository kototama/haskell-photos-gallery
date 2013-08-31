{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Upload where

import Data.Text (unpack, isInfixOf)
import qualified Data.Text.IO as T
import System.Directory (createDirectory, doesDirectoryExist)
import Yesod.Core.Types
import Control.Monad (when)

import Import


getUploadR :: Handler Html
getUploadR = do
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

copyFileToGallery :: Text -> Text -> ([Char] -> IO ()) -> HandlerT App IO Html
copyFileToGallery gallery fName fMove = do
    extra <- getExtra
    let galleriesPath = extraGalleriesPath extra
    let galleryPath = (unpack galleriesPath) ++ "/" ++ (unpack gallery)
    galleryExists <- lift $ doesDirectoryExist galleryPath

    when (not galleryExists) $
       lift $ createDirectory galleryPath

    let path = galleryPath
               ++ "/"
               ++ (unpack fName)
    lift $ fMove $ path
    displayUploadPage

postUploadR :: Handler Html
postUploadR = do
     -- dropzone generates <input type="file" name="file" />
     (fileInfo, gallery) <- runInputPost $ (,)
                            <$> ireq fileField "file"
                            <*> ireq textField "gallery"

     let FileInfo fName fContentType _ fMove = fileInfo
     let invalidGalleryName = (isInfixOf ".." gallery) ||
                              (isInfixOf " " gallery) ||
                              (isInfixOf "/" gallery)

     when invalidGalleryName $
       invalidArgs ["Invalid gallery name"]

     case (unpack fContentType) of
       "image/png" -> copyFileToGallery gallery fName fMove
       "image/jpeg" -> copyFileToGallery gallery fName fMove
       _ -> invalidArgs ["Only PNG, JPEG and Zip files are accepted"]
