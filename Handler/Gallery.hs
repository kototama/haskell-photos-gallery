module Handler.Gallery where

import System.Directory
import Data.Text (unpack, pack)
import qualified Data.ByteString as B


import Import

getGalleryR :: Text -> Handler Html
getGalleryR gallery = do
    extra <- getExtra
    let galleriesPath = extraGalleriesPath extra
    let galleryPath = (unpack galleriesPath) ++ "/" ++ (unpack gallery)
    folders <- lift $ getDirectoryContents galleryPath
    let files = filter (\g -> g /= "." && g /= ".." ) folders
    let photos = map pack files

    defaultLayout $ do
      addScriptRemote "http://code.jquery.com/jquery-1.10.2.min.js"
      addStylesheet $ StaticR css_fotorama_css
      addScript $ StaticR js_fotorama_js
      aDomId <- newIdent
      setTitle "Gallery page"
      $(widgetFile "gallerypage")

getPhotoR :: Text -> Text -> Handler ()
getPhotoR gallery photo = do
  extra <- getExtra
  let galleriesPath = extraGalleriesPath extra
  let galleryPath = (unpack galleriesPath) ++ "/" ++ (unpack gallery)
  let photoPath = galleryPath ++ "/" ++ (unpack photo)

  sendFile typePlain photoPath
