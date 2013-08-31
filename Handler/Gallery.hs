module Handler.Gallery where

import System.Directory
import Data.Text (unpack)
import Import


getGalleryR gallery = do
    extra <- getExtra
    let galleriesPath = extraGalleriesPath extra
    folders <- lift $ getDirectoryContents (unpack galleriesPath)
    let galleries = filter (\g -> g /= "." && g /= ".." ) folders

    defaultLayout $ do
      aDomId <- newIdent
      setTitle "Gallery page"
      $(widgetFile "gallerypage")
