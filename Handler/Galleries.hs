{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Galleries where

import System.Directory
import Data.Text (unpack, pack)
import Import

getGalleriesR :: Handler Html
getGalleriesR = do
    extra <- getExtra
    let galleriesPath = extraGalleriesPath extra
    folders <- lift $ getDirectoryContents (unpack galleriesPath)
    let galleriesPaths = filter (\g -> g /= "." && g /= ".." ) folders
    let galleries = map pack galleriesPaths

    defaultLayout $ do
      aDomId <- newIdent
      setTitle "Galleries page"
      $(widgetFile "galleriespage")
