{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Data.Text.IO as T
import Yesod.Core.Types

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    let Just (fileInfo, text) = submission
    let FileInfo fName fContentType fSourceRaw fMove = fileInfo

    defaultLayout $ do
      aDomId <- newIdent
      lift $ putStr "Submission ="
      lift $ T.putStr fName
      lift $ fMove "/tmp/thamer"
      -- lift $ putStr (show fileInfo)
      setTitle "Welcome To Yesod!!"
      $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

-- doStuff :: Num a => a -> a -> a
-- doStuff a b = a + b

-- doStuff2 = let x = 1
--                y = 2
--            in doStuff x y
