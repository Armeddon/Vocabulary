{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import Yesod

import Data.Text (Text, pack)
import Data.Char (ord, chr)

data Vocabulary = Vocabulary

mkYesod "Vocabulary" [parseRoutes|
/ HomeR  GET
/#Text WordR GET
|]

myLayout :: Widget -> Handler Html
myLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    <meta charset=utf-8>
                    <style>body { font-family: verdana }
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
        |]

instance Yesod Vocabulary where
    defaultLayout = myLayout

addSuffixes :: Int -> Text -> Char -> Widget 
addSuffixes maxBnd prefix char = 
    if maxBnd < ord char
    then
        mempty
    else 
        let str = prefix <> (pack [char]) in do
            toWidgetBody
                [hamlet|
                    <p>
                        <a href=@{WordR str}>#{str}
                |]
            addSuffixes maxBnd prefix (chr $ ord char + 1)

minWordCountBound :: Int
minWordCountBound = ord 'a'

maxWordCountBound :: Int
maxWordCountBound = ord 'z'

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    setTitle "The vocabulary!"
    toWidget [lucius| h1 { color: cyan; }|]
    toWidget
        [hamlet|
            <h1>The vocabulary of all words!
        |]
    addSuffixes maxWordCountBound "" (chr minWordCountBound)

getWordR :: Text -> Handler Html
getWordR word = defaultLayout $ do
    setTitle "The vocabulary!"
    toWidget [lucius| h1 { color: cyan; }|]
    toWidget
        [hamlet|
            <h1>The vocabulary of all words starting with #{word}!
        |]
    addSuffixes maxWordCountBound word (chr minWordCountBound)

main :: IO ()
main = warp 3000 Vocabulary
