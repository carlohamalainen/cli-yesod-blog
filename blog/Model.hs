{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Time (UTCTime, getCurrentTime)
import Text.Blaze
import Text.Blaze.Renderer.Utf8

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance Show Entry where
    show (Entry title mashedTitle year month day content visible) = "title: " ++ (show title) ++ " " ++ "mashed title: " ++ (show mashedTitle) ++ " " ++ "year: " ++ (show year) ++ " " ++ "month: " ++ (show month) ++ " " ++ "day: " ++ (show day) ++ " " ++ "content: " ++ (show $ renderHtml content) ++ " " ++ "visible: " ++ (show visible)
