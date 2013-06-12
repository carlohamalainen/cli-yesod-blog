{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time (UTCTime, getCurrentTime, utctDay, toGregorian)
import Model
import Control.Monad (when)

import Database.Persist.Store (PersistValue(PersistInt64))

import qualified Data.Text as DT

getHomeR :: Handler RepHtml
getHomeR = do
    entries <- runDB $ selectList [EntryVisible ==. True] [ Desc EntryPostedYear
                                                          , Desc EntryPostedMonth
                                                          , Desc EntryPostedDay
                                                          ]

    defaultLayout $ do
        setTitleI MsgWelcomeHomepage
        [whamlet|
$if null entries
    <p>_{MsgNoEntries}
$else
    <ul>
        $forall Entity eid (Entry title mashedTitle year month day content visible) <- entries
            <li> <a href=@{EntryLongR year month day mashedTitle}>#{year}-#{month}-#{day} #{title}</a>
<p>Boo</p>
|]

getEntryR :: EntryId -> Handler RepPlain
getEntryR eid = do
    entry <- runDB $ get eid

    return $ RepPlain $ toContent $ show entry

getEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
getEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    case e of (Just (Entity eid (Entry title' mashedTitle' year' month' day' content' visible'))) -> return $ RepHtml $ toContent content'
              _                                                                            -> notFound
