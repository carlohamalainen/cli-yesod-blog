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

    -- return $ RepPlain $ toContent $ show entry
    return $ RepPlain "FIXME"

-- foo (Entity cid (Comment entryId commentDate commentAuthor commentText visible))) = c
-- FIXME This sort of pattern must be comment, so what's the right way to do it?
-- Something like fromEntity :: Entity k e -> (k, e) ?
foo :: Entity Comment -> (Key Comment, Comment)
foo (Entity cid c@(Comment _ _ _ _ _)) = (cid, c)

getEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
getEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    case e of (Just (Entity eid (Entry title' mashedTitle' year' month' day' content' visible'))) -> do comments <- runDB $ selectList [CommentEntry ==. eid] []
                                                                                                        let blerps = map (show . foo) comments
                                                                                                        -- return $ RepHtml $ toContent (show blerps) -- content'
                                                                                                        -- return $ RepHtml $ toContent content'
                                                                                                        defaultLayout $ do
                                                                                                            setTitleI MsgWelcomeHomepage
                                                                                                            [whamlet|
<h1>#{title'}
<article>#{content'}
|]
              _                                                                            -> notFound

