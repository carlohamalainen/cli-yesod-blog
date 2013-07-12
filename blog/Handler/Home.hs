{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time (UTCTime, getCurrentTime, utctDay, toGregorian)
import Model
import Control.Monad (when)

import Database.Persist.Store (PersistValue(PersistInt64))

import qualified Data.Text as DT

import Text.Printf

commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing
    <*> pure False -- <* recaptchaAForm

getHomeR :: Handler RepHtml
getHomeR = do
    entries <- runDB $ selectList [EntryVisible ==. True] [ Desc EntryPostedYear
                                                          , Desc EntryPostedMonth
                                                          , Desc EntryPostedDay
                                                          ]

    let entriesAsTuples = map deconstructEntryEntity entries

    defaultLayout $ do
        setTitleI MsgWelcomeHomepage
        [whamlet|
$if null entries
    <p>_{MsgNoEntries}
$else
    <ul>
        $forall (title, mashedTitle, year, month, mm, day, dd, content, visible) <- entriesAsTuples
            <li> <a href=@{EntryLongR year month day mashedTitle}>#{year}-#{mm}-#{dd} #{title}</a>
<p>Boo</p>
|]

    -- This deconstruction to a tuple is a bit clunky, but I can't work out how to put
    -- the printf into the #{mm} in the hamlet.
    where deconstructEntryEntity (Entity _ (Entry title mashedTitle year month day content visible)) = (title, mashedTitle, year, month, printf "%02d" month :: String, day, printf "%02d" day :: String, content, visible)

getEntryR :: EntryId -> Handler RepPlain
getEntryR eid = do
    entry <- runDB $ get eid

    -- return $ RepPlain $ toContent $ show entry
    return $ RepPlain "FIXME"

getEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
getEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    case e of (Just (Entity eid (Entry title' mashedTitle' year' month' day' content' visible'))) -> do comments <- runDB $ selectList [CommentEntry ==. eid] [Asc CommentPosted]
                                                                                                        defaultLayout $ do
                                                                                                            setTitleI title'
                                                                                                            [whamlet|
<h1>#{title'}
<article>#{content'}
    <section .comments>
        <h2>_{MsgCommentsHeading}
        <br>
        $if null comments
            <p>_{MsgNoComments}
        $else
            $forall Comment _entry posted name text visible <- map entityVal comments
                <hr>
                <h3>#{name}
                <h4>#{show posted}
                <p>#{toHtml text}
            <hr>
|]
              _                                                                            -> notFound
