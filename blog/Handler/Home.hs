{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time (UTCTime, getCurrentTime, utctDay, toGregorian)
import Model
import Control.Monad (when)

import Database.Persist.Store (PersistValue(PersistInt64))

import qualified Data.Text as DT

import Text.Printf
import Data.Maybe
import Control.Applicative
import Yesod.ReCAPTCHA

commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing
    <*> pure False <* recaptchaAForm

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

getEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
getEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    case e of (Just (Entity eid (Entry title' mashedTitle' year' month' day' content' visible'))) -> do comments <- runDB $ selectList [CommentEntry ==. eid, CommentVisible ==. True] [Asc CommentPosted]
                                                                                                        (commentWidget, enctype) <- generateFormPost (commentForm eid)

                                                                                                        defaultLayout $ do
                                                                                                            setTitleI title'
                                                                                                            [whamlet|
<h1>#{title'}
<article>#{content'}
    <section .comments>
        <div id="comments"></div>
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
        <section>
            <h1>_{MsgAddCommentHeading}

            <form method=post enctype=#{enctype}>
                ^{commentWidget}
                <div>
                    <input type=submit value=_{MsgAddCommentButton}>

|]
              _                                                                            -> notFound


postEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
postEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    let entryId = entityToEntryId (fromJust e) -- FIXME handle the Nothing case here
        title   = entityToTitle (fromJust e) -- FIXME handle the Nothing case?

    ((res, commentWidget), enctype) <- runFormPost (commentForm entryId)
    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            defaultLayout $ do
                setTitleI MsgCommentAdded
                [whamlet|
<p> Comment has been added to the moderation queue.

<p> Return to the post: <a href=@{EntryLongR year month day mashedTitle}>#{title}</a>

|]
        _ -> defaultLayout $ do
            setTitleI MsgPleaseCorrectComment
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{commentWidget}
    <div>
        <input type=submit value=_{MsgAddCommentButton}>
|]


    where entityToEntryId (Entity eid (Entry {})) = eid -- FIXME should not have to roll our own for these?
          entityToTitle (Entity _ (Entry title _ _ _ _ _ _)) = title -- FIXME use standard function to rip out title, templated something?
