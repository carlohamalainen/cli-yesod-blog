{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Network.Mail.Mime
import qualified Data.Text.Lazy as DTL

import qualified Text.RSS as RSS
import Network.URI
import Data.Time.Calendar
import Data.Time.Clock
import Safe

import Data.List (sortBy)
import Data.Function (on)

import Data.Yaml (decodeFile, Value)
import Data.Aeson.Types

import Control.Monad

import Yesod.Default.Config


---------------------------------------------------------------------
data Person = Person { personName :: Text }
    deriving Show

personForm :: Form Person
personForm = renderDivs $ Person <$> areq textField "Name" Nothing

getFormResult f = do
    (result, _) <- aFormToForm f
    return result

---------------------------------------------------------------------


commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
    <$> pure entryId
    <*> aformM (liftIO getCurrentTime)
    <*> areq textField (fieldSettingsLabel MsgCommentName) Nothing
    <*> aopt emailField (fieldSettingsLabel MsgCommentEmail) Nothing
    <*> aopt urlField (fieldSettingsLabel MsgCommentUrl) Nothing
    <*> areq textareaField (fieldSettingsLabel MsgCommentText) Nothing
    <*> pure False <* recaptchaAForm

-- My posts are specified by year/month/day and mashed-title, so
-- pretend that they all happened at midday.
midday = fromIntegral (12*3600 :: Integer)

latestPost :: [Entry] -> UTCTime
latestPost entries = maximum dates
    where dates = map dateOfPost entries :: [UTCTime]

dateOfPost :: Entry -> UTCTime
dateOfPost (Entry _ _ year month day _ _) = UTCTime (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)) midday

entryToItem :: String -> String -> Entry -> [RSS.ItemElem]
entryToItem url author (Entry title mashedTitle year month day content visible) = [ RSS.Title $ DT.unpack title
                                                                                  , RSS.Link postURI
                                                                                  , RSS.Author author
                                                                                  , RSS.Comments commentURI
                                                                                  , RSS.PubDate postDateTime
                                                                                  , RSS.Guid False postURL
                                                                                  ]
    where postDateTime = UTCTime (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)) midday
          postURL = url ++ (show year) ++ "/" ++ (show month) ++ "/" ++ (show day) ++ "/" ++ (DT.unpack mashedTitle)
          postURI = fromJust $ parseURI postURL
          commentURL = postURL ++ "#comments"
          commentURI = fromJust $ parseURI commentURL

getFeedR :: Handler RepXml
getFeedR = do
    e <- getExtra

    entryEntities <- runDB $ selectList [] []

    url <- DT.unpack <$> fmap (appRoot . settings) getYesod

    let entries = reverse $ sortBy (compare `on` dateOfPost) (map entityVal entryEntities) :: [Entry]
        author  = DT.unpack $ extraRssWebMaster e
        items   = map (entryToItem url author) entries :: [[RSS.ItemElem]]

        channel = [ RSS.Language  $ DT.unpack $ extraRssLanguage e
                  , RSS.Copyright $ DT.unpack $ extraRssCopyright e
                  , RSS.WebMaster $ DT.unpack $ extraRssWebMaster e
                  , RSS.LastBuildDate $ latestPost entries
                  , RSS.Generator "rss-3000"
                  ]

    m <- getYesod
    let blogTitle       = DT.unpack $ renderMessage m [] MsgBlogTitle
        blogDescription = DT.unpack $ renderMessage m [] MsgBlogDescription

    return $ RepXml $ toContent $ (RSS.showXML . RSS.rssToXML) (RSS.RSS blogTitle (fromJust $ parseURI url) blogDescription channel items)

getHomeR :: Handler RepHtml
getHomeR = do
    entries <- runDB $ selectList [EntryVisible ==. True] [ Desc EntryPostedYear
                                                          , Desc EntryPostedMonth
                                                          , Desc EntryPostedDay
                                                          ]

    let entriesAsTuples = map deconstructEntryEntity entries

    e <- getExtra
    url <- DT.unpack <$> fmap (appRoot . settings) getYesod

    defaultLayout $ do
        setTitleI MsgWelcomeHomepage
        [whamlet|

<h1><a href=#{url}>_{MsgBlogTitle}</a>
<hr>
$if null entries
    <p>_{MsgNoEntries}
$else
    <ul>
        $forall (title, mashedTitle, year, month, mm, day, dd, content, visible) <- entriesAsTuples
            <li> <a href=@{EntryLongR year month day mashedTitle}>#{year}-#{mm}-#{dd} #{title}</a>
|]

    -- This deconstruction to a tuple is a bit clunky, but I can't work out how to put
    -- the printf into the #{mm} in the hamlet.
    where deconstructEntryEntity (Entity _ (Entry title mashedTitle year month day content visible)) = (title, mashedTitle, year, month, printf "%02d" month :: String, day, printf "%02d" day :: String, content, visible)

getEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
getEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    url <- DT.unpack <$> fmap (appRoot . settings) getYesod

    case e of (Just (Entity eid (Entry title' mashedTitle' year' month' day' content' visible'))) -> do comments <- runDB $ selectList [CommentEntry ==. eid, CommentVisible ==. True] [Asc CommentPosted]
                                                                                                        (commentWidget, enctype) <- generateFormPost (commentForm eid)

                                                                                                        defaultLayout $ do
                                                                                                            setTitleI title'
                                                                                                            [whamlet|
<p align="right"><h1><a href=#{url}>_{MsgBlogTitle}</a>
<hr>
<h1>#{title'}
<article>#{content'}
    <section .comments>
        <div id="comments"></div>
        <h2>_{MsgCommentsHeading}
        <br>
        $if null comments
            <p>_{MsgNoComments}
        $else
            $forall Comment _entry posted name email url text visible <- map entityVal comments
                <hr>
                $if isNothing url
                    <h3>#{name}
                $else
                    <h3><a href=#{fromJust url}>#{name}</a>
                <h3>#{name} #{fromMaybe "" url}
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

    let entryId = entityKey (fromJust e) -- FIXME handle the Nothing case here
        title   = (entryTitle . entityVal) (fromJust e) -- FIXME handle the Nothing case?

    ((res, commentWidget), enctype) <- runFormPost (commentForm entryId)
    case res of
        FormSuccess comment -> do
            _ <- runDB $ insert comment -- FIXME check length of comment? Rate limit comments by IP???

            let Comment _ _ name _ _ text _ = comment
                subjectLine = DT.pack $ "new comment from [" ++ (DT.unpack name) ++ "] on post [" ++ (DT.unpack title) ++ "]"

            x <- liftIO $ simpleMail (Address (Just "blog") "carlo@carlo-hamalainen.net") (Address (Just "blog") "carlo@carlo-hamalainen.net") subjectLine (DTL.pack $ DT.unpack $ unTextarea text) (DTL.pack $ DT.unpack $ unTextarea text) []
            liftIO $ renderSendMail x

            defaultLayout $ do
                setTitleI MsgCommentAdded
                [whamlet|
<p> Comment has been added to the moderation queue:

<pre>
    <p> Name: #{name}
    <p> Comment: #{text}

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
