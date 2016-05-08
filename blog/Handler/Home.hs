module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import qualified Data.Text as DT
import qualified System.FilePath.Posix as FP
import qualified Text.RSS as RSS

import Data.Maybe (fromJust)
import Data.String.Conversions
import Text.Printf
import Yesod.ReCAPTCHA

import Network.URI
import Network.Wai
import Network.Mail.Mime

import qualified Data.List as DL
import qualified Data.Text.Lazy as DTL
import qualified Text.Blaze.Html.Renderer.Text as TBHRT

cu :: FP.FilePath -> FP.FilePath -> FP.FilePath
cu x y = FP.dropTrailingPathSeparator $ (FP.dropTrailingPathSeparator x) FP.</> y

baseUrl = (appBaseUrl . appSettings) <$> getYesod

commentForm :: EntryId -> Html -> MForm Handler (FormResult Comment, Widget)
commentForm entryId extra = do
    (nameRes, nameView)     <- mreq textField  (fieldSettingsLabel MsgCommentName)  Nothing
    (emailRes, emailView)   <- mopt emailField (fieldSettingsLabel MsgCommentEmail) Nothing
    (urlRes, urlView)       <- mopt urlField   (fieldSettingsLabel MsgCommentUrl)   Nothing
    (textRes, textView)     <- mreq htmlField  (fieldSettingsLabel MsgCommentText)  Nothing

    (recapRes, recapView)   <- recaptchaMForm

    let recapView0 = recapView DL.!! 0

    now <- liftIO getCurrentTime

    let c = Comment
              <$> pure entryId
              <*> pure now
              <*> nameRes
              <*> emailRes
              <*> urlRes
              <*> textRes
              <*> pure False <* recapRes

    let widget = do
            toWidget
                [lucius|
                    ##{fvId nameView} {
                        width: 70ch;
                    }
                    ##{fvId emailView} {
                        width: 70ch;
                    }
                    ##{fvId urlView} {
                        width: 70ch;
                    }
                    ##{fvId textView} {
                        width: 120ch;
                        height: 120ch;
                    }
                |]
            [whamlet|
                #{extra}
                <p>
                    Name                 #
                <p>
                    ^{fvInput nameView}
                <p>
                    \ Email (not shown)  #
                <p>
                    ^{fvInput emailView}
                <p>
                    \ URL (optional)     #
                <p>
                    ^{fvInput urlView}
                <p>
                    \ Comment            #
                <p>
                    ^{fvInput textView}
                <p>
                    ^{fvInput recapView0}
            |]

    return (c, widget)

-- My posts are specified by year/month/day and mashed-title, so
-- pretend that they all happened at midday.
midday = fromIntegral (12*3600 :: Integer)

latestPost :: [Entry] -> UTCTime
latestPost entries = DL.maximum dates
    where dates = map dateOfPost entries :: [UTCTime]

dateOfPost :: Entry -> UTCTime
dateOfPost (Entry _ _ year month day _ _) = UTCTime (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)) midday

entryToItem :: String -> String -> Entry -> [RSS.ItemElem]
entryToItem url author (Entry title mashedTitle year month day content visible) = [ RSS.Title $ cs title
                                                                                  , RSS.Link postURI
                                                                                  , RSS.Author author
                                                                                  , RSS.Comments commentURI
                                                                                  , RSS.PubDate postDateTime
                                                                                  , RSS.Guid False postURL
                                                                                  -- TODO , RSS.Description content
                                                                                  ]
    where postDateTime = UTCTime (fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)) midday
          postURL = url `cu` (show year) `cu` (show month) `cu` (show day) `cu` (cs mashedTitle)
          -- postURI = trace ("would have used: " ++ (show postURL)) $ fromJust $ parseURI "http://foo.com" -- $ parseURI postURL
          postURI = fromJust $ parseURI postURL
          commentURL = postURL ++ "#comments"
          commentURI = fromJust $ parseURI commentURL

getFeedR :: Handler RepXml
getFeedR = do
    entryEntities <- runDB $ selectList [] []

    base <- cs <$> baseUrl

    settings <- appSettings <$> getYesod

    let root = cs $ appRoot settings
    let url = root `cu` base

    let entries = reverse $ sortBy (compare `on` dateOfPost) (map entityVal entryEntities) :: [Entry]
        author  = cs $ appRssWebMaster settings
        items   = map (entryToItem url author) entries :: [[RSS.ItemElem]]

        channel = [ RSS.Language  $ cs $ appRssLanguage      settings
                  , RSS.Copyright $ cs $ appRssCopyright     settings
                  , RSS.WebMaster $ cs $ appRssWebMaster     settings
                  , RSS.LastBuildDate $ latestPost entries
                  , RSS.Generator "rss-3000"
                  ]

    m <- getYesod
    let blogTitle       = cs $ renderMessage m [] MsgBlogTitle
        blogDescription = cs $ renderMessage m [] MsgBlogDescription

    return $ RepXml $ toContent $ (RSS.showXML . RSS.rssToXML) (RSS.RSS blogTitle (fromJust $ parseURI url) blogDescription channel items)

getHomeR :: Handler RepHtml
getHomeR = do
    master <- getYesod

    entries <- runDB $ selectList [EntryVisible ==. True] [ Desc EntryPostedYear
                                                          , Desc EntryPostedMonth
                                                          , Desc EntryPostedDay
                                                          ]

    let entriesAsTuples = map deconstructEntryEntity entries

    let latestEntries   = take 8 entriesAsTuples
        fmtDateString y m d = printf "%04d-%02d-%02d" y m d :: String

    let url = cs $ appRoot $ appSettings master

    base <- cs <$> baseUrl

    let feedUrl = url `cu` base `cu` "feed"

    defaultLayout $ do
        setTitleI MsgWelcomeHomepage
        [whamlet|

<h1><a href=#{url}>_{MsgBlogTitle}</a>
<hr>
$if null entries
    <p>_{MsgNoEntries}
$else
    $forall (title, mashedTitle, year, month, mm, day, dd, content, visible) <- latestEntries
        <h1><a href=@{EntryLongR year month day mashedTitle}>#{title}</a>
        <h3>#{fmtDateString year month day}
        <article>#{content}
<hr>
$if null entries
    <p>_{MsgNoEntries}
$else
    <ul>
        $forall (title, mashedTitle, year, month, mm, day, dd, content, visible) <- entriesAsTuples
            <li> <a href=@{EntryLongR year month day mashedTitle}>#{year}-#{mm}-#{dd} #{title}</a>

<p> <a href="#{feedUrl}">Posts: RSS</a>
|]

    -- This deconstruction to a tuple is a bit clunky, but I can't work out how to put
    -- the printf into the #{mm} in the hamlet.
    where deconstructEntryEntity (Entity _ (Entry title mashedTitle year month day content visible)) = (title, mashedTitle, year, month, printf "%02d" month :: String, day, printf "%02d" day :: String, content, visible)


getEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
getEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    url <- (cs . appRoot . appSettings) <$> getYesod

    case e of (Just (Entity eid (Entry title' mashedTitle' year' month' day' content' True)))     -> do comments <- runDB $ selectList [CommentEntry ==. eid, CommentVisible ==. True] [Asc CommentPosted]

                                                                                                        maxNrComments <- (appMaxNrComments . appSettings) <$> getYesod
                                                                                                        let commentsOpen = length comments < maxNrComments
                                                                                                        base <- cs <$> baseUrl

                                                                                                        (commentWidget, enctype) <- generateFormPost (commentForm eid)

                                                                                                        let dateString = printf "%04d-%02d-%02d" year' month' day' :: String

                                                                                                        defaultLayout $ do
                                                                                                            setTitleI title'
                                                                                                            [whamlet|
<p align="right"><h1><a href=#{cu url base}>_{MsgBlogTitle}</a>
<hr>
<h1>#{title'}
<h3>#{dateString}
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
                <h4>#{show posted}
                <p>#{toHtml text}

        $if commentsOpen
            <section>
                <h1>_{MsgAddCommentHeading}

                <form method=post enctype=#{enctype}>
                    ^{commentWidget}
                    <div>
                        <input type=submit value=_{MsgAddCommentButton}>
        $else
            <p> Comments are closed.
|]
              _                                                                            -> notFound

sendEmailNotification comment title ip = do
    settings <- appSettings <$> getYesod

    let Comment _ _ name email _ text _ = comment
        niceEmail = maybe "<no email supplied>" id (fmap cs email)
        subjectLine = cs $ "new comment from [" ++ (cs name) ++ "] with email [" ++ niceEmail ++ "] with address [" ++ (show ip) ++ "] on post [" ++ (cs title) ++ "]"

    x <- liftIO $ simpleMail (Address (Just $ appEmailNotificationFromName settings) (appEmailNotificationFromAddress settings))
                             (Address (Just $ appEmailNotificationToName settings)   (appEmailNotificationToAddress settings))
                             subjectLine
                             (TBHRT.renderHtml $ text)
                             (TBHRT.renderHtml $ text)
                             []

    liftIO $ renderSendMail x

successfulCommentPost year month day mashedTitle comment title ip = do
    _ <- runDB $ insert comment
    sendEmailNotification comment title ip


    defaultLayout $ do
        setTitleI MsgCommentAdded
        [whamlet|
<p> Comment has been added to the moderation queue:

<pre>
    <p> Name: #{commentName comment}
    <p> Comment: #{commentText comment}

<p> Return to the post: <a href=@{EntryLongR year month day mashedTitle}>#{title}</a>
|]

unsuccessfulCommentPost commentWidget enctype formTitle = do
    defaultLayout $ do
        setTitleI formTitle
        [whamlet|
<form method=post enctype=#{enctype}>
    ^{commentWidget}
    <div>
        <input type=submit value=_{MsgAddCommentButton}>
|]

postEntryLongR :: Int -> Int -> Int -> Text -> Handler RepHtml
postEntryLongR year month day mashedTitle = do
    e <- runDB $ getBy $ EntryYMDMashed year month day mashedTitle

    settings <- appSettings <$> getYesod

    let entryId = entityKey (fromJust e) -- FIXME handle the Nothing case here
        title   = (entryTitle . entityVal) (fromJust e) -- FIXME handle the Nothing case?

    ((res, commentWidget), enctype) <- runFormPost (commentForm entryId)
    case res of
        FormSuccess comment -> do if (DTL.length $ TBHRT.renderHtml $ commentText comment) < (fromIntegral $ appMaxCommentLength settings :: Int64)
                                    then do ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
                                            successfulCommentPost year month day mashedTitle comment title ip
                                    else unsuccessfulCommentPost commentWidget enctype MsgPleaseCorrectTooLong

        _ -> unsuccessfulCommentPost commentWidget enctype MsgPleaseCorrectComment


