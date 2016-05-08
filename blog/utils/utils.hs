{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- imports copied from app/main.hs
import Prelude              (IO, read, (!!), tail)
import Yesod.Default.Main   (defaultMain)
-- import Application          (makeApplication)

import qualified Prelude as P

-- my imports
import Import
import Yesod.Default.Config
import Database.Persist.Sqlite
import Settings
import Data.String.Conversions
import Data.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
-- import Control.Monad (forM_, liftM)
import Database.Persist.Sql (SqlBackend)
import System.Process
import Data.Maybe
import Data.Char
import System.IO (openFile, getContents, IOMode(ReadMode))
import qualified System.FilePath as FP
import System.Directory
import qualified Data.List as DL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as DM
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding.Error as DTEE

import qualified Text.Blaze as TB
import qualified Text.Blaze.Html as TBH
import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.Text as TBHRT

import Database.Persist.Sql
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger -- (runStdoutLoggingT, LoggingT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import qualified Text.RSS as RSS
import Network.URI
import Data.Time.Calendar
import Data.Time.Clock
import Safe


sanitiseTitle :: DT.Text -> DT.Text
sanitiseTitle = cs . wpHack . nukeNonAlNum . map hack . map dotToDash . cs . dashes . lowerCase
    where dashes :: DT.Text -> DT.Text
          dashes = DT.intercalate "-" . DT.words

          lowerCase :: DT.Text -> DT.Text
          lowerCase = DT.toLower

          nukeNonAlNum :: String -> String
          nukeNonAlNum = cs . mapMaybe (\c -> if c == '-' || isAlphaNum c then Just c else Nothing)

          dotToDash :: Char -> Char
          dotToDash '.' = '-'
          dotToDash c = c

          hack :: Char -> Char
          hack 'ň' = 'n'
          hack 'é' = 'e'
          hack c   = c

          -- I'll be damned if I can be bothered to work out Wordpress' scheme for making permalinks, so here
          -- are a few manual exceptions to sanitiseTitle.
          wpHack :: String -> String
          wpHack t
            | t == "pyx-0-10-experimental-package"                              = "pyx-010-experimental-package"
            | t == "pyx-0-10-experimental-package"                              = "pyx-010-experimental-package"
            | t == "telstra-prepaid-wireless-broadband-on-ubuntu-9-0410-04"     = "telstra-prepaid-wireless-broadband-on-ubuntu-9-04"
            | t == "recovery-of-data-from-a-raid-5-disk"                        = "recovery-of-data-from-a-raid5-disk"
            | t == "scipy-znst8iosbase4initd1ev-and-link-flags"                 = "scipy-_znst8ios_base4initd1ev-and-link-flags"
            | t == "passing-a-numpy-array-to-a-c-function"                      = "passing-numpy-array-to-c-function"
            | t == "xfce4-xfapplet-plugin-for-centos-6-3"                       = "xfce4-xfapplet-panel-for-centos-6-3"
            | otherwise                                                         = t


-- newtype SanitisedTitle = SanitisedTitle Text deriving (Show, Eq, Read)
-- instance PathPiece SanitisedTitle where
--     toPathPiece (SanitisedTitle t) = pack $ show t
--     fromPathPiece t = if t == (sanitiseTitle t) then Just (SanitisedTitle t) else Nothing

{-

myRunDB is like Yesod's runDB except that it happens in the IO monad,
not inside a Handler. The contents for myRunDB are taken from bits
of Application.hs and Foundation.hs in a normal scaffolded Yesod
application.

Note the use of "runNoLoggingT $ runResourceT" for the call
to runPool. Without them one has issies with MonadResource and
MonadLogger:

    Utils.hs:51:26:
        No instances for (Control.Monad.Trans.Resource.MonadResource IO,
                          Control.Monad.Logger.MonadLogger IO)
          arising from a use of `selectList'
        Possible fix:
          add instance declarations for
          (Control.Monad.Trans.Resource.MonadResource IO,
           Control.Monad.Logger.MonadLogger IO)
        In the second argument of `($)', namely `selectList [] []'
        In a stmt of a 'do' block:
          entries <- myRunDB $ selectList [] [] :: IO [Entity Entry]
        In the expression:
          do { now <- getCurrentTime
               let e = Entry "first post" now "Hi there!" True
               entryId <- myRunDB $ insert e
               print entryId
               .... }

-}

myRunDB :: SqlPersistT (ResourceT (NoLoggingT IO)) b -> IO b
myRunDB f = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Development)

    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Sql.loadConfig >>=
              Database.Persist.Sql.applyEnv
    p <- Database.Persist.Sql.createPoolConfig (dbconf :: SqliteConf)

    runNoLoggingT $ runResourceT $ runSqlPool f p

{-

-- TODO Check the code here:
-- https://github.com/yesodweb/yesod/wiki/Using-Database.Persist.runPool-without-Foundation
-- and see if it provides a better way to sort out myRunDB, especially with choosing the right
-- configSettings parameter.

-- also http://www.yesodweb.com/blog/2011/12/resourcet

data WorkerConf = WorkerConf
    { getConfig :: SqliteConf
    , getPool   :: Database.Persist.Sql.ConnectionPool
    }

type WorkerM = ReaderT WorkerConf (ResourceT (LoggingT IO)) -- FIXME Unused?

blah = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings (confName "Development")) { csParseExtra = parseExtra }

    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)

    let workerConf = WorkerConf dbconf p

    -- print "derp"
    return (dbconf, p)

    where confName "Development" = Development
          confName "Production"  = Production
          confName x             = error $ "unknown configuration name: " ++ x


-}



-- http://stackoverflow.com/questions/19861914/yesod-how-to-show-the-pure-value-of-a-persistint64-key
-- unKey has gone away, so use toPathPiece?
-- unKey' :: PathPiece (Key a) => Key a -> Text
-- unKey' = toPathPiece

-- Actually, there is now fromSqlKey and toSqlKey so just use them.
unKey' :: ToBackendKey SqlBackend record => Key record -> Text
unKey' = cs . show . fromSqlKey

printBlogPost :: Key Entry -> Entry -> IO ()
printBlogPost eid (Entry title mashedTitle year month day content visible) = do
    let niceEntryId :: DT.Text
        niceEntryId = unKey' eid

        niceURL :: DT.Text
        niceURL = DT.intercalate "/" (map cs [show year, show month, show day, cs mashedTitle])

        niceTitle :: DT.Text
        niceTitle = title

        niceContent :: DT.Text
        niceContent = DTE.decodeUtf8With DTEE.lenientDecode $ BS.concat . BSL.toChunks $ renderHtml content -- Html converted to Text

        niceContent' :: DT.Text
        niceContent' = take 50 $ DT.replace "\n" " ... " niceContent -- rip out newlines, take first 50 characters

        niceVisible :: DT.Text
        niceVisible = if visible then "VISIBLE" else "HIDDEN"

    putStrLn $ niceEntryId ++ " " ++ niceVisible ++ " " ++ niceURL ++ " " ++ " '" ++ niceTitle ++ "' " ++ niceContent'

printBlogPostEntity :: Entity Entry -> IO ()
printBlogPostEntity (Entity eid entry) = printBlogPost eid entry

listBlogPosts :: IO ()
listBlogPosts = do
    posts <- myRunDB $ selectList [] [] :: IO [Entity Entry]

    forM_ posts printBlogPostEntity

instance Show Comment where
    show (Comment entryId commentDate commentAuthor commentAuthorEmail commentAuthorUrl commentText visible) = show commentDate ++ " " ++ show commentAuthor -- FIXME show/render the text/html too

instance Show Entry where
    show (Entry title sanitisedTitle year month day content False) = show title -- FIXME show more fields

dumpBlogPosts = do
    posts <- myRunDB $ selectList [] [] :: IO [Entity Entry]
    forM_ posts print

    comments <- myRunDB $ selectList [] [] :: IO [Entity Comment]
    forM_ comments print

    return ()

deleteAllBlogPosts = do
    entries <- myRunDB $ selectList [] [] :: IO [Entity Entry]

    forM_ (map entityToEntryId entries) (myRunDB . delete)

    where entityToEntryId (Entity eid (Entry {})) = eid

getYMD :: UTCTime -> (Int, Int, Int)
getYMD utc = (fromInteger y, m, d)
    where (y, m, d) = toGregorian $ utctDay utc

makeFakeBlogPosts = do
    (year1, month1, day1) <- liftM getYMD getCurrentTime

    print (year1, month1, day1)
    let e1 = Entry "first post" (sanitiseTitle "first post") year1 month1 day1 (toHtml ("Hi there!" :: String)) False

    (year2, month2, day2) <- liftM getYMD getCurrentTime
    print (year2, month2, day2)
    let e2 = Entry "second post" (sanitiseTitle "second post") year2 month2 day2 (toHtml ("Hi there! Do de dah!" :: String)) False

    forM_ [e1, e2] (void . myRunDB . insert)

    return ()

maybeTextToText = fromMaybe ""

showBlogPost i = do
    let entryId = toSqlKey i
    entry    <- myRunDB $ get entryId

    case (entry :: Maybe Entry) of (Just e) -> do let content = show $ renderHtml $ entryContent e
                                                  printBlogPost entryId e

                                                  comments <- myRunDB $ selectList [CommentEntry ==. entryId] [] :: IO [Entity Comment]

                                                  forM_ comments (\c -> do let (Entity cid (Comment _ posted name email url text visible)) = c
                                                                               niceCommentId    = unKey' cid
                                                                               niceName         = name
                                                                               niceEmail        = maybeTextToText email
                                                                               niceUrl          = maybeTextToText url
                                                                               niceText         = cs $ show $ lines $ TBHRT.renderHtml text
                                                                               niceVisible      = if visible then "VISIBLE" else "HIDDEN"

                                                                           putStrLn $ "comment: " ++ (cs $ show i) ++ " " ++ niceCommentId ++ " " ++ niceVisible ++ " " ++ niceName ++ " " ++ niceText)
                                   Nothing  -> print "boo"
    -- where foo (PersistInt64 i) = i

addBlogPostFromEditor title = do
    r <- system "vim /tmp/blah.html"
    content <- liftM toHtml $ liftM TB.preEscapedToMarkup $ P.readFile "/tmp/blah.html"

    (year, month, day) <- liftM getYMD getCurrentTime

    myRunDB $ insert (Entry title (sanitiseTitle title) year month day content False)

-- For importing entries; requires year, month, day to be specified.
addBlogPostFromStdin title year month day = do
    content <- liftM toHtml getContents
    myRunDB $ insert (Entry title (sanitiseTitle title) year month day content False)

addBlogPostFromFile fileName = do
    print fileName

    h <- openFile fileName ReadMode

    contents <- BS.hGetContents h
    let x = map cs $ lines $ DTE.decodeUtf8With DTEE.lenientDecode contents

    -- x <- lines <$> readFile fileName

    let ymd     = P.head x
        year    = read $ words ymd !! 0 :: Int
        month   = read $ words ymd !! 1 :: Int
        day     = read $ words ymd !! 2 :: Int
        title   = cs $ P.head $ tail x
        content = toHtml $ TB.preEscapedToMarkup $ unlines $ drop 2 x

    eid <- myRunDB $ insert (Entry title (sanitiseTitle title) year month day content False)

    print eid -- FIXME tidyup

    -- Now the comments, if any.
    commentFiles <- getCommentFiles fileName
    forM_ commentFiles (addCommentFromFile eid)

addCommentFromFile entryId commentFile = do
    x <- liftM lines (P.readFile commentFile)

    let commentAuthor       = cs $ x !! 0
        commentAuthorEmail  = castTextToMaybe $ cs $ x !! 1
        commentAuthorUrl    = castTextToMaybe $ cs $ x !! 2
        commentDate         = read (x !! 3) :: UTCTime
        commentText         = (toHtml . TB.preEscapedToMarkup) $ unlines $ drop 4 x

        visible = False

    c <- myRunDB $ insert (Comment entryId commentDate commentAuthor commentAuthorEmail commentAuthorUrl commentText visible)
    print c -- FIXME do something more verbose

    where castTextToMaybe s = if s == "" then Nothing else Just s

getCommentFiles fileName = liftM (DL.sort . commentFile . addDirectoryPrefix . keepOurs) directoryContents
    where directoryContents = getDirectoryContents (FP.takeDirectory fileName) :: IO [FP.FilePath]
          baseName = FP.dropExtension $ FP.takeFileName fileName  :: FP.FilePath
          keepOurs = filter (DL.isPrefixOf baseName)        :: [FP.FilePath] -> [FP.FilePath]
          commentFile = filter (DL.isInfixOf "comment")     :: [FP.FilePath] -> [FP.FilePath]
          directoryName = FP.takeDirectory fileName
          addDirectoryPrefix = map (directoryName FP.</>) :: [FP.FilePath] -> [FP.FilePath]

editBlogPost i = do
    let entryId = toSqlKey i
    entry <- myRunDB $ get entryId

    case (entry :: Maybe Entry) of (Just e) -> do let content1 = renderHtml $ entryContent e
                                                  BSL.writeFile "/tmp/blah.html" content1

                                                  r <- system "vim /tmp/blah.html"
                                                  content2 <- liftM (toHtml . TB.preEscapedToMarkup) $ P.readFile "/tmp/blah.html"
                                                  myRunDB $ update entryId [EntryContent =. content2]
                                   Nothing  -> print "boo"

-- FIXME add url/email to the edit thing, in case have to tidy
-- up dodgy/spammy comment.
editComment cid = do
    let commentId = toSqlKey cid

    comment <- myRunDB $ get commentId

    case (comment :: Maybe Comment) of (Just c) -> do let name1 = commentName c
                                                          url1  = if isNothing (commentUrl c) then "" else cs $ fromJust $ commentUrl c
                                                          text1 = cs $ TBHRT.renderHtml $ commentText c
                                                      writeFile "/tmp/blah.html" (name1 ++ "\n" ++ url1 ++ "\n" ++ text1) -- FIXME use a temp file

                                                      r <- system "vim /tmp/blah.html"
                                                      x <- readFile "/tmp/blah.html"

                                                      let name2 = P.head $ lines x
                                                          url2  = P.head $ P.drop 1 $ lines x
                                                          url2' = if url2 == "" then Nothing else Just url2
                                                          text2 = (toHtml . TB.preEscapedToMarkup) (unlines $ drop 2 $ lines x)

                                                      myRunDB $ update commentId [ CommentName =. name2
                                                                                 , CommentUrl  =. url2'
                                                                                 , CommentText =. text2
                                                                                 ]

                                       Nothing  -> print "boo"

deleteBlogPost i = do
    let entryId = toSqlKey i :: Key Entry
    myRunDB $ delete entryId

    comments <- myRunDB $ selectList [CommentEntry ==. entryId] [] :: IO [Entity Comment]

    forM_ (map entityToCommentId comments) (myRunDB . delete)

    where entityToCommentId (Entity cid (Comment {})) = cid

deleteComment i = do
    let commentId = toSqlKey i :: Key Comment
    myRunDB $ delete commentId

setEntryVisible :: Integer -> Bool -> IO ()
setEntryVisible i visible = myRunDB $ update (toSqlKey $ fromIntegral i) [EntryVisible =. visible]

setCommentVisible :: Integer -> Bool -> IO ()
setCommentVisible i visible = myRunDB $ update (toSqlKey $ fromIntegral i) [CommentVisible =. visible]

reportUnmoderatedComments = do
    comments <- myRunDB $ selectList [CommentVisible ==. False] [Desc CommentPosted]

    forM_ comments (\c -> do let (Entity cid (Comment eid posted name email url text visible)) = c
                                 niceEntryId      = unKey' eid
                                 niceCommentId    = unKey' cid
                                 niceName         = name
                                 niceEmail        = maybeTextToText email
                                 niceUrl          = maybeTextToText url
                                 niceText         = cs $ show $ lines $ TBHRT.renderHtml text
                                 niceVisible      = if visible then "VISIBLE" else "HIDDEN"

                             putStrLn $ niceEntryId ++ " " ++ niceCommentId ++ " " ++ niceVisible ++ " " ++ niceName ++ " " ++ niceEmail ++ " " ++ niceUrl ++ " " ++ niceText)

go :: [String] -> IO ()
go ["--list"] = listBlogPosts
go ["--dump"] = dumpBlogPosts

go ["--show", entryId]      = showBlogPost (read entryId)
go ["--edit-post", entryId]      = editBlogPost (read entryId)
go ["--edit-comment", commentId]      = editComment (read commentId)
go ["--delete-post", entryId]    = deleteBlogPost (read entryId)
go ["--delete-comment", commentId]    = deleteComment (read commentId)

go ["--add", title]         = do x <- addBlogPostFromEditor (cs title)
                                 print x -- FIXME tidy this up

go ["--add-from-stdin", title, year, month, day]  = do x <- addBlogPostFromStdin (cs title) (read year) (read month) (read day)
                                                       print x -- FIXME tidy this up

go ["--add-from-file", fileName]  = addBlogPostFromFile fileName

go ["--set-post-visible", i]     = setEntryVisible (read i) True
go ["--set-post-invisible", i]   = setEntryVisible (read i) False

go ["--set-comment-visible", i]     = setCommentVisible (read i) True
go ["--set-comment-invisible", i]   = setCommentVisible (read i) False

go ["--report-unmoderated"] = reportUnmoderatedComments

go _ = do
    putStrLn ""

    putStrLn "Usage:"
    putStrLn ""

    putStrLn "  --list    List all blog posts."
    putStrLn "  --dump    Verbose dump of all blog posts including comments."
    putStrLn ""

    putStrLn "  --show           <eid>  Detailed contents of blog post with numeric ID <eid>."
    putStrLn "  --edit-post      <eid>  Edit blog post <eid> using vim."
    putStrLn "  --edit-comment   <cid>  Edit comment with numeric ID <cid>."
    putStrLn "  --delete-post    <eid>  Delete blog post. DOES NOT ASK FOR CONFIRMATION!"
    putStrLn "  --delete-comment <cid>  Delete comment. DOES NOT ASK FOR CONFIRMATION!"
    putStrLn ""

    putStrLn "  --add <title>     Add a new blog post with the given title."
    putStrLn ""
    putStrLn "  --add-from-stdin <title> <year> <month> <day>  Add a blog post from stdin."
    putStrLn "  --add-from-file  <filename>                    Add a blog post from a file."
    putStrLn ""

    putStrLn "  --set-post-visible   <eid>     Set a post to be visible."
    putStrLn "  --set-post-invisible <eid>     Set a post to be invisible."
    putStrLn ""

    putStrLn "  --set-comment-visible   <cid>  Set a comment to be visible."
    putStrLn "  --set-comment-invisible <cid>  Set a comment to be invisible."
    putStrLn ""

    putStrLn "  --report-unmoderated      Report all unmoderated (invisible) comments."
    putStrLn ""

main :: IO ()
main = getArgs >>= (return . map cs) >>= go
