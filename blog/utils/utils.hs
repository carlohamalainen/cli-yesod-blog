-- imports copied from app/main.hs
import Prelude              (IO, print, head, tail, readFile, writeFile)
import Yesod.Default.Main   (defaultMain)
import Application          (makeApplication)

-- my imports
import Import
import Yesod.Default.Config
import Database.Persist.Store
import Settings
import Data.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad (forM_, liftM)
import Database.Persist.GenericSql.Raw (SqlBackend)
import System.Process
import Data.Maybe
import Data.Char
import System.Environment ( getArgs )
import System.IO (openFile, getContents, IOMode(ReadMode))
import System.FilePath
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

import Database.Persist.GenericSql
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
sanitiseTitle = wpHack . DT.pack . nukeNonAlNum . map hack . map dotToDash . DT.unpack . dashes . lowerCase
    where dashes        = DT.intercalate (DT.pack "-") . DT.words
          lowerCase     = DT.toLower
          nukeNonAlNum  = mapMaybe (\c -> if c == '-' || isAlphaNum c then Just c else Nothing)

          dotToDash '.' = '-'
          dotToDash c = c

          hack 'ň' = 'n'
          hack 'é' = 'e'
          hack c   = c

          -- I'll be damned if I can be bothered to work out Wordpress' scheme for making permalinks, so here
          -- are a few manual exceptions to sanitiseTitle.
          wpHack t
            | t == DT.pack "pyx-0-10-experimental-package"                              = DT.pack "pyx-010-experimental-package"
            | t == DT.pack "pyx-0-10-experimental-package"                              = DT.pack "pyx-010-experimental-package"
            | t == DT.pack "telstra-prepaid-wireless-broadband-on-ubuntu-9-0410-04"     = DT.pack "telstra-prepaid-wireless-broadband-on-ubuntu-9-04"
            | t == DT.pack "recovery-of-data-from-a-raid-5-disk"                        = DT.pack "recovery-of-data-from-a-raid5-disk"
            | t == DT.pack "scipy-znst8iosbase4initd1ev-and-link-flags"                 = DT.pack "scipy-_znst8ios_base4initd1ev-and-link-flags"
            | t == DT.pack "passing-a-numpy-array-to-a-c-function"                      = DT.pack "passing-numpy-array-to-c-function"
            | t == DT.pack "xfce4-xfapplet-plugin-for-centos-6-3"                       = DT.pack "xfce4-xfapplet-panel-for-centos-6-3"
            | otherwise                                                                 = t


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


myRunDB f = do
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Production) { csParseExtra = parseExtra }

    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)

    runNoLoggingT $ runResourceT $ Database.Persist.Store.runPool dbconf f p


-- TODO Check the code here:
-- https://github.com/yesodweb/yesod/wiki/Using-Database.Persist.runPool-without-Foundation
-- and see if it provides a better way to sort out myRunDB, especially with choosing the right
-- configSettings parameter.

-- also http://www.yesodweb.com/blog/2011/12/resourcet

data WorkerConf = WorkerConf { getConfig :: Settings.PersistConfig
                             , getPool   :: Database.Persist.GenericSql.ConnectionPool
                             }

type WorkerM = ReaderT WorkerConf (ResourceT (LoggingT IO))

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

printBlogPost eid (Entry title mashedTitle year month day content visible) = do
    let niceEntryId = show $ foo $ unKey eid :: String
        niceURL = DT.unpack $ DT.intercalate (DT.pack "/") (map DT.pack [show year, show month, show day, DT.unpack mashedTitle])
        niceTitle = DT.unpack title
        niceContent = DTE.decodeUtf8With DTEE.lenientDecode $ BS.concat . BSL.toChunks $ renderHtml content -- Html converted to Text
        niceContent' = take 50 $ DT.unpack $ DT.replace (DT.pack "\n") (DT.pack " ... ") niceContent -- rip out newlines, take first 50 characters
        niceVisible = if visible then "VISIBLE" else "HIDDEN"

    putStrLn $ niceEntryId ++ " " ++ niceVisible ++ " " ++ niceURL ++ " " ++ " '" ++ niceTitle ++ "' " ++ niceContent'

    where foo (PersistInt64 i) = i

printBlogPostEntity (Entity eid entry) = printBlogPost eid entry

listBlogPosts = do
    posts <- myRunDB $ selectList [] [] :: IO [Entity Entry]

    forM_ posts printBlogPostEntity

instance Show Comment where
    show (Comment entryId commentDate commentAuthor commentAuthorEmail commentAuthorUrl commentText visible) = show commentDate ++ " " ++ show commentAuthor -- FIXME show/render the text/html too

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

{-
makeFakeBlogPosts = do
    (year1, month1, day1) <- liftM getYMD getCurrentTime
    let e1 = Entry (DT.pack "first post") (sanitiseTitle $ DT.pack "first post") year1 month1 day1 (DT.pack "Hi there!") False

    (year2, month2, day2) <- liftM getYMD getCurrentTime
    let e2 = Entry (DT.pack "second post") (sanitiseTitle $ DT.pack "second post") year2 month2 day2 (DT.pack "Hi there! Do de dah!") False

    forM_ [e1, e2] (myRunDB . insert)
-}

addBlogPostFromEditor title = do
    r <- system "vim /tmp/blah.html"
    content <- liftM toHtml $ liftM TB.preEscapedToMarkup $ readFile "/tmp/blah.html"

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
    let x = lines $ DT.unpack $ DTE.decodeUtf8With DTEE.lenientDecode contents

    -- x <- lines <$> readFile fileName

    let ymd     = head x
        year    = read $ words ymd !! 0 :: Int
        month   = read $ words ymd !! 1 :: Int
        day     = read $ words ymd !! 2 :: Int
        title   = DT.pack $ head $ tail x
        content = toHtml $ TB.preEscapedToMarkup $ unlines $ drop 2 x

    eid <- myRunDB $ insert (Entry title (sanitiseTitle title) year month day content False)

    print eid -- FIXME tidyup

    -- Now the comments, if any.
    commentFiles <- getCommentFiles fileName
    forM_ commentFiles (addCommentFromFile eid)

addCommentFromFile entryId commentFile = do
    x <- liftM lines (readFile commentFile)

    let commentAuthor       = DT.pack $ x !! 0
        commentAuthorEmail  = castTextToMaybe $ DT.pack $ x !! 1
        commentAuthorUrl    = castTextToMaybe $ DT.pack $ x !! 2
        commentDate         = read (x !! 3) :: UTCTime
        commentText         = (toHtml . TB.preEscapedToMarkup) $ DT.pack $ unlines $ drop 4 x

        visible = False

    c <- myRunDB $ insert (Comment entryId commentDate commentAuthor commentAuthorEmail commentAuthorUrl commentText visible)
    print c -- FIXME do something more verbose

    where castTextToMaybe s = if s == DT.pack "" then Nothing else Just s

getCommentFiles fileName = liftM (DL.sort . commentFile . addDirectoryPrefix . keepOurs) directoryContents
    where directoryContents = getDirectoryContents (takeDirectory fileName) :: IO [FilePath]
          baseName = dropExtension $ takeFileName fileName  :: FilePath
          keepOurs = filter (DL.isPrefixOf baseName)        :: [FilePath] -> [FilePath]
          commentFile = filter (DL.isInfixOf "comment")     :: [FilePath] -> [FilePath]
          directoryName = takeDirectory fileName
          addDirectoryPrefix = map (directoryName </>) :: [FilePath] -> [FilePath]

editBlogPost i = do
    let entryId = Key $ PersistInt64 (fromIntegral i)
    entry <- myRunDB $ get entryId

    case (entry :: Maybe Entry) of (Just e) -> do let content1 = renderHtml $ entryContent e
                                                  BSL.writeFile "/tmp/blah.html" content1

                                                  r <- system "vim /tmp/blah.html"
                                                  content2 <- liftM (toHtml . TB.preEscapedToMarkup) $ readFile "/tmp/blah.html"
                                                  myRunDB $ update entryId [EntryContent =. content2]
                                   Nothing  -> print "boo"

-- FIXME add url/email to the edit thing, in case have to tidy
-- up dodgy/spammy comment.
editComment cid = do
    let commentId = Key $ PersistInt64 (fromIntegral cid)

    comment <- myRunDB $ get commentId

    case (comment :: Maybe Comment) of (Just c) -> do let name1 = DT.unpack $ commentName c
                                                          url1  = if isNothing (commentUrl c) then "" else DT.unpack $ fromJust $ commentUrl c
                                                          text1 = DTL.unpack $ TBHRT.renderHtml $ commentText c
                                                      writeFile "/tmp/blah.html" (name1 ++ "\n" ++ url1 ++ "\n" ++ text1) -- FIXME use a temp file

                                                      r <- system "vim /tmp/blah.html"
                                                      x <- readFile "/tmp/blah.html"

                                                      let name2 = DT.pack $ head $ lines x
                                                          url2  = DT.pack $ head $ drop 1 $ lines x
                                                          url2' = if url2 == DT.pack "" then Nothing else Just url2
                                                          text2 = (toHtml . TB.preEscapedToMarkup) (DT.pack $ unlines $ drop 2 $ lines x)

                                                      myRunDB $ update commentId [ CommentName =. name2
                                                                                 , CommentUrl  =. url2'
                                                                                 , CommentText =. text2
                                                                                 ]

                                       Nothing  -> print "boo"

showBlogPost i = do
    let entryId = Key $ PersistInt64 (fromIntegral i)
    entry    <- myRunDB $ get entryId

    case (entry :: Maybe Entry) of (Just e) -> do let content = show $ renderHtml $ entryContent e
                                                  printBlogPost entryId e

                                                  comments <- myRunDB $ selectList [CommentEntry ==. entryId] [] :: IO [Entity Comment]

                                                  forM_ comments (\c -> do let (Entity cid (Comment _ posted name email url text visible)) = c
                                                                               niceCommentId    = show $ foo $ unKey cid :: String
                                                                               niceName         = DT.unpack name
                                                                               niceEmail        = DT.unpack $ maybeTextToText email
                                                                               niceUrl          = DT.unpack $ maybeTextToText url
                                                                               niceText         = show $ lines $ DTL.unpack $ TBHRT.renderHtml text
                                                                               niceVisible      = if visible then "VISIBLE" else "HIDDEN"

                                                                           putStrLn $ "comment: " ++ show i ++ " " ++ niceCommentId ++ " " ++ niceVisible ++ " " ++ niceName ++ " " ++ niceText)
                                   Nothing  -> print "boo"
    where foo (PersistInt64 i) = i

deleteBlogPost i = do
    let entryId = Key $ PersistInt64 (fromIntegral i) :: KeyBackend Database.Persist.GenericSql.Raw.SqlBackend Entry
    myRunDB $ delete entryId

    comments <- myRunDB $ selectList [CommentEntry ==. entryId] [] :: IO [Entity Comment]

    forM_ (map entityToCommentId comments) (myRunDB . delete)

    where entityToCommentId (Entity cid (Comment {})) = cid

deleteComment i = do
    let commentId = Key $ PersistInt64 (fromIntegral i) :: KeyBackend Database.Persist.GenericSql.Raw.SqlBackend Comment
    myRunDB $ delete commentId

setEntryVisible :: Integer -> Bool -> IO ()
setEntryVisible i visible = myRunDB $ update (Key $ PersistInt64 (fromIntegral i)) [EntryVisible =. visible]

setCommentVisible :: Integer -> Bool -> IO ()
setCommentVisible i visible = myRunDB $ update (Key $ PersistInt64 (fromIntegral i)) [CommentVisible =. visible]

reportUnmoderatedComments = do
    comments <- myRunDB $ selectList [CommentVisible ==. False] [Desc CommentPosted]

    forM_ comments (\c -> do let (Entity cid (Comment eid posted name email url text visible)) = c
                                 niceEntryId      = show $ foo $ unKey eid :: String
                                 niceCommentId    = show $ foo $ unKey cid :: String
                                 niceName         = DT.unpack name
                                 niceEmail        = DT.unpack $ maybeTextToText email
                                 niceUrl          = DT.unpack $ maybeTextToText url
                                 niceText         = show $ lines $ DTL.unpack $ TBHRT.renderHtml text
                                 niceVisible      = if visible then "VISIBLE" else "HIDDEN"

                             putStrLn $ niceEntryId ++ " " ++ niceCommentId ++ " " ++ niceVisible ++ " " ++ niceName ++ " " ++ niceEmail ++ " " ++ niceUrl ++ " " ++ niceText)

    where foo (PersistInt64 i) = i

maybeTextToText (Just t) = t    -- FIXME something like this is surely standard.
maybeTextToText Nothing  = DT.pack ""

go :: [String] -> IO ()
go ["--list"] = listBlogPosts
go ["--dump"] = dumpBlogPosts

go ["--show", entryId]      = showBlogPost (read entryId)
go ["--edit-post", entryId]      = editBlogPost (read entryId)
go ["--edit-comment", commentId]      = editComment (read commentId)
go ["--delete-post", entryId]    = deleteBlogPost (read entryId)
go ["--delete-comment", commentId]    = deleteComment (read commentId)

go ["--add", title]         = do x <- addBlogPostFromEditor (DT.pack title)
                                 print x -- FIXME tidy this up

go ["--add-from-stdin", title, year, month, day]  = do x <- addBlogPostFromStdin (DT.pack title) (read year) (read month) (read day)
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
main = getArgs >>= go
