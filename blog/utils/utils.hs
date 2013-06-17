-- imports copied from app/main.hs
import Prelude              (IO, print, head, tail, readFile, writeFile)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
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
import Database.Persist.Store (PersistValue(PersistInt64))
import Database.Persist.GenericSql.Raw (SqlBackend)
import qualified Data.Text as DT
import System.Process
import Data.Maybe
import Data.Char
import System.Environment ( getArgs )
import System.IO ( openFile, getContents )
import System.IO ( IOMode( ReadMode ) )
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

sanitiseTitle :: Text -> Text
sanitiseTitle = DT.pack . nukeNonAlNum . (map hack) . DT.unpack . dashes . lowerCase
    where dashes        = DT.intercalate (DT.pack "-") . DT.words
          lowerCase     = DT.toLower
          nukeNonAlNum  = catMaybes . map (\c -> if c == '-' || isAlphaNum c then Just c else Nothing)
          hack 'ň' = 'n'
          hack 'é' = 'e'
          hack c   = c

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
    conf <- Yesod.Default.Config.loadConfig $ (configSettings Development) { csParseExtra = parseExtra }

    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)

    runNoLoggingT $ runResourceT $ Database.Persist.Store.runPool dbconf f p

printBlogPost eid (Entry title mashedTitle year month day content visible) = do
    let niceEntryId = show $ foo $ unKey $ eid :: String
        niceURL = DT.unpack $ DT.intercalate (DT.pack "/") (map DT.pack [show year, show month, show day, DT.unpack mashedTitle])
        niceTitle = DT.unpack title
        niceContent = DTE.decodeUtf8With DTEE.lenientDecode $ BS.concat . BSL.toChunks $ renderHtml content -- Html converted to Text
        niceContent' = take 50 $ DT.unpack $ DT.replace (DT.pack "\n") (DT.pack " ... ") $ niceContent -- rip out newlines, take first 50 characters
        niceVisible = if visible then "VISIBLE" else "HIDDEN"

    putStrLn $ niceEntryId ++ " " ++ niceVisible ++ " " ++ niceURL ++ " " ++ " '" ++ niceTitle ++ "' " ++ niceContent'

    where foo (PersistInt64 i) = i

printBlogPostEntity (Entity eid entry) = printBlogPost eid entry

listBlogPosts = do
    posts <- myRunDB $ selectList [] [] :: IO [Entity Entry]

    forM_ posts printBlogPostEntity

dumpBlogPosts = do
    posts <- myRunDB $ selectList [] [] :: IO [Entity Entry]
    forM_ posts print

    comments <- myRunDB $ selectList [] [] :: IO [Entity Comment]
    forM_ comments print

    return ()

deleteAllBlogPosts = do
    entries <- myRunDB $ selectList [] [] :: IO [Entity Entry]

    forM_ (map entityToEntryId entries) (myRunDB . delete)

    where entityToEntryId (Entity eid (Entry _ _ _ _ _ _ _)) = eid


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
    content <- liftM toHtml $ readFile "/tmp/blah.html"

    (year, month, day) <- liftM getYMD getCurrentTime

    myRunDB $ insert (Entry title (sanitiseTitle title) year month day content False)

-- For importing entries; requires year, month, day to be specified.
addBlogPostFromStdin title year month day = do
    content <- liftM toHtml $ getContents
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

    let commentAuthor   = DT.pack $ x !! 0
        commentDate     = read (x !! 1) :: UTCTime
        commentText     = Textarea $ DT.pack $ unlines $ drop 2 x

        visible = False

    c <- myRunDB $ insert (Comment entryId commentDate commentAuthor commentText visible)
    print c

getCommentFiles fileName = liftM (DL.sort . commentFile . addDirectoryPrefix . keepOurs) directoryContents
    where directoryContents = getDirectoryContents (takeDirectory fileName) :: IO [FilePath]
          baseName = dropExtension $ takeFileName fileName  :: FilePath
          keepOurs = filter (DL.isPrefixOf baseName)        :: [FilePath] -> [FilePath]
          commentFile = filter (DL.isInfixOf "comment")     :: [FilePath] -> [FilePath]
          addDirectoryPrefix = map ((takeDirectory fileName) </>) :: [FilePath] -> [FilePath]

editBlogPost i = do
    let entryId = Key $ PersistInt64 (fromIntegral i)
    entry <- myRunDB $ get entryId

    case (entry :: Maybe Entry) of (Just e) -> do let content1 = renderHtml $ entryContent e
                                                  BSL.writeFile "/tmp/blah.html" content1

                                                  r <- system "vim /tmp/blah.html"
                                                  content2 <- liftM (toHtml . TB.preEscapedToMarkup) $ readFile "/tmp/blah.html"
                                                  myRunDB $ update entryId [EntryContent =. content2]
                                   Nothing  -> print "boo"

editComment cid = do
    let commentId = Key $ PersistInt64 (fromIntegral cid)

    comment <- myRunDB $ get commentId

    case (comment :: Maybe Comment) of (Just c) -> do let name1 = DT.unpack $ commentName c
                                                          text1 = DT.unpack $ unTextarea $ commentText c
                                                      writeFile "/tmp/blah.html" (name1 ++ "\n" ++ text1) -- FIXME use a temp file

                                                      r <- system "vim /tmp/blah.html"
                                                      x <- readFile "/tmp/blah.html"

                                                      let name2 = DT.pack $ head $ lines x
                                                          text2 = Textarea (DT.pack $ unlines $ drop 1 $ lines x)

                                                      myRunDB $ update commentId [ CommentName =. name2
                                                                                 , CommentText =. text2
                                                                                 ]

                                       Nothing  -> print "boo"

showBlogPost i = do
    let entryId = Key $ PersistInt64 (fromIntegral i)
    entry    <- myRunDB $ get entryId

    case (entry :: Maybe Entry) of (Just e) -> do let content = show $ renderHtml $ entryContent e
                                                  printBlogPost entryId e

                                                  comments <- myRunDB $ selectList [CommentEntry ==. entryId] [] :: IO [Entity Comment]

                                                  forM_ comments (\c -> do let (Entity cid (Comment _ posted name text visible)) = c
                                                                               niceCommentId    = show $ foo $ unKey $ cid :: String
                                                                               niceName         = DT.unpack name
                                                                               niceText         = show $ lines $ DT.unpack $ unTextarea text
                                                                               niceVisible      = if visible then "VISIBLE" else "HIDDEN"

                                                                           putStrLn $ "comment: " ++ (show i) ++ " " ++ niceCommentId ++ " " ++ niceVisible ++ " " ++ niceName ++ " " ++ niceText)
                                   Nothing  -> print "boo"
    where foo (PersistInt64 i) = i

deleteBlogPost i = do
    let entryId = Key $ PersistInt64 (fromIntegral i) :: KeyBackend Database.Persist.GenericSql.Raw.SqlBackend Entry
    myRunDB $ delete entryId

    comments <- myRunDB $ selectList [CommentEntry ==. entryId] [] :: IO [Entity Comment]

    forM_ (map entityToCommentId comments) (myRunDB . delete)

    where entityToCommentId (Entity cid (Comment _ _ _ _ _)) = cid

setEntryVisible :: Integer -> Bool -> IO ()
setEntryVisible i visible = myRunDB $ update (Key $ PersistInt64 (fromIntegral i)) [EntryVisible =. visible]

setCommentVisible :: Integer -> Bool -> IO ()
setCommentVisible i visible = myRunDB $ update (Key $ PersistInt64 (fromIntegral i)) [CommentVisible =. visible]

go :: [String] -> IO ()
go ["--list"] = listBlogPosts
go ["--dump"] = dumpBlogPosts

go ["--show", entryId]      = showBlogPost (read entryId)
go ["--edit-post", entryId]      = editBlogPost (read entryId)
go ["--edit-comment", commentId]      = editComment (read commentId)
go ["--delete", entryId]    = deleteBlogPost (read entryId)

go ["--add", title]         = do x <- addBlogPostFromEditor (DT.pack title)
                                 print x -- FIXME tidy this up

go ["--add-from-stdin", title, year, month, day]  = do x <- addBlogPostFromStdin (DT.pack title) (read year) (read month) (read day)
                                                       print x -- FIXME tidy this up

go ["--add-from-file", fileName]  = addBlogPostFromFile fileName

go ["--set-post-visible", i]     = setEntryVisible (read i) True
go ["--set-post-invisible", i]   = setEntryVisible (read i) False

go ["--set-comment-visible", i]     = setCommentVisible (read i) True
go ["--set-comment-invisible", i]   = setCommentVisible (read i) False

go _ = do
    putStrLn "Usage:"
    putStrLn ""

main :: IO ()
main = getArgs >>= go
