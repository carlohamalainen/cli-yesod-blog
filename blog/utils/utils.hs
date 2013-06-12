-- imports copied from app/main.hs
import Prelude              (IO, print, head, readFile, writeFile)
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

sanitiseTitle :: Text -> Text
sanitiseTitle = DT.pack . nukeNonAlNum . DT.unpack . dashes . lowerCase
    where dashes        = DT.intercalate (DT.pack "-") . DT.words
          lowerCase     = DT.toLower
          nukeNonAlNum  = catMaybes . map (\c -> if c == '-' || isAlphaNum c then Just c else Nothing)

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

listBlogPosts = do
    posts <- myRunDB $ selectList [] [] :: IO [Entity Entry]

    forM_ posts (\e -> do let (Entity eid (Entry title mashedTitle year month day content visible)) = e
                              niceEntryId = show $ foo $ unKey $ eid :: String
                              niceURL = DT.unpack $ DT.intercalate (DT.pack "/") (map DT.pack [show year, show month, show day, DT.unpack mashedTitle])
                              niceTitle = DT.unpack title
                              niceContent = take 50 $ DT.unpack $ DT.replace (DT.pack "\n") (DT.pack " ... ") $ content
                              niceVisible = if visible then "VISIBLE" else "HIDDEN"
                          putStrLn $ niceEntryId ++ " " ++ niceVisible ++ " " ++ niceURL ++ " " ++ " '" ++ niceTitle ++ "' " ++ niceContent
                )

    where foo (PersistInt64 i) = i

dumpBlogPosts = do
    posts <- myRunDB $ selectList [] [] :: IO [Entity Entry]
    forM_ posts print

deleteAllBlogPosts = do
    entries <- myRunDB $ selectList [] [] :: IO [Entity Entry]

    forM_ (map entityToEntryId entries) (myRunDB . delete)

    where entityToEntryId (Entity eid (Entry _ _ _ _ _ _ _)) = eid


getYMD :: UTCTime -> (Int, Int, Int)
getYMD utc = (fromInteger y, m, d)
    where (y, m, d) = toGregorian $ utctDay utc

makeFakeBlogPosts = do
    (year1, month1, day1) <- liftM getYMD getCurrentTime
    let e1 = Entry (DT.pack "first post") (sanitiseTitle $ DT.pack "first post") year1 month1 day1 (DT.pack "Hi there!") False

    (year2, month2, day2) <- liftM getYMD getCurrentTime
    let e2 = Entry (DT.pack "second post") (sanitiseTitle $ DT.pack "second post") year2 month2 day2 (DT.pack "Hi there! Do de dah!") False

    forM_ [e1, e2] (myRunDB . insert)

addBlogPostFromEditor title = do
    r <- system "vim /tmp/blah.html"
    content <- liftM DT.pack $ readFile "/tmp/blah.html"

    (year, month, day) <- liftM getYMD getCurrentTime

    myRunDB $ insert (Entry title (sanitiseTitle title) year month day content False)

editBlogPost i = do
    let entryId = Key $ PersistInt64 (fromIntegral i)
    entry <- myRunDB $ get entryId

    case (entry :: Maybe Entry) of (Just e) -> do let content1 = DT.unpack $ entryContent e
                                                  writeFile "/tmp/blah.html" content1

                                                  r <- system "vim /tmp/blah.html"
                                                  content2 <- liftM DT.pack $ readFile "/tmp/blah.html"
                                                  myRunDB $ update entryId [EntryContent =. content2]
                                   Nothing  -> print "boo"

deleteBlogPost i = do
    let entryId = Key $ PersistInt64 (fromIntegral i) :: KeyBackend Database.Persist.GenericSql.Raw.SqlBackend Entry
    myRunDB $ delete entryId

setEntryVisible :: Integer -> Bool -> IO ()
setEntryVisible i visible = myRunDB $ update (Key $ PersistInt64 (fromIntegral i)) [EntryVisible =. visible]


go :: [String] -> IO ()
go ["--list"] = listBlogPosts
go ["--dump"] = dumpBlogPosts

go ["--edit", entryId]      = editBlogPost (read entryId)
go ["--delete", entryId]    = deleteBlogPost (read entryId)

go _ = do
    putStrLn "Usage:"
    putStrLn ""

main :: IO ()
main = getArgs >>= go
