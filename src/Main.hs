module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = Test.SimpleTest.Mock.writeFile "snippets.ben" (DB.serialize DB.empty)

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  DB.load >>= handleGetHelperFunc 
  where
    handleGetHelperFunc databaseAux =
        case databaseAux of 
            Success okDb ->
               case DB.findFirst predicate okDb of
                  Just val -> 
                    putStrLn $ entrySnippet val
                  Nothing -> 
                    putStrLn "Nothing"
            Error error ->
               putStrLn "Failed to load DB"
    predicate prediEntry = entryId prediEntry == id
    id = getOptId getOpts

displayTheEntries :: TestableMonadIO m => [Entry] -> m ()
displayTheEntries entries =
  case entries of 
    [] -> 
      return ()
    x:xs ->
      putStrLn (show (FmtEntry x)) >> displayTheEntries xs 

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
   DB.load >>= handleSearchHelperFunc 
   where
     handleSearchHelperFunc databaseAux =
        case databaseAux of
          Success valEntr ->
              let
                 allEntriesSearch = DB.findAll(\auxEntry -> Entry.Entry.matchedByAllQueries (searchOptTerms searchOpts) auxEntry) <$> databaseAux
              in 
                case allEntriesSearch of
                  Success val 
                    | val == [] -> 
                      putStrLn "No entries found"
                    | otherwise ->
                       displayTheEntries val                      
                  _ ->
                     putStrLn "No entries found"
          Error error ->
             putStrLn "Failed to load DB"

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  fileContentToBeAdded <- readFile (addOptFilename addOpts)
  databaseAux <- DB.load
  
  case databaseAux of 
    Success val ->
      let 
        modifiedDatabase = DB.insertWith (\idToBeAdded -> makeNewDBEntry idToBeAdded fileContentToBeAdded addOpts) databaseEmpty where databaseEmpty = getSuccess databaseAux DB.empty
        doesEntryExist = DB.findFirst(\entryContent -> entrySnippet entryContent == fileContentToBeAdded) (getSuccess databaseAux DB.empty)
      in
        case doesEntryExist of
          Just valueEnt ->
            Prelude.mapM_ putStrLn (["Entry with this content already exists: ", (show (FmtEntry valueEnt))])
          _ -> do
            DB.save modifiedDatabase
            return ()
    _ ->
       putStrLn "Failed to load DB"
  return ()
  where
    makeNewDBEntry :: Int -> String -> AddOptions -> Entry
    makeNewDBEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
