{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Main where

import qualified Data.ByteString.Char8         as B8
import           Text.Regex.PCRE.Heavy
import qualified Data.Set                      as Set
import           Data.List
import qualified Data.Map                      as Map
import           System.Exit

-- if the file has RCIs, gather all of them into a list
-- otherwise return the empty list

data FileWithRci = FileWithRci B8.ByteString [B8.ByteString] Bool

extractFile :: FileWithRci -> B8.ByteString
extractFile (FileWithRci f _ _) = f

extractRcis :: FileWithRci -> [B8.ByteString]
extractRcis (FileWithRci _ rcis _) = rcis

fileIsRciDef :: FileWithRci -> Bool
fileIsRciDef (FileWithRci _ _ b) = b

whiteList :: Set.Set B8.ByteString
whiteList = Set.fromList [B8.pack "RCI9999999"]

gatherRciDefs :: B8.ByteString -> IO (FileWithRci)
gatherRciDefs filePath = do
  file <- B8.readFile fp
  let lines' = B8.lines file
  if (fileHasRci file)
    then return $ FileWithRci filePath (gatherRci lines') True
    else return $ FileWithRci filePath (gatherTargetRci lines') False
  where fp = B8.unpack filePath
        fileHasRci = B8.isInfixOf $ B8.pack "implements RootCause"

-- takes in a List of lines and checks each one for an RCI code
-- may return 0 to n matches per line, which get flattened in
-- the return
gatherRci :: [B8.ByteString] -> [B8.ByteString]
gatherRci lines' = concat maybeRcis
 where
  getRcis = scan [re|[A-Z]{2,5}\d{4,10}|]
  maybeRcis =
    map ((map fst) . getRcis) lines'

gatherTargetRci :: [B8.ByteString] -> [B8.ByteString]
gatherTargetRci lines' = gatherRci validLines
  where validLines = filter (\l -> (not $ isImport l) && (not $ isComment l)) lines'
          where isImport l = B8.isPrefixOf (B8.pack "import") l
                isComment l = B8.isPrefixOf "* " stripped || B8.isPrefixOf "//" stripped
                  where stripped = B8.dropWhile (== ' ') l

countRciDupes :: [FileWithRci] -> Set.Set B8.ByteString -> [B8.ByteString]
countRciDupes targetFiles rcis = filter (flip Set.member $ rcis) dupes
  where extractedRcis = concat $ map extractRcis targetFiles
        frequency :: (Ord a) => [a] -> [(a, Int)]
        frequency xs = Map.toList (Map.fromListWith (+) [ (x, 1) | x <- xs ])
        dupes =  map fst (filter ((>=2) . snd) $ frequency extractedRcis)

  -- need to filter out files with "/test/" and "/it/"
isTestFilePath :: B8.ByteString -> Bool
isTestFilePath filePath =
  test `B8.isInfixOf` filePath || it `B8.isInfixOf` filePath
 where
  test = B8.pack "/test/"
  it   = B8.pack "/it/"

main :: IO ()
main = do
  input <- B8.getContents
  let files = B8.lines input
  filesAndRciDefs <-  mapM gatherRciDefs files

  let rciDefFiles = filter fileIsRciDef filesAndRciDefs
  let setRcis = Set.fromList $ concat (map extractRcis rciDefFiles)

  let targetFiles = filter (\f -> not (isTestFilePath (extractFile f)) && not (fileIsRciDef f)) filesAndRciDefs
  let duplicates = countRciDupes targetFiles setRcis
  let whiteListedDuplicates = filter (flip Set.notMember $ whiteList) duplicates
  if (null whiteListedDuplicates)
    then do
      putStrLn $ "No duplicate RCIs"
    else do
      putStrLn $ "Duplicate RCIs:\n" <> intercalate
        "\n"
        (map (B8.unpack) whiteListedDuplicates)
      exitFailure
