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
gatherJavaRcis :: B8.ByteString -> IO ([B8.ByteString])
gatherJavaRcis filePath = do
  file <- B8.readFile fp
  let lines' = B8.lines file
  if (fileHasRci file) then return $ gatherRci lines' else return []
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

-- maps over each file to gather all the rcis and <> them into one big list
-- returns: a list of RCIs that are duplicates
gatherRciFromScala :: [B8.ByteString] -> Set.Set B8.ByteString -> IO [B8.ByteString]
gatherRciFromScala scalaFiles rcis = do
  scalaRcis <- mconcat <$> mapM gatherScalaRci scalaFiles
  let dupes =  map fst (filter ((>=2) . snd) $ frequency scalaRcis)
  return $ filter (flip Set.member $ rcis) dupes
  where
     -- use a map to store list elements and their count and convert back to a list
    frequency :: (Ord a) => [a] -> [(a, Int)]
    frequency xs = Map.toList (Map.fromListWith (+) [ (x, 1) | x <- xs ])

gatherScalaRci :: B8.ByteString -> IO [B8.ByteString]
gatherScalaRci filePath = do
  file <- B8.readFile $ fp
  let lines' =
        filter (\l -> (not $ isImport l) && (not $ isComment l)) $ B8.lines file
  return $ gatherRci lines'
 where
  fp = B8.unpack filePath
  isImport l = B8.isPrefixOf (B8.pack "import") l
  isComment l = B8.isPrefixOf "* " stripped || B8.isPrefixOf "//" stripped
    where stripped = B8.dropWhile (== ' ') l

endsWithScala :: B8.ByteString -> Bool
endsWithScala l = l =~ [re|\.scala\n?|]

endsWithJava :: B8.ByteString -> Bool
endsWithJava l = l =~ [re|\.java\n?|]

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
  let files     = B8.lines input
  let javaFiles = filter endsWithJava files
  rcis <- mapM gatherJavaRcis javaFiles
  let scalaFiles =
        filter (\fp -> endsWithScala fp && (not $ isTestFilePath fp)) files
  let setRcis = Set.fromList $ concat rcis
  duplicates <- gatherRciFromScala scalaFiles setRcis
  if (null duplicates)
    then do
      putStrLn $ "No duplicate RCIs"
    else do
      putStrLn $ "Duplicate RCIs:\n" <> intercalate
        "\n"
        (map (B8.unpack) duplicates)
      exitFailure
