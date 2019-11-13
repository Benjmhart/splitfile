{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import ClassyPrelude
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified System.IO as SIO
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import System.Exit (die)

data FileStats = FileStats 
  { fileLines :: V.Vector Text
  , fileName :: FilePath
  , fileExt :: FilePath
  , maxFileLines :: Int
  }
  deriving (Eq, Read, Show)

splitFileMain :: IO ()
splitFileMain = do
  putStrLn . pack $ "splitfile initializing..."
  fileStats <- processArgs
  let dirname = unpack (fileName fileStats)
  Dir.createDirectory dirname
  putStrLn . pack $ "writing files to ./" <> dirname 
  repackFiles fileStats



processArgs ::  IO FileStats
processArgs = do
  (name, size) <- parseArgs
  fileContents <- V.fromList . lines <$> readFileUtf8 (unpack name)
  let 
    basename = withText FP.takeBaseName name
    extension = withText FP.takeExtension name
  return (FileStats fileContents basename extension size)

parseArgs :: IO (Text, Int)
parseArgs = do
    mNameSize <- parseArgs' <$> getArgs
    maybe (die "Supply a filepath and a number as args") pure mNameSize
  

withText f = pack . f . unpack

parseArgs' :: [Text] -> Maybe (Text, Int)
parseArgs' []     = Nothing
parseArgs' (x:[]) = Nothing
parseArgs' (x:y:_)  = 
  let maynum = readMay y in
  case maynum of
    Nothing -> Nothing
    Just n -> Just (x,n)

repackFiles :: FileStats -> IO ()
repackFiles (FileStats fLines name ext maxlines) 
  = go (V.take 1 fLines) (V.drop 1 fLines) 1
  where 
    go :: Vector Text -> Vector Text  -> Int -> IO ()
    go l1 ls n 
      = do
        let 
          destPath = name </> (name <> "-" <> (show n) <.> ext)
          destContent = unlines . V.toList $ (l1 <> (V.take maxlines ls))
        writeFileUtf8 destPath destContent
        let 
          nextLs = V.drop maxlines ls
        case V.length nextLs of
            0 -> do
                putStrLn . pack $ "Operation completed, wrote " <> (show n) <> " files."
                return ()
            _ -> go l1 nextLs (n + 1)


