module Main where

import Codec.Utils
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Internal (c2w, w2c)
import Data.Digest.MD5
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Posix.Files (getFileStatus, isDirectory)
import System.FilePath.Posix
           
data Flags = Flags { flagVerbose :: Bool
                   , flagDir :: FilePath } deriving (Show)

type FSDupes = M.Map FSTree [FilePath]                                                    

type FSForest = S.Set FSTree
data FSTree = File ![Octet] | Dir !FSForest
  deriving (Show, Eq, Ord)
  
{- TODO:
 * --size-only (instead of hashing)
 * make verbose work
 * "mostly-same" dirs
-}

defaultOptions = Flags { flagVerbose = False                                                    
                       , flagDir = "." }

options :: [OptDescr (Flags -> Flags)]
options = [ Option ['d'] ["directory"] (ReqArg (\d flags -> flags { flagDir = d } ) "DIR") "top directory"
          , Option ['v'] ["verbose"]   (NoArg (\flags -> flags { flagVerbose = True } ) ) "be verbose" ]
          
parseArgs argv =          
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: dupdirdetect -d <dir>"

main = do
  args <- getArgs
  (opts,nonOpts) <- parseArgs args
  dupes <- walkDir (flagDir opts)
  print $ M.elems $ M.filter (\v -> length v > 1) dupes
  
walkDir :: FilePath -> IO FSDupes
walkDir fp = step M.empty fp
  where step dupes f = do
          cfp <- canonicalizePath f
          isDir <- doesDirectoryExist cfp
          if not isDir 
            then do putStrLn $ "Hashing " ++ cfp
                    fh <- hashFile cfp
                    return $! M.singleton (File fh) [cfp]
            else do putStrLn $ "Entering " ++ cfp
                    subs <- getDirectoryContents cfp >>= 
                            mapM (canonicalizePath . (cfp </>)) . filter (not . flip elem [".",".."])
                    subdupes <- mapM (step M.empty) subs
                    putStrLn $ "Leaving " ++ cfp
                    return $! M.unionsWith (++) (M.singleton (Dir (S.fromList (concatMap M.keys subdupes))) [cfp]
                                                : dupes : subdupes)
        hashFile s = withBinaryFile s ReadMode                     
                     (\hdl -> do cnts <- LBS.hGetContents hdl
                                 return $! hash . listToOctets $ map c2w (LBS.unpack cnts) )
