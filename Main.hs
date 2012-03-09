module Main where

import Codec.Utils
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Internal (c2w, w2c)
import Data.Digest.MD5
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Posix.Files (getFileStatus, isDirectory, fileSize, fileAccess, isRegularFile)
import System.Posix.Types (FileOffset)
import System.FilePath.Posix
           
data Flags = Flags { flagVerbose :: Bool
                   , flagDir :: FilePath 
                   , flagFiles :: Bool 
                   , flagSize :: Bool 
                   , flagHelp :: Bool } deriving (Show)

type FSDupes = M.Map FSTree [FilePath]                                                    

type FSForest = S.Set FSTree
data FSTree = File FileOffset [Octet] 
            | Dir FSForest
  deriving (Show)
           
instance Eq FSTree where           
  (File sz1 hs1) == (File sz2 hs2) = and [ sz1 == sz2
                                         , hs1 == hs2 ]
  (Dir for1) == (Dir for2) = for1 == for2                                     
  _ == _ = False

-- I wish I didn't need this
instance Ord FSTree where  
  (File sz1 hs1) `compare` (File sz2 hs2) = (sz1,hs1) `compare` (sz2,hs2)
  (Dir hs1) `compare` (Dir hs2) = hs1 `compare` hs2
  (File _ _) `compare` (Dir _) = LT
  (Dir _) `compare` (File _ _) = GT

defaultOptions = Flags { flagVerbose = False                                                    
                       , flagDir = "." 
                       , flagFiles = False
                       , flagSize = False
                       , flagHelp = False }

options :: [OptDescr (Flags -> Flags)]
options = [ Option ['d'] ["directory"] (ReqArg (\d fs -> fs { flagDir = d } ) "DIR") "top directory"
          , Option ['v'] ["verbose"]   (NoArg (\fs -> fs { flagVerbose = True } ) ) "be verbose" 
          , Option ['f'] ["files"]     (NoArg (\fs -> fs { flagFiles = True } ) ) "also display duplicate files"
          , Option ['s'] ["size-only"] (NoArg (\fs -> fs { flagSize = True } ) ) "compare files by size only"
          , Option ['h'] ["help"]      (NoArg (\fs -> fs { flagHelp = True } ) ) "display help" ]
          
parseArgs argv =          
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> error (usageInfo "invalid option(s)" options)


main = do
  args <- getArgs
  (opts,nonOpts) <- parseArgs args
  when (flagHelp opts) (error $ usageInfo "help" options)
  tree <- walkDir (flagDir opts) (flagVerbose opts) (flagSize opts)
  let dupes = if flagFiles opts
              then M.elems $ M.filter (\v -> length v > 1) tree
              else M.elems $ M.filterWithKey (\k v -> case k of
                                                 (File _ _) -> False
                                                 (Dir _) -> length v > 1) tree
  mapM print dupes
  
walkDir :: FilePath -> Bool -> Bool -> IO FSDupes
walkDir fp oVerbose oSize = step M.empty fp
  where step dupes f = do
          cfp <- canonicalizePath f
          fs <- getFileStatus cfp
          isR <- fileAccess cfp True False False
          isRX <- fileAccess cfp True False True
          if and [isDirectory fs, isRX]
            then do when oVerbose (putStrLn $ "Entering " ++ cfp)
                    subs <- getDirectoryContents cfp >>= 
                            mapM (canonicalizePath . (cfp </>)) . filter (not . flip elem [".",".."])
                    subdupes <- mapM (step M.empty) subs
                    when oVerbose (putStrLn $ "Leaving " ++ cfp)
                    return $! M.unionsWith (++) (M.singleton (Dir (S.fromList (concatMap M.keys subdupes))) [cfp]
                                                 : dupes : subdupes)
            else if and [isRegularFile fs, isR]
                   then do fh <- hashFile cfp (fileSize fs)
                           return $! M.singleton (File (fileSize fs) fh) [cfp]
                   else do when oVerbose (putStrLn $ "Skipping: " ++ cfp)
                           return M.empty
        hashFile s sz = if oSize 
                     then return [read . show $ sz]
                     else withBinaryFile s ReadMode                     
                          (\hdl -> do when oVerbose (putStrLn $ "Hashing " ++ s)
                                      cnts <- LBS.hGetContents hdl
                                      return $! hash . listToOctets $ map c2w (LBS.unpack cnts) )
