module Main where

import Codec.Utils
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Internal (c2w, w2c)
import Data.Digest.MD5
import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..))
import qualified Data.Set as S
import Prelude hiding (foldl)
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.Posix.Files (getFileStatus, isDirectory)
import System.FilePath.Posix


-- data Flag = Verbose | Dir String
--   deriving (Show)
           
data Flags = Flags { flagVerbose :: Bool
                   , flagDir :: FilePath } deriving (Show)
                                                    
type FSForest = S.Set FSTree
data FSTree = File FilePath ![Octet] | Dir FilePath !FSForest
  deriving (Show)
           
data HashTree a = Leaf a ![Octet] | Bin a !(S.Set (HashTree a))

instance Eq FSTree where
  (File _ ha) == (File _ hb) = ha == hb
  (Dir _ dsa) == (Dir _ dsb) = dsa == dsb
  (File _ _) == (Dir _ _) = False
  (Dir _ _) == (File _ _) = False
  
instance Ord FSTree where
  (File _ ha) `compare` (File _ hb) = ha `compare` hb
  (Dir _ dsa) `compare` (Dir _ dsb) = dsa `compare` dsb
  (File _ _) `compare` (Dir _ _) = LT
  (Dir _ _) `compare` (File _ _) = GT

instance Foldable HashTree where
  foldMap f (Leaf a _) = f a
  foldMap f (Bin a forest) = f a `mappend` foldMap f forest

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
  print opts
  forest <- walkDir (flagDir opts)
  putStrLn "Done."
  
walkDir :: FilePath -> IO FSForest
walkDir fp = do
    putStrLn $ "Entering " ++ fp
    subs <- getDirectoryContents fp
    trees <- mapM (buildTree fp) $ 
             map (\s -> fp </> s) (filter (\s -> and [ s /= ".", s /= ".." ]) subs)
    return $ S.fromList trees
  where buildTree p s = do
          fstat <- getFileStatus s
          if isDirectory fstat
            then do trees <- walkDir s
                    return $ Dir s trees
            else do hash <- hashFile s
                    return $ File s hash
        hashFile s = withBinaryFile s ReadMode (\hdl -> do cnts <- LBS.hGetContents hdl
                                                           return $! hash . listToOctets $ map c2w (LBS.unpack cnts) )

-- | groupBy
findCommonDirs :: FSTree -> FSTree -> [FSForest]
findCommonDirs ta@(Dir tafp _) tb@(Dir tbfp tbfs) = let set = S.singleton tafp in
    if tb == ta then S.insert tbfp else S.union step ta tbfs 
  where step focus forest = let (same,diff) = S.partition (==focus) forest
                                set = S.fromList (map (\x -> case x of
                                                           (Dir fp _) -> fp
                                                           (File fp _) -> fp) same) in
                            S.unions [set , step focus (forestsOf diff) ]
        forestsOf :: [FSTree] -> [S.Set FSTree]
        forestsOf ts = map (\t -> case t of
                               (Dir fp for) -> for
                               (File _ _) -> S.empty ) ts
  


{- 

data Dir = Dir path Forest
instance Eq Dir where
 (Dir _ fa) == (Dir _ fb) = fa == fb

type DirSet = S.Set Dir

if member newDir dirSet
then 

-}