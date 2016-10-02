module Main where

import Debug.Trace
import System.Environment
import System.FilePath
import Language.Haskell.Exts
import Codec.Archive.Zip
import Data.Char
import Data.ByteString.Lazy as B hiding (map, length, getContents, putStrLn)
import qualified Data.Map.Strict as M
import Control.Monad

import Lib
import Desugar



toModPath :: String -> String -> String
toModPath str basePath = basePath ++ [pathSeparator] ++ map go str where
  go '.' = pathSeparator
  go c = c

-- should I use `ExceptT String IO (Module SrcSpanInfo)`?
readModule :: ParseMode -> String -> String -> IO (Maybe (Module SrcSpanInfo))
readModule parseMode modName basePath = do
  let filename = toModPath modName basePath ++ (extSeparator : "hs")
  let newParseMode = parseMode { parseFilename = filename }
  content <- Prelude.readFile filename
  let res = parseModuleWithMode newParseMode content
  return $ case res of
    ParseOk mod -> Just mod
    ParseFailed _ msg -> trace msg Nothing

readModuleFromZip :: ParseMode -> String -> String -> String -> IO (Maybe (Module SrcSpanInfo))
readModuleFromZip parseMode modName basePath zipPath = do
  let filename = toModPath modName basePath ++ (extSeparator : "hs")
  let newParseMode = parseMode { parseFilename = filename }
  let zipname = basePath ++ [pathSeparator] ++ zipPath ++ (extSeparator : "zip")
  archive <- fmap toArchive $ B.readFile zipname
  return $ case findEntryByPath filename archive of
    Nothing -> trace ("module not found: " ++ filename ++ "@" ++ zipname) Nothing
    Just entry -> do
      let res = parseModuleWithMode newParseMode $ map (chr . fromIntegral) $ B.unpack $ fromEntry entry
      case res of
        ParseOk mod -> Just mod
        ParseFailed _ msg -> trace msg Nothing

collectModule :: ParseMode -> IO (M.Map String (Module SrcSpanInfo)) -> Module SrcSpanInfo -> String -> IO (M.Map String (Module SrcSpanInfo))
collectModule parseMode ioMap mod basePath =
  case mod of
    Module _ mModuleHead _ imports _ -> do
      let
        modName = case mModuleHead of
          Just (ModuleHead _ (ModuleName _ name) _ _) -> name
          Nothing -> "Main"
      map' <- ioMap
      let
        map'' = case M.member modName map' of
          False -> M.insert modName mod map'
          True -> map'
        go acc [] = acc
        go acc (m : ms) = go modMap ms where
          modMap = do
            let (ModuleName _ name) = importModule m
            case name of
              "Prelude" -> acc
              _ ->
                case importPkg m of
                  Nothing -> do
                    mMod <- readModule parseMode name basePath
                    case mMod of
                      Just mm -> collectModule parseMode acc mm basePath
                      Nothing -> acc
                  Just zipPath -> do
                    mMod <- readModuleFromZip parseMode name basePath zipPath
                    case mMod of
                      Just mm -> collectModule parseMode acc mm basePath
                      Nothing -> acc
      go (return map'') imports
    _ -> ioMap



myParseMode filename = ParseMode
  { parseFilename = filename
  , baseLanguage = Haskell2010
  , extensions = map EnableExtension
    [ PackageImports
    ]
  , ignoreLanguagePragmas = True
  , ignoreLinePragmas = False
  , fixities = Just preludeFixities
  , ignoreFunctionArity = True
  }

cleanupModule :: Module a -> Module ()
cleanupModule = fmap $ const ()

main :: IO ()
main = do
  args <- getArgs
  let
    argsWithDefaults = case length args of
      0 -> args ++ ["", ""]
      1 -> args ++ [""]
      _ -> args
    [basePath, packageName] = argsWithDefaults
  inputStr <- getContents
  let parseMode = myParseMode inputStr
  let res = parseModuleWithMode parseMode inputStr
  allMods <- case res of
    ParseOk mod -> collectModule parseMode (return M.empty) mod basePath
    ParseFailed _ msg -> return (trace msg M.empty)
  let cleanMods = fmap cleanupModule allMods
  putStrLn $ show $ (fmap desugar cleanMods :: M.Map String (Desugar (Module ())))
