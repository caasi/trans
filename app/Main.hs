module Main where

import Debug.Trace
import System.Environment
import System.FilePath
import Language.Haskell.Exts.Annotated
import qualified Data.Map.Strict as M
import Control.Monad

import Lib
import Desugar



toModPath :: String -> String -> String
toModPath str basePath = basePath ++ [pathSeparator] ++ map go str where
  go '.' = pathSeparator
  go c = c

readModule :: ParseMode -> String -> String -> IO (Maybe (Module SrcSpanInfo))
readModule parseMode modName basePath = do
  let filename = toModPath modName basePath ++ (extSeparator : "hs")
  let newParseMode = parseMode { parseFilename = filename }
  content <- readFile filename
  let res = parseModuleWithMode newParseMode content
  return $ case res of
    ParseOk mod -> Just mod
    ParseFailed _ msg -> Nothing

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
              _ -> do
                mMod <- readModule parseMode name basePath
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
  let cleanMods = (fmap . fmap) (const ()) allMods
  putStrLn $ show $ (fmap desugar cleanMods :: M.Map String (Desugar (Module ())))
