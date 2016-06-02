module Main where

import System.Environment
import System.FilePath
import Language.Haskell.Exts.Annotated
import qualified Data.Map.Strict as M
import Control.Monad

import Lib

type Info = Int

desugarModuleHead :: Info

-- 如果我希望， Maybe 有東西時，我會得到新的 info ，沒有的時候得到舊的，
-- List 有東西時， info 會一個一個傳進去並加下去，空的時候得到舊的，我該怎麼做？
-- 我該重寫整個 Info 成為一個 Monad 嗎？
-- haskell-src-exts 的 Syntax 看來是 Traversable 的，這幫得上我的忙嗎？
-- source 看起來只有 `deriving (Traversable)` ，這樣做了什麼？又沒做什麼？
--
-- 7 年前， GHC 開始可以 deriving Traversable ：
--   https://ghc.haskell.org/trac/ghc/ticket/2953
-- 實作在：
--   https://github.com/ghc/ghc/commit/1c15bee5a8fc004c16693d7d7a2d95b442549b66
-- 後來清理過：
--   https://github.com/ghc/ghc/commit/a82956df5b34175410e0feb9e2febe7d39b60b49
desugarModule :: Info -> Module -> (Info, Module)
desugerModule info (Module l mModuleHead lModulePragma lImportDecl lDecl) =
  let
    (info', _) = case mModuleHead of
      Just moduleHead -> desugarModuleHead info moduleHead
      Nothing -> (info, Nothing)



toModPath :: String -> String -> String
toModPath str basePath = basePath ++ [pathSeparator] ++ map go str where
  go '.' = pathSeparator
  go c = c

readModule :: String -> String -> IO (Maybe (Module SrcSpanInfo))
readModule modName basePath = do
  content <- readFile $ toModPath modName basePath ++ (extSeparator : "hs")
  let res = parseModule content
  putStrLn $ modName ++ ": " ++ show res
  return $ case res of
    ParseOk mod -> Just mod
    ParseFailed _ msg -> Nothing

collectModule :: IO (M.Map String (Module SrcSpanInfo)) -> Module SrcSpanInfo -> String -> IO (M.Map String (Module SrcSpanInfo))
collectModule ioMap mod basePath =
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
                mMod <- readModule name basePath
                case mMod of
                  Just mm -> collectModule acc mm basePath
                  Nothing -> acc
      go (return map'') imports
    _ -> ioMap

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
  let res = parseModule inputStr
  allMods <- case res of
    ParseOk mod -> collectModule (return M.empty) mod basePath
    ParseFailed _ msg -> return M.empty
  putStrLn $ show $ fmap (fmap (const ())) allMods
