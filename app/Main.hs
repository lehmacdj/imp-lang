{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Trans.Class
import Data.List (intercalate)
import Data.Version
import GHC.Generics
import Language.IMP
import Options.Applicative
import System.Console.Haskeline

type Repl = StateT Env (InputT IO)

replPrint :: String -> Repl ()
replPrint = lift . lift . putStrLn

putAndPrint :: (String, Maybe Env) -> Repl ()
putAndPrint (str, env) = do
  replPrint str
  maybe (pure ()) put env

evalLine :: String -> Repl (Either String (String, Maybe Env))
evalLine line = do
  env <- get
  res <-
    pure $
      foldl1
        (<|>)
        [ fmap ((show &&& Just) . evalCommand env) (parseCommand line),
          fmap ((,Nothing) . (++ " : BExp") . show . evalBExp env) (parseBExp line),
          fmap ((,Nothing) . (++ " : AExp") . show . evalAExp env) (parseAExp line)
        ]
  pure res

repl :: Repl ()
repl = do
  line <- lift $ getInputLine "imp> "
  case mfilter (/= ":q") line of
    Nothing -> replPrint "Goodbye!" *> pure ()
    Just line' -> do
      result <- evalLine line'
      either replPrint putAndPrint result
      repl

data Options
  = InterpretFile FilePath
  | Interactive
  | ShowVersion
  deriving (Show, Eq, Ord, Generic)

optionsP :: Parser Options
optionsP =
  flag'
    ShowVersion
    ( long "version" <> help "display the version of imp this interpreter interprets and then quit"
    )
    <|> ( maybe Interactive InterpretFile
            <$> optional
              ( strArgument
                  ( metavar "<file>"
                      <> help "file to interpret, if not specified runs interactively"
                  )
              )
        )

runWithOptions :: (Options -> IO ()) -> IO ()
runWithOptions mainFunc = do
  options <- execParser parser
  mainFunc options
  where
    parser =
      info
        (optionsP <**> helper)
        ( fullDesc
            <> progDesc "interpreter for imp programming language"
        )

versionString :: String
versionString = case impVersion of
  Version versionTiers _ -> intercalate "." $ show <$> versionTiers

main :: IO ()
main = runWithOptions $ \case
  InterpretFile filePath -> do
    parsed <- parseProgram filePath
    case parsed of
      Left err -> putStrLn err
      Right prog -> print $ evalCommand emptyEnv prog
  ShowVersion ->
    putStrLn $ "impi distributed via cabal package imp-" ++ versionString
  Interactive ->
    runInputT defaultSettings $ fst <$> runStateT repl emptyEnv
