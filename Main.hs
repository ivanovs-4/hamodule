module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Fail (MonadFail)
import Data.Bool
import Data.Char
import Data.List
import Data.List.Split
import Data.String
import Options.Applicative as Opt
import System.Exit
import System.FilePath
import System.Process.Typed

import qualified Data.ByteString as BS

main :: IO ()
main = join $ customExecParser (prefs showHelpOnError) $ info (opts <**> helper)
        ( fullDesc
       <> progDesc "Automatically add line with module name to haskell file"
       )
  where
    opts :: Parser (IO ())
    opts = hsubparser (
         command "inplace" (info (hamoduleMain <$> hamoduleOps) fullDesc)
       )

data HamoduleOps = HamoduleOps {
  target :: FilePath
} deriving (Show)

hamoduleOps :: Parser HamoduleOps
hamoduleOps = do
    target <- Opt.argument str $
                metavar "TARGET_FILE"
    pure HamoduleOps {..}

hamoduleMain :: HamoduleOps -> IO ()
hamoduleMain HamoduleOps {..} = do
    ensure "Should be haskell file" $ takeExtension target == ".hs"
    r <- runProcess $ proc "perl" $
           (words "-0777 -i -pe")
        <> [
             "$M += "
          <> "s/^(module\\s)\\s*\\S+\\s*?(\\s?\\(|where)/$1" <> modname <> "$2/m"
          <> ";END{exit 1 unless $M>0}"
           , target
           ]
    when (r == ExitFailure 1) $ do
        b <- BS.readFile target
        BS.writeFile target . (<> b) . fromString $ "module " <> modname <> " where\n"
  where
    modname = moduleName target

moduleName :: String -> String
moduleName s = s
      & splitOneOf "/"
      & (reversed . ix 0 %~ dropExtension)
      & filter (not . null)
      & filter (\(head -> c) -> isAlpha c && isUpper c)
      & intercalate "."

ensure :: MonadFail m => String -> Bool -> m ()
ensure msg = bool (fail msg) (pure ())


-- perl = "perl -0777 -i -pe 's/(module\s)\s*\S+\s*?(\(|\s?where)/\1" <> "\2/'"
