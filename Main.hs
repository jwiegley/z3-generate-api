{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import           Control.Arrow (second)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Types hiding ((.=), Parser, Options, parse)
import           Data.C2Hsc
import           Data.Char
import           Data.Foldable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List
import           Data.List.Split
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Monoid
import           Data.Traversable
import           Data.Typeable
import qualified Data.Yaml as Y
import           Debug.Trace
import           GHC.Generics
import           Language.C.Data.Ident
import           Language.C.Data.InputStream
import           Language.C.Data.Node
import           Language.C.Data.Position
import           Language.C.Parser hiding (execParser)
import           Language.C.Pretty
import           Language.C.Syntax.AST
import           Language.C.System.GCC
import           Language.C.System.Preprocess
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import           System.IO.Temp
import           System.Process
import           Text.Parsec hiding ((<|>), many, option)

data Type
  = TUnit
  | TCUInt
  | TEntry String
  | TPtr Type
  | TIO Type
  | TFun [Type]

makePrisms ''Type

data Entry = Entry
  { _entryKind    :: String
  , _entryName    :: String
  -- , _entryType    :: [Type]
  -- , _entryArgs    :: [String]
  , _entryType    :: Maybe String
  , _entryDoxygen :: Maybe String
  }

instance Show Entry where
  show Entry {..} =
    "#" ++ _entryKind ++ " " ++ _entryName
      ++ case _entryType of
           Nothing  -> ""
           Just def -> " , " ++ def

makeLenses ''Entry

data Substitution = Substitution
  { _substKind    :: String
  , _substName    :: String
  , _substNewKind :: Maybe String
  , _substNewName :: Maybe String
  , _substNewType :: Maybe String
  }
  deriving (Generic, Typeable)

customSubstitution :: Aeson.Options
customSubstitution =
  defaultOptions { fieldLabelModifier = \x -> toLower (x !! 6) : drop 7 x }

instance ToJSON Substitution where
    toJSON     = genericToJSON customSubstitution
    toEncoding = genericToEncoding customSubstitution

instance FromJSON Substitution where
    parseJSON = genericParseJSON customSubstitution

makeLenses ''Substitution

data Options = Options
  { _version          :: String
  , _gitURI           :: String
  , _gitTag           :: String
  , _gitWorkdir       :: FilePath
  , _cPrefacePrimary  :: String
  , _cPrefaceRest     :: String
  , _hscPreface       :: String
  , _headersPath      :: FilePath
  , _headers          :: [FilePath]
  , _headerOutputPath :: FilePath
  , _substitutions    :: [Substitution]
  }
  deriving (Generic, Typeable)

customOptions :: Aeson.Options
customOptions = defaultOptions { fieldLabelModifier = tail }

instance ToJSON Options where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON Options where
    parseJSON = genericParseJSON customOptions

makeLenses ''Options

defOpts :: Options
defOpts = Options
  { _version          = error "Must specify version"
  , _gitURI           = "https://github.com/Z3Prover/z3"
  , _gitTag           = "z3-#{version}"
  , _gitWorkdir       = "z3-#{version}"
  , _cPrefacePrimary  = error "Must specify cPrefacePrimary"
  , _cPrefaceRest     = error "Must specify cPrefaceRest"
  , _hscPreface       = error "Must specify hsccPreface"
  , _headersPath      = "z3/src/api"
  , _headers          = error "Must specify headers to process"
  , _headerOutputPath = "#{header}.hsc"
  , _substitutions    = []
  }

generateOpts :: Options -> Parser Options
generateOpts opts = Options
    <$> strOption
        (   long "version"
         <> help "Z3 API version"
         <> value (opts^.version))
    <*> strOption
        (   long "git-uri"
            <> help "Git URI"
            <> value (opts^.gitURI))
    <*> strOption
        (   long "git-tag"
            <> help "Git tag pattern"
            <> value (opts^.gitTag))
    <*> strOption
        (   long "git-workdir"
            <> help "Directory to checkout the repository into"
            <> value (opts^.gitWorkdir))
    <*> strOption
        (   long "c-preface-primary"
            <> help "Preface to be parsed before z3_api.h"
            <> value (opts^.cPrefacePrimary))
    <*> strOption
        (   long "c-preface-rest"
            <> help "Preface to be parsed before each header except z3_api.h"
            <> value (opts^.cPrefaceRest))
    <*> strOption
        (   long "hsc-preface"
            <> help "Preface to be included at the top of each .hsc file"
            <> value (opts^.hscPreface))
    <*> strOption
        (   long "headers-path"
            <> help "Path to API headers within z3 repository"
            <> value (opts^.headersPath))
    <*> option parseList
        (   long "headers"
         <> help "List of comma-separated header names to process"
         <> value (opts^.headers))
    <*> strOption
        (   long "output"
            <> help "Pattern used to generate output header names"
            <> value (opts^.headerOutputPath))
    <*> pure (opts^.substitutions)
  where
    parseList :: ReadM [FilePath]
    parseList = eitherReader $ \v -> return (splitOn "," v)

optsDef :: Options -> ParserInfo Options
optsDef opts = info (helper <*> generateOpts opts)
  (fullDesc <> progDesc "" <> header "Haskell Z3 API Generator")

optionsHashMap :: Options -> HashMap String String
optionsHashMap Options {..} = M.empty &~ do
  at "version"          ?= _version
  at "git-uri"          ?= _gitURI
  at "git-tag"          ?= _gitTag
  at "git-workdir"      ?= _gitWorkdir
  at "cPrefacePrimary"  ?= _cPrefacePrimary
  at "cPrefaceRest"     ?= _cPrefaceRest
  at "hscPreface"       ?= _hscPreface
  at "headers-path"     ?= _headersPath
  at "headers"          ?= concat _headers
  at "headerOutputPath" ?= _headerOutputPath

interpolate :: HashMap String String -> String -> String
interpolate _ "" = ""

interpolate vars ('#':'{':xs) =
  case elemIndex '}' xs of
    Nothing -> error $ "Unclosed interpolation tag: #{" ++ xs
    Just i  ->
      let tag = take i xs in
      vars ^?! failing (ix tag) (error $ "Unknown options tag: " ++ tag)
        ++ interpolate vars (drop (i + 1) xs)

interpolate vars (x:xs) = x : interpolate vars xs

-- Given a 'file', mutate it and pass the mutated temp file to 'k'.
mutateHeader :: FilePath
             -> String
             -> (FilePath -> IO a)
             -> IO a
mutateHeader file preface k =
  withSystemTempFile "z3header.h" $ \temp handle -> do
    hPutStrLn handle preface
    withFile file ReadMode $ hPutStr handle <=< hGetContents
    hClose handle
    k temp

main :: IO ()
main = do
  args <- getArgs
  opts <- case elemIndex "--yaml" args of
    Nothing -> execParser (optsDef defOpts)
    Just i  -> do
      let path = args !! succ i
      putStrLn $ "Reading configuration options from " ++ path
      eres <- Y.decodeFileEither path
      case eres of
        Left err -> error $ "Error processing YAML file " ++ path
                        ++ ": " ++ show err
        Right opts  ->
          handleParseResult $
            execParserPure defaultPrefs (optsDef opts)
              (take i args ++ drop (i + 2) args)

  -- STEP 1
  --
  -- Clone requested API tag of Z3 from GitHub, in order to have access to
  -- the C header files.
  --
  --   git clone https://github.com/Z3Prover/z3 z3-4.5.0

  let optVars = optionsHashMap opts
      interp  = interpolate optVars
      workdir = interp (opts^.gitWorkdir)
  b <- doesDirectoryExist workdir
  unless b $ do
    mgit <- findExecutable "git"
    case mgit of
      Nothing -> error "Cannot find 'git'!"
      Just git -> do
        let cmd = git ++ " clone --depth 1 --branch "
                      ++ interp (opts^.gitTag) ++ " "
                      ++ interp (opts^.gitURI) ++ " "
                      ++ workdir
        putStrLn cmd
        callCommand cmd

  -- STEP 2
  --
  -- Walk through the API headers for the Z3 package, modifying them so they
  -- can be standalone processed. This is done by adding the following at
  -- the beginning of each header:
  --
  --   #include <stdio.h>
  --   #include "z3_macros.h"
  --   #include "z3_api.h"
  --
  -- The headers to be processed are:
  --
  --   z3_algebraic.h
  --   z3_api.h
  --   z3_ast_containers.h
  --   z3_fixedpoint.h
  --   z3_fpa.h
  --   z3_interp.h
  --   z3_logger.h
  --   z3_optimization.h
  --   z3_polynomial.h
  --   z3_private.h
  --   z3_rcf.h
  --   z3_replayer.h
  --   z3_v1.h

  pwd <- getCurrentDirectory
  -- The real work is done in 'mutateHeader'
  let path = interpolate optVars (opts^.headersPath)

  -- STEP 3
  --
  -- Apply c2hsc to each header file, to generate the bindings-DSL macros to
  -- wrap the API. However, instead of using the command-line utility, we'll
  -- using C2Hsc.parseFile directly, so that we can munge the generated text
  -- before emitting it.

  gccExe  <- findExecutable "gcc"
  gccPath <- case gccExe of
    Nothing      -> error "Cannot find executable 'gcc'"
    Just gccPath -> return gccPath

  filesToDecls <- forM (opts^.headers) $ \hdr ->
    mutateHeader (path </> hdr)
      (interpolate (M.empty & at "path" ?~ pwd </> path)
         (if hdr == "z3_api.h"
          then opts^.cPrefacePrimary
          else opts^.cPrefaceRest)) $ \temp -> do
      result <- runPreprocessor (newGCC gccPath) (rawCppArgs [] temp)
      case result of
        Left err     -> error $ "Failed to run cpp: " ++ show err
        Right stream -> do
          let overrideState = defaultOverrides
          let pos = initPos temp
              HscOutput hscs _helpercs _ =
                execState (do overrideState
                              parseCFile stream (posFile pos ==) pos)
                          newHscState
          return (hdr, hscs)

  -- STEP 4
  --
  -- Do a pre-scan of all the declarations we gathered, to identify certain
  -- globals that should be treated specially, such as declarations without
  -- definitions (indicated as '#globalvar').

  globalNames <- flip execStateT (M.empty :: HashMap String String) $
    forM_ filesToDecls $ \(_, decls) -> forM_ decls $ \decl ->
      when ("#globalvar" `isPrefixOf` decl) $ do
        let eres = (\f -> parse f "" decl) $ do
              name <- string "#globalvar " *> manyTill anyChar space
              typ  <- string ", Ptr " *> manyTill anyChar eof
              return (name, typ)
        case eres of
          Left err -> error $ "Error parsing '" ++ decl ++ "': " ++ show err
          Right (n, t) -> at n ?= t

  -- STEP 5
  --
  -- Walk through the bindings-DSL macro calls that were generated by c2hsc,
  -- and expand them manually into ForeignFFI declarations tailored for
  -- haskell-z3.

  let subst decl =
        case parse parseHscEntry decl decl of
          Left err -> error $ "Error in parseHscEntry: " ++ show err
          Right Nothing -> Left decl
          Right (Just entry) ->
            Right (substEntry (opts^.substitutions) entry)
      fdecls' = map (second (map subst)) filesToDecls

  docs <- fmap mconcat $ forM fdecls' $ \(file, _) ->
    readDoxygen file <$> readFile (pwd </> path </> file)

  forM_ fdecls' $ \(file, decls) -> do
    putStrLn file
    let modName = capitalize (drop 3 (takeBaseName file))
        mvars   = M.empty & at "name" ?~ modName
    withFile (interpolate (optVars <> mvars)
                          (opts^.headerOutputPath)) WriteMode $ \h -> do
      let vars = M.empty &~ do
            at "path"   ?= pwd </> path
            at "header" ?= file
            at "module" ?= "Z3.Base.C." ++ modName
      hPutStr h (interpolate vars (opts^.hscPreface))

      if modName == "Api"
        then forM_ (M.keys globalNames) $ \name -> do
          hPutStrLn h $ "data " ++ name
          hPutStrLn h $ "type C'" ++ name ++ " = Ptr " ++ name
        else
          hPutStrLn h "import Z3.Base.C.Api"

      forM_ decls $ \case
        Left s  -> hPutStrLn h s
        Right e -> do
          forM_ (docs ^. at (e^.entryName)) $ \doc ->
            let doc' = trim
                     . unlines
                     . filter (not . ("def_API(" `isInfixOf`))
                     . lines $ doc
                doc'' = if "\\brief " `isPrefixOf` doc'
                        then drop 7 doc'
                        else doc' in
            hPutStrLn h $ "\n{- | " ++ doc'' ++ " -}"

          unless (e^.entryKind == "globalvar") $
            hPrint h e

  -- STEP 6
  --
  -- Download the HTML source for the Z3 C API reference, and process it
  -- into an AST representing the documentation. The link for a particular
  -- API call looks like this (but without newlines):
  --
  --   <a class="el"
  --      href="group__capi.html#ga1fe9f2eb9e4d60cee0dd3eec555684f9">
  --     Z3_algebraic_is_pos
  --   </a>

  -- STEP 7
  --
  -- By combining the information from STEP 3 and STEP 4, we are ready to
  -- produce the raw Haskell interface with function imports from the C API.

  -- STEP 8
  --
  -- Produce much of the contents Z3.Base and Z3.Monad by converting, for
  -- example, from:
  --
  --   z3_mk_real :: Ptr Z3_context -> CInt -> CInt -> IO (Ptr Z3_ast)
  --
  -- to the following for Z3.Base:
  --
  --   mkReal :: Context -> Int -> Int -> IO AST
  --
  -- to the following for Z3.Monad:
  --
  --   mkReal :: MonadZ3 z3 => Int -> Int -> z3 AST

  -- STEP 9
  --
  -- Generate Haddocks for each API method in Z3.Base and Z3.Monad by
  -- searching through the C headers again, this time treating them as
  -- textual source. The Doxygen for 'Z3_mk_real' looks like this:
  --
  --   /**
  --      \brief Create a real from a fraction.
  --
  --      \param c logical context.
  --      \param num numerator of rational.
  --      \param den denomerator of rational.
  --
  --      \pre den != 0
  --
  --      \sa Z3_mk_numeral
  --      \sa Z3_mk_int
  --      \sa Z3_mk_unsigned_int
  --
  --      def_API('Z3_mk_real', AST, (_in(CONTEXT), _in(INT), _in(INT)))
  --   */
  --
  -- The Haddock generated from this could be:
  --
  --   -- | Create a real from a fraction.
  --   --
  --   -- Pre-conditions: den != 0
  --   --
  --   -- Reference: <URI>
  --   --
  --   -- See also:
  --   --   - 'mkNumeral'
  --   --   - 'mkInt'
  --   --   - 'mkUnsignedInt'
  --   mkReal :: MonadZ3 z3
  --     => Int       -- ^ num: numerator of rational
  --     -> Int       -- ^ den: denomerator of rational
  --     -> z3 AST    --

substEntry :: [Substitution] -> Entry -> Entry
substEntry substs e = flip execState e $
  case find (\s -> s^.substKind == e^.entryKind &&
                   s^.substName == e^.entryName) substs of
    Nothing -> return ()
    Just Substitution {..} -> do
      forM_ _substNewKind (entryKind .=)
      forM_ _substNewName (entryName .=)
      forM_ _substNewType (entryType ?=)

parseHscEntry :: Stream s m Char => ParsecT s u m (Maybe Entry)
parseHscEntry = do
  isMacro <- (True <$ char '#') <|> pure False
  if isMacro
    then fmap Just $ Entry
      <$> manyTill anyChar space <* many space
      <*> manyTill anyChar ((() <$ space) <|> eof)
      <*> ((do _ <- many space
               _ <- string ","
               _ <- many space
               Just <$> manyTill anyChar eof) <|> return Nothing)
      <*> pure Nothing
  else return Nothing

readDoxygen :: FilePath -> String -> HashMap String String
readDoxygen path contents =
  case parse (many (try (skipToComment *> parser)) <* skipMany anyChar <* eof)
             path contents of
    Left err -> error $ "Parsing error: " ++ show err
    Right xs -> M.fromList (catMaybes xs)
 where
  skipToComment = manyTill anyChar (try (string "/**"))

  parser = do
    _ <- many space
    doc <- manyTill anyChar (try (string "*/"))
    if "\\brief" `isPrefixOf` doc
      then (do name <- parseTypedef <|> parseDefine <|> parseDecl
               return $ Just (name, doc)) <|> return Nothing
      else return Nothing

  parseTypedef =
    string "typedef"
      *> fmap last (some (many space *>
                          some (satisfy (\c -> isAlphaNum c || c == '_'))))

  parseDefine =
    string "#define"
      *> many space
      *> some (satisfy (\c -> isAlphaNum c || c == '_'))

  parseDecl =
    manyTill anyChar (try (string "Z3_API" >> many space))
      *> manyTill anyChar (try (char '('))

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
