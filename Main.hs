{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import           Control.Arrow (first, second)
import           Control.Lens hiding (re)
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Types hiding ((.=), Parser, Options, parse)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.C2Hsc
import           Data.Char
import           Data.Foldable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.List
import           Data.List.Split
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import           Text.Regex.PCRE.Heavy

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

data DocFix = DocFix
  { _docMatch :: String
  , _docSubst :: String
  }
  deriving (Generic, Typeable)

customDocFix :: Aeson.Options
customDocFix =
  defaultOptions { fieldLabelModifier = \x -> toLower (x !! 4) : drop 5 x }

instance ToJSON DocFix where
    toJSON     = genericToJSON customDocFix
    toEncoding = genericToEncoding customDocFix

instance FromJSON DocFix where
    parseJSON = genericParseJSON customDocFix

makeLenses ''DocFix

compileRe :: String -> Regex
compileRe s =
  either (const (error $ "Failed to compile regex: " ++ s))
         id (compileM (T.encodeUtf8 (T.pack s)) [])

compileDocFixes :: [DocFix] -> [(DocFix, Regex)]
compileDocFixes = map (\x -> (x, compileRe (x^.docMatch)))

data Options = Options
  { _version          :: String
  , _gitURI           :: String
  , _gitTag           :: String
  , _gitWorkdir       :: FilePath
  , _cPreface         :: String
  , _hscPreface       :: String
  , _hscAddition      :: String
  , _headersPath      :: FilePath
  , _headers          :: [FilePath]
  , _headerOutputPath :: FilePath
  , _userApiModule    :: FilePath
  , _substitutions    :: [Substitution]
  , _docFixes         :: [DocFix]
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
  , _cPreface         = error "Must specify cPreface"
  , _hscPreface       = error "Must specify hscPreface"
  , _hscAddition      = ""
  , _headersPath      = "z3/src/api"
  , _headers          = error "Must specify headers to process"
  , _headerOutputPath = "Z3/Base/C/#{name}.hsc"
  , _userApiModule    = "Z3/Base/C.hsc"
  , _substitutions    = []
  , _docFixes         = []
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
        (   long "c-preface"
            <> help "C preface to be parsed before each Z3 C header"
            <> value (opts^.cPreface))
    <*> strOption
        (   long "hsc-preface"
            <> help "Preface to be included near the top of each .hsc file"
            <> value (opts^.hscPreface))
    <*> strOption
        (   long "hsc-addition"
            <> help "Code that is appended to the C.hsc file"
            <> value (opts^.hscAddition))
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
    <*> strOption
        (   long "api-module"
            <> help "The .hsc file where user-facing declarations are written"
            <> value (opts^.userApiModule))
    <*> pure (opts^.substitutions)
    <*> pure (opts^.docFixes)
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
  at "cPreface"         ?= _cPreface
  at "hscPreface"       ?= _hscPreface
  at "hscAddition"      ?= _hscAddition
  at "headers-path"     ?= _headersPath
  at "headers"          ?= concat _headers
  at "headerOutputPath" ?= _headerOutputPath
  at "userApiModule"    ?= _userApiModule

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
    withFile file ReadMode $
      hPutStr handle
        -- jww (2017-12-15): This is a hack at present!
        . sub [re|typedef int Z3_bool;|]
              "typedef enum\n{\n  Z3_FALSE = 0,\n  Z3_TRUE\n} Z3_bool;"
        . sub [re|#define Z3_TRUE  1|] ""
        . sub [re|#define Z3_FALSE 0|] ""
        <=< hGetContents
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
      (interpolate (M.empty &~ do
                       at "path" ?= pwd </> path
                       at "includes" ?=
                         if hdr == "z3_api.h"
                         then ""
                         else "#include \""
                           ++ pwd </> path ++ "/z3_api.h\"")
                   (opts^.cPreface)) $ \temp -> do
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
  -- definitions (indicated as '#globalvar'). Also, pay attention to how
  -- integral constant are defined.

  let subst decl =
        case parse parseHscEntry decl decl of
          Left err -> error $ "Error in parseHscEntry: " ++ show err
          Right Nothing -> Left decl
          Right (Just entry) ->
            Right (substEntry (opts^.substitutions) entry)

      fdecls' = map (second (map subst)) filesToDecls

      (globalNames, _, constants, callbacks, _synonyms) =
        flip execState (M.empty, Nothing, M.empty, M.empty, M.empty) $
          forM_ fdecls' $ \(_, decls) -> forM_ decls $ \case
            Left _ -> return ()
            Right e -> case e^.entryKind of
              "globalvar" ->
                _1.at (e^.entryName) .= e^.entryType
              "callback_t" ->
                _4.at (e^.entryName) .= e^.entryType
              "integral_t" -> do
                _2 ?= e^.entryName
                _3.at (e^.entryName) ?= []
              "synonym_t" ->
                _5.at (e^.entryName) .= e^.entryType
              "num" -> do
                Just name <- use _2
                _3.ix name <>= [e^.entryName]
              _ -> return ()

  docs <- fmap mconcat $ forM fdecls' $ \(file, _) ->
    readDoxygen file <$> readFile (pwd </> path </> file)

  let compiledFixes = compileDocFixes (opts^.docFixes)

  -- STEP 5
  --
  -- Walk through the bindings-DSL macro calls that were generated by c2hsc,
  -- and expand them manually into ForeignFFI declarations tailored for
  -- haskell-z3.

  withFile (opts^.userApiModule) WriteMode $ \api -> do
    -- let includes =
    --       concatMap (\(file, _) -> "#include \"" ++ file ++ "\"\n")
    --                 fdecls'
    hPutStr api $ interpolate (M.empty &~ do
                                  at "module" ?= ""
                                  at "includes" ?= "#include <z3.h>")
                              (opts^.hscPreface)

    hPutStr api $ opts^.hscAddition

    forM_ fdecls' $ \(file, decls) -> do
      putStrLn $ "Writing " ++ file ++ "..."

      let modName = capitalize (drop 3 (takeBaseName file))
          mvars   = M.empty & at "name" ?~ modName
      withFile (interpolate (optVars <> mvars)
                            (opts^.headerOutputPath)) WriteMode $ \h -> do
        let vars = M.empty &~ do
              at "includes" ?=
                "#include <bindings.dsl.h>\n" ++
                "#include \"z3_macros.h\"\n" ++
                (if modName == "Api" then "" else "#include \"z3_api.h\"\n")
                  ++ "#include \"" ++ file ++ "\"\n"
              at "module"   ?= "." ++ modName
        hPutStr h (interpolate vars (opts^.hscPreface))

        if modName == "Api"
          then forM_ [h, api] $ \handle ->
            forM_ (M.keys globalNames) $ \name -> do
              hPutStrLn handle $ "data " ++ name
              hPutStrLn handle $ "type C'" ++ name ++ " = Ptr " ++ name
          else
            hPutStrLn h "import Z3.Base.C.Api"

        forM_ decls $ \case
          Left s  -> hPutStrLn h s
          Right e -> case e^.entryKind of
            "globalvar" -> return ()

            "synonym_t" -> do
              hPrint h e

              forM_ (docs ^. at (e^.entryName)) $ \doc ->
                hPutStrLn api $ "\n{- | "
                  ++ convertDoxygen compiledFixes doc ++ " -}"

              let ty = fromMaybe (error "Missing type") (e^.entryType)
              hPutStrLn api $ "type " ++ e^.entryName ++ " = "
                ++ convertType globalNames callbacks ty

            "num" -> hPrint h e
            "integral_t" -> do
              hPrint h e

              forM_ (docs ^. at (e^.entryName)) $ \doc ->
                hPutStrLn api $ "\n{- | "
                  ++ convertDoxygen compiledFixes doc ++ " -}"

              hPutStrLn api $ "newtype " ++ e^.entryName ++
                " = " ++ e^.entryName ++ " CUInt deriving (Eq, Ord)"

              forM_ (constants ^?! ix (e^.entryName)) $ \sym -> do
                hPutStrLn api $ map toLower sym ++ " :: " ++ e^.entryName
                hPutStrLn api $ map toLower sym
                  ++ " = " ++ e^.entryName ++ " (#const " ++ sym ++ ")"

            "callback_t" -> do
              hPrint h e

              forM_ (docs ^. at (e^.entryName)) $ \doc ->
                hPutStrLn api $ "\n{- | "
                  ++ convertDoxygen compiledFixes doc ++ " -}"

              let ty = fromMaybe (error "Missing type") (e^.entryType)
              hPutStrLn api $ "type " ++ e^.entryName ++ " = "
                ++ convertType globalNames callbacks ty

            "ccall" -> do
              hPrint h e

              forM_ (docs ^. at (e^.entryName)) $ \doc ->
                hPutStrLn api $ "\n{- | "
                  ++ convertDoxygen compiledFixes doc ++ " -}"

              let ty = fromMaybe (error "Missing type") (e^.entryType)
              hPutStrLn api $ "foreign import ccall unsafe \""
                ++ e^.entryName ++ "\""
              hPutStrLn api $ "  " ++ map toLower (e^.entryName)
                ++ " :: " ++ convertType globalNames callbacks ty

            _ -> hPrint h e

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

convertDoxygen :: [(DocFix, Regex)] -> String -> String
convertDoxygen fixes
  = (\d -> if "\\brief " `isPrefixOf` d
          then drop 7 d
          else d)
  . trim
  . (\s -> foldr (\(df, regex) -> gsub regex $ \gs ->
                    interpolate (M.fromList (map (first show)
                                                 (zip [1 :: Int ..] gs)))
                                (df^.docSubst))
                 s fixes)
  . unlines
  . filter (not . ("def_API(" `isInfixOf`))
  . lines

-- | Convert a type of the form:
-- @
--   <Z3_context> -> <Z3_func_decl> -> CUInt -> Ptr <Z3_ast> -> IO <Z3_ast>
-- @
-- into:
-- @
--   Ptr Z3_context -> Ptr Z3_func_decl -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)
-- @
--
-- This is done by remember which types were defined using:
-- @
--   DEFINE_TYPE(Z3_config);
-- @
-- Where the expansion of this macro is:
-- @
--   typedef struct _Z3_config *Z3_config
-- @
--
-- Other than these, we simply drop angle brackets where we find them.
convertType :: HashMap String String -> HashMap String String -> String -> String
convertType globals callbacks
  = intercalate " -> "
  . map (sub [re|<([^>]+?)>|] (\[x] -> convertName x) . trim)
  . splitOn "->"
 where
  convertName name = case globals ^. at name of
    Nothing -> case callbacks ^. at name of
      Nothing -> name
      Just _ty -> "(FunPtr " ++ name ++ ")"
    Just _ty -> case name of
        "Z3_string" -> "CString"
        _ -> "(Ptr " ++ name ++ ")"
