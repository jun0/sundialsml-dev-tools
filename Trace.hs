import Text.Parsec hiding (many, (<|>), optional)
import Text.Parsec.String
import Text.Printf
import Control.Applicative hiding (Const)
import Control.Monad
import System.IO
import Data.List
import System.Environment
import qualified Data.Set as Set
import Data.Set (Set)

data Phrase = Text String | FunDecl Prototype | FunDef Prototype

data Prototype = Prototype { ret :: Type
                           , name :: String
                           , args :: [Arg] }
            deriving (Eq,Ord)

instance Show Prototype where
    show (Prototype ret name args) = show_prototype ret name args

show_prototype :: Type -> String -> [Arg] -> String
show_prototype ret name args =
    printf "%s %s (%s)" (show ret) name $
           intercalate ", " [if a == "" then show t
                             else printf "%s %s" (show t) a
                            | (t,a) <- args]

data Type = Void
          | Char
          | NVector
          | RealType
          | Atomic String
          | Integral String
          | Ptr Type
          | Const Type
            deriving (Eq,Ord)

instance Show Type where
    show Void = "void"
    show Char = "char"
    show NVector = "N_Vector"
    show RealType = "realtype"
    show (Atomic s) = s
    show (Integral s) = s
    show (Ptr t) = show t ++ "*"
    show (Const t) = "const " ++ show t

type Arg = (Type, String)

dispatch f g h (Text s) = f s
dispatch f g h (FunDecl p) = g p
dispatch f g h (FunDef p) = h p

p_c :: Parser [Phrase]
p_c = manyTill (FunDef <$> try p_funtype <|> Text <$> p_line) eof

p_header :: Parser [Phrase]
p_header = manyTill (FunDecl <$> p_prototype <|> Text <$> p_line) eof

lexeme :: String -> Parser String
lexeme s = do try (string s)
              skipMany1 space <|> eof
              return s

keyword :: String -> Parser String
keyword s = try $ do string s
                     notFollowedBy (alphaNum <|> char '_')
                     skipMany space
                     return s

operator :: String -> Parser String
operator s = do try (string s)
                skipMany space
                return s

p_line :: Parser String
p_line = (++) <$> many (noneOf "\n\r") <*> (many1 newline
                                            <|> (eof >> return ""))

p_prototype :: Parser Prototype
p_prototype = keyword "SUNDIALS_EXPORT" >> p_funtype <* char ';'

p_funtype :: Parser Prototype
p_funtype = Prototype <$> p_type <*> p_identifier <*> p_args

p_args :: Parser [Arg]
p_args = operator "(" *> sepBy p_arg (operator ",") <* operator ")"

p_arg :: Parser Arg
p_arg = (,) <$> p_type <*> (p_identifier <|> return "")

qualify_left :: Parser (a -> a) -> Parser a -> Parser a
qualify_left f x = chainl f (return (.)) id <*> x

qualify_right :: Parser a -> Parser (a -> a) -> Parser a
qualify_right x f = x <**> chainl f (return (.)) id

p_type :: Parser Type
p_type = (qualify_right (qualify_left pre_qual base_type) post_qual <?> "type")
         <* spaces
    where
      pre_qual = keyword "const" >> return Const
                 <|>
                 return id
      post_qual = operator "*" >> return Ptr
      base_type = choice [keyword "void" >> return Void
                         ,keyword "N_Vector" >> return NVector
                         ,keyword "char" >> return Char
                         ,keyword "realtype" >> return RealType
                         ,try $ Integral <$> p_inttype
                         ,Atomic <$> p_identifier
                         ]

-- FIXME: no attempt at doing this systematically or completely, obviously.
p_inttype = choice [keyword "unsigned" >> ("unsigned " ++ ) <$> p_inttype
                   ,keyword "int"
                   ,keyword "short" <* optional (keyword "int")
                   ,keyword "long" <* optional (keyword "int")
                   ,keyword "size_t"]

p_identifier :: Parser String
p_identifier = do c <- oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
                  cs <- many $ oneOf $
                        ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
                  skipMany space
                  return $ c:cs

invent_names :: [Arg] -> [Arg]
invent_names = zipWith f [0..]
    where f i (t, "") = (t, "arg" ++ show i ++ "_")
          f i (t, a)  = (t, a)

-- Creation, deletion, and pointer manipulation will always show up
-- differently in C and OCaml, and are just annoying, so ignore them.
-- For clone, we'd typically like to know that a clone happened, which
-- is reported for N_VClone(), so we disable only its monomorphic
-- counterparts like N_VClone_Serial().
blacklist = ["N_VGetArrayPointer", "N_VDestroy", "N_VSetArrayPointer",
             "N_VSpace", "N_VClone_", "N_VNew", "N_VNewEmpty", "N_VCloneEmpty"]

blacklisted name = any (`isPrefixOf` name) blacklist

normalize :: Prototype -> Prototype
normalize (Prototype ret name [(Void,"")]) = Prototype ret name []
normalize p = p

trace_prototype :: Prototype -> IO ()
trace_prototype (Prototype ret name args') = do
  let args = invent_names args'
      typed_arglist = intercalate ", " [show t ++ " " ++ a | (t,a) <- args]
      arglist = intercalate ", " [a | (t,a) <- args]
  printf "SUNDIALS_EXPORT %s;\n" $ show_prototype ret name args
  unless (blacklisted name) $ do
    putStr "static " >> gen_traced ret ("traced__" ++ name) name name args
    -- We hijack the definition with function-like macros.  This way,
    -- nvector operations assigned to the Ops struct won't be affected.
    printf "#define %s(%s) traced__%s(%s)" name arglist name arglist

trace_defn :: Set String -> Prototype -> IO ()
trace_defn exports p | name p `Set.member` exports = trace p
                     | otherwise                   = printf "%s\n" (show p)
    where trace (Prototype ret name args') = do
            let args = invent_names args'
            when (name == "SUNRsqrt") $ do
              putStrLn "#include <limits.h>"
              putStrLn "int sundials_trace_depth = 0;"
              putStrLn "int sundials_trace_depth_max = INT_MAX;"
            if blacklisted name then
                printf "%s\n" $ show_prototype ret name args
            else do
              printf "static %s;\n" $ show_prototype ret (name ++ "_impl") args
              gen_traced ret name name (name ++ "_impl") args
              printf "static %s\n" $ show_prototype ret (name ++ "_impl") args

ret_blacklist = []

ret_blacklisted name = any (`isPrefixOf` name) ret_blacklist

gen_traced :: Type -> String -> String -> String -> [Arg] -> IO ()
gen_traced ret declname origname callname args = do
  let typed_arglist = intercalate ", " [show t ++ " " ++ a | (t,a) <- args]
      arglist = intercalate ", " [a | (t,a) <- args]
  printf "%s %s (%s)\n" (show ret) declname typed_arglist
  printf "{\n"
  when (ret /= Void) $ printf "  %s ret_;\n" (show ret)
  let prerr fmt = printf ("    fprintf (stderr, " ++ fmt ++ ");\n")
  let pp :: String -> Type -> IO ()
      pp a Char = printf "    fputc (%s, stderr);\n" a
      pp a NVector = do
        printf "    if (%s->ops->nvgetarraypointer && %s->ops->nvspace) {\n"
               a a
        printf "      long _lrw_, _liw_, _i_;\n"
        printf "      realtype *_p_ = %s->ops->nvgetarraypointer (%s);\n" a a
        printf "      %s->ops->nvspace (%s, &_lrw_, &_liw_);\n" a a
        printf "      if (_p_) {\n"
        printf "        fprintf (stderr, \"{\");\n"
        printf "        for (_i_ = 0; _i_ < _lrw_; ++_i_) {\n"
        printf "          if (_i_ > 0) fprintf (stderr, \",\");\n"
        printf "          fprintf (stderr, \"%%a\", _p_[_i_]);\n"
        printf "        }\n"
        printf "        fprintf (stderr, \"}\");\n"
        printf "      }\n"
        printf "      else\n"
        printf "        fprintf (stderr, \"{(null, length %%lld)}\", "
               >> putStr "(long long)_lrw_);\n"
        printf "    }\n"
        printf "    else\n"
        printf "  " >> prerr "\"(N_Vector)%%p\", %s" a
      pp a RealType = prerr "\"%%a\", %s" a
      -- pp a (Atomic s) = if "Fn" `isSuffixOf` s
      --                   then prerr "\"(%s)%%p\", %s" s a
      --                   else prerr "\"<%s>\"" s
      pp a (Atomic s)   = prerr "\"(%s)\"" s
      pp a (Integral i) = prerr "\"%%lld\", (long long)%s" a
      pp a (Ptr Char)         = prerr "\"%%s\", %s" a
      pp a (Ptr (Const Char)) = prerr "\"%%s\", %s" a
      -- pp a (Ptr Void) = prerr "\"%%p\", %s" a
      -- pp a (Ptr t)    = prerr "\"(%s*)%%p\", %s" (show t) a
      pp a (Ptr t)    = prerr "\"(%s*)\"" (show t)
      pp a (Const t)  = pp a t
  printf "  if (sundials_trace_depth < sundials_trace_depth_max) {\n"
  printf "    sundials_trace_indent ();\n"
  prerr "\"%s (\"" origname
  sequence_ $ intersperse (prerr "\", \"") [pp a t | (t, a) <- args]
  prerr "\");\\n\""
  printf "  }\n"
  printf "  SUNDIALS_TRACE_ENTER();\n"
  when (ret /= Void) $ printf "  ret_ =\n  "
  printf "  %s (%s);\n" callname arglist
  printf "  SUNDIALS_TRACE_LEAVE();\n"
  when (ret /= Void) $ do
    printf "  if (sundials_trace_depth < sundials_trace_depth_max) {\n"
    printf "    sundials_trace_indent ();\n"
    if ret_blacklisted origname
    then printf "  /* %s returns uninitialized value */" origname
    else do prerr "\"-> \""
            pp "ret_" ret
            prerr "\"\\n\""
    printf "  }\n"
    printf "  return ret_;\n"
  printf "}\n"

trace_header :: String -> IO ()
trace_header = either prerr go . parse p_header "<stdin>"
    where go = mapM_ (dispatch putStr (trace_prototype . normalize) undefined)

trace_c :: Set String -> String -> IO ()
trace_c exports = either prerr go . parse p_c "<stdin>"
    where go = mapM_ (dispatch putStr undefined
                      (trace_defn exports . normalize))

extract_exports :: String -> IO ()
extract_exports = either prerr (mapM_ go) . parse p_header "<stdin>"
    where go (FunDecl d) = putStrLn $ name d
          go _           = return ()

load_exports :: String -> IO (Set String)
load_exports path = do
  h <- openFile path ReadMode
  set <- Set.fromList . lines <$> hGetContents h
  Set.size set `seq` hClose h
  return set

prerr e = hPrintf stderr "Error: %s\n" (show e)

preamble = do
  putStrLn "#ifndef sundials_trace_indent"
  putStrLn "#include <stdio.h>"
  putStrLn "#define sundials_trace_indent() \\"
  putStrLn "    do {\\"
  putStrLn "      int _i_;\\"
  putStrLn "      for (_i_ = 0; _i_ < sundials_trace_depth; ++_i_)\\"
  putStrLn "        fputc ('\\t', stderr);\\"
  putStrLn "    } while (0)"
  putStrLn "#define SUNDIALS_TRACE_ENTER() (++sundials_trace_depth)"
  putStrLn "#define SUNDIALS_TRACE_LEAVE() (--sundials_trace_depth)"
  putStrLn "#endif"

main = do
  args <- getArgs
  -- "-m" means we have a dedicated module for defining trace-specific
  -- functions and variables.
  case args of
    "-c":f:_ -> do exports <- load_exports f
                   putStrLn "#include <stdlib.h>"
                   putStrLn "#ifndef malloc"
                   -- Banish uninitialized buffers, whose use can be
                   -- detected readily by valgrind.  Wish we could do
                   -- this for realloc too...  I guess we could, by
                   -- hijacking all of malloc, calloc, realloc, and
                   -- free, so that we can insert our own header that
                   -- records size information.  Is it worth the
                   -- trouble?
                   putStrLn "#define malloc(n) calloc((n), 1)"
                   putStrLn "#endif"
                   putStrLn "extern int sundials_trace_depth;"
                   putStrLn "extern int sundials_trace_depth_max;"
                   preamble
                   trace_c exports =<< getContents
    "-x":_ -> extract_exports =<< getContents
    "-m":_ -> do putStrLn "extern int sundials_trace_depth;"
                 putStrLn "extern int sundials_trace_depth_max;"
                 preamble
                 trace_header =<< getContents
    _ -> do putStrLn "static int sundials_trace_depth = 0;"
            putStrLn "#define sundials_trace_depth_max 1"
            preamble
            trace_header =<< getContents

