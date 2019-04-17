--
-- S-expression parser.
--

module Sexpr where

import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------------
-- Datatypes.
----------------------------------------------------------------------

data Atom =
    BoolA  Bool
  | IntA   Integer
  | FloatA Double
  | IdA    String  -- identifier
  | StringA String 
  deriving (Show)

data Sexpr =
    AtomS Atom
  | ListS [Sexpr]
  deriving (Show)

----------------------------------------------------------------------
-- Parsers.
----------------------------------------------------------------------

parseBool :: Parser Bool
parseBool =
  char '#' >>
  ((char 'f' >> return False)
   <|> (char 't' >> return True))
  <?> "boolean"

parseInt :: Parser Integer
parseInt = do
  sign <- option "" (string "-")
  digits <- many1 digit  -- many1 (oneOf "0123456789")
  return (read (sign ++ digits) :: Integer)
  <?> "integer"

parseFloat :: Parser Double
parseFloat = do
  sign <- option "" (string "-")
  digits <- many1 digit
  char '.'
  f <- many1 digit
  isExp <- many (oneOf "Ee")
  expSign <- option "" (string "-")
  expSign <- option expSign (string "+")
  exp <- many digit 
  return (read (sign ++ digits ++ "." ++ f ++ isExp ++ expSign ++ exp) :: Double)
  <?> "floating-point number"

parseId :: Parser String
parseId = many1 (alphaNum <|> oneOf "_+-*/=?!") <?> "identifier"

parseString :: Parser String 
parseString = do
    char '\"'
    str <- many (noneOf "\"")
    char '\"'
    return str 
    <?> "string"

parseAtom :: Parser Atom
parseAtom =
  (parseBool >>= return . BoolA)
  <|> try (parseFloat >>= return . FloatA)
  <|> try (parseInt >>= return . IntA)
  <|> (parseString >>= return. StringA)
  <|> (parseId >>= return . IdA)
  <?> "atom"

parseComment :: Parser ()
parseComment = do
  char ';'
  many (noneOf "\n")
  char '\n'
  return ()

parseWhitespace :: Parser ()
parseWhitespace = many1 space >> return ()

-- Parse a separator (whitespace or comment).
parseSep :: Parser ()
parseSep = 
  many1 (parseComment <|> parseWhitespace) >> return ()
  <?> "separator"

-- C.3: We do not need to use the try combinator in the parseList function 
-- because the try combinator is used to backtrack when we fail after
-- reading some input. Howver, in such cases, we want the beginning and end
-- delimiters to match (i.e. '(' followed by ')' , '[' followed by ']', and 
-- '{' followed by '}'). Hence, once we read the opening delimiter, we know
-- what to expect for the ending delimiter so once we read input, there
-- is no need to backtrack and hence we do not need to use try.

-- Parse a list of S-expressions, delimited by parentheses, brackets, or
-- curly braces separated by whitespace/comments.
parseList :: Parser [Sexpr]
parseList = do
    parseHelper '(' ')'
    <|> parseHelper '[' ']'
    <|> parseHelper '{' '}'
    <?> "list of S-expressions"

-- Parse a quoted expression.
parseQuote :: Parser Sexpr
parseQuote = do
    char '\'' 
    q <- parseSexpr 
    return (ListS[AtomS (IdA "quote"), q])
  <?> "quoted S-expression"

-- Takes two delimiter characters and outputs a parser. Used by parseList
-- to handle scenarios where lists are delimited by parenthesis, square
-- brackets, and/or curly braces.
parseHelper :: Char -> Char -> Parser [Sexpr]
parseHelper a b = do 
    char a 
    optional parseSep
    ss <- parseSexpr `sepEndBy` parseSep
    char b 
    return ss 
    <?> "helper for list of S-expressions"

-- Parse a single S-expressions.
parseSexpr :: Parser Sexpr
parseSexpr = 
  (parseAtom >>= return . AtomS)
  <|> (parseList >>= return . ListS)
  <|> parseQuote
  <?> "S-expression"

-- Parse a series of Sexprs from a string representing the entire contents of a
-- file.
parseSexprsFromFile :: Parser [Sexpr]
parseSexprsFromFile = do
  optional parseSep
  ss <- parseSexpr `sepEndBy` parseSep
  eof
  return ss
  <?> "file of S-expressions"

----------------------------------------------------------------------
-- Pretty-printer.
----------------------------------------------------------------------

indent :: Int -> String
indent i = replicate i ' '

-- Pretty-print a Sexpr.
ppSexpr :: Int -> Sexpr -> String
ppSexpr i (AtomS a)  = indent i ++ show a
ppSexpr i (ListS ss) = 
  indent i
  ++ "ListS[\n" 
  ++ concatMap (\s -> ppSexpr (i + 2) s ++ "\n") ss
  ++ indent i ++ "]"


-- Parse all expressions in a file and run the pretty-printer on them.
runPpSexpr :: FilePath -> IO ()
runPpSexpr f = do
  p <- parseFromFile parseSexprsFromFile f
  case p of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right ss -> 
      mapM_ (\s -> do
        putStrLn (ppSexpr 0 s)
        putStrLn "") ss

----------------------------------------------------------------------
-- Tests.
----------------------------------------------------------------------

test :: IO ()
test = runPpSexpr "test.scm"