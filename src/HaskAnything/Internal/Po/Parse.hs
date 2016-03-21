module HaskAnything.Internal.Po.Parse where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Identity
import Text.ParserCombinators.Parsec.Language (emptyDef,commentLine,reservedNames)
import Control.Arrow (first, second)
import qualified Data.Text.IO as T (readFile)
import Data.Text (unpack)

-------------------------------------------------------------------------------
-- Type declarations
-------------------------------------------------------------------------------
newtype Msgid = Msgid String deriving (Show,Eq,Ord)

type Msgstr = String

newtype Locale = Locale String deriving (Show,Eq,Ord)

type Context = String

-- | The Internationalization monad built using monad transformers.
type I18n a = ReaderT (Locale, L10n, Maybe Context) Identity a

-- | The Localization structure.
type L10n = M.Map Locale
                    (M.Map (Maybe Context)
                             (M.Map Msgid
                                      [Msgstr]))

data MsgDec = MsgDec (Maybe Context) Msgid [Msgstr] deriving (Show)

test = do
  parsed <- parsePo "drupal.pot"
  let r = case parsed of
            (Left x) -> error (show x)
            (Right x) -> x
  print r



parsePo :: FilePath -> IO (Either ParseError [MsgDec])
parsePo n = do
    contents <- T.readFile n
    return $ parse po n (unpack contents)

-------------------------------------------------------------------------------
-- .po Parser
-------------------------------------------------------------------------------
{- EBNF
    PO            ::= msg*
    msg           ::= [msg-context] (msg-singular | msg-plural)
    msg-context   ::= "msgctxt" string*
    msg-singular  ::= msgid msgstr
    msg-plural    ::= msgid msgid-plural msgstr-plural*
    msgid         ::= "msgid"  string*
    msgid-plural  ::= "msgid_plural" string*
    msgstr        ::= "msgstr" string*
    msgstr-plural ::= "msgstr" form string*
    form          ::= "[" number "]"
    number        ::= (0-9)* | "N"
    string        ::= "\"" (char | escaped-char)* "\""
    escaped-char  ::= "\\" char
    char          ::= (any UTF8 character)
-}
lexer = P.makeTokenParser (emptyDef {
    commentLine   = "#",
    reservedNames = ["msgctxt","msgid","msgid_plural","msgstr"] })

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme     lexer
reserved   = P.reserved   lexer

po = do whiteSpace
        msgs <- many msg
        eof
        return msgs

msg = do ctxt      <- msg_context
         (id,strs) <- try msg_singular <|> msg_plural
         return (MsgDec ctxt id strs)

msg_context = try $ option Nothing $ do
    lexeme (reserved "msgctxt")
    strs <- many1 str
    return (Just (concat strs))

msg_singular = do id  <- lexeme msgid
                  str <- lexeme msgstr
                  return (Msgid id, [str])

msg_plural = do id    <- lexeme msgid
                idp   <- lexeme msgid_plural
                strps <- lexeme (many1 msgstr_plural)
                return (Msgid id, strps)

msgid = do lexeme (reserved "msgid")
           strs <- many1 str
           return (concat strs)

msgid_plural = do lexeme (reserved "msgid_plural")
                  strs <- many1 str
                  return (concat strs)

msgstr = do lexeme (reserved "msgstr")
            strs <- many1 str
            return (concat strs)

msgstr_plural = do lexeme (reserved "msgstr")
                   char '['
                   try (do c <- oneOf ['n','N']
                           return [c])
                       <|> many1 (oneOf ['0'..'9'])
                   char ']'
                   whiteSpace
                   strs <- many1 str
                   return (concat strs)

str = lexeme $ do char '"'
                  chs <- many ch
                  char '"'
                  return chs

ch = try escaped_ch <|> noneOf ['"']

escaped_ch = do e <- char '\\'
                c <- anyChar
                case reads ['\'',e,c,'\''] :: [(Char,String)] of
                    (c,s):[] -> return c
                    _        -> return c

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
--separateEithers = foldr (either (first . (:)) (second . (:))) ([],[])