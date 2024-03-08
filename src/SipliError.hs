module SipliError (
  SipliError(..),
  SipliErrorIF(..),
  ParseErr(..),
  LexErr(..),
  TDEvalFail(..)
) where


data SipliError = LexError LexErr | ParseError ParseErr | TopDownEvalFail TDEvalFail | UndefError

instance Show SipliError where
  show (LexError e) = show e
  show (ParseError e) = show e
  show UndefError     = "Undefined Error"

instance MonadFail (Either SipliError) where 
  fail _ = Left UndefError

class SipliErrorIF a where
  return_error::a ->SipliError
  return_error _ = UndefError

data ParseErr = ParseErr String

instance SipliErrorIF ParseErr where
  return_error s = ParseError s

instance Show ParseErr where
  show (ParseErr e) = e

data LexErr = LexErr {line::Int, col::Int, ch::Char}

instance SipliErrorIF LexErr where 
  return_error s = LexError s

instance Show LexErr where 
  show (LexErr line col ch) = "Invalid character : " ++ Prelude.show ch ++ " at line : " ++ Prelude.show line ++ " col : " ++ Prelude.show col 

data TDEvalFail = TDFail String

instance Show TDEvalFail where
  show (TDFail s) = s 



