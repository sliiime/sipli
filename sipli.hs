import System.IO
import System.Directory
import Lexer
import Parser
import Unifier

{-# ANN module ("hlint: ignore Use camelCase") #-}

data Cmd = Filename String | Unify [String] | Query String | Exit | Undefined String deriving (Show) 
data Context = Ctx String | Empty | EOC 

parse_input::String->Cmd
parse_input ":q" = Exit 
parse_input input = case x of 
                    ["load", "file", filename] -> Filename filename
                    ("unify":preds) -> Unify preds --Unify map parse_pred preds
                    _ -> Undefined "Undefined control sequence"
                    where x = words input


print_tokens::Either [Token] LexError -> IO ()
print_tokens (Left (t:ts)) = do
                                print t
                                print_tokens (Left ts)

print_tokens (Right err) = print err
print_tokens (Left []) = print '\n'

read_preds::[String]->Either String [ASTNode]
read_preds [] = Right []
read_preds (s:ss) = case tokenize s 0 0 of 
                      Right e     -> Left (show e)
                      Left tokens -> case parse_pred tokens of 
                                      Right ([],pred) -> do
                                                          preds <- read_preds ss
                                                          return (pred : preds)
                                      Left err        -> Left err

print_recursively::(Show a) => [a] -> Context -> IO Context
print_recursively [] ctx = return ctx
print_recursively (a:as) ctx = do 
                            print a 
                            print_recursively as ctx


execute_cmd::Cmd->Context->IO Context
execute_cmd (Filename filename) ctx = do
                                    putStrLn filename
                                    exists <- doesFileExist filename
                                    if not exists 
                                      then do 
                                        putStrLn ("File: " ++ filename ++ " does not exist")
                                        return ctx
                                    else do
                                      handle <- openFile filename ReadMode
                                      contents  <- hGetContents handle
                                      let lex_result = tokenize contents 0 0
                                      case lex_result of 
                                        Left tokens -> do 
                                                          print tokens
                                                          print (parse_program tokens)

                                        Right err   -> print err


                                      putStrLn contents
                                      return ctx

execute_cmd (Unify preds_s) ctx  = case read_preds preds_s of 
                                    Left  err       -> print_recursively [err] ctx
                                    Right (p1:p2:t) -> case unify p1 p2 [] of
                                                        Right subs -> print_recursively subs ctx
                                                        Left  err  -> print_recursively [err] ctx

execute_cmd (Undefined msg) ctx = do 
                                 putStrLn msg 
                                 return ctx 

execute_cmd Exit _ = return EOC

sipli::Context->IO ()
sipli EOC = return ()

sipli ctx = do
              input <- getLine
              let cmd = parse_input input
              ctx_1 <- execute_cmd cmd ctx
              sipli ctx_1

main = sipli Empty 

