import System.IO
import System.Directory
import Lexer
import Parser
import Unifier
import TopDownEval
import SipliError

{-# ANN module ("hlint: ignore Use camelCase") #-}

data Cmd = Filename String | Unify [ASTNode] | TDQuery [ASTNode] | None | Exit | Undefined String deriving (Show) 
data Context = Ctx ASTNode | Empty | EOC 

sip::(SipliErrorIF a) => Either a b -> Either SipliError b
sip (Left e)  = Left (return_error e)
sip (Right v) = Right v   

print_list::Show a => [a] -> IO ()
print_list [] = return ()
print_list (a:as) = do
                      print a
                      print_list as

read_pred_list::String->Either SipliError ASTNode
read_pred_list input = do
                        tokens         <- sip (tokenize input 0 0)
                        (_, pred_list) <- sip (parse_pred_list tokens)
                        return pred_list

parse_command::String->IO Cmd
parse_command ":q" = return Exit 
parse_command input = case x of 
                    ["load", "file", filename] -> return (Filename filename)
                    ("unify":preds) -> case strings_to_atoms preds of 
                                        Left err -> do
                                                      print err
                                                      return None
                                        Right atoms -> return (Unify atoms)

                    ("?":query_s)  -> case query of 
                                        Left err -> do 
                                                      print err
                                                      return None
                                                      
                                        Right (PredList query) -> return (TDQuery query)

                                      where 

                                        query = read_pred_list input
                                        input = concat query_s
                                      

                    _ -> return (Undefined "Undefined control sequence")

                    where 
                      x = words input
                      


print_tokens::Either [Token] LexErr -> IO ()
print_tokens (Left (t:ts)) = do
                                print t
                                print_tokens (Left ts)

print_tokens (Right err) = print err
print_tokens (Left []) = print '\n'

strings_to_atoms::[String]-> Either SipliError [ASTNode]
strings_to_atoms [] = Right []
strings_to_atoms (s:ss) = do
                      tokens    <- sip (tokenize s 0 0)
                      ([],atom) <- sip (parse_pred tokens)
                      atoms     <- strings_to_atoms ss
                      return (atom:atoms)

print_recursively::(Show a) => [a] -> Context -> IO Context
print_recursively [] ctx = return ctx
print_recursively (a:as) ctx = do 
                            print a 
                            print_recursively as ctx


add_to_context::ASTNode -> Context -> Context
add_to_context rules  Empty = Ctx rules
add_to_context (RuleList new_rules) (Ctx (RuleList all_rules)) = Ctx (RuleList(all_rules ++ new_rules))

load_file::String -> Either SipliError ASTNode
load_file contents = do 
                      tokens <- sip (tokenize contents 0 0)
                      ([], program) <- sip (parse_program tokens)
                      return program

execute_cmd::Cmd->Context->IO Context 
execute_cmd (Filename filename) ctx = do
                                    exists <- doesFileExist filename
                                    if not exists 
                                      then do 
                                        putStrLn ("File: " ++ filename ++ " does not exist")
                                        return ctx
                                    else do
                                      handle <- openFile filename ReadMode
                                      contents  <- hGetContents handle
                                      case load_file contents of 
                                        Left err -> do
                                                      print err
                                                      return ctx
                                        Right rules -> return $ add_to_context rules ctx

execute_cmd (Unify (p1:p2:t)) ctx  = case unify p1 p2 [] of 
                                      Left err -> do
                                                    print err
                                                    return ctx
                                      Right subs -> do 
                                                      print_subs subs
                                                      return ctx

execute_cmd (TDQuery query) ctx = case top_down_evaluation goal rules of 
                                    Left err              -> do
                                                              print err
                                                              return ctx

                                    Right (TDSucc subs _) -> do
                                                              print subs
                                                              return ctx
                                  where 
                                    (goal:_) = query
                                    (Ctx rules) = ctx                                    

execute_cmd (Undefined msg) ctx = do 
                                    print msg
                                    return ctx
                                    
                                 
execute_cmd Exit _ = return EOC

sipli::Context->IO ()
sipli EOC = return ()

sipli ctx = do
              input <- getLine
              cmd <- parse_command input
              ctx_1 <- execute_cmd cmd ctx
              sipli ctx_1
              return ()

main = sipli Empty 

