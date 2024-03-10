import System.IO
import System.Directory
import Lexer
import Parser
import Unifier
import TopDownEval
import BotUpEval
import SipliError
import Utils
import Data.Maybe

{-# ANN module ("hlint: ignore Use camelCase") #-}

data Cmd = Filename String | Unify [ASTNode] | TDQuery [ASTNode] | BottomUp (Maybe Int) | None | Exit deriving (Show) 
data Context = Ctx ASTNode | EOC 

unexpected_input::String
unexpected_input = "Unexpected character sequence"

sip::(SipliErrorIF a) => Either a b -> Either SipliError b
sip (Left e)  = Left (return_error e)
sip (Right v) = Right v   

read_pred_list::String->Either SipliError ASTNode
read_pred_list input = do
                        tokens         <- sip (tokenize input 0 0)
                        (_, pred_list) <- sip (parse_pred_list tokens)
                        return pred_list

backtrack_decision::TDStack->ASTNode->Int->[ASTNode]->IO ()
backtrack_decision ss rules state_id query_vars = case ss of 
                                        []  -> return ()
                                        _   ->  do
                                                  tryBt <- getLine
                                                  case tryBt of 
                                                    ";" -> case backtrack ss rules state_id of 
                                                            Left  (TDFail msg) -> print msg
                                                            Right (TDSucc subs ss_1 state_id_1) -> do
                                                                                                    print_subs subs_1
                                                                                                    backtrack_decision ss_1 rules state_id_1 query_vars 
                                                                                                   where 
                                                                                                    subs_1 = subs_of subs query_vars
                                                    "." -> return ()
                                                    
                                                    _   -> print unexpected_input


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

                    "v^":tail ->  case tail of 
                                      rounds:_ -> return (BottomUp (isNatural rounds))
                                      []       -> return (BottomUp Nothing)

                    _ -> do 
                          print unexpected_input 
                          return None

                    where 
                      x = words input
                      

strings_to_atoms::[String]-> Either SipliError [ASTNode]
strings_to_atoms [] = Right []
strings_to_atoms (s:ss) = do
                      tokens    <- sip (tokenize s 0 0)
                      ([],atom) <- sip (parse_pred tokens)
                      atoms     <- strings_to_atoms ss
                      return (atom:atoms)

add_to_context::ASTNode -> Context -> Context
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
                                      Left err   -> do
                                                      print err
                                                      return ctx
                                      Right subs -> do 
                                                      print_subs subs
                                                      return ctx

execute_cmd (TDQuery query) ctx = case top_down_evaluation query rules of 
                                    Left err               -> do
                                                                print err
                                                                return ctx

                                    Right (TDSucc subs ss state_id) -> do
                                                                        print_subs subs
                                                                        backtrack_decision ss rules state_id query_vars
                                                                        return ctx
                                                                       where
                                                                        query_vars = foldr (\x acc -> vars_of x ++ acc) [] query
                                  where 
                                    (Ctx rules) = ctx                                    

execute_cmd (BottomUp rounds) ctx = do 
                                      print_list infered show_pretty 
                                      return ctx
                                    where
                                     Ctx rules = ctx
                                     mode = fromMaybe (-2) rounds 
                                     infered = bottom_up_evaluation [] rules mode 
execute_cmd None ctx = return ctx                                    
                                 
execute_cmd Exit _ = return EOC

sipli::Context->IO ()
sipli EOC = return ()


sipli ctx = do
              input <- getLine
              cmd <- parse_command input
              ctx_1 <- execute_cmd cmd ctx
              sipli ctx_1
              return ()

main = sipli (Ctx (RuleList []))

