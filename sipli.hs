import System.IO
import System.Directory
import Lexer

{-# ANN module ("hlint: ignore Use camelCase") #-}

data Cmd = Filename String | Query String | Exit | Undefined String deriving (Show) 
data Context = Ctx String | Empty | EOC 

parse_input::String->Cmd
parse_input ":q" = Exit 
parse_input input = case x of 
                    ["load", "file", filename] -> Filename filename
                    _ -> Undefined "Undefined control sequence"
                    where x = words input


print_tokens::Either [Token] LexError -> IO ()
print_tokens (Left (t:ts)) = do
                                print t
                                print_tokens (Left ts)

print_tokens (Right err) = print err
print_tokens (Left []) = print '\n'

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
                                      let tokens = tokenize contents 0 0 
                                      print_tokens tokens
                                      putStrLn contents
                                      return ctx


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

