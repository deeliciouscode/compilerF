import System.IO
import GHC.IO.Handle.Types (Handle(FileHandle))
import System.Environment
import Parser
import CodeGeneration
import DataStructures
import Lexer
import Emulator
import Data.Maybe

main = do 
    filename <- getArgs
    fileHandle <- openFile (head filename) ReadMode 
    contents <- hGetContents fileHandle
    putStrLn ""
    putStrLn "------------ Program Code ------------"
    putStrLn contents
    putStrLn ""
    putStrLn "--------------- Tokens ---------------"
    let tokens = genListOfTokens contents
    print tokens
    putStrLn ""
    putStrLn "-------- Abstract Syntax Tree --------"
    let (Just ast, _) = parseProgram tokens
    print ast
    putStrLn ""
    putStrLn "-------------- Def List --------------"
    let (defList, code) = generate ast
    print defList
    putStrLn ""
    putStrLn "---------------- Code ----------------"
    print code
    putStrLn ""
    putStrLn "--------------- Result ---------------"
    let result = emulate (code, [], defList, [], 0, 0)
    print result
    hClose fileHandle
