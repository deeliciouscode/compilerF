import System.IO
import GHC.IO.Handle.Types (Handle(FileHandle))
import Interpreter
import Parser
import Network.URI (parseAbsoluteURI)
import CodeGeneration

main = do 
    filename <- getLine
    fileHandle <- openFile filename ReadMode 
    contents <- hGetContents fileHandle
    putStrLn ""
    putStrLn "------------ Program Code ------------"
    putStrLn contents
    putStrLn ""
    putStrLn "-------- Abstract Syntax Tree --------"
    let ast = parseWith parseProgram contents
    print ast
    let (defList, code) = gen ast
    putStrLn "-------- Def List --------"
    print defList
    putStrLn "-------- Code --------"
    print code
    putStrLn "-------- Result --------"
    
    hClose fileHandle
