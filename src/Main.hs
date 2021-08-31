import System.IO
import GHC.IO.Handle.Types (Handle(FileHandle))
import Interpreter
import Parser
import Network.URI (parseAbsoluteURI)
import CodeGeneration
import DataStructures
import Emulator

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
    putStrLn ""
    putStrLn "-------- Def List --------"
    print defList
    putStrLn ""
    putStrLn "-------- Code --------"
    print code
    putStrLn ""
    putStrLn "-------- Result --------"
    let result = emulate (code, [], defList, [], EmptyInstruction, 0, 0)
    print result
    hClose fileHandle
