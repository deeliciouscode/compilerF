import System.IO
import GHC.IO.Handle.Types (Handle(FileHandle))
import Interpreter
import Parser
import Network.URI (parseAbsoluteURI)

main = do 
    filename <- getLine
    fileHandle <- openFile filename ReadMode 
    contents <- hGetContents fileHandle
    putStrLn ""
    putStrLn "------------ Program Code ------------"
    putStrLn contents
    putStrLn ""
    putStrLn "-------- Abstract Syntax Tree --------"
    print (parseWith parseProgram contents)
    hClose fileHandle
