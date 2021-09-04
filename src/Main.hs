import System.IO
import GHC.IO.Handle.Types (Handle(FileHandle))
import Interpreter
import Parser
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
    let (defList, code) = generate ast
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


---------------------------------------------------------------

-- k1 0 (k1 1 2)

-- k1 (k1 0 1) 2

-- k1 1 (3 + 2)

-- out' = AppX (AppX (AppX (VarX "k1") (IntX 0)))
-- out = AppX 
--         (AppX 
--             (AppX 
--                 (VarX "k1") 
--                 (IntX 0)
--             ) 
--             (AppX 
--                 (VarX "k1") 
--                 (IntX 1)
--             )
--         ) 
--         (IntX 2)