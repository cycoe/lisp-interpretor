module Main where


import System.Console.Haskeline (runInputT, defaultSettings)
import Libs.Shell (shell)
import Libs.Builtin (symbols)

main :: IO ()
main = runInputT defaultSettings (shell symbols)
