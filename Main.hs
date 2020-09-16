import Tokens
import Grammar
import System.Environment
import Control.Exception
import System.IO
import Data.List
import Eval
import Data.Char


main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs 
           sourceText <- readFile fileName
--           putStrLn ("Tokens : " ++ ( show (alexScanTokens sourceText)) ++ "\n \n")
           let parsedProg = parseCalc (alexScanTokens sourceText)
--           putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
           content <- getContents
           let sep = lines content
           let lists = transfer sep
           let result = evalLoop (toState parsedProg (transform lists))
           if lists == [] then return()
           else (mapM_ print (reverse $ read result :: [Int] ))
           


noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

transfer :: [String] -> [[Int]]
transfer [] = []
transfer (x:xs) | allInt (x:xs) = [(map read $ words x :: [Int])] ++ transfer xs
                | hasEmpty (x:xs) = []
                | otherwise = error "[Invalid Input] Expected Integers"

allInt [] = True
allInt ((y:ys):xs) | isDigit y = allInt xs
                   | otherwise = False

hasEmpty [] = True
hasEmpty ((y:ys):xs) | y == ' ' = hasEmpty xs
                     | otherwise = False

toSpaceSeparatedString :: [String] -> String
toSpaceSeparatedString = intercalate " "

matrixToStringList :: [[Int]] -> [String]
matrixToStringList (x:xs) = [(toSpaceSeparatedString (map show x))]++matrixToStringList xs
matrixToStringList [] = []

transform:: [[a]]->[[a]]
transform [] = [] 
transform ([]:_) = []
transform x = (map head x) : transform (map tail x)
