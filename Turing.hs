{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module emulates a simple Turing Machine.
module Main where

import System.Environment (getArgs)
import Text.Printf (printf, fmtChar, fmtPrecision, formatArg, vFmt, formatString, errorBadFormat, PrintfArg)

import Data.List (intercalate)
import Text.Parsec hiding (State(..))

-- | Direction to move on the tape.
data Direction = L | R
  deriving (Show, Read, Eq, Ord)

-- | Symbols on the tape are represented by characters, '_' being reserved for the blank symbol.
newtype Symbol = Sym { retrieveChar :: Char } deriving (Eq, Ord, PrintfArg)

-- | Symbols are shown without any quotes or constructor.
instance Show Symbol where
  show c = [retrieveChar c]

-- | Symbols are read without any quotes or constructor.
instance Read Symbol where
  readsPrec _ str = case parse sym "" str of
    (Right symbol) -> [(symbol, "")]
    (Left _) -> []

-- | States are labeled with strings.
newtype State  = St { retrieveString :: String } deriving (Eq, Ord, PrintfArg)

-- | States are shown without any quotes or constructor.
instance Show State where
  show = retrieveString

-- | States are read without any quotes or constructor.
instance Read State where
  readsPrec _ str = case parse st "" str of
    (Right state) -> [(state, "")]
    (Left _) -> []

-- | A Turing Machine is represented by a list of tuples δ, representing the update function; q, the current / initial state; and f, a list of final states.
data Turing = Turing { δ :: [((State, Symbol), (State, Symbol, Direction))]
                     , q :: State
                     , f :: [State]
                     } deriving (Eq)

-- | A Turing-machine is shown as a three-tuple containing the initial state, a list of final states and a list of mappings for.δ.
instance Show Turing where
  show (Turing δ q f) = printf "<%s, [%s], [%s]>" q (intercalate ", " . map retrieveString $ f) (intercalate ", " . map showRule $ δ)
    where showRule ((s,y), (s', y', d)) = printf "%s %c → %s %c %s" s y s' y' (show d)

-- | Reads a Turing machine. Accepts both output from show and an ASCII-variant with -> instead of arrows.
instance Read Turing where
  readsPrec _ str = case parse tm "" str of
    (Right machine) -> [(machine, "")]
    (Left _) -> []

-- | The tape is represented by a zipper over a list of Symbols.
data Tape = Tape [Symbol] [Symbol]

-- | Tapes are printed as a single list of characters separated by spaces. A 'q' indicates the current head position, and a blank is added at the end to indicate the end of the tape.
instance Show Tape where
  show (Tape l r) = unwords $ map show l ++ "q" : (map show . takeWhile (not . (== blank)) $ r) ++ ["_"]

-- | Tapes are read the same way they are shown.
instance Read Tape where
  readsPrec _ input =
    let input' = words input
        (l,r) | 'q' `elem` input = splitAtElement "q" input'
              | otherwise        = ([], input')
    in [(Tape (map read l) (map read r), "")]

-- | Tape is made an instance of PrintfArg for easier printing of computation results.
instance PrintfArg Tape where
  formatArg x fmt | fmtChar (vFmt 'T' fmt) == 'T' =
    formatString (show x) (fmt { fmtChar = 's', fmtPrecision = Nothing })
  formatArg _ fmt = errorBadFormat $ fmtChar fmt

-- | A blank is represented by the '_' character.
blank :: Symbol
blank = Sym '_'

-- * Tape manipulation

-- | Convert a list of symbols into a tape.
fromList :: [Symbol] -> Tape
fromList = Tape [blank] . (++ (repeat blank))

-- | Move the tape to the right.
right :: Tape -> Tape
right (Tape l r) = Tape (l ++ [head r]) (tail r)

-- | Move the tape to the left.
left :: Tape -> Tape
left (Tape l r) = Tape (init l) (last l : r)

-- | Read the current symbol on the tape.
current :: Tape -> Symbol
current (Tape _ r) = head r

-- | Replace the symbol on the current position, then move the tape left or right.
update :: Symbol -> Direction -> Tape -> Tape
update s dir (Tape l r) = case dir of
                            L -> left  tape'
                            R -> right tape'
  where tape' = Tape l (s : tail r)

-- * Running the machine and printing the results

-- | Run the Turing Machine on the tape, yielding a list of state/tape tuples culminating in either a final state and tape, or an error when δ is not total.
run :: Turing-> Tape -> [Either String (State, Tape)]
run tm tape | state `elem` final = [Right (state, tape)]
            | otherwise = case lookup (state, current tape) (δ tm) of
                Just (newstate, newsymbol, dir) -> let tape' = update newsymbol dir tape
                                                   in  Right (state, tape) : (run (tm {q = newstate}) $ tape')
                Nothing -> [Left "missing rule in δ or unexpected state/symbol combination!"]
  where state = q tm
        final = f tm

-- | Show the aggregated result of running Turing Machine tm on Tape tape.
showResults :: Turing -> Tape -> String
showResults tm = ("  " ++) . intercalate "\n⇝ " . map showResult . run tm

-- | Show an intermediate result of running Turing Machine tm on Tape tape for a single step.
showResult :: Either String (State, Tape) -> String
showResult (Right (result, tape)) = printf "(%s, %T)" result tape
showResult (Left error) = printf "Error: %s" error

-- | Read a Turing Machine and tape from the command line and show the result. If no arguments are provided, the Machine and tape are read from the console.
main = do
  putStrLn ""
  args <- getArgs
  input <- if length args > 0
    then return $ unwords args
    else getLine
  case parse computation "" input of
    Right computation -> putStrLn $ uncurry showResults computation
    Left  error       -> print error

-- * Utility

-- | Split a list at the position of a given element; used to read the tape from a string.
splitAtElement :: Eq a => a -> [a] -> ([a],[a])
splitAtElement e es = (takeWhile (/= e) es, tail $ dropWhile (/= e) es)

-- * Parsers

-- | Type synonym for a string parser with no state.
type Parser = Parsec String ()

-- | Values are separated by commas and optional whitespace.
sep :: Parser ()
sep = spaces >> char ',' >> spaces

-- | Parses a computation, being a Turing Machine followed by a tape.
computation :: Parser (Turing, Tape)
computation = do spaces
                 machine <- tm
                 spaces
                 input <- tape
                 optional newline
                 return (machine, input)

-- | Parses a Turing machine.
tm :: Parser Turing
tm = do char '<'
        spaces
        q <- st
        sep
        f <- between (char '[') (char ']') (sepBy st sep)
        sep
        δ <- between (char '[') (char ']') (sepBy mapping sep)
        spaces
        char '>'
        return $ Turing δ q f

-- | Parses a δ function.
mapping :: Parser ((State, Symbol), (State, Symbol, Direction))
mapping = do preState <- st
             spaces
             preSym <- sym
             spaces
             string "->" <|> string "→"
             spaces
             postState <- st
             spaces
             postSym <- sym
             spaces
             direction <- direction
             return ((preState, preSym), (postState, postSym, direction))

-- | Parses a state.
st :: Parser State
st = St <$> many1 (alphaNum <|> char '_')

-- | Parses a symbol.
sym :: Parser Symbol
sym = Sym <$> (letter <|> char '_')

-- | Parses a direction.
direction :: Parser Direction
direction = read <$> (string "L" <|> string "R")

-- | Parses a tape.
tape :: Parser Tape
tape = fromList <$> (spaces >> sepBy sym space)
