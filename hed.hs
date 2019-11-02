{-

File: LineEditor.hs
Author: Henry Farr

Programmed for a coding assignment by
  Dr. Fluet (mtf@cs.rit.edu)
  https://www.cs.rit.edu/~mtf/

Inspired to keep adding on, expanding
functionality.


A line editor is a tool for editing files
line by line in modeled after physical
teletype printers from the early days of
computing. Users on GNU/Linux or Unix
machines may be familiar with 'ed',
to which this program behaves most
similarly.

-}

--{-# OPTIONS -Wall -Wno-unused-imports #-}

module LineEditor where

import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Text.Read (readMaybe)
{-
Useful Text.Read functions.

readMaybe :: Read a => String -> Maybe a
-- Parse a string using the `Read` instance. Succeeds if there is exactly one valid result.
-}

import qualified Data.Map as Map
import qualified Data.Text as Text -- Unneeded?

import Data.Foldable (forM_)

data OpenFile = F FilePath [EditList String] deriving (Show)
data EditList a = EditList [a] [a] deriving (Show)

begin :: [a] -> EditList a
begin = EditList []

end :: EditList a -> [a]
end (EditList left right) = reverse left ++ right

make :: String -> String -> OpenFile
make path = F path . (:[]) . (begin . lines)

-- the IO actions later can just run (view 3) on a file or whatever
view :: Int -> EditList String -> [String]
view n (EditList left (r:right)) = 
  (reverse . prepend) left ++ (" * " ++ r) : prepend right
  where
    prepend = map (" - "++) . take n
view n (EditList left []) = map (" - "++) (reverse (take n left)) ++ [" * "]
  

delete :: EditList a -> EditList a
delete (EditList left (_:rightTail)) = EditList left rightTail
--delete (EditList (_:leftTail) [])    = EditList leftTail [] -- unnecessary case
delete el = el

insert :: EditList a -> a -> EditList a
insert (EditList left right) a = EditList left (a:right)

edit :: EditList a -> a -> EditList a
edit = insert . delete

forward :: EditList a -> EditList a
forward (EditList left (r:right)) = EditList (r:left) right
forward el = el

backward :: EditList a -> EditList a
backward (EditList (l:left) right) = EditList left (l:right)
backward el = el

apply :: (a -> a) -> Int -> a -> a
apply _ 0 = id
apply f n = f . apply f (n-1)

pureTail :: [a] -> [a]
pureTail (_:x:xs) = x:xs
pureTail xs = xs

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          args <- getArgs
          --putStrLn "Line Editor\n"
          if length args > 0 then
            do fileText <- readFile (head args)
               commandLoop [begin (lines fileText)]
          else  
            commandLoop [begin []]


-- editList is guaranteed to have at least one element
-- this is not pure by any stretch of the imagination
-- TODO replace with data that reinforces the 
-- "an EditList to edit exists", like NonEmpty or
-- some custome type.
commandLoop :: [EditList String] -> IO ()
commandLoop editList = 
   do putStr "> "
      command <- getChar
      arg <- getLine
      let arg' = if not (null arg) then Just (trim arg) else Nothing
      case Map.lookup command commands of   -- Ugly bit of deconstruction going on here
        Nothing    -> do putStrLn ("Not a command: " ++ show command)
                         commandLoop editList
        Just (_,f) -> do editList' <- f editList arg'
                         forM_ editList' commandLoop

-- Remove a leading space from a string, if one exists
-- Used because it is conventional to leave a space after
-- inputting a command before inputting arguments
trim :: String -> String
trim (' ':s) = s
trim string = string

-- Convenience function
justReturn :: a -> IO (Maybe a)
justReturn = return . Just

-- it's amusing to me that most of the IO functionality is in here
commands :: Map.Map 
              Char 
              (String, [EditList String] -> Maybe String -> IO (Maybe [EditList String]))
commands = Map.fromList [
  ('v', ("Show current line and the three preceeding and succeeding",
   \els@(el:_) args -> do let strings = view (optArg args toInt 3) el
                          mapM_ putStrLn strings
                          justReturn els)),
  ('l', ("Load a file, replacing the current file in the buffer.",
  \els args -> case args of   -- I would prefer to pattern match in the lambda args
                 Nothing -> do putStrLn "Need to supply name of a file."
                               justReturn els
                 Just arg' -> do text <- readFile arg'
                                 justReturn [begin (lines text)])),
  ('s', ("Save a file",
  \els@(el:_) args -> case args of
                        Nothing -> do putStrLn "Need to supply name of a file."
                                      justReturn els
                        Just arg' -> do writeFile arg' ((unlines . end) el)
                                        justReturn els)),
  ('d', ("Delete the line under the cursor",
  \els@(el:_) args -> justReturn (applyDefault delete 1 args el :els))),
  ('i', ("Insert a line at the cursor, before the previous line at the cursor",
  \els@(el:_) args -> case args of 
                        Just arg' -> justReturn (insert el arg':els)
                        Nothing   -> justReturn (insert el "":els))),
  ('e', ("Replace the line under the cursor with a new one",
  \els@(el:_) args -> case args of
                        Just arg' -> justReturn (edit el arg':els)
                        Nothing   -> justReturn (edit el "":els))),
  ('f', ("Advance the cursor a line",
  \(el:els) args -> justReturn (applyDefault forward 1 args el : els))),
  ('b', ("Move the cursor to a the line",
  \(el:els) args -> justReturn (applyDefault backward 1 args el : els))),
  ('q', ("Quit line editor", (const . const) (return Nothing))),
  ('u', ("Undo edits", 
  \els args -> justReturn (applyDefault pureTail 1 args els))),
  ('?', ("Shows help", -- TODO let bindings only need one let
  \els _ -> do let helpStrs  = foldr ((:) . fst) [] commands
               let helpPairs = zip (Map.keys commands) helpStrs
               let strs = map (\(c,s) -> putStrLn (c:':':' ':s)) helpPairs
               sequence_ strs
               justReturn els))
  ]

-- A not too nice function. Should probably use Maybe but its after midnight
optArg :: Maybe a -> (a->b) -> b -> b
optArg Nothing   _         arg = arg
optArg (Just ls) transform _   = transform ls

-- Apply a function a default number of times, unless
-- an argument was provided. Could probably be improved
-- if Maybe was used in optArg, and in the commandLoop
applyDefault :: (a -> a) -> Int -> Maybe String -> a -> a
applyDefault f n args = apply f (optArg args toInt n)

-- TODO Replace with ReadMaybe
toInt :: String -> Int
toInt = read :: String -> Int 

{-

README:
  Line Editor models files using a data structure nearly
  identical to the EditList from an earlier recitation.
  This edit list has reduced functionality. It does not
  instance any interesting type classes. The pure functions 
  that manipulate it are straight forward and are in the
  first half of the file.
  Basic construction, updating data, and moving the
  cursor.

  Then the meat of the program is in the commandLoop method.
  Command loop accepts a list of  edit lists as an argument. 
  (The list of edit lists is guaranteed non-empty).
  It prompts for commands and arguments. Commands trigger
  actions to potentially run the pure functions (generating
  a new edit list) and an updated list of edit lists is
  handed back to the command loop. It proceeds to loop on
  the result.
  
  The result is Maybe [EditList String]. 'Nothing' is
  returned when the user invokes the quit action, indicating
  to the command loop not to recurse.

  The list of edit lists represents the history of
  modifications. It is never allowed to be empty, so that
  each action does not have to individually handle the
  failure case of receiving an empty list. They are all
  free extract the first element via pattern matching.

  Actions are stored in a Map, paired with help text,
  keyed by the character which invokes them.
  This indirection means commandLoop does not need to
  know any of the underlying details of how commands
  work.
  The organization enables the Help command to generate
  help text during runtime.

  The optArg method allows commands to optionally accept 
  arguments for some commands. Take 'f'. If no arg is specified
  it will default to advancing the cursor by 1, but if the
  user gives an integer it will advance the cursor by that
  integer. 
  This enables some of the 'additional functionality',
  such as moving forward at a brisker pace than one
  line at a time. Perfect for bigger files if I do say so
  myself.

-}
