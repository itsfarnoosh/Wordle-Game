{-# LANGUAGE InstanceSigs #-}

module Wordle (module Wordle) where

import           Data.List (delete, elemIndices)

data WordleAnswers = Correct | Misplaced | Incorrect
  deriving (Eq)

instance Show WordleAnswers where
  show :: WordleAnswers -> String
  show Correct   = "green"
  show Misplaced = "yellow"
  show Incorrect = "gray"

type InternalState = Int

-- | generate the next element in a pseudorandom sequence from the prevSeed
-- Taken from the Week 10 workshop!
nextRand :: InternalState -> InternalState
nextRand prevSeed = (a * prevSeed + c) `mod` m
  where
    -- Parameters for linear congruential RNG.
    a = 1664525
    c = 1013904223
    m = 2 ^ (32 :: Int)

-- | Read the "src/all_words.txt" file in to a list
-- This files contains all possible five letter words
-- wordle will accept
-- >>> length <$> validWords
-- 14855
validWords :: IO [String]  -- The function returns an IO action that, when executed, produces a list of strings
validWords = do            -- 'do' introduces a block of IO actions
  contents <- readFile "src/all_words.txt"  -- 'readFile' reads the contents of the file as a single string, 'contents' holds the result
  return (lines contents)   -- 'lines' breaks the string into a list of lines (each line is a word in this case), and 'return' wraps it in IO


-- | Read the "src/possible_answers.txt" file in to a list
-- This files contains all possible answers wordle may use
-- >>> length <$> validAnswers
-- 2310
validAnswers :: IO [String]  -- The function returns an IO action that, when executed, produces a list of strings
validAnswers = do            -- 'do' introduces a block of IO actions
  contents <- readFile "src/possible_answers.txt"  -- 'readFile' reads the contents of the file as a single string, 'contents' holds the result
  return (lines contents)     -- 'lines' splits the single string into a list of lines (each line is an answer word), and 'return' wraps it in IO

-- | Generate a random Wordle answer
-- Do not use the current seed, but the next seed...
-- /Hint/: Use !! to index an array
-- >>> generateRandomAnswer 1
-- (1015568748,"halve")
-- >>> generateRandomAnswer 9223372036854775806
-- (1010575173,"reedy")
generateRandomAnswer :: InternalState -> IO (InternalState, String)
generateRandomAnswer seed = do
  wordsList <- validAnswers   -- Load the list of possible answers from the file
  let nextSeed = nextRand seed  -- Generate the next seed using the pseudo-random generator
      index = nextSeed `mod` length wordsList  -- Find the index by taking the modulo of nextSeed with the list length
      randomWord = wordsList !! index  -- Use the index to select a word from the list
  return (nextSeed, randomWord)  -- Return the next seed and the selected word

-- | Check if the user has guessed a valid word (a 5 letter word that is one of the validWords)
-- >>> checkValid "notValid"
-- False
-- >>> checkValid "hello"
-- True
-- >>> checkValid "waacs"
-- True
-- >>> checkValid "abcde"
-- False
checkValid :: String -> IO Bool
checkValid g = do
  validList <- validWords  -- Load the list of valid words
  return (g `elem` validList && length g == 5)  -- Check if the guessed word is in the list and has 5 letters

-- | Generate feedback for a Wordle guess compared to a target word.
-- This function compares two words of equal length letter by letter and returns
-- a list of WordleAnswers, which could be:
--   * `Correct`   -> The letter is in the correct position (green)
--   * `Misplaced` -> The letter is in the word but not in the correct position (yellow)
--   * `Incorrect` -> The letter is not in the word at all (gray)
--
-- The algorithm works in two passes:
-- 1. The first pass builds a list of "unmatched" letters in the target that have not yet been used.
-- 2. The second pass checks if the remaining letters in the guess are present in the
--    unmatched letters (Misplaced), or not present at all (Incorrect).
-- We need to keep track of the unmatched letters because we can't use the same letter in the target
-- more than once when marking a letter in the guess as Misplaced (yellow). See the
-- `makeFeedback "aaacc" "bbbaa"` test below for an example.
--
-- Feel free to do this yourself, it is a fun problem!
-- >>> makeFeedback "hello" "atoll"
-- [gray,gray,yellow,green,yellow]
-- >>> makeFeedback "clued" "queue"
-- [gray,gray,yellow,yellow,gray]
-- >>> makeFeedback "aaaaa" "abcde"
-- [green,gray,gray,gray,gray]
-- >>> makeFeedback "atlol" "goooy"
-- [gray,gray,gray,green,gray]
-- >>> makeFeedback "aaabb" "acada"
-- [green,yellow,green,gray,gray]
-- >>> makeFeedback "aaacc" "bbbaa"
-- [yellow,yellow,gray,gray,gray]
makeFeedback :: String -> String -> [WordleAnswers]
makeFeedback guess target = snd $ foldl f (unmatched, []) (zip guess target)
  where
    -- List of letters in target that are incorrect in the guess ('unmatched')
    unmatched = snd <$> filter (uncurry (/=)) (zip guess target)
    -- The fold function
    -- us: unmatched characters we haven't used yet
    -- answers: current list of WordleAnswers so far
    -- g: letter in guess
    -- t: letter in target
    f (us, answers) (g, t)
      -- If the letter was correct, return Correct
      | g == t      = (us,          answers ++ [Correct])
      -- If the letter is incorrect and we haven't used this character yet
      -- (it is still in the list of unmatched characters), return Misplaced (yellow)
      -- and remove it from the list of unmatched characters
      | g `elem` us = (delete g us, answers ++ [Misplaced])
      -- Otherwise, we have used this letter already, so it is Incorrect (grey)
      | otherwise   = (us,          answers ++ [Incorrect])

--   ___ ___                  .___    _____             .___
--  /   |   \_____ _______  __| _/   /     \   ____   __| _/____
-- /    ~    \__  \\_  __ \/ __ |   /  \ /  \ /  _ \ / __ |/ __ \
-- \    Y    // __ \|  | \/ /_/ |  /    Y    (  <_> ) /_/ \  ___/
--  \___|_  /(____  /__|  \____ |  \____|__  /\____/\____ |\___  >
--        \/      \/           \/          \/            \/    \/
-- In Hard Mode, when you receive clues such as green (correct position)
-- or yellow (correct letter, wrong position) for a letter in your guess,
-- your subsequent guesses must incorporate those clues

-- | All Correct Letters must be in consecutive guesses
-- >>> ensureGreens "slate" "slime" "slope"
-- True
-- >>> ensureGreens "slate" "plime" "slope"
-- False
ensureGreens :: String -> String -> String -> Bool
ensureGreens prev guess target = all (\(p, g, t) -> p /= t || g == t) (zip3 prev guess target)

-- | Ensure that yellow letters are in the next guess (can be anywhere)
-- >>> ensureYellows "slate" "slime" "slope"
-- True
-- >>> ensureYellows "world" "dlrow" "orwld"
-- True
-- >>> ensureYellows "aorld" "xxxxx" "orwld"
-- False
ensureYellows :: String -> String -> String -> Bool
ensureYellows prev guess target = all (\c -> c `elem` guess) misplacedLetters
  where
    misplacedLetters = [p | (p, t) <- zip prev target, p /= t, p `elem` target]


-- | Make sure the guesses follows both ensureGreens and ensureYellows as well, as the guess not being the same as previous
-- >>> ensureCriteria Nothing "stove" "ready"
-- True
-- >>> ensureCriteria (Just "aorld") "xxxxx" "orwld"
-- False
-- >>> ensureCriteria (Just "slate") "slime" "slope"
-- True
-- >>> ensureCriteria (Just "slate") "plime" "slope"
-- False
-- >>> ensureCriteria (Just "slate") "slate" "slope"
-- False
ensureCriteria :: Maybe String -> String -> String -> Bool
ensureCriteria Nothing guess target = True  
  -- If there is no previous guess (represented by 'Nothing'), the current guess is always valid.

ensureCriteria (Just prev) guess target = 
  guess /= prev &&                    -- Ensure the current guess is different from the previous guess.
  ensureGreens prev guess target &&    -- Check that the greens (correct positions) from the previous guess
                                       -- are still in the correct positions in the current guess.
  ensureYellows prev guess target      -- Check that all the yellow letters (correct but wrong position)
                                       -- from the previous guess are present somewhere in the current guess.