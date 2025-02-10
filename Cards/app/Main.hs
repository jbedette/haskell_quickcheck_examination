-- Name: John Bedette and Vivek Srirama
-- Final Project
module Main (main) where

import BlackJack
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "\nRun quickCheck, fail when player 1 wins"
    runQuickCheckWin 
    putStrLn "\nRun quickCheck, fail wwhen player 1 loses"
    runQuickCheckLose
    putStrLn "\nExamples of a hand and possible action\
    \ the player might take after this round"
    sample generatePossibleAction
    putStrLn "\nExamples of a hand and possible winnings\
    \ if the hand beats the dealer's hand"
    sample generatePossibleBet
