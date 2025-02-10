-- Name: John Bedette and Vivek Srirama
-- Final Project

module BlackJack (runQuickCheckWin, runQuickCheckLose, generatePossibleAction, generatePossibleBet) where

import Test.QuickCheck


-- Function to check if player 1 has won against the dealer
hasPlayer1Won :: ([Card], [Card], [Card], [Card], [Card]) -> Bool
hasPlayer1Won (player1, _, _, _, dealer) =
  not (handValue player1 > handValue dealer && handValue player1 < 21)

hasPlayer1Lost :: ([Card], [Card], [Card], [Card], [Card]) -> Bool
hasPlayer1Lost (player1, _, _, _, dealer) = 
  handValue player1 > handValue dealer && handValue player1 < 21

-- Running QuickCheck to test if player 1 has won against the dealer
runQuickCheckWin :: IO ()
runQuickCheckWin = quickCheck (forAll dealHands hasPlayer1Won)

-- Running QuickCheck to test if player 1 has lost against the dealer
runQuickCheckLose :: IO ()
runQuickCheckLose = quickCheck (forAll dealHands hasPlayer1Lost)

-- Function to calculate the sum of a hand
handValue :: [Card] -> Int
handValue = sum . map (\(Card value) -> value)

-- Define the Card data type
data Card = Card Int deriving (Show, Eq)

-- Define the Acfion data type
data Action = ContinuePlaying | LeaveGame deriving (Show, Eq) 

-- Arbitrary instance for a Card
instance Arbitrary Card where
  arbitrary = do
    value <- choose (1, 2)
    return (Card value)

-- Generate a random playing card
generateCard :: Gen Card
generateCard = do
  value <- choose (1, 11) -- Generate a random value between 1 and 10
  return (Card value)

-- Generate a hand of two playing cards
generateHand :: Gen [Card]
generateHand = do
  card1 <- generateCard -- Generate the first card
  card2 <- generateCard -- Generate the second card
  return [card1, card2]

-- A generator to generate possible bets that can be placed
generateBet :: Gen Int
generateBet = elements [5..100]

-- Produces a generator for a tuple of a list of cards and an integer
generatePossibleBet :: Gen ([Card], Int)
generatePossibleBet = do
  hand <- generateHand
  bet <- generateBet
  return (hand, bet)

-- Generates an action
generateAction :: Gen Action
generateAction = frequency [(2, return LeaveGame), (6, return ContinuePlaying)]

-- Produces a generator for a tuple of a list of cards and an action
generatePossibleAction :: Gen ([Card], Action)
generatePossibleAction = do
  hand <- generateHand
  action <- generateAction
  return (hand, action)


-- Deal out four players and a dealer
dealHands :: Gen ([Card], [Card], [Card], [Card], [Card])
dealHands = do
  player1 <- generateHand -- Generate hand for player 1
  player2 <- generateHand -- Generate hand for player 2
  player3 <- generateHand -- Generate hand for player 3
  player4 <- generateHand -- Generate hand for player 4
  dealer <- generateHand -- Generate hand for the dealer
  return (player1, player2, player3, player4, dealer)