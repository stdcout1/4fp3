
-- data: new represntation 
data Suit = Heart 
          | Diamond 
          | Spade
          | Club
instance Show Suit where 
    show Heart = "♥"
    show Diamond = "◆"
    show Spade = "♠"
    show Club = "♣"

-- data: new represntation 
--
--
data Rank
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Show, Eq, Ord, Enum, Bounded)


-- type: alias of existing type ex fn or somthign (has a tag)
-- new type: is new repr but is untagged (single constructor )
data Card = Card Suit Rank 

instance Show Card where 
    show (Card suit rank) = "hello"
