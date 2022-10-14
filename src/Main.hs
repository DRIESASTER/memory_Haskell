{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import System.Random.Shuffle (shuffle')

import Data.Fixed (mod')
import Control.Applicative (Alternative(empty))

-- Geeft de richting van een zet aan.
type Direction = (Int, Int)

-- De status van een kaart. Een kaart kan ofwel zijn kleur 
-- tonen, ofwel zijn achterkant tonen.
data CardStatus = Hidden | Shown deriving(Show, Eq)

-- Een positie op het speelveld.
type Coordinate = (Int, Int)

-- Representatie van een kaart. Een kaart heeft een status, 
-- een kleur en een positie.
data Card = Card {
    cardCoordinate::Coordinate,
    cardColor::Color,
    cardStatus::CardStatus} deriving Show


-- Representatie van het speelveld.
data Board = Board {
    -- Het speelveld is een lijst van kaarten.
    cards :: [Card],
    -- Hou de omgedraaide kaarten tijdens een beurt bij (maximaal 2).
    turned :: [Card],
    -- Hou de huidige geselecteerde kaart bij.
    selector :: Coordinate
}

-- Aantal kaarten op de x-as.
width :: Int
width = 3

-- Aantal kaarten op de y-as.
height :: Int
height = 3

-- De grootte van een kaart.
scaling :: Int
scaling = 150

-- De grootte van de ruimte tussen de kaarten.
cardInset :: Int
cardInset = 10

-- Initiele positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (200, 200)

-- Seed voor de random generator.
seed :: Int
seed = 45

-- Framerate van het spel.
fps :: Int
fps = 60

-- Bereken het aantal kaarten op het speelveld.
amountOfCards :: Int
amountOfCards   | even n    = n
                | otherwise = n - 1
                where n = width * height

-- Het Gloss venster
window :: Display
window = InWindow "Memory" (width * scaling, height * scaling) windowPosition
-- Het initiele speelveld.
initBoard :: Board
initBoard = Board {
    cards = generateShuffledCards amountOfCards,
    turned = [],
    selector = (0, 0)
}
----------------------------------------------------------------------
-- Vanaf hier zal het nodig zijn om de functies aan te vullen.
-- De functies die je moet aanvullen zijn steeds gemarkeerd met
-- undefined.
----------------------------------------------------------------------

-- De mogelijke richtingen van de selector.
left, right, up, down :: Direction
left  = (-1,0)
right = (1,0)
up    = (0,1)
down  = (0,-1)

-- Controleer of twee kaarten dezelfde kleur hebben.
match :: Card -> Card -> Bool
match card1 card2 = cardColor card1 == cardColor card2

-- Zoek een kaart binnen een lijst van kaarten op basis van een positie.
-- Wanneer een kaart gevonden is, wordt deze teruggegeven. Anders wordt
-- een error teruggegeven.
find :: Coordinate -> [Card] -> Card
find target cards
    | null cards = error "not found"
    | cardCoordinate (head cards) == target = head cards
    | otherwise = find target (drop 1 cards)


-- Geef een permutatie van een gegeven lijst terug.
-- Hint: Kijk zeker eens naar de System.Random en 
--       System.Random.Shuffle bibliotheken.
shuffleList :: [a] -> [a]
shuffleList l = shuffle' l (length l) (mkStdGen seed)

-- Genereer een lijst met n verschillende kleuren.
-- Hint: Je kan gebruikmaken van de generateColor-functie.
generateColors :: Int -> [Color]
generateColors n = [generateColor x | x <- take n $ randomRs(0.0,360.0) (mkStdGen seed)]

-- Genereer een lijst van n kaarten (n/2 kleurenparen).
-- coord nog doen, rest werkt wel al
generateShuffledCards :: Int -> [Card]
generateShuffledCards n = shuffleList [Card {cardCoordinate = (0,0), cardColor = generateColors (div n 2)!! div (x-1) 2 , cardStatus = Hidden} | x <- [1..n]]



-- Controleer of een positie op het spelbord een kaart bevat.
hasCard :: Coordinate -> Bool
hasCard (x, y) = hasCardSub (x, y) (cards initBoard)

hasCardSub :: Coordinate -> [Card] -> Bool
hasCardSub (x, y) l
    | length l == 0 = False
    | cardCoordinate (head l) == (x, y) = True
    | otherwise = hasCardSub (x, y) (drop 1 l)

-- Controleer of de selector vanaf een gegeven locatie in een 
-- gegeven richting kan bewegen.
canMove :: Coordinate -> Direction -> Bool
canMove coord direction = fst coord + fst direction < width && fst coord + fst direction >= 0 && snd coord + snd direction < height && snd coord + snd direction >= 0

-- Beweeg de selector in een gegeven richting.
move :: Board -> Direction -> Board
move board direction = board {selector = (fst(selector board) + fst direction , snd(selector board) + snd direction)}

-- Verander de status van een kaart op een gegeven positie 
-- wanneer de posities overeenkomen.
changeCard :: Coordinate -> CardStatus -> Card -> Card
changeCard c s card
    | cardCoordinate card == c = card {cardStatus = s}
    | otherwise = card

-- Verander de status van een enkele kaart in een reeks van 
-- kaarten. Deze functie geeft een lijst terug waar de status 
-- van de kaart is aangepast naar `Shown`.
showCard :: Coordinate -> [Card] -> [Card]
showCard target cards= [card | card <- cards, cardCoordinate card /= target] ++ [changeCard target Shown $ find target cards]

-- Verander de status van een enkele kaart in een reeks van 
-- kaarten. Deze functie geeft een lijst terug waar de status 
-- van de kaart is aangepast naar `Hidden`.
hideCard :: Coordinate -> [Card] -> [Card]
hideCard target cards= [card | card <- cards, cardCoordinate card /= target] ++ [changeCard target Hidden $ find target cards]

-- Draai de kaart op een gegeven positie op het bord om 
-- als deze nog niet eerder werd omgedraaid.
flipCard :: Coordinate -> Board -> Board
-- zoek kaart met find in (turned board) -> error dan moet ge hem nog draaien
flipCard target board = undefined

flipMultipleCards :: [Coordinate] -> Board -> Board
flipMultipleCards [c1] board = flipCard (head l) board
flipMultipleCards l board = flipMultipleCards (drop 1 l) (flipCard (head l) board)

-- Reset de laatste omgedraaide kaarten terug naar de `Hidden` status.
resetTurned :: Board -> Board
resetTurned board = flipMultipleCards [cardCoordinate $ cards board!!i | i <- [1..length (cards board)], i >= length (cards board)-1] board

-- Bereken het volgende bord op basis van de omgedraaide kaarten.
-- Hint: We hebben de drie gevallen voor deze functie al voorzien.
nextBoard :: Board -> Board
nextBoard b@Board{ turned = [] }         = b
nextBoard b@Board{ turned = [c1] }       = b
nextBoard b@Board{ turned = [c1, c2] }
    | cardColor c1 == cardColor c2 = b
    | otherwise = resetTurned b

-- Zet een positie op het bord om naar een positie op het scherm.
-- Hint: hou zeker rekening met het coordinatensysteem van Gloss.
convert :: Int -> Int -> Float
convert location axis = 0.0

-- Render een vierkant met een gegeven kleur en grootte.
renderColoredSquare :: Int -> Color -> Picture
renderColoredSquare size c = color c (rectangleSolid (sqrt fromIntegral size) (sqrt fromIntegral size))

-- Render de selector.
renderSelector :: Coordinate -> Picture
renderSelector coord = undefined

-- Render een kaart.
renderCard :: Card -> Picture
renderCard card = undefined

-- Render alle kaarten.
renderCards :: [Card] -> Picture
renderCards = undefined

-- Render het speelveld.
render :: Board -> Picture
render board = undefined

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _  _                                   = False

-- Handel alle toetsaanslagen af.
-- Hint: Je kan gebruikmaken van de isKey hulpfunctie.
handleInput :: Event -> Board -> Board
handleInput ev board = undefined

-- Startpunt
main :: IO ()
main = play window white fps initBoard render handleInput step

----------------------------------------------------------------------
-- Hieronder staan een aantal hulpfuncties die je kan gebruiken.
----------------------------------------------------------------------

-- Representatie van een HSL-kleur.
type HSL = (Float, Float, Float)

-- Representatie van een RGB-kleur.
type RGB = (Float, Float, Float)

-- Omzetting van de HSL-kleurenruimte naar de RGB-kleurenruimte.
hslToRgb :: HSL -> RGB
hslToRgb (h, s, l) = (r + m, g + m, b + m)
    where
        h' = h / 60
        c = (1 - abs (2 * l - 1)) * s
        x = c * (1 - abs (h' `mod'` 2 - 1))
        m = l - c / 2
        getRGB h
            | h < 1     = (c, x, 0)
            | h < 2     = (x, c, 0)
            | h < 3     = (0, c, x)
            | h < 4     = (0, x, c)
            | h < 5     = (x, 0, c)
            | otherwise = (c, 0, x)
        (r, g, b) = getRGB h'

-- Genereer een kleur op basis van een hue-waarde [0 - 360].
generateColor :: Float -> Color
generateColor hue = makeColor r g b 1
    where (r, g, b) = hslToRgb (hue, 0.5, 0.5)

-- Update het bord in elke stap.
step :: Float -> Board -> Board
step _ b = b
