import Cartas
import System.Random
import Data.Array.IO
import Control.Monad
import Data.List
import qualified System.Process as SP


main = do
    
    shuffledDeck <- shuffle baralho
    
    let jogador1 = criaJogador 0 shuffledDeck
    
    let shuffledDeck1 = atualizaBaralho True 4 shuffledDeck
    
    let shuffledDeck = shuffledDeck1
    
    let jogador2 = criaJogador 0 shuffledDeck
    
    let shuffledDeck1 = atualizaBaralho True 4 shuffledDeck
    
    let shuffledDeck = shuffledDeck1
    
    SP.system "clear"
    putStr "Jogador 1: "
    print jogador1
    putStr "Jogador 2: "
    print jogador2
    print mesa 
    
    atualizaJogo mesa jogador1 jogador2 shuffledDeck  
       

atualizaJogo mesaInit jogador1 jogador2 shuffledDeck = do
    
    --atualizaMonte mesaInit jogador1 jogador2 shuffledDeck  
    if length jogador1 == 0 || length jogador2 == 0   
        then do putStr "Jogador 1: "
                print jogador1
                putStr "Jogador 2: "
                print jogador2
                --return ()  
        else do jogaJogador1 mesaInit jogador1 jogador2 shuffledDeck
            --atualizaJogo mesaInit jogador1 jogador2 shuffledDeck                             

jogaJogador1 mesaInit jogador1 jogador2 shuffledDeck = do
    -- Tamanho da lista do jogador 1 antes de tentar jogar uma carta
    let lenJ11 = length jogador1
    
    line <- getLine
    SP.system "clear"
    let mesa2 = atualizaMesa mesaInit jogador1 (length jogador2)
    
    putStr "Jogador 1: "
    print jogador1
    print mesa2
    
    let auxJogador1 = jogaCarta mesa2 jogador1 (length jogador2)
    let jogador1 = auxJogador1
    
    -- Tamanho da lista do jogador 1 depois de tentar jogar uma carta
    let lenJ12 = length jogador1
    
    
    if (lenJ11 == lenJ12)
        then do incementaMaoJogador1 mesa2 jogador1 jogador2 shuffledDeck
        
        else do jogaJogador2 mesa2 jogador1 jogador2 shuffledDeck
    
incementaMaoJogador1 mesaInit jogador1 jogador2 shuffledDeck = do
    
    line <- getLine
    SP.system "clear"
    let auxJogador1 = incrementaMao True jogador1 shuffledDeck
    let jogador1 = auxJogador1
    
    let auxShuffledDeck = atualizaBaralho True 1 shuffledDeck
    let shuffledDeck = auxShuffledDeck
    
    let mesa2 = atualizaMesa mesaInit jogador1 (length jogador2)
    
    putStr "Jogador 1 (Atualizado): "
    print jogador1
    print mesa2
    
    let auxJogador1 = jogaCarta mesa2 jogador1 (length jogador2)
    let jogador1 = auxJogador1

    
    jogaJogador2 mesa2 jogador1 jogador2 shuffledDeck

jogaJogador2 mesaInit jogador1 jogador2 shuffledDeck = do

    -- Tamanho da lista do jogador 2 antes de tentar jogar uma carta
    let lenJ21 = length jogador2
    
    line <- getLine
    SP.system "clear"
    let mesa2 = atualizaMesa mesaInit jogador2 (length jogador1)

    putStr "Jogador 2: "
    print jogador2
    print mesa2

    let auxJogador2 = jogaCarta mesa2 jogador2 (length jogador1)
    let jogador2 = auxJogador2
    
    -- Tamanho da lista do jogador 2 depois de tentar jogar uma carta
    let lenJ22 = length jogador2
    
    
    if (lenJ21 == lenJ22)
        then do incementaMaoJogador2 mesa2 jogador1 jogador2 shuffledDeck
        
        else do atualizaJogo mesa2 jogador1 jogador2 shuffledDeck
    
incementaMaoJogador2 mesaInit jogador1 jogador2 shuffledDeck = do
    
    line <- getLine
    SP.system "clear"
    let auxJogador2 = incrementaMao True jogador2 shuffledDeck
    let jogador2 = auxJogador2
    
    let auxShuffledDeck = atualizaBaralho True 1 shuffledDeck
    let shuffledDeck = auxShuffledDeck
    
    let mesa2 = atualizaMesa mesaInit jogador2 (length jogador1)
    
    putStr "Jogador 2 (Atualizado): "
    print jogador2
    print mesa2
    
    let auxJogador2 = jogaCarta mesa2 jogador2 (length jogador1)
    let jogador2 = auxJogador2

    
    atualizaJogo mesa2 jogador1 jogador2 shuffledDeck
    
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

jogaCarta :: Carta -> [Carta] -> Int -> [Carta]
jogaCarta _ (x:xs) 0 = (x:xs)
jogaCarta c (x:xs) n = delete c (x:xs) 
    
criaJogador :: Int -> [Carta] -> [Carta]
criaJogador 4 _ = []
criaJogador a (x:xs) = x: criaJogador (a+1) xs

{-jogador1 :: [Carta]
jogador1 = [Carta "Um" "Amarela",Carta "Um" "Verde", Carta "Dois" "Vermelha", Carta "Dois" "Verde"]

jogador2 :: [Carta]
jogador2 = [Carta "Um" "Vermelha", Carta "Tres" "Verde", Carta "Dois" "Amarela", Carta "Dois" "Verde"]-}

mesa :: Carta 
mesa = Carta "Um" "Azul"

atualizaMesa :: Carta -> [Carta] -> Int -> Carta
atualizaMesa (Carta t c) _ 0 = Carta "" "" 
atualizaMesa (Carta t c) [] a = (Carta t c)
atualizaMesa (Carta t c) (head:tail) a
        | (t == tipo head) || (c == cor head) = head
        | otherwise =  atualizaMesa (Carta t c) tail a

atualizaBaralho :: Bool -> Int -> [Carta] -> [Carta]
atualizaBaralho True n (x:xs) = drop n (x:xs)
atualizaBaralho False _ (x:xs) = (x:xs)

incrementaMao :: Bool -> [Carta] -> [Carta] -> [Carta]
incrementaMao True (x:xs) (y:ys) = y:(x:xs)
incrementaMao False (x:xs) _ = (x:xs) 






 
