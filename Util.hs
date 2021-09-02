module Util  
( embaralhaLista  
, mapeiaOpcaoJogarReal  
, mapeiaQuantidadeJogadores  
, mapeiaIndicePorString  
, mapeiaCorPorNumero  
, exibiCartasViaveis
, atualizaCorCoringaJogadorReal
, atualizaCorCoringa 
, atualizaMesa
, atualizaBaralho
, atualizaListaJogadores
, jogaCarta
, criaListaJogadores
, testJogaCoringaMais4
, incrementaMao
, cores
, numProxJogador
, inverteOrdem
, ajustaIndice
, listaCartasSorteio
, converteValorCarta
, contaValorRepetidoCarta
, calculaMaiorValorCarta
, findIndiceCarta
, selecionaPrimeiraCartaNaoCoringa
, verificaViabilidadeJogada
, verificaViabilidadeIndice
, verificaIndicesDisponiveis
) where  

import Cartas
import System.Random
import Data.Array.IO
import Control.Monad
import Data.List

embaralhaLista :: [a] -> IO [a]
embaralhaLista lista = do
        listaEmbalhada <- novaLista len lista
        forM [1..len] $ \indice -> do
            noveIndice <- randomRIO (indice,len)
            elem1 <- readArray listaEmbalhada indice -- readArray: Read an element from a mutable array 
            elem2 <- readArray listaEmbalhada noveIndice
            writeArray listaEmbalhada noveIndice elem1 -- writeArray: Write an element in a mutable array 
            return elem2
  where
    len = length lista
    novaLista :: Int -> [a] -> IO (IOArray Int a)
    novaLista len lista = newListArray (1,len) lista -- newListArray: Constructs a mutable array from a list of initial elements. The list gives the elements of the array in ascending order beginning with the lowest index. 


mapeiaOpcaoJogarReal :: String -> Int
mapeiaOpcaoJogarReal numStr
        | numStr == "1" = 1
        | numStr == "2" = 2
        | otherwise     = (-1) 


mapeiaQuantidadeJogadores :: String -> Int
mapeiaQuantidadeJogadores numStr
        | numStr == "2" = 2
        | numStr == "3" = 3
        | numStr == "4" = 4
        | otherwise     = (-1)  

        
mapeiaIndicePorString :: String -> Int -> Int
mapeiaIndicePorString numString lenMao
        | numString `elem` [show x | x <- [0,1 .. (lenMao-1)]]  = read numString 
        | otherwise        = (-1)        
  
        
mapeiaCorPorNumero :: String -> String
mapeiaCorPorNumero numStr                    
        | numStr == "1" = "Azul"
        | numStr == "2" = "Verde"
        | numStr == "3" = "Amarela"
        | numStr == "4" = "Vermelha"
        | otherwise     = "null"
          
          
exibiCartasViaveis :: Int -> [Int] -> [Carta] -> String
exibiCartasViaveis i indices _
        | i == length indices = ""
exibiCartasViaveis i indices cartas = (show (indices !! i)) ++ ": "  ++ (show (cartas !! (indices !! i))) ++ "\n" ++  (exibiCartasViaveis (i + 1) indices cartas)
 

atualizaCorCoringaJogadorReal :: String -> Carta -> Carta
atualizaCorCoringaJogadorReal cor (Carta t c) = (Carta t cor)


atualizaCorCoringa :: Bool -> String -> Carta -> Carta
atualizaCorCoringa teste cor (Carta t c)
        | teste == True = (Carta t cor)
        | teste == False = (Carta t c)
        
        
atualizaMesa :: Carta -> [Carta] -> Bool -> Carta
atualizaMesa (Carta t c) [] _ = (Carta t c)
atualizaMesa (Carta t c) (head:tail) teste
        | teste = (Carta "CoringaMais4" "Preta")
        | (t == tipo head) || (c == cor head) || (tipo head == "Coringa") = head
        | otherwise = atualizaMesa (Carta t c) tail teste 

   
atualizaBaralho :: Int -> [Carta] -> [Carta]
atualizaBaralho n (x:xs) = drop n (x:xs)


atualizaListaJogadores :: Int -> [[Carta]] -> [[Carta]] -> [[Carta]]
atualizaListaJogadores index jog listJog = (take (index) listJog) ++ jog ++ (drop (index+1) listJog)


jogaCarta :: Carta -> [Carta] -> [Carta]
jogaCarta _ [] = []
jogaCarta c (x:xs) = delete c (x:xs)


criaJogadorParaLista :: Int -> Int -> [Carta] -> [Carta]
criaJogadorParaLista 4 _ _ = []
criaJogadorParaLista numCards index deck = (deck !! index): criaJogadorParaLista (numCards+1) (index+1) deck


criaListaJogadores :: Int -> Int -> [Carta] -> [[Carta]]
criaListaJogadores 0 _ _ = []
criaListaJogadores nPlayers index deck = [criaJogadorParaLista 0 index deck] ++ criaListaJogadores (nPlayers-1) (index + 4) deck       


testaViabilidadeCoringaMais4 :: Carta -> [Carta] -> Bool
testaViabilidadeCoringaMais4 (Carta t c) [] = True
testaViabilidadeCoringaMais4 (Carta t c) (head:tail)
        | (t == tipo head) && (t /= "CoringaMais4") = False
        | (c == cor head) = False
        | (tipo head == "Coringa") = False
        | otherwise = testaViabilidadeCoringaMais4 (Carta t c) tail


contemCoringaMais4 :: [Carta] -> Bool
contemCoringaMais4 [] = False
contemCoringaMais4 (head:tail) 
        | head == (Carta "CoringaMais4" "Preta") = True
        | otherwise = contemCoringaMais4 tail


testJogaCoringaMais4 :: Carta -> [Carta] -> Bool
testJogaCoringaMais4 carta mao
        | (testaViabilidadeCoringaMais4 carta mao) && (contemCoringaMais4 mao) = True
        | otherwise = False


incrementaMao :: Int -> [Carta] -> [Carta] -> [Carta]
incrementaMao n (x:xs) (y:ys) = (take n (y:ys)) ++ (x:xs)


cores :: [String]
cores = ["Azul", "Verde", "Amarela", "Vermelha"]
        

numProxJogador :: String -> Int -> Int -> Int
numProxJogador ord anterior numJogadores
        | (ord == "horario") && ( (anterior + 1) >= (numJogadores) ) = 0 
        | (ord == "horario") && ( (anterior + 1) <  (numJogadores) ) = anterior + 1
        | (ord == "anti-horario") && ( (anterior - 1) <= -1 ) = numJogadores - 1
        | (ord == "anti-horario") && ( (anterior - 1) > -1 ) = anterior - 1


inverteOrdem :: String -> String
inverteOrdem ord
        | ord == "horario" = "anti-horario"
        | ord == "anti-horario" = "horario"


ajustaIndice :: Int -> Int -> Int
ajustaIndice indiceJogador numJogadores
        | (indiceJogador == 0) && (numJogadores == 2) = 1
        | (indiceJogador == 1) && (numJogadores == 2) = 0
        | otherwise = indiceJogador


listaCartasSorteio :: Int -> [Carta] -> [Carta]
listaCartasSorteio num baralho = take num baralho


converteValorCarta :: Carta -> Int
converteValorCarta (Carta t _)
        | t == "Zero"   = 0
        | t == "Um"     = 1
        | t == "Dois"   = 2
        | t == "Tres"   = 3
        | t == "Quatro" = 4
        | t == "Cinco"  = 5
        | t == "Seis"   = 6
        | t == "Sete"   = 7
        | t == "Oito"   = 8
        | t == "Nove"   = 9
        | otherwise     = 0


contaValorRepetidoCarta :: Carta -> [Carta] -> Int
contaValorRepetidoCarta carta [] = 0
contaValorRepetidoCarta carta (head:tail)
        | (converteValorCarta carta) == (converteValorCarta head) = 1 + contaValorRepetidoCarta carta tail
        | otherwise = 0 + contaValorRepetidoCarta carta tail 
   
        
calculaMaiorValorCarta :: Carta -> [Carta] -> Carta
calculaMaiorValorCarta carta [] = carta
calculaMaiorValorCarta carta (head:tail)
        | (converteValorCarta head) > (converteValorCarta carta) = calculaMaiorValorCarta head tail
        | otherwise = calculaMaiorValorCarta carta tail 


findIndiceCarta :: Carta -> [Carta] -> Int
findIndiceCarta carta cartas = fromJustToInt(carta `elemIndex` cartas)


fromJustToInt :: Maybe Int -> Int
fromJustToInt (Just i) = i 
fromJustToInt Nothing = error "Valor inexistente."


selecionaPrimeiraCartaNaoCoringa :: [Carta] -> Carta
selecionaPrimeiraCartaNaoCoringa (head:tail)
        | ("Preta" == cor head) = selecionaPrimeiraCartaNaoCoringa tail
        | otherwise = head
 
        
verificaViabilidadeJogada :: Int -> Carta -> [Carta] -> Bool
verificaViabilidadeJogada indice mesa mao
        | indice == length mao = False  
        | verificaViabilidadeIndice indice mesa mao = True
        | otherwise = verificaViabilidadeJogada (indice+1) mesa mao
   
        
verificaIndicesDisponiveis :: Int -> Carta -> [Carta] -> [Int]
verificaIndicesDisponiveis indice mesa mao
        | indice == length mao = []
        | verificaViabilidadeIndice indice mesa mao = indice : verificaIndicesDisponiveis (indice+1) mesa mao
        | otherwise = verificaIndicesDisponiveis (indice+1) mesa mao
        
        
verificaViabilidadeIndice :: Int -> Carta -> [Carta] -> Bool
verificaViabilidadeIndice (-1) _ _ = False
verificaViabilidadeIndice i mesa mao
        | (tipo (mao !! i) == "CoringaMais4") && (testaViabilidadeCoringaMais4 mesa mao) = True
        | (tipo (mao !! i) == tipo mesa) && (tipo mesa /= "CoringaMais4") = True
        | ((cor (mao !! i)) == (cor mesa)) = True
        | tipo (mao !! i) == "Coringa" = True
        | otherwise = False
