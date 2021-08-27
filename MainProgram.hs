import Cartas
import System.Random
import Data.Array.IO
import Control.Monad
import Data.List
import qualified System.Process as SP

main = do
    SP.system "clear"
    
    shuffledDeck <- shuffle baralho
       
    putStr "Qual a quantidade de jogadores desejada? "
    numJ <- getLine
    SP.system "clear"
    
    let numJogadores = (read numJ :: Int) 
    let listaJogadores = criaListaJogadores numJogadores 0 shuffledDeck
    let newDeck = atualizaBaralho (numJogadores*4) shuffledDeck
    
    putStrLn "Lista de Cartas Retiradas do Monte Embaralhado Para Cada Jogador: "
    putStrLn ""
    if (numJogadores == 2)
        then do painelInicial listaJogadores 2
    
    else if (numJogadores == 3)
        then do painelInicial listaJogadores 3
     
    else if (numJogadores == 4)
        then do painelInicial listaJogadores 4
             
        else do return() 
    
    line <- getLine
    
    sorteiaJogadorIniciante numJogadores listaJogadores newDeck 
       
sorteiaJogadorIniciante numJogadores listaJogadores newDeck = do
    
    shuffledDeck <- shuffle newDeck
    
    let cartasSorteio = listaCartasSorteio numJogadores shuffledDeck
    
    let maiorCarta = calculaMaiorValorCarta (Carta "Zero" "") cartasSorteio
    
    let numValorRepetido = contaValorRepetidoCarta maiorCarta cartasSorteio
    
    let indiceMaiorCarta = -1
    
    if (numValorRepetido > 1)
        then do painelSorteio cartasSorteio numJogadores indiceMaiorCarta
                line <- getLine 
                sorteiaJogadorIniciante numJogadores listaJogadores newDeck
        else do let indiceMaiorCarta = findIndiceCarta maiorCarta cartasSorteio  
                painelSorteio cartasSorteio numJogadores indiceMaiorCarta
                iniciaPartica listaJogadores indiceMaiorCarta numJogadores shuffledDeck 

iniciaPartica listaJogadores indiceJogadorInicial numJogadores newDeck = do

    shuffledDeck <- shuffle newDeck
    
    let mesaInicial = shuffledDeck !! 0
    putStrLn ""    
    putStr "Mesa inicial: "    
    print mesaInicial  
    
    let newDeck = atualizaBaralho 1 shuffledDeck
    let ordInicial = "horario"   
    
    line <- getLine
        
    turnoJogador mesa listaJogadores indiceJogadorInicial ordInicial numJogadores newDeck                     

turnoJogador mesaInit listaJogadores indiceJogador ord numJogadores shuffledDeck = do
    
    SP.system "clear"

    let jogador = (listaJogadores !! indiceJogador) 
    let len1J = length jogador
    
    let indiceProxJogador = numProxJogador ord indiceJogador numJogadores
    let proximoJogador = (listaJogadores !! indiceProxJogador)

    let testCoringaMais4 = testJogaCoringaMais4 mesaInit jogador
    let mesa2 = atualizaMesa mesaInit jogador testCoringaMais4    

    
    if (numJogadores == 2)
        then do painel2Jogadores listaJogadores indiceJogador "normal"
    
    else if (numJogadores == 3)
        then do painel3Jogadores listaJogadores indiceJogador "normal"
        
    else if (numJogadores == 4)
        then do painel4Jogadores listaJogadores indiceJogador "normal"
        
        else do return()
        
    let auxJogador = jogaCarta mesa2 jogador
    let jogador = auxJogador
    
    let len2J = length jogador
    
    shuffledColors <- shuffle cores
    
    let mesaAux = atualizaCorCoringa ((cor mesa2) == "Preta") (head shuffledColors) mesa2
    let mesa2 = mesaAux
    
    putStrLn "" 
    putStr "Mesa: " 
    print mesa2
    

    let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
    
    if (len1J == len2J)
        then do incementaMaoJogador mesa2 listaJogadores2 indiceJogador ord numJogadores shuffledDeck
        
        else do atualizaJogo mesaInit mesa2 listaJogadores2 indiceJogador ord numJogadores shuffledDeck
    
incementaMaoJogador mesaInit listaJogadores indiceJogador ord numJogadores shuffledDeck = do
    
    line <- getLine
    SP.system "clear"
    
    let jogador = (listaJogadores !! indiceJogador)

    let indiceProxJogador = numProxJogador ord indiceJogador numJogadores
    let proximoJogador = (listaJogadores !! indiceProxJogador)
    
    let auxJogador = incrementaMao 1 jogador shuffledDeck
    let jogador = auxJogador
    
    let auxShuffledDeck = atualizaBaralho 1 shuffledDeck
    let shuffledDeck = auxShuffledDeck
    
    let testCoringaMais4 = testJogaCoringaMais4 mesaInit jogador
    let mesa2 = atualizaMesa mesaInit jogador testCoringaMais4

    let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
                   
    if (numJogadores == 2)
        then do painel2Jogadores listaJogadores2 indiceJogador "monte"
    
    else if (numJogadores == 3)
        then do painel3Jogadores listaJogadores2 indiceJogador "monte"
     
    else if (numJogadores == 4)
        then do painel4Jogadores listaJogadores2 indiceJogador "monte"
             
        else do return()              
    
    let auxJogador = jogaCarta mesa2 jogador
    let jogador = auxJogador
    
    shuffledColors <- shuffle cores
    
    let mesaAux = atualizaCorCoringa ((cor mesa2) == "Preta") (head shuffledColors) mesa2
    let mesa2 = mesaAux

    putStrLn "" 
    putStr "Mesa: " 
    print mesa2
    
    
    let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
    
    atualizaJogo mesaInit mesa2 listaJogadores2 indiceJogador ord numJogadores shuffledDeck


atualizaJogo mesaInit mesa2 listaJogadores indiceJogador ord numJogadores shuffledDeck = do

    line <- getLine
    SP.system "clear"
    
    let indiceProxJogador = numProxJogador ord indiceJogador numJogadores
    
    let jogador = (listaJogadores !! indiceJogador)  
    
    let proximoJogador = (listaJogadores !! indiceProxJogador)
    
    let novoIndiceProxJogador = numProxJogador ord indiceProxJogador numJogadores
   
    if ((length jogador == 0) && (numJogadores == 2))
        then do painel2Jogadores listaJogadores indiceJogador "fim"
    
    else if ((length jogador == 0) && (numJogadores == 3))
        then do painel3Jogadores listaJogadores indiceJogador "fim"
    
    else if ((length jogador == 0) && (numJogadores == 4))
        then do painel4Jogadores listaJogadores indiceJogador "fim"
    
    else if ((mesaInit /= mesa2) && (tipo mesa2 == "Inverter"))
        then do let novaOrd = inverteOrdem ord
                let indiceProxJogador = ajustaIndice (numProxJogador novaOrd indiceJogador numJogadores) numJogadores
                turnoJogador mesa2 listaJogadores indiceProxJogador novaOrd numJogadores shuffledDeck
    
    else if ((mesaInit /= mesa2) && (tipo mesa2 == "Pular"))
        then do let novoIndiceProxJogador = numProxJogador ord indiceProxJogador numJogadores
                turnoJogador mesa2 listaJogadores novoIndiceProxJogador ord numJogadores shuffledDeck
        
    else if ((mesaInit /= mesa2) && (tipo mesa2 == "Mais2"))
        then do let auxProximoJogador = incrementaMao 2 proximoJogador shuffledDeck
                let proximoJogador = auxProximoJogador
                
                let auxShuffledDeck = atualizaBaralho 2 shuffledDeck
                let shuffledDeck = auxShuffledDeck
                
                let listaJogadores2 = atualizaListaJogadores indiceProxJogador [proximoJogador] listaJogadores
                turnoJogador mesa2 listaJogadores2 novoIndiceProxJogador ord numJogadores shuffledDeck             

    else if ((mesaInit /= mesa2) && (tipo mesa2 == "CoringaMais4"))
        then do let auxProximoJogador = incrementaMao 4 proximoJogador shuffledDeck
                let proximoJogador = auxProximoJogador
                
                let auxShuffledDeck = atualizaBaralho 4 shuffledDeck
                let shuffledDeck = auxShuffledDeck
                
                let listaJogadores2 = atualizaListaJogadores indiceProxJogador [proximoJogador] listaJogadores
                turnoJogador mesa2 listaJogadores2 novoIndiceProxJogador ord numJogadores shuffledDeck
        
        else do turnoJogador mesa2 listaJogadores indiceProxJogador ord numJogadores shuffledDeck


painelSorteio cartasSorteio numJogadores indiceMaiorCarta = do
         
         SP.system "clear"
         
         putStrLn "Sorteio Para Definir o Jogador Iniciante: "
         putStrLn " "
         
         if ((numJogadores == 2) && (indiceMaiorCarta == -1))
                then do putStrLn "EMPATE!"
                        putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
         
         else if (numJogadores == 3 && (indiceMaiorCarta == -1))
                then do putStrLn "EMPATE!"
                        putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2))
                               
         else if (numJogadores == 4 && (indiceMaiorCarta == -1))
                then do putStrLn "EMPATE!"
                        putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2))
                        putStrLn ("Jogador 4: " ++ show (cartasSorteio !! 3))

         else if ((numJogadores == 2) && (indiceMaiorCarta == 0))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0) ++ " <-- Vencedor do Sorteio")
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
         
         else if ((numJogadores == 2) && (indiceMaiorCarta == 1))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1) ++ " <-- Vencedor do Sorteio")
         
         else if (numJogadores == 3 && (indiceMaiorCarta == 0))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0) ++ " <-- Vencedor do Sorteio")
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2))
                        
         else if (numJogadores == 3 && (indiceMaiorCarta == 1))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1) ++ " <-- Vencedor do Sorteio")
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2))

         else if (numJogadores == 3 && (indiceMaiorCarta == 2))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2) ++ " <-- Vencedor do Sorteio")                                     
                
         else if (numJogadores == 4 && (indiceMaiorCarta == 0))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0) ++ " <-- Vencedor do Sorteio")
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2))
                        putStrLn ("Jogador 4: " ++ show (cartasSorteio !! 3))
        
         else if (numJogadores == 4 && (indiceMaiorCarta == 1))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1) ++ " <-- Vencedor do Sorteio")
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2))
                        putStrLn ("Jogador 4: " ++ show (cartasSorteio !! 3))                        

         else if (numJogadores == 4 && (indiceMaiorCarta == 2))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2) ++ " <-- Vencedor do Sorteio")
                        putStrLn ("Jogador 4: " ++ show (cartasSorteio !! 3))                         
                
         else if (numJogadores == 4 && (indiceMaiorCarta == 3))
                then do putStrLn ("Jogador 1: " ++ show (cartasSorteio !! 0))
                        putStrLn ("Jogador 2: " ++ show (cartasSorteio !! 1))
                        putStrLn ("Jogador 3: " ++ show (cartasSorteio !! 2))
                        putStrLn ("Jogador 4: " ++ show (cartasSorteio !! 3) ++ " <-- Vencedor do Sorteio")                         
                
                else do return()
                 
                 
painelInicial listaJogadores numJogadores= do
                  
         if (numJogadores == 2)
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
         
         else if (numJogadores == 3)
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                               
         else if (numJogadores == 4)
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
                
                else do return() 

painel2Jogadores listaJogadores indiceJogador tipo = do         
         
         if (indiceJogador == 0 && tipo == "normal")
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0) ++ " <--")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        
         else if (indiceJogador == 1 && tipo == "normal") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1) ++ " <--")         
         
         else if (indiceJogador == 0 && tipo == "monte") 
                then do putStrLn ("Jogador 1 [Monte]: " ++ show (listaJogadores !! 0) ++ " <--")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        
         else if (indiceJogador == 1 && tipo == "monte") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2 [Monte]: " ++ show (listaJogadores !! 1) ++ " <--")

         else if (indiceJogador == 0 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: VENCEDOR!")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))

         else if (indiceJogador == 1 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: VENCEDOR!")
                else do return()
                
painel3Jogadores listaJogadores indiceJogador tipo = do
         
         if (indiceJogador == 0 && tipo == "normal")
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0) ++ " <--")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        
         else if (indiceJogador == 1 && tipo == "normal") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1) ++ " <--")
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
         
         else if (indiceJogador == 2 && tipo == "normal")
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2) ++ " <--")
         
         else if (indiceJogador == 0 && tipo == "monte") 
                then do putStrLn ("Jogador 1 [Monte]: " ++ show (listaJogadores !! 0) ++ " <--")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        
         else if (indiceJogador == 1 && tipo == "monte") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2 [Monte]: " ++ show (listaJogadores !! 1) ++ " <--")
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
         
         else if (indiceJogador == 2 && tipo == "monte") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3 [Monte]: " ++ show (listaJogadores !! 2) ++ " <--")
                        
         else if (indiceJogador == 0 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: VENCEDOR!")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
         
         else if (indiceJogador == 1 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: VENCEDOR!")
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                                       
         else if (indiceJogador == 2 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: VENCEDOR!")
                else do return()

painel4Jogadores listaJogadores indiceJogador tipo = do
         
         if (indiceJogador == 0 && tipo == "normal")
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0) ++ " <--")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
                        
         else if (indiceJogador == 1 && tipo == "normal") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1) ++ " <--")
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
         
         else if (indiceJogador == 2 && tipo == "normal")
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2) ++ " <--")
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
         
         else if (indiceJogador == 3 && tipo == "normal")
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3) ++ " <--")
         
         else if (indiceJogador == 0 && tipo == "monte") 
                then do putStrLn ("Jogador 1 [Monte]: " ++ show (listaJogadores !! 0) ++ " <--")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
                        
         else if (indiceJogador == 1 && tipo == "monte") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2 [Monte]: " ++ show (listaJogadores !! 1) ++ " <--")
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
         
         else if (indiceJogador == 2 && tipo == "monte") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3 [Monte]: " ++ show (listaJogadores !! 2) ++ " <--")
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
                        
         else if (indiceJogador == 3 && tipo == "monte") 
                then do putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4 [Monte]: " ++ show (listaJogadores !! 3) ++ " <--")
                        
         else if (indiceJogador == 0 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: VENCEDOR!")
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
         
         else if (indiceJogador == 1 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: VENCEDOR!")
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
                                       
         else if (indiceJogador == 2 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: VENCEDOR!")
                        putStrLn ("Jogador 4: " ++ show (listaJogadores !! 3))
         
         else if (indiceJogador == 3 && tipo == "fim")
                then do putStrLn ("-- Fim --") 
                        putStrLn ("Jogador 1: " ++ show (listaJogadores !! 0))
                        putStrLn ("Jogador 2: " ++ show (listaJogadores !! 1))
                        putStrLn ("Jogador 3: " ++ show (listaJogadores !! 2))
                        putStrLn ("Jogador 4: VENCEDOR!")
                else do return()


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

jogaCarta :: Carta -> [Carta] -> [Carta]
jogaCarta _ [] = []
jogaCarta c (x:xs) = delete c (x:xs)

    
criaJogador :: Int -> [Carta] -> [Carta]
criaJogador 4 _ = []
criaJogador a (x:xs) = x: criaJogador (a+1) xs

criaJogadorParaLista :: Int -> Int -> [Carta] -> [Carta]
criaJogadorParaLista 4 _ _ = []
criaJogadorParaLista numCards index deck = (deck !! index): criaJogadorParaLista (numCards+1) (index+1) deck

criaListaJogadores :: Int -> Int -> [Carta] -> [[Carta]]
criaListaJogadores 0 _ _ = []
criaListaJogadores nPlayers index deck = [criaJogadorParaLista 0 index deck] ++ criaListaJogadores (nPlayers-1) (index + 4) deck

jogador1 :: [Carta]
jogador1 = [Carta "Um" "Amarela",Carta "Mais2" "Amarela", Carta "Coringa" "Preta", Carta "Inverter" "Azul"]

jogador2 :: [Carta]
jogador2 = [Carta "Mais2" "Amarela", Carta "Um" "Verde", Carta "Sete" "Verde", Carta "Cinco" "Amarela"]

jogador3 :: [Carta]
jogador3 = [Carta "Sete" "Vermelha", Carta "Pular" "Verde", Carta "Nove" "Verde", Carta "Mais2" "Azul"]

jogador4 :: [Carta]
jogador4 = [Carta "Tres" "Azul", Carta "Inverter" "Verde", Carta "Oito" "Vermelha", Carta "CoringaMais4" "Preta"]

mesa :: Carta 
mesa = Carta "Um" "Azul"

atualizaMesa :: Carta -> [Carta] -> Bool -> Carta
atualizaMesa (Carta t c) [] _ = (Carta t c)
atualizaMesa (Carta t c) (head:tail) teste
        | teste = (Carta "CoringaMais4" "Preta")
        | (t == tipo head) || (c == cor head) || (tipo head == "Coringa") = head
        | otherwise = atualizaMesa (Carta t c) tail teste        


testaViabilidadeCoringaMais4 :: Carta -> [Carta] -> Bool
testaViabilidadeCoringaMais4 (Carta t c) [] = True
testaViabilidadeCoringaMais4 (Carta t c) (head:tail)
        | (t == tipo head) || (c == cor head) && (c /= "Preta") = False
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

   
atualizaBaralho :: Int -> [Carta] -> [Carta]
atualizaBaralho n (x:xs) = drop n (x:xs)


incrementaMao :: Int -> [Carta] -> [Carta] -> [Carta]
incrementaMao n (x:xs) (y:ys) = (take n (y:ys)) ++ (x:xs)


cores :: [String]
cores = ["Azul", "Verde", "Amarela", "Vermelha"]


atualizaCorCoringa :: Bool -> String -> Carta -> Carta
atualizaCorCoringa teste cor (Carta t c)
        | teste == True = (Carta t cor)
        | teste == False = (Carta t c)


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

        
atualizaListaJogadores :: Int -> [[Carta]] -> [[Carta]] -> [[Carta]]
atualizaListaJogadores index jog listJog = (take (index) listJog) ++ jog ++ (drop (index+1) listJog)


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
  
