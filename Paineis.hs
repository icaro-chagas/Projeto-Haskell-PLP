module Paineis  
( painelCartasViaveis  
, painelJogadaIndisponivel
, painelTrocaCorCoringa
, painelSorteio
, painelInicial
, painelPressioneEnter
, painel2Jogadores
, painel3Jogadores
, painel4Jogadores
) where  

import qualified System.Process as SP
import Util (exibiCartasViaveis)


painelCartasViaveis mesa indicesViaveis jogador = do
         
         putStrLn ""
         putStr "Mesa: " 
         print mesa
         putStrLn ""
         putStrLn "Indices possíveis: "
         putStrLn (exibiCartasViaveis 0 indicesViaveis jogador)
         

painelJogadaIndisponivel mesa tipo = do         
        
        if (tipo == "normal")
                then do putStr "Mesa: " 
                        print mesa
                        putStrLn "" 
                        putStrLn "Jogada indisponível!"
        
        else if (tipo == "monte")
                then do putStr "Mesa: " 
                        print mesa
                        putStrLn "" 
                        putStrLn "[Monte] Jogada indisponível!"
                        
                else do return()
        

painelTrocaCorCoringa = do
         
         putStrLn ""
         putStrLn "Insira a nova cor desejada."
         putStrLn "Opções:"
         putStrLn "1 - Azul"
         putStrLn "2 - Verde"
         putStrLn "3 - Amarela"
         putStrLn "4 - Vermelha"
         putStrLn ""
         putStr   "Cor: "
        
painelSorteio cartasSorteio numJogadores indiceMaiorCarta = do
         
         SP.system "clear"
         
         putStrLn "Sorteio a Partir de Cartas Retiradas do Monte Para Definir o Jogador Iniciante: "
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


painelPressioneEnter = do

    putStrLn ""
    putStrLn ""
    putStr "Pressione Enter para continuar!" 

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
