import Cartas
import System.Random
import Data.Array.IO
import Control.Monad
import Data.List
import Data.Char
import qualified System.Process as SP
import Control.Concurrent

main = do

    SP.system "clear"
    
    validaQuantidadeJogadores   

validaQuantidadeJogadores = do
    
    putStr "Qual a quantidade de jogadores desejada? "
    numJogadoresInput <- getLine
    
    let numJogadoresStr = read (show numJogadoresInput) :: String
    
    let numJogadoresInt = mapeiaQuantidadeJogadores numJogadoresStr
       
    if (numJogadoresInt /= (-1))
        then do exibePainelInicial numJogadoresInt      
        
        else do putStrLn "Você digitou um opção inválida. Tente novamente!"
                threadDelay 2000000
                SP.system "clear"
                validaQuantidadeJogadores
                
exibePainelInicial numJogadores = do
    
    SP.system "clear"
    
    shuffledDeck <- shuffle baralho
    
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
    
    putStrLn ""
    putStrLn ""
    putStr "Pressione Enter para continuar!"
    line <- getLine
        
    validaOpcaoJogadorReal numJogadores listaJogadores newDeck 
        
validaOpcaoJogadorReal numJogadores listaJogadores shuffledDeck = do
    
    SP.system "clear"
    
    putStrLn "Você deseja jogar ou apenas observar bots jogarem? "
    putStrLn ""
    putStrLn "Opções Disponíveis:"
    putStrLn "1 - Jogar"
    putStrLn "2 - Observar"
    putStrLn ""
    putStr "Opção: "
    opcaoJogadorRealInput <- getLine
    
    let opcaoJogadorRealStr = read (show opcaoJogadorRealInput) :: String
    
    let opcaoJogadorRealInt = mapeiaOpcaoJogarReal opcaoJogadorRealStr
    
    if (opcaoJogadorRealInt == 1)
        then do putStrLn "Você é o Jogador 1, boa Sorte!"
                threadDelay 2000000
                let opcaoJogadorRealBool = True
                sorteiaJogadorIniciante numJogadores listaJogadores shuffledDeck opcaoJogadorRealBool

    else if (opcaoJogadorRealInt == 2)
        then do putStrLn "Ok, divirta-se!"
                threadDelay 2000000
                let opcaoJogadorRealBool = False
                sorteiaJogadorIniciante numJogadores listaJogadores shuffledDeck opcaoJogadorRealBool
        
        else do putStrLn "Você digitou um opção inválida. Tente novamente!"
                threadDelay 2000000
                SP.system "clear"
                validaOpcaoJogadorReal numJogadores listaJogadores shuffledDeck
               

sorteiaJogadorIniciante numJogadores listaJogadores shuffledDeck opcaoJogadorReal = do
    
    SP.system "clear"
    
    newDeck <- shuffle shuffledDeck

    let cartasSorteio = listaCartasSorteio numJogadores shuffledDeck
    
    let maiorCarta = calculaMaiorValorCarta (Carta "Zero" "") cartasSorteio
    
    let numValorRepetido = contaValorRepetidoCarta maiorCarta cartasSorteio
    
    let indiceMaiorCarta = -1
    
    if (numValorRepetido > 1)
        then do painelSorteio cartasSorteio numJogadores indiceMaiorCarta
                putStrLn ""
                putStrLn ""
                putStr "Pressione Enter para continuar!"
                line <- getLine 
                sorteiaJogadorIniciante numJogadores listaJogadores newDeck opcaoJogadorReal
        else do let indiceMaiorCarta = findIndiceCarta maiorCarta cartasSorteio  
                painelSorteio cartasSorteio numJogadores indiceMaiorCarta
                putStrLn ""
                putStrLn ""
                putStr "Pressione Enter para continuar!"
                line <- getLine 
                iniciaPartica listaJogadores indiceMaiorCarta numJogadores shuffledDeck opcaoJogadorReal 

iniciaPartica listaJogadores indiceJogadorInicial numJogadores shuffledDeck opcaoJogadorReal = do

    newDeck <- shuffle shuffledDeck
    
    let mesaInicial = selecionaPrimeiraCartaNaoCoringa newDeck
    putStrLn ""    
    putStr "Mesa inicial: "    
    print mesaInicial  
    
    let shuffledDeck = jogaCarta mesaInicial newDeck
    let ordInicial = "horario"   
            
    
    verificaJogadorReal mesaInicial listaJogadores indiceJogadorInicial ordInicial numJogadores shuffledDeck opcaoJogadorReal
                              

turnoJogador mesaInit listaJogadores indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal = do
    
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
        then do incementaMaoJogador mesa2 listaJogadores2 indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal
        
        else do atualizaJogo mesaInit mesa2 listaJogadores2 indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal
    
incementaMaoJogador mesaInit listaJogadores indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal = do
    
    putStrLn ""
    putStrLn ""
    putStr "Pressione Enter para continuar!"
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
    
    atualizaJogo mesaInit mesa2 listaJogadores2 indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal
        

turnoJogadorReal mesaInit listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal = do

    SP.system "clear"
    
    let indiceJogador = 0
    let jogador = (head listaJogadores) 
      
    if (numJogadores == 2)
        then do painel2Jogadores listaJogadores indiceJogador "normal"
    
    else if (numJogadores == 3)
        then do painel3Jogadores listaJogadores indiceJogador "normal"
        
    else if (numJogadores == 4)
        then do painel4Jogadores listaJogadores indiceJogador "normal"
        
        else do return()
    
    let viabilidadeJogada = verificaViabilidadeJogada 0 mesaInit jogador
    
    if (viabilidadeJogada)
        then do let indicesDisponiveis = verificaIndicesDisponiveis indiceJogador mesaInit jogador 
                painelCartasViaveis mesaInit indicesDisponiveis jogador
                
                solicitaJogadaJogadorReal mesaInit listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal
        
        else do putStrLn ""
                painelJogadaIndisponivel mesaInit "normal"
                incementaMaoJogadorReal mesaInit listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal

incementaMaoJogadorReal mesaInit listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal = do   
    
    line <- getLine
    SP.system "clear"
    
    let indiceJogador = 0
    let jogador = (head listaJogadores)

    
    let auxJogador = incrementaMao 1 jogador shuffledDeck
    let jogador = auxJogador
    
    let auxShuffledDeck = atualizaBaralho 1 shuffledDeck
    let shuffledDeck = auxShuffledDeck

    let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
                   
    if (numJogadores == 2)
        then do painel2Jogadores listaJogadores2 indiceJogador "monte"
    
    else if (numJogadores == 3)
        then do painel3Jogadores listaJogadores2 indiceJogador "monte"
     
    else if (numJogadores == 4)
        then do painel4Jogadores listaJogadores2 indiceJogador "monte"
             
        else do return()              
    
    let viabilidadeJogada = verificaViabilidadeJogada 0 mesaInit jogador
    
    if (viabilidadeJogada)
        then do let indicesDisponiveis = verificaIndicesDisponiveis indiceJogador mesaInit jogador
                painelCartasViaveis mesaInit indicesDisponiveis jogador
                solicitaJogadaJogadorReal mesaInit listaJogadores2 ord numJogadores shuffledDeck opcaoJogadorReal
        
        else do putStrLn ""
                painelJogadaIndisponivel mesaInit "monte"
                atualizaJogo mesaInit mesaInit listaJogadores2 indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal
                

solicitaJogadaJogadorReal mesaInit listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal = do           

    putStr "Digite o índice referente a carta que você deseja jogar: "
    indiceCartaInput <- getLine
    
    let indiceJogador = 0
    let jogador = (listaJogadores !! indiceJogador)
    
    let indiceCartaString = read (show indiceCartaInput) :: String
    let indiceCarta = mapeiaIndicePorString indiceCartaString (length jogador) 
    
    let viabilidadeIndice = verificaViabilidadeIndice indiceCarta mesaInit jogador

    if (viabilidadeIndice)
        then do let mesa2 = (jogador !! indiceCarta)
                
                let auxJogador = jogaCarta mesa2 jogador
                let jogador = auxJogador
                
                let listaJogadores2 = atualizaListaJogadores 0 [jogador] listaJogadores
                isCoringa mesaInit mesa2 listaJogadores2 ord numJogadores shuffledDeck opcaoJogadorReal 
        
        else do putStrLn ""
                putStrLn "Você digitou uma opção inválida, tente novamente!"
                putStrLn ""
                solicitaJogadaJogadorReal mesaInit listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal
                

isCoringa mesaInit mesa2 listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal  = do

    let indiceJogador = 0
    
    if ((tipo mesa2 == "CoringaMais4") || (tipo mesa2 == "Coringa"))               
        then do painelTrocaCorCoringa
                
                corInput <- getLine
                let corNumStr = read (show corInput) :: String

                let corPalavra = mapeiaCorPorNumero corNumStr               
                testaValidadeCorCoringa mesaInit mesa2 listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal corPalavra
        
        else do atualizaJogo mesaInit mesa2 listaJogadores indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal 
                                

testaValidadeCorCoringa mesaInit mesa2 listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal cor = do

    let indiceJogador = 0
    
    if (cor == "null")                  
        then do putStrLn ""
                putStrLn "Você digitou uma opção inválida. Tente novamente!"
                threadDelay 2000000
                SP.system "clear"  
                putStr "Mesa: " 
                print mesa2
                isCoringa mesaInit mesa2 listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal
                
        else do let mesa3 = atualizaCorCoringaJogadorReal cor mesa2
                atualizaJogo mesaInit mesa3 listaJogadores indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal


atualizaJogo mesaInit mesa2 listaJogadores indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal = do
    
    putStrLn ""
    putStrLn ""
    putStr "Pressione Enter para continuar!"
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
                verificaJogadorReal mesa2 listaJogadores indiceProxJogador novaOrd numJogadores shuffledDeck opcaoJogadorReal
    
    else if ((mesaInit /= mesa2) && (tipo mesa2 == "Pular"))
        then do let novoIndiceProxJogador = numProxJogador ord indiceProxJogador numJogadores
                verificaJogadorReal mesa2 listaJogadores novoIndiceProxJogador ord numJogadores shuffledDeck opcaoJogadorReal
        
    else if ((mesaInit /= mesa2) && (tipo mesa2 == "Mais2"))
        then do let auxProximoJogador = incrementaMao 2 proximoJogador shuffledDeck
                let proximoJogador = auxProximoJogador
                
                let auxShuffledDeck = atualizaBaralho 2 shuffledDeck
                let shuffledDeck = auxShuffledDeck
                
                let listaJogadores2 = atualizaListaJogadores indiceProxJogador [proximoJogador] listaJogadores
                verificaJogadorReal mesa2 listaJogadores2 novoIndiceProxJogador ord numJogadores shuffledDeck opcaoJogadorReal             

    else if ((mesaInit /= mesa2) && (tipo mesa2 == "CoringaMais4"))
        then do let auxProximoJogador = incrementaMao 4 proximoJogador shuffledDeck
                let proximoJogador = auxProximoJogador

                let auxShuffledDeck = atualizaBaralho 4 shuffledDeck
                let shuffledDeck = auxShuffledDeck
                
                let listaJogadores2 = atualizaListaJogadores indiceProxJogador [proximoJogador] listaJogadores
                verificaJogadorReal mesa2 listaJogadores2 novoIndiceProxJogador ord numJogadores shuffledDeck opcaoJogadorReal
        
        else do verificaJogadorReal mesa2 listaJogadores indiceProxJogador ord numJogadores shuffledDeck opcaoJogadorReal


verificaJogadorReal mesa listaJogadores indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal = do
    
    if (opcaoJogadorReal && indiceJogador == 0)
        then do turnoJogadorReal mesa listaJogadores ord numJogadores shuffledDeck opcaoJogadorReal
        
        else do turnoJogador mesa listaJogadores indiceJogador ord numJogadores shuffledDeck opcaoJogadorReal


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
                        putStrLn ""
                        putStrLn ""
                        putStr "Pressione Enter para continuar!"
        
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
    newArray n xs = newListArray (1,n) xs

lowerString :: String -> String
lowerString = \str -> map toLower str

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
          

exibiCartasViaveis :: Int -> [Int] -> [Carta] -> String
exibiCartasViaveis i indices _
        | i == length indices = ""
exibiCartasViaveis i indices cartas = (show (indices !! i)) ++ ": "  ++ (show (cartas !! (indices !! i))) ++ "\n" ++  (exibiCartasViaveis (i + 1) indices cartas)
        
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

atualizaCorCoringaJogadorReal :: String -> Carta -> Carta
atualizaCorCoringaJogadorReal cor (Carta t c) = (Carta t cor)

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
