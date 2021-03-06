import Cartas
import Util
import Paineis
import qualified System.Process as SP
import Control.Concurrent

valorInicialRecursao :: Int
valorInicialRecursao = 0

main = do

    SP.system "clear"
    
    validaQuantidadeJogadores   

validaQuantidadeJogadores = do
    
    putStrLn "Qual a quantidade de jogadores desejada? "
    putStrLn "Opções disponíveis: 2, 3, 4"
    putStrLn ""
    putStr "Opção: "
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
    
    baralhoEmbaralhado <- embaralhaLista baralho
    
    let listaJogadores = criaListaJogadores numJogadores valorInicialRecursao baralhoEmbaralhado
    
    let numCartasRetiradas = numJogadores*4
    let newDeck = atualizaBaralho numCartasRetiradas baralhoEmbaralhado
    
    putStrLn "Lista de Cartas Retiradas do Monte Embaralhado Para Cada Jogador: "
    putStrLn ""
    if (numJogadores == 2)
        then do painelInicial listaJogadores numJogadores
    
    else if (numJogadores == 3)
        then do painelInicial listaJogadores numJogadores
     
    else if (numJogadores == 4)
        then do painelInicial listaJogadores numJogadores
             
        else do return()
    
    painelPressioneEnter
    line <- getLine
        
    validaOpcaoJogadorReal numJogadores listaJogadores newDeck 
        
validaOpcaoJogadorReal numJogadores listaJogadores baralhoEmbaralhado = do
    
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
                sorteiaJogadorIniciante numJogadores listaJogadores baralhoEmbaralhado opcaoJogadorRealBool

    else if (opcaoJogadorRealInt == 2)
        then do putStrLn "Ok, divirta-se!"
                threadDelay 2000000
                let opcaoJogadorRealBool = False
                sorteiaJogadorIniciante numJogadores listaJogadores baralhoEmbaralhado opcaoJogadorRealBool
        
        else do putStrLn "Você digitou um opção inválida. Tente novamente!"
                threadDelay 2000000
                validaOpcaoJogadorReal numJogadores listaJogadores baralhoEmbaralhado
               

sorteiaJogadorIniciante numJogadores listaJogadores baralhoEmbaralhado opcaoJogadorReal = do
    
    SP.system "clear"
    
    newDeck <- embaralhaLista baralhoEmbaralhado

    let cartasSorteio = listaCartasSorteio numJogadores baralhoEmbaralhado
    
    let cartaMenorValor = (Carta "Zero" "")
    let maiorCarta = calculaMaiorValorCarta cartaMenorValor cartasSorteio
    
    let numValorRepetido = contaValorRepetidoCarta maiorCarta cartasSorteio
    
    let indiceMaiorCarta = -1
    
    if (numValorRepetido > 1)
        then do painelSorteio cartasSorteio numJogadores indiceMaiorCarta
                painelPressioneEnter
                line <- getLine 
                sorteiaJogadorIniciante numJogadores listaJogadores newDeck opcaoJogadorReal
        else do let indiceMaiorCarta = findIndiceCarta maiorCarta cartasSorteio  
                painelSorteio cartasSorteio numJogadores indiceMaiorCarta
                iniciaPartica listaJogadores indiceMaiorCarta numJogadores baralhoEmbaralhado opcaoJogadorReal 
                

iniciaPartica listaJogadores indiceJogadorInicial numJogadores baralhoEmbaralhado opcaoJogadorReal = do

    newDeck <- embaralhaLista baralhoEmbaralhado
    
    let mesaInicial = selecionaPrimeiraCartaNaoCoringa newDeck
    putStrLn ""    
    putStr "Mesa inicial: "    
    print mesaInicial  
    
    painelPressioneEnter
    line <- getLine 
    
    let baralhoEmbaralhado = jogaCarta mesaInicial newDeck
    let ordemInicial = "horario"   
               
    verificaJogadorReal mesaInicial listaJogadores indiceJogadorInicial ordemInicial numJogadores baralhoEmbaralhado opcaoJogadorReal
    

verificaJogadorReal mesa listaJogadores indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal = do
    
    if (opcaoJogadorReal && indiceJogador == 0)
        then do turnoJogadorReal mesa listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
        
        else do turnoJogador mesa listaJogadores indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal     
                                 

turnoJogador mesaInicial listaJogadores indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal = do
    
    SP.system "clear"

    let jogador = (listaJogadores !! indiceJogador) 
    let len1J = length jogador

    let testCoringaMais4 = testJogaCoringaMais4 mesaInicial jogador
    let mesa2 = atualizaMesa mesaInicial jogador testCoringaMais4    
    
    let tipoPainel = "normal"
    if (numJogadores == 2)
        then do painel2Jogadores listaJogadores indiceJogador tipoPainel
    
    else if (numJogadores == 3)
        then do painel3Jogadores listaJogadores indiceJogador tipoPainel
        
    else if (numJogadores == 4)
        then do painel4Jogadores listaJogadores indiceJogador tipoPainel
        
        else do return()
        
    let auxJogador = jogaCarta mesa2 jogador
    let jogador = auxJogador
    
    let len2J = length jogador
    
    coresEmbaralhadas <- embaralhaLista cores
    
    let verificaCorPreta = (cor mesa2) == "Preta"
    let corAleatoria = head coresEmbaralhadas
    let mesaAux = atualizaCorCoringa verificaCorPreta corAleatoria mesa2
    let mesa2 = mesaAux
    
    putStrLn "" 
    putStr "Mesa: " 
    print mesa2
    

    let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
    
    if (len1J == len2J)
        then do incementaMaoJogador mesa2 listaJogadores2 indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
        
        else do atualizaJogo mesaInicial mesa2 listaJogadores2 indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
    
incementaMaoJogador mesaInicial listaJogadores indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal = do
    
    painelPressioneEnter
    line <- getLine
    SP.system "clear"
    
    let jogador = (listaJogadores !! indiceJogador)
    
    let valorAtualizacao = 1
    let auxJogador = incrementaMao valorAtualizacao jogador baralhoEmbaralhado
    let jogador = auxJogador
    
    let auxBaralhoEmbaralhado = atualizaBaralho valorAtualizacao baralhoEmbaralhado
    let baralhoEmbaralhado = auxBaralhoEmbaralhado
    
    let testCoringaMais4 = testJogaCoringaMais4 mesaInicial jogador
    let mesa2 = atualizaMesa mesaInicial jogador testCoringaMais4

    let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
    
    let tipoPainel = "monte"               
    if (numJogadores == 2)
        then do painel2Jogadores listaJogadores2 indiceJogador tipoPainel
    
    else if (numJogadores == 3)
        then do painel3Jogadores listaJogadores2 indiceJogador tipoPainel
     
    else if (numJogadores == 4)
        then do painel4Jogadores listaJogadores2 indiceJogador tipoPainel
             
        else do return()              
    
    let auxJogador = jogaCarta mesa2 jogador
    let jogador = auxJogador
    
    coresEmbaralhadas <- embaralhaLista cores
    
    let verificaCorPreta = (cor mesa2) == "Preta"
    let corAleatoria = head coresEmbaralhadas
    let mesaAux = atualizaCorCoringa verificaCorPreta corAleatoria mesa2
    let mesa2 = mesaAux

    putStrLn "" 
    putStr "Mesa: " 
    print mesa2
    
    
    let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
    
    atualizaJogo mesaInicial mesa2 listaJogadores2 indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
        

turnoJogadorReal mesaInicial listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal = do

    SP.system "clear"
    
    let indiceJogador = 0
    let jogador = (head listaJogadores) 
    
    let tipoPainel = "normal"  
    if (numJogadores == 2)
        then do painel2Jogadores listaJogadores indiceJogador tipoPainel
    
    else if (numJogadores == 3)
        then do painel3Jogadores listaJogadores indiceJogador tipoPainel
        
    else if (numJogadores == 4)
        then do painel4Jogadores listaJogadores indiceJogador tipoPainel
        
        else do return()
    
    let viabilidadeJogada = verificaViabilidadeJogada valorInicialRecursao mesaInicial jogador
    
    if (viabilidadeJogada)
        then do let indicesDisponiveis = verificaIndicesDisponiveis indiceJogador mesaInicial jogador 
                painelCartasViaveis mesaInicial indicesDisponiveis jogador
                solicitaJogadaJogadorReal mesaInicial listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
        
        else do putStrLn ""
                painelJogadaIndisponivel mesaInicial tipoPainel
                painelPressioneEnter
                incementaMaoJogadorReal mesaInicial listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal

incementaMaoJogadorReal mesaInicial listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal = do   
    
    line <- getLine
    SP.system "clear"
    
    let indiceJogador = 0
    let jogador = (head listaJogadores)

    let valorAtualizacao = 1
    let auxJogador = incrementaMao valorAtualizacao jogador baralhoEmbaralhado
    let jogador = auxJogador
    
    let auxBaralhoEmbaralhado = atualizaBaralho valorAtualizacao baralhoEmbaralhado
    let baralhoEmbaralhado = auxBaralhoEmbaralhado

    let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
    
    let tipoPainel = "monte"               
    if (numJogadores == 2)
        then do painel2Jogadores listaJogadores2 indiceJogador tipoPainel
    
    else if (numJogadores == 3)
        then do painel3Jogadores listaJogadores2 indiceJogador tipoPainel
     
    else if (numJogadores == 4)
        then do painel4Jogadores listaJogadores2 indiceJogador tipoPainel
             
        else do return()              
    
    let viabilidadeJogada = verificaViabilidadeJogada valorInicialRecursao mesaInicial jogador
    
    if (viabilidadeJogada)
        then do let indicesDisponiveis = verificaIndicesDisponiveis indiceJogador mesaInicial jogador
                painelCartasViaveis mesaInicial indicesDisponiveis jogador
                solicitaJogadaJogadorReal mesaInicial listaJogadores2 ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
        
        else do putStrLn ""
                painelJogadaIndisponivel mesaInicial tipoPainel
                atualizaJogo mesaInicial mesaInicial listaJogadores2 indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
                

solicitaJogadaJogadorReal mesaInicial listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal = do           

    putStr "Digite o índice referente a carta que você deseja jogar: "
    indiceCartaInput <- getLine
    
    let indiceJogador = 0
    let jogador = (listaJogadores !! indiceJogador)
    
    let indiceCartaString = read (show indiceCartaInput) :: String
    let indiceCarta = mapeiaIndicePorString indiceCartaString (length jogador) 
    
    let viabilidadeIndice = verificaViabilidadeIndice indiceCarta mesaInicial jogador

    if (viabilidadeIndice)
        then do let mesa2 = (jogador !! indiceCarta)          
                let auxJogador = jogaCarta mesa2 jogador
                let jogador = auxJogador
                
                let listaJogadores2 = atualizaListaJogadores indiceJogador [jogador] listaJogadores
                isCoringa mesaInicial mesa2 listaJogadores2 ordem numJogadores baralhoEmbaralhado opcaoJogadorReal 
        
        else do putStrLn ""
                putStrLn "Você digitou uma opção inválida, tente novamente!"
                putStrLn ""
                solicitaJogadaJogadorReal mesaInicial listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
                

isCoringa mesaInicial mesa2 listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal  = do

    let indiceJogador = 0
    
    if ((tipo mesa2 == "CoringaMais4") || (tipo mesa2 == "Coringa"))               
        then do painelTrocaCorCoringa
                
                corInput <- getLine
                let corNumStr = read (show corInput) :: String

                let corPalavra = mapeiaCorPorNumero corNumStr               
                testaValidadeCorCoringa mesaInicial mesa2 listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal corPalavra
        
        else do atualizaJogo mesaInicial mesa2 listaJogadores indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal 
                                

testaValidadeCorCoringa mesaInicial mesa2 listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal cor = do

    let indiceJogador = 0
    
    if (cor == "null")                  
        then do putStrLn ""
                putStrLn "Você digitou uma opção inválida. Tente novamente!"
                threadDelay 2000000
                SP.system "clear"  
                putStr "Mesa: " 
                print mesa2
                isCoringa mesaInicial mesa2 listaJogadores ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
                
        else do let mesa3 = atualizaCorCoringaJogadorReal cor mesa2
                atualizaJogo mesaInicial mesa3 listaJogadores indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal


atualizaJogo mesaInicial mesa2 listaJogadores indiceJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal = do
    
    painelPressioneEnter
    line <- getLine
    SP.system "clear"
    
    let indiceProxJogador = numProxJogador ordem indiceJogador numJogadores
    
    let jogador = (listaJogadores !! indiceJogador)  
    
    let proximoJogador = (listaJogadores !! indiceProxJogador)

    let novoIndiceProxJogador = numProxJogador ordem indiceProxJogador numJogadores
    
    let tipoPainel = "fim" 
       
    if ((length jogador == 0) && (numJogadores == 2))
        then do painel2Jogadores listaJogadores indiceJogador tipoPainel
    
    else if ((length jogador == 0) && (numJogadores == 3))
        then do painel3Jogadores listaJogadores indiceJogador tipoPainel
    
    else if ((length jogador == 0) && (numJogadores == 4))
        then do painel4Jogadores listaJogadores indiceJogador tipoPainel
    
    else if ((mesaInicial /= mesa2) && (tipo mesa2 == "Inverter"))
        then do let novaOrd = inverteOrdem ordem
                let indiceProxJogador = ajustaIndice (numProxJogador novaOrd indiceJogador numJogadores) numJogadores
                verificaJogadorReal mesa2 listaJogadores indiceProxJogador novaOrd numJogadores baralhoEmbaralhado opcaoJogadorReal
    
    else if ((mesaInicial /= mesa2) && (tipo mesa2 == "Pular"))
        then do verificaJogadorReal mesa2 listaJogadores novoIndiceProxJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
        
    else if ((mesaInicial /= mesa2) && (tipo mesa2 == "Mais2"))
        then do let valorAtualizacao = 2
        
                let auxProximoJogador = incrementaMao valorAtualizacao proximoJogador baralhoEmbaralhado
                let proximoJogador = auxProximoJogador
                
                let auxBaralhoEmbaralhado = atualizaBaralho valorAtualizacao baralhoEmbaralhado
                let baralhoEmbaralhado = auxBaralhoEmbaralhado
                
                let listaJogadores2 = atualizaListaJogadores indiceProxJogador [proximoJogador] listaJogadores
                verificaJogadorReal mesa2 listaJogadores2 novoIndiceProxJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal             

    else if ((mesaInicial /= mesa2) && (tipo mesa2 == "CoringaMais4"))
        then do let valorAtualizacao = 4
        
                let auxProximoJogador = incrementaMao valorAtualizacao proximoJogador baralhoEmbaralhado
                let proximoJogador = auxProximoJogador

                let auxBaralhoEmbaralhado = atualizaBaralho valorAtualizacao baralhoEmbaralhado
                let baralhoEmbaralhado = auxBaralhoEmbaralhado
                
                let listaJogadores2 = atualizaListaJogadores indiceProxJogador [proximoJogador] listaJogadores
                verificaJogadorReal mesa2 listaJogadores2 novoIndiceProxJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal
        
        else do verificaJogadorReal mesa2 listaJogadores indiceProxJogador ordem numJogadores baralhoEmbaralhado opcaoJogadorReal

