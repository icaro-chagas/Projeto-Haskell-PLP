<h1 align="center">
  <img alt="Uno" title="#Uno" src="https://a-static.mlcdn.com.br/618x463/4-jogo-uno-de-cartas-entre-amigos-2-a-10-pessoas-descontao-copag/prvariedades/7896008981891-43/7d6f80218bd39066a66c755103badd17.jpg" width="450px" />
</h1>

<p align="center">
  <a href="#uno">PROJETO</a>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;
    <a href="#como_executar">COMO EXECUTAR</a>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;
  <a href="#funcionalidades">FUNCIONALIDADES</a>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;
  <a href="#o-time">TIME</a>
</p>

## Uno

O Uno é um jogo de cartas, ao qual pode ser jogado por até 4 pessoas e a primeira pessoa que não tiver mais cartas para jogar ganha a partida.

* **Descrição do baralho** 

   19 Cartas azuis, numeradas de 0 a 9; <br>
   19 Cartas verdes, numeradas de 0 a 9; <br>
   19 Cartas amarelas, numeradas de 0 a 9;<br>
   19 Cartas vermelhas, numeradas de 0 a 9;<br>

    8 Cartas de inversão de sentido (duas de cada cor);<br>
    8 Cartas de +2 (duas de cada cor);<br>
    8 Cartas de Pular (duas de cada cor);<br>
    4 Cartas coringa para troca de cor;<br>
    4 Cartas coringa para troca de cor e +4.<br>
    
## Como executar

* **Caso você rode o programa em windows**: 
  * 1. Aparentemente a biblioteca System.Random não está incluída no pacote bibliotecas padrão do Haskell para windows, então você precisa incluí-la para o programa rodar.
  * 2. Por algum motivo o comando runhaskell não é reconhecido. Então será necessário compilar o programa com o comando `ghc --make MainProgram.hs`, e em seguida executar o programa com `./MainProgram`.
  * 3. Ainda assim, provavelmente, o programa não apresentará o comportamento desejado (por motivo desconhecido o windows não gera algumas saídas de dados implementadas no código).

* **Caso você rode o programa em Linux**:
  * Deve-se usar o comando: `runghci MainProgram.hs` ou `runhaskell MainProgram.hs`

## Funcionalidades

* **Regra para o início do jogo**: Cada jogador retira uma carta de uma pilha, o jogador que retirar a maior carta começa o jogo. Cartas especiais valem 0, e caso mais de um jogador retire cartas com o mesmo valor essas cartas são devolvidas para a pilha e um desempate acontece com nova retirada de cartas. Após o sorteio do primeiro jogador, uma carta é retirada do topo da pilha e o jogo começa.

* **Regras gerais para o decorrer do jogo**: As cartas a serem descartadas precisam combinar com a carta na mesa em número ou cor. Se uma carta de número específico encontra-se na mesa, cartas de qualquer outra cor podem ser descartadas desde que apresentem o mesmo número. O contrário também deve ser possível, ou seja, caso uma carta de determinada cor se encontre na mesa, outras cartas da mesma cor podem ser descartadas independentemente dos números destas.
  Caso o jogador não possua uma carta que combine com a carta da “mesa”, torna-se necessário sacar uma nova da pilha. Se a nova carta obtida combinar com a carta da vez, ela pode ser descartada imediatamente; caso contrário, o usuário deve guardar essa nova carta e a rodada passa para o próximo jogador. Vence o primeiro jogador que conseguir descartar todas as suas cartas.

**Descrição da função das cartas**:

* **Carta Coringa**: Carta para troca de cor: Esta carta pode ser jogada a qualquer momento no jogo e por cima de qualquer carta na “mesa”. Esta carta tem como objetivo alterar a cor da carta da rodada, ou seja, determina qual cor o próximo usuário terá que jogar.

* **Carta +2**: Tal carta tem a função de obrigar o jogador seguinte a tirar mais duas cartas, e, adicionalmente, retira a sua vez de jogar. Esta carta só pode ser jogada caso esteja na mesa uma carta com a mesma cor ou com um símbolo igual.

* **Carta Inverter**: Esta carta tem a função de trocar o sentido do jogo. Tal carta só pode ser jogada caso esteja na “mesa” uma carta com a mesma cor ou com um símbolo igual.

* **Carta Pular**: Esta carta tem a finalidade de impedir o usuário seguinte de jogar. Tal carta só pode ser jogada caso esteja na “mesa” uma carta com a mesma cor ou com um símbolo igual.

* **Carta Coringa +4**: Esta carta permite determinar a próxima cor a ser jogada e força o próximo jogador a tirar mais 4 cartas da pilha. Contudo, ela só pode ser jogada se o usuário não tiver outras cartas adequadas para descartar

