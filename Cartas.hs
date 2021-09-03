module Cartas ( Carta(..), baralho) where  

data Carta = Carta {tipo :: String, cor :: String} deriving (Eq)

cartasZero :: [Carta]
cartasZero = [Carta "Zero" "Azul", Carta "Zero" "Verde", Carta "Zero" "Amarela", Carta "Zero" "Vermelha"]

cartasUm :: [Carta]
cartasUm = [Carta "Um" "Azul", Carta "Um" "Azul", Carta "Um" "Verde", Carta "Um" "Verde", 
            Carta "Um" "Amarela", Carta "Um" "Amarela", Carta "Um" "Vermelha", Carta "Um" "Vermelha"]

cartasDois :: [Carta]
cartasDois = [Carta "Dois" "Azul", Carta "Dois" "Azul", Carta "Dois" "Verde", Carta "Dois" "Verde", 
              Carta "Dois" "Amarela", Carta "Dois" "Amarela", Carta "Dois" "Vermelha", Carta "Dois" "Vermelha"]

cartasTres :: [Carta]
cartasTres = [Carta "Tres" "Azul", Carta "Tres" "Azul", Carta "Tres" "Verde", Carta "Tres" "Verde", 
              Carta "Tres" "Amarela", Carta "Tres" "Amarela", Carta "Tres" "Vermelha", Carta "Tres" "Vermelha"]

cartasQuatro :: [Carta]
cartasQuatro = [Carta "Quatro" "Azul", Carta "Quatro" "Azul", Carta "Quatro" "Verde", Carta "Quatro" "Verde", 
                Carta "Quatro" "Amarela", Carta "Quatro" "Amarela", Carta "Quatro" "Vermelha", Carta "Quatro" "Vermelha"]

cartasCinco :: [Carta]
cartasCinco = [Carta "Cinco" "Azul", Carta "Cinco" "Azul", Carta "Cinco" "Verde", Carta "Cinco" "Verde",  
               Carta "Cinco" "Amarela", Carta "Cinco" "Amarela", Carta "Cinco" "Vermelha", Carta "Cinco" "Vermelha"]

cartasSeis :: [Carta]
cartasSeis = [Carta "Seis" "Azul", Carta "Seis" "Azul", Carta "Seis" "Verde", Carta "Seis" "Verde", 
              Carta "Seis" "Amarela", Carta "Seis" "Amarela", Carta "Seis" "Vermelha", Carta "Seis" "Vermelha"]

cartasSete :: [Carta]
cartasSete = [Carta "Sete" "Azul", Carta "Sete" "Azul", Carta "Sete" "Verde", Carta "Sete" "Verde", 
              Carta "Sete" "Amarela", Carta "Sete" "Amarela", Carta "Sete" "Vermelha", Carta "Sete" "Vermelha"]

cartasOito :: [Carta]
cartasOito = [Carta "Oito" "Azul", Carta "Oito" "Azul", Carta "Oito" "Verde", Carta "Oito" "Verde", 
              Carta "Oito" "Amarela", Carta "Oito" "Amarela", Carta "Oito" "Vermelha", Carta "Oito" "Vermelha"]

cartasNove :: [Carta]
cartasNove = [Carta "Nove" "Azul", Carta "Nove" "Azul", Carta "Nove" "Verde", Carta "Nove" "Verde", 
              Carta "Nove" "Amarela", Carta "Nove" "Amarela", Carta "Nove" "Vermelha", Carta "Nove" "Vermelha"]
                       
cartasMais2 :: [Carta]
cartasMais2 = [Carta "Mais2" "Azul", Carta "Mais2" "Azul", Carta "Mais2" "Verde", Carta "Mais2" "Verde", 
               Carta "Mais2" "Amarela", Carta "Mais2" "Amarela", Carta "Mais2" "Vermelha", Carta "Mais2" "Vermelha"]

cartasInverter :: [Carta]
cartasInverter = [Carta "Inverter" "Azul", Carta "Inverter" "Azul", Carta "Inverter" "Verde", Carta "Inverter" "Verde", 
                  Carta "Inverter" "Amarela", Carta "Inverter" "Amarela", Carta "Inverter" "Vermelha", Carta "Inverter" "Vermelha"]

cartasPular :: [Carta]
cartasPular = [Carta "Pular" "Azul", Carta "Pular" "Azul", Carta "Pular" "Verde", Carta "Pular" "Verde", 
               Carta "Pular" "Amarela", Carta "Pular" "Amarela", Carta "Pular" "Vermelha", Carta "Pular" "Vermelha"]

cartasCoringa :: [Carta]
cartasCoringa = [Carta "Coringa" "Preta", Carta "Coringa" "Preta", Carta "Coringa" "Preta", Carta "Coringa" "Preta"]

cartasCoringaMais4 :: [Carta]
cartasCoringaMais4 = [Carta "CoringaMais4" "Preta", Carta "CoringaMais4" "Preta", Carta "CoringaMais4" "Preta", Carta "CoringaMais4" "Preta"]


baralho :: [Carta]
baralho = cartasZero ++ cartasUm ++ cartasDois ++ cartasTres ++ cartasQuatro ++ cartasCinco ++ 
          cartasSeis ++ cartasSete ++ cartasOito ++ cartasNove ++ cartasInverter ++ cartasPular ++
          cartasCoringa ++ cartasCoringaMais4


instance Show Carta where  
    show Carta {tipo = a, cor = b} = a ++ " - " ++ b
