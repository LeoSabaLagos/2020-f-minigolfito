import Test.Hspec
import Lib

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)

main :: IO()
main = hspec $ do
   describe "Punto 1a" $ do
      it "Tiro putter" $ do
         putter (habilidad bart) `shouldBe` UnTiro 10 120 0 
      it "Tiro madera" $ do
         madera (habilidad bart) `shouldBe` UnTiro 100 30 5
      it "Tiro hierro n (ejemplo con hierro 4)" $ do
         hierro 4 (habilidad bart) `shouldBe` UnTiro 100 15 1
   
   {- describe "Punto 1b" $ do
      it "Constante palos (lista con todos los palos que se pueden usar en el juego)" $ do
         palos `shouldBe` [putter,madera,(hierro 1),(hierro 2),(hierro 3),(hierro 4),(hierro 5),(hierro 6),(hierro 7),(hierro 8),(hierro 9),(hierro 10)] -}

      describe "Punto 2 - golpe" $ do
         it "golpe de bart con putter" $ do
            golpe putter bart `shouldBe` UnTiro 10 120 0 
         it "golpe de bart con madera" $ do
            golpe madera bart `shouldBe` UnTiro 100 30 5
         it "golpe de bart con hierro 4" $ do
            golpe (hierro 4) bart `shouldBe` UnTiro 100 15 1
      
      describe "Punto 3" $ do
         it "Tunel rampita superado" $ do
            superaObstaculo UnTunelRampita (UnTiro 200 333 0) `shouldBe` UnTiro 400 100 0
         it "Tunel rampita NO superado (por precision)" $ do
            superaObstaculo UnTunelRampita (UnTiro 500 50 0) `shouldBe` UnTiro 0 0 0
         it "Tunel rampita NO superado (por altura)" $ do
            superaObstaculo UnTunelRampita (UnTiro 500 100 10) `shouldBe` UnTiro 0 0 0
         it "Laguna Superada" $ do
            superaObstaculo (UnaLaguna 2) (UnTiro 100 80 4) `shouldBe` UnTiro 100 80 2
         it "Laguna NO Superada (por velocidad)" $ do
            superaObstaculo (UnaLaguna 2) (UnTiro 20 80 4) `shouldBe` UnTiro 0 0 0
         it "Laguna NO Superada (por altura)" $ do
            superaObstaculo (UnaLaguna 2) (UnTiro 100 80 8) `shouldBe` UnTiro 0 0 0
         it "Hoyo Superado" $ do
            superaObstaculo UnHoyo (UnTiro 15 100 0) `shouldBe` UnTiro 0 0 0
         it "Hoyo NO Superado (por velocidad)" $ do
            superaObstaculo UnHoyo (UnTiro 50 100 0) `shouldBe` UnTiro 0 0 0
         it "Hoyo NO Superado (por precision)" $ do
            superaObstaculo UnHoyo (UnTiro 15 50 0) `shouldBe` UnTiro 0 0 0
         it "Hoyo NO Superado (por altura)" $ do
            superaObstaculo UnHoyo (UnTiro 15 100 3) `shouldBe` UnTiro 0 0 0
         
      

