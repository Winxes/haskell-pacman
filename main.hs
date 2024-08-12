module Main where

import Control.Monad (mapM_)

-- Definição dos tipos
data Map
  = Road (Int, Int)
  | Wall (Int, Int)
  deriving (Show)

-- Função para criar uma matriz 36x28 de Map sem ScoredRoad
criarMatriz :: [[Map]]
criarMatriz = [[elemento (r, c) | c <- [0..27]] | r <- [0..35]]
  where
    elemento (r, c)
      | r == 0 || r == 35 || c == 0 || c == 27 = Wall (r, c)  -- Bordas
      | (r `mod` 2 == 0 && c `mod` 2 == 0) = Wall (r, c)     -- Padrão de paredes
      | otherwise = Road (r, c)                             -- Caminhos internos

-- Função para converter um Map em uma letra/símbolo
mapToSymbol :: Map -> String
mapToSymbol (Road _) = "-"
mapToSymbol (Wall _) = "|"

-- Função para imprimir a matriz usando letras/símbolos
printMatriz :: [[Map]] -> IO ()
printMatriz = mapM_ (putStrLn . unwords . map mapToSymbol)

 -- Cria Usuario tal função deve criar um usuário com um nome mas com a pontuação zerada como parâmetros.

  -- Instacia Fantasmas tal função deve criar um fantasma com uma posição específica dentro do mapa.

  -- Instancia Pacman tal função deve criar um pacman com uma posição específica dentro do mapa.

  -- Deleta Usuário tal função deve deletar um usuário da lista de usuários.

  -- Atualiza MaxScore tal função deve atualizar a pontuação máxima de um usuário.

  -- Atualiza Posição tal função deve atualizar a posição de um pacman/fantasma

  -- Mata Usuario essa função mata o pacman, ou seja, gameover.

-- calcula a distancia entre dois pontos
distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- calcula os pontos de um usuario
returnHighScore :: [(String, Int)] -> String -> Int
returnHighScore scores name =
  case lookup name scores of
    Just score -> score
    Nothing -> 0
    
-- Verificar usuario existente
checkUser :: [(String, Int)] -> String -> Bool
checkUser scores name = any (\(n, _) -> n == name) scores

-- Função principal
main :: IO ()
main = do
  putStrLn "Mapa do Pacman:"
  printMatriz criarMatriz
