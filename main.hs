module Main where

main :: IO ()
main = putStrLn "Hello, world!"

data Pacman
  = Name String
  | MaxScore Int
  | Position (Int, Int)

data Map
  = Road (Int, Int)
  | Wall (Int, Int)
  | Gate (Int, Int)
  | ScoredRoad (Int, Int)

data Game
  = Mapa Map
  | Ghost (Int, Int)
  | Pacman Pacman
  | Score Int
  | GameOver

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

-- criar mapa deve instanciar um mapa como uma matriz, o mapa só pode ser instanciado uma vez só

