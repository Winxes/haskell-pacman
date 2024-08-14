import Control.Monad (mapM_)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Definição dos tipos
data Map
  = Road (Int, Int)     -- Estrada normal
  | EmptyRoad (Int, Int) -- Estrada vazia (sem pontuação)
  | Wall (Int, Int)     -- Parede
  deriving (Show, Eq)

data Entity = Entity
  { entityName :: String
  , maxScore   :: Int
  , score      :: Int
  , position   :: (Int, Int)
  } deriving (Show)

-- Função para criar uma matriz 24x21 de Map
criarMatriz :: [[Map]]
criarMatriz = [[elemento (r, c) | c <- [0..20]] | r <- [0..23]]
  where
    elemento (r, c)
      | r == 0 || r == 23 || c == 0 || c == 20 = Wall (r, c)  -- Bordas
      | r `mod` 2 == 0 && c `mod` 2 == 0 = Wall (r, c)  -- Padrão de paredes
      | otherwise = Road (r, c)  -- Caminhos internos

-- Função para converter um Map em uma letra/símbolo
mapToSymbol :: Map -> String
mapToSymbol (Road _) = "."
mapToSymbol (EmptyRoad _) = " "  -- Estrada vazia representada como espaço
mapToSymbol (Wall _) = "\ESC[34m🞓\ESC[0m"

-- Função para imprimir a matriz, considerando as posições das entidades
printMatrizComEntidades :: [[Map]] -> [Entity] -> IO ()
printMatrizComEntidades matriz entidades = mapM_ (putStrLn . unwords . map (mostrarComEntidades entidades)) coordenadas
  where
    coordenadas = [[(r, c) | c <- [0..20]] | r <- [0..23]]
    mostrarComEntidades ents (r, c) =
      case filter (\(Entity _ _ _ (er, ec)) -> (er, ec) == (r, c)) ents of
        [] -> mapToSymbol (matriz !! r !! c)
        (Entity nome _ _ _ : _)
          | nome == "Pacman" -> "\ESC[0m⚉\ESC[0m"  --
          | nome == "Azul"    -> "\ESC[34m⬤\ESC[0m"  -- 
          | nome == "Vermelho" -> "\ESC[31m⬤\ESC[0m"  -- 
          | nome == "Rosa"    -> "\ESC[35m⬤\ESC[0m"  -- 
          | nome == "Laranja" -> "\ESC[33m⬤\ESC[0m"  -- 
          | otherwise -> "\ESC[31m⬤\ESC[0m"  -- 

-- Função para criar uma entidade com nome, posição e maxScore zerado
criarEntidade :: String -> (Int, Int) -> Entity
criarEntidade nome pos = Entity { entityName = nome, maxScore = 0, score = 0, position = pos }

-- Função para atualizar a matriz e a pontuação do Pacman
atualizarMapa :: [[Map]] -> (Int, Int) -> ([[Map]], Int)
atualizarMapa matriz pos =
  let (r, c) = pos
      roadOrWall = matriz !! r !! c
      (novaRoad, pontos) = case roadOrWall of
        Road _ -> (EmptyRoad (r, c), 1)  -- Converte Road para EmptyRoad e dá 1 ponto
        Wall _ -> (roadOrWall, 0)        -- Não altera Wall
        EmptyRoad _ -> (roadOrWall, 0)   --
      matrizAtualizada = take r matriz ++ [take c (matriz !! r) ++ [novaRoad] ++ drop (c + 1) (matriz !! r)] ++ drop (r + 1) matriz
  in (matrizAtualizada, pontos)

-- Função para capturar e verificar o caractere do usuário e atualizar a posição
moverPacman :: [[Map]] -> Entity -> Char -> ([[Map]], Entity)
moverPacman matriz pacman movimento =
  let (r, c) = position pacman
      novaPos = case movimento of
        'w' -> (r - 1, c)  -- Mover para cima
        's' -> (r + 1, c)  -- Mover para baixo
        'a' -> (r, c - 1)  -- Mover para a esquerda
        'd' -> (r, c + 1)  -- Mover para a direita
        _   -> (r, c)      
      (novaMatriz, pontos, novaPosValida) =
        if isMovimentoValido matriz novaPos
        then let (m, p) = atualizarMapa matriz novaPos in (m, p, novaPos)
        else (matriz, 0, (r, c)) -- Se movimento não for válido, mantém a posição e matriz atuais
      pacmanAtualizado = pacman { position = novaPosValida, score = score pacman + pontos }
  in (novaMatriz, pacmanAtualizado)

-- Função para verificar se a nova posição é válida (não é uma parede)
isMovimentoValido :: [[Map]] -> (Int, Int) -> Bool
isMovimentoValido matriz (r, c) =
  case matriz !! r !! c of
    Road _ -> True
    EmptyRoad _ -> True  -- Permite mover sobre EmptyRoad
    Wall _ -> False

-- Função para gerar um movimento aleatório válido
randomMove :: [[Map]] -> (Int, Int) -> IO (Int, Int)
randomMove matriz (r, c) = do
  let movimentos = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
  let movimentosValidos = filter (isMovimentoValido matriz) movimentos
  if null movimentosValidos
    then return (r, c)  -- Se não houver movimentos válidos, permanece na posição atual
    else do
      idx <- randomRIO (0, length movimentosValidos - 1)
      return (movimentosValidos !! idx)

-- Função para calcular o movimento mais próximo do Pacman com diferentes precisões
moverFantasmaEmDirecao :: [[Map]] -> Entity -> Entity -> IO Entity
moverFantasmaEmDirecao matriz fantasma pacman = do
  chance <- randomRIO (1 :: Int, 100 :: Int)
  let precisao = case entityName fantasma of
        "Rosa"     -> 80  -- 80% de chance de seguir o Pacman
        "Azul"     -> 60  -- 60% de chance de seguir o Pacman
        "Vermelho" -> 50  -- 50% de chance de seguir o Pacman
        "Laranja"  -> 30  -- 30% de chance de seguir o Pacman
        _          -> 40  -- Padrão 40% 

  if chance <= precisao  
    then do
      let (fx, fy) = position fantasma
          (px, py) = position pacman
          movimentos = [(fx - 1, fy), (fx + 1, fy), (fx, fy - 1), (fx, fy + 1)]
          movimentosValidos = filter (isMovimentoValido matriz) movimentos
          melhorMovimento = minimumBy (comparing (\(mx, my) -> abs (mx - px) + abs (my - py))) movimentosValidos
      return $ fantasma { position = melhorMovimento }
    else do
      novaPos <- randomMove matriz (position fantasma)
      return $ fantasma { position = novaPos }

-- Função para mover os fantasmas em direção ao Pacman
moverFantasmas :: [[Map]] -> [Entity] -> Entity -> IO [Entity]
moverFantasmas matriz fantasmas pacman = mapM (\fantasma -> moverFantasmaEmDirecao matriz fantasma pacman) fantasmas

-- Função para verificar se o Pacman encontrou algum fantasma
verificarColisao :: Entity -> [Entity] -> Bool
verificarColisao pacman fantasmas =
  any (\f -> position pacman == position f) fantasmas

-- Função para verificar se ainda há Road no mapa
haRoadsRestantes :: [[Map]] -> Bool
haRoadsRestantes = any (any (\map -> case map of Road _ -> True; _ -> False))  -- Verifica se há algum Road restante

-- Função principal de jogo
main :: IO ()
main = do
  let matriz = criarMatriz
      pacman = criarEntidade "Pacman" (11, 7)
      fantasmaAzul = criarEntidade "Azul" (7, 7)
      fantasmaVermelho = criarEntidade "Vermelho" (6, 6)
      fantasmaRosa = criarEntidade "Rosa" (6, 7)
      fantasmaLaranja = criarEntidade "Laranja" (6, 8)
      fantasmas = [fantasmaAzul, fantasmaVermelho, fantasmaRosa, fantasmaLaranja]
  
  loopJogo matriz pacman fantasmas

-- Função para o loop principal do jogo
loopJogo :: [[Map]] -> Entity -> [Entity] -> IO ()
loopJogo matriz pacman fantasmas = do
  clearScreen
  printMatrizComEntidades matriz (pacman : fantasmas)
  putStrLn $ "Pontuação: " ++ show (score pacman)
  putStrLn "Use W A S D para mover o Pacman."

  if verificarColisao pacman fantasmas
    then putStrLn "Game Over! Você foi capturado por um fantasma."
    else if not (haRoadsRestantes matriz)
      then putStrLn "Você venceu! Todos os pontos foram coletados."
      else do
        movimento <- getChar
        let (novaMatriz, pacmanAtualizado) = moverPacman matriz pacman movimento
        fantasmasAtualizados <- moverFantasmas novaMatriz fantasmas pacmanAtualizado
        loopJogo novaMatriz pacmanAtualizado fantasmasAtualizados
