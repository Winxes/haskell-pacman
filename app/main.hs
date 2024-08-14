{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Control.Monad ()
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)
import Data.List ( minimumBy, isPrefixOf )
import Data.Ord (comparing)
import System.IO (withFile, IOMode(ReadMode), hGetContents)


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
      | r == 21 && c > 6 && c <= 13 = Wall(r,c)
      | r == 16 && c > 6 && c <= 13 = Wall(r,c)
      | r == 18 && c > 6 && c < 9 = Wall(r,c)
      | r == 18 && c > 11 && c < 14 = Wall(r,c)
      | r == 19 && c == 7 = Wall(r,c)
      | r == 19 && c == 13 = Wall(r,c)
      | r > 15 && r < 19 && c == 10 = Wall(r,c)
      | r > 18 && r < 21 && c > 15 && c < 18 = Wall(r,c)
      | r > 18 && r < 21 && c > 2 && c < 5 = Wall(r,c)
      | r == 17 && c > 2 && c < 6 = Wall(r,c)
      | r == 17 && c > 14 && c < 18 = Wall(r,c)
      | r > 13 && r < 17 && c == 3 = Wall(r,c)
      | r > 13 && r < 17 && c == 17 = Wall(r,c)
      | r == 15 && c == 2 = Wall (r,c)
      | r == 15 && c == 18 = Wall (r,c)
      | r > 8 && r < 16 && c == 5 = Wall(r,c)
      | r > 8 && r < 16 && c == 15 = Wall(r,c)  
      | r > 7 && r < 12 && c == 2 = Wall(r,c)
      | r > 7 && r < 12 && c == 18 = Wall(r,c)
      | r == 9 && c == 3 = Wall (r,c)
      | r == 9 && c == 17 = Wall (r,c)
      | r == 14 && c > 6 && c <= 13 = Wall (r,c)
      | r > 10 && r < 14 && c == 7 = Wall(r,c)
      | r > 10 && r < 14 && c == 13 = Wall(r,c)
      | r == 8 && c > 6 && c <= 13 = Wall (r,c)
      | r == 9 && c == 10 = Wall(r,c)
      | r > 5 && r < 8 && c == 5 = Wall(r,c)
      | r > 5 && r < 8 && c == 15 = Wall(r,c)
      | r > 2 && r < 7 && c == 6 = Wall(r,c)
      | r > 2 && r < 7 && c == 14 = Wall(r,c)
      | r == 3 && c == 7 = Wall(r,c)
      | r == 3 && c == 13 = Wall(r,c)
      | r == 5 && c > 1 && c < 4 = Wall(r,c)
      | r == 5 && c > 16 && c < 19 = Wall(r,c)
      | r == 3 && c > 1 && c < 4 = Wall(r,c)
      | r == 3 && c > 16 && c < 19 = Wall(r,c)
      | r == 2 && c == 3 = Wall(r,c)
      | r == 2 && c == 17 = Wall(r,c)
      | r > 1 && r < 6 && c == 10 = Wall(r,c)
      | r == 4 && c == 9 = Wall(r,c)
      | r == 4 && c == 11 = Wall(r,c)
      | r == 20 && c == 2 = Wall(r,c)
      | r == 20 && c == 18 = Wall(r,c)
      | r == 21 && c == 3 = Wall(r,c)
      | r == 21 && c == 17 = Wall(r,c)
      | r > 5 && r < 8 && c == 8 = Wall(r,c)
      | r > 5 && r < 8 && c == 12 = Wall(r,c)
      | r == 9 && c == 7 = Wall(r,c)
      | r == 9 && c == 13 = Wall(r,c)
      | r == 7 && c > 1 && c < 4 = Wall(r,c)
      | r == 7 && c > 16 && c < 19 = Wall(r,c)
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
          | nome == "Pacman" -> "\ESC[93m⬤\ESC[0m"  --
          | nome == "Azul"    -> "\ESC[34mᗣ\ESC[0m"  -- 
          | nome == "Vermelho" -> "\ESC[31mᗣ\ESC[0m"  -- 
          | nome == "Rosa"    -> "\ESC[35mᗣ\ESC[0m"  -- 
          | nome == "Laranja" -> "\ESC[33mᗣ\ESC[0m"  -- 
          | otherwise -> "\ESC[31mᗣ\ESC[0m"  -- 

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
      novaPosicao = case movimento of
        'w' -> (r - 1, c)  -- Mover para cima
        's' -> (r + 1, c)  -- Mover para baixo
        'a' -> (r, c - 1)  -- Mover para a esquerda
        'd' -> (r, c + 1)  -- Mover para a direita
        _   -> (r, c)
      (novaMatriz, pontos, novaPosicaoValida) =
        if verificaMovimentoValido matriz novaPosicao
        then let (m, p) = atualizarMapa matriz novaPosicao in (m, p, novaPosicao)
        else (matriz, 0, (r, c)) -- Se movimento não for válido, mantém a posição e matriz atuais
      pacmanAtualizado = pacman { position = novaPosicaoValida, score = score pacman + pontos }
  in (novaMatriz, pacmanAtualizado)

-- Função para verificar se a nova posição é válida (não é uma parede)
verificaMovimentoValido :: [[Map]] -> (Int, Int) -> Bool
verificaMovimentoValido matriz (r, c) =
  case matriz !! r !! c of
    Road _ -> True
    EmptyRoad _ -> True  -- Permite mover sobre EmptyRoad
    Wall _ -> False

-- Função para gerar um movimento aleatório válido
randomMove :: [[Map]] -> (Int, Int) -> IO (Int, Int)
randomMove matriz (r, c) = do
  let movimentos = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
  let movimentosValidos = filter (verificaMovimentoValido matriz) movimentos
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
        "Rosa"     -> 100  -- 100% de chance de seguir o Pacman
        "Azul"     -> 100  -- 100% de chance de seguir o Pacman
        "Vermelho" -> 100  -- 100% de chance de seguir o Pacman
        "Laranja"  -> 80  -- 80% de chance de seguir o Pacman
        _          -> 40  -- Padrão 40% 

  if chance <= precisao
    then do
      let (fx, fy) = position fantasma
          (px, py) = position pacman
          movimentos = [(fx - 1, fy), (fx + 1, fy), (fx, fy - 1), (fx, fy + 1)]
          movimentosValidos = filter (verificaMovimentoValido matriz) movimentos
          melhorMovimento = minimumBy (comparing (\(mx, my) -> abs (mx - px) + abs (my - py))) movimentosValidos
      return $ fantasma { position = melhorMovimento }
    else do
      novaPosicao <- randomMove matriz (position fantasma)
      return $ fantasma { position = novaPosicao }

-- Função para mover os fantasmas em direção ao Pacman
moverFantasmas :: [[Map]] -> [Entity] -> Entity -> IO [Entity]
moverFantasmas matriz fantasmas pacman = mapM (\fantasma -> moverFantasmaEmDirecao matriz fantasma pacman) fantasmas

-- Função para verificar se o Pacman encontrou algum fantasma
verificarColisao :: Entity -> [Entity] -> Bool
verificarColisao pacman = any (\f -> position pacman == position f)

-- Função para verificar se ainda há Road no mapa
existePontosRestantes :: [[Map]] -> Bool
existePontosRestantes = any (any (\m -> case m of Road _ -> True; _ -> False))  -- Verifica se há algum Road restante

-- Pergunta o nome do jogador
perguntarNome :: IO String
perguntarNome = do
  putStrLn "Digite seu nome:"
  getLine

-- Salva a pontuação do jogador no arquivo pontuacoes.txt
salvarPontuacao :: Entity -> IO ()
salvarPontuacao pacman = do

  nome <- perguntarNome
  let pontos = score pacman

  -- Lê o conteúdo atual do arquivo
  novasLinhas <- withFile "pontuacoes.txt" ReadMode $ \handle -> do
    conteudo <- hGetContents handle
    let linhas = lines conteudo
    -- lê o arquivo completamente
    length linhas `seq` return (atualizarOuAdicionarPontuacao linhas nome pontos)

  -- Sobrescrever o arquivo com o novo conteúdo
  writeFile "pontuacoes.txt" (unlines novasLinhas)


atualizarOuAdicionarPontuacao :: [String] -> String -> Int -> [String]
atualizarOuAdicionarPontuacao [] nome pontos = [nome ++ ": " ++ show pontos]
-- Atualiza a pontuação se o nome já existir, caso contrário, adiciona uma nova linha
atualizarOuAdicionarPontuacao (linha:linhas) nome pontos
  | nome `isPrefixOf` linha =
    -- Atualiza a pontuação se o novo valor for maior
      let scoreAtual = read (drop (length nome + 2) linha) :: Int
      in if pontos > scoreAtual
         then (nome ++ ": " ++ show pontos) : linhas
         else linha : linhas
  | otherwise = linha : atualizarOuAdicionarPontuacao linhas nome pontos

-- retorna as pontuações salvas
retornarPontuacoes :: IO ()
retornarPontuacoes = do
  conteudo <- readFile "pontuacoes.txt"
  putStrLn conteudo

-- Função principal de jogo
main :: IO ()
main = do
  let matriz = criarMatriz
      pacman = criarEntidade "Pacman" (20, 10)
      fantasmaAzul = criarEntidade "Azul" (12, 10)
      fantasmaVermelho = criarEntidade "Vermelho" (11, 11)
      fantasmaRosa = criarEntidade "Rosa" (11, 9)
      fantasmaLaranja = criarEntidade "Laranja" (11, 10)
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
    then putStrLn "Game Over! Você foi capturado por um fantasma." >> salvarPontuacao pacman >> retornarPontuacoes

    else if not (existePontosRestantes matriz)
      then putStrLn "Você venceu! Todos os pontos foram coletados." >> salvarPontuacao pacman >> retornarPontuacoes
      else do
        movimento <- getChar
        let (novaMatriz, pacmanAtualizado) = moverPacman matriz pacman movimento
        fantasmasAtualizados <- moverFantasmas novaMatriz fantasmas pacmanAtualizado
        loopJogo novaMatriz pacmanAtualizado fantasmasAtualizados
