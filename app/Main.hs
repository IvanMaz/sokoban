module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap()
import Graphics.Gloss.Data.Picture()
import System.IO

import Prelude hiding (Either(..))

update :: Float -> World -> World
update _ world = world

to_float :: Int -> Float
to_float x = fromIntegral (x :: Int) :: Float

--Загрузка текстур
get_textures :: IO Textures
get_textures = do
        box_img <- loadBMP "./images/box.bmp"
        box_docked_img <- loadBMP "./images/box_docked.bmp"
        dock_img <- loadBMP "./images/dock.bmp"
        tile_img <- loadBMP "./images/tile.bmp"
        wall_img <- loadBMP "./images/wall.bmp"
        worker_img <- loadBMP "./images/worker.bmp"
        return Textures {box_texture = box_img,
                                box_docked_texture = box_docked_img,
                                dock_texture = dock_img,
                                tile_texture = tile_img,
                                wall_texture = wall_img,
                                worker_texture = worker_img}

--Размер ячейки в пикселях
texture_size :: Int
texture_size = 32

--Начальная генерация пустого мира
empty_world :: Textures -> World
empty_world in_textures = World {boxes = [],
                                boxes_docked = [],
                                docks = [],
                                tiles = [],
                                walls = [],
                                worker = (0,0),
                                size_x = 0,
                                size_y = 0,
                                textures = in_textures}

--Разбор входного файла. Правила: '@' - рабочий, 'o' - коробка, '.' - цель, '#' - стена, ' ' - пустое место 
add_symbol :: World -> Char -> Coord -> World
add_symbol world symbol coord =
        case symbol of
                'o' -> world {boxes = coord : boxes world}
                '.' -> world {docks = coord : docks world}
                ' ' -> world {tiles = coord : tiles world}
                '#' -> world {walls = coord : walls world}
                '@' -> world {worker = coord, tiles = coord : tiles world}
                _ -> error (show symbol ++ " not recognized")

--Наполняем данные по входному файлу
new_world :: Textures -> String -> World
new_world in_textures input = snd (foldl fill_data ((0, 0), (empty_world in_textures)) input)
        where fill_data ((i, j), world) symbol
                | symbol == '\n' = ((i + 1, 0), world{size_y = max j (size_y world), size_x = max (i + 2) (size_x world)})
                | otherwise = ((i, j + 1), add_symbol world{size_y = max (j + 2) (size_y world)} symbol (i, j))

--По координатам и текстуре получаем картинку в нужных координатах
draw_picture :: [Coord] -> Picture -> Int -> Int -> [Picture]
draw_picture [] _ _ _ = []
draw_picture ((i, j) : xs) pic max_x max_y = (translate x y pic) : draw_picture xs pic max_x max_y 
        where
                x = (to_float (-(max_y `div` 2) * texture_size + j * texture_size)) 
                y = (to_float ( (max_x `div` 2) * texture_size - i * texture_size)) 

--Функция отрисовки объектов
render :: World -> Picture
render world = pictures (map pictures pictures_to_draw ++ worker_to_draw)
        where
                all_textures = textures world
                x = size_x world
                y = size_y world
                
                objects_list = map (\f -> f world) [boxes, docks, tiles, walls, boxes_docked]
                textures_list = map (\f -> f all_textures) [box_texture, dock_texture, tile_texture, wall_texture, box_docked_texture]
                pictures_to_draw = map (\(object, texture) -> draw_picture object texture x y ) (zip objects_list textures_list)
                worker_to_draw = draw_picture ((worker world) : []) (worker_texture all_textures) x y 

--Обработчик нажатий на клавиши
handler :: Event -> World -> World
handler (EventKey (Char c) Down _ _ ) world = case c of
                                                'w' -> change_position (-1, 0) world
                                                'a' -> change_position (0, -1) world
                                                's' -> change_position (1, 0) world
                                                'd' -> change_position (0, 1) world
                                                _ -> world
handler _ world = world

change_position :: (Int, Int) -> World -> World
change_position (di, dj) world = move_worker world (i + di, j + dj) (i + 2*di, j + 2*dj)
        where
                i = fst (worker world)
                j = snd (worker world)

--Функция по необходимости изменяет состояние мира, если запрашиваемой новой позиции соответсвует действие
move_worker :: World -> Coord -> Coord -> World
move_worker world new_pos pos_throught_one
        --НЕ двигаем, если дальше стена, две коробки, коробка и стена
        | (check new_pos wallsCoords) ||
                ((check new_pos boxesCoords) || (check new_pos boxes_dockedCoords)) && ((check pos_throught_one boxesCoords) || 
                        (check pos_throught_one boxes_dockedCoords) || (check pos_throught_one wallsCoords)) = world

        | (check new_pos boxesCoords) = world {worker = new_pos,
                                                --Если передвигаем коробку на нужное место, то переводим ее в список установленных коробок, рисуем новый пустой пол
                                                boxes = if (check pos_throught_one docksCoords) 
                                                        then (deleteBox new_pos boxesCoords)
                                                        else (moveBox new_pos pos_throught_one boxesCoords),
                                                boxes_docked = if (check pos_throught_one docksCoords) 
                                                                then pos_throught_one : boxes_dockedCoords
                                                                else boxes_dockedCoords,
                                                tiles = if (check pos_throught_one docksCoords) 
                                                        then new_pos : tilesCoords
                                                        else (moveSpace new_pos pos_throught_one tilesCoords)}

        | (check new_pos boxes_dockedCoords) = world {worker = new_pos,
                                                --Если сдвигаем коробку с цели, то делаем обратную операцию
                                                boxes = if (check pos_throught_one docksCoords) 
                                                        then boxesCoords
                                                        else pos_throught_one : boxesCoords,
                                                boxes_docked = if (check pos_throught_one docksCoords) 
                                                                then (moveBox new_pos pos_throught_one boxes_dockedCoords)
                                                                else (deleteBox new_pos boxes_dockedCoords),
                                                tiles = if (check pos_throught_one docksCoords) 
                                                        then tilesCoords
                                                        else (deleteSpace pos_throught_one tilesCoords)}
        --Иначе просто передвигаем рабочего
        | otherwise = world {worker = new_pos}
        where
                wallsCoords = (walls world)
                docksCoords = (docks world)
                tilesCoords = (tiles world)
                boxesCoords = (boxes world)
                boxes_dockedCoords = (boxes_docked world)


check :: Coord -> [Coord] -> Bool
check (i, j) objects = foldr (\(a, b) acc -> (i == a && j == b) || acc) False objects

moveBox :: Coord -> Coord -> [Coord] -> [Coord]
moveBox (i, j) (newI, newJ) _boxes = map (\(a, b) -> if (i == a && j == b) then (newI, newJ) else (a, b)) _boxes

moveSpace :: Coord -> Coord -> [Coord] -> [Coord]
moveSpace (i, j) (newI, newJ) _tiles = map (\(a, b) -> if (newI == a && newJ == b) then (i, j) else (a, b)) _tiles

deleteBox :: Coord -> [Coord] -> [Coord]
deleteBox (i, j) _boxes = filter (\(a, b) -> not (i == a && j == b)) _boxes

deleteSpace :: Coord -> [Coord] -> [Coord]
deleteSpace (i, j) _tiles = filter (\(a, b) -> not (i == a && j == b)) _tiles

main :: IO()
main = do
        --Загружаем головоломку и текстуры из файлов
        src <- readFile "level.txt"
        all_textures <- get_textures

        world <- return (new_world all_textures src)
        play (InWindow "Sokoban" (640, 480) (320, 240)) (makeColorI 222 214 174 255) 30 world render handler update
        return ()
