module Lib (
        Coord(..),
        World(..),
        Textures(..)
) where

import Graphics.Gloss.Data.Picture

type Coord = (Int, Int)

data World = World {boxes :: [Coord],
                        boxes_docked :: [Coord],
                        docks :: [Coord],
                        tiles :: [Coord],
                        walls :: [Coord],
                        worker :: Coord,
                        size_x :: Int,
                        size_y :: Int,
                        textures :: Textures} deriving Show

data Textures = Textures {box_texture :: Picture,
                                box_docked_texture :: Picture,
                                dock_texture :: Picture,
                                tile_texture :: Picture,
                                wall_texture :: Picture,
                                worker_texture :: Picture} deriving Show
