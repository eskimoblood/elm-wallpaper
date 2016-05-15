module WallpaperGroup.Settings exposing (..) 

import WallpaperGroup.Group exposing (..)
import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Tile exposing (Tile)
import WallpaperGroup.Geom.BoundingBox exposing (..)
import WallpaperGroup.Geom.Util as Util
import WallpaperGroup.Geom.GlideTranslate exposing (glideTranslate)
import WallpaperGroup.Geom.Mirror as Mirror
import WallpaperGroup.Geom.Rotate exposing (rotate90, rotate120, rotate180)
import WallpaperGroup.Geom.Translate as Translate
import List as L


linesToTile : List Tile -> List Tile
linesToTile tiles =
    [ L.concat tiles ]


type alias Setting =
    { steps : List (List Tile -> List Tile)
    , translate : Int -> Int -> Point
    , tileCoordinates : BoundingBox
    }


getGroupSettings : Group -> Setting
getGroupSettings gr =
    case gr of
        P1 w h ->
            { steps = []
            , translate = Translate.w1h1 w h
            , tileCoordinates = Util.rectCoords w h
            }

        P2 w h ->
            { steps =
                [ rotate180 { x = w / 2, y = h }
                ]
            , translate = Translate.w1h2 w h
            , tileCoordinates = Util.rectCoords w h
            }

        Pm w h ->
            { steps =
                [ Mirror.mirrorVertical w h
                ]
            , translate = Translate.w2h1 w h
            , tileCoordinates = Util.rectCoords w h
            }

        Pg w h ->
            { steps =
                [ glideTranslate (Mirror.mirror { p1 = { x = 0, y = h / 2 }, p2 = { x = w, y = h / 2 } }) w 0
                ]
            , translate = Translate.w2h1 w h
            , tileCoordinates = Util.rectCoords w h
            }

        Cm w h ->
            { steps =
                [ Mirror.mirrorHorizontal w h
                ]
            , translate = Translate.shifted w h
            , tileCoordinates = Util.triangleCoords w h
            }

        P2mm w h ->
            { steps =
                [ Mirror.mirrorHorizontal w h
                , linesToTile
                , Mirror.mirrorVertical w h
                ]
            , translate = Translate.w2h2 w h
            , tileCoordinates = Util.rectCoords w h
            }

        P2mg w h ->
            { steps =
                [ rotate180 { x = w, y = h / 2 }
                , linesToTile
                , Mirror.mirrorHorizontal w h
                ]
            , translate = Translate.w2h2 w h
            , tileCoordinates = Util.rectCoords w h
            }

        P2gg w h ->
            { steps =
                [ rotate180 { x = w / 2, y = h }
                ]
            , translate = Translate.shifted (w * 2) h
            , tileCoordinates = Util.rectCoords w h
            }

        C2mm w h ->
            { steps =
                [ Mirror.mirrorHorizontal w h
                , Mirror.mirrorVertical w h
                , Mirror.mirrorHorizontal w h
                ]
            , translate = Translate.shifted (w * 2) (h * 2)
            , tileCoordinates = Util.rectCoords w h
            }

        P4 w h ->
            { steps =
                [ rotate90 { x = w, y = h }
                , rotate90 { x = w, y = h }
                , rotate90 { x = w, y = h }
                ]
            , translate = Translate.w2h2 w h
            , tileCoordinates = Util.rectCoords w h
            }

        P4mm w h ->
            { steps =
                [ Mirror.mirrorDiagonalRL w h
                , linesToTile
                , rotate90 { x = w, y = h }
                , rotate90 { x = w, y = h }
                , rotate90 { x = w, y = h }
                ]
            , translate = Translate.w2h2 w h
            , tileCoordinates = Triangle { x = 0, y = 0 } { x = w, y = h } { x = 0, y = h }
            }

        P4mg w h ->
            { steps =
                [ Mirror.mirrorDiagonalLR w h
                , linesToTile
                , rotate90 { x = w, y = h }
                , rotate90 { x = w, y = h }
                , rotate90 { x = w, y = h }
                ]
            , translate = Translate.w2h2 w h
            , tileCoordinates = Util.rightTriangleCoords w h
            }

        P3 w ->
            let
                centerX = (sqrt 3) / 2 * w
            in
                { steps =
                    [ rotate120 { x = centerX, y = w }
                    , rotate120 { x = centerX, y = w }
                    ]
                , translate = Translate.hex (centerX * 2) (w * 2)
                , tileCoordinates = Rect { x = 0, y = w / 2 } { x = centerX, y = w } { x = centerX, y = w * 2 } { x = 0, y = w * 1.5 }
                }

        P3m1 w ->
            let
                centerX = (sqrt 3) / 2 * w
            in
                { steps =
                    [ Mirror.mirrorHex w
                    , linesToTile
                    , rotate120 { x = centerX, y = w }
                    , rotate120 { x = centerX, y = w }
                    ]
                , translate = Translate.hex (centerX * 2) (w * 2)
                , tileCoordinates = Triangle { x = centerX, y = w } { x = 0, y = w * 0.5 } { x = 0, y = w * 1.5 }
                }

        P31m w ->
            let
                h = (sqrt 3) / 2 * w
            in
                { steps =
                    [ rotate120 { x = w / 2, y = 2 * h / 3 }
                    , rotate120 { x = w / 2, y = 2 * h / 3 }
                    , linesToTile
                    , Mirror.mirrorTriangle w h
                    ]
                , translate = Translate.shifted w h
                , tileCoordinates = Triangle { x = 0, y = h } { x = w / 2, y = 2 * h / 3 } { x = w, y = h }
                }

        P6 w ->
            let
                h = (sqrt 3) / 2 * w
            in
                { steps =
                    [ rotate120 { x = w / 2, y = 2 * h / 3 }
                    , rotate120 { x = w / 2, y = 2 * h / 3 }
                    , linesToTile
                    , rotate180 (Util.split { x = w / 2, y = 0 } { x = w, y = h } 0.5)
                    ]
                , translate = Translate.shifted w h
                , tileCoordinates = Triangle { x = 0, y = h } { x = w / 2, y = 2 * h / 3 } { x = w, y = h }
                }
