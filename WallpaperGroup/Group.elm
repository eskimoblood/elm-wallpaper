module WallpaperGroup.Group where

import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Line exposing (Line)
import WallpaperGroup.Geom.Tile exposing (Tile)
import WallpaperGroup.Geom.Util as Util
import WallpaperGroup.Geom.GlideTranslate exposing (glideTranslate)
import WallpaperGroup.Geom.Mirror as Mirror
import WallpaperGroup.Geom.Rotate exposing (rotate90, rotate120, rotate180)
import WallpaperGroup.Geom.Translate as Translate
import List as L

linesToTile :  Tile ->  Tile
linesToTile lines = [L.concat lines]


type alias Setting = {
  steps: List (Tile -> Tile),
  translate:  (Int -> Point),
  tileCoordinates: List Point
}


type Group
  = P1 Float Float Int
  | P2 Float Float Int
  | Pm Float Float Int
  | Pg Float Float Int
  | Cm Float Float Int
  | P2mm Float Float Int
  | P2mg Float Float Int
  | P2gg Float Float Int
  | C2mm Float Float Int
  | P4 Float Float Int
  | P4mm Float Float Int
  | P4mg Float Float Int
  | P3 Float Int
  | P3m1 Float Int
  | P31m Float Int
  | P6 Float Int


group : Group -> Setting
group gr =
  case gr of
    P1 w h columns ->
      {
        steps= [],
        translate= Translate.w1h1 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    P2 w h columns ->
      {
        steps= [
          rotate180 {x= w / 2, y= h}
        ],
        translate= Translate.w1h2 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    Pm w h columns ->
      {
        steps= [
          Mirror.mirrorVertical w h
        ],
        translate= Translate.w1h2 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    Pg w h columns ->
      {
        steps= [
          glideTranslate (Mirror.mirror {p1= {x= 0, y= h / 2}, p2= {x= w, y= h / 2}}) w 0
        ],
        translate= Translate.w2h1 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    Cm w h columns ->
      {
        steps= [
          Mirror.mirrorHorizontal w h
        ],
        translate= Translate.shifted w h columns,
        tileCoordinates= Util.triangleCoords w h
      }

    P2mm w h columns ->
      {
        steps= [
          Mirror.mirrorHorizontal w h,
          linesToTile,
          Mirror.mirrorVertical w h
        ],
        translate= Translate.w2h2 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    P2mg w h columns ->
      {
        steps= [
          rotate180 {x= w, y= h / 2}
        ],
        translate= Translate.w2h2 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    P2gg w h columns ->
      {
        steps= [
          rotate180 {x= w / 2, y= h}
        ],
        translate= Translate.shifted w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    C2mm w h columns ->
      {
        steps= [
          Mirror.mirrorHorizontal w h,
          Mirror.mirrorVertical w h,
          Mirror.mirrorHorizontal w h
        ],
        translate= Translate.shifted (w * 2) h columns,
        tileCoordinates= Util.rectCoords w h
      }

    P4 w h columns ->
      {
        steps= [
          rotate90 {x= w, y= h},
          rotate90 {x= w, y= h},
          rotate90 {x= w, y= h}
        ],
        translate= Translate.w2h2 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    P4mm w h columns ->
      {
        steps= [
          Mirror.mirrorDiagonalRL w h,
          linesToTile,
          rotate90 {x= w, y= h},
          rotate90 {x= w, y= h},
          rotate90 {x= w, y= h}
        ],
        translate= Translate.w2h2 w h columns,
        tileCoordinates= [
          {x= 0, y= 0},
          {x= w, y= h},
          {x= 0, y= h}
        ]
      }

    P4mg w h columns ->
      {
        steps= [
          Mirror.mirrorDiagonalLR w h,
          linesToTile,
          rotate90 {x= w, y= h},
          rotate90 {x= w, y= h},
          rotate90 {x= w, y= h}
        ],
        translate= Translate.w2h2 w h columns,
        tileCoordinates= Util.rightTriangleCoords w h
      }

    P3 w columns ->
      let centerX = (sqrt 3) / 2 * w
      in
        {
          steps= [
            rotate120 {x= centerX, y= w},
            rotate120 {x= centerX, y= w}
          ],
          translate= Translate.hex (centerX * 2) (w * 2) columns,
          tileCoordinates= [
            {x= 0, y= w / 2},
            {x= centerX, y= w},
            {x= centerX, y= w * 2},
            {x= 0, y= w * 1.5}
          ]
        }

    P3m1 w columns ->
      let centerX = (sqrt 3) / 2 * w
      in
        {
          steps= [
            Mirror.mirrorHex w,
            linesToTile,
            rotate120 {x= centerX, y= w},
            rotate120 {x= centerX, y= w}
          ],
          translate= Translate.hex (centerX * 2) (w * 2) columns,
          tileCoordinates= [
            {x= centerX, y= w},
            {x= 0, y= w * 0.5},
            {x= 0, y= w * 1.5}
          ]
        }

    P31m w columns ->
      let h = (sqrt 3) / 2 * w
      in
        {
          steps= [
            rotate120 {x= w / 2, y= 2 * h / 3},
            rotate120 {x= w / 2, y= 2 * h / 3},
            linesToTile,
            Mirror.mirrorTriangle w h
          ],
          translate= Translate.shifted w h columns,
          tileCoordinates= [
            {x= 0, y= h},
            {x= w / 2, y= 2 * h / 3},
            {x= w, y= h}
          ]
        }

    P6 w columns ->
      let h = (sqrt 3) / 2 * w
      in
        {
          steps= [
            rotate180 (Util.split {x= w / 2, y= 0} {x= w , y= h} 0.5),
            rotate180 (Util.split {x= w / 2, y= 0} {x= w , y= h} 0.5),
            linesToTile,
            rotate180 (Util.split {x= w / 2, y= 0} {x= w , y= h}  0.5)
          ],
          translate= Translate.shifted w h columns,
          tileCoordinates= [
            {x= 0, y= h},
            {x= w / 2, y= 2 * h / 3},
            {x= w, y= h}
          ]
        }
