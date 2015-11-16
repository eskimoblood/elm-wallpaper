module WallpaperGroup.Group where

import WallpaperGroup.Geom.Point exposing (Point)
import WallpaperGroup.Geom.Line exposing (Line)
import WallpaperGroup.Geom.Util as Util
import WallpaperGroup.Geom.GlideTranslate as GlideTranslate
import WallpaperGroup.Geom.Mirror as Mirror
import WallpaperGroup.Geom.Rotate as Rotate
import WallpaperGroup.Geom.Translate as Translate


linesToTile : Lines List Lines
linesToTile lines = [concat lines]


type alias Setting = {
  step: List (List Line -> List Line),
  translate: List (Int -> Point),
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
  | P3 Float Int
  | P3m1 Float Int
  | P31m Float Int
  | P6 Float Int


group : Group -> Settings
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
          mirrorVertical w h
        ],
        translate= Translate.w1h2 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    Pg w h columns ->
      {
        steps= [
          glideTranslate (mirror [{x= 0, y= h / 2}, {x= w, y= h / 2}]) w 0
        ],
        translate= Translate.w2h1 w h columns,
        tileCoordinates= Util.rectCoords w h
      }

    Cm w h columns ->
      {
        steps= [
          mirrorHorizontal w h
        ],
        translate= Translate.shifted w h columns,
        tileCoordinates= Util.triangleCoords w h
      }

    P2mm w h columns ->
      {
        steps= [
          mirrorHorizontal w h,
          linesToTile,
          mirrorVertical w h
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
          mirrorHorizontal w h,
          mirrorVertical w h,
          mirrorHorizontal w h
        ],
        translate= Translate.shifted w * 2 h columns,
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
          mirrorDiagonalRL(width, height),
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
          mirrorDiagonalLR w h,
          linesToTile,
          rotate90 {x= w, y= h},
          rotate90 {x= w, y= h},
          rotate90 {x= w, y= h}
        ],
        translate= Translate.w2h2 w h columns,
        tileCoordinates= Util.rightTriangleCoords w h
      }

    P3 w columns ->
      let centerX = sqrt(3) / 2 * w
      in
        {
          steps= [
            rotate120 {x= centerX, y= w},
            rotate120 {x= centerX, y= w}
          ],
          translate= Translate.hex centerX * 2, w * 2, columns,
          tileCoordinates= [
            {x= 0, y= w / 2},
            {x= centerX, y= w},
            {x= centerX, y= w * 2},
            {x= 0, y= w * 1.5}
          ]
        }

    P3m1 w columns ->
      let centerX = sqrt(3) / 2 * w
      in
        {
          steps= [
            mirrorHex w,
            linesToTile,
            rotate120 {x= centerX, y= w},
            rotate120 {x= centerX, y= w}
          ],
          translate= Translate.hex centerX * 2, w * 2, columns,
          tileCoordinates= [
            {x= centerX, y= w},
            {x= 0, y= w * 0.5},
            {x= 0, y= w * 1.5}
          ]
        }

    P31m w columns ->
      let h = sqrt(3) / 2 * w
      in
        {
          steps= [
            rotate120({x= w / 2, y= 2 * h / 3})
            rotate120({x= w / 2, y= 2 * h / 3})
            linesToTile,
            mirrorTriangle w h
          ],
          translate= Translate.shifted w h columns,
          tileCoordinates= [
            {x= 0, y= h},
            {x= w / 2, y= 2 * h / 3},
            {x= w, y= h}
          ]
        }

    P6 w columns ->
      let h = sqrt(3) / 2 * w
      in
        {
          steps= [
            rotate180 (split {x= w / 2, y= 0} {x= w , y= h} 0.5),
            rotate180 (split {x= w / 2, y= 0} {x= w , y= h} 0.5),
            linesToTile,
            rotate180 (split {x= w / 2, y= 0} {x= w , y= h}  0.5)
          ],
          translate= TranslateS.shifted w h columns,
          tileCoordinates= [
            {x= 0, y= h},
            {x= w / 2, y= 2 * h / 3},
            {x= w, y= h}
          ]
        }
