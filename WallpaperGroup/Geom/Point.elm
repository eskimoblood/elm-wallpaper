module WallpaperGroup.Geom.Point (..) where


type alias Point =
    { x : Float, y : Float }


subtract : Point -> Point -> Point
subtract p1 p2 =
    { x = p1.x - p2.x, y = p1.y - p2.y }


add : Point -> Point -> Point
add p1 p2 =
    { x = p1.x + p2.x, y = p1.y + p2.y }
