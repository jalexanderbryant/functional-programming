module PolygonArea where

-- Function computeArea: Computes area of a polygon
computeArea :: [(Double,Double)] -> Double
computeArea [] = error "Hold on there...you can't calculate the area with an empty list."
computeArea [x] = error "Hold on there...you can't calculate the area with a single vertex."

computeArea (x:xs) = ((det (last xs) (x) ) + detRecursive (x:xs)) *0.5
computeArea _ = 0.0


-- Function detRecursive: Computes determinants recursively of all vertices
detRecursive :: [(Double, Double)] -> Double
detRecursive [x,y] = det x y
detRecursive (x:xs) = (det (x) (xs!!0)) + detRecursive( xs )


-- Function det: Computes 2 x 2 determinant
det :: (Double, Double) -> (Double, Double) -> Double
det (a, b) (c, d) = (a*d) - (b*c)
