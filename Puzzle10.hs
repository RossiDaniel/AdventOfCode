
type Coord = (Int,Int)
type Board = [[Char]]
type Quad = (Int,Int)
test :: Board 
test = ["#..#....#...#.#..#.......##.#.####","#......#..#.#..####.....#..#...##.",".##.......#..#.#....#.#..#.#....#.","###..#.....###.#....##.....#...#..","...#.##..#.###.......#....#....###",".####...##...........##..#..#.##..","..#...#.#.#.###....#.#...##.....#.","......#.....#..#...##.#..##.#..###","...###.#....#..##.#.#.#....#...###","..#.###.####..###.#.##..#.##.###..","...##...#.#..##.#............##.##","....#.##.##.##..#......##.........",".#..#.#..#.##......##...#.#.#...##",".##.....#.#.##...#.#.#...#..###...","#.#.#..##......#...#...#.......#..","#.......#..#####.###.#..#..#.#.#..",".#......##......##...#..#..#..###.","#.#...#..#....##.#....#.##.#....#.","....#..#....##..#...##..#..#.#.##.","#.#.#.#.##.#.#..###.......#....###","...#.#..##....###.####.#..#.#..#..","#....##..#...##.#.#.........##.#..",".#....#.#...#.#.........#..#......","...#..###...#...#.#.#...#.#..##.##",".####.##.#..#.#.#.#...#.##......#.",".##....##..#.#.#.......#.....####.","#.##.##....#...#..#.#..###..#.###.","...###.#..#.....#.#.#.#....#....#.","......#...#.........##....#....##.",".....#.....#..#.##.#.###.#..##....",".#.....#.#.....#####.....##..#....",".####.##...#.......####..#....##..",".#.#.......#......#.##..##.#.#..##","......##.....##...##.##...##......"]

board :: Board
board = ["......#.#.","#..#.#....","..#######.",".#.#.###..",".#..#.....","..#....#.#","#..#....#.",".##.#..###","##...#..#.",".#....####"]

board1 :: Board
board1 = [".#..#",".....","#####","....#","...##"]

board2 :: Board
board2 = [".#....#####...#..","##...##.#####..##","##...#...#.#####.","..#.....#...###..","..#.#.....#....##"]

board3 :: Board
board3 = [".#..##.###...#######","##.############..##.",".#.######.########.#",".###.#######.####.#.","#####.##.#.##.###.##","..#####..#.#########","####################","#.####....###.#.#.##","##.#################","#####.##.###..####..","..######..##.#######","####.##.####...##..#",".#####..#.######.###","##...#.##########...","#.##########.#######",".####.#.###.###.#.##","....##.##.###..#####",".#.#.###########.###","#.#.#.#####.####.###","###.##.####.##.#..##"]

intorno :: Board -> Coord -> Int ->  [Coord]
intorno b (x,y) n =  gx ++ gy
                  where 
                    gx = [(x+xn,y+yn) | xn <- [((-n)+1)..(n-1)], yn <- [(-n),n], (x+xn) >= 0 && (y+yn) >= 0 && (x+xn) < (length (b!!0)) && (y+yn) < (length b) && isast b ((x+xn),(y+yn))]
                    gy = [(x+xn,y+yn) | xn <- [(-n),n], yn <- [(-n)..n], (x+xn) >= 0 && (y+yn) >= 0 && (x+xn) < (length (b!!0)) && (y+yn) < (length b) && isast b ((x+xn),(y+yn))]

coordinates :: Board -> Coord ->[Coord] 
coordinates b (i,j)= concat [intorno b (i,j) n | n <- [1..(max (length (b!!0)) (length b))]]

isast :: Board -> Coord -> Bool
isast b (x,y) | (b!!y)!!x == '#' = True
              | otherwise        = False

clear :: [Coord] -> [Coord] -> [Coord]
clear [] ys = ys
clear (x:xs) ys = clear xs [y | y <- ys, y /= x]

generate :: Board -> Coord -> Int -> Int -> [Coord]
generate b (x,y) xd yd | x >= 0 && y >= 0 && x < (length (b!!0)) && y < (length b) = (x,y):(generate b (x+xd,y+yd) xd yd)
                       | otherwise = []

countast :: Board -> Coord -> [Coord] -> Int
countast _ _ [] = 0
countast b (x,y) ((cx,cy):cs) = 1 + (countast b (x,y) (clear (generate b (x,y) ((cx-x) `div` g) ((cy-y) `div` g)) cs))
                              where
                                  g = gcd (cx-x) (cy-y)

search :: Board -> [(Coord,Int)]
search b = [((i,j),(countast b (i,j) (coordinates b (i,j)))) | i <- [0..(length (b!!0) -1)], j <- [0..((length b) -1 )], isast b (i,j)]

best :: [(Coord,Int)] -> (Coord,Int)
best [] = ((0,0),0)
best ((x,i):xs) | i > bi    = (x,i)
                | otherwise = (bx,bi)
                where 
                    (bx,bi) = best xs 

--search :: Board -> [(Coord,[((Coord,Coord),[Coord],[Coord],[Coord])])]
--search b = [((i,j),(countast b (i,j) (coordinates b (i,j)))) | i <- [0..(length (b!!0) -1)], j <- [0..((length b) -1 )], isast b (i,j)]
                    

-- countast :: Board -> Coord -> [Coord] -> [((Coord,Coord),[Coord],[Coord],[Coord])]
-- countast b (x,y) ((cx,cy):cs) | isast b (cx,cy) = (((x,y),(cx,cy)),cs,(clear (generate b (x,y) (cx-x) (cy-y)) cs),(generate b (x,y) (cx-x) (cy-y))): (countast b (x,y) (clear (generate b (x,y) (cx-x) (cy-y)) cs))

--parte 2

m :: Coord -> Coord -> Float
m (x1,y1) (x2,y2) = (fromIntegral (x2-x1)) / (fromIntegral (y2-y1))

qsort :: [((Coord),Float)] -> [Coord]
qsort [] = []
qsort (((x,y),m):ns) = (qsort smaller) ++ [(x,y)] ++ (qsort greater)
             where 
                smaller = [((xi,yi),mi)|((xi,yi),mi) <- ns, mi <= m]
                greater = [((xi,yi),mi)|((xi,yi),mi) <- ns,  mi > m]

hitast :: Board -> Coord -> [Coord] -> [Coord]
hitast _ _ [] = []
hitast b (x,y) ((cx,cy):cs) = (cx,cy) : (hitast b (x,y) (clear (generate b (x,y) ((cx-x) `div` g) ((cy-y) `div` g)) cs))
                            where
                                g = gcd (cx-x) (cy-y)

coeff :: Coord -> [Coord] -> [((Coord),Float)]
coeff (x,y) ast = [((ax,ay),(m (x,y) (ax,ay))) | (ax,ay) <- ast, ax /= x && ay /= y]

axis :: (Coord -> Coord -> Bool) -> Board -> Coord -> [Coord] -> [Coord]
axis f b (x,y) cs= [(xi,yi) | (xi,yi) <- cs, f (x,y) (xi,yi)]

ciao :: (Coord -> Coord -> Bool) -> ([Coord] -> [Coord]) -> Board -> Coord -> [Coord] -> [Coord]
ciao f g b (i,j) cs = (axis f b (i,j) cs) ++ (g (qsort (coeff (i,j) cs)))

upboard :: Board -> [Coord] -> Board
upboard b [] = b
upboard b ((x,y):ns) = upboard ((take y b) ++ [(take x (b!!y)) ++ "." ++ (drop (x+1) (b!!y))] ++ (drop (y+1) b)) ns

coorquad :: Board -> Coord -> Int -> Int ->[Coord]
coorquad b (i,j) sx sy | sx == 1 && sy == 1   = ciao (\ (a,b) (c,d) -> a == c) (\x -> reverse x) b (i,j) [(x,y) | (x,y) <- (hitast b (i,j) (coordinates b (i,j))), i <= x && j > y]
                       | sx == 1 && sy == -1  = ciao (\ (a,b) (c,d) -> b == d) (\x -> reverse x) b (i,j) [(x,y) | (x,y) <- (hitast b (i,j) (coordinates b (i,j))), i < x && j <= y]
                       | sx == -1 && sy == -1 = ciao (\ (a,b) (c,d) -> a == c) (\x -> reverse x) b (i,j) [(x,y) | (x,y) <- (hitast b (i,j) (coordinates b (i,j))), i >= x && j < y]
                       | sx == -1 && sy == 1  = ciao (\ (a,b) (c,d) -> b == d) (\x -> reverse x) b (i,j) [(x,y) | (x,y) <- (hitast b (i,j) (coordinates b (i,j))), i > x && j >= y]

stop :: Board -> [Coord] -> Bool 
stop _ [] = False
stop b ((x,y):cs) | (b!!y)!!x == '#' = True
                | otherwise        = stop b cs

nextquad sx sy | sx == 1 && sy == 1   = (1,(-1))
               | sx == 1 && sy == -1  = ((-1),(-1))
               | sx == -1 && sy == -1 = ((-1),1)
               | sx == -1 && sy == 1  = (1,1)

laser :: Board -> Coord -> Int -> Int -> [Coord]
laser b (x,y) sx sy | stop b (coordinates b (x,y)) = obj ++ (laser (upboard b obj) (x,y) newsx newsy) 
                    | otherwise = []
                    where
                        obj = coorquad b (x,y) sx sy
                        (newsx,newsy) = nextquad sx sy