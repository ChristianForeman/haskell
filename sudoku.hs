-- Basic types
type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

boxsize :: Int
boxsize = 3

blank :: Grid
blank = replicate 9 (replicate 9 '.')

easy :: Grid
easy = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..8...2.",
        ".5..69784",
        "4..25...."]

rows :: Matrix a -> [Row a]
rows = id

-- Rows has the property: rows . rows = id

-- try to implement tranpose without the library
cols :: Matrix a -> [Row a]
cols = transpose

-- Cols has the property: cols . cols = id

boxes                  :: Matrix a -> [Row a]
boxes                  =  unpack . map cols . pack
                          where
                            pack   = split . map split
                            split  = chop boxsize
                            unpack = map concat . concat

chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)

-- boxess has the property: boxes . boxes = id

valid                 :: Grid -> Bool
valid g               =  all nodups (rows g) &&
                         all nodups (cols g) &&
                         all nodups (boxes g)

nodups                :: Eq a => [a] -> Bool
nodups []             =  True
nodups (x:xs)         =  not (elem x xs) && nodups xs