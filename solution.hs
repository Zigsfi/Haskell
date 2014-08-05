zipi :: [a] -> [b] -> [(a, b)]

zipi (x:[]) (y:_) = [(x, y)]
zipi (x:_)  (y:[]) = [(x, y)]
zipi (x:xs) (y:ys) = (x, y) : zip xs ys

unzipi :: [(a, b)] -> ([a], [b])

unzipi ((x, y):[]) = ([x], [y])
unzipi ((x, y):xys) =  let (xs, ys) = unzip xys
                    in (x:xs, y:ys)



argmax _ (x:[]) = x
argmax f (x:xs) =   let result = f x; curmax = argmax f xs
                    in  if result > f curmax
                            then x
                            else curmax


