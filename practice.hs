mystery0  [] = 0
mystery0  (a:x) = 1 + mystery0  x
mystery1  xs  =  [y | x <- xs,  y <- [x,x] ]
mystery2   f    []       =  []
mystery2   f    (a:x)  =  f    a   :  mystery2 f x
mystery3   item  []  = []
mystery3  item  (x:y) | item==x   = mystery3 item y
                      | otherwise = x : mystery3 item y
mysteryd = (*) 2
mysteryr f x = f (f x)
mystery4 = mysteryr mysteryd
