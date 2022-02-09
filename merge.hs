merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge a@(h:first) b@(c:second) = h:merge first b
       