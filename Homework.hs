square 0 b c = ( -c / b, -c / b)
square 0 0 _ = error "no roots"
square a b c | discriminant < 0 = error "NO REAL ROOTS"
                        | otherwise = (x1, x2)
                        where
                             discriminant = b * b - 4 * a * c
                             x1 = (-b + sqrt discriminant) / (2 * a)
                             x2 = (-b - sqrt discriminant) / (2 * a)
