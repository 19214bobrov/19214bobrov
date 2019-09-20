jam a b c 
    |( a == 0 && b == 0) = error "NO ROOTS!"
    |( a == 0 && b /= 0) = (x,x) 
    |( discriminant < 0) = error "NO REAL ROOTS!"
    |( a /= 0) = (x1, x2)
        where
            discriminant = b^2 - 4 * a * c
            x1 = (-b + sqrt discriminant) / (2 * a)
            x2 = (-b - sqrt discriminant) / (2 * a)
            x = (-c / b)