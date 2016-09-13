module NumeralSystems(
    convert,
    factorial_ns,
    fibonacci_ns,
    base_ns
    ) where
    
    convert :: Int -> [Int] -> [Int]
    convert number (base:numeralSystem)
        | number < base = [number]
        | otherwise = (convert divider numeralSystem) ++ [remainder]
        where 
            divider = number `div` base
            remainder = number `mod` base

    -- factorial numeral system
    factorials :: [Int]
    factorials = 2 : 6 : zipWith (*) [4..] (tail factorials)

    factorial_ns :: [Int]
    factorial_ns = factorials

    -- fibonacci numeral system
    fibonaccis :: [Int]
    fibonaccis = 1 : 1 : zipWith (+) fibonaccis (tail fibonaccis)

    fibonacci_ns :: [Int]
    fibonacci_ns = fibonaccis

    base_ns :: Int -> [Int]
    base_ns base = repeat base
