module For exposing (..)

-- First Int (`i`) is to check how the for process
-- Second Int (`n`) set the times of function `f`
-- Type constant (`c`) is the outer environment constant
-- ( constant -> Int->  io -> io) set a function which use (outer environemnt constant) & (for function process) to map a function from type io to type io
-- the left is used for the above function


for_constant : Int -> Int -> (constant -> Int -> io -> io) -> constant -> io -> io
for_constant i n f c x =
    if i >= n then
        x

    else
        for_constant (i + 1) n f c (f c i x)


for_nonconstant : Int -> Int -> (Int -> io -> io) -> io -> io
for_nonconstant i n f x =
    if i >= n then
        x

    else
        for_nonconstant (i + 1) n f (f i x)
