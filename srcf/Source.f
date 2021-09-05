-- fibonacci example in haskell:
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)

-- in f:

fib n = if n == 0
        then 0
        else 
            if n == 1 
            then 1 
            else (fib (n - 1)) + (fib (n - 2));

main = fib 8;
