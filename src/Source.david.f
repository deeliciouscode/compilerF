-- a = 1;
-- b = 2;
-- c = a - b;
-- main = if a == 1 then c * 3 else a;

-- a = 1;
-- b = 2;
-- c = a - b;
-- main = c * (-11);

-- k1 a b = b;
-- main = k1 0 1; 

-- fun a b c d = c; 
-- main = fun 1 2 (3 + 2 + 10) 4;

-- main = f 0 1 2; 
-- f a b c = not (8 == (c * c * c)); 
-- k1 a b = b;

-- main = first 0 1 2; 
-- first a b c = second a b; 
-- second a b = b;

-- main = let a = 1 in a * 2;

-- main = k1 0 (k1 1 2); k1 a b = b;

-- main = - (1 + 2);

-- main = id 0; id a = a;

-- main = f 0 1 2; f a b c = c;

-- main = false == (not false);

-- main = true;

-- main = f 0 1 2; f a b c = c;

main = f 0 1 2; f a b c = k1 a b; k1 a b = b;