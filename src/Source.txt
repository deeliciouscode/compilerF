a = 1;
b = 2;
c = a + b;
f x y z = if x < 10 then let y = y * y in y * z else x / y;
main = f a b c;