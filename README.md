# fplox
Free Pascal implementation of clox bytecode virtual machine from the book Crafting Interpreters

Added some of the optimizations:
- nan-boxing
- hash table optimization
- switch statement
- conditional expressions: print x == 5 ? "5" : nil;
- break and continue in loops

also added:

- arrow functions, e.g. 

fun add(a,b) -> a+b
fun sub(a,b) -> a-b
fun mul(a,b) -> a*b
fun div(a,b) -> a/b

print add(32,10);
print sub(52,10);
print mul(6,7);
print div(84,2);

- anonymous functions

// calculates the integral of a function over the interval a..b in n steps

fun integral(f, a, b, n) {
  var sum = 0;
  var dt = (b-a)/n;

  for (var x = 0; x<n; x=x+1) {
    sum = sum + f(a+(x+0.5)*dt);
  }

  return sum*dt;
}

// using arrow notation
print integral(fun(x)->x*x-2*x+4, 0, 1, 10000);
print integral(fun(x)->sqrt(1-x*x), -1, 1, 10000);  // 1/2 PI

- declarations of local var in if-stmt e.g. if (var x=a; x<6) and while-stmt e.g. 

while (var x=0; x<10) {
  x=x-1;
  print x;
}

- ensure statement to ensure a certain condition is true
- native functions e.g. 
print milliseconds();
print pi();
print sqrt(64);
print sqr(8);
print trunc(9.17);
print frac(9.17);
print date();
print time();
print now();
print today();

randomize();
print random();
randLim(20);

print toNum("333");
print length("abcdef");
print toStr(99999);

error();

