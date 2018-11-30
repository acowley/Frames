To build:
```
$ stack build
```
To run:
```
$ stack exec framestack-exe
```
Output:
```
In-core representation prepared
Considering 36558 records...
Average latitude: 28.087046540127773
Average longitude: -81.90380988232442
```

REPL examples:
```
$ stack repl

λ> :l Main

λ> tbl >>= mapM_ print
{name :-> "joe", age :-> 21}
{name :-> "sue", age :-> 23}
{name :-> "bob", age :-> 44}
{name :-> "laura", age :-> 18}

λ> tbl2 >>= mapM_ print
{name :-> "joe", age :-> 21}
{name :-> "sue", age :-> 23}
{name :-> "laura", age :-> 18}

λ> tbl2a >>= mapM_ print
{Just (name :-> "joe"), Just (age :-> 21)}
{Just (name :-> "sue"), Just (age :-> 23)}
{Just (name :-> "bob"), Nothing}
{Just (name :-> "laura"), Just (age :-> 18)}
```
