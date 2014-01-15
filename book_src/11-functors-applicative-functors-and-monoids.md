-   [Functionally Solving Problems](functionally-solving-problems.html)
-   [Table of contents](chapters.html)
-   [A Fistful of Monads](a-fistful-of-monads.html)

Functors, Applicative Functors and Monoids
==========================================

Haskell's combination of purity, higher order functions, parameterized
algebraic data types, and typeclasses allows us to implement
polymorphism on a much higher level than possible in other languages. We
don't have to think about types belonging to a big hierarchy of types.
Instead, we think about what the types can act like and then connect
them with the appropriate typeclasses. An `Int`{.fixed} can act like a
lot of things. It can act like an equatable thing, like an ordered
thing, like an enumerable thing, etc.

Typeclasses are open, which means that we can define our own data type,
think about what it can act like and connect it with the typeclasses
that define its behaviors. Because of that and because of Haskell's
great type system that allows us to know a lot about a function just by
knowing its type declaration, we can define typeclasses that define
behavior that's very general and abstract. We've met typeclasses that
define operations for seeing if two things are equal or comparing two
things by some ordering. Those are very abstract and elegant behaviors,
but we just don't think of them as anything very special because we've
been dealing with them for most of our lives. We recently met functors,
which are basically things that can be mapped over. That's an example of
a useful and yet still pretty abstract property that typeclasses can
describe. In this chapter, we'll take a closer look at functors, along
with slightly stronger and more useful versions of functors called
applicative functors. We'll also take a look at monoids, which are sort
of like socks.

Functors redux
--------------

![frogs dont even need money](lyah/frogtor.png)

We've already talked about functors in [their own little
section](making-our-own-types-and-typeclasses.html#the-functor-typeclass).
If you haven't read it yet, you should probably give it a glance right
now, or maybe later when you have more time. Or you can just pretend you
read it.

Still, here's a quick refresher: Functors are things that can be mapped
over, like lists, `Maybe`{.fixed}s, trees, and such. In Haskell, they're
described by the typeclass `Functor`{.fixed}, which has only one
typeclass method, namely `fmap`{.fixed}, which has a type of
`fmap :: (a -> b) -> f a -> f b`{.fixed}. It says: give me a function
that takes an `a`{.fixed} and returns a `b`{.fixed} and a box with an
`a`{.fixed} (or several of them) inside it and I'll give you a box with
a `b`{.fixed} (or several of them) inside it. It kind of applies the
function to the element inside the box.

*A word of advice.* Many times the box analogy is used to help you get
some intuition for how functors work, and later, we'll probably use the
same analogy for applicative functors and monads. It's an okay analogy
that helps people understand functors at first, just don't take it too
literally, because for some functors the box analogy has to be stretched
really thin to still hold some truth. A more correct term for what a
functor is would be *computational context*. The context might be that
the computation can have a value or it might have failed
(`Maybe`{.fixed} and `Either a`{.fixed}) or that there might be more
values (lists), stuff like that.

If we want to make a type constructor an instance of `Functor`{.fixed},
it has to have a kind of `* -> *`{.fixed}, which means that it has to
take exactly one concrete type as a type parameter. For example,
`Maybe`{.fixed} can be made an instance because it takes one type
parameter to produce a concrete type, like `Maybe Int`{.fixed} or
`Maybe String`{.fixed}. If a type constructor takes two parameters, like
`Either`{.fixed}, we have to partially apply the type constructor until
it only takes one type parameter. So we can't write
`instance Functor Either where`{.fixed}, but we can write
`instance Functor (Either a) where`{.fixed} and then if we imagine that
`fmap`{.fixed} is only for `Either a`{.fixed}, it would have a type
declaration of `fmap :: (b -> c) -> Either a b -> Either a c`{.fixed}.
As you can see, the `Either a`{.fixed} part is fixed, because
`Either a`{.fixed} takes only one type parameter, whereas just
`Either`{.fixed} takes two so
`fmap :: (b -> c) -> Either b -> Either c`{.fixed} wouldn't really make
sense.

We've learned by now how a lot of types (well, type constructors really)
are instances of `Functor`{.fixed}, like `[]`{.fixed}, `Maybe`{.fixed},
`Either a`{.fixed} and a `Tree`{.fixed} type that we made on our own. We
saw how we can map functions over them for great good. In this section,
we'll take a look at two more instances of functor, namely `IO`{.fixed}
and `(->) r`{.fixed}.

If some value has a type of, say, `IO String`{.fixed}, that means that
it's an I/O action that, when performed, will go out into the real world
and get some string for us, which it will yield as a result. We can use
`<-`{.fixed} in *do* syntax to bind that result to a name. We mentioned
that I/O actions are like boxes with little feet that go out and fetch
some value from the outside world for us. We can inspect what they
fetched, but after inspecting, we have to wrap the value back in
`IO`{.fixed}. By thinking about this box with little feet analogy, we
can see how `IO`{.fixed} acts like a functor.

Let's see how `IO`{.fixed} is an instance of `Functor`{.fixed}. When we
`fmap`{.fixed} a function over an I/O action, we want to get back an I/O
action that does the same thing, but has our function applied over its
result value.

~~~~ {.haskell:hs name="code"}
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
~~~~

The result of mapping something over an I/O action will be an I/O
action, so right off the bat we use *do* syntax to glue two actions and
make a new one. In the implementation for `fmap`{.fixed}, we make a new
I/O action that first performs the original I/O action and calls its
result `result`{.fixed}. Then, we do `return (f result)`{.fixed}.
`return`{.fixed} is, as you know, a function that makes an I/O action
that doesn't do anything but only presents something as its result. The
action that a *do* block produces will always have the result value of
its last action. That's why we use return to make an I/O action that
doesn't really do anything, it just presents `f result`{.fixed} as the
result of the new I/O action.

We can play around with it to gain some intuition. It's pretty simple
really. Check out this piece of code:

~~~~ {.haskell:hs name="code"}
main = do line <- getLine 
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"
~~~~

The user is prompted for a line and we give it back to the user, only
reversed. Here's how to rewrite this by using `fmap`{.fixed}:

~~~~ {.haskell:hs name="code"}
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
~~~~

![w00ooOoooOO](lyah/alien.png)

Just like when we `fmap`{.fixed} `reverse`{.fixed} over
`Just "blah"`{.fixed} to get `Just "halb"`{.fixed}, we can
`fmap`{.fixed} `reverse`{.fixed} over `getLine`{.fixed}.
`getLine`{.fixed} is an I/O action that has a type of
`IO String`{.fixed} and mapping `reverse`{.fixed} over it gives us an
I/O action that will go out into the real world and get a line and then
apply `reverse`{.fixed} to its result. Like we can apply a function to
something that's inside a `Maybe`{.fixed} box, we can apply a function
to what's inside an `IO`{.fixed} box, only it has to go out into the
real world to get something. Then when we bind it to a name by using
`<-`{.fixed}, the name will reflect the result that already has
`reverse`{.fixed} applied to it.

The I/O action `fmap (++"!") getLine`{.fixed} behaves just like
`getLine`{.fixed}, only that its result always has `"!"`{.fixed}
appended to it!

If we look at what `fmap`{.fixed}'s type would be if it were limited to
`IO`{.fixed}, it would be `fmap :: (a -> b) -> IO a -> IO b`{.fixed}.
`fmap`{.fixed} takes a function and an I/O action and returns a new I/O
action that's like the old one, except that the function is applied to
its contained result.

If you ever find yourself binding the result of an I/O action to a name,
only to apply a function to that and call that something else, consider
using `fmap`{.fixed}, because it looks prettier. If you want to apply
multiple transformations to some data inside a functor, you can declare
your own function at the top level, make a lambda function or ideally,
use function composition:

~~~~ {.haskell:hs name="code"}
import Data.Char
import Data.List

main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line
~~~~

~~~~ {.plain name="code"}
$ runhaskell fmapping_io.hs
hello there
E-R-E-H-T- -O-L-L-E-H
~~~~

As you probably know, `intersperse '-' . reverse . map toUpper`{.fixed}
is a function that takes a string, maps `toUpper`{.fixed} over it, the
applies `reverse`{.fixed} to that result and then applies
`intersperse '-'`{.fixed} to that result. It's like writing
`(\xs -> intersperse '-' (reverse (map toUpper xs)))`{.fixed}, only
prettier.

Another instance of `Functor`{.fixed} that we've been dealing with all
along but didn't know was a `Functor`{.fixed} is `(->) r`{.fixed}.
You're probably slightly confused now, since what the heck does
`(->) r`{.fixed} mean? The function type `r -> a`{.fixed} can be
rewritten as `(->) r a`{.fixed}, much like we can write `2 + 3`{.fixed}
as `(+) 2 3`{.fixed}. When we look at it as `(->) r a`{.fixed}, we can
see `(->)`{.fixed} in a slighty different light, because we see that
it's just a type constructor that takes two type parameters, just like
`Either`{.fixed}. But remember, we said that a type constructor has to
take exactly one type parameter so that it can be made an instance of
`Functor`{.fixed}. That's why we can't make `(->)`{.fixed} an instance
of `Functor`{.fixed}, but if we partially apply it to `(->) r`{.fixed},
it doesn't pose any problems. If the syntax allowed for type
constructors to be partially applied with sections (like we can
partially apply `+`{.fixed} by doing `(2+)`{.fixed}, which is the same
as `(+) 2`{.fixed}), you could write `(->) r`{.fixed} as
`(r ->)`{.fixed}. How are functions functors? Well, let's take a look at
the implementation, which lies in `Control.Monad.Instances`{.fixed}

We usually mark functions that take anything and return anything as
`a -> b`{.fixed}. `r -> a`{.fixed} is the same thing, we just used
different letters for the type variables.

~~~~ {.haskell:hs name="code"}
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
~~~~

If the syntax allowed for it, it could have been written as

~~~~ {.haskell:hs name="code"}
instance Functor (r ->) where
    fmap f g = (\x -> f (g x))
~~~~

But it doesn't, so we have to write it in the former fashion.

First of all, let's think about `fmap`{.fixed}'s type. It's
`fmap :: (a -> b) -> f a -> f b`{.fixed}. Now what we'll do is mentally
replace all the `f`{.fixed}'s, which are the role that our functor
instance plays, with `(->) r`{.fixed}'s. We'll do that to see how
`fmap`{.fixed} should behave for this particular instance. We get
`fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`{.fixed}. Now what we can
do is write the `(->) r a`{.fixed} and `(-> r b)`{.fixed} types as infix
`r -> a`{.fixed} and `r -> b`{.fixed}, like we normally do with
functions. What we get now is
`fmap :: (a -> b) -> (r -> a) -> (r -> b)`{.fixed}.

Hmmm OK. Mapping one function over a function has to produce a function,
just like mapping a function over a `Maybe`{.fixed} has to produce a
`Maybe`{.fixed} and mapping a function over a list has to produce a
list. What does the type
`fmap :: (a -> b) -> (r -> a) -> (r -> b)`{.fixed} for this instance
tell us? Well, we see that it takes a function from `a`{.fixed} to
`b`{.fixed} and a function from `r`{.fixed} to `a`{.fixed} and returns a
function from `r`{.fixed} to `b`{.fixed}. Does this remind you of
anything? Yes! Function composition! We pipe the output of
`r -> a`{.fixed} into the input of `a -> b`{.fixed} to get a function
`r -> b`{.fixed}, which is exactly what function composition is about.
If you look at how the instance is defined above, you'll see that it's
just function composition. Another way to write this instance would be:

~~~~ {.haskell:hs name="code"}
instance Functor ((->) r) where
    fmap = (.)
~~~~

This makes the revelation that using `fmap`{.fixed} over functions is
just composition sort of obvious. Do
`:m + Control.Monad.Instances`{.fixed}, since that's where the instance
is defined and then try playing with mapping over functions.

~~~~ {.haskell:hs name="code"}
ghci> :t fmap (*3) (+100)
fmap (*3) (+100) :: (Num a) => a -> a
ghci> fmap (*3) (+100) 1
303
ghci> (*3) `fmap` (+100) $ 1
303
ghci> (*3) . (+100) $ 1
303
ghci> fmap (show . (*3)) (*100) 1
"300"
~~~~

We can call `fmap`{.fixed} as an infix function so that the resemblance
to `.`{.fixed} is clear. In the second input line, we're mapping
`(*3)`{.fixed} over `(+100)`{.fixed}, which results in a function that
will take an input, call `(+100)`{.fixed} on that and then call
`(*3)`{.fixed} on that result. We call that function with `1`{.fixed}.

How does the box analogy hold here? Well, if you stretch it, it holds.
When we use `fmap (+3)`{.fixed} over `Just 3`{.fixed}, it's easy to
imagine the `Maybe`{.fixed} as a box that has some contents on which we
apply the function `(+3)`{.fixed}. But what about when we're doing
`fmap (*3) (+100)`{.fixed}? Well, you can think of the function
`(+100)`{.fixed} as a box that contains its eventual result. Sort of
like how an I/O action can be thought of as a box that will go out into
the real world and fetch some result. Using `fmap (*3)`{.fixed} on
`(+100)`{.fixed} will create another function that acts like
`(+100)`{.fixed}, only before producing a result, `(*3)`{.fixed} will be
applied to that result. Now we can see how `fmap`{.fixed} acts just like
`.`{.fixed} for functions.

The fact that `fmap`{.fixed} is function composition when used on
functions isn't so terribly useful right now, but at least it's very
interesting. It also bends our minds a bit and let us see how things
that act more like computations than boxes (`IO`{.fixed} and
`(->) r`{.fixed}) can be functors. The function being mapped over a
computation results in the same computation but the result of that
computation is modified with the function.

![lifting a function is easier than lifting a million
pounds](lyah/lifter.png)

Before we go on to the rules that `fmap`{.fixed} should follow, let's
think about the type of `fmap`{.fixed} once more. Its type is
`fmap :: (a -> b) -> f a -> f b`{.fixed}. We're missing the class
constraint `(Functor f) =>`{.fixed}, but we left it out here for
brevity, because we're talking about functors anyway so we know what the
`f`{.fixed} stands for. When we first learned about [curried
functions](higher-order-functions.html#curried-functions), we said that
all Haskell functions actually take one parameter. A function
`a -> b -> c`{.fixed} actually takes just one parameter of type
`a`{.fixed} and then returns a function `b -> c`{.fixed}, which takes
one parameter and returns a `c`{.fixed}. That's how if we call a
function with too few parameters (i.e. partially apply it), we get back
a function that takes the number of parameters that we left out (if
we're thinking about functions as taking several parameters again). So
`a -> b -> c`{.fixed} can be written as `a -> (b -> c)`{.fixed}, to make
the currying more apparent.

In the same vein, if we write
`fmap :: (a -> b) -> (f a -> f b)`{.fixed}, we can think of
`fmap`{.fixed} not as a function that takes one function and a functor
and returns a functor, but as a function that takes a function and
returns a new function that's just like the old one, only it takes a
functor as a parameter and returns a functor as the result. It takes an
`a -> b`{.fixed} function and returns a function `f a -> f b`{.fixed}.
This is called *lifting* a function. Let's play around with that idea by
using GHCI's `:t`{.fixed} command:

~~~~ {.haskell:hs name="code"}
ghci> :t fmap (*2)
fmap (*2) :: (Num a, Functor f) => f a -> f a
ghci> :t fmap (replicate 3)
fmap (replicate 3) :: (Functor f) => f a -> f [a]
~~~~

The expression `fmap (*2)`{.fixed} is a function that takes a functor
`f`{.fixed} over numbers and returns a functor over numbers. That
functor can be a list, a `Maybe `{.fixed}, an `Either String`{.fixed},
whatever. The expression `fmap (replicate 3)`{.fixed} will take a
functor over any type and return a functor over a list of elements of
that type.

When we say *a functor over numbers*, you can think of that as *a
functor that has numbers in it*. The former is a bit fancier and more
technically correct, but the latter is usually easier to get.

This is even more apparent if we partially apply, say,
`fmap (++"!")`{.fixed} and then bind it to a name in GHCI.

You can think of `fmap`{.fixed} as either a function that takes a
function and a functor and then maps that function over the functor, or
you can think of it as a function that takes a function and lifts that
function so that it operates on functors. Both views are correct and in
Haskell, equivalent.

The type `fmap (replicate 3) :: (Functor f) => f a -> f [a]`{.fixed}
means that the function will work on any functor. What exactly it will
do depends on which functor we use it on. If we use
`fmap (replicate 3)`{.fixed} on a list, the list's implementation for
`fmap`{.fixed} will be chosen, which is just `map`{.fixed}. If we use it
on a `Maybe a`{.fixed}, it'll apply `replicate 3`{.fixed} to the value
inside the `Just`{.fixed}, or if it's `Nothing`{.fixed}, then it stays
`Nothing`{.fixed}.

~~~~ {.haskell:hs name="code"}
ghci> fmap (replicate 3) [1,2,3,4]
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
ghci> fmap (replicate 3) (Just 4)
Just [4,4,4]
ghci> fmap (replicate 3) (Right "blah")
Right ["blah","blah","blah"]
ghci> fmap (replicate 3) Nothing
Nothing
ghci> fmap (replicate 3) (Left "foo")
Left "foo"
~~~~

Next up, we're going to look at the *functor laws*. In order for
something to be a functor, it should satisfy some laws. All functors are
expected to exhibit certain kinds of functor-like properties and
behaviors. They should reliably behave as things that can be mapped
over. Calling `fmap`{.fixed} on a functor should just map a function
over the functor, nothing more. This behavior is described in the
functor laws. There are two of them that all instances of
`Functor`{.fixed} should abide by. They aren't enforced by Haskell
automatically, so you have to test them out yourself.

*The first functor law states that if we map the `id`{.fixed} function
over a functor, the functor that we get back should be the same as the
original functor.* If we write that a bit more formally, it means that
`fmap id = id`{.label .law}. So essentially, this says that if we do
`fmap id`{.fixed} over a functor, it should be the same as just calling
`id`{.fixed} on the functor. Remember, `id`{.fixed} is the identity
function, which just returns its parameter unmodified. It can also be
written as `\x -> x`{.fixed}. If we view the functor as something that
can be mapped over, the `fmap id = id`{.label .law} law seems kind of
trivial or obvious.

Let's see if this law holds for a few values of functors.

~~~~ {.haskell:hs name="code"}
ghci> fmap id (Just 3)
Just 3
ghci> id (Just 3)
Just 3
ghci> fmap id [1..5]
[1,2,3,4,5]
ghci> id [1..5]
[1,2,3,4,5]
ghci> fmap id []
[]
ghci> fmap id Nothing
Nothing
~~~~

If we look at the implementation of `fmap`{.fixed} for, say,
`Maybe`{.fixed}, we can figure out why the first functor law holds.

~~~~ {.haskell:hs name="code"}
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
~~~~

We imagine that `id`{.fixed} plays the role of the `f`{.fixed} parameter
in the implementation. We see that if wee `fmap id`{.fixed} over
`Just x`{.fixed}, the result will be `Just (id x)`{.fixed}, and because
`id`{.fixed} just returns its parameter, we can deduce that
`Just (id x)`{.fixed} equals `Just x`{.fixed}. So now we know that if we
map `id`{.fixed} over a `Maybe`{.fixed} value with a `Just`{.fixed}
value constructor, we get that same value back.

Seeing that mapping `id`{.fixed} over a `Nothing`{.fixed} value returns
the same value is trivial. So from these two equations in the
implementation for `fmap`{.fixed}, we see that the law
`fmap id = id`{.fixed} holds.

![justice is blind, but so is my dog](lyah/justice.png)

*The second law says that composing two functions and then mapping the
resulting function over a functor should be the same as first mapping
one function over the functor and then mapping the other one.* Formally
written, that means that `fmap (f . g) = fmap f . fmap g`{.label .law}.
Or to write it in another way, for any functor *F*, the following should
hold: `fmap (f . g) F = fmap f (fmap g F)`{.label .law}.

If we can show that some type obeys both functor laws, we can rely on it
having the same fundamental behaviors as other functors when it comes to
mapping. We can know that when we use `fmap`{.fixed} on it, there won't
be anything other than mapping going on behind the scenes and that it
will act like a thing that can be mapped over, i.e. a functor. You
figure out how the second law holds for some type by looking at the
implementation of `fmap`{.fixed} for that type and then using the method
that we used to check if `Maybe`{.fixed} obeys the first law.

If you want, we can check out how the second functor law holds for
`Maybe`{.fixed}. If we do `fmap (f . g)`{.fixed} over `Nothing`{.fixed},
we get `Nothing`{.fixed}, because doing a `fmap`{.fixed} with any
function over `Nothing`{.fixed} returns `Nothing`{.fixed}. If we do
`fmap f (fmap g Nothing)`{.fixed}, we get `Nothing`{.fixed}, for the
same reason. OK, seeing how the second law holds for `Maybe`{.fixed} if
it's a `Nothing`{.fixed} value is pretty easy, almost trivial.

How about if it's a `Just something`{.fixed} value? Well, if we do
`fmap (f . g) (Just x)`{.fixed}, we see from the implementation that
it's implemented as `Just ((f . g) x)`{.fixed}, which is, of course,
`Just (f (g x))`{.fixed}. If we do `fmap f (fmap g (Just x))`{.fixed},
we see from the implementation that `fmap g (Just x)`{.fixed} is
`Just (g x)`{.fixed}. Ergo, `fmap f (fmap g (Just x))`{.fixed} equals
`fmap f (Just (g x))`{.fixed} and from the implementation we see that
this equals `Just (f (g x))`{.fixed}.

If you're a bit confused by this proof, don't worry. Be sure that you
understand how [function
composition](higher-order-functions.html#composition) works. Many times,
you can intuitively see how these laws hold because the types act like
containers or functions. You can also just try them on a bunch of
different values of a type and be able to say with some certainty that a
type does indeed obey the laws.

Let's take a look at a pathological example of a type constructor being
an instance of the `Functor`{.fixed} typeclass but not really being a
functor, because it doesn't satisfy the laws. Let's say that we have a
type:

~~~~ {.haskell:hs name="code"}
data CMaybe a = CNothing | CJust Int a deriving (Show)
~~~~

The C here stands for *counter*. It's a data type that looks much like
`Maybe a`{.fixed}, only the `Just`{.fixed} part holds two fields instead
of one. The first field in the `CJust`{.fixed} value constructor will
always have a type of `Int`{.fixed}, and it will be some sort of counter
and the second field is of type `a`{.fixed}, which comes from the type
parameter and its type will, of course, depend on the concrete type that
we choose for `CMaybe a`{.fixed}. Let's play with our new type to get
some intuition for it.

~~~~ {.haskell:hs name="code"}
ghci> CNothing
CNothing
ghci> CJust 0 "haha"
CJust 0 "haha"
ghci> :t CNothing
CNothing :: CMaybe a
ghci> :t CJust 0 "haha"
CJust 0 "haha" :: CMaybe [Char]
ghci> CJust 100 [1,2,3]
CJust 100 [1,2,3]
~~~~

If we use the `CNothing`{.fixed} constructor, there are no fields, and
if we use the `CJust`{.fixed} constructor, the first field is an integer
and the second field can be any type. Let's make this an instance of
`Functor`{.fixed} so that everytime we use `fmap`{.fixed}, the function
gets applied to the second field, whereas the first field gets increased
by 1.

~~~~ {.haskell:hs name="code"}
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)
~~~~

This is kind of like the instance implementation for `Maybe`{.fixed},
except that when we do `fmap`{.fixed} over a value that doesn't
represent an empty box (a `CJust`{.fixed} value), we don't just apply
the function to the contents, we also increase the counter by 1.
Everything seems cool so far, we can even play with this a bit:

~~~~ {.haskell:hs name="code"}
ghci> fmap (++"ha") (CJust 0 "ho")
CJust 1 "hoha"
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
CJust 2 "hohahe"
ghci> fmap (++"blah") CNothing
CNothing
~~~~

Does this obey the functor laws? In order to see that something doesn't
obey a law, it's enough to find just one counter-example.

~~~~ {.haskell:hs name="code"}
ghci> fmap id (CJust 0 "haha")
CJust 1 "haha"
ghci> id (CJust 0 "haha")
CJust 0 "haha"
~~~~

Ah! We know that the first functor law states that if we map
`id`{.fixed} over a functor, it should be the same as just calling
`id`{.fixed} with the same functor, but as we've seen from this example,
this is not true for our `CMaybe`{.fixed} functor. Even though it's part
of the `Functor`{.fixed} typeclass, it doesn't obey the functor laws and
is therefore not a functor. If someone used our `CMaybe`{.fixed} type as
a functor, they would expect it to obey the functor laws like a good
functor. But `CMaybe`{.fixed} fails at being a functor even though it
pretends to be one, so using it as a functor might lead to some faulty
code. When we use a functor, it shouldn't matter if we first compose a
few functions and then map them over the functor or if we just map each
function over a functor in succession. But with `CMaybe`{.fixed}, it
matters, because it keeps track of how many times it's been mapped over.
Not cool! If we wanted `CMaybe`{.fixed} to obey the functor laws, we'd
have to make it so that the `Int`{.fixed} field stays the same when we
use `fmap`{.fixed}.

At first, the functor laws might seem a bit confusing and unnecessary,
but then we see that if we know that a type obeys both laws, we can make
certain assumptions about how it will act. If a type obeys the functor
laws, we know that calling `fmap`{.fixed} on a value of that type will
only map the function over it, nothing more. This leads to code that is
more abstract and extensible, because we can use laws to reason about
behaviors that any functor should have and make functions that operate
reliably on any functor.

All the `Functor`{.fixed} instances in the standard library obey these
laws, but you can check for yourself if you don't believe me. And the
next time you make a type an instance of `Functor`{.fixed}, take a
minute to make sure that it obeys the functor laws. Once you've dealt
with enough functors, you kind of intuitively see the properties and
behaviors that they have in common and it's not hard to intuitively see
if a type obeys the functor laws. But even without the intuition, you
can always just go over the implementation line by line and see if the
laws hold or try to find a counter-example.

We can also look at functors as things that output values in a context.
For instance, `Just 3`{.fixed} outputs the value `3`{.fixed} in the
context that it might or not output any values at all. `[1,2,3]`{.fixed}
outputs three values—`1`{.fixed}, `2`{.fixed}, and `3`{.fixed}, the
context is that there may be multiple values or no values. The function
`(+3)`{.fixed} will output a value, depending on which parameter it is
given.

If you think of functors as things that output values, you can think of
mapping over functors as attaching a transformation to the output of the
functor that changes the value. When we do `fmap (+3) [1,2,3]`{.fixed},
we attach the transformation `(+3)`{.fixed} to the output of
`[1,2,3]`{.fixed}, so whenever we look at a number that the list
outputs, `(+3)`{.fixed} will be applied to it. Another example is
mapping over functions. When we do `fmap (+3) (*3)`{.fixed}, we attach
the transformation `(+3)`{.fixed} to the eventual output of
`(*3)`{.fixed}. Looking at it this way gives us some intuition as to why
using `fmap`{.fixed} on functions is just composition
(`fmap (+3) (*3)`{.fixed} equals `(+3) . (*3)`{.fixed}, which equals
`\x -> ((x*3)+3)`{.fixed}), because we take a function like
`(*3)`{.fixed} then we attach the transformation `(+3)`{.fixed} to its
output. The result is still a function, only when we give it a number,
it will be multiplied by three and then it will go through the attached
transformation where it will be added to three. This is what happens
with composition.

Applicative functors
--------------------

![disregard this analogy](lyah/present.png)

In this section, we'll take a look at applicative functors, which are
beefed up functors, represented in Haskell by the `Applicative`{.fixed}
typeclass, found in the `Control.Applicative`{.fixed} module.

As you know, functions in Haskell are curried by default, which means
that a function that seems to take several parameters actually takes
just one parameter and returns a function that takes the next parameter
and so on. If a function is of type `a -> b -> c`{.fixed}, we usually
say that it takes two parameters and returns a `c`{.fixed}, but actually
it takes an `a`{.fixed} and returns a function `b -> c`{.fixed}. That's
why we can call a function as `f x y`{.fixed} or as `(f x) y`{.fixed}.
This mechanism is what enables us to partially apply functions by just
calling them with too few parameters, which results in functions that we
can then pass on to other functions.

So far, when we were mapping functions over functors, we usually mapped
functions that take only one parameter. But what happens when we map a
function like `*`{.fixed}, which takes two parameters, over a functor?
Let's take a look at a couple of concrete examples of this. If we have
`Just 3`{.fixed} and we do `fmap (*) (Just 3)`{.fixed}, what do we get?
From the instance implementation of `Maybe`{.fixed} for
`Functor`{.fixed}, we know that if it's a `Just something`{.fixed}
value, it will apply the function to the `something`{.fixed} inside the
`Just`{.fixed}. Therefore, doing `fmap (*) (Just 3)`{.fixed} results in
`Just ((*) 3)`{.fixed}, which can also be written as
`Just (* 3)`{.fixed} if we use sections. Interesting! We get a function
wrapped in a `Just`{.fixed}!

~~~~ {.haskell:hs name="code"}
ghci> :t fmap (++) (Just "hey")
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
ghci> :t fmap compare (Just 'a')
fmap compare (Just 'a') :: Maybe (Char -> Ordering)
ghci> :t fmap compare "A LIST OF CHARS"
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]
fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]
~~~~

If we map `compare`{.fixed}, which has a type of
`(Ord a) => a -> a -> Ordering`{.fixed} over a list of characters, we
get a list of functions of type `Char -> Ordering`{.fixed}, because the
function `compare`{.fixed} gets partially applied with the characters in
the list. It's not a list of `(Ord a) => a -> Ordering`{.fixed}
function, because the first `a`{.fixed} that got applied was a
`Char`{.fixed} and so the second `a`{.fixed} has to decide to be of type
`Char`{.fixed}.

We see how by mapping "multi-parameter" functions over functors, we get
functors that contain functions inside them. So now what can we do with
them? Well for one, we can map functions that take these functions as
parameters over them, because whatever is inside a functor will be given
to the function that we're mapping over it as a parameter.

~~~~ {.haskell:hs name="code"}
ghci> let a = fmap (*) [1,2,3,4]
ghci> :t a
a :: [Integer -> Integer]
ghci> fmap (\f -> f 9) a
[9,18,27,36]
~~~~

But what if we have a functor value of `Just (3 *)`{.fixed} and a
functor value of `Just 5`{.fixed} and we want to take out the function
from `Just (3 *)`{.fixed} and map it over `Just 5`{.fixed}? With normal
functors, we're out of luck, because all they support is just mapping
normal functions over existing functors. Even when we mapped
`\f -> f 9`{.fixed} over a functor that contained functions inside it,
we were just mapping a normal function over it. But we can't map a
function that's inside a functor over another functor with what
`fmap`{.fixed} offers us. We could pattern-match against the
`Just`{.fixed} constructor to get the function out of it and then map it
over `Just 5`{.fixed}, but we're looking for a more general and abstract
way of doing that, which works across functors.

Meet the `Applicative`{.fixed} typeclass. It lies in the
`Control.Applicative`{.fixed} module and it defines two methods,
`pure`{.fixed} and `<*>`{.fixed}. It doesn't provide a default
implementation for any of them, so we have to define them both if we
want something to be an applicative functor. The class is defined like
so:

~~~~ {.haskell:hs name="code"}
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
~~~~

This simple three line class definition tells us a lot! Let's start at
the first line. It starts the definition of the `Applicative`{.fixed}
class and it also introduces a class constraint. It says that if we want
to make a type constructor part of the `Applicative`{.fixed} typeclass,
it has to be in `Functor`{.fixed} first. That's why if we know that if a
type constructor is part of the `Applicative`{.fixed} typeclass, it's
also in `Functor`{.fixed}, so we can use `fmap`{.fixed} on it.

The first method it defines is called `pure`{.fixed}. Its type
declaration is `pure :: a -> f a`{.fixed}. `f`{.fixed} plays the role of
our applicative functor instance here. Because Haskell has a very good
type system and because everything a function can do is take some
parameters and return some value, we can tell a lot from a type
declaration and this is no exception. `pure`{.fixed} should take a value
of any type and return an applicative functor with that value inside it.
When we say *inside it*, we're using the box analogy again, even though
we've seen that it doesn't always stand up to scrutiny. But the
`a -> f a`{.fixed} type declaration is still pretty descriptive. We take
a value and we wrap it in an applicative functor that has that value as
the result inside it.

A better way of thinking about `pure`{.fixed} would be to say that it
takes a value and puts it in some sort of default (or pure) context—a
minimal context that still yields that value.

The `<*>`{.fixed} function is really interesting. It has a type
declaration of `f (a -> b) -> f a -> f b`{.fixed}. Does this remind you
of anything? Of course, `fmap :: (a -> b) -> f a -> f b`{.fixed}. It's a
sort of a beefed up `fmap`{.fixed}. Whereas `fmap`{.fixed} takes a
function and a functor and applies the function inside the functor,
`<*>`{.fixed} takes a functor that has a function in it and another
functor and sort of extracts that function from the first functor and
then maps it over the second one. When I say *extract*, I actually sort
of mean *run* and then extract, maybe even *sequence*. We'll see why
soon.

Let's take a look at the `Applicative`{.fixed} instance implementation
for `Maybe`{.fixed}.

~~~~ {.haskell:hs name="code"}
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
~~~~

Again, from the class definition we see that the `f`{.fixed} that plays
the role of the applicative functor should take one concrete type as a
parameter, so we write `instance Applicative Maybe where`{.fixed}
instead of writing `instance Applicative (Maybe a) where`{.fixed}.

First off, `pure`{.fixed}. We said earlier that it's supposed to take
something and wrap it in an applicative functor. We wrote
`pure = Just`{.fixed}, because value constructors like `Just`{.fixed}
are normal functions. We could have also written
`pure x = Just x`{.fixed}.

Next up, we have the definition for `<*>`{.fixed}. We can't extract a
function out of a `Nothing`{.fixed}, because it has no function inside
it. So we say that if we try to extract a function from a
`Nothing`{.fixed}, the result is a `Nothing`{.fixed}. If you look at the
class definition for `Applicative`{.fixed}, you'll see that there's a
`Functor`{.fixed} class constraint, which means that we can assume that
both of `<*>`{.fixed}'s parameters are functors. If the first parameter
is not a `Nothing`{.fixed}, but a `Just`{.fixed} with some function
inside it, we say that we then want to map that function over the second
parameter. This also takes care of the case where the second parameter
is `Nothing`{.fixed}, because doing `fmap`{.fixed} with any function
over a `Nothing`{.fixed} will return a `Nothing`{.fixed}.

So for `Maybe`{.fixed}, `<*>`{.fixed} extracts the function from the
left value if it's a `Just`{.fixed} and maps it over the right value. If
any of the parameters is `Nothing`{.fixed}, `Nothing`{.fixed} is the
result.

OK cool great. Let's give this a whirl.

~~~~ {.haskell:hs name="code"}
ghci> Just (+3) <*> Just 9
Just 12
ghci> pure (+3) <*> Just 10
Just 13
ghci> pure (+3) <*> Just 9
Just 12
ghci> Just (++"hahah") <*> Nothing
Nothing
ghci> Nothing <*> Just "woot"
Nothing
~~~~

We see how doing `pure (+3)`{.fixed} and `Just (+3)`{.fixed} is the same
in this case. Use `pure`{.fixed} if you're dealing with `Maybe`{.fixed}
values in an applicative context (i.e. using them with `<*>`{.fixed}),
otherwise stick to `Just`{.fixed}. The first four input lines
demonstrate how the function is extracted and then mapped, but in this
case, they could have been achieved by just mapping unwrapped functions
over functors. The last line is interesting, because we try to extract a
function from a `Nothing`{.fixed} and then map it over something, which
of course results in a `Nothing`{.fixed}.

With normal functors, you can just map a function over a functor and
then you can't get the result out in any general way, even if the result
is a partially applied function. Applicative functors, on the other
hand, allow you to operate on several functors with a single function.
Check out this piece of code:

~~~~ {.haskell:hs name="code"}
ghci> pure (+) <*> Just 3 <*> Just 5
Just 8
ghci> pure (+) <*> Just 3 <*> Nothing
Nothing
ghci> pure (+) <*> Nothing <*> Just 5
Nothing
~~~~

![whaale](lyah/whale.png)

What's going on here? Let's take a look, step by step. `<*>`{.fixed} is
left-associative, which means that
`pure (+) <*> Just 3 <*> Just 5`{.fixed} is the same as
`(pure (+) <*> Just 3) <*> Just 5`{.fixed}. First, the `+`{.fixed}
function is put in a functor, which is in this case a `Maybe`{.fixed}
value that contains the function. So at first, we have
`pure (+)`{.fixed}, which is `Just (+)`{.fixed}. Next,
`Just (+) <*> Just 3`{.fixed} happens. The result of this is
`Just (3+)`{.fixed}. This is because of partial application. Only
applying `3`{.fixed} to the `+`{.fixed} function results in a function
that takes one parameter and adds 3 to it. Finally,
`Just (3+) <*> Just 5`{.fixed} is carried out, which results in a
`Just 8`{.fixed}.

Isn't this awesome?! Applicative functors and the applicative style of
doing `pure f <*> x <*> y <*> ...`{.fixed} allow us to take a function
that expects parameters that aren't necessarily wrapped in functors and
use that function to operate on several values that are in functor
contexts. The function can take as many parameters as we want, because
it's always partially applied step by step between occurences of
`<*>`{.fixed}.

This becomes even more handy and apparent if we consider the fact that
`pure f <*> x`{.fixed} equals `fmap f x`{.fixed}. This is one of the
applicative laws. We'll take a closer look at them later, but for now,
we can sort of intuitively see that this is so. Think about it, it makes
sense. Like we said before, `pure`{.fixed} puts a value in a default
context. If we just put a function in a default context and then extract
and apply it to a value inside another applicative functor, we did the
same as just mapping that function over that applicative functor.
Instead of writing `pure f <*> x <*> y <*> ...`{.fixed}, we can write
`fmap f x <*> y <*> ...`{.fixed}. This is why
`Control.Applicative`{.fixed} exports a function called `<$>`{.fixed},
which is just `fmap`{.fixed} as an infix operator. Here's how it's
defined:

~~~~ {.haskell:hs name="code"}
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
~~~~

*Yo!* Quick reminder: type variables are independent of parameter names
or other value names. The `f`{.fixed} in the function declaration here
is a type variable with a class constraint saying that any type
constructor that replaces `f`{.fixed} should be in the `Functor`{.fixed}
typeclass. The `f`{.fixed} in the function body denotes a function that
we map over `x`{.fixed}. The fact that we used `f`{.fixed} to represent
both of those doesn't mean that they somehow represent the same thing.

By using `<$>`{.fixed}, the applicative style really shines, because now
if we want to apply a function `f`{.fixed} between three applicative
functors, we can write `f <$> x <*> y <*> z`{.fixed}. If the parameters
weren't applicative functors but normal values, we'd write
`f x y z`{.fixed}.

Let's take a closer look at how this works. We have a value of
`Just "johntra"`{.fixed} and a value of `Just "volta"`{.fixed} and we
want to join them into one `String`{.fixed} inside a `Maybe`{.fixed}
functor. We do this:

~~~~ {.haskell:hs name="code"}
ghci> (++) <$> Just "johntra" <*> Just "volta"
Just "johntravolta"
~~~~

Before we see how this happens, compare the above line with this:

~~~~ {.haskell:hs name="code"}
ghci> (++) "johntra" "volta"
"johntravolta"
~~~~

Awesome! To use a normal function on applicative functors, just sprinkle
some `<$>`{.fixed} and `<*>`{.fixed} about and the function will operate
on applicatives and return an applicative. How cool is that?

Anyway, when we do `(++) <$> Just "johntra" <*> Just "volta"`{.fixed},
first `(++)`{.fixed}, which has a type of
`(++) :: [a] -> [a] -> [a]`{.fixed} gets mapped over
`Just "johntra"`{.fixed}, resulting in a value that's the same as
`Just ("johntra"++)`{.fixed} and has a type of
`Maybe ([Char] -> [Char])`{.fixed}. Notice how the first parameter of
`(++)`{.fixed} got eaten up and how the `a`{.fixed}s turned into
`Char`{.fixed}s. And now `Just ("johntra"++) <*> Just "volta"`{.fixed}
happens, which takes the function out of the `Just`{.fixed} and maps it
over `Just "volta"`{.fixed}, resulting in `Just "johntravolta"`{.fixed}.
Had any of the two values been `Nothing`{.fixed}, the result would have
also been `Nothing`{.fixed}.

So far, we've only used `Maybe`{.fixed} in our examples and you might be
thinking that applicative functors are all about `Maybe`{.fixed}. There
are loads of other instances of `Applicative`{.fixed}, so let's go and
meet them!

Lists (actually the list type constructor, `[]`{.fixed}) are applicative
functors. What a suprise! Here's how `[]`{.fixed} is an instance of
`Applicative`{.fixed}:

~~~~ {.haskell:hs name="code"}
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
~~~~

Earlier, we said that `pure`{.fixed} takes a value and puts it in a
default context. Or in other words, a minimal context that still yields
that value. The minimal context for lists would be the empty list,
`[]`{.fixed}, but the empty list represents the lack of a value, so it
can't hold in itself the value that we used `pure`{.fixed} on. That's
why `pure`{.fixed} takes a value and puts it in a singleton list.
Similarly, the minimal context for the `Maybe`{.fixed} applicative
functor would be a `Nothing`{.fixed}, but it represents the lack of a
value instead of a value, so `pure`{.fixed} is implemented as
`Just`{.fixed} in the instance implementation for `Maybe`{.fixed}.

~~~~ {.haskell:hs name="code"}
ghci> pure "Hey" :: [String]
["Hey"]
ghci> pure "Hey" :: Maybe String
Just "Hey"
~~~~

What about `<*>`{.fixed}? If we look at what `<*>`{.fixed}'s type would
be if it were limited only to lists, we get
`(<*>) :: [a -> b] -> [a] -> [b]`{.fixed}. It's implemented with a [list
comprehension](starting-out.html#im-a-list-comprehension). `<*>`{.fixed}
has to somehow extract the function out of its left parameter and then
map it over the right parameter. But the thing here is that the left
list can have zero functions, one function, or several functions inside
it. The right list can also hold several values. That's why we use a
list comprehension to draw from both lists. We apply every possible
function from the left list to every possible value from the right list.
The resulting list has every possible combination of applying a function
from the left list to a value in the right one.

~~~~ {.haskell:hs name="code"}
ghci> [(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]
~~~~

The left list has three functions and the right list has three values,
so the resulting list will have nine elements. Every function in the
left list is applied to every function in the right one. If we have a
list of functions that take two parameters, we can apply those functions
between two lists.

~~~~ {.haskell:hs name="code"}
ghci> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
~~~~

Because `<*>`{.fixed} is left-associative, `[(+),(*)] <*> [1,2]`{.fixed}
happens first, resulting in a list that's the same as
`[(1+),(2+),(1*),(2*)]`{.fixed}, because every function on the left gets
applied to every value on the right. Then,
`[(1+),(2+),(1*),(2*)] <*> [3,4]`{.fixed} happens, which produces the
final result.

Using the applicative style with lists is fun! Watch:

~~~~ {.haskell:hs name="code"}
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
~~~~

Again, see how we used a normal function that takes two strings between
two applicative functors of strings just by inserting the appropriate
applicative operators.

You can view lists as non-deterministic computations. A value like
`100`{.fixed} or `"what"`{.fixed} can be viewed as a deterministic
computation that has only one result, whereas a list like
`[1,2,3]`{.fixed} can be viewed as a computation that can't decide on
which result it wants to have, so it presents us with all of the
possible results. So when you do something like
`(+) <$> [1,2,3] <*> [4,5,6]`{.fixed}, you can think of it as adding
together two non-deterministic computations with `+`{.fixed}, only to
produce another non-deterministic computation that's even less sure
about its result.

Using the applicative style on lists is often a good replacement for
list comprehensions. In the second chapter, we wanted to see all the
possible products of `[2,5,10]`{.fixed} and `[8,10,11]`{.fixed}, so we
did this:

~~~~ {.haskell:hs name="code"}
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]   
[16,20,22,40,50,55,80,100,110]   
~~~~

We're just drawing from two lists and applying a function between every
combination of elements. This can be done in the applicative style as
well:

~~~~ {.haskell:hs name="code"}
ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]
~~~~

This seems clearer to me, because it's easier to see that we're just
calling `*`{.fixed} between two non-deterministic computations. If we
wanted all possible products of those two lists that are more than 50,
we'd just do:

~~~~ {.haskell:hs name="code"}
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
[55,80,100,110]
~~~~

It's easy to see how `pure f <*> xs`{.fixed} equals `fmap f xs`{.fixed}
with lists. `pure f`{.fixed} is just `[f]`{.fixed} and
`[f] <*> xs`{.fixed} will apply every function in the left list to every
value in the right one, but there's just one function in the left list,
so it's like mapping.

Another instance of `Applicative`{.fixed} that we've already encountered
is `IO`{.fixed}. This is how the instance is implemented:

~~~~ {.haskell:hs name="code"}
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
~~~~

![ahahahah!](lyah/knight.png)

Since `pure`{.fixed} is all about putting a value in a minimal context
that still holds it as its result, it makes sense that `pure`{.fixed} is
just `return`{.fixed}, because `return`{.fixed} does exactly that; it
makes an I/O action that doesn't do anything, it just yields some value
as its result, but it doesn't really do any I/O operations like printing
to the terminal or reading from a file.

If `<*>`{.fixed} were specialized for `IO`{.fixed} it would have a type
of `(<*>) :: IO (a -> b) -> IO a -> IO b`{.fixed}. It would take an I/O
action that yields a function as its result and another I/O action and
create a new I/O action from those two that, when performed, first
performs the first one to get the function and then performs the second
one to get the value and then it would yield that function applied to
the value as its result. We used *do* syntax to implement it here.
Remember, *do* syntax is about taking several I/O actions and gluing
them into one, which is exactly what we do here.

With `Maybe`{.fixed} and `[]`{.fixed}, we could think of `<*>`{.fixed}
as simply extracting a function from its left parameter and then sort of
applying it over the right one. With `IO`{.fixed}, extracting is still
in the game, but now we also have a notion of *sequencing*, because
we're taking two I/O actions and we're sequencing, or gluing, them into
one. We have to extract the function from the first I/O action, but to
extract a result from an I/O action, it has to be performed.

Consider this:

~~~~ {.haskell:hs name="code"}
myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b
~~~~

This is an I/O action that will prompt the user for two lines and yield
as its result those two lines concatenated. We achieved it by gluing
together two `getLine`{.fixed} I/O actions and a `return`{.fixed},
because we wanted our new glued I/O action to hold the result of
`a ++ b`{.fixed}. Another way of writing this would be to use the
applicative style.

~~~~ {.haskell:hs name="code"}
myAction :: IO String
myAction = (++) <$> getLine <*> getLine
~~~~

What we were doing before was making an I/O action that applied a
function between the results of two other I/O actions, and this is the
same thing. Remember, `getLine`{.fixed} is an I/O action with the type
`getLine :: IO String`{.fixed}. When we use `<*>`{.fixed} between two
applicative functors, the result is an applicative functor, so this all
makes sense.

If we regress to the box analogy, we can imagine `getLine`{.fixed} as a
box that will go out into the real world and fetch us a string. Doing
`(++) <$> getLine <*> getLine`{.fixed} makes a new, bigger box that
sends those two boxes out to fetch lines from the terminal and then
presents the concatenation of those two lines as its result.

The type of the expression `(++) <$> getLine <*> getLine`{.fixed} is
`IO String`{.fixed}, which means that this expression is a completely
normal I/O action like any other, which also holds a result value inside
it, just like other I/O actions. That's why we can do stuff like:

~~~~ {.haskell:hs name="code"}
main = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a
~~~~

If you ever find yourself binding some I/O actions to names and then
calling some function on them and presenting that as the result by using
`return`{.fixed}, consider using the applicative style because it's
arguably a bit more concise and terse.

Another instance of `Applicative`{.fixed} is `(->) r`{.fixed}, so
functions. They are rarely used with the applicative style outside of
code golf, but they're still interesting as applicatives, so let's take
a look at how the function instance is implemented.

If you're confused about what `(->) r`{.fixed} means, check out the
previous section where we explain how `(->) r`{.fixed} is a functor.

~~~~ {.haskell:hs name="code"}
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
~~~~

When we wrap a value into an applicative functor with `pure`{.fixed},
the result it yields always has to be that value. A minimal default
context that still yields that value as a result. That's why in the
function instance implementation, `pure`{.fixed} takes a value and
creates a function that ignores its parameter and always returns that
value. If we look at the type for `pure`{.fixed}, but specialized for
the `(->) r`{.fixed} instance, it's `pure :: a -> (r -> a)`{.fixed}.

~~~~ {.haskell:hs name="code"}
ghci> (pure 3) "blah"
3
~~~~

Because of currying, function application is left-associative, so we can
omit the parentheses.

~~~~ {.haskell:hs name="code"}
ghci> pure 3 "blah"
3
~~~~

The instance implementation for `<*>`{.fixed} is a bit cryptic, so it's
best if we just take a look at how to use functions as applicative
functors in the applicative style.

~~~~ {.haskell:hs name="code"}
ghci> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a
ghci> (+) <$> (+3) <*> (*100) $ 5
508
~~~~

Calling `<*>`{.fixed} with two applicative functors results in an
applicative functor, so if we use it on two functions, we get back a
function. So what goes on here? When we do
`(+) <$> (+3) <*> (*100)`{.fixed}, we're making a function that will use
`+`{.fixed} on the results of `(+3)`{.fixed} and `(*100)`{.fixed} and
return that. To demonstrate on a real example, when we did
`(+) <$> (+3) <*> (*100) $ 5`{.fixed}, the `5`{.fixed} first got applied
to `(+3)`{.fixed} and `(*100)`{.fixed}, resulting in `8`{.fixed} and
`500`{.fixed}. Then, `+`{.fixed} gets called with `8`{.fixed} and
`500`{.fixed}, resulting in `508`{.fixed}.

~~~~ {.haskell:hs name="code"}
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
~~~~

![SLAP](lyah/jazzb.png)

Same here. We create a function that will call the function
`\x y z -> [x,y,z]`{.fixed} with the eventual results from
`(+3)`{.fixed}, `(*2)`{.fixed} and `(/2)`{.fixed}. The `5`{.fixed} gets
fed to each of the three functions and then
`\x y z -> [x, y, z]`{.fixed} gets called with those results.

You can think of functions as boxes that contain their eventual results,
so doing `k <$> f <*> g`{.fixed} creates a function that will call
`k`{.fixed} with the eventual results from `f`{.fixed} and `g`{.fixed}.
When we do something like `(+) <$> Just 3 <*> Just 5`{.fixed}, we're
using `+`{.fixed} on values that might or might not be there, which also
results in a value that might or might not be there. When we do
`(+) <$> (+10) <*> (+5)`{.fixed}, we're using `+`{.fixed} on the future
return values of `(+10)`{.fixed} and `(+5)`{.fixed} and the result is
also something that will produce a value only when called with a
parameter.

We don't often use functions as applicatives, but this is still really
interesting. It's not very important that you get how the
`(->) r`{.fixed} instance for `Applicative`{.fixed} works, so don't
despair if you're not getting this right now. Try playing with the
applicative style and functions to build up an intuition for functions
as applicatives.

An instance of `Applicative`{.fixed} that we haven't encountered yet is
`ZipList`{.fixed}, and it lives in `Control.Applicative`{.fixed}.

It turns out there are actually more ways for lists to be applicative
functors. One way is the one we already covered, which says that calling
`<*>`{.fixed} with a list of functions and a list of values results in a
list which has all the possible combinations of applying functions from
the left list to the values in the right list. If we do
`[(+3),(*2)] <*> [1,2]`{.fixed}, `(+3)`{.fixed} will be applied to both
`1`{.fixed} and `2`{.fixed} and `(*2)`{.fixed} will also be applied to
both `1`{.fixed} and `2`{.fixed}, resulting in a list that has four
elements, namely `[4,5,2,4]`{.fixed}.

However, `[(+3),(*2)] <*> [1,2]`{.fixed} could also work in such a way
that the first function in the left list gets applied to the first value
in the right one, the second function gets applied to the second value,
and so on. That would result in a list with two values, namely
`[4,4]`{.fixed}. You could look at it as `[1 + 3, 2 * 2]`{.fixed}.

Because one type can't have two instances for the same typeclass, the
`ZipList a`{.fixed} type was introduced, which has one constructor
`ZipList`{.fixed} that has just one field, and that field is a list.
Here's the instance:

~~~~ {.haskell:hs name="code"}
instance Applicative ZipList where
        pure x = ZipList (repeat x)
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
~~~~

`<*>`{.fixed} does just what we said. It applies the first function to
the first value, the second function to the second value, etc. This is
done with `zipWith (\f x -> f x) fs xs`{.fixed}. Because of how
`zipWith`{.fixed} works, the resulting list will be as long as the
shorter of the two lists.

`pure`{.fixed} is also interesting here. It takes a value and puts it in
a list that just has that value repeating indefinitely.
`pure "haha"`{.fixed} results in
`ZipList (["haha","haha","haha"...`{.fixed}. This might be a bit
confusing since we said that `pure`{.fixed} should put a value in a
minimal context that still yields that value. And you might be thinking
that an infinite list of something is hardly minimal. But it makes sense
with zip lists, because it has to produce the value on every position.
This also satisfies the law that `pure f <*> xs`{.fixed} should equal
`fmap f xs`{.fixed}. If `pure 3`{.fixed} just returned
`ZipList [3]`{.fixed}, `pure (*2) <*> ZipList [1,5,10]`{.fixed} would
result in `ZipList [2]`{.fixed}, because the resulting list of two
zipped lists has the length of the shorter of the two. If we zip a
finite list with an infinite list, the length of the resulting list will
always be equal to the length of the finite list.

So how do zip lists work in an applicative style? Let's see. Oh, the
`ZipList a`{.fixed} type doesn't have a `Show`{.fixed} instance, so we
have to use the `getZipList`{.label .function} function to extract a raw
list out of a zip list.

~~~~ {.haskell:hs name="code"}
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
[101,102,103]
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
[101,102,103]
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
[5,3,3,4]
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
[('d','c','r'),('o','a','a'),('g','t','t')]
~~~~

The `(,,)`{.fixed} function is the same as `\x y z -> (x,y,z)`{.fixed}.
Also, the `(,)`{.fixed} function is the same as `\x y -> (x,y)`{.fixed}.

Aside from `zipWith`{.fixed}, the standard library has functions such as
`zipWith3`{.fixed}, `zipWith4`{.fixed}, all the way up to 7.
`zipWith`{.fixed} takes a function that takes two parameters and zips
two lists with it. `zipWith3`{.fixed} takes a function that takes three
parameters and zips three lists with it, and so on. By using zip lists
with an applicative style, we don't have to have a separate zip function
for each number of lists that we want to zip together. We just use the
applicative style to zip together an arbitrary amount of lists with a
function, and that's pretty cool.

`Control.Applicative`{.fixed} defines a function that's called
`liftA2`{.label .function}, which has a type of
`liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c`{.fixed}
. It's defined like this:

~~~~ {.haskell:hs name="code"}
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
~~~~

Nothing special, it just applies a function between two applicatives,
hiding the applicative style that we've become familiar with. The reason
we're looking at it is because it clearly showcases why applicative
functors are more powerful than just ordinary functors. With ordinary
functors, we can just map functions over one functor. But with
applicative functors, we can apply a function between several functors.
It's also interesting to look at this function's type as
`(a -> b -> c) -> (f a -> f b -> f c)`{.fixed}. When we look at it like
this, we can say that `liftA2`{.fixed} takes a normal binary function
and promotes it to a function that operates on two functors.

Here's an interesting concept: we can take two applicative functors and
combine them into one applicative functor that has inside it the results
of those two applicative functors in a list. For instance, we have
`Just 3`{.fixed} and `Just 4`{.fixed}. Let's assume that the second one
has a singleton list inside it, because that's really easy to achieve:

~~~~ {.haskell:hs name="code"}
ghci> fmap (\x -> [x]) (Just 4)
Just [4]
~~~~

OK, so let's say we have `Just 3`{.fixed} and `Just [4]`{.fixed}. How do
we get `Just [3,4]`{.fixed}? Easy.

~~~~ {.haskell:hs name="code"}
ghci> liftA2 (:) (Just 3) (Just [4])
Just [3,4]
ghci> (:) <$> Just 3 <*> Just [4]
Just [3,4]
~~~~

Remember, `:`{.fixed} is a function that takes an element and a list and
returns a new list with that element at the beginning. Now that we have
`Just [3,4]`{.fixed}, could we combine that with `Just 2`{.fixed} to
produce `Just [2,3,4]`{.fixed}? Of course we could. It seems that we can
combine any amount of applicatives into one applicative that has a list
of the results of those applicatives inside it. Let's try implementing a
function that takes a list of applicatives and returns an applicative
that has a list as its result value. We'll call it `sequenceA`{.fixed}.

~~~~ {.haskell:hs name="code"}
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
~~~~

Ah, recursion! First, we look at the type. It will transform a list of
applicatives into an applicative with a list. From that, we can lay some
groundwork for an edge condition. If we want to turn an empty list into
an applicative with a list of results, well, we just put an empty list
in a default context. Now comes the recursion. If we have a list with a
head and a tail (remember, `x`{.fixed} is an applicative and
`xs`{.fixed} is a list of them), we call `sequenceA`{.fixed} on the
tail, which results in an applicative with a list. Then, we just prepend
the value inside the applicative `x`{.fixed} into that applicative with
a list, and that's it!

So if we do `sequenceA [Just 1, Just 2]`{.fixed}, that's
`(:) <$> Just 1 <*> sequenceA [Just 2] `{.fixed}. That equals
`(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])`{.fixed}. Ah! We
know that `sequenceA []`{.fixed} ends up as being `Just []`{.fixed}, so
this expression is now
`(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])`{.fixed}, which is
`(:) <$> Just 1 <*> Just [2]`{.fixed}, which is `Just [1,2]`{.fixed}!

Another way to implement `sequenceA`{.fixed} is with a fold. Remember,
pretty much any function where we go over a list element by element and
accumulate a result along the way can be implemented with a fold.

~~~~ {.haskell:hs name="code"}
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
~~~~

We approach the list from the right and start off with an accumulator
value of `pure []`{.fixed}. We do `liftA2 (:)`{.fixed} between the
accumulator and the last element of the list, which results in an
applicative that has a singleton in it. Then we do `liftA2 (:)`{.fixed}
with the now last element and the current accumulator and so on, until
we're left with just the accumulator, which holds a list of the results
of all the applicatives.

Let's give our function a whirl on some applicatives.

~~~~ {.haskell:hs name="code"}
ghci> sequenceA [Just 3, Just 2, Just 1]
Just [3,2,1]
ghci> sequenceA [Just 3, Nothing, Just 1]
Nothing
ghci> sequenceA [(+3),(+2),(+1)] 3
[6,5,4]
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
[]
~~~~

Ah! Pretty cool. When used on `Maybe`{.fixed} values,
`sequenceA`{.fixed} creates a `Maybe`{.fixed} value with all the results
inside it as a list. If one of the values was `Nothing`{.fixed}, then
the result is also a `Nothing`{.fixed}. This is cool when you have a
list of `Maybe`{.fixed} values and you're interested in the values only
if none of them is a `Nothing`{.fixed}.

When used with functions, `sequenceA`{.fixed} takes a list of functions
and returns a function that returns a list. In our example, we made a
function that took a number as a parameter and applied it to each
function in the list and then returned a list of results.
`sequenceA [(+3),(+2),(+1)] 3`{.fixed} will call `(+3)`{.fixed} with
`3`{.fixed}, `(+2)`{.fixed} with `3`{.fixed} and `(+1)`{.fixed} with
`3`{.fixed} and present all those results as a list.

Doing `(+) <$> (+3) <*> (*2)`{.fixed} will create a function that takes
a parameter, feeds it to both `(+3)`{.fixed} and `(*2)`{.fixed} and then
calls `+`{.fixed} with those two results. In the same vein, it makes
sense that `sequenceA [(+3),(*2)]`{.fixed} makes a function that takes a
parameter and feeds it to all of the functions in the list. Instead of
calling `+`{.fixed} with the results of the functions, a combination of
`:`{.fixed} and `pure []`{.fixed} is used to gather those results in a
list, which is the result of that function.

Using `sequenceA`{.fixed} is cool when we have a list of functions and
we want to feed the same input to all of them and then view the list of
results. For instance, we have a number and we're wondering whether it
satisfies all of the predicates in a list. One way to do that would be
like so:

~~~~ {.haskell:hs name="code"}
ghci> map (\f -> f 7) [(>4),(<10),odd]
[True,True,True]
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
True
~~~~

Remember, `and`{.fixed} takes a list of booleans and returns
`True`{.fixed} if they're all `True`{.fixed}. Another way to achieve the
same thing would be with `sequenceA`{.fixed}:

~~~~ {.haskell:hs name="code"}
ghci> sequenceA [(>4),(<10),odd] 7
[True,True,True]
ghci> and $ sequenceA [(>4),(<10),odd] 7
True
~~~~

`sequenceA [(>4),(<10),odd]`{.fixed} creates a function that will take a
number and feed it to all of the predicates in
`[(>4),(<10),odd]`{.fixed} and return a list of booleans. It turns a
list with the type `(Num a) => [a -> Bool]`{.fixed} into a function with
the type `(Num a) => a -> [Bool]`{.fixed}. Pretty neat, huh?

Because lists are homogenous, all the functions in the list have to be
functions of the same type, of course. You can't have a list like
`[ord, (+3)]`{.fixed}, because `ord`{.fixed} takes a character and
returns a number, whereas `(+3)`{.fixed} takes a number and returns a
number.

When used with `[]`{.fixed}, `sequenceA`{.fixed} takes a list of lists
and returns a list of lists. Hmm, interesting. It actually creates lists
that have all possible combinations of their elements. For illustration,
here's the above done with `sequenceA`{.fixed} and then done with a list
comprehension:

~~~~ {.haskell:hs name="code"}
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2],[3,4]]
[[1,3],[1,4],[2,3],[2,4]]
ghci> [[x,y] | x <- [1,2], y <- [3,4]]
[[1,3],[1,4],[2,3],[2,4]]
ghci> sequenceA [[1,2],[3,4],[5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
ghci> [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
~~~~

This might be a bit hard to grasp, but if you play with it for a while,
you'll see how it works. Let's say that we're doing
`sequenceA [[1,2],[3,4]]`{.fixed}. To see how this happens, let's use
the `sequenceA (x:xs) = (:) <$> x <*> sequenceA xs`{.fixed} definition
of `sequenceA`{.fixed} and the edge condition
`sequenceA [] = pure []`{.fixed}. You don't have to follow this
evaluation, but it might help you if have trouble imagining how
`sequenceA`{.fixed} works on lists of lists, because it can be a bit
mind-bending.

-   We start off with `sequenceA [[1,2],[3,4]]`{.fixed}
-   That evaluates to `(:) <$> [1,2] <*> sequenceA [[3,4]]`{.fixed}
-   Evaluating the inner `sequenceA`{.fixed} further, we get
    `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])`{.fixed}
-   We've reached the edge condition, so this is now
    `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])`{.fixed}
-   Now, we evaluate the `(:) <$> [3,4] <*> [[]]`{.fixed} part, which
    will use `:`{.fixed} with every possible value in the left list
    (possible values are `3`{.fixed} and `4`{.fixed}) with every
    possible value on the right list (only possible value is
    `[]`{.fixed}), which results in `[3:[], 4:[]]`{.fixed}, which is
    `[[3],[4]]`{.fixed}. So now we have
    `(:) <$> [1,2] <*> [[3],[4]]`{.fixed}
-   Now, `:`{.fixed} is used with every possible value from the left
    list (`1`{.fixed} and `2`{.fixed}) with every possible value in the
    right list (`[3]`{.fixed} and `[4]`{.fixed}), which results in
    `[1:[3], 1:[4], 2:[3], 2:[4]]`{.fixed}, which is
    `[[1,3],[1,4],[2,3],[2,4]`{.fixed}

Doing `(+) <$> [1,2] <*> [4,5,6]`{.fixed}results in a non-deterministic
computation `x + y`{.fixed} where `x`{.fixed} takes on every value from
`[1,2]`{.fixed} and `y`{.fixed} takes on every value from
`[4,5,6]`{.fixed}. We represent that as a list which holds all of the
possible results. Similarly, when we do
`sequence [[1,2],[3,4],[5,6],[7,8]]`{.fixed}, the result is a
non-deterministic computation `[x,y,z,w]`{.fixed}, where `x`{.fixed}
takes on every value from `[1,2]`{.fixed}, `y`{.fixed} takes on every
value from `[3,4]`{.fixed} and so on. To represent the result of that
non-deterministic computation, we use a list, where each element in the
list is one possible list. That's why the result is a list of lists.

When used with I/O actions, `sequenceA`{.fixed} is the same thing as
`sequence`{.fixed}! It takes a list of I/O actions and returns an I/O
action that will perform each of those actions and have as its result a
list of the results of those I/O actions. That's because to turn an
`[IO a]`{.fixed} value into an `IO [a]`{.fixed} value, to make an I/O
action that yields a list of results when performed, all those I/O
actions have to be sequenced so that they're then performed one after
the other when evaluation is forced. You can't get the result of an I/O
action without performing it.

~~~~ {.haskell:hs name="code"}
ghci> sequenceA [getLine, getLine, getLine]
heyh
ho
woo
["heyh","ho","woo"]
~~~~

Like normal functors, applicative functors come with a few laws. The
most important one is the one that we already mentioned, namely that
`pure f <*> x = fmap f x`{.label .law} holds. As an exercise, you can
prove this law for some of the applicative functors that we've met in
this chapter.The other functor laws are:

-   `pure id <*> v = v`{.label .law}
-   `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`{.label .law}
-   `pure f <*> pure x = pure (f x)`{.label .law}
-   `u <*> pure y = pure ($ y) <*> u`{.label .law}

We won't go over them in detail right now because that would take up a
lot of pages and it would probably be kind of boring, but if you're up
to the task, you can take a closer look at them and see if they hold for
some of the instances.

In conclusion, applicative functors aren't just interesting, they're
also useful, because they allow us to combine different computations,
such as I/O computations, non-deterministic computations, computations
that might have failed, etc. by using the applicative style. Just by
using `<$>`{.fixed} and `<*>`{.fixed} we can use normal functions to
uniformly operate on any number of applicative functors and take
advantage of the semantics of each one.

The newtype keyword
-------------------

![why\_ so serious?](lyah/maoi.png)

So far, we've learned how to make our own algebraic data types by using
the *data* keyword. We've also learned how to give existing types
synonyms with the *type* keyword. In this section, we'll be taking a
look at how to make new types out of existing data types by using the
*newtype* keyword and why we'd want to do that in the first place.

In the previous section, we saw that there are actually more ways for
the list type to be an applicative functor. One way is to have
`<*>`{.fixed} take every function out of the list that is its left
parameter and apply it to every value in the list that is on the right,
resulting in every possible combination of applying a function from the
left list to a value in the right list.

~~~~ {.haskell:hs name="code"}
ghci> [(+1),(*100),(*5)] <*> [1,2,3]
[2,3,4,100,200,300,5,10,15]
~~~~

The second way is to take the first function on the left side of
`<*>`{.fixed} and apply it to the first value on the right, then take
the second function from the list on the left side and apply it to the
second value on the right, and so on. Ultimately, it's kind of like
zipping the two lists together. But lists are already an instance of
`Applicative`{.fixed}, so how did we also make lists an instance of
`Applicative`{.fixed} in this second way? If you remember, we said that
the `ZipList a`{.fixed} type was introduced for this reason, which has
one value constructor, `ZipList`{.fixed}, that has just one field. We
put the list that we're wrapping in that field. Then, `ZipList`{.fixed}
was made an instance of `Applicative`{.fixed}, so that when we want to
use lists as applicatives in the zipping manner, we just wrap them with
the `ZipList`{.fixed} constructor and then once we're done, unwrap them
with `getZipList`{.fixed}:

~~~~ {.haskell:hs name="code"}
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
[2,200,15]
~~~~

So, what does this have to do with this *newtype* keyword? Well, think
about how we might write the data declaration for our ZipList a type.
One way would be to do it like so:

~~~~ {.haskell:hs name="code"}
data ZipList a = ZipList [a]
~~~~

A type that has just one value constructor and that value constructor
has just one field that is a list of things. We might also want to use
record syntax so that we automatically get a function that extracts a
list from a `ZipList`{.fixed}:

~~~~ {.haskell:hs name="code"}
data ZipList a = ZipList { getZipList :: [a] }
~~~~

This looks fine and would actually work pretty well. We had two ways of
making an existing type an instance of a type class, so we used the
*data* keyword to just wrap that type into another type and made the
other type an instance in the second way.

The *newtype* keyword in Haskell is made exactly for these cases when we
want to just take one type and wrap it in something to present it as
another type. In the actual libraries, `ZipList  a`{.fixed} is defined
like this:

~~~~ {.haskell:hs name="code"}
newtype ZipList a = ZipList { getZipList :: [a] }
~~~~

Instead of the *data* keyword, the *newtype* keyword is used. Now why is
that? Well for one, *newtype* is faster. If you use the *data* keyword
to wrap a type, there's some overhead to all that wrapping and
unwrapping when your program is running. But if you use *newtype*,
Haskell knows that you're just using it to wrap an existing type into a
new type (hence the name), because you want it to be the same internally
but have a different type. With that in mind, Haskell can get rid of the
wrapping and unwrapping once it resolves which value is of what type.

So why not just use *newtype* all the time instead of *data* then? Well,
when you make a new type from an existing type by using the *newtype*
keyword, you can only have one value constructor and that value
constructor can only have one field. But with *data*, you can make data
types that have several value constructors and each constructor can have
zero or more fields:

~~~~ {.haskell:hs name="code"}
data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession
~~~~

When using *newtype*, you're restricted to just one constructor with one
field.

We can also use the *deriving* keyword with *newtype* just like we would
with *data*. We can derive instances for `Eq`{.fixed}, `Ord`{.fixed},
`Enum`{.fixed}, `Bounded`{.fixed}, `Show`{.fixed} and `Read`{.fixed}. If
we derive the instance for a type class, the type that we're wrapping
has to be in that type class to begin with. It makes sense, because
*newtype* just wraps an existing type. So now if we do the following, we
can print and equate values of our new type:

~~~~ {.haskell:hs name="code"}
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
~~~~

Let's give that a go:

~~~~ {.haskell:hs name="code"}
ghci> CharList "this will be shown!"
CharList {getCharList = "this will be shown!"}
ghci> CharList "benny" == CharList "benny"
True
ghci> CharList "benny" == CharList "oisters"
False
~~~~

In this particular *newtype*, the value constructor has the following
type:

~~~~ {.haskell:hs name="code"}
CharList :: [Char] -> CharList
~~~~

It takes a `[Char]`{.fixed} value, such as "my sharona" and returns a
CharList value. From the above examples where we used the
`CharList`{.fixed} value constructor, we see that really is the case.
Conversely, the `getCharList`{.fixed} function, which was generated for
us because we used record syntax in our *newtype*, has this type:

~~~~ {.haskell:hs name="code"}
getCharList :: CharList -> [Char]
~~~~

It takes a `CharList`{.fixed} value and converts it to a
`[Char]`{.fixed} value. You can think of this as wrapping and
unwrapping, but you can also think of it as converting values from one
type to the other.

### Using newtype to make type class instances

Many times, we want to make our types instances of certain type classes,
but the type parameters just don't match up for what we want to do. It's
easy to make `Maybe`{.fixed} an instance of `Functor`{.fixed}, because
the `Functor`{.fixed} type class is defined like this:

~~~~ {.haskell:hs name="code"}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
~~~~

So we just start out with:

~~~~ {.haskell:hs name="code"}
instance Functor Maybe where 
~~~~

And then implement `fmap`{.fixed}. All the type parameters add up
because the `Maybe`{.fixed} takes the place of `f`{.fixed} in the
definition of the `Functor`{.fixed} type class and so if we look at
`fmap`{.fixed} like it only worked on `Maybe`{.fixed}, it ends up
behaving like:

~~~~ {.haskell:hs name="code"}
fmap :: (a -> b) -> Maybe a -> Maybe b
~~~~

![wow, very evil](lyah/krakatoa.png)

Isn't that just peachy? Now what if we wanted to make the tuple an
instance of `Functor`{.fixed} in such a way that when we `fmap`{.fixed}
a function over a tuple, it gets applied to the first component of the
tuple? That way, doing `fmap (+3) (1,1)`{.fixed} would result in
`(4,1)`{.fixed}. It turns out that writing the instance for that is kind
of hard. With Maybe, we just say `instance Functor  Maybe where`{.fixed}
because only type constructors that take exactly one parameter can be
made an instance of `Functor`{.fixed}. But it seems like there's no way
to do something like that with `(a,b)`{.fixed} so that the type
parameter `a`{.fixed} ends up being the one that changes when we use
`fmap`{.fixed}. To get around this, we can *newtype* our tuple in such a
way that the second type parameter represents the type of the first
component in the tuple:

~~~~ {.haskell:hs name="code"}
newtype Pair b a = Pair { getPair :: (a,b) }
~~~~

And now, we can make it an instance of `Functor`{.fixed} so that the
function is mapped over the first component:

~~~~ {.haskell:hs name="code"}
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)
~~~~

As you can see, we can pattern match on types defined with *newtype*. We
pattern match to get the underlying tuple, then we apply the function
`f`{.fixed} to the first component in the tuple and then we use the
`Pair`{.fixed} value constructor to convert the tuple back to our
`Pair b a`{.fixed}. If we imagine what the type `fmap`{.fixed} would be
if it only worked on our new pairs, it would be:

~~~~ {.haskell:hs name="code"}
fmap :: (a -> b) -> Pair c a -> Pair c b
~~~~

Again, we said `instance Functor (Pair c) where`{.fixed} and so
`Pair c`{.fixed} took the place of the `f`{.fixed} in the type class
definition for `Functor`{.fixed}:

~~~~ {.haskell:hs name="code"}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
~~~~

So now, if we convert a tuple into a `Pair b a`{.fixed}, we can use
`fmap`{.fixed} over it and the function will be mapped over the first
component:

~~~~ {.haskell:hs name="code"}
ghci> getPair $ fmap (*100) (Pair (2,3))
(200,3)
ghci> getPair $ fmap reverse (Pair ("london calling", 3))
("gnillac nodnol",3)
~~~~

### On newtype laziness

We mentioned that *newtype* is usually faster than *data*. The only
thing that can be done with *newtype* is turning an existing type into a
new type, so internally, Haskell can represent the values of types
defined with *newtype* just like the original ones, only it has to keep
in mind that the their types are now distinct. This fact means that not
only is *newtype* faster, it's also lazier. Let's take a look at what
this means.

Like we've said before, Haskell is lazy by default, which means that
only when we try to actually print the results of our functions will any
computation take place. Furthemore, only those computations that are
necessary for our function to tell us the result will get carried out.
The `undefined`{.fixed} value in Haskell represents an erronous
computation. If we try to evaluate it (that is, force Haskell to
actually compute it) by printing it to the terminal, Haskell will throw
a hissy fit (technically referred to as an exception):

~~~~ {.haskell:hs name="code"}
ghci> undefined
*** Exception: Prelude.undefined
~~~~

However, if we make a list that has some `undefined`{.fixed} values in
it but request only the head of the list, which is not
`undefined`{.fixed}, everything will go smoothly because Haskell doesn't
really need to evaluate any other elements in a list if we only want to
see what the first element is:

~~~~ {.haskell:hs name="code"}
ghci> head [3,4,5,undefined,2,undefined]
3
~~~~

Now consider the following type:

~~~~ {.haskell:hs name="code"}
data CoolBool = CoolBool { getCoolBool :: Bool }
~~~~

It's your run-of-the-mill algebraic data type that was defined with the
*data* keyword. It has one value constructor, which has one field whose
type is `Bool`{.fixed}. Let's make a function that pattern matches on a
`CoolBool`{.fixed} and returns the value `"hello"`{.fixed} regardless of
whether the `Bool`{.fixed} inside the `CoolBool`{.fixed} was
`True`{.fixed} or `False`{.fixed}:

~~~~ {.haskell:hs name="code"}
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
~~~~

Instead of applying this function to a normal `CoolBool`{.fixed}, let's
throw it a curveball and apply it to `undefined`{.fixed}!

~~~~ {.haskell:hs name="code"}
ghci> helloMe undefined
"*** Exception: Prelude.undefined
~~~~

Yikes! An exception! Now why did this exception happen? Types defined
with the *data* keyword can have multiple value constructors (even
though `CoolBool`{.fixed} only has one). So in order to see if the value
given to our function conforms to the `(CoolBool _)`{.fixed} pattern,
Haskell has to evaluate the value just enough to see which value
constructor was used when we made the value. And when we try to evaluate
an `undefined`{.fixed} value, even a little, an exception is thrown.

Instead of using the *data* keyword for `CoolBool`{.fixed}, let's try
using *newtype*:

~~~~ {.haskell:hs name="code"}
newtype CoolBool = CoolBool { getCoolBool :: Bool }
~~~~

We don't have to change our `helloMe`{.fixed} function, because the
pattern matching syntax is the same if you use *newtype* or *data* to
define your type. Let's do the same thing here and apply
`helloMe`{.fixed} to an `undefined`{.fixed} value:

~~~~ {.haskell:hs name="code"}
ghci> helloMe undefined
"hello"
~~~~

![top of the mornin to ya!!!](lyah/shamrock.png)

It worked! Hmmm, why is that? Well, like we've said, when we use
*newtype*, Haskell can internally represent the values of the new type
in the same way as the original values. It doesn't have to add another
box around them, it just has to be aware of the values being of
different types. And because Haskell knows that types made with the
*newtype* keyword can only have one constructor, it doesn't have to
evaluate the value passed to the function to make sure that it conforms
to the `(CoolBool _)`{.fixed} pattern because *newtype* types can only
have one possible value constructor and one field!

This difference in behavior may seem trivial, but it's actually pretty
important because it helps us realize that even though types defined
with *data* and *newtype* behave similarly from the programmer's point
of view because they both have value constructors and fields, they are
actually two different mechanisms. Whereas *data* can be used to make
your own types from scratch, *newtype* is for making a completely new
type out of an existing type. Pattern matching on *newtype* values isn't
like taking something out of a box (like it is with *data*), it's more
about making a direct conversion from one type to another.

### `type`{.fixed} vs. `newtype`{.fixed} vs. `data`{.fixed}

At this point, you may be a bit confused about what exactly the
difference between *type*, *data* and *newtype* is, so let's refresh our
memory a bit.

The *type* keyword is for making type synonyms. What that means is that
we just give another name to an already existing type so that the type
is easier to refer to. Say we did the following:

~~~~ {.haskell:hs name="code"}
type IntList = [Int]
~~~~

All this does is to allow us to refer to the `[Int]`{.fixed} type as
`IntList`{.fixed}. They can be used interchangeably. We don't get an
`IntList`{.fixed} value constructor or anything like that. Because
`[Int]`{.fixed} and `IntList`{.fixed} are only two ways to refer to the
same type, it doesn't matter which name we use in our type annotations:

~~~~ {.haskell:hs name="code"}
ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])
[1,2,3,1,2,3]
~~~~

We use type synonyms when we want to make our type signatures more
descriptive by giving types names that tell us something about their
purpose in the context of the functions where they're being used. For
instance, when we used an association list of type
`[(String,String)]`{.fixed} to represent a phone book, we gave it the
type synonym of `PhoneBook`{.fixed} so that the type signatures of our
functions were easier to read.

The *newtype* keyword is for taking existing types and wrapping them in
new types, mostly so that it's easier to make them instances of certain
type classes. When we use *newtype* to wrap an existing type, the type
that we get is separate from the original type. If we make the following
*newtype*:

~~~~ {.haskell:hs name="code"}
newtype CharList = CharList { getCharList :: [Char] }
~~~~

We can't use `++`{.fixed} to put together a `CharList`{.fixed} and a
list of type `[Char]`{.fixed}. We can't even use `++`{.fixed} to put
together two `CharList`{.fixed}s, because `++`{.fixed} works only on
lists and the `CharList`{.fixed} type isn't a list, even though it could
be said that it contains one. We can, however, convert two
`CharList`{.fixed}s to lists, `++`{.fixed} them and then convert that
back to a `CharList`{.fixed}.

When we use record syntax in our *newtype* declarations, we get
functions for converting between the new type and the original type:
namely the value constructor of our *newtype* and the function for
extracting the value in its field. The new type also isn't automatically
made an instance of the type classes that the original type belongs to,
so we have to derive or manually write them.

In practice, you can think of *newtype* declarations as *data*
declarations that can only have one constructor and one field. If you
catch yourself writing such a *data* declaration, consider using
*newtype*.

The *data* keyword is for making your own data types and with them, you
can go hog wild. They can have as many constructors and fields as you
wish and can be used to implement any algebraic data type by yourself.
Everything from lists and `Maybe`{.fixed}-like types to trees.

If you just want your type signatures to look cleaner and be more
descriptive, you probably want type synonyms. If you want to take an
existing type and wrap it in a new type in order to make it an instance
of a type class, chances are you're looking for a *newtype*. And if you
want to make something completely new, odds are good that you're looking
for the *data* keyword.

Monoids
-------

![wow this is pretty much the gayest pirate ship
ever](lyah/pirateship.png)

Type classes in Haskell are used to present an interface for types that
have some behavior in common. We started out with simple type classes
like Eq, which is for types whose values can be equated, and
`Ord`{.fixed}, which is for things that can be put in an order and then
moved on to more interesting ones, like Functor and
`Applicative`{.fixed}.

When we make a type, we think about which behaviors it supports, i.e.
what it can act like and then based on that we decide which type classes
to make it an instance of. If it makes sense for values of our type to
be equated, we make it an instance of the `Eq`{.fixed} type class. If we
see that our type is some kind of functor, we make it an instance of
`Functor`{.fixed}, and so on.

Now consider the following: `*`{.fixed} is a function that takes two
numbers and multiplies them. If we multiply some number with a 1, the
result is always equal to that number. It doesn't matter if we do
`1 * x`{.fixed} or `x *  1`{.fixed}, the result is always `x`{.fixed}.
Similarly, ++ is also a function which takes two things and returns a
third. Only instead of multiplying numbers, it takes two lists and
concatenates them. And much like `*`{.fixed}, it also has a certain
value which doesn't change the other one when used with `++`{.fixed}.
That value is the empty list: `[]`{.fixed}.

~~~~ {.haskell:hs name="code"}
ghci> 4 * 1
4
ghci> 1 * 9
9
ghci> [1,2,3] ++ []
[1,2,3]
ghci> [] ++ [0.5, 2.5]
[0.5,2.5]
~~~~

It seems that both `*`{.fixed} together with 1 and `++`{.fixed} along
with `[]`{.fixed} share some common properties:

-   The function takes two parameters.
-   The parameters and the returned value have the same type.
-   There exists such a value that doesn't change other values when used
    with the binary function.

There's another thing that these two operations have in common that may
not be as obvious as our previous observations: when we have three or
more values and we want to use the binary function to reduce them to a
single result, the order in which we apply the binary function to the
values doesn't matter. It doesn't matter if we do `(3 * 4) * 5`{.fixed}
or `3  * (4 * 5)`{.fixed}. Either way, the result is `60`{.fixed}. The
same goes for `++`{.fixed}:

~~~~ {.haskell:hs name="code"}
ghci> (3 * 2) * (8 * 5)
240
ghci> 3 * (2 * (8 * 5))
240
ghci> "la" ++ ("di" ++ "da")
"ladida"
ghci> ("la" ++ "di") ++ "da"
"ladida"
~~~~

We call this property *associativity*. `*`{.fixed} is associative, and
so is `++`{.fixed}, but `-`{.fixed}, for example, is not. The
expressions `(5 - 3) - 4`{.fixed} and `5 - (3 - 4)`{.fixed} result in
different numbers.

By noticing and writing down these properties, we have chanced upon
*monoids*! A monoid is when you have an associative binary function and
a value which acts as an identity with respect to that function. When
something acts as an identity with respect to a function, it means that
when called with that function and some other value, the result is
always equal to that other value. 1 is the identity with respect to
`*`{.fixed} and [] is the identity with respect to ++. There are a lot
of other monoids to be found in the world of Haskell, which is why the
`Monoid`{.fixed} type class exists. It's for types which can act like
monoids. Let's see how the type class is defined:

~~~~ {.haskell:hs name="code"}
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
~~~~

![woof dee do!!!](lyah/balloondog.png)

The `Monoid`{.fixed} type class is defined in
`import Data.Monoid`{.fixed}. Let's take some time and get properly
acquainted with it.

First of all, we see that only concrete types can be made instances of
`Monoid`{.fixed}, because the `m`{.fixed} in the type class definition
doesn't take any type parameters. This is different from
`Functor`{.fixed} and `Applicative`{.fixed}, which require their
instances to be type constructors which take one parameter.

The first function is `mempty`{.fixed}. It's not really a function,
since it doesn't take parameters, so it's a polymorphic constant, kind
of like `minBound`{.fixed} from Bounded. `mempty`{.fixed} represents the
identity value for a particular monoid.

Next up, we have `mappend`{.fixed}, which, as you've probably guessed,
is the binary function. It takes two values of the same type and returns
a value of that type as well. It's worth noting that the decision to
name `mappend`{.fixed} as it's named was kind of unfortunate, because it
implies that we're appending two things in some way. While ++ does take
two lists and append one to the other, \* doesn't really do any
appending, it just multiplies two numbers together. When we meet other
instances of Monoid, we'll see that most of them don't append values
either, so avoid thinking in terms of appending and just think in terms
of `mappend`{.fixed} being a binary function that takes two monoid
values and returns a third.

The last function in this type class definition is `mconcat`{.fixed}. It
takes a list of monoid values and reduces them to a single value by
doing `mappend`{.fixed} between the list's elements. It has a default
implementation, which just takes `mempty`{.fixed} as a starting value
and folds the list from the right with `mappend`{.fixed}. Because the
default implementation is fine for most instances, we won't concern
ourselves with `mconcat`{.fixed} too much from now on. When making a
type an instance of `Monoid`{.fixed}, it suffices to just implement
`mempty`{.fixed} and `mappend`{.fixed}. The reason `mconcat`{.fixed} is
there at all is because for some instances, there might be a more
efficient way to implement `mconcat`{.fixed}, but for most instances the
default implementation is just fine.

Before moving on to specific instances of `Monoid`{.fixed}, let's take a
brief look at the monoid laws. We mentioned that there has to be a value
that acts as the identity with respect to the binary function and that
the binary function has to be associative. It's possible to make
instances of `Monoid`{.fixed} that don't follow these rules, but such
instances are of no use to anyone because when using the
`Monoid`{.fixed} type class, we rely on its instances acting like
monoids. Otherwise, what's the point? That's why when making instances,
we have to make sure they follow these laws:

-   `` mempty `mappend` x = x ``{.label .law}
-   `` x `mappend` mempty = x ``{.label .law}
-   `` (x `mappend` y) `mappend` z = x `mappend` (y      `mappend` z) ``{.label
    .law}

The first two state that `mempty`{.fixed} has to act as the identity
with respect to `mappend`{.fixed} and the third says that
`mappend`{.fixed} has to be associative i.e. that it the order in which
we use `mappend`{.fixed} to reduce several monoid values into one
doesn't matter. Haskell doesn't enforce these laws, so we as the
programmer have to be careful that our instances do indeed obey them.

### Lists are monoids

Yes, lists are monoids! Like we've seen, the `++`{.fixed} function and
the empty list `[]`{.fixed} form a monoid. The instance is very simple:

~~~~ {.haskell:hs name="code"}
instance Monoid [a] where
    mempty = []
    mappend = (++)
~~~~

Lists are an instance of the `Monoid`{.fixed} type class regardless of
the type of the elements they hold. Notice that we wrote
`instance Monoid [a]`{.fixed} and not `instance Monoid []`{.fixed},
because Monoid requires a concrete type for an instance.

Giving this a test run, we encounter no surprises:

~~~~ {.haskell:hs name="code"}
ghci> [1,2,3] `mappend` [4,5,6]
[1,2,3,4,5,6]
ghci> ("one" `mappend` "two") `mappend` "tree"
"onetwotree"
ghci> "one" `mappend` ("two" `mappend` "tree")
"onetwotree"
ghci> "one" `mappend` "two" `mappend` "tree"
"onetwotree"
ghci> "pang" `mappend` mempty
"pang"
ghci> mconcat [[1,2],[3,6],[9]]
[1,2,3,6,9]
ghci> mempty :: [a]
[]
~~~~

![smug as hell](lyah/smug.png)

Notice that in the last line, we had to write an explicit type
annotation, because if we just did `mempty`{.fixed}, GHCi wouldn't know
which instance to use, so we had to say we want the list instance. We
were able to use the general type of `[a]`{.fixed} (as opposed to
specifying `[Int]`{.fixed} or `[String]`{.fixed}) because the empty list
can act as if it contains any type.

Because `mconcat`{.fixed} has a default implementation, we get it for
free when we make something an instance of `Monoid`{.fixed}. In the case
of the list, `mconcat`{.fixed} turns out to be just `concat`{.fixed}. It
takes a list of lists and flattens it, because that's the equivalent of
doing `++`{.fixed} between all the adjecent lists in a list.

The monoid laws do indeed hold for the list instance. When we have
several lists and we `mappend`{.fixed} (or `++`{.fixed}) them together,
it doesn't matter which ones we do first, because they're just joined at
the ends anyway. Also, the empty list acts as the identity so all is
well. Notice that monoids don't require that `` a `mappend` b ``{.fixed}
be equal to `` b `mappend` a ``{.fixed}. In the case of the list, they
clearly aren't:

~~~~ {.haskell:hs name="code"}
ghci> "one" `mappend` "two"
"onetwo"
ghci> "two" `mappend` "one"
"twoone"
~~~~

And that's okay. The fact that for multiplication `3 * 5`{.fixed} and
`5 * 3`{.fixed} are the same is just a property of multiplication, but
it doesn't hold for all (and indeed, most) monoids.

### `Product`{.fixed} and `Sum`{.fixed}

We already examined one way for numbers to be considered monoids. Just
have the binary function be `*`{.fixed} and the identity value
`1`{.fixed}. It turns out that that's not the only way for numbers to be
monoids. Another way is to have the binary function be `+`{.fixed} and
the identity value `0`{.fixed}:

~~~~ {.haskell:hs name="code"}
ghci> 0 + 4
4
ghci> 5 + 0
5
ghci> (1 + 3) + 5
9
ghci> 1 + (3 + 5)
9
~~~~

The monoid laws hold, because if you add 0 to any number, the result is
that number. And addition is also associative, so we get no problems
there. So now that there are two equally valid ways for numbers to be
monoids, which way do choose? Well, we don't have to. Remember, when
there are several ways for some type to be an instance of the same type
class, we can wrap that type in a *newtype* and then make the new type
an instance of the type class in a different way. We can have our cake
and eat it too.

The `Data.Monoid`{.fixed} module exports two types for this, namely
`Product`{.fixed} and `Sum`{.fixed}. `Product`{.fixed} is defined like
this:

~~~~ {.haskell:hs name="code"}
newtype Product a =  Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)
~~~~

Simple, just a *newtype* wrapper with one type parameter along with some
derived instances. Its instance for `Monoid`{.fixed} goes a little
something like this:

~~~~ {.haskell:hs name="code"}
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
~~~~

`mempty`{.fixed} is just `1`{.fixed} wrapped in a `Product`{.fixed}
constructor. `mappend`{.fixed} pattern matches on the `Product`{.fixed}
constructor, multiplies the two numbers and then wraps the resulting
number back. As you can see, there's a `Num a`{.fixed} class constraint.
So this means that `Product a`{.fixed} is an instance of
`Monoid`{.fixed} for all `a`{.fixed}'s that are already an instance of
`Num`{.fixed}. To use `Producta a`{.fixed} as a monoid, we have to do
some *newtype* wrapping and unwrapping:

~~~~ {.haskell:hs name="code"}
ghci> getProduct $ Product 3 `mappend` Product 9
27
ghci> getProduct $ Product 3 `mappend` mempty
3
ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
24
ghci> getProduct . mconcat . map Product $ [3,4,2]
24
~~~~

This is nice as a showcase of the `Monoid`{.fixed} type class, but no
one in their right mind would use this way of multiplying numbers
instead of just writing `3 * 9`{.fixed} and `3 * 1`{.fixed}. But a bit
later, we'll see how these `Monoid`{.fixed} instances that may seem
trivial at this time can come in handy.

`Sum`{.fixed} is defined like `Product`{.fixed} and the instance is
similar as well. We use it in the same way:

~~~~ {.haskell:hs name="code"}
ghci> getSum $ Sum 2 `mappend` Sum 9
11
ghci> getSum $ mempty `mappend` Sum 3
3
ghci> getSum . mconcat . map Sum $ [1,2,3]
6
~~~~

### `Any`{.fixed} and `All`{.fixed}

Another type which can act like a monoid in two distinct but equally
valid ways is `Bool`{.fixed}. The first way is to have the *or* function
`||`{.fixed} act as the binary function along with `False`{.fixed} as
the identity value. The way *or* works in logic is that if any of its
two parameters is `True`{.fixed}, it returns `True`{.fixed}, otherwise
it returns `False`{.fixed}. So if we use `False`{.fixed} as the identity
value, it will return `False`{.fixed} when *or*-ed with `False`{.fixed}
and `True`{.fixed} when *or*-ed with `True`{.fixed}. The `Any`{.fixed}
*newtype* constructor is an instance of `Monoid`{.fixed} in this
fashion. It's defined like this:

~~~~ {.haskell:hs name="code"}
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
~~~~

Its instance looks goes like so:

~~~~ {.haskell:hs name="code"}
instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)
~~~~

The reason it's called `Any`{.fixed} is because
`` x `mappend` y ``{.fixed} will be `True`{.fixed} if *any* one of those
two is `True`{.fixed}. Even if three or more `Any`{.fixed} wrapped
`Bool`{.fixed}s are `mappend`{.fixed}ed together, the result will hold
`True`{.fixed} if any of them are `True`{.fixed}:

~~~~ {.haskell:hs name="code"}
ghci> getAny $ Any True `mappend` Any False
True
ghci> getAny $ mempty `mappend` Any True
True
ghci> getAny . mconcat . map Any $ [False, False, False, True]
True
ghci> getAny $ mempty `mappend` mempty
False
~~~~

The other way for `Bool`{.fixed} to be an instance of `Monoid`{.fixed}
is to kind of do the opposite: have `&&`{.fixed} be the binary function
and then make `True`{.fixed} the identity value. Logical *and* will
return `True`{.fixed} only if both of its parameters are `True`{.fixed}.
This is the *newtype* declaration, nothing fancy:

~~~~ {.haskell:hs name="code"}
newtype All = All { getAll :: Bool }
        deriving (Eq, Ord, Read, Show, Bounded)
~~~~

And this is the instance:

~~~~ {.haskell:hs name="code"}
instance Monoid All where
        mempty = All True
        All x `mappend` All y = All (x && y)
~~~~

When we `mappend`{.fixed} values of the `All`{.fixed} type, the result
will be `True`{.fixed} only if *all* the values used in the
`mappend`{.fixed} operations are `True`{.fixed}:

~~~~ {.haskell:hs name="code"}
ghci> getAll $ mempty `mappend` All True
True
ghci> getAll $ mempty `mappend` All False
False
ghci> getAll . mconcat . map All $ [True, True, True]
True
ghci> getAll . mconcat . map All $ [True, True, False]
False
~~~~

Just like with multiplication and addition, we usually explicitly state
the binary functions instead of wrapping them in *newtype*s and then
using `mappend`{.fixed} and `mempty`{.fixed}. `mconcat`{.fixed} seems
useful for `Any`{.fixed} and `All`{.fixed}, but usually it's easier to
use the `or`{.fixed} and `and`{.fixed} functions, which take lists of
`Bool`{.fixed}s and return `True`{.fixed} if any of them are
`True`{.fixed} or if all of them are `True`{.fixed}, respectively.

### The `Ordering`{.fixed} monoid

Hey, remember the `Ordering`{.fixed} type? It's used as the result when
comparing things and it can have three values: `LT`{.fixed},
`EQ`{.fixed} and `GT`{.fixed}, which stand for *less than*, *equal* and
*greater than* respectively:

~~~~ {.haskell:hs name="code"}
ghci> 1 `compare` 2
LT
ghci> 2 `compare` 2
EQ
ghci> 3 `compare` 2
GT
~~~~

With lists, numbers and boolean values, finding monoids was just a
matter of looking at already existing commonly used functions and seeing
if they exhibit some sort of monoid behavior. With `Ordering`{.fixed},
we have to look a bit harder to recognize a monoid, but it turns out
that its `Monoid`{.fixed} instance is just as intuitive as the ones
we've met so far and also quite useful:

~~~~ {.haskell:hs name="code"}
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
~~~~

![did anyone ORDER pizza?!?! I can't BEAR these puns!](lyah/bear.png)

The instance is set up like this: when we `mappend`{.fixed} two
`Ordering`{.fixed} values, the one on the left is kept, unless the value
on the left is `EQ`{.fixed}, in which case the right one is the result.
The identity is `EQ`{.fixed}. At first, this may seem kind of arbitrary,
but it actually resembles the way we alphabetically compare words. We
compare the first two letters and if they differ, we can already decide
which word would go first in a dictionary. However, if the first two
letters are equal, then we move on to comparing the next pair of letters
and repeat the process.

For instance, if we were to alphabetically compare the words
`"ox"`{.fixed} and `"on"`{.fixed}, we'd first compare the first two
letters of each word, see that they are equal and then move on to
comparing the second letter of each word. We see that 'x' is
alphabetically greater than 'n', and so we know how the words compare.
To gain some intuition for `EQ`{.fixed} being the identity, we can
notice that if we were to cram the same letter in the same position in
both words, it wouldn't change their alphabetical ordering.
`"oix"`{.fixed} is still alphabetically greater than and
`"oin"`{.fixed}.

It's important to note that in the `Monoid`{.fixed} instance for
`Ordering`{.fixed}, `` x `mappend` y ``{.fixed} doesn't equal
`` y `mappend` x ``{.fixed}. Because the first parameter is kept unless
it's `EQ`{.fixed}, LT \`mappend\` GT will result in LT, whereas
`` GT `mappend` LT ``{.fixed} will result in `GT`{.fixed}:

~~~~ {.haskell:hs name="code"}
ghci> LT `mappend` GT
LT
ghci> GT `mappend` LT
GT
ghci> mempty `mappend` LT
LT
ghci> mempty `mappend` GT
GT
~~~~

OK, so how is this monoid useful? Let's say you were writing a function
that takes two strings, compares their lengths, and returns an Ordering.
But if the strings are of the same length, then instead of returning
`EQ`{.fixed} right away, we want to compare them alphabetically. One way
to write this would be like so:

~~~~ {.haskell:hs name="code"}
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y 
                        b = x `compare` y
                    in  if a == EQ then b else a
~~~~

We name the result of comparing the lengths `a`{.fixed} and the result
of the alphabetical comparison `b`{.fixed} and then if it turns out that
the lengths were equal, we return their alphabetical ordering.

But by employing our understanding of how `Ordering`{.fixed} is a
monoid, we can rewrite this function in a much simpler manner:

~~~~ {.haskell:hs name="code"}
import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)
~~~~

We can try this out:

~~~~ {.haskell:hs name="code"}
ghci> lengthCompare "zen" "ants"
LT
ghci> lengthCompare "zen" "ant"
GT
~~~~

Remember, when we use `mappend`{.fixed}, its left parameter is always
kept unless it's `EQ`{.fixed}, in which case the right one is kept.
That's why we put the comparison that we consider to be the first, more
important criterion as the first parameter. If we wanted to expand this
function to also compare for the number of vowels and set this to be the
second most important criterion for comparison, we'd just modify it like
this:

~~~~ {.haskell:hs name="code"}
import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")
~~~~

We made a helper function, which takes a string and tells us how many
vowels it has by first filtering it only for letters that are in the
string "aeiou" and then applying `length`{.fixed} to that.

~~~~ {.haskell:hs name="code"}
ghci> lengthCompare "zen" "anna"
LT
ghci> lengthCompare "zen" "ana"
LT
ghci> lengthCompare "zen" "ann"
GT
~~~~

Very cool. Here, we see how in the first example the lengths are found
to be different and so `LT`{.fixed} is returned, because the length of
`"zen"`{.fixed} is less than the length of `"anna"`{.fixed}. In the
second example, the lengths are the same, but the second string has more
vowels, so `LT`{.fixed} is returned again. In the third example, they
both have the same length and the same number of vowels, so they're
compared alphabetically and `"zen"`{.fixed} wins.

The `Ordering`{.fixed} monoid is very cool because it allows us to
easily compare things by many different criteria and put those criteria
in an order themselves, ranging from the most important to the least.

### `Maybe`{.fixed} the monoid

Let's take a look at the various ways that `Maybe a`{.fixed} can be made
an instance of `Monoid`{.fixed} and what those instances are useful for.

One way is to treat `Maybe a`{.fixed} as a monoid only if its type
parameter `a`{.fixed} is a monoid as well and then implement
`mappend`{.fixed} in such a way that it uses the `mappend`{.fixed}
operation of the values that are wrapped with `Just`{.fixed}. We use
`Nothing`{.fixed} as the identity, and so if one of the two values that
we're `mappend`{.fixed}ing is `Nothing`{.fixed}, we keep the other
value. Here's the instance declaration:

~~~~ {.haskell:hs name="code"}
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
~~~~

Notice the class constraint. It says that `Maybe a`{.fixed} is an
instance of `Monoid`{.fixed} only if a is an instance of
`Monoid`{.fixed}. If we `mappend`{.fixed} something with a Nothing, the
result is that something. If we `mappend`{.fixed} two Just values, the
contents of the Justs get `mappended`{.fixed} and then wrapped back in a
Just. We can do this because the class constraint ensures that the type
of what's inside the `Just`{.fixed} is an instance of `Monoid`{.fixed}.

~~~~ {.haskell:hs name="code"}
ghci> Nothing `mappend` Just "andy"
Just "andy"
ghci> Just LT `mappend` Nothing
Just LT
ghci> Just (Sum 3) `mappend` Just (Sum 4)
Just (Sum {getSum = 7})
~~~~

This comes in use when you're dealing with monoids as results of
computations that may have failed. Because of this instance, we don't
have to check if the computations have failed by seeing if they're a
`Nothing`{.fixed} or `Just`{.fixed} value; we can just continue to treat
them as normal monoids.

But what if the type of the contents of the `Maybe`{.fixed} aren't an
instance of `Monoid`{.fixed}? Notice that in the previous instance
declaration, the only case where we have to rely on the contents being
monoids is when both parameters of `mappend`{.fixed} are `Just`{.fixed}
values. But if we don't know if the contents are monoids, we can't use
`mappend`{.fixed} between them, so what are we to do? Well, one thing we
can do is to just discard the second value and keep the first one. For
this, the `First a`{.fixed} type exists and this is its definition:

~~~~ {.haskell:hs name="code"}
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)
~~~~

We take a `Maybe a`{.fixed} and we wrap it with a *newtype*. The
`Monoid`{.fixed} instance is as follows:

~~~~ {.haskell:hs name="code"}
instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x
~~~~

Just like we said. `mempty`{.fixed} is just a `Nothing`{.fixed} wrapped
with the `First`{.fixed} *newtype* constructor. If `mappend`{.fixed}'s
first parameter is a `Just`{.fixed} value, we ignore the second one. If
the first one is a `Nothing`{.fixed}, then we present the second
parameter as a result, regardless of whether it's a `Just`{.fixed} or a
`Nothing`{.fixed}:

~~~~ {.haskell:hs name="code"}
ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
Just 'a'
ghci> getFirst $ First Nothing `mappend` First (Just 'b')
Just 'b'
ghci> getFirst $ First (Just 'a') `mappend` First Nothing
Just 'a'
~~~~

`First`{.fixed} is useful when we have a bunch of Maybe values and we
just want to know if any of them is a `Just`{.fixed}. The
`mconcat`{.fixed} function comes in handy:

~~~~ {.haskell:hs name="code"}
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
Just 9
~~~~

If we want a monoid on `Maybe a`{.fixed} such that the second parameter
is kept if both parameters of `mappend`{.fixed} are `Just`{.fixed}
values, `Data.Monoid`{.fixed} provides a the `Last a`{.fixed} type,
which works like First a, only the last non-Nothing value is kept when
`mappend`{.fixed}ing and using mconcat:

~~~~ {.haskell:hs name="code"}
ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
Just 10
ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")
Just "two"
~~~~

### Using monoids to fold data structures

One of the more interesting ways to put monoids to work is to make them
help us define folds over various data structures. So far, we've only
done folds over lists, but lists aren't the only data structure that can
be folded over. We can define folds over almost any data structure.
Trees especially lend themselves well to folding.

Because there are so many data structures that work nicely with folds,
the `Foldable`{.label .class} type class was introduced. Much like
`Functor`{.fixed} is for things that can be mapped over,
`Foldable`{.fixed} is for things that can be folded up! It can be found
in `Data.Foldable`{.fixed} and because it export functions whose names
clash with the ones from the `Prelude`{.fixed}, it's best imported
qualified (and served with basil):

~~~~ {.haskell:hs name="code"}
import qualified Foldable as F
~~~~

To save ourselves precious keystrokes, we've chosen to import it
qualified as `F`{.fixed}. Alright, so what are some of the functions
that this type class defines? Well, among them are `foldr`{.fixed},
`foldl`{.fixed}, `foldr1`{.fixed} and `foldl1`{.fixed}. Huh? But we
already know these functions, what's so new about this? Let's compare
the types of `Foldable`{.fixed}'s `foldr`{.fixed} and the
`foldr`{.fixed} from the `Prelude`{.fixed} to see how they differ:

~~~~ {.haskell:hs name="code"}
ghci> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
ghci> :t F.foldr
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
~~~~

Ah! So whereas `foldr`{.fixed} takes a list and folds it up, the
`foldr`{.fixed} from `Data.Foldable`{.fixed} accepts any type that can
be folded up, not just lists! As expected, both `foldr`{.fixed}
functions do the same for lists:

~~~~ {.haskell:hs name="code"}
ghci> foldr (*) 1 [1,2,3]
6
ghci> F.foldr (*) 1 [1,2,3]
6
~~~~

Okay then, what are some other data structures that support folds? Well,
there's the `Maybe`{.fixed} we all know and love!

~~~~ {.haskell:hs name="code"}
ghci> F.foldl (+) 2 (Just 9)
11
ghci> F.foldr (||) False (Just True)
True
~~~~

But folding over a `Maybe`{.fixed} value isn't terribly interesting,
because when it comes to folding, it just acts like a list with one
element if it's a `Just`{.fixed} value and as an empty list if it's
`Nothing`{.fixed}. So let's examine a data structure that's a little
more complex then.

Remember the tree data structure from the [Making Our Own Types and
Typeclasses](making-our-own-types-and-typeclasses.html#recursive-data-structures)
chapter? We defined it like this:

~~~~ {.haskell:hs name="code"}
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
~~~~

We said that a tree is either an empty tree that doesn't hold any values
or it's a node that holds one value and also two other trees. After
defining it, we made it an instance of `Functor`{.fixed} and with that
we gained the ability to `fmap`{.fixed} functions over it. Now, we're
going to make it an instance of `Foldable`{.fixed} so that we get the
abilty to fold it up. One way to make a type constructor an instance of
`Foldable`{.fixed} is to just directly implement `foldr`{.fixed} for it.
But another, often much easier way, is to implement the
`foldMap`{.fixed} function, which is also a part of the
`Foldable`{.fixed} type class. The `foldMap`{.fixed} function has the
following type:

~~~~ {.haskell:hs name="code"}
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
~~~~

Its first parameter is a function that takes a value of the type that
our foldable structure contains (denoted here with `a`{.fixed}) and
returns a monoid value. Its second parameter is a foldable structure
that contains values of type `a`{.fixed}. It maps that function over the
foldable structure, thus producing a foldable structure that contains
monoid values. Then, by doing `mappend`{.fixed} between those monoid
values, it joins them all into a single monoid value. This function may
sound kind of odd at the moment, but we'll see that it's very easy to
implement. What's also cool is that implementing this function is all it
takes for our type to be made an instance of `Foldable`{.fixed}. So if
we just implement `foldMap`{.fixed} for some type, we get
`foldr`{.fixed} and `foldl`{.fixed} on that type for free!

This is how we make `Tree`{.fixed} an instance of `Foldable`{.fixed}:

~~~~ {.haskell:hs name="code"}
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
~~~~

![find the visual pun or whatever](lyah/accordion.png)

We think like this: if we are provided with a function that takes an
element of our tree and returns a monoid value, how do we reduce our
whole tree down to one single monoid value? When we were doing
`fmap`{.fixed} over our tree, we applied the function that we were
mapping to a node and then we recursively mapped the function over the
left sub-tree as well as the right one. Here, we're tasked with not only
mapping a function, but with also joining up the results into a single
monoid value by using `mappend`{.fixed}. First we consider the case of
the empty tree — a sad and lonely tree that has no values or sub-trees.
It doesn't hold any value that we can give to our monoid-making
function, so we just say that if our tree is empty, the monoid value it
becomes is `mempty`{.fixed}.

The case of a non-empty node is a bit more interesting. It contains two
sub-trees as well as a value. In this case, we recursively
`foldMap`{.fixed} the same function `f`{.fixed} over the left and the
right sub-trees. Remember, our `foldMap`{.fixed} results in a single
monoid value. We also apply our function `f`{.fixed} to the value in the
node. Now we have three monoid values (two from our sub-trees and one
from applying `f`{.fixed} to the value in the node) and we just have to
bang them together into a single value. For this purpose we use
`mappend`{.fixed}, and naturally the left sub-tree comes first, then the
node value and then the right sub-tree.

Notice that we didn't have to provide the function that takes a value
and returns a monoid value. We receive that function as a parameter to
`foldMap`{.fixed} and all we have to decide is where to apply that
function and how to join up the resulting monoids from it.

Now that we have a `Foldable`{.fixed} instance for our tree type, we get
`foldr`{.fixed} and `foldl`{.fixed} for free! Consider this tree:

~~~~ {.haskell:hs name="code"}
testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )
~~~~

It has `5`{.fixed} at its root and then its left node is has `3`{.fixed}
with `1`{.fixed} on the left and `6`{.fixed} on the right. The root's
right node has a 9 and then an `8`{.fixed} to its left and a 10 on the
far right side. With a `Foldable`{.fixed} instance, we can do all of the
folds that we can do on lists:

~~~~ {.haskell:hs name="code"}
ghci> F.foldl (+) 0 testTree
42
ghci> F.foldl (*) 1 testTree
64800
~~~~

And also, `foldMap`{.fixed} isn't only useful for making new instances
of `Foldable`{.fixed}; it comes in handy for reducing our structure to a
single monoid value. For instance, if we want to know if any number in
our tree is equal to `3`{.fixed}, we can do this:

~~~~ {.haskell:hs name="code"}
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
True
~~~~

Here, `\x -> Any $ x == 3`{.fixed} is a function that takes a number and
returns a monoid value, namely a `Bool`{.fixed} wrapped in
`Any`{.fixed}. `foldMap`{.fixed} applies this function to every element
in our tree and then reduces the resulting monoids into a single monoid
with `mappend`{.fixed}. If we do this:

~~~~ {.haskell:hs name="code"}
ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree
False
~~~~

All of the nodes in our tree would hold the value `Any  False`{.fixed}
after having the function in the lambda applied to them. But to end up
`True`{.fixed}, `mappend`{.fixed} for `Any`{.fixed} has to have at least
one `True`{.fixed} value as a parameter. That's why the final result is
`False`{.fixed}, which makes sense because no value in our tree is
greater than `15`{.fixed}.

We can also easily turn our tree into a list by doing a
`foldMap`{.fixed} with the `\x -> [x]`{.fixed} function. By first
projecting that function onto our tree, each element becomes a singleton
list. The `mappend`{.fixed} action that takes place between all those
singleton list results in a single list that holds all of the elements
that are in our tree:

~~~~ {.haskell:hs name="code"}
ghci> F.foldMap (\x -> [x]) testTree
[1,3,6,5,8,9,10]
~~~~

What's cool is that all of these trick aren't limited to trees, they
work on any instance of `Foldable`{.fixed}.

-   [Functionally Solving Problems](functionally-solving-problems.html)
-   [Table of contents](chapters.html)
-   [A Fistful of Monads](a-fistful-of-monads.html)

