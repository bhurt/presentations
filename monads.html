<HTML>
<HEAD>
<TITLE>A Monad Tutorial</TITLE>
</HEAD>
<BODY>
<P><DIV ALIGN="CENTER"><H1>Brian's Monad Tutorial</H1></DIV></P>

<P><DIV ALIGN="CENTER"><H2>The Problems With Monad Tutorials</H2></DIV></P>

<P><OL><LI>Bad metaphors- "A monad is like a burrito wrapped in a space
suit- it protects you from the toxic waste, and you can just reach in
and grab all the apples you want."  Solution: no metaphors, just
code.</LI>

<LI>Excess of category theory- "A monad is just a monoid in the category
of endofunctors, what's the problem?"  Solution: <B>NO CATEGORY
THEORY!</B></LI>

<LI>A need for "hands on" experience- see <A HREF="https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/">Abstract Intuition and the Monad Tutoral Fallacy</A>.  
Solution: stay concrete, lots of code, homework exercises. For our
concrete examples, I borrow from the blog post 
<A HREF="http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html">You Could Have Invented Monads! (And Maybe You Already Have.)</A>.
</LI>

<LI>The "four faces" of monads: <OL><LI>Where they come from (category
theory)</LI><LI>How they are implemented</LI><LI>How they are
used</LI><LI>How they are thought about or designed with.</LI></OL>
Solution: address each face separately, except the category theory which
we don't need to address.</LI>

<LI>Unfamiliarity with the concept of functions as data- many Monads are
based around treating functions as data, and this is rare in other
languages, and thus unfamiliar to most programmers.

</OL></P>

<P><DIV ALIGN="CENTER"><H2>Motivating Example #1</H2></DIV></P>

<P>We have three functions, f, g, and h, so f calls g, and g calls h:</P>
<P><CODE><PRE>
    f :: ... -&gt; something
    f ... =
        ...
        let r = g ... in
        ...
        something

    g :: ... -&gt; somethingElse
    g ... =
        ...
        let r = h ... in
        ...
        somethingElse

    h :: ... -&gt; whatever
    h ... =
        ...
</PRE></CODE></P>

<P>Now h needs some information (of type blah) from where f is called.  So
now we need to thread that information through the whole calling stack:</P>

<P><CODE><PRE>
    f :: ... -&gt; blah -&gt; something
    f ... blah =
        ...
        let r = g ... blah in
        ...
        something

    g :: ... -&gt; blah -&gt; somethingElse
    g ... blah =
        ...
        let r = h ... blah in
        ...
        somethingElse

    h :: ... -&gt; blah -&gt; whatever
    h ... blah =
        ...
</PRE></CODE></P>

<P><DIV ALIGN="CENTER"><H2>The Reader Type</H2></DIV></P>

<P><CODE><PRE>
    type Reader s a = s -&gt; a
</PRE></CODE></P>

<P>So now we can clean up our types a little:</P>

<P><CODE><PRE>
    f :: ... -&gt; Reader blah something
    ...

    g :: ... -&gt; Reader blah somethingElse
    ...

    h :: ... -&gt; Reader blah whatever
    ...
</PRE></CODE></P>

<P>What we need is a special "reader let", which automatically appends the
blah value.  This is hard.  But, if we have:</P>

<P><CODE><PRE>
    let x = foo in bar
</PRE></CODE></P>

<P>this is the same as:</P>

<P><CODE><PRE>
    (\x -&gt; bar) foo
</PRE></CODE></P>

<P>So, instead of a special let, let's consider a special apply.  Flip the
order of the args, so we can do:</P>

<P><CODE><PRE>
    apply foo (\x -&gt; bar)
</PRE></CODE></P>

<P>We want foo to have the type <CODE>Reader s a</CODE>, and we want the
whole thing to have a return type of <CODE>Reader s b</CODE>, so we can
go:</P>

<P><CODE><PRE>
    f :: ... -&gt; Reader blah something
    f ... =
        ...
        apply (g ...)
            (\r -&gt;
                ...
                something)
</PRE></CODE></P>

<P>and have apply do the right thing.  And we can make an initial
cut at the code:</P>

<P><CODE><PRE>
apply :: (Reader r a) -&gt; (a -&gt; b) -&gt; Reader r b
-- apply :: (r -&gt; a) -gt; (a -&gt; b) -&gt; r -&gt; b
apply f g s = g (f s)
</PRE></CODE></P>

<P>This type signature looks familiar.  Where have I seen it before?</P>

<P><CODE><PRE>
    fmap :: Functor f =&gt; (a -&gt; b) -&gt; f a -&gt; f b
</PRE></CODE></P>

<P>But problem: what happens if we want to call two functions and thread
the variable into both?</P>

<P><CODE><PRE>
    f :: ... -&gt; Reader blah something
    f ... =
        ...
        apply (g ...)
            (\r -&gt;
                ...
                apply (g2 ...)
                    (\r2 -&gt;
                        ...
                        something))
</PRE></CODE></P>

<P>I can use fmap for the original <CODE>apply</CODE>, I want
<CODE>apply</CODE> to have the type: <CODE>(Reader r a) -&gt; (a -&gt;
Reader r b) -&gt; Reader r b</CODE>.  The <CODE>apply</CODE> function
then becomes:</P>

<P><CODE><PRE>
    apply :: Reader r a -&gt; (a -&gt; Reader r b) -&gt; Reader r b
    -- apply :: (r -&gt; a) -&gt; (a -&gt; r &gt; b) -&gt; r -&gt; b
    apply f g s = g (f s) s
</PRE></CODE></P>

<P>But this causes another problem:</P>

<P><CODE><PRE>
    f :: ... -&gt; Reader blah something
    f ... =
        ...
        apply (g ...)
            (\r -&gt;
                ...
                apply (g2 ...)
                    (\r2 -&gt;
                        ...
                        something)) -- Type error right here!
</PRE></CODE></P>

<P>The <CODE>something</CODE> expression has type
<CODE>something</CODE>, but we want it to have type <CODE>Reader blah
something</CODE>.  We could user <CODE>fmap</CODE>, or we could just
provide a fixup function:</P>

<P><CODE><PRE>
    fixup :: a -&gt; Reader r a
    -- fixup :: a -&gt; r -&gt; a
    fixup x _ = x
</PRE></CODE></P>

<P>Now we can go:</P>

<P><CODE><PRE>
    f :: ... -&gt; Reader blah something
    f ... =
        ...
        apply (g ...)
            (\r -&gt;
                ...
                apply (g2 ...)
                    (\r2 -&gt;
                        ...
                        fixup something))
</PRE></CODE></P>

<P>I will, at this point, just rename <CODE>apply</CODE> to be the operator
<CODE>&gt;&gt;=</CODE>, and the <CODE>fixup</CODE> function to
<CODE>return</CODE>.  This should start looking familiar:</P>

<P><CODE><PRE>
    f :: ... -&gt; Reader blah something
    f ... =
        ...
        (g ...) &gt;&gt;=
            (\r -&gt;
                ...
                (g2 ...) &gt;&gt;=
                    (\r2 -&gt;
                        ...
                        return something))
</PRE></CODE></P>

<P>Two last things we need, if we want to have Reader be an abstract
data type (i.e., we don't let people see it's just a function under the
hood).  We need some way to call f with our initial blah.  We need some
function of type:</P>

<P><CODE><PRE>
    runReader :: Reader r a -&gt; r -&gt; a
</PRE></CODE></P>

<P>And, we need some way to access the read variable down where we need
it (down in <CODE>h</CODE>).  If we had a function:</P>

<P><CODE><PRE>
    ask :: Reader r r
</PRE></CODE></P>

<P>We could write <CODE>h</CODE> like:</P>

<P><CODE><PRE>
    h :: ... -&gt; Reader blah whatever
    h ... = ask (\blah -&gt; ...)
</PRE></CODE></P>

<P><B>HOMEWORK:</B> Implement the <CODE>runReader</CODE> and
<CODE>ask</CODE> functions.</P>

<P><DIV ALIGN="CENTER"><B>Performance</B></DIV></P>

<P>One of the concerns people have with writing this abstract code is
the loss of performance- we're introducing a lot of unnecessary closures
and lambda expressions.  Rest assured, the ghc compiler optimizes all of
this, and recovers code equalivalent to our original hand-threaded
code.</P>

<P><DIV ALIGN="CENTER"><H2>Motivating Example #2</H2></DIV></P>

<P>Same as above, except now h doesn't just take a blah, it returns an
updated blah (common with immutable data):

<P><CODE><PRE>
    f :: ... -&gt; blah -&gt; (something, blah)
    f ... blah =
        ...
        let (r, blah') = g ... blah in
        ...
        (something, blah')

    g :: ... -&gt; blah -&gt; (somethingElse, blah)
    g ... blah =
        ...
        let (r, blah') = h ... blah in
        ...
        (somethingElse, blah')

    h :: ... -&gt; blah -&gt; (whatever, blah)
    h ... blah =
        ...
</PRE></CODE></P>

<P><DIV ALIGN="CENTER"><H2>The State Type</H2></DIV></P>

<P>We pull the same trick, just a little more complicated.  We
define:</P>

<P><CODE><PRE>
    type State s a = s -&t; (a, s)

    (&gt;&gt;=) :: State s a -&gt; (a -&gt; State s b) -&gt; State s b
    -- (&gt;&gt;=) :: (s -&gt; (a, s)) -&gt; (a -&gt; s -&gt; (b, s)) -&gt; s -&gt; (b, s)
    f &gt;&gt;= g = \s -&gt;
        let (x, s') = f s
        g x s'

    return :: a -&gt; State s a
    -- return :: a -&gt; s -&gt; (a, s)
    return x s = (x, s)

</PRE></CODE></P>

<P>So now I can do:</P>

<P><CODE><PRE>
    f :: ... -&gt; State blah something
    f ... =
        ...
        (g ...) &gt;&gt;=
            (\r -&gt;
                ...
                return something)

    g :: ... -&gt; State blah somethingElse
    g ... blah =
        ...
        (h ...) &gt;&gt;=
            (\r -&gt;
                ...
                somethingElse)

</PRE></CODE></P>

<P><B>HOMEWORK:</B> To make <CODE>State</CODE> an abstract type (like we
made <CODE>Reader</CODE>), we need three new functions:
<P><CODE><PRE>
    runState :: State s a -&gt; s -&gt; (a, s)
    get :: State s s
    put :: s -&gt; State s ()
</PRE></CODE></P>
<P>Write these functions.  With these funtions, how would you write
<CODE>h</CODE> (approximately)?</P>

<P><DIV ALIGN="CENTER"><H2>The Monad Typeclass</H2></DIV></P>

<P>Now we have a problem.  We have two definitions (with very similar
types) of the <CODE>&gt;&gt;=</CODE> operator:</P>

<P><CODE><PRE>
    (&gt;&gt;=) :: Reader r a -&gt; (a -&gt; Reader r b) -&gt; Reader r b
    (&gt;&gt;=) :: State s a -&gt; (a -&gt; State s b) -&gt; State s b
</PRE></CODE></P>

<P>And two copies of the <CODE>return</CODE> function:</P>
<P><CODE><PRE>
    return :: a -&gt; Reader r a
    return :: a -&gt; State s a
</PRE></CODE></P>

<P>The solution is to introduce a type class:</P>

<P><CODE><PRE>
    class Monad m where
        (&gt;&gt;=) :: m a -&gt; (a -&gt; m b) -&gt; m b
        return :: a -&gt; m a
</PRE></CODE></P>

<P>Our implementation of <CODE>Reader</CODE> would then be:</P>

<P><CODE><PRE>
    type Reader r a = r -&gt; a
    instance Monad (Reader r) where
        -- We replace the m with (Reader r), so
        -- m a -&gt; (a -&gt; m b) -&gt; m b
        -- (Reader r) a -&gt; (a -&gt; (Reader r) b) -&gt; (Reader r) b
        -- Reader r a -&gt; (a -&gt; Reader r b) -&gt; Reader r b
        f &gt;&gt;= g = \r -&gt; g (f r) r

        -- a -&gt; m a
        -- a -&gt; (Reader r) a
        -- a -&gt; Reader r a
        return x = \_ -&gt; x
</PRE></CODE></P>

<P>And <CODE>State</CODE> would be:</P>
<P><CODE><PRE>
    type State s a = s -&gt; (a, s)
    instance Monad (State s) where
        f &gt;&gt; g = \s -&gt; let (x, s') = f s in g x s'
        return x = \s -&gt; (x, s)
</PRE></CODE></P>

<P><B>HOMEWORK:</B> Is it possible to write the <CODE>fmap</CODE>
function using only <CODE>&gt;&gt;=</CODE> and <CODE>return</CODE>?
What does this mean about the relationship between <CODE>Functor</CODE>
and <CODE>Monad</CODE>?</P>

<P><B>HOMEWORK:</B> Read up on the <CODE>Applicative</CODE> type class-
can it be implemented using only <CODE>&gt;&gt;=</CODE> and
<CODE>return</CODE>?  What does that say about the relationship between
<CODE>Applicative</CODE> and <CODE>Monad</CODE>?</P>

<P><B>HOMEWORK:</B> The <CODE>Reader</CODE> type is "half of" the
<CODE>State</CODE> type.  Can you implement the "other half"- the
<CODE>Writer</CODE> type?  How would you deal with the state not being
written, or being written multiple times?</P>

<P><DIV ALIGN="CENTER"><H2>A Digression</H2></DIV></P>

<P>Consider the following code for a moment:</P>
<P><CODE><PRE>
    instance Monad [] where
        [] &gt;&gt;= _ = []
        (x : xs) &gt;&gt;= f = (f x) ++ (xs &gt;&gt;= f)
        return x = [ x ]
</PRE></CODE></P>

<P>So lists are monads.</P>

<P><B>HOMEWORK:</B> Can you write similar definitions for
<CODE>Maybe</CODE>?  How about <CODE>Either String</CODE>?</P>

<P><DIV ALIGN="CENTER"><H2>Side effects and the IO Type</H2></DIV></P>

<P>Problem: the only way to guarantee that expression A is evaluated
before expression B is to have expression B depend upon the value
produced by A.  But side-effecting computations have ordering dependency
instead of data dependency- we want to print the <CODE>"password:
"</CODE> and <EM>then</EM> read the input line.  Many side-effecting
expressions do not even have a sensible data output- OK,
<CODE>getLine</CODE> returns a <CODE>String</CODE>, but what does
<CODE>putStr</CODE> return?</P>

<P>Solution: we model the ordering dependency as a "fake" data
dependency.  We introduce a special value, of type <CODE>World#</CODE>
which represents "the state of the outside world".  So now:</P>

<P><CODE><PRE>
putStr :: String -&gt; World# -&gt; ((), World#)
getLine :: World# -&gt; (String, World#)
</PRE></CODE></P>

<P>All side-effecting computations now return at least one peice of
"meaningful" data- the updated state of the world.</P>

<P>Problem: Now we need to thread the <CODE>World#</CODE> value through
your code.  If only there was a way to do that...</P>

<P><CODE><PRE>
type IO a = State World# a

putStr :: String -&gt; IO ()
getLine :: IO String
</PRE></CODE></P>

<P><B>HOMEWORK:</B> Consider the following Haskell program:</P>

<P><CODE><PRE>
    main = getLine &gt;&gt;=
            (\line1 -&gt;
                getLine &gt;&gt;=
                    (\line2 -&gt;
                        putStrLn line1))
</PRE></CODE></P>

<P>If I run it, and give it the input:<PRE>
hello
world
</PRE> (note, as two separate lines), what does the program output, and
why?  What does a value of the type <CODE>IO String</CODE> represent?</P>

<P><DIV ALIGN="CENTER"><H2>The Do Notation</H2></DIV></P>

<P>Problem: continually have to introduce new lambda expressions to bind
the results of monadic computations is a pain.  It sure would be nice to
have a "monadic let".  And Haskell gives us that with the Do
notation:</P>

<P><CODE><PRE>
    main = do
            line1 &lt;- getLine
            line2 &lt;- getLine
            putStrLn line1
</PRE></CODE></P>

<P>The do notation "desugars" to a sequence of &gt;&gt;= calls and
lambda expressions.  So the code:</P>

<P><CODE><PRE>
    do
        var &lt;- expr
        ...
</PRE></CODE></P>

<P>becomes:</P>

<P><CODE><PRE>
    expr &gt;&gt;=
        (\var -&gt;
            do
                ...)
</PRE></CODE></P>

<P>Likewise,</P>

<P><CODE><PRE>
    do
        expr
        ...
</PRE></CODE></P>

<P>becomes:</P>

<P><CODE><PRE>
    expr &gt;&gt;=
        (\ _ -&gt;
            do
                ...)
</PRE></CODE></P>

<P>And:</P>

<P><CODE><PRE>
    do
        let var = expr
        ...
</PRE></CODE></P>

<P>(Note the lack of an "in" there!) becomes: </P>

<P><CODE><PRE>
    let var = expr in
        do
            ...
</PRE></CODE></P>

<P>The last statement in a do block is a pure expression which is the
final result.  This is normally a <CODE>return</CODE> call, but it
can be a control flow expression that itself contains do blocks:</P>


<P><CODE><PRE>
main = do
    putStrLn "What is your guess?"
    guess &lt;- getLine
    -- Now, this is the last statement in this do block
    if (guess == 3) then
        -- Which contains two do blocks inside
        do
            putStrLn "Correct!"
            return ()
    else
        do
            putStrLn "Wrong!"
            main
</PRE></CODE></P>

<P><B>HOMEWORK:</B> Rewrite the guessing game example above to
explicitly thread the <CODE>World#</CODE> value around (i.e. desugar the
do blocks, inline the definition of <CODE>&gt;&gt;=</CODE>).  How does
the last call to main work?</P>

<P><DIV ALIGN="CENTER"><H2>Designing with Monads</H2></DIV></P>
<P><DIV ALIGN="CENTER"><H2>Monad Transformers</H2></DIV></P>

<P>So, <CODE>IO</CODE> is "impure"- but a lot of the monads we've seen
are pure- <CODE>State</CODE>, <CODE>Reader</CODE>, <CODE>Writer</CODE>,
<CODE>Maybe</CODE>, <CODE>Either</CODE>, <CODE>List</CODE>, and so on.
They do mix fairly promiscuously, and cleanly, with other monads.  Can
we express that?</P>

<P><CODE><PRE>
    type StateT s m a = s -&gt; m (a, s)

    instance Monad m =&gt; Monad StateT s m where
        f &gt;&gt;= g = \s -&gt; do
            (x, s') &lt;- f s
            g x s'
        return x s = return (x, s)
</PRE></CODE></P>

<P>This is the "State Transformer"- it stacks on top of another monad
and threads a state variable through.</P>

</BODY>
