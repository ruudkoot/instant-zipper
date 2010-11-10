\documentclass[a4paper,twocolumn]{article}

%include polycode.fmt
%format ... = "\ ... \ "

\begin{document}

\title{Instant Zipper}
\author{Ruud Koot\quad Bram Schuur}

\maketitle

\section{Introduction}
In this paper we present an implementation of a generic zipper type implemented in Instant Generics.


\section{Using the Instant Zipper}

Before moving on the implementation details of our zipper we present an example of how it can be used to edit a heterogeneous datatype. We present a similar example as given by Adams for his Scrap Your Zippers, to make it clear what the differences of our approach are.

We start with a number of datatypes represeting a university departement and its employees:

> type Salary    = Float
> type Manager   = Employee
> type Name      = String
> 
> data Dept = D Manager [Employee]
>     deriving (Eq, Show, Typeable)
>
> data Employee = E Name Salary
>     deriving (Eq, Show, Typeable)

To define a zipper over these datatype we will need to:

\begin{enumerate}
    \item Make the datatypes an instance of Typeable.
    \item Derive the Instant Generics representation using Template Haskell:
> $(deriveAll ''Dept)
> $(deriveAll ''Employee)
    \item Declare a Family for all the types we want to navigate into:
> data Fam a where
>     Dept      :: Fam Dept
>     Employee  :: Fam Employee
>     Salary    :: Fam Salary
>     Name      :: Fam Name
>     List      :: (Show a) => Fam a -> Fam [a]
>
> deriving instance Show (Fam a)
>    
> instance Family Fam
    \item Finally, we instantiate the the datatypes as Zippers:
> instance Zipper Dept
> instance Zipper Employee
\end{enumerate}

While we could easily have designed our zipper in such a way as to not require the Family instance, this approach will make our navigations more elegant in appearance. Furthermore, we believe most of this can be automatically derived using Template Haskell.

We can now give a concrete value for the Dept type:

> dept :: Dept
> dept = D doaitse [johan, sean, pedro]
>     where doaitse, johan, sean, pedro :: Employee
>           doaitse  = E "Doaitse"  8000
>           johan    = E "Johan"    8000
>           sean     = E "Sean"     2600
>           pedro    = E "Pedro"    2400
          
and edit it using the zipper:

> fixDept :: Maybe Dept
> fixDept  =    return (enter dept)
>          >>=  down   Employee
>          >>=  down   Name
>          >>=  return . setHole "Prof. dr. Swierstra"
>          >>=  right  Salary
>          >>=  return . setHole 9000.0
>          >>=  return . up
>          >>=  return . up
>          >>=  downR (List Employee)
>          >>=  down  (List Employee)
>          >>=  down  (List Employee)
>          >>=  down   Employee
>          >>=  downR  Salary
>          >>=  return . setHole 100.0
>          >>=  return . leave

The diffence with SYZ is clear: we need to annotate our navigation function with types. Giving a wrong type can cause a failure (Nothing) at runtime, but can also be important when moving down along the spine of list, or, down into a value in a list. The getHole and setHole operations can now be statically typed however.

\section{Implementing the Instant Zipper}

A zipper consists of the part of the data structure which is currently \emph{in focus}, together with the \emph{context} in which it appears. Alternatively we can view the part of the data structure which is in focus as filling a \emph{hole} in the context.

> data Loc hole root ... = Loc
>   { focus    :: hole
>   , context  :: Context hole root ... }

Our zipper can navigate over \emph{heterogeneous datatypes}. Therefore the zipper is parameterized over both the type currently in focus, as well as the type of the datastructure we originally entered (the \emph{root}). Our implementation requires a third parameter, the necesity of which we can only explained after having seen the navigation functions. We will ignore it for the time being.

The context is a stack of \emph{one-hole contexts}:

> data Context hole root ... where
>     Empty :: Context hole hole ...
>     Push  :: (Zipper parent) =>
>           Derivative (Rep parent)
>       ->  Context   parent  root  ...
>       ->  Context   hole    root  ...

If the context stack is empty the whole datastructure will be in focus, as if we just entered it, and the its type will be that of both the hole and the root.

As we move down into the datastructure we peel of the constructors (minus one hole) for the structure in focus and push it onto the context stack. The type of a datatype with one hole per constructor is given by the \emph{derivative} of that datatype \cite{mcbride03}.

\subsection{Derivatives}

We store the derivative of a datatype in an associated datatype:

> class Derivable f where
>     data Derivative f :: * 

Calculating the derivative in the sum-of-products view used by Instant Generics is straigforward, following the algebraic rules:
    
> instance Derivable Int where
>     data Derivative Int
>    
> instance Derivable U where
>     data Derivative U
>    
> instance (Derivable f, Derivable g) =>
>   Derivable (f :+: g) where
>     data Derivative (f :+: g)
>       =  CL (Derivative f)
>       |  CR (Derivative g)
>    
> instance (Derivable f, Derivable g) =>
>   Derivable (f :*: g) where
>     data Derivative (f :*: g)
>       =  C1 (Derivative f) g
>       |  C2 f (Derivative g)
>    
> instance (Derivable a) =>
>   Derivable (Rec a) where
>     data Derivative (Rec a) = Recursive
>    
> instance (Derivable a) =>
>   Derivable (Var a) where
>     data Derivative (Var a) = Variable

\subsection{Navigation}

In Instant Generics we create functions on representation types by creating a type class wrapping the function and creating an instance
of this type-class for each member of the representation type. An important function for zipper is the fill function, which retrieves a 
context and a value and puts the value into the hole in the context, resulting in a new value. 

> class Fillable f where
>    fill' :: (Typeable a) => 
>          Derivative f -> a -> Maybe f
    
Important here is the Typeable class-constraint for the value we want to plug in. The Derivative can have a hole of any type (see explaination of Derivative), and thus we can not guarantee, using the type system, that the type of the element we want to plug in is indeed the type of the hole. To overcome this problem we require both the hole and the item we want to plug in to be Typeable so we can use cast. This is also why the fill' function returns a Maybe type, beacause casting may fail. Now the instance for Rec (the hole) looks as follows:

> instance (Typeable a) => 
>              Fillable (Rec a) where
>    fill' Recursive v = Rec <$> cast v

Note that we give a similar instance for Var. Rec and Var are treated equally in our zipper library.


Another important function for our zipper is the first function. The first function takes a value and splits this value into its leftmost value and the corresponding context. It effectively punches a hole in a value. A similar problem as with the fill' function arises here. We want to produce a hole with type a. Here again we use casting to achieve this. 

> class Firstable f where
>    first' :: (Zipper a) => 
>           f -> Maybe (a, Derivative f)

Notice that when this first' function is invoked with a type for a that is not present in one of the holes of Derivative f, the function will produce Nothing. The function will create a hole and context for the types that are given, where the hole is the leftmost hole with the specified type. 

> instance (Firstable f, Firstable g) => 
>               Firstable (f :*: g) where
>    first' (l :*: r) = 
>          mapSnd (flip C1 r) <$> first' l
>      <|> mapSnd (C2 l) <$> first' r
>
> instance (Typeable f) => 
>            Firstable (Rec f) where
>    first' (Rec v) = 
>       (\x -> (x, Recursive)) <$> cast v

The product instance tries to create a result to the left first, if it fails to the right. If both fail no context can be given.


As we have seen, the behaviour of first' and fill' is determined by the type with which they are invoked. This because the way casting works. We are now going to use these functions which operate on values and contexts and use them to create functions which work on the Loc datatype. We will see that the extra type-information needed to use the first' and fill' functions drives the way in which we define our up and down functions and ultimately our Loc and Context datatypes.


We would like to implement the up function (the function that goes one hole up in the context) in the following way:

> up (Loc h (Push c cs)) = 
>       (\x -> Loc (to x) cs) <$> fill' c h

We fill the top hole with the current value, yielding a new value and the rest of the context. We cannot use casting here to get the types right, we need explicit typing on all variables because the fill' function already contains a cast, and thus needs fully explicit types. If we try to use another cast to get the types right, we get an ambiguous type occurence (similar to the read . show problem). Of course a solution here is to require explicit type annotations on every use of the up function, but we can be smarter. Using a trick similar to \cite{HList-HW04}, extended with GADTs to guide the pattern matching, we can keep all the types of the previous contexts in our Context GADT. We build a list at the type-level, which contains the type of each context we have visited, this way, when we go up, we can retrieve the type in this type-level list instead of asking the user for extra information. The result looks as follows:

> --List at type-level
> data Epsilon = Epsilon
> data (:<:) c cs = c :<: cs
> data Context ... l where
>    Empty :: Context ... Epsilon
>    Push  :: (Zipper parent) 
>               => Derivative (Rep parent)
>               -> Context ... cs
>               -> Context ... (parent :<: cs)
>
> up :: (Zipper h, Zipper h') => 
>           Loc h r (h' :<: c) -> Loc h' r c
> up (Loc h (Push c cs)) = 
>     fromJust $ (\x -> Loc (to x) cs) <$> fill' c h

Note that the additional type information also prevents us form going up in an empty context. We can also add fromJust because we can be sure the result will be correct.


We still left some information open in our Context datatype, this has to do with the leave function. The leave function repeatedly applies the up function until we are left with our original datatype.

> leave (Loc h Empty) = h
> leave loc@(Loc _ (Push _ _)) = leave . up  $ loc

To be able to give this function a result type, we need to know the type of the root of our tree. What we also need is the guarantee that the type of the top hole equals the type of the root type, else the Empty case won't typecheck. Thus we get:

> data Loc hole root c = 
>    Loc { focus :: hole, 
>          context :: Context hole root c }
>
> data Context hole root l where
>    Empty :: Context hole hole Epsilon
>    Push  :: (Zipper parent) => 
>            Derivative (Rep parent)
>         -> Context parent root cs
>         -> Context hole root (parent :<: cs)
>
> leave :: (Zipper h) => Loc h r c -> r
> leave (Loc h Empty) = h
> leave loc@(Loc _ (Push _ _)) = leave . up  $ loc


The last important function that we need to implement is the down function, which goes down into the current hole, adding a context to the context stack and creates a new hole. 
We use the first' function to do this. In the previous up/leave functions we could avoid the need for type-annotations by chosing our datastructures cleverly and maintaining type information. With
the down function there is no way for us to infer what type we want our new hole to have, so we need type-annotations from the user. The normal down function looks as follows:

> down :: (Zipper h, Zipper h') => 
>           Loc h r c -> Maybe (Loc h' r (h :<: c))
> down (Loc h cs) = 
>   (\(h', c) -> Loc h' (Push c cs)) <$> first (from h)

Specifically, the type of h' is the type which we cannot infer. This system is unworkable if we have to annotate each call to down with its full type signature. Thus we introduce a more
convenient way for specifying the type of h', a phantom variable! This is a variable which is not used but only there for the extra type information, the function now looks like this:

> down' :: (Zipper h, Zipper h') => 
>   h' -> Loc h r c -> Maybe (Loc h' r (h :<: c))
> down' _ (Loc h cs) = 
>   (\(h', c) -> Loc h' (Push c cs)) <$> first (from h)

Filling in the argument with a static type is now sufficient, this function could be used in such a way:

> downInt = down (undefined :: Int)

Thus there are 2 ways to call the down function, with or without phantom argument type.


The functions right and left, which move the hole right or left into a new hole of a new (specified) type, are implemented in the same manner as the down function. For these two functions we also need
extra type annotations.

\subsection{Context}

HCtx again

\subsection{Tying the recursive knot}

Having defined all operations on our we need to ``tie the recursive knot'':

> class  (  Representable    f
>        ,  Typeable         f
>        ,  Fillable   (Rep  f)
>        ,  Firstable  (Rep  f)
>        ,  Nextable   (Rep  f)
>        ,  Lastable   (Rep  f)
>        ,  Prevable   (Rep  f)) => Zipper f
>
> instance Zipper Int
> instance Zipper Char
> instance Zipper Float
> instance (Zipper a) => Zipper [a]

\section{Future reasearch}

\section{Related work}

\section{Conclusion}

\section*{Acknowledgements}

\bibliographystyle{plain}
\bibliography{instant-zipper}

\end{document}

