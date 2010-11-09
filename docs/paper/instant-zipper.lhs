\documentclass[a4paper,twocolumn]{article}

%include polycode.fmt


\begin{document}

\title{Instant Zipper}
\author{Ruud Koot\quad Bram Schuur}

\maketitle

\section{Introduction}

\section{Using the Instant Zipper}

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
>    fill' :: (Typeable a) => Derivative f -> a -> Maybe f
    
Important here is the Typeable class-constraint for the value we want to plug in. The Derivative can have a hole of any type (see explaination of Derivative), and thus we can not guarantee, using the type system, that the type of the element we want to plug in is indeed the type of the hole. To overcome this problem we require both the hole and the item we want to plug in to be Typeable so we can use cast. This is also why the fill' function returns a Maybe type, beacause casting may fail. Now the instance for Rec (the hole) looks as follows:

> instance (Typeable a) => Fillable (Rec a) where
>    fill' Recursive v = Rec <$> cast v

Note that we give a similar instance for Var. Rec and Var are treated equally in our zipper library.


Another important function for our zipper is the first function. The first function takes a value and splits this value into its leftmost value and the corresponding context. It effectively punches a hole in a value. A similar problem as with the fill' function arises here. We want to produce a hole with type a. Here again we use casting to achieve this. 

> class Firstable f where
>    first' :: (Zipper a) => f -> Maybe (a, Derivative f)

Notice that when this first' function is invoked with a type for a that is not present in one of the holes of Derivative f, the function will produce Nothing. The function will create a hole and context for the types that are given, where the hole is the leftmost hole with the specified type. 

> instance (Firstable f, Firstable g) => Firstable (f :*: g) where
>    first' (l :*: r) = mapSnd (flip C1 r) <$> first' l
>                   <|> mapSnd (C2 l) <$> first' r
>
> instance (Typeable f) => Firstable (Rec f) where
>    first' (Rec v) = (\x -> (x, Recursive)) <$> cast v

The product instance tries to create a result to the left first, if it fails to the right. If both fail no context can be given.


The fact that first' and fill' works this way is both a curse and a blessing. we will later see that this approach need 

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

