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

fill, first, down, up, ...

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

