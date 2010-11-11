\documentclass{beamer}

%include polycode.fmt
%format ... = "\ ... \ "

\begin{document}

\title{Instant Zipper}
\subtitle{}
\author{Bram Schuur\quad Ruud Koot}
\date{12 November 2010}

\maketitle

\begin{frame}{TOC}
    \tableofcontents
\end{frame}

\begin{frame}
    \begin{itemize}      
        \item Zipper for heterogenous types
        \item i.e. The Zipper can traverse into structures of any type
        \item (Much like SYB zipper)
        \item But with some extras!
    \end{itemize}
\end{frame}

\section{Usage}

\begin{frame}{Usage}

Given our familiar example..

> type Salary    = Float
> type Manager   = Employee
> type Name      = String
> 
> data Dept = D Manager [Employee]
> data Employee = E Name Salary

> dept :: Dept
> dept = D doaitse [johan, sean, pedro]
>     where doaitse, johan, sean, pedro :: Employee
>           doaitse  = E "Doaitse"  8000
>           johan    = E "Johan"    8000
>           sean     = E "Sean"     2600
>           pedro    = E "Pedro"    2400

\end{frame}

\begin{frame}{Usage}

We can fix it using our zipper as follows:

> fixDept :: Either String Dept
> fixDept  =    return (enter dept)
>          >>=  down   Employee
>          >>=  down   Name
>          >>=  return . setHole "Prof. dr. Swierstra"
>          >>=  right  Salary
>          >>=  return . setHole 9000.0
>          >>=  return . leave

\end{frame}

\section{Implementation}

\begin{frame}{Under the hood}
    \begin{itemize}
        \item Basic ingredients of a Zipper:
        \item We define our Zipper onto the generic representation of Instant Generics
        
        \item Basic operations to create/manipulate contexts
        \item Navigation functions
    \end{itemize}    
\end{frame}

\begin{frame}{Zipper}
    \begin{itemize}
    \item A zipper consists of the part of the data structure which is currently \emph{in focus}, together with the \emph{context} in which it appears. \\
    
> data Loc ... = Loc { focus :: hole, context  :: Context ... }
    
    \item The Context is a stack of \emph{one-hole contexts}
    \item A type for all one-hole contexts of a datatype can be found by taking its derivative  

> data Context ... where
>     Empty :: Context ...
>     Push  ::  Derivative (Rep parent) --One-hole context
>           ->  Context parent ... 
>           ->  Context   ...

    \end{itemize}
\end{frame}

\begin{frame}{Derivative}
    We calculate the derivative data structure of a structure representation using associated datatypes.
    
> class Derivable f where
>     data Derivative f :: *    
>
> instance (Derivable f, Derivable g) => Derivable (f :+: g) where
>     data Derivative (f :+: g) =  
>           CL (Derivative f) | CR (Derivative g)
>    
> instance (Derivable f, Derivable g) => Derivable (f :*: g) where
>     data Derivative (f :*: g) =  
>           C1 (Derivative f) g | C2 f (Derivative g) 
>
> instance (Derivable a) => Derivable (Rec a) where
>     data Derivative (Rec a) = Recursive

Note that we throw away the type information at the recursive position!
\end{frame}

\begin{frame}{Operations on Contexts}
    \begin{itemize}
        \item Now we can define some functions for one-hole contexts which help us create navigation functions for our zipper
        \item The function fill takes a one-hole context and a value and inserts the value into the hole, yielding the original value.
    
> class Fillable f where
>    fill :: (Typeable a) => Derivative f -> a -> Maybe f
>
> instance (Fillable f, Fillable g) => Fillable (f :*: g) where
>    fill (C1 c r) v = flip (:*:) r  <$> fill c v
>    fill (C2 l c) v = (l :*:)       <$> fill c v
>
> instance (Typeable a) => Fillable (Rec a) where
>    fill CRec v = Rec <$> cast v

        \item Note that we need casting and Typeable to make this function work!
        \item The use of cast here has a great impact on the design of the rest of the Zipper
        \item This is why the function return a Maybe
    \end{itemize}
\end{frame}

\begin{frame}{First}
    \begin{itemize}
        \item Another important function is the first function
        \item This function takes a value and splits it into the leftmost value within this value and the corresponding context
        
> class Firstable f where
>    first :: (Zipper a) => 
>           f -> Maybe (a, Derivative f)
>
> instance (Firstable f, Firstable g) => Firstable (f :*: g) where
>    first (l :*: r)  =    mapSnd (flip C1 r)  <$> first l
>                     <|>  mapSnd (C2 l)       <$> first r
>
> instance (Typeable f) => Firstable (Rec f) where
>    first (Rec v) = (\x -> (x, Recursive)) <$> cast v        

    \end{itemize}
\end{frame}

\begin{frame}{Casting \& Ambiguous types}
    \begin{itemize}
        \item The usage of casting poses us with a problem
        \item Suppose we write the following

> id x = uncurry fill <$> first x

        \item This will produce an "ambiguous type" error
        \item This is because we use cast after another cast and the types in between cannot be inferred
        \item To solve this, we need explicit typing information
        \item This means the user will have provide typing annotations when some functions are invoked
        \item We employ several techniques to limit the amount of type annotations required
    \end{itemize}
\end{frame}

\begin{frame}{Context}
    \begin{itemize}
        \item One of them is extending the context datatype with a type-level list, so the types of context values are maintained
        \item We also maintain type information on what the hole and root types are, just like the SYB zipper
    \end{itemize}
    
> data Loc hole root c = 
>    Loc { focus :: hole, context :: Context hole root c }
>
> data Context hole root l where
>    Empty :: Context hole hole Epsilon
>    Push  :: (Zipper parent)  =>  Derivative (Rep parent)
>                              ->  Context parent root cs
>                              ->  Context hole root (parent :<: cs)
    
\end{frame}

\begin{frame}{Up}
    \begin{itemize}
        \item With or extended context, we can easily write the up function, which goes one item up in the context stack

> up :: (Zipper h, Zipper h') => Loc h r (h' :<: c) -> Loc h' r c
> up (Loc h (Push c cs)) = 
>       fromJust $ (\x -> Loc (to x) cs) <$> fill c h
        
        \item Note that the type-level list ensures that we cannot go up in the empty context
        \item We can also be sure that the fill succeeds, because else our program wouldn't typecheck, thus we can use fromJust
        \item The suer does not have to type this function explicitly, the type information is maintained in the context
    \end{itemize}
\end{frame}

\begin{frame}{Down}
    \begin{itemize}
        \item The down function navigates down into the current value 

> down :: (Zipper h, Zipper h') => 
>          Loc h r c -> Maybe (Loc h' r (h :<: c))
> down (Loc h cs) = 
>       (\(h', c) -> Loc h' (Push c cs)) <$> first (from h)
        
        \item The type of h' (the type of the hole we navigate into) cannot be known up front
        \item We need user-annotations for this to type-check
        \item We can avoid having to write the whole type signature

> down' :: (Zipper h, Zipper h') => 
>   h' -> Loc h r c -> Maybe (Loc h' r (h :<: c))
> down' _ (Loc h cs) = down
        
        \item Here we have a phantom variable h' which is not used but is there to guide the type-checking

> downInt = down' (undefined :: Int)
        
    \end{itemize}
\end{frame}

\begin{frame}{GADTs}
\end{frame}

\begin{frame}
    \begin{itemize}
        \item Some of the operations on our zipper may fail
        \item For example, when tying to go down into a type which isn't there
        \item Instead of returning Nothing, we return an error message with information on what operation went wrong
        \item If the user uses the GADT typing system, additional typing information is given
    \end{itemize}
\end{frame}

\begin{frame}{Future work}
    \begin{itemize}
        \item Write TH code to generate Family instances
        \item Employ type families to catch errors at compile time
    \end{itemize}
\end{frame}

\end{document}

