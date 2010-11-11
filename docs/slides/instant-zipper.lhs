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
        \item i.e. It can traverse into any type
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
        \item Derivative data structure for representaing contexts
        \item Basic operations to create/manipulate contexts
        \item Navigation functions
    \end{itemize}    
\end{frame}

\begin{frame}{Derivative}
    We calculate the derivative data structure of a strcuture representation using associated datatypes.
    
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

\begin{frame}
\end{frame}

\begin{frame}
\end{frame}



\end{document}

