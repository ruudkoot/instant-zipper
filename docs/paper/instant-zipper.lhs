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

The context is a stack of \emph{one-hole contexts}:

> data Context hole root ... where
>     Empty :: Context hole hole ...
>     Push  :: (Zipper parent) =>
>           Derivative (Rep parent)
>       ->  Context   parent  root  ...
>       ->  Context   hole    root  ...



\subsection{Navigation}

fill, first, down, up, ...

\subsection{Context}

HCtx again

\subsection{Tying the recursive knot}

class Zipper

\section{Future reasearch}

\section{Related work}

\section{Conclusion}

\section*{Acknowledgements}

trata\cite{mcbride03}

\bibliographystyle{plain}
\bibliography{instant-zipper}

\end{document}

