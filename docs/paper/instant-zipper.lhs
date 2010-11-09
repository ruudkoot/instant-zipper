\documentclass[a4paper,twocolumn]{article}

%include polycode.fmt


\begin{document}

\title{Instant Zipper}
\author{Ruud Koot\quad Bram Schuur}

\maketitle

\section{Introduction}

\section{Using the Instant Zipper}

\section{Implementing the Instant Zipper}

A zipper consists of the part of the data structure which is currently \emph{in focus}, together with the \emph{one-hole context} in which it appears:

> data Loc h r c = Loc { val :: h, ctxs :: HCtx h r c }

Loc, HCtx, Derivable

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

