\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{si671}
[2017/12/26 LaTeX template for Assignments for SI671 Course]
\LoadClass{article}
\RequirePackage{titlesec}
\RequirePackage{ifthen}

\newcommand{\headerx}[2]{\title{SI 630 - Winter 2018\\#1}
\author{#2}
\newcommand{\comment}[1]{}
%\ifthenelse{\equal{#3}{}}{}{\date{#3}}
%\date{#3}
\maketitle}


\newcommand{\header}[2]{\title{SI 630 - Winter 2018\\#1}
\author{#2}
\newcommand{\comment}[1]{}
%\ifthenelse{\equal{#3}{}}{}{\date{#3}}
%\date{#3}
\maketitle}


\titleformat{\section}         % Customise the \section command 
  {\Large\raggedright\bfseries} % Make the \section headers large (\Large),
                               % small capitals (\scshape) and left aligned (\raggedright)
  {}{0em}                      % Can be used to give a prefix to all sections, like 'Section ...'
  {}                           % Can be used to insert code before the heading
%[\titlerule] % Inserts a horizontal line after the heading

