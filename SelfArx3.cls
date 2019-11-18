% ---------------------------------------------------------------------
% Conference proceedings and article templates for
% personal open-archiving activities
% September 2012
% ---------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SelfArx}[25/01/2012, v1.0]
\RequirePackage{ifthen}
\RequirePackage{calc}
\AtEndOfClass{\RequirePackage{microtype}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions*
\LoadClass{article}
\RequirePackage{ifpdf}      % Needed to pick between latex and pdflatex

%----------------------------------------------------------------------
%	FONTS
%----------------------------------------------------------------------

\usepackage{savesym}
\RequirePackage{times}      % Loads the Times-Roman Fonts
\savesymbol{circledS}
%\RequirePackage{mathptmx}   % Loads the Times-Roman Math Fonts
\RequirePackage[charter]{mathdesign}
\restoresymbol{whatev}{circledS}
\usepackage{listings}

%----------------------------------------------------------------------
%	VARIOUS USEFUL PACKAGES
%----------------------------------------------------------------------

\RequirePackage[utf8x]{inputenc}
\RequirePackage{amsmath,amsfonts,amssymb}
\RequirePackage{graphicx}
\RequirePackage[usenames,dvipsnames,svgnames,table]{xcolor}
\RequirePackage[english]{babel}
\RequirePackage{booktabs}
\RequirePackage{etoolbox}

%----------------------------------------------------------------------
%	MARGINS	
%----------------------------------------------------------------------
	  	  
\RequirePackage[left=2cm,%
                right=2cm,%
				top=2.25cm,%
				bottom=2.25cm,%
				headheight=11pt,%
				letterpaper]{geometry}%
				
%----------------------------------------------------------------------
%	FIGURES AND TABLES CAPTIONS
%----------------------------------------------------------------------

\RequirePackage[labelfont={bf,sf,small},%
                labelsep=period,%
                justification=raggedright]{caption}
\setlength{\abovecaptionskip}{0pt}
\setlength{\belowcaptionskip}{0pt}
				
%----------------------------------------------------------------------
%	PAGE HEADER
%----------------------------------------------------------------------

\RequirePackage{fancyhdr}  % Needed to define custom headers/footers
\RequirePackage{lastpage}  % Number of pages in the document
\pagestyle{fancy}          % Enables the custom headers/footers
% Headers
\chead{}%
\rhead{\footnotesize\sffamily\bfseries\textcolor{acolor}{\@PaperTitle}}%
\lhead{\footnotesize\sffamily\bfseries\textcolor{acolor}{\@JournalInfo}}
% Footers
\lfoot{\footnotesize\sffamily \authaa, \authbb, \authcc, \ \theyear}
\cfoot{\includegraphics[width=10pt]{ZZ_Images/cc.png} \includegraphics[width=10pt]{ZZ_Images/by.png} \includegraphics[width=10pt]{ZZ_Images/nc.png} \includegraphics[width=10pt]{ZZ_Images/sa.png}}%
\rfoot{\footnotesize \sffamily Page \thepage\ of\ \pageref{LastPage}}%
\renewcommand{\headrulewidth}{0pt}% % No header rule
\renewcommand{\footrulewidth}{0pt}% % No footer rule

%----------------------------------------------------------------------
%	SECTION/SUBSECTION/PARAGRAPH SET-UP
%----------------------------------------------------------------------

\RequirePackage[explicit]{titlesec}
\titleformat{\section}
  {\color{acolor}\large\sffamily\bfseries}
  {}
  {0em}
  {\colorbox{bcolor!10}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\centering\arabic{section}. #1}}}
  []
\titleformat{name=\section,numberless}
  {\color{acolor}\large\sffamily\bfseries}
  {}
  {0em}
  {\colorbox{bcolor!10}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\centering#1}}}
  []  
\titleformat{\subsection}
  {\color{acolor}\sffamily\bfseries}
  {\thesubsection}
  {0.5em}
  {#1}
  []
\titleformat{\subsubsection}
  {\sffamily\bfseries}
  {\thesubsubsection}
  {0.5em}
  {#1}
  []    
\titleformat{\paragraph}[runin]
  {\sffamily\bfseries}
  {}
  {0em}
  {#1} 
\titleformat{\subparagraph}[runin]
  {\sffamily\bfseries}
  {}
  {0em}
  {#1} 
  
\titlespacing*{\section}{0pc}{3ex \@plus4pt \@minus3pt}{5pt}
\titlespacing*{\subsection}{0pc}{2.5ex \@plus3pt \@minus2pt}{0pt}
\titlespacing*{\subsubsection}{0pc}{2ex \@plus2.5pt \@minus1.5pt}{0pt}
\titlespacing*{\paragraph}{0pc}{1.5ex \@plus2pt \@minus1pt}{10pt}

%----------------------------------------------------------------------
%	TABLEOFCONTENTS SET-UP
%----------------------------------------------------------------------
\newlength{\tocsep} 
\setlength\tocsep{1.5pc} % Sets the indentation of the sections in the table of contents
\setcounter{tocdepth}{3} % Three levels in the table of contents section: sections, subsections and subsubsections

\usepackage{titletoc}
\contentsmargin{0cm}
\titlecontents{section}[\tocsep]
  {\addvspace{4pt}\bfseries\sffamily}
  {\contentslabel[\thecontentslabel]{0.7\tocsep}}
  {}
  {\hfill\thecontentspage}
  []
\titlecontents{subsection}[\tocsep]
  {\small \sffamily}
  {\qquad\contentslabel[\thecontentslabel]{\tocsep}}
  {}
  {\ \titlerule*[.5pc]{.}\ \thecontentspage}
  []
\titlecontents*{subsubsection}[0.5\tocsep]
  {\footnotesize\sffamily}
  {}
  {}
  {}
  [ \textbullet\ ]  

%----------------------------------------------------------------------
%	MULTIPLE AUTHOR SET
%----------------------------------------------------------------------  
  
\newcount\@authcnt
\newcount\@tmpcnt\@tmpcnt\z@

\def\@affiliation{%
  \ifnum\@tmpcnt<\@authcnt
   \global\advance\@tmpcnt1
    \raggedright \csname @auth\romannumeral\the\@tmpcnt\endcsname\hfill\\%
   \let\next\@affiliation \vskip1pt
  \else
   \let\next\relax
  \fi
\next}
	 
\newcommand{\affiliation}[1]{%
    \global\advance\@authcnt1
    \expandafter\gdef\csname @auth\romannumeral\the\@authcnt\endcsname
    {#1}}

	
%----------------------------------------------------------------------
%	LIST CONTROL
%----------------------------------------------------------------------  

\RequirePackage{enumitem}
%\setlist{nolistsep} % Uncomment to remove spacing between items in lists (enumerate, itemize)

%----------------------------------------------------------------------
%	ABSTRACT+AUTHOR FRAME
%----------------------------------------------------------------------  

\newcommand{\PaperTitle}[1]{\def\@PaperTitle{#1}}
\newcommand{\Archive}[1]{\def\@Archive{#1}}
\newcommand{\Authors}[1]{\def\@Authors{#1}}
\newcommand{\JournalInfo}[1]{\def\@JournalInfo{#1}}
\newcommand{\Abstract}[1]{\def\@Summary{#1}}
\newcommand{\Keywords}[1]{\def\@Keywords{#1}}

% ---------------------------------------------------------------------

\renewcommand{\@maketitle}{%
\twocolumn[{%
\thispagestyle{empty}%
\vskip-36pt%
{\raggedright\small\sffamily\bfseries\@JournalInfo\\\par}%
\vskip20pt%
{\raggedright\color{acolor}\sffamily\bfseries\fontsize{20}{25}\selectfont \@PaperTitle\par}%
\vskip10pt%
{\raggedright\color{acolor}\sffamily\fontsize{12}{16}\selectfont  \@Authors\par}%
\vskip18pt%
\fcolorbox{acolor}{white}{%
\parbox{\textwidth-2\fboxsep-2\fboxrule}{\centering%
\colorbox{bcolor!10}{%
\parbox{\textwidth-4\fboxsep-2\fboxrule}{%
\ifx\@Keywords\@empty%
\sffamily\textbf{\large \abstractname}\\ {\large\@Summary}%
\else%
\sffamily\textbf{\large \abstractname}\\ {\large \@Summary}\\[4pt]%
\textbf{\normalsize \keywordname}\\{\normalsize\@Keywords}\\[4pt]%
\textbf{\normalsize \fundingname}\\{\normalsize\@Funding}
\fi%
}%
}%
\vskip4pt%
\begingroup%
\raggedright\sffamily%
\small\@affiliation\par%
\endgroup%%
}%
}%
\vskip25pt%
}]%
}%


%----------------------------------------------------------------------
%	REFERENCES
%----------------------------------------------------------------------  

% Change section header to paragraph header
\patchcmd{\thebibliography}{\section*{\refname}}{\paragraph*{\refname}}{}{}
% Remove brackets from numbering in List of References
\renewcommand{\@biblabel}[1]{\bfseries\color{acolor}\textsuperscript{[#1]}}
%\setlength{\bibitemsep}{0cm}
\let\oldbibliography\thebibliography
\renewcommand{\thebibliography}[1]{%
%\addcontentsline{toc}{section}{\refname}%
\oldbibliography{#1}%
\setlength\itemsep{0pt}}%



