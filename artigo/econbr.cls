%% This is file `econbr.cls' v0.0.3 - derived from `econsocart.cls' v1.4.1
%%
%% LaTeX 2e style file for the processing of LaTeX2e files
%% of the Brazilian Review of Econometrics (BRE) and the Revista Brasileira de Economia (RBE).
%%
%% Original TeX programming: Edgaras Sakuras, VTeX, Lithuania
%% ECONBR adaptation: Eduardo Horta, Brazil
%% Please submit bugs or your comments to eduardo.ohorta@outlook.com
%%
%% The original distribution is located at:
%% https://www.e-publications.org/es/support/
%%
%% Copyright (c) 2022 UAB "VTeX"
%%
%% Permission is granted to copy, distribute and/or modify this
%% software under the terms of the LaTeX Project Public License
%% (LPPL), version 1.3c or any later version.
%%
\NeedsTeXFormat{LaTeX2e}[2000/06/01]
\ProvidesClass{econbr}
% 
\RequirePackage{etoolbox}
% 
\newif\if@supplement \@supplementfalse
\newif\if@onlineonly \@onlineonlyfalse
\newif\if@supplement@onlineonly \@supplement@onlineonlyfalse
\newif\if@sici \@sicifalse
\newif\if@biblink \@biblinkfalse
\newif\ifendbibitem@ \endbibitem@false
\newif\if@contcaption
\newif\if@nameyear \@nameyearfalse
\newif\if@load@amsmath \@load@amsmathfalse
\newif\if@seceqn \@seceqnfalse
\newif\if@wwwviewable
\newif\if@wwwprintable
\newif\if@urllinks \@urllinksfalse
\newif\if@bookmarks
\newif\if@nowps@md \@nowps@mdfalse
\newif\if@xmlfmbm \@xmlfmbmfalse
\newif\if@open@access \@open@accessfalse
\newif\if@displaystyle@addons \@displaystyle@addonstrue
\newif\ifrbe@layout \rbe@layoutfalse
\newif\ifte@layout \te@layoutfalse
\newif\ifbre@layout \bre@layoutfalse
\newif\iffirstitem \firstitemtrue
\newif\ifseconditem \seconditemfalse
\newif\if@enum
\newif\if@longlist
\newif\if@fleqn
\newif\if@tabcell@hook
\newif\if@firstkeywordinlist \@firstkeywordinlisttrue
\newif\if@allsections \@allsectionsfalse
\newif\if@labelled \@labelledfalse
\newif\if@unsdlbl
\newif\if@econbr@querypage \@econbr@querypagetrue
\newif\ifnot@ead@star
\newif\if@printead@opt
\newif\if@econbr@draft \@econbr@draftfalse
\newif\if@econbr@final \@econbr@finalfalse
\newif\if@econbr@numberlines \@econbr@numberlinesfalse
\newif\if@econbr@debug \@econbr@debugfalse
% 
% Class cfg load
% \InputIfFileExists{econbr-vtex.cfg}{}{\input{econbr.cfg}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONTENTS OF FILE econbr.cfg
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\protected\def\econbr@error#1{\ClassError{econbr}{#1}\@ehc}
\let\get@changed@counter@info@for@xml\@gobbletwo
% 
\def\econbr@amsmath@vtexmth{}
% 
\def\econbr@pdfoptions{%
    \PassOptionsToPackage{setpagesize=false}{hyperref}%
    }
% 
\def\econbr@nolistindent@mattson@amsmath{}
% 
\def\econbr@fileinfo{%
    \RequirePackage{fix-cm}%
    \ifcsundef{jobstart@date@info}%
        {\xdef\jobstart@date@info{\the\year\two@digits{\the\month}\two@digits{\the\day}}}%
        {}%
    \let\pkgattr@setkeys\@gobble
    \def\pkgattr@namedef{\long\gdef}%
    \def\pkgattr@namedef@def##1{\expandafter\pkgattr@namedef\csname ##1\endcsname}%
    \protected\def\setpkgattr{\@ifnextchar[{\@setpkgattr}{\@@setpkgattr}}%
    \long\def\@setpkgattr[##1]##2##3##4{%
        \pkgattr@setkeys{##1}%
        \pkgattr@namedef@def{##2@##3}{##4}%
        }%
    \long\def\@@setpkgattr##1##2##3{%
        \pkgattr@namedef@def{##1@##2}{##3}%
        }%
    }
%
\def\econbr@setpagesize{%
    % luatex with version>85
    \ifcsundef{pagewidth}{}{\global\pagewidth=\paperwidth}%
    \ifcsundef{pageheight}{}{\global\pageheight=\paperheight}%
    % pdftex, xetex
    \ifcsundef{pdfpagewidth}{}{\global\pdfpagewidth=\paperwidth}%
    \ifcsundef{pdfpageheight}{}{\global\pdfpageheight=\paperheight}%
    % dvi
    \AtBeginDvi{\special{papersize=\the\paperwidth,\the\paperheight}}%
    }
\def\econbr@def@proof@papersize{%
    \def\proof@papersize{%
        \voffset=-1in%
        \hoffset=-1in%
        \global\paperwidth=\paper@width
        \global\paperheight=\paper@height
        \evensidemargin=\paperwidth
        \advance\evensidemargin by -\textwidth
        \divide\evensidemargin by \tw@
        \oddsidemargin=\evensidemargin
        \topmargin=1.25in%
        \advance\topmargin by \headheight@text@gap
        \econbr@setpagesize
        }%
    \if@econbr@draft
        \ifbre@layout
            \renewcommand\normalsize{%
                \@setfontsize\normalsize\@xiipt{14.4\p@ \@plus .36\p@ \@minus 0.36\p@}%
                \abovedisplayskip=12\p@ \@plus 2.4\p@ \@minus 2.4\p@
                \abovedisplayshortskip=7.2\p@ \@plus 2.4\p@
                \belowdisplayshortskip=7.2\p@ \@plus 2.4\p@
                \belowdisplayskip=\abovedisplayskip
                }%
            \normalsize
            \renewcommand\small{%
                \@setfontsize\small{10.8}{13.2\p@ \@plus 0.24\p@ \@minus 0.24\p@}%
                \abovedisplayskip=9\p@ \@plus 4.8\p@ \@minus 1.2\p@
                \belowdisplayskip=\abovedisplayskip
                \abovedisplayshortskip=\abovedisplayskip
                \belowdisplayshortskip=\abovedisplayskip
                }%
            \renewcommand\footnotesize{%
                \@setfontsize\footnotesize{9.6}{12\p@ \@plus .12\p@ \@minus .12\p@}%
                \abovedisplayskip=7.2\p@ \@plus 4\p@ \@minus .12\p@
                \belowdisplayskip=\abovedisplayskip
                \abovedisplayshortskip=\abovedisplayskip
                \belowdisplayshortskip=\abovedisplayskip
                }%
        \else
            \renewcommand\normalsize{%
                \@setfontsize\normalsize\@xiipt{16.2\p@ \@plus .6\p@ \@minus .6\p@}%
                \abovedisplayskip=12\p@ \@plus 2.4\p@ \@minus 2.4\p@
                \abovedisplayshortskip=7.2\p@ \@plus 2.4\p@
                \belowdisplayshortskip=7.2\p@ \@plus 2.4\p@
                \belowdisplayskip=\abovedisplayskip
                }%
            %
            \renewcommand\small{%
                \@setfontsize\small{10.8}{14.94\p@ \@plus .36\p@ \@minus .36\p@}%
                \abovedisplayskip=9\p@ \@plus 4.8\p@ \@minus 1.2\p@
                \belowdisplayskip=\abovedisplayskip
                \abovedisplayshortskip=\abovedisplayskip
                \belowdisplayshortskip=\abovedisplayskip
                }%
            %
            \renewcommand\footnotesize{%
                \@setfontsize\footnotesize{9.6}{13.2\p@ \@plus .24\p@ \@minus .24\p@}%
                \abovedisplayskip=7.2\p@ \@plus 4.8\p@ \@minus 1.2\p@
                \belowdisplayskip=\abovedisplayskip
                \abovedisplayshortskip=\abovedisplayskip
                \belowdisplayshortskip=\abovedisplayskip
                }%
        \fi
        \renewcommand\scriptsize{\@setfontsize\scriptsize{8.4}{9.6}}%
        \renewcommand\tiny{\@setfontsize\tiny{6}{7.2}}%
        \renewcommand\large{\@setfontsize\large{14.4}{16.8}}%
        \renewcommand\Large{\@setfontsize\Large{16.8}{19.2}}%
        \renewcommand\LARGE{\@setfontsize\LARGE{19.2}{21.6}}%
        \renewcommand\huge{\@setfontsize\huge{21.6}{24}}%
        \renewcommand\Huge{\@setfontsize\Huge{24}{28.8}}%
        \parindent=12\p@
    \fi
    }
%
\def\econbr@def@crc@papersize{%
    \def\crc@papersize{%
        \voffset=-1in%
        \hoffset=-1in%
        \global\paperheight=\trim@height
        \global\paperwidth=\trim@width
        \econbr@setpagesize
        }%
    }
\def\econbr@def@view@and@wwwprint@papersize{%
    \def\view@papersize{%
        \voffset=-1in%
        \hoffset=-1in%
        \global\paperwidth=\view@width
        \global\paperheight=\view@height
        \econbr@setpagesize
        }%
    \def\wwwprint@papersize{%
        \global\paperwidth=\paper@width
        \global\paperheight=\paper@height
        \econbr@setpagesize
        }%
    }
% 
\def\econbr@pagecrop{%
    \if@econbr@draft
        \proof@papersize
        \def\baselinestretch{1.4}%
        \ifbre@layout
            \footnotesep=11.66\p@
        \else
            \footnotesep=13\p@
        \fi
    \else
        \if@wwwprintable
            \wwwprint@papersize
        \else
            \if@wwwviewable
                \view@papersize
            \else
                \crc@papersize
            \fi
        \fi
    \fi
    }
% 
\def\econbr@graphicx{%
    \RequirePackage{graphicx}%
    }
% 
\def\econbr@helvet{%
    \def\econbr@putx{put}%
    \ifbre@layout
        \renewcommand{\rmdefault}{ptm}%
    \fi
    \RequirePackage{textcase}%
    \RequirePackage[scaled=.9]{helvet}%
    }
% 
\def\econbr@mathdutc{%
    \def\defaultscriptratio{.76}%
    \def\defaultscriptscriptratio{.6}%
    \DeclareMathSizes{5}{6}{6}{6}%
    \DeclareMathSizes{6}{6}{6}{6}%
    \DeclareMathSizes{7}{6.8}{6}{6}%
    \DeclareMathSizes{8}{8}{6.8}{6}%
    \DeclareMathSizes{9}{9}{7.6}{6}%
    \DeclareMathSizes{10}{10}{7.6}{6}%
    \DeclareMathSizes{10.95}{10.95}{7.6}{6}%
    \DeclareMathSizes{12}{12}{9}{7}%
    \DeclareMathSizes{14.4}{14.4}{10}{8}%
    \DeclareMathSizes{17.28}{17.28}{12}{9}%
    \DeclareMathSizes{20.74}{20.74}{14.4}{10}%
    \DeclareMathSizes{24.88}{24.88}{17.28}{12}%
    }
% 
\def\econbr@frontmatter@macros{%
    \def\sep@key@value##1=##2/?/##3{\setpkgattr{##3}{##1}{##2}}%
    \def\do@option@list##1##2{%
        \@for\curr@option:={##2}\do{%
            \expandafter\sep@key@value\curr@option/?/{##1}\relax
            }%
        }%
    \def\setvaluelist##1##2{%
        \@tempcnta=0\relax
        \@for\@curr@val:=##2\do{%
            \advance\@tempcnta by1\relax
            \expandafter\protected@xdef\csname ##1@item@\the\@tempcnta\endcsname{\@curr@val}%
            }%
        \expandafter\protected@xdef\csname ##1@item@0\endcsname{\the\@tempcnta}%
        }%
    \xdef\getitemvalue##1##2{\noexpand\csname ##1@item@##2\endcsname}%
    \let\check@localdefs@status\relax
    \def\startlocaldefs{%
        \gdef\check@localdefs@status{\dbl@error{Missing command endlocaldefs!}}%
        \gdef\startlocaldefs{\dbl@error{Command startlocaldefs already exist!}}%
        \makeatletter
        }%
    \def\endlocaldefs{%
        \makeatother
        \ifx\check@localdefs@status\relax
            \gdef\check@localdefs@status{\dbl@error{Missing command startlocaldefs!}}%
        \else
            \global\let\check@localdefs@status\relax
        \fi
        \gdef\endlocaldefs{\dbl@error{Command endlocaldefs already exist!}}%
        }%
        \AtBeginDocument{\check@localdefs@status}%
    \def\dbl@error##1{\@latex@error{##1}\@ehc\special{mt:error ##1}}%
    \def\@issue{0}%
    \def\@volume{0}%
    \newcount\@pubyear
    \newcount\@copyear
    \bgroup
        \global\@pubyear=\number\year
        \def\tmp@a{\expandafter\@gobbletwo\the\year}%
        \global\@copyear\number\tmp@a\relax
    \egroup
    \def\set@pagerange{}%
    \def\saferef##1{%
        \expandafter\safe@setref\csname r@##1\endcsname\@firstoftwo{##1}%
        }%
    \let\safe@setref\@setref
    \def\safelabel##1{%
        \@bsphack
        \protected@write\@auxout{}{%
            \string\thanksnewlabel{##1}{{\@currentlabel}{\thepage}}%
            }%
        \@esphack
        }%
    % {url}{text}
    \def\safe@href##1##2{##2}%
    \let\thanksnewlabel\newlabel
    % we want to use various counters:
    \def\usethankscounter##1{%
        \@ifundefined{current@thankscounter}%
            {\gdef\previous@thankscounter{##1}}%
            {\xdef\previous@thankscounter{\current@thankscounter}}%
        \def\current@thankscounter{##1}%
        }%
    \def\restorethankscounter{\xdef\current@thankscounter{\previous@thankscounter}}%
    \newcounter{thanks}%
    \def\thethanks{\arabic{thanks}}%
    \usethankscounter{thanks}%
    \let\thanksmark@shape\relax
    \def\thanksmark@fmt##1{\@textsuperscript{\normalfont\thanksmark@shape##1}}%
    \def\thanksref@sep{,}%
    \def\thankref@hyperlink##1{\saferef{##1thanks}}%
    \def\thanks@hypertarget##1{}%
    \def\thanksmark@link##1##2{##2}%
    \def\thanksref{\@ifnextchar[{\@tempswatrue\@thanksref}{\@tempswafalse\@thanksref[]}}%
    \def\@thanksref[##1]##2{%
        \if@tempswa% []
            \let\thanksref@first\relax
            \@for\@@refb:=##2\do{%
                \ifx\relax\thanksref@first\relax\xdef\thanksref@first{\@@refb}\fi
                }%
            \thanksmark@link{\thanksref@first}{\thanksmark@fmt{##1}}%
        \else
            \let\@tempa\@empty
            \thanksmark@fmt{%
                \@for\@tempb:=##2\do{%
                    \@tempa \let\@tempa\thanksref@sep
                    \edef\@tempb{\expandafter\@firstofone\@tempb\@empty}%
                    \thankref@hyperlink{\@tempb}%
                    }%
                }%
        \fi
        }%
    \def\thanksmark{\@ifnextchar[{\@tempswatrue\@thanksmark}{\@tempswafalse\@thanksmark[]}}%
    \def\@thanksmark[##1]##2{%
        \@thankslabel[##1]{##2}%
        \thanksmark@fmt{\expandafter\saferef{##2thanks}\thanks@hypertarget{##2}}%
        }%
    \def\thankslabel{\@ifnextchar[{\@tempswatrue\@thankslabel}{\@tempswafalse\@thankslabel[]}}%
    \def\@thankslabel[##1]##2{%
        \if@tempswa% []
            \protected@edef\@currentlabel{##1}%
        \else
            \refstepcounter{\current@thankscounter}%
        \fi
        \safelabel{##2thanks}%
        }%
    \def\thankstext{\@ifnextchar[{\@tempswatrue\@thankstext}{\@tempswafalse\@thankstext[]}}%
    \def\thanks@thefnmark##1{%
        \begingroup
            \unrestored@protected@xdef\@thefnmark{\saferef{##1}}%
        \endgroup
        }%
    % ead
    \RequirePackage{keyval}%
    \def\email@text{e-mail: }%
    \def\url@text{url: }%
    \def\fullurl@text{url: }%
    \def\ead@sep{;~}%
    \let\ead@size\relax
    \def\printead@fmt##1{##1}%
    \define@key{ead}{email}[true]{\def\ead@type{email}}%
    \define@key{ead}{url}[true]{%
        \def\@tempa{fullurl}%
        \ifx\ead@type\@tempa
        \else
            \def\ead@type{url}%
        \fi
        }%
    \define@key{ead}{label}{\def\ead@label{##1}}%
    \define@key{ead}{text}{%
        \bgroup
            \def\\{\string\break}%
            \def\break{\string\break}%
            \protected@edef\@currentlabel{##1}%
            \safelabel{\ead@label @\ead@type text}%
        \egroup
        }%
    \define@key{ead}{nopdflink}[true]{%
        \protected@edef\@currentlabel{nolink}%
        \safelabel{\ead@label @nopdflink}%
        }%
    \DeclareRobustCommand\ead[2][label= ,email]{{%
        \def\ead@type{email}% default
        \checkead@prefix##2://\end
        \def\texttildelow{\noexpand\texttildelow}%
        \setkeys{ead}{##1}%
        \csname ead@hook\endcsname
        \protected@edef\@currentlabel{##2}%
        \safelabel{\ead@label @\ead@type}%
        }}%
    \def\checkead@prefix##1://##2\end{\ifx.##2.\else\def\ead@type{fullurl}\fi}%
    % printead
    \DeclareRobustCommand{\printead}{%
        \@ifstar{\not@ead@starfalse\@printead}{\not@ead@startrue\@printead}%
        }%
    \def\@printead{\@ifnextchar[{\@printead@opttrue\@@printead}{\@printead@optfalse\@@printead[]}}%
    \def\@@printead[##1]##2{{%
        \if@printead@opt%[]
            \def\safe@href@text{##1}%
            \not@ead@starfalse
        \fi
        \let\prev@ead@text\relax
        \let\@ead@sep\relax
        \let\ead@text\relax
        \let\ead@prefix\relax
        \def\ead@type{}%
        \@tempcnta=0%
        \let\sv@safe@href\safe@href
        \printead@fmt{%
            \@for\ead@ref:=##2\do{%
                \advance\@tempcnta by1%
                \let\safe@href\sv@safe@href
                \@ead@sep\let\@ead@sep\ead@sep
                \@ifundefined{r@\ead@ref @nopdflink}%
                    {}%
                    {\def\safe@href####1##2{####2}}%
                \@ifundefined{r@\ead@ref @email}%
                    {}%
                    {\let\ead@text\email@text\def\ead@type{email}\def\ead@prefix{mailto:}}%
                \@ifundefined{r@\ead@ref @url}%
                    {}%
                    {\let\ead@text\url@text\def\ead@type{url}\def\ead@prefix{http://}}%
                \@ifundefined{r@\ead@ref @fullurl}%
                    {}%
                    {\let\ead@text\fullurl@text\def\ead@type{fullurl}\def\ead@prefix{}}%
                \ifx\prev@ead@text\ead@text \let\ead@text\relax \fi
                \csname printead@hook\endcsname
                \if@printead@opt
                    \ifnum\@tempcnta>1%
                        \@latex@error{Command \@backslashchar printead[]{e1} could 
                            have only one parameter "e1"!}\@eha
                    \fi
                \else
                    \@ifundefined{r@\ead@ref @\ead@type text}%
                        {\def\safe@href@text{\saferef{\ead@ref @\ead@type}}}%
                        {\def\safe@href@text{\saferef{\ead@ref @\ead@type text}}}%
                    \fi
                \ifnot@ead@star \ead@text \fi
                \bgroup
                    \ead@size\def\null{}%
                    \safe@href{\ead@prefix\saferef{\ead@ref @\ead@type}}{\safe@href@text}%
                \egroup
                \@ifundefined{ead@text}%
                    {}%
                    {\let\prev@ead@text\ead@text}%
                }%
            }%
        }}%
    \def\normaltext{%
        \let\\=\@normalcr
        \leftskip=\z@
        \@rightskip=\z@
        \rightskip=\@rightskip
        \parfillskip\@flushglue
        }%
    \def\nohyphen{%
        \pretolerance=\@M
        \tolerance=\@M
        \hyphenpenalty=\@M
        \exhyphenpenalty=\@M
        }%
    }
% 
\def\econbr@list@macros{%
    \def\list@parindent{1pc}%
    % longlist
    \def\longlist@indent{\list@parindent}%
    \def\longlist@parindent{\list@parindent}%
    \def\longlist@labelsep{.5em}%
    \def\longlist@topsep{\smallskipamount}%
    \def\longlist@rightskip{\z@}%
    \let\longlist@hook\relax
    \newcounter{longlist}%
    \def\thelonglist{\@roman\c@longlist}%
    \def\labellonglist{\thelonglist)}%
    \def\longlist{\@ifnextchar[{\@longlist}{\@longlist[]}}%
    \def\@longlist[##1]{%
        \list
            {\labellonglist}%
            {%
                \usecounter{longlist}%
                \def\makelabel####1{{\hfill\normalfont ####1}}%
                \ifx.##1.%
                    \setcounter{longlist}{3}% 
                    \settowidth\labelwidth{\labellonglist}%
                    \setcounter{longlist}{0}%
                \else
                    \settowidth{\labelwidth}{##1}%
                \fi
                \labelsep=\longlist@labelsep
                \itemindent=\labelwidth
                \advance\itemindent\labelsep
                \advance\itemindent by\longlist@indent
                \rightmargin=\z@
                \leftmargin=\z@
                \partopsep=\z@
                \parsep=\z@
                \itemsep=\z@
                \topsep=\longlist@topsep
                \listparindent=\longlist@parindent
                \@rightskip=\longlist@rightskip
                \longlist@hook
            }%
        }%
    \let\endlonglist =\endlist
    % enumerate
    \def\enumerate@indent{\z@}%
    \def\enumerate@parindent{\list@parindent}%
    \def\enumerate@labelsep{.5em}%
    \def\enumerate@firsttopsep{\smallskipamount}%
    \def\enumerate@topsep{\z@}%
    \def\enumerate@rightskip{\z@}%
    \let\enumerate@hook\relax
    \def\theenumi{\@arabic\c@enumi}%
    \def\theenumii{\@alph\c@enumii}%
    \def\theenumiii{\@roman\c@enumiii}%
    \def\theenumiv{\@Alph\c@enumiv}%
    \def\labelenumi{\theenumi.}%
    \def\labelenumii{\theenumii)}%
    \def\labelenumiii{\theenumiii.}%
    \def\labelenumiv{\theenumiv.}%
    \def\enumerate{\@ifnextchar[{\@enumerate}{\@enumerate[]}}%
    \def\@enumerate[##1]{%
        \ifnum\@enumdepth>\thr@@
            \@toodeep
        \else
            \advance\@enumdepth by \@ne 
            \edef\@enumctr{enum\romannumeral\the\@enumdepth}%
            \list{\csname label\@enumctr\endcsname}{%
                \usecounter{\@enumctr}%
                \def\makelabel####1{{\hfill\normalfont ####1}}%
                \ifx.##1.%
                    \setcounter{\@enumctr}{3}% 
                    \expandafter\settowidth\expandafter\labelwidth\expandafter{\csname label\@enumctr\endcsname}%
                    \setcounter{\@enumctr}{0}%
                \else
                    \settowidth{\labelwidth}{##1}%
                \fi
                \labelsep=\enumerate@labelsep
                \itemindent=\labelwidth
                \advance\itemindent by \labelsep
                \ifnum \@enumdepth=1\advance\itemindent by\enumerate@indent\fi
                \leftmargin=\the\itemindent
                \itemindent=\z@
                \partopsep=\z@
                \parsep=\z@
                \itemsep=\z@
                \rightmargin=\z@
                \ifnum\@enumdepth=1\relax
                    \topsep=\enumerate@firsttopsep
                \else
                    \topsep=\enumerate@topsep
                \fi
                \listparindent=\enumerate@parindent
                \@rightskip=\enumerate@rightskip
                \enumerate@hook
                }%
        \fi
        }%
    \let\endenumerate =\endlist
    % itemize
    \def\itemize@indent{\z@}%
    \def\itemize@parindent{\list@parindent}%
    \def\itemize@labelsep{.5em}%
    \def\itemize@firsttopsep{\smallskipamount}%
    \def\itemize@topsep{\z@}%
    \def\itemize@rightskip{\z@}%
    \let\itemize@hook\relax
    \def\labelitemi{\textbullet}%
    \def\labelitemii{\textendash}%
    \def\labelitemiii{\textasteriskcentered}%
    \def\labelitemiv{{\footnotesize +}}%
    \def\itemize{\@ifnextchar[{\@itemize}{\@itemize[]}}%
    \def\@itemize[##1]{%
        \ifnum\@itemdepth>\thr@@
            \@toodeep
        \else
            \advance\@itemdepth by \@ne
            \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
            \list{\csname\@itemitem\endcsname}{%
                \def\makelabel####1{{\hfill\normalfont ####1}}%
                \ifx.##1.%
                    \settowidth\labelwidth{\csname\@itemitem\endcsname}%
                \else
                    \settowidth{\labelwidth}{##1}%
                \fi
                \labelsep=\itemize@labelsep
                \itemindent=\labelwidth
                \advance\itemindent by \labelsep
                \ifnum\@itemdepth=1\advance\itemindent by\itemize@indent\fi
                \leftmargin=\the\itemindent
                \itemindent=\z@
                \partopsep=\z@
                \parsep=\z@
                \itemsep=\z@
                \rightmargin=\z@
                \ifnum\@itemdepth=1%
                    \topsep=\itemize@firsttopsep
                \else
                    \topsep=\itemize@topsep
                \fi
                \listparindent=\itemize@parindent
                \@rightskip=\itemize@rightskip
                \itemize@hook
                }%
        \fi
    }%
    \let\enditemize =\endlist
    % description
    \def\description@indent{\list@parindent}%
    \let\description@topsep\smallskipamount
    \let\description@hook\relax
    \def\description{%
        \list{}{%
            \labelwidth=\z@
            \labelsep=\z@
            \rightmargin=\z@
            \leftmargin=\z@
            \advance\leftmargin by\description@indent
            \itemindent=-\description@indent
            \partopsep=\z@
            \topsep=\description@topsep
            \parsep=\z@
            \itemsep=\z@
            \listparindent=\z@
            \description@hook
            \let\makelabel\descriptionlabel
            }%
        }%
    \def\enddescription{\endlist}%
    \def\descriptionlabel##1{\if.##1.\else\bfseries ##1\hskip.5em\fi}%
    % quotation
    \let\quotation@size\footnotesize
    \def\quotation@itemindent{\list@parindent}%
    \def\quotation@parindent{\list@parindent}%
    \def\quotation@leftmargin{\list@parindent}%
    \let\quotation@rightmargin\z@
    \let\quotation@topsep\smallskipamount
    \def\quotation{%
        \list{}{%
            \quotation@size
            \listparindent=\quotation@parindent
            \itemindent=\quotation@itemindent
            \rightmargin=\quotation@rightmargin
            \leftmargin=\quotation@leftmargin
            \partopsep=\z@
            \topsep=\quotation@topsep
            \parsep=\z@
            }%
        \item[\Q@strut]\relax
        }%
    \def\endquotation{\endlist}%
    \def\Q@strut{\leavevmode\hbox{\vrule \@height 9\p@ \@depth 1\p@ \@width\z@}}%
    % quote
    \let\quote@size\footnotesize
    \def\quote@indent{\z@}%
    \def\quote@leftmargin{2pc}%
    \def\quote@rightmargin{2pc}%
    \let\quote@topsep\smallskipamount
    \def\quote{%
        \list{}{%
            \quote@size
            \listparindent=\quote@indent
            \itemindent=\listparindent
            \rightmargin=\quote@rightmargin
            \leftmargin=\quote@leftmargin
            \partopsep=\z@
            \topsep=\quote@topsep
            \parsep=\z@
            }%
        \item\relax
        }%
    \def\endquote{\endlist}%
    % verse
    \def\verse@margin{2pc}%
    \let\verse@topsep\smallskipamount
    \def\verse{%
        \let\\\@centercr
        \list{}{%
            \itemindent=\z@
            \listparindent=\z@
            \rightmargin=\verse@margin
            \leftmargin=\rightmargin
            \partopsep=\z@
            \parsep=\z@
            \topsep=\verse@topsep
            \itemsep=\z@
            }%
        \item\relax
    }%
    \def\endverse{\endlist}%
    % general lists parameters
    \@lowpenalty=51%
    \@medpenalty=151%
    \@highpenalty=301%
    \@beginparpenalty=-\@lowpenalty
    \@endparpenalty=-\@lowpenalty
    \@itempenalty=-\@lowpenalty
    \parsep=\z@
    \topsep=\z@
    \partopsep=\z@
    \itemsep=\z@
    \labelsep=.5em%
    \def\@listi{\leftmargin\leftmargini}%
    \def\@listii{%
        \leftmargin=\leftmarginii
        \labelwidth=\leftmarginii
        \advance\labelwidth by -\labelsep
        }%
    \def\@listiii{%
        \leftmargin=\leftmarginiii
        \labelwidth=\leftmarginiii
        \advance\labelwidth by -\labelsep
        }%
    \def\@listiv{%
        \leftmargin=\leftmarginiv
        \labelwidth=\leftmarginiv
        \advance\labelwidth by -\labelsep
        }%
    \setlength\leftmargini{2em}%
    \setlength\leftmarginii{2.2em}%
    \setlength\leftmarginiii{1.87em}%
    \setlength\leftmarginiv{1.7em}%
    \@listi
    \setlength\labelwidth{\leftmargini}%
    \addtolength\labelwidth{-\labelsep}%
    }
% 
\def\econbr@vtexsec{%
    \setcounter{secnumdepth}{5}%
    \@ifundefined{c@part}{\newcounter{part}}{}%
    \@ifundefined{c@chapter}{\newcounter{chapter}}{}%
    \@ifundefined{c@section}{\newcounter{section}}{}%
    \@ifundefined{c@subsection}{\newcounter{subsection}[section]}{}%
    \@ifundefined{c@subsubsection}{\newcounter{subsubsection}[subsection]}{}%
    \@ifundefined{c@paragraph}{\newcounter{paragraph}[subsubsection]}{}%
    \@ifundefined{c@subparagraph}{\newcounter{subparagraph}[paragraph]}{}%
    \renewcommand\thepart{\@Roman\c@part}%
    \renewcommand\thechapter{\@arabic\c@chapter}%
    \renewcommand\thesection{\@arabic\c@section}%
    \renewcommand\thesubsection{\thesection.\@arabic\c@subsection}%
    \renewcommand\thesubsubsection{\thesubsection .\@arabic\c@subsubsection}%
    \renewcommand\theparagraph{\thesubsubsection.\@arabic\c@paragraph}%
    \renewcommand\thesubparagraph{\theparagraph.\@arabic\c@subparagraph}%
    }
% 
\def\econbr@vtexmth{%
    \arraycolsep=1.2\p@
    \tabcolsep=6\p@
    \arrayrulewidth=.4\p@
    \doublerulesep=2\p@
    \tabbingsep=6\p@
    \jot=6\p@
    \thinmuskip=2.5mu%
    \medmuskip=3.5mu plus 1.5mu minus 2mu%
    \thickmuskip=3.75mu plus 1.5mu%
    \mathsurround=0.1\p@
    }
%
\def\econbr@amsmath{%
    \RequirePackage{amsmath}%
    \interdisplaylinepenalty=100%
    \ifdefined\mathindent
    \else
        \newskip\mathindent
        \mathindent=2pc
    \fi
    \ifdefined\@mathmargin
        \@mathmargin=\mathindent
    \fi
    }
%
\def\econbr@scshape{}
%
\def\econbr@zishape{%
    \let\zishape\itshape
    \def\enable@zishape{\itshape\mdseries}
    }
%
\def\econbr@thm@macros{%
    \RequirePackage{amsthm}%
    }
%
\def\econbr@bibliography{%
    \newenvironment{thebibliography}{}{}%
    \def\newblock{}%
    \RequirePackage[authoryear]{natbib}%
    \renewcommand\NAT@sep{,}% 
    \preto\bibitem@fin{%
        \clubpenalty=\@M
        \widowpenalty=\@M
        }%
    \ifbre@layout
        \def\bibsep{0\p@ \@plus.5\p@ \@minus.5\p@}%
        \def\bibfont{\footnotesize}%
        \setlength{\bibhang}{8\p@}%
        \def\bysame{\@ifnextchar\bgroup{\@bysame}{\bysame@hook\@bysame{}}}%
        \def\@bysame##1{\vrule \@height 1.5\p@ \@depth -1\p@ \@width 3em \hskip 0.5em\relax}%
        \let\bysame@hook\relax
    \else
        \def\bibfont{\normalsize}%
        \def\bibsep{6\p@ \@plus.5\p@ \@minus.5\p@}%
        \setlength{\bibhang}{\z@}%
    \fi
    \def\bibsection{\section*{\bibname}}%
    }
%
\def\econbr@numeqnrefcheck{}
% 
\def\econbr@floats{%
    \setlength\floatsep{18\p@ \@plus 6\p@ \@minus 2\p@}%
    \setlength\textfloatsep{18\p@ \@plus 6\p@ \@minus 2\p@}%
    \setlength\intextsep{18\p@ \@plus 6\p@ \@minus 2\p@}%
    \setlength\dblfloatsep{18\p@ \@plus 6\p@ \@minus 2\p@}%
    \setlength\dbltextfloatsep{24\p@ \@plus 6\p@ \@minus 2\p@}%
    \setlength\@fptop{0\p@ \@plus 1fil}%
    \setlength\@fpsep{8\p@ \@plus 1000fil}%
    \setlength\@fpbot{0\p@ \@plus 1fil}%
    \setlength\@dblfptop{0\p@ \@plus 1fil}%
    \setlength\@dblfpsep{8\p@ \@plus 1000fil}%
    \setlength\@dblfpbot{0\p@ \@plus 1fil}%
    \setcounter{topnumber}{5}%
    \renewcommand\topfraction{.99}%
    \setcounter{bottomnumber}{5}%
    \renewcommand\bottomfraction{.99}%
    \setcounter{totalnumber}{10}%
    \renewcommand\textfraction{.05}%
    \renewcommand\floatpagefraction{.9}%
    \setcounter{dbltopnumber}{5}%
    \renewcommand\dbltopfraction{.99}%
    \renewcommand\dblfloatpagefraction{.8}%
    \def\figurename{Figure}%
    \def\tablename{Table}%
    \let\figurefloat@shape\centering
    \def\figurecaption@size{\footnotesize}%
    \def\figurecaption@skip{8\p@}%
    \def\figurename@skip{\hskip.5em}%
    \let\figurename@size\relax
    \let\starttable@hook\relax
    \let\tablefloat@shape\centering
    \let\table@size\footnotesize
    \def\tablecaption@shape{\centering}%
    \def\tablecaption@size{\footnotesize}%
    \def\tablecaption@skip{4\p@}%
    \def\tablename@skip{\hskip.5em}%
    \let\tablename@size\relax
    \def\firsttabnote@skip{\vskip4\p@}%
    \def\tabnote@skip{\lineskip2\p@}%
    \let\tabnote@size\footnotesize
    \let\tabnote@indent\leavevmode
    \newcount\c@figure
    \let\cl@figure\@empty
    \let\p@figure\@empty
    \def\thefigure{\@arabic\c@figure}%
    \def\fps@figure{tbp}%
    \def\ftype@figure{1}%
    \def\ext@figure{lof}%
    \def\fnum@figure{\figurename~\thefigure.}%
    \def\figure{%
        \let\@makecaption\@makefigurecaption
        \let\@floatboxreset\figure@boxreset
        \@float{figure}%
        }%
    \def\endfigure{\end@float}%
    \@namedef{figure*}{%
        \let\@makecaption\@makefigurecaption
        \let\@floatboxreset\figure@boxreset
        \@dblfloat{figure}%
        }%
    \@namedef{endfigure*}{\end@dblfloat}%
    \def\figure@boxreset{%
        \reset@font
        \normalsize
        \@setminipage
        \figurefloat@shape
        }%
    \long\def\@makefigurecaption##1##2{%
        \vskip\figurecaption@skip
        \figurecaption@size
        \setbox\@tempboxa\hbox{%
            {\figurename@size ##1}\figurename@skip
            \if@contcaption
                \contcaptionbraces@left\contcaption@shape{##2}\contcaptionbraces@right
            \else
                ##2%
            \fi
            }%
        \ifdim\wd\@tempboxa>\hsize
            \unhbox\@tempboxa\par
        \else
            \figurecaption@fmt{\box\@tempboxa}\par
        \fi
        }%
    \def\figurecaption@fmt##1{\hbox to\hsize{\hfill##1\hfill}}%
    \newcount\c@table
    \let\cl@table\@empty
    \let\p@table\@empty
    \def\thetable{\@arabic\c@table}%
    \def\fps@table{tbp}%
    \def\ftype@table{2}%
    \def\ext@table{lot}%
    \def\fnum@table{\tablename~\thetable.}%
    \def\table{%
        \let\@makecaption\@maketablecaption
        \starttable@hook
        \tablewidth\hsize
        \let\@floatboxreset\table@boxreset\@float{table}%
        }%
    \def\endtable{\end@float}%
    \@namedef{table*}{%
        \let\@makecaption\@maketablecaption
        \starttable@hook
        \tablewidth\textwidth
        \let\@floatboxreset\table@boxreset\@dblfloat{table}%
        }%
    \@namedef{endtable*}{\end@dblfloat}%
    \def\table@boxreset{%
        \reset@font
        \tablefloat@shape
        \table@size
        \@setnobreak
        \@setminipage
        \setcounter{tabnote}{0}%
        }%
    \newdimen\tablewidth \tablewidth\textwidth
    \long\def\@maketablecaption##1##2{%
        \tablecaption@shape
        \parbox[t]{\tablewidth}{%
            \tablecaption@size{\tablename@size ##1}\tablename@skip
            \if@contcaption
                \contcaptionbraces@left\contcaption@shape{##2}\contcaptionbraces@right
            \else
                ##2%
            \fi
            \par
            }%
        \vskip\tablecaption@skip
        }%
    \newcounter{tabnote}%
    \def\thetabnote{\@fnsymbol\c@tabnote}%
    \newenvironment{tabnotes}{}{}%
    \let\tabnoteref@shape\relax
    \def\tabnotemark@fmt##1{\hbox{$^{##1}$}}%
    \def\tabnoteref@sep{,}%
    \protected\def\tabnoteref{%
        \@ifnextchar[{\@tempswatrue\@tabnoteref}{\@tempswafalse\@tabnoteref[]}%
        }%
    \def\@tabnoteref[##1]##2{%
        \if@tempswa% []
            \let\tabnoteref@firstref\relax
            \@for\@@refb:=##2\do{%
                \ifx\relax\tabnoteref@firstref\relax
                    \xdef\tabnoteref@firstref{\@@refb}%
                \fi
                }%
            \tabnotemark@link{\tabnoteref@firstref}{\tabnotemark@fmt{##1}}%
        \else
            \let\@tempa\@empty
            \tabnotemark@fmt{%
                \@for\@tempb:=##2\do{%
                    \@tempa\let\@tempa\tabnoteref@sep
                    \edef\@tempb{\expandafter\@firstofone\@tempb\@empty}%
                    \tabnotemark@link{\@tempb}{\saferef{\the\c@table @\@tempb tabnote}}%
                    }%
                }%
        \fi
        }%
    \def\tabnotemark@link##1##2{##2}%
    \def\tabnotemark{\@ifnextchar[{\@tabnotemark}{\@tabnotemark[]}}%
    \def\@tabnotemark[##1]##2{\@latex@error{Use command \string\tabnoteref!}\@ehc}%
    \def\@tabnotelabel[##1]##2{%
        \if@tempswa% []
            \protected@edef\@currentlabel{##1}%
        \else
            \refstepcounter{tabnote}%
        \fi
        \safelabel{\the\c@table @##2tabnote}%
        }%
    \def\first@tabnote{0}%
    \protected\long\def\tabnotetext{%
        \@ifnextchar[{\@tempswatrue\@tabnotetext}{\@tempswafalse\@tabnotetext[]}%
        }%
    \long\def\@tabnotetext[##1]##2##3{%
        \@tabnotelabel[##1]{##2}%
        \if@tempswa\ifx.##1.\notabnotemark\fi\fi
        \ifcase\first@tabnote\firsttabnote@skip\def\first@tabnote{1}\fi
        \tabnote@skip
        \parbox[t]{\tablewidth}{%
            \tabnote@size\tabnote@indent
            \tabnote@hypertarget{##2}%
            \tabnotemark@fmt{\expandafter\saferef{\the\c@table @##2tabnote}}%
            ##3\par
            }%
        \vtop to\z@{ }\par
        }%
    \let\tabnote@hypertarget\@gobble
    \def\notabnotemark{%
        \global\let\sv@tabnotemark@fmt\tabnotemark@fmt
        \gdef\tabnotemark@fmt####1{\global\let\tabnotemark@fmt\sv@tabnotemark@fmt}%
        }%
    \def\legend##1{\tabnotetext[]{}{##1}}%
    \def\vtexfltpdf@hyperref{%
        \def\tabnotemark@link####1####2{%
            \hyperlink{\the\c@table @####1tabnote.label}{####2}%
            }%
        \def\tabnote@hypertarget####1{%
            \hypertarget{\the\c@table @####1tabnote.label}{\strut}%
            }%
        \def\tabnotemark@fmt####1{\hbox{$^{\mathrm{####1}}$}}%
        }%
    \arrayrulewidth=0.4\p@
    \doublerulesep=2\p@
    \let\savehline\hline
    \def\thline{\noalign{\vskip3\p@}\savehline\noalign{\vskip3\p@}}%
    \def\hline{%
        \noalign{\vskip1\p@}%
        \noalign{\ifnum0=`}\fi\hrule \@height \arrayrulewidth 
        \futurelet\reserved@a\@xhline
        }%
    \AtBeginDocument{%
        \def\@xhline{%
            \ifx\reserved@a\hline
                \vskip\doublerulesep
                \vskip-\arrayrulewidth
            \fi
            \ifnum0=`{\fi}%
                \noalign{\vskip2\p@}%
            }%
        }%
    }
% 
\def\econbr@checklastpage{}
% 
\def\econbr@fmadd{%
    \let\history@postfix\relax
    \ifbre@layout
        \let\form@runauthors\relax
        \def\econbr@fm@markboth@hook{%
            \markboth{\MakeTextUppercase{\@runauthor}}{\MakeTextUppercase{\@runtitle}}%
            }%
    \else
        \def\form@runauthors{%
            \def\@runjournal{%
                \if@supplement
                    \sup@journal
                \else
                    % Submitted to \protect\url@fmt{}{\itshape}{\@journal}{\journal@url}%
                \fi
                }%
            }%
        \def\support@footnotetext@prehook{\hskip-\parindent}%
        \def\printauthor@footnotetext@prehook{\hskip-\parindent}%
    \fi
    \def\limited@wd@runtitle##1##2##3##4{##4}%
    \def\copyright@text{%
        \if@supplement
            \sup@journal
        \else
            \url@fmt{}{\itshape}{\@journal}{\journal@url}{, Vol. \@volume}{, No. \@issue}{ (\@pubyear),}{ pp. \@firstpage-\@lastpage} % # Eduardo Horta - 2022/11/19
        \fi
        \ifbre@layout\else\hfill\fi
        }%
    \let\fm@add@inits@from@name@by@aux\@gobble
    \def\cmd@for@aut@elem##1##2##3{}%
    \let\short@journal\@shortjid
    \def\copyrightline@text{\textcopyright\ \@pubyear\ \url@fmt{}{}{\copyright@owner}{\owner@url}\hfill \url@fmt{}{}{\doi@base\@doi}{\doi@base\@doi}}%##added by Eduardo Horta - 2022/11/19
    \ifbre@layout
        \def\title@size##1{\MakeTextUppercase{##1}}%
        \def\section@size{\footnotesize\baselineskip12\p@\MakeTextUppercase}%
    \else
        \def\section@size{\mdseries\scshape}%
        \def\author@size{\normalsize\mdseries\scshape}%
        \def\keywordname@size{\mdseries\scshape}%
    \fi
    \if@econbr@numberlines
        \def\numberlines@size{\scriptsize\ttfamily}%
        \if@econbr@draft
            \def\numberlines@skip{10\p@}%
        \else
            \def\numberlines@skip{20\p@}%
        \fi
        \def\numberlines@hook{%
            \appto\@evenhead\put@numberlines@box
            \appto\@oddhead\put@numberlines@box
            }%
        \gappto\ps@headings\numberlines@hook
        \gappto\ps@copyright\numberlines@hook
        \newbox\numberlines@box
        \newskip\numberlines@box@skip
        \def\set@numberlines@box{%
            \setlength\numberlines@box@skip\headsep
            \addtolength\numberlines@box@skip{5\p@}%
            \setbox\numberlines@box\vtop to\textheight{%
                \parindent=\z@
                \vskip\z@
                \@tempcnta=\z@
                \@tempdima=\z@
                \loop
                    \advance\@tempcnta by \@ne
                    \advance\@tempdima by \baselineskip
                    \hbox to \textwidth{%
                        \llap{\numberlines@size\the\@tempcnta\kern\numberlines@skip}\hfill
                        \rlap{\numberlines@size\kern\numberlines@skip\the\@tempcnta}%
                        }%
                \ifdim\@tempdima<\textheight\repeat
                \vss
                }%
            \ht\numberlines@box=\z@
            \dp\numberlines@box=\z@
            }%
        \def\put@numberlines@box{%
            \lower\numberlines@box@skip\hbox to\z@{\hss\copy\numberlines@box}%
            }%
        \AtEndPreamble{\set@numberlines@box}%
    \fi
    \ifbre@layout
        \gappto\econbr@appendix@hook{%
            \let\appendixname@uc\MakeTextUppercase
            \def\section@prefix{\MakeTextUppercase{\appendixname}\ }%
            }%
    \fi
    \def\vtexfmpdf@hyperref{%
        \def\thankref@hyperlink####1{\edef\@tempx{####1thanks}\hyperlink{####1}{\saferef{\@tempx}}}%
        \def\thanks@hypertarget####1{\smash{\raise\baselineskip\hbox{\protect\hypertarget{####1}{}}}}%
        \def\thanksmark@link####1####2{\hyperlink{####1}{####2}}%
        \let\safe@href\href
        }%
    \if@econbr@draft
        \ifbre@layout
            \def\keyword@skip{25\p@}%
        \else
            \def\keyword@skip{21\p@}%
        \fi
    \fi
    }
% 
\def\econbr@cmn@settings{%
    \global\let\mdforvkt\@gobblethree
    \global\let\mdauxout\@gobblethree
    \let\startchkuriskip\relax
    \let\endchkuriskip\relax
    \def\sep@by@num@in@seq@error##1{\@latex@error{##1}\@ehc}%
    \def\sep@by@num@in@seq##1##2##3##4##5##6{%
        \ifnum##1>\numexpr##2\relax
            \sep@by@num@in@seq@error{First argument of command \string\@sep@by@num@in@seq\space 
                '##1' can not be greater than second '##2'}%
        \fi
        \ifnum##1<\@ne
            \sep@by@num@in@seq@error{First argument of command \string\@sep@by@num@in@seq\space 
                '##1' can not be smaller than 1}%
        \fi
        \ifnum##2>\z@
            \ifnum##2>\@ne
                \ifnum##2>\tw@
                    \ifnum##1<\numexpr##2\relax
                        \ifnum\numexpr##2-##1\relax>\@ne
                            ##4%
                        \else
                            ##5%
                        \fi
                    \else
                        ##6%
                    \fi
                \else
                    \ifnum##1<\numexpr##2\relax
                        ##3%
                    \else
                        ##6%
                    \fi
                \fi
            \else
                ##6%
            \fi
        \else
            \sep@by@num@in@seq@error{%
                Number of elements in command \string\@sep@by@num@in@seq\space 
                '##2' must be greater than zero%
                }%
        \fi
        }%
    }
% 
\def\econbr@imstoc{%
    \def\ignorenumberline{\let\sv@numberline\numberline\let\numberline\@gobble}%
    \def\restorenumberline{\let\numberline\sv@numberline}%
    \def\write@toc@ignorenumberline{\addtocontents{toc}{\protect\ignorenumberline}}%
    \def\write@toc@restorenumberline{\addtocontents{toc}{\protect\restorenumberline}}%
    \def\ignorecontentsline{\let\sv@contentsline\contentsline\let\contentsline\@gobblefour}%
    \def\restorecontentsline{\let\contentsline\sv@contentsline}%
    \def\write@toc@ignorecontentsline{\addtocontents{toc}{\protect\ignorecontentsline}}%
    \def\write@toc@restorecontentsline{\addtocontents{toc}{\protect\restorecontentsline}}%
    }
% 
\def\econbr@hyper@settings{}
%
\def\econbr@print@msnbibl{}
%
\def\econbr@urilist{}
%
\def\econbr@bibliography@exists{}
%
\def\econbr@offprint{%
    \let\write@offdata\@gobblefour
    \let\off@data\@gobblefour
    \let\OFFPRINT\relax
    \let\printoffprint\relax
    \let\nooffprint\relax
    }
%
\def\econbr@bibtags{%
    \@ifundefined{endbibitem}{\let\endbibitem\relax}{}%
    }
%
\def\econbr@sici{}
%
\def\econbr@backref{%
    \AtBeginDocument{%
        \ifendbibitem@
            \backrefparscanfalse
        \else
            \let\econbr@save@BR@backref\BR@backref
            \def\BR@backref##1{%
                \cspreto{br@##1}{[}%
                \csappto{br@##1}{]}%
                \econbr@save@BR@backref{##1}%
                }%
        \fi
        }%
    \AtEndPreamble{%
        \RequirePackage{backref}%
        \def\backref{}%
        \let\BR@backrefprint\relax
        \def\endbibitem{%
            \unskip\unkern\ 
            [{%
                \let\par\@empty 
                \backrefprint
            }]%
            \write@endbibitem
            \par
            }%
        \def\write@endbibitem{%
            \immediate\write\@mainaux{\string\global\string\endbibitem@true}%
            \global\let\write@endbibitem\relax
            }%
        }%
    }
%
\def\econbr@queries{%
    \def\set@dup@queries{%
        \def\query@hskip{25pt}%
        \def\query@page@title@size{}%
        \def\query@page@title{%
            \write@toc@ignorecontentsline
            \section*{Copyeditor\textquoteright s queries}%
            \write@toc@restorecontentsline
            }%
        \if@econbr@final
            \relax@queries
        \fi
        }%
    \AtEndPreamble{%
        \@ifpackageloaded{dup-query}%
            {\set@dup@queries}%
            {%
                \IfFileExists{dup-query.sty}%
                    {%
                        \RequirePackage[querypagebycount]{dup-query}%
                        \set@dup@queries
                    }%
                    {}%
            }%
        }%
    }
%
\def\econbr@bookmarks@true{%
    \setcounter{tocdepth}{6}%
    \AtEndPreamble{%
        \@ifpackageloaded{hyperref}%
            {%
                \vtexfltpdf@hyperref
                \vtexfmpdf@hyperref
                \preto\bibsection{\pdfbookmark[1]{\bibname}{bibliography-section}}%
            }%
            {}%
        }%
    }
%
\def\econbr@xml@settings{%
    \AtEndPreamble{%
        \@ifpackageloaded{hyperref}%
            {}%
            {\let\@setref\econbr@setref}%
        }%
    }
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% END OF FILE econbr.cfg
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Definitions
\newcommand\bibname{References}
\newcommand\appendixname{Appendix}
\newcommand\abstractname{}
\newcommand\acknowledgementsname{Acknowledgements}
\overfullrule=\z@
%
% Debug
\DeclareOption{debug}{%
    \@econbr@debugtrue
    }
% 
% General Options
\DeclareOption{amsmath}{%
    \@load@amsmathtrue
    \csuse{econbr@amsmath@vtexmth}%
    }
\DeclareOption{noamsmath}{%
    \@load@amsmathfalse
    }
\DeclareOption{seceqn}{\@seceqntrue}
%
% Specific Options
\def\thepage@prefix{}
\DeclareOption{notes}{\AtBeginDocument{\layout@notes}}
\DeclareOption{supplement}{%
    \global\@supplementtrue
    \global\@supplement@onlineonlytrue
    }
% This option adds an 'o' before page numbers (comment by Eduardo Horta)
\DeclareOption{onlineonly}{%
    \global\@onlineonlytrue
    \global\@supplement@onlineonlytrue
    \def\thepage@prefix{o}%
    }
%
% Different paper sizes
\DeclareOption{wwwview}{\@wwwviewabletrue}
\DeclareOption{wwwprint}{\@wwwprintabletrue}
%
\csuse{econbr@pdfoptions}
%
\DeclareOption{bookmarks}{\global\@bookmarkstrue}
\DeclareOption{nobookmarks}{\global\@bookmarksfalse}
\DeclareOption{nowpsmd}{\global\@nowps@mdtrue}
%
% XML options
\DeclareOption{xmlfm}{\@xmlfmbmtrue}
\DeclareOption{xmlbm}{\@xmlfmbmtrue}
%
% open access
\DeclareOption{openaccess}{\global\@open@accesstrue}
%
% query page
\DeclareOption{noquerypage}{\global\@econbr@querypagefalse}
%
% put lines numbers in margins
\DeclareOption{linenumbers}{\global\@econbr@numberlinestrue}
\DeclareOption{nolinenumbers}{\global\@econbr@numberlinesfalse}
%
% submission mode
\DeclareOption{draft}{%
    \@econbr@finalfalse
    \@econbr@drafttrue
    \@econbr@numberlinestrue
    \PassOptionsToPackage{draft=false}{hyperref}%
    \PassOptionsToPackage{final}{graphics}%
    }
% print mode
\DeclareOption{final}{%
    \@econbr@finaltrue
    \@econbr@draftfalse
    \@econbr@numberlinesfalse
    }
%


%% Journals 
\DeclareOption{te}{%
    \global\te@layouttrue
    \def\issn{1933-6837}%
    \def\@eissn{1555-7561}%
    \def\@journal{Theoretical Economics}%
    \def\journal@url{https://econtheory.org}%
    \def\sup@journal{Supplementary Material}%
    \def\copyright@owner{Copyright owner}%
    \let\copyright@default\copyright@owner
    \def\@jid{TE}%
    \def\@shortjid{te}%
    }
%
\DeclareOption{rbe}{%
    \global\rbe@layouttrue
    \def\@issn{0034-7140}%
    \def\@eissn{1806-9134}%
    \def\@journal{Revista Brasileira de Economia}%
    \def\journal@url{https://bibliotecadigital.fgv.br/ojs/index.php/rbe}%
    \def\sup@journal{Supplementary Material}%
    \def\copyright@owner{Fundação Getúlio Vargas}%
    \let\copyright@default\copyright@owner
    \def\owner@url{https://portal.fgv.br/}
    \def\@jid{RBE}%
    \def\@shortjid{rbe}%
    \csuse{econbr@nolistindent@mattson@amsmath}%
    }
%
\DeclareOption{bre}{%
    \global\bre@layouttrue
    \def\@issn{1980-2447}%
    \def\@eissn{0101-7012}%
    \def\@journal{Brazilian Review of Econometrics}%
    \def\journal@url{https://bibliotecadigital.fgv.br/ojs/index.php/bre}%
    \def\sup@journal{Supplementary Material}%
    \def\sup@journal@url{https://bibliotecadigital.fgv.br/ojs/index.php/bre/issue/archive}%
    \def\copyright@owner{Brazilian Econometric Society}%
    \let\copyright@default\copyright@owner
    \def\owner@url{https://www.sbe.org.br/en/home/}
    \def\@jid{BRE}%
    \def\@shortjid{bre}%
    \csuse{bre@nolistindent@mattson@amsmath}%
    }
%
% Default information about journal
% \def\@issn{0000-0000}
% \def\@eissn{0000-0000}
% \def\@journal{Unknown Journal}
% \def\sup@journal{Supplementary Material}
% \def\journal@url{https://www.econometricsociety.org/}
% \def\copyright@owner{The Econometric Society}
% \def\@jid{UJ}
% \def\@shortjid{uj}
%
% Math options
\DeclareOption{nodisplaystyleaddons}{\global\@displaystyle@addonsfalse}
%
\ExecuteOptions{twoside,nameyear,amsmath,url,bookmarks,outlines,urllinks,pdflinks,breaklinks}
\ProcessOptions
%
\if@onlineonly
    \ifbre@layout
        \def\sup@journal@url{https://www.econometricsociety.org/} 
    \else
        \AtBeginDocument{\econbr@error{onlineonly is not prepared for \@jid}}
    \fi
\fi
%
\if@displaystyle@addons
    \appto\econbr@xml@info{\edef\ds@on@off{1}}
\else
    \appto\econbr@xml@info{\edef\ds@on@off{0}}
\fi
%
\csuse{econbr@fileinfo}
%
\let\debug@wlog\@gobble
\if@econbr@debug
    \let\debug@wlog\wlog
\fi
%
% FONTS
\def\@xpt{10.08}
\def\@ixpt{9.2}
\def\@viiipt{8.28}
\def\@xipt{11}
\def\@xivpt{14}
\def\@xvipt{16}
\def\@xviiipt{18}
\def\@xxpt{20}
\def\@xxivpt{24}
%
\ifbre@layout
    \def\@xpt{10.5}
    \def\@ixpt{9.5}
    \def\@viiipt{8.5}
\fi
%
\renewcommand\normalsize{%
    \@setfontsize\normalsize\@xpt{13.5\p@ \@plus .5\p@ \@minus .5\p@}%
    \abovedisplayskip=10\p@ \@plus 2\p@ \@minus 2\p@
    \abovedisplayshortskip=6\p@ \@plus 2\p@
    \belowdisplayshortskip=6\p@ \@plus 2\p@
    \belowdisplayskip=\abovedisplayskip
    }
%
\newcommand\small{%
    \@setfontsize\small\@ixpt{12.45\p@ \@plus .3\p@ \@minus .3\p@}%
    \abovedisplayskip=7.5\p@ \@plus 4\p@ \@minus 1\p@
    \belowdisplayskip=\abovedisplayskip
    \abovedisplayshortskip=\abovedisplayskip
    \belowdisplayshortskip=\abovedisplayskip
    }
%
\newcommand\footnotesize{%
    \@setfontsize\footnotesize\@viiipt{11\p@ \@plus .2\p@ \@minus .2\p@}%
    \abovedisplayskip=6\p@ \@plus 4\p@ \@minus 1\p@
    \belowdisplayskip=\abovedisplayskip
    \abovedisplayshortskip=\abovedisplayskip
    \belowdisplayshortskip=\abovedisplayskip
    }
%
\newcommand\scriptsize{\@setfontsize\scriptsize\@viipt\@viiipt}
\newcommand\tiny{\@setfontsize\tiny\@vpt\@vipt}
\newcommand\large{\@setfontsize\large\@xiipt{14}}
\newcommand\Large{\@setfontsize\Large\@xivpt{16}}
\newcommand\LARGE{\@setfontsize\LARGE\@xvipt{18}}
\newcommand\huge{\@setfontsize\huge\@xviiipt{20}}
\newcommand\Huge{\@setfontsize\Huge\@xxpt{24}}
%
\ifbre@layout
    \renewcommand\normalsize{%
        \@setfontsize\normalsize\@xpt{12\p@ \@plus .3\p@ \@minus .3\p@}%
        \abovedisplayskip=10\p@ \@plus 2\p@ \@minus 2\p@
        \abovedisplayshortskip=6\p@ \@plus 2\p@
        \belowdisplayshortskip=6\p@ \@plus 2\p@
        \belowdisplayskip=\abovedisplayskip
        }
    \renewcommand\small{%
        \@setfontsize\small\@ixpt{11\p@ \@plus .2\p@ \@minus .2\p@}%
        \abovedisplayskip=7.5\p@ \@plus 4\p@ \@minus 1\p@
        \belowdisplayskip=\abovedisplayskip
        \abovedisplayshortskip=\abovedisplayskip
        \belowdisplayshortskip=\abovedisplayskip
        }
    \renewcommand\footnotesize{%
        \@setfontsize\footnotesize\@viiipt{10\p@ \@plus .1\p@ \@minus .1\p@}%
        \abovedisplayskip=6\p@ \@plus 4\p@ \@minus 1\p@
        \belowdisplayskip=\abovedisplayskip
        \abovedisplayshortskip=\abovedisplayskip
        \belowdisplayshortskip=\abovedisplayskip
        }
\fi
%
% Customization of fonts
\renewcommand\sldefault{it}
\renewcommand\bfdefault{b}
\let\slshape\itshape
%
% Compatibility with old latex:
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\let\sl\it
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
%
% DIMENSIONS
\setlength\parindent{16.89\p@}
\ifbre@layout
    \setlength\parindent{10\p@}
\fi
\def\sv@parindent{10\p@}
\setlength\textwidth{33pc}
\setlength\textheight{49pc}
\if@econbr@draft
    \textwidth=6in% letter paper width minus 2.5in margins
    \textheight=8.5in% letter paper height minus 2.5in margins
\fi
\setlength\columnsep{1pc}
\setlength\columnwidth{14pc}
\setlength\columnseprule{0\p@}
%
% HEADS:
\setlength\headheight{12\p@}
\setlength\headsep{14\p@}
\setlength\topskip{10\p@}
\setlength\footskip{22\p@}
\setlength\maxdepth{.5\topskip}
\if@econbr@draft
    \advance\textheight by -\headsep
    \advance\textheight by -\headheight
    \def\headheight@text@gap{-3.864\p@}% gap between head height and text
    \advance\textheight by -\headheight@text@gap
\fi
% 
% PAPER AND TRIM SIZE
\def\paper@width{8.5in}
\def\paper@height{11in}
\def\trim@width{6.75in}
\def\trim@height{9.75in}
\def\view@width{6in}
\def\view@height{9.1in}
%
\csuse{econbr@def@proof@papersize}
%
% CENTER ON PAGE
\@tempdima=\paper@width 
\advance\@tempdima by -\trim@width
\divide\@tempdima by 2
\advance\@tempdima by -1in
\hoffset=\@tempdima
%
\@tempdima=\paper@height
\advance\@tempdima by -\trim@height
\divide\@tempdima by 2
\advance\@tempdima by -1in
\voffset=\@tempdima
%
\csuse{econbr@def@crc@papersize}
%
\csuse{econbr@def@view@and@wwwprint@papersize}
%
% SIDE MARGINS
\setlength\oddsidemargin{0.75in}% gutter margin
% 
\@tempdima=\trim@width
\advance\@tempdima by-\oddsidemargin
\advance\@tempdima by-\textwidth
\setlength\evensidemargin{\@tempdima}% outer
\setlength\topmargin{.5in}% 0.75
\advance\topmargin by -1\p@
% 
\if@wwwprintable
    \addtolength\oddsidemargin{\evensidemargin}
    \divide\oddsidemargin by 2
    \setlength\evensidemargin{\oddsidemargin}
\fi
%
\if@wwwviewable
    \addtolength\oddsidemargin{\evensidemargin}
    \divide\oddsidemargin by 2
    \setlength\evensidemargin{\oddsidemargin}
    \@tempdima=\trim@width
    \advance\@tempdima by -\view@width
    \divide\@tempdima by 2
    \advance\oddsidemargin by -\@tempdima
    \evensidemargin=\oddsidemargin
    \@tempdima=\trim@height
    \advance\@tempdima by -\view@height
    \divide\@tempdima by 2
    \advance\topmargin by -\@tempdima
\fi
%
\setlength\marginparsep{7\p@}
\setlength\marginparpush{5\p@}
%
% TEXT PARAMETERS
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}
\setlength\parskip{0\p@}
%
% BREAKS
\setlength\smallskipamount{6\p@ \@plus 1\p@ \@minus 1\p@}
\setlength\medskipamount{12\p@ \@plus 3\p@ \@minus 3\p@}
\setlength\bigskipamount{18\p@ \@plus 3\p@ \@minus 3\p@}
%
% PAGE-BREAKING PENALTIES
\ifrbe@layout
\else
    \brokenpenalty=10000
    \finalhyphendemerits=10000
\fi
\ifbre@layout
    \brokenpenalty=100
    \finalhyphendemerits=5000
\fi
\clubpenalty=10000
\widowpenalty=10000
\displaywidowpenalty=50
\predisplaypenalty=10000
\pretolerance=100
\tolerance=800
\hbadness=800
\emergencystretch=3\p@
\hfuzz=1\p@
% 
% FOOTNOTES
\setlength\footnotesep{7\p@}
\setlength{\skip\footins}{11\p@ \@plus 2\p@ \@minus 1\p@}
\ifbre@layout
    \setlength{\skip\footins}{18\p@ \@plus 4\p@ \@minus 2\p@}
\fi
\skip\@mpfootins = \skip\footins
%
% FRAMED BOXES
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
%
% LOADING PACKAGES:
\csuse{econbr@pagecrop}
%
\csuse{econbr@graphicx}
%
\RequirePackage[T1]{fontenc}
\RequirePackage{times}
\ifbre@layout
    \renewcommand{\rmdefault}{btm}
    \def\econbr@putx{btm}
\else
    \renewcommand{\rmdefault}{put}
    \def\econbr@putx{putx}
\fi
%
\csuse{econbr@helvet}
%
\RequirePackage{textcomp}
\normalfont\normalsize
%
\csuse{econbr@mathdutc}
\RequirePackage{amssymb}
% \newcommand\hmmax{0}
% \newcommand\bmmax{1}
% \RequirePackage{bm} % Will load this after mathptmx
%
\DeclareMathSizes{10.5}{10.5}{6.5}{5.5}
\DeclareMathSizes{9.5}{9.5}{6}{5}
\DeclareMathSizes{8.5}{8.5}{5.5}{4.5}
%
% FRONT MATTER MACROS
\csuse{econbr@frontmatter@macros}
\def\volume@label{Vol.}
%
\def\email@text{}
\def\url@text{}
\def\fullurl@text{}
%
\ifbre@layout
\else
    \font\myfont=cmtt10 at 9.39\p@
    \def\ead@size{\myfont}
\fi
%
% Thanks
\def\thanks[#1]#2{\thankstext{#1}{#2}}
%
\DeclareRobustCommand\blackwell@fagency[2][]{#2}
\DeclareRobustCommand\blackwell@fnumber[2][]{#2}
\long\def\@thankstext[#1]#2#3{%
    \@thankslabel[#1]{#2}%
    \bgroup
        \let\fagency\blackwell@fagency
        \let\fnumber\blackwell@fnumber
        \protected@xdef\@thanks{%
            \@thanks\protect\thanks@thefnmark{#2thanks}%
            \protect\@footnotetext{\thanks@hypertarget{#2}#3}%
            }%
    \egroup
    }
\def\support@thefnmark#1{%
    \begingroup
        \unrestored@protected@xdef\@thefnmark{#1}%
    \endgroup
    }%
\long\def\support#1{%
    \bgroup
        \let\fagency\blackwell@fagency
        \let\fnumber\blackwell@fnumber
        \protected@xdef\@thanks{%
            \@thanks\protect\support@thefnmark{}%
            \protect\@footnotetext{\csuse{support@footnotetext@prehook}#1}%
            }%
    \egroup
    }
%
% url package
\RequirePackage{xurl} % originally {url} - Eduardo Horta - 2022/11/19
\urlstyle{same}
%
% LISTS
\csuse{econbr@list@macros}
%
\ifbre@layout
    \def\enumerate@indent{\list@parindent}
    \def\enumerate@firsttopsep{\z@}
    \def\itemize@firsttopsep{\z@}
    \def\list@parindent{10\p@}
    \def\longlist@topsep{\z@}
    \renewcommand\labellonglist{(\thelonglist)}
    \def\labelenumii{(\theenumii)}
    \def\itemize@indent{\list@parindent}
\else
    \def\enumerate@indent{\list@parindent}
    \def\enumerate@firsttopsep{.55\baselineskip}
    \def\enumerate@topsep{.55\baselineskip}%
    \def\enumerate@hook{\itemsep=.55\baselineskip}%
    \def\itemize@firsttopsep{.55\baselineskip}%
    \def\itemize@hook{\itemsep=.55\baselineskip}%
    \def\itemize@indent{\parindent}
    \def\list@parindent{10\p@}
    \def\longlist@firsttopsep{.55\baselineskip}
    \def\longlist@topsep{.55\baselineskip}%
    \def\longlist@hook{\itemsep=.275\baselineskip}%
    \renewcommand\labellonglist{(\thelonglist)}
    \def\labelenumii{(\theenumii)}
    \def\itemize@indent{\list@parindent}
\fi
%
% quotation
\def\quotation@leftmargin{1pc}
\def\quotation@rightmargin{1pc}
\ifbre@layout
\else
    \let\quotation@size\small
\fi
%
% quote
\def\quote@leftmargin{1pc}
\def\quote@rightmargin{1pc}
\ifbre@layout
\else
    \let\quote@size\small
\fi
%
% SECTIONS
\csuse{econbr@vtexsec}
%
% MATH
\csuse{econbr@vtexmth}
\if@load@amsmath
    \csuse{econbr@amsmath}
    \def\tmp@bre@or@rbe{0}
    \ifbre@layout\edef\tmp@bre@or@rbe{1}\fi
    \ifrbe@layout\edef\tmp@bre@or@rbe{1}\fi
    \ifnum\tmp@bre@or@rbe>\z@
        % gathered is left aligned
        \renewenvironment{gathered}[1][c]{\relax
            \ifmmode
            \else
                \nonmatherr@{\begin{gathered}}%
            \fi
            \null\,%
            \if #1t%
                \vtop 
            \else 
                \if#1b%
                    \vbox 
                \else 
                    \vcenter 
                \fi
            \fi 
            \bgroup
                \Let@ \chardef\dspbrk@context\@ne \restore@math@cr
                \spread@equation
                \ialign\bgroup
                    \if@fleqn
                    \else
                        \expandafter\hfil
                    \fi
                    \strut@$\m@th\displaystyle##$\hfil
                    \crcr
        }{%
            \endaligned
        }
    \fi
    \setlength\arraycolsep{1.2\p@}
    \setlength\tabcolsep{6\p@}
    \setlength\arrayrulewidth{0.4\p@}
    \setlength\doublerulesep{0.4\p@}
    \ifbre@layout
    \else
        \setlength\tabbingsep{0.5em}
    \fi
    \if@displaystyle@addons
        \protected\def\frac#1#2{{%
            \mathchoice
                {\displaystyle\begingroup #1\endgroup \@@over\displaystyle #2}%
                {\begingroup #1\endgroup \@@over #2}%
                {\begingroup #1\endgroup \@@over #2}%
                {\begingroup #1\endgroup \@@over #2}%
            }}
    \fi
    \@displaystyle@addonsfalse
    % show formulas with no refs in margin
    \let\@setmarginlbl\relax
    \let\temp@tagform@\tagform@
    \let\orig@mmeasure@\mmeasure@
    \def\mmeasure@#1{%
        \let\curr@tagform@\tagform@
        \let\tagform@\temp@tagform@
        \orig@mmeasure@{#1}%
        \let\tagform@\curr@tagform@
        }
    \apptocmd\tagform@{\@setmarginlbl}{}{}
    \def\label@in@display#1{%
        \global\@labelledtrue
        \@ifundefined{lab@#1}%
            {\global\@unsdlbltrue}%
            {\global\@unsdlblfalse}%
        \ifx\df@label\@empty
        \else
            \econbr@error{amsmath error: multiple \string\label's: 
                label '\df@label' will be lost}%
        \fi
        \gdef\df@label{#1}%
        }
    \ifcsundef{aligncolsep}%
        {}%
        {\aligncolsep{1em}}
    \def\cases@column@sep{\quad}
    \ifbre@layout
    \else
        \mathindent=42\p@
    \fi
\else
    \ifbre@layout
        \def\@eqnnum{%
            \hb@xt@.01\p@{}%
            \rlap{%
                \normalfont\normalcolor
                \hskip-\displayindent
                \hskip -\displaywidth(\theequation)%
                }%
            }
    \else
        \def\@eqnnum{{\normalfont\upshape \normalcolor (\theequation)}}
    \fi
\fi
%
% dummy hack to add text to number (todo: is it needed?)
\def\eqntext#1{\global\let\curr@eqnnum\@eqnnum
    \gdef\@eqnnum##1##2{$#1$\global\let\@eqnnum\curr@eqnnum}}
% 
\if@seceqn
    \@addtoreset{equation}{section}
    \renewcommand\theequation{\thesection.\@arabic\c@equation}
\fi
% 
\csuse{econbr@scshape}
\csuse{econbr@zishape}
% THEOREMS
\csuse{econbr@thm@macros}
\def\thm@space@after{.5em}
\def\econbr@thm@indent{\z@}
\ifbre@layout
    \def\econbr@thm@indent{\parindent}
    \def\thmnote@pre{\bgroup\normalfont\unskip\kern.07em---\kern.07em}
    \def\thmnote@post{\egroup}
    \def\thmnumber@pre{\bgroup\normalfont}
    \def\thmnumber@post{\egroup}
    \def\thmnumber@space{\space\kern0.02em}
\else
    \def\thmnote@pre{\bgroup\normalfont(}
    \def\thmnote@post{)\egroup}
    \def\thmnumber@pre{}
    \def\thmnumber@post{}
    \def\thmnumber@space{ }
\fi
\newtheoremstyle{plain}%
    {\medskipamount}%
    {\medskipamount}%
    {%
        \normalfont
        \ifrbe@layout
            \enable@zishape
        \else
            \ifbre@layout
                \zishape
                \let\econbr@setref@textup\textup
            \else
                \itshape
            \fi
        \fi
        \csuse{corthmlist@addons}%
    }%
    {\econbr@thm@indent}%
    {\scshape}%
    {\ifbre@layout:\else.\fi\csuse{corthmlist@head@pos}}%
    {\thm@space@after}%
    {%
        \thmname{#1}%
        \thmnumber{\thmnumber@pre\@ifnotempty{#1}{\thmnumber@space}\@upn{#2}\thmnumber@post}%
        \thmnote{ \thmnote@pre #3\thmnote@post}%
    }%
\newtheoremstyle{remark}%
    {\medskipamount}%
    {\medskipamount}%
    {\upshape \csuse{corthmlist@addons}}%
    {\econbr@thm@indent}%
    {\scshape}%
    {\ifbre@layout:\else.\fi\csuse{corthmlist@head@pos}}%
    {\thm@space@after}%
    {%
        \thmname{#1}%
        \thmnumber{\thmnumber@pre\@ifnotempty{#1}{\thmnumber@space}\@upn{#2}\thmnumber@post}%
        \thmnote{ \thmnote@pre #3\thmnote@post}%
    }%
\ifte@layout
    \def\econbr@currenvir@example{example}
    \def\thm@end@example@symbol{\lozenge}
    \gpreto\@endtheorem{%
        \ifx\@currenvir\econbr@currenvir@example
            \def\qedsymbol{\ensuremath{\lozenge}}\qed
        \fi
        }
\fi
\def\mathend@qed{\hbox to\hsize{\hfill\qedsymbol}}
\ifbre@layout
    \def\qedsymbol{\textit{Q.E.D.}}
    \DeclareRobustCommand{\qed}{%
        \ifmmode \mathqed
        \else
            \leavevmode\unskip\penalty9999 \hbox{ }\nobreak\hfill
            \hbox{\qedsymbol}%
        \fi
        }
\fi
\renewenvironment{proof}[1][\proofname]%
    {%
        \par
        \ifte@layout
            \ifstrequal{#1}{tr}%
                {%
                    \def\qedsymbol{\ensuremath{\vartriangleleft}}%
                }%
                {}%
        \fi
        \pushQED{\qed}%
        \normalfont
        \topsep=\medskipamount
        \trivlist
            \item[\hskip\labelsep
                \ifte@layout
                    \ifstrequal{#1}{tr}%
                        {\scshape{\proofname}}%
                        {\scshape{#1}}%
                \else
                    \ifbre@layout
                        \hskip\parindent
                    \fi
                    \scshape{#1}%
                \fi
            \ifbre@layout\@addpunct{:}\else\@addpunct{.}\fi]\ignorespaces
    }%
    {%
        \popQED
        \endtrivlist
        \@endpefalse
    }
%
% BIBLIOGRAPHY
\csuse{econbr@bibliography}
%
\marginparwidth=120\p@
\marginparsep=25\p@
% 
% FLOATS
\csuse{econbr@floats}
% 
% figure
\def\figurename{Figure}
\def\longfigurecaption@style{}
\ifbre@layout
    \def\figurename@size{\scshape\hskip10\p@}
    \def\figurename@skip{---}
    \def\figurecaption@fmt#1{\hbox to\hsize{\hfill\hskip-10\p@ #1\hfill}}
\else
    \def\figurename@size{\scshape}
    \def\figurename@skip{\enspace}
    \def\figurecaption@size{\small}
    \def\figurecaption@fmt#1{\hbox to\hsize{\hfill#1\hfill}}
\fi
%
\long\def\@makefigurecaption#1#2{%
    \vskip\figurecaption@skip
    \figurecaption@size
    \setbox\@tempboxa\hbox{#2}%
    \ifdim\wd\@tempboxa<\p@
        \def\figurename@skip{}%
    \fi
    \setbox\@tempboxa\hbox{%
        {\figurename@size #1}\figurename@skip
        \if@contcaption
            \contcaptionbraces@left\contcaption@shape{#2}\contcaptionbraces@right
        \else
            #2%
        \fi
        }%
    \ifdim \wd\@tempboxa >\hsize
        \longfigurecaption@style
        \unhbox\@tempboxa\par
    \else
        \figurecaption@fmt{\box\@tempboxa}\par
    \fi
    }
%
% table
\ifbre@layout
    \def\thetable{\@Roman\c@table}
    \def\fnum@table{\tablename~\thetable}
    \def\tablename{TABLE}
    \def\starttable@hook{\let\hline\thline}
    \def\tablename@skip{\endgraf\vskip3\p@}
    \def\tablecaption@size{\centering\footnotesize\scshape}
\else
    \def\tablename{Table}
    \def\starttable@hook{\let\hline\thline}
    \def\tablename@skip{\enspace}
    \def\tablename@size{\scshape}
    \def\tablecaption@size{\small}
    \def\tablecaption@shape{}%
    \def\tablecaption@fmt#1{\hbox to\hsize{\hfill#1\hfill}}
    \def\longfigurecaption@style{}
\fi
%
\ifrbe@layout
    \def\tablecaption@shape{\centering}
    \long\def\@maketablecaption#1#2{%
        \tablecaption@shape
        \setbox\@tempboxa\hbox{%
            \tablecaption@size{\tablename@size #1}\tablename@skip
            \if@contcaption
                \contcaptionbraces@left\contcaption@shape{#2}\contcaptionbraces@right
            \else
                #2%
            \fi
            }%
        \ifdim\wd\@tempboxa<\tablewidth
            \hbox to \hsize{\hfill\box\@tempboxa\hfill}\par
        \else
            \parbox[t]{\tablewidth}{%
                \tablecaption@size{\tablename@size #1}\tablename@skip
                \if@contcaption
                    \contcaptionbraces@left\contcaption@shape{#2}\contcaptionbraces@right
                \else
                    #2%
                \fi\par
                }%
        \fi
        \vskip\tablecaption@skip
        }
\else
    \ifbre@layout
    \else
        \def\tablecaption@shape{\centering}
        \long\def\@maketablecaption#1#2{%
            \vskip\tablecaption@skip
            \tablecaption@shape
            \setbox\@tempboxa\hbox{#2}%
            \ifdim\wd\@tempboxa<\p@
                \def\tablename@skip{}%
            \fi
            \setbox\@tempboxa\hbox{%
                \tablecaption@size{\tablename@size #1}\tablename@skip
                \if@contcaption
                    \contcaptionbraces@left\contcaption@shape{#2}\contcaptionbraces@right
                \else
                    #2%
                \fi
                }%
            \ifdim\wd\@tempboxa>\tablewidth
                \parbox[t]{\tablewidth}{%
                    \tablecaption@size{\tablename@size #1}\tablename@skip
                    \if@contcaption
                        \contcaptionbraces@left\contcaption@shape{#2}\contcaptionbraces@right
                    \else
                        #2%
                    \fi\par
                    }%
            \else
                \tablecaption@fmt{\box\@tempboxa}\par
            \fi
            }
    \fi
\fi
% 
\def\dblhline{\noalign{\vskip3\p@}\oldhline\noalign{\vskip1\p@}\oldhline\noalign{\vskip3\p@}}
% 
\def\thetabnote{\@alph\c@tabnote}
\def\tabnote@size{\scriptsize}
\def\firsttabnote@skip{\relax}
% \def\legend@tabnotemark@fmt#1{\textit{#1}:\hskip5\p@}
\def\legend@tabnotemark@fmt#1{}
\ifbre@layout
    \def\tabnote@indent{\parindent=10\p@}% was \hskip10\p@
\else
    \def\tabnote@indent{\hskip10\p@}
    \gappto\vtexfltpdf@hypermark{\def\tabnotemark@fmt#1{\textsuperscript{#1}}}
\fi
\def\notabnotemark{%
    \global\let\sv@tabnotemark@fmt\tabnotemark@fmt
    \gdef\tabnotemark@fmt##1{%
        \ifbre@layout
            % \legend@tabnotemark@fmt{Note}%
            \legend@tabnotemark@fmt{}%
        \else
            % \legend@tabnotemark@fmt{Note}%
            \legend@tabnotemark@fmt{}%
        \fi
        \global\let\tabnotemark@fmt\sv@tabnotemark@fmt
        }%
    }
%
\def\startscript{%
    \noalign{\gdef\tabular@cell@hook{\if@tabcell@hook\scriptsize\fi}\global\@tabcell@hooktrue}%
    }
\def\endscript{\noalign{\global\let\tabular@cell@hook\relax\global\@tabcell@hookfalse}}
\def\thead{\dblhline\startscript}
\def\endthead{\endscript\hline}
%
\long\def\note#1{{\nomark@tabnotetext{\textit{Note}:\hskip5\p@ #1}}}
%
\let\note\undefined
%
\newcounter{econbr@legend@count}
\def\legend#1{%
    \stepcounter{econbr@legend@count}%
    \tabnotetext[]{econbrlegend\the\c@econbr@legend@count}{#1}%
    }
%
\long\def\nomark@tabnotetext#1{%
    \tabnote@skip
    \parbox[t]{\tablewidth}{\tabnote@size\tabnote@indent\strut#1\par}%
    \vtop to\z@{ }\par
    }
%
\let\econbr@save@tabnotetext\@tabnotetext
\long\def\@tabnotetext[#1]#2#3{%
    \endgraf
    \econbr@save@tabnotetext[#1]{#2}{#3}%
    }
%
\AtBeginDocument{% todo: check if these hooks are needed
    \let\tabular@cell@hook\relax
    \def\@addtopreamble#1{\xdef\@preamble{\@preamble #1\tabular@cell@hook}}%
    \long\def\multicolumn#1#2#3{%
        \multispan{#1}%
        \begingroup
            \def\@addamp{\if@firstamp \@firstampfalse \else\@preamerr 5\fi}%
            \let\saved@tabular@cell@hook\tabular@cell@hook
            \let\tabular@cell@hook\relax
            \@mkpream{#2}%
            \@addtopreamble\@empty
            \let\tabular@cell@hook\saved@tabular@cell@hook
        \endgroup
        \def\@sharp{#3}%
        \@arstrut\@preamble\null
        \ignorespaces
        }%
    }
%
% Last page control
\csuse{econbr@checklastpage}
% 
% HEADINGS
\ifbre@layout
    \def\ps@headings{%
        \let\@oddfoot\@empty
        \let\@evenfoot\@empty
        \def\@evenhead{%
            \runninghead@size
            \rlap{\pagenumber@size\thepage@prefix\thepage}\hfill
            \leftmark\hfill
            }%
        \def\@oddhead{%
            \runninghead@size\hfill
            \limited@wd@runtitle{%
                \pagenumber@size\thepage@prefix\sprintf@four@nines{\thepage}%
                \hskip.5em\runninghead@size\rightmark\pagenumber@size
                \hskip.5em\thepage@prefix\sprintf@four@nines{\thepage}%
                }%
                {\the\textwidth}%
                {\thepage}%
                {\rightmark}%
            \hfill\llap{\pagenumber@size\thepage@prefix\thepage}%
            }%
        }%
\else
    \def\ps@headings{%
        \let\@oddfoot\@empty
        \let\@evenfoot\@empty
        \def\@evenhead{\runninghead@size{\pagenumber@size\thepage}\enskip\enskip\leftmark}%
        \def\@oddhead{%
            \runninghead@size
            \limited@wd@runtitle{%
                \runninghead@size
                \if@supplement@onlineonly
                    \@runjournal
                \else
                    \@journal\ \sprintf@with@nines{2}{\@volume}\ (\the\@pubyear)%
                \fi
                \enskip\enskip\@runtitle
                \enskip\enskip\pagenumber@size\sprintf@four@nines{\thepage}%
                }%
                {\the\hsize}%
                {\thepage}%
                {\rightmark}%
            \enskip\enskip{\pagenumber@size\thepage}%
            }%
        }%
\fi
\def\ps@empty{%
    \let\@mkboth\@gobbletwo
    \let\@oddhead\@empty
    \let\@oddfoot\@empty
    \let\@evenhead\@oddhead
    \let\@evenfoot\@oddfoot
    }
\def\ps@plain{%
    \let\@mkboth\@gobbletwo
    \let\@oddhead\@empty
    \let\@oddfoot\@empty
    \let\@evenhead\@oddhead
    \def\@oddfoot{%
        \hfill\pagenumber@plainsize\thepage\hfill
        }%
    \let\@evenfoot\@oddfoot
    }
% First page RH
\ifbre@layout
    \def\ps@copyright{%
        \let\@mkboth\@gobbletwo
        \def\@evenhead{\hfill\copyright@size\copyright@text\hfill}%
        \let\@oddhead\@evenhead
        \def\@oddfoot{\copyright@size\copyrightline@text}%
        \let\@evenfoot\@oddfoot
        }
\else
    \def\ps@copyright{%
        \let\@mkboth\@gobbletwo
        \def\@evenhead{\copyright@size\copyright@text}%
        \let\@oddhead\@evenhead
        \def\@oddfoot{\copyright@size\copyrightline@text}%
        % \def\@oddfoot{%
        %     \parbox[b][\z@][b]{\hsize}{%
        %         \vskip\copyrightline@skip
        %         \copyright@size\copyrightline@text\hfill
        %         \ifrbe@layout
        %             \vadjust{\vskip7.3\p@}%
        %         \else
        %             \vadjust{\vskip5.8\p@}%
        %         \fi
        %         }%
        %     }%
        \let\@evenfoot\@oddfoot
        }%
\fi
%
% FOOTNOTE
\newcommand\@makefntext[1]{%
    \parindent=10\p@
    \@makefnmark #1%
    }
\ifbre@layout
    \def\@makefnmark{\@textsuperscript{\normalfont\@thefnmark}}%
\else
    \def\@makefnmark{\@textsuperscript{\normalfont\upshape\@thefnmark}}%
\fi
%
% hook for hypermark
\gdef\@makefntext@fmt#1{%
    \parindent=10\p@
    \hskip10\p@
    \@makefnmark
    }
\DeclareRobustCommand\freefootnotetext[2][]{%
    \begingroup
        \unrestored@protected@xdef\@thefnmark{#1}%
    \endgroup
    \@footnotetext{#2}%
    }
\def\footnoterule{%
    \kern-3\p@
    \hrule\@width 4pc%
    \kern2.6\p@
    }
%
% Sectioning commands
\def\section@size{\fontfamily{\econbr@putx}\fontseries{m}\fontshape{sc}\selectfont}
\def\section@cnt@size{\normalsize\upshape}
\ifbre@layout
    \def\section@size{\footnotesize\baselineskip12\p@\fontshape{uc}\selectfont}
\fi
% 
% last line is centered by page not by a text the first line
\def\fl@hangfrom#1{\noindent {#1}}
\def\TO@fl{\let\@hangfrom\fl@hangfrom}
%
\newcommand\section{%
    \@startsection
        {section}%
        {1}%
        {\z@}%
        {\bigskipamount}%
        {\smallskipamount}%
        {\global\@afterindentfalse\centering\TO@fl\section@size}%
    }
\newcommand\subsection{%
    \@startsection
        {subsection}%
        {2}%
        {\z@}%
        {\bigskipamount}%
        {\smallskipamount}%
        {\global\@afterindentfalse\itshape\centering\TO@fl}%
    }
\newcommand\subsubsection{%
    \@startsection
        {subsubsection}%
        {3}%
        {\z@}%
        {1.55ex\@plus.5ex\@minus.2ex}%
        {-1em}%
        {\normalfont\normalsize\itshape}%
    }
\newcommand\paragraph{%
    \@startsection
        {paragraph}%
        {4}%
        {\z@}%
        {1.55ex\@plus.5ex\@minus.2ex}%
        {-1em}%
        {\normalfont\normalsize\itshape}%
    }
\newcommand\subparagraph{%
    \@startsection
        {subparagraph}%
        {5}%
        {\parindent}%
        {1.55ex\@plus.5ex\@minus.2ex}%
        {-1em}%
        {\normalfont\normalsize\itshape}%
    }
\ifbre@layout
    \renewcommand\section{%
        \@startsection
            {section}%
            {1}%
            {\z@}%
            {\bigskipamount}%
            {\smallskipamount}%
            {\centering\TO@fl\section@size}%
        }
    \renewcommand\subsection{%
        \@startsection
            {subsection}%
            {2}%
            {\z@}%
            {\bigskipamount}%
            {\smallskipamount}%
            {\itshape\centering\TO@fl}%
        }
    \renewcommand\subsubsection{%
        \@startsection
            {subsubsection}%
            {3}%
            {\z@}%
            {\medskipamount}%
            {\smallskipamount}%
            {\itshape\raggedright}%
        }
    \renewcommand\paragraph{%
        \@startsection
            {paragraph}%
            {4}%
            {\parindent}%
            {\medskipamount}%
            {-10\p@}%
            {\itshape}%
        }
    \renewcommand\subparagraph{%
        \@startsection
            {subparagraph}%
            {5}%
            {\parindent}%
            {0.1\p@}%
            {-1em}%
            {\itshape}%
        }
\fi
%
\def\section@numbersep{.}
\def\subsection@numbersep{}
\def\subsubsection@numbersep{}
\def\paragraph@numbersep{}
\def\subparagraph@numbersep{}
%
\ifbre@layout
    \def\subsection@numbersep{.}
    \def\subsubsection@numbersep{.}
    \def\paragraph@numbersep{.}
    \def\subparagraph@numbersep{.}
\fi
%
\def\@seccntformat#1{{%
    \section@cnt@size\csname #1@prefix\endcsname
    \csname the#1\endcsname\csname#1@numbersep\endcsname\enspace
    }}
%
% an article begins with uppercase letters
\def\caps#1{\noindent\textsc{#1}}
%
% Acknowledgements
\def\acknowledgements@error{%
    \econbr@error{\string\acks\space must be encoded as environment (for XML)}%
    }
\let\acknowledgements@closed\relax
\def\acknowledgements{%
    \global\let\acknowledgements@closed\acknowledgements@error
    \section*{\acknowledgementsname}%
    }
\def\endacknowledgements{%
    \global\let\acknowledgements@closed\relax
    }
\let\acks\acknowledgements
\let\endacks\endacknowledgements
\AtEndDocument{\acknowledgements@closed}
%
% appendix
\def\app@addcontentsline#1#2#3{%
    \ifcsundef{app*star*section}%
        {}%
        {%
            \ifbre@layout
            \else
                \if@allsections\else\write@toc@ignorecontentsline\fi
            \fi
        }%
    \ifstrequal{#1+#2}{toc+section}{\write@toc@ignorenumberline}{}%
    \app@orig@addcontentsline{#1}{#2}{#3}%
    \ifstrequal{#1+#2}{toc+section}{\write@toc@restorenumberline}{}%
    \ifcsundef{app*star*section}%
        {}%
        {%
            \ifbre@layout
            \else
                \if@allsections\else\write@toc@restorecontentsline\fi
            \fi
        }%
    }
%
\newcommand\appendix[1][]{\par
    \def\section@cnt@size{\scshape}%
    \def\section@size{\scshape}%
    \def\section@numbersep{:}%
    \let\old@section\section
    \let\old@subsection\subsection
    \let\old@subsubsection\subsubsection
    \ifbre@layout
        \def\section@prefix{\fontshape{uc}\selectfont\appendixname\ }%
        \def\appendixname@uc##1{{\fontshape{uc}\selectfont ##1}}%
        \def\@appendixname{\appendixname}%
        \def\@thesection{\thesection\section@numbersep}%
    \else
        \let\appendixname@uc\@firstofone
        \ifx.#1.\relax% todo: find out if appendix optional argument is used at all
            \def\@appendixname{\appendixname}%
            \def\@thesection{\thesection\section@numbersep}%
            \def\section@prefix{\fontshape{sc}\selectfont\@appendixname\ }%
        \else
            \def\section@numbersep{.}%
            \def\@appendixname{}%
            \def\@thesection{\thesection\section@numbersep}%
            \old@section*{#1}%
            \def\section@prefix{}%
        \fi
    \fi
    \global\let\app@orig@addcontentsline\addcontentsline
    \let\addcontentsline\app@addcontentsline
    \def\section{\@ifnextchar*{\@appsectionstar}{\@appsectionnostar}}%
    \def\subsection{\@ifnextchar*{\@appsubsectionstar}{\@appsubsectionnostar}}%
    \def\subsubsection{\@ifnextchar*{\@appsubsubsectionstar}{\@appsubsubsectionnostar}}%
    \setcounter{section}{0}%
    \def\thesection{\@Alph\c@section}%
    \if@seceqn
        \@addtoreset{equation}{section}%
        \def\theequation{\thesection.\arabic{equation}}%
    \fi
    \ifte@layout
    \else
        \@addtoreset{figure}{section}%
        \def\thefigure{\thesection.\arabic{figure}}%
        \@addtoreset{table}{section}%
        \ifbre@layout
            \def\thetable{\thesection.\Roman{table}}%
        \else
            \def\thetable{\thesection.\arabic{table}}%
        \fi
    \fi
    \get@changed@counter@info@for@xml{section}{appendix}%
    \if@seceqn
        \get@changed@counter@info@for@xml{equation}{appendix}%
    \fi
    \ifte@layout
    \else
        \get@changed@counter@info@for@xml{table}{appendix}%
        \get@changed@counter@info@for@xml{figure}{appendix}%
    \fi
    \csname econbr@appendix@hook\endcsname
    }
%
\def\@appsectionstar*#1{%
    \csedef{app*star*section}{1}%
    \def\section@size{\scshape}%
    \ifx.#1.%
        \old@section*{\appendixname@uc{\appendixname}}%
    \else
        \def\section@numbersep{:}%
        \old@section*{\appendixname@uc{\appendixname}\section@numbersep\enspace #1}%
    \fi
    \ifbre@layout
        \def\section@size{\scshape}%
    \fi
    \setcounter{section}{1}%
    \csundef{app*star*section}%
    }
%
\def\@appsubsectionstar*#1{%
    \csedef{app*star*section}{2}%
    \old@subsection*{#1}%
    \csundef{app*star*section}%
    }
%
\def\@appsubsectionnostar{\old@subsection}
%
\def\@appsubsubsectionstar*#1{%
    \csedef{app*star*section}{3}%
    \old@subsubsection*{#1}%
    \csundef{app*star*section}%
    }
%
\def\@appsubsubsectionnostar{\old@subsubsection}
%
\def\@appsectionnostar#1{%
    \ifx.#1.%
        \def\section@numbersep{}%
        \old@section[\@appendixname\ \@thesection]{}%
    \else
        \def\section@numbersep{:}%
        \def\@argi{#1}%
        \old@section[\@appendixname\ \@thesection\ \@argi]{#1}%
    \fi
    }
%
% FRONTMATTER FORMATTING PARAMETERS
\def\frontmatter@style{\centering}
\def\address@style{\normaltext}
\def\affiliation@style{\normaltext}
\def\abstract@style{\normaltext}
\def\keyword@style{\normaltext}
%
% FRONT MATTER SKIPS
\def\title@skip{14\p@}
\def\authors@skip{12\p@}
\def\dedicated@skip{12\p@}
\def\affiliation@skip{4\p@}
\def\address@skip{4\p@ \@plus 2\p@}
\def\abstract@skip{10\p@}
\def\keyword@skip{6\p@}
\def\history@skip{14\p@ \@plus 2\p@ \@minus 2\p@}
\def\frontmatter@cmd{\vskip24\p@ \@plus 3\p@ \@minus 3\p@}
\def\abstractname@afterskip{}
\def\abstractname@style{}
\ifbre@layout
    \def\title@skip{-6\p@}
    \def\keyword@skip{10\p@}
    \def\history@skip{8\p@ \@plus 4\p@ \@minus 2\p@}
    \def\frontmatter@cmd{%
        \if@open@access
            \ifcsundef{econbr@too@long@open@access@text}%
                {}%
                {%
                    \xdef\econbr@too@long@open@access@corr{%
                        \the\dimexpr\econbr@too@long@open@access@corr+20\p@\relax
                        }%
                    \econbr@calc@oa@dim(\econbr@too@long@open@access@text/%
                        \econbr@too@long@open@access@spare)*20\p@\@nil
                    \vskip-\econbr@temp@oa@skip
                }%
        \fi
        \vskip20\p@ \@plus 3\p@ \@minus 3\p@
        }
\fi
% 
% FRONT MATTER DIMENSIONS
\def\abstract@width{28pc}
\def\abstract@indent{0\p@\parindent=10\p@}
\def\keyword@width{28pc}
\def\keyword@indent{0\p@}
\ifbre@layout
    \def\abstract@width{26pc}
    \def\abstract@indent{10\p@}
    \def\keyword@width{26pc}
    \def\keyword@indent{10\p@}
\fi
%
% FRONT MATTER FONTS 
\def\dochead@size{\normalsize\fontshape{uc}\selectfont}
\def\title@size{%
    \fontsize{13.25}{16}\selectfont
    \bfseries\mathversion{bold}%
    }%
\ifbre@layout
    \def\title@size{\normalsize\fontshape{uc}\selectfont}
\fi
\def\author@size{%
    \normalsize
    \fontfamily{\econbr@putx}\fontseries{m}\fontshape{sc}\selectfont
    }
\ifbre@layout
    \def\author@size{\normalsize}
\fi
\def\address@size{\normalsize\itshape}
\def\affiliation@size{\normalsize\itshape}
\def\dedicated@size{\normalsize\itshape}
\def\abstract@size{\small\upshape}
\def\keyword@size{\small\upshape}
\def\keywordname@size{\fontfamily{\econbr@putx}\fontseries{m}\fontshape{sc}\selectfont}
\def\history@size{\small\noindent}
%
\def\runninghead@size{\small}
\def\pagenumber@size{\normalsize\upshape}
\def\pagenumber@plainsize{\footnotesize}
\def\copyright@size{\footnotesize}
\ifbre@layout
    \def\abstract@size{\footnotesize\upshape}
    \def\keyword@size{\footnotesize\upshape}
    \def\keywordname@size{\scshape}
    \def\history@size{\centering\footnotesize\itshape}
    \def\runninghead@size{\footnotesize\fontshape{uc}\selectfont}
\fi
% 
% TEXT, etc.
\def\@snumber{%
    \number\@pubyear
    \ifnum\@firstpage<10\relax 
        000\@firstpage
    \else
        \ifnum\@firstpage<100\relax 
            00\@firstpage
        \else 
            \ifnum\@firstpage<1000\relax 
                0\@firstpage
            \else
                \ifnum\@firstpage<10000\relax
                    \@firstpage
                \else 
                    ????\econbr@error{number \@firstpage\space too large for \string\@snumber}%
                \fi
            \fi
        \fi
    \fi
    }
%
\if@supplement@onlineonly
    \if@supplement
        \AtBeginDocument{\mdforvkt{main-article-info}{articletype}{supplement}}
    \else
        \AtBeginDocument{\mdforvkt{main-article-info}{articletype}{onlineonly}}
    \fi
    \ifbre@layout
        \def\copyright@text{\hfill\url@fmt{}{\itshape}{\sup@journal}{\sup@journal@url}\hfill}
    \else
        \def\copyright@text{\sup@journal\hfill}
    \fi
\else
    \def\copyright@text{%
        \hfilneg
        \url@fmt{}{\upshape}{\@journal}{\journal@url/}\
        \@gobble{\volume@label\ }\@volume
        \@gobble{\issue@label\ \@issue\ } (\the\@pubyear),
        \@pagerange\hfill \@eissn/\@snumber
        }
    \ifbre@layout
        \def\copyright@text{\url@fmt{}{\itshape}{\@journal}{\journal@url}, 
            \volume@label\ \@volume, \issue@label\ \@issue\ (\thepubmonth,\ 
            \the\@pubyear), \@pagerange
            }
    \fi
\fi
\def\copyrightline@skip{\z@}
\ifbre@layout
    \def\license@type{???license@type???}
    \csdef{license@???license@type???@licencetype}{???}
    \csdef{license@???license@type???@licencetypetext}{???}
    \let\bre@styledata@noexpand\noexpand
    \def\bre@oa@published@text{ Brazilian Review of Econometrics published by the Brazilian Econometric Society}%
    \def\license@CC@BY@NC@licencetype{creativeCommonsBy-nc}
    \def\license@CC@BY@NC@licencetypetext{This is an open access article under the terms of the 
        \bre@styledata@noexpand\href{https://creativecommons.org/licenses/by-nc/4.0/}%
            {Creative Commons Attribution-NonCommercial} License, which permits use, 
        distribution and reproduction in any medium, provided the original work is properly 
        cited and is not used for commercial purposes.%
        }
    \def\license@CC@BY@NC@ND@licencetype{creativeCommonsBy-nc-nd}
    \def\license@CC@BY@NC@ND@licencetypetext{This is an open access article under the terms of the 
        \bre@styledata@noexpand\href{https://creativecommons.org/licenses/by-nc-nd/4.0/}%
            {Creative Commons Attribution-NonCommercial-NoDerivs} License, 
        which permits use and distribution in any medium, 
        provided the original work is properly cited, 
        the use is non-commercial and no modifications or adaptations are made.%
        }
    \def\license@CC@BY@licencetype{creativeCommonsBy}
    \def\license@CC@BY@licencetypetext{This is an open access article under the terms of the 
        \bre@styledata@noexpand\href{https://creativecommons.org/licenses/by/4.0/}%
            {Creative Commons Attribution} License, 
        which permits use, distribution and reproduction in any medium, 
        provided the original work is properly cited.%
        }
    \csdef{copyright@U.S. Government work@holdertext}{Published \the\@copyear. 
        This article is a U.S. Government work and 
        is in the public domain in the USA.%
        }
    \if@open@access
        \def\econbr@too@long@open@access@corr{0pt}
        \def\econbr@calc@oa@dim(#1/#2)*#3\@nil{{%
            \@tempdima=#1\relax
            \@tempdimb=#2\relax
            \@tempdimc=#3\relax
            \debug@wlog{econbr@calc@oa@dim input: 
                (\the\@tempdima/\the\@tempdimb)*\the\@tempdimc}%
            \@tempcnta=\@tempdima
            \@settopoint\@tempdimb
            \edef\@tempdimb@nopt{\strip@pt\@tempdimb}%
            \edef\@tempdima@tosp{\the\@tempcnta sp}%
            \debug@wlog{econbr@calc@oa@dim 1st arg to sp: \@tempdima@tosp}%
            \debug@wlog{econbr@calc@oa@dim 2nd arg to integer without pt: \@tempdimb@nopt}%
            \edef\@tempratio{\strip@pt\dimexpr\@tempdima@tosp/\@tempdimb@nopt\relax}%
            \debug@wlog{econbr@calc@oa@dim 1st arg divided by 2nd and pt removed: 
                \@tempdima@tosp/\@tempdimb@nopt=\@tempratio}%
            \@tempdima=\@tempratio\@tempdimc
            \xdef\econbr@temp@oa@skip{\the\@tempdima}%
            \debug@wlog{econbr@calc@oa@dim 3rd arg ratio multiplied by ratio: 
                \the\@tempdimc*\@tempratio=\econbr@temp@oa@skip}%
            }}
        \def\copyrightline@text{%
            \setbox\@tempboxa\vbox{{%
                \parindent=\z@
                \nohyphen
                \spaceskip=\fontdimen2\font \@plus 1\p@ \@minus 1\p@
                \ifcsundef{copyright@holder}%
                    {%
                        \textcopyright\ \the\@copyear\ The Author\ifnum\c@author>1s\fi.%
                        \bre@oa@published@text.
                    }%
                    {%
                        \ifcsundef{copyright@\copyright@holder @holdertext}%
                            {%
                                \vtxClassError{econbr}%
                                    {%
                                        There is no text for copyright holder "\copyright@holder".
                                        Please contact TeX support%
                                    }%
                                    {??}%
                            }%
                            {\csuse{copyright@\copyright@holder @holdertext}\\}%
                    }%
                \bgroup
                    \let\bre@styledata@noexpand\@empty
                    \ifdefined\econbr@author@corref@num
                        \ifcsundef{econbr@author@numid@\econbr@author@corref@num}%
                            {%
                                \textbf{???CORREF AUTHOR??}%
                                \econbr@error{failed to find id for author with number 
                                    \econbr@author@corref@num\space for openaccess license}%
                            }%
                            {%
                                \edef\tmp@id{%
                                    \csuse{econbr@author@numid@\econbr@author@corref@num}%
                                    }%
                                \csuse{author@from@label\tmp@id}\
                            }%
                    \else
                        \econbr@error{failed to find author with corref key}%
                        \textbf{???CORREF AUTHOR??}%
                    \fi
                    is the corresponding author on this paper.
                    \ifcsundef{copyright@holder}%
                        {\csuse{license@\license@type @licencetypetext}}%
                        {}%
                \egroup
                }}%
            \ifdim\ht\@tempboxa>\dimexpr3\baselineskip\relax
                \ifdim\ht\@tempboxa>\dimexpr5\baselineskip\relax
                    \econbr@error{open access text in copyright is longer than 5 lines, 
                        contact TeX support}%
                \fi
                \edef\tmp@oa@corr{\the\dimexpr\baselineskip\relax}%
                \ifdim\ht\@tempboxa>\dimexpr4\baselineskip\relax
                    \edef\tmp@oa@corr{\the\dimexpr\baselineskip*2\relax}%
                \fi
                \immediate\write\@auxout{%
                    \string\xdef\string\econbr@too@long@open@access@text{\tmp@oa@corr}%
                    }%
                \ifdim\econbr@too@long@open@access@corr>\z@
                    \immediate\write\@auxout{%
                        \string\xdef\string\econbr@too@long@open@access@spare{%
                            \econbr@too@long@open@access@corr}%
                        }%
                \else
                    \immediate\write\@auxout{%
                        \string\xdef\string\econbr@too@long@open@access@spare{\the\maxdimen}%
                        }%
                \fi
                \edef\tmp@oa@corr{\the\dimexpr6.62\p@+\tmp@oa@corr\relax}%
                \raisebox{\tmp@oa@corr}[\z@][\z@]{\vtop{\unvbox\@tempboxa}}%
            \else
                \raisebox{6.62\p@}[\z@][\z@]{\vtop{\unvbox\@tempboxa}}%
            \fi
            }
    \else
        \def\copyrightline@text{%
            \ifcsundef{copyright@holder}%
                {%
                    \textcopyright\ \the\@copyear\ 
                    \url@fmt{}{}{\copyright@owner}{\journal@url}\hfill
                    \@ifundefined{@doi}%
                        {}%
                        {\startchkuriskip\safe@href{\doi@base\@doi}{\doi@base\@doi}\endchkuriskip}%
                }%
                {%
                    \setbox\@tempboxa\vbox{{%
                        \parindent=\z@
                        \csuse{copyright@\copyright@holder @holdertext}\\
                        \@ifundefined{@doi}%
                            {}%
                            {\startchkuriskip\safe@href{\doi@base\@doi}{\doi@base\@doi}\endchkuriskip}%
                        }}%
                    \raisebox{6.62\p@}[\z@][\z@]{\vtop{\unvbox\@tempboxa}}%

                }%
            }
    \fi
    \def\author@prefix{%
        \ifcsundef{econbr@too@long@open@access@text}%
            {}%
            {\enlargethispage{-\econbr@too@long@open@access@text}}%
        }
\else
    \csdef{copyright@U.S. Government work@holdertext}{Published \the\@copyear. 
        This article is a U.S. Government work and 
        is in the public domain in the USA.%
        }
    \def\copyrightline@text{%
        \ifcsundef{copyright@holder}%
            {%
                {\fontfamily{ptm}\selectfont\textcopyright}\ 
                \the\@copyear\ 
                The Author\ifnum\c@author>1 s\fi. Licensed under the 
                \safe@href{https://creativecommons.org/licenses/by-nc/4.0/legalcode}%
                    {Creative Commons Attribution-NonCommercial License 4.0}.\break
            }%
            {\csuse{copyright@\copyright@holder @holdertext}\\}%
        Available at \url{\journal@url}.
        \@ifundefined{@doi}{}{\startchkuriskip\url{\doi@base\@doi}\endchkuriskip}%
        }
    \def\author@prefix{%
        \enlargethispage{\the\dimexpr-2\baselineskip+11\p@\relax}%
        }% 2 cp lines only (not 3)
\fi
%
\def\keyword@postfix{\unskip.}
%
% HISTORY
\def\history@prefix{}
\def\history@postfix{.}
\def\received@prefix{Manuscript received~}
\def\received@postfix{}
\global\let\revised@prefix\undefined
\global\let\revised@postfix\undefined
\def\accepted@prefix{; final version accepted~}
\def\accepted@postfix{}
\def\pubonline@prefix{; available online~}
\def\pubonline@postfix{}
\def\coeditor@prefix{Editor }
\def\coeditor@postfix{ handled this manuscript.}
\ifbre@layout
    \define@key{coeditor}{prefix}{\gdef\coeditor@prefix{#1\ }}%
    \define@key{coeditor}{postfix}{\gdef\coeditor@postfix{\ #1}}%
    \define@key{coeditor}{name}{%
        \gdef\coeditor@postfix{ handled this manuscript as an invited #1 lecture. 
            The invitation to deliver the #1 lecture is also an invitation 
            to publish a suitable version of the lecture in Econometrica.%
            }%
        }%
    \define@key{coeditor}{praddress}[1]{%
        \gdef\coeditor@postfix{ handled this manuscript, which is based 
            on a Presidential Address.}%
        }%
\fi
%
% COUNTERS, ETC
\newcounter{author}
\newcounter{address}
\newdimen\sv@mathsurround
\newcount\@tempcntc
\newcount\@tempcntd
\newcount\@tempcnte
%
% RUNNING HEAD
\def\@runtitle{}
\def\runtitle#1{%
    \gdef\@runtitle{#1}%
    }
\def\@runauthor{}
\def\runauthor#1{{%
    \def\etal{et al.}%
    \gdef\@runauthor{#1}%
    }}
%
% FRONTMATTER STUFF
\csuse{econbr@fmadd}
%
\newbox\fm@box
\newdimen\fm@size
%
\let\hy@frontmatter\relax
\let\hy@endfrontmatter\relax
%
\def\frontmatter{%
    \global\c@author=\z@
    \global\c@address=\z@
    \ifbre@layout\clearpage\fi% add this line to skip problems with offprint
    \csname fm@hook\endcsname
    \def\pdftitle##1{\write@pdfinfo{\user@hy@title}{##1}}%
    \def\pdfauthor##1{\write@pdfinfo{\user@hy@author}{##1}}%
    \def\pdfsubject##1{\write@pdfinfo{\user@hy@subject}{##1}}%
    \def\pdfkeywords##1{\write@pdfinfo{\user@hy@keywords}{##1}}%
    \sv@mathsurround=\mathsurround \m@th
    \parindent=\z@
    \hy@frontmatter
    \open@fm
    \ignorespaces
    }
\ifbre@layout
\else
    \g@addto@macro\frontmatter{\def\@makefntext@fmt#1{\hskip\z@\@makefnmark}}
\fi
% ENDFRONTMATTER
%
\def\n@authors{0}
% todo: remove when old aux file will be moved
\def\journalcopyrightline#1{}
\def\authorcopyrightline#1{}
\def\xmlrunauthor#1{}
\def\xmlpubmonth#1{}
\def\endfrontmatter{%
    \global\@topnum=\z@
    \set@pagerange
    \if@supplement@onlineonly
        \gdef\@runjournal{\sup@journal}%
    \else
        \gdef\@runjournal{\@journal\ \@volume\ (\the\@pubyear)}%
    \fi
    \form@runauthors
    \ifbre@layout
        \markboth{{\let\.\econbr@redef@uc@dot\@runauthor}}{\@runtitle}%
    \else
        \markboth{\@runauthor\hfill\@runjournal}{\@runjournal\hfill\@runtitle}%
    \fi
    \csname econbr@fm@markboth@hook\endcsname
    \thispagestyle{copyright}%
    \@thanks
    \abstract@fmt
    \keyword@fmt
    \close@fm
    \ifbre@layout
    \else
        \ifx\copyright@owner\copyright@default
            \ifnum\c@author=2\relax
                \gdef\authors@sep@b{\iflanguage{brazil}{ e }{ and }}%
            \else
                \ifnum\c@author>2\relax
                    \def\last@num{\alph{author}}%
                    \expandafter\gdef\csname authors@sep@\last@num\endcsname{\iflanguage{brazil}{, e }{, and }}%
                \fi
            \fi
            % \gdef\copyright@owner{\the\authors@list}%
        \fi
        \setbox\@tempboxa\vbox{%
            \parbox[b]{\hsize}{\copyright@size\copyrightline@text\hfill
                \ifrbe@layout
                    \vadjust{\vskip7.3\p@}%
                \else
                    \vadjust{\vskip5.8\p@}%
                \fi
                }%
            }%
        \@tempdimc=\ht\@tempboxa
        \ifdim\@tempdimc>26\p@\relax% usually ~26pt
            \advance\@tempdimc by -26\p@
            \enlargethispage{-\@tempdimc}%
        \fi
    \fi
    \write@pdfinfo{\hy@author}{\the\authors@list}%
    \bgroup
        \no@harm
        \xdef\@argi{\@runauthor}%
    \egroup
    \ifx\@runauthor\@empty
    \else
        \expandafter\gdef\expandafter\xml@runauthor\expandafter{\@argi}%
    \fi
    \expandafter\ifx\expandafter&\the\authors@list&%
        \xdef\xml@authorcopyrightline{\noexpand\copyright\ 
            \the\@copyear\space The Econometric Society}%
    \else
        \ifnum\c@author>\@ne
            \xdef\xml@authorcopyrightline{\noexpand\copyright\ 
                \the\@copyear\space The Authors.}%
        \else
            \xdef\xml@authorcopyrightline{\noexpand\copyright\ 
                \the\@copyear\space The Author.}%
        \fi
    \fi
    \ifbre@layout
        \ifcsundef{copyright@holder}%
            {%
                \if@open@access
                    \xappto\xml@authorcopyrightline{\bre@oa@published@text}%
                \else
                    \global\let\xml@authorcopyrightline\undefined
                \fi
            }%
            {%
                \xdef\xml@authorcopyrightline{%
                    \csuse{copyright@\copyright@holder @holdertext}%
                    }%
            }%
    \else
        \ifcsundef{copyright@holder}%
            {\gpreto\xml@authorcopyrightline{Copyright }}%
            {%
                \xdef\xml@authorcopyrightline{%
                    \csuse{copyright@\copyright@holder @holdertext}%
                    }%
            }%
    \fi
    \bgroup
        \let\smonth\@firstofone
        \xdef\xml@pubmonth{\@pubmonth}%
    \egroup
    \protected@write\@auxout{}{\string\gdef\string\n@authors{\the\c@author}}%
    \if@xmlfmbm
    \else
        \write@pdfinfo{\hy@subject}{\@journal, \volume@label\space\@volume, 
            \issue@label\space\@issue, \thepubmonth, \the\@pubyear, \@pagerange}%
    \fi
    \if@xmlfmbm
    \else
        \write@pdfinfo{\hy@keywords}{\the\keywords@list}%
    \fi
    \mdforvkt{main-article-info}{volume}{\@volume}%
    \mdforvkt{main-article-info}{issue}{\@issue}%
    \mdforvkt{main-article-info}{pubyear}{\the\@pubyear}%
    \mdforvkt{main-article-info}{copyear}{\the\@copyear}%
    \mdforvkt{main-article-info}{firstpage}{\thepage@prefix\@firstpage}%
    \mdforvkt{main-article-info}{lastpage}{\thepage@prefix\@lastpage}%
    \hy@endfrontmatter
    \global\mathsurround=\sv@mathsurround
    \global\let\@thanks\@empty
    \gdef\keyword@exist{0}%
    \global\c@footnote=\c@thanks
    \aftergroup\frontmatter@cmd
    }
\let\maketitle\relax
\newdimen\t@xtheight
\def\init@settings{%
    \splittopskip=\topskip
    \splitmaxdepth=\maxdepth
    \t@xtheight=\textheight
    \advance\t@xtheight by -\splittopskip
    }
\def\no@harm{%
    \def\qq@group@start{}%
    \def\qq@group@end{}%
    \let\qq@cmd\@firstofone
    \def\qq@style{}%
    \let\thanks\@gobble
    \let\thanksref\@gobble
    \let~\space
    \def\ead[##1]##2{}%
    \let\\\@empty
    \def\protect{\noexpand\protect\noexpand}%
    }
%
\def\open@fm{%
    \global\setbox\fm@box=\vbox\bgroup
        \hsize=\textwidth
        \frontmatter@style
    }
%
\def\close@fm{%
        \par
    \egroup
    \fm@size=\dp\fm@box
    \advance\fm@size by \ht\fm@box
    \@whiledim\fm@size>\t@xtheight\do{%
        \global\setbox\@tempboxa=\vsplit\fm@box to \t@xtheight
        \unvbox\@tempboxa
        \fm@size=\dp\fm@box
        \advance\fm@size by \ht\fm@box
        }%
    \unvbox\fm@box
    }
%
% DOCHEAD
\def\dochead#1{%
    \bgroup
        \dochead@size
        \leavevmode\vphantom{\strut}#1\par
    \egroup
    \def\title@skip{8\p@}%
    }
%
\csuse{econbr@cmn@settings}
%
% TITLE
\def\title#1{%
    \vglue\title@skip
    \bgroup
        \no@harm
        \xdef\@argi{#1}%
    \egroup
    \write@pdfinfo{\hy@title}{\@argi}%
    \ifbre@layout
        \gdef\title@for@off{#1}%
        \write@offdata{title}{}{}{\expandafter\strip@prefix\meaning\title@for@off}%
    \fi
    \mdforvkt{main-article-info}{title}{#1}%
    \bgroup
        \title@size{#1}\par
    \egroup
    }
%
% AUG - author block
\let\sv@thanksref\thanksref % saving previous \thanksref macro
\@namedef{auto@1@and@user@1@printauthor@error}{%
    \econbr@error{macro \string\printauthor\space must be used 
        for all authors, because automatic printing for partial list
        can not process ranking correctly}%
    }
\newenvironment{aug}%
    {%
        \def\thanksref{%
            \ifcsundef{author@thanksref@used}%
                {%
                    \csxdef{author@thanksref@used}{\the\c@author}%
                }%
                {%
                    \edef\@tmp{\the\c@author}%
                    \ifnum\author@thanksref@used=\@tmp
                    \else
                        \econbr@error{more than once \string\thanksref\space found}%
                    \fi
                }% 
            \sv@thanksref
            }%
    }%
    {%
        \ifnum\n@authors>\z@
            \@tempcnta=\z@
            \edef\auto@printauthor@used{0}%
            \edef\user@printauthor@used{0}%
            \@whilenum\@tempcnta<\c@author\do{%
                \advance\@tempcnta by \@ne
                \edef\auto@prnaut@tmp{\@nameuse{econbr@author@numid@\the\@tempcnta}}%
                \edef\auto@prnead@tmp{\@nameuse{econbr@author@\the\@tempcnta @eadref@numid}}%
                \@ifundefined{printauthor@used@\auto@prnaut@tmp}%
                    {% auto
                        \@ifundefined{econbr@author@numdeceased@\the\@tempcnta}%
                            {%
                                \@ifundefined{econbr@author@\the\@tempcnta @eadref@numid}%
                                    {}%
                                    {%
                                        \begingroup\unrestored@protected@xdef\@thefnmark{}\endgroup
                                        \@footnotetext{%
                                            \csuse{printauthor@footnotetext@prehook}%
                                            \@printauthor{\auto@prnaut@tmp}: 
                                            \printead{\auto@prnead@tmp}%
                                            }%
                                        \edef\auto@printauthor@used{1}%
                                    }%
                            }%
                            {}%
                    }%
                    {% user
                        \@ifundefined{econbr@author@numdeceased@\the\@tempcnta}%
                            {\edef\user@printauthor@used{1}}%
                            {}%
                    }%
                }%
            \@nameuse{auto@\auto@printauthor@used @and@user@%
                \user@printauthor@used @printauthor@error}%
        \fi
    }
%
% AUTHOR
\def\and{\iflanguage{brazil}{\unskip~e~}{\unskip~and~}}
\newcounter{econbraddressref}
%
\define@key{ead}{label}{%
    \def\ead@label{#1}%
    \protected@write\@auxout{}{\string\csxdef{econbr@author@\the\c@author @eadref@numid}{#1}}%
    }
\define@key{author}{corref}[true]{\xdef\econbr@author@corref@num{\the\c@author}}
\define@key{author}{id}[default]{%
    \csxdef{econbr@author@numid@\the\c@author}{#1}%
    \protected@write\@auxout{}{%
        \string\csxdef{econbr@author@numid@\the\c@author}{#1}%
        \string\csxdef{econbr@author@idnum@#1}{\the\c@author}%
        }%
    }
\define@key{author}{deceased}[default]{%
    \csxdef{econbr@author@numdeceased@\the\c@author}{1}%
    \protected@write\@auxout{}{\string\csxdef{econbr@author@numdeceased@\the\c@author}{1}}%
    }
\define@key{author}{addressref}[default]{%
    \xdef\addressref@list{#1}%
    \@for\tmp@addr@ref:=#1\do{%
        \stepcounter{econbraddressref}%
        \@ifundefined{econbr@address@idnum@\tmp@addr@ref}%
            {%
                \ifnum\n@authors>\z@
                    \econbr@error{there is no address with label: \tmp@addr@ref}%
                \fi
            }%
            {}%
        \protected@write\@auxout{}{%
            \string\csxdef{econbr@author@\the\c@author @addressref@numid@%
                \the\c@econbraddressref}{\tmp@addr@ref}%
            }%
        }%
    \protected@write\@auxout{}{%
        \string\csxdef{econbr@author@\the\c@author @addressrefs}{\the\c@econbraddressref}%
        }%
    }
%
\ifbre@layout
    \let\econbr@save@sc@dot\.
    \csdef{econbr@\detokenize{\i}@sc@dot}{{\fontsize{.8em}{.8em}\selectfont\.I}}
    \protected\def\econbr@redef@sc@dot#1{%
        \ifcsundef{econbr@\detokenize{#1}@sc@dot}%
            {\econbr@save@sc@dot{#1}}%
            {\csuse{econbr@\detokenize{#1}@sc@dot}}%
        }
    \let\econbr@save@uc@dot\.
    \csdef{econbr@\detokenize{\i}@uc@dot}{\.I}
    \protected\def\econbr@redef@uc@dot#1{%
        \ifcsundef{econbr@\detokenize{#1}@uc@dot}%
            {\econbr@save@uc@dot{#1}}%
            {\csuse{econbr@\detokenize{#1}@uc@dot}}%
        }
\fi
\def\author{\@ifnextchar[{\author@fmt}{\author@fmt[]}}
%
\def\author@fmt[#1]#2{%
    \stepcounter{author}%
    \setcounter{econbraddressref}{0}%
    \ifnum\c@author=\@ne \author@fmt@init\fi
    \bgroup% for offdata and pdf metadata
        \def\inits##1{##1}%
        \def\fnms##1{{##1}}%
        \def\particle##1{##1}%
        \def\snm##1{##1}%
        \setkeys{author}{#1}%
        \edef\tmp@aut@id{\@nameuse{econbr@author@numid@\@arabic\c@author}}%
        \global\@namedef{author@from@label\tmp@aut@id}{%
            \def\inits####1{####1}%
            \def\fnms####1{####1}%
            \def\particle####1{####1}%
            \def\snm####1{####1}%
            \def\ead####1####{\@gobble}%
            #2%
            }%
        \ifnum\c@author=\@ne
            \addto@authors@list{\alph{author}}{}{#2}%
        \else
            \addto@authors@list{\alph{author}}{, }{#2}%
        \fi
        \ifbre@layout
            \ifx.#1.%
                \write@offdata{author}{\@arabic\c@author}{default}{#2}%
            \else
                \write@offdata{author}{\@arabic\c@author}%
                    {\csuse{econbr@author@numid@\@arabic\c@author}}{#2}%
            \fi
        \fi
    \egroup
    \bgroup% for view and aux metadata
        \def\fnms##1{%
            \ifbre@layout\fm@add@inits@from@name@by@aux{##1}\fi
            \cmd@for@aut@elem{fnms}{\the\c@author}{##1}%
            \mdforvkt{main-article-info}{fnm-\the\c@author}{##1}%
            \ifbre@layout
                \bgroup
                    \let\.\econbr@redef@sc@dot
                    ##1%
                \egroup
            \else
                ##1%
            \fi
            }%
        \def\inits##1{%
            \ifbre@layout
                \fm@add@inits@used
                \protected@write\@auxout{}{\string\csxdef{author@\the\c@author @inits@used}{1}}%
            \fi
            \cmd@for@aut@elem{inits}{\the\c@author}{##1}%
            \mdforvkt{main-article-info}{inits-\the\c@author}{##1}%
            ##1%
            }%
        \def\particle##1{%
            \cmd@for@aut@elem{snm}{\the\c@author}{##1 }%
            ##1%
            }% prefix to snm
        \def\snm##1{%
            \csgappto{\short@journal @snm@\the\c@author @author}{##1}%
            \mdforvkt{main-article-info}{snm-\the\c@author}{%
                \csuse{\short@journal @snm@\the\c@author @author}%
                }%
            \ifbre@layout
                \bgroup
                    \let\.\econbr@redef@sc@dot
                    ##1%
                \egroup
            \else
                ##1%
            \fi
            }%
        \ifbre@layout
            \scshape
        \fi
        #2%
    \egroup
    \endgraf
    \ifbre@layout
        \makeauthornumaffiliations{\the\c@author}%
    \fi
    \aff@fmt{\printauthornumaddresses{\the\c@author}}%
    }
% 
\def\authors@septwo{\iflanguage{brazil}{\unskip~e \ignorespaces}{\unskip~and \ignorespaces}}
\def\authors@sepmid{\unskip, \ignorespaces}
\def\authors@seplast{\iflanguage{brazil}{\unskip~e \ignorespaces}{\unskip~and \ignorespaces}}
\def\authors@endgraf{\endgraf}
% 
\def\author@fmt@init{%
    \vskip\authors@skip
    \leavevmode\author@size\author@prefix
    }
% 
\def\@printauthor#1{%
    \@ifundefined{author@from@label#1}%
        {\econbr@error{there is no author with label: #1}}%
        {\csname author@from@label#1\endcsname}%
    }
\newcommand\printauthor[1][]{%
    \@printauthor{#1}%
    \protected@write\@auxout{}{\string\csxdef{printauthor@used@#1}{1}}%
    }
%
\def\makeauthornumaffiliations#1{%
    \@ifundefined{econbr@aff@made@for@#1@author}%
        {%
            \@ifundefined{econbr@author@#1@addressrefs}%
                {}%
                {%
                    \@tempcntb=\z@
                    \edef\aff@toks{}%
                    \@whilenum\@tempcntb<\csuse{econbr@author@#1@addressrefs}\do{%
                        \advance\@tempcntb\@ne
                        \xdef\tmp@add@ref{%
                            \csuse{econbr@author@#1@addressref@numid@\the\@tempcntb}%
                            }%
                        \protected@edef\aff@toks{\aff@toks
                            \csuse{address@for@affiliation@num@%
                                \csuse{econbr@address@idnum@\tmp@add@ref}}%
                            \protect\sep@by@num@in@seq{\the\@tempcntb}%
                                {\csuse{econbr@author@#1@addressrefs}}%
                                {\iflanguage{brazil}{{} e }{{} and }}{, }{\iflanguage{brazil}{, e }{, and }} {}}%
                        }%
                        \protected@write\@auxout{}{%
                            \string\csxdef{affiliation@\csuse{econbr@author@numid@#1}}%
                            {\aff@toks}%
                            }%
                }%
        }%
        {}%
    }
%
% print all author addresses
\def\printauthornumaddresses#1{%
    \ifnum\n@authors>\z@
        \@tempcntb=\z@
        \@whilenum\@tempcntb<\@nameuse{econbr@author@#1@addressrefs}\do{%
            \advance\@tempcntb\@ne
            \edef\tmp@add@ref{\@nameuse{econbr@author@#1@addressref@numid@\the\@tempcntb}}%
            \printaddressnum{\@nameuse{econbr@address@idnum@\tmp@add@ref}}%
            \sep@by@num@in@seq{\the\@tempcntb}%
                {\@nameuse{econbr@author@#1@addressrefs}}%
                {\iflanguage{brazil}{{} e }{{} and }}{, }{\iflanguage{brazil}{, e }{, and }}{}%
            }%
    \fi
    }
%
% DEDICATED
\def\dedicated#1{%
    \vskip\dedicated@skip
    \bgroup
        \dedicated@size #1\par
    \egroup
    }
%
% ADDRESS
\define@key{address}{id}[default]{%
    \protected@write\@auxout{}{%
        \string\csxdef{econbr@address@numid@\the\c@address}{#1}%
        \string\csxdef{econbr@address@idnum@#1}{\the\c@address}%
        }%
    \csxdef{econbr@address@numid@\the\c@address}{#1}%
    \csxdef{econbr@address@idnum@#1}{\the\c@address}%
    }
%
\def\address{\@ifnextchar[{\address@fmt}{\address@fmt[default]}}
%
\ifbre@layout
    \def\aff@eat@stop{}
    \def\aff@eat@this#1\aff@eat@stop{}
    \def\aff@eat@this@till@comma#1,#2\aff@eat@stop{\ignorespaces #2}
    \def\def@orgdiv@for@aux{\def\orgdiv##1{\protect\aff@eat@stop\space##1}}
    \def\address@fmt[#1]#2{%
        \stepcounter{address}%
        \setkeys{address}{#1}%
        \edef\tmp@addr@id{\csuse{econbr@address@numid@\the\c@address}}%
        \expandafter\protected@xdef\csname address@ref@\tmp@addr@id\endcsname{\the\c@address}%
        \expandafter\newtoks\csname @address@\the\c@address\endcsname
            \expandafter\expandafter\expandafter\global
            \csname @address@\the\c@address\endcsname={#2}%
        \protected@write\@auxout
            {%
                \def@orgdiv@for@aux
                \def\dept##1{\protect\aff@eat@this@till@comma\space##1}%
                \def\orgname##1{\protect\aff@eat@stop\space##1}%
                \def\state##1{\protect\aff@eat@stop\space##1}%
                \def\street##1{\protect\aff@eat@this\space##1}%
                \def\postcode##1{\protect\aff@eat@stop\space##1}%
                \def\postbox##1{\protect\aff@eat@stop\space##1}%
                \def\city##1{\protect\aff@eat@stop\space##1}%
                \def\cny##1{\protect\aff@eat@stop\space##1}%
            }{%
                \string\global\string\@namedef{address@for@affiliation@num@\the\c@address}%
                {#2\string\aff@eat@stop}%
            }%
        \setkeys{address}{#1}%
        \protected@write\@auxout
            {%
                \let\dept\@firstofone
                \let\orgdiv\@firstofone
                \let\orgname\@firstofone
                \let\postbox\@firstofone
            }{%
                \string\global\string\@namedef{econbr@address@for@author@\the\c@address}{#2}%
            }%
        }
\else
    \def\address@fmt[#1]#2{%
        \stepcounter{address}%
        \setkeys{address}{#1}%
        \protected@write\@auxout
            {%
                \let\orgdiv\@firstofone
                \let\orgname\@firstofone
                \let\state\@firstofone
                \let\street\@firstofone
                \let\postcode\@firstofone
                \let\postbox\@firstofone
                \let\city\@firstofone
                \let\cny\@firstofone
            }{%
                \string\global\string\@namedef{econbr@address@for@author@\the\c@address}{#2}%
            }%
        \expandafter\newtoks\csname @address@\the\c@address\endcsname
        \expandafter\expandafter\expandafter\global\csname @address@\the\c@address\endcsname={#2}%
        }
\fi
\def\printaddressnum#1{%
    \ifnum\n@authors>\z@
        \bgroup
            \@ifundefined{econbr@address@for@author@#1}%
                {\econbr@error{there is no address with number: #1}}%
                {}%
            \@nameuse{econbr@address@for@author@#1}%
        \egroup
    \fi
    }
%
\gpreto\enddocument{%
    \@ifundefined{bibliographyexists}%
        {%
            \printhistory
        }%
        {%
            \medskip
            \ifbre@layout
            \else
                \hrule\@width 18mm\relax
            \fi
            \printhistory
        }%
    }
%
% affiliation
\def\affiliation{\@ifnextchar[{\affiliation@fmt}{\affiliation@fmt[default]}}
%
\long\def\aff@fmt#1{\bgroup\footnotesize\upshape#1\par\egroup
    \if@open@access
        \ifcsundef{econbr@too@long@open@access@text}%
            {}%
            {%
                \xdef\econbr@too@long@open@access@corr{%
                    \the\dimexpr\econbr@too@long@open@access@corr+\baselineskip\relax
                    }%
                \econbr@calc@oa@dim(\econbr@too@long@open@access@text/%
                    \econbr@too@long@open@access@spare)*\baselineskip\@nil
                \vskip-\econbr@temp@oa@skip
            }%
    \fi
    \vskip\baselineskip
    }
\ifbre@layout
    \def\affiliation@fmt[#1]#2{%
        \@for\tmp@aff@id:=#1\do{%
            \@ifundefined{econbr@author@idnum@\tmp@aff@id}%
                {\econbr@error{there is no author with id '\tmp@aff@id'}}%
                {}%
            \protected@write\@auxout{}{%
                \string\csxdef{econbr@aff@made@for@%
                    \csuse{econbr@author@idnum@\tmp@aff@id}@author}{1}%
                }%
            \write@offdata{affiliation}{}{\tmp@aff@id}{#2}%
            }%
        }
\else
    \def\affiliation@fmt[#1]#2{\aff@fmt{#2}}%
\fi
%
% ABSTRACT
\newbox\abstract@box
%
\def\abstract{%
    \global\setbox\abstract@box=\vbox\bgroup
        \@tempdima=\textwidth
        \advance\@tempdima by-\abstract@width
        \divide\@tempdima by 2%
        \abstract@style
        \leftskip=\@tempdima
        \rightskip=\@tempdima
        \abstract@size
        \parindent=\sv@parindent
        \noindent
        \hskip\abstract@indent
        {\abstractname@style\abstractname}%
        \abstractname@afterskip\ignorespaces
    }
\def\endabstract{\par\egroup}
%
\def\abstract@fmt{%
    \ifvoid\abstract@box
    \else
        \if@open@access
            \ifbre@layout
                \ifcsundef{econbr@too@long@open@access@text}%
                    {}%
                    {%
                        \xdef\econbr@too@long@open@access@corr{%
                            \the\dimexpr\econbr@too@long@open@access@corr+\abstract@skip\relax
                            }%
                        \econbr@calc@oa@dim(\econbr@too@long@open@access@text/%
                            \econbr@too@long@open@access@spare)*\abstract@skip\@nil
                        \vskip-\econbr@temp@oa@skip
                    }%
            \fi
        \fi
        \vskip\abstract@skip
        \unvbox\abstract@box
    \fi
    }
%
% pubmonth, doi, aid
\def\pubmonth#1{%
    \gdef\@pubmonth{#1}%
    \global\let\set@pubmonth@by@issue\relax
    }
\def\@pubmonth{0}
%
\AtBeginDocument{\set@pubmonth@by@issue}
%
\def\thepubmonth{%
    \@ifundefined{month@item@\@pubmonth}%
        {????}%
        {\getitemvalue{month}{\@pubmonth}}%
    }
\ifbre@layout
    \def\pubmonth@by@issue@list{0,1,3,5,7,9,11}
\else
    \def\pubmonth@by@issue@list{0,1,5,7,11}
\fi
\def\set@pubmonth@by@issue{%
    \@tempcnta=\z@
    \@for\my@iss:=\pubmonth@by@issue@list\do{%
        \ifnum\@issue=\@tempcnta
            \xdef\@pubmonth{\my@iss}%
            \xdef\pubmonth@set{1}%
        \fi
        \advance\@tempcnta\@ne
        }%
    \@ifundefined{pubmonth@set}%
        {\econbr@error{not found month by issue value \issue\space 
            from month list: \pubmonth@by@issue@list}}%
        {}%
    }
%
\def\doi#1{\gdef\@doi{#1}}
\def\@doi{}
\def\doi@base{https://doi.org/}
%
\def\@aid{<undef>}
\def\aid#1{%
    \ifx&#1&%
        \econbr@error{\string\aid\space number can not be empty!}%
    \else
        \gdef\@aid{#1}%
    \fi
    }
%
% doctopic
\DeclareRobustCommand\doctopic[2][]{%
    \mdforvkt{main-article-info}{doctopic}{#2}%
    \def\@tempa{#2}%
    \ifx\@tempa\@empty
    \else
        \def\@tempa{#1}%
        \ifx\@tempa\@empty
            \gdef\@doctopic{#2}%
        \else
            \@ifundefined{doctopic@#1@text@#2}%
                {\econbr@error{there is no such doctopic code!}}%
                {\csname doctopic@#1@extra@#2\endcsname
                    \xdef\@doctopic{\csname doctopic@#1@text@#2\endcsname}%
                }%
        \fi
    \fi
    }
%
\def\seqiniss#1{\gdef\@seqiniss{#1}\mdforvkt{main-article-info}{seqiniss}{#1}}
%
% HISTORY: received, accepted
\def\history@exist{0}
%
\def\@received{\@nil}
\def\received#1{%
    \def\@tempa{#1}%
    \ifx\@tempa\@empty
    \else
        \gdef\@received{#1}%
        \gdef\history@exist{1}%
    \fi
    }
\def\@accepted{\@nil}
\def\accepted#1{%
    \def\@tempa{#1}%
    \ifx\@tempa\@empty
    \else
        \gdef\@accepted{#1}%
        \gdef\history@exist{1}%
    \fi
    }
\global\let\revised\undefined
\def\@pubonline{\@nil}
\def\pubonline#1{%
    \def\@tempa{#1}%
    \ifx\@tempa\@empty
    \else
        \gdef\@pubonline{#1}%
        \gdef\history@exist{1}%
    \fi
    }
\def\@coeditor{\@nil}
\def\coeditor{\@ifnextchar[{\coeditor@}{\coeditor@[]}}
\def\coeditor@[#1]#2{%
    \ifx.#1.%
    \else
        \gdef\coeditor@prefix{#1\ }%
    \fi
    \def\@tempa{#2}%
    \ifx\@tempa\@empty
    \else
        \gdef\@coeditor{#2}%
        \gdef\history@exist{1}%
    \fi
    }
\ifbre@layout
    \def\coeditor@[#1]#2{%
        \setkeys{coeditor}{#1}%
        \def\@tempa{#2}%
        \ifx\@tempa\@empty
        \else
            \gdef\@coeditor{#2}%
            \gdef\history@exist{1}%
        \fi
        }
\fi
\def\empty@data{\@nil}
\def\history@fmt{%
    \ifcase\history@exist
    \else
        \ifbre@layout
            \par
            \vskip-\medskipamount
            \centerline{\rule[-3\p@]{18mm}{.4\p@}}%
        \fi
        \bgroup
            \nobreak
            \vskip\history@skip
            \nobreak
            \history@size
            \leavevmode
            \history@prefix
            \ifx\@coeditor\empty@data
            \else
                \bgroup
                    \let\prefix\@firstofone
                    \let\fnm\@firstofone
                    \let\snm\@firstofone
                    \let\suffix\@firstofone
                    \coeditor@prefix\@coeditor\coeditor@postfix
                    \smallskip\ifbre@layout\else\hfill\fi\break
                \egroup
            \fi
            \ifx\@received\empty@data
            \else
                \received@prefix\@received\received@postfix
            \fi
            \ifx\@accepted\empty@data
            \else
                \accepted@prefix\@accepted\accepted@postfix
            \fi
            \ifx\@pubonline\empty@data
            \else
                \pubonline@prefix\@pubonline\pubonline@postfix
            \fi
            \history@postfix\par
        \egroup
        \gdef\history@exist{0}%
    \fi
    }
\let\printhistory\history@fmt
%
\def\sday#1{#1}
\def\smonth@by@word#1{%
    \@ifundefined{month@item@#1}%
        {\econbr@error{there is no month with number: #1}}%
        {\getitemvalue{month}{#1}}%
    }
\ifrbe@layout
    \let\smonth\smonth@by@word
\else
    \def\smonth#1{#1}
    \let\smonth\smonth@by@word
\fi
\def\syear#1{#1}
\setvaluelist{month}{January,February,March,April,May,June,July,%
    August,September,October,November,December}
\expandafter\def\csname month@item@0\endcsname{????}
%
% KEYWORDS
\newbox\keyword@box
\newdimen\pre@kwd@depth
%
\def\keyword@AMS{AMS 2000 subject classifications:}
\def\keyword@KWD{\iflanguage{brazil}{Palavras-chave.}{Keywords.}}
\def\keyword@JEL{\iflanguage{brazil}{Classificação JEL.}{JEL classif{i}cation.}}
\def\keyword@class{KWD}
\ifbre@layout
    \def\keyword@KWD{\iflanguage{Palavras-chave:}{Keywords:}}
\fi
\gdef\keyword{\@ifnextchar[{\@keyword}{\@keyword[class=KWD]}}
\gdef\@keyword[#1]{%
    \do@option@list{keyword}{#1}%
    \def\keyword@name{\csname keyword@\keyword@class\endcsname}%
    \let\kwd@sep\relax
    \global\setbox\keyword@box=\vbox\bgroup
        \ifvoid\keyword@box
        \else
            \unvbox\keyword@box
            \vskip-\pre@kwd@depth
            \vtop to\pre@kwd@depth{}%
            \ifbre@layout\else\vskip2\p@\fi
        \fi
        \@tempdima\textwidth
        \advance\@tempdima by-\keyword@width
        \divide\@tempdima by 2%
        \keyword@style
        \leftskip\@tempdima
        \rightskip\@tempdima
        \keyword@size
        \parindent\sv@parindent
        \noindent
        \hskip\keyword@indent{\keywordname@size\keyword@name}\space
        \hskip.1\p@
    }
\def\endkeyword{%
        \keyword@postfix
        \par
        \global\pre@kwd@depth=\prevdepth
    \egroup
    }
\def\keyword@fmt{%
    \ifvoid\keyword@box
    \else
        \if@open@access
            \ifbre@layout
                \ifcsundef{econbr@too@long@open@access@text}%
                    {}%
                    {%
                        % keywords are close too abstract so only 30% of spare space is used
                        \@tempskipa=\keyword@skip
                        \xdef\econbr@too@long@open@access@corr{%
                            \the\dimexpr\econbr@too@long@open@access@corr+0.5\@tempskipa\relax
                            }%
                        \econbr@calc@oa@dim(\econbr@too@long@open@access@text/%
                            \econbr@too@long@open@access@spare)*0.5\@tempskipa\@nil
                        \vskip-\econbr@temp@oa@skip
                    }%
            \fi
        \fi
        \vskip\keyword@skip
        \unvbox\keyword@box
    \fi
    }
\def\sep{\unskip\string, }
\DeclareRobustCommand*\kwd{\@ifnextchar[\@kwd{\@kwd[\kwd@sep]}}
\gdef\keywords@list@sep{}
\def\@kwd[#1]#2{%
    \unskip
    #1{#2}%
    \addto@keywords@list{\keywords@list@sep #2}%
    \xdef\keywords@list@sep{, }%
    \let\kwd@sep\sep
    }
%
% Layouts:
\def\layout@notes{% todo: update for new converter?
    \xdef\layout@notes@used{1}%
    \let\seccnt@size\small
    \setlength\smallskipamount{4.5\p@ \@plus 1\p@ \@minus 1\p@}%
    \setlength\medskipamount{11\p@ \@plus 3\p@ \@minus 3\p@}%
    \setlength\bigskipamount{17\p@ \@plus 3\p@ \@minus 3\p@}%
    \def\dochead@size{\small\fontshape{uc}\selectfont}%
    \def\title@size{\small\fontshape{uc}\selectfont}%
    \def\author@size{\small\scshape}%
    \def\affiliation@size{\small\itshape}%
    \def\dedicated@size{\small\itshape}%
    \small
    }
%
% \tableofcontents
\csuse{econbr@imstoc}
%
% interaction with hyperref: {prefix}{font}{text}{url}
\def\url@fmt#1#2#3#4{%
    \edef\@tempa{#3}%
    \ifx\@tempa\@empty
    \else
        #1{#2\safe@href{#4}{#3}}%
    \fi
    }
%
\csuse{econbr@hyper@settings}
%
\def\write@pdfinfo#1#2{}
%
\newtoks\authors@list
\def\addto@authors@list#1#2#3{%
    \expandafter\gdef\csname authors@sep@#1\endcsname{#2}%
    \begingroup
        \no@harm
        \xdef\@act{%
            \global\noexpand\authors@list{%
                \the\authors@list\noexpand\csname authors@sep@#1\endcsname #3%
                }%
            }%
        \@act
    \endgroup
    }
%
\newtoks\keywords@list
\def\addto@keywords@list#1{%
    \begingroup
        \no@harm
        \xdef\@act{\global\noexpand\keywords@list{\the\keywords@list#1}}%
        \@act
    \endgroup
    }
%
% bibitem
\let\real@bibitem\bibitem
%
\csuse{econbr@print@msnbibl}
%
\csuse{econbr@urilist}
%
\csuse{econbr@bibliography@exists}
%
\csuse{econbr@offprint}
%
\csuse{econbr@bibtags}
%
\csuse{econbr@sici}
%
\csuse{econbr@backref}
%
\csuse{econbr@queries}
%
% SUPPORTING INFORMATION
\def\online@title{Supporting Information}
\def\online@text{Additional Supporting Information may be found in the online 
    version of this article at the \mbox{publisher's} website:}
\define@key{online}{title}{\def\online@title{#1}}
\define@key{online}{text}{\long\def\online@text{#1}}
\newenvironment{online}[1][]%
    {\setbox\@tempboxa\vbox\bgroup}%
    {\egroup\aftergroup\ignorespaces}%
\def\ESM#1#2#3{#3\endgraf}% todo: move to cfg if it is not needed in template
%% INITIALIZATION
% DIMENSIONS
\@twosidetrue
\pagenumbering{arabic}
\ifte@layout
    \def\etal{et~al\spacefactor=999\relax}
\else
    % condition \ifnum\sfcode`\.=\@m from kernel will fail, as \sfcode`.=1006 (from amsgen)
    \let\normalsfcodes\frenchspacing
    \frenchspacing
\fi
\init@settings
\pagestyle{headings}
%
\if@bookmarks
    \csuse{econbr@bookmarks@true}
\else
    \newwrite\@outlinefile
    \if@filesw\immediate\openout\@outlinefile=\jobname.out
        \immediate\closeout\@outlinefile
    \fi
\fi
%
\let\econbr@setref\@setref
\ifbre@layout
    \let\econbr@setref@textup\@firstofone
    \def\@setref#1#2#3{%
        \econbr@setref@textup{\econbr@setref{#1}{#2}{#3}}%
        }
\else
    \ifrbe@layout
        \def\@setref#1#2#3{%
            \textup{\econbr@setref{#1}{#2}{#3}}%
            }
    \fi
\fi
%
\if@xmlfmbm
    \def\xmlfirstpage#1{\gdef\@fp{#1}}
    \def\xmllastpage#1{\gdef\@lp{#1}}
    \renewenvironment{frontmatter}{}{}
    \def\title#1{%
        \bgroup
            \no@harm
            \xdef\@argi{#1}%
        \egroup
        \write@pdfinfo{\hy@title}{\@argi}%
        \write@pdfinfo{\hy@subject}{\@journal, \volume@label\space\@volume, 
            \issue@label\space\@issue, \thepubmonth, \the\@pubyear, \@fp-\@lp}%
        \ifbre@layout% there was no nor journalcopyrightline neither authorcopyrightline in ECTA
        \else
            \xdef\xml@authorcopyrightline{\noexpand\copyright\ \the\@pubyear\space 
                Brazilian Econometric Society}%
        \fi
        }%
\fi
% dirty trick...
\def\label@hyperref[#1]#2{% working backref for hyperlinks
    \protected@write\@auxout{}{\string\backrefcite{#1}{\thepage}}%
    \b@cite@color\hyper@linkstart{link}{#1.label}#2\hyper@linkend\e@cite@color
    }
%
\let\geqslant\geq
\let\leqslant\leq
% \frac, \cases, \(p)matrix arguments should be displaystyle 
% in displaystyle environments (as in ECTA)
\if@displaystyle@addons% todo: this one is redundant it duplicates amsmath env
    % original macros
    \let\terbe@orig@displaystyle\displaystyle
    \let\terbe@orig@frac\frac
    \let\terbe@orig@cases\cases
    \let\terbe@orig@matrix\matrix
    % display style addons (dsa) redefs
    \def\dsa@frac#1#2{\terbe@orig@frac{\terbe@orig@displaystyle #1}{\terbe@orig@displaystyle #2}}
    \def\dsa@cases#1{\left\{\,\vcenter{\normalbaselines\m@th
        \ialign{$\terbe@orig@displaystyle ##\hfil$&\quad{##}\hfil\crcr#1\crcr}}\right.}
    \def\dsa@matrix#1{%
        \null\,\vcenter{%
            \normalbaselines\m@th
            \ialign{%
                \hfil$\terbe@orig@displaystyle ##$\hfil&&\quad\hfil
                $\terbe@orig@displaystyle ##$\hfil\crcr
                \mathstrut\crcr\noalign{\kern-\baselineskip}%
                #1\crcr\mathstrut\crcr\noalign{\kern-\baselineskip}%
                }%
            }\,
        }
    % macro to activate dsa
    \def\displaystyle@addons{%
        \let\frac\dsa@frac
        \let\cases\dsa@cases
        \let\matrix\dsa@matrix% pmatrix comes from matrix
    }
    % dsa activating
    \everydisplay{\displaystyle@addons}
    \def\displaystyle{\displaystyle@addons\terbe@orig@displaystyle}
\fi
% macros for XML
\let\xmlstring\@gobble
\let\noxml\@firstofone
\def\inlinefig{\ignorespaces}
\newenvironment{displayfig}{\par}{\par}
\newenvironment{sgmlfig}{\ignorespaces}{}
\def\floatanchor#1{}
\csuse{econbr@xml@settings}
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CUSTOM SANS AND MONO FONTS - EDUARDO HORTA
\ifrbe@layout
 \RequirePackage[scaled]{berasans}
 \renewcommand*\familydefault{\sfdefault}
\fi
\RequirePackage[scaled]{beramono}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Additional packages - Eduardo Horta - 2023/01/12
\RequirePackage[dvipsnames]{xcolor}
\RequirePackage[colorlinks,citecolor=Blue,linkcolor=Blue,urlcolor=Blue,pagebackref,bookmarksnumbered]{hyperref}
\RequirePackage{mathptmx}
 \DeclareMathAlphabet{\mathcal}{OMS}{cmsy}{m}{n}
\RequirePackage{mathrsfs}
\RequirePackage{booktabs}
\RequirePackage{siunitx}
\RequirePackage{xurl}
\newcommand\hmmax{0}
\newcommand\bmmax{1}
\RequirePackage{bm}
\RequirePackage{anyfontsize} % prevents some warning messages regarding font sizes
\RequirePackage{scalefnt} % \scalefont{2} selects the current font in twice the current size. \scalefont{.75} reduces the current font size by three quarters.
% \usepackage[autostyle, english = american]{csquotes} % causes compilation crash
%   \MakeOuterQuote{"}
\RequirePackage{upquote} % correctly displays ' in verbatim and listings environments
\RequirePackage{icomma} % comma as decimal separator
\RequirePackage{pdflscape}
\RequirePackage{multirow}
% Some stuff from the natbib package:
\setcitestyle{notesep={; },round,aysep={},yysep={;}}
\bibpunct[,~]{(}{)}{;}{a}{,}{;}
\RequirePackage[brazil,english]{babel}
 \addto\captionsenglish{\renewcommand{\abstractname}{}}
 \addto\captionsbrazil{\renewcommand{\abstractname}{}}
\RequirePackage{placeins} % Control float placement (prevents floats from violating a section barrier). Usage: \FloatBarrier
\RequirePackage{subcaption}
\RequirePackage{orcidlink}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                          %%
%% Uncomment next line to change            %%
%% the type of equation numbering           %%
%%                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% \numberwithin{equation}{section}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                          %%
%% For Assumption, Axiom, Claim, Corollary, %%
%% Lemma, Theorem, Proposition, Hypothesis, %%
%% Fact                                     %%
%% use \theoremstyle{plain}                 %%
%%                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\theoremstyle{plain}
\newtheorem{axiom}{\iflanguage{brazil}{Axioma}{Axiom}}
\newtheorem{theorem}{\iflanguage{brazil}{Teorema}{Theorem}}
\newtheorem{proposition}{\iflanguage{brazil}{Proposição}{Proposition}}
\newtheorem{claim}{\iflanguage{brazil}{Afirmação}{Claim}}
\newtheorem{lemma}{\iflanguage{brazil}{Lema}{Lemma}}
\newtheorem*{fact}{\iflanguage{brazil}{Fato}{Fact}}
\newtheorem{conjecture}{\iflanguage{brazil}{Conjectura}{Conjecture}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                          %%
%% For Definition, Example, Remark,         %%
%% Notation, Property                       %%
%% use \theoremstyle{remark}                %%
%%                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\theoremstyle{remark}
\newtheorem{definition}{\iflanguage{brazil}{Definição}{Definition}}
\newtheorem*{example}{\iflanguage{brazil}{Exemplo}{Example}}
\newtheorem{remark}{\iflanguage{brazil}{Comentário}{Remark}}
\endinput
%%
%% End of file `econbr.cls'