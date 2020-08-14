(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "float"
    "geometry"
    "algorithm2e"
    "xcolor"
    "pifont"
    "subcaption"
    "enumitem"
    "newfloat"
    "todonotes"
    "kantlipsum"
    "rugscriptie")
   (TeX-add-symbols
    "cmark"
    "xmark"
    "altasterism")
   (LaTeX-add-labels
    "sec:orgde1411c"
    "sec:org955c84e"
    "sec:org5e0b774"
    "sec:org2e4d289"
    "section:background"
    "sec:org3f24cad"
    "sec:org9e5c1ce"
    "sec:orgda34ec5"
    "sec:org2d39b8e"
    "sec:org83d3092"
    "subsection:chaos_game"
    "chaosGame"
    "sec:orgb3f4a04"
    "sec:org2bf1ab0"
    "subsection:chaos_game_gpu"
    "sec:org340f0e7"
    "subsection:deterministic_gpu"
    "sec:org70905fa"
    "sec:org56620cd"
    "sec:org1fbc49b"
    "sec:orgea7407b"
    "sec:org4b7c4d4"
    "subsection:point_cloud_optimizations"
    "sec:orgbe5cb60"
    "subsection:self_similarity"
    "sec:orga07a04a"
    "sec:orgfaa2da5"
    "sec:org15753fd"
    "sec:org2e5b81c"
    "sec:org7728112"
    "sec:orgee3f036"
    "sec:org8c9f369"
    "sec:org034a619"
    "sec:org97036da"
    "sec:orgfb615a4"
    "sec:org5e7e4dc"
    "sec:org6c64b3f"
    "sec:org6d22faf"
    "sec:orgc5ae2be"
    "sec:orga8ef36f"
    "sec:org39909ce")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

