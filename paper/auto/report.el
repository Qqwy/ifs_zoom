(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
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
    "sec:org782f9d6"
    "sec:org16646db"
    "sec:org48b19dd"
    "sec:org154a729"
    "section:background"
    "sec:orgbe4e2fc"
    "sec:orgaab5fd9"
    "sec:org03f2397"
    "sec:org56ea640"
    "subsection:viewport_transformation"
    "sec:org1ff68b9"
    "sec:orgbe18a8c"
    "sec:org6e66bad"
    "subsection:chaos_game"
    "chaosGame"
    "sec:orgeceb933"
    "sec:org536623b"
    "subsection:chaos_game_gpu"
    "sec:org2344154"
    "subsection:deterministic_gpu"
    "sec:org7da9d97"
    "section:research_question"
    "sec:orgc6ae034"
    "section:approach"
    "sec:org1087d3f"
    "sec:org4dcdc14"
    "sec:org70d7668"
    "subsection:point_cloud_optimizations"
    "sec:orgcd38a76"
    "subsection:self_similarity"
    "sec:org610e614"
    "subsection:coloring"
    "sec:org60c3ccb"
    "figure:program_flow"
    "sec:orgb0a68cc"
    "sec:orgc38f891"
    "sec:orgf768fb0"
    "sec:org2826a46"
    "sec:org245a010"
    "sec:org9035a3d"
    "sec:org65ea20c"
    "sec:org6f842db"
    "section:findings"
    "sec:org8f1cabd"
    "subsection:jumping_restrictions"
    "sec:org488bbf1"
    "sec:org0beb14e"
    "figigure:dragon_curve_a"
    "figigure:dragon_curve_b"
    "fig:five over x"
    "figigure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org0fd95bc"
    "sec:org9a80e89"
    "section:conclusion"
    "sec:org8df2c05"
    "section:further_work")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

