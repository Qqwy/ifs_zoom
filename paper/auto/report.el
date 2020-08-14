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
    "sec:org4ce1f34"
    "sec:org4fac3a5"
    "sec:orgd1c0691"
    "sec:org00bff84"
    "section:background"
    "sec:org3c3c85b"
    "figure:sierpinsky_iterations"
    "sec:org2b0b9d9"
    "sec:orge0e3ca2"
    "sec:org72d009b"
    "subsection:viewport_transformation"
    "sec:org8fd1462"
    "sec:org99d4759"
    "sec:orgade29b6"
    "subsection:chaos_game"
    "chaosGame"
    "sec:org5827968"
    "sec:orge7c52a3"
    "subsection:chaos_game_gpu"
    "sec:org8dbf566"
    "subsection:deterministic_gpu"
    "sec:orgd15dd1c"
    "section:research_question"
    "sec:org118baf6"
    "section:approach"
    "sec:org7a15ed7"
    "sec:orgd7b6cf9"
    "sec:org1bb2bc3"
    "subsection:point_cloud_optimizations"
    "sec:orgb1f4922"
    "subsection:self_similarity"
    "sec:org51ff82f"
    "subsection:coloring"
    "sec:org6af2525"
    "figure:program_flow"
    "sec:orgf58f1a8"
    "sec:org3c3b2b3"
    "sec:orgb946bcb"
    "sec:orgb30e0ad"
    "sec:org5d5e5c2"
    "sec:org5dd0a31"
    "sec:org7b39d67"
    "sec:org39e32bd"
    "section:findings"
    "sec:org4bb8045"
    "subsection:jumping_restrictions"
    "sec:org70b49b0"
    "sec:org74826ba"
    "figigure:dragon_curve_a"
    "figigure:dragon_curve_b"
    "fig:five over x"
    "figigure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org82da4a8"
    "sec:org15662eb"
    "section:conclusion"
    "sec:org4e032ab"
    "section:further_work"
    "sec:orgcdc464f"
    "ifs:sierpinsky")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

