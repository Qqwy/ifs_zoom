(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "listings"
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
    "sec:org9623959"
    "sec:orgc0a5047"
    "sec:org92f0775"
    "sec:org42f5cf5"
    "section:background"
    "sec:org5f57f8d"
    "figure:sierpinsky_iterations"
    "sec:org3a6e622"
    "sec:orgcb6b293"
    "sec:orgef1c077"
    "subsection:viewport_transformation"
    "sec:orgc031634"
    "sec:org6591535"
    "sec:org0a0eb5a"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org02810b9"
    "sec:org3c1881b"
    "subsection:chaos_game_gpu"
    "sec:orgee2755d"
    "subsection:deterministic_gpu"
    "sec:org75b6d29"
    "section:research_question"
    "sec:org2f2185d"
    "section:approach"
    "sec:org09c95d4"
    "sec:org2aede4b"
    "sec:orgf5ea7df"
    "subsection:point_cloud_optimizations"
    "sec:org5759827"
    "subsection:self_similarity"
    "sec:org2a0a9cc"
    "subsection:coloring"
    "sec:org6ceeb97"
    "figure:program_flow"
    "sec:orgac17e25"
    "sec:orgec3c2f0"
    "sec:org4c167f2"
    "listing:barnsley_fern_ifs_file"
    "sec:org32e7e65"
    "sec:org7720b39"
    "sec:org5ce38bb"
    "sec:org0244476"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org490ff10"
    "section:findings"
    "sec:org4128958"
    "subsection:jumping_restrictions"
    "sec:org9b1e3d1"
    "sec:org3cc73ed"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org8774ee9"
    "sec:orge79537a"
    "section:conclusion"
    "sec:org0a4d1ed"
    "section:further_work"
    "sec:org302d291"
    "ifs:sierpinsky"
    "ifs:barnsley_fern"
    "ifs:dragon_curve")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

