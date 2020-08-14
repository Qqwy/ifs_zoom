(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "sec:org8f9c883"
    "sec:orgeb02fc9"
    "sec:org4fb4e63"
    "sec:orgbb4f03d"
    "section:background"
    "sec:orge4015d3"
    "figure:sierpinsky_iterations"
    "sec:orgf8ede43"
    "sec:org05fc178"
    "sec:org3fb9c6e"
    "subsection:viewport_transformation"
    "sec:orgd9c88b7"
    "sec:orgec9e894"
    "sec:org87844be"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orgd60f7e2"
    "sec:org995995a"
    "subsection:chaos_game_gpu"
    "sec:org5cab05e"
    "subsection:deterministic_gpu"
    "sec:orgb9df1e7"
    "section:research_question"
    "sec:org671eab7"
    "section:approach"
    "sec:orgc1566b9"
    "sec:org5c0d0f3"
    "sec:org422a315"
    "subsection:point_cloud_optimizations"
    "sec:orgc7faecc"
    "subsection:self_similarity"
    "sec:org79e031d"
    "subsection:coloring"
    "sec:org56f356b"
    "figure:program_flow"
    "sec:orgb4b7abb"
    "sec:org11f0740"
    "sec:org3d35ac1"
    "listing:barnsley_fern_ifs_file"
    "sec:orgb39f3f5"
    "sec:orgcac2cd0"
    "sec:orgf81d514"
    "sec:orga1cd3d9"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org3d4553f"
    "section:findings"
    "sec:orgd89b691"
    "subsection:jumping_restrictions"
    "sec:orga5e6fc5"
    "sec:org45314e8"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org7a0c967"
    "sec:orgae3806a"
    "section:conclusion"
    "sec:orgb665ec3"
    "section:further_work"
    "sec:orgb44c1e0"
    "ifs:sierpinsky"
    "ifs:barnsley_fern"
    "ifs:dragon_curve")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

