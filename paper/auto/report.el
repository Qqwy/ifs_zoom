(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("caption" "font={small, it}" "labelfont=bf") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
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
    "caption"
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
    "sec:org75ae872"
    "sec:org3f54b6c"
    "sec:orgda465c1"
    "sec:orgbc60861"
    "section:background"
    "sec:orgc4f789c"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:org592ca6c"
    "sec:org380bcd7"
    "sec:org853dad3"
    "subsection:viewport_transformation"
    "sec:orgbe3dc2b"
    "sec:org15ab9e7"
    "sec:org7cc6ddb"
    "sec:org5a382e3"
    "sec:orgbb5d40d"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org50c7bb2"
    "sec:org5ab1bc5"
    "subsection:chaos_game_gpu"
    "sec:orgfc7308d"
    "subsection:deterministic_gpu"
    "sec:orgaa3d097"
    "section:research_question"
    "sec:org5e300a8"
    "section:approach"
    "sec:orge92a303"
    "sec:org0b3226e"
    "sec:org0d12b56"
    "subsection:point_cloud_optimizations"
    "sec:org68f4b58"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:orga15df82"
    "subsection:coloring"
    "sec:org9df2d44"
    "figure:program_flow"
    "sec:orge59b2d2"
    "sec:org2a63df0"
    "sec:org0611e65"
    "listing:barnsley_fern_ifs_file"
    "sec:org83a6d4a"
    "sec:org8b2b925"
    "sec:orgf9f2112"
    "sec:orga43ce0e"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:orga80cb18"
    "section:findings"
    "sec:org25ae74f"
    "subsection:jumping_restrictions"
    "sec:org18addf4"
    "figure:sierpinsky_transformation_borders"
    "sec:org9c6d048"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org85e37f9"
    "sec:orgaa9b652"
    "section:conclusion"
    "sec:org0b9f756"
    "section:further_work"
    "sec:org113c10d"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

