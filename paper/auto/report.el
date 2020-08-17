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
    "sec:org0fe5ba9"
    "sec:org96ca77f"
    "sec:org9a7be85"
    "sec:orgd745088"
    "section:background"
    "sec:org4fdc02f"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:orga12ce52"
    "sec:orgda13e30"
    "sec:org8356919"
    "subsection:viewport_transformation"
    "sec:org59ac3d0"
    "sec:orgc0a2502"
    "sec:orgf7a627d"
    "sec:org1bcb7ef"
    "sec:org919f668"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org7298fa1"
    "sec:orgea36aaf"
    "subsection:chaos_game_gpu"
    "sec:orgef041cd"
    "subsection:deterministic_gpu"
    "sec:org42ccbaa"
    "section:research_question"
    "sec:orga39d3cb"
    "section:approach"
    "sec:org2089d58"
    "sec:org085a858"
    "sec:org5d21df7"
    "subsection:point_cloud_optimizations"
    "sec:orgb6ab90f"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org58dadf0"
    "subsection:coloring"
    "sec:org34bfe03"
    "figure:program_flow"
    "sec:org0db8ecc"
    "sec:orgdc0e1cf"
    "sec:org7b475a1"
    "listing:barnsley_fern_ifs_file"
    "sec:orga92ebb0"
    "sec:orgc48e0db"
    "sec:org8322f01"
    "sec:org957095f"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org193001b"
    "section:findings"
    "sec:orgc1c3d57"
    "subsection:jumping_restrictions"
    "sec:orgd9cfd4a"
    "figure:sierpinsky_transformation_borders"
    "sec:org7edcf05"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orgd12c6f9"
    "sec:orgd5e1e5a"
    "section:conclusion"
    "sec:org534d592"
    "section:further_work"
    "sec:org9577f3e"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

