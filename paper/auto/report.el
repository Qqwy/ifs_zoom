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
    "sec:org8b90939"
    "sec:orgb30d28b"
    "sec:org4a81518"
    "sec:org4fc9f93"
    "section:background"
    "sec:org1c491c6"
    "figure:sierpinsky_iterations"
    "sec:org90e6ebd"
    "sec:orgc5382c4"
    "sec:org3afa762"
    "subsection:viewport_transformation"
    "sec:orgfe40b54"
    "sec:orgf289b5f"
    "sec:org174c807"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org59f0f57"
    "sec:org9464c26"
    "subsection:chaos_game_gpu"
    "sec:org5993225"
    "subsection:deterministic_gpu"
    "sec:org397c674"
    "section:research_question"
    "sec:org8b69147"
    "section:approach"
    "sec:orgfa872b7"
    "sec:org1859678"
    "sec:orge5bc69c"
    "subsection:point_cloud_optimizations"
    "sec:org175b8ed"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org9ea1652"
    "subsection:coloring"
    "sec:orgdf81a66"
    "figure:program_flow"
    "sec:org4a6d5df"
    "sec:orgf90ab66"
    "sec:orge18b94f"
    "listing:barnsley_fern_ifs_file"
    "sec:orga03b977"
    "sec:org0fc2d49"
    "sec:org28168d0"
    "sec:orgf84b755"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:orgbdedfaf"
    "section:findings"
    "sec:org2261461"
    "subsection:jumping_restrictions"
    "sec:orgbb9f9e0"
    "sec:orgd6d5a96"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org6534f10"
    "sec:org0ae9120"
    "section:conclusion"
    "sec:orgc2e6b35"
    "section:further_work"
    "sec:org8f8ab2f"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

