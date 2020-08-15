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
    "sec:org9854f1c"
    "sec:orgd1b06aa"
    "sec:orgc42c656"
    "sec:orga9c5e61"
    "section:background"
    "sec:org326c556"
    "figure:sierpinsky_iterations"
    "sec:orgf8b444a"
    "sec:org49102d5"
    "sec:orgc1a2b21"
    "subsection:viewport_transformation"
    "sec:org9da4c3f"
    "sec:org217e165"
    "sec:orgf93f516"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org49b5ea7"
    "sec:org029d2ae"
    "subsection:chaos_game_gpu"
    "sec:org575e15b"
    "subsection:deterministic_gpu"
    "sec:org450d7ed"
    "section:research_question"
    "sec:orgae4a53b"
    "section:approach"
    "sec:org3cdbdd0"
    "sec:org6b49e1a"
    "sec:org05e8372"
    "subsection:point_cloud_optimizations"
    "sec:org57c8b7f"
    "subsection:self_similarity"
    "sec:org0b00b60"
    "subsection:coloring"
    "sec:org0ef1806"
    "figure:program_flow"
    "sec:orga2c51f5"
    "sec:orgc94da2a"
    "sec:org4c22d9b"
    "listing:barnsley_fern_ifs_file"
    "sec:org4507a57"
    "sec:orgb079939"
    "sec:org503fd53"
    "sec:org302e9d4"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:orgc9570dc"
    "section:findings"
    "sec:org66f8907"
    "subsection:jumping_restrictions"
    "sec:org5801534"
    "sec:org1eef9d6"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org8f0c896"
    "sec:org859e1d7"
    "section:conclusion"
    "sec:orge9c6be0"
    "section:further_work"
    "sec:orgd0fd36b"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

