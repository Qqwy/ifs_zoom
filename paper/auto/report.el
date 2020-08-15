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
    "sec:orgbf73d8d"
    "sec:org449267a"
    "sec:orge15cc62"
    "sec:orgbf93686"
    "section:background"
    "sec:orge63dd7c"
    "figure:sierpinsky_iterations"
    "sec:org6bbb3c0"
    "sec:orgfce7251"
    "sec:orgf986d6f"
    "subsection:viewport_transformation"
    "sec:org76381a2"
    "sec:org49cfff6"
    "sec:org94c5930"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:orgf26de1e"
    "sec:orge6ac5f7"
    "subsection:chaos_game_gpu"
    "sec:org5b04555"
    "subsection:deterministic_gpu"
    "sec:orgc9da940"
    "section:research_question"
    "sec:orgba5c68e"
    "section:approach"
    "sec:org60385e8"
    "sec:orge016262"
    "sec:org017fd4b"
    "subsection:point_cloud_optimizations"
    "sec:orgd05001c"
    "subsection:self_similarity"
    "sec:orgfa8677d"
    "subsection:coloring"
    "sec:orgf632874"
    "figure:program_flow"
    "sec:org5954dc1"
    "sec:org9bdcc64"
    "sec:org92b912e"
    "listing:barnsley_fern_ifs_file"
    "sec:org4471ce4"
    "sec:org0d212d3"
    "sec:orgf83c71c"
    "sec:org3240249"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org4136bac"
    "section:findings"
    "sec:org6f088f2"
    "subsection:jumping_restrictions"
    "sec:org6681592"
    "sec:orgf2038ec"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:org55cf330"
    "sec:orgea1fb94"
    "section:conclusion"
    "sec:org5966c97"
    "section:further_work"
    "sec:org9de4cd9"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

