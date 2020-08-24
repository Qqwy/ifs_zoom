(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("geometry" "a4paper" "total={7in, 9in}") ("caption" "font={small, it}" "labelfont=bf") ("algorithm2e" "ruled" "procnumbered") ("xcolor" "dvipsnames") ("enumitem" "shortlabels")))
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
    "sec:org3f9993d"
    "sec:orge8b8518"
    "sec:org4af0e97"
    "sec:orgcbf3c27"
    "section:background"
    "sec:org6645525"
    "subsection:informal_description"
    "figure:sierpinsky_iterations"
    "sec:orgcc748a2"
    "sec:org1ef4bed"
    "sec:org12f59c1"
    "subsection:viewport_transformation"
    "sec:orge7d4493"
    "sec:orge0ae8ed"
    "sec:org251c56b"
    "sec:orgef4b8a3"
    "sec:orgd925052"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org046b784"
    "sec:org9e84a46"
    "subsection:chaos_game_gpu"
    "sec:org838bcba"
    "subsection:deterministic_gpu"
    "sec:org2e4dac1"
    "section:research_question"
    "sec:orgebdc313"
    "section:approach"
    "sec:orgf748eb7"
    "sec:org0ba52fc"
    "sec:orga5b617f"
    "subsection:point_cloud_optimizations"
    "sec:org11b646a"
    "subsection:self_similarity"
    "figure:sierpinsky_jump"
    "algorithm:self_similarity_jump_up"
    "algorithm:self_similarity_jump_down"
    "sec:org9844bd2"
    "subsection:coloring"
    "sec:org0ad68f8"
    "figure:program_flow"
    "sec:orgb5aa0b0"
    "sec:org1084a42"
    "sec:org953ebb1"
    "listing:barnsley_fern_ifs_file"
    "sec:org392da5d"
    "sec:org03f73d9"
    "sec:orgc156aa8"
    "sec:orgc1fad93"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org7f50e09"
    "section:findings"
    "sec:org459c193"
    "subsection:jumping_restrictions"
    "sec:org67836f4"
    "figure:sierpinsky_transformation_borders"
    "sec:orgd1048e2"
    "figure:barnsley_jump_a"
    "figure:barnsley_jump_b"
    "figure:barnsley_jump"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orgf5d2945"
    "sec:org56d19a9"
    "section:conclusion"
    "sec:orgd2bca6d"
    "section:further_work"
    "sec:org30b64ea"
    "ifs:sierpinsky"
    "ifs:dragon_curve"
    "ifs:barnsley_fern")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("ifs" "")))
 :latex)

