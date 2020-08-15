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
    "sec:org9c8bb6e"
    "sec:org1bd7991"
    "sec:orgc74ff05"
    "sec:org3ec764b"
    "section:background"
    "sec:orgce4ee5a"
    "figure:sierpinsky_iterations"
    "sec:org43f6db7"
    "sec:orgef26424"
    "sec:org8aab46c"
    "subsection:viewport_transformation"
    "sec:org9edaaea"
    "sec:orgf36cb96"
    "sec:org9e7b017"
    "subsection:chaos_game"
    "figure:barnsley_mil"
    "figure:barnsley_ten_mil"
    "figure:barnsley_chaos_game_points"
    "chaosGame"
    "sec:org77a71e4"
    "sec:orgc099e97"
    "subsection:chaos_game_gpu"
    "sec:orgdd6db12"
    "subsection:deterministic_gpu"
    "sec:org184af1e"
    "section:research_question"
    "sec:org83d71c9"
    "section:approach"
    "sec:org4b50041"
    "sec:org15675e5"
    "sec:org323a291"
    "subsection:point_cloud_optimizations"
    "sec:orgee77b5d"
    "subsection:self_similarity"
    "sec:orgf2ddce1"
    "subsection:coloring"
    "sec:org3b8a51b"
    "figure:program_flow"
    "sec:orgd08fab0"
    "sec:org6761dcb"
    "sec:org81f9728"
    "listing:barnsley_fern_ifs_file"
    "sec:org6dfbb5d"
    "sec:orgc96ea5c"
    "sec:orgefe09b0"
    "sec:org76daa23"
    "figure:barnsley_guides"
    "figure:barnsley_guides_and_points"
    "figure:barnsley_guides_vs_points"
    "sec:org559db90"
    "section:findings"
    "sec:org9031a15"
    "subsection:jumping_restrictions"
    "sec:orgda709c9"
    "sec:org4f85e5e"
    "figure:dragon_curve_a"
    "figure:dragon_curve_b"
    "figure:dragon_curve_c"
    "figure:dragon_curve_d"
    "figure:dragon_curve_overlaps"
    "sec:orgdff8cf5"
    "sec:orgff3fbfb"
    "section:conclusion"
    "sec:org447c2ec"
    "section:further_work"
    "sec:org1bada2f"
    "ifs:sierpinsky"
    "ifs:barnsley_fern"
    "ifs:dragon_curve")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-newfloat-DeclareFloatingEnvironments
    '("lsystem" "")))
 :latex)

