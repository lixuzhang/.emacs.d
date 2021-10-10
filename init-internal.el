;;; init-internal.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020,2021  李旭章

;; Author: 李旭章 <lixuzhang@lovefeeling.org>
;; Keywords: internal


;; *** 内部包
;; #+BEGIN_SRC emacs-lisp
;; autoinsert.el --- automatic mode-dependent insertion of text into new files
(use-package autoinsert
  :custom (auto-insert-mode t))

;; autorevert.el --- revert buffers when files on disk change
(use-package autorevert
  :custom (global-auto-revert-mode t))

;; avoid.el --- make mouse pointer stay out of the way of editing
(use-package avoid
  :custom (mouse-avoidance-mode 'animate))

;; battery.el --- display battery status information
(use-package battery
  :hook (after-init . display-battery-mode))

;; bookmark.el --- set bookmarks, maybe annotate them, jump to them later
(use-package bookmark)

;; dabbrev.el --- dynamic abbreviation package
(use-package dabbrev)

;; delsel.el --- delete selection if you insert
(use-package delsel
  :custom (delete-selection-mode t))

;; descr-text.el --- describe text mode
(use-package descr-text
  :custom (describe-char-unidata-list t))

;; desktop.el --- save partial status of Emacs when killed
(use-package desktop
  :custom
  (desktop-load-locked-desktop t)
  (desktop-save-mode t))

;; doc-view.el --- View PDF/PostScript/DVI files in Emacs
(use-package doc-view
  :init (if (executable-find "gswin32c")
            (setq doc-view-ghostscript-program "gswin32c")))

;; hexl.el --- edit a file in a hex dump format using the hexl filter
(use-package hexl
  :hook
  (hexl-mode-hook . hexl-follow-line)
  (hexl-mode-hook . hexl-activate-ruler))

;; hippie-exp.el --- expand text trying various ways to find its expansion
(use-package hippie-exp
  :after dabbrev
  :bind ("M-/" . hippie-expand)
  :custom (hippie-expand-try-functions-list
              '(try-expand-dabbrev
                try-expand-dabbrev-visible
                try-expand-dabbrev-from-kill
                try-expand-dabbrev-all-buffers
                try-complete-file-name-partially
                try-complete-file-name
                try-expand-all-abbrevs
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol
                try-expand-list
                try-expand-line)))

;; hl-line.el --- highlight the current line
(use-package hl-line
  :config (global-hl-line-mode 1))

;; ido.el --- interactively do things with buffers and files
(use-package ido
  :custom
  (ido-auto-merge-work-directories-length 0)
  (ido-enable-flex-matching t)
  (ido-use-faces nil)
  (ido-use-virtual-buffers t)
  (ido-use-filename-at-point nil)
  (ido-mode 1)
  (ido-everywhere 1)
  :config
  (add-to-list 'ido-ignore-files "\\.DS_Store"))

;; image-file.el --- support for visiting image files
(use-package image-file
  :custom (auto-image-file-mode t))

;; let-alist.el --- Easily let-bind values of an assoc-list by their names
(use-package let-alist)

;; leuven-theme.el --- Awesome Emacs color theme on white background
(use-package leuven-theme
  :custom
  (leuven-scale-outline-headlines nil)
  (leuven-scale-org-agenda-structure nil)
  :init
  (load-theme 'leuven t))

;; linum.el --- display line numbers in the left margin
(use-package linum
  :hook (prog-mode . linum-mode))

;; paren.el --- highlight matching paren
(use-package paren
  :custom
  (show-paren-style 'expression)
  (show-paren-mode t))

;; savehist.el --- Save minibuffer history
(use-package savehist
  :custom (savehist-mode t))

;; saveplace.el --- automatically save place in files
(use-package saveplace
  :custom (save-place-mode t))

;; select.el --- lisp portion of standard selection support
(use-package select
  :custom
  (select-enable-primary t)
  (select-enable-clipboard t)
  :init (progn
          (when (display-graphic-p)
            (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))))

;; server.el --- Lisp code for GNU Emacs running as server process
(use-package server
  :custom
  (server-use-tcp t)
  (server-mode t))

;; skeleton.el --- Lisp language extension for writing statement skeletons
(use-package skeleton
  :custom
  (skeleton-further-elements '((abbrev-mode nil)))
  (skeleton-pair t)
  (skeleton-pair-alist
        '((?\' _ "\'" >)
          (?\" _ "\"" >)
          (?\< _ ">" >)
          (?\( _ ")" >)
          (?\[ _ "]" >)
          (?\{ _ "}" >))))

;; time.el --- display time, load and mail indicator in mode line of Emacs
(use-package time
  :custom
  (display-time-use-mail-icon t)
  (display-time-24hr-formatxk t)
  (display-time-day-and-date nil)
  (display-time-default-load-average nil)
  (display-time-mode t))

;; time-stamp.el --- Maintain last change time stamps in files edited by Emacs
(use-package time-stamp
  :hook (before-save . time-stamp)
  :custom (time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %U"))

;; whitespace.el --- minor mode to visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :diminish global-whitespace-mode
  :custom
  (whitespace-line-column nil) ; Use fill-column
  (whitespace-style
   '(face trailing lines-tail tabs))
  (global-whitespace-mode t))

;; winner.el --- Restore old window configurations
(use-package winner
  :config (when (fboundp 'winner-mode)
            (winner-mode t)))
;; #+END_SRC

;; **** calendar
;; #+BEGIN_SRC emacs-lisp
;; calendar.el --- calendar functions
(use-package calendar
  :init (progn
          (setq diary-file "~/life/diary")
          ;; Week days
          (setq calendar-week-start-day 0) ; 0:星期天, 1:星期一
          (setq calendar-date-style 'iso)
          ;; [[http://huayuqiao.org/articles/huangheqing/hhq16.htm][从七曜说到“礼拜”、“星期”、“周”的语源]]
          ;; [[http://www.zhihu.com/question/19903543][为什么日语从周一到周日称作“月曜日、火曜日、水曜日、木曜日、金曜日、土曜日、日曜日”？和中国的五行有什么关系吗？]]
          (setq calendar-day-name-array
                ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])
          (setq calendar-day-abbrev-array
                ["周日" "周一" "周二" "周三" "周四" "周五" "周六"])
          (setq calendar-day-header-array ;; ["日" "月" "火" "水" "木" "金" "土"]
                ["日" "一" "二" "三" "四" "五" "六"])
          ;; Month
          (setq calendar-month-name-array
                ["一月" "二月" "三月" "四月" "五月" "六月"
                 "七月" "八月" "九月" "十月" "十一月" "十二月"])
          (setq calendar-month-abbrev-array
                ["㋀" "㋁" "㋂" "㋃" "㋄" "㋅" "㋆" "㋇" "㋈" "㋉" "㋊" "㋋"])
          (setq calendar-month-header '(propertize (format "%d 年 %d 月" year month)
                                                   'font-lock-face
                                                   'calendar-month-header))
          (setq calendar-chinese-all-holidays-flag t)
          (setq calendar-mark-diary-entries-flag t)
          (setq calendar-mark-holidays-flag t)
          ;; lunar.el --- calendar functions for phases of the moon
          (setq lunar-phase-names '("🌑" "🌓" "🌕" "🌗"))
          ;; holidays.el --- holiday functions for the calendar package
          (setq calendar-chinese-celestial-stem   ; 天干
                ["甲" "乙" "丙" "丁" "戊" "已" "庚" "辛" "壬" "癸"])
          (setq calendar-chinese-terrestrial-branch ; 地支
                ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
          (setq calendar-chinese-month-name-array
                ["正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "冬月" "腊月"])
          (setq holiday-general-holidays ; 法定假日
                '((holiday-fixed 1 1 "新年")
                  (holiday-fixed 3 8 "妇女节")
                  (holiday-fixed 5 1 "劳动节")
                  (holiday-fixed 5 4 "青年节")
                  (holiday-fixed 6 1 "儿童节")
                  (holiday-fixed 9 10 "教师节")
                  (holiday-fixed 10 1 "国庆节")))
          (setq holiday-local-holidays  ; 法定节日
                '((holiday-fixed 3 12 "植树节")
                  (holiday-fixed 5 12 "护士节")
                  (holiday-fixed 7 1 "建党节")
                  (holiday-fixed 8 1 "建军节")
                  (holiday-fixed 11 8 "记者节")
                  (holiday-fixed 12 13 "国家公祭日")))
          (setq holiday-other-holidays  ; 民众节日
                '((holiday-fixed 2 14 "情人节")
                  (holiday-fixed 4 1 "愚人节")
                  (holiday-float 5 0 2 "母亲节")
                  (holiday-float 6 0 3 "父亲节")
                  (holiday-fixed 10 24 "程序员节")
                  (holiday-fixed 10 31 "万圣节")
                  (holiday-fixed 11 11 "光棍节")
                  (holiday-float 11 4 4 "感恩节")
                  (holiday-fixed 12 25 "圣延节")))
          ;; cal-china.el --- calendar functions for the Chinese calendar
          (setq holiday-oriental-holidays    ; 传统节日
                '((holiday-chinese-new-year) ; "春节"
                  (holiday-chinese-qingming) ; "清明"
                  (holiday-chinese 5 5 "端午")
                  (holiday-chinese 7 7 "七夕")
                  (holiday-chinese 8 15 "中秋")
                  (holiday-chinese 9 9 "重阳")
                  (holiday-chinese-winter-solstice) ; "冬至"
                  (holiday-chinese 12 30 "除夕"))))
  :config (progn
            (require 'holidays)
            (require 'cal-china)
            (setq calendar-holidays
                  (append holiday-general-holidays
                          holiday-other-holidays
                          holiday-oriental-holidays
                          holiday-local-holidays))
            (require 'appt) ;; appt.el --- appointment notification functions
            (appt-activate t)))

;; diary-lib.el --- diary functions
(use-package diary-lib
  :after calendar
  :init (setq diary-remind-message
              '("提醒：离"
                diary-entry
                "仅剩"
                (if (zerop (% days 7))
                    (format "%d 周" (/ days 7))
                  (format "%d 天" days)))))

;; timeclock.el --- mode for keeping track of how much you work
(use-package timeclock
  :bind (("C-x t i" . timeclock-in)
         ("C-x t o" . timeclock-out)
         ("C-x t c" . timeclock-change)
         ("C-x t r" . timeclock-reread-log)
         ("C-x t u" . timeclock-update-mode-line)
         ("C-x t w" . timeclock-when-to-leave-string))
  :config (timeclock-mode-line-display))
;;; ---------------------------------------------------------------------------
;; #+END_SRC

;; **** cedet
;; #+BEGIN_SRC emacs-lisp
;;; ---------------------------------------------------------------------------
;;; cedet

;; cedet.el --- Setup CEDET environment
(use-package cedet)

;; ede.el --- Emacs Development Environment gloss
(use-package ede
  :init (setq ede-project-directories t)
  :config (progn
            (global-ede-mode 1)
            ;; ede/generic.el --- Base Support for generic build systems
            (require 'ede/generic)
            (ede-enable-generic-projects)))

;; semantic.el --- Semantic buffer evaluator.
(use-package semantic
  :init (setq semantic-default-submodes
              '(global-semanticdb-minor-mode
                ;; global-semantic-idle-scheduler-mode
                ;; global-semantic-idle-summary-mode
                ;; global-semantic-idle-completions-mode
                ;; global-semantic-idle-local-symbol-highlight-mode
                global-semantic-decoration-mode
                global-semantic-mru-bookmark-mode
                global-semantic-stickyfunc-mode
                global-semantic-highlight-func-mode
                global-semantic-highlight-edits-mode
                global-semantic-show-unmatched-syntax-mode
                global-semantic-show-parser-state-mode))
  :config (progn
            (semantic-mode 1)

            ;; semantic/db-find.el --- Searching through semantic databases.
            (require 'semantic/db-find)
            (setq-mode-local c-mode semanticdb-find-default-throttle
                             '(project unloaded system recursive))

            ;; semantic/db-global.el --- Semantic database extensions for GLOBAL
            (require 'semantic/db-global)
            (semanticdb-enable-gnu-global-databases 'c-mode t)
            (semanticdb-enable-gnu-global-databases 'c++-mode t)

            ;; semantic/ia.el --- Interactive Analysis functions
            (require 'semantic/ia)
            (setq hippie-expand-try-functions-list
                  (cons 'semantic-ia-complete-symbol hippie-expand-try-functions-list))

            ;; semantic/bovine/gcc.el --- gcc querying special code for the C parser
            (require 'semantic/bovine/gcc)
            (semantic-gcc-setup)

            ;; semantic/bovine/c.el --- Semantic details for C
            (require 'semantic/bovine/c)
            (setq semantic-c-dependency-system-include-path
                  (semantic-gcc-get-include-paths "c"))

            ;; semantic/bovine/scm.el --- Semantic details for Scheme (guile)
            (require 'semantic/bovine/scm)))


;; srecode.el --- Semantic buffer evaluator.
(use-package srecode
  :config (progn
            ;; srecode/mode.el --- Minor mode for managing and using SRecode templates
            (require 'srecode/mode)
            (global-srecode-minor-mode 1)))
;;; ---------------------------------------------------------------------------
;; #+END_SRC

;; **** org
;; #+BEGIN_SRC emacs-lisp
;;; ---------------------------------------------------------------------------
;;; org

;; org.el --- Outline-based notes management and organizer
(use-package org
  :after remember
  :init (progn
          (setq org-fontify-whole-heading-line  t
                org-support-shift-select        t
                org-startup-indented            t
                org-startup-align-all-tables    t
                org-startup-with-inline-images  t
                org-startup-with-latex-preview  nil)
          ;; 任务管理
          (setq org-directory "~/life")
          (setq org-default-notes-file remember-data-file) ; 备忘录
          (setq org-agenda-files '("~/life/"))
          (setq org-enforce-todo-dependencies t)
          (setq org-track-ordered-property-with-tag t)
          (setq org-todo-keywords
                '((type "🗁PROJECT(P)" "🗲ACTION(A)" "🗒SOMEDAY/MAYBE(S)" "🛈REFERENCE(R)" "🚮TRASH(T)") ; 任务分类
                  (sequence "TODO(t)" "HOLD(h@/!)" "NEXT(n@/!)" "WAIT(w@/!)" "SCHEDULE(s)" "PHONE(p/!)" "MEETTING(m/!)"
                            "|" "CANCELLED(c@/!)" "DONE(d)"))) ; 执行状态
          ;; "🗁PROJECT(P)" "🗲ACTION(A)" "🗒SOMEDAY/MAYBE(S)" "🛈REFERENCE(R)" "🚮TRASH(T)"
          ;; "☐(t)" "◯(h@/!)" "⭙(n@/!)" "⏳(w@/!)" "📆(s)" "✆(p/!)" "🗫(m/!)" "|" "☒(c@/!)" "☑(d)"

          (setq org-todo-keyword-faces
                '(("☐" :foreground "red" :weight bold)
                  ("◯" :foreground "magenta" :weight bold)
                  ("⭙" :foreground "blue" :weight bold)
                  ("⏳" :foreground "orange" :weight bold)
                  ("📆" :foreground "blue" :weight bold)
                  ("✆" :foreground "forest green" :weight bold)
                  ("🗫" :foreground "forest green" :weight bold)
                  ("☒" :foreground "forest green" :weight bold)
                  ("☑" :foreground "forest green" :weight bold)))

          (setq org-use-fast-todo-selection t)
          (setq org-treat-S-cursor-todo-selection-as-state-change t)
          ;; (setq org-todo-state-tags-triggers
          ;;       '(("🗁PROJECT" ("🗁" . t) ("🗲") ("🗒") ("🛈") ("🚮") ("🏳") ("⏳"))
          ;;         ("🗲ACTION" ("🗁") ("🗲" . t) ("🗒") ("🛈") ("🚮") ("🏳") ("⏳"))
          ;;         ("🗒SOMEDAY/MAYBE" ("🗁") ("🗲") ("🗒" . t) ("🛈") ("🚮") ("🏳") ("⏳"))
          ;;         ("🛈REFERENCE"  ("🗁") ("🗲") ("🗒") ("🛈" . t) ("🚮") ("🏳") ("⏳"))
          ;;         ("🚮TRASH" ("🗁") ("🗲") ("🗒") ("🛈") ("🚮" . t) ("🏳") ("⏳"))
          ;;         ("⏳WAITING" ("⏳" . t))
          ;;         (todo ("🗁") ("🗲") ("🗒") ("🛈") ("🚮") ("🏳") ("⏳"))
          ;;         (done ("🗁") ("🗲") ("🗒") ("🛈") ("🚮") ("🏳") ("⏳"))
          ;;         ))
          (setq org-tag-alist
                '((:startgroup) ("🗁" . ?P) ("🗲" . ?A) ("🗒" . ?M) ("🛈" . ?R) ("🚮" . ?X) (:endgroup) ; 类型
                  (:startgroup) ("🏠" . ?h) ("🏢" . ?o) ("🛒" . ?m) ("🚌" . ?w) (:endgroup) ; 情境
                  (:startgroup) ("💻" . ?c) ("📱" . ?p) (:endgroup) ; 工具
                  (:startgroup) ("🏷" . ?b) ("🎔" . ?f) ("󳊙" . ?s) (:endgroup))) ; 性质

          (setq org-tags-exclude-from-inheritance
                '("🗁" "󳊙"))
          (setq org-fontify-done-headline t)
          ;; 代码着色
          (setq org-src-fontify-natively t)
          ;; Latex 输出
          (setq org-latex-create-formula-image-program 'imagemagick)
          ;; (setq org-latex-default-packages-alist
          ;;       '(("AUTO" "inputenc" t
          ;;          ("pdflatex"))
          ;;         ("T1" "fontenc" t
          ;;          ("pdflatex"))
          ;;         ("" "graphicx" t)
          ;;         ("" "grffile" t)
          ;;         ("" "longtable" nil)
          ;;         ("" "wrapfig" nil)
          ;;         ("" "rotating" nil)
          ;;         ("normalem" "ulem" t)
          ;;         ("" "amsmath" t)
          ;;         ("" "textcomp" t)
          ;;         ("" "amssymb" t)
          ;;         ("" "capt-of" nil)
          ;;         ("colorlinks" "hyperref" nil)
          ;;         ("" "fixltx2e" nil)
          ;;         ("" "float" nil)
          ;;         ("" "marvosym" t)
          ;;         ("" "wasysym" t)
          ;;         "\\tolerance=1000"))
          (setq org-latex-packages-alist
                '(("" "ctex" t) ;使用 CTEX 实现中文支持
                  ("" "minted" t))))
  :config (progn
            (org-babel-do-load-languages 'org-babel-load-languages
                                         '((emacs-lisp . t)
                                           (plantuml . t)
                                           (ditaa . t)
                                           (dot . t)
                                           (python . t)
                                           (scheme . t)
                                           (lilypond t)))

            ;; ob-core.el --- working with code blocks in org-mode
            (setq org-confirm-babel-evaluate nil)

            ;; ob-ditaa.el --- org-babel functions for ditaa evaluation
            (setq org-ditaa-jar-path
                  (if (file-exists-p "/usr/share/ditaa/ditaa.jar")
                      "/usr/share/ditaa/ditaa.jar"
                    (or (locate-file "ditaa.jar" exec-path)
                        (locate-file "ditaa" exec-path))))

            ;; ob-plantuml.el --- org-babel functions for plantuml evaluation
            (setq org-plantuml-jar-path
                  (if (file-exists-p "/usr/share/plantuml/plantuml.jar")
                      "/usr/share/plantuml/plantuml.jar"
                    (locate-file "plantuml.jar" exec-path)))

            ;; org-capture.el --- Fast note taking in Org-mode
            ;;  :bind ("C-c c" . org-capture)

            ;; org-indent.el --- Dynamic indentation for  Org-mode
            ;; :diminish org-indent-mode

            ;; ox.el
            (setq org-export-with-toc             nil
                  org-export-headline-levels      9
                  org-export-with-section-numbers 9
                  org-export-default-language     "zh-CN")

            ;; ox-latex.el
            (progn
              (setq org-latex-listings 'minted)
              (setq org-latex-minted-langs
                    '((emacs-lisp "common-lisp")
                      (cc "c++")
                      (shell-script "bash")))
              (setq org-latex-compiler "xelatex")
              (setq org-latex-pdf-proces
                    '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                      "bibtex %b"
                      "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                      "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
              (setq org-latex-classes
                    '(("article" "\\documentclass[11pt]{article}\n[NO-PACKAGES]"
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       ("\\paragraph{%s}" . "\\paragraph*{%s}")
                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                      ("report" "\\documentclass[11pt]{report}\n[NO-PACKAGES]"
                       ("\\part{%s}" . "\\part*{%s}")
                       ("\\chapter{%s}" . "\\chapter*{%s}")
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                      ("book" "\\documentclass[11pt]{book}\n[NO-PACKAGES]"
                       ("\\part{%s}" . "\\part*{%s}")
                       ("\\chapter{%s}" . "\\chapter*{%s}")
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                      ("ctexart" "\\documentclass{ctexart}\n[NO-PACKAGES]"
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       ("\\paragraph{%s}" . "\\paragraph*{%s}")
                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                      ("ctexrep" "\\documentclass{ctexrep}\n[NO-PACKAGES]"
                       ("\\part{%s}" . "\\part*{%s}")
                       ("\\chapter{%s}" . "\\chapter*{%s}")
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                      ("ctexbook" "\\documentclass{ctexbook}\n[NO-PACKAGES]"
                       ("\\part{%s}" . "\\part*{%s}")
                       ("\\chapter{%s}" . "\\chapter*{%s}")
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
              (setq org-latex-default-class "ctexart")
              (setq org-latex-image-default-width "")
              (setq org-latex-image-default-height "")
              (setq org-latex-image-default-option ""))))
;;; ---------------------------------------------------------------------------
;; #+END_SRC

;; **** progmodes
;; #+BEGIN_SRC emacs-lisp
;;; ---------------------------------------------------------------------------
;;; progmodes

;; cc-mode.el --- major mode for editing C and similar languages
(use-package cc-mode
  :init (progn
          ;; cc-vars.el --- user customization variables for CC Mode
          (require 'cc-vars)
          (setq-default c-basic-offset 4)
          (setq c-default-style '((awk-mode  . "awk")
                                  (c-mode    . "k&r")
                                  (c++-mode  . "stroustrup")
                                  (java-mode . "java")
                                  (other     . "gnu")))))

;; executable.el --- base functionality for executable interpreter scripts
(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; gdb-mi.el --- User Interface for running GDB
(use-package gdb-mi
  :init (progn
          (setq gdb-many-windows t)
          (setq gdb-show-main t)))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :diminish hs-minor-mode)

;; python.el --- Python's flying circus support for Emacs
(use-package python
  :init (progn
          (setq python-skeleton-autoinsert t)
          (when (executable-find "python3")
            (setq python-shell-interpreter "python3"))))

;; scheme.el --- Scheme (and DSSSL) editing mode
(use-package scheme
  :init (when (executable-find "chezscheme")
          (setq scheme-program-name "chezscheme")))

;; sql.el --- specialized comint.el for SQL interpreters
(use-package sql
  :init (setq sql-postgres-login-params '((server :default "localhost")
                                          (user :default "postgres")
                                          (database :default "postgres"))))

;; subword.el --- Handling capitalized subwords in a nomenclature
(use-package subword
  :diminish subword-mode
  :config (global-subword-mode))
;;; ---------------------------------------------------------------------------
;; #+END_SRC

;; **** textmodes
;; #+BEGIN_SRC emacs-lisp
;;; ---------------------------------------------------------------------------
;;; textmodes

;; ispell.el --- interface to International Ispell Versions 3.1 and 3.2
(use-package ispell
  :defines ispell-program-name
  :config (setq ispell-dictionary "english"))

;; flyspell.el --- On-the-fly spell checker
(use-package flyspell
  :after ispell
  :if (executable-find ispell-program-name)
  :diminish flyspell-mode
  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

;; remember --- a mode for quickly jotting down things to remember
(use-package remember
  :init (setq remember-data-file "~/life/remember.org")
  :config (add-to-list 'remember-handler-functions
                       #'remember-diary-extract-entries))
;;; ---------------------------------------------------------------------------
;; #+END_SRC
