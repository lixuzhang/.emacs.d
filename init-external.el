;;; init-external.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021  李旭章

;; Author: 李旭章 <17728928@qq.org>
;; Keywords: external


;; *** 外部包
;; #+BEGIN_SRC emacs-lisp
;; ===========================================================================
;; 第三方扩展
;; ===========================================================================
;; (org-babel-load-file
;;  (expand-file-name "init-packages.org" user-emacs-directory))
;; (load
;;  (expand-file-name "init-packages.el" user-emacs-directory))

(unless (package-installed-p 'jquery-doc)
  (package-install 'jquery-doc))

;; (unless (package-installed-p 'r5rs)
;;   (package-install 'r5rs))

(unless (package-installed-p 'sicp)
  (package-install 'sicp))

;;; ---------------------------------------------------------------------------
(use-package ac-html-angular)
(use-package ac-html-bootstrap)
(use-package ac-html-csswatcher)

;; ace-pinyin.el --- Jump to Chinese characters using ace-jump-mode or aby
(use-package ace-pinyin)

(use-package ace-window)

;; aggressive-fill-paragraph.el --- A mode to automatically keep paragraphs filled
(use-package aggressive-fill-paragraph
  :diminish aggressive-fill-paragraph-mode
  :config (afp-setup-recommended-hooks))

;; ;; aggressive-indent.el --- Minor mode to aggressively keep your code always indented
;; (use-package aggressive-indent
;;   :diminish aggressive-indent-mode
;;   :config (progn
;;             (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;;             (add-to-list 'aggressive-indent-dont-indent-if
;;                          '(and (derived-mode-p 'c-mode 'c++-mode 'objc-mode 'java-mode)
;;                                (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
;;                                                    (thing-at-point 'line)))))))

;; ;; anzu.el --- Show number of matches in mode-line while searching
;; (use-package anzu
;;   :diminish anzu-mode
;;   :bind (("M-%" . anzu-query-replace)
;;          ("C-M-%" . anzu-query-replace-regexp))
;;   :config (global-anzu-mode))

;; apache-mode.el --- major mode for editing Apache configuration files
(use-package apache-mode)

;; artbollocks-mode.el --- Improve your writing (especially about art)
(use-package artbollocks-mode
  :diminish artbollocks-mode
  ;; :hook (text-mode . artbollocks-mode)
  :init (setq artbollocks-weasel-words-regex
              (concat "\\b\\(many\\|various\\|very\\|fairly\\|several\\|extremely\\|exceedingly\\|quite\\|remarkably\\|few\\|surprisingly\\|mostly\\|largely\\|huge\\|tiny\\|\\(\\(are\\|is\\) a number\\)\\|excellent\\|interestingly\\|significantly\\|substantially\\|clearly\\|vast\\|relatively\\|completely\\)\\b"
                      (regexp-opt '("许多" "几乎" "很少" "差不多" "有点" "大量"
                                    "大致" "迟点" "迟些" "过几天" "大体上")
                                  t))))

(use-package auto-correct)

(use-package captain)

;; AUCTEX
(use-package tex-site
  :ensure auctex
  :init (progn
          (setq TeX-auto-save t)
          (setq TeX-parse-self t)
          (setq-default TeX-master t)))

;; cal-china-x.el --- Chinese localization, lunar/horoscope/zodiac info and more...
(use-package cal-china-x
  :config (progn
            (setq calendar-month-header
                  '(propertize (format "%d 年 %d 月" year month)
                               'font-lock-face 'calendar-month-header))
            (setq calendar-day-name-array
                  ["日" "一" "二" "三" "四" "五" "六"])
            (setq cal-china-x-important-holidays
                  cal-china-x-chinese-holidays)   ; 法定假日
            (setq holiday-oriental-holidays
                  '((holiday-lunar 1 1 "春节" 0)
                    (holiday-solar-term "清明" "清明")
                    (holiday-lunar 5 5 "端午" 0)
                    (holiday-lunar 7 7 "七夕")
                    (holiday-lunar 8 15 "中秋" 0)
                    (holiday-lunar 9 9 "重阳")
                    (holiday-solar-term  "冬至"  "冬至")
                    (holiday-lunar 12 30 "除夕")))
            (setq calendar-holidays
                  (append holiday-general-holidays
                          holiday-local-holidays
                          holiday-other-holidays
                          holiday-oriental-holidays))))

;; calfw.el --- Calendar view framework on Emacs
(use-package calfw
  :init (progn
          (setq cfw:event-format-location "\n  位置:    %s")
          ;; 重定义
          (defun calendar ()
            (interactive)
            (cfw:open-calendar-buffer
             :contents-sources
             (list
              (cfw:cal-create-source "Orange") ; diary source
              (cfw:org-create-source "Green"))))))  ; orgmode source


;; calfw-cal.el --- calendar view for emacs diary
(use-package calfw-cal
  :after calfw)

;; calfw-org.el --- calendar view for org-agenda
(use-package calfw-org
  :after (calfw org)
  :init (progn
          (setq cfw:org-agenda-schedule-args '(:timestamp))
          (setq cfw:org-overwrite-default-keybinding t)))

;; c-eldoc.el --- helpful description of the arguments to C functions
(use-package c-eldoc
  :hook ((c-mode-hook c++-mode-hook) . c-turn-on-eldoc-mode)
  :init (setq c-eldoc-cpp-command "cpp"))

;; chinese-wbim.el --- Enable Wubi(五笔) Input Method in Emacs.
(use-package chinese-wbim
  :init (progn
          (setq chinese-wbim-use-tooltip nil)
          (setq chinese-wbim-wb-use-gbk t))
  :config (progn
            (register-input-method
             "chinese-wbim" "Chinese" 'chinese-wbim-use-package
             "五笔" "汉字五笔输入法" "wb.txt")
            (require 'chinese-wbim-extra)
            (global-set-key ";" 'chinese-wbim-insert-ascii)))

;; chm-view.el --- View CHM file.
;(use-package chm-view)

;; clean-aindent-mode.el --- Simple indent and unindent, trims indent white-space
(use-package clean-aindent-mode
  :config (clean-aindent-mode 1))

;; clean-buffers.el --- clean useless buffers
(use-package clean-buffers
  :config (progn
            (dolist (bn '("*CEDET Global*" "*Messages*" "*Help*" "*helm M-x*"))
              (add-to-list 'clean-buffers-useless-buffer-names bn))
            (clean-buffers-turn-on-auto-clean-buffers)))

;; comment-dwim-2.el --- An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; commify.el --- Toggle grouping commas in numbers
(use-package commify)

;; company.el --- Modular text completion framework
(use-package company
  :diminish company-mode
  :bind ("C-c /" . company-files)
  :hook (after-init . global-company-mode)
  :init (progn
          (setq company-tooltip-limit 20)                      ; bigger popup window
          (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
          (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
          (setq company-begin-commands '(self-insert-command)))) ; start autocompletion only after typing

(use-package company-box
  :after company
  :diminish company-box-mode)

;; company-c-headers.el --- Company mode backend for C/C++ header files
(use-package company-c-headers
  :after (company semantic)
  :commands company-c-headers
  :config (progn
            (add-hook 'c-mode-hook
                      (lambda ()
                        (setq company-c-headers-path-system
                              (semantic-gcc-get-include-paths "c"))))
            (add-hook 'c++-mode-hook
                      (lambda ()
                        (setq company-c-headers-path-system
                              (semantic-gcc-get-include-paths "c++")))))
  :config (add-to-list 'company-backends #'company-c-headers))

;; company-flx.el --- flx based fuzzy matching for company
(use-package company-flx
  :after company
  :config (company-flx-mode +1))

(use-package company-math
  :after company)

;; (use-package company-lsp
;;   :after (company lsp-mode)
;;   :commands company-lsp
;;   :config (add-to-list 'company-backends #'company-lsp))

;; company-php.el --- company completion source for php
(use-package company-php
  :after (company php-mode)
  :commands company-ac-php-backend
  :config (add-hook 'php-mode-hook
                    (lambda ()
                      (add-to-list 'company-backends #'company-ac-php-backend))))

(use-package company-quickhelp
  :after company)

;; company-shell.el --- Company mode backend for shell functions
(use-package company-shell
  :after company)

(use-package company-statistics
  :after company
  :hook (after-init . company-statistics-mode))

(use-package company-web
  :after (company web-mode)
  :config (progn
            (require 'company-web-html)
            (require 'company-web-jade)
            (require 'company-web-slim)
            (define-key web-mode-map (kbd "C-'") 'company-web-html)))

(use-package composer)

;; coverlay.el --- Test coverage overlays
(use-package coverlay)

;; css-eldoc.el --- an eldoc-mode plugin for CSS source code
(use-package css-eldoc
  :config (css-eldoc-enable))

;; cursor-chg.el --- Change cursor dynamically, depending on the context.
;; (use-package cursor-chg
;;   :config (progn
;;             (change-cursor-mode 1)
;;             (curchg-toggle-cursor-type-when-idle 1)))

(use-package dap-mode)
;; (use-package dap-LANGUAGE)

;; deft.el --- quickly browse, filter, and edit plain text notes
(use-package deft
  :after org
  :init (progn
          (setq deft-extension "org")
          (setq deft-text-mode 'org-mode)
          (setq deft-directory "~/notes")
          (setq deft-recursive t)
          (setq deft-use-filename-as-title t)))

;; disaster.el --- Disassemble C/C++ code under cursor in Emacs
;; 显示当前文件的汇编代码
(use-package disaster)

(use-package drag-stuff
  :diminish drag-stuff-mode
  :config (drag-stuff-mode 1))

;; drupal-mode.el --- Advanced minor mode for Drupal development
(use-package drupal-mode
  :after php-mode)

;; dtrt-indent.el --- Adapt to foreign indentation offsets
(use-package dtrt-indent
  :init (setq dtrt-indent-verbosity 1) ; Silent:0  Normal:1  Verbose:2  Diagnostics:3
  :config (dtrt-indent-mode 1))

(use-package duplicate-thing
  :bind ("M-c" . duplicate-thing))

(use-package dynamic-ruler)

;; ecb.el --- a code browser for Emacs
;; (use-package ecb
;;   :commands ecb-activate
;;   :init (progn
;;           (setq ecb-version-check nil)
;;           ;; (setq ecb-auto-activate t)
;;           (setq ecb-compile-window-height 8)
;;           (setq ecb-compile-window-temporally-enlarge 'both)
;;           (setq ecb-compile-window-width 'edit-window)
;;           (setq ecb-fix-window-size 'auto)
;;           (setq ecb-layout-name "left3")
;;           (setq ecb-windows-height 0.25)
;;           (setq ecb-windows-width 0.25)
;;           (setq ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
;;           (setq ecb-select-edit-window-on-redraw t)
;;           (setq ecb-source-path (quote (("~" "HOME") ("/" "/"))))
;;           (setq ecb-eshell-auto-activate t)
;;           (setq ecb-tip-of-the-day nil)
;;           (setq ecb-tip-of-the-day-file
;;                 (expand-file-name "ecb/tip-of-day.el" user-emacs-directory))
;;           (setq ecb-user-layouts
;;                 (expand-file-name "ecb/user-layouts.el" user-emacs-directory))
;;           (setq ecb-redraw-layout-quickly t)))

;; ede-php-autoload.el --- Simple EDE PHP Project
(use-package ede-php-autoload
  :after (ede php-mode)
  :hook (php-mode . ede-php-autoload-mode))

;; ede-php-autoload-composer-installers.el --- Composer installers support for ede-php-autoload
(use-package ede-php-autoload-composer-installers
  :after ede-php-autoload)

;; ede-php-autoload-drupal.el --- Drupal support for ede-php-autoload
(use-package ede-php-autoload-drupal
  :after ede-php-autoload)

;; electric-case.el --- insert camelCase, snake_case words without "Shift"ing
(use-package electric-case
  :hook ((ahk-mode . electric-case-ahk-init)
         (c-mode . electric-case-c-init)
         (java-mode . electric-case-java-init)
         (scala-mode . electric-case-scala-init)))

;; electric-operator.el --- Automatically add spaces around operators
;; 发展过程：smart-operator => electric-spacing => electric-operator
(use-package electric-operator)

(use-package elpy
  :init (elpy-enable))

;; Zen Coding => Emmet
(use-package emmet-mode
  :diminish emmet-mode
  :hook ((sgml-mode css-mode web-mode) . emmet-mode)
  :init (progn
          (setq emmet-move-cursor-between-quotes t)
          (setq emmet-expand-jsx-className? t)))

(use-package emms
  :init (progn
          (setq emms-player-mpg321-command-name "mpg123")
          (setq emms-player-list                  ; 只使用 mplayer 播放
                (list 'emms-player-mplayer-playlist 'emms-player-mplayer)))
  :config (emms-standard))

(use-package eshell-did-you-mean
  :after eshell)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package fancy-narrow
  :diminish fancy-narrow-mode
  :config (fancy-narrow-mode))

;; fic-mode.el --- Show FIXME/TODO/BUG/KLUDGE in special face only in comments and strings
(use-package fic-mode
  :diminish fic-mode
  :hook (prog-mode . fic-mode))

(use-package figlet)

(use-package fliptext)

;; flx-ido.el --- flx integration for ido
(use-package flx-ido
  :after ido
  :config (flx-ido-mode 1))

(use-package flycheck
  :after semantic
  :init (setq flycheck-gcc-include-path (semantic-gcc-get-include-paths "c++")))

(use-package flycheck-color-mode-line
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-package
  :after (flycheck package-lint)
  :config (flycheck-package-setup))

(use-package flycheck-pkg-config
  :after flycheck)

(use-package flycheck-vala
  :after (flycheck vala-mode))

(use-package fold-dwim-org)

(use-package geiser
  :after scheme
  :init (progn
          (setq geiser-default-implementation 'chez)
          (setq geiser-active-implementations '(chez guile))))

(use-package ggtags
  :commands ggtags-mode
  :init (progn
          (setq gtags-suggested-key-mapping t))
  :config (add-hook 'c-mode-common-hook
                    (lambda ()
                      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                        (ggtags-mode 1)))))

(use-package gitattributes-mode
  :after magit)

(use-package gitconfig-mode
  :after magit)

(use-package git-gutter-fringe
  :after magit
  :diminish git-gutter-mode
  ;; :config (global-git-gutter-mode 1)
  )

(use-package gitignore-mode
  :after magit)

;; git-messenger.el --- Pop up last commit information of current line
(use-package git-messenger
  :after magit
  :commands magit-commit-mode
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:use-magit-popup t))

(use-package git-timemachine
  :after magit)

;; (use-package golden-ratio
;;   :diminish golden-ratio-mode
;;   :init (progn
;;           (setq golden-ratio-auto-scale t)
;;           (setq golden-ratio-exclude-modes
;;                 '(bs-mode
;;                   calc-mode
;;                   compilation-mode
;;                   ediff-mode
;;                   fundamental-mode
;;                   dired-mode
;;                   gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode
;;                   gdb-locals-mode gdb-inferior-io-mode gdb-memory-mode
;;                   gdb-threads-mode gdb-registers-mode gud-mode
;;                   restclient-mode
;;                   speedbar-mode)))
;;   :config (progn
;;             ;; 弹出 helm 窗口时不处理
;;             (add-to-list 'golden-ratio-inhibit-functions
;;                          (lambda ()
;;                            (if (boundp 'helm-alive-p)
;;                                (symbol-value 'helm-alive-p))))
;;             ;; ECB 激活时不处理
;;             ;; (add-to-list 'golden-ratio-inhibit-functions
;;             ;;              (lambda ()
;;             ;;                (if (boundp 'ecb-minor-mode)
;;             ;;                    (symbol-value 'ecb-minor-mode))))
;;             (golden-ratio-mode 1)))

(use-package google-c-style
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

(use-package graphviz-dot-mode
  :init (progn
          (unless (getenv "GRAPHVIZ_DOT")
            (setenv "GRAPHVIZ_DOT" (executable-find "dot")))
          (setq graphviz-dot-indent-width tab-width)))

(use-package guess-language)

;; helm.el --- Emacs incremental and narrowing framework
(use-package helm
  :after ido
  :diminish helm-mode
  :bind ("M-x" . helm-M-x)
  :init (progn
          (setq helm-scroll-amount                    8
                helm-split-window-in-side-p           t
                helm-move-to-line-cycle-in-source     t
                helm-ff-search-library-in-sexp        t
                helm-ff-file-name-history-use-recentf t
                helm-apropos-function-list            t
                helm-M-x-fuzzy-match                  t
                helm-buffers-fuzzy-matching           t
                helm-recentf-fuzzy-match              t
                helm-semantic-fuzzy-match             t
                helm-imenu-fuzzy-match                t
                helm-locate-fuzzy-match               t
                helm-lisp-fuzzy-completion            t)
          (when (executable-find "curl")
            (setq helm-google-suggest-use-curl-p t)))
  :config (progn
            ;; Ido-everywhere is incompatible with Helm-mode
            (ido-everywhere -1)
            (require 'helm-config)
            (require 'helm-grep)
            (helm-mode 1)
            (helm-autoresize-mode 1)
            (add-hook 'helm-minibuffer-setup-hook
                      (lambda () (abbrev-mode -1)))))

(use-package helm-backup
  :after helm)

(use-package helm-company
  :after (helm company))

;; helm-descbinds.el --- Yet Another `describe-bindings' with `helm'.
(use-package helm-descbinds
  :after helm)

;; helm-emmet.el --- helm sources for emmet-mode's snippets
(use-package helm-emmet
  :after (helm emmet-mode))

;; helm-gtags.el --- GNU GLOBAL helm interface
(use-package helm-gtags
  :after (helm ggtags)
  :bind (("C-c g a" . helm-gtags-tags-in-this-function)
         ("C-j"     . helm-gtags-select)
         ("M-."     . helm-gtags-dwim)
         ("M-,"     . helm-gtags-pop-stack)
         ("C-c <"   . helm-gtags-previous-history)
         ("C-c >"   . helm-gtags-next-history))
  :hook ((dired-mode eshell-mode c++-mode java-mode asm-mode) . helm-gtags-mode)
  :init (progn
          (setq helm-gtags-ignore-case t
                helm-gtags-auto-update t
                helm-gtags-use-input-at-cursor t
                helm-gtags-pulse-at-cursor t
                helm-gtags-prefix-key "\C-cg"
                helm-gtags-suggested-key-mapping t)))

;; helm-make.el --- Select a Makefile target with helm
(use-package helm-make
  :after (helm projectile))

;; helm-mode-manager.el --- Select and toggle major and minor modes with helm
(use-package helm-mode-manager
  :after helm)       ; 与 manage-minor-mode 类似?

(use-package helm-lsp
  :after (helm lsp-mode)
  :commands helm-lsp-workspace-symbol)

;; helm-projectile.el --- Helm integration for Projectile
(use-package helm-projectile
  :after (helm projectile)
  :init (progn
          (setq projectile-completion-system 'helm)
          (setq projectile-switch-project-action 'helm-projectile-find-file)
          (setq projectile-switch-project-action 'helm-projectile))
  :config (progn
            (add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
            (add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html
            (helm-projectile-on)))

(use-package helm-smex
  :after (helm smex))

;; helm-swoop.el --- Efficiently hopping squeezed lines powered by helm interface
(use-package helm-swoop
  :after helm)

;; hide-comnt.el --- Hide/show comments in code.
(use-package hide-comnt
  :commands hide/show-comments)

;; hideif.el --- hides selected code within ifdef
(use-package hideif
  :init (setq hide-ifdef-initially t))

;; hideshow-org.el --- Provides org-mode like hide and show for hideshow.el
(use-package hideshow-org
  :hook (prog-mode . hs-org/minor-mode))

;; hideshowvis.el --- Add markers to the fringe for regions foldable by hideshow.el
(use-package hideshowvis
  :hook (prog-mode . hideshowvis-enable)
  :config (hideshowvis-symbols))

;; hungry-delete.el --- hungry delete minor mode
(use-package hungry-delete
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

;; hydra.el --- Make bindings that stick around.
(use-package hydra)

;; ido-completing-read+.el --- A completing-read-function using ido
(use-package ido-completing-read+
  :after ido
  :config (ido-ubiquitous-mode 1))

;; idomenu.el --- imenu tag selection a la ido
(use-package idomenu
  :after ido)

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package import-js)

;; (use-package indium)

;; (use-package interleave)

;; (use-package jdee
;;   :after ecb)

;; inferior-js-mode
(use-package js-comint
  :config (if (executable-find "node")
              (setq inferior-js-program-command "node --interactive")
            (setq inferior-js-program-command "js")))

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :hook (js-mode . js2-minor-mode))

(use-package json-mode)

;; (use-package jsx-mode
;;   :mode "\\.jsx\\'")


;; kanban.el --- Parse org-todo headlines to use org-tables as Kanban tables
(use-package kanban)

;; keyfreq.el --- track command frequencies
(use-package keyfreq
  :config (progn
            (keyfreq-mode 1)
            (keyfreq-autosave-mode 1)))

;; lentic-mode.el --- minor mode for lentic buffers
(use-package lentic
  :diminish lentic-mode
  :config (global-lentic-mode))

(use-package lsp-mode
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

;; magit.el --- A Git porcelain inside Emacs
(use-package magit
  :ensure-system-package git
  :init (progn
          (setq magit-auto-revert-mode nil)
          (setq magit-diff-paint-whitespace 'status)
          ;; https://github.com/magit/magit/issues/1839
          (setq magit-last-seen-setup-instructions "1.4.0")))

;; manage-minor-mode.el --- Manage your minor-modes easily
(use-package manage-minor-mode)

(use-package mmm-mode
  :init (progn
          (setq mmm-submode-decoration-level 2)
          (setq mmm-global-mode 'buffers-with-submode-classes)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; mwim.el --- Switch between the beginning/end of line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; nameframe.el --- Manage frames by name.
(use-package nameframe
  :bind (("M-P" . nameframe-switch-frame)))

;; ;; nameframe-perspective.el --- Nameframe integration with perspective.el
;; (use-package nameframe-perspective
;;   :after nameframe
;;   :config (nameframe-perspective-mode t))

;; nameframe-projectile.el --- Nameframe integration with Projectile
(use-package nameframe-projectile
  :after (nameframe projectile)
  :config (nameframe-projectile-mode t))

(use-package never-comment)

(use-package ob-browser
  :after org)

(use-package ob-http
  :after org)

(use-package org-brain
  :after org)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :init (setq org-bullets-bullet-list '("🄌" "➊" "➋" "➌" "➍" "➎" "➏" "➐" "➑" "➒" "➓")))
;; "◉" "☯" "⁂" "❖" "✿" "🔯" "🌼" 🌸🏵🏶❀❁✾❅❉❂
;; 🄋 ➀ ➁ ➂ ➃ ➄ ➅ ➆ ➇ ➈ ➉
;; 🄌 ➊ ➋ ➌ ➍ ➎ ➏ ➐ ➑ ➒ ➓

(use-package org-doing
  :after org
  :init (setq org-doing-file "~/life/doing.org"))

;; (use-package org-edna
;;   :after org
;;   :config (org-edna-load))

(use-package org-mind-map
  :ensure-system-package (gvgen . graphviz)
  :init
  (require 'ox-org)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

(use-package org-pomodoro
  :after org)

;; org-present.el --- Minimalist presentation minor-mode for Emacs org-mode.
(use-package org-present
  :after org
  :commands org-present
  :config (progn
            (add-hook 'org-present-mode-hook
                      (lambda ()
                        (org-present-big)
                        (org-display-inline-images)
                        (org-present-hide-cursor)
                        (org-present-read-only)))
            (add-hook 'org-present-mode-quit-hook
                      (lambda ()
                        (org-present-small)
                        (org-remove-inline-images)
                        (org-present-show-cursor)
                        (org-present-read-write)))))

;; org-eww => org-preview-html
(use-package org-preview-html
  :after (org eww))

(use-package org-projectile
  :after (org projectile)
  :bind (("C-c n p" . org-projectile:project-todo-completing-read))
  :init (progn
          (setq org-confirm-elisp-link-function nil)
          (setq org-projectile:per-repo-filename "TODO.org"))
  :config (progn
            (add-to-list 'org-agenda-files
                         (org-projectile:todo-files))
            (add-to-list 'org-capture-templates
                         (org-projectile:project-todo-entry "p"))
            (org-projectile:per-repo)))

;; org-repo-todo.el --- Simple repository todo management with org-mode
(use-package org-repo-todo
  :after org
  :bind (("C-;" . ort/capture-todo)
         ("C-'" . ort/capture-checkitem)
         ("C-`" . ort/goto-todos))
  :init (setq ort/prefix-arg-directory "~/life/"))

(use-package org-time-budgets
  :after org)

(use-package ox-bibtex-chinese
  :after org)

;; pangu-spacing.el --- Minor-mode to add space between Chinese and English characters.
(use-package pangu-spacing
  :diminish pangu-spacing-mode
  :hook (text-mode . pangu-spacing-mode)
  :init (setq pangu-spacing-real-insert-separtor t))

;; (use-package parinfer
;;   :after lispy
;;   :diminish parinfer-mode
;;   :hook ((lisp-mode scheme-mode emacs-lisp-mode common-lisp-mode clojure-mode) . parinfer-mode)
;;   :init (setq parinfer-extensions
;;               '(defaults         ; should be included.
;;                  pretty-parens   ; different paren styles for different modes.
;;                  ;; evil            ; If you use Evil.
;;                  lispy           ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;;                  ;; paredit         ; Introduce some paredit commands.
;;                  smart-tab       ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;                  smart-yank)))   ; Yank behavior depend on mode.

(use-package persp-mode
  :commands persp-mode
  :diminish persp-mode
  :init (progn
          (setq wg-morph-on nil) ;; switch off animation
          (setq persp-autokill-buffer-on-remove 'kill-weak))
  :config (add-hook 'after-init-hook (lambda () (persp-mode 1))))

(use-package persp-mode-projectile-bridge
  :after (persp-mode projectile)
  :commands persp-mode-projectile-bridge-mode
  :config (progn
            (add-hook 'after-init-hook
                      (lambda ()
                        (persp-mode-projectile-bridge-mode 1)))
            (add-hook 'persp-mode-projectile-bridge-mode-hook
                      (lambda ()
                        (if persp-mode-projectile-bridge-mode
                            (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                          (persp-mode-projectile-bridge-kill-perspectives))))))

;; php-mode.el --- Major mode for editing PHP code
(use-package php-mode
  :mode ("\\.php\\'" "\\.phtml\\'")
  :init (progn
          (setq php-executable (executable-find "php"))
          (setq php-template-compatibility nil)))

(use-package php-boris
  :after php-mode)
(use-package php-boris-minor-mode
  :after (php-mode php-boris)
  :hook (php-mode . php-boris-minor-mode))
(use-package php-eldoc
  :after php-mode)
;; (use-package php-extras
;;   :after php-mode)
(use-package php-refactor-mode
  :after php-mode)
(use-package phpunit
  :after php-mode)

(use-package pinyin-search)

(use-package pkgbuild-mode)

(use-package private-diary
  :init (setq private-diary-file "~/life/private.gpg"))

(use-package programmer-dvorak)

(use-package projectile
  :diminish projectile-mode
  :init (progn
          (setq projectile-enable-caching t)
          (setq projectile-indexing-method 'alien))
  :config (progn
            (add-to-list 'projectile-globally-ignored-directories "backup")
            (projectile-global-mode)))

(use-package projectile-speedbar
  :after (projectile sr-speedbar))

(use-package plantuml-mode
  :after graphviz-dot-mode            ; 需要 GRAPHVIZ_DOT 环境变量
  :mode "\\.plu\\'"
  :init (progn
          (setq plantuml-default-exec-mode 'executable)
          (setq plantuml-jar-path org-plantuml-jar-path)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; rainbow-mode.el --- Colorize color names in buffers
(use-package rainbow-mode)

(use-package region-bindings-mode
  :after multiple-cursors
  :diminish region-bindings-mode
  :init (progn
          (setq region-bindings-mode-disabled-modes nil) ; 黑名单
          (setq region-bindings-mode-enabled-modes nil) ; 白名单
          (setq region-bindings-mode-disable-predicates
                ((lambda () buffer-read-only))))
  :config (progn (region-bindings-mode-enable)
                 (define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
                 (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
                 (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
                 (define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)))

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe))

(use-package skewer-mode
  :diminish skewer-html-mode
  :config (skewer-setup))

(use-package smart-mode-line
  :config (progn
            (setq sml/no-confirm-load-theme t)
            ;; (setq sml/theme 'light)
            (sml/setup)))

;; smart-tabs-mode.el --- Intelligently indent with tabs, align with spaces!
;; Smart tabs are only used when indent-tabs-mode is non-nil
(use-package smart-tabs-mode
  :config (progn
            ;; (add-to-list 'smart-tab-disabled-major-modes 'shell-mode) ;; 没此变量
            (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)))

;; smartscan.el --- Jumps between other symbols found at point
(use-package smartscan
  :config (global-smartscan-mode 1))

(use-package smex
  ;; :bind (("M-x" . smex)
  ;;        ("M-X" . smex-major-mode-commands)
  ;;        ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

;; smooth-scrolling.el --- Make emacs scroll smoothly
(use-package smooth-scrolling
  :config (setq smooth-scroll-margin 4))

(use-package sotlisp)

;; (use-package sr-speedbar
;;   :after (helm ecb))

;; srefactor.el --- A refactoring tool based on Semantic parser framework
(use-package srefactor
  :config (progn
            (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
            (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)))

;; stickyfunc-enhance.el --- An enhancement to stock `semantic-stickyfunc-mode'
(use-package stickyfunc-enhance)

;; super-save.el --- Auto-save buffers, based on your activity.
(use-package super-save
  :config (super-save-mode +1))

(use-package tagedit
  :diminish tagedit-mode
  :hook (html-mode . tagedit-mode)
  :config (tagedit-add-paredit-like-keybindings))

;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :config (global-undo-tree-mode))

(use-package unicode-whitespace)

(use-package vala-mode)

(use-package vigenere)

(use-package voca-builder
  :init (setq voca-builder/voca-file "~/life/vocabulary.org"
              voca-builder/current-tag "default"))

;; volatile-highlights.el --- Minor mode for visual feedback on some operations.
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode 1))

;; 替代 workgroups2
(use-package wconf
  :after desktop
  :commands wconf-load
  :bind (("C-c w s" . wconf-store)
         ("C-c w S" . wconf-store-all)
         ("C-c w R" . wconf-restore-all)
         ("C-c w r" . wconf-restore)
         ("C-c w w" . wconf-switch-to-config)
         ("C-<prior>" . wconf-use-previous)
         ("C-<next>" . wconf-use-next))
  :config (progn (add-hook 'desktop-after-read-hook ; so we have all buffers again
                           (lambda ()
                             (wconf-load)
                             (wconf-switch-to-config 0)))
                 (add-hook 'kill-emacs-hook
                           (lambda ()
                             (wconf-store-all)
                             (wconf-save)))))

(use-package web-mode
  :mode ("\\.html?\\'" "\\.tpl\\'" "\\.tpla\\'" "\\.[agj]sp\\'"
         "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
  :init (progn
          (setq web-mode-enable-auto-pairing              t)
          (setq web-mode-enable-block-face                t)
          (setq web-mode-enable-comment-keywords          t)
          (setq web-mode-enable-css-colorization          t)
          (setq web-mode-enable-current-element-highlight t)
          ;; (setq web-mode-enable-current-column-highlight  t)
          (setq web-mode-enable-heredoc-fontification     t)
          (setq web-mode-enable-part-face                 t)
          (setq web-mode-engines-alist
                '(("php"    . "\\.phtml\\'")
                  ("blade"  . "\\.blade\\."))))
  :config (set-face-background 'web-mode-current-element-highlight-face "lavender"))

;; which-key.el --- Display available keybindings in popup
;; which-key started as a rewrite of guide-key, but the feature sets have since diverged.
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

;; whitespace-cleanup-mode.el --- Intelligently call whitespace-cleanup on save
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

(use-package with-editor
  :commands with-editor-export-editor
  :hook ((shell-mode term-mode eshell-mode) . with-editor-export-editor)
  :config (shell-command-with-editor-mode 1))

;; ;; ws-butler.el --- Unobtrusively remove trailing whitespace.
;; (use-package ws-butler
;;   :diminish ws-butler-mode
;;   :hook (prog-mode . ws-butler-mode))
;; #+END_SRC


;; ---------------------------------------------------------------------------
;; 手工安装的扩展。
;; ---------------------------------------------------------------------------
;; #+BEGIN_SRC emacs-lisp
;; (use-package epubmode
;;   :load-path "site-lisp")
;; (use-package hanconvert
;;   :load-path "site-lisp")
;; (use-package sdcv-mode
;;   :load-path "site-lisp"
;;   :bind ("C-c d" . sdcv-search))
;; (use-package unicad
;;   :load-path "site-lisp")

;; ;; lilypond-mode.el -- Major mode for editing GNU LilyPond music scores
;; (use-package lilypond-mode
;;   :load-path "site-lisp"
;;   :demand
;;   :mode ("\\.ly$" "\\.ily$")
;;   :init (setq lyqi-default-language 'italiano)
;;   :config (load "lilypond-quick-insert-mode"))


;; (defun top-level-add-to-load-path (topdir)
;;   (let ((default-directory topdir))
;;     (add-to-list 'load-path default-directory)
;;     (normal-top-level-add-subdirs-to-load-path)))
;; (top-level-add-to-load-path
;;  (expand-file-name "site-lisp/" user-emacs-directory))

;; #+END_SRC



;; ---------------------------------------------------------------------------
;; 放弃的扩展
;; ---------------------------------------------------------------------------
;; #+BEGIN_SRC emacs-lisp
;; 已经不维护
;; (use-package puml-mode
;;   :mode "\\.plu\\'"
;;   :after graphviz-dot-mode            ; 需要 GRAPHVIZ_DOT 环境变量
;;   :init (progn
;;           (defalias 'plantuml-mode 'puml-mode) ; 与 org-plantuml 兼容
;;           ;; 加载前必须先定义 puml-plantuml-jar-path，如果找不到则出错。
;;           (setq puml-plantuml-jar-path
;;                 (substring (locate-file "plantuml.jar" exec-path) 2)))
;;   :config (puml-set-output-type "png"))

;; (use-package auto-package-update
;;   :config
;;    (setq auto-package-update-delete-old-versions t)
;;    (setq auto-package-update-hide-results t)
;;    (auto-package-update-maybe))

;; (require 'el-get)
;; (el-get 'sync)

;; (use-package esup)

;; (use-package easy-lentic
;;   :after lentic
;;   :config (easy-lentic-mode-setup))

;; (use-package ergoemacs-mode
;;   :init (progn
;;           (setq ergoemacs-theme nil)
;;           (setq ergoemacs-keyboard-layout "programmer-dv"))
;;   :config (ergoemacs-mode 1))

;; (use-package guile-scheme
;;   :after scheme)

;; (use-package outlined-elisp-mode
;;   :config (add-hook 'emacs-lisp-mode-hook #'outlined-elisp-find-file-hook))

;; 启用 rainbow-delimiters 后无效果，并可替代
;; (use-package paren-face
;;   :config (progn
;;             (set-face-foreground 'parenthesis "light gray")
;;             (global-paren-face-mode 1)))

;; 与 rainbow-delimiters 有冲突？
;; (use-package htmlize
;;   :defer t)

;; ;; rainbow-blocks.el --- Block syntax highlighting for lisp code
;; (use-package rainbow-blocks)            ; rainbow-delimiters 修改版，对 blocks 进行着色

;; 与 rainbow-blocks 类似，支持 elisp 和 js
;; (use-package context-coloring
;;   :after js2-mode
;;   :init (progn
;;           (setq context-coloring-syntactic-comments nil)
;;           (setq context-coloring-syntactic-strings nil))
;;   :config (progn
;;             (add-hook 'eval-expression-minibuffer-setup-hook #'context-coloring-mode)
;;             ;; JavaScript:
;;             (add-hook 'js2-mode-hook #'context-coloring-mode)
;;             ;; Emacs Lisp:
;;             (add-hook 'emacs-lisp-mode-hook #'context-coloring-mode)))


;; 没什么用处，看光标或高亮当前行就可以了。
;; (use-package fringe-current-line
;;   :config (global-fringe-current-line-mode))
;; 效果差
;; (use-package highlight-indent-guides
;;   :config (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))
;; (use-package indent-guide)
;; (use-package highlight-indentation)
;; (use-package page-break-lines
;;   :config (global-page-break-lines-mode))
;; (use-package fill-column-indicator
;;   :config (fci-mode 1))

;; (use-package company-auctex
;;   :after (company tex-site))

;; 没什么用，不安装。
;; (use-package fancy-battery
;;   :disabled t
;;   :after battery
;;   :config (progn
;;             (remove-hook 'after-init-hook #'display-battery-mode)
;;             (add-hook 'after-init-hook #'fancy-battery-mode)))

;; (use-package-force load-dir
;;   :init
;;   (setq load-dir-ignore-errors t
;;         load-dir-recursive     t
;;         load-dirs              t))

;; (use-package outorg)
;; (use-package outshine)
;; (use-package navi-mode)

;; (use-package minimap)

;; (use-package paredit
;;   :commands enable-paredit-mode
;;   :diminish paredit-mode
;;   :config (progn
;;             (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;;             (add-hook 'scheme-mode-hook #'enable-paredit-mode)
;;             (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
;;             (add-hook 'clojure-mode-hook #'enable-paredit-mode)))

;; (use-package paredit-everywhere
;;   :commands paredit-everywhere-mode
;;   :diminish paredit-everywhere-mode
;;   :config (progn
;;             (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
;;             (add-hook 'css-mode-hook #'paredit-everywhere-mode)))

;; (use-package paxedit
;;   :commands paxedit-mode
;;   :diminish paxedit-mode
;;   :bind (("M-<right>" . paxedit-transpose-forward)
;;          ("M-<left>" . paxedit-transpose-backward)
;;          ("M-<up>" . paxedit-backward-up)
;;          ("M-<down>" . paxedit-backward-end)
;;          ("M-b" . paxedit-previous-symbol)
;;          ("M-f" . paxedit-next-symbol)
;;          ("C-%" . paxedit-copy)
;;          ("C-&" . paxedit-kill)
;;          ("C-*" . paxedit-delete)
;;          ("C-^" . paxedit-sexp-raise)
;;          ("M-u" . paxedit-symbol-change-case)
;;          ("C-@" . paxedit-symbol-copy)
;;          ("C-#" . paxedit-symbol-kill))
;;   :config (progn
;;             (add-hook 'lisp-mode-hook #'paxedit-mode)
;;             (add-hook 'scheme-mode-hook #'paxedit-mode)
;;             (add-hook 'emacs-lisp-mode-hook #'paxedit-mode)
;;             (add-hook 'clojure-mode-hook #'paxedit-mode)))

;; (use-package rw-language-and-country-codes)
;; (use-package rw-ispell
;;   :config (rw-ispell-set-up-pdicts))
;; (use-package rw-hunspell
;;   :config (rw-hunspell-setup))

;; session.el --- use variables, registers and buffer places across sessions
;; (use-package session
;;   :disabled t
;;   :config (progn
;;             (add-hook 'after-init-hook #'session-initialize)
;;             (add-to-list 'session-globals-exclude 'org-mark-ring)))

;; (use-package smartparens
;;   :diminish smartparens-mode
;;   :after smartparens-config
;;   :init
;;   (setq sp-base-key-bindings 'paredit)
;;   (setq sp-autoskip-closing-pair 'always)
;;   (setq sp-hybrid-kill-entire-symbol nil)
;;   :config
;;   (smartparens-global-mode 1)
;;   (show-smartparens-global-mode 1)
;;   (sp-use-smartparens-bindings))

;; (use-package slime)
;; (use-package slime-js)

;; (use-package pomodoro)

;; (use-package pointback
;;   :config (global-pointback-mode))

;; (use-package org-annotate-file)

;; (use-package windmove
;;   :config (progn
;;             ;; Make windmove work in org-mode
;;             (add-hook 'org-shiftup-final-hook #'windmove-up)
;;             (add-hook 'org-shiftleft-final-hook #'windmove-left)
;;             (add-hook 'org-shiftdown-final-hook #'windmove-down)
;;             (add-hook 'org-shiftright-final-hook #'windmove-right)))

;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :config
;;   (yas-global-mode 1)
;;   (use-package java-snippets)
;;   (use-package php-auto-yasnippets)
;;   (use-package vala-snippets))

;; (use-package org-chinese-utils
;;   :after org
;;   :config (org-chinese-utils-enable))
;; (use-package ox-latex-chinese
;;   :after org)
;; (use-package ob-php
;;   :after (org php-mode))
;; (use-package speed-type)

;; (use-package persp-projectile
;;   :after (perspective projectile))

;; (use-package perspective
;;   :after desktop
;;   :commands persp-mode
;;   :config (add-hook 'desktop-after-read-hook (lambda () (persp-mode 1))))


;; ;; cnfonts.el --- A simple Chinese fonts config tool
;; (use-package cnfonts
;;   :init (progn
;;           (setq cnfonts-use-face-font-rescale nil)
;;           (setq cnfonts-profiles '("general" "program" "other"))
;;           (setq cnfonts--current-profile "general")
;;           (setq cnfonts--profiles-steps '(("general" . 4))))
;;   :config (progn
;;             (cnfonts-enable)
;;             (cnfonts-set-spacemacs-fallback-fonts)))

;; (use-package unicode-fonts
;;   :init (progn
;;           (setq unicode-fonts-fontset-names '("fontset-default"))
;;           (setq unicode-fonts-skip-font-groups
;;                 '(buggy-before-vista decorative low-quality-glyphs multicolor non-free)))
;;   :config (unicode-fonts-setup))

;; (use-package mode-icons
;;   :config (mode-icons-mode))

;; (use-package all-the-icons)
;; (use-package all-the-icons-dired
;;   :after all-the-icons
;;   :hook (dired-mode . all-the-icons-dired-mode))
;; (use-package all-the-icons-gnus
;;   :after all-the-icons
;;   :config (all-the-icons-gnus-setup))
;; (use-package all-the-icons-ivy
;;   :after all-the-icons
;;   :config (all-the-icons-ivy-setup))

;; (unless (package-installed-p 'r5rs)
;;   (package-install 'r5rs))
;; #+END_SRC


;; phps-mode scroll-on-drag spacebar with-Emacs
