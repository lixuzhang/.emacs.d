;;; init-external.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  李旭章

;; Author: 李旭章 <lixuzhang@lovefeeling.org>
;; Keywords: external



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

(unless (package-installed-p 'r5rs)
  (package-install 'r5rs))

(unless (package-installed-p 'sicp)
  (package-install 'sicp))

;;; ---------------------------------------------------------------------------
(req-package ac-html-angular)
(req-package ac-html-bootstrap)
(req-package ac-html-csswatcher)

;; ace-pinyin.el --- Jump to Chinese characters using ace-jump-mode or aby
(req-package ace-pinyin)

(req-package ace-window)

(req-package aggressive-fill-paragraph
  :config (afp-setup-recommended-hooks))

;; aggressive-indent.el --- Minor mode to aggressively keep your code always indented
(req-package aggressive-indent
  :diminish aggressive-indent-mode
  :config (progn
            (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
            (add-to-list 'aggressive-indent-dont-indent-if
                         '(and (derived-mode-p 'c-mode 'c++-mode 'objc-mode 'java-mode)
                               (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                                   (thing-at-point 'line)))))))


;; anzu.el --- Show number of matches in mode-line while searching
(req-package anzu
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode))

;; apache-mode.el --- major mode for editing Apache configuration files
(req-package apache-mode)

;; artbollocks-mode.el --- Improve your writing (especially about art)
(req-package artbollocks-mode
  :commands artbollocks-mode
  :diminish artbollocks-mode
  :init (setq artbollocks-weasel-words-regex
              (concat "\\b\\(many\\|various\\|very\\|fairly\\|several\\|extremely\\|exceedingly\\|quite\\|remarkably\\|few\\|surprisingly\\|mostly\\|largely\\|huge\\|tiny\\|\\(\\(are\\|is\\) a number\\)\\|excellent\\|interestingly\\|significantly\\|substantially\\|clearly\\|vast\\|relatively\\|completely\\)\\b"
                      (regexp-opt '("许多" "几乎" "很少" "差不多" "有点" "大量"
                                    "大致" "迟点" "迟些" "过几天" "大体上")
                                  t)))
  :config (add-hook 'text-mode-hook #'artbollocks-mode))

(req-package auto-correct)
(req-package captain)
(req-package vigenere)

;; AUCTEX
(req-package tex-site
  :ensure auctex
  :init (progn
          (setq TeX-auto-save t)
          (setq TeX-parse-self t)
          (setq-default TeX-master t)))

;; cal-china-x.el --- Chinese localization, lunar/horoscope/zodiac info and more...
(req-package cal-china-x
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
(req-package calfw
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
(req-package calfw-cal
  :require calfw)

;; calfw-org.el --- calendar view for org-agenda
(req-package calfw-org
  :require calfw
  :init (progn
          (setq cfw:org-agenda-schedule-args '(:timestamp))
          (setq cfw:org-overwrite-default-keybinding t)))

;; c-eldoc.el --- helpful description of the arguments to C functions
(req-package c-eldoc
  :commands c-turn-on-eldoc-mode
  :init (setq c-eldoc-cpp-command "cpp")
  :config (progn
            (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
            (add-hook 'c++-mode-hook #'c-turn-on-eldoc-mode)))

;; chinese-fonts-setup.el --- A fonts config tool enforcing double-width Chinese character display
(req-package chinese-fonts-setup
  :disabled t
  :init (progn
          (setq cfs-use-face-font-rescale nil)
          (setq cfs-profiles '("general" "program" "other"))
          (setq cfs--current-profile "general")
          (setq cfs--profiles-steps '(("general" . 4))))
  :config (chinese-fonts-setup-enable))

;; chinese-wbim.el --- Enable Wubi(五笔) Input Method in Emacs.
(req-package chinese-wbim
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
(req-package chm-view)

;; clean-aindent-mode.el --- Simple indent and unindent, trims indent white-space
(req-package clean-aindent-mode
  :config (clean-aindent-mode 1))

;; clean-buffers.el --- clean useless buffers
(req-package clean-buffers
  :config (dolist (bn '("*CEDET Global*" "*Messages*" "*Help*"))
            (add-to-list 'clean-buffers-useless-buffer-names bn)))

;; comment-dwim-2.el --- An all-in-one comment command to rule them all
(req-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; commify.el --- Toggle grouping commas in numbers
(req-package commify)

;; company.el --- Modular text completion framework
(req-package company
  :commands global-company-mode
  :diminish company-mode
  :bind ("C-c /" . company-files)
  :init (progn
          (setq company-tooltip-limit 20)                      ; bigger popup window
          (setq company-tooltip-align-annotations 't)          ; align annotations to the right tooltip border
          (setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
          (setq company-begin-commands '(self-insert-command))) ; start autocompletion only after typing
  :config (add-hook 'after-init-hook #'global-company-mode))

(req-package company-box
  :require company
  :hook (company-mode . company-box-mode))

;; company-c-headers.el --- Company mode backend for C/C++ header files
(req-package company-c-headers
  :require (company semantic)
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
(req-package company-flx
  :require company
  :config (company-flx-mode +1))

(req-package company-math
  :require company)

(req-package company-lsp
  :require (company lsp-mode)
  :commands company-lsp)

;; company-php.el --- company completion source for php
(req-package company-php
  :require (company php-mode)
  :commands company-ac-php-backend
  :config (add-hook 'php-mode-hook
                    (lambda ()
                      (add-to-list 'company-backends #'company-ac-php-backend))))

(req-package company-quickhelp
  :require company)

;; company-shell.el --- Company mode backend for shell functions
(req-package company-shell
  :require company)

(req-package company-statistics
  :require company
  :commands company-statistics-mode
  :config (add-hook 'after-init-hook #'company-statistics-mode))

(req-package company-web
  :require (company web-mode)
  :config (progn
            (require 'company-web-html)
            (require 'company-web-jade)
            (require 'company-web-slim)
            (define-key web-mode-map (kbd "C-'") 'company-web-html)))

(req-package composer)

;; coverlay.el --- Test coverage overlays
(req-package coverlay)

;; css-eldoc.el --- an eldoc-mode plugin for CSS source code
(req-package css-eldoc
  :config (css-eldoc-enable))

;; cursor-chg.el --- Change cursor dynamically, depending on the context.
(req-package cursor-chg
  :config (progn
            (change-cursor-mode 1)
            (curchg-toggle-cursor-type-when-idle 1)))

(req-package dap-mode)
;; (req-package dap-LANGUAGE)

;; deft.el --- quickly browse, filter, and edit plain text notes
(req-package deft
  :require org
  :init (progn
          (setq deft-extension "org")
          (setq deft-text-mode 'org-mode)
          (setq deft-directory "~/notes")
          (setq deft-recursive t)
          (setq deft-use-filename-as-title t)))

;; disaster.el --- Disassemble C/C++ code under cursor in Emacs
;; 显示当前文件的汇编代码
(req-package disaster)

(req-package diminish)

(req-package drag-stuff
  :diminish drag-stuff-mode
  :config (drag-stuff-mode 1))

;; drupal-mode.el --- Advanced minor mode for Drupal development
(req-package drupal-mode
  :require php-mode)

;; dtrt-indent.el --- Adapt to foreign indentation offsets
(req-package dtrt-indent
  :init (setq dtrt-indent-verbosity 1) ; Silent:0  Normal:1  Verbose:2  Diagnostics:3
  :config (dtrt-indent-mode 1))

(req-package duplicate-thing
  :bind ("M-c" . duplicate-thing))

(req-package dynamic-ruler)

;; ecb.el --- a code browser for Emacs
(req-package ecb
  :commands ecb-activate
  :init (progn
          (setq ecb-version-check nil)
          ;; (setq ecb-auto-activate t)
          (setq ecb-compile-window-height 8)
          (setq ecb-compile-window-temporally-enlarge 'both)
          (setq ecb-compile-window-width 'edit-window)
          (setq ecb-fix-window-size 'auto)
          (setq ecb-layout-name "left3")
          (setq ecb-windows-height 0.25)
          (setq ecb-windows-width 0.25)
          (setq ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
          (setq ecb-select-edit-window-on-redraw t)
          (setq ecb-source-path (quote (("~" "HOME") ("/" "/"))))
          (setq ecb-eshell-auto-activate t)
          (setq ecb-tip-of-the-day nil)
          (setq ecb-tip-of-the-day-file
                (expand-file-name "ecb/tip-of-day.el" user-emacs-directory))
          (setq ecb-user-layouts
                (expand-file-name "ecb/user-layouts.el" user-emacs-directory))
          (setq ecb-redraw-layout-quickly t)))

;; ede-php-autoload.el --- Simple EDE PHP Project
(req-package ede-php-autoload
  :require (ede php-mode)
  :commands ede-php-autoload-mode
  :config (progn
            (add-hook 'php-mode-hook #'ede-php-autoload-mode)))

;; ede-php-autoload-composer-installers.el --- Composer installers support for ede-php-autoload
(req-package ede-php-autoload-composer-installers
  :require ede-php-autoload)

;; ede-php-autoload-drupal.el --- Drupal support for ede-php-autoload
(req-package ede-php-autoload-drupal
  :require ede-php-autoload)

;; electric-case.el --- insert camelCase, snake_case words without "Shift"ing
(req-package electric-case
  :commands (electric-case-ahk-init electric-case-c-init electric-case-java-init electric-case-scala-init)
  :config (progn
            (add-hook 'ahk-mode-hook #'electric-case-ahk-init)
            (add-hook 'c-mode-hook #'electric-case-c-init)
            (add-hook 'java-mode-hook #'electric-case-java-init)
            (add-hook 'scala-mode-hook #'electric-case-scala-init)))

;; electric-operator.el --- Automatically add spaces around operators
;; 发展过程：smart-operator => electric-spacing => electric-operator
(req-package electric-operator
  ;; :config (progn
  ;;           (apply 'electric-operator-add-rules-for-mode 'php-mode
  ;;                  electric-operator-prog-mode-rules)
  ;;           (electric-operator-add-rules-for-mode 'php-mode
  ;;                                                 (cons "->" "->")
  ;;                                                 (cons "=>" "=>")))
  )

;; Zen Coding => Emmet
(req-package emmet-mode
  :commands emmet-mode
  :diminish emmet-mode
  :init (progn
          (setq emmet-move-cursor-between-quotes t)
          (setq emmet-expand-jsx-className? t))
  :config (progn
            (add-hook 'sgml-mode-hook #'emmet-mode)
            (add-hook 'css-mode-hook  #'emmet-mode)
            (add-hook 'web-mode-hook #'emmet-mode)))

(req-package emms
  :init (progn
          (setq emms-player-mpg321-command-name "mpg123")
          (setq emms-player-list                  ; 只使用 mplayer 播放
                (list 'emms-player-mplayer-playlist 'emms-player-mplayer)))
  :config (emms-standard))

(req-package eshell-did-you-mean
  :require eshell
  :config (eshell-did-you-mean-setup))

(req-package expand-region
  :bind (("C-=" . er/expand-region)))

(req-package fancy-narrow
  :diminish fancy-narrow-mode
  :config (fancy-narrow-mode))

;; fic-mode.el --- Show FIXME/TODO/BUG/KLUDGE in special face only in comments and strings
(req-package fic-mode
  :commands fic-mode
  :diminish fic-mode
  :config (add-hook 'prog-mode-hook #'fic-mode))

(req-package figlet)

(req-package fliptext)

;; flx-ido.el --- flx integration for ido
(req-package flx-ido
  :require ido
  :config (flx-ido-mode 1))

(req-package flycheck
  :require semantic
  :init (setq flycheck-gcc-include-path (semantic-gcc-get-include-paths "c++")))

(req-package flycheck-color-mode-line
  :require flycheck
  :commands flycheck-color-mode-line-mode
  :config (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

(req-package flycheck-package
  :require (flycheck package-lint)
  :config (flycheck-package-setup))

(req-package flycheck-pkg-config
  :require flycheck)

(req-package flycheck-vala
  :require (flycheck vala-mode))

(req-package fold-dwim-org)

(req-package geiser
  :require scheme
  :init (progn
          (setq geiser-default-implementation 'chez)
          (setq geiser-active-implementations '(chez guile))))

(req-package ggtags
  :commands ggtags-mode
  :init (progn
          (setq gtags-suggested-key-mapping t))
  :config (add-hook 'c-mode-common-hook
                    (lambda ()
                      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                        (ggtags-mode 1)))))

(req-package gitattributes-mode)

(req-package gitconfig-mode)

(req-package git-gutter-fringe
  :diminish git-gutter-mode
  ;; :config (global-git-gutter-mode 1)
  )

(req-package gitignore-mode)

;; git-messenger.el --- Pop up last commit information of current line
(req-package git-messenger
  :require magit
  :commands magit-commit-mode
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:use-magit-popup t))

(req-package git-timemachine)

(req-package golden-ratio
  :diminish golden-ratio-mode
  :init (progn
          (setq golden-ratio-auto-scale t)
          (setq golden-ratio-exclude-modes
                '(bs-mode
                  calc-mode
                  compilation-mode
                  ediff-mode
                  fundamental-mode
                  dired-mode
                  gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode
                  gdb-locals-mode gdb-inferior-io-mode gdb-memory-mode
                  gdb-threads-mode gdb-registers-mode gud-mode
                  restclient-mode
                  speedbar-mode)))
  :config (progn
            ;; 弹出 helm 窗口时不处理
            (add-to-list 'golden-ratio-inhibit-functions
                         (lambda ()
                           (if (boundp 'helm-alive-p)
                               (symbol-value 'helm-alive-p))))
            ;; ECB 激活时不处理
            ;; (add-to-list 'golden-ratio-inhibit-functions
            ;;              (lambda ()
            ;;                (if (boundp 'ecb-minor-mode)
            ;;                    (symbol-value 'ecb-minor-mode))))
            (golden-ratio-mode 1)))

(req-package google-c-style
  :commands (google-set-c-style google-make-newline-indent)
  :config (progn
            (add-hook 'c-mode-common-hook #'google-set-c-style)
            (add-hook 'c-mode-common-hook #'google-make-newline-indent)))

(req-package graphviz-dot-mode
  :init (progn
          (unless (getenv "GRAPHVIZ_DOT")
            (setenv "GRAPHVIZ_DOT" (executable-find "dot")))
          (setq graphviz-dot-indent-width tab-width)))

;; helm.el --- Emacs incremental and narrowing framework
(req-package helm
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
            (require 'helm-config)
            (require 'helm-grep)
            (helm-mode 1)
            (helm-autoresize-mode 1)
            (add-hook 'helm-minibuffer-setup-hook
                      (lambda () (abbrev-mode -1)))))

(req-package helm-backup
  :require helm)

(req-package helm-company
  :require (helm company))

;; helm-descbinds.el --- Yet Another `describe-bindings' with `helm'.
(req-package helm-descbinds
  :require helm)

;; helm-emmet.el --- helm sources for emmet-mode's snippets
(req-package helm-emmet
  :require (helm emmet-mode))

;; helm-gtags.el --- GNU GLOBAL helm interface
(req-package helm-gtags
  :require (helm ggtags)
  :commands helm-gtags-mode
  :bind (("C-c g a" . helm-gtags-tags-in-this-function)
         ("C-j"     . helm-gtags-select)
         ("M-."     . helm-gtags-dwim)
         ("M-,"     . helm-gtags-pop-stack)
         ("C-c <"   . helm-gtags-previous-history)
         ("C-c >"   . helm-gtags-next-history))
  :init (progn
          (setq helm-gtags-ignore-case t
                helm-gtags-auto-update t
                helm-gtags-use-input-at-cursor t
                helm-gtags-pulse-at-cursor t
                helm-gtags-prefix-key "\C-cg"
                helm-gtags-suggested-key-mapping t))
  :config (progn
            (add-hook 'dired-mode-hook  #'helm-gtags-mode)
            (add-hook 'eshell-mode-hook #'helm-gtags-mode)
            (add-hook 'c-mode-hook      #'helm-gtags-mode)
            (add-hook 'c++-mode-hook    #'helm-gtags-mode)
            (add-hook 'java-mode-hook   #'helm-gtags-mode)
            (add-hook 'asm-mode-hook    #'helm-gtags-mode)))

;; helm-make.el --- Select a Makefile target with helm
(req-package helm-make
  :require (helm projectile))

;; helm-mode-manager.el --- Select and toggle major and minor modes with helm
(req-package helm-mode-manager
  :require helm)       ; 与 manage-minor-mode 类似?

(req-package helm-lsp
  :require (helm lsp-mode)
  :commands helm-lsp-workspace-symbol)

;; helm-projectile.el --- Helm integration for Projectile
(req-package helm-projectile
  :require (helm projectile)
  :init (progn
          (setq projectile-completion-system 'helm)
          (setq projectile-switch-project-action 'helm-projectile-find-file)
          (setq projectile-switch-project-action 'helm-projectile))
  :config (progn
            (add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
            (add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html
            (helm-projectile-on)))

(req-package helm-smex
  :require (helm smex))

;; helm-swoop.el --- Efficiently hopping squeezed lines powered by helm interface
(req-package helm-swoop
  :require helm)

;; hide-comnt.el --- Hide/show comments in code.
(req-package hide-comnt)

;; hideif.el --- hides selected code within ifdef
(req-package hideif
  :init (setq hide-ifdef-initially t))

;; hideshow-org.el --- Provides org-mode like hide and show for hideshow.el
(req-package hideshow-org
  :commands hs-org/minor-mode
  :config (add-hook 'prog-mode-hook #'hs-org/minor-mode))

;; hideshowvis.el --- Add markers to the fringe for regions foldable by hideshow.el
(req-package hideshowvis
  :commands hideshowvis-enable
  :config (progn
            (add-hook 'prog-mode-hook #'hideshowvis-enable)
            (hideshowvis-symbols)))

;; hungry-delete.el --- hungry delete minor mode
(req-package hungry-delete
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

;; hydra.el --- Make bindings that stick around.
(req-package hydra)

;; ido-completing-read+.el --- A completing-read-function using ido
(req-package ido-completing-read+
  :require ido
  :config (ido-ubiquitous-mode 1))

;; idomenu.el --- imenu tag selection a la ido
(req-package idomenu
  :require ido)

(req-package iedit
  :bind ("C-;" . iedit-mode))

(req-package import-js)

(req-package indium)

(req-package interleave)

(req-package jdee
  :require ecb)

;; inferior-js-mode
(req-package js-comint
  :config (if (executable-find "node")
              (setq inferior-js-program-command "node --interactive")
            (setq inferior-js-program-command "js")))

(req-package js2-mode
  :commands js2-minor-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config (add-hook 'js-mode-hook #'js2-minor-mode))

(req-package json-mode)

(req-package jsx-mode
  :mode "\\.jsx\\'")


;; kanban.el --- Parse org-todo headlines to use org-tables as Kanban tables
(req-package kanban)

;; keyfreq.el --- track command frequencies
(req-package keyfreq
  :config (progn
            (keyfreq-mode 1)
            (keyfreq-autosave-mode 1)))

;; lentic-mode.el --- minor mode for lentic buffers
(req-package lentic
  :require m-buffer
  :diminish lentic-mode
  :config (global-lentic-mode))

(req-package lispy)


(req-package lsp-mode
  ;; :hook (XXX-mode . lsp)
  :commands lsp)

(req-package lsp-ui
  :require lsp-mode
  :hook (lsp-mode-hook . lsp-ui-mode)
  :commands lsp-ui-mode)

(req-package lsp-treemacs
  :require lsp-mode
  :commands lsp-treemacs-errors-list)

;; magit.el --- A Git porcelain inside Emacs
(req-package magit
  :init (progn
          (setq magit-auto-revert-mode nil)
          (setq magit-diff-paint-whitespace 'status)
          ;; https://github.com/magit/magit/issues/1839
          (setq magit-last-seen-setup-instructions "1.4.0")))

;; manage-minor-mode.el --- Manage your minor-modes easily
(req-package manage-minor-mode)

(req-package mmm-mode
  :init (progn
          (setq mmm-submode-decoration-level 2)
          (setq mmm-global-mode 'buffers-with-submode-classes)))

(req-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("M-<mouse-1>" . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; mwim.el --- Switch between the beginning/end of line or code
(req-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; nameframe.el --- Manage frames by name.
(req-package nameframe
  :bind (("M-P" . nameframe-switch-frame)))

;; ;; nameframe-perspective.el --- Nameframe integration with perspective.el
;; (req-package nameframe-perspective
;;   :require nameframe
;;   :config (nameframe-perspective-mode t))

;; nameframe-projectile.el --- Nameframe integration with Projectile
(req-package nameframe-projectile
  :require nameframe
  :config (nameframe-projectile-mode t))

(req-package never-comment)

(req-package ob-browser
  :require org)

(req-package ob-http
  :require org)

(req-package ox-bibtex-chinese
  :require org)

(req-package org-brain
  :require org)

(req-package org-bullets
  :require org
  :commands org-bullets-mode
  :init (setq org-bullets-bullet-list '("◉" "☯" "🍁" "🍀" "⚝" "🔯" "🌼")) ; ⁂❖🌸🏵🏶✿❉
  :config (add-hook 'org-mode-hook #'org-bullets-mode))

(req-package org-doing
  :require org
  :init (setq org-doing-file "~/life/doing.org"))

(req-package org-edna
  :require org
  :config (org-edna-load))

(req-package org-pomodoro
  :require org)

;; org-present.el --- Minimalist presentation minor-mode for Emacs org-mode.
(req-package org-present
  :require org
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
(req-package org-preview-html
  :require (org eww))

(req-package org-projectile
  :require (org projectile)
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
(req-package org-repo-todo
  :require org
  :bind (("C-;" . ort/capture-todo)
         ("C-'" . ort/capture-checkitem)
         ("C-`" . ort/goto-todos))
  :init (setq ort/prefix-arg-directory "~/life/"))

(req-package org-time-budgets
  :require org)

;; pangu-spacing.el --- Minor-mode to add space between Chinese and English characters.
(req-package pangu-spacing
  :commands pangu-spacing-mode
  :diminish pangu-spacing-mode
  :init (setq pangu-spacing-real-insert-separtor t)
  :config (add-hook 'text-mode-hook #'pangu-spacing-mode))

(req-package parinfer
  :require (lispy paredit)
  :diminish parinfer-mode
  :bind (("C-," . parinfer-toggle-mode))
  :init (setq parinfer-extensions
              '(defaults         ; should be included.
                 pretty-parens   ; different paren styles for different modes.
                 evil            ; If you use Evil.
                 lispy           ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
                 paredit         ; Introduce some paredit commands.
                 smart-tab       ; C-b & C-f jump positions and smart shift with tab & S-tab.
                 smart-yank))    ; Yank behavior depend on mode.
  :config (progn
            (add-hook 'clojure-mode-hook #'parinfer-mode)
            (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
            (add-hook 'common-lisp-mode-hook #'parinfer-mode)
            (add-hook 'scheme-mode-hook #'parinfer-mode)
            (add-hook 'lisp-mode-hook #'parinfer-mode)))

(req-package persp-mode
  :commands persp-mode
  :diminish persp-mode
  :init (progn
          (setq wg-morph-on nil) ;; switch off animation
          (setq persp-autokill-buffer-on-remove 'kill-weak))
  :config (add-hook 'after-init-hook (lambda () (persp-mode 1))))

(req-package persp-mode-projectile-bridge
  :require (persp-mode projectile)
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
(req-package php-mode
  :init (progn
          (setq php-executable (executable-find "php"))
          (setq php-template-compatibility nil))
  :config (require 'php-ext))

(req-package php-boris
  :require php-mode)
(req-package php-boris-minor-mode
  :require (php-mode php-boris)
  :commands php-boris-minor-mode
  :config (add-hook 'php-mode-hook #'php-boris-minor-mode))
(req-package php-eldoc
  :require php-mode)
(req-package php-extras
  :require php-mode)
(req-package php-refactor-mode
  :require php-mode)
(req-package phpunit
  :require php-mode)

(req-package pinyin-search)

(req-package pkgbuild-mode)

(req-package private-diary
  :init (setq private-diary-file "~/life/private.gpg"))

(req-package programmer-dvorak)

(req-package projectile
  :diminish projectile-mode
  :init (progn
          (setq projectile-enable-caching t)
          (setq projectile-indexing-method 'alien))
  :config (progn
            (add-to-list 'projectile-globally-ignored-directories "backup")
            (projectile-global-mode)))

(req-package projectile-speedbar
  :require (projectile sr-speedbar))

;; (req-package plantuml-mode
;;   :mode "\\.plu\\'"
;;   :require graphviz-dot-mode            ; 需要 GRAPHVIZ_DOT 环境变量
;;   :init (progn
;;           ;; 加载前必须先定义 plantuml-plantuml-jar-path，如果找不到则出错。
;;           (setq plantuml-jar-path
;;                 (locate-file "plantuml.jar" exec-path))))

(req-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; rainbow-mode.el --- Colorize color names in buffers
(req-package rainbow-mode)

(req-package region-bindings-mode
  :require multiple-cursors
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

(req-package skewer-mode
  :diminish skewer-html-mode
  :config (skewer-setup))

;; smart-tabs-mode.el --- Intelligently indent with tabs, align with spaces!
;; Smart tabs are only used when indent-tabs-mode is non-nil
(req-package smart-tabs-mode
  :config (progn
            ;; (add-to-list 'smart-tab-disabled-major-modes 'shell-mode) ;; 没此变量
            (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)))

;; smartscan.el --- Jumps between other symbols found at point
(req-package smartscan
  :config (global-smartscan-mode 1))

(req-package smex
  ;; :bind (("M-x" . smex)
  ;;        ("M-X" . smex-major-mode-commands)
  ;;        ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

;; smooth-scrolling.el --- Make emacs scroll smoothly
(req-package smooth-scrolling
  :config (setq smooth-scroll-margin 4))

(req-package sotlisp)



(req-package sr-speedbar
  :require (helm ecb))

;; srefactor.el --- A refactoring tool based on Semantic parser framework
(req-package srefactor
  :config (progn
            (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
            (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)))

;; stickyfunc-enhance.el --- An enhancement to stock `semantic-stickyfunc-mode'
(req-package stickyfunc-enhance)

;; super-save.el --- Auto-save buffers, based on your activity.
(req-package super-save
  :config (super-save-initialize))

(req-package tagedit
  :commands tagedit-mode
  :diminish tagedit-mode
  :config (progn (add-hook 'html-mode-hook #'tagedit-mode)
                 (tagedit-add-paredit-like-keybindings)))

(req-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(req-package unicode-fonts
  :disabled t
  :init (progn
          (setq unicode-fonts-fontset-names '("fontset-default"))
          (setq unicode-fonts-skip-font-groups
                '(buggy-before-vista decorative low-quality-glyphs multicolor non-free)))
  :config (unicode-fonts-setup))

(req-package unicode-whitespace)

(req-package vala-mode)

(req-package voca-builder
  :init (setq voca-builder/voca-file "~/life/vocabulary.org"
              voca-builder/current-tag "default"))

;; volatile-highlights.el --- Minor mode for visual feedback on some operations.
(req-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode 1))

;; 替代 workgroups2
(req-package wconf
  :requires desktop
  :commands wconf-load
  :bind (("C-c w s" . wconf-store)
         ("C-c w S" . wconf-store-all)
         ("C-c w R" . wconf-restore-all)
         ("C-c w r" . wconf-restore)
         ("C-c w w" . wconf-switch-to-config)
         ("C-<prior>" . wconf-use-previous)
         ("C-<next>" . wconf-use-next))
  :config (add-hook 'desktop-after-read-hook ; so we have all buffers again
                    (lambda ()
                      (wconf-load)
                      (wconf-switch-to-config 0)
                      (add-hook 'kill-emacs-hook
                                (lambda ()
                                  (wconf-store-all)
                                  (wconf-save))))))

(req-package web-mode
  :mode ("\\.html?\\'" "\\.tpl\\'" "\\.tpla\\'" "\\.php\\'" "\\.phtml\\'"
         "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
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
(req-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

;; whitespace-cleanup-mode.el --- Intelligently call whitespace-cleanup on save
(req-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

(req-package with-editor
  :commands with-editor-export-editor
  :config (progn
            (add-hook 'shell-mode-hook  #'with-editor-export-editor)
            (add-hook 'term-mode-hook   #'with-editor-export-editor)
            (add-hook 'eshell-mode-hook #'with-editor-export-editor)
            (shell-command-with-editor-mode 1)))

;; ws-butler.el --- Unobtrusively remove trailing whitespace.
(req-package ws-butler
  :commands ws-butler-mode
  :diminish ws-butler-mode
  :config (add-hook 'prog-mode-hook #'ws-butler-mode))
;; #+END_SRC


;; ---------------------------------------------------------------------------
;; 手工安装的扩展。
;; ---------------------------------------------------------------------------
;; #+BEGIN_SRC emacs-lisp
(defun top-level-add-to-load-path (topdir)
  (let ((default-directory topdir))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(top-level-add-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

(require 'epubmode)
(require 'hanconvert)
(require 'sdcv-mode)
;; :bind ("C-c d" . sdcv-search)
(require 'unicad)

;; lilypond-mode.el -- Major mode for editing GNU LilyPond music scores
(require 'lilypond-mode)
(setq auto-mode-alist (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

;; lilypond-quick-insert-mode.el
(setq lyqi-default-language 'italiano)
;; (setq lyqi-use-midi-default t)
;; (setq lyqi-midi-keyboard-command "~/bin/mymidikbd")
(load "lilypond-quick-insert-mode")
;; #+END_SRC



;; ---------------------------------------------------------------------------
;; 放弃的扩展
;; ---------------------------------------------------------------------------
;; #+BEGIN_SRC emacs-lisp
;; 已经不维护
;; (req-package puml-mode
;;   :mode "\\.plu\\'"
;;   :require graphviz-dot-mode            ; 需要 GRAPHVIZ_DOT 环境变量
;;   :init (progn
;;           (defalias 'plantuml-mode 'puml-mode) ; 与 org-plantuml 兼容
;;           ;; 加载前必须先定义 puml-plantuml-jar-path，如果找不到则出错。
;;           (setq puml-plantuml-jar-path
;;                 (substring (locate-file "plantuml.jar" exec-path) 2)))
;;   :config (puml-set-output-type "png"))

;; (req-package auto-package-update
;;   :config (auto-package-update-maybe))

;; (require 'el-get)
;; (el-get 'sync)

;; (req-package esup)

;; (req-package easy-lentic
;;   :require lentic
;;   :config (easy-lentic-mode-setup))

;; (req-package ergoemacs-mode
;;   :init (progn
;;           (setq ergoemacs-theme nil)
;;           (setq ergoemacs-keyboard-layout "programmer-dv"))
;;   :config (ergoemacs-mode 1))

;; (req-package guile-scheme
;;   :require scheme)

;; (req-package outlined-elisp-mode
;;   :config (add-hook 'emacs-lisp-mode-hook #'outlined-elisp-find-file-hook))

;; 启用 rainbow-delimiters 后无效果，并可替代
;; (req-package paren-face
;;   :config (progn
;;             (set-face-foreground 'parenthesis "light gray")
;;             (global-paren-face-mode 1)))

;; 与 rainbow-delimiters 有冲突？
;; (req-package htmlize
;;   :defer t)

;; ;; rainbow-blocks.el --- Block syntax highlighting for lisp code
;; (req-package rainbow-blocks)            ; rainbow-delimiters 修改版，对 blocks 进行着色

;; 与 rainbow-blocks 类似，支持 elisp 和 js
;; (req-package context-coloring
;;   :require js2-mode
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
;; (req-package fringe-current-line
;;   :config (global-fringe-current-line-mode))
;; 效果差
;; (req-package highlight-indent-guides
;;   :config (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))
;; (req-package indent-guide)
;; (req-package highlight-indentation)
;; (req-package page-break-lines
;;   :config (global-page-break-lines-mode))
;; (req-package fill-column-indicator
;;   :config (fci-mode 1))

;; (req-package company-auctex
;;   :require (company tex-site))

;; 没什么用，不安装。
;; (req-package fancy-battery
;;   :disabled t
;;   :require battery
;;   :config (progn
;;             (remove-hook 'after-init-hook #'display-battery-mode)
;;             (add-hook 'after-init-hook #'fancy-battery-mode)))

;; (req-package-force load-dir
;;   :init
;;   (setq load-dir-ignore-errors t
;;         load-dir-recursive     t
;;         load-dirs              t))

;; (req-package outorg)
;; (req-package outshine)
;; (req-package navi-mode)

;; (req-package minimap)

;; (req-package paredit
;;   :commands enable-paredit-mode
;;   :diminish paredit-mode
;;   :config (progn
;;             (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;;             (add-hook 'scheme-mode-hook #'enable-paredit-mode)
;;             (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
;;             (add-hook 'clojure-mode-hook #'enable-paredit-mode)))

;; (req-package paredit-everywhere
;;   :commands paredit-everywhere-mode
;;   :diminish paredit-everywhere-mode
;;   :config (progn
;;             (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
;;             (add-hook 'css-mode-hook #'paredit-everywhere-mode)))

;; (req-package paxedit
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

;; (req-package rw-language-and-country-codes)
;; (req-package rw-ispell
;;   :config (rw-ispell-set-up-pdicts))
;; (req-package rw-hunspell
;;   :config (rw-hunspell-setup))

;; session.el --- use variables, registers and buffer places across sessions
;; (req-package session
;;   :disabled t
;;   :config (progn
;;             (add-hook 'after-init-hook #'session-initialize)
;;             (add-to-list 'session-globals-exclude 'org-mark-ring)))

;; (req-package smartparens
;;   :diminish smartparens-mode
;;   :require smartparens-config
;;   :init
;;   (setq sp-base-key-bindings 'paredit)
;;   (setq sp-autoskip-closing-pair 'always)
;;   (setq sp-hybrid-kill-entire-symbol nil)
;;   :config
;;   (smartparens-global-mode 1)
;;   (show-smartparens-global-mode 1)
;;   (sp-use-smartparens-bindings))

;; (req-package slime)
;; (req-package slime-js)

;; (req-package pomodoro)

;; (req-package pointback
;;   :config (global-pointback-mode))

;; (req-package powerline
;;   :disabled t
;;   :init (progn
;;           (setq powerline-default-separator 'wave)
;;           (setq powerline-height 0.5))
;;   :config (powerline-default-theme))

;; (req-package org-annotate-file)

;; (req-package windmove
;;   :config (progn
;;             ;; Make windmove work in org-mode
;;             (add-hook 'org-shiftup-final-hook #'windmove-up)
;;             (add-hook 'org-shiftleft-final-hook #'windmove-left)
;;             (add-hook 'org-shiftdown-final-hook #'windmove-down)
;;             (add-hook 'org-shiftright-final-hook #'windmove-right)))

;; (req-package yasnippet
;;   :diminish yas-minor-mode
;;   :config
;;   (yas-global-mode 1)
;;   (req-package java-snippets)
;;   (req-package php-auto-yasnippets)
;;   (req-package vala-snippets))

;; (req-package org-chinese-utils
;;   :require org
;;   :config (org-chinese-utils-enable))
;; (req-package ox-latex-chinese
;;   :require org)
;; (req-package ob-php
;;   :require (org php-mode))
;; (req-package speed-type)

;; (req-package persp-projectile
;;   :require (perspective projectile))

;; (req-package perspective
;;   :requires desktop
;;   :commands persp-mode
;;   :config (add-hook 'desktop-after-read-hook (lambda () (persp-mode 1))))

;; (unless (package-installed-p 'r5rs)
;;   (package-install 'r5rs))
;; #+END_SRC

;; sr-speedbar, speed-type, region-bindings-mode, python, projectile-speedbar,
;; persp-mode-projectile-bridge, package-lint, ox-latex-chinese,
;; org-chinese-utils, ob-php, nameframe-projectile, helm-gtags, helm-emmet,
;; ede-php-autoload-drupal, diminish, company-web, chinese-fonts-setup,
;; ace-jump-mode, proceed? (y or n) n
