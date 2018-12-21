;;; init.el --- Emacs 配置文件                        -*- lexical-binding: t; -*-

;;; Header:

;; Copyright (C) 2016, 2017, 2018 李旭章

;; Author: 李旭章 <lixuzhang@lovefeeling.org>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 考虑过多种组织方式，比如按应用分类、按主次模式分类等，但总有各种特殊情况感觉
;; 这些分类方式不合理。于是干脆按 emacs 的核心功能、扩展功能、第三方扩展进行分类，
;; 并按字母顺序排列。

;;; Code:

;; ** 核心
;; #+BEGIN_SRC emacs-lisp
;; emacs.c --- Fully extensible Emacs, running on Unix, intended for GNU.
(let ((minver "24.5"))
  (when (version<= emacs-version minver)
    (error "本配置文件要求 Emacs 版本应不低于 V%s" minver)))

;; buffer.c --- Buffer manipulation primitives for GNU Emacs.
;; 类似 default-* 的变量名从 23.2 版之后已经过时
;; (setq-default major-mode 'text-mode)
(setq-default tab-width 4)
(setq-default abbrev-mode t)
(setq-default fill-column 80)
(setq-default truncate-lines t)
(setq-default transient-mark-mode t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
;; (setq mode-line-format )

;; eval.c --- Evaluator for GNU Emacs Lisp interpreter.
(setq debug-on-error t)

;; xkdispnew.c --- Updating of data structures for redisplay.
(setq visible-bell t)

;; edfns.c --- Lisp functions pertaining to editing.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; fns.c --- Random utility Lisp functions.
(setq use-file-dialog nil)
(setq use-dialog-box nil)


;; fontset.c --- Fontset handler.
;; 在 emacs 默认的字体设置之前添加“Noto”、“花园明体”作为默认字体
;; 所有 Script 清单可通过 (char-table-extra-slot char-script-table 0) 获取
;; Script 表 http://www.unicode.org/charts/
;; 列出字符集命令：list-charset-chars
;; 字符区域范围：
;; http://www.unicode.org/Public/UCD/latest/ucd/Blocks.txt
;; http://www.unicode.org/Public/UNIDATA/Blocks.txt
;; https://en.wikipedia.org/wiki/Unicode_block
;; http://fonts.jp/hanazono/
;; 特定字符范围的字符集写法类似于：
;;     (cons (decode-char 'ucs #x0000) (decode-char 'ucs #xFFFF))
(dolist (charset '(emacs nil))
  (set-fontset-font t charset           ; 覆盖 CJK B/C/D/E
                    (font-spec :family "HanaMinB")
                    nil 'prepend)
  (set-fontset-font t charset ; 覆盖非汉字、URO 及其扩展、CJK A、互换文字及其扩展
                    (font-spec :family "HanaMinA")
                    nil 'prepend)
  (set-fontset-font t charset
                    (font-spec :family "Noto Sans Mono CJK SC Regular")
                    nil 'prepend)
  (set-fontset-font t charset
                    (font-spec :family "Symbola")
                    nil 'prepend))

;; 标准字体
(cond
 ;; Microsoft Windows
 ((string-equal system-type "windows-nt")
  ;; 英文字体
  (if (member "Consolas" (font-family-list))
      (create-fontset-from-ascii-font
       "-outline-Consolas-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
       nil "standard")
    (create-fontset-from-ascii-font
     "-outline-Courier New-normal-normal-normal-mono-17-*-*-*-c-*-iso8859-1"
     nil "standard"))
  ;; 中文字体
  (if (member "微软雅黑" (font-family-list))
      (set-fontset-font "fontset-standard" 'gb18030
                        (font-spec :family "微软雅黑" :size 18)
                        nil 'prepend)
    (set-fontset-font "fontset-standard" 'gb18030
                      (font-spec :family "新宋体" :size 18) nil 'prepend))
  ;; 扩展字体
  (when (member "SimSun-ExtB" (font-family-list))
    (set-fontset-font "fontset-standard" 'gb18030
                      (font-spec :family "SimSun-ExtB" :size 18) nil 'append)))
 ;; GNU/Linux
 ((string-equal system-type "gnu/linux")
  (create-fontset-from-fontset-spec
   "-outline-DejaVu Sans Mono-normal-normal-normal-mono-16-*-*-*-c-*-fontset-standard"))
 ;; Mac OS X
 ((string-equal system-type "darwin")
  ;; TODO
  ))


;; frame.c --- Generic frame functions.
;; 只有本设置才对窗口显示字体起作用，set-face-attribute、set-face-font 和
;; set-frame-font 均不起作用。
(setq default-frame-alist
      '((font .  "fontset-standard")
        (menu-bar-lines . nil)
        (vertical-scroll-bars . nil)
        (alpha . (100 90))))
;; (setq make-pointer-invisible nil)

;; indent.c --- Indentation functions.
(setq-default indent-tabs-mode nil)

;; lread.c --- Lisp parsing and input streams.
;; (add-to-list 'load-path
;;              (expand-file-name "el-get/el-get" user-emacs-directory))

;; xdisp.c --- Display generation from window structure and buffer text.
(setq x-stretch-cursor t)

;; xfaces.c --- "Face" primitives.
;; (setq face-font-rescale-alist
;;       '(("Courier New" . 1.0)           ; ASCII 字符 基准字体
;;         ("Hack" . 1.0)
;;         ("宋体" . 1.25)
;;         ("楷体" . 1.25)
;;         ("新宋体" . 1.25)
;;         ("微软雅黑" . 1.25)
;;         ("思源黑体.*" . 1.25)
;;         ("Noto Sans CJK.*" . 1.25)
;;         ("Microsoft Yahei UI" . 1.25)
;;         ("HanaMinA" . 1.25)
;;         ("HanaMinB" . 1.25)))

"
  ========== face-font-rescale-alist ===== 效果测试 ==========
  01|23|45|67|89|01|23|45|67|89|01|23|45|67|89|01|23|45|67|89|
  零|一|二|三|四|五|六|七|八|九|零|一|二|三|四|五|六|七|八|九|
  　|  正常字体    |  粗体        |   粗斜体        |
  　|--------------+--------------+-----------------|
  　|  堂堂正正    |  *五大三粗*  |   /东倒西歪/    |
  　|  I'm normal. |  *I'm bold!* |   /I'm italic?/ |
  　|  𠄀𠄁𠄂𠄃    |  *𠄄𠄅𠄆𠄇*  |   /𠄈𠄉𠄊𠄋/    |
  零|一|二|三|四|五|六|七|八|九|零|一|二|三|四|五|六|七|八|九|
  01|23|45|67|89|01|23|45|67|89|01|23|45|67|89|01|23|45|67|89|
"

;; w32font.c --- Font backend for the Microsoft Windows API.
(when (string-equal system-type "windows-nt")
  (setq w32-charset-info-alist
        (cons '("gbk" w32-charset-gb2312 . 936) w32-charset-info-alist)))
;; #+END_SRC

;; *** Lisp
;; #+BEGIN_SRC emacs-lisp
;;; ===========================================================================
;;; lisp
;;; ===========================================================================

;; indent.el --- indentation commands for Emacs
(setq-default tab-always-indent 'complete)

;; international/mule-cmds.el --- commands for multilingual environment
(prefer-coding-system 'utf-8)

;; textmodes/text-mode.el --- text mode, and its idiosyncratic commands
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; textmodes/paragraphs.el --- paragraph and sentence parsing
(setq sentence-end-double-space nil)
(use-hard-newlines 'guess)

;; subr.el --- basic lisp subroutines for Emacs
(fset 'yes-or-no-p 'y-or-n-p)

;; startup.el --- process Emacs shell arguments
;; (setq fancy-splash-image nil)
(setq inhibit-startup-screen t) ; inhibit-startup-message => inhibit-splash-screen
(setq inhibit-startup-echo-area-message "Happy Hacking!")
;; #+END_SRC

;; *** emacs-lisp
;; #+BEGIN_SRC emacs-lisp
;;; ---------------------------------------------------------------------------
;;; emacs-lisp

;; advice.el --- An overloading mechanism for Emacs Lisp functions
(setq ad-redefinition-action 'accept)
(require 'advice)

;; copyright.el --- update the copyright notice in current buffer
(require 'copyright)

;; eieio.el --- Enhanced Implementation of Emacs Interpreted Objects
;;              or maybe Eric's Implementation of Emacs Interpreted Objects
(require 'eieio)

;; eldoc.el --- Show function arglist or variable docstring in echo area
(require 'eldoc)
(eldoc-mode 1)
;; (global-eldoc-mode 1)

;; package.el --- Simple package system for Emacs
(require 'package)
(dolist (pa '(;;("gnu" . "http://elpa.gnu.org/packages/") ; 默认已有
              ("org" . "http://orgmode.org/elpa/")
              ;; ("popkit" . "http://elpa.popkit.org/packages/")
              ;;("melpa" . "http://melpa.org/packages/")
              ;;("marmalade" . "https://marmalade-repo.org/packages/")
              ))
  (add-to-list 'package-archives pa t))
(setq package-enable-at-startup nil)
;; #+END_SRC

;; ** 基础包
;; #+BEGIN_SRC emacs-lisp
;; abbrev.el --- abbrev mode commands for Emacs
(setq abbrev-file-name
      (expand-file-name "abbrev_defs.el" user-emacs-directory))
(require 'abbrev)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))
;; 在 minibuffer 中禁用缩写功能
(add-hook 'minibuffer-setup-hook (lambda () (abbrev-mode -1)))
;; (add-hook 'minibuffer-exit-hook (lambda () (abbrev-mode 1)))

;; apropos.el --- apropos commands for users and programmers
(setq apropos-do-all t)
(require 'apropos)

;; cus-edit.el --- tools for customizing Emacs and Lisp packages
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'cus-edit)

;; custom.el --- tools for declaring and initializing options
(require 'custom)

;; env.el --- functions to manipulate environment variables
(require 'env)
(dolist (e '(("JAVA_TOOL_OPTIONS" .
              "-Dfile.encoding=utf-8 -Duser.language=zh_CN -Duser.country=CN")))
  (let ((name (car e))
        (value (cdr e)))
    (unless (getenv name)
      (setenv name value))))
;; 获取 gcc include paths 的方法
;; (process-lines "sh" "-c" "echo | cpp -x c++ -Wp,-v - 2>&1 | sed '/#/d;/ignoring/d;/End of/d;s/^ //g' | xargs cygpath.exe -w | sort")
;; 或 (semantic-gcc-get-include-paths "c++")

;; faces.el --- Lisp faces
(require 'faces)
;; 要有本句才能对 tooltip 窗口的字体生效。
;; (set-face-font 'default "fontset-standard")
(set-face-attribute 'default nil :font "fontset-standard")

;; files.el --- file input and output commands for Emacs
(setq-default require-final-newline t)
(setq find-file-suppress-same-file-warnings t)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-default nil)
(setq make-backup-files nil)
(require 'files)
;; (add-hook 'before-save-hook #'copyright-update)

;; frame.el --- multi-frame management independent of window systems
(setq initial-frame-alist '((fullscreen . fullboth)))
(setq minibuffer-frame-alist '((font . "fontset-standard")))
(require 'frame)
;; (blink-cursor-mode 1)
;; (set-frame-font "fontset-standard" nil t)  ; 不起作用?

;; menu-bar.el --- define a default menu bar
;; (require 'menu-bar)
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))

;; midnight.el --- run something every midnight, e.g., kill old buffers
(require 'midnight)

;; mouse.el --- window system-independent mouse support
(require 'mouse)
(setq mouse-yank-at-point t)

;; progmodes/prog-mode.el --- Generic major mode for programming
(require 'prog-mode)
(global-prettify-symbols-mode 1)

;; scroll-bar.el --- window system-independent scroll bar support
;; (require 'scroll-bar)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;; simple.el --- basic editing commands for Emacs
(require 'simple)
;;  :bind ("RET" . newline-and-indent)
(setq line-number-mode t)
(setq column-number-mode t)
(setq size-indication-mode t)
(setq save-interprogram-paste-before-kill t)

;; tool-bar.el --- setting up the tool bar
;; (require 'tool-bar)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

;; tooltip.el --- show tooltip windows
;; (require 'tooltip)
(when (fboundp 'tooltip-mode)
  (tooltip-mode 1))

;; uniquify.el --- unique buffer names dependent on file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;; #+END_SRC

;; ** 扩展包
;; 加载扩展之前，必须先初始化。
;; #+BEGIN_SRC emacs-lisp
(package-initialize)

(unless (package-installed-p 'req-package)
  (package-refresh-contents)
  (package-install 'req-package))

;; (require 'bind-key)
;; (require 'diminish)
;; (setq use-package-always-ensure t)
;; (setq use-package-debug t)
;; (setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(setq req-package-log-level 'error)
(require 'req-package)
;; #+END_SRC

;; *** 内置软件包
;; #+BEGIN_SRC emacs-lisp
;; autoinsert.el --- automatic mode-dependent insertion of text into new files
(req-package autoinsert
  :config (auto-insert-mode 1))

;; autorevert.el --- revert buffers when files on disk change
(req-package autorevert
  :config (global-auto-revert-mode 1))

;; avoid.el --- make mouse pointer stay out of the way of editing
(req-package avoid
  :config (mouse-avoidance-mode 'animate))

;; battery.el --- display battery status information
(req-package battery
  :commands display-battery-mode
  :init (add-hook 'after-init-hook #'display-battery-mode))

;; bookmark.el --- set bookmarks, maybe annotate them, jump to them later
(req-package bookmark)

;; dabbrev.el --- dynamic abbreviation package
(req-package dabbrev)

;; delsel.el --- delete selection if you insert
(req-package delsel
  :config (delete-selection-mode 1))

;; descr-text.el --- describe text mode  -*- lexical-binding:t -*-
(req-package descr-text
  :init (setq describe-char-unidata-list t))

;; desktop.el --- save partial status of Emacs when killed
(req-package desktop
  :init (setq desktop-load-locked-desktop t)
  :config (desktop-save-mode 1))

;; doc-view.el --- View PDF/PostScript/DVI files in Emacs
(req-package doc-view
  :init (if (executable-find "gswin32c")
            (setq doc-view-ghostscript-program "gswin32c")))

;; hexl.el --- edit a file in a hex dump format using the hexl filter
(req-package hexl
  :commands (hexl-follow-line hexl-active-ruler)
  :init (progn
          (add-hook 'hexl-mode-hook #'hexl-follow-line)
          (add-hook 'hexl-mode-hook #'hexl-activate-ruler)))

;; hippie-exp.el --- expand text trying various ways to find its expansion
(req-package hippie-exp
  :require dabbrev
  :bind ("M-/" . hippie-expand)
  :init (setq hippie-expand-try-functions-list
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
(req-package hl-line
  :config (global-hl-line-mode 1))

;; ido.el --- interactively do things with buffers and files
(req-package ido
  :init (progn
          (setq ido-auto-merge-work-directories-length 0)
          (setq ido-enable-flex-matching t)
          (setq ido-use-faces nil)
          (setq ido-use-virtual-buffers t)
          (setq ido-use-filename-at-point nil))
  :config (progn
            (add-to-list 'ido-ignore-files "\\.DS_Store")
            (ido-mode 1)
            (ido-everywhere 1)))

;; image-file.el --- support for visiting image files
(req-package image-file
  :config (auto-image-file-mode 1))

;; let-alist.el --- Easily let-bind values of an assoc-list by their names
(req-package let-alist)

;; leuven-theme.el --- Awesome Emacs color theme on white background
(req-package leuven-theme
  :config (load-theme 'leuven t))

;; linum.el --- display line numbers in the left margin
(req-package linum
  :commands linum-mode
  :init (add-hook 'prog-mode-hook #'linum-mode))

;; paren.el --- highlight matching paren
(req-package paren
  :init (setq show-paren-style 'expression)
  :config (show-paren-mode 1))

;; savehist.el --- Save minibuffer history
(req-package savehist
  :init (savehist-mode 1))

;; saveplace.el --- automatically save place in files
(req-package saveplace
  :init (setq-default save-place t))

;; select.el --- lisp portion of standard selection support
(req-package select
  :init (progn
          (setq select-enable-primary t)
          (setq select-enable-clipboard t)
          (when (display-graphic-p)
            (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))))

;; server.el --- Lisp code for GNU Emacs running as server process
(req-package server
  :config (server-start))

;; skeleton.el --- Lisp language extension for writing statement skeletons
(req-package skeleton
  :init (progn
          (setq skeleton-further-elements '((abbrev-mode nil)))
          (setq skeleton-pair t)
          (setq skeleton-pair-alist
                '((?\' _ "\'" >)
                  (?\" _ "\"" >)
                  (?\< _ ">" >)
                  (?\( _ ")" >)
                  (?\[ _ "]" >)
                  (?\{ _ "}" >)))))

;; time.el --- display time, load and mail indicator in mode line of Emacs
(req-package time
  :init (progn
          (setq display-time-use-mail-icon t)
          (setq display-time-24hr-formatxk t)
          (setq display-time-day-and-date nil)
          (setq display-time-default-load-average nil))
  :config (display-time-mode 1))

;; time-stamp.el --- Maintain last change time stamps in files edited by Emacs
(req-package time-stamp
  :commands time-stamp
  :init (progn
          (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %Z %U")
          ;; (add-hook 'before-save-hook #'time-stamp)
          ))

;; whitespace.el --- minor mode to visualize TAB, (HARD) SPACE, NEWLINE
(req-package whitespace
  :diminish global-whitespace-mode
  :init (progn
          (setq whitespace-line-column nil) ; Use fill-column
          (setq whitespace-style
                '(face trailing lines-tail tabs)))
  :config (global-whitespace-mode 1))
;; #+END_SRC

;; **** calendar
;; #+BEGIN_SRC emacs-lisp
;; calendar.el --- calendar functions
(req-package calendar
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
(req-package diary-lib
  :require calendar
  :init (setq diary-remind-message
              '("提醒：离"
                diary-entry
                "仅剩"
                (if (zerop (% days 7))
                    (format "%d 周" (/ days 7))
                  (format "%d 天" days)))))

;; timeclock.el --- mode for keeping track of how much you work
(req-package timeclock
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
(req-package cedet)

;; ede.el --- Emacs Development Environment gloss
(req-package ede
  :init (setq ede-project-directories t)
  :config (progn
            (global-ede-mode 1)
            ;; ede/generic.el --- Base Support for generic build systems
            (require 'ede/generic)
            (ede-enable-generic-projects)))

;; semantic.el --- Semantic buffer evaluator.
(req-package semantic
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
            (require 'semantic/bovine/scm)
            ))

;; srecode.el --- Semantic buffer evaluator.
(req-package srecode
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
(req-package org
  :require remember
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
                  (sequence "☐(t)" "◯(h@/!)" "⭙(n@/!)" "⏳(w@/!)" "📆(s)" "✆(p/!)" "🗫(m/!)" "|" "☒(c@/!)" "☑(d)") ; 执行状态
                  ))
          (setq org-todo-keyword-faces
                '(("☐" :foreground "red" :weight bold)
                  ("◯" :foreground "magenta" :weight bold)
                  ("⭙" :foreground "blue" :weight bold)
                  ("⏳" :foreground "orange" :weight bold)
                  ("📆" :foreground "blue" :weight bold)
                  ("✆" :foreground "forest green" :weight bold)
                  ("🗫" :foreground "forest green" :weight bold)
                  ("☒" :foreground "forest green" :weight bold)
                  ("☑" :foreground "forest green" :weight bold)
                  ))
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
                  (:startgroup) ("🏷" . ?b) ("🎔" . ?f) ("󳊙" . ?s) (:endgroup) ; 性质
                  ))
          (setq org-tags-exclude-from-inheritance
                '("🗁" "󳊙"))
          (setq org-fontify-done-headline t)
          ;; 代码着色
          (setq org-src-fontify-natively t)
          ;; Latex 输出
          (setq org-latex-create-formula-image-program 'imagemagick)
          (setq org-latex-default-packages-alist
                '(("AUTO" "inputenc" t
                   ("pdflatex"))
                  ("T1" "fontenc" t
                   ("pdflatex"))
                  ("" "graphicx" t)
                  ("" "grffile" t)
                  ("" "longtable" nil)
                  ("" "wrapfig" nil)
                  ("" "rotating" nil)
                  ("normalem" "ulem" t)
                  ("" "amsmath" t)
                  ("" "textcomp" t)
                  ("" "amssymb" t)
                  ("" "capt-of" nil)
                  ("colorlinks" "hyperref" nil)
                  ("" "fixltx2e" nil)
                  ("" "float" nil)
                  ("" "marvosym" t)
                  ("" "wasysym" t)
                  "\\tolerance=1000"))
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
                  (locate-file "ditaa.jar" exec-path))

            ;; ob-plantuml.el --- org-babel functions for plantuml evaluation
            (setq org-plantuml-jar-path
                  (locate-file "plantuml.jar" exec-path))

            ;; org-capture.el --- Fast note taking in Org-mode
            (require 'org-capture)
            ;;("C-c c" . org-capture)

            ;; org-indent.el --- Dynamic indentation for  Org-mode
            (require 'org-indent)
            ;; :diminish org-indent-mode

            ;; ox.el
            (setq org-export-with-toc             nil
                  org-export-with-section-numbers t
                  org-export-default-language     "zh-CN")

            ;; ox-latex.el
            (setq org-latex-listings 'minted)
            (setq org-latex-minted-langs
                  '((emacs-lisp "common-lisp")
                    (cc "c++")
                    (shell-script "bash")))
            (setq org-latex-pdf-process
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
            (setq org-latex-image-default-option "")))
;;; ---------------------------------------------------------------------------
;; #+END_SRC

;; **** progmodes
;; #+BEGIN_SRC emacs-lisp
;;; ---------------------------------------------------------------------------
;;; progmodes

;; cc-mode.el --- major mode for editing C and similar languages
(req-package cc-mode
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
(req-package executable
  :commands executable-make-buffer-file-executable-if-script-p
  :init (add-hook 'after-save-hook
                  #'executable-make-buffer-file-executable-if-script-p))

;; gdb-mi.el --- User Interface for running GDB
(req-package gdb-mi
  :init (progn
          (setq gdb-many-windows t)
          (setq gdb-show-main t)))

(req-package hideshow
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :init (add-hook 'prog-mode-hook #'hs-minor-mode))

;; python.el --- Python's flying circus support for Emacs
(req-package python
  :init (progn
          (setq python-skeleton-autoinsert t)
          (when (executable-find "python3")
            (setq python-shell-interpreter "python3"))))

;; scheme.el --- Scheme (and DSSSL) editing mode
(req-package scheme
  ;; :init (when (executable-find "guile")
  ;;         (setq scheme-program-name "guile"))
  )

;; sql.el --- specialized comint.el for SQL interpreters
(req-package sql
  :init (setq sql-postgres-login-params '((server :default "localhost")
                                          (user :default "postgres")
                                          (database :default "postgres"))))

;; subword.el --- Handling capitalized subwords in a nomenclature
(req-package subword
  :diminish subword-mode
  :config (global-subword-mode))
;;; ---------------------------------------------------------------------------
;; #+END_SRC

;; **** textmodes
;; #+BEGIN_SRC emacs-lisp
;;; ---------------------------------------------------------------------------
;;; textmodes

;; ispell.el --- interface to International Ispell Versions 3.1 and 3.2
(req-package ispell
  :config
  (setq ispell-dictionary "english"))

;; flyspell.el --- On-the-fly spell checker
(req-package flyspell
  :require ispell
  :commands (turn-on-flyspell flyspell-prog-mode)
  :diminish flyspell-mode
  :config (when (executable-find ispell-program-name)
            (add-hook 'text-mode-hook #'turn-on-flyspell)
            (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

;; remember --- a mode for quickly jotting down things to remember
(req-package remember
  :init (setq remember-data-file "~/life/remember.org")
  :config (add-to-list 'remember-handler-functions
                       #'remember-diary-extract-entries))

;;; ---------------------------------------------------------------------------
;; #+END_SRC

;; *** 第三方扩展
;; #+begin_src emacs-lisp
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

;; ace-pinyin.el --- Jump to Chinese characters using ace-jump-mode or aby
(req-package ace-pinyin)

(req-package ac-html-angular)
(req-package ac-html-bootstrap)
(req-package ac-html-csswatcher)

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
                                                   (thing-at-point 'line)))))
            ;(global-aggressive-indent-mode 1)
            ))

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
  :init (progn
          (setq artbollocks-weasel-words-regex
                (concat "\\b\\(many\\|various\\|very\\|fairly\\|several\\|extremely\\|exceedingly\\|quite\\|remarkably\\|few\\|surprisingly\\|mostly\\|largely\\|huge\\|tiny\\|\\(\\(are\\|is\\) a number\\)\\|excellent\\|interestingly\\|significantly\\|substantially\\|clearly\\|vast\\|relatively\\|completely\\)\\b"
                        (regexp-opt '("许多" "几乎" "很少" "差不多" "有点" "大量"
                                      "大致" "迟点" "迟些" "过几天" "大体上")
                                    t)))
          (add-hook 'text-mode-hook #'artbollocks-mode)))

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
              (cfw:org-create-source "Green")  ; orgmode source
              )))))

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
  :init (progn
          (setq c-eldoc-cpp-command "cpp")
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
  :config (dolist (bn '("*CEDET Global*" "*Messages*" "\\*helm-mode-\\*"))
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
          (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
          (add-hook 'after-init-hook #'global-company-mode)))

;; company-c-headers.el --- Company mode backend for C/C++ header files
(req-package company-c-headers
  :require (company semantic)
  :commands company-c-headers
  :init (progn
          (add-hook 'c-mode-hook
                    (lambda ()
                      (setq company-c-headers-path-system
                            (semantic-gcc-get-include-paths "c"))))
          (add-hook 'c++-mode-hook
                    (lambda ()
                      (setq company-c-headers-path-system
                            (semantic-gcc-get-include-paths "c++"))))
          (add-to-list 'company-backends #'company-c-headers)))

;; company-flx.el --- flx based fuzzy matching for company -*- lexical-binding: t -*-
(req-package company-flx
  :require company
  :config (company-flx-mode +1))

(req-package company-math
  :require company)

;; company-php.el --- company completion source for php
(req-package company-php
  :require (company php-mode)
  :commands company-ac-php-backend
  :init (add-hook 'php-mode-hook
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
  :init (add-hook 'after-init-hook #'company-statistics-mode))

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

(req-package drag-stuff
  :diminish drag-stuff-mode
  :config (drag-stuff-mode 1))

;; drupal-mode.el --- Advanced minor mode for Drupal development
(req-package drupal-mode
  :require php-mode)

;; dtrt-indent.el --- Adapt to foreign indentation offsets
(req-package dtrt-indent
  :init (setq dtrt-indent-verbosity 0) ; Silent:0  Normal:1  Verbose:2  Diagnostics:3
  :config (dtrt-indent-mode 1))

(req-package duplicate-thing
  :bind ("M-c" . duplicate-thing))

(req-package dynamic-ruler)

;; (req-package easy-lentic
;;   :require lentic
;;   :config (easy-lentic-mode-setup))

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
  :init (progn
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
  :init (progn
          (add-hook 'ahk-mode-hook #'electric-case-ahk-init)
          (add-hook 'c-mode-hook #'electric-case-c-init)
          (add-hook 'java-mode-hook #'electric-case-java-init)
          (add-hook 'scala-mode-hook #'electric-case-scala-init)))

;; electric-operator.el --- Automatically add spaces around operators
;; 发展过程：smart-operator => electric-spacing => electric-operator
(req-package electric-operator
  :config (progn
            (apply 'electric-operator-add-rules-for-mode 'php-mode
                   electric-operator-prog-mode-rules)
            (electric-operator-add-rules-for-mode 'php-mode
                                                  (cons "->" "->")
                                                  (cons "=>" "=>"))))

;; Zen Coding => Emmet
(req-package emmet-mode
  :commands emmet-mode
  :diminish emmet-mode
  :init (progn
          (setq emmet-move-cursor-between-quotes t)
          (setq emmet-expand-jsx-className? t)
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
  :init (add-hook 'prog-mode-hook #'fic-mode))

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
  :init (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

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
          (setq gtags-suggested-key-mapping t)
          (add-hook 'c-mode-common-hook
                    (lambda ()
                      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                        (ggtags-mode 1))))))

(req-package gitattributes-mode)

(req-package gitconfig-mode)

(req-package git-gutter-fringe
  :config (global-git-gutter-mode 1))

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
  :init (progn
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
            (setq helm-google-suggest-use-curl-p t))
          (add-hook 'helm-minibuffer-setup-hook
                    (lambda () (abbrev-mode -1))))
  :config (progn
            (require 'helm-config)
            (require 'helm-grep)
            (helm-mode 1)
            (helm-autoresize-mode 1)))

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
                helm-gtags-suggested-key-mapping t)
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
  :init (add-hook 'prog-mode-hook #'hs-org/minor-mode))

;; hideshowvis.el --- Add markers to the fringe for regions foldable by hideshow.el
(req-package hideshowvis
  :commands hideshowvis-enable
  :init (add-hook 'prog-mode-hook #'hideshowvis-enable)
  :config (hideshowvis-symbols))

;; hungry-delete.el --- hungry delete minor mode
(req-package hungry-delete
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

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
  :init (add-hook 'js-mode-hook #'js2-minor-mode))

(req-package json-mode)

(req-package jsx-mode
  :mode "\\.jsx\\'" )


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
  :config (global-lentic-mode))

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
  :require (projectile perspective)
  :bind (("M-P" . nameframe-switch-frame)))

;; nameframe-perspective.el --- Nameframe integration with perspective.el
(req-package nameframe-perspective
  :require nameframe
  :config (nameframe-perspective-mode t))

;; nameframe-projectile.el --- Nameframe integration with Projectile
(req-package nameframe-projectile
  :require nameframe
  :config (nameframe-projectile-mode t))

(req-package never-comment)

(req-package ob-browser
  :require org)

(req-package ob-http
  :require org)

(req-package ob-php
  :require (org php-mode))

(req-package org-chinese-utils
  :require (org ox)
  :config (org-chinese-utils-enable))

(req-package ox-bibtex-chinese
  :require org)

(req-package org-brain
  :require org)

(req-package ox-latex-chinese
  :require org)

(req-package org-bullets
  :require org
  :commands org-bullets-mode
  :init (progn
          (setq org-bullets-bullet-list '("◉" "☯" "🍁" "🍀" "⚝" "🔯" "🌼")) ; ⁂❖🌸🏵🏶✿❉
          (add-hook 'org-mode-hook #'org-bullets-mode)))

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
  :init (progn
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
         ("C-`" . ort/goto-todos)))

(req-package org-time-budgets
  :require org)

;; pangu-spacing.el --- Minor-mode to add space between Chinese and English characters.
(req-package pangu-spacing
  :commands pangu-spacing-mode
  :diminish pangu-spacing-mode
  :init (progn
          (setq pangu-spacing-real-insert-separtor t)
          (add-hook 'text-mode-hook #'pangu-spacing-mode)))

(req-package paredit
  :commands enable-paredit-mode
  :diminish paredit-mode
  :init (progn
          (add-hook 'lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'scheme-mode-hook #'enable-paredit-mode)
          (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
          (add-hook 'clojure-mode-hook #'enable-paredit-mode)))

(req-package paredit-everywhere
  :commands paredit-everywhere-mode
  :diminish paredit-everywhere-mode
  :init (progn
          (add-hook 'prog-mode-hook #'paredit-everywhere-mode)
          (add-hook 'css-mode-hook #'paredit-everywhere-mode)))

(req-package paxedit
  :commands paxedit-mode
  :diminish paxedit-mode
  :bind (("M-<right>" . paxedit-transpose-forward)
         ("M-<left>" . paxedit-transpose-backward)
         ("M-<up>" . paxedit-backward-up)
         ("M-<down>" . paxedit-backward-end)
         ("M-b" . paxedit-previous-symbol)
         ("M-f" . paxedit-next-symbol)
         ("C-%" . paxedit-copy)
         ("C-&" . paxedit-kill)
         ("C-*" . paxedit-delete)
         ("C-^" . paxedit-sexp-raise)
         ("M-u" . paxedit-symbol-change-case)
         ("C-@" . paxedit-symbol-copy)
         ("C-#" . paxedit-symbol-kill))
  :init (progn
          (add-hook 'lisp-mode-hook #'paxedit-mode)
          (add-hook 'scheme-mode-hook #'paxedit-mode)
          (add-hook 'emacs-lisp-mode-hook #'paxedit-mode)
          (add-hook 'clojure-mode-hook #'paxedit-mode)))

(req-package persp-projectile
  :require (perspective projectile))

(req-package perspective
  :requires desktop
  :commands persp-mode
  :init ;; (add-hook 'desktop-after-read-hook
  ;;           (lambda () (persp-mode 1)))
  )

;; (req-package persp-mode
;;   :commands persp-mode
;;   :diminish persp-mode
;;   :init (progn
;;           (setq wg-morph-on nil) ;; switch off animation
;;           (setq persp-autokill-buffer-on-remove 'kill-weak)
;;           (add-hook 'after-init-hook (lambda ()
;;                                        (persp-mode 1)))))

;; (req-package persp-mode-projectile-bridge
;;   :require (persp-mode projectile)
;;   :commands persp-mode-projectile-bridge-mode
;;   :init (progn
;;           (add-hook 'after-init-hook
;;                     (lambda ()
;;                       (persp-mode-projectile-bridge-mode 1)))
;;           (add-hook 'persp-mode-projectile-bridge-mode-hook
;;                     (lambda ()
;;                       (if persp-mode-projectile-bridge-mode
;;                           (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
;;                         (persp-mode-projectile-bridge-kill-perspectives))))))

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
  :init (add-hook 'php-mode-hook #'php-boris-minor-mode))
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

(req-package plantuml-mode
  :mode "\\.plu\\'"
  :require graphviz-dot-mode            ; 需要 GRAPHVIZ_DOT 环境变量
  :init (progn
          ;; 加载前必须先定义 plantuml-plantuml-jar-path，如果找不到则出错。
          (setq plantuml-jar-path
                (locate-file "plantuml.jar" exec-path))))

(req-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :config (smex-initialize))

;; smooth-scrolling.el --- Make emacs scroll smoothly
(req-package smooth-scrolling
  :config (setq smooth-scroll-margin 4))

(req-package sotlisp)

(req-package speed-type)

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
  :init (add-hook 'html-mode-hook #'tagedit-mode)
  :config (tagedit-add-paredit-like-keybindings))

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
  :init (add-hook 'desktop-after-read-hook ; so we have all buffers again
                  (lambda ()
                    (wconf-load)
                    (wconf-switch-to-config 0)
                    (add-hook 'kill-emacs-hook
                              (lambda ()
                                (wconf-store-all)
                                (wconf-save))))))

(req-package web-mode
  :mode ("\\.html?\\'" "\\.tpl\\'" "\\.tpla\\'" "\\.phtml\\'"
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
  :init (progn
          (add-hook 'shell-mode-hook  #'with-editor-export-editor)
          (add-hook 'term-mode-hook   #'with-editor-export-editor)
          (add-hook 'eshell-mode-hook #'with-editor-export-editor))
  :config (shell-command-with-editor-mode 1))

;; ws-butler.el --- Unobtrusively remove trailing whitespace.
(req-package ws-butler
  :commands ws-butler-mode
  :diminish ws-butler-mode
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))
;; #+end_src

;; *** 放弃的扩展
;; #+BEGIN_SRC emacs-lisp

;;; 已经不维护
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

;; (req-package guile-scheme
;;   :require scheme)

;; (req-package outlined-elisp-mode
;;   :init (add-hook 'emacs-lisp-mode-hook #'outlined-elisp-find-file-hook))

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
;;   :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))
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
;;   :init (progn
;;           (remove-hook 'after-init-hook #'display-battery-mode)
;;           (add-hook 'after-init-hook #'fancy-battery-mode)))

;; (req-package-force load-dir
;;   :init
;;   (setq load-dir-ignore-errors t
;;         load-dir-recursive     t
;;         load-dirs              t))

;; (req-package outorg)
;; (req-package outshine)
;; (req-package navi-mode)

;; (req-package minimap)


;; (req-package rw-language-and-country-codes)
;; (req-package rw-ispell
;;   :config (rw-ispell-set-up-pdicts))
;; (req-package rw-hunspell
;;   :config (rw-hunspell-setup))

;; session.el --- use variables, registers and buffer places across sessions
;; (req-package session
;;   :disabled t
;;   :init (add-hook 'after-init-hook #'session-initialize)
;;   :config (add-to-list 'session-globals-exclude 'org-mark-ring))

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
;;   :init (progn
;;           ;; Make windmove work in org-mode
;;           (add-hook 'org-shiftup-final-hook #'windmove-up)
;;           (add-hook 'org-shiftleft-final-hook #'windmove-left)
;;           (add-hook 'org-shiftdown-final-hook #'windmove-down)
;;           (add-hook 'org-shiftright-final-hook #'windmove-right)))

;; (req-package yasnippet
;;   :diminish yas-minor-mode
;;   :config
;;   (yas-global-mode 1)
;;   (req-package java-snippets)
;;   (req-package php-auto-yasnippets)
;;   (req-package vala-snippets))

;; (unless (package-installed-p 'r5rs)
;;   (package-install 'r5rs))
;; #+END_SRC

;; ** 手工包
;; 手工安装的扩展。
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

;; ** 完成配置
;; #+BEGIN_SRC emacs-lisp
(req-package-finish)
(when (file-exists-p custom-file)
  (load custom-file))
;; #+END_SRC

;;; 文件结束:
;; #+BEGIN_SRC emacs-lisp
;; Local Variables:
;; lentic-init: lentic-orgel-org-init
;; End:
;; #+END_SRC
