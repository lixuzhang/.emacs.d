;;; init.el --- Emacs 配置文件                        -*- lexical-binding: t; -*-

;;; Header:

;; Copyright (C) 2016, 2017, 2018, 2019, 2020, 2021 李旭章

;; Author: 李旭章 <17728928@qq.com>
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
;; *** C 内核
;; #+BEGIN_SRC emacs-lisp
;; emacs.c --- Fully extensible Emacs, running on Unix, intended for GNU.
(let ((minver "27"))
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
   "-*-Noto Sans Mono CJK SC-normal-normal-normal-*-14-*-*-*-*-0-fontset-standard"))
 
 ;; Mac OS X
 ((string-equal system-type "darwin")
  (create-fontset-from-fontset-spec
   "-outline-Hiragino Sans GB-normal-normal-normal-mono-16-*-*-*-c-*-fontset-standard")))

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
(defun top-level-add-to-load-path (topdir)
  (let ((default-directory topdir))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(top-level-add-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))


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

;;; saveplace.el --- automatically save place in files
(save-place-mode)

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
(add-hook 'before-save-hook #'copyright-update)

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
;; #+BEGIN_SRC emacs-lisp
(dolist (pa '(("gnu" . "http://elpa.gnu.org/packages/")
              ("org" ."http://orgmode.org/elpa/")
              ("melpa" . "http://melpa.org/packages/")
              ;; ("marmalade" . "https://marmalade-repo.org/packages/")
              ;; ("GNU ELPA" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
              ;; ("MELPA" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
              ;; ("MELPA Stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
              ;; ("Marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")
              ;; ("Org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
              ;; ("popkit" . "http://elpa.popkit.org/packages/")
              ))
  (add-to-list 'package-archives pa t))
(setq package-enable-at-startup nil)
;; 加载扩展之前，必须先初始化。
(package-initialize)

;; 增强包管理
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
;; (setq use-package-always-defer t)
(setq use-package-debug t)
(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
;; (use-package bind-key)
;; (use-package diminish)

;; (setq req-package-log-level 'error)
;; (require 'req-package)
(load (expand-file-name "init-internal.el" user-emacs-directory))
(load (expand-file-name "init-system.el" user-emacs-directory))
;; (load (expand-file-name "init-external.el" user-emacs-directory))
;; (req-package-finish)
;; #+END_SRC

;; ** 完成配置
;; #+BEGIN_SRC emacs-lisp
(when (file-exists-p custom-file)
  (load custom-file))
;; #+END_SRC

;;; 文件结束:
;; #+BEGIN_SRC emacs-lisp
;; Local Variables:
;; lentic-init: lentic-orgel-org-init
;; End:
;; #+END_SRC
