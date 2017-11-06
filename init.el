(server-start)

;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Custom functions
(load (concat dotfiles-dir "funcs.el"))

(defun undef (sym) (makunbound sym))

;; Load up ELPA, the package manager
(add-to-list 'load-path (concat dotfiles-dir "/elpa"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/site-lisp"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))


(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.milkbox.net/packages/")))
(package-initialize)


;;==================================================
;; starter kit defaults
;;==================================================

(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)


;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
;(setq show-paren-style 'parenthesis)
(setq show-paren-style 'mixed)

; yes!
(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'auto-revert-tail-mode 'tail-mode)

;; Don't clutter up directories with files~

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)


(setq
 ansi-color-for-comint-mode t
 backup-directory-alist `(("." . ,(expand-file-name (concat dotfiles-dir "backups"))))
 color-theme-is-global t
 diff-switches "-u -w"
 echo-keystrokes 0.1
 ediff-window-setup-function 'ediff-setup-windows-plain
 ffap-machine-p-known 'reject
 font-lock-maximum-decoration t
 fringe-mode (cons 4 0)
 inhibit-startup-message t
 mouse-yank-at-point t
 oddmuse-directory (concat dotfiles-dir "oddmuse")
 process-connection-type t
 require-final-newline nil
 save-place-file (concat dotfiles-dir "places")
 shift-select-mode nil
 tramp-default-method "sshx"
 transient-mark-mode t
 truncate-partial-width-windows nil
 uniquify-buffer-name-style 'post-forward
 visible-bell nil
 visible-bell t
 whitespace-line-column 100
 whitespace-style '(trailing lines space-before-tab face indentation space-after-tab)
 xterm-mouse-mode t
 interprogram-paste-function 'x-cut-buffer-or-selection-value
 x-select-enable-clipboard t ; use clipboard
)

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))



(when window-system
  (defun set-frame-size-according-to-resolution ()
    (interactive)
    (if (memq window-system '(ns mac))
      (progn
        ;;; cmd key for meta
        (setq mac-option-key-is-meta nil)
        (setq mac-command-key-is-meta t)
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'none)
        (set-default-font "-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
        ;;; unretard OSX repeatchar
        (if (boundp 'ns-set-resource)
            (ns-set-resource nil "ApplePressAndHoldEnabled" "NO"))
        ;;; Set cmd-H to hide Emacs and cmd-shift-h to hide others, as usual in Mac OS
        ;;; X. Usually bound to mark-paragraph
        (global-set-key "\M-h" 'ns-do-hide-emacs)
        (global-set-key "\M-H" 'ns-do-hide-others)

        ;; Meta-Return Fullscreen
        (global-unset-key (kbd "M-RET"))
        (if (boundp 'ns-toggle-fullscreen)
            (global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
            (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen))

        ;;; In Emacs 23 (Cocoa) in Snow Leopard, Apple delete key deletes backward, not
        ;;; forward as is usual. This fixes this behaviour.
        (normal-erase-is-backspace-mode 1)

        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1200)
            (add-to-list 'default-frame-alist (cons 'width 160))
            (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        ;(add-to-list 'default-frame-alist
        ;             ;(cons 'height (/ (- (x-display-pixel-height) 200) (frame-char-height))))
        ;             (cons 'height 50))
        ; jerk/spawn to initial offset (left-corner)
        ;(setq initial-frame-alist '((top . 100) (left . 100)))
        (modify-frame-parameters nil '((wait-for-wm . nil)))
        )))
  (set-frame-size-according-to-resolution)

  (let* ((fonts '("-monotype-Andale Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                  "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                  "Ubuntu Mono:pixelsize=20:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")))
    (set-default-font (nth 2 fonts)))

  (if (> (x-display-pixel-width) 1200)
      (add-to-list 'default-frame-alist (cons 'width 160))
    (add-to-list 'default-frame-alist (cons 'width 80)))

  ;; apple-style increase-fontsize
  (defun bump-up() (interactive) (text-scale-increase 1))
  (defun bump-down() (interactive) (text-scale-decrease 1))
  (global-unset-key "\M-=")
  (global-set-key "\M-=" 'bump-up)
  (global-set-key "\M--" 'bump-down)

  ;;============================================================
  ;; window/frame control
  ;;============================================================
  ;; frames
  (defun frame-small ()
    (interactive)
    (set-frame-size (selected-frame) 80 40))
  (defun frame-double ()
    (interactive)
    (set-frame-size (selected-frame) 165 60))

  (global-set-key [C-f1] 'frame-small)
  (global-set-key [C-f2] 'frame-double)
  ; apple-` to cycle frames
  (global-unset-key "\M-`")
  (global-set-key "\M-`" 'other-frame)
  (global-set-key (kbd "<M-f2>") 'other-frame)
  (global-set-key (kbd "<M-f3>") 'make-frame-command)
  (global-set-key (kbd "<M-f4>") 'delete-frame)

  ;; windows
  (defun window-swap-pair-vertical (win right vertical)
    (interactive)
    (let ((buff (window-buffer right)))
      (delete-window right)
      (if (eq vertical t)
          (split-window-vertically)
        (split-window-horizontally))
      (other-window 1)
      (switch-to-buffer buff)))

  (defun window-swap-orientation ()
    (interactive)
    ; nuke myself
    (walk-windows
     (lambda (me)
       ; check i'm still "live" - i have a buffer
       (if (window-buffer me)
           (let ((right (window-in-direction 'right me))
                 (left (window-in-direction 'left me))
                 (above (window-in-direction 'above me))
                 (below (window-in-direction 'below me))
                 (next (window-right me)))
             (if (and (> (count-windows) 1)
                      (not (eq next nil)))
                 (cond
                  ((eq next right) (window-swap-pair-vertical me next t))
                  ((eq next below) (window-swap-pair-vertical me next nil)))))))
     nil nil))

  (defun my-bury-window ()
    (interactive)
    (delete-window (get-buffer-window)))

)

(global-set-key [f12]              'delete-other-windows)
(global-set-key [C-f12]            'split-window-horizontally)
(global-set-key [M-f12]            'split-window-vertically)
(global-set-key [C-M-f12]          'window-swap-orientation)
(global-set-key "\C-x4"            'window-swap-orientation)
(global-set-key [C-f4]             'my-bury-window) ; also C-x0


;; ========================================
;; ========================================
;; ========================================
;; ========================================
;; ========================================



(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tool-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq load-path (cons  "~/.emacs.d/extensions" load-path))

(load-theme 'zenburn t)

; controls whether we get backtraces on lisp errors.
; useful for tracing plugin bugs. super annoying when not doing so
;(setq debug-on-error t)

;; expose system paths to emacs
(push (concat (getenv "HOME") "/bin") exec-path)
(push (concat (getenv "PATH") ":/usr/local/bin") exec-path)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(push "/usr/local/bin" exec-path)
(push "/usr/local/go/bin" exec-path)


;; makes a brand new *scratch* buffer if not present, switch to it
(global-set-key (kbd "C-c C-s") 'go-go-gadget-scratch-buffer)
(defun go-go-gadget-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; Save your minibuffer history
(setq savehist-file (expand-file-name "~/.emacs.d/history"))
(savehist-mode 1)

(define-key global-map (kbd "RET") 'newline-and-indent)

;;==================================================
;; dired
;;==================================================
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; ediff - don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; Kill default buffer without the extra emacs questions
(defun kill-buffer-quiet() (interactive) (kill-buffer (buffer-name)))

(global-set-key [f4]               'kill-buffer-quiet)


;;==================================================
;; starter-kit tweaks
;;==================================================
(remove-hook 'coding-hook 'turn-on-auto-fill)
(remove-hook 'coding-hook 'local-comment-auto-fill)
(remove-hook 'coding-hook 'pretty-lambdas)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'ipython-shell-hook 'ansi-color-for-comint-mode-on)

;;==================================================
;; general plugins
;;==================================================
(require 'projectile)
(projectile-global-mode t)

(global-set-key [f2] 'projectile-find-file)
(global-set-key (kbd "C-c C-b") 'projectile-find-file)
(global-set-key (kbd "C-c C-d") 'projectile-find-dir)


(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(global-set-key [f11] 'session-toggle-permanent-flag)

(autoload 'tramp "Tramp")


(require 'color-theme)
(defun toggle-color-theme ()
  "Switch to/from night color scheme."
  (interactive)
  ;; create the snapshot if necessary
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (load-theme 'zen-and-art)
    (load-theme 'zenburn t)
    )
  )

(global-set-key (kbd "<f9>") 'toggle-color-theme)


;; Utilities that increase legibility and reduce code duplication
(defun current-file-remotep ()
  "Tell if the file is remote"
  (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))

(defun disable-flymake-on-remote ())
(add-hook 'coding-hook 'disable-flymake-on-remote)


;;============================================================
;; cscope configuation
;;============================================================
;;(require 'xcscope)
;; (defun cscope-done ()
;;   (interactive)
;;   (cscope-select-entry-one-window)
;;   (cscope-quit)
;;   (save-current-buffer
;;     (let ((buf (switch-to-buffer "*cscope*")))
;;       (if buf (kill-buffer))))
;;   t)

;; (if cscope-list-entry-keymap
;;     (define-key cscope-list-entry-keymap "q" 'cscope-done))

;; (defun cscope-find-egrep-pattern-no-prompt ()
;;   (interactive)
;;   (cscope-find-egrep-pattern (thing-at-point 'sexp)))

;; (global-set-key [C-f3]             'cscope-find-global-definition-no-prompting)

;; (global-set-key [f3]               'cscope-find-egrep-pattern)
;; (global-set-key [C-M-f3]           'cscope-find-egrep-pattern-no-prompt)


;;============================================================
;; iswitch configuation
;;============================================================
;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; (require 'iswitchb)
;; (iswitchb-mode 't)

;; (add-hook 'iswitchb-minibuffer-setup-hook
;;           '(lambda () (set (make-local-variable 'max-mini-window-height) 3)))

;; (defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
;;   "*Regenerate the list of matching buffer names after a kill.
;;     Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
;;     set to non-nil."
;;   (setq iswitchb-buflist iswitchb-matches)
;;   (iswitchb-rescan))

;; (defun iswitchb-rescan ()
;;   "*Regenerate the list of matching buffer names."
;;   (interactive)
;;   (iswitchb-make-buflist iswitchb-default)
;;   (setq iswitchb-rescan t))

;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K)
;;           (let* ((key (car K)) (fun (cdr K)))
;;             (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;;         '(("<right>" . iswitchb-next-match)
;;           ("\C-f" . iswitchb-next-match)
;;           ("<left>"  . iswitchb-prev-match)
;;           ("\C-b"  . iswitchb-prev-match)
;;           ("<up>"    . ignore             )
;;           ("<down>"  . ignore             ))))

;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; (add-to-list 'iswitchb-buffer-ignore "*Completions")
;; (add-to-list 'iswitchb-buffer-ignore "*Buffer")
;; (add-to-list 'iswitchb-buffer-ignore "*Pymacs")


(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key "\C-c\\" 'toggle-comment-on-line)
(global-set-key "\C-c\C-\\" 'comment-or-uncomment-region)

;;==================================================
;; imenu
;;==================================================
;; (require 'imenu)
;; (imenu-add-menubar-index)
;; ;; navigate through symbols
;; (global-set-key "\C-\M-t"     'my-imenu-jump-to-function)
;; (global-set-key "\M-\S-t"     'imenu-make-selection-buffer)


;;============================================================
;; expansion and completion
;;============================================================
(require 'auto-complete)
(global-auto-complete-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-expand-line-all-buffers ; slow
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'hippie-expand)


;;============================================================
;; flymake
;;============================================================
(require 'flymake)
(setq flymake-run-in-place nil)
; disable flymake for tramp files
(add-hook 'tramp-mode
          '(progn
             (when (tramp-file-name-p current-file-name)
               (flymake-mode-off))))
(global-set-key [f10] 'flymake-display-err-menu-for-current-line)



;;============================================================
;; browser
;;============================================================
(global-set-key (kbd "<M-f10>") 'browse-url)


;;============================================================
;; MAJOR/MINOR MODES
;;============================================================


;;============================================================
;; magit
;;============================================================
(require 'magit)
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))

(global-set-key [f5]               'magit-status)
(global-set-key [M-f5]             'magit-status)
(global-set-key [f6]               'vc-diff)
(global-set-key [M-f6]             'vc-revert-buffer)
(global-set-key [C-f6]             'vc-annotate)




;;==================================================
;; emacs-lisp
;;==================================================
(define-key emacs-lisp-mode-map "\C-cd" 'edebug-defun)

;; debugging via edebug
(add-hook 'edebug-mode-hook
          (progn
            ; by default, edebug will only pause for a second on breakpoints.
            ; rendering continue pretty much useless
            (setq edebug-sit-for-seconds 3600)))


(setq electric-indent-mode nil)
(defun funky-indent-relative (&optional arg)
  "Newline and indent 4 spaces relative to previous line.  With C-u, indent to same level as previous line."
  (interactive "P")
  (let* ((amount (if arg 0 4))
         (indent (+ amount (save-excursion
                             (back-to-indentation)
                             (current-column)))))
    (newline 1)
    (insert (make-string indent ?\s))))

;(global-key (kbd "C-<return>") #'funky-indent-relative)

(require 'web-mode)

(defun my-tab ()
  (interactive)
  (indent-relative))
(defun my-newline-and-indent ()
  (interactive)
  (newline)
  (indent-relative))
(add-hook 'web-mode-hook (progn
                           (define-key web-mode-map (kbd "RET") 'newline)
                           (define-key web-mode-map (kbd "TAB") 'indent-relative-maybe)
                           ))



;;==================================================
;; javascript
;;==================================================
;; js-mode vs js2-mode selected as default mode in both:
;;    - starter-kit-js
;;    - starter-kit-misc
(setq js2-mirror-mode nil)
;(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))

(add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;(add-hook 'js2-mode-hook 'moz-minor-mode)
;(add-hook 'js2-mode-hook 'run-coding-hook)




(defun custom-js2-config ()
  (progn
    (setq js2-mirror-mode nil)
    (setq tab-width 4)
    (setq js2-basic-offset 4)
    (setq js2-indent-on-enter-key nil)
    (setq js2-electric-keys '())
    (setq js2-bounce-indent-p nil)
    (setq js2-strict-missing-semi-warning nil)
    (setq js2-auto-indent-p nil)))


(add-hook 'js2-mode-hook 'custom-js2-config)

;; lint
(defun jslint-thisfile ()
  (interactive)
  (compile (format "/usr/local/bin/jsl -conf /etc/jsl.conf -process %s" (buffer-file-name))))
(add-hook 'javascript-mode-hook '(lambda () (local-set-key [f8] 'jslint-thisfile)))
(add-hook 'js2-mode-hook '(lambda () (local-set-key [f8] 'jslint-thisfile)))
(add-hook 'espresso-mode-hook '(lambda () (local-set-key [f8] 'jslint-thisfile)))


;;==================================================
;; ruby
;;==================================================

;(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;(autoload 'inf-ruby-keys "inf-ruby" "" nil)

(require 'ruby-mode)
(font-lock-add-keywords 'ruby
                        '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                          ("\\<\\(WARNING\\):" 1 font-lock-warning-face t)
                          ("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
                          ("\\<\\(IMPORTANT\\):" 1 font-lock-warning-face t)
                          ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                          ("\\<\\(TBC\\)" 1 font-lock-warning-face t)
                          ("\\<\\(TBD\\)" 1 font-lock-warning-face t))
                        )


(setq load-path (cons  "~/.emacs.d/albert/rdebug" load-path))
(require 'gud)
;(require 'gdb-ui)
;(require 'rdebug)
(setq rdebug-many-windows nil)
;(setq rdebug-debug-active t)
(setq rdebug-debug-active nil)

;(require 'rvm)
;(rvm-use "ruby-1.9.2-p290" "global")
;(rvm-use "ruby-1.9.2-p290" "bootstrap")

(defun rdebug-eval-thing-at-point ()
  (interactive)
  (rdebug-print-cmd (thing-at-point 'sexp)))

(defun my-rdebug-keys (map)
  (define-key map [f8]    'gud-cont)
  (define-key map [S-f5]  'rdebug-quit)
  (define-key map [f9]    'rdebug-toggle-source-breakpoint)
  (define-key map [C-f9]  'rdebug-toggle-source-breakpoint-enabled)
  (define-key map [f10]   'rdebug-next)
  (define-key map [f11]   'rdebug-step)
  (define-key map (kbd "\C-x\C-e") 'rdebug-eval-thing-at-point)
  (define-key map [S-f11] 'gud-finish))

(setq rdebug-populate-common-keys-function 'my-rdebug-keys)


(defun rdebug-start-debugging (&optional host port filename)
  (interactive)
  (let* ((visiting (or filename (buffer-file-name (current-buffer))))
         (filename (concat "./" (file-name-nondirectory visiting)))
         (hostname (or host (read-minibuffer "hostname: " "127.0.0.1")))
         (portno (or port (read-minibuffer "port: " "8989"))))
    (add-minor-mode 'rdebug-on 'rdebug-debugger-support-minor-mode)
    (rdebug (format "rdebug --emacs 3 --annotate 3 -c  -h %s -p %s --script %s" hostname portno visiting))))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map [C-f5]     'rdebug-start-debugging)
     (define-key ruby-mode-map (kbd "C-j")   'newline-and-indent)
     (setq tab-width 2)
     (setq indent-tabs t)
     (local-unset-key "\C-c\C-s")))


; remove inferrior ruby process, you never use it
(remove-hook 'ruby-mode-map 'inf-ruby-keys)


;;==================================================
;; PYTHON
;;==================================================
;(require 'python-mode)
(setq py-shell-name "ipython")
(setq py-python-command-args '("-i"))

(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)

; default to 4 spaces, but guess based on buffer
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs t)
            (setq tab-width 4)
            (setq python-indent 4)
            (autoload 'python-pep8 "pep8" )
            (define-key python-mode-map (kbd "RET") 'newline-and-indent)
            (define-key python-mode-map [f8] 'pep8)
            (define-key python-mode-map [M-f7] 'gud-step)
            (define-key python-mode-map [M-f8] 'gud-next)
            (define-key python-mode-map [M-f9] 'gud-cont)
            (define-key python-mode-map [M-f5] 'gud-stop)
            (font-lock-add-keywords 'python
                                    '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                                      ("\\<\\(WARNING\\):" 1 font-lock-warning-face t)
                                      ("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
                                      ("\\<\\(IMPORTANT\\):" 1 font-lock-warning-face t)
                                      ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                                      ("\\<\\(TBC\\)" 1 font-lock-warning-face t)
                                      ("\\<\\(TBD\\)" 1 font-lock-warning-face t))
                                    )
            ))
;(require 'ipython)

(add-hook 'python-mode-hook 'auto-complete-mode)


;;==================================================
;; org
;;==================================================
(require 'org)

(global-unset-key "\C-\M-o")
(global-set-key "\C-\M-o" 'org-insert-link-global)
(global-set-key "\C-c o" 'org-open-at-point-global)

(defun my-org-mode ()
  (require 'org-mobile)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org"))
  (setq org-refile-use-outline-path "~/org")
  (setq org-agenda-filter nil)
  (setq org-mobile-directory "~/org")
  ;; all top-level headlines in the current buffer are used (first) as a refile target
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))
  (define-key org-mode-map "\M-r" 'org-table-recalculate-buffer-tables)
  (define-key org-mode-map "\C-c\C-d" 'org-archive-subtree)
)

(add-hook 'org-mode-hook 'my-org-mode)
;(global-set-key [f1] 'org-agenda)
;(global-unset-key [f1])
;(global-set-key [S-f1] 'org-clock-goto)
;(global-set-key [M-f1] 'org-capture)


;;==================================================
;; csv
;;==================================================
(require 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.tsv$" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))

;;==================================================
;; go
;;==================================================
(require 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

(defun go-go-mode-hook ()
  (setq tab-width 8)
  (setq indent-tabs nil)

  ; Use goimports instead of go-fmt
  ;(setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (define-key go-mode-map [M-f1] 'godef-jump-other-window)
  (local-set-key [M-f1] 'godef-jump-other-window)

  (add-to-list 'load-path (expand-file-name "~/go/src/github.com/dougm/goflymake"))

  (require 'go-flympake)
  (setq flymake-run-in-place nil)

  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (ac-config-default)
  (setenv "GOPATH" (expand-file-name "~/go"))
  )

(add-hook 'go-mode-hook 'go-go-mode-hook)
;(load (expand-file-name "~/go/src/github.com/golang/tools/cmd/oracle/oracle.el"))
;(add-hook 'go-mode-hook 'go-oracle-mode)

;; C-c C-o <       go-oracle-callers
;; C-c C-o >       go-oracle-callees
;; C-c C-o c       go-oracle-peers
;; C-c C-o d       go-oracle-definition
;; C-c C-o f       go-oracle-freevars
;; C-c C-o g       go-oracle-callgraph
;; C-c C-o i       go-oracle-implements
;; C-c C-o p       go-oracle-pointsto
;; C-c C-o r       go-oracle-referrers
;; C-c C-o s       go-oracle-callstack
;; C-c C-o t       go-oracle-describe


;;==================================================
;; markdown
;;==================================================
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(defun open-with-shiba ()
  "open a current markdown file with shiba"
  (interactive)
  (start-process "shiba" "*shiba*" "shiba" "--detach" buffer-file-name))
(define-key markdown-mode-map (kbd "C-c C-c") 'open-with-shiba)



;;==================================================
;;   GLOBAL SHORTCUTS / KEYBINDINGS
;;   alt keycode (kbd "C-c k")
;;
;; \242 :: f12
;; \275 :: "M-="
;; ^C   :: "C-c"


;(global-unset-key "\C-x\C-c") ;; keep closing emacs?
(global-set-key "\C-cf"      'grep-find)
(global-unset-key "\C-o") ; keep other-buffer working in dired
(define-key dired-mode-map "\C-o" 'other-window)
(global-set-key "\C-o"       'other-window)
(global-set-key "\C-xg"       'goto-line)
(global-set-key "\C-x\C-w"    'what-line)
(global-set-key "\C-o"        'other-window)
(global-set-key "\C-z"        'term)
(global-set-key "\C-xt"       'delete-trailing-whitespace)
(global-set-key "\M-\S-c"     'comment-or-uncomment-region)
(global-set-key "\C-ck"       'browse-kill-ring) ; browse recent cuts
(global-unset-key "\M-k")
(global-set-key "\M-k"        'kill-buffer)
(global-set-key "\M-]" 'my-shift-region-right)
(global-set-key "\M-[" 'my-shift-region-left)

; vim style shift-j
(fset 'join-lines
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([14 134217848 106 111 105 110 45 108 105 110 101 return] 0 "%d")) arg)))
(global-set-key "\C-\M-j" 'join-lines)

;; common fatfingering, disable narrow-to-region
(global-unset-key "\C-xnn")
(global-unset-key "\C-xnp")
(global-unset-key "\C-xnd")

;(global-set-key (kbd "<M-f10>") 'browse-url)
(global-unset-key "\M-U")
(global-set-key "\M-U" 'browse-url)

(defun ac-gsearch ()
  "Do a Google search of the region or symbol at the point"
  (interactive)
  (let ((phrase (elt (region-or-thing 'symbol) 0)))
    (browse-url (concat "http://www.google.com/search?q="
                        (replace-regexp-in-string " " "+" phrase)))))
(global-set-key "\M-?" 'ac-gsearch)

(global-set-key [M-f7] 'repeat) ; repeat-last-command

(defun doc-last-command()
  (interactive)
  (describe-function last-command))
(global-set-key [C-f7] 'doc-last-command)

(global-unset-key "\M-t")
(global-set-key "\M-p" 'projectile-find-file)

;; Nice quick buffer switching
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;; (defun fancy-align-regexp()
;;   (interactive)
;;   ;(mark-paragraph)
;;   (align-region (region-beginning) (region-end) (read-from-minibuffer "align to: ") nil nil)
;;   )
(global-set-key "\C-c=" 'align-regexp)



; patched golang gud-based debugger
;(load (expand-file-name "~/.emacs.d/go-dlv/go-dlv.el"))


;; (defun on-save-hook()
;;   (when (eq major-mode 'org-mode)
;;     (progn ()))
;;     )

;; (remove-hook 'after-save-hook 'on-save-hook)
;; (add-hook 'after-save-hook 'on-save-hook)


(require 'google)

(require 'p4-google)                ;; g4-annotate, improves find-file-at-point
(require 'compilation-colorization) ;; colorizes output of (i)grep
(require 'rotate-clients)           ;; google-rotate-client
(require 'rotate-among-files)       ;; google-rotate-among-files
(require 'googlemenu)               ;; handy Google menu bar
(require 'p4-files)                 ;; transparent support for Perforce filesystem
(require 'google3-build)            ;; support for blaze builds
(require 'google3-mode)             ;; minor mode in google3
(require 'csearch)                  ;; Search the whole Google code base.
(require 'google3-display-coverage)

(global-set-key "\C-cl" #'google-lint)   ; bind lint to control-c + l

(grok-init)

;; ========================================
;; ========================================
;;
;; EXPERIMENTAL!!!! VOODOO!
;;
;; ========================================
;; ========================================

(require 'dumb-jump)
(global-set-key "\C-\M-g" 'dumb-jump-go)

(global-set-key [f3] 'grok-jump-to-definition-at-point)
