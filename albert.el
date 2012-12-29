
(setq load-path (cons  "~/.emacs.d/" load-path))
(setq load-path (cons  "~/.emacs.d/albert" load-path))
(setq load-path (cons  "~/.emacs.d/albert/auto-complete" load-path))

; controls whether we get backtraces on lisp errors.
; useful for tracing plugin bugs. super annoying when not doing so
;(setq debug-on-error t) 
(setq debug-on-error t) 


;; expose system paths to emacs
(push (concat (getenv "HOME") "/bin") exec-path)
(push "/usr/local/bin" exec-path)


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

(server-start)

;; Misc utility functions
(load "albert-elisp.el")

;; [M-S-t] to jump to a function
;; [M-C-t] to list functions
(load "codenav.el")

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq visible-bell nil)

(setq ansi-color-for-comint-mode t)

(setq process-connection-type t)




(when window-system
  ;;; cmd key for meta
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")
  ;;; In Emacs 23 (Cocoa) in Snow Leopard, Apple delete key deletes backward, not
  ;;; forward as is usual. This fixes this behaviour.
  (normal-erase-is-backspace-mode 1)
  ;;; Set cmd-H to hide Emacs and cmd-shift-h to hide others, as usual in Mac OS
  ;;; X. Usually bound to mark-paragraph
  (global-set-key "\M-h" 'ns-do-hide-emacs)
  (global-set-key "\M-H" 'ns-do-hide-others)

  (set-frame-parameter nil 'fullscreen 'fullboth)
  (global-unset-key (kbd "M-RET"))
  (global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
  
  (defun set-frame-size-according-to-resolution ()
    (interactive)
    (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1200)
            (add-to-list 'default-frame-alist (cons 'width 165))
            (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     ;(cons 'height (/ (- (x-display-pixel-height) 200) (frame-char-height))))
                     (cons 'height 50))
        )))
  (set-frame-size-according-to-resolution)
  
  ; jerk/spawn to initial offset (left-corner)
  (setq initial-frame-alist '((top . 100) (left . 300)))
  (modify-frame-parameters nil '((wait-for-wm . nil)))
  


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

  
  (global-set-key [f12]              'delete-other-windows)
  (global-set-key [C-f12]            'split-window-horizontally)
  (global-set-key [M-f12]            'split-window-vertically)
  (global-set-key [C-M-f12]          'window-swap-orientation)
  (global-set-key "\C-x4"            'window-swap-orientation)
  (global-set-key [C-f4]             'my-bury-window) ; also C-x0
  )




(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)


;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
;(setq show-paren-style 'parenthesis)
(setq show-paren-style 'mixed)



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

(global-set-key [f2] 'projectile-jump-to-project-file)
(global-set-key (kbd "C-c C-b") 'projectile-jump-to-project-file)
(global-set-key (kbd "C-c b") 'projectile-jump-to-project-file)


(require 'iedit)

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
; ??
(global-set-key [f11] 'session-toggle-permanent-flag)


(autoload 'tramp "Tramp")
(setq tramp-default-method "sshx")


(defun toggle-color-theme ()
  "Switch to/from night color scheme."
  (interactive)
  ;; create the snapshot if necessary
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (load-theme 'zen-and-art)
    (load-theme 'zenburn)
    )
  )

(require 'color-theme)
;(color-theme-charcoal-black)
(color-theme-zenburn)

;(global-set-key (kbd "<f9>") 'toggle-color-theme)


;; Utilities that increase legibility and reduce code duplication
(defun current-file-remotep ()
  "Tell if the file is remote"
  (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))

(defun disable-flymake-on-remote ()
  )
(add-hook 'coding-hook 'disable-flymake-on-remote)


;;============================================================
;; cscope configuation
;;============================================================
(require 'xcscope)

(defun cscope-done ()
  (interactive)
  (cscope-select-entry-one-window)
  (cscope-quit)
  (save-current-buffer
    (let ((buf (switch-to-buffer "*cscope*")))
      (if buf (kill-buffer))))
  t)

(if cscope-list-entry-keymap
    (define-key cscope-list-entry-keymap "q" 'cscope-done))

(defun cscope-find-egrep-pattern-no-prompt ()
  (interactive)
  (cscope-find-egrep-pattern (thing-at-point 'sexp)))

(global-set-key [C-f3]             'cscope-find-global-definition-no-prompting)
(global-set-key [f3]               'cscope-find-egrep-pattern)
(global-set-key [C-M-f3]           'cscope-find-egrep-pattern-no-prompt)


;;============================================================
;; iswitch configuation
;;============================================================
;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(require 'iswitchb)
;(require 'iswitchb-highlight)
(iswitchb-mode 't)

(add-hook 'iswitchb-minibuffer-setup-hook
          '(lambda () (set (make-local-variable 'max-mini-window-height) 3)))

(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("\C-f" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("\C-b"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "*Buffer")
(add-to-list 'iswitchb-buffer-ignore "*Pymacs")
;(add-to-list 'iswitchb-buffer-ignore "^[tT][aA][gG][sS]$")


;;==================================================
;; imenu
;;==================================================
(require 'imenu)

;; navigate through symbols
(global-set-key "\C-\M-t"     'my-imenu-jump-to-function)
(global-set-key "\M-\S-t"     'imenu-make-selection-buffer)


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

(add-hook 'tramp-mode
          '(progn
                                        ; disable flymake for tramp files
             (when (tramp-file-name-p current-file-name)
               (flymake-mode-off))))
(global-set-key [f10] 'flymake-display-err-menu-for-current-line)
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




;;==================================================
;; javascript
;;==================================================
;; js-mode vs js2-mode selected as default mode in both:
;;    - starter-kit-js
;;    - starter-kit-misc
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-hook 'js2-mode-hook 'moz-minor-mode)
(add-hook 'js2-mode-hook 'run-coding-hook)
(add-hook 'js2-mode-hook '(lambda () (setq tab-width 2)))

;; some sane js2 customize defaults
;  '(js2-bounce-indent-p t)
;  '(js2-indent-on-enter-key t)
;  '(js2-strict-missing-semi-warning nil)
;  '(tab-width 2) ; no idea where js2-mode is pulling default of 8 from, so sledgehammer it



;; lint
(defun jslint-thisfile ()
  (interactive)
  (compile (format "jsl -conf /etc/jsl.conf -process %s" (buffer-file-name))))
(add-hook 'javascript-mode-hook '(lambda () (local-set-key [f8] 'jslint-thisfile)))
(add-hook 'js2-mode-hook '(lambda () (local-set-key [f8] 'jslint-thisfile)))
(add-hook 'espresso-mode-hook '(lambda () (local-set-key [f8] 'jslint-thisfile)))


;; js2 includes realtime inline warnings
;(load "flymake-jslint.el")
;(require 'flymake-js)
;(add-hook 'js-mode-hook 'flymake-jslint-load)


;; slime awesomeness
;; invoke with
;;    M-x slime-js-run-swank
;;    M-x slime-js-jack-in-node
;;    M-x slime-js-jack-in-browser

;; for swank-js
(setq load-path (cons  "~/.emacs.d/albert/magnars/" load-path))
(setq load-path (cons  "~/.emacs.d/albert/magnars/mark-multiple" load-path))
(setq load-path (cons  "~/.emacs.d/albert/magnars/js2-refactor" load-path))
(add-hook 'js2-mode-hook
          (lambda ()
            (progn
              (when (locate-library "slime-js")
                (require 'setup-slime-js)))))






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
(require 'rdebug)
(setq rdebug-many-windows nil)
;(setq rdebug-debug-active t)
(setq rdebug-debug-active nil)

(rvm-use "ruby-1.9.2-p290" "global")
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


(load "rdebug-dirmap.el")
(load "rdebug-monkeypatches.el")



(defun rdebug-start-debugging (&optional host port filename)
  (interactive)
  (let* ((visiting (or filename (buffer-file-name (current-buffer))))
         (filename (concat "./" (file-name-nondirectory visiting)))
         (hostname (or host (read-minibuffer "hostname: " "127.0.0.1")))
         (portno (or port (read-minibuffer "port: " "8989"))))
    (add-minor-mode 'rdebug-on 'rdebug-debugger-support-minor-mode)
    (rdebug (format "rdebug --emacs 3 --annotate 3 -c  -h %s -p %s --script %s" hostname portno visiting))))

;(add-hook 'rdebug-debugger-support-minor-mode
;          '(lambda()
;             (define-key rdebug-debugger-support-minor-mode-map-when-active [f8] 'rdebug-continue)
;             (define-key rdebug-debugger-support-minor-mode-map-when-active [f9] 'rdebug-toggle-source-breakpoint)
;             (define-key rdebug-debugger-support-minor-mode-map-when-active (kbd "\C-x\C-e") 'rdebug-eval-thing-at-point)
;             ))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map [C-f5]     'rdebug-start-debugging)
     (define-key ruby-mode-map (kbd "C-j")   'newline-and-indent)
     (setq tab-width 2)
     (setq indent-tabs t)
     (local-unset-key "\C-c\C-s")))


; remove inferrior ruby process, you never use it
(remove-hook 'ruby-mode-map 'inf-ruby-keys)
;(add-hook 'ruby-mode 'my-ruby-hook)



;;==================================================
;; PYTHON
;;==================================================
(setq py-install-directory "~/.emacs.d/python-mode.el-0.6.12")
(setq load-path (cons py-install-directory load-path))
(require 'python-mode)
(setq py-shell-name "ipython")

(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs t)
                                        ; default to 4 spaces, but guess based on buffer
            (setq tab-width 4)
            (setq python-indent 4)
            (py-guess-indent-offset)
            (define-key py-mode-map (kbd "RET") 'newline-and-indent)

            ))
(require 'ipython)


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
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-index-file "index.org")
  (setq org-mobile-inbox-for-pull "~/org/refile.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/org")
                                        ;(setq org-mobile-directory "~/Dropbox/MobileOrg")

  (setq org-refile-targets '((nil :maxlevel . 2)
                                        ; all top-level headlines in the
                                        ; current buffer are used (first) as a
                                        ; refile target
                             (org-agenda-files :maxlevel . 2)))

  (define-key org-mode-map "\M-r" 'org-table-recalculate-buffer-tables))

(add-hook 'org-mode-hook 'my-org-mode)



(global-set-key [f1] 'org-agenda)
;(global-set-key [S-f1] 'org-clock-goto)
(global-set-key [M-f1] 'org-capture)
(global-unset-key "\M-U")
(global-set-key "\M-U" 'org-open-at-point)


;;==================================================
;; coffee
;;==================================================
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))



;;==================================================
;; indent on paste
;;==================================================

;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes '(python-mode ruby-mode LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped). Only
modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))


(defun my-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun my-shift-region-left (start end &optional count)
  "stolen form py-shift-region-left"
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))

  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (my-shift-region start end (- (prefix-numeric-value (or count 2))))
  t
)

(defun my-shift-region-right (start end &optional count)
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (my-shift-region start end (prefix-numeric-value (or count 2)))
  t
)

(global-set-key "\M-]" 'my-shift-region-right)
(global-set-key "\M-[" 'my-shift-region-left)

;;==================================================



(load "keybindings.el")

(load "experimental.el")






