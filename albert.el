
(setq load-path (cons  "~/.emacs.d/" load-path))
(setq load-path (cons  "~/.emacs.d/albert" load-path))
(setq load-path (cons  "~/.emacs.d/albert/auto-complete" load-path))



;(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;; FIXES DROPBOX issue
;; Find the appropriate format for displaying uid, gid, and
;; file size, by finding the longest strings among all the
;; files we are about to display.
;;
;; (dolist (elt file-alist)
;;   (setq attr (cdr elt)
;;         fuid (or (nth 2 attr) "")
;;         uid-len (if (stringp fuid) (string-width fuid)
;;                   (length (format "%d" fuid)))
;;         fgid (or (nth 3 attr) "")
;;         gid-len (if (stringp fgid) (string-width fgid)
;;                   (length (format "%d" fgid)))
;;         file-size (or (nth 7 attr) 0))
;;   (if (> uid-len max-uid-len)))

(server-start)

;; Misc utility functions
(load "albert-elisp.el")

;; [M-S-t] to jump to a function
;; [M-C-t] to list functions
(load "codenav.el")




(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))



;(ido-mode 'both)

;(ido-mode 'both)
;(setq ido-enable-prefix nil
      ;ido-enable-flex-matching t
      ;ido-create-new-buffer 'always
      ;ido-use-filename-at-point 'guess
      ;ido-max-prospects 10
      ;ido-default-file-method 'selected-window)

;(paredit-mode -1)


; controls whether multiple lisp debuggers are spawned
; non-nil means: when another error is encountered, spawn yet another *Backtrace* lisp debugger



(setq visible-bell nil)

(setq ansi-color-for-comint-mode t)

(setq process-connection-type t)


(require 'projectile)
(projectile-global-mode t)

(require 'iedit)



;; expose system paths to emacs
(push (concat (getenv "HOME") "/bin") exec-path)
(push "/usr/local/bin" exec-path)

; (rvm-use "ruby-1.9.2-p290" "global")

;; makes a brand new *scratch* buffer if not present, switch to it
(global-set-key (kbd "C-c C-s") 'go-go-gadget-scratch-buffer)
(defun go-go-gadget-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))


;; Save your minibuffer history
(setq savehist-file (expand-file-name "~/.emacs.d/history"))
(savehist-mode 1)


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
  (modify-frame-parameters nil '((wait-for-wm . nil)))
  
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
  )


(defun toggle-color-theme ()
  "Switch to/from night color scheme."
  (interactive)
  ;; create the snapshot if necessary
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (load-theme 'solarized-dark)
      (load-theme 'solarized-zenburn)
  )
)

(color-theme-zenburn)
; (color-theme-blackboard)

;(require 'color-theme)
;(color-theme-zenburn)
;(set-cursor-color 'yellow)





(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)




;;============================================================
;; expansion and completion
;;============================================================
;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

(global-set-key (kbd "M-/") 'hippie-expand) ;; sucks
;(global-set-key (kbd "M-/") 'dabbrev-expand) ;; 'hippie-expand sucks

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
;(setq show-paren-style 'parenthesis)
(setq show-paren-style 'mixed)

(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; ediff - don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

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


(defun albert-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun albert-shift-region-left (start end &optional count)
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
  (albert-shift-region start end (- (prefix-numeric-value (or count 2))))
  t
)

(defun albert-shift-region-right (start end &optional count)
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (albert-shift-region start end (prefix-numeric-value (or count 2)))
  t
)




;;============================================================
;; MAJOR/MINOR MODES
;;============================================================

(require 'magit)
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(autoload 'tramp "Tramp")
(setq tramp-default-method "sshx")

(require 'xcscope)


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
;; MISC MODE HOOKS
;;==================================================

(remove-hook 'coding-hook 'turn-on-auto-fill)
(remove-hook 'coding-hook 'local-comment-auto-fill)
(remove-hook 'coding-hook 'pretty-lambdas)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'ipython-shell-hook 'ansi-color-for-comint-mode-on)


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


;; some sane js2 customize defaults
;  '(js2-bounce-indent-p t)
;  '(js2-indent-on-enter-key t)
;  '(js2-strict-missing-semi-warning nil)
;  '(tab-width 2) ; no idea where js2-mode is pulling default of 8 from, so sledgehammer it

(add-hook 'js2-mode-hook 
          '(lambda() 
             (setq tab-width 4)))



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

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "" t)

(setq load-path (cons  "~/.emacs.d/rdebug" load-path))
(require 'gud)
(require 'rdebug)
(setq rdebug-many-windows nil)
(setq rdebug-debug-active 't)

(font-lock-add-keywords 'ruby
                        '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                          ("\\<\\(WARNING\\):" 1 font-lock-warning-face t)
                          ("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
                          ("\\<\\(IMPORTANT\\):" 1 font-lock-warning-face t)
                          ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                          ("\\<\\(TBC\\)" 1 font-lock-warning-face t)
                          ("\\<\\(TBD\\)" 1 font-lock-warning-face t))
                        )


;;==================================================
;; PYTHON
;;==================================================
(setq load-path (cons  "~/.emacs.d/albert/pylookup" load-path))
(setq load-path (cons  "~/.emacs.d/albert/python-libs" load-path))

(autoload 'virtualenv-workon "virtualenv"
  "Activate a Virtual Environment present using virtualenvwrapper" t)

(eval-after-load 'python-mode
  '(progn
     (setq ipython-command "/usr/local/bin/ipython")
     (setq py-python-command-args '())
     (setq ipython-args '())
     (require 'ipython)

     (autoload 'pylookup-lookup "pylookup")
     (autoload 'pylookup-update "pylookup")
     (setq pylookup-program "~/.emacs.d/albert/pylookup/pylookup.py")
     (setq pylookup-db-file "~/.emacs.d/albert/pylookup/pylookup.db")
     (define-key py-mode-map "\C-ch" 'pylookup-lookup)

     (autoload 'python-pep8 "pep8" )

     ;;pdb setup, note the python version
     (setq pdb-path '/usr/lib/python2.6/pdb.py
           gud-pdb-command-name (symbol-name pdb-path))
     
     (defadvice pdb (before gud-query-cmdline activate)
       "Provide a better default command line when called interactively."
       (interactive
        (list (gud-query-cmdline pdb-path
                                 (file-name-nondirectory buffer-file-name)))))

     ;(define-key py-mode-map "\t"    'py-indent-line)
     ;(define-key py-mode-map [M-f1]  'py-shell)
     (define-key py-mode-map (kbd "RET") 'newline-and-indent)

     (define-key py-mode-map [f8] 'gud-next)
     (define-key py-mode-map [S-f8] 'gud-step)
     (define-key py-mode-map [M-f8] 'gud-cont)
     (define-key py-mode-map [C-f7] 'ipython-send-and-indent)

     (font-lock-add-keywords 'python
                             '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                               ("\\<\\(WARNING\\):" 1 font-lock-warning-face t)
                               ("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
                               ("\\<\\(IMPORTANT\\):" 1 font-lock-warning-face t)
                               ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                               ("\\<\\(TBC\\)" 1 font-lock-warning-face t)
                               ("\\<\\(TBD\\)" 1 font-lock-warning-face t))
                             )

     ;; Utilities that increase legibility and reduce code duplication
     (defun current-file-remotep ()
       "Tell if the file is remote"
       (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))

     )
  )



;; pymacs+rope is sweet but soooooo buggy and lags startup
;; M-: (rope) to include a bunch of super python hooks
;; requires python packages: Pymacs rope ropemode ropemacs
;; be installed in site-packages
;; * this is the callback for post-ropemacs-loaded

(defun rope()
 (setenv "PYTHONPATH"
         (concat
          (getenv "PYTHONPATH") path-separator
          "~/emacs.d/albert/python-libs"))

 (require 'pymacs)
 (autoload 'pymacs-apply "pymacs")
 (autoload 'pymacs-call "pymacs")
 (autoload 'pymacs-eval "pymacs" nil t)
 (autoload 'pymacs-exec "pymacs" nil t)
 (autoload 'pymacs-load "pymacs" nil t)
 (pymacs-load "ropemacs" "rope-")

 (setq ropemacs-enable-autoimport 't)
 (setup-ropemacs)
 )

(defun setup-ropemacs ()
  "Setup the ropemacs harness"

  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-autoimport-modules '("os" "sys" "httplib" "urllib" "hashlib" "shutil"))

  (define-key py-mode-map [C-f3]  'rope-goto-definition)
  (define-key py-mode-map [M-f3]  'rope-show-doc)
  (define-key py-mode-map [M-/]   'auto-complete)

  ;; Adding hook to automatically open a rope project if there is one
  ;; in the current or in the upper level directory
  (add-hook 'python-mode-hook
            (lambda ()
              (cond ((file-exists-p ".ropeproject")
                     (rope-open-project default-directory)
                     (virtualenv-workon (car (cdr (reverse (split-string default-directory "/"))))))
                    ((file-exists-p "../.ropeproject")
                     (rope-open-project (concat default-directory "..")))
                    (virtualenv-workon  (car (cdr (reverse (split-string default-directory "/")))))))))


;;==================================================
;; org
;;==================================================
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



;;==================================================
;; coffee
;;==================================================
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))




;;==================================================


;(load "experimental.el")
(load "keybindings.el")







