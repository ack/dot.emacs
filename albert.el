(setq load-path (cons  "~/.emacs.d/albert" load-path))
(setq load-path (cons  "~/.emacs.d/albert/auto-complete" load-path))
(setq load-path (cons  "~/.emacs.d/albert/pylookup" load-path))
(setq load-path (cons  "~/.emacs.d/albert/python-libs" load-path))
(setq load-path (cons  "~/.emacs.d/albert/solarized" load-path))

(global-set-key (kbd "M-/") 'dabbrev-expand) ;; 'hippie-expand sucks

(ido-mode -1)
;(paredit-mode -1)
(setq visible-bell nil)


; set t to enable tracing in popup
(setq debug-on-error t)


; expose system paths to emacs
(push (concat (getenv "HOME") "/bin") exec-path)
(push "~/bin" exec-path)
(push "/opt/local/bin" exec-path)
(push "/usr/local/bin" exec-path)


(setq ansi-color-for-comint-mode t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'ipython-shell-hook 'ansi-color-for-comint-mode-on)



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
          (add-to-list 'default-frame-alist (cons 'width 150))
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


  ; jerk/spawn to 100x100 offset
  (setq initial-frame-alist '((top . 100) (left . 100)))
  )



(defun toggle-color-theme ()
  "Switch to/from night color scheme."
  (interactive)
  ;; create the snapshot if necessary
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (load-theme 'solarized-dark)
      (load-theme 'solarized-light)
  )
)

; (color-theme-zenburn)
; (color-theme-blackboard)
(require 'color-theme)
(color-theme-zenburn)
(set-cursor-color 'yellow)



;;============================================================
;; MAJOR/MINOR MODES
;;============================================================

(require 'magit)
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))

(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(autoload 'tramp "Tramp")

(require 'xcscope)
(require 'browse-kill-ring)             ; browse recent cuts

;--------------------------------------------------
;     iswitch configuation
;--------------------------------------------------
(require 'uniquify)                     ; unique buffer names




;; auto-complete

(require 'auto-complete)
(global-auto-complete-mode t)

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




(load "keybindings.el")


;; lint
(defun jslint-thisfile ()
  (interactive)
  (compile (format "jsl -conf /etc/jsl.conf -process %s" (buffer-file-name))))

(add-hook 'javascript-mode-hook
  '(lambda () (local-set-key [f8] 'jslint-thisfile)))
;(add-hook 'js-mode-hook
;  '(lambda ()
;  (local-set-key [f8] 'jslint-thisfile)))
(add-hook 'espresso-mode-hook
  '(lambda () (local-set-key [f8] 'jslint-thisfile)))
  




(autoload 'virtualenv-workon "virtualenv" "Activate a Virtual Environment present using virtualenvwrapper" t)






;;==================================================
;; PYTHON
;;==================================================

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

     (require 'tramp)
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




;; Save your minibuffer history
(setq savehist-file (expand-file-name "~/.emacs.d/history"))
(savehist-mode 1)

(global-set-key (kbd "C-c C-s") 'go-go-gadget-scratch-buffer)

;; Create/Goto *scratch* buffer SUPERFAST!
(defun go-go-gadget-scratch-buffer ()
  (interactive)
  ;; Make a brand new *scratch* buffer if not present
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))


(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))


(require 'deft)


(remove-hook 'coding-hook 'turn-on-auto-fill)
(remove-hook 'coding-hook 'local-comment-auto-fill)
(remove-hook 'coding-hook 'pretty-lambdas)
;(remove-hook 'ruby-mode-hook 'turn-on-auto-fill)
;(remove-hook 'python-mode-hook 'turn-on-auto-fill)



(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))


(load "codenav.el")



(defun find-thing-at-point (&optional always-ask)
  (interactive "P")
  (let* ((at-point (thing-at-point 'symbol))
         (s (and at-point (intern at-point)))
         (v (or (variable-at-point)
                (and s (boundp s) s)))
         (f (or (function-called-at-point)
                (and s (fboundp s) s))))
    (push-mark (point) t)
    (cond
      (always-ask (call-interactively 'find-function))
      ((and v (not (numberp v)))
       (find-variable v))
      ((and f (subrp (symbol-function f)))
       (let ((buf-pos (find-function-search-for-symbol
                       f nil (help-C-file-name (symbol-function f) 'subr))))
         (and (car buf-pos) (pop-to-buffer (car buf-pos)))))
      (f (find-function f))
      (t (call-interactively 'find-function)))))




(setq deft-extension "*")
(setq deft-directory "~/src/appfog/Notes")
(setq deft-text-mode 'markdown-mode)



(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)





(require 'iswitchb)
;(require 'iswitchb-highlight)
(iswitchb-mode 't)

(add-hook 'iswitchb-minibuffer-setup-hook
	  '(lambda () (set (make-local-variable 'max-mini-window-height) 3)))

;(iswitchb-default-keybindings)

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


(load "experimental.el")
