
;;============================================================
;;
;;
;;   GLOBAL SHORTCUTS / KEYBINDINGS
;;   alt keycode (kbd "C-c k")
;;
;; \242 :: f12
;; \275 :: "M-="
;; ^C   :: "C-c"
;;
;;============================================================



; vim style shift-j
(fset 'join-lines
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([14 134217848 106 111 105 110 45 108 105 110 101 return] 0 "%d")) arg)))
(global-set-key "\C-\M-j" 'join-lines)



;;; TAB KEY
(global-set-key "\M-]" 'albert-shift-region-right)
(global-set-key "\M-[" 'albert-shift-region-left)


;; i hate narrowing
;; apropos narrow-to-region
(global-unset-key "\C-xnn")
(global-unset-key "\C-xnp")
(global-unset-key "\C-xnd")


(global-unset-key "\M-U")
(global-set-key "\M-U" 'org-open-at-point)




;(global-unset-key "\C-i")
;(global-set-key "\C-i"               'IS-TAB-KEY)

;(global-set-key "\C-i" 'indent-relative)
;(define-key global-map "\t" 'indent-for-tab-command)



;; FRAMES
(global-unset-key "\M-`")
(global-set-key (kbd "<M-f2>") 'other-frame)
(global-set-key "\M-`" 'other-frame)
(global-set-key (kbd "<M-f3>") 'make-frame-command)
(global-set-key (kbd "<M-f4>") 'delete-frame)




(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key  (kbd "RET") 'newline-and-indent)
(global-unset-key "\C-m")
(global-set-key "\C-m" 'newline-and-indent)
(require 'ruby-mode)
(define-key ruby-mode-map (kbd "C-j")   'newline-and-indent)

;(global-unset-key "\C-x\C-c") ;; keep closing emacs?
(global-unset-key "\C-o")
;(global-unset-key "\C-i")
;(global-unset-key "\M-t")
;(global-unset-key "\M-\S-t")
;(global-unset-key "\M-\C-t")
;(global-unset-key [M-down-mouse-3]) ; cuz i've never understood how it works

(global-set-key "\C-xg"       'goto-line)
(global-set-key "\C-x\C-w"    'what-line)
(global-set-key "\C-o"        'other-window)
(global-set-key "\C-z"        'term)

(global-set-key "\C-xt"       'delete-trailing-whitespace)

(global-set-key (kbd "RET")   'newline-and-indent)

(global-set-key (kbd "<C-f5>") 'toggle-truncate-lines)

(global-set-key "\M-\S-c" 'comment-or-uncomment-region)


;; Nice quick buffer switching
;(global-set-key "\C-x\C-b" 'electric-buffer-list) ; reserved for C-F4



(defun my-maximize-focus-other-window ()
  (interactive)
  (other-window 1)
  (delete-other-windows))
(global-set-key "\C-\M-o" 'my-maximize-focus-other-window)


; keep other-buffer working in dired
(define-key dired-mode-map "\C-o" 'other-window)


(global-set-key "\C-\M-g" 'py-guess-indent-offset)

;; browse recent cuts
(global-set-key "\C-ck"      'browse-kill-ring)

;; navigate through symbols
(global-set-key "\C-\M-t"     'my-imenu-jump-to-function)
(global-set-key "\M-\S-t"     'imenu-make-selection-buffer)


(defun scroll-down-keep-cursor () (interactive) (scroll-down 5))
(defun scroll-up-keep-cursor ()   (interactive) (scroll-up 5))
(global-set-key (kbd "C-+") 'scroll-up-keep-cursor)
(global-set-key (kbd "C--") 'scroll-down-keep-cursor)
(global-set-key (kbd "<kp-add>") 'scroll-up-keep-cursor)
(global-set-key (kbd "<kp-subtract>") 'scroll-down-keep-cursor)

(global-unset-key (kbd "<M-k>"))
(global-set-key (kbd "<M-k>") 'kill-buffer)

;; Kill default buffer without the extra emacs questions
(defun kill-buffer-quiet() (interactive) (kill-buffer (buffer-name)))

;
;----------
; <F1>
;(global-set-key [f1]                 'fold-selective-display-more) ; folding
;(global-set-key [C-f1]                 'fold-selective-display-less) ; folding
;(global-set-key [M-f1]               'fold-selective-display-reset) ; folding

;(global-set-key [f1]                 'find-file)
;(global-set-key [f1]               'electric-buffer-list)

;----------
; <F2>
;(global-set-key [f2]              'other-window)
;(global-set-key [f2]            'highlight-phrase)
;(global-set-key [C-f2]          'unhighlight-regexp)

;----------
; <F3>
(global-set-key [C-f3]             'find-tag-at-point) ; ETAGS
(global-set-key [f3]               'cscope-find-egrep-pattern) ; 'cscope-find-this-symbol
(global-set-key [C-M-f3]             'cscope-index-files)



;----------
; <F4>
(global-set-key [f4]               'kill-buffer-quiet)
;----------
; <F5>
;(global-set-key [f5]             'svn-status)
;(global-set-key [C-f5]             'magit-status)
;(global-set-key [f5]               'magit-status)
(global-set-key [f5]               'magit-status)
;(global-set-key [M-f5]             'magit-status)
;(define-key ruby-mode-map [C-f5]     'rails-script:console)
;(define-key ruby-mode-map [C-f5]   'rails-log:open-development)
;----------
; <F6>
(global-set-key [f6]               'vc-diff)
(global-set-key [M-f6]             'vc-revert-buffer)
;----------
; <F7>
;(global-set-key [f7]               'vc-annotate)
;----------
; <F8>

(global-set-key (kbd "<f8>")       'hs-toggle-hiding)
(global-set-key (kbd "<M-f8>")     'my-toggle-hideshow-all)

;(global-set-key (kbd "<M-S-f8>") 'hs-show-all)

;; (define-key ruby-mode-map [f8]     'run-ruby)
;; (define-key ruby-mode-map [C-f8]   'ruby-send-definition-and-go)
;----------
; <F9>
(global-set-key (kbd "<f9>") 'toggle-color-theme)


;----------
; <F10>
(global-set-key [f10] 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "<M-f10>") 'browse-url)

;----------
; <F11>
(global-set-key [f11] 'session-toggle-permanent-flag)


;----------
; <F12>
(global-set-key [f12]              'delete-other-windows)
(global-set-key [C-f12]            'split-window-horizontally)
(global-set-key [M-f12]            'split-window-vertically)
; my window extensions
(global-set-key [C-M-f12]          'window-swap-orientation)
(global-set-key "\C-x4"            'window-swap-orientation)
(global-set-key [C-f4]             'bury-window) ; also C-x0



;;; Set cmd-H to hide Emacs and cmd-shift-h to hide others, as usual in Mac OS
;;; X. Usually bound to mark-paragraph
(global-set-key "\M-h" 'ns-do-hide-emacs)
(global-set-key "\M-H" 'ns-do-hide-others)



;;; Toggle fullscreen mode
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
(global-set-key [f7] 'toggle-fullscreen)




;; apple-style increase-fontsize
(defun bump-up() (interactive) (text-scale-increase 1))
(defun bump-down() (interactive) (text-scale-decrease 1))
(global-unset-key "\M-=")
(global-set-key "\M-=" 'bump-up)
(global-set-key "\M--" 'bump-down)


(global-set-key (kbd "C-c f")       'grep-find)

(global-set-key (kbd "C-c C-b") 'projectile-jump-to-project-file)
(global-set-key [f2] 'projectile-grep-in-project)

(global-set-key [f1] 'org-agenda)
;(global-set-key [S-f1] 'org-clock-clock-out)
(global-set-key [M-f1] 'org-capture)


(defun toggle-split-axis()
  (interactive)
  
  (split-window-horizontally)
  )

(require 'org)
(define-key org-mode-map "\M-r" 'org-table-recalculate-buffer-tables)


(defun google ()
  "Do a Google search of the region or symbol at the point"
  (interactive)
  (let ((phrase (elt (region-or-thing 'symbol) 0)))
    (browse-url (concat "http://www.google.com/search?q="
                        (replace-regexp-in-string " " "+" phrase)))))
(global-set-key "\M-?" 'google)



; repeat-last-command
(global-set-key [M-f7] 'repeat)

; doc-last-command
(defun doc-last-command()
  (interactive)
  (describe-function last-command))
(global-set-key [C-f7] 'doc-last-command)


