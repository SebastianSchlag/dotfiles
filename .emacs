(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load CEDET - taken from: https://gist.github.com/alexott/3930120
;; adapted according to: http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq cedet-root-path (file-name-as-directory "~/.emacs.d/cedet/"))

(load-file (concat cedet-root-path "cedet-devel-load.el"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))
(autoload 'gtags-mode "gtags" "" t)

;; select which submodes we want to activat
;; activates CEDET's context menu that is bound to right mouse button;
;;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode) 
;; activates highlighting of first line for current tag (function, class, etc.);
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; activates displaying of possible name completions in the idle time. Requires that global-semantic-idle-scheduler-mode was enabled; 
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;; activates highlighting of local names that are the same as name of tag under cursor
;(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
;; activates automatic parsing of source code in the idle time;
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; enables automatic bookmarking of tags that you edited, so you can return to them later with the semantic-mrub-switch-tags command;
;;(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
; shows which elements weren't processed by current parser's rules;
;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;; enables global support for Semanticdb; 
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-breadcrumbs-mode)
;; activates displaying of information about current tag in the idle time. Requires that global-semantic-idle-scheduler-mode was enabled.
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode) 
;; activates mode when name of current tag will be shown in top line of buffer;
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;; Activate semantic
(semantic-mode 1)

;; load contrib library
(require 'eassist)
(require 'semantic/ia)
(require 'semantic/bovine/gcc) ;;find system-wide libs

;; Add additional directories like this:
(semantic-add-system-include "/usr/local/include/boost" 'c++-mode)

;; if you want to enable support for gnu global
(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; ;; customisation of modes
;; (defun my-cedet-hook ()
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu) ;; whatever the symbol you are typing, this hot key automatically complete it for you.
;;   (local-set-key "\C-c?" 'semantic-ia-complete-symbol) ;; another way to complete the symbol you are typing
;;   (local-set-key "\C-c>" 'semantic-complete-analyze-inline) ;; when you typed . or -> after an object name, use this key to show possible public member functions or data members.
;;   (local-set-key "\C-c<" 'semantic-ia-fast-jump) ;; jump to the definition of the symbol under cursor
;;   (local-set-key "\C-c=" 'semantic-decoration-include-visit)  ;; visit the header file under cursor 
;;   (local-set-key "\C-cj" 'semantic-complete-jump) ;; jump to the definition of the symbol under cursor 
;;   (local-set-key "\C-cq" 'semantic-ia-show-doc)  ;;  show the document of the symbol under cursor
;;   (local-set-key "\C-cs" 'semantic-ia-show-summary) ;; show a summary about the symbol under cursor
;;   (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle) ;; toggle between the implementation and a prototype of symbol under cursor
;;   (local-set-key "\C-c+" 'semantic-tag-folding-show-block) ;; unfold the block under cursor
;;   (local-set-key "\C-c-" 'semantic-tag-folding-fold-block) ;; fold the block under cursor
;;   (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all) ;; unfold all
;;   (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all) ;; fold all
;;   (local-set-key "\C-cr" 'semantic-symref)
;;   ;; rename local variable under cursor
;;   (local-set-key "\C-c\C-r" 'semantic-symref-rename-local-variable)
;;   (gtags-mode t)
;;   (local-set-key "\C-cf" 'gtags-find-tag)
;;   (flyspell-prog-mode)
;;   )
;; (add-hook 'c-mode-common-hook 'my-cedet-hook)
;; (add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)

;; (defun alexott/c-mode-cedet-hook ()
;;   (local-set-key "\C-ct" 'eassist-switch-h-cpp)
;;   (local-set-key "\C-xt" 'eassist-switch-h-cpp)
;;   (local-set-key "\C-ce" 'eassist-list-methods)
;;   (local-set-key "\C-c\C-r" 'semantic-symref)
;;   )
;; (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)
;; (add-hook 'c++-mode-hook 'alexott/c-mode-cedet-hook)

;; (defun my-c-mode-cedet-hook ()
;;   (add-to-list 'ac-sources 'ac-source-gtags)
;;   (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
;; (add-hook 'c++-mode-hook 'my-c-mode-cedet-hook)

;; ;; enable support for gnu global
(when (cedet-gnu-global-version-check t)
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t))
 
;; ;; EDE
;; (global-ede-mode 1)
;; ;; (ede-enable-generic-projects)
;; ;; (semantic-load-enable-code-helpers)

;; ;; (ede-cpp-root-project "hypergraph-partitioning"
;; ;;                       :file "~/repo/schlag_git/CMakeLists.txt"
;; ;; 		      :include-path '("/src")
;; ;;                       )

;; Integration with imenu
;; (defun semantic-imenu-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'semantic-imenu-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; Add the user-contributed repository
;; (add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; Add the original Emacs Lisp Package Archive
 (add-to-list 'package-archives
              '("elpa" . "https://tromey.com/elpa/") t)

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))


;; initialize packages
(package-initialize)

;; (require 'auto-install)

;; packages not handled by cask/pallet are stored in dotfile repo and should be
;; symlinked to this directory
(add-to-list 'load-path "~/.emacs.d/external")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECB - Emacs Code Browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path
                     "~/.emacs.d/ecb-snap")
(require 'ecb)
(require 'ecb-autoloads)
(setq stack-trace-on-error t)
(setq ecb-tip-of-the-day nil) ;; no ecb tip of the day

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advanced Cmake Syntax Highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'andersl-cmake-font-lock-activate "andersl-cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'andersl-cmake-font-lock-activate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tramp mode should use ssh
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CC Mode and C/C++ related configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)

;; Use Google's C Style:
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Allow us to get english language explanations of complex C++ declarations
;; To use, select a declaration such as "char*(*(*p)[5])(int)", and then type: "C-x cdecl"
;; (defun cdecl () 
;;   (called-interactively)
;;   (if (eq major-mode 'c++-mode) 
;;       (progn (called-interactively) (shell-command
;;                             (concat "c++decl explain \"" (buffer-substring (region-beginning)
;;                                                                            (region-end)) "\"")))
;;     (progn (called-interactively) (shell-command
;;                           (concat "cdecl explain \"" (buffer-substring (region-beginning)
;;                                                                        (region-end)) "\"")))
;;     )
;;   )

;;Create Header Guards with f12
(global-set-key [f12] 
  		'(lambda () 
  		   (called-interactively)
  		   (if (buffer-file-name)
  		       (let*
  			   ((fName (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
  			    (ifDef (concat "#ifndef " fName "_H_" "\n#define " fName "_H_" "\n"))
  			    (begin (point-marker))
  			    )
  			 (progn
  					; If less then 5 characters are in the buffer, insert the class definition
  			   (if (< (- (point-max) (point-min)) 5 )
  			       (progn
  				 (insert "\nclass " (capitalize fName) "{\npublic:\n\nprivate:\n\n};\n")
  				 (goto-char (point-min))
  				 (next-line-nomark 3)
  				 (setq begin (point-marker))
  				 )
  			     )
  			   
  					;Insert the Header Guard
  			   (goto-char (point-min))
  			   (insert ifDef)
  			   (goto-char (point-max))
  			   (insert "\n#endif" "  // " fName "_H_")
  			   (goto-char begin))
  			 )
					;else
  		     (message (concat "Buffer " (buffer-name) " must have a filename"))
  		     )
  		   )
  		)


  ;; add C++11 keywords
  (font-lock-add-keywords
   'c++-mode '(
               ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . 'font-lock-keyword-face)
               ("\\<\\(char[0-9]+_t\\)\\>" . 'font-lock-keyword-face)))

;; enforce column size
(load-file "~/.emacs.d/external/column-enforce-mode.el")
(require 'column-enforce-mode)
(add-hook 'c-mode-common-hook '100-column-rule)
(add-hook 'c++-mode-common-hook '100-column-rule)

;; code folding
;; C-c @ C-c	Command: hs-toggle-hiding  Toggle hiding/showing of a block
;; C-c @ C-h	Command: hs-hide-block     Select current block at point and hide it
;; C-c @ C-l	Command: hs-hide-level     Hide all block with indentation levels below this block
;; C-c @ C-s	Command: hs-show-block     Select current block at point and show it.
;; C-c @ C-M-h	Command: hs-hide-all       Hide all top level blocks, displaying only first and last lines.
;; C-c @ C-M-s	Command: hs-show-all       Show everything
(add-hook 'c-mode-common-hook   'hs-minor-mode)

;; narrowing
;; C-x n d	Command: narrow-to-defun   Narrow buffer to current function at point
;; C-x n r/n	Command: narrow-to-region  Narrow buffer to active region
;; C-x n w	Command: widen             Widen buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnipped and auto-complete config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet) ;; should be loaded before auto-complete
(yas-reload-all)
(add-hook 'c-mode-common-hook '(lambda () (yas-minor-mode)))
(add-hook 'c++-mode-hook '(lambda () (yas-minor-mode)))

;; auto-complete
(require 'cl)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(setq ac-comphist-file (expand-file-name
             "~/.emacs.d/ac-comphist.dat"))
(ac-config-default)

;; never start automatically
(setq ac-auto-start nil)

;(require 'auto-complete-clang-async)

(require 'ac-math) 
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

 (defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
   (setq ac-sources
         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                 ac-sources))
   )
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(setq ac-math-unicode-in-math-p t)

;(require 'auto-complete-auctex)


;; Select candidates with C-n/C-p only when completion menu is displayed:
(setq ac-use-menu-map t)
(define-key ac-menu-map "C-n" 'ac-next)
(define-key ac-menu-map "C-p" 'ac-previous)

(require 'xcscope)
(setq cscope-index-recursively t)

(setq-default ac-sources '(
			   ac-source-abbrev 
			   ac-source-dictionary 
			   ac-source-words-in-same-mode-buffers 
			   ac-source-filename 
			   ac-source-yasnippet)
)

(setq ac-candidate-limit 100) ;; do not stall with too many results
(setq ac-auto-start 0)
(setq ac-auto-show-menu t)
(setq ac-quick-help-delay 0)
(setq ac-use-fuzzy t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-expand-on-auto-complete nil)
(setq ac-quick-help-height 20)
(setq ac-menu-height 20)
(define-key ac-mode-map  [(control tab)] 'auto-complete)


;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")


;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/external/clang-complete")
;;   (setq clang-completion-suppress-error 't)
;;   (setq ac-clang-cflags (append '("-std=c++11") ac-clang-cflags))
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process)
;; )

(defun my-ac-config ()
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;set up hunspell for flyspell-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-program-name "/usr/bin/hunspell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load Dired+ when dired is loaded
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-copy-paste)

(define-key dired-mode-map "\C-w" 'dired-copy-paste-do-cut)
(define-key dired-mode-map "\M-w" 'dired-copy-paste-do-copy)
(define-key dired-mode-map "\C-z" 'dired-copy-paste-do-paste)

(defun my-dired-mouse-find-file (event)
  "In dired, visit the file or directory name you click on."
  (called-interactively "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (progn
              (select-window window)
              (dired file)))
      (select-window window)
      (find-file (file-name-sans-versions file t)))))

(defun my-dired-terminal (&optional arg)
  "Launch terminal in current directory."
  (called-interactively)
  ;(start-process "terminal" "*scratch*" "/usr/bin/urxvt")
  (start-process "terminal" nil "/usr/bin/zsh")
)

(defun set-my-dired-keys-hook ()
  "My favorite dired keys."
  ; for some reason mouse-2 = left click (mouse-1)
  (define-key dired-mode-map [mouse-2] 'my-dired-mouse-find-file)
  (define-key dired-mode-map [M-mouse-2] 'diredp-mouse-find-file-other-frame)
  ; backspace
  (define-key dired-mode-map [backspace] 'dired-up-directory)
  ; F4 -> launch terminal
  (define-key dired-mode-map [f4] 'my-dired-terminal)
)

(add-hook 'dired-mode-hook 'set-my-dired-keys-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'powerline)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smartparens default configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smartparens-config)
(smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-hook 'c-mode-hook 'projectile-mode)
;(add-hook 'c++-mode-hook 'projectile-mode)
;; let projectile be usable everywhere
;(setq projectile-require-project-root nil)
;(projectile-global-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change unicode font for pretty symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 12
                               :weight 'normal)))
(global-prettify-symbols-mode +1)


(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(global-set-key (kbd "<C-f12>") 'magit-status)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (called-interactively)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun mu-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git gutter fringe
;; Show git diffs in fringe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'git-gutter-fringe)

(global-git-gutter-mode +1)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-specific options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(column-number-mode t)

(setq inhibit-startup-message t)

(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Format the title-bar to always include the buffer name
(setq frame-title-format "%b")

;; in every buffer, the line which contains the cursor will be fully highlighted
(global-hl-line-mode 1)

;; enable inline images:
(iimage-mode)


(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )


;; treat .h and .hpp files as c++ files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (called-interactively)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
(global-set-key (kbd "C-c k") 'kill-other-buffers)

;; split windows horizontally in ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-multiframe)

;; unique buffer names
(require 'uniquify)

;; undo and redo window configurations via C-c right C-c left
(winner-mode 1)

;; if you set it to 'mixed, it will behave like 'parenthesis 
;; when the matching parenthesis is visible, and like 'expression otherwise.
(setq show-paren-style 'mixed)

;; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;;;;;;;;;;;;;
;;; ORG MODE
;;;;;;;;;;;;;
;; todo entry changes to done once all children are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; useful shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; only show one star for headlines
(setq org-hide-leading-stars 'hidestars)

;; enter follows links
(setq org-return-follows-link t)

;; Einen Zeitstempel eintragen, wenn eine Aufgabe als erledigt markiert wird
(setq org-log-done 'time)

;; Einen eigenen Drawer benutzen
(setq org-log-into-drawer t)

(setq org-todo-keywords
 '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "IDEA(i)" "THINK(n)" 
             "DELEGATED(g@/!)" "|" "DONE(d@/!)" "CANCELED(c@)")))

;; Farben anpassen
(setq org-todo-keyword-faces
      '(("TODO"  . (:foreground "#b70101" :weight bold))
        ("STARTED"  . (:foreground "#b70101" :weight bold))
        ("WAITING"  . (:foreground "orange" :weight bold))
	("IDEA" . (:foreground "#436eee" :weight bold))
	("THINK" . (:foreground "#ff7f24" :weight bold))
        ("DONE"  . (:foreground "forestgreen" :weight bold))
        ("DELEGATED"  . (:foreground "forestgreen" :weight bold))
        ("CANCELED"  . shadow)))

;; Fast TODO Selection
(setq org-use-fast-todo-selection t)

(defun org-archive-all-done-item ()
  "Archive all item that have with prefix DONE."
  (called-interactively)
  (save-excursion
    (show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ DONE" nil t)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "^[\\*]+ DONE" nil t)
            (org-advertized-archive-subtree))
          (org-display-all-todo-item)
          (message "Archive finished"))
      (org-display-all-todo-item)
      (message "No need to archive"))))

;;-------------------------------------------------------------------------
;; Supporting Functions
;;-------------------------------------------------------------------------
(require 'org)
(define-key org-mode-map "\C-cs" 'org-sort)

;; put all temporary files into /tmp
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)
(setq tramp-auto-save-directory emacs-tmp-dir)
(setq tramp-persistency-file-name (format "%s/tramp" emacs-tmp-dir))
(setq image-dired-dir (format "%s/image-dired" emacs-tmp-dir))

;; http://zeekat.nl/articles/making-emacs-work-for-me.html
(defvar my/org-babel-evaluated-languages
  '(emacs-lisp)
  "List of languages that may be evaluated in Org documents")

(org-babel-do-load-languages
 'org-babel-load-languages
 (mapcar (lambda (lang)
           (cons lang t))
         my/org-babel-evaluated-languages))
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
(add-to-list 'my/org-babel-evaluated-languages 'dot)
(add-to-list 'my/org-babel-evaluated-languages 'ditaa)
(add-to-list 'my/org-babel-evaluated-languages 'plantuml)

(setq org-ditaa-jar-path "~/repo/dotfiles/emacs-plugins/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/repo/dotfiles/emacs-plugins/plantuml.jar")
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (ditaa . t) 
   (R . t)))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'yank)       ; paste

;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)
;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
;; toggle comment
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;;ecb
(global-set-key (kbd "C-c e") 'ecb-activate)
(global-set-key (kbd "C-c w") 'ecb-deactivate)

(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

(global-set-key [C-x C-b] 'buffer-menu)
(global-set-key [M-S-up] 'buffer-menu)

(global-set-key [M-S-left] 'previous-buffer)
(global-set-key [M-S-right] 'next-buffer)

; window handling
(global-set-key "\M-`" 'delete-other-windows)
(global-set-key "\M-2" 'new-frame)
(global-set-key "\M-3" 'delete-frame)

; go to last edit point
(global-set-key [(ctrl meta l)] 'goto-last-change);

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better Scrolling
;; http://zeekat.nl/articles/making-emacs-work-for-me.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sublime-style multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight ToDos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
(add-hook 'c++-mode-hook 'fic-mode) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; svn integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

(global-set-key (kbd "<C-f11>") 'svn-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window related shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'windmove)
(windmove-default-keybindings)
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ggtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ggtags)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	      (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)
(require 'helm-grep)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq
 helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
 helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
 helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
 helm-candidate-number-limit 500 ; limit the number of displayed canidates
 helm-ff-file-name-history-use-recentf t
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
 helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers

 )

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-c h C-c w") 'helm-wikipedia-suggest)

(global-set-key (kbd "C-c h x") 'helm-register)
;; (global-set-key (kbd "C-x r j") 'jump-to-register)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; show minibuffer history with Helm
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

(define-key global-map [remap find-tag] 'helm-etags-select)

(define-key global-map [remap list-buffers] 'helm-buffers-list)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
		      ;(display-buffer--maybe-pop-up-frame-or-window)
		      (display-buffer-below-selected)
		      ;(display-buffer-at-bottom)
		      ;(display-buffer-in-side-window)
		      ;(inhibit-same-window . t)
                         (window-height . 0.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm-swoop                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locate the helm-swoop folder to your path
(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "C-c s") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

;; Enable helm-gtags-mode in Dired so you can jump to any tag
;; when navigate project tree with Dired
(add-hook 'dired-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in Eshell for the same reason as above
(add-hook 'eshell-mode-hook 'helm-gtags-mode)

;; Enable helm-gtags-mode in languages that GNU Global supports
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; key bindings
(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean aindent mode & wsbutler - whitespace handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)
(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

(require 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GDB config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gdb-many-windows t  ;; use gdb-many-windows by default
      gdb-show-main t     ;; Non-nil means display source file containing the main routine at startup
 )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)
;; ;(delete 'company-semantic company-backends)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)

;; ;; company-c-headers
;; (add-to-list 'company-backends 'company-c-headers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smarter navigation to the beginning of a line
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first.  If
   point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
		'smarter-move-beginning-of-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History
;; http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tweaks from
;; http://pages.sachachua.com/.emacs.d/Sacha.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;; go back to previous position
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; change text size
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-fold-env-spec-list
   (quote
    (("[tikzpicture]"
      ("tikzpicture"))
     ("[algorithm]"
      ("algorithm"))
     ("[algorithm2e]"
      ("algorithm2e")))))
 '(LaTeX-fold-macro-spec-list nil)
 '(LaTeX-fold-math-spec-list nil)
 '(TeX-command-list
   (quote
    (("LaTeX" "%`~/.emacs.d/flymake-pdflatex -shell-escape --synctex=1 %(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-fold-env-spec-list nil)
 '(TeX-fold-macro-spec-list nil)
 '(TeX-fold-math-spec-list nil)
 '(TeX-save-query nil)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection
   (quote
    ((engine-omega "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(ac-auto-show-menu 1.0)
 '(ac-auto-start nil)
 '(ac-non-trigger-commands
   (quote
    (*table--cell-self-insert-command electric-buffer-list)))
 '(ac-quick-help-prefer-pos-tip t)
 '(ac-trigger-commands-on-completing
   (quote
    (autopair-backspace paredit-backward-delete paredit-backward-delete-word)))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"))
 '(bibtex-comma-after-last-field t)
 '(bibtex-entry-format
   (quote
    (opts-or-alts required-fields numerical-fields whitespace realign last-comma delimiters sort-fields)))
 '(bibtex-maintain-sorted-entries (quote crossref))
 '(column-number-mode t)
 '(company-auto-complete-chars (quote (46)))
 '(compilation-always-kill t)
 '(compilation-scroll-output (quote first-error))
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(display-buffer-base-action (quote (display-buffer-same-window)))
 '(ecb-auto-activate nil)
 '(ecb-ignore-display-buffer-function (quote always))
 '(ecb-layout-name "left2")
 '(ecb-layout-window-sizes
   (quote
    (("left2"
      (ecb-directories-buffer-name 0.08823529411764706 . 0.4838709677419355)
      (ecb-sources-buffer-name 0.08823529411764706 . 0.5))
     ("left-dir-plus-speedbar"
      (ecb-directories-buffer-name 0.08823529411764706 . 0.5)
      (ecb-speedbar-buffer-name 0.08823529411764706 . 0.4838709677419355))
     ("leftright2"
      (ecb-directories-buffer-name 0.09663865546218488 . 0.6290322580645161)
      (ecb-sources-buffer-name 0.09663865546218488 . 0.3548387096774194)
      (ecb-methods-buffer-name 0.14285714285714285 . 0.6290322580645161)
      (ecb-history-buffer-name 0.14285714285714285 . 0.3548387096774194)))))
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness nil)
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote ("~/repo/" ("/" "/"))))
 '(fci-rule-color "#eee8d5")
 '(font-latex-fontify-sectioning 1.0)
 '(font-latex-match-reference-keywords
   (quote
    (("printbibliography" "[{")
     ("addbibresource" "[{")
     ("autoref" "[{")
     ("cref" "[{")
     ("Cref" "[{")
     ("crefrange" "[{")
     ("Crefrange" "[{"))))
 '(global-semantic-idle-summary-mode nil)
 '(magit-gitk-executable nil)
 '(magit-popup-display-buffer-action nil)
 '(magit-restore-window-configuration nil)
 '(magit-save-some-buffers (quote dontask))
 '(magit-server-window-for-commit nil)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(make-backup-files nil)
 '(nxhtml-autoload-web nil t)
 '(openwith-associations
   (quote
    (("\\.pdf\\'" "evince"
      (file))
     ("\\.pdf\\'" "evince"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer"
      ("-idx" file)))))
 '(org-agenda-files (quote ("~/Dropbox/org/todo.org")))
 '(org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame))))
 '(package-selected-packages
   (quote
    (company-lsp projectile cquery indent-guide base16-theme ssh markdown-mode+ cpputils-cmake cmake-font-lock helm-ag helm-flymake auctex-lua gtags yasnippet yaml-mode xcscope ws-butler use-package smartparens powerline pallet multiple-cursors magit helm-swoop helm-gtags google-c-style git-gutter-fringe ggtags fic-mode color-theme-sanityinc-solarized clean-aindent-mode ac-math)))
 '(popwin:special-display-config
   (quote
    (("*Miniedit Help*" :noselect t)
     (help-mode)
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t)
     (grep-mode :noselect t)
     (occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     ("*Shell Command Output*")
     ("*vc-diff*")
     ("*vc-change-log*")
     (" *undo-tree*" :width 60 :position right)
     ("^\\*anything.*\\*$" :regexp t)
     ("*slime-apropos*")
     ("*slime-macroexpansion*")
     ("*slime-description*")
     ("*slime-compilation*" :noselect t)
     ("*slime-xref*")
     (sldb-mode :stick t)
     (slime-repl-mode)
     (slime-connection-list-mode))))
 '(powerline-default-separator (quote arrow-fade))
 '(rebox-style-loop (quote (370 243)))
 '(reftex-ref-style-default-list (quote ("Cleveref")))
 '(safe-local-variable-values
   (quote
    ((eval add-to-list
	   (make-variable-buffer-local
	    (quote LaTeX-fold-math-spec-list))
	   (quote
	    ("[tabular]"
	     ("tabular"))))
     (c-tab-always-indent . t)
     (nxml-child-indent . 4)
     (rebox-min-fill-column . 100)
     (rebox-min-fill-column . 110)
     (rebox-min-fill-column . 120))))
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-displayor-tooltip-initial-max-tags 5)
 '(show-paren-mode t)
 '(sp-autodelete-closing-pair nil)
 '(sp-autodelete-opening-pair nil)
 '(sp-autodelete-pair nil)
 '(sp-autoescape-string-quote t)
 '(sp-autoinsert-quote-if-followed-by-closing-pair t)
 '(temp-buffer-show-function (quote ecb-temp-buffer-show-function-emacs))
 '(tool-bar-mode nil)
 '(undo-limit 8000000)
 '(undo-outer-limit 120000000)
 '(undo-strong-limit 120000000)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))


;; adapt region and highlight face to match theme
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "#657b83" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Monaco"))))
 '(powerline-active1 ((t (:inherit mode-line :background "#93a1a1" :foreground "#657b83"))))
 '(powerline-inactive1 ((t (:inherit mode-line :background "#93a1a1" :foreground "#657b83"))))
 '(powerline-inactive2 ((t (:inherit mode-line :background "#eee8d5" :foreground "#657b83"))))
 '(region ((t (:background "gray90"))))
 '(semantic-highlight-edits-face ((t (:background "gray90"))))
 '(yas-field-highlight-face ((t (:background "gray90")))))


;; ;; special bindings for latex quickies

(defun my-latex-key-bindings ()
  "Add some latex macro keys"
  (called-interactively)
  (turn-on-reftex)
  (visual-line-mode 1)
  (local-set-key "\C-\M-o" (lambda () (called-interactively) (insert "\\operatorname{")))
  (local-set-key [M-S-down] (lambda () (called-interactively) (reftex-toc)))
  (local-set-key "\C-\M-r" (lambda () (called-interactively) (reftex-reference)))
  (local-set-key "\C-\M-c" (lambda () (called-interactively) (reftex-citation)))
  ;; show frames in section list -> very useful for beamer presentations
  (setq reftex-section-levels
        (cons '("begin{frame}" . 3) reftex-section-levels))
  ;; The following makes \C-c\C-c not ask, just do the default action. Adds C-c-a for asking
  (setq TeX-command-force "LaTeX")
  ;; 'a' for ask, change to anything you want
  (local-set-key "\C-c\C-a" (lambda (arg) (called-interactively "P")
                            (let ((TeX-command-force nil)) (TeX-command-master arg))))
  ;; auto folding of tikzpicture and algorithm environments in tex files
  (TeX-fold-mode 0)
  (add-hook 'find-file-hook 'TeX-fold-buffer t)
)

(add-hook 'tex-mode-hook 'my-latex-key-bindings)
(add-hook 'latex-mode-hook 'my-latex-key-bindings)
(add-hook 'TeX-mode-hook 'my-latex-key-bindings)
(add-hook 'LaTeX-mode-hook 'my-latex-key-bindings)

(add-to-list 'auto-mode-alist '("\\.tikz$" . latex-mode))

;; -----------------------------
;; --- reftex customizations ---
;; -----------------------------

;; disable query for \ref or \pageref style
(setq reftex-ref-macro-prompt nil)

;; additional label styles
(setq reftex-label-alist
      '(
        ("theorem"   ?t "thm:" "~\\ref{%s}" t   ("theorem"   "th.") -2)
        ("lemma"     ?t "lem:" "~\\ref{%s}" t   ("lemma"     "lem") -2)
        ("algorithm" ?a "alg:" "~\\ref{%s}" t   ("algorithm" "alg") -2)
        ))

;; --------------------
;; --- flymake mode ---
;; --------------------

(eval-after-load "flymake"
  '(progn
     ;; compile tex using pdflatex
     (defun flymake-get-tex-args (file-name)
       (list "~/.emacs.d/flymake-pdflatex" (list "-file-line-error" "-interaction=nonstopmode" "-shell-escape" file-name)))

     ;; severe logging
     ;(setq flymake-log-level 9)

     ;; changes timeout
     (setq flymake-no-changes-timeout 5)

     ;; dont run on enters
     (setq flymake-start-syntax-check-on-newline nil)

     ;; scan complete files for matching include lines
     (setq flymake-check-file-limit nil)

     ;; fixes unbalanced braces in LaTeX files
     (push '("^\\(.*?\.tex\\):\\([0-9]*?\\):\\(.*?\\):\\(.*?\\)" nil 2 3 4) flymake-err-line-patterns)
     (push '("^\\(.*?\.tikz\\):\\([0-9]*?\\):\\(.*?\\):\\(.*?\\)" nil 2 3 4) flymake-err-line-patterns)
     (push '("Runaway argument?" nil nil nil) flymake-err-line-patterns)
     (push '("Emergency stop." nil nil nil) flymake-err-line-patterns)
     (push '("Package tikz Error:" nil nil nil) flymake-err-line-patterns)

     ;; disable master search for numeric endings
     (setq flymake-allowed-file-name-masks
           (delete '("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
                   flymake-allowed-file-name-masks)) ; don't use multipart tex files

     ;; enable master search for -fig.tex endings
     (add-to-list 'flymake-allowed-file-name-masks
                  '("fig\\.tex\\'" flymake-master-tex-init flymake-master-cleanup))

     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.tikz\\'" flymake-master-tex-init flymake-master-cleanup))
     ))

;; ;; -------------------------------
;; ;; --- Less Latency with Tramp ---
;; ;; -------------------------------

;; (defvar disable-tramp-backups '(all))

;; (eval-after-load "tramp"
;;   '(progn
;;      ;; Modified from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
;;      (setq backup-enable-predicate
;;            (lambda (name)
;;              (and (normal-backup-enable-predicate name)
;;                   ;; Disable all tramp backups
;;                   (and disable-tramp-backups
;;                        (member 'all disable-tramp-backups)
;;                        (not (file-remote-p name 'method)))
;;                   (not ;; disable backup for tramp with the listed methods
;;                    (let ((method (file-remote-p name 'method)))
;;                      (when (stringp method)
;;                        (member method disable-tramp-backups)))))))

;;      (defun tramp-set-auto-save--check (original)
;;        (if (funcall backup-enable-predicate (buffer-file-name))
;;            (funcall original)
;;          (auto-save-mode -1)))

;;      (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'sanityinc-solarized-light t)
(toggle-frame-fullscreen)
;(setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OSX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns))
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none))

(when (memq window-system '(mac ns))
  (setq ispell-program-name "/usr/local/bin/hunspell")
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))  
  (setq exec-path (append exec-path '("/Library/TeX/texbin/")))
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2017/bin/x86_64-linux/"))  
(setq exec-path (append exec-path '("/usr/local/texlive/2017/bin/x86_64-linux/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ssh)
    (add-hook 'ssh-mode-hook
              (lambda ()
                (setq ssh-directory-tracking-mode t)
                (shell-dirtrack-mode t)
                (setq dirtrackp nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'indent-guide)
(indent-guide-global-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cquery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'lsp-mode)

;; (lsp-define-stdio-client
;;  ;; This can be a symbol of your choosing. It will be used as a the
;;  ;; prefix for a dynamically generated function "-enable"; in this
;;  ;; case: lsp-prog-major-mode-enable
;;  lsp-prog-major-mode
;;  "language-id"
;;  ;; This will be used to report a project's root directory to the LSP
;;  ;; server.
;;  (lambda () default-directory)
;;  ;; This is the command to start the LSP server. It may either be a
;;  ;; string containing the path of the command, or a list wherein the
;;  ;; car is a string containing the path of the command, and the cdr
;;  ;; are arguments to that command.
;;  '("/opt/cquery/bin/cquery"))

;; ;; Here we'll add the function that was dynamically generated by the
;; ;; call to lsp-define-stdio-client to the major-mode hook of the
;; ;; language we want to run it under.
;; ;;
;; ;; This function will turn lsp-mode on and call the command given to
;; ;; start the LSP server.
;; ;; (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

;; (require 'cquery)
;; (setq cquery-executable "/opt/cquery/bin/cquery")
;; (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))

;; (defun cquery//enable ()
;;   (condition-case nil
;;       (lsp-cquery-enable)
;;     (user-error nil)))

;; (setq cquery-project-roots '("~/repo/kahypar" ))
;; (with-eval-after-load 'projectile
;;   (setq projectile-project-root-files-top-down-recurring
;;         (append '("compile_commands.json"
;;                   ".cquery")
;;                 projectile-project-root-files-top-down-recurring)))

;;   (use-package cquery
;;     :commands lsp-cquery-enable
;;     :init (add-hook 'c-mode-hook #'cquery//enable)
;;           (add-hook 'c++-mode-hook #'cquery//enable))
;; ;; Also see lsp-project-whitelist lsp-project-blacklist cquery-root-matchers

;; (require 'company-lsp)
;; (push 'company-lsp company-backends)
;; (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
;; (setq cquery-extra-init-params '(:completion (:detailedLabel t)))




