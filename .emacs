(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load CEDET - taken from: https://gist.github.com/alexott/3930120
;; adapted according to: http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq cedet-root-path (file-name-as-directory "~/.emacs.d/cedet/"))

(load-file (concat cedet-root-path "cedet-devel-load.el"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))

;; select which submodes we want to activat
;; activates CEDET's context menu that is bound to right mouse button;
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode) 
;; activates highlighting of first line for current tag (function, class, etc.);
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; activates displaying of possible name completions in the idle time. Requires that global-semantic-idle-scheduler-mode was enabled; 
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;; activates highlighting of local names that are the same as name of tag under cursor
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
;; activates automatic parsing of source code in the idle time;
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; enables automatic bookmarking of tags that you edited, so you can return to them later with the semantic-mrub-switch-tags command;
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
; shows which elements weren't processed by current parser's rules;
;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;; enables global support for Semanticdb; 
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-breadcrumbs-mode)
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

;; customisation of modes
(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu) ;; whatever the symbol you are typing, this hot key automatically complete it for you.
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol) ;; another way to complete the symbol you are typing
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline) ;; when you typed . or -> after an object name, use this key to show possible public member functions or data members.
  (local-set-key "\C-c<" 'semantic-ia-fast-jump) ;; jump to the definition of the symbol under cursor
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)  ;; visit the header file under cursor 
  (local-set-key "\C-cj" 'semantic-complete-jump) ;; jump to the definition of the symbol under cursor 
  (local-set-key "\C-cq" 'semantic-ia-show-doc)  ;;  show the document of the symbol under cursor
  (local-set-key "\C-cs" 'semantic-ia-show-summary) ;; show a summary about the symbol under cursor
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle) ;; toggle between the implementation and a prototype of symbol under cursor
  (local-set-key "\C-c+" 'semantic-tag-folding-show-block) ;; unfold the block under cursor
  (local-set-key "\C-c-" 'semantic-tag-folding-fold-block) ;; fold the block under cursor
  (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all) ;; unfold all
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all) ;; fold all
  (local-set-key "\C-cr" 'semantic-symref)
  ;; rename local variable under cursor
  (local-set-key "\C-c\C-r" 'semantic-symref-rename-local-variable)
  (gtags-mode t)
  (local-set-key "\C-cf" 'gtags-find-tag)
  (flyspell-prog-mode)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)

(defun alexott/c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)
(add-hook 'c++-mode-hook 'alexott/c-mode-cedet-hook)

(defun my-c-mode-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
(add-hook 'c++-mode-hook 'my-c-mode-cedet-hook)

;; enable support for gnu global
(when (cedet-gnu-global-version-check t)
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t))
 
;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)
(semantic-load-enable-code-helpers)

(ede-cpp-root-project "hypergraph-partitioning"
                      :file "~/repo/schlag_git/CMakeLists.txt"
		      :include-path '("/"
				      "/src")
                      )

;; Integration with imenu
(defun semantic-imenu-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'semantic-imenu-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; Add melpa
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; initialize packages
;(package-initialize)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

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
;; CC Mode and C/C++ related configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)

;; Use Google's C Style:
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Allow us to get english language explanations of complex C++ declarations
;; To use, select a declaration such as "char*(*(*p)[5])(int)", and then type: "C-x cdecl"
(defun cdecl () 
  (interactive)
  (if (eq major-mode 'c++-mode) 
      (progn (interactive) (shell-command
                            (concat "c++decl explain \"" (buffer-substring (region-beginning)
                                                                           (region-end)) "\"")))
    (progn (interactive) (shell-command
                          (concat "cdecl explain \"" (buffer-substring (region-beginning)
                                                                       (region-end)) "\"")))
    )
  )

;;Create Header Guards with f12
(global-set-key [f12] 
  		'(lambda () 
  		   (interactive)
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

;; enforce column size
(load-file "~/.emacs.d/external/column-enforce-mode.el")
(require 'column-enforce-mode)
(add-hook 'c-mode-common-hook '100-column-rule)
(add-hook 'c++-mode-common-hook '100-column-rule)

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

(require 'auto-complete-auctex)


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
  (interactive "e")
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
  (interactive)
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
(add-hook 'c-mode-hook 'projectile-mode)
(add-hook 'c++-mode-hook 'projectile-mode)
;; let projectile be usable everywhere
(setq projectile-require-project-root nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cpputils-cmake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cpputils-cmake)
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))
;; OPTIONAL, somebody reported that they can use this package with Fortran
(add-hook 'c90-mode-hook (lambda () (cppcm-reload-all)))
;; OPTIONAL, avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
 '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))
;; OPTIONAL, some users need specify extra flags forwarded to compiler
(setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))
(setq cppcm-build-dirname "debug")

;; (require 'flymake)
;; (require 'flymake-cursor)

;; (defun turn-on-flymake-mode()
;;   (if (and (boundp 'flymake-mode) flymake-mode)
;;       ()
;;     (flymake-mode t)))

;; (add-hook 'c-mode-common-hook (lambda () (turn-on-flymake-mode)))
;; (add-hook 'c++-mode-hook (lambda () (turn-on-flymake-mode)))

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

;; treat .h and .hpp files as c++ files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'yank)       ; paste

;; magit
(global-set-key (kbd "<C-f12>") 'magit-status)
;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)
;; duplicate the current line or region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
;; toggle comment
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;;ecb
(global-set-key (kbd "C-c e") 'ecb-activate)
(global-set-key (kbd "C-c w") 'ecb-deactivate)

;; bind Backspace and Delete keys with M- and C- to special kill functions
(defun dove-backward-kill-word (&optional arg)
  "Backward kill word, but do not insert it into kill-ring"
  (interactive "P")
  (let (( end (point) )
        ( beg (progn (backward-word arg) (point)))
        )
    (delete-region beg end)
    )
  )

(defun dove-forward-kill-word (&optional arg)
  "Backward kill word, but do not insert it into kill-ring"
  (interactive "P")
  (let (( beg (point) )
        ( end (progn (forward-word arg) (point)))
        )
    (delete-region beg end)
    )
  )

(global-set-key [(meta backspace)] 'backward-kill-word)
(global-set-key [(control backspace)] 'dove-backward-kill-word)
(global-set-key [(meta delete)] 'kill-word)
(global-set-key [(control delete)] 'dove-forward-kill-word)

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
;; Smooth Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Scroll line by line
(setq redisplay-dont-pause t)
;; number of lines at the top and bottom of a window.
(setq scroll-margin 2)
;; Controls if scroll commands move point to keep its screen position unchanged.
(setq scroll-preserve-screen-position nil)   
(require 'smooth-scrolling)
 ;; four line at a time
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)))
 ;; accelerate scrolling
(setq mouse-wheel-progressive-speed 't)
 ;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; keyboard scroll four line at a time
(setq scroll-step 4)
;; number of lines at the top and bottom of a window.
(setq smooth-scroll-margin 3)
(setq smooth-scroll-strict-margins 't)

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
(add-hook 'c++-mode-hook 'turn-on-fic-mode) 

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
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 1.0)
 '(ac-auto-start 0)
 '(ac-non-trigger-commands (quote (*table--cell-self-insert-command electric-buffer-list)))
 '(ac-quick-help-prefer-pos-tip t)
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compilation-scroll-output (quote first-error))
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(ecb-auto-activate nil)
 '(ecb-layout-name "leftright2")
 '(ecb-layout-window-sizes (quote (("leftright2" (ecb-directories-buffer-name 0.0970464135021097 . 0.6333333333333333) (ecb-sources-buffer-name 0.0970464135021097 . 0.35) (ecb-methods-buffer-name 0.14345991561181434 . 0.6333333333333333) (ecb-history-buffer-name 0.14345991561181434 . 0.35)))))
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness nil)
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote ("~/repo/" ("/" "/"))))
 '(fci-rule-color "#eee8d5")
 '(flycheck-clang-language-standard "c++11")
 '(flymake-log-level -1)
 '(flymake-master-file-dirs (quote ("." "./src" "./UnitTest" "~/repo/schlag_git/src/application")))
 '(global-semantic-idle-summary-mode nil)
 '(magit-gitk-executable nil)
 '(magit-restore-window-configuration nil)
 '(magit-save-some-buffers (quote dontask))
 '(magit-server-window-for-commit nil)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(make-backup-files nil)
 '(nxhtml-autoload-web nil t)
 '(openwith-associations (quote (("\\.pdf\\'" "evince" (file)) ("\\.pdf\\'" "evince" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file)))))
 '(org-agenda-files (quote ("~/Dropbox/org/todo.org")))
 '(org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame) (vm-imap . vm-visit-imap-folder-other-frame) (gnus . org-gnus-no-new-news) (file . find-file) (wl . wl-other-frame))))
 '(powerline-default-separator (quote arrow-fade))
 '(rebox-style-loop (quote (370 243)))
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-displayor-tooltip-initial-max-tags 5)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198") (120 . "#268bd2") (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16") (220 . "#b58900") (240 . "#859900") (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682") (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16"))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'sanityinc-solarized-light t)

;;(setq debug-on-error t)
