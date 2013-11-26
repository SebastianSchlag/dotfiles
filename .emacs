;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load CEDET - taken from: https://gist.github.com/alexott/3930120
;; adapted according to: http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq cedet-root-path (file-name-as-directory "~/.emacs.d/cedet-bzr/trunk/"))

(load-file (concat cedet-root-path "cedet-devel-load.el"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))

;; select which submodes we want to activate
;; enables automatic bookmarking of tags that you edited, so you can return to them later with the semantic-mrub-switch-tags command;
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; enables global support for Semanticdb
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; activates automatic parsing of source code in the idle time;
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; activates displaying of possible name completions in the idle time. Requires that global-semantic-idle-scheduler-mode was enabled; 
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;; activates displaying of information about current tag in the idle time. Requires that global-semantic-idle-scheduler-mode was enabled.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode) 
;; activates mode when name of current tag will be shown in top line of buffer;
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; activates CEDET's context menu that is bound to right mouse button;
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode) 
;; activates highlighting of first line for current tag (function, class, etc.);
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;; enables global support for Semanticdb; 
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; activates use of separate styles for tags decoration. These styles are defined in the semantic-decoration-styles list; 
;;(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
;; activates highlighting of local names that are the same as name of tag under cursor
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
;; shows which elements weren't processed by current parser's rules;
;;(add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)
;; shows changes in the text that weren't processed by incremental parser yet.
;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-edits-mode)

;; Activate semantic
(semantic-mode 1)

;; load contrib library
(require 'eassist)
(require 'semantic/ia)
(require 'semantic/bovine/gcc) ;;find system-wide libs

;; Add additional directories like this:
;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

;; customisation of modes
(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol) ;; whatever the symbol you are typing, this hot key automatically complete it for you.
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu) ;; another way to complete the symbol you are typing
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline) ;; when you typed . or -> after an object name, use this key to show possible public member functions or data members.
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)  ;; visit the header file under cursor 
  (local-set-key "\C-cj" 'semantic-ia-fast-jump) ;; jump to the definition of the symbol under cursor 
  (local-set-key "\C-cq" 'semantic-ia-show-doc)  ;;  show the document of the symbol under cursor
  (local-set-key "\C-cs" 'semantic-ia-show-summary) ;; show a summary about the symbol under cursor
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle) ;; toggle between the implementation and a prototype of symbol under cursor
  (local-set-key "\C-c+" 'semantic-tag-folding-show-block) ;; unfold the block under cursor
  (local-set-key "\C-c-" 'semantic-tag-folding-fold-block) ;; fold the block under cursor
  (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all) ;; unfold all
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all) ;; fold all
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

;; enable support for gnu global
(when (cedet-gnu-global-version-check t)
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t))

;; enable ctags for some languages
(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))
 
;; SRecode
(global-srecode-minor-mode 1)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

;; Integration with imenu
(defun semantic-imenu-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'semantic-imenu-hook)

;; auto-complete intrgration
(defun c-mode-autocomplete-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'c-mode-autocomplete-cedet-hook)

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
(column-enforce-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
'(autopair-blink t)
'(setq autopair-autowrap t)

;; prevent the { (opening brace) character from being autopaired in C++ comments:
(add-hook 'c++-mode-hook
          #'(lambda ()
               (push ?{
                     (getf autopair-dont-pair :comment))))
;; disable pair creation when there already is a non-whitespace character after the cursor
(defun autopair-dont-if-point-non-whitespace (action pair pos-before)
  (if (or (eq 'opening action) (eq 'insert-quote action))
      (let ((delete? (save-excursion
         ;;move forward past the paired element
         (goto-char (+ (point) 1))
         (let* ((eol? (eq (point) (line-end-position)))
                (next-whitespace (save-excursion (search-forward " " (point-max) t) (point)))
                (next-char-is-whitespace? (eq next-whitespace (+ (point) 1)))
                (delete? (not (or eol? next-char-is-whitespace?))))
           delete?))))
        (if delete? (delete-char 1) 't))
    't))
(add-hook 'c++-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-dont-if-point-non-whitespace))))
(add-hook 'c-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-dont-if-point-non-whitespace))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)
(require 'cmake-project)
(require 'flymake-cursor)

(setq flymake-gui-warnings-enabled nil)
(set-variable 'flymake-start-syntax-check-on-newline nil)

(defun maybe-cmake-project-hook ()
  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
(add-hook 'c-mode-hook 'maybe-cmake-project-hook)
(add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

(defun turn-on-flymake-mode()
(if (and (boundp 'flymake-mode) flymake-mode)
    ()
  (flymake-mode t)))

(add-hook 'c-mode-common-hook (lambda () (turn-on-flymake-mode)))
(add-hook 'c++-mode-hook (lambda () (turn-on-flymake-mode)))

(defun cmake-project-current-build-command ()
  "Command line to compile current project as configured in the
build directory."
  (concat "cmake --build "
          (shell-quote-argument (expand-file-name
                                 cmake-project-build-directory)) " -- -j 1" ))

(defun cmake-project-flymake-init ()
  (list (executable-find "cmake")
        (list "--build" (expand-file-name cmake-project-build-directory) "--" "-j" "1" )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnipped and auto-complete config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet) ;; should be loaded before auto-complete
(yas-global-mode 1)

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;auto complete and corresponding cedet configuration
(defun my-c-mode-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic-raw))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-specific options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(column-number-mode t)

(add-to-list 'load-path "~/.emacs.d")
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Format the title-bar to always include the buffer name
(setq frame-title-format "%b")
;; Scroll line by line - http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(require 'smooth-scrolling)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time    
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling    
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


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
;; remember window size and buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remember size of window
(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
  (let (
        (framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry"))
        )

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file))))
  )

(defun load-framegeometry ()
  "Loads ~/.emacs.d/framegeometry which should load the previous frame's geometry."
  (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)))
  )

;; Special work to do ONLY when there is a window system being used
(if window-system
    (progn
      (add-hook 'after-init-hook 'load-framegeometry)
      (add-hook 'kill-emacs-hook 'save-framegeometry))
  )

;; eof

;; remember everything else on close
(desktop-save-mode 1) 
;; additionally store ecb window sizes after adjustment via ecb-store-window-sizes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 0)
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#657b83" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#fdf6e3"))
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(ecb-auto-activate t)
 '(ecb-layout-name "leftright2")
 '(ecb-layout-window-sizes (quote (("leftright2" (ecb-directories-buffer-name 0.10344827586206896 . 0.6296296296296297) (ecb-sources-buffer-name 0.10344827586206896 . 0.35185185185185186) (ecb-methods-buffer-name 0.19704433497536947 . 0.6296296296296297) (ecb-history-buffer-name 0.19704433497536947 . 0.35185185185185186)))))
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness nil)
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote ("~/repo/" ("/" "/"))))
 '(fci-rule-color "#eee8d5")
 '(show-paren-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198") (120 . "#268bd2") (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16") (220 . "#b58900") (240 . "#859900") (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682") (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))


;; adapt region and highlight face to match theme
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "#657b83" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(region ((t (:background "gray90"))))
 '(semantic-highlight-edits-face ((t (:background "gray90"))))
 '(yas-field-highlight-face ((t (:background "gray90")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'sanityinc-solarized-light t)

;; force restore of window sizes
(run-with-idle-timer 0.2 nil 'ecb-restore-window-sizes)


