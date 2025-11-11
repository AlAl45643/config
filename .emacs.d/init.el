;;; package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; Sometimes necessary
;; (straight-pull-recipe-repositories)


(use-package use-package-core
  :custom
  (use-package-always-defer t))


;; diminish needs to be here for it to work with straight.el
(use-package diminish
  :straight t
  )


;;; keybinds
;;;; rules
;;;;; keymaps to function
;; Keymaps                                  Default binding functions                     General.el functions                   Examples
;; Overriding terminal-local map                                                                                                 transient
;; Overriding local map
;; Point Property keymaps                                                                                                        magit diffs
;; Emulation mode map alists
;; Intercept keymaps              -        evil-make-intercept-map             -         general-def '(states) 'override        edebug-mode-map 
;; Local state keymaps            -        evil-local-set-key                  -         general-def '(states) 'local            
;; Minor-mode keymaps             -        evil-define-minor-mode-key          -         general-def '(states) minor-mode-map   evil org mode
;; Auxiliary/mode keymaps         -        evil-define-key                     -         general-def '(states) major-mode-map   evil commentary
;; Overriding keymaps             -        evil-make-overriding-map            -             
;; Global state keymaps           -        evil-global-set-key                 -         general-def '(states)...               vim keybindings
;; Minor mode overriding map alist -                                                      general-def 'override 
;; Buffer local keymap (general.el)-                                                      general-def 'local
;; Minor mode keymaps              -        define-key minor-mode-map           -         general-def keymap...                  tab-bar-mode
;; Local keymaps                   -        define-key major-mode-map           -         general-def keymap...                   
;; Global keymaps                  -        global-set-key                      -         general-def... 

;;;;; notes
;; + Using (evil-make-intercept-map) will not place the keymap in evil-intercept-maps as that has no effect but in the minor-mode-keymaps with an intercept state which has the same functionality.
;; + Keys defined with evil-define-key for a minor-mode and general-def depending on if it uses evil-define-key internally may not work immediately requiring #'evil-normalize-keymaps to be added to the mode's hook. Consider using general-add-hook for its transient feature in removing the function from the hook after running.

;;;;; keybind conventions
;; q ephermal quit
;; Z Q non epehermal quit
;; C-j C-k when j k isn't available
;; g j g k next same heading
;; ]] [[ next visible heading
;; C-S-j C-S-k next grouping or scroll

;;;;; guiding values 
;; + A keybind should be 1. useful 2. memorable 3. shorter than less used and longer than more used keybinds 4. easy to press
;; + Structure makes memorable keybinds.
;; + Keybinds can be separated into three semantic categories. The first being keybinds that are only useful within a context (mode), the second being keybinds that are useful in any context (mode), and the third being keybinds that are somewhere between. 
;; + Emacs prefixes should be incorporated into your keybind scheme so that you can discover more useful keybinds and that your keybind scheme if not optimal is most likely useful.

;;;;; binding rules
;; + SPC prefix commands should be commands with higher frequency of use than , prefix commands.
;; + The , prefix and the SPC prefix are restricted to global commands while the \ prefix is restricted to major mode.
;; + Inbetween keybinds shall be global through functions or local depending on whichever solution is cleaner.
;; + All keybinds shall be categorized by the module they are bounded in. 
;; + Keybinds must be changed in notes before they are changed in code.
;; + Incorporate emacs prefixes such as C-h and C-x.
;; + Prefer binding to prefix-maps when incorporating emacs prefixes.
;; + When defining a keybind for a command follow these steps: 
;;    1. Is the command always useful? If it is then bind it in a global manner. If not, which mode and state is it useful in?
;;    2. What is the shortest memorable keybind you can think of?
;;    3. Is the keybind available? if it is, then bind the keybind to the command. if not, then is the command currently bound less used than our command? if it is, then replace the command and redo step 7 for the command you replaced. if it is not, then think of the next shortest memorable keybind and redo step 3.
;; +. Only defer load for keybinds when keymap isn't available or commands aren't needed until package is loaded.
;; +. Don't put functions in after!
;;
;; SPC prefix semantic rules
;; 1. Only infix keys are allowed in the top level SPC- menu.
;; 2. Infix keys should be a unique and specific keyword for each command it contains.
;; 3. Commands with similar functionality and keyword but in different infixes should have the same key.
;;
;; , prefix semantic rules
;; 1. No infix keys except modifier keys.

;;;; textual keybinds
;;;;; evil-intercept-maps
;; all (C-) , (general-simulate-key "C-c")
;; normal j evil-next-visual-line
;; normal k evil-previous-visual-line

;;;;; my-main-leader-evil-map
;; all (C-) SPC s switch-to-buffer
;; all (C-) SPC r my-run-program
;; all (C-) SPC t popper-toggle
;; all (C-) SPC e my-save-all-buffers
;; all (C-) SPC f find-file
;; all (C-) SPC y copy-file 
;; all (C-) SPC l vterm
;; all (C-) SPC d dired
;; all (C-) SPC n eval-defun
;; all (C-) SPC i imenu-list-smart-toggle
;; all (C-) SPC k kill-buffer
;; all (C-) SPC v my-eval-expression 
;; all (C-) SPC u my-browse-documentation 
;; all (C-) SPC c toggle-truncate-lines 
;; all (C-) SPC 1 my-window-bookmark-home
;; all (C-) SPC m make-frame-command
;; all (C-) SPC o my-online-search
;; all (C-) SPC g magit-status
;; all (C-) SPC p org-pomodoro 
;; all (C-) SPC a org-agenda 
;; all (C-) SPC h my-go-to-help-buffer
;; all (C-) SPC b remember
;;;;; my-second-leader-map
;; all (C-) \ e t my-test-code 
;; all (C-) \ e b my-build-code
;; all (C-) \ e a my-code-action
;; all (C-) \ e d dape
;; all (C-) \ e m sharper-main-transient
;; all (C-) \ e n my-code-rename
;; all (C-) \ s t toggle-theme
;; all (C-) \ s e my-persist-eldoc
;; all (C-) \ s n remember-notes
;; all (C-) \ p p completion-at-point
;; all (C-) \ f d delete-file
;; all (C-) \ j s project-switch-to-buffer
;; all (C-) \ j f project-find-file
;; all (C-) \ j j project-switch-project
;; all (C-) \ j h project-search
;; all (C-) \ v s eval-last-sexp
;; all (C-) \ v b eval-buffer
;; all (C-) \ v r eval-region
;; all (C-) \ m (general-simulate-key "C-c")
;; all (C-) \ h help-map
;; all (C-) \ x ctl-x-map

;;;;; transient-sticky-map
;; q transient-quit-seq

;;;;; evil-normal-state-map evil-insert-state-map
;; normal = = format-buffer
;; normal C-S-d evil-scroll-up
;; [ x xref-go-back
;; ] x xref-go-forward
;; g d xref-find-definitions
;; TAB smart-tab
;; g h help-at-point
;; C-w C-v my-evil-window-vsplit-left

;;;;; help-map
;; normal insert C-h f helpful-callable
;; normal insert C-h v helpful-variable
;; normal insert C-h k helpful-key
;; normal insert C-h x helpful-command
;; normal insert C-h F helpful-function

;;;;; vertico-map
;; C-j vertico-next
;; C-k vertico-previous
;; C-b evil-backward char
;; C-f evil-forward char
;; C-S-j scroll-down-command
;; C-S-k scroll-up-command

;;;;; corfu-mode-map
;; insert M-SPC corfu-insert-separator
;; insert RET nil

;;;;; corfu-popupinfo-map
;; C-h corfu-popupinfo-toggle
;; C-S-j corfu-popupinfo-scroll-up
;; C-S-k corfu-popupinfo-scroll-down

;;;;; python-mode-map csharp-mode-map
;; normal insert (C-) , <  dape-stack-select-up      
;; normal insert (C-) , >  dape-stack-select-down    
;; normal insert (C-) , B  dape-breakpoint-remove-all
;; normal insert (C-) , D  dape-disconnect-quit      
;; normal insert (C-) , M  dape-disassemble          
;; normal insert (C-) , R  dape-repl                 
;; normal insert (C-) , S  dape-select-stack         
;; normal insert (C-) , b  dape-breakpoint-toggle    
;; normal insert (C-) , e  dape-breakpoint-expression
;; normal insert (C-) , f  dape-restart-frame        
;; normal insert (C-) , h  dape-breakpoint-hits      
;; normal insert (C-) , i  dape-info                 
;; normal insert (C-) , l  dape-breakpoint-log       
;; normal insert (C-) , m  dape-memory               
;; normal insert (C-) , p  dape-pause                
;; normal insert (C-) , q  dape-quit                 
;; normal insert (C-) , r  dape-restart              
;; normal insert (C-) , t  dape-select-thread        
;; normal insert (C-) , u  dape-until                
;; normal insert (C-) , w  dape-watch-dwim           
;; normal insert (C-) , x  dape-evaluate-expression  

;;;;; dape-active-mode-map
;; insert normal F5 dape-continue
;; insert normal F6 dape-step-out
;; insert normal F7 dape-step-in
;; insert normal F8 dape-next
;; normal Z Q dape-quit

;;;;; edebug-mode-map
;; insert normal F8 edebug-step-mode
;; insert normal F5 edebug-go-mode
;; insert normal F6 edebug-step-out
;; insert normal F7 edebug-step-in
;; normal Z Q top-level
;; normal q top-level
;; insert normal (C-) , n       edebug-next-mode
;; insert normal (C-) , G       edebug-Go-nonstop-mode
;; insert normal (C-) , t       edebug-trace-mode
;; insert normal (C-) , T       edebug-Trace-fast-mode
;; insert normal (C-) , c       edebug-continue-mode
;; insert normal (C-) , C       edebug-Continue-fast-mode
;; insert normal (C-) , f       edebug-forward-sexp
;; insert normal (C-) , h       edebug-goto-here
;; insert normal (C-) , I       edebug-instrument-callee
;; insert normal (C-) , q       top-level
;; insert normal (C-) , Q       edebug-top-level-nonstop
;; insert normal (C-) , a       abort-recursive-edit
;; insert normal (C-) , S       edebug-stop
;; insert normal (C-) , b       edebug-set-breakpoint
;; insert normal (C-) , u       edebug-unset-breakpoint
;; insert normal (C-) , U       edebug-unset-breakpoints
;; insert normal (C-) , B       edebug-next-breakpoint
;; insert normal (C-) , x       edebug-set-conditional-breakpoint
;; insert normal (C-) , X       edebug-set-global-break-condition
;; insert normal (C-) , D       edebug-toggle-disable-breakpoint
;; insert normal (C-) , r       edebug-previous-result
;; insert normal (C-) , e       edebug-eval-expression
;; insert normal (C-) , C-x C-e edebug-eval-last-sexp
;; insert normal (C-) , E       edebug-visit-eval-list
;; insert normal (C-) , w       edebug-where
;; insert normal (C-) , v       edebug-view-outside        ; maybe obsolete??
;; insert normal (C-) , p       edebug-bounce-point
;; insert normal (C-) , P       edebug-view-outside        ; same as v
;; insert normal (C-) , W       edebug-toggle-save-windows
;; insert normal (C-) , ?       edebug-help
;; insert normal (C-) , d       edebug-pop-to-backtrace
;; insert normal (C-) , -       negative-argument

;;;;; edebug-eval-mode-map
;; insert normal RET edebug-update-eval-list
;; insert normal <home> edebug-where

;;;;; magit-mode-map magit-section-mode-map
;; insert normal visual ] ] magit-section-forward
;; [ [ magit-section-backward

;;;;; org-mode-map
;; C-<tab> org-cycle
;; C-<iso-lefttab> org-shifttab
;; normal insert (C-) , t org-match-sparse-tree-heading

;;;;; org-remark-mode-map
;; visual , 1 org-remark-mark-understand
;; visual , 2 org-remark-mark-keyword
;; visual , 3 org-remark-mark-sentence
;; visual , 4 org-remark-mark-argument
;; visual enter org-remark-mark-highlight
;; normal o org-remark-open
;; normal ]m org-remark-view-next
;; normal r org-remark-delete
;;;;; yas-keymap
;; C-<tab> yas-next-field
;; C-<iso-lefttab> yas-prev-field

;;;;; imenu-list-major-mode-map
;; normal <return> imenu-goto-node


;;;;; inferior-python-mode-map
;; insert normal (C-) , ? my-python-eldoc-at-point

;;;;; dired-mode-map
;; q evil-window-delete
;;;;; ispell-minor-keymap
;; insert normal (C-) d ispell-word
;;;; code keybinds
;;;;; keybind dependencies
(use-package general
  :straight t
  :demand t
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)
  )

(use-package evil
  :straight t
  :demand t
  :init
  (progn
    (setq evil-undo-system 'undo-redo)
    ;; `evil-collection' assumes `evil-want-keybinding' is set to
    ;; `nil' before loading `evil' and `evil-collection'
    (setq evil-want-keybinding nil))
  (setq evil-want-minibuffer t)
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  (setq evil-insert-state-message nil)
  (setq evil-emacs-state-message nil)
  (setq evil-replace-state-message nil)
  (setq evil-visual-state-message nil)
  (setq evil-mode-line-format nil)
  (setq evil-visual-char-message nil)
  (setq evil-visual-line-message nil)
  (setq evil-visual-block-message nil)
  (setq evil-visual-screen-line-message nil)
  (evil-mode 1)
  )

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and diff-hl have loaded)
    (after! (magit diff-hl) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. No-ops for package that are disabled by the user (via `package!') or not
   installed yet.
2. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

;;;;; evil-intercept-maps
(general-def '(normal visual) 'override
  "," (general-simulate-key "C-c")
  "<menu>" (general-simulate-key "C-c")
  "C-c ," 'evil-repeat-find-char-reverse)

(general-def 'insert 'override
  "C-," (general-simulate-key "C-c")
  "C-<menu>" (general-simulate-key "C-c"))

(general-def '(normal motion) 
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

;;;;; my-main-leader-evil-map
(defun my-run-program ()
  "Run program at point depending on mode."
  (interactive)
  (cond
   ((equal major-mode (or 'python-ts-mode 'python-mode))
    (if (not (get-buffer "*Python*"))    
        (run-python "python3 -i" nil t)
      (let ((kill-buffer-query-functions nil)
            (python (get-buffer-window "*Python*")))
        (cond
         (python (with-selected-window python
                   (kill-buffer-and-window)))
         ((get-buffer "*Python*") (kill-buffer "*Python*"))))
      (run-python "python3 -i" nil t)
      ;; Without this line python returns NameError: name '__PYTHON_EL_eval_file' is not defined
      (sit-for 1)
      (python-shell-send-buffer)           
      (pop-to-buffer "*Python*")           
      ))
   ((equal major-mode (or 'csharp-ts-mode 'csharp-mode)) (sharper-transient-run))
   ((equal major-mode 'LaTeX-mode) (call-interactively #'TeX-command-master))
   (t (message "No program to run"))))

(defun my-eval-expression ()
  "Eval expression depending on mode."
  (interactive)
  (cond
   ((and (featurep 'dape) dape-active-mode)
    (call-interactively #'dape-evaluate-expression))
   ((and (featurep 'edebug) edebug-mode)
    (call-interactively #'edebug-eval-expression))
   (t (call-interactively #'eval-expression)))
  )

(defun my-save-all-buffers ()
  "Save all buffers."
  (interactive)
  (save-some-buffers "!"))

(defun my-browse-csharp-docs (x)
  "Browse csharp docs by searching duckduckgo with site: learn.microsoft.com."
  (interactive "sSearch: ")
  (browse-url (browse-url (concat "https://duckduckgo.com/?q=" x "+site%3Alearn.microsoft.com"))))

(defun my-browse-python-docs (x)
  "Browse python docs by search duckduckgo with site: docs.python.org"
  (interactive "sSearch: ")
  (browse-url (concat "https://duckduckgo.com/?q=" x "+site%3Adocs.python.org")))


(defun my-browse-documentation ()
  "Browse documentation for mode."
  (interactive)
  (cond
   ((equal major-mode 'emacs-lisp-mode)
    (info-other-window "elisp"))
   ((equal major-mode (or 'csharp-ts-mode 'csharp-mode))
    (call-interactively #'my-browse-csharp-docs))
   ((equal major-mode (or 'php-ts-mode php-mode))
    (php-browse-manual))
   ((equal major-mode (or 'python-ts-mode 'python-mode))
    (call-interactively #'my-browse-python-docs))))


(defun my-window-bookmark-home ()
  "Move to home bookmark."
  (interactive)
  (bookmark-maybe-load-default-file)
  ;; work around to get org-agenda buffer working in bookmarks
  (org-agenda-list)
  (bookmark-jump "Burly: home"))



(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator)
 :prefix-map 'my-main-leader-map
 :prefix "SPC"
 :non-normal-prefix "C-SPC")

(general-create-definer main-leader-definer
  :keymaps 'my-main-leader-map)

(main-leader-definer
  "s" 'switch-to-buffer
  "r" 'my-run-program
  "t" 'popper-toggle
  "e" 'my-save-all-buffers
  "p" 'org-pomodoro
  "f" 'find-file
  "l" 'vterm
  "d" 'dired
  "n" 'eval-defun
  "i" 'consult-imenu
  "g" 'magit-project-status
  "k" 'kill-buffer
  "v" 'my-eval-expression
  "a" 'org-agenda-list
  "u" 'my-browse-documentation
  "c" 'toggle-truncate-lines
  "m" 'make-frame-command
  "1" 'my-window-bookmark-home
  "o" 'my-online-search
  "b" 'remember
  )


;;;;; my-second-leader-evil-map
(defun my-online-search (x)
  "Search duckduckgo with X"
  (interactive "sSearch: ")
  (browse-url (concat "https://duckduckgo.com/?q=" x))
  )

(defun my-test-code ()
  "Run test cases for program."
  (interactive)
  (cond
   ((equal major-mode (or 'csharp-ts-mode 'csharp-mode))
    (sharper-transient-test))
   (t (message "%s" "No test cases to run."))))

(defun my-build-code ()
  "Build program."
  (interactive)
  (cond
   ((equal major-mode (or 'csharp-ts-mode 'csharp-mode))
    (sharper-transient-build))
   (t (message "%s" "No program to build."))))

(defun my-code-action ()
  "Perform code actions at point."
  (interactive)
  (cond
   ((and (featurep 'eglot) eglot--managed-mode) (call-interactively 'eglot-code-actions))
   (t (message "%s" "No code actions at point."))))


(defun my-cycle-theme ()
  "Cycle through preferred themes."
  (interactive)
  (cond ((equal custom-enabled-themes '(modus-vivendi)) (disable-theme 'modus-vivendi) (load-theme 'tango-dark))
        ((equal custom-enabled-themes '(tango-dark)) (disable-theme 'tango-dark) (load-theme 'modus-operandi-tinted))
        ((equal custom-enabled-themes '(modus-operandi-tinted)) (disable-theme 'modus-operandi-tinted) (load-theme 'modus-vivendi-tinted))
        ((equal custom-enabled-themes '(modus-vivendi-tinted)) (disable-theme 'modus-vivendi-tinted) (load-theme 'modus-vivendi))))

(defun my-persist-eldoc (interactive)
  (interactive (list t))
  (if (get-buffer "*persisted eldoc*")
      (kill-buffer "*persisted eldoc*"))
  (with-current-buffer eldoc--doc-buffer
    (let ((s (buffer-string)))
      (with-current-buffer (generate-new-buffer "*persisted eldoc*")
        (insert s)
        (display-buffer (current-buffer))
        (set-window-start (get-buffer-window "*persisted eldoc*") 0)))))

(defun my-code-rename ()
  (interactive)
  (cond
   ((and (featurep 'eglot) eglot--managed-mode) (call-interactively 'eglot-rename))
   (t (message "%s" "Nothing to rename at point."))))

(general-define-key
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator)
 :prefix-map 'my-second-leader-map
 :prefix "\\"
 :non-normal-prefix "C-\\")

(defmacro +general-global-menu! (name prefix-key &rest body)
  "Create a definer named +general-global-NAME.
  Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
  (declare (indent 2))
  `(progn
     ;; find a way to replace this with keymap based replacement
     (which-key-add-key-based-replacements (concat "\\ " ,prefix-key) ,name)
     (which-key-add-key-based-replacements (concat "C-\\ " ,prefix-key) ,name)
     (general-define-key
      :keymaps 'my-second-leader-map
      :prefix-map ',(intern (concat "my-" name "-map"))
      :prefix ,prefix-key)
     (general-create-definer ,(intern (concat "+general-global-" name))
       :keymaps ',(intern (concat "my-" name "-map")))
     (,(intern (concat "+general-global-" name))
      ,@body)))

(+general-global-menu! "code" "e"
  "t" 'my-test-code
  "b" 'my-build-code
  "a" 'my-code-action
  "d" 'dape
  "n" 'my-code-rename
  "k" 'docker
  )

(+general-global-menu! "miscellaneous" "s"
  "t" 'my-cycle-theme
  "e" 'my-persist-eldoc
  "n" 'remember-notes)
(+general-global-menu! "eval" "v"
  "s" 'eval-last-sexp
  "b" 'eval-buffer
  "r" 'eval-region)

(+general-global-menu! "completion" "p"
  "p" 'completion-at-point)

(+general-global-menu! "project" "j"
  "f" 'project-find-file
  "s" 'project-switch-to-buffer
  "j" 'project-switch-project
  "h" 'project-search)

(+general-global-menu! "file" "f"
  "d" 'delete-file
  "c" 'copy-file)

(general-define-key
 :keymaps 'my-second-leader-map
 "x" `("extend" . ,ctl-x-map)
 )


;;;;; evil-normal-state-map evil-insert-state-map
(defun smart-tab ()
  (interactive)
  (cond ((buffer-local-value 'vertico--input (current-buffer)) (vertico-insert))
        ((minibufferp) (let ((res (run-hook-wrapped 'completion-at-point-functions #'completion--capf-wrapper 'all)))
                         (if res
                             (completion-at-point)
                           (hippie-expand nil)))) 
        ((derived-mode-p 'eshell-mode) (let ((res (run-hook-wrapped 'completion-at-point-functions #'completion--capf-wrapper 'all)))
                                         (if res
                                             (completion-at-point)
                                           (hippie-expand nil))))
        ((and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)) (corfu-insert))
        (mark-active (indent-region (region-beginning) (region-end)))
        ((looking-at "\\_>") (hippie-expand nil))
        (t (indent-for-tab-command))))

(defun my-format-buffer ()
  (interactive)
  (cond
   ((and (featurep 'eglot) eglot--managed-mode) (call-interactively #'eglot-format-buffer))
   (t (indent-region (point-min) (point-max)))))


(defun my-help-at-point ()
  (interactive)
  (cond
   ((and (featurep 'eglot) eglot--managed-mode) 
    (call-interactively 'eldoc-doc-buffer))
   ((equal major-mode 'inferior-python-mode)
    (my-python-eldoc-at-point))
   (t (save-selected-window (helpful-at-point)))))

(evil-define-command my-evil-window-vsplit-left (&optional count file)
  "Split the current window vertically, COUNT columns width,
editing a certain FILE. The new window will be created to the
left. If COUNT and `evil-auto-balance-windows'are both non-nil
then all children of the parent of the splitted window are
rebalanced."
  :repeat nil
  (interactive "<wc><f>")
  (split-window (selected-window) (when count (- count))
                'left)
  (when (and (not count) evil-auto-balance-windows)
    (balance-windows (window-parent)))
  (when file
    (evil-edit file)))

(general-def 'normal
  "=" (general-key-dispatch 'evil-indent
        "=" 'my-format-buffer)
  "C-S-d" 'evil-scroll-up
  "[ x" 'xref-go-back
  "] x" 'xref-go-forward
  "g d" 'xref-find-definitions
  "g h" 'my-help-at-point
  "C-w C-v" 'my-evil-window-vsplit-left
  )

(after! evil-easymotion
  (general-def 'normal
    ;; "s" 'evilem-motion-find-char
    ;; "S" 'evilem-motion-find-char-backward
    ;; "g f" 'evilem-motion-find-char
    ;; "g F" 'evilem-motion-find-char-backward
    "g s" evilem-map
    ))

(general-def 'insert
  "TAB" 'smart-tab)


;;;;; help-map
(defun my-helpful-callable-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-callable)))

(defun my-helpful-variable-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-variable)))

(defun my-helpful-key-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-key)))

(defun my-helpful-command-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-command)))

(defun my-helpful-function-save-window ()
  (interactive)
  (save-selected-window (call-interactively 'helpful-function)))

(after! help    
  (general-def help-map
    "C-h f" 'my-helpful-callable-save-window
    "C-h v" 'my-helpful-variable-save-window
    "C-h k" 'my-helpful-key-save-window
    "C-h x" 'my-helpful-command-save-window
    "C-h F" 'my-helpful-function-save-window
    ))

;;;;; vertico-map
(after! vertico
  (general-def '(insert normal) vertico-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous
    "C-S-j" 'scroll-up-command
    "C-b" 'evil-backward-char
    "C-f" 'evil-forward-char
    "C-S-k" 'scroll-down-command
    )
  )


;;;;; corfu-mode-map
(after! corfu
  (general-def corfu-map
    "M-SPC" 'corfu-insert-separator
    "RET" nil
    ))

;;;;; corfu-popupinfo-map
(after! corfu-popupinfo
  (general-def corfu-popupinfo-map
    "C-h" 'corfu-popupinfo-toggle
    "C-S-j" 'corfu-popupinfo-scroll-up
    "C-S-k" 'corfu-popupinfo-scroll-down))

;;;;; dape-maps*
;;;;;; dape-active-mode-map
(after! dape
  (general-def '(normal insert) 'override
    :predicate 'dape-active-mode
    "<f5>" 'dape-continue
    "<f6>" 'dape-step-out
    "<f7>" 'dape-step-in
    "<f8>" 'dape-next)
  
  (general-def 'normal 'override
    :predicate 'dape-active-mode
    "Z Q" '("dape-quit" . dape-quit))

  (push (cons '("Z Q" . nil)
              (lambda (kb)
                (cons (car kb)
                      (if dape-active-mode
                          "dape-quit"
                        "evil-quit"))))
        which-key-replacement-alist)
  )

;;;;;; python-ts-mode-map
(after! python
  (general-def python-ts-mode-map
    "C-c B" 'dape-breakpoint-remove-all
    "C-c D" 'dape-disconnect-quit
    "C-c M" 'dape-disassemble
    "C-c R" 'dape-repl
    "C-c S" 'dape-select-stack
    "C-c b" 'dape-breakpoint-toggle
    "C-c e" 'dape-breakpoint-expression
    "C-c f" 'dape-restart-frame
    "C-c h" 'dape-breakpoint-hits
    "C-c i" 'dape-info
    "C-c l" 'dape-breakpoint-log
    "C-c m" 'dape-memory
    "C-c p" 'dape-pause
    "C-c q" 'dape-quit
    "C-c r" 'dape-restart
    "C-c t" 'dape-select-thread
    "C-c u" 'dape-until
    "C-c w" 'dape-watch-dwim
    )
  )

;;;;;; csharp-ts-mode
(after! csharp-mode
  (general-def csharp-ts-mode-map
    "C-c B" 'dape-breakpoint-remove-all
    "C-c D" 'dape-disconnect-quit
    "C-c M" 'dape-disassemble
    "C-c R" 'dape-repl
    "C-c S" 'dape-select-stack
    "C-c b" 'dape-breakpoint-toggle
    "C-c e" 'dape-breakpoint-expression
    "C-c f" 'dape-restart-frame
    "C-c h" 'dape-breakpoint-hits
    "C-c i" 'dape-info
    "C-c l" 'dape-breakpoint-log
    "C-c m" 'dape-memory
    "C-c p" 'dape-pause
    "C-c q" 'dape-quit
    "C-c r" 'dape-restart
    "C-c t" 'dape-select-thread
    "C-c u" 'dape-until
    "C-c w" 'dape-watch-dwim
    ))

;;;;; edebug-mode-map edebug-eval-mode-map
(defun set-edebug-map ()
  "Set the edebug mode map"
  (setq edebug-mode-map (make-sparse-keymap))
  (general-def '(insert normal) edebug-mode-map
    "<f8>" 'edebug-step-mode
    "<f5>" 'edebug-go-mode
    "<f6>" 'edebug-step-out
    "<f7>" 'edebug-step-in
    )
  (general-def 'normal edebug-mode-map
    "Z Q" 'top-level
    "q" 'top-level)
  (general-def edebug-mode-map
    "C-c n"       'edebug-next-mode
    "C-c G"       'edebug-Go-nonstop-mode
    "C-c t"       'edebug-trace-mode
    "C-c T"       'edebug-Trace-fast-mode
    "C-c c"       'edebug-continue-mode
    "C-c C"       'edebug-Continue-fast-mode

    ;;"f"       #'edebug-forward ; not implemented
    "C-c f"       'edebug-forward-sexp
    "C-c h"       'edebug-goto-here

    "C-c I"       'edebug-instrument-callee

    ;; quitting and stopping
    "C-c q"       'top-level
    "C-c Q"       'edebug-top-level-nonstop
    "C-c a"       'abort-recursive-edit
    "C-c S"       'edebug-stop

    ;; breakpoints
    "C-c b"       'edebug-set-breakpoint
    "C-c u"       'edebug-unset-breakpoint
    "C-c U"       'edebug-unset-breakpoints
    "C-c B"       'edebug-next-breakpoint
    "C-c x"       'edebug-set-conditional-breakpoint
    "C-c X"       'edebug-set-global-break-condition
    "C-c D"       'edebug-toggle-disable-breakpoint

    ;; evaluation
    "C-c r"       'edebug-previous-result
    "C-c e"       'edebug-eval-expression
    "C-c C-x C-e" 'edebug-eval-last-sexp
    "C-c E"       'edebug-visit-eval-list

    ;; views
    "C-c w"       'edebug-where
    "C-c v"       'edebug-view-outside        ; maybe obsolete??
    "C-c p"       'edebug-bounce-point
    "C-c P"       'edebug-view-outside        ; same as v
    "C-c W"       'edebug-toggle-save-windows

    ;; misc
    "C-c ?"       'edebug-help
    "C-c d"       'edebug-pop-to-backtrace

    "C-c -"       'negative-argument

    ;; statistics
    "C-c ="       'edebug-temp-display-freq-count)

  (general-def '(normal insert) edebug-eval-mode-map
    "RET" 'edebug-update-eval-list
    )
  )

(after! edebug
  (add-hook 'edebug-setup-hook #'set-edebug-map)
  )



;;;;; magit-mode-map magit-section-mode-map
(after! magit
  (general-def '(visual normal) magit-mode-map
    "] ]" 'magit-section-forward
    "[ [" 'magit-section-backward)
  (general-def 'normal magit-section-mode-map
    "] ]" 'magit-section-forward
    "[ [" 'magit-section-backward))

(defun org-match-sparse-tree-heading ()
  "Check heading of each tag."
  (interactive)
  (org-save-outline-visibility nil
    (org-match-sparse-tree)
    ))

;;;;; org-mode-map
(after! org
  (general-def org-mode-map
    "C-<tab>" 'org-cycle
    "C-<iso-lefttab>" 'org-shifttab
    "C-c t" 'org-match-sparse-tree-heading))


;;;;; org-remark-mode-map
(after! org-remark
  (org-remark-create "understand"
                     '(:background "#1d3c25"))

  (org-remark-create "keyword"
                     '(:strike-through "cyan"))

  (org-remark-create "sentence"
                     '(:underline "white"))

  (org-remark-create "argument"
                     '(:overline "red"))

  (org-remark-create "highlight"
                     '(:foreground "#fce94f"))

  (general-def 'visual org-remark-mode-map 
    "<return>" 'org-remark-mark-highlight
    "C-c 1" 'org-remark-mark-understand
    "C-c 2" 'org-remark-mark-keyword
    "C-c 3" 'org-remark-mark-sentence
    "C-c 4" 'org-remark-mark-argument)
  (general-def 'normal org-remark-mode-map
    "o" 'org-remark-open
    "]m" 'org-remark-view-next
    "[m" 'org-remark-view-prev
    "r" 'org-remark-delete))

;;;;; yas-keymap
(after! yasnippet
  (general-def yas-keymap
    "<tab>" nil
    "TAB" nil
    "C-<tab>" 'yas-next-field
    "C-<iso-lefttab>" 'yas-prev-field))

;;;;; imenu-list-major-mode-map
(after! imenu-list
  (general-def 'normal imenu-list-major-mode-map
    "<return>" (lambda (arg) (interactive "P")
                 (if arg
                     (progn (evil-goto-line arg) (imenu-list-goto-entry) (evil-scroll-line-to-top (line-number-at-pos)))
                   (imenu-list-goto-entry) (evil-scroll-line-to-top (line-number-at-pos))))
    ))


;;;;; doc-view-mode-map
(after! doc-view
  (general-def 'normal doc-view-mode-map
    "j" 'doc-view-next-line-or-next-page
    "k" 'doc-view-previous-line-or-previous-page))

;;;;; python-mode-map
(defun my-python-repl ()
  "Go to Python REPL and create it if needed."
  (interactive)
  (if (python-shell-get-buffer)
      (python-shell-switch-to-shell)
    (run-python "python3 -i" nil t)))

(defun my-python-keybinds ()
  (after! python
    (general-def 'normal python-ts-mode-map
      "g z" 'my-python-repl)))

(add-hook 'python-ts-mode-hook #'my-python-keybinds)
;;;;; inferior-python-mode-map
(defun my-python-eldoc-at-point ()
  "Get python documentation in eldoc with `python-eldoc--get-doc-at-point'."
  (interactive)
  (call-interactively 'eldoc-doc-buffer)
  (eldoc-display-in-buffer `((,(python-eldoc-function))) nil)
  )
(after! python
  (general-def inferior-python-mode-map
    "C-c ?" 'my-python-eldoc-at-point)) 

;;;;; dired-mode-map
(defun my-dired-find-file ()
  "In Dired, visit the file or directory named on this line."
  (interactive nil dired-mode)
  (my-dired--find-possibly-alternative-file (dired-get-file-for-visit)))

(defun my-dired--find-possibly-alternative-file (file)
  "Find FILE, but respect `dired-kill-when-opening-new-dired-buffer'."
  (if (and dired-kill-when-opening-new-dired-buffer
           (file-directory-p file)
           (< (length (get-buffer-window-list)) 2))
      (progn
        (set-buffer-modified-p nil)
        (dired--find-file #'find-alternate-file file))
    (my-dired--find-file file)))

(defun my-dired--find-file (file)
  "Call FIND-FILE-FUNCTION on FILE, but bind some relevant variables."
  ;; Bind `find-file-run-dired' so that the command works on directories
  ;; too, independent of the user's setting.
  (let ((find-file-run-dired t)
        ;; This binding prevents problems with preserving point in
        ;; windows displaying Dired buffers, because reverting a Dired
        ;; buffer empties it, which changes the places where the
        ;; markers used by switch-to-buffer-preserve-window-point
        ;; point.
        (switch-to-buffer-preserve-window-point
         (if dired-auto-revert-buffer
             nil
           switch-to-buffer-preserve-window-point)))
    (org-display-buffer-in-window (find-file-noselect file) `((window . ,(window-in-direction 'right))))
    (delete-window)
    ))

(defun my-dired-keybinds ()
  "Create keybinds for dired."
  (general-def 'normal dired-mode-map
    "q" 'evil-window-delete
    "RET" 'my-dired-find-file))

(add-hook 'dired-mode-hook #'my-dired-keybinds)
;;;;; ispell-minor-keymap
(after! ispell
  (general-def ispell-minor-keymap
    "C-c d" 'ispell-word
    "RET" nil
    "SPC" nil))
;;;;; fixing overrides

(general-unbind iedit-mode-keymap
  "TAB"
  "<tab>"
  "<backtab")



;;; packages
;;;; evil
(use-package evil-easymotion
  :straight t
  :demand t)
(use-package evil-collection
  :straight t
  :demand t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)
  :diminish evil-collection-unimpaired-mode
  )
(use-package evil-owl
  :diminish evil-owl-mode
  :straight t
  :custom
  (evil-owl-display-method 'posframe)
  (evil-owl-extra-posframe-args '(:width 50 :height 20))
  (evil-owl-max-string-length 50)
  (evil-owl-idle-delay 0.5)
  :init
  (evil-owl-mode)
  )
(use-package evil-mc
  :straight t
  :init
  (global-evil-mc-mode)
  :diminish evil-mc-mode
  )
(use-package evil-commentary
  :straight t
  :hook (prog-mode . evil-commentary-mode)
  :diminish evil-commentary-mode
  )
(defun my-evil-multiedit-maintain-visual-cursor-advice (func &rest args)
  (if evil-visual-state-minor-mode
      (set-window-point (selected-window) (- (point) 1))))
(use-package evil-multiedit
  :straight t
  :demand t
  :config
  (evil-multiedit-default-keybinds)
  (advice-add 'evil-multiedit-match-and-next :before #'my-evil-multiedit-maintain-visual-cursor-advice)
  (advice-add 'evil-multiedit-match-and-prev :before #'my-evil-multiedit-maintain-visual-cursor-advice)
  )
;;;; org
(defun my-org-agenda-to-appt ()
  "Erase all reminders and rebuilt reminders for today from the agenda."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun my-org-inf-repeat ()
  (interactive)
  "Treat the TODO as a repeater by logging it."
  (if (org-element-property :REPEAT (org-element-at-point))
      (let ((note (cdr (assq 'state org-log-note-headings)))
            lines)
        (setq org-log-note-marker (set-marker (make-marker) (aref (plist-get (plist-get (org-element-at-point) 'headline) :standard-properties) 0)))
        (setq org-log-note-return-to (set-marker (make-marker) (aref (plist-get (plist-get (org-element-at-point) 'headline) :standard-properties) 0)))
        (setq note
              (org-replace-escapes
               note
               (list (cons "%u" (user-login-name))
		     (cons "%U" user-full-name)
		     (cons "%t" (format-time-string
			         (org-time-stamp-format 'long 'inactive)
			         (current-time)))
		     (cons "%T" (format-time-string
			         (org-time-stamp-format 'long nil)
			         (current-time)))
		     (cons "%d" (format-time-string
			         (org-time-stamp-format nil 'inactive)
			         (current-time)))
		     (cons "%D" (format-time-string
			         (org-time-stamp-format nil nil)
			         (current-time)))
		     (cons "%s" (cond
			         ((not org-log-note-state) "\"DONE\"")
			         ((string-match-p org-ts-regexp
						  org-log-note-state)
				  (format "\"[%s]\""
					  (substring org-log-note-state 1 -1)))
			         (t (format "\"%s\"" org-log-note-state))))
		     (cons "%S"
			   (cond
			    ((not org-log-note-previous-state) "\"TODO\"")
			    ((string-match-p org-ts-regexp
					     org-log-note-previous-state)
			     (format "\"[%s]\""
				     (substring
				      org-log-note-previous-state 1 -1)))
			    (t (format "\"%s\""
				       org-log-note-previous-state)))))))
        (push note lines)
        (org-fold-core-ignore-modifications
          (org-with-wide-buffer
           ;; Find location for the new note.
           (goto-char org-log-note-marker)
           (set-marker org-log-note-marker nil)
           ;; Note associated to a clock is to be located right after
           ;; the clock.  Do not move point.
           (unless (eq org-log-note-purpose 'clock-out)
             (goto-char (org-log-beginning t)))
           ;; Make sure point is at the beginning of an empty line.
           (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert-and-inherit "\n")))
	         ((looking-at "[ \t]*\\S-") (save-excursion (insert-and-inherit "\n"))))
           ;; In an existing list, add a new item at the top level.
           ;; Otherwise, indent line like a regular one.
           (let ((itemp (org-in-item-p)))
             (if itemp
	         (indent-line-to
	          (let ((struct (save-excursion
			          (goto-char itemp) (org-list-struct))))
	            (org-list-get-ind (org-list-get-top-point struct) struct)))
	       (org-indent-line)))
           (insert-and-inherit (org-list-bullet-string "-") (pop lines))
           (let ((ind (org-list-item-body-column (line-beginning-position))))
             (dolist (line lines)
	       (insert-and-inherit "\n")
               (unless (string-empty-p line)
	         (indent-line-to ind)
	         (insert-and-inherit line))))
           (run-hooks 'org-after-note-stored-hook)
           (message "Note stored")
           (org-back-to-heading t)))
        (with-current-buffer (marker-buffer org-log-note-return-to)
          (goto-char org-log-note-return-to)))))



(use-package org
  :straight t
  :hook ((org-agenda-finalize . my-org-agenda-to-appt)
         (org-agenda-finalize . append))
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-clock-sound (concat user-emacs-directory "bell.wav"))
  (org-agenda-timegrid-use-ampm t)
  ;; Activate appointments so we get notifications
  (appt-activate t)
  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "24:01" nil 'my-org-agenda-to-appt)
  ;; keep track of when todo is finished when created
  (org-log-done 'time)
  ;; set agenda files
  (org-agenda-files nil)
  (org-log-into-drawer t)
  ;; file path for plantuml
  ;; ask before killing a pomodora timer
  (org-src-lang-modes
   '(("C" . c)
     ("C++" . c++)
     ("asymptote" . asy)
     ("bash" . sh)
     ("beamer" . latex)
     ("calc" . fundamental)
     ("cpp" . c++)
     ("ditaa" . artist)
     ("desktop" . conf-desktop)
     ("dot" . fundamental)
     ("elisp" . emacs-lisp)
     ("ocaml" . tuareg)
     ("screen" . shell-script)
     ("shell" . sh)
     ("sqlite" . sql)
     ("toml" . conf-toml)
     ("plantuml" . plantuml)))
  (org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)
                                                           (dot . t)
                                                           (scheme . t)
                                                           (racket . t)
                                                           (python . t)
                                                           (shell . t)))
  (setq org-agenda-files '("~/org/TODO.org"))
  (add-to-list 'org-shiftright-hook #'my-org-inf-repeat)
  )


(use-package org-remark
  :after org
  :straight t
  :hook ((after-init . org-remark-global-tracking-mode)
         (Info-mode . org-remark-info-mode)
         (nov-mode . org-remark-nov-mode))
  :diminish org-remark-global-tracking-mode
  :diminish org-remark-mode
  )


(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode))


(defun my-org-pomodoro-choose-break-time (arg)
  "Choose break time for pomodoro."
  (interactive "nBreak time (0 if overtime): ")
  (setq org-pomodoro-short-break-length arg))


(defun my-org-pomodoro-finished-with-overtime-advice (orig-fun &rest args)
  "Advise around `org-pomodoro-finished' to choose break time"
  (org-pomodoro-play-sound :pomodoro)
  (call-interactively #'my-org-pomodoro-choose-break-time)
  (cond ((= org-pomodoro-short-break-length 0) (org-pomodoro-overtime))
        ((zerop (mod (+ org-pomodoro-count 1) org-pomodoro-long-break-frequency)) (apply orig-fun args))
        (t (apply orig-fun args))))


(defun my-org-pomodoro-resume-after-break ()
  "Resume pomodoro timer if pomodoro timer is not currently in overtime."
  (save-window-excursion
    (org-clock-goto)
    (org-pomodoro)))

(defun my-org-pomodoro-clockout-before-kill-advice ()
  "Clock out time before exiting `org-pomodoro' so time is accurately tracked."
  (if (org-clocking-p)
      (save-window-excursion
        (org-clock-out))))


(use-package org-pomodoro
  :straight t
  :after org
  :hook (org-pomodoro-break-finished . my-org-pomodoro-resume-after-break)
  :custom
  (org-pomodoro-ask-upon-killing t)
  ;; change finish sound to differentiate between starting and stopping
  (org-pomodoro-finished-sound (concat user-emacs-directory "bell.wav"))
  ;; change pomo length and pomo break length
  (org-pomodoro-length 30)
  (org-pomodoro-short-break-length 7)
  (org-pomodoro-long-break-length 15)
  :config
  (advice-add 'org-pomodoro-finished :around #'my-org-pomodoro-finished-with-overtime-advice)
  (advice-add 'org-pomodoro-kill :before #'my-org-pomodoro-clockout-before-kill-advice))

(use-package evil-org
  :straight t
  :after (evil org)
  :hook (org-mode .  evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar shift todo heading))
  :diminish evil-org-mode)

(use-package ob-racket
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
	    #'ob-racket-raco-make-runtime-library)
  :straight (ob-racket
	     :type git :host github :repo "hasu/emacs-ob-racket"
	     :files ("*.el" "*.rkt")))
;;;; code
;;;;; tools
(use-package docker
  :straight t
  )

(use-package magit
  :straight t)

(use-package pet
  :demand t
  :straight t
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  )

(use-package sharper
  :straight t
  :demand t
  )

;;;;; modes

(use-package racket-mode
  :straight t
  )

(use-package tex
  :straight auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  )

(use-package php-mode
  :straight t
  :mode ("\\.php\\'" . php-ts-mode)
  )

(use-package js2-mode
  :straight t
  :mode ("\\.js\\'" . js2-mode))


(use-package elisp-mode
  :hook (emacs-lisp-mode . (lambda () (setq imenu-generic-expression (append (list  (list "Use Package" "^(use-package \\(.+\\)" 1)) imenu-generic-expression))))
  )


(use-package web-mode
  :straight t
  :mode ((("\\.phtml\\'") . web-mode)
         (("\\page\\'") . web-mode))
  )


(use-package plantuml-mode
  :straight t
  :custom
  (plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  )

;;;;; debugging
(use-package dape
  :straight t
  :init
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  (dape-display-source . pulse-momentary-highlight-one-line)
  (dape-start . (lambda () (save-some-buffers t t)))
  (dape-start . (lambda () (repeat-mode 1)))
  (dape-stopped . (lambda () (repeat-mode -1)))
  (dape-compile . kill-buffer)
  (python-ts-mode . dape-breakpoint-global-mode)
  (csharp-ts-mode . dape-breakpoint-global-mode)
  :custom
  (dape-key-prefix nil)
  (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)

  )

;;;;; language server
(defvar my-eglot-completion-functions (list #'yasnippet-capf #'eglot-completion-at-point)
  "The list of completion functions to combine to replace `eglot-completion-at-point'.")

(defun my-eglot-capf ()
  "Configure `completion-at-point-functions' to replace `eglot-completion-at-point' with completion results including all completions in `my-eglot-capf'."
  ;; Remember that local values in completion-at-point-functions take priority over global values."
  (setq-local completion-at-point-functions
              (list (apply 'cape-capf-super my-eglot-completion-functions))))

(defun my-file-completion-for-eglot ()
  "Give `cape-file' priority in `completion-at-point-functions'."
  ;; Remember that local values in completion-at-point-functions take priority over global values."
  (add-hook 'completion-at-point-functions #'cape-file -100 t))

(use-package eglot
  :hook ((csharp-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (LaTeX-mode . eglot-ensure))
  :config
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf)
  (add-hook 'eglot-managed-mode-hook #'my-file-completion-for-eglot 100)
  ;; if lsp-server returns many completions then turn off but if it doesn't then turn it on
  ;; This line causes function to delete or add characters when exiting https://github.com/minad/cape/issues/81
  ;;  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-to-list 'eglot-server-programs
               '(LaTeX-mode . ("texlab")))
  (setf (alist-get '(csharp-mode csharp-ts-mode) eglot-server-programs) '("csharp-language-server")))

(defvar eldoc-ratio 0.30)

(defun my-buffer-distance (string buffer)
  "Get Levenshtein distance of STRING and BUFFER."
  (string-distance string (tramp-get-buffer-string buffer)))

(defun my-buffer-length (buffer)
  "Get string length of BUFFER."
  (length (tramp-get-buffer-string buffer)))

(defun my-eldoc-docs-string (list)
  "Get all strings in eldoc LIST and concat them."
  (let (value)
    (dolist (elt list value)
      (setq value (concat value (substring-no-properties (car elt)))))))


(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  :diminish eldoc-mode
  )






;;;; completion
(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :custom
  ;; cycle when reaching end of popup
  (corfu-cycle t)
  (corfu-on-exact-match 'quit)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'nil)
  (corfu-preselect 'first)
  (corfu-auto t)
  ;; if it doesn't work it is probably because the lsp is overriding it with :company-prefix-length
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.3)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (global-corfu-minibuffer nil)
  (corfu-popupinfo-delay nil)
  )

(use-package hippie-exp
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))
  :config
  (defadvice hippie-expand (around hippie-expand-case-fold)
    (let ((case-fold-search nil))
      ad-do-it))
  (ad-activate 'hippie-expand)
  )

(use-package yasnippet
  :straight t
  :init
  (yas-global-mode 1)
  :custom
  (yas-also-auto-indent-first-line t)
  :diminish yas-minor-mode
  )

(use-package yasnippet-snippets
  :straight t
  )

(defun my-yasnippet-add-completion-functions ()
  "Add yasnippet-capf to `completion-at-point-functions'."
  ;; Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; Add yasnippet-capf globally
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

(use-package yasnippet-capf
  :straight t
  :init
  :hook ((prog-mode org-mode) . my-yasnippet-add-completion-functions)
  )


(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  :config
  ;; fixes C-k defaulting to adding a digraph in M-x
  (eval-after-load "evil-maps"
    (dolist (map '(evil-insert-state-map))
      (define-key (eval map) "\C-k" nil)
      ))
  )

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  )


(use-package consult
  :straight t
  :custom
  ;; turns on vertico for : in evil
  (completion-in-region-function 'consult-completion-in-region)
  )


(use-package cape
  :straight t
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  ;; (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  ;; (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  ;; (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; adds cape-file globally
  ;; Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  
  (add-hook 'completion-at-point-functions #'cape-file)
  )

;;;; ui
(use-package treesit-auto
  :straight t
  :demand t
  :custom
  (treesit-font-lock-level 4)
  :config
  (global-treesit-auto-mode))
;; modus-operandi-tinted python
;; modus-viviendi-tinted C#
;; modus-viviendi Elisp
(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer t)
  )

(use-package popper
  :straight t ; or :straight t
  :init
  (setq popper-display-control 'user)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eshell\\*"
          "\\*dotnet"
          "events\\*"
          "\\*shell\\*"
          "\\*dape-shell\\*" 
          "\\*vterm\\*"
          "^\\* docker.+ up"
          "^\\* docker.+ exec"
          "\\*Racket"
          (lambda (buf) (with-current-buffer buf
                          (derived-mode-p 'comint-mode)))
          debugger-mode
          compilation-mode
          ))
  (popper-mode +1)
  (popper-echo-mode +1))

(defun my-fit-window-to-buffer (&optional window max-height min-height max-width min-width preserve-size)
  (let* ((wind (if window window (selected-window)))
         (initial-width (window-width wind))
         (initial-height (window-height window)))
    (fit-window-to-buffer window max-height min-height max-width min-width preserve-size)
    (if (or (not (equal (window-width wind) initial-width)) (not (equal (window-height wind) (window-height wind))))
        t
      nil)))


(defun my-fit-window-to-right-side (window)
  "Use `fit-window-to-buffer' with right side window specifications."
  (let ((max-width (floor (* 0.35 (frame-width))))
        (max-height (floor (* 0.50 (frame-height)))))
    (if (my-fit-window-to-buffer window max-height window-min-height max-width)
        nil
      (window-resize window (- max-width (window-width window)) t)
      (window-resize window (- max-height (window-height window))))))


;; TODO
(defun my-fit-window-to-magit (window)
  "Use 'fit-window-to-buffer' but make it work for magit's behavior."
  (let ((max-width (floor (* 0.50 (frame-width))))
        (max-height (floor (* 1.00 (frame-height)))))
    (fit-window-to-buffer window max-height window-min-height max-width)))

(use-package window
  :custom
  (menu-bar-mode nil)
  (fit-window-to-buffer-horizontally t)
  (tab-bar-mode nil)
  (tool-bar-mode nil)
  (line-number-mode nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions nil)
  (window-sides-slots '(2 2 2 2))
  (display-buffer-alist
   '(("\\*info\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (slot . 0)
      (window-width . my-fit-window-to-right-side))
     ("\\*helpful\\|\\*Help\\*\\|\\*eldoc\\*\\|\\*persisted eldoc\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (slot . -1)
      (window-width . my-fit-window-to-right-side))
     ((or "\\*dotnet\\|\\*Messages\\*\\|Output\\*\\|events\\*\\|\\*eshell\\*\\|\\*shell\\*\\|\\*dape-shell\\*\\|\\*vterm\\*\\|^\\* docker.+ up\\|^\\* docker.+ exec\\|\\*Racket" (major-mode . compilation-mode)  (major-mode . debugger-mode) (derived-mode . comint-mode)) 
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-height . 0.50))
     ((or "^\\*docker.+\\*$" (derived-mode . magit-mode) "\\*Remember\\*")
      (display-buffer-reuse-window display-buffer-in-direction)
      (window . root)
      (window-width . 0.50)
      (direction . left))
     ((major-mode . dired-mode)
      (display-buffer-reuse-mode-window display-buffer-in-direction)
      (window . root)
      (window-width . 0.50)
      (direction . left))
     )))


(use-package font-core
  :config
  (set-frame-font "JetBrains Mono 10" nil t))


(use-package kind-icon
  :straight t
  :after corfu
  :demand t
  :custom
  ;; (kind-icon-blend-background t)
  ;; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  (kind-icon-mapping
   '((array "a" :icon "code-brackets" :face font-lock-type-face)
     (boolean "b" :icon "circle-half-full" :face
              font-lock-builtin-face)
     (class "c" :icon "view-grid-plus-outline" :face
            font-lock-type-face)
     (color "#" :icon "palette" :face success)
     (command "cm" :icon "code-greater-than" :face default)
     (constant "co" :icon "lock-remove-outline" :face
               font-lock-constant-face)
     (constructor "cn" :icon "table-column-plus-after" :face
                  font-lock-function-name-face)
     (enummember "em" :icon "order-bool-ascending-variant" :face
                 font-lock-builtin-face)
     (enum-member "em" :icon "order-bool-ascending-variant" :face
                  font-lock-builtin-face)
     (enum "e" :icon "format-list-bulleted-square" :face
           font-lock-builtin-face)
     (event "ev" :icon "lightning-bolt-outline" :face
            font-lock-warning-face)
     (field "fd" :icon "application-braces-outline" :face
            font-lock-variable-name-face)
     (file "f" :icon "file-document-outline" :face
           font-lock-string-face)
     (folder "d" :icon "folder" :face font-lock-doc-face)
     (interface "if" :icon "application-brackets-outline" :face
                font-lock-type-face)
     (keyword "kw" :icon "key-variant" :face font-lock-keyword-face)
     (macro "mc" :icon "lambda" :face font-lock-keyword-face)
     (magic "ma" :icon "auto-fix" :face font-lock-builtin-face)
     (method "m" :icon "function-variant" :face
             font-lock-function-name-face)
     (function "f" :icon "function" :face font-lock-function-name-face)
     (module "{" :icon "file-code-outline" :face
             font-lock-preprocessor-face)
     (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
     (operator "op" :icon "plus-minus" :face
               font-lock-comment-delimiter-face)
     (param "pa" :icon "cog" :face default)
     (property "pr" :icon "wrench" :face font-lock-variable-name-face)
     (reference "rf" :icon "library" :face
                font-lock-variable-name-face)
     (snippet "S" :icon "content-cut" :face font-lock-string-face)
     (string "s" :icon "sticker-text-outline" :face
             font-lock-string-face)
     (struct "%" :icon "code-braces" :face
             font-lock-variable-name-face)
     (text "tx" :icon "script-text-outline" :face font-lock-doc-face)
     (typeparameter "tp" :icon "format-list-bulleted-type" :face
                    font-lock-type-face)
     (type-parameter "tp" :icon "format-list-bulleted-type" :face
                     font-lock-type-face)
     (unit "u" :icon "ruler-square" :face font-lock-constant-face)
     (value "v" :icon "plus-circle-outline" :face
            font-lock-builtin-face)
     (variable "va" :icon "variable" :face
               font-lock-variable-name-face)
     (t "." :icon "crosshairs-question" :face font-lock-warning-face)))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  )



;;Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  )


(use-package posframe
  :straight t
  )


(use-package adaptive-wrap
  :straight t
  :hook ((eshell-mode help-mode html-ts-mode prog-mode evil-org-mode dired-mode helpful-mode info-mode) . adaptive-wrap-prefix-mode)
  )

(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-max-description-length nil)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-separator ":")
  (which-key-allow-multiple-replacements t)
  (which-key-popup-type 'minibuffer)
  :init
  (which-key-mode)
  :diminish which-key-mode
  )


;;;; writing
(use-package ispell
  :hook ((prog-mode . ispell-minor-mode)
         (org-mode . ispell-minor-mode))
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US")
  :diminish ispell-minor-mode
  )

(use-package flyspell
  :hook ((org-mode . flyspell-mode)
         (LaTeX-mode . flyspell-mode))
  :diminish flyspell-mode)

;;;; miscaleanous
(use-package pdf-tools
  :straight t
  :init
  (pdf-tools-install)
  )

(use-package burly
  :demand t
  :straight t)

(use-package vterm
  :straight t
  :hook (vterm-mode . (lambda () (setq evil-insert-state-modes nil))))

(use-package savehist
  :init
  (savehist-mode))


(use-package eshell
  :hook ((eshell-first-time-mode . (lambda () (yas-minor-mode -1)))
         (((eshell-mode shell-mode) . (lambda () (corfu-mode -1)))))
  :config
  ;; Setup eshell sudo
  (require 'em-tramp)
  (setq password-cache-expiry 3600)
  (setq eshell-prefer-lisp-functions t)
  (setq eshell-prefer-lisp-variables t)
  (setq password-cache t) 
  (setq password-cache-expiry 3600) 
  )

(use-package helpful
  :straight t
  :demand t
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package elisp-demos
  :straight t
  )

(use-package outline
  :mode ("init.el" . (lambda () (emacs-lisp-mode) (outline-minor-mode 1) (evil-close-folds)))
  :diminish outline-minor-mode)
;;;; emacs default
(use-package emacs
  :mode ("\\.sql\\'" . sql-mode)
  :hook (((Info-mode prog-mode evil-org-mode html-ts-mode ibuffer-mode imenu-list-minor-mode dired-mode LaTeX-mode) . (lambda () (setq display-line-numbers 'visual)))
         ((prog-mode html-ts-mode) . (lambda () (setq indent-tabs-mode nil))))
  :config
  ;; ellipsis marker single character of three dots in org
  (with-eval-after-load 'mule-util
    (setq truncate-string-ellipsis "…"))
  ;; disable transparency
  (add-to-list 'default-frame-alist '(alpha-background . 1.0))
  ;; yes or no now y or n
  (if (version<= emacs-version "28")
      (defalias 'yes-or-no-p 'y-or-n-p)
    (setopt use-short-answers t))

  (when scroll-bar-mode
    (scroll-bar-mode -1))

  (add-to-list 'custom-enabled-themes 'tango-dark)
  (load-theme 'tango-dark)
  (blink-cursor-mode 0)
  ;; paths
  (add-to-list 'exec-path (concat user-emacs-directory "bin/"))
  (add-to-list 'Info-directory-list (concat user-emacs-directory "info/"))

  :custom
  (undo-limit 400000)           ;; 400kb (default is 160kb)
  (undo-strong-limit 3000000)   ;; 3mb   (default is 240kb)
  (undo-outer-limit 48000000)  ;; 48mb  (default is 24mb)


  ;; turn off comp warnings
  (native-comp-async-report-warnings-error nil)
  ;; get rid of menu bar, tab bar, and tool bar
  ;; setup different directory for backups and autosaves

  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/"))))
  ;; tabs insert spaces
  (indent-tabs-mode nil)
  ;; cursor over actual space of character ;; doesn't do anything??
  (x-stretch-cursor t)
  ;; take new window space from all other windows
  (window-combination-resize t) 
  ;; buffer is same version as file when opened
  (global-auto-revert-mode 1)
  ;; end double space between sentences
  (sentence-end-double-space nil)
  (desktop-save-mode 1)

  (doc-view-resolution 200)
  ;; support opening new minibuffers from inside existing minibuffers for evil :
  (enable-recursive-minibuffers t)
  ;; hide commands in M-x which do not work in current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; do not allow cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))



  ;; example of customizing colors
  )
