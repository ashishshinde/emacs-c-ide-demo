(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)


;; function-args
;; (require 'function-args)
;; (fa-config-default)
;;(define-key c-mode-map  [(tab)] 'company-complete)
;;(define-key c++-mode-map  [(tab)] 'company-complete)
(define-key helm-map (kbd "C-r") 'helm-minibuffer-history)

;;-------------------------------------------------------------
;; external package
(add-to-list 'load-path "~/.emacs.d/external/")

;;-------------------------------------------------------------
;; selection mode to allow shift arrow to extend region
(setq shift-select-mode t)
(global-set-key [s-left] 'backward-char-mark)
(global-set-key [s-right] 'forward-char-mark)
(global-set-key [s-up] 'previous-line-mark)
(global-set-key [s-down] 'next-line-mark)


;;-------------------------------------------------------------
;; History related
(savehist-mode 1)
(setq undo-limit 10000)

;;--------------------------------------------------------------------
;; deal with large files
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;;-------------------------------------------------------------
;; Appearance
(require 'sublime-themes)
;;(load-theme 'clues t)
(load-theme 'modus-vivendi t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-default-font "Noto Mono-11")
(set-face-attribute 'mode-line nil  :height 100)
(set-face-attribute 'mode-line-inactive nil :height 100)

(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
    (set-face-background 'default "unspecified-bg" frame)))
(on-frame-open (selected-frame))
(add-hook 'after-make-frame-functions 'on-frame-open)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;;-------------------------------------------------------------
;; Terminal mode
;;-------------------------------------------------------------;; Appearance
(define-advice server-eval-and-print (:filter-args (args) no-print)
  (list (car args) nil))


;;-------------------------------------------------------------
;; search related
(define-key isearch-mode-map (kbd "C-d")
  'xah-search-current-word)
(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq ξp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq ξp2 (point))))
    (setq mark-active nil)
    (when (< ξp1 (point))
      (goto-char ξp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties ξp1 ξp2))))

;; wrap search around
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

(ad-activate 'isearch-repeat)

;; wrap query replace around
; advise the new version to repeat the search after it
;; finishes at the bottom of the buffer the first time:
(defadvice query-replace-repeat
  (around replace-wrap
          (FROM-STRING TO-STRING &optional DELIMITED START END))
  "Execute a query-replace, wrapping to the top of the buffer
   after you reach the bottom"
  (save-excursion
    (let ((start (point)))
      ad-do-it
      (beginning-of-buffer)
      (ad-set-args 4 (list (point-min) start))
      ad-do-it)))

;; Turn on the advice
(ad-activate 'query-replace-repeat)

;;-------------------------------------------------------------
;; Find file in repository
(require 'find-file-in-repository)
(require 'ido)
;; (require 'ido-ubiquitous)
;; (require 'ido-vertical-mode)
(global-set-key (kbd "C-x f") 'find-file-in-repository)
;; (ido-ubiquitous-mode 1)
;; (ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


;;-------------------------------------------------------------
;; windowing
;; disable menubar, toolbar, and scrollbar
(global-set-key (kbd "C-c C-<left>")  'windmove-left)
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<up>")    'windmove-up)
(global-set-key (kbd "C-c C-<down>")  'windmove-down)
;; enable clipboard in emacs
(setq x-select-enable-clipboard t)
;; unicode encoding in terminal mode
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; transparency
(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))
(transparency 98)


;;-------------------------------------------------------------
;; ggtags
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

;;-------------------------------------------------------------
;; company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
;; (add-to-list 'company-backends 'company-c-headers)

;; revert / reload file without confirmation
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "C-M-z") 'revert-buffer-no-confirm)

;;-------------------------------------------------------------
;; git related
(require 'git)
(require 'git-blamed)
(setq vc-follow-symlinks nil)

;;-------------------------------------------------------------
;; multiterm related
(require 'multi-term)
(when (require 'multi-term nil t)
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "term"
        multi-term-program "/bin/zsh")
  (setq multi-term-scroll-to-bottom-on-output t)
  (setq default-terminal-coding-system "utf-8-unix")
  )

(set-terminal-coding-system 'utf-8-unix)
(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

(when (require 'term nil t) ; only if term can be loaded..
  (setq term-bind-key-alist
        (list (cons "C-c C-c" 'term-interrupt-subjob)
              (cons "C-p" 'previous-line)
              (cons "C-n" 'next-line)
              (cons "M-f" 'term-send-forward-word)
              (cons "M-b" 'term-send-backward-word)
              (cons "C-c C-j" 'term-line-mode)
              (cons "C-c C-k" 'term-char-mode)
              (cons "M-DEL" 'term-send-backward-kill-word)
              (cons "M-d" 'term-send-forward-kill-word)
              (cons "<C-left>" 'term-send-backward-word)
              (cons "<C-right>" 'term-send-forward-word)
              (cons "C-r" 'term-send-reverse-search-history)
              (cons "M-p" 'term-send-raw-meta)
              (cons "M-y" 'term-send-raw-meta)
              (cons "C-y" 'term-send-raw))))

(when (require 'term nil t)
  (defun term-handle-ansi-terminal-messages (message)
    (while (string-match "\eAnSiT.+\n" message)
      ;; Extract the command code and the argument.
      (let* ((start (match-beginning 0))
             (command-code (aref message (+ start 6)))
             (argument
              (save-match-data
                (substring message
                           (+ start 8)
                           (string-match "\r?\n" message
                                         (+ start 8))))))
        ;; Delete this command from MESSAGE.
        (setq message (replace-match "" t t message))

        (cond ((= command-code ?c)
               (setq term-ansi-at-dir argument))
              ((= command-code ?h)
               (setq term-ansi-at-host argument))
              ((= command-code ?u)
               (setq term-ansi-at-user argument))
              ((= command-code ?e)
               (save-excursion
                 (find-file-other-window argument)))
              ((= command-code ?x)
               (save-excursion
                 (find-file argument))))))

    (when (and term-ansi-at-host term-ansi-at-dir term-ansi-at-user)
      (setq buffer-file-name
            (format "%s@%s:%s" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))
      (set-buffer-modified-p nil)
        (setq default-directory (if (string= term-ansi-at-host (system-name))
                                    (concatenate 'string term-ansi-at-dir "/")
                                  (format "/%s@%s:%s/" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))))
    message))

(add-hook 'term-mode-hook (lambda()
        (setq yas-dont-activate t)))

;;-------------------------------------------------------------
;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; (global-set-key (kbd "M-)")           (quote move-forward-paren))
;; (global-set-key (kbd "M-(")           (quote move-backward-paren))

;; (global-set-key (kbd "M-]")           (quote move-forward-sqrParen))
;; (global-set-key (kbd "M-[")           (quote move-backward-sqrParen))

;; (global-set-key (kbd "M-}")           (quote move-forward-curlyParen))
;; (global-set-key (kbd "M-{")           (quote move-backward-curlyParen))

(global-set-key (kbd "C-M-/") 'my-expand-file-name-at-point)
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))


;;-------------------------------------------------------------
;; semantic
(require 'cc-mode)
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
(global-semantic-idle-summary-mode 1)

;;-------------------------------------------------------------
;; sticky function
(require 'stickyfunc-enhance)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;;-------------------------------------------------------------
;; show function interface at point
(setq-local eldoc-documentation-function #'ggtags-eldoc-function)


;;-------------------------------------------------------------
;; indentation
(setq
 c-default-style "linux" ;; set style to "linux"
 )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;;-------------------------------------------------------------
;; cleanup trailing whitespace
(require 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;-------------------------------------------------------------
;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;-------------------------------------------------------------
;; smartparens
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
               (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
               (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                         ("* ||\n[i]" "RET"))))

;;-------------------------------------------------------------
;; riti formatter
(require 'riti)
(setq-default riti-on-save nil)
(setq-default riti-cfg-file "/home/ashish/.riti.xml")
(global-set-key [C-M-tab] 'riti)

;;-------------------------------------------------------------
;; shift numbers
(require 'shift-number)
(autoload 'shift-number-up "shift-number" nil t)
(autoload 'shift-number-down "shift-number" nil t)
(global-set-key (kbd "M-+") 'shift-number-up)
(global-set-key (kbd "M-_") 'shift-number-down)

;;-------------------------------------------------------------
;; autosave
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.saves/" t)))
(setq backup-directory-alist `(("." . "~/.emacs.saves/")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;-------------------------------------------------------------
;; golang
(defun set-exec-path-from-shell-PATH ()
(let ((path-from-shell (replace-regexp-in-string
                        "[ \t\n]*$"
                        ""
                        (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
  (setenv "PATH" path-from-shell)
  (setq eshell-path-env path-from-shell) ; for eshell users
  (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; GOPATH
(setenv "GOPATH" "/home/ashish/go")

;; Auto format on save
(add-to-list 'exec-path "/home/ashish/go/bin")
(add-hook 'before-save-hook 'gofmt-before-save)

;; godef bindings
(defun my-go-mode-hook ()
  ; Call Gofmt and golint before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'after-save-hook 'golint nil 'make-it-local)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
)

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; autocomplete
(defun auto-complete-for-go ()
(auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(defun auto-complete-for-go ()
(auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
(require 'go-autocomplete))

;;Configure golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
;; Close golint buffer if there are no messages
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "golint" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (goto-char 1)
          (search-forward ".go:" nil t))))
      (run-with-timer 0 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                        )
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(add-to-list 'load-path "$GOPATH/src/github.com/dougm/goflymake")
;(require 'go-flymake)

;;-------------------------------------------------------------
;; python
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;;-------------------------------------------------------------
(require 'solaire-mode)

;; brighten buffers (that represent real files)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

;; ...if you use auto-revert-mode:
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; highlight the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!
;;
;; NOTE: This is necessary for themes in the doom-themes package!
(solaire-mode-swap-bg)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "2a7beed4f24b15f77160118320123d699282cbf196e0089f113245d4b729ba5d" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e" "1a212b23eb9a9bedde5ca8d8568b1e6351f6d6f989dd9e9de7fba8621e8ef82d" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "31f8d16d264e14e8e39c4f291e26cdd5516772a41660ef2ad895244c22024bd2" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "379a804655efccc13a3d446468992bfdfc30ff27d19cfda6f62c7f9c9e7a8a7d" "c59ed2bceca3ba0c01a7689cb9067c9b7f11924aaad98ed4b0c1f0818e542a92" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7" "de65dc21fefce202883a5071170962c73b4bf4b691156d0a28239765f71b23e5" "f11e219c9d043cbd5f4b2e01713c2c24a948a98bed48828dc670bd64ae771aa1" "a77ced882e25028e994d168a612c763a4feb8c4ab67c5ff48688654d0264370c" "1e9001d2f6ffb095eafd9514b4d5974b720b275143fbc89ea046495a99c940b0" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" default)))
 '(package-selected-packages
   (quote
    (modus-vivendi-theme solarized-theme golint ample-zen-theme ample-theme flatland-theme gruber-darker-theme almost-mono-themes vscdark-theme doom-themes backup-walker clues-theme arjen-grey-theme soothe-theme afternoon-theme cyberpunk-theme night-owl-theme atom-dark-theme atom-one-dark-theme go-mode markdownfmt markdown-toc markdown-mode+ window-jump rainbow-delimiters kotlin-mode yaml-mode magit zygospore yasnippet ws-butler volatile-highlights use-package undo-tree sublime-themes stickyfunc-enhance solaire-mode smartparens shift-number rainbow-identifiers multi-term iedit ido-vertical-mode helm-swoop helm-projectile helm-gtags git-blamed git ggtags find-file-in-repository dtrt-indent company clean-aindent-mode anzu)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))))))
