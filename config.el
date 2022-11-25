;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Timothée Bineau"
      user-mail-address "jatimix@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 13 :weight 'normal))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
(map! :map vertico-map "C-j" #'vertico-insert)

(map! "C-:" #'hippie-expand)
(map! "M-n" #'forward-paragraph)
(map! "M-p" #'backward-paragraph)
(map! "C-s" #'tim/consult-line)
(map! "²" #'+popup/toggle)

(setq projectile-globally-ignored-directories '("__build" ".cache" "venv"))

(setq tab-width 4)
(setq typescript-indent-level 2)
(setq c-basic-offset 2)

;; deactivate project indexing on tramp remote
;; Basically resolve the huge connection time on tramp
(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

(use-package! vundo)
(map! "C-x u" #'vundo)
;;; Personnal Stuf

(defun tim/consult-line ()
  "When region is selected use it as candidate."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (set-mark-command 0)
        (consult-line (buffer-substring-no-properties beg end)))
    (consult-line)))

(defun tim/find-current-build-folder ()
  "Return the build folder __build in current project"
   (when (doom-project-p)
     (let ((compile-file (directory-files-recursively (doom-project-root) "compile_commands.json")))
       (when compile-file
         (file-name-directory (car compile-file))))))

(defun tim/add-compile-json-clangd-path (args)
  "Used as advice, add compile command directory in the build folder for clangd"
  (let ((build-folder (tim/find-current-build-folder)))
    (if build-folder
        `(,@args ,(concat "--compile-commands-dir=" build-folder))
      args)))

(advice-add 'lsp-clients--clangd-command :filter-return #'tim/add-compile-json-clangd-path)
;; (advice-remove 'lsp-clients--clangd-command #'tim/add-compile-json-clangd-path) ; for debug purpose
;;
; Better clangd
(setq lsp-clients-clangd-args '("-j=4"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--query-driver=/**/*"
                                "--header-insertion-decorators=0"))

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;;
;; TO get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
