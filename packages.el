;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

(package! lorem-ipsum :pin "4e87a899868e908a7a9e1812831d76c8d072f885")
(package! yaml-mode :pin "d91f878729312a6beed77e6637c60497c5786efa")

(package! vue-mode :pin "4853d97ea8d7b2fab7d331d3a8aad18d02b792ad")
(package! vundo :pin "f57937d7f57e6d081f567debf14f11d87a28962f")

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

(package! lab
  :recipe (:host github :repo "isamert/lab.el") :pin "91b5917cfbc77ef2f7e2af1c4f02f9706ef4a642")

(package! lsp-biome
  :recipe (:host github :repo "cxa/lsp-biome") :pin "145c8196c72ff889f47e084c4f70c9adbc8518f2")

(package! alert :pin "79f6936ab4d85227530959811143429347a6971b")
(package! alert-toast :pin "ba931529a266537783cfec2a28c2b8c058364ff2")

;; (package! nix-mode :pin "719feb7868fb567ecfe5578f6119892c771ac5e5")

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :pin "952fc7a8ae06091a46995d32ebde4380e0c71142")

(package! sops :recipe (:host github :repo "djgoku/sops") :files ("*.el") :pin "7cce0d6800eff1e9c21ab43fffe1918bcc006e7d")

(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el" "dist"))
  :pin "6a75b8ec1e05554abd41293acd67bfe3a99bc877")

(package! pet
  :recipe (:host github :repo "wyuenho/emacs-pet" :files ("*.el")) :pin "1f7450237549ad9850543fbc78d12f9fd375324d")

(package! swagg
  :recipe (:host github :repo "isamert/swagg.el") :pin "87bd1f698bc4c77a1c0d6b252e15f4ee345d2afa")

(package! pyenv-mode
  :recipe (:host github :repo "pythonic-emacs/pyenv-mode") :pin "364bddb8f0c8ec022796210d8d3625a520e984b0")
;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
