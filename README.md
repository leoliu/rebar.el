emacs wrapper for rebar
=======================

# Prerequisite #

Emacs 24.x.

`cl-lib` is required for emacs `24.1` and `24.2`, which can be readily
installed via `M-x package-install RET cl-lib RET`.

# Usage #

`(add-hook 'erlang-mode-hook 'rebar-mode)`

See the documentation on `rebar-mode` or `global-rebar-mode`. A menu
named `Rebar` is added to the menu bar if `rebar-mode` or
`global-rebar-mode` is enabled.
