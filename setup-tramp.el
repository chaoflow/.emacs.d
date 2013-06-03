(require 'tramp-sh)

;; Tell Tramp to use the value of "echo $PATH", which works better for
;; NixOS machines with a broken "getconf PATH".
(add-to-list 'tramp-remote-path 'tramp-own-remote-path t)

(provide 'setup-tramp)
