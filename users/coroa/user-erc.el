;;; ERC Config
(setq erc-email-userid "coroa@online.de"
      erc-nick (user-login-name)
      erc-user-full-name "Jonas"
      erc-server "irc.freenode.net"
      erc-fill-function 'erc-fill-variable
      ;; erc-autojoin-channels-alist '(("freenode.net" "#bewelcome" "#patterns"))
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"))
      ;; #1987.4

(add-hook 'erc-echo-notice-hook 'erc-echo-notice-in-minibuffer)

(defun erc-pumuckl ()
  "Connect to ZNC on Pumuckl."
  (interactive)
  (erc-tls :server "pumuckl.fsr.physik.uni-potsdam.de"
           :port 669
           :nick "coroa"
           :full-name "Jonas"))

(provide 'user-erc)
