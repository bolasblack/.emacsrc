(require 'server)

(provide-me)

(unless (server-running-p)
  (server-start))
