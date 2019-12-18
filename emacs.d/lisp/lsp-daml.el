(require 'lsp-mode)
(require 'lsp-mode)
(defvar lsp-language-id-configuration '(daml-mode . "daml"))
(lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/home/gyorgy/.daml/bin/daml" "ide"))
                    :major-modes '(daml-mode)
                    :server-id 'daml-ide))

(provide 'daml-lsp)
