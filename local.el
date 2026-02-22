;;; local.el --- Host specific settings  -*- lexical-binding: t; -*-

;;; Theme and font
(load-theme 'horizons-chauds t)
(set-face-attribute 'default nil :font "Berkeley Mono-9")

;;; Local LLM backend (Foundry on local network)
(with-eval-after-load 'gptel
  (setq gptel-backend
        (gptel-make-openai "Foundry"
          :host "172.29.80.1:1234"
          :endpoint "/v1/chat/completions"
          :protocol "http"
          :stream t
          :models '(qwen2.5-7b-instruct-qnn-npu:1 deepseek-r1-distill-qwen-14b-qnn-npu:1))))
