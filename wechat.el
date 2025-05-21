;;; wechat.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'wechat-emoji)
(require 'shr)

(defcustom wechat-cli (executable-find "wechat")
  "The path of wechat-cli command.")

(defcustom wechat-common-chats nil
  "The list of wechat-cli Common Contacts.")

(defcustom wechat-input-prompt ">>> "
  "The prompt for input.")

(defcustom wechat-message-seperator ">"
  "The separator for message.")

(defcustom wechat-message-date-seperator "-"
  "The separator for message date.")

(defcustom wechat-message-region-max-width 120
  "The max width for message region.")

(defcustom wechat-chat-list-region-max-width 170
  "The max width for chat list region.")

(defcustom wechat-mute-symbol "🔕"
  "The symbol to express mute.")
(defcustom wechat-stick-symbol "❤️"
  "The symbol to express stick.")

(defcustom wechat-unread-symbol "📩"
  "The symbol to express unread.")

(defcustom wechat-notification-time 3
  "The notification time")

(defface wechat-message-user
  '((t (:foreground "#7757d6")))
  "wechat message user face."
  :group 'wechat)

(defface wechat-unread
  '((t (:foreground "red" :weight bold)))
  "wechat unread"
  :group 'wechat)

(defvar wechat--notification-timer nil)

(defvar wechat--unreads nil)

(defvar wechat--emojis nil)

(defvar wechat--input-marker nil)
(make-variable-buffer-local 'wechat--input-marker)

(defvar wechat--chat-title nil)
(make-variable-buffer-local 'wechat--chat-title)

(defun wechat--chats-to-tabulated-entries (json)
  "Convert chats json to tabulated-entries."
  (mapcar (lambda (item)
            (list (gethash "title" item)
                  (vector
                   (if (string= "t" (symbol-name (gethash "messageMute" item)))
                       wechat-mute-symbol
                     (if (string= "t" (symbol-name (gethash "stick" item)))
                         wechat-stick-symbol
                       ""))
                   (gethash "title" item)
                   (propertize
                    (if (not (equal 0 (gethash "unread" item)))
                        (concat (number-to-string (gethash "unread" item))
                                " "
                                wechat-unread-symbol)
                      "")
                    'face 'wechat-unread)
                   (string-replace "￼" "." (string-replace  "\n" "" (gethash "lastMessage" item)))
                   (gethash "lastDate" item))))

          json))

(defun wechat--json-parse-string (json-str)
  (condition-case err
      (json-parse-string json-str)
    (json-parse-error
     (error "JSON parse Error: %s" json-str))))

(defun wechat--format-date (date-str)
  "Format Chat Date."
  (let* ((width (min wechat-message-region-max-width (window-width)))
         (half-width (- (/ width 2) (length date-str)))
         (split-line (make-string half-width (string-to-char wechat-message-date-seperator))))
    (concat "\n"
            split-line
            date-str
            split-line
            "\n" "\n")))


(defun wechat--format-emoji (msg)
  (let ((str msg)
        (regexp "\\[.*?\\]")
        (pos 0)
        match
        img
        img-path)
    (while (string-match regexp str pos)
      (setq match (match-string 0 str))
      (setq msg (string-replace match
                                (wechat-emoji-format-image match)
                                msg))
      (setq pos (match-end 0)))
    msg))

(defun wechat--format-message (hash-msg)
  "Format Chat Message."
  (let ((user (propertize (gethash "user" hash-msg)
                          'face 'wechat-message-user))
        (msg (gethash "message" hash-msg))
        (index (gethash "index" hash-msg)))
    (if (or (string-prefix-p "发送了一个网页," msg)
            (string-prefix-p "发送了一个视频," msg)
            (string= msg "发送了一个图片"))
        (format "%s %s %s \n" user
                wechat-message-seperator
                (buttonize
                 msg
                 (lambda (data) (wechat-preview
                                 wechat--chat-title
                                 (number-to-string index)))))
      (format "%s %s %s \n" user wechat-message-seperator
              (wechat--format-emoji msg)))))

(defun wechat--set-margin (max-width)
  (let ((max-width (or max-width (window-width)))
        (margin (/ (- (window-width) max-width) 2))
        (inhibit-read-only t))
    (set-left-margin (point-min) (point-max) margin)
    (set-right-margin (point-min) (point-max) margin)))

(defun wechat--wrap-long-message ()
  (save-excursion)
  (goto-char (point-min))
  (let* ((max-width (min wechat-message-region-max-width (window-width)))
         (max-pixel-width (string-pixel-width (make-string max-width (string-to-char "-"))))
         (pixel-width))
    (while (not (equal (point) (point-max)))
      (setq pixel-width (string-pixel-width (buffer-substring (point) (line-end-position))))
      (if (< pixel-width (+ 10 max-pixel-width))
          (beginning-of-line 2)
        (print (current-line))
        (print (wechat--forward-pixel (- max-pixel-width 10)))
        (insert "\n " wechat-message-seperator " ")))))

(defun wechat--forward-pixel (pixel)
  (while (and (< (point) (line-end-position 1))
              (< (shr-pixel-column) pixel))
    (forward-char 1)))

(defun wechat--insert-chat-detail (messages input)
  "Insert Chat Detail in Current buffer."
  (let* ((date "")
         (inhibit-read-only t))
    (insert (wechat--format-date date))
    (mapc (lambda (message)
            (unless (string= date (gethash "date" message))
              (setq date (gethash "date" message))
              (insert (wechat--format-date date)))
            (insert (wechat--format-message message)))
          messages)
    (insert
     (wechat--format-date "Input"))
    (wechat--wrap-long-message)
    (align-regexp (point-min) (point-max) (format "\\(\\s-*\\)%s" wechat-message-seperator))
    (add-text-properties (point-min) (1- (point-max)) '(read-only t))
    (insert (propertize wechat-input-prompt
                        'rear-nonsticky t
                        'front-sticky t
                        'read-only t))
    (wechat--set-margin wechat-message-region-max-width)
    (setq-local wechat--input-marker (mark-marker))
    (set-marker wechat--input-marker (point))
    (when input
      (insert input))))

(defun wechat--run-process (program-and-args callback-fn)
  "Run PROGRAM-AND-ARGS and use the sentinel function to capture its complete output after the process terminates.
  PROGRAM-AND-ARGS is a list where the first element is the program path and the remaining elements are the arguments.
  CALLBACK-FN is a function that takes one parameter: the complete output string from the process."
  (let* ((process-name "*wechat-async-proc*")
         (output-buffer-name (generate-new-buffer-name "*wechat-proc-output*"))
         (output-buffer (get-buffer-create output-buffer-name))
         (process nil)) ; Forward declaration for process variable
    ;; Clear or prepare the output buffer.
    (with-current-buffer output-buffer
      (erase-buffer))
    ;; Start the process and redirect both stdout and stderr to the output buffer.
    (setq process (apply #'start-process
                         process-name  ;; The unique name of the process.
                         output-buffer ;; The output will be written to this buffer.
                         (car program-and-args)
                         (cdr program-and-args)))
    (if (null process)
        (progn
          (kill-buffer output-buffer)
          (error "Failed to start the process: %s" (string-join program-and-args " "))))

    ;; Set sentinel function
    (set-process-sentinel
     process
     (lambda (proc msg)
       ;; msg is typically a string like "finished\n" or "exited abnormally with code X\n"
       ;; (message "Process %s Event: %s" proc msg)
       ;; Ensure that the process has truly ended (either exited or received a termination signal).
       (when (memq (process-status proc) '(exit signal))
         (let ((full-output ""))
           ;; Retrieve content from the output buffer
           (with-current-buffer (process-buffer proc) ; Use process-buffer to obtain the correct buffer.
             (setq full-output (buffer-substring (point-min) (point-max))))

           ;; Call the callback function to handle output
           (funcall callback-fn full-output)

           ;; Cleanup: Delete temporary output buffer
           ;; Ensure the buffer still exists and is the previously created one
           (when (and (get-buffer output-buffer-name) (buffer-live-p (get-buffer output-buffer-name)))
             (kill-buffer output-buffer-name))))
       process))))


(defun wechat--show-chat-messages (json-str)
  "Show Chat Detail in a separate buffer."
  (let* ((json (wechat--json-parse-string json-str))
         (title (gethash "title" json))
         (msgs (gethash "messages" json))
         (input (when wechat--input-marker
                  (buffer-substring
                   (marker-position wechat--input-marker)
                   (point-max))))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create (format "*WeChat-%s*" title))
      (erase-buffer)
      (wechat--insert-chat-detail msgs input)
      (message "Chat Detail Updated"))))

(defun wechat-show (&optional chat-name only-visible)
  "Show a chat by CHAT-NAME."
  (interactive)
  (unless chat-name
    (setq chat-name
          (completing-read "Select an chat: "
                           wechat-common-chats)))
  (switch-to-buffer (get-buffer-create (format "*WeChat-%s*" chat-name)))
  (wechat-chat-mode 1)
  (setq-local wechat--chat-title chat-name)
  (wechat--refresh-messages chat-name))

(defun wechat-send-in-chat ()
  "Send a message entered in the chat."
  (interactive)
  (let ((title wechat--chat-title)
        (msg))
    (save-excursion
      (goto-char (marker-position wechat--input-marker))
      (setq msg (buffer-substring (point) (point-max)))
      (delete-region (point) (point-max)))
    (wechat-send title msg)))

(defun wechat-show-at-point ()
  "Display chat messages at the current point."
  (interactive)
  (wechat-show (tabulated-list-get-id)))

(defun wechat--refresh-messages (title &optional only-visible)
  (wechat--run-process (list
                        wechat-cli
                        "show"
                        title
                        "-o"
                        (if only-visible "true" "false")
                        "-f"
                        "json")
                       #'wechat--show-chat-messages))

(defun wechat--show-chats (json-str)
  "Show all chats."
  (with-current-buffer (get-buffer-create "*WeChat Chats*")
    (let ((json (wechat--json-parse-string json-str))
          (buffer-read-only))
      (erase-buffer)
      (wechat-chat-list-mode)
      (setq tabulated-list-entries
            (wechat--chats-to-tabulated-entries json))
      (tabulated-list-print t)
      ;; (wechat--set-margin wechat-chat-list-region-max-width)
      (goto-char (point-min)))
    (message "Chats Updated.")))

(defun wechat--refresh-chats (&optional only-visible)
  (wechat--run-process
   (list
    wechat-cli
    "list-chats"
    "-o"
    (if only-visible "true" "false")
    "-f"
    "json")
   #'wechat--show-chats))

(defun wechat-list-chats (&optional only-visible)
  "List All Chats."
  (interactive)
  (switch-to-buffer (get-buffer-create "*WeChat Chats*"))
  (wechat--refresh-chats))

(defun wechat-send (&optional chat-name message)
  "Send a message to the chat named CHAT-NAME."
  (interactive)
  (unless chat-name
    (setq chat-name
          (completing-read "Select an chat: "
                           wechat-common-chats)))
  (unless message
    (setq message (read-string (format "Send to (%s) :" chat-name))))
  (wechat--run-process (list
                        wechat-cli
                        "send"
                        chat-name
                        message)
                       (lambda (json) (wechat--refresh-messages chat-name))))

(defun wechat-preview (&optional chat-name index)
  "Preview the index-th message from the chat named CHAT-NAME."
  (interactive)
  (unless chat-name
    (setq chat-name
          (completing-read "Select an chat: "
                           wechat-common-chats)))
  (unless index
    (setq index (read-string "Input an index :")))
  (wechat--run-process (list
                        wechat-cli
                        "preview"
                        chat-name
                        index)
                       (lambda (json) ())))

(defun wechat-refresh-messages ()
  "Refresh messages"
  (interactive)
  (when wechat--chat-title
    (wechat--refresh-messages wechat--chat-title)))

(defun wechat-refresh-chats ()
  "Refresh the chat list"
  (interactive)
  (wechat--refresh-chats))

(defun wechat-check-in-foreground ()
  "Check if Emacs is foreground"
  (wechat--run-process
   (list
    "osascript"
    "-e"
    "tell application \"System Events\" to get name of first application process whose frontmost is true")
   #'wechat-check-unread))


(defun wechat-check-unread (str)
  "Check unread."
  (when (string= "Emacs" (string-trim str))
    (wechat--run-process
     (list
      wechat-cli
      "list-chats"
      "-o"
      "true"
      "-f"
      "json")
     #'wechat--check-unread)))

(defun wechat--check-unread (json-str)
  "Check unread by response JSON-STR."
  (let* ((json (wechat--json-parse-string json-str))
         (unreads (seq-filter (lambda (item)
                                (> (gethash "unread" item) 0))
                              json)))
    (setq wechat--unreads unreads)
    (when (and wechat--chat-title
               (seq-find
                (lambda (item)
                  (string= wechat--chat-title (gethash "title" item)))
                wechat--unreads))
      (wechat-refresh-messages))))

(defun wechat-start-notification ()
  "Start a timer to check notification."
  (interactive)
  (unless wechat--notification-timer
    (setq wechat--notification-timer
          (run-with-idle-timer wechat-notification-time
                               wechat-notification-time #'wechat-check-in-foreground))))

(defun wechat-restart-notification ()
  "Restart a timer to check notification."
  (interactive)
  (wechat-stop-notification)
  (wechat-start-notification))

(defun wechat-stop-notification ()
  "Stop a timer to check notification."
  (interactive)
  (when wechat--notification-timer
    (cancel-timer wechat--notification-timer)
    (setq wechat--notification-timer nil)))

(defun wechat-awesome-tray-notification ()
  (let ((unread-str
         (string-join
          (mapcar
           (lambda (item)
             (format
              "%s:%s"
              (propertize (gethash "title" item)
                          'face 'wechat-message-user)
              (propertize (number-to-string (gethash "unread" item))
                          'face 'wechat-unread)))
           wechat--unreads)
          ";"))))
  (when (not (string-empty-p unread-str))
    (concat
     wechat-unread-symbol
     unread-str)))

(if (featurep 'awesome-tray)
    (add-to-list 'awesome-tray-module-alist
                 '("wechat-notification" . (wechat-awesome-tray-notification))))

(define-derived-mode wechat-chat-list-mode tabulated-list-mode "Wechat Dialogues"
  "Major mode for handling a list of Wechat Dialogue."
  (let* ((max-width (min (window-width) wechat-chat-list-region-max-width))
         (padding 9)
         (state-width 2)
         (last-date-width 20)
         (unread-width 10)
         (rest-width (- max-width state-width last-date-width unread-width padding))
         (title-width (/ rest-width 3))
         (last-message-width (- rest-width title-width)))
    (setq tabulated-list-format (vector
                                 (list "" state-width t)
                                 (list "Title" title-width t)
                                 (list "Unread" unread-width  t :right-align t)
                                 (list "Last Message" last-message-width t)
                                 (list "Last Date" last-date-width t  :right-align t))))
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (setq buffer-read-only t)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'wechat-show-at-point)
    (define-key map (kbd "<f5>") #'wechat-refresh-chats)
    (use-local-map map)))


(define-minor-mode wechat-chat-mode
  "Wechat chat mode."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<return>") #'wechat-send-in-chat)
            (define-key map (kbd "<f5>") #'wechat-refresh-messages)
            map)
  :init-value nil)

(provide 'wechat)
;;; wechat.el ends here
