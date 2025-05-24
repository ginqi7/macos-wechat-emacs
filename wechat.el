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

;; This Emacs Lisp package provides an interface for interacting with a
;; `wechat-cli` command-line tool. It allows users to list chats,
;; view message history, send messages, and receive notifications for unread
;; messages within Emacs.

;;; Code:

;; Require necessary libraries
(require 'wechat-emoji)
(require 'shr)          ; For `shr-pixel-column` and related text properties
(require 'json)         ; For JSON parsing

(defgroup wechat nil
  "Customization group for WeChat related Emacs extensions or configurations."
  :group 'applications
  :prefix "wechat-")

;; Customization variables
(defcustom wechat-cli (executable-find "wechat")
  "The path of wechat-cli command.
Ensure 'wechat' executable is in your system's PATH or provide the full path."
  :type 'file
  :group 'wechat)

(defcustom wechat-common-chats nil
  "The list of wechat-cli Common Contacts.
This list can be used for tab completion when selecting chats."
  :type '(repeat string)
  :group 'wechat)

(defcustom wechat-input-prompt ">>> "
  "The prompt displayed before the user's input area in a chat buffer."
  :type 'string
  :group 'wechat)

(defcustom wechat-message-seperator ">"
  "The separator used between the user name and the message content."
  :type 'string
  :group 'wechat)

(defcustom wechat-message-date-seperator "-"
  "The character used to create the horizontal lines around date and 'Input' sections."
  :type 'string
  :group 'wechat)

(defcustom wechat-message-region-max-width 120
  "The maximum width for the message display region in chat buffers.
Messages will be wrapped to fit within this width, centered in the window."
  :type 'integer
  :group 'wechat)

(defcustom wechat-chat-list-region-max-width 170
  "The maximum width for the chat list display region in the '*WeChat Chats*' buffer.
The chat list will be centered within this width."
  :type 'integer
  :group 'wechat)

(defcustom wechat-mute-symbol "🔕"
  "The symbol to express a muted chat."
  :type 'string
  :group 'wechat)

(defcustom wechat-stick-symbol "❤️"
  "The symbol to express a sticky (pinned) chat."
  :type 'string
  :group 'wechat)

(defcustom wechat-unread-symbol "📩"
  "The symbol to express unread messages."
  :type 'string
  :group 'wechat)

(defcustom wechat-notification-time 3
  "The interval (in seconds) at which to check for new notifications."
  :type 'integer
  :group 'wechat)

;; Faces for styling
(defface wechat-message-user
  '((t (:foreground "#7757d6")))
  "Face for displaying the user's name in chat messages."
  :group 'wechat)

(defface wechat-unread
  '((t (:foreground "red" :weight bold)))
  "Face for displaying unread message counts."
  :group 'wechat)

(defvar wechat--unreads nil
  "List of unread chat items (parsed JSON hash tables).")

(defvar wechat--chats-json-str nil
  "The raw JSON string of the last fetched chat list. Used for comparison.")

(defvar wechat--input-marker nil
  "Marker pointing to the beginning of the user input area in a chat buffer.")
(make-variable-buffer-local 'wechat--input-marker)

(defvar wechat--chat-title nil
  "The title of the currently displayed chat buffer.")
(make-variable-buffer-local 'wechat--chat-title)

(defvar wechat--notify-process nil
  "The WeChat Notification Process.")

;; Helper functions

(defun wechat--chats-to-tabulated-entries (json-array)
  "Convert a JSON array of chat objects to `tabulated-list-mode` entries.
JSON-ARRAY is a list of hash tables, each representing a chat."
  (mapcar (lambda (item)
            (let* ((title (gethash "title" item))
                   (message-mute (string= "t" (gethash "messageMute" item)))
                   (stick (string= "t" (gethash "stick" item)))
                   (unread-count (gethash "unread" item))
                   (last-message (gethash "lastMessage" item))
                   (last-date (gethash "lastDate" item))
                   (status-symbol (cond
                                   (message-mute wechat-mute-symbol)
                                   (stick wechat-stick-symbol)
                                   (t "")))
                   (unread-display (if (> unread-count 0)
                                       (propertize (format "%d %s" unread-count wechat-unread-symbol)
                                                   'face 'wechat-unread)
                                     ""))
                   ;; Clean up common unwanted characters from last message
                   (cleaned-last-message (replace-regexp-in-string "￼" "." (replace-regexp-in-string "\n" "" last-message))))
              (list title
                    (vector
                     status-symbol
                     title
                     unread-display
                     cleaned-last-message
                     last-date))))
          json-array))

(defun wechat--json-parse-string (json-str)
  "Parse JSON-STR into an Emacs Lisp object (hash table or list).
Signals an error if parsing fails."
  (condition-case err
      (json-parse-string json-str)
    (json-parse-error
     (error "JSON parse Error: %s\nInput string: %s" (cadr err) json-str))))

(defun wechat--format-date (date-str)
  "Format a date string with separators for display in chat buffer.
DATE-STR is the date string (e.g., '2025-05-23') or 'Input'."
  (let* ((width (min wechat-message-region-max-width (window-width)))
         ;; Calculate half-width for separators, accounting for date string length
         (half-width (max 0 (- (/ width 2) (ceiling (/ (string-width date-str) 2)))))
         (split-line (make-string half-width (aref wechat-message-date-seperator 0))))
    (format "\n%s %s %s\n\n" split-line date-str split-line)))

(defun wechat--format-emoji (msg)
  "Replace emoji placeholders like '[smile]' in MSG with actual emoji images.
Uses `wechat-emoji-format-image` from the 'wechat-emoji' library."
  ;; Use `replace-regexp-in-string` for more efficient and idiomatic replacement
  (replace-regexp-in-string "\\[.*?\\]"
                            (lambda (match) (wechat-emoji-format-image match))
                            msg t t)) ; t t for literal match and not to match empty string

(defun wechat--format-message (hash-msg)
  "Format a single chat message (HASH-MSG) for display.
Handles special message types like web pages, videos, and images by making
them clickable buttons to preview the content."
  (let* ((user (propertize (gethash "user" hash-msg)
                           'face 'wechat-message-user))
         (msg (gethash "message" hash-msg))
         (index (gethash "index" hash-msg)))
    (if (or (string-prefix-p "发送了一个网页," msg)
            (string-prefix-p "发送了一个视频," msg)
            (string-prefix-p "发送了一个语音," msg)
            (string= msg "发送了一个图片"))
        ;; For special message types, create a clickable button
        (format "%s%s%s \n" user
                wechat-message-seperator
                (buttonize
                 msg
                 (lambda (data) ; data is the button's value, which is the message string itself
                   (wechat-preview wechat--chat-title (number-to-string index)))
                 'help-echo (format "Click to preview message #%s" index)))
      ;; For regular text messages, format emojis
      (format "%s %s %s \n" user wechat-message-seperator
              (wechat--format-emoji msg)))))

(defun wechat--set-margin (max-width)
  "Set left and right margins for the current buffer to center content.
MAX-WIDTH is the desired content width."
  (let* ((actual-max-width (min max-width (window-width)))
         (margin (max 0 (/ (- (window-width) actual-max-width) 2)))
         (inhibit-read-only t)) ; Allow modification of buffer properties
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
      (if (< pixel-width (* 1.1 max-pixel-width))
          (beginning-of-line 2)
        (wechat--forward-pixel (- max-pixel-width 10))
        (insert "\n# " wechat-message-seperator " ")))))

(defun wechat--forward-pixel (pixel)
  (while (and (< (point) (line-end-position 1))
              (< (shr-pixel-column) pixel))
    (forward-char 1)))

(defun wechat--insert-chat-detail (messages input)
  "Insert chat messages and input prompt into the current buffer.
MESSAGES is a list of message hash tables. INPUT is the current user input."
  (let ((date "")
        (inhibit-read-only t)) ; Temporarily allow buffer modification
    ;; Insert messages, adding date separators as needed
    (mapc (lambda (message)
            (unless (string= date (gethash "date" message))
              (setq date (gethash "date" message))
              (insert (wechat--format-date date)))
            (insert (wechat--format-message message)))
          messages)

    ;; Insert input prompt and user's current input
    (insert (wechat--format-date "Input"))
    (wechat--wrap-long-message) ; Wrap the messages before alignment
    ;; Align message content based on the separator
    (align-regexp (point-min) (point-max) (format "\\(\\s-*\\)%s" wechat-message-seperator))
    ;; Make the displayed messages read-only
    (add-text-properties (point-min) (1- (point-max)) '(read-only t))

    ;; Insert the input prompt, making it read-only and non-sticky
    (insert (propertize wechat-input-prompt
                        'rear-nonsticky t
                        'front-sticky t
                        'read-only t))
    ;; Set buffer margins to center the content
    (wechat--set-margin wechat-message-region-max-width)

    ;; Set the input marker to the current point (start of user input area)
    (setq-local wechat--input-marker (point-marker))

    ;; If there was previous input, insert it
    (when input
      (insert input))))

(defun wechat--run-process (program-and-args callback-fn)
  "Run PROGRAM-AND-ARGS asynchronously and call CALLBACK-FN with its output.
PROGRAM-AND-ARGS is a list where the first element is the program path and
the rest are arguments. CALLBACK-FN is a function that takes one parameter:
the complete output string from the process."
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

    ;; Set sentinel function to be called when the process finishes
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
  "Show chat messages parsed from JSON-STR in the current chat buffer.
This function is typically used as a callback from `wechat--run-process`."
  (let* ((json (wechat--json-parse-string json-str))
         (title (gethash "title" json))
         (msgs (gethash "messages" json))
         (input (when wechat--input-marker
                  ;; Get current input from the marker to the end of buffer
                  (buffer-substring (marker-position wechat--input-marker)
                                    (point-max))))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create (format "*WeChat-%s*" title))
      (erase-buffer) ; Clear existing content
      (wechat--insert-chat-detail msgs input)
      (message "Chat Detail Updated: %s" title))))

;; User-facing commands

(defun wechat-show (&optional chat-name)
  "Show a chat by CHAT-NAME.
If CHAT-NAME is not provided, prompts the user for selection from common chats."
  (interactive)
  (unless chat-name
    (setq chat-name
          (completing-read "Select a chat: "
                           wechat-common-chats)))
  ;; Switch to or create the chat buffer
  (switch-to-buffer (get-buffer-create (format "*WeChat-%s*" chat-name)))
  ;; Activate chat mode for keybindings
  (wechat-chat-mode 1)
  ;; Set buffer-local chat title
  (setq-local wechat--chat-title chat-name)
  ;; Refresh messages for the selected chat
  (wechat--refresh-messages chat-name))

(defun wechat-send-in-chat ()
  "Send the message currently entered in the active chat buffer.
The message is taken from the input area of the current buffer."
  (interactive)
  (let ((title wechat--chat-title)
        (msg ""))
    (unless title
      (error "Not in a WeChat chat buffer."))
    (unless wechat--input-marker
      (error "Input marker not set. Cannot send message."))

    (save-excursion
      ;; Go to the input marker and get the text from there to the end of buffer
      (goto-char (marker-position wechat--input-marker))
      (setq msg (buffer-substring (point) (point-max)))
      ;; Delete the sent message from the input area
      (delete-region (point) (point-max)))
    ;; Call the generic send function
    (wechat-send title msg)))

(defun wechat-show-at-point ()
  "Display chat messages for the chat entry at the current line in the chat list.
This function is intended for use in the '*WeChat Chats*' buffer."
  (interactive)
  (wechat-show (tabulated-list-get-id)))

(defun wechat--refresh-messages (title &optional only-visible)
  "Internal function to refresh messages for a given TITLE.
If ONLY-VISIBLE is t, requests only visible messages from `wechat-cli`."
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
  "Internal function to display all chats parsed from JSON-STR.
This function is typically used as a callback from `wechat--run-process`."
  (with-current-buffer (get-buffer-create "*WeChat Chats*")
    (let ((json (wechat--json-parse-string json-str))
          (buffer-read-only))
      ;; Store the raw JSON string and parsed JSON for later comparison
      (setq wechat--chats-json-str json-str)
      (erase-buffer) ; Clear existing content
      (wechat-chat-list-mode) ; Ensure chat list mode is active
      ;; Populate tabulated list entries
      (setq tabulated-list-entries
            (wechat--chats-to-tabulated-entries json))
      (tabulated-list-print t)
      (goto-char (point-min)))
    (message "Chats Updated.")))

(defun wechat--refresh-chats (&optional only-visible)
  "Internal function to refresh the list of chats.
If ONLY-VISIBLE is t, requests only visible chats from `wechat-cli`."
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
  "List all chats in a new buffer ('*WeChat Chats*')."
  (interactive)
  (switch-to-buffer (get-buffer-create "*WeChat Chats*"))
  (wechat--refresh-chats))

(defun wechat-send (&optional chat-name message)
  "Send a MESSAGE to the chat named CHAT-NAME.
If arguments are not provided, prompts the user."
  (interactive)
  (unless chat-name
    (setq chat-name
          (completing-read "Select a chat: "
                           wechat-common-chats)))
  (unless message
    (setq message (read-string (format "Send to (%s): " chat-name))))
  (wechat--run-process (list
                        wechat-cli
                        "send"
                        chat-name
                        message)
                       ;; Refresh messages after sending
                       (lambda (output)
                         (message "Message sent to %s: %s" chat-name output)
                         (wechat--refresh-messages chat-name))))

(defun wechat-preview (&optional chat-name index)
  "Preview the INDEX-th message from the chat named CHAT-NAME.
This is typically used for rich content messages (web pages, videos, images)."
  (interactive)
  (unless chat-name
    (setq chat-name
          (completing-read "Select a chat: "
                           wechat-common-chats)))
  (unless index
    (setq index (read-string "Input message index to preview: ")))
  (wechat--run-process (list
                        wechat-cli
                        "preview"
                        chat-name
                        index)
                       (lambda (json) ())))

(defun wechat-refresh-messages ()
  "Refresh messages in the currently active chat buffer."
  (interactive)
  (when wechat--chat-title
    (wechat--refresh-messages wechat--chat-title)))

(defun wechat-refresh-chats ()
  "Refresh the chat list in the '*WeChat Chats*' buffer."
  (interactive)
  (wechat--refresh-chats))

;; Notification related functions
(defun wechat-check-unread (foreground-app-name-str)
  "Check for unread messages if Emacs is the foreground application.
FOREGROUND-APP-NAME-STR is the output from `wechat-check-in-foreground`."
  (wechat--run-process
   (list
    wechat-cli
    "list-chats"
    "-o"
    "true"
    "-f"
    "json")
   #'wechat--process-unread-check-result))

(defun wechat--process-unread-check-result (json-str)
  "Process the JSON-STR containing chat list for unread messages.
This function is called as a callback from `wechat-check-unread`."
  (let* ((new-json-parsed (wechat--json-parse-string json-str))
         (unreads (seq-filter (lambda (item)
                                (> (gethash "unread" item) 0))
                              new-json-parsed)))
    (setq wechat--unreads unreads)

    ;; If we are in the chat list buffer and the chat list has changed, refresh it.
    ;; Use `cl-equalp` for robust comparison of hash tables.
    (when (and (equal major-mode 'wechat-chat-list-mode)
               (not (string-prefix-p
                     (substring json-str 4 -3)
                     (substring wechat--chats-json-str 4))))
      (wechat-refresh-chats))
    ;; If there's an active chat buffer and it has unread messages, refresh it.
    (when (and wechat--chat-title
               (seq-find
                (lambda (item)
                  (string= wechat--chat-title (gethash "title" item)))
                wechat--unreads))
      (wechat-refresh-messages))))

(defun wechat--notify-filter (process string)
  (wechat-check-unread nil))

(defun wechat-start-notification ()
  "Start a timer to periodically check for notifications."
  (interactive)
  (unless wechat--notify-process
    (let* ((process-name "wechat-notify")
           (command wechat-cli)
           (args '("notify"))
           (process (apply 'start-process process-name nil  command args)))
      (setq wechat--notify-process process)
      (set-process-filter process 'wechat--notify-filter)
      (message "Wechat Notify process has started..."))))

(defun wechat-restart-notification ()
  "Restart the notification timer. Stops and then starts it."
  (interactive)
  (wechat-stop-notification)
  (wechat-start-notification))

(defun wechat-stop-notification ()
  "Stop the notification timer."
  (interactive)
  (when wechat--notify-process
    (kill-process wechat--notify-process)
    (setq wechat--notify-process nil)))

(defun wechat-awesome-tray-notification ()
  "Function to provide notification string for `awesome-tray`."
  (when wechat--unreads
    (let* ((first-unread (car wechat--unreads))
           (title (gethash "title" first-unread))
           (unread-count (gethash "unread" first-unread)))
      (concat
       wechat-unread-symbol
       (format
        "%s:%s"
        (propertize title 'face 'wechat-message-user)
        (propertize (number-to-string unread-count) 'face 'wechat-unread))))))

;; Integrate with `awesome-tray` if available
(if (featurep 'awesome-tray)
    (add-to-list 'awesome-tray-module-alist
                 '("wechat-notification" . (wechat-awesome-tray-notification))))

;; Major mode for the chat list buffer
(define-derived-mode wechat-chat-list-mode tabulated-list-mode "Wechat Dialogues"
  "Major mode for handling a list of Wechat Dialogue."
  (let* ((max-width (min (window-width) wechat-chat-list-region-max-width))
         (padding 9)
         (state-width 2)
         (last-date-width 20)
         (unread-width 10)
         ;; Calculate remaining width for title and last message
         (rest-width (max 0 (- max-width state-width last-date-width unread-width padding)))
         (title-width (floor (/ rest-width 3))) ; Allocate 1/3 for title
         (last-message-width (- rest-width title-width))) ; Remaining for last message
    (setq tabulated-list-format (vector
                                 (list "" state-width t)
                                 (list "Title" title-width t)
                                 (list "Unread" unread-width  t :right-align t)
                                 (list "Last Message" last-message-width t)
                                 (list "Last Date" last-date-width t  :right-align t))))
  (tabulated-list-init-header) ; Initialize header based on format
  (tabulated-list-print t)     ; Print the initial list
  (setq buffer-read-only t)   ; Make the buffer read-only
  ;; Define local keymap for this mode
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'wechat-show-at-point) ; Enter to show chat
    (define-key map (kbd "<f5>") #'wechat-refresh-chats) ; F5 to refresh chat list
    (use-local-map map)))

;; Minor mode for individual chat buffers
(define-minor-mode wechat-chat-mode
  "Wechat chat mode.
Provides keybindings for sending messages and refreshing the chat."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<return>") #'wechat-send-in-chat) ; Enter to send message
            (define-key map (kbd "<f5>") #'wechat-refresh-messages) ; F5 to refresh messages
            map)
  :init-value nil)

(provide 'wechat)
;;; wechat.el ends here
