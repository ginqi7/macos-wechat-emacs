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

(defcustom wechat-cli (executable-find "wechat")
  "The path of wechat-cli command.")

(defcustom wechat-common-chats nil
  "The path of wechat-cli Common Contacts.")

(defun wechat--chats-to-tabulated-entries (json)
  "Convert chats json to tabulated-entries."
  (mapcar (lambda (item)
            (list (gethash "title" item)
                  (vector
                   (gethash "title" item)
                   (gethash "lastMessage" item)
                   (gethash "lastDate" item)
                   (symbol-name (gethash "messageMute" item))
                   (symbol-name (gethash "stick" item))
                   (number-to-string (gethash "unread" item)))))
          json))

(defun wechat--format-date (date-str)
  "Format Chat Date."
  (let* ((width (window-width))
         (half-width (- (/ width 2) 10))
         (split-line (make-string half-width ?-)))
    (concat "\n"
            split-line
            date-str
            split-line
            "\n" "\n")))

(defun wechat--format-message (hash-msg)
  "Format Chat Message."
  (let ((user (gethash "user" hash-msg))
        (msg (gethash "message" hash-msg)))
    (format "%s > %s \n" user msg)))

(defun wechat--insert-chat-detail (messages)
  "Insert Chat Detail in Current buffer."
  (let ()
    (mapc (lambda (group)
            (insert (wechat--format-date (gethash "date" group)))
            (mapc (lambda (message)
                    (insert (wechat--format-message message)))
                  (gethash "messages" group)))
          messages)
    (insert
     (wechat--format-date "Input"))
    (insert "me > ")))

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

    (print process)
    (if (null process)
        (progn
          (kill-buffer output-buffer)
          (error "Failed to start the process: %s" (string-join program-and-args " "))))

    ;; Set sentinel function
    (set-process-sentinel
     process
     (lambda (proc msg)
       ;; msg is typically a string like "finished\n" or "exited abnormally with code X\n"
       (message "Process %s Event: %s" proc msg)
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

(defun wechat--show-chat-detail (json-str)
  "Show Chat Detail in a separate buffer."
  (let* ((json (json-parse-string json-str))
         (title (gethash "title" json))
         (messages (gethash "messages" json)))
    (with-current-buffer (get-buffer-create (format "*WeChat-%s*" title))
      (put-text-property (point-min) (point-max) 'read-only nil)
      (erase-buffer)
      (wechat--insert-chat-detail messages))))

(defun wechat-show (&optional chat-name)
  "Show a chat by CHAT-NAME."
  (interactive)
  (unless chat-name
    (setq chat-name
          (completing-read "Select an chat: "
                           wechat-common-chats)))
  (switch-to-buffer (get-buffer-create (format "*WeChat-%s*" chat-name)))
  (wechat--run-process (list
                        wechat-cli
                        "show"
                        chat-name
                        "-f"
                        "json")
                       #'wechat--show-chat-detail))

(defun wechat--send-in-chat ()
  "Send message in Chat."
  (interactive)
  (let ((title (substring (substring (buffer-name)
                                     0 (1- (length (buffer-name))))
                          (length "*WeChat-")))
        (msg))

    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "^me >")
      (setq msg (buffer-substring (point) (point-max)))
      (delete-region (point) (point-max)))

    (wechat-send title (string-trim msg))))

(defun wechat--show-at-point ()
  "Show Chat at point."
  (interactive)
  (wechat-show (tabulated-list-get-id)))


(defun wechat--show-chats (json-str)
  "Show all chats."
  (with-current-buffer (get-buffer-create "*WeChat Chats*")
    (let ((json (json-parse-string json-str))
          (buffer-read-only))
      (erase-buffer)
      (wechat-chat-list-mode)
      (setq tabulated-list-entries
            (wechat--chats-to-tabulated-entries json))
      (tabulated-list-print t)
      (goto-char (point-min)))))

(defun wechat-list-chats ()
  "List All Chats."
  (interactive)
  (switch-to-buffer (get-buffer-create "*WeChat Chats*"))
  (wechat-chat-mode)
  (wechat--run-process
   (list
    wechat-cli
    "list-chats"
    "-f"
    "json")
   #'wechat--show-chats))

(defun wechat-send (&optional chat-name message)
  "Send MESSAGE to CHAT-NAME."
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
                       (lambda (json-str)
                         (wechat-show chat-name))))

(define-derived-mode wechat-chat-list-mode tabulated-list-mode "Wechat Dialogues"
  "Major mode for handling a list of Wechat Dialogue."
  (setq tabulated-list-format [("Title" 30 t)
                               ("Last Message" 60 t)
                               ("Last Date" 20 t)
                               ("Mute" 10 t)
                               ("Stick" 10 t)
                               ("Unread" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'wechat--show-at-point)
    (use-local-map map)))


(define-minor-mode wechat-chat-mode-map
  "Wechat chat mode."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'wechat--send-in-chat)
            map)
  :init-value nil)

(provide 'wechat)
;;; wechat.el ends here
