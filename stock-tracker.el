;;; stock-tracker.el --- Track stock price -*- lexical-binding: t; -*-

(require 'async)
(require 'dash)
(require 'desktop)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'text-property-search)
(require 'url)
(require 'which-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup stock-tracker nil
  "Track stock price in Emacs."
  :version "0.1"
  :group 'tools)

(defcustom stock-tracker-list-of-stocks '("BABA")
  "List of stock to monitor."
  :type 'list
  :group 'stock-tracker)

(defcustom stock-tracker-subprocess-kill-delay 12
  "Kill subprocess in N * 10 SECS."
  :type 'integer
  :group 'stock-tracker)

(defcustom stock-tracker-enable-log nil
  "Display log messages."
  :type 'boolean
  :group 'stock-tracker)

(defcustom stock-tracker-up-red-down-green t
  "Display up as red, down as green, set nil to reverse this."
  :type 'boolean
  :group 'stock-tracker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct stock-tracker--chn-symbol)

(cl-defgeneric stock-tracker--api-url (object)
  "Stock-Tracker API template for stocks listed in SS, SZ, HK, US basd on OBJECT.")

(cl-defmethod stock-tracker--api-url ((s stock-tracker--chn-symbol))
  "API to get stock for S from CHN."
  (ignore s)
  "https://stock.xueqiu.com/v5/stock/realtime/quotec.json?symbol=%s")

(cl-defgeneric stock-tracker--result-prefix (object)
  "Stock-Tracker result prefix based on OBJECT.")

(cl-defmethod stock-tracker--result-prefix ((s stock-tracker--chn-symbol))
  "Stock-Tracker result prefix for S from CHN."
  (ignore s)
  "data\":\\[")

(cl-defgeneric stock-tracker--result-fields (object)
  "Stock-Tracker result fields based on OBJECT.")

(cl-defmethod stock-tracker--result-fields ((s stock-tracker--chn-symbol))
  "Stock-Tracker result fields for S from CHN."
  (ignore s)
  '((symbol . symbol)
    (current . current)
    (percent . percent)
    (chg . chg)
    (volume . volume)
    (amount . amount)
    (market_capital . market_capital)
    (float_market_capital . float_market_capital)
    (turnover_rate . turnover_rate)
    (amplitude . amplitude)
    (open . open)
    (last_close . last_close)
    (high . high)
    (low . low)
    (avg_price . avg_price)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst stock-tracker--buffer-name "*stock-tracker*"
  "Stock-Tracker result Buffer name.")

(defconst stock-tracker--result-header
  ;; "|-\n| symbol | current | percent | chg | volume | amount | market_capital | float_market_capital | turnover_rate | amplitude | open | last_close | high | low | avg_price |\n"
  "|-\n| 股票代码 | 现价 | 涨幅 | 涨幅 | 总手 | 金额 | 总巿值 | 流通值 | 换手 | 振幅 | 开盘 | 昨收 | 最高 | 最低 | 均价 |\n"
  "Stock-Tracker result header.")

(defconst stock-tracker--result-item-format
  "|-\n| %s | %s | %.2f%% | %.2f | %s | %s | %s | %s | %.2f%% | %.2f%% | %s | %s | %s | %s | %s |\n"
  "Stock-Tracker result item format.")

(defconst stock-tracker--response-buffer "*api-response*"
  "Buffer name for error report when fail to read server response.")

(defconst stock-tracker--header-string
  "* Stocks refreshed at: [ %current-time% ] auto-refreshing is: [ %refresh-state% ]"
  "Stock-Tracker header string.")

(defvar stock-tracker--refresh-timer nil
  "Stock-Tracker refresh timer.")

(defvar stock-tracker--check-timer nil
  "Stock-Tracker check timer.")

(defvar stock-tracker--data-timestamp (time-to-seconds)
  "Stock-Tracker latest data timestamp.")

(defvar stock-tracker--data nil
  "Stock-Tracker latest data.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stock-tracker--align-colorize-tables ()
  "Align all org tables and do colorization."
  (org-table-map-tables 'org-table-align t)
  (stock-tracker--colorize-content))

;;; @see https://www.emacswiki.org/emacs/AddCommasToNumbers
(defun stock-tracker--add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
Optional SEPARATOR is the string to use to separate groups.
It defaults to a comma."
  (let ((num (number-to-string number))
        (op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat
                 (match-string 1 num) op
                 (match-string 2 num))))
    num))

(defun stock-tracker--get-chn-stocks (stocks)
  "Separate chn stock from us stock with `STOCKS'."
  (let ((chn-stocks nil))
    (dolist (stock stocks)
      (unless (string-empty-p stock)
        (push stock chn-stocks)))
    (setq chn-stocks (reverse chn-stocks))))

(defun stock-tracker--list-to-string (string-list separter)
  "Concat STRING-LIST to a string with SEPARTER."
  (mapconcat #'identity string-list separter))

(defun stock-tracker--kill-hanging-subprocess()
  "Kill hanging *emacs* subprocess."
  (dolist (buffer (mapcar #'buffer-name (buffer-list)))
    (when (string-match "*emacs*" buffer)
      (when-let ((process (get-buffer-process buffer)))
        ;; set the process as killable without query by default
        (set-process-query-on-exit-flag process nil)
        (delete-process process)
        (sit-for 0.5))
      (and (get-buffer buffer)
           (null (get-buffer-process buffer))
           (kill-buffer buffer)))))

(defun stock-tracker--log (message)
  "Log MESSAGE."
  (when stock-tracker-enable-log
    (with-temp-message message
      (sit-for 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stock-tracker--request-synchronously (stock tag)
  "Get STOCK data with TAG synchronously, return a list of JSON each as alist."
  (let (jsons)
    (ignore-errors
      (with-current-buffer
          (url-retrieve-synchronously
           (format (stock-tracker--api-url tag) (url-hexify-string stock)) t nil 5)
        (set-buffer-multibyte t)
        (goto-char (point-min))
        (when (string-match "200 OK" (buffer-string))
          (re-search-forward (stock-tracker--result-prefix tag) nil 'move)
          (setq
           jsons
           (json-read-from-string (buffer-substring-no-properties (point) (point-max)))))
        (kill-current-buffer)))
    jsons))

(defun stock-tracker--format-json (json tag)
  "Format stock information from JSON with TAG."
  (let ((result-filds (stock-tracker--result-fields tag))
        symbol current percent color
        volume amount market_capital float_market_capital turnover_rate amplitude open last_close high low)

    (setq
      symbol               (assoc-default (map-elt result-filds 'symbol)               json)
      current              (assoc-default (map-elt result-filds 'current)              json)
      percent              (assoc-default (map-elt result-filds 'percent)              json)
      chg                  (assoc-default (map-elt result-filds 'chg)                  json)
      volume               (assoc-default (map-elt result-filds 'volume)               json)
      amount               (assoc-default (map-elt result-filds 'amount)               json)
      market_capital       (assoc-default (map-elt result-filds 'market_capital)       json)
      float_market_capital (assoc-default (map-elt result-filds 'float_market_capital) json)
      turnover_rate        (assoc-default (map-elt result-filds 'turnover_rate)        json)
      amplitude            (assoc-default (map-elt result-filds 'amplitude)            json)
      open                 (assoc-default (map-elt result-filds 'open)                 json)
      last_close           (assoc-default (map-elt result-filds 'last_close)           json)
      high                 (assoc-default (map-elt result-filds 'high)                 json)
      low                  (assoc-default (map-elt result-filds 'low)                  json)
      avg_price            (assoc-default (map-elt result-filds 'avg_price)            json))

    ;; sanity check
    (unless (and symbol current percent volume amount market_capital float_market_capital turnover_rate amplitude open last_close high low)
      (stock-tracker--log "Invalid data received !!!")
      (throw 'break 0))

    ;; formating
    ;; (and (stringp volume)    (setq volume (string-to-number volume)))

    ;; color setting
    (if stock-tracker-up-red-down-green
        (if (> chg 0) (setq color "red") (setq color "green"))
      (if (> chg 0) (setq color "green") (setq color "red")))

    ;; construct data for display
    (and symbol
         (propertize
          (format stock-tracker--result-item-format symbol current percent chg
                  (stock-tracker--add-number-grouping volume ",")
                  (stock-tracker--add-number-grouping amount ",")
                  (stock-tracker--add-number-grouping market_capital ",")
                  (stock-tracker--add-number-grouping float_market_capital ",")
                  turnover_rate amplitude open last_close high low avg_price)
          'stock-code  symbol
          'stock-color color))))

(defun stock-tracker--format-response (response tag &optional asynchronously)
  "Format stock information from RESPONSE with TAG, with optional ASYNCHRONOUSLY."
  (let ((jsons response)
        (result "") result-list)
    (catch 'break
      ;; handle difference in async handling
      (and asynchronously
           (cl-typep tag 'stock-tracker--chn-symbol))

        (when-let ((info (stock-tracker--format-json jsons tag)))
          (push info result-list))

          (throw 'break t))
    (and result-list
         (setq result (stock-tracker--list-to-string (reverse result-list) "")))
    result))

(defun stock-tracker--colorize-content ()
  "Colorize stock base on price."
  (let ((ended nil) pos beg end (color "red"))
    (goto-char (point-min))
    ; colorize timestamp
    (re-search-forward "%current-time%" nil 'move)
    (let ((ov (make-overlay (- (point) (length "%current-time%")) (point))))
      (overlay-put ov 'display (current-time-string))
      (overlay-put ov 'face '(:foreground "green"))
      (overlay-put ov 'intangible t))
    ; colorize refresh state
    (re-search-forward "%refresh-state%" nil 'move)
    (let ((ov (make-overlay (- (point) (length "%refresh-state%")) (point))))
      (if stock-tracker--refresh-timer
          (progn
            (overlay-put ov 'face '(:foreground "green"))
            (overlay-put ov 'display "ON"))
        (overlay-put ov 'face '(:foreground "red"))
        (overlay-put ov 'display "OFF"))
      (overlay-put ov 'intangible t))
    ; colorize table
    (while (not ended)
      (setq pos (next-single-property-change (point) 'stock-code)
            beg pos end beg
            color (and pos (get-text-property pos 'stock-color nil)))
      (if (not pos) (setq ended t) ; done
        (goto-char pos)
        (setq end (line-end-position)))
      (while (and end (<= (point) end) (re-search-forward "|" nil 'move))
        (and beg (1- (point))
             ;; (propertize "Red Text" 'font-lock-face '(:foreground "red"))
             ;; propertize doesn't work in org-table-cell, so use overlay here
             (overlay-put (make-overlay beg (1- (point))) 'face `(:foreground ,color))
             (setq beg (point)))))))

(defun stock-tracker--refresh-content (stocks-info)
  "Refresh stocks with STOCKS-INFO."
  (and stocks-info
       (null (seq-empty-p stocks-info))
       (with-current-buffer (get-buffer-create stock-tracker--buffer-name)
         (let ((inhibit-read-only t)
               (origin (point)))
           (setq stock-tracker--data stocks-info)
           (erase-buffer)
           (stock-tracker-mode)
           (which-function-mode -1)
           (font-lock-mode 1)
           (insert (format "%s\n\n" stock-tracker--header-string))
           (insert stock-tracker--result-header)
           (dolist (info stocks-info) (insert info))
           ;; (insert "|-\n")
           (stock-tracker--align-colorize-tables)
           (goto-char origin)
           (set-buffer-modified-p nil)))))

(defun stock-tracker--refresh-async (chn-stocks)
  "Refresh list of stocks namely CHN-STOCKS and US-STOCKS."
  (let* ((chn-stocks-string (mapconcat #'identity chn-stocks ","))
         (chn-symbol (make-stock-tracker--chn-symbol))
         (data-retrieve-timestamp (time-to-seconds)))

    (stock-tracker--log "Fetching stock data async ...")

    ;; start subprocess
    (async-start

     ;; What to do in the child process
     `(lambda ()

        ;; libraries
        (require 'subr-x)
        (require 'url)

        ;; pass params to subprocess, use literal (string, integer, float) here
        (setq subprocess-chn-stocks-string ,chn-stocks-string
              subprocess-kill-delay ,stock-tracker-subprocess-kill-delay)

        (defun stock-tracker--subprocess-request-synchronously (stock string-tag)
          "Get stock data synchronously, return a list of JSON each as alist."
          (let (jsons)
            (ignore-errors
              (with-current-buffer
                  (url-retrieve-synchronously
                   (format "https://stock.xueqiu.com/v5/stock/realtime/quotec.json?symbol=%s" (url-hexify-string stock)) t nil 5)
                (set-buffer-multibyte t)
                (goto-char (point-min))
                (when (string-match "200 OK" (buffer-string))
                  (re-search-forward "data\":\\[" nil 'move)
                  (setq
                   jsons
                   (json-read-from-string (buffer-substring-no-properties (point) (point-max)))))
                (kill-current-buffer)))
            jsons))

        ;; make sure subprocess can exit successfully
        (progn
          (setq kill-buffer-query-functions
                (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

          (when (>= emacs-major-version 28)
            (setq backtrace-on-error-noninteractive nil))

          ;; setup self-destruction timer
          (run-with-timer (* 10 subprocess-kill-delay) 10 (lambda () (kill-emacs))))

        ;; do real business here
        (let ((result '((chn-stock . 0)))
              (chn-result nil))

          ;; fetch chn stocks
          (unless (string-empty-p subprocess-chn-stocks-string)
            (dolist (chn-stock (split-string subprocess-chn-stocks-string ","))
              (push
               (stock-tracker--subprocess-request-synchronously chn-stock "chn-stock") chn-result))
            (when chn-result (map-put! result 'chn-stock chn-result)))

          result))

     ;; What to do when it finishes
     (lambda (result)
       (let ((chn-result (cdr (assoc 'chn-stock result)))
             (all-collected-stocks-info nil))

         (if (< data-retrieve-timestamp stock-tracker--data-timestamp)

             (stock-tracker--log "Outdated data received !!!")

           ;; update timestamp
           (setq stock-tracker--data-timestamp data-retrieve-timestamp)

           (stock-tracker--log "Fetching stock done")

           ;; format chn stocks
           (unless (numberp chn-result)
             (dolist (chn-stock chn-result)
               (push (stock-tracker--format-response chn-stock chn-symbol t)
                     all-collected-stocks-info)))

           ;; populate stocks
           (when all-collected-stocks-info
             (stock-tracker--refresh-content (reverse all-collected-stocks-info)))))))))

(defun stock-tracker--refresh (&optional asynchronously)
  "Refresh list of stocks ASYNCHRONOUSLY or not."
  (when-let* ((has-stocks stock-tracker-list-of-stocks)
              (valid-stocks (delq nil (delete-dups has-stocks)))) ;; 去掉相同和空项,提取有效项
    (let* ((chn-stocks (stock-tracker--get-chn-stocks valid-stocks))
           (all-collected-stocks-info nil)
           (chn-symbol (make-stock-tracker--chn-symbol)))
      (if asynchronously
          ;; asynchronously
          (stock-tracker--refresh-async chn-stocks)
        ;; synchronously
        (with-temp-message "Fetching stock data ..."
          (dolist (chn-stock chn-stocks)
            (push
             (stock-tracker--format-response (stock-tracker--request-synchronously chn-stock chn-symbol) chn-symbol)
             all-collected-stocks-info))
          (when all-collected-stocks-info
            (stock-tracker--refresh-content (reverse all-collected-stocks-info)))
          (setq stock-tracker--data-timestamp (time-to-seconds)))))))

(defun stock-tracker--run-timers ()
  "Run stock tracker timers."
  (setq stock-tracker--check-timer
        (run-with-timer (* 10 6 3)
                        (* 10 6 3)
                        'stock-tracker--kill-hanging-subprocess)
        stock-tracker--refresh-timer
        (run-with-timer (* 10 1)
                        (* 10 1)
                        'stock-tracker--refresh
                        t)
        ))

(defun stock-tracker--cancel-timers ()
  "Cancel stock tracker timers."
  (when stock-tracker--check-timer
    (cancel-timer stock-tracker--check-timer)
    (setq stock-tracker--check-timer nil))
  (when stock-tracker--refresh-timer
    (cancel-timer stock-tracker--refresh-timer)
    (setq stock-tracker--refresh-timer nil)))

(defun stock-tracker--cancel-timer-on-exit ()
  "Cancel timer when stock tracker buffer is being killed."
  (when (eq major-mode 'stock-tracker-mode)
    (stock-tracker--cancel-timers)))

(defun stock-tracker-stop-refresh ()
  "Stop refreshing stocks."
  (interactive)
  (when (and stock-tracker--data stock-tracker--refresh-timer)
    (cancel-timer stock-tracker--refresh-timer)
    (setq stock-tracker--refresh-timer nil)
    (stock-tracker--refresh-content stock-tracker--data)))

;;;###autoload
(defun stock-tracker-start ()
  "Start stock-tracker, show result in `stock-tracker--buffer-name' buffer."
  (interactive)
  (when stock-tracker-list-of-stocks
    (stock-tracker--cancel-timers)
    (stock-tracker--run-timers)
    (stock-tracker--refresh)
    (unless (get-buffer-window stock-tracker--buffer-name)
      (switch-to-buffer-other-window stock-tracker--buffer-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stock-tracker-add-stock ()
  "Add new stock in table."
  (interactive)
  (let* ((stock (format "%s" (read-from-minibuffer "stock? ")))
         (tag (make-stock-tracker--chn-symbol)))
    (when-let* ((recved-stocks-info (stock-tracker--format-response (stock-tracker--request-synchronously stock tag) tag))
                (success (not (string= "" recved-stocks-info))))
      (with-current-buffer stock-tracker--buffer-name
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert recved-stocks-info)
          (stock-tracker--align-colorize-tables)
          (setq stock-tracker-list-of-stocks (reverse stock-tracker-list-of-stocks))
          (push stock stock-tracker-list-of-stocks)
          (setq stock-tracker-list-of-stocks (reverse stock-tracker-list-of-stocks))
          (set-buffer-modified-p nil))))))

(defun stock-tracker-remove-stock ()
  "Remove STOCK from table."
  (interactive)
  (save-mark-and-excursion
    (with-current-buffer stock-tracker--buffer-name
      (let ((inhibit-read-only t)
            (list-of-stocks stock-tracker-list-of-stocks)
            code tmp-stocks)
        (beginning-of-line)
        (when-let (stock-code (text-property-search-forward 'stock-code))
          (while (setq code (pop list-of-stocks))
            (unless (equal (upcase code) (upcase (prop-match-value stock-code)))
              (push code tmp-stocks)))
          (when tmp-stocks
            (setq stock-tracker-list-of-stocks (reverse tmp-stocks))
            (org-table-kill-row)
            (re-search-backward "|-" nil 'move)
            (org-table-kill-row)
            (stock-tracker--align-colorize-tables)
            (set-buffer-modified-p nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar stock-tracker-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "a") 'stock-tracker-add-stock)
    (define-key map (kbd "d") 'stock-tracker-remove-stock)
    (define-key map (kbd "g") 'stock-tracker-start)
    (define-key map (kbd "s") 'stock-tracker-stop-refresh)
    map)
  "Keymap for `stock-tracker' major mode.")

;;;###autoload
(define-derived-mode stock-tracker-mode org-mode "stock-tracker"
  "Major mode for viewing Stock-Tracker result.
\\{stock-tracker-mode-map}"
  :group 'stock-tracker
  (buffer-disable-undo)
  (setq truncate-lines t
        buffer-read-only t
        show-trailing-whitespace nil)
  (setq-local line-move-visual t)
  (setq-local view-read-only nil)
  (setq desktop-globals-to-save
        (add-to-list 'desktop-globals-to-save 'stock-tracker-list-of-stocks))
  (add-hook 'kill-buffer-hook #'stock-tracker--cancel-timer-on-exit)
  (run-mode-hooks))


(provide 'stock-tracker)

;;; stock-tracker.el ends here
