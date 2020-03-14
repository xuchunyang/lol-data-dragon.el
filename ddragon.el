;;; ddragon.el --- Browse Data Dragon                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/ddragon.el
;; Package-Requires: ((emacs "25.1"))
;; Version: 0

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

;; Browse Champions of League of Legends

;; [[https://developer.riotgames.com/ddragon.html][Riot Developer Portal]]

;;; Code:

(require 'seq)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'org)                          ; `org-mode'
(require 'image-dired)

(defgroup ddragon nil
  "Browse Data Dragon."
  :group 'applications)

(defun ddragon-versions ()
  "Return a list of ddragon versions (strings), the first is the latest."
  (with-current-buffer (url-retrieve-synchronously
                        "https://ddragon.leagueoflegends.com/api/versions.json")
    ;; `url-http-end-of-headers' seems to be 1 after cache is used
    ;; (goto-char url-http-end-of-headers)
    (goto-char (point-min))
    (re-search-forward "^\r?\n")
    (let ((json-array-type 'list))
      (json-read))))

(defun ddragon-url ()
  "Return url to the latest version of data dragon.
Such as the URL `https://ddragon.leagueoflegends.com/cdn/dragontail-10.3.1.tgz'."
  (format "https://ddragon.leagueoflegends.com/cdn/dragontail-%s.tgz"
          (car (ddragon-versions))))

(defcustom ddragon-dir
  (pcase (expand-file-name
          "dragontail-10.3.1/"
          (file-name-directory
           (or load-file-name buffer-file-name)))
    ((and (pred file-exists-p) dir) dir))
  "Directory to dragontail.
Such as '~/src/ddragon.el/dragontail-10.3.1/'."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "Directory to dragontail"))
  :group 'ddragon)

(defun ddragon-dir-main ()
  "Main directory in `ddragon-dir', such as '10.3.1'."
  (seq-find
   (lambda (x)
     (and (file-directory-p x)
          (string-match-p
           (rx (1+ (and (1+ num) ".")) (1+ num))
           (file-name-nondirectory x))))
   (directory-files ddragon-dir t)))

(defun ddragon-languages ()
  "Return a list of languages."
  (let ((json-array-type 'list))
    (json-read-file (expand-file-name "languages.json" ddragon-dir))))

(defvar ddragon-champions nil
  "Cache, use the function `ddragon-champions' instead.")

(defun ddragon-champions ()
  "Return a list of champions IDs."
  (unless ddragon-champions
    (setq ddragon-champions
          (mapcar
           #'symbol-name
           (mapcar
            #'car
            (alist-get
             'data
             (json-read-file
              (expand-file-name
               ;; any language should work
               "data/en_US/champion.json"
               (ddragon-dir-main))))))))
  ddragon-champions)

(defun ddragon--fill-string (string)
  "Fill STRING."
  (with-temp-buffer
    (insert string)
    (fill-region (point-min) (point-max))
    (buffer-string)))

(defun ddragon--json-read-file (file)
  "Read first JSON object in FILE and return it."
  (let ((json-object-type 'alist)
        (json-array-type  'list)
        (json-key-type    'symbol)
        (json-false       nil)
        (json-null        nil))
    (json-read-file file)))

;; XXX need cache? hash table or C-h P memoize
(defun ddragon-champion-data (id lang)
  "Return champion ID's data in LANG."
  (alist-get
   (intern id)
   (alist-get
    'data
    (ddragon--json-read-file
     (expand-file-name (format "data/%s/champion/%s.json" lang id)
                       (ddragon-dir-main))))))

;;;###autoload
(defun ddragon-champion-show-QWER (id lang)
  "Show QWER of a champion by ID in LANG."
  (interactive (list (completing-read "Champion: " (ddragon-champions))
                     (completing-read "Language: " (ddragon-languages))))
  (with-current-buffer (get-buffer-create (format "*%s*" id))
    (read-only-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let-alist (ddragon-champion-data id lang)
        (insert (concat .name " " .title) "\n\n")
        (when (display-graphic-p)
          (insert-image (create-image
                         (expand-file-name
                          (format "img/champion/%s.png" id)
                          (ddragon-dir-main))))
          (insert "\n\n"))
        (insert
         (string-join
          `(,(format "(P) %s\n\n%s"
                     .passive.name
                     (ddragon--fill-string
                      .passive.description))
            ,@(seq-mapn (lambda (key spell)
                          (format "(%c) %s\n\n%s"
                                  key
                                  (alist-get 'name spell)
                                  (ddragon--fill-string
                                   (replace-regexp-in-string
                                    (rx "<br>") "\n"
                                    (alist-get 'description spell)))))
                        "QWER"
                        .spells))
          "\n\n")))
      (goto-char (point-min)))
    (display-buffer (current-buffer))))

(defun ddragon-file-version (filename)
  "Get version number in FILENAME.
E.g., return 15 with Ahri_15.jpg."
  (and (string-match (rx (+ num)) filename)
       (string-to-number (match-string 0 filename))))

(defun ddragon-file-sort-by-version (filenames)
  "Sort FILENAMES by version from old to new."
  (sort filenames (lambda (f1 f2)
                    (< (ddragon-file-version f1)
                       (ddragon-file-version f2)))))

;;;###autoload
(defun ddragon-champion-show-skins (id lang)
  "Show skins of the champion ID.
LANG is the language that the skin name is in."
  (interactive (list (completing-read "Champion: " (ddragon-champions))
                     (pcase current-prefix-arg
                       ('nil "en_US")
                       (_    (completing-read "Language: " (ddragon-languages))))))
  (let ((skins (alist-get 'skins (ddragon-champion-data id lang)))
        (getfile (lambda (num)
                   ;; ~/src/ddragon.el/dragontail-10.3.1/img/champion/splash/Aatrox_0.jpg
                   (concat (expand-file-name "img/champion/splash/" ddragon-dir)
                           (format "%s_%d.jpg" id num))))
        (bufname (format "*%s skins (%s)*" id lang)))
    (unless (get-buffer bufname)
      (with-current-buffer (get-buffer-create bufname)
        (dolist (sk skins)
          (insert (alist-get 'name sk) "\n")
          (let ((fname (funcall getfile (alist-get 'num sk))))
            (insert-image (create-image fname) fname))
          (insert "\n\n"))
        (goto-char (point-min))
        (read-only-mode)))
    (display-buffer bufname)))

;;;###autoload
(defun ddragon-champion-show-tiles (id)
  "Show tiles of the champion ID."
  (interactive (list (completing-read "Champion: " (ddragon-champions))))
  (let* ((dir (expand-file-name "img/champion/tiles/" ddragon-dir))
         (files (mapcar
                 (lambda (f) (expand-file-name f dir))
                 (ddragon-file-sort-by-version
                  (directory-files dir nil (rx-to-string `(and bos ,id))))))
         (bufname (format "*%s tiles*" id)))
    (unless (get-buffer bufname)
      (with-current-buffer (get-buffer-create bufname)
        (dolist (f files)
          (insert-image (create-image f) f)
          (when (>= (current-column) (window-width))
            (insert "\n")))
        (goto-char (point-min))
        (read-only-mode)))
    (display-buffer bufname)))

(defun ddragon-n-random (n list)
  "Return N random elements in LIST."
  (cl-assert (<= n (length list)))
  (let ((list (copy-sequence list))
        (result ()))
    (dotimes (_ n)
      (let ((elt (seq-elt list (random (seq-length list)))))
        (push elt result)
        (setq list (delete elt list))))
    result))

;;;###autoload
(defun ddragon-random-random-tiles (n)
  "Display N random tiles."
  (interactive (list (pcase current-prefix-arg
                       ('nil 10)
                       (_ (read-number "N random tiles: ")))))
  (let* ((dir (expand-file-name "img/champion/tiles/" ddragon-dir))
         (files (ddragon-n-random n (directory-files dir 'full nil 'nosort)))
         (bufname "*random tiles*"))
    (with-current-buffer (get-buffer-create bufname)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (f files)
          (insert-image (create-image f) f)
          (when (>= (current-column) (window-width))
            (insert "\n"))))
      (goto-char (point-min))
      (read-only-mode))
    (display-buffer bufname)))

(defvar ddragon-champions-data-table
  (make-hash-table :test #'equal
                   ;; (length (ddragon-languages))
                   ;; => 27
                   :size 30)
  "Cache for `ddragon-champions-data'.
The key will be the lang, the value will be the data.")

(defun ddragon-champions-data (lang)
  "Return all champions' data in LANG as a list."
  (pcase (gethash lang ddragon-champions-data-table)
    ('nil
     (let ((data
            (alist-get
             'data
             (ddragon--json-read-file
              ;; ~/src/ddragon.el/dragontail-10.3.1/10.3.1/data/en_US/champion.json (134K)
              ;; ~/src/ddragon.el/dragontail-10.3.1/10.3.1/data/en_US/championFull.json (3.4M)
              (expand-file-name
               (format "data/%s/champion.json" lang)
               (ddragon-dir-main))))))
       (puthash lang data ddragon-champions-data-table)
       data))
    (data data)))

;;;###autoload
(defun ddragon-list-champions-in-org-table (lang)
  "List all champions in Org mode table using language LANG."
  (interactive (list (completing-read "Lang: " (ddragon-languages))))
  (with-current-buffer (get-buffer-create (format "*Champions (%s)*" lang))
    (erase-buffer)
    (insert "|ID|Name|Title|Tags|\n")
    (dolist (c (ddragon-champions-data lang))
      (let-alist c
        (insert
         (format "|%s|\n"
                 (string-join (list .id .name .title (string-join .tags ", "))
                              "|")))))
    (org-mode)
    (goto-char (point-min))
    (org-table-insert-hline)
    (org-table-align)
    (display-buffer (current-buffer))))


;;; Image Dired

(defvar ddragon-default-language "en_US"
  "The default language to use.")

(defun ddragon-image-dired-champion-at-point ()
  "Get champion name at point in `image-dired-thumbnail-mode'."
  (cl-assert (derived-mode-p 'image-dired-thumbnail-mode))
  (file-name-sans-extension
   (file-name-nondirectory
    (image-dired-original-file-name))))

(defun ddragon-image-dired-show-QWER ()
  "Show QWER for champion at point."
  (interactive)
  (ddragon-champion-show-QWER
   (ddragon-image-dired-champion-at-point)
   ddragon-default-language))

(defun ddragon-image-dired-show-skins ()
  "Show skins for champion at point."
  (interactive)
  (ddragon-champion-show-skins
   (ddragon-image-dired-champion-at-point)
   ddragon-default-language))

(defun ddragon-image-dired-show-tiles ()
  "Show tiles for champion at point."
  (interactive)
  (ddragon-champion-show-tiles
   (ddragon-image-dired-champion-at-point)))

(defvar ddragon-image-dired-thumbnail-mode-info-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'ddragon-image-dired-show-QWER)
    (define-key map "s" #'ddragon-image-dired-show-skins)
    (define-key map "\r" #'ddragon-image-dired-show-skins)
    (define-key map "t" #'ddragon-image-dired-show-tiles)
    map)
  "Keymap for ddragon commands in `image-dired-thumbnail-mode'.")

(defvar ddragon-image-dired-thumbnail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-dired-thumbnail-mode-map)
    (define-key map "x" ddragon-image-dired-thumbnail-mode-info-map)
    map)
  "Keymap for `ddragon-image-dired-thumbnail-mode'.")

(define-minor-mode ddragon-image-dired-thumbnail-mode
  "Minor Mode to setup our own key in `image-dired-thumbnail-mode'.")

;;;###autoload
(defun ddragon-champion-image-dired ()
  "Show all champions using `image-dired'."
  (interactive)
  ;; There are 145 champions in League of Legends as of Aug 7, 2019
  (let ((image-dired-show-all-from-dir-max-files 200))
    (image-dired (expand-file-name "img/champion" (ddragon-dir-main)))
    (ddragon-image-dired-thumbnail-mode)))

(provide 'ddragon)
;;; ddragon.el ends here
