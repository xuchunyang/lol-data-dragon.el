;;; lol-data-dragon.el --- Browse Champions of League of Legends on Data Dragon -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/lol-data-dragon.el
;; Package-Requires: ((emacs "25.1"))
;; Version: 0
;; Keywords: games hypermedia

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

;; Browse Champions of League of Legends on Data Dragon

;; [[https://developer.riotgames.com/ddragon.html][Riot Developer Portal]]

;;; Code:

(require 'seq)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'org)                          ; `org-mode'
(require 'image-dired)

(defgroup lol-data-dragon nil
  "Browse Champions of League of Legends on Data Dragon."
  :group 'applications)

(defun lol-data-dragon-versions ()
  "Return a list of ddragon versions (strings), the first is the latest."
  (with-current-buffer (url-retrieve-synchronously
                        "https://ddragon.leagueoflegends.com/api/versions.json")
    ;; `url-http-end-of-headers' seems to be 1 after cache is used
    ;; (goto-char url-http-end-of-headers)
    (goto-char (point-min))
    (re-search-forward "^\r?\n")
    (let ((json-array-type 'list))
      (json-read))))

(defun lol-data-dragon-url ()
  "Return url to the latest version of data dragon.
Such as the URL `https://ddragon.leagueoflegends.com/cdn/dragontail-10.3.1.tgz'."
  (format "https://ddragon.leagueoflegends.com/cdn/dragontail-%s.tgz"
          (car (lol-data-dragon-versions))))

(defcustom lol-data-dragon-dir
  (pcase (expand-file-name
          "dragontail-10.3.1/"
          (file-name-directory
           (or load-file-name buffer-file-name)))
    ((and (pred file-exists-p) dir) dir))
  "Directory to dragontail.
Such as '~/src/lol-data-dragon.el/dragontail-10.3.1/'."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "Directory to dragontail"))
  :group 'lol-data-dragon)

(defun lol-data-dragon-dir-main ()
  "Main directory in `lol-data-dragon-dir', such as '10.3.1'."
  (seq-find
   (lambda (x)
     (and (file-directory-p x)
          (string-match-p
           (rx (1+ (and (1+ num) ".")) (1+ num))
           (file-name-nondirectory x))))
   (directory-files lol-data-dragon-dir t)))

(defun lol-data-dragon-languages ()
  "Return a list of languages."
  (let ((json-array-type 'list))
    (json-read-file (expand-file-name "languages.json" lol-data-dragon-dir))))

(defvar lol-data-dragon-champions nil
  "Cache, use the function `lol-data-dragon-champions' instead.")

(defun lol-data-dragon-champions ()
  "Return a list of champions IDs."
  (unless lol-data-dragon-champions
    (setq lol-data-dragon-champions
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
               (lol-data-dragon-dir-main))))))))
  lol-data-dragon-champions)

(defun lol-data-dragon--fill-string (string)
  "Fill STRING."
  (with-temp-buffer
    (insert string)
    (fill-region (point-min) (point-max))
    (buffer-string)))

(defun lol-data-dragon--json-read-file (file)
  "Read first JSON object in FILE and return it."
  (let ((json-object-type 'alist)
        (json-array-type  'list)
        (json-key-type    'symbol)
        (json-false       nil)
        (json-null        nil))
    (json-read-file file)))

;; XXX need cache? hash table or C-h P memoize
(defun lol-data-dragon-champion-data (id lang)
  "Return champion ID's data in LANG."
  (alist-get
   (intern id)
   (alist-get
    'data
    (lol-data-dragon--json-read-file
     (expand-file-name (format "data/%s/champion/%s.json" lang id)
                       (lol-data-dragon-dir-main))))))

;;;###autoload
(defun lol-data-dragon-champion-show-QWER (id lang)
  "Show QWER of a champion by ID in LANG."
  (interactive (list (completing-read "Champion: " (lol-data-dragon-champions))
                     (completing-read "Language: " (lol-data-dragon-languages))))
  (with-current-buffer (get-buffer-create (format "*%s*" id))
    (read-only-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let-alist (lol-data-dragon-champion-data id lang)
        (insert (concat .name " " .title) "\n\n")
        (when (display-graphic-p)
          (insert-image (create-image
                         (expand-file-name
                          (format "img/champion/%s.png" id)
                          (lol-data-dragon-dir-main))))
          (insert "\n\n"))
        (insert
         (string-join
          `(,(format "(P) %s\n\n%s"
                     .passive.name
                     (lol-data-dragon--fill-string
                      .passive.description))
            ,@(seq-mapn (lambda (key spell)
                          (format "(%c) %s\n\n%s"
                                  key
                                  (alist-get 'name spell)
                                  (lol-data-dragon--fill-string
                                   (replace-regexp-in-string
                                    (rx "<br>") "\n"
                                    (alist-get 'description spell)))))
                        "QWER"
                        .spells))
          "\n\n")))
      (goto-char (point-min)))
    (display-buffer (current-buffer))))

(defun lol-data-dragon-file-version (filename)
  "Get version number in FILENAME.
E.g., return 15 with Ahri_15.jpg."
  (and (string-match (rx (+ num)) filename)
       (string-to-number (match-string 0 filename))))

(defun lol-data-dragon-file-sort-by-version (filenames)
  "Sort FILENAMES by version from old to new."
  (sort filenames (lambda (f1 f2)
                    (< (lol-data-dragon-file-version f1)
                       (lol-data-dragon-file-version f2)))))

;;;###autoload
(defun lol-data-dragon-champion-show-skins (id lang)
  "Show skins of the champion ID.
LANG is the language that the skin name is in."
  (interactive (list (completing-read "Champion: " (lol-data-dragon-champions))
                     (pcase current-prefix-arg
                       ('nil "en_US")
                       (_    (completing-read "Language: " (lol-data-dragon-languages))))))
  (let ((skins (alist-get 'skins (lol-data-dragon-champion-data id lang)))
        (getfile (lambda (num)
                   ;; ~/src/lol-data-dragon.el/dragontail-10.3.1/img/champion/splash/Aatrox_0.jpg
                   (concat (expand-file-name "img/champion/splash/" lol-data-dragon-dir)
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
(defun lol-data-dragon-champion-show-tiles (id)
  "Show tiles of the champion ID."
  (interactive (list (completing-read "Champion: " (lol-data-dragon-champions))))
  (let* ((dir (expand-file-name "img/champion/tiles/" lol-data-dragon-dir))
         (files (mapcar
                 (lambda (f) (expand-file-name f dir))
                 (lol-data-dragon-file-sort-by-version
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

(defun lol-data-dragon-n-random (n list)
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
(defun lol-data-dragon-random-random-tiles (n)
  "Display N random tiles."
  (interactive (list (pcase current-prefix-arg
                       ('nil 10)
                       (_ (read-number "N random tiles: ")))))
  (let* ((dir (expand-file-name "img/champion/tiles/" lol-data-dragon-dir))
         (files (lol-data-dragon-n-random n (directory-files dir 'full nil 'nosort)))
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

(defvar lol-data-dragon-champions-data-table
  (make-hash-table :test #'equal
                   ;; (length (lol-data-dragon-languages))
                   ;; => 27
                   :size 30)
  "Cache for `lol-data-dragon-champions-data'.
The key will be the lang, the value will be the data.")

(defun lol-data-dragon-champions-data (lang)
  "Return all champions' data in LANG as a list."
  (pcase (gethash lang lol-data-dragon-champions-data-table)
    ('nil
     (let ((data
            (alist-get
             'data
             (lol-data-dragon--json-read-file
              ;; ~/src/lol-data-dragon.el/dragontail-10.3.1/10.3.1/data/en_US/champion.json (134K)
              ;; ~/src/lol-data-dragon.el/dragontail-10.3.1/10.3.1/data/en_US/championFull.json (3.4M)
              (expand-file-name
               (format "data/%s/champion.json" lang)
               (lol-data-dragon-dir-main))))))
       (puthash lang data lol-data-dragon-champions-data-table)
       data))
    (data data)))

;;;###autoload
(defun lol-data-dragon-list-champions-in-org-table (lang)
  "List all champions in Org mode table using language LANG."
  (interactive (list (completing-read "Lang: " (lol-data-dragon-languages))))
  (with-current-buffer (get-buffer-create (format "*Champions (%s)*" lang))
    (erase-buffer)
    (insert "|ID|Name|Title|Tags|\n")
    (dolist (c (lol-data-dragon-champions-data lang))
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

(defvar lol-data-dragon-default-language "en_US"
  "The default language to use.")

(defun lol-data-dragon-image-dired-champion-at-point ()
  "Get champion name at point in `image-dired-thumbnail-mode'."
  (cl-assert (derived-mode-p 'image-dired-thumbnail-mode))
  (file-name-sans-extension
   (file-name-nondirectory
    (image-dired-original-file-name))))

(defun lol-data-dragon-image-dired-show-QWER ()
  "Show QWER for champion at point."
  (interactive)
  (lol-data-dragon-champion-show-QWER
   (lol-data-dragon-image-dired-champion-at-point)
   lol-data-dragon-default-language))

(defun lol-data-dragon-image-dired-show-skins ()
  "Show skins for champion at point."
  (interactive)
  (lol-data-dragon-champion-show-skins
   (lol-data-dragon-image-dired-champion-at-point)
   lol-data-dragon-default-language))

(defun lol-data-dragon-image-dired-show-tiles ()
  "Show tiles for champion at point."
  (interactive)
  (lol-data-dragon-champion-show-tiles
   (lol-data-dragon-image-dired-champion-at-point)))

(defvar lol-data-dragon-image-dired-thumbnail-mode-info-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'lol-data-dragon-image-dired-show-QWER)
    (define-key map "s" #'lol-data-dragon-image-dired-show-skins)
    (define-key map "\r" #'lol-data-dragon-image-dired-show-skins)
    (define-key map "t" #'lol-data-dragon-image-dired-show-tiles)
    map)
  "Keymap for lol-data-dragon commands in `image-dired-thumbnail-mode'.")

(defvar lol-data-dragon-image-dired-thumbnail-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-dired-thumbnail-mode-map)
    (define-key map "x" lol-data-dragon-image-dired-thumbnail-mode-info-map)
    map)
  "Keymap for `lol-data-dragon-image-dired-thumbnail-mode'.")

(define-minor-mode lol-data-dragon-image-dired-thumbnail-mode
  "Minor Mode to setup our own key in `image-dired-thumbnail-mode'.")

;;;###autoload
(defun lol-data-dragon-champion-image-dired ()
  "Show all champions using `image-dired'."
  (interactive)
  ;; There are 145 champions in League of Legends as of Aug 7, 2019
  (let ((image-dired-show-all-from-dir-max-files 200))
    (image-dired (expand-file-name "img/champion" (lol-data-dragon-dir-main)))
    (lol-data-dragon-image-dired-thumbnail-mode)))

(provide 'lol-data-dragon)
;;; lol-data-dragon.el ends here
