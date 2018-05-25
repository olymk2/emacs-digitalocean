;;; digitalocean.el --- Create and manipulate digitalocean droplets -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Oliver Marks

;; Author: Oliver Marks <oly@digitaloctave.com>
;; URL: https://github.com/olymk2/digitalocean-api
;; Keywords: Processes tools
;; Version: 0.1
;; Created 01 July 2018
;; Package-Requires: ((requests "2.5")(emacs "24.3")(widget ""))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implid warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides a set of magit style popups for interacting with your containers.
;; It wraps docker and docker-compose commands and allows you to select containers and toggle
;; the various paramters passed to the commands.

;; It can be extended to run tests inside containers and comes with some predefined setups, it
;; should also be easy to add in your own common commands to the interactive popups

;;; Code:

(require 'request)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

;;(defcustom do-default-directory nil)

(defun do/array-or-nil (value)
  ;; Helper which will convert strings to a list or nil if an empty string
  (if (= (length value) 0)
      nil
      (list value)))

(defun do/align-labels (txt size)
  ;; Helper which will space out strings to be X in length
  (setq num (length txt))
  (while (< num size)
    (setq txt (concat txt " "))
    (setq num (1+ num)))
  txt)


(defun make-get-request (url)
  ;; Get request wrapper which auto appends the header tokens 
  (if (not (boundp 'digitalocean-token)) 
      (error "User variable digitalocean-token not set"))
  (request-response-data
   (request url
	    :parser 'json-read
	    :headers `(
		       ("Content-Type" . "application/json")
		       ("Authorization" . ,digitalocean-token))
	    :error
	    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
			   (message "Digitalocean GET request errored: %S %S"
				    args error-thrown)))
	    :sync t)))

(defun make-post-request (url params)
  ;; Post request wrapper which auto appends the header tokens 
  (if (not (boundp 'digitalocean-token)) 
      (error "User variable digitalocean-token not set"))
  (request-response-data
   (request url
	    :type "POST"
            :data (json-encode params)
	    :parser 'json-read
	    :headers `(
		       ("Content-Type" . "application/json")
		       ("Authorization" . ,digitalocean-token))
            :success
            (cl-function (lambda (&key data &allow-other-keys)
			   (message "Post request complete")
                           (kill-buffer "*DO Form*")))
	    :error
	    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
			   (message "Digitalocean POST request errored: %S %S"
				    args error-thrown)))
	    :sync t)))


(defun do/fetch-droplets ()
  ;;Fetch droplet list endpoint 
  (make-get-request "https://api.digitalocean.com/v2/droplets"))

(defun do/fetch-images ()
  ;;Fetch image list endpoint 
  (make-get-request "https://api.digitalocean.com/v2/images"))

(defun do/fetch-regions ()
  ;;Fetch region list endpoint 
  (make-get-request "https://api.digitalocean.com/v2/regions"))

(defun do/fetch-sizes ()
  ;;Fetch instance sizes endpoint
  (make-get-request "https://api.digitalocean.com/v2/sizes"))

(defun do/fetch-account-info ()
  ;;Fetch account info
  (make-get-request "https://api.digitalocean.com/v2/account"))

(defun do/exec-droplet-action (droplet-id action)
  ;;Execute the give action on a specific droplet Endpoint
  (interactive "sDroplet Id: \nsAction :")
  (make-post-request
   (concat "https://api.digitalocean.com/v2/droplets/" droplet-id "/actions")
   `(("type" . ,action))))

(defun do/fetch-droplet-by-id (droplet-id)
  ;;Return specific droplet details
  (interactive "sDroplet Id: ")
  (make-get-request
   (concat "https://api.digitalocean.com/v2/droplets/" droplet-id)))


(defun do/format-results (alist &rest keys)
  ;;Helper function to return only the values we are intrested in
  (if keys
      (string-join
       (loop for x in keys collect
	     (format "%s" (cdr (assoc x alist)))) " ")
    (string-join
     (loop for (k v) in alist collect
	   (format "%s" v " ")))))

(defun do/format-results-sep (alist &rest keys)
  (concat "| "
	  (string-join
	   (loop for x in keys
		 collect (format "%s" (cdr (assoc x alist)))
		 ) " | ") " |"))

(defun do/format-response (res head &rest keys)
  ;; Helper to filter responses into cons format for helm
  (if (consp (cdr (assoc head res)))
      ;; cons list so pass direct to format lsit
      (progn
	(cons head (apply 'do/format-results (cdr (assoc head res)) keys)))
    ;; vector array so loop over all items
    (mapcar #'(lambda (x)
		(cons 
		 (apply 'do/format-results x keys)
		 x))
	    (cdr (assoc head res)))))


(defun do/format-response-lines (res head &rest keys)
  ;; Helper to filter responses into cons format for helm

  (if (consp (cdr (assoc head res)))
      ;; cons list so pass direct to format lsit
      (progn
	(list (apply 'do/format-results (cdr (assoc head res)) keys)))
    ;; vector array so loop over all items
    (mapcar #'(lambda (x)
		(apply 'do/format-results x keys))
	    (cdr (assoc head res)))))

(defun do/insert-lines (res)
  (mapcar #'(lambda (x) (insert (concat x "\n"))) res))


(defun do/digitalocean-report ()
  ;; Pull back info from digitalocean account into org buffer
  (interactive)
  (with-current-buffer 
      (get-buffer-create "*Digitalocean*") 
    (org-mode)
    (erase-buffer)
    (insert "#+TITLE: Digitalocean report\n")
    (insert "* Account Info\n")
    (do/insert-lines (do/format-response-lines
    		      (do/fetch-account-info) 'account 'email))
    (insert "* Droplets\n")
    (do/insert-lines (do/format-response-lines
    		      (do/fetch-droplets) 'droplets 'id 'name))
    (insert "* Sizes\n")
    (do/insert-lines (do/format-response-lines
		      (do/fetch-sizes) 'sizes 'vcpus 'memory 'disk 'price_hourly))
    (insert "* Images\n")
    (do/insert-lines (do/format-response-lines (do/fetch-images) 'images 'distribution 'name))))

(defun do/digitalocean-droplet-list ()
  ;; return list of droplets with specific attributes
  (do/format-response (do/fetch-droplets) 'droplets 'id 'name 'status))

(defun do/digitalocean-images-list ()
  ;; Return list of Image
  (do/format-response (do/fetch-images) 'images 'name))

(defun do/digitalocean-regions-list ()
  ;; Return list of region slugs
  (do/format-response (do/fetch-regions) 'regions 'slug))

(defun do/digitalocean-sizes-list ()
  ;; Return list of sizes
  (do/format-response (do/fetch-sizes) 'sizes 'slug ))

(defun do/create-droplet (values)
  (interactive)
  (make-post-request
   "https://api.digitalocean.com/v2/droplets"
   values))

;;(do/create-droplet-test)
(defun do/create-droplet-test ()
  ;; TODO remove in future good for quickly testing
  (interactive)
  (make-post-request "https://api.digitalocean.com/v2/droplets"
		     `(("name" . "elisp")
		      ("region" . "lon1")
                      ("size" . "512mb")
                      ("image" . "ubuntu-16-04-x64"))))


(defun do/create-droplet-user ()
  (interactive)
  (do/create-droplet 
   (read-string "Droplet Name: ")
   (ido-completing-read
    "Select Region: "
    (car (list (loop for (k v) in 
		     (do/format-response (do/fetch-regions) 'regions 'slug)
		collect k))))
   (ido-completing-read
    "Select Size: "
    (car (list (loop for (k, v) in
		     (do/format-response (do/fetch-sizes) 'sizes 'slug)
	  collect k))))
   (ido-completing-read
    "Select Image: "
    (car (list (loop for (k, v) in
		     (do/format-response (do/fetch-images) 'images 'slug)))))))



(defun do/get-droplet-id-from-name-str (droplet-name)
  (number-to-string (do/get-droplet-id-from-name droplet-name)))


(defun do/get-droplet-id-from-name (droplet-name)
  ;; Give a droplet name, search the droplets and return a matching id
   (cdr (assoc 'id
	       (cdr (assoc droplet-name
			   (do/format-response
			    (do/fetch-droplets)
                            'droplets 'name))))))


(defun do/get-droplet-ip4-from-id (droplet-id)
  ;; Give a droplet id, lookup the droplet and get the ipv4 address
  (cdr (assoc 'ip_address
	      (elt (cdr (assoc 'v4
			  (cdr (assoc 'networks
				      (cdr (assoc 'droplet
						  (do/fetch-droplet-by-id droplet-id))))))) 0))))



(defun do/launch-shell (droplet-name dir)
  ;;Simple shell wrapper, used with build ssh path to opne a shell with a tramp path
  (interactive "sShell name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell (concat "*do-" droplet-name "*"))))

(defun do/build-ssh-path (candidate dir)
  ;;Give a helm candidiate return a tramp ssh path
  (format "/ssh:root@%s:%s"
	  (do/get-droplet-ip4-from-id
	   (do/get-droplet-id-from-candidate candidate)) dir))





  ;;(defvar widget-example-repeat)

  ;;(defun align-spaces (string size)
  ;;  (setq num (length string))
  ;;  (while (< num size)
  ;;    (add-to-list string " ")
  ;;    (setq num (1+ num)))
  ;;  string)


(defun do/create-droplet-form ()
  ;; Implements a form to create a droplet based on the below url`
  ;; https://developers.digitalocean.com/documentation/v2/#create-a-new-droplet
  (interactive)
  "Create the widgets from the Widget manual."
  (switch-to-buffer "*DO Form*")
  (kill-all-local-variables)
  
  (setq-local do/widgets ())
  
  (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (widget-insert "Digitalocean droplet creation\n")
  (widget-insert (do/align-labels "\nName: " 30))
  (add-to-list 'do/widgets 
	       (widget-create 'editable-field
			      :size 25
			      "droplet-name"))

  (widget-insert (do/align-labels "\nRegion: " 30))
  (add-to-list 'do/widgets 
	       (widget-create 'editable-field
			      :size 25
			      "lon1"))
    
  (widget-insert (do/align-labels "\nSize: " 30))
  (add-to-list 'do/widgets 
	       (widget-create 'editable-field
			      :size 25
			      "512mb"))

  (widget-insert (do/align-labels "\nImage: " 30))
  (add-to-list 'do/widgets 
	       (widget-create 'editable-field
			      :size 25
			      "ubuntu-16-04-x64"))
  
  (widget-insert "\n\nOptional fields")
  (widget-insert (do/align-labels "\nSSH Keys: " 30))
  (add-to-list 'do/widgets
	       (widget-create 'editable-field
			      :size 25
			      ""))
  
  (widget-insert (do/align-labels "\nBackups: " 30))
  (add-to-list 'do/widgets 
	       (widget-create 'checkbox nil))
  
  (widget-insert (do/align-labels "\nEnable IPV6: " 30))
  (add-to-list 'do/widgets 
	       (widget-create 'checkbox t))
  
  (widget-insert (do/align-labels "\nPrivate Networking: " 30))
  (add-to-list 'do/widgets 
	       (widget-create 'checkbox nil))
  
  (widget-insert "\nUser data can be used to run commands at launch")
  (widget-insert (do/align-labels "\nUser Commands: " 30))
  (add-to-list 'do/widgets 
	       (widget-create 'text
			      :size 25
			      ""))
  
  (widget-insert (do/align-labels "\nMonitoring: " 30))
  (add-to-list 'do/widgets
	       (widget-create 'checkbox t))
  
  (widget-insert (do/align-labels "\nVolumes: " 30))
  (add-to-list 'do/widgets
	       (widget-create 'editable-field
			      :size 25
			      ""))
  
  (widget-insert (do/align-labels "\nTags: " 30))
  (add-to-list 'do/widgets
	       (widget-create 'editable-field
			      :size 25
			      "from-emacs"))
  
  (widget-insert "\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore) 
			   (let ((values `(("name" . ,(widget-value (nth 11 do/widgets)))
					   ("region" . ,(widget-value (nth 10 do/widgets)))
					   ("size" . ,(widget-value (nth 9 do/widgets)))
					   ("image" . ,(widget-value (nth 8 do/widgets)))
					   ("ssh_keys" . ,(do/array-or-nil
							   (widget-value (nth 7 do/widgets))))
					   ("backups" . ,(widget-value (nth 6 do/widgets)))
					   ("ipv6" . ,(widget-value (nth 5 do/widgets)))
					   ("private_networking" . ,(widget-value (nth 4 do/widgets)))
					   ("user_data" . ,(widget-value (nth 3 do/widgets)))
					   ("monitoring" . ,(widget-value (nth 2 do/widgets)))
					   ("volumes" . ,(do/array-or-nil
							  (widget-value (nth 1 do/widgets))))
					   ("tags" . ,(do/array-or-nil
						       (widget-value (nth 0 do/widgets)))))))
			     (message "Please wait new droplet sent for creation.")
			     (do/create-droplet values)))
			   "Create droplet")
		 (widget-insert "\n")
		 (use-local-map widget-keymap)
		 (widget-setup))

;;; (Features)
(provide 'digitalocean)
;;; digitalocean.el ends here
