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

(defcustom do-default-directory nil
  "Set the default directory when connecting to a droplet."
  :type 'string
  :group 'digitalocean)

(defun do/array-or-nil (value)
  "Helper which will VALUE to a list or nil if empty."
  (if (= (length value) 0)
      nil
      (list value)))


(defun make-get-request (url)
  "Perform a get request on URL which auto append the header tokens."
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
  "Perform a post request on URL with PARAMS data auto append the header tokens."
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
            (cl-function (lambda ()
			   (message "Post request complete")
                           (kill-buffer "*DO Form*")))
	    :error
	    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
			   (message "Digitalocean POST request errored: %S %S"
				    args error-thrown)))
	    :sync t)))


(defun do/fetch-droplets ()
  "Fetch droplet list endpoint."
  (make-get-request "https://api.digitalocean.com/v2/droplets"))

(defun do/fetch-images ()
  "Fetch image list endpoint."
  (make-get-request "https://api.digitalocean.com/v2/images"))

(defun do/fetch-regions ()
  "Fetch region list endpoint."
  (make-get-request "https://api.digitalocean.com/v2/regions"))

(defun do/fetch-sizes ()
  "Fetch instance sizes endpoint."
  (make-get-request "https://api.digitalocean.com/v2/sizes"))

(defun do/fetch-account-info ()
  "Fetch account info."
  (make-get-request "https://api.digitalocean.com/v2/account"))

(defun do/create-droplet (values)
  "Post create droplet data.
Argument VALUES y."
  (make-post-request "https://api.digitalocean.com/v2/droplets" values))

(defun do/exec-droplet-action (droplet-id action)
  "Give a unique DROPLET-ID Execute the given ACTION on the droplet."
  (interactive "sDroplet Id: \nsAction :")
  (make-post-request
   (concat "https://api.digitalocean.com/v2/droplets/" droplet-id "/actions")
   `(("type" . ,action))))

(defun do/fetch-droplet-by-id (droplet-id)
  "Return specific droplet details for DROPLET-ID."
  (interactive "sDroplet Id: ")
  (make-get-request
   (concat "https://api.digitalocean.com/v2/droplets/" droplet-id)))

(defun do/format-results (alist &rest keys)
  "Helper function given ALIST to return only the values matching KEYS."
  (if keys
      (string-join
       (loop for x in keys collect
	     (format "%s " (cdr (assoc x alist)))))
    (string-join
     (loop for item in alist collect
	   (format "%s " (car item))))))

(defun do/format-response (res head &rest keys)
  "Helper to filter response RES response root HEAD KEYS to match in the reponse."
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
  "Helper to filter RES response HEAD KEYS into cons format for helm."
  (if (consp (cdr (assoc head res)))
      ;; cons list so pass direct to format lsit
      (progn
	(list (apply 'do/format-results (cdr (assoc head res)) keys)))
    ;; vector array so loop over all items
    (mapcar #'(lambda (x)
		(apply 'do/format-results x keys))
	    (cdr (assoc head res)))))


(defun do/digitalocean-droplet-list ()
  "Return list of droplets with specific attributes."
  (do/format-response (do/fetch-droplets) 'droplets 'id 'name 'status))

(defun do/digitalocean-images-list ()
  "Return list of Image."
  (do/format-response (do/fetch-images) 'images 'name))

(defun do/digitalocean-regions-list ()
  "Return list of region slugs."
  (do/format-response (do/fetch-regions) 'regions 'slug))

(defun do/digitalocean-sizes-list ()
  "Return list of sizes."
  (do/format-response (do/fetch-sizes) 'sizes 'slug ))


(defun do/get-droplet-id-from-name-str (droplet-name)
  "Given DROPLET-NAME try and match and return a droplet id as a string."
  (number-to-string (do/get-droplet-id-from-name droplet-name)))


(defun do/get-droplet-id-from-name (droplet-name)
  "Given DROPLET-NAME try and match and return a droplet id."
   (cdr (assoc 'id
	       (cdr (assoc droplet-name
			   (do/format-response
			    (do/fetch-droplets)
                            'droplets 'name))))))

(defun do/build-ssh-path (droplet-id dir)
  "Give a DROPLET-ID and DIR build a tramp ssh path."
  (format "/ssh:root@%s:%s"
	  (do/get-droplet-ip4-from-id droplet-id) dir))

(defun do/get-droplet-ip4-from-id (droplet-id)
  "Givne a DROPLET-ID, lookup the droplet and get the ipv4 address."
  (cdr (assoc 'ip_address
	      (elt (cdr (assoc 'v4
			  (cdr (assoc 'networks
				      (cdr (assoc 'droplet
						  (do/fetch-droplet-by-id droplet-id))))))) 0))))

(defun do/launch-shell (droplet-name dir)
  "Simple shell wrapper, create a sheel using DROPLET-NAME as the buffer at DIR location."
  (let ((default-directory dir))
    (shell (concat "*do-" droplet-name "*"))))

(defun do/droplet-shell (droplet-id droplet)
  "Given a DROPLET-ID and DROPLET alist create the dir and sent to launch shell."
  (do/launch-shell
   (cdr (assoc 'name (first droplet)))
   (do/build-ssh-path droplet-id "~/")))


(defun do/completing-read-friendly (msg res main key reskey)
  "Custom completing read which can handle key value completion.
Given MSG and RES response match the root key MAIN show KEY values.
match RESKEY and return the match and the dropplet response."
  (let ((match
	 (ido-completing-read msg
			      (mapcar #'(lambda (x)
					  (cdr (assoc key x)))
				      (cdr (assoc main res))))))
    (let ((result (seq-filter
			   #'(lambda (x)
			       (if (string= (cdr (assoc key x)) match)
				   t nil))
			   (cdr (assoc main res)))))

      (list (cdr
       (assoc reskey
	      (nth 0 result)))
	    (nth 0 result)))))


(defun do/completing-read (msg res main key)
  "Custom completing read which filters an api response.
Given MSG and RES response match the root key MAIN show KEY values."
  "helper to generate user selection from api response."
  (ido-completing-read msg
		       (mapcar #'(lambda (x)
				   (cdr (assoc key x)))
			       (cdr (assoc main res)))))

(defun do/droplet-completing-read ()
  "Completing read for droplets."
  (do/completing-read-friendly "Select Droplet: " (do/fetch-droplets) 'droplets 'name 'id))

(defun do/image-completing-read ()
  "Completing read for images."
  (do/completing-read "Select Images: " (do/fetch-images) 'images 'name))

(defun do/region-completing-read ()
  "Completing read for regions."
  (do/completing-read "Select Region: " (do/fetch-regions) 'regions 'slug))

(defun do/sizes-completing-read ()
  "Completing read for sizes."
  (do/completing-read "Select Size: " (do/fetch-sizes) 'sizes 'slug))



(defun do/create-droplet-form ()
  "Implements a form to create a droplet with all options."
  (interactive)
  "Create the widgets from the Widget manual."
  (switch-to-buffer "*DO Form*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((do/widgets ())) 
  
    (widget-insert "Digitalocean droplet creation\n")
    (widget-insert (do/align-labels "\nName: " 30))
    (push
     (widget-create 'editable-field
		    :size 25
		    "droplet-name")
     do/widgets)
    
    (widget-insert (do/align-labels "\nRegion: " 30))
    (push
     (widget-create 'editable-field
		    :size 25
		    "lon1")
     do/widgets)
    
    (widget-insert (do/align-labels "\nSize: " 30))
    (push
     (widget-create 'editable-field
		    :size 25
		    "512mb")
     do/widgets)
    
    (widget-insert (do/align-labels "\nImage: " 30))
    (push
     (widget-create 'editable-field
		    :size 25
		    "ubuntu-16-04-x64")
     do/widgets)
    
    (widget-insert "\n\nOptional fields")
    (widget-insert (do/align-labels "\nSSH Keys: " 30))
    (push
     (widget-create 'editable-field
		    :size 25
		    "")
     do/widgets)
    
    (widget-insert (do/align-labels "\nBackups: " 30))
    (push
     (widget-create 'checkbox nil)
     do/widgets)
    
    (widget-insert (do/align-labels "\nEnable IPV6: " 30))
    (push
     (widget-create 'checkbox t)
     do/widgets)
    
    (widget-insert (do/align-labels "\nPrivate Networking: " 30))
    (push
     (widget-create 'checkbox nil)
     do/widgets)
    
    (widget-insert "\nUser data can be used to run commands at launch")
    (widget-insert (do/align-labels "\nUser Commands: " 30))
    (push
     (widget-create 'text
		    :size 25
		    "")
     do/widgets)
    
    (widget-insert (do/align-labels "\nMonitoring: " 30))
    (push
     (widget-create 'checkbox t)
     do/widgets)
    
    (widget-insert (do/align-labels "\nVolumes: " 30))
    (push
     (widget-create 'editable-field
		    :size 25
		    "")
     do/widgets)
    
    (widget-insert (do/align-labels "\nTags: " 30))
    (push
     (widget-create 'editable-field
		    :size 25
		    "from-emacs")
     do/widgets)
    
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
    (widget-setup)))

;;; User droplet endpoints
(defun do/droplet-open-shell ()
  "Open a shell for selected droplet."
  (interactive)
  (let ((result (do/completing-read-friendly "Select Droplet: " (do/fetch-droplets) 'droplets 'name 'id)))
  (do/droplet-shell
   (number-to-string (first result))
   (last result))))
(defun do/droplet-snapshot ()
  "Create a snapshot of the selected droplet."
  (interactive)
  (do/exec-droplet-action
   (number-to-string
    (car (do/completing-read-friendly "Select Droplet: " (do/fetch-droplets) 'droplets 'name 'id)))
   "snapshot"))
(defun do/droplet-restart ()
  "Restart the selected droplet."
  (interactive)
  (do/exec-droplet-action
   (number-to-string
    (car (do/completing-read-friendly "Select Droplet: " (do/fetch-droplets) 'droplets 'name 'id)))
   "reboot"))
(defun do/droplet-shutdown ()
  "Shutdown the selected droplet."
  (interactive)
  (do/exec-droplet-action
   (number-to-string
    (car (do/completing-read-friendly "Select Droplet: " (do/fetch-droplets) 'droplets 'name 'id)))
   "power_off"))
(defun do/droplet-startup ()
  "Start the selected droplet."
  (interactive)
  (do/exec-droplet-action
   (number-to-string
    (car (do/completing-read-friendly "Select Droplet: " (do/fetch-droplets) 'droplets 'name 'id)))
   "power_on"))
(defun do/droplet-destroy ()
  "Destroy the selected droplet."
  (interactive)
  (do/exec-droplet-action
   (number-to-string
    (car (do/completing-read-friendly "Select Droplet: " (do/fetch-droplets) 'droplets 'name 'id)))
   "destroy"))
(defun do/droplet-simple-create ()
  "Create a droplet quickly using minimum inputs."
  (interactive)
  (do/create-droplet
   `(("name" . ,(read-string "Droplet name: "))
     ("region" . ,(car (do/completing-read-friendly "Select Region: " (do/fetch-regions) 'regions 'name 'slug)))
     ("size" . ,(do/completing-read "Select Size: " (do/fetch-sizes) 'sizes 'slug))
     ("image" . ,(car (do/completing-read-friendly "Select Image: " (do/fetch-images) 'images 'name 'slug))))))


;;; (Features)
(provide 'digitalocean)
;;; digitalocean.el ends here
