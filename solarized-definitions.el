(eval-when-compile
  (require 'cl))

(defconst solarized-description
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized.")

(defcustom solarized-degrade nil
  "For test purposes only; when in GUI mode, forces Solarized to use the 256
degraded color mode to test the approximate color values for accuracy."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-diff-mode 'normal
  "Sets the level of highlighting to use in diff-like modes."
  :type 'symbol
  :options '(high normal low)
  :group 'solarized)

(defcustom solarized-bold t
  "Stops Solarized from displaying bold when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-underline t
  "Stops Solarized from displaying underlines when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-italic t
  "Stops Solarized from displaying italics when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-contrast 'normal
  "Stick with normal! It's been carefully tested. Setting this option to high or
low does use the same Solarized palette but simply shifts some values up or
down in order to expand or compress the tonal range displayed."
  :type 'symbol
  :options '(high normal low)
  :group 'solarized)

(defcustom solarized-broken-srgb (if (eq system-type 'darwin) t nil)
  "Emacs bug #8402 results in incorrect color handling on Macs. If this is t
\(the default on Macs), Solarized works around it with alternative colors.
However, these colors are not totally portable, so you may be able to edit
the \"Gen RGB\" column in solarized-definitions.el to improve them further."
  :type 'boolean
  :group 'solarized)

;; FIXME: The Generic RGB colors will actually vary from device to device, but
;;        hopefully these are closer to the intended colors than the sRGB values
;;        that Emacs seems to dislike
(defvar solarized-colors           ; ANSI(Solarized terminal)
  ;; name     sRGB      Gen RGB   256       16              8
  '((base03  "#002b36" "#042028" "#1c1c1c" "brightblack"   "black")
    (base02  "#073642" "#0a2832" "#262626" "black"         "black")
    (base01  "#586e75" "#465a61" "#585858" "brightgreen"   "green")
    (base00  "#657b83" "#52676f" "#626262" "brightyellow"  "yellow")
    (base0   "#839496" "#708183" "#808080" "brightblue"    "blue")
    (base1   "#93a1a1" "#81908f" "#8a8a8a" "brightcyan"    "cyan")
    (base2   "#eee8d5" "#e9e2cb" "#e4e4e4" "white"         "white")
    (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "brightwhite"   "white")
    (yellow  "#b58900" "#a57705" "#af8700" "yellow"        "yellow")
    (orange  "#cb4b16" "#bd3612" "#d75f00" "brightred"     "red")
    (red     "#dc322f" "#c60007" "#d70000" "red"           "red")
    (magenta "#d33682" "#c61b6e" "#af005f" "magenta"       "magenta")
    (violet  "#6c71c4" "#5859b7" "#5f5faf" "brightmagenta" "magenta")
    (blue    "#268bd2" "#2075c7" "#0087ff" "blue"          "blue")
    (cyan    "#2aa198" "#259185" "#00afaf" "cyan"          "cyan")
    (green   "#859900" "#728a05" "#5f8700" "green"         "green"))
  "This is a table of all the colors used by the Solarized color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")


(defun solarized-face-for-index (facespec index &optional light)
  "Creates a face from facespec where the colors use the names from
  `solarized-colors`."
  (let ((new-fontspec (copy-list facespec)))
    (dolist (property '(:foreground :background :color))
      (let ((color-name (plist-get new-fontspec property)))
        (when color-name
          ;; NOTE: We try to turn an 8-color term into a 10-color term by not
          ;;       using default background and foreground colors, expecting the
          ;;       user to have the right colors set for them.
          (when (and (= index 5)
                     (or (and (eq property :background)
                              (eq color-name 'back))
                         (and (eq property :foreground)
                              (member color-name '(base0 base1)))))
            (setf color-name nil))
          (when (eq color-name 'back)
            (setf color-name 'base03))
          (when light
            (setf color-name
                  (case color-name
                    (base03 'base3)
                    (base02 'base2)
                    (base01 'base1)
                    (base00 'base0)
                    (base0 'base00)
                    (base1 'base01)
                    (base2 'base02)
                    (base3 'base03)
                    (otherwise color-name))))
          (plist-put new-fontspec
                     property
                     (nth index (assoc color-name solarized-colors))))))
    (when (plist-get new-fontspec :box)
      (plist-put new-fontspec
                 :box
                 (solarized-face-for-index (plist-get new-fontspec :box) index
                                           light)))
    new-fontspec))

(defun create-face-spec (name facespec)
  `(,name ((((background dark) (type graphic))
            ,(solarized-face-for-index facespec
                                       (cond (solarized-degrade     3)
                                             (solarized-broken-srgb 2)
                                             (t                     1))))
           (((background dark) (type tty) (min-colors 256))
            ,(solarized-face-for-index facespec 3))
           (((background dark) (type tty) (min-colors  16))
            ,(solarized-face-for-index facespec 4))
           (((background dark) (type tty) (min-colors   8))
            ,(solarized-face-for-index facespec 5))
           (((background light) (type graphic))
            ,(solarized-face-for-index facespec
                                       (cond (solarized-degrade     3)
                                             (solarized-broken-srgb 2)
                                             (t                     1))
                                       t))
           (((background light) (type tty) (min-colors 256))
            ,(solarized-face-for-index facespec 3 t))
           (((background light) (type tty) (min-colors  16))
            ,(solarized-face-for-index facespec 4 t))
           (((background light) (type tty) (min-colors   8))
            ,(solarized-face-for-index facespec 5 t)))))

(defun solarized-color-definitions ()
  (let ((bold        (if solarized-bold 'bold 'normal))
        (bright-bold (if solarized-bold 'normal 'bold))
        (underline   (if solarized-underline t nil))
        (opt-under   nil)
        (italic      (if solarized-italic 'italic 'normal)))
    (cond ((eq 'high solarized-contrast)
           (let ((orig-base3 base3))
             (rotatef base01 base00 base0 base1 base2 base3)
             (setf base3 orig-base3)))
          ((eq 'low solarized-contrast)
           (setf back      base02
                 opt-under t)))
    (let ((bg-back   '(:background back))
          (bg-base03 '(:background base03))
          (bg-base02 '(:background base02))
          (bg-base01 '(:background base01))
          (bg-base00 '(:background base00))
          (bg-base0 '(:background base0))
          (bg-base1 '(:background base1))
          (bg-base2 '(:background base2))
          (bg-base3 '(:background base3))
          (bg-green '(:background green))
          (bg-yellow '(:background yellow))
          (bg-orange '(:background orange))
          (bg-red '(:background red))
          (bg-magenta '(:background magenta))
          (bg-violet '(:background violet))
          (bg-blue '(:background blue))
          (bg-cyan '(:background cyan))
          
          (fg-base03 '(:foreground base03))
          (fg-base02 '(:foreground base02))
          (fg-base01 '(:foreground base01))
          (fg-base00 '(:foreground base00))
          (fg-base0 '(:foreground base0))
          (fg-base1 '(:foreground base1))
          (fg-base2 '(:foreground base2))
          (fg-base3 '(:foreground base3))
          (fg-green '(:foreground green))
          (fg-yellow '(:foreground yellow))
          (fg-orange '(:foreground orange))
          (fg-red '(:foreground red))
          (fg-magenta '(:foreground magenta))
          (fg-violet '(:foreground violet))
          (fg-blue '(:foreground blue))
          (fg-cyan '(:foreground cyan))

          (fmt-none `(:weight normal :slant normal  :underline nil        :inverse-video nil))
          (fmt-bold `(:weight ,bold  :slant normal  :underline nil        :inverse-video nil))
          (fmt-bldi `(:weight ,bold                 :underline nil        :inverse-video nil))
          (fmt-undr `(:weight normal :slant normal  :underline ,underline :inverse-video nil))
          (fmt-undb `(:weight ,bold  :slant normal  :underline ,underline :inverse-video nil))
          (fmt-undi `(:weight normal                :underline ,underline :inverse-video nil))
          (fmt-uopt `(:weight normal :slant normal  :underline ,opt-under :inverse-video nil))
          ;; FIXME: not quite the same
          (fmt-curl `(:weight normal :slant normal  :underline t          :inverse-video nil))
          (fmt-ital `(:weight normal :slant ,italic :underline nil        :inverse-video nil))
          ;; FIXME: not quite the same
          (fmt-stnd `(:weight normal :slant normal  :underline nil        :inverse-video t))
          (fmt-revr `(:weight normal :slant normal  :underline nil        :inverse-video t))
          (fmt-revb `(:weight ,bold  :slant normal  :underline nil        :inverse-video t))
          (fmt-revbb `(:weight ,bright-bold :slant normal :underline nil  :inverse-video t))
          (fmt-revbbu `(:weight ,bright-bold :slant normal  :underline ,underline :inverse-video t)))
      (mapcar (lambda (face) (apply 'create-face-spec face))
              `(;; basic
                (default (,@fg-base0 ,@bg-back))   ; Normal
                (cursor (,@fg-base03 ,@bg-base0))  ; Cursor
                (error (,@fmt-bold ,@fg-red))      ; Error
                
                
                )))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'solarized-definitions)
