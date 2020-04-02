;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: LGPL-2.1-or-later

;;; Rewrite of Basic.h of XForms v1.4

(library (scforms api basic)
  (export)
  (import (rnrs base)
          (rnrs arithmetic bitwise)
          (rnrs arithmetic fixnums)
          (scforms api xforms)
          (scforms misc)
          (pffi))

;;;; XForms API as of v1.4

;;; Constant values
  (defines
    (fl-on 1)
    (fl-ok 1)
    (fl-valid 1)
    (fl-preempt 1)
    (fl-auto 2)
    (fl-when-needed fl-auto)
    (fl-off 0)
    (fl-cancel 0)
    (fl-invalid 0)
    (fl-ignore -1)
    (fl-path-max 1024))

  (define fl-color unsigned-long)

;;; Screen coordinates
  (define fl-coord int)
  (define fl-coord-unit int)
  (define-enum 0
    (fl-coord-pixel
     fl-coord-mm
     fl-coord-point
     fl-coord-centimm
     fl-coord-centipoint))

;;; Object classes
  (define fl-class int)
  (define-enum 0
    (fl-invalid-class
     fl-button
     fl-lightbutton
     fl-roundbutton
     fl-round3dbutton
     fl-checkbutton
     fl-bitmapbutton
     fl-pixmapbutton
     fl-bitmap
     fl-pixmap
     fl-box
     fl-text
     fl-menu
     fl-chart
     fl-choice
     fl-counter
     fl-slider
     fl-valslider
     fl-input
     fl-browser
     fl-dial
     fl-timer
     fl-clock
     fl-positioner
     fl-free
     fl-xyplot
     fl-frame
     fl-labelframe
     fl-canvas
     fl-glcanvas
     fl-tabfolder
     fl-scrollbar
     fl-scrollbutton
     fl-menubar
     fl-textbox
     fl-labelbutton
     fl-combobox
     fl-imagecanvas
     fl-thumbwheel
     fl-colorwheel
     fl-formbrowser
     fl-select
     fl-nmenu
     fl-spinner
     fl-tbox))

  (define fl-begin-group 10000)
  (define fl-end-group 20000)

  (define fl-user-class-start 1001)
  (define fl-user-class-start 9999)

  (define fl-max-bw 10)

;;; Form display properties
  (define fl-place int)
  (defines
    (fl-place-free 0)
    (fl-place-mouse 1)
    (fl-place-center 2)
    (fl-place-position 4)
    (fl-place-size 8)
    (fl-place-geometry 16)
    (fl-place-aspect 32)
    (fl-place-fullscreen 64)
    (fl-place-hotspot 128)
    (fl-place-iconic 256)
    (fl-free-size (bitwise-arithmetic-shift-left 1 14))
    (fl-place-free-center (bitwise-ior fl-place-center fl-free-size)))

;;; Window decorations
  (define-enum 1
    (fl-fullborder
     fl-transient
     fl-noborder))

;;; Box types
  (define fl-box-type int)
  (define-enum 0
    (fl-no-box
     fl-up-box
     fl-down-box
     fl-border-box
     fl-shadow-box
     fl-frame-box
     fl-rounded-box
     fl-embossed-box
     fl-flat-box
     fl-rflat-box
     fl-rshadow-box
     fl-oval-box
     fl-rounded3d-upbox
     fl-rounded3d-downbox
     fl-oval3d-upbox
     fl-oval3d-downbox
     fl-oval3d-framebox
     fl-oval3d-embossedbox))

  (define (fl-is-upbox t)
    (c-if (memq t (list fl-up-box fl-oval3d-upbox fl-rounded3d-upbox))))
  (define (fl-is-downbox t)
    (c-if (memq t (list fl-down-box fl-oval3d-downbox fl-rounded3d-downbox))))
  (define (fl-to-downbox t)
    (cond
     ((= t fl-up-box) fl-down-box)
     ((= t fl-rounded3d-upbox) fl-rounded3d-downbox)
     ((= t fl-oval3d-upbox) fl-oval3d-downbox)))

;;; Text alignment
  (define fl-align int)
  (defines
    (fl-align-center 0)
    (fl-align-top 1)
    (fl-align-bottom 2)
    (fl-align-left 4)
    (fl-align-right 8)
    (fl-align-left-top (bitwise-ior fl-align-left fl-align-top))
    (fl-align-right-top (bitwise-ior fl-align-right fl-align-top))
    (fl-align-left-bottom (bitwise-ior fl-align-left fl-align-bottom))
    (fl-align-right-bottom (bitwise-ior fl-align-right fl-align-bottom))
    (fl-align-inside (bitwise-arithmetic-shift-left 1 13))
    (fl-align-vert (bitwise-arithmetic-shift-left 1 14)))

  (define-foreign-procedure fl-is-inside-lalign xforms
    int fl_is_inside_lalign (int))
  (define-foreign-procedure fl-is-outside-lalign xforms
    int fl_is_outside_lalign (int))
  (define-foreign-procedure fl-to-inside-lalign xforms
    int fl_to_inside_lalign (int))
  (define-foreign-procedure fl-to-outside-lalign xforms
    int fl_to_outside_lalign (int))

;;; Mouse buttons
  (define-enum 1
    (fl-mbutton1
     fl-mbutton2
     fl-mbutton3
     fl-mbutton4
     fl-mbutton5))

  (defines
    (fl-left-mouse fl-mbutton1)
    (fl-middle-mouse fl-mbutton2)
    (fl-right-mouse fl-mbutton3)
    (fl-scrollup-mouse fl-mbutton4)
    (fl-scrolldown-mouse fl-mbutton5))

;;; Object return conditions
  (defines
    (fl-return-none 0)
    (fl-return-changed 1)
    (fl-return-end 2)
    (fl-return-end-changed 4)
    (fl-return-selection 8)
    (fl-return-deselection 16)
    (fl-return-triggered 1024))
  ;; Unsigned
  (define-foreign-variable xforms unsigned-int FL_RETURN_ALWAYS)

;;; Colour indices
  (define fl-pd-col int)
  (define-enum 0
    (fl-black
     fl-red
     fl-green
     fl-yellow
     fl-blue
     fl-magenta
     fl-cyan
     fl-white
     fl-tomato
     fl-indianred
     fl-slateblue
     fl-col1
     fl-right-bcol
     fl-bottom-bcol
     fl-top-bcol
     fl-left-bcol
     fl-mcol
     fl-inactive
     fl-palegreen
     fl-darkgold
     fl-orchid
     fl-darkcyan
     fl-darktomato
     fl-wheat
     fl-darkorange
     fl-deeppink
     fl-chartreuse
     fl-darkviolet
     fl-springgreen
     fl-dodgerblue
     fl-lighter-col1
     fl-darker-col1
     fl-aliceblue
     fl-antiquewhite
     fl-aqua
     fl-aquamarine
     fl-azure
     fl-beige
     fl-bisque
     fl-blanchedalmond
     fl-blueviolet
     fl-brown
     fl-burlywood
     fl-cadetblue
     fl-chocolate
     fl-coral
     fl-cornflowerblue
     fl-cornsilk
     fl-crimson
     fl-darkblue
     fl-darkgoldenrod
     fl-darkgray
     fl-darkgreen
     fl-darkgrey
     fl-darkkhaki
     fl-darkmagenta
     fl-darkolivegreen
     fl-darkorchid
     fl-darkred
     fl-darksalmon
     fl-darkseagreen
     fl-darkslateblue
     fl-darkslategray
     fl-darkslategrey
     fl-darkturquoise
     fl-deepskyblue
     fl-dimgray
     fl-dimgrey
     fl-firebrick
     fl-floralwhite
     fl-forestgreen
     fl-fuchsia
     fl-gainsboro
     fl-ghostwhite
     fl-gold
     fl-goldenrod
     fl-gray
     fl-greenyellow
     fl-grey
     fl-honeydew
     fl-hotpink
     fl-indigo
     fl-ivory
     fl-khaki
     fl-lavender
     fl-lavenderblush
     fl-lawngreen
     fl-lemonchiffon
     fl-lightblue
     fl-lightcoral
     fl-lightcyan
     fl-lightgoldenrodyellow
     fl-lightgray
     fl-lightgreen
     fl-lightgrey
     fl-lightpink
     fl-lightsalmon
     fl-lightseagreen
     fl-lightskyblue
     fl-lightslategray
     fl-lightslategrey
     fl-lightsteelblue
     fl-lightyellow
     fl-lime
     fl-limegreen
     fl-linen
     fl-maroon
     fl-mediumaquamarine
     fl-mediumblue
     fl-mediumorchid
     fl-mediumpurple
     fl-mediumseagreen
     fl-mediumslateblue
     fl-mediumspringgreen
     fl-mediumturquoise
     fl-mediumvioletred
     fl-midnightblue
     fl-mintcream
     fl-mistyrose
     fl-moccasin
     fl-navajowhite
     fl-navy
     fl-oldlace
     fl-olive
     fl-olivedrab
     fl-orange
     fl-orangered
     fl-palegoldenrod
     fl-paleturquoise
     fl-palevioletred
     fl-papayawhip
     fl-peachpuff
     fl-peru
     fl-pink
     fl-plum
     fl-powderblue
     fl-purple
     fl-rosybrown
     fl-royalblue
     fl-saddlebrown
     fl-salmon
     fl-sandybrown
     fl-seagreen
     fl-seashell
     fl-sienna
     fl-silver
     fl-skyblue
     fl-slategray
     fl-slategrey
     fl-snow
     fl-steelblue
     fl-tan
     fl-teal
     fl-thistle
     fl-turquoise
     fl-violet
     fl-whitesmoke
     fl-yellowgreen
     fl-colour-sentinel))

  (define-enum 255
    (fl-color-chooser-color
     fl-free-col1
     fl-free-col2
     fl-free-col3
     fl-free-col4
     fl-free-col5
     fl-free-col6
     fl-free-col7
     fl-free-col8
     fl-free-col9
     fl-free-col10
     fl-free-col11
     fl-free-col12
     fl-free-col13
     fl-free-col14
     fl-free-col15
     fl-free-col16))

  (define fl-nocolor (greatest-fixnum))

  (define fl-built-in-cols fl-colour-sentinel)
  (define fl-inactive-cols fl-inactive)

  (defines
    (fl-gray16 fl-right-bcol)
    (fl-gray35 fl-bottom-bcol)
    (fl-gray80 fl-top-bcol)
    (fl-gray90 fl-left-bcol)
    (fl-gray63 fl-col1)
    (fl-gray75 fl-mcol)
    (fl-lcol fl-black))

;;; Reacted events
  (define fl-events int)
  (define-enum 0
    (fl-noevent
     fl-draw
     fl-push
     fl-release
     fl-enter
     fl-leave
     fl-motion
     fl-focus
     fl-unfocus
     fl-keypress
     fl-update
     fl-step
     fl-shortcut
     fl-freemem
     fl-other
     fl-drawlabel
     fl-dblclick
     fl-trplclick
     fl-attrib
     fl-keyrelease
     fl-ps
     fl-moveorigin
     fl-resized
     fl-paste
     fl-trigger))

;;; Resize policies
  (define fl-resize-t int)
  (define-enum 0
    (fl-resize-none
     fl-resize-x
     fl-resize-y))
  (define fl-resize-all (bitwise-ior fl-resize-x fl-resize-y))

;;; Keyboard focus
  (define fl-key int)
  (defines
    (fl-key-normal 1)
    (fl-key-tap 2)
    (fl-key-special 4)
    (fl-key-all 7))

  (defines
    (fl-alt-mask (bitwise-arithmetic-shift-left 1 25))
    (fl-control-mask (bitwise-arithmetic-shift-left 1 26))
    (fl-shift-mask (bitwise-arithmetic-shift-left 1 27)))

  (define max-shortcuts 8)

;;; Popup menu item attributes
  (defines
    (fl-pup-none 0)
    (fl-pup-grey 1)
    (fl-pup-box 2)
    (fl-pup-check 4)
    (fl-pup-radio 8)
    (fl-pup-gray fl-pup-grey)
    (fl-pup-toggle fl-pup-box)
    (fl-pup-inactive fl-pup-grey))

  (define fl-pup-cb pointer)
  (define-foreign-struct fl-pup-entry
    (fields
     (pointer text)
     (fl-pup-cb callback)
     (pointer shortcut)
     (int mode)))

  (define fl-menu-entry fl-pup-entry)

;;; Fonts
  (define fl-maxfonts 48)

  (define fl-text-style int)
  (define-enum -1
    (fl-invalid-style
     fl-normal-style
     fl-bold-style
     fl-italic-style
     fl-bolditalic-style

     fl-fixed-style
     fl-fixedbold-style
     fl-fixeditalic-style
     fl-fixedbolditalic-style

     fl-times-style
     fl-timesbold-style
     fl-timesitalic-style
     fl-timesbolditalic-style

     fl-misc-style
     fl-miscbold-style
     fl-miscitalic-style
     fl-symbol-style))

  (defines
    (fl-shadow-style (bitwise-arithmetic-shift-left 1 9))
    (fl-engraved-style (bitwise-arithmetic-shift-left 1 10))
    (fl-embossed-style (bitwise-arithmetic-shift-left 1 11)))

  (define (special-style a)
    (c-if
     (and (>= a fl-shadow-style)
          (<= a (+ fl-embossed-style fl-maxfonts)))))

  (defines
   (fl-tiny-size 8)
   (fl-small-size 10)
   (fl-normal-size 12)
   (fl-medium-size 14)
   (fl-large-size 18)
   (fl-huge-size 24)
   (fl-default-size fl-small-size))

  (define fl-bound-width 1)

  (define fl-click-timeout 400)

  ;; Yow!
  (define-foreign-struct fl-object
    (fields
     (pointer form)
     (pointer u-vdata)
     (pointer u-cdata)
     (long u-ldata)
     (int objclass)
     (int type)
     (int boxtype)
     (fl-coord x) (fl-coord y) (fl-coord z)
     (fl-coord w) (fl-coord h)
     (double fl1) (double fr1)
     (double ft1) (double fb1)
     (double fl2) (double fr2)
     (double ft2) (double fb2)
     (fl-coord bw)
     (fl-color col1) (double col2)
     (pointer label)
     (fl-color lcol)
     (int align)
     (int lsize lstyle)
     (pointer shortcut)
     (pointer handle)
     (pointer object_callback)
     (long argument)
     (pointer spec)
     (pointer prehandle)
     (pointer posthandle)
     (pointer set_return)
     (unsigned-int resize)
     (unsigned-int nwgravity)
     (unsigned-int segravity)
     (pointer prev)
     (pointer next)
     (pointer parent)
     (pointer child)
     (pointer nc)
     (pointer flpixmap)
     (int use_pixmap)
     (int returned)
     (unsigned-int how-return)
     (int double-buffer)
     (int pushed)
     (int focus)
     (int belowmouse)
     (int active)
     (int input)
     (int wantkey)
     (int radio)
     (int automatic)
     (int redraw)
     (int visible)
     (int is_under)
     (int click)
     (unsigned-long click-timeout)
     (pointer c-vdata)
     (pointer c-cdata)
     (long c-ldata)
     (fl-color dbl-background)
     (pointer tooltip)
     (int tipID)
     (int group-id)
     (int want-motion)
     (int want-update)))

  (defines
    (fl-formcallbackptr pointer)
    (fl-callbackptr pointer)
    (fl-raw-callback pointer)
    (fl-form-atclose pointer)
    (fl-form-atdeactivate pointer)
    (fl-form-atactivate pointer)
    (fl-handleptr pointer)
    (fl-error-func pointer)
    (fl-event pointer))

;;; Form
  (defines
    (fl-being-hidden -1)
    (fl-hidden 0)
    (fl-invisible fl-hidden)
    (fl-visible 1))

  ;; Yikes
  (define-foreign-struct fl-form
    (fields
     (pointer fdui)
     (pointer u-vdata)
     (pointer u-cdata)
     (long u-ldata)
     (pointer label)
     (window window)
     (fl-coord x) (fl-coord y)
     (fl-coord w) (fl-coord h)
     (int handle-dec-x)
     (int handle-dec-y)
     (fl-coord hotx hoty)
     (double w-hr) (double h-hr)
     (pointer first) (pointer last)
     (pointer focusobj)
     (fl-formcallbackptr form-callback)
     (fl-form-atactivate activate-callback)
     (fl-form-atdeactivate deactivate-callback)
     (pointer form-cb-data)
     (pointer activate-data)
     (pointer deactivate-data)
     (fl-raw-callback key-callback)
     (fl-raw-callback push-callback)
     (fl-raw-callback crossing-callback)
     (fl-raw-callback motion-callback)
     (fl-raw-callback all-callback)
     (unsigned-long compress-mask)
     (unsigned-long evmask)
     (fl-form-atclose close-callback)
     (pointer close-data)
     (pointer flpixmap)
     (pixmap icon-pixmap)
     (pixmap icon-mask)
     (int deactivated)
     (int use-pixmap)
     (int frozen)
     (int visible)
     (int wm-border)
     (unsigned-int prop)
     (int num-auto-objects)
     (int needs-full-redraw)
     (int soft-of-modal)
     (pointer parent)
     (pointer child)
     (pointer parent-obj)
     (int attached)
     (pointer pre-attach)
     (pointer attach-data)
     (int in-redraw)))

;;; IO
  (define-foreign-struct fd-any
    (fields
     (pointer form)
     (pointer vdata)
     (pointer cdata)
     (long ldata)))

  (defines
    (fl-read 1)
    (fl-write 2)
    (fl-except 4))

  (define fl-io-callback pointer)
  (define-foreign-procedure fl-add-io-callback xforms
    void fl_add_io_callback (int unsigned-int fl-io-callback pointer))
  (define-foreign-procedure fl-remove-io-callback xforms
    void fl_remove_io_callback (int unsigned-int fl-io-callback))

;;; Signals
  (define fl-signal-handler pointer)
  (define-foreign-procedure fl-add-signal-callback xforms
    void fl_add_signal_callback (int fl-signal-handler pointer))
  (define-foreign-procedure fl-remove-signal-callback xforms
    void fl_signal_callback (int))
  (define-foreign-procedure fl-signal-caught xforms
    void fl_signal_caught (int))
  (define-foreign-procedure fl-app-signal-direct xforms
    void fl_app_signal_direct (int))
  (define-enum 0
    (fl-input-end-event-classic
     fl-input-end-event-always))

  (define-foreign-procedure fl-input-end-return-handling xforms
    int fl_input_end_return_handling (int))

;;; Timeouts
  (define fl-timeout-callback pointer)
  (define-foreign-procedure fl-add-timeout xforms
    int fl_add_timeout (long fl-timeout-callback pointer))
  (define-foreign-procedure fl-remove-timeout xforms
    void fl_remove_timeout (int))

;;; Public routines
  (define fl-library-version
    foreign-foreign-procedure-proce fl_library_version (pointer pointer))
  (define-foreign-procedure fl-library-version xforms
    long fl_library_full_version (pointer pointer pointer pointer))

;;; Generic routines
  (define-foreign-procedure fl-bgn-form xforms
    pointer fl_bgn_form (int fl-coord fl-coord))
  (define-foreign-procedure fl-end-form xforms
    void fl_end_form (void))

  (define-foreign-procedure fl-do-forms xforms
    pointer fl_do_forms (void))
  (define-foreign-procedure fl-check-forms xforms
    pointer fl_check_forms (void))
  (define-foreign-procedure fl-do-only-forms xforms
    pointer fl_do_only_forms (void))

  (define-foreign-procedure fl-freeze-form xforms
    void fl_freeze_form (pointer))
  (define-foreign-procedure fl-unfreeze-form xforms
    void fl_unfreeze_form (pointer))
  (define-foreign-procedure fl-freeze-all-forms xforms
    void fl_freeze_all_forms (void))
  (define-foreign-procedure fl-unfreeze-all-forms xforms
    void fl_unfreeze_all_forms (void))

  (define-foreign-procedure fl-set-focus-object xforms
    void fl_set_focus_object (pointer pointer))
  (define-foreign-procedure fl-get-focus-object xforms
    pointer fl_get_focus_object (pointer))
  (define-foreign-procedure fl-reset-focus-object xforms
    void fl_reset_focus_object (pointer))

  (define-foreign-procedure fl-set-form-atclose xforms
    fl-form-atclose fl_set_form_atclose (pointer fl-form-atclose pointer))
  (define-foreign-procedure fl-set-atclose xforms
    fl-form-atclose fl_set_atclose (fl-form-atclose pointer))

  (define-foreign-procedure fl-set-form-atactivate xforms
    fl-form-atactivate fl_set_form_atactivate (pointer fl-form-atactivate pointer))
  (define-foreign-procedure fl-form-is-activated xforms
    int fl_form_is_activated (pointer))
  (define-foreign-procedure fl-deactivate-form xforms
    void fl_deactivate_form (pointer))
  (define-foreign-procedure fl-activate-form xforms
    void fl_activate_form (pointer))
  (define-foreign-procedure fl-deactivate-all-forms xforms
    void fl_deactivate_all_forms (void))
  (define-foreign-procedure fl-activate-all-forms xforms
    void fl_activate_all_forms (void))

  (define-foreign-procedure fl-scale-form xforms
    void fl_scale_form (pointer double double))
  (define-foreign-procedure fl-set-form-position xforms
    void fl_set_form_position (pointer fl-coord fl-coord))

  (define-foreign-procedure fl-set-form-title xforms
    void fl_set_form_title (pointer pointer))
  ;; (define-foreign-procedure fl-set-form-title-f xforms
  ;;   void fl_set_form_title_f (pointer pointer ...))

  (define-foreign-procedure fl-set-app-mainform xforms
    void fl_set_app_mainform (pointer))
  (define-foreign-procedure fl-get-app-mainform xforms
    pointer fl_get_app_mainform (void))
  (define-foreign-procedure fl-set-app-nomainform xforms
    void fl_set_app_nomainform (pointer))

  (define-foreign-procedure fl-set-form-callback xforms
    void fl_set_form_callback (pointer fl-formcallbackptr pointer))

  (define-foreign-procedure fl-set-form-size xforms
    void fl_set_form_size (pointer fl-coord fl-coord))
  (define-foreign-procedure fl-set-form-minsize xforms
    void fl_set_form_minsize (pointer fl-coord fl-coord))
  (define-foreign-procedure fl-set-form-maxsize xforms
    void fl_set_form_maxsize (pointer fl-coord fl-coord))
  (define-foreign-procedure fl-adjust-form-size xforms
    double fl_adjust_form_size (pointer))

  (define-foreign-procedure fl-set-form-background-color xforms
    void fl_set_form_background_color (pointer))
  (define-foreign-procedure fl-get-form-background-color xforms
    fl-color fl_get_form_background_color (pointer))

  (define-foreign-procedure fl-set-form-hotspot xforms
    fl-void fl_set_form_hotspot (pointer fl-coord fl-coord))
  (define-foreign-procedure fl-set-form-hotobject xforms
    void fl_set_form_hotobject (pointer pointer))

  (define-foreign-procedure fl-set-form-event-cmask xforms
    unsigned-long fl_get_form_event_cmask (pointer))

  (define-foreign-procedure fl_set_form_geometry xforms
    void fl_set_form_geometry (pointer fl-coord fl-coord fl-coord fl-coord))

  (define-foreign-procedure fl-show-form xforms
    window fl_show_form (pointer int int pointer))
  ;; (define-foreign-procedure fl-show-form-f xforms
  ;;   window (pointer int int pointer ...))
  (define-foreign-procedure fl-hide-form xforms
    void fl_hide_form (pointer))
  (define-foreign-procedure fl-free-form xforms
    void fl_free_form (pointer))
  (define-foreign-procedure fl-form-is-visible xforms
    int fl_form_is_visible (pointer))

  (define-foreign-procedure fl-redraw-form xforms
    void fl_redraw_form (pointer))
  (define-foreign-procedure fl-set-form-dblbuffer xforms
    void fl_set_form_dblbuffer (pointer int))

  (define-foreign-procedure fl-prepare-form-window xforms
    window fl_prepare_form_window (pointer int int pointer))
  ;; (define-foreign-procedure fl-prepare-form-window-f xforms
  ;;   window fl_prepare_form_window_f (pointer int int pointer ...))
  (define-foreign-procedure fl-show-form-window xforms
    window fl_show_form_window (pointer))

  (define-foreign-procedure fl-form-is-iconified xforms
    int fl_form_is_iconified (pointer))

  (define-foreign-procedure fl-register-raw-callback xforms
    fl-raw-callback fl_register_raw_callback (pointer unsigned-long fl-raw-callback))

  (define-foreign-procedure fl-bgn-group xforms
    pointer fl_bgn_group (void))
  (define-foreign-procedure fl_end_group xforms
    void fl_end_group (void))
  (define-foreign-procedure fl-addto-group xforms
    pointer fl_addto_group (pointer))

;;; FL_OBJECT routines
;;; ...
  )
