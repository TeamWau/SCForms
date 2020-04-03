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

;;;; Note; Variadic routines were diked out because PFFI does not
;;;; handle varargs. This is fine, as there are fixed routines for
;;;; each of them.

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
    (fl-path-max 1024)
    (fl-max-menu-choice-items 128))

;;; Screen coordinates
  (define-enum 0
    (fl-coord-pixel
     fl-coord-mm
     fl-coord-point
     fl-coord-centimm
     fl-coord-centipoint))

;;; Object classes
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
  (define FL-box-type int)
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

  (define-foreign-procedure xforms
    int fl_is_inside_lalign (int))
  (define-foreign-procedure xforms
    int fl_is_outside_lalign (int))
  (define-foreign-procedure xforms
    int fl_to_inside_lalign (int))
  (define-foreign-procedure xforms
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

  (define-foreign-variable xforms
    unsigned-int FL_RETURN_ALWAYS)

;;; Colour indices
  (define FL-pd-col int)
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
    fl-color-chooser-color
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
    fl-free-col16)

  (define fl-nocolor (greatest-fixnum))

  (define fl-built-in-cols fl-color-sentinel)
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
  (define FL-resize-t int)
  (define-enum 0
    (fl-resize-none
     fl-resize-x
     fl-resize-y))
  (define fl-resize-all (bitwise-ior fl-resize-x fl-resize-y))

;;; Keyboard focus
  (define FL-key int)
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

  (define-foreign-struct fl-pup-entry
    (fields
     (pointer text)
     (pointer callback)
     (pointer shortcut)
     (int mode)))

  (define FL-menu-entry fl-pup-entry)

;;; Fonts
  (define FL-maxfonts 48)

  (define FL-text-style int)
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
     (int x) (int y) (int z)
     (int w) (int h)
     (double fl1) (double fr1)
     (double ft1) (double fb1)
     (double fl2) (double fr2)
     (double ft2) (double fb2)
     (int bw)
     (unsigned-long col1) (double col2)
     (pointer label)
     (unsigned-long lcol)
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
     (unsigned-long dbl-background)
     (pointer tooltip)
     (int tipID)
     (int group-id)
     (int want-motion)
     (int want-update)))

  (define-foreign-variable xforms pointer FL_EVENT)

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
     (unsigned-long window)
     (int x) (int y)
     (int w) (int h)
     (int handle-dec-x)
     (int handle-dec-y)
     (int hotx hoty)
     (double w-hr) (double h-hr)
     (pointer first) (pointer last)
     (pointer focusobj)
     (pointer form-callback)
     (pointer activate-callback)
     (pointer deactivate-callback)
     (pointer form-cb-data)
     (pointer activate-data)
     (pointer deactivate-data)
     (pointer key-callback)
     (pointer push-callback)
     (pointer crossing-callback)
     (pointer motion-callback)
     (pointer all-callback)
     (unsigned-long compress-mask)
     (unsigned-long evmask)
     (pointer close-callback)
     (pointer close-data)
     (pointer flpixmap)
     (unsigned-long icon-pixmap)
     (unsigned-long icon-mask)
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

  (define-foreign-procedure xforms
    void fl_add_io_callback (int unsigned-int pointer pointer))
  (define-foreign-procedure xforms
    void fl_remove_io_callback (int unsigned-int pointer))

;;; Signals
  (define-foreign-procedure xforms
    void fl_add_signal_callback (int pointer pointer))
  (define-foreign-procedure xforms
    void fl_signal_callback (int))
  (define-foreign-procedure xforms
    void fl_signal_caught (int))
  (define-foreign-procedure xforms
    void fl_app_signal_direct (int))
  (define-enum 0
    (fl-input-end-event-classic
     fl-input-end-event-always))

  (define-foreign-procedure xforms
    int fl_input_end_return_handling (int))

;;; Timeouts
  (define-foreign-procedure xforms
    int fl_add_timeout (long pointer pointer))
  (define-foreign-procedure xforms
    void fl_remove_timeout (int))

;;; Public routines
  (define-foreign-procedure xforms
    int fl_library_version (pointer pointer))
  (define-foreign-procedure xforms
    long fl_library_full_version (pointer pointer pointer pointer))

;;; Generic routines
  (define-foreign-procedure xforms
    pointer fl_bgn_form (int int int))
  (define-foreign-procedure xforms
    void fl_end_form (void))

  (define-foreign-procedure xforms
    pointer fl_do_forms (void))
  (define-foreign-procedure xforms
    pointer fl_check_forms (void))
  (define-foreign-procedure xforms
    pointer fl_do_only_forms (void))

  (define-foreign-procedure xforms
    void fl_freeze_form (pointer))
  (define-foreign-procedure xforms
    void fl_unfreeze_form (pointer))
  (define-foreign-procedure xforms
    void fl_freeze_all_forms (void))
  (define-foreign-procedure xforms
    void fl_unfreeze_all_forms (void))

  (define-foreign-procedure xforms
    void fl_set_focus_object (pointer pointer))
  (define-foreign-procedure xforms
    pointer fl_get_focus_object (pointer))
  (define-foreign-procedure xforms
    void fl_reset_focus_object (pointer))

  (define-foreign-procedure xforms
    pointer fl_set_form_atclose (pointer pointer pointer))
  (define-foreign-procedure xforms
    pointer fl_set_atclose (pointer pointer))

  (define-foreign-procedure xforms
    pointer fl_set_form_atactivate (pointer pointer pointer))
  (define-foreign-procedure xforms
    int fl_form_is_activated (pointer))
  (define-foreign-procedure xforms
    void fl_deactivate_form (pointer))
  (define-foreign-procedure xforms
    void fl_activate_form (pointer))
  (define-foreign-procedure xforms
    void fl_deactivate_all_forms (void))
  (define-foreign-procedure xforms
    void fl_activate_all_forms (void))

  (define-foreign-procedure xforms
    void fl_scale_form (pointer double double))
  (define-foreign-procedure xforms
    void fl_set_form_position (pointer int int))

  (define-foreign-procedure xforms
    void fl_set_form_title (pointer pointer))
  ;; (define-foreign-procedure xforms
  ;;   void fl_set_form_title_f (pointer pointer ...))

  (define-foreign-procedure xforms
    void fl_set_app_mainform (pointer))
  (define-foreign-procedure xforms
    pointer fl_get_app_mainform (void))
  (define-foreign-procedure xforms
    void fl_set_app_nomainform (pointer))

  (define-foreign-procedure xforms
    void fl_set_form_callback (pointer pointer pointer))

  (define-foreign-procedure xforms
    void fl_set_form_size (pointer int int))
  (define-foreign-procedure xforms
    void fl_set_form_minsize (pointer int int))
  (define-foreign-procedure xforms
    void fl_set_form_maxsize (pointer int int))
  (define-foreign-procedure xforms
    double fl_adjust_form_size (pointer))

  (define-foreign-procedure xforms
    void fl_set_form_background_color (pointer))
  (define-foreign-procedure xforms
    unsigned-long fl_get_form_background_color (pointer))

  (define-foreign-procedure xforms
    fl-void fl_set_form_hotspot (pointer int int))
  (define-foreign-procedure xforms
    void fl_set_form_hotobject (pointer pointer))

  (define-foreign-procedure xforms
    unsigned-long fl_get_form_event_cmask (pointer))

  (define-foreign-procedure xforms
    void fl_set_form_geometry (pointer int int int int))

  (define-foreign-procedure xforms
    unsigned-long fl_show_form (pointer int int pointer))
  ;; (define-foreign-procedure xforms
  ;;   unsigned-long (pointer int int pointer ...))
  (define-foreign-procedure xforms
    void fl_hide_form (pointer))
  (define-foreign-procedure xforms
    void fl_free_form (pointer))
  (define-foreign-procedure xforms
    int fl_form_is_visible (pointer))

  (define-foreign-procedure xforms
    void fl_redraw_form (pointer))
  (define-foreign-procedure xforms
    void fl_set_form_dblbuffer (pointer int))

  (define-foreign-procedure xforms
    unsigned-long fl_prepare_form_window (pointer int int pointer))
  ;; (define-foreign-procedure xforms
  ;;   unsigned-long fl_prepare_form_window_f (pointer int int pointer ...))
  (define-foreign-procedure xforms
    unsigned-long fl_show_form_window (pointer))

  (define-foreign-procedure xforms
    int fl_form_is_iconified (pointer))

  (define-foreign-procedure xforms
    pointer fl_register_raw_callback (pointer unsigned-long pointer))

  (define-foreign-procedure xforms
    pointer fl_bgn_group (void))
  (define-foreign-procedure xforms
    void fl_end_group (void))
  (define-foreign-procedure xforms
    pointer fl_addto_group (pointer))

;;; FL_OBJECT routines
  (define-foreign-procedure xforms
    int fl_get_object_objclass (pointer))
  (define-foreign-procedure xforms
    int fl_get_object_type (pointer))

  (define-foreign-procedure xforms
    void fl_set_object_boxtype (pointer int))
  (define-foreign-procedure xforms
    int fl_get_object_boxtype (pointer))

  (define-foreign-procedure xforms
    void fl_set_object_bw (pointer int))
  (define-foreign-procedure xforms
    int fl_get_object_bw (pointer int))

  (define-foreign-procedure xforms
    void fl_set_object_resize (pointer unsigned-int))
  (define-foreign-procedure xforms
    void fl_get_object_resize (pointer unsigned-int))

  (define-foreign-procedure xforms
    void fl_set_object_gravity (pointer unsigned-int unsigned-int))
  (define-foreign-procedure xforms
    void fl_get_object_gravity (pointer unsigned-int unsigned-int))

  (define-foreign-procedure xforms
    void fl_set_object_lsize (pointer int))
  (define-foreign-procedure xforms
    int fl_get_object_lsize (pointer))

  (define-foreign-procedure xforms
    void fl_set_object_lstyle (pointer int))
  (define-foreign-procedure xforms
    int fl_get_object_lstyle (pointer))

  (define-foreign-procedure xforms
    void fl_set_object_lcol (pointer unsigned-long))
  (define-foreign-procedure xforms
    unsigned-long fl_get_object_lcol (pointer))

  (define-foreign-procedure xforms
    unsigned-int fl_set_object_return (pointer unsigned-int))
  (define-foreign-procedure xforms
    unsigned-int (pointer))

  (define-foreign-procedure xforms
    void fl_set_object_lalign (pointer int))
  (define-foreign-procedure xforms
    int fl_get_object_lalign (pointer))

  (define-foreign-procedure xforms
    void fl_set_object_shortcut (pointer pointer int))
  (define-foreign-procedure xforms
    void fl_set_object_shortcutkey (pointer unsigned-int))

  (define-foreign-procedure xforms
    void fl_set_object_dblbuffer (pointer int))

  (define-foreign-procedure xforms
    void fl_set_object_color (pointer unsigned-long unsigned-long))
  (define-foreign-procedure xforms
    void fl_get_object_color (pointer pointer pointer))

  (define-foreign-procedure xforms
    void fl_set_object_label (pointer pointer))
  ;; (define-foreign-procedure xforms
  ;;   void fl_set_object_label_f (pointer pointer ...))
  (define-foreign-procedure xforms
    pointer fl_get_object_label (pointer))

  (define-foreign-procedure xforms
    void fl_set_object_helper (pointer pointer))
  ;; (define-foreign-procedure xforms
  ;;   void fl_set_object_helper_f (pointer pointer ...))

  (define-foreign-procedure xforms
    void fl_set_object_position (pointer int int))
  (define-foreign-procedure xforms
    void fl_get_object_position (pointer pointer pointer))

  (define-foreign-procedure xforms
    void fl_get_object_size (pointer pointer pointer))
  (define-foreign-procedure xforms
    void fl_set_object_size (pointer int int))

  (define-foreign-procedure xforms
    void fl_set_object_automatic (pointer int))
  (define-foreign-procedure xforms
    int fl_object_is_automatic (pointer))

  (define-foreign-procedure xforms
    void fl_draw_object_label (pointer))
  (define-foreign-procedure xforms
    void fl_draw_object_label_outside (pointer))

  (define-foreign-procedure xforms
    pointer fl_get_object_component (pointer int int int))

  (define-foreign-procedure xforms
    void fl_for_all_objects (pointer pointer pointer))

  (define-foreign-procedure xforms
    void fl_set_object_dblclick (pointer unsigned-long))
  (define-foreign-procedure xforms
    unsigned-long fl_get_object_dblclick (pointer))

  (define-foreign-procedure xforms
    void fl_set_object_geometry (pointer int int int int))
  (define-foreign-procedure xforms
    void fl_get_object_geometry (pointer pointer pointer pointer pointer))

  (define-foreign-procedure xforms
    void fl_move_object (pointer int int))

  (define-foreign-procedure xforms
    void fl_fit_object_label (pointer int int))

  (define-foreign-procedure xforms
    void fl_get_object_bbox (pointer pointer pointer pointer pointer))
  (define fl-compute-object-geometry fl-get-object-bbox)

  (define-foreign-procedure xforms
    pointer fl_set_object_prehandler (pointer pointer))
  (define-foreign-procedure xforms
    pointer fl_set_object_callback (pointer pointer long))
  (define-foreign-procedure xforms
    pointer fl_set_object_posthandler (pointer pointer))
  (define-foreign-procedure xforms
    void fl_call_object_callback (pointer))

  (define-foreign-procedure xforms
    void fl_redraw_object (pointer))
  (define-foreign-procedure xforms
    void fl_show_object (pointer))
  (define-foreign-procedure xforms
    void fl_hide_object (pointer))
  (define-foreign-procedure xforms
    int fl_object_is_visible (pointer))

  (define-foreign-procedure xforms
    void fl_free_object (pointer))
  (define-foreign-procedure xforms
    void fl_delete_object (pointer))

  (define-foreign-procedure xforms
    int fl_get_object_return_state (pointer))

  (define-foreign-procedure xforms
    void fl_trigger_object (pointer))
  (define-foreign-procedure xforms
    void fl_activate_object (pointer))
  (define-foreign-procedure xforms
    void fl_deactivate_object (pointer))
  (define-foreign-procedure xforms
    int fl_object_is_active (pointer))

  (define-foreign-procedure xforms
    int fl_enumerate_fonts (pointer int))
  (define-foreign-procedure xforms
    int fl_set_font_name (int pointer))
  ;; (define-foreign-procedure xforms
  ;;   int fl_set_font_name_f (int pointer ...))
  (define-foreign-procedure xforms
    pointer fl_get_font_name (int))
  (define-foreign-procedure xforms
    void fl_set_font (int int))

;;; Free object routines
  (define-foreign-procedure xforms
    int fl_get_char_width (int int))
  (define-foreign-procedure xforms
    int fl_get_char_height (int int pointer pointer))

  (define-foreign-procedure xforms
    int fl_get_string_width (int int pointer int))
  (define-foreign-procedure xforms
    int fl_get_string_widthTAB (int int pointer pint))
  (define-foreign-procedure xforms
    int fl_get_string_height (int int pointer int pointer pointer))
  (define-foreign-procedure xforms
    void fl_get_string_dimension (int int pointer int pointer pointer))

  (define-foreign-procedure xforms
    void fl_get_align_xy (int int int int int int int int int pointer pointer))

  (define-foreign-procedure xforms
    int fl_get_label_char_at_mouse (pointer))

  (define-foreign-procedure xforms
    void fl_draw_text (int int int int int unsigned-long int int pointer))
  (define-foreign-procedure xforms
    void fl_draw_text_beside (int int int int int int int pointer))
  (define-foreign-procedure xforms
    void fl_draw_text_cursor (int int int int int unsigned-long int int pointer unsigned-long int)) ; wow

  (define-foreign-procedure xforms
    void fl_draw_box (int int int int int unsigned-long int))

  (define-foreign-procedure xforms
    int fl_add_symbol (pointer pointer int))
  (define-foreign-procedure xforms
    int fl_delete_symbol (pointer))
  (define-foreign-procedure xforms
    int fl_draw_symbol (pointer int int int int unsigned-long))

  (define-foreign-procedure xforms
    unsigned-long fl_mapcolor (unsigned-long int int int))
  (define-foreign-procedure xforms
    long fl_mapcolorname (unsigned-long pointer))
  (define-foreign-procedure xforms
    void fl_free_colors (pointer int))
  (define-foreign-procedure xforms
    void fl_free_pixels (pointer int))
  (define-foreign-procedure xforms
    void fl_set_color_leak (int))

  (define-foreign-procedure xforms
    unsigned-long fl_getmcolor (unsigned-long pointer pointer pointer))
  (define-foreign-procedure xforms
    unsigned-long fl_get_pixel (unsigned-long))
  (define-foreign-procedure xforms
    void fl_get_icm_color (unsigned-long pointer pointer pointer))
  (define-foreign-procedure xforms
    void fl_set_icm_color (unsigned-long int int int))
  (define-foreign-procedure xforms
    void fl_color (unsigned-long))
  (define-foreign-procedure xforms
    void fl_bk_color (unsigned-long))
  (define-foreign-procedure xforms
    void fl_set_gamma (double double double))

  (define-foreign-procedure xforms
    void fl_show_errors (int))

;;; New object routines
  (define-foreign-variable xforms
    pointer fl_current_form)

  (define-foreign-procedure xforms
    void fl_add_object (pointer pointer))
  (define-foreign-procedure xforms
    pointer fl_addto_form (pointer))
  (define-foreign-procedure xforms
    pointer fl_make_object (int int int int int int pointer pointer))
  (define-foreign-procedure xforms
    void fl_add_child (pointer pointer))

  (define-foreign-procedure xforms
    void fl_set_coordunit (int))

  (define-foreign-procedure xforms
    void fl_set_border_width (int))
  (define-foreign-procedure xforms
    int fl_get_border_width (void))

  (define-foreign-procedure xforms
    void fl_set_scrollbar_type (int))

  (define-foreign-procedure xforms
    void fl_flip_yorigin (void))

;;; Miscellaneous routines
  (define-foreign-procedure xforms
    void fl_ringbell (int))
  (define-foreign-procedure xforms
    void fl_gettime (pointer pointer))
  (define-foreign-procedure xforms
    pointer fl_now (void))
  (define-foreign-procedure xforms
    pointer fl_whoami (void))
  (define-foreign-procedure xforms
    long fl_mouse_button (void))
  (define-foreign-procedure xforms
    int fl_current_event (void))
  (define-foreign-procedure xforms
    pointer fl_strdup (pointer))
  (define-foreign-procedure xforms
    void fl_set_err_logfp (pointer))
  (define-foreign-procedure xforms
    void fl_set_error_handler (pointer))
  (define-foreign-procedure xforms
    pointer fl_get_cmdline_args (pointer))
  (define-foreign-procedure xforms
    int fl_is_same_object (pointer pointer)))
