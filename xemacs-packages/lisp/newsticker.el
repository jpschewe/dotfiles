;;; newsticker.el --- A Newsticker for Emacs.

;; Copyright (C) 2003-2004 by Ulf Jasper

;; This file is NOT part of GNU Emacs.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newsticker.el
;; URL:         http://www.nongnu.org/newsticker
;; Created:     17. June 2003
;; Keywords:    News, RSS
;; Time-stamp:  "10. November 2004, 20:36:53 (ulf)"
;; CVS-Version: $Id: newsticker.el,v 1.89 2004/11/10 19:42:12 u11 Exp $

(defconst newsticker-version "1.6" "Version number of newsticker.el.")

;; ======================================================================

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; ======================================================================
;;; Commentary:

;; Overview
;; --------
;; Newsticker provides a newsticker for Emacs. A newsticker is a thing
;; that asynchronously retrieves headlines from a list of news sites,
;; prepares these headlines for reading, and allows for loading the
;; corresponding articles in a web browser.

;; Headlines consist of a title and (possibly) a small description. They
;; are contained in RSS (RDF Site Summary) files.  Newsticker should work
;; with all RSS files that follow the RDF Rich Site Summary 1.0
;; specification (http://purl.org/rss/1.0/spec). It should also work with
;; version 2.0 as well as other/older/alternative rss formats (like
;; 0.9<something> or such). In other words: Newsticker is a ``RSS
;; reader'' or ``RSS aggregator''.

;; Newsticker provides several commands for reading headlines, navigating
;; through them, marking them as read/unread, hiding old headlines etc.
;; Headlines can be displayed as plain text or as rendered HTML.

;; Headlines can be displayed in the echo area, either scrolling like
;; messages in a stock-quote ticker, or just changing.

;; Requirements
;; ------------
;; Newsticker can be used with GNU Emacs
;; (http://www.gnu.org/software/emacs/emacs.html) version 21.1 or later
;; as well as XEmacs (http://www.xemacs.org).  It requires an XML-parser
;; (xml.el) which is part of GNU Emacs.  If you are using XEmacs you want
;; to get the net-utils package which contains xml.el for XEmacs.

;; Newsticker requires a program which can retrieve files via http and
;; prints them to stdout.  By default Newsticker will use wget
;; (http://www.gnu.org/software/wget/wget.html) for this task.


;; Installation
;; ------------
;; Place Newsticker in a directory where Emacs can find it. Add the
;; following line to your Emacs startup file (~/.emacs).

;;  (add-to-list 'load-path "/path/to/newsticker/")
;;  (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
;;  (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)

;; Newsticker-mode supports imenu. It allows for navigating with the help
;; of a menu. In order to use this feature you should also add the
;; following.

;;  (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)

;; That's it.

;; Usage
;; -----
;; The command `newsticker-show-news' will display all available
;; headlines in a special buffer (`*newsticker*').  It will also start
;; the asynchronous download of headlines.  The modeline in the
;; `*newsticker*' buffer informs whenever new headlines have arrived.
;; Clicking mouse-button 2 or pressing RET in this buffer on a headline
;; will call `browse-url' to load the corresponding news story in your
;; favourite web browser.

;; The scrolling, or flashing of headlines in the echo area, can be
;; started with the command `newsticker-start-ticker'. It can be stopped
;; with `newsticker-stop-ticker'.

;; If you just want to start the periodic download of headlines use the
;; command `newsticker-start'. Calling `newsticker-stop' will stop the
;; periodic download, but will call `newsticker-stop-ticker' as well.

;; Configuration
;; -------------
;; All Newsticker options are customizable, i.e. they can be changed with
;; Emacs customization methods: Call the command `customize-group' and
;; enter `newsticker' for the customization group.

;; All Newsticker options have reasonable default values, so that in most
;; cases it is not necessary to customize settings before starting
;; Newsticker for the first time.

;; The most important options are the following.

;; * `newsticker-url-list' defines the list of headlines which are
;;   retrieved.
;; * `newsticker-retrieval-interval' defines how often headlines
;;   are retrieved.
;; * `newsticker-display-interval' and `newsticker-scroll-smoothly'
;;   define how headlines are shown in the echo area.

;; PS: This newsticker is designed do its job silently in the
;;     background without disturbing you.  However, it is probably
;;     impossible to prevent such a tool from slightly attenuating your
;;     Editor's responsiveness every once in a while.

;;     Byte-compiling newsticker.el is recommended.

;; ======================================================================
;;; History:

;; 1.6 * Support for (some) optional RSS elements: guid, dc:date. See
;;       `newsticker-show-all-rss-elements' `newsticker-extra-face'.
;;     * Better support for w3m -- `newsticker-default-face' is obsolete
;;       now, removed `newsticker-w3m-toggle-inline-image'.
;;     * Added `newsticker-desc-comp-max' -- comparison of item descriptions
;;       can take quite some time.
;;     * Added `newsticker--buffer-make-item-completely-visible' to
;;       ensure that the current item is fully visible.
;;     * Allow for non-positive retrieval-interval, which make newsticker
;;       get news only once.
;;     * Use :set for customizable variables.
;;     * Added `newsticker-buffer-force-update', bound to key `U'.
;;     * Added concept of obsolete items, see
;;       `newsticker-keep-obsolete-items', `newsticker-obsolete-item-face',
;;       `newsticker-obsolete-item-max-age'.
;;     * Added `newsticker-add-url'.
;;     * OPML export.
;;     * Save pre-formatted titles => even better performance!!
;;     * `newsticker-*-new-item' wraps at beginning/end of buffer.
;;     * Always sort obsolete items to end of item list.
;;     * Bugfixes:
;;       - newsticker-hide-entry,
;;       - changes of feed-titles led to duplicate feed items,
;;       - faces for rendered HTML texts,
;;       - length of ticker-text (for "exotic"/multibyte texts),
;;         Thanks to Hiroshi Maruyama.
;;       - suppress items with empty title and description
;;       - newsticker-sort-method was ignored!
;;       - prevent call of fill-region on HTML-rendered descriptions.

;; 1.5 * Rewrote the visibility stuff. newsticker does not inherit
;;       outline anymore.  Now you have complete freedom for
;;       `newsticker-*-format'.
;;     * Save pre-formatted descriptions => incredible performance boost!!
;;     * Introduced `newsticker-(start|stop)-ticker'.
;;     * Introduced statistics for heading-format and
;;       `newsticker-statistics-face'.
;;     * Introduced `newsticker-enable-logo-manipulations'.
;;     * Compare link of items (as well as title and desc).
;;     * Added `newsticker-start-hook' and `newsticker-stop-hook', thanks
;;       to mace.
;;     * Bugfixes -- thanks to Ryan Yeske, Jari Aalto, Bruce Ingalls.
;;     * Tested with Emacs 21.3.50, 21.3.1, 21.2, 21.1; XEmacs 21.4.15

;; 1.4 * Enabled HTML rendering, added `newsticker-html-renderer' to
;;       choose a HTML rendering engine, thanks to Greg Scott for testing
;;     * New Outline handling using text properties instead of "**"
;;       prefixes.
;;     * Added possibility to mark single item as old (bound to key
;;       `o' (`newsticker-mark-item-at-point-as-read').
;;     * Added possibility to mark single item as immortal (bound to key
;;       `i' (`newsticker-mark-item-at-point-as-immortal').
;;     * Added possibility to display feed logos.
;;     * Added `newsticker-heading-format', `newsticker-item-format'.
;;     * Added `newsticker-date-format'.
;;     * Added `newsticker-justification'.
;;     * Added `newsticker-automatically-mark-visited-items-as-old'.
;;     * Added `newsticker-w3m-toggle-inline-image' which calls
;;       `w3m-toggle-inline-image' if `newsticker-html-renderer' is
;;       `w3m-region'. Exists for convenience only (bound to key
;;       `RET').

;; 1.3 * Compare title AND desc to check whether item is old, except
;;       for feed desc
;;     * Mark as not-up-to-date only after new items have arrived.
;;     * Added XEmacs compatibility code, tested with XEmacs 21.4.13.
;;     * Tested with Emacs 21.3.50 and Emacs 21.2.something.
;;     * Bugfix: Apply coding-systems to feed title and description,
;;       thanks to OHASHI Akira
;;     * Bugfix: xml-parser-workaround did not work for japanese texts,
;;       thanks to OHASHI Akira
;;     * Kill wget-buffers unless newsticker-debug is not nil.
;;     * Bugfix: xml-parser-workaround for "DOCTYPE rdf:RDF"

;; 1.2 Peter S Galbraith <psg@debian.org>
;;     * Added `newsticker-url-list-defaults', splitting the URLs into
;;       a customizable selection list, and a user add-on list.
;;     * Minor checkdoc fixes.

;; 1.1 * Introduced optional feed-specific wget-arguments.
;;     * Keep order of feeds as given in `newsticker-url-list' in
;;       *newsticker* buffer.
;;     * Ignore unsupported coding systems.

;; 1.0 * Introduced feed-specific retrieval-timers.
;;     * Removed dependency on 'cl (cddddr).
;;     * Thanks to Kevin Rodgers and T.V.  Raman for their help.
;;     * Use utf-8 for reading and writing cache data.
;;     * Reported to work with Emacs 21.3.50.

;; 0.99 * Minor tweaks.
;;      * Tested with Emacs 21.3.2

;; 0.98 * Check exit status of wget processes.  Keep cache data if
;;        something went wrong.  Throw error when old wget-processes
;;        are hanging around.
;;      * Introduced newsticker-specific faces.
;;      * Added `newsticker-show-descriptions-of-new-items'.
;;      * Added `newsticker-hide-old-items-in-newsticker-buffer'.
;;      * Added `newsticker-(hide|show)-old-items'.

;; 0.97 * Minor tweaks.

;; 0.96 * Added caching.
;;      * newsticker-mode inherits outline-mode.
;;      * newsticker-mode supports imenu.
;;      * Easy buffer-navigation with newsticker-mode's keymap.
;;      * Some bugs fixed.
;;      * Thanks to Moritz Epple for documentation tips.

;; 0.95 * Added newsticker-mode -- Thanks to T.V.  Raman.
;;      * Catch xml-parser errors -- Thanks to T.V.  Raman.
;;      * Remove stupid newlines in titles (headlines) -- Thanks to
;;        Jeff Rancier.

;; 0.94 * Added clickerability and description for channel headings.
;;      * Made it work for (at least some) rss 0.9<something> feeds.

;; 0.93 * Added some more sites.
;;      * Do not flood the *Messages* buffer.
;;      * First attempt at handling coding systems.

;; 0.92 * Added `newsticker-wget-name'.
;;      * Try to display message only if minibuffer and echo area are
;;        not in use already.
;;      * Dirty workaround for newer versions of xml.el: Remove
;;        whitespace in rdf.
;;      * Tested with Emacs 21.3.2 and CVS-snapshot of 2003-06-21.

;; 0.91 * First bugfix: *newsticker* is read-only.

;; 0.9  * First release.
;;      * Tested with Emacs 21.3.2 and wget 1.8.2.

;; ======================================================================
;;; To Do:

;; * Image handling for XEmacs (create-image does not exist)
;; * (texi) documentation
;; * speed

;; ======================================================================
;;; Code:

(require 'derived)
(require 'xml)

;; ======================================================================
;;; Customizables
;; ======================================================================
(defgroup newsticker nil
  "Newsticker settings.")

(defconst newsticker--raw-url-list-defaults
  '(("CNET News.com"
     "http://export.cnet.com/export/feeds/news/rss/1,11176,,00.xml")
    ("Debian Security Advisories"
    "http://www.debian.org/security/dsa.en.rdf")
    ("Debian Security Advisories - Long format"
    "http://www.debian.org/security/dsa-long.en.rdf")
    ("Emacs Wiki"
    "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss"
    nil
    3600)
    ("Freshmeat.net"
    "http://freshmeat.net/backend/fm.rdf")
    ("Kuro5hin.org"
    "http://www.kuro5hin.org/backend.rdf")
    ("LWN (Linux Weekly News)"
    "http://lwn.net/headlines/rss")
    ("NewsForge"
    "http://newsforge.com/index.rss")
    ("NY Times: Technology"
    "http://partners.userland.com/nytRss/technology.xml")
    ("NY Times"
    "http://partners.userland.com/nytRss/nytHomepage.xml")
    ("Quote of the day"
    "http://www.quotationspage.com/data/qotd.rss"
    "07:00"
    86400)
    ("The Register"
    "http://www.theregister.co.uk/tonys/slashdot.rdf")
    ("slashdot"
    "http://slashdot.org/index.rss"
    nil
    3600)			 ;/. will ban you if under 3600 seconds!
    ("Wired News"
    "http://www.wired.com/news_drop/netcenter/netcenter.rdf")
    ("Heise News (german)"
    "http://www.heise.de/newsticker/heise.rdf")
    ("Tagesschau (german)"
    "http://www.tagesschau.de/newsticker.rdf"
    nil
    1800)
    ("Telepolis (german)"
    "http://www.heise.de/tp/news.rdf"))
  "Default URL list in raw form.
This list is fed into defcustom via `newsticker--splicer'.")

(defun newsticker--splicer (item)
  "Convert ITEM for splicing into `newsticker-url-list-defaults'."
  (let ((result (list 'list :tag (nth 0 item) (list 'const (nth 0 item))))
	(element (cdr item)))
    (while element
      (setq result (append result (list (list 'const (car element)))))
      (setq element (cdr element)))
    result))

;; ======================================================================
;;; Customization
;; ======================================================================
(defun newsticker--set-customvar (symbol value)
  "Set newsticker-variable SYMBOL value to VALUE.

Calls all necessary actions which are necessary in order to make
the new value effective. Changing `newsticker-url-list', for example,
will re-start the retrieval-timers."
  (unless (condition-case nil
	      (eq (symbol-value symbol) value)
	    (error nil))
    (set symbol value)
    (cond ((eq symbol 'newsticker-sort-method)
	   (when (fboundp 'newsticker--cache-sort)
	     (message "Applying new sort method...")
	     (newsticker--cache-sort)
	     (newsticker--buffer-set-uptodate nil)
	     (message "Applying new sort method...done")))
	  ((memq symbol '(newsticker-url-list-defaults
			  newsticker-url-list
			  newsticker-retrieval-interval))
	   (when (and (fboundp 'newsticker-running-p)
		      (newsticker-running-p))
	     (message "Restarting newsticker")
	     (newsticker-stop)
	     (newsticker-start)))
	  ((eq symbol 'newsticker-display-interval)
	   (when (and (fboundp 'newsticker-running-p)
		      (newsticker-running-p))
	     (message "Restarting ticker")
	     (newsticker-stop-ticker)
	     (newsticker-start-ticker)
	     (message "")))
	  ((memq symbol '(newsticker-hide-old-items-in-echo-area
			  newsticker-hide-obsolete-items-in-echo-area
			  newsticker-hide-immortal-items-in-echo-area))
	   (when (and (fboundp 'newsticker-running-p)
		      (newsticker-running-p))
	     (message "Restarting newsticker")
	     (newsticker-stop-ticker)
	     (newsticker--ticker-text-setup)
	     (newsticker-start-ticker)
	     (message "")))
	  ((memq symbol '(newsticker-hide-old-items-in-newsticker-buffer
			  newsticker-show-descriptions-of-new-items))
	   (when (fboundp 'newsticker--buffer-set-uptodate)
	     (newsticker--buffer-set-uptodate nil)))
	  ((memq symbol '(newsticker-heading-format
			  newsticker-item-format
			  newsticker-desc-format
			  newsticker-date-format
			  newsticker-statistics-format
			  newsticker-justification
			  newsticker-use-full-width
			  newsticker-html-renderer
			  newsticker-feed-face
			  newsticker-new-item-face
			  newsticker-old-item-face
			  newsticker-immortal-item-face
			  newsticker-obsolete-item-face
			  newsticker-date-face
			  newsticker-statistics-face
			  ;;newsticker-default-face
			  ))
	   (when (fboundp 'newsticker--forget-preformatted)
	     (newsticker--forget-preformatted)))
	  (t
	   (error "Ooops %s" symbol)))))

(defcustom newsticker-url-list-defaults
 '(("Emacs Wiki"
    "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss"
    nil
    3600))
  "A customizable list of news feeds to select from.
These were mostly extracted from the Radio Community Server at
http://subhonker6.userland.com/rcsPublic/rssHotlist.

You may add other entries in `newsticker-url-list'."
  :type `(set ,@(mapcar `newsticker--splicer
			newsticker--raw-url-list-defaults))
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-url-list nil
  "The news feeds which you like to watch.

This alist will be used in addition to selection made customizing
`newsticker-url-list-defaults'.

This is an alist.  Each element consists of two items: a LABEL and a URL,
optionally followed by a START-TIME, INTERVAL specifier and WGET-ARGUMENTS.

The LABEL gives the name of the news feed.  It can be an arbitrary string.

The URL gives the location of the news feed.  It must point to a valid
RSS file.  The RSS file is retrieved by calling wget, or whatever you
specify as `newsticker-wget-name'.

The START-TIME can be either a string, or nil.  If it is a string it
specifies a fixed time at which this feed shall be retrieved for the
first time.  (Examples: \"11:00pm\", \"23:00\").  If it is nil (or
unspecified), this feed will be retrieved immediately after calling
`newsticker-start'.

The INTERVAL specifies the time between retrievals for this feed.  If it
is nil (or unspecified) the default interval value as set in
`newsticker-retrieval-interval' is used.

\(newsticker.el calls `run-at-time'. The newsticker-parameters START-TIME
and INTERVAL correspond to the `run-at-time'-parameters TIME and REPEAT.)

WGET-ARGUMENTS specifies arguments for wget (see `newsticker-wget-name')
which apply for this feed only, overriding the value of
`newsticker-wget-arguments'."
  :type '(repeat (list :tag "News feed"
                       (string :tag "Label")
                       (string :tag "URI")
                       (choice :tag "Start"
                               (const   :tag "Default" nil)
                               (string  :tag "Fixed Time"))
                       (choice :tag "Interval"
                               (const   :tag "Default" nil)
                               (integer :tag "Interval"))
                       (choice :tag "Wget Arguments"
                               (const  :tag "Default arguments" nil)
                               (repeat :tag "Special arguments" string))))
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-scroll-smoothly
  t
  "Decides whether to flash or scroll news items.
If t the news headlines are scrolled (more-or-less) smoothly in the echo
area.  If nil one headline after another is displayed in the echo area.
The variable `newsticker-display-interval' determines how fast this
display moves/changes and whether headlines are shown in the echo area
at all.  If you change `newsticker-scroll-smoothly' you should also change
`newsticker-display-interval'."
  :type 'boolean
  :group 'newsticker)

(defcustom newsticker-display-interval
  0.3
  "Time interval for displaying news items (seconds).
If equal or less than 0 no messages are shown in the echo area.  For
smooth display (see `newsticker-scroll-smoothly') a value of 0.3 seems
reasonable.  For non-smooth display a value of 10 is a good starting
point."
  :type 'number
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-retrieval-interval
  3600
  "Time interval for retrieving new news items (seconds).
If this value is not positive (i.e. less than or equal to 0)
items are retrieved only once!
Please note that some feeds, e.g. Slashdot, will ban you if you
make it less than 1800 seconds (30 minutes)!"
  :type 'integer
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-wget-name
  "wget"
  "Name of the program which is called to retrieve news from the web.
The canonical choice is wget but you may take any other program which is
able to return the contents of a news feed file on stdout."
  :type 'string
  :group 'newsticker)

(defcustom newsticker-wget-arguments
  '("-q" "-O" "-")
  "Arguments which are passed to wget.
There is probably no reason to change the default settings, unless you
are living behind a firewall."
  :type '(repeat (string :tag "Argument"))
  :group 'newsticker)

(defcustom newsticker-sort-method
  'sort-by-original-order
  "Sort method for news items.
The following sort methods are available:
* `sort-by-original-order' keeps the order in which the items
  appear in the RSS file (please note that for immortal items,
  which have been removed from the news feed, there is no original
  order),
* `sort-by-time' looks at the time at which an item has been seen
  the first time.  The most recent item is put at top,
* `sort-by-title' will put the items in an alphabetical order."
  :type '(choice
	  (const :tag "Keep original order" sort-by-original-order)
	  (const :tag "Sort by time"        sort-by-time)
	  (const :tag "Sort by title"       sort-by-title))
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-automatically-mark-items-as-old
  t
  "Decides whether to automatically mark items as old.
If t a new item is considered as new only after its first retrieval.  As
soon as it is retrieved a second time, it becomes old.  If not t all
items stay new until you mark them as old.  This is done in the
*newsticker* buffer."
  :type 'boolean
  :group 'newsticker)

(defcustom newsticker-automatically-mark-visited-items-as-old
  t
  "Decides whether to automatically mark visited items as old.
If t an item is marked as old as soon as the associated link is
visited, i.e. after pressing RET or mouse2 on the item's
headline."

  :type 'boolean
  :group 'newsticker)

(defcustom newsticker-keep-obsolete-items
  t
  "Decides whether to keep unread items which have been removed from feed.
If t a new item, which has been removed from the feed, is kept in
the cache until it is marked as read."
  :type 'boolean
  :group 'newsticker)

(defcustom newsticker-obsolete-item-max-age
  (* 60 60 24)
  "Maximal age of obsolete items, in seconds.
Obsolete items which are older than this value will be silently
deleted at the next retrieval."
  :type 'integer
  :group 'newsticker)
  
(defcustom newsticker-hide-immortal-items-in-echo-area
  t
  "Decides whether to show immortal/non-expiring news items in the ticker.
If t the echo area will not show immortal items.  See also
`newsticker-hide-old-items-in-echo-area."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-hide-old-items-in-echo-area
  t
  "Decides whether to show only the newest news items in the ticker.
If t the echo area will show only new items, i.e. only items which have
been added between the last two retrievals."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-hide-obsolete-items-in-echo-area
  t
  "Decides whether to show obsolete items items in the ticker.
If t the echo area will not show obsolete items.  See also
`newsticker-hide-old-items-in-echo-area."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-hide-old-items-in-newsticker-buffer
  nil
  "Decides whether to automatically hide old items in the *newsticker* buffer.
If set to t old items will be completely folded and only new items
will show up in the *newsticker* buffer.  Otherwise old as well as new
items will be visible."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-show-descriptions-of-new-items
  t
  "Whether to automatically show descriptions of new items in *newsticker*.
If set to t old items will be folded and new items will be
unfolded.  Otherwise old as well as new items will be folded."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-start-hook
  nil
  "Hook run when starting newsticker.
This hook is run at the very end of `newsticker-start'."
  :options '(newsticker-start-ticker)
  :type 'hook
  :group 'newsticker)

(defcustom newsticker-stop-hook
  nil
  "Hook run when stopping newsticker.
This hook is run at the very end of `newsticker-stop'."
  :options nil
  :type 'hook
  :group 'newsticker)

(defcustom newsticker-cache-filename
  "~/.newsticker-cache"
  "Name of the newsticker cache file."
  :type 'string
  :group 'newsticker)

;; layout related things
(defcustom newsticker-heading-format
  "%l
%t %d %s"
  "Format string for feed headings.
The following printf-like specifiers can be used:
%d  The date the feed was retrieved.  See `newsticker-date-format'.
%l  The logo (image) of the feed.  Most RSS feeds provide a small
    image as logo.  Newsticker can display them, if Emacs can --
    see `image-types' for a list of supported image types.
%L  The logo (image) of the feed.  If the logo is not available
    the title of the feed is used.
%s  The statistical data of the feed.  See `newsticker-statistics-format'.
%t  The title of the feed, i.e. its name."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-item-format
  "%t %d"
  "Format string for news item headlines.
The following printf-like specifiers can be used:
%d  The date the item was (first) retrieved.  See `newsticker-date-format'.
%l  The logo (image) of the feed.  Most RSS feeds provide a small
    image as logo.  Newsticker can display them, if Emacs can --
    see `image-types' for a list of supported image types.
%t  The title of the item."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-desc-format
  "%d %c"
  "Format string for news descriptions (contents).
The following printf-like specifiers can be used:
%c  The contents (description) of the item.
%d  The date the item was (first) retrieved.  See `newsticker-date-format'."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-date-format
  "(%A, %H:%M)"
  "Format for the date part in item and feed lines.
See `format-time-string' for a list of valid specifiers."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-statistics-format
  "[%n + %i + %o + %O = %a]"
  "Format for the statistics part in feed lines.
The following printf-like specifiers can be used:
%a  The number of all items in the feed.
%i  The number of immortal items in the feed.
%n  The number of new items in the feed.
%o  The number of old items in the feed.
%O  The number of obsolete items in the feed."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker)


;; image related things
(defcustom newsticker-enable-logo-manipulations
  t
  "If non-nil newsticker manipulates logo images.
This enables the following image properties: heuristic mask for all
logos, and laplace-conversion for images without new items."
:type 'boolean
:group 'newsticker)


(defcustom newsticker-imagecache-dirname
  "~/.newsticker-images"
  "Name of the directory where newsticker stores cached images."
  :type 'string
  :group 'newsticker)

;; rendering
(defcustom newsticker-justification
  'left
  "How to fill item descriptions.
If non-nil newsticker calls `fill-region' to wrap long lines in
item descriptions.  However, if an item description contains HTML
text and `newsticker-html-renderer' is non-nil, filling is not
done."
  :type '(choice :tag "Justification"
		 (const :tag "No filling" nil)
		 (const :tag "Left"       left)
		 (const :tag "Right"      right)
		 (const :tag "Center"     center)
		 (const :tag "Full"       full))
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-use-full-width
  t
  "Decides whether to use the full window width when filling.
If non-nil newsticker sets `fill-column' so that the whole
window is used when filling.  See also `newsticker-justification'."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-html-renderer
  nil
  "Function for rendering HTML contents.
If non-nil, newsticker.el will call this function whenever it finds
HTML-like tags in item descriptions.  Possible functions are, for
example, `w3m-region', `w3-region', and (if you have htmlr.el installed)
`newsticker-htmlr-render'.

In order to make sure that the HTML renderer is loaded when you run
newsticker, you should add the following statements to your .emacs:

  (autoload 'w3m-region \"w3m\"
    \"Render region in current buffer and replace with result.\" t)

or

  (require 'w3-auto)

or

  (autoload 'htmlr-reset  \"htmlr\" \"HTML rendering in Elisp\")

or

  (require 'htmlr)"
  :type '(choice :tag "Function"
		 (const :tag "None" nil)
		 (const :tag "w3" w3-region)
		 (const :tag "w3m" w3m-region)
		 (const :tag "htmlr" newsticker-htmlr-render))
  :set 'newsticker--set-customvar
  :group 'newsticker)

;; faces

(defface newsticker-feed-face
  '((((class color) (background dark))
     (:family "helvetica" :bold t :italic nil
              :foreground "misty rose"))
    (((class color) (background light))
     (:family "helvetica" :bold t :italic nil
              :foreground "black")))
  "Face for news feeds."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

(defface newsticker-new-item-face
  '((((class color) (background dark))
     (:bold t :foreground "orange"))
    (((class color) (background light))
     (:bold t :foreground "blue")))
  "Face for old news items."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

(defface newsticker-old-item-face
  '((((class color) (background dark))
     (:bold t))
    (((class color) (background light))
     (:bold t)))
  "Face for old news items."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

(defface newsticker-immortal-item-face
  '((((class color) (background dark))
     (:italic t :foreground "orange"))
    (((class color) (background light))
     (:italic t :foreground "blue")))
  "Face for immortal news items."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

(defface newsticker-obsolete-item-face
  '((((class color) (background dark))
     (:bold t :foreground "orange" :underline t))
    (((class color) (background light))
     (:bold t :foreground "blue2" :underline t)))
  "Face for old news items."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

(defface newsticker-date-face
  '((((class color) (background dark))
     (:italic t))
    (((class color) (background light))
     (:italic t)))
  "Face for newsticker dates."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

(defface newsticker-statistics-face
  '((((class color) (background dark))
     (:italic t))
    (((class color) (background light))
     (:italic t)))
  "Face for newsticker dates."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

(defface newsticker-extra-face
  '((((class color) (background dark))
     (:italic t :foreground "gray50" :height 0.8))
    (((class color) (background light))
     (:italic t :foreground "gray50" :height 0.8)))
  "Face for newsticker dates."
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

;; (defface newsticker-default-face
;;   '((((class color) (background dark))
;;      (:inherit default))
;;     (((class color) (background light))
;;      (:inherit default)))
;;   "Face for the description of news items."
;;   ;;:set 'newsticker--set-customvar
;;   :group 'newsticker)

(defcustom newsticker-desc-comp-max
  100
  "Maximum number of characters which will be taken into account
  when newsticker compares two item descriptions."
  :type 'integer
  :group 'newsticker)

;; debugging
(defcustom newsticker-debug
  nil
  "Enables some features needed for debugging newsticker.el.

If set to t newsticker.el will print lots of debugging messages, and the
buffers *wget-newsticker-<feed>* will not be closed."
  :type 'boolean
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

(defcustom newsticker-show-all-rss-elements
  t
  "Show all RSS elements."
  :type 'boolean
  ;;:set 'newsticker--set-customvar
  :group 'newsticker)

;; ======================================================================
;;; Compatibility section, XEmacs, Emacs
;; ======================================================================
(unless (fboundp 'time-add)
  (require 'time-date)
  (defun time-add (t1 t2)
    (seconds-to-time (+ (time-to-seconds t1) (time-to-seconds t2)))))

(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (re rp st)
    (save-match-data ;; apparently XEmacs needs save-match-data
      (replace-in-string st re rp))))

;; copied from subr.el
(unless (fboundp 'add-to-invisibility-spec)
  (defun add-to-invisibility-spec (arg)
    "Add elements to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
    (if (eq buffer-invisibility-spec t)
	(setq buffer-invisibility-spec (list t)))
    (setq buffer-invisibility-spec
	  (cons arg buffer-invisibility-spec))))

;; copied from subr.el
(unless (fboundp 'remove-from-invisibility-spec)
  (defun remove-from-invisibility-spec (arg)
    "Remove elements from `buffer-invisibility-spec'."
    (if (consp buffer-invisibility-spec)
	(setq buffer-invisibility-spec (delete arg buffer-invisibility-spec)))))

;; ======================================================================
;;; Internal variables
;; ======================================================================
(defvar newsticker--display-timer nil
  "Timer for newsticker display.")
(defvar newsticker--retrieval-timer-list nil
  "List of timers for news retrieval.
This is an alist, each element consisting of (feed-name . timer)")
(defvar newsticker--item-list nil
  "List of newsticker items.")
(defvar newsticker--item-position 0
  "Actual position in list of newsticker items.")
(defvar newsticker--prev-message "There was no previous message yet!"
  "Last message that the newsticker displayed.")
(defvar newsticker--scrollable-text ""
  "The text which is scrolled smoothly in the echo area.")
(defvar newsticker--buffer-uptodate-p nil
  "Tells whether the newsticker buffer is up to date.")
(defvar newsticker--latest-update-time (current-time)
  "The time at which the latest news arrived.")

(defvar newsticker--cache nil "Cached newsticker data.
This is a list of the form

 ((label1
   (title description link time age index preformatted-contents
    preformatted-title)
   ...)
  (label2
   (title description link time age index preformatted-contents
    preformatted-title)
   ...)
  ...)

where LABEL is a symbol.  TITLE, DESCRIPTION, and LINK are
strings.  TIME is a time value as returned by `current-time'.
AGE is a symbol: 'new, 'old, 'immortal, and 'obsolete denote
ordinary news items, whereas 'feed denotes an item which is not a
headline but describes the feed itself.  INDEX denotes the
original position of the item -- used for restoring the original
order.  PREFORMATTED-CONTENTS and PREFORMATTED-TITLE hold the
formatted contents of the item's description and title.  This
speeds things up if HTML rendering is used, which is rather
slow.")

;; ======================================================================
;;; Newsticker mode
;; ======================================================================
(makunbound 'newsticker-mode)
(define-derived-mode newsticker-mode fundamental-mode
  "NewsTicker"
  "Viewing RSS news feeds in Emacs."
;;  (set (make-local-variable 'tool-bar-map) newsticker-tool-bar-map)
  (set (make-local-variable 'imenu-sort-function) nil)
  (setq imenu-create-index-function 'newsticker--imenu-create-index)
  (setq buffer-read-only t)
  (auto-fill-mode -1) ;; turn auto-fill off!
  (font-lock-mode -1) ;; turn off font-lock!!
  (set (make-local-variable 'font-lock-defaults) nil)
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (add-to-invisibility-spec 'explicit)
  (newsticker--buffer-set-uptodate nil))

;; refine its mode-map
(define-key newsticker-mode-map "sO" 'newsticker-show-old-items)
(define-key newsticker-mode-map "hO" 'newsticker-hide-old-items)
(define-key newsticker-mode-map "sa" 'newsticker-show-all-desc)
(define-key newsticker-mode-map "ha" 'newsticker-hide-all-desc)
(define-key newsticker-mode-map "sf" 'newsticker-show-feed-desc)
(define-key newsticker-mode-map "hf" 'newsticker-hide-feed-desc)
(define-key newsticker-mode-map "so" 'newsticker-show-old-item-desc)
(define-key newsticker-mode-map "ho" 'newsticker-hide-old-item-desc)
(define-key newsticker-mode-map "sn" 'newsticker-show-new-item-desc)
(define-key newsticker-mode-map "hn" 'newsticker-hide-new-item-desc)
(define-key newsticker-mode-map "se" 'newsticker-show-entry)
(define-key newsticker-mode-map "he" 'newsticker-hide-entry)

(define-key newsticker-mode-map " "  'scroll-up)
(define-key newsticker-mode-map "q"  'newsticker-close-buffer)
(define-key newsticker-mode-map "p"  'newsticker-previous-item)
(define-key newsticker-mode-map "\t" 'newsticker-next-item)
(define-key newsticker-mode-map "n"  'newsticker-next-item)
(define-key newsticker-mode-map "M"  'newsticker-mark-all-items-as-read)
(define-key newsticker-mode-map "m"
  'newsticker-mark-all-items-at-point-as-read)
(define-key newsticker-mode-map "o"  'newsticker-mark-item-at-point-as-read)
(define-key newsticker-mode-map "P"  'newsticker-previous-new-item)
(define-key newsticker-mode-map "N"  'newsticker-next-new-item)
(define-key newsticker-mode-map "G"  'newsticker-get-all-news)
(define-key newsticker-mode-map "g"  'newsticker-get-news-at-point)
(define-key newsticker-mode-map "u"  'newsticker-buffer-update)
(define-key newsticker-mode-map "U"  'newsticker-buffer-force-update)
(define-key newsticker-mode-map "a"  'newsticker-add-url)

(define-key newsticker-mode-map "i"
  'newsticker-mark-item-at-point-as-immortal)

;; maps for the clickable portions
(defvar newsticker--heading-keymap (make-sparse-keymap)
  "Key map for click-able headings in the newsticker buffer.")
(define-key newsticker--heading-keymap [mouse-2]
  'newsticker-mouse-browse-url)
(define-key newsticker--heading-keymap "\n"
  'newsticker-browse-url)
(define-key newsticker--heading-keymap "\C-m"
  'newsticker-browse-url)

;; newsticker menu
(defvar newsticker-menu (make-sparse-keymap "Newsticker"))

(define-key newsticker-menu [newsticker-browse-url]
  '("Browse URL for item at point" . newsticker-browse-url))
(define-key newsticker-menu [newsticker-separator-1]
  '("--"))
(define-key newsticker-menu [newsticker-buffer-update]
  '("Update buffer" . newsticker-buffer-update))
(define-key newsticker-menu [newsticker-separator-2]
  '("--"))
(define-key newsticker-menu [newsticker-get-all-news]
  '("Get news from all feeds" . newsticker-get-all-news))
(define-key newsticker-menu [newsticker-get-news-at-point]
  '("Get news from feed at point" . newsticker-get-news-at-point))
(define-key newsticker-menu [newsticker-separator-3]
  '("--"))
(define-key newsticker-menu [newsticker-mark-all-items-as-read]
  '("Mark all items as read" . newsticker-mark-all-items-as-read))
(define-key newsticker-menu [newsticker-mark-all-items-at-point-as-read]
  '("Mark all items in feed at point as read" .
    newsticker-mark-all-items-at-point-as-read))
(define-key newsticker-menu [newsticker-mark-item-at-point-as-read]
  '("Mark item at point as read" .
    newsticker-mark-item-at-point-as-read))
(define-key newsticker-menu [newsticker-mark-item-at-point-as-immortal]
  '("Toggle immortality for item at point" .
    newsticker-mark-item-at-point-as-immortal))
(define-key newsticker-menu [newsticker-separator-4]
  '("--"))
(define-key newsticker-menu [newsticker-hide-old-items]
  '("Hide old items" . newsticker-hide-old-items))
(define-key newsticker-menu [newsticker-show-old-items]
  '("Show old items" . newsticker-show-old-items))
(define-key newsticker-menu [newsticker-next-item]
  '("Go to next item" . newsticker-next-item))
(define-key newsticker-menu [newsticker-previous-item]
  '("Go to previous item" . newsticker-previous-item))

;; bind menu to mouse
(define-key newsticker-mode-map [down-mouse-3] newsticker-menu)
;; Put menu in menu-bar
(define-key newsticker-mode-map [menu-bar Newsticker]
  (cons "Newsticker" newsticker-menu))

;; tool-bar
;; (makunbound 'newsticker-tool-bar-map)
;; (defvar newsticker-tool-bar-map
;;   (let ((tool-bar-map (make-sparse-keymap)))
;;     (tool-bar-add-item-from-menu 'newsticker-previous-item "left_arrow"
;; 				 newsticker-mode-map)
;;     (tool-bar-add-item-from-menu 'newsticker-next-item "right_arrow"
;; 				 newsticker-mode-map)
;;     (tool-bar-add-item-from-menu 'newsticker-mark-item-at-point-as-read
;; 				 "cancel" newsticker-mode-map)
;;     (tool-bar-add-item-from-menu 'newsticker-get-news-at-point "show"
;; 				 newsticker-mode-map)
;;     (tool-bar-add-item-from-menu 'newsticker-close-buffer
;; 				 "exit" newsticker-mode-map)
;;     tool-bar-map))


;; ======================================================================
;;; shortcuts
;; ======================================================================
(defsubst newsticker--title (item)
  "Return title of ITEM."
  (nth 0 item))
(defsubst newsticker--desc (item)
  "Return description of ITEM."
  (nth 1 item))
(defsubst newsticker--link (item)
  "Return link of ITEM."
  (nth 2 item))
(defsubst newsticker--time (item)
  "Return time of ITEM."
  (nth 3 item))
(defsubst newsticker--age (item)
  "Return age of ITEM."
  (nth 4 item))
(defsubst newsticker--pos (item)
  "Return position/index of ITEM."
  (nth 5 item))
(defsubst newsticker--preformatted-contents (item)
  "Return pre-formatted text of ITEM."
  (nth 6 item))
(defsubst newsticker--preformatted-title (item)
  "Return pre-formatted title of ITEM."
  (nth 7 item))
(defsubst newsticker--extra (item)
  "Return extra atributes of ITEM."
  (nth 8 item))
(defsubst newsticker--guid (item)
  "Return guid of ITEM."
  (let ((guid (assoc 'guid (newsticker--extra item))))
    (if (stringp guid)
        guid
      (car (xml-node-children guid)))))


;; ======================================================================
;;; User fun
;; ======================================================================

(defun newsticker-start (&optional do-not-complain-if-running)
  "Start the newsticker.
Start the timers for display and retrieval.  If the newsticker, i.e. the
timers, are running already a warning message is printed unless
DO-NOT-COMPLAIN-IF-RUNNING is not nil.
Run `newsticker-start-hook' if newsticker was not running already."
  (interactive)
  (let ((running (newsticker-running-p)))
    ;; read old cache if it exists and newsticker is not running
    (unless running
      (let* ((coding-system-for-read 'utf-8)
             (buf (find-file-noselect newsticker-cache-filename)))
        (when buf
          (set-buffer buf)
          (goto-char (point-min))
          (condition-case nil
              (setq newsticker--cache (read buf))
            (error (setq newsticker--cache nil))))))
    ;; start retrieval timers -- for sake of simplicity we will start
    ;; one timer for each feed
    (mapc (lambda (item)
	    (let* ((feed-name (car item))
		   (start-time (nth 2 item))
		   (interval (or (nth 3 item)
				 newsticker-retrieval-interval))
		   (timer (assoc (car item)
				 newsticker--retrieval-timer-list)))
	      (if timer
		  (or do-not-complain-if-running
		      (message "Timer for %s is running already!"
			       feed-name))
		(newsticker--debug-msg "Starting timer for %s: %s, %d"
				       feed-name start-time interval)
		;; do not repeat retrieval if interval not positive
		(if (<= interval 0)
		    (setq interval nil))
		(setq timer (run-at-time start-time interval
					 'newsticker-get-news feed-name))
		(if interval
		    (add-to-list 'newsticker--retrieval-timer-list
				 (cons feed-name timer))))))
	  (append newsticker-url-list-defaults newsticker-url-list))
    (unless running
      (run-hooks 'newsticker-start-hook)
      (message "Newsticker started!"))))

(defun newsticker-start-ticker ()
  "Start newsticker's ticker (but not the news retrieval.
Start display timer for the actual ticker if wanted and not
running already."
  (interactive)
  (if (and (> newsticker-display-interval 0)
	   (not newsticker--display-timer))
      (setq newsticker--display-timer
	    (run-at-time newsticker-display-interval
			 newsticker-display-interval
			 'newsticker--display-tick))))
  
(defun newsticker-stop ()
  "Stop the newsticker and the newsticker-ticker.
Cancel the timers for display and retrieval.  Run `newsticker-stop-hook'
if newsticker has been running."
  (interactive)
  (newsticker-stop-ticker)
  (when (newsticker-running-p)
    (mapc (lambda (name-and-timer)
	    (cancel-timer (cdr name-and-timer)))
	  newsticker--retrieval-timer-list)
    (setq newsticker--retrieval-timer-list nil)
    (run-hooks 'newsticker-stop-hook)
    (message "Newsticker stopped!")))

(defun newsticker-stop-ticker ()
  "Stop newsticker's ticker (but not the news retrieval)."
  (interactive)
  (when newsticker--display-timer
    (cancel-timer newsticker--display-timer)
    (setq newsticker--display-timer nil)))


;; the functions we need for retrieval and display
(defun newsticker-show-news ()
  "Switch to newsticker buffer.  You may want to bind this to a key."
  (interactive)
  (newsticker-start t) ;; will start only if not running
  (newsticker-buffer-update)
  (switch-to-buffer "*newsticker*"))

(defun newsticker-buffer-force-update ()
  "Update the newsticker buffer, even if not necessary."
  (interactive)
  (newsticker-buffer-update t))

(defun newsticker-buffer-update (&optional force)
  "Update the *newsticker* buffer.
Unless FORCE is t this is donly only if necessary, i.e. when the
*newsticker* buffer is not up-to-date."
  (interactive)
  ;; bring cache data into proper order....
  (newsticker--cache-sort)
  ;; fill buffer
  (save-current-buffer
    (let ((buf (get-buffer "*newsticker*")))
      (if buf
	  (set-buffer buf)
        (set-buffer (get-buffer-create "*newsticker*"))
        (newsticker--buffer-set-uptodate nil)))
   (when (or force
	     (not newsticker--buffer-uptodate-p))
     (message "Preparing newsticker buffer...")
     (let ((inhibit-read-only t))
       (set-buffer-modified-p nil)
       (erase-buffer)
       (newsticker-mode)
       ;; Emacs 21.3.50 does not care if we turn off auto-fill in the
       ;; definition of newsticker-mode, so we do it here (again)
       (auto-fill-mode -1)
       
       (set-buffer-file-coding-system 'utf-8)

       (if newsticker-use-full-width
	   (set (make-local-variable 'fill-column) (1- (window-width))))
       (newsticker--buffer-insert-all-items)

       ;; FIXME: needed for methods buffer in ecb
       ;; (set-visited-file-name "*newsticker*")

       (set-buffer-modified-p nil)
        (newsticker-hide-all-desc)
        (if newsticker-hide-old-items-in-newsticker-buffer
            (newsticker-hide-old-items))
        (if newsticker-show-descriptions-of-new-items
            (newsticker-show-new-item-desc))
       )
     (message ""))
   (newsticker--buffer-set-uptodate t)))

(defun newsticker-get-all-news ()
  "Launch retrieval of news from all configured newsticker sites.
This does NOT start the retrieval timers."
  (interactive)
  ;; launch retrieval of news
  (mapc (lambda (item)
	  (newsticker-get-news (car item)))
	(append newsticker-url-list-defaults newsticker-url-list)))

(defun newsticker-get-news-at-point ()
  "Launch retrieval of news for the feed point is in.
This does NOT start the retrieval timers."
  (interactive)
  ;; launch retrieval of news
  (let ((feed (get-text-property (point) 'feed)))
      (when feed
        (newsticker--debug-msg "Getting news for %s" (symbol-name feed))
        (newsticker-get-news (symbol-name feed)))))


(defun newsticker-add-url (url name)
  "Add given URL under given NAME to `newsticker-url-list'.
If URL is nil it is searched at point."
  (interactive
   (list
    (read-string "URL: "
		 (save-excursion
		   (end-of-line)
		   (and
		    (re-search-backward
		     "http://"
		     (if (> (point) (+ (point-min) 100))
			 (- (point) 100)
		       (point-min))
		     t)
		    (re-search-forward
		     "http://[-a-zA-Z0-9&/_.]*"
		     (if (< (point) (- (point-max) 200))
			 (+ (point) 200)
		       (point-max))
		     t)
		    (buffer-substring-no-properties (match-beginning 0)
						    (match-end 0)))))
    (read-string "Name: ")))
  (add-to-list 'newsticker-url-list (list name url nil nil nil))
  (customize-variable 'newsticker-url-list))

;; ======================================================================
;;; keymap stuff
;; ======================================================================
(defun newsticker-close-buffer ()
  "Close the newsticker buffer."
  (interactive)
  (newsticker--cache-save)
  (bury-buffer))

(defun newsticker-next-new-item (&optional do-not-wrap-at-eob)
  "Go to next new news item.
If no new item is found behind point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-EOB
is non-nil."
  (interactive)
  (let ((go-ahead t))
    (while go-ahead
      (unless (newsticker--buffer-goto '(item) 'new)
	;; found nothing -- wrap
	(unless do-not-wrap-at-eob
	  (goto-char (point-min))
	  (newsticker-next-new-item t))
	(setq go-ahead nil))
      (unless (memq (get-text-property (point) 'invisible)
		    buffer-invisibility-spec)
	;; this item is invisible -- continue search
	(setq go-ahead nil))))
  (newsticker--buffer-make-item-completely-visible)
  (point))

(defun newsticker-previous-new-item (&optional do-not-wrap-at-bob)
  "Go to previous new news item.
If no new item is found before point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-BOB
is non-nil."
  (interactive)
  (let ((go-ahead t))
    (while go-ahead
      (unless (newsticker--buffer-goto '(item) 'new t)
        (unless do-not-wrap-at-bob
          (goto-char (point-max))
          (newsticker--buffer-goto '(item) 'new t)))
      (unless (memq (get-text-property (point) 'invisible)
		    buffer-invisibility-spec)
	(setq go-ahead nil))))
  (newsticker--buffer-make-item-completely-visible)
  (point))

(defun newsticker-next-item (&optional do-not-wrap-at-eob)
  "Go to next news item.
Return new buffer position.
If no item is found below point, search is continued at beginning
of buffer unless optional argument DO-NOT-WRAP-AT-EOB is
non-nil."
  (interactive)
  (let ((go-ahead t))
    (while go-ahead
      (unless (newsticker--buffer-goto '(item feed))
	;; found nothing -- wrap
	(unless do-not-wrap-at-eob
	  (goto-char (point-min))
	  ;;(newsticker-next-item t)
          )
	(setq go-ahead nil))
      (unless (memq (get-text-property (point) 'invisible)
		    buffer-invisibility-spec)
	(setq go-ahead nil))))
  (newsticker--buffer-make-item-completely-visible)
  (point))

(defun newsticker-previous-item (&optional do-not-wrap-at-bob)
  "Go to previous news item.
Return new buffer position.
If no item is found before point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-BOB
is non-nil."
  (interactive)
  (let ((go-ahead t))
    (when (bobp)
      (unless do-not-wrap-at-bob
        (goto-char (point-max))))
    (while go-ahead
      (newsticker--buffer-goto '(item feed) nil t)
      (unless (memq (get-text-property (point) 'invisible)
		    buffer-invisibility-spec)
	(setq go-ahead nil))))
  (newsticker--buffer-make-item-completely-visible)
  (point))

(defun newsticker-mark-all-items-at-point-as-read ()
  "Mark all items as read and clear ticker contents."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark items as read? "))
    (let ((feed (get-text-property (point) 'feed))
          (pos (point)))
      (when feed
        (message "Marking all items as read for %s" (symbol-name feed))
        (newsticker--cache-replace-age newsticker--cache feed 'new 'old)
        (newsticker--cache-replace-age newsticker--cache feed 'obsolete 'old)
        (newsticker--cache-save)
        (newsticker--buffer-set-uptodate nil)
        (newsticker--ticker-text-setup)
        (newsticker-buffer-update)
        ;; go back to where we came frome
        (goto-char pos)
        (end-of-line)
        (newsticker--buffer-goto '(feed) nil t)))))

(defun newsticker-mark-item-at-point-as-read (&optional respect-immortality)
  "Mark item at point as read.
If optional argument RESPECT-IMMORTALITY is not nil immortal items do
not get changed."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark this item as read? "))
    (let ((feed (get-text-property (point) 'feed))
	  (item nil))
      (when feed
	(save-excursion
	  (newsticker--buffer-beginning-of-item)
	  (let ((inhibit-read-only t)
		(age (get-text-property (point) 'nt-age))
		(title (get-text-property (point) 'nt-title))
		(guid (get-text-property (point) 'nt-guid))
		(nt-desc (get-text-property (point) 'nt-desc))
		(pos (next-single-property-change (point) 'nt-type)))
	    (when (or (eq age 'new)
		      (eq age 'obsolete)
		      (and (eq age 'immortal)
			   (not respect-immortality)))
	      ;; find item
	      (setq item (newsticker--cache-contains newsticker--cache
						     feed title nt-desc
						     nil nil guid))
	      ;; mark as old
	      (when item
		(setcar (nthcdr 4 item) 'old))
	      ;; clean up ticker
	      (if (or (and (eq age 'new)
			   newsticker-hide-immortal-items-in-echo-area)
		      (and (memq age '(old immortal))
			   (not
			    (eq newsticker-hide-old-items-in-newsticker-buffer
				newsticker-hide-immortal-items-in-echo-area))))
		  (newsticker--ticker-text-remove feed title))
	      ;; set faces etc.
	      (put-text-property (point) pos 'nt-age 'old)
	      (newsticker--buffer-set-faces (point) pos)
	      (newsticker-hide-entry)
	      (set-buffer-modified-p nil))))
	(if item
	    (newsticker-next-item t))))))

(defun newsticker-mark-item-at-point-as-immortal ()
  "Mark item at point as read."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark this item as read? "))
    (let ((feed (get-text-property (point) 'feed))
	  (item nil))
      (when feed
	(save-excursion
	  (newsticker--buffer-beginning-of-item)
	  (let ((inhibit-read-only t)
		(oldage (get-text-property (point) 'nt-age))
		(title (get-text-property (point) 'nt-title))
		(guid (get-text-property (point) 'nt-guid))
		(pos (next-single-property-change (point) 'nt-type)))
	    (let ((newage 'immortal))
	      (if (eq oldage 'immortal)
		  (setq newage 'old))
	      (setq item (newsticker--cache-contains newsticker--cache
						     feed title nil nil nil
						     guid))
	      ;; change age
	      (when item
		(setcar (nthcdr 4 item) newage))
	      (if (or (and (eq newage 'immortal)
			   newsticker-hide-immortal-items-in-echo-area)
		      (and (eq newage 'obsolete)
			   newsticker-hide-obsolete-items-in-echo-area)
		      (and (eq oldage 'immortal)
			   (not
			    (eq newsticker-hide-old-items-in-newsticker-buffer
				newsticker-hide-immortal-items-in-echo-area))))
		  (newsticker--ticker-text-remove feed title)
		(newsticker--ticker-text-setup))
	      (put-text-property (point) pos 'nt-age newage)
	      (if (eq newage 'immortal)
		  (put-text-property (point) pos 'nt-age 'immortal)
		(put-text-property (point) pos 'nt-age 'old))
	      (newsticker--buffer-set-faces (point) pos)
	      (when (eq newage 'old)
		(newsticker-hide-entry)
		(set-buffer-modified-p nil)))))
	(if item
	    (newsticker-next-item t))))))
  

(defun newsticker-mark-all-items-as-read ()
  "Mark all items as read and clear ticker contents."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark items as read? "))
    (newsticker--cache-replace-age newsticker--cache 'any 'new 'old)
    (newsticker--buffer-set-uptodate nil)
    (newsticker--ticker-text-setup)
    (newsticker--cache-save)
    (newsticker-buffer-update)))

(defun newsticker-hide-old-item-desc ()
  "Hide the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-old nil)
  (newsticker--buffer-redraw))
  
(defun newsticker-show-old-item-desc ()
  "Show the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'item-old t)
  (newsticker--buffer-hideshow 'desc-old t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-new-item-desc ()
  "Hide the description of new items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-new nil)
  (newsticker--buffer-hideshow 'desc-immortal nil)
  (newsticker--buffer-hideshow 'desc-obsolete nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-new-item-desc ()
  "Show the description of new items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-new t)
  (newsticker--buffer-hideshow 'desc-immortal t)
  (newsticker--buffer-hideshow 'desc-obsolete t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-feed-desc ()
  "Hide the description of feeds."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-feed-desc ()
  "Show the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-all-desc ()
  "Hide the descriptions of feeds and all items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed nil)
  (newsticker--buffer-hideshow 'desc-immortal nil)
  (newsticker--buffer-hideshow 'desc-obsolete nil)
  (newsticker--buffer-hideshow 'desc-new  nil)
  (newsticker--buffer-hideshow 'desc-old  nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-all-desc ()
  "Show the descriptions of feeds and all items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed t)
  (newsticker--buffer-hideshow 'desc-immortal  t)
  (newsticker--buffer-hideshow 'desc-obsolete  t)
  (newsticker--buffer-hideshow 'desc-new  t)
  (newsticker--buffer-hideshow 'desc-old  t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-old-items ()
  "Hide old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-old nil)
  (newsticker--buffer-hideshow 'item-old nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-old-items ()
  "Show old items."
  (interactive)
  (newsticker--buffer-hideshow 'item-old t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-entry ()
  "Hide description of entry at point."
  (interactive)
  (save-excursion
    (newsticker--buffer-beginning-of-item)
    (let* ((inhibit-read-only t)
	   (pos0 (newsticker--buffer-beginning-of-item))
	   (pos1 (newsticker--buffer-goto '(desc)))
	   (pos2 (newsticker--buffer-end-of-item))
	   (pos3 (progn
		   (backward-char 1)
		   (newsticker--buffer-beginning-of-item))))
      (and pos1 pos2 (= pos0 pos3)
	   (put-text-property (1- pos1) (1- pos2) 'invisible 'explicit))))
  (newsticker--buffer-redraw))

(defun newsticker-show-entry ()
  "Show description of entry at point."
  (interactive)
  (save-excursion
    (let* ((pos1 (newsticker--buffer-beginning-of-item))
	   (pos2 (newsticker--buffer-end-of-item))
	   (inhibit-read-only t))
      (and pos1 pos2
	   (put-text-property (max (point-min) (1- pos1))
                              (1- pos2) 'invisible nil))))
  (newsticker--buffer-redraw))

;; ======================================================================
;;; local stuff
;; ======================================================================
(defun newsticker-running-p ()
  "Check whether newsticker is running.
Return t if newsticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not empty."
  (> (length newsticker--retrieval-timer-list) 0))

(defun newsticker-ticker-running-p ()
  "Check whether newsticker's actual ticker is running.
Return t if ticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not
empty."
  (timerp newsticker--display-timer))

;; ======================================================================
;;; local stuff
;; ======================================================================
(defun newsticker-get-news (feed-name)
  "Get news from the site FEED-NAME and load feed logo.
FEED-NAME must be a string which occurs as the label (i.e. the first element)
in an element of `newsticker-url-list' or `newsticker-url-list-defaults'."
  (newsticker--debug-msg "%s: Getting news for %s"
			 (format-time-string "%A, %H:%M" (current-time))
			 feed-name)
  (let* ((buffername (concat "*wget-newsticker-" feed-name "*"))
         (item (or (assoc feed-name newsticker-url-list)
                   (assoc feed-name newsticker-url-list-defaults)
                   (error
                    "Cannot get news for %s: Check newsticker-url-list"
		    feed-name)))
         (url (cadr item))
         (wget-arguments (or (car (cdr (cdr (cdr (cdr item)))))
                             newsticker-wget-arguments)))
    (save-current-buffer
      (set-buffer (get-buffer-create buffername))
      (erase-buffer)
      ;; throw an error if there is an old wget-process around
      (if (get-process feed-name)
          (error "Another wget-process is running for %s" feed-name))
      ;; start wget
      (let* ((args (append wget-arguments (list url)))
	     (proc (apply 'start-process feed-name buffername
			  newsticker-wget-name args)))
	(set-process-coding-system proc 'no-conversion 'no-conversion)
	(set-process-sentinel proc 'newsticker--sentinel)))))

  
(defun newsticker-mouse-browse-url (event)
  "Call `browse-url' for the link of the item at which the EVENT occurred."
  (interactive "e")
  (save-current-buffer
    (set-buffer (window-buffer (posn-window (event-end event))))
    (let ((url (get-text-property (posn-point (event-end event))
                                  'nt-link)))
      (when url
        (browse-url url)
	(save-excursion
	  (goto-char (posn-point (event-end event)))
	  (if newsticker-automatically-mark-visited-items-as-old
	      (newsticker-mark-item-at-point-as-read t)))))))

(defun newsticker-browse-url ()
  "Call `browse-url' for the link of the item at point."
  (interactive)
  (let ((url (get-text-property (point) 'nt-link)))
    (when url
      (browse-url url)
      (if newsticker-automatically-mark-visited-items-as-old
	  (newsticker-mark-item-at-point-as-read t)))))

(defun newsticker--sentinel (process event)
  "Sentinel for extracting news titles from an RDF buffer.
Argument PROCESS is the process which has just changed its state.
Argument EVENT tells what has happened to the process."
  (let* ((p-status (process-status process))
         (exit-status (process-exit-status process))
         (time (current-time))
	 (something-was-added nil))
    ;; catch known errors (zombie processes, rubbish-xml etc.
    ;; if an error occurs the news feed is not updated!
    (catch 'oops
      (unless (and (eq p-status 'exit)
                   (= exit-status 0))
        (message "%s: Error while retrieving news from %s"
                 (format-time-string "%A, %H:%M" (current-time))
                 (process-name process))
        (throw 'oops nil))
      (let* ((coding-system nil)
             (node-list
              (save-current-buffer
                (set-buffer (process-buffer process))
                ;; a very very dirty workaround to overcome the
                ;; problems with the newest (20030621) xml.el:
                ;; remove all unnecessary whitespace
                (goto-char (point-min))
                (while (re-search-forward ">[ \t\r\n]+<" nil t)
                  (replace-match "><" nil t))
		;; and another brutal workaround (20031105)!  For some
		;; reason the xml parser does not like the colon in the
		;; doctype name "rdf:RDF"
                (goto-char (point-min))
                (if (re-search-forward "<!DOCTYPE[ \t\n]+rdf:RDF" nil t)
                  (replace-match "<!DOCTYPE rdfColonRDF" nil t))
		;; finally.... ~##^Â°!!!!!
                (goto-char (point-min))
                (while (search-forward "\r\n" nil t)
                  (replace-match "\n" nil t))
		;; still more brutal workarounds (20040309)!  The xml
		;; parser does not like doctype rss
                (goto-char (point-min))
                (if (re-search-forward "<!DOCTYPE[ \t\n]+rss[ \t\n]*>" nil t)
                  (replace-match "" nil t))
		;;
		(set-buffer-modified-p nil)
                (goto-char (point-min))
                (if (re-search-forward "encoding=\"\\([^\"]+\\)\""
                                       nil t)
                    (setq coding-system (intern
                                         (downcase(match-string 1)))))
                (condition-case errordata
                    ;; The xml parser might fail
                    ;; or the xml might be bugged
                    (xml-parse-region (point-min) (point-max))
                  (error (message "Could not parse %s: %s"
                                  (buffer-name) (cadr errordata))
                         (throw 'oops nil)))))
             (topnode (car node-list))
             (channelnode (car (xml-get-children topnode 'channel)))
	     (imageurl nil)
	     (name (process-name process))
             (name-symbol (intern name))
             (position 0))
        ;; mark all items as obsolete
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'new 'obsolete-new)
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'old 'obsolete-old)
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'feed 'obsolete-old)
        ;; gather the news
        (if (eq (xml-node-name topnode) 'rss)
            ;; this is RSS 0.91 or something similar
            ;; all items are inside the channel node
	    (setq topnode channelnode))
	(setq imageurl
	      (car (xml-node-children
		    (car (xml-get-children
			  (car (xml-get-children
				topnode
				'image))
			  'url)))))
        (let ((title (or (car (xml-node-children (car (xml-get-children
                                                       channelnode 'title))))
                         "[untitled]"))
              (link (or (car (xml-node-children (car (xml-get-children
                                                      channelnode 'link))))
                        ""))
              (desc (or (car (xml-node-children (car (xml-get-children
                                                      channelnode
                                                      'content:encoded))))
                        (car (xml-node-children (car (xml-get-children
                                                      channelnode
                                                      'description))))
                        "[No description available]"))
              (old-item nil))
	  ;; check coding system
	  (setq coding-system
		(condition-case nil
		    (check-coding-system coding-system)
		  (coding-system-error
		   (message "newsticker.el: %s %s %s %s"
			    "ignoring coding system "
			    coding-system
			    " for "
			    name)
		   nil)))
	  ;; apply coding system
	  (when coding-system
	    (setq title (newsticker--decode-coding-string title coding-system))
	    (if desc
		(setq desc (newsticker--decode-coding-string desc coding-system)))
	    (setq link (newsticker--decode-coding-string link coding-system)))
          ;; decode numeric entities
          (setq title (newsticker--decode-numeric-entities title))
          (setq desc  (newsticker--decode-numeric-entities desc))
          (setq link  (newsticker--decode-numeric-entities link))

          ;; handle the feed itself
          (unless (newsticker--cache-contains newsticker--cache
                                              name-symbol title
                                              desc link 'feed)
            (setq something-was-added t))
          (setq newsticker--cache
                (newsticker--cache-add newsticker--cache name-symbol
                                       title desc link time 'feed position 
                                       'feed time nil nil
                                       (xml-node-children channelnode)))
          ;; gather all items for this feed
          (mapc (lambda (node)
		  (when (eq (xml-node-name node) 'item)
		    (setq position (1+ position))
		    (setq title (or (car (xml-node-children
					  (car (xml-get-children
						node 'title))))
				    "[untitled]"))
		    (setq link (or (car (xml-node-children
					 (car (xml-get-children
					       node 'link))))
				   ""))
		    (setq desc (or
                                (car (xml-node-children
                                      (car (xml-get-children
                                            node 'content:encoded))))
                                (car (xml-node-children
                                      (car (xml-get-children
                                            node 'description))))))
		    ;; use dc:date value if present
		    (setq time (or (newsticker--decode-iso8601-date
				    (car (xml-node-children
					  (car (xml-get-children
						node 'dc:date)))))
				   time))
		    ;; It happened that the title or description
		    ;; contained evil HTML code that confused the
		    ;; xml parser.  Therefore:
		    (unless (stringp title)
		      (setq title (prin1-to-string title)))
		    (unless (or (stringp desc) (not desc))
		      (setq desc (prin1-to-string desc)))
                    ;; ignore items with empty title AND empty desc
                    (when (or (> (length title) 0)
			      (> (length desc) 0))
                      ;; apply coding system
                      (when coding-system
                        (setq title (newsticker--decode-coding-string title coding-system))
                        (if desc
                            (setq desc (newsticker--decode-coding-string desc
                                                             coding-system)))
                        (setq link (newsticker--decode-coding-string link coding-system)))
                      ;; decode numeric entities
                      (setq title (newsticker--decode-numeric-entities title))
                      (when desc
                        (setq desc  (newsticker--decode-numeric-entities desc)))
                      (setq link (newsticker--decode-numeric-entities link))
                      ;; remove whitespace from title and desc
                      (setq title (newsticker--remove-whitespace title))
                      (setq desc (newsticker--remove-whitespace desc))
                      ;; add data to cache
                      ;; do we have this item already?
                      (setq old-item
                            (newsticker--cache-contains newsticker--cache
                                                        name-symbol title
                                                        desc link nil))
                      ;; add this item, or mark it as old, or do nothing
                      (let ((age1 'new)
                            (age2 'old))
                        (if old-item
                            (let ((prev-age (newsticker--age old-item)))
                              (unless
                                  newsticker-automatically-mark-items-as-old
                                (if (eq prev-age 'obsolete-old)
                                    (setq age2 'old)
                                  (setq age2 'new)))
                              (if (eq prev-age 'immortal)
                                  (setq age2 'immortal)))
                          ;; item was not there
                          (setq something-was-added t))
                        (setq newsticker--cache
                              (newsticker--cache-add
                               newsticker--cache name-symbol title desc link
                               time age1 position
                               age2 nil nil nil (xml-node-children node)))))))
		(xml-get-children topnode 'item)))
        ;; Remove those old items from cache which have been removed from
        ;; the feed
        (newsticker--cache-replace-age newsticker--cache
				       name-symbol 'obsolete-old 'deleteme)
        (newsticker--cache-remove newsticker--cache name-symbol
                                  'deleteme)
        ;; Remove those new items from cache which have been removed from
        ;; the feed.  Or keep them as `obsolete'
	(if (not newsticker-keep-obsolete-items)
	    (newsticker--cache-remove newsticker--cache
				      name-symbol 'obsolete-new)
	  (setq newsticker--cache
		(newsticker--cache-mark-expired
		 newsticker--cache name-symbol 'obsolete 'obsolete-expired
		 newsticker-obsolete-item-max-age))
	  (newsticker--cache-remove newsticker--cache
				    name-symbol 'obsolete-expired)
	  (newsticker--cache-replace-age newsticker--cache
					 name-symbol 'obsolete-new
					 'obsolete))
	;; bring cache data into proper order....
        ;; (newsticker--cache-sort)
	;; setup scrollable text
	(newsticker--ticker-text-setup)
        ;; do not save here -- too expensive
	;; (newsticker--cache-save)
	(setq newsticker--latest-update-time (current-time))
	(when something-was-added
	  ;; FIXME: should we care about removed items as well?
	  (newsticker--buffer-set-uptodate nil))
	;; kill the process buffer if wanted
	(unless newsticker-debug
	  (kill-buffer (process-buffer process)))
	;; launch retrieval of image
	(when (and imageurl
		   (string-match "%l" newsticker-heading-format))
	  (newsticker--image-get name imageurl))))))
  
(defun newsticker--display-tick ()
  "Called from the display timer.
This function calls a display function, according to the variable
`newsticker-scroll-smoothly'."
  (if newsticker-scroll-smoothly
      (newsticker--display-scroll)
    (newsticker--display-jump)))

(defsubst newsticker--echo-area-clean-p ()
  "Check whether somebody is using the echo area / minibuffer.
Return t if echo area and minibuffer are unused."
  (not (or (active-minibuffer-window)
           (and (current-message)
                (not (string= (current-message)
                              newsticker--prev-message))))))

(defun newsticker--display-jump ()
  "Called from the display timer.
This function displays the next ticker item in the echo area, unless
there is another message displayed or the minibuffer is active."
  (let ((message-log-max nil));; prevents message text from being logged
    (when (newsticker--echo-area-clean-p)
      (setq newsticker--item-position (1+ newsticker--item-position))
      (when (>= newsticker--item-position (length newsticker--item-list))
        (setq newsticker--item-position 0))
      (setq newsticker--prev-message
            (nth newsticker--item-position newsticker--item-list))
      (message newsticker--prev-message))))

(defun newsticker--display-scroll ()
  "Called from the display timer.
This function scrolls the ticker items in the echo area, unless
there is another message displayed or the minibuffer is active."
  (when (newsticker--echo-area-clean-p)
    (let* ((width (- (frame-width) 1))
           (message-log-max nil);; prevents message text from being logged
           (i newsticker--item-position)
           subtext
           (s-text newsticker--scrollable-text)
           (l (length s-text)))
      ;; don't show anything if there is nothing to show
      (unless (< (length s-text) 1)
	;; repeat the ticker string if it is shorter than frame width
        (while (< (length s-text) width)
          (setq s-text (concat s-text s-text)))
	;; get the width of the printed string
        (setq l (length s-text))
        (cond ((< i (- l width))
	       (setq subtext (substring s-text i (+ i width))))
              (t
               (setq subtext (concat
                              (substring s-text i l)
                              (substring s-text 0 (- width (- l i)))))))
	;; Take care of multibyte strings, for which (string-width) is
	;; larger than (length).
	;; Actually, such strings may be smaller than (frame-width)
	;; because return values of (string-width) are too large:
	;; (string-width "<japanese character>") => 2
	(let ((t-width (1- (length subtext))))
	  (while (> (string-width subtext) width)
	    (setq subtext (substring subtext 0 t-width))
	    (setq t-width (1- t-width))))
	;; show the ticker text and save current position
	(message subtext)
        (setq newsticker--prev-message subtext)
        (setq newsticker--item-position (1+ i))
        (when (>= newsticker--item-position l)
          (setq newsticker--item-position 0))))))

;; ======================================================================
;;; misc
;; ======================================================================
(defun newsticker--decode-coding-string (string coding-system)
  "Wrapper around decode-coding-string."
  (condition-case nil
      (decode-coding-string string coding-system)
    (error
     (message "CANNOT DECODE ENCODED STRING!"))))

(defun newsticker--decode-numeric-entities (s)
  "Decode SGML numeric entities by their respective utf characters.
Example: \"...&#42;...\" gets replaced by \"...*...\"."
  (let ((start 0))
    (while (string-match "&#\\([0-9]+\\);" s start)
      (condition-case nil
          (setq s (replace-match
                   (string (read (substring s (match-beginning 1)
					    (match-end 1))))
                   nil nil s))
        (error nil))
      (setq start (1+ (match-beginning 0))))
    s))

(defun newsticker--remove-whitespace (string)
  "Remove leading and trailing whitespace from STRING."
  ;; we must have ...+ but not ...* in the regexps otherwise xemacs loops
  ;; endlessly...
  (when string
    (replace-regexp-in-string
     "[ \t\n]+$" ""
     (replace-regexp-in-string "^[ \t\n]+" "" string))))

(defun newsticker--forget-preformatted ()
  "Forget all cached pre-formatted data.
Remove the pre-formatted from `newsticker--cache'."
  (mapc (lambda (feed)
	  (mapc (lambda (item)
		  (if (nthcdr 7 item)
		      (setcar (nthcdr 7 item) nil))
		  (if (nthcdr 6 item)
		      (setcar (nthcdr 6 item) nil)))
		(cdr feed)))
	newsticker--cache)
  (newsticker--buffer-set-uptodate nil))

(defun newsticker--debug-msg (string &rest args)
  "Print messages if echo area is clean.
This function calls `message' with arguments STRING and ARGS, if
`newsticker-debug' is non-nil and if the minibuffer is in not
active."
  (and newsticker-debug
       (not (active-minibuffer-window))
       (not (current-message))
       (apply 'message string args)))

(defun newsticker--decode-iso8601-date (iso8601-string)

  "Return ISO8601-STRING in format like `decode-time'.
Converts from ISO-8601 to Emacs representation.  If ISO8601-STRING
Examples: 
2004-09-17T05:09:49+00:00
2004-09-17T05:09+00:00
2004-09-17T05:09:49
2004-09-17T05:09
2004-09-17
2004-09
2004
"
  (if iso8601-string
      (when (string-match 
	     (concat
	      "\\([0-9]\\{4\\}\\)"
              "\\(-\\([0-9]\\{2\\}\\)"
              "\\(-\\([0-9]\\{2\\}\\)"
	      "\\(T"
	      "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
	      "\\(:\\([0-9]\\{2\\}\\)\\)?"
	      "\\(\\([-+Z]\\)\\(\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)?"
	      "\\)?\\)?\\)?")
	     iso8601-string)
	(let ((year (read (match-string 1 iso8601-string)))
	      (month (read (match-string 3 iso8601-string)))
	      (day (read (match-string 5 iso8601-string)))
	      (hour (read (or (match-string 7 iso8601-string)
			      "0")))
	      (minute (read (or (match-string 8 iso8601-string)
				"0")))
	      (second (read (or (match-string 10 iso8601-string)
				"0")))
	      (sign (match-string 12 iso8601-string))
	      (offset-hour (read (or (match-string 14 iso8601-string)
				     "0")))
	      (offset-minute (read (or (match-string 15 iso8601-string)
				       "0")))
	      (second 0))
          (cond ((string= sign "+")
                 (setq hour (- hour offset-hour))
                 (setq minute (- minute offset-minute)))
                ((string= sign "-")
                 (setq hour (+ hour offset-hour))
                 (setq minute (+ minute offset-minute))))
          ;; if UTC subtract current-time-zone offset
          ;;(setq second (+ (car (current-time-zone)) second)))

	  (condition-case nil
	      (encode-time second minute hour day month year t)
	    (error
	     (message "Cannot decode \"%s\"" iso8601-string)
	     nil))))
    nil))


;; ======================================================================
;;; images
;; ======================================================================
(defun newsticker--image-get (feed-name url)
  "Get image of the news site FEED-NAME from URL."
  (newsticker--debug-msg "%s: Getting image for %s"
			 (format-time-string "%A, %H:%M" (current-time))
			 feed-name)
  (let* ((buffername (concat "*wget-newsticker-image-" feed-name "*"))
         (item (or (assoc feed-name newsticker-url-list)
                   (assoc feed-name newsticker-url-list-defaults)
                   (error
                    "Cannot get news for %s: Check newsticker-url-list"
		    feed-name)))
         (wget-arguments (or (car (cdr (cdr (cdr (cdr item)))))
                             newsticker-wget-arguments)))
    (save-current-buffer
      (set-buffer (get-buffer-create buffername))
      (erase-buffer)
      ;; throw an error if there is an old wget-process around
      (if (get-process feed-name)
          (error "Another wget-process is running for image %s" feed-name))
      ;; start wget
      (let* ((args (append wget-arguments (list url)))
               (proc (apply 'start-process feed-name buffername
                            newsticker-wget-name args)))
        (set-process-coding-system proc 'no-conversion 'no-conversion)
        (set-process-sentinel proc 'newsticker--image-sentinel)))))

(defun newsticker--image-sentinel (process event)
  "Sentinel for image-retrieving PROCESS caused by EVENT."
  (let* ((p-status (process-status process))
         (exit-status (process-exit-status process))
	 (feed-name (process-name process))
	 (feed-name-symbol (intern feed-name))
         (time (current-time)))
    ;; catch known errors (zombie processes, rubbish-xml etc.
    ;; if an error occurs the news feed is not updated!
    (catch 'oops
      (unless (and (eq p-status 'exit)
                   (= exit-status 0))
        (message "%s: Error while retrieving image from %s"
                 (format-time-string "%A, %H:%M" (current-time))
                 feed-name)
        (throw 'oops nil))
      (let (image-name)
	(save-current-buffer
	  (set-buffer (process-buffer process))
	  (setq image-name (concat newsticker-imagecache-dirname "/"
				   feed-name))
	  (set-buffer-file-coding-system 'no-conversion)
	  ;; make sure the cache dir exists
	  (unless (file-directory-p newsticker-imagecache-dirname)
	    (make-directory newsticker-imagecache-dirname))
	  ;; write and close buffer
	  (let ((require-final-newline nil)
		(backup-inhibited t))
	    (write-file image-name))
	  (set-buffer-modified-p nil)
	  (kill-buffer (current-buffer)))))))

(defun newsticker--image-read (feed-name-symbol disabled)
  "Read the cached image for FEED-NAME-SYMBOL from disk.
If DISABLED is non-nil the image will be converted to a disabled look
\(unless newsticker-enable-logo-manipulations is not t\). 
Return the image."
  (let ((image-name (concat newsticker-imagecache-dirname "/"
			    (symbol-name feed-name-symbol)))
	(img nil))
    (when (file-exists-p image-name)
      (condition-case error-data
	  (setq img (create-image
		     image-name nil nil
		     :conversion (and newsticker-enable-logo-manipulations 
                                      disabled
				      'disabled)
		     :mask (and newsticker-enable-logo-manipulations
				'heuristic)
		     :ascent 70))
	(error
	 (message "Error: cannot create image: %s"
		  (cadr error-data)))))
    img))

;; ======================================================================
;;; imenu stuff
;; ======================================================================
(defun newsticker--imenu-create-index ()
  "Scan newsticker buffer and return an index for imenu."
  (save-excursion
    (goto-char (point-min))
    (let ((index-alist nil)
	  (feed-list nil)
	  (go-ahead t))
      (while (newsticker--buffer-goto '(item feed))
	(let ((type  (get-text-property (point) 'nt-type))
	      (title (get-text-property (point) 'nt-title)))
	  (cond ((eq type 'feed)
		 ;; we're on a feed heading
		 (when feed-list
		   (if index-alist
		       (nconc index-alist (list feed-list))
		     (setq index-alist (list feed-list))))
		 (setq feed-list (list title)))
		(t
		 (nconc feed-list
			(list (cons title (point))))))))
      index-alist)))

(defun newsticker--imenu-goto (name pos &rest args)
  "Go item NAME at position POS and show item.
ARGS are ignored."
  (goto-char pos)
  (newsticker-show-entry))


;; ======================================================================
;;; buffer stuff
;; ======================================================================
(defun newsticker--buffer-set-uptodate (value)
  "Sets the uptodate-status of the newsticker buffer to VALUE.
The modeline is changed accordingly."
  (setq newsticker--buffer-uptodate-p value)
  (let ((b (get-buffer "*newsticker*")))
    (when b
      (save-excursion
       (set-buffer b)
       (if value
           (setq mode-name "Newsticker -- up to date -- ")
         (setq mode-name "Newsticker -- NEED UPDATE -- "))))))


(defun newsticker--buffer-redraw ()
  "Sometimes (CVS) Emacs forgets to update the window..."
  (when (fboundp 'force-window-update)
    (force-window-update (current-buffer))))

(defun newsticker--buffer-insert-all-items ()
  "Insert all cached newsticker items into the current buffer.
Keeps order of feeds as given in `newsticker-url-list' and
`newsticker-url-list-defaults'."
  (goto-char (point-min))
  (mapc (lambda (url-item)
	  (let* ((feed-name-symbol (intern (car url-item)))
		 (feed (assoc feed-name-symbol newsticker--cache))
		 (items (cdr feed))
		 (pos (point)))
	    (when feed
	      ;; insert the feed description
	      (mapc (lambda (item)
		      (when (eq (newsticker--age item) 'feed)
			(newsticker--buffer-insert-item item
							feed-name-symbol)))
		    items)
	      ;;insert the items
	      (mapc (lambda (item)
		      (if (memq (newsticker--age item) '(new immortal old
							     obsolete))
			  (newsticker--buffer-insert-item item
                                                          feed-name-symbol)))
		    items)
	      (put-text-property pos (point) 'feed (car feed))
	      ;; insert empty line between feeds
	      (let ((p (point)))
		(insert "\n")
		(put-text-property p (point) 'hard t)))))
	(append newsticker-url-list newsticker-url-list-defaults))
  
  ;; fill if necessary
;;   (when newsticker-justification
;;     (let ((use-hard-newlines t))
;;       ;; set margin
;;       ;; (set-left-margin (point-min) (point-max) 2)
;;       (fill-region (point-min) (point-max) newsticker-justification)))

  (newsticker--buffer-set-faces (point-min) (point-max))
  (goto-char (point-min)))


(defun newsticker--buffer-insert-item (item &optional feed-name-symbol)
  "Insert a news item in the current buffer.
Insert the string PREFIX and a formatted representation of the
ITEM.  The optional parameter FEED-NAME-SYMBOL determines how the
item is formatted and whether the item-retrieval time is added as
well."
  ;; insert headline
  (if (eq (newsticker--age item) 'feed)
      (newsticker--buffer-do-insert-text item 'feed feed-name-symbol)
    (newsticker--buffer-do-insert-text item 'item feed-name-symbol))
  ;; insert the description
  (newsticker--buffer-do-insert-text item 'desc feed-name-symbol))


(defun newsticker--buffer-do-insert-text (item type feed-name-symbol)
  "Actually insert contents of news item, format it, render it and all that.
ITEM is a news item, TYPE tells which part of the item shall be inserted,
FEED-NAME-SYMBOL tells to which feed this item belongs."
  (let* ((pos (point))
	 (format newsticker-desc-format)
	 (pos-date-start nil)
	 (pos-date-end nil)
	 (pos-stat-start nil)
	 (pos-stat-end nil)
	 (pos-text-start nil)
	 (pos-text-end nil)
         (pos-extra-start nil)
         (pos-extra-end nil)
	 (age (newsticker--age item))
	 (preformatted-contents (newsticker--preformatted-contents item))
	 (preformatted-title (newsticker--preformatted-title item)))
    (cond ((and preformatted-contents
		(not (eq (aref preformatted-contents 0) ?\n));; we must NOT have a
						       ;; line break!
		(eq type 'desc))
           (insert preformatted-contents)
	   (add-text-properties pos (point) (list 'nt-type type
                                                  ;;'nt-face type
                                                  'nt-age  age)))
          ((and preformatted-title
		(not (eq (aref preformatted-title 0) ?\n));; we must NOT have a
						    ;; line break!
                (eq type 'item))
           (insert preformatted-title)
	   (add-text-properties pos (point) (list 'nt-type type
                                                  ;;'nt-face type
                                                  'nt-age  age)))
          (t
           ;; item was not formatted before.
           ;; Let's go.
           (if (eq type 'item)
               (setq format newsticker-item-format)
             (if (eq type 'feed)
                 (setq format newsticker-heading-format)))
	   
           (while (> (length format) 0)
             (let ((prefix (if (> (length format) 1)
                               (substring format 0 2)
                             "")))
               (cond ((string= "%c" prefix)
                      ;; contents
                      (when (newsticker--desc item)
                        (setq pos-text-start (point-marker))
                        (insert (newsticker--desc item))
                        (setq pos-text-end (point-marker)))
                      (setq format (substring format 2)))
                     ((string= "%d" prefix)
                      ;; date
                      (setq pos-date-start (point-marker))
                      (if (newsticker--time item)
                          (insert (format-time-string newsticker-date-format
                                                      (newsticker--time item))))
                      (setq pos-date-end (point-marker))
                      (setq format (substring format 2)))
                     ((string= "%l" prefix)
                      ;; logo
                      (let ((disabled (cond ((eq (newsticker--age item) 'feed)
                                             (= (newsticker--stat-num-items
                                                 feed-name-symbol 'new) 0))
                                            (t
                                             (not (eq (newsticker--age item) 'new))))))
                        (let ((img (newsticker--image-read feed-name-symbol disabled)))
                          (when img
                            (newsticker--insert-image img (car item)))))
                      (setq format (substring format 2)))
                     ((string= "%L" prefix)
                      ;; logo or title
                      (let ((disabled (cond ((eq (newsticker--age item) 'feed)
                                             (= (newsticker--stat-num-items
                                                 feed-name-symbol 'new) 0))
                                            (t
                                             (not (eq (newsticker--age item) 'new))))))
                        (let ((img (newsticker--image-read feed-name-symbol disabled)))
                          (if img
                              (newsticker--insert-image img (car item))
                            (when (car item)
                              (setq pos-text-start (point-marker))
                              (insert (car item))
                              (setq pos-text-end (point-marker))))))
                      (setq format (substring format 2)))
                     ((string= "%s" prefix)
                      ;; statistics
                      (setq pos-stat-start (point-marker))
                      (if (eq (newsticker--age item) 'feed)
                          (insert (newsticker--buffer-statistics
				   feed-name-symbol)))
                      (setq pos-stat-end (point-marker))
                      (setq format (substring format 2)))
                     ((string= "%t" prefix)
                      ;; title
                      (when (car item)
                        (setq pos-text-start (point-marker))
                        (insert (car item))
                        (setq pos-text-end (point-marker)))
                      (setq format (substring format 2)))
                     ((string-match "%." prefix)
                      ;; unknown specifier!
                      (insert prefix)
                      (setq format (substring format 2)))
                     ((string-match "^\\([^%]*\\)\\(.*\\)" format) ;; FIXME!
                      ;; everything else
                      (let ((p (point)))
                        (insert (substring format
                                           (match-beginning 1) (match-end 1)))
                        ;; in case that the format string contained newlines
                        (put-text-property p (point) 'hard t))
                      (setq format (substring format (match-beginning 2)))))))
    
           ;; decode HTML if possible...
	   (let ((is-rendered-HTML nil))
	     (when (and newsticker-html-renderer pos-text-start pos-text-end)
	       (condition-case error-data
		   (save-excursion
		     ;; check whether it is necessary to call html renderer
		     ;; (regexp inspired by htmlr.el)
		     (goto-char pos-text-start)
		     (when (re-search-forward
			    "</?[A-Za-z1-6]*\\|&#?[A-Za-z0-9]+;" pos-text-end t)
                       ;; (message "%s" (newsticker--title item))
		       (funcall newsticker-html-renderer pos-text-start
				pos-text-end)
		       ;; FIXME: compiler warning about free variable
		       ;; w3m-minor-mode-map
                       (if (eq newsticker-html-renderer 'w3m-region)
                           (add-text-properties pos (point-max)
                                            (list 'keymap w3m-minor-mode-map)))
		       (setq is-rendered-HTML t)))
		 (error
		  (message "Error: HTML rendering failed: %s, %s"
			   (car error-data) (cdr error-data)))))
	     (when (and newsticker-justification
			(not is-rendered-HTML))
	       (let ((use-hard-newlines t))
		 (fill-region pos (point-max) newsticker-justification))))

	   ;; remove leading and trailing newlines
	   (goto-char pos)
           (unless (= 0 (skip-chars-forward " \t\r\n"))
             (delete-region pos (point)))
	   (goto-char (point-max))
	   (let ((end (point)))
	     (unless (= 0 (skip-chars-backward " \t\r\n" (1+ pos)))
	       (delete-region (point) end)))
	   (goto-char (point-max))

           ;; closing newline
	   (unless nil ;;(eq pos (point))
	     (insert "\n")
	     (put-text-property (1- (point)) (point) 'hard t))

           ;; show extra elements
           (when (and newsticker-show-all-rss-elements
                      (eq type 'desc))
             (goto-char (point-max))
             (setq pos-extra-start (point))
             (mapc (lambda (extra-element)
                     (unless (memq (car extra-element)
                                   '(items link title description content:encoded
                                           dc:subject dc:date item guid))
                       (insert (format "%s:\t%s\n" (car extra-element)
                                       (nth 2 extra-element)))))
                   (newsticker--extra item))
             (setq pos-extra-end (point)))

           ;; text properties
           (when (memq type '(feed item))
             (add-text-properties pos (1- (point))
                                  (list 'mouse-face 'highlight
                                        'nt-link (newsticker--link item)
                                        'help-echo "mouse-2: visit item"
                                        'keymap newsticker--heading-keymap
                                        ))
             (add-text-properties pos (point)
                                  (list 'nt-title (newsticker--title item)
					'nt-desc (newsticker--desc item))))
    
           (add-text-properties pos (point) (list 'nt-type type
                                                  'nt-face type
                                                  'nt-age  age
                                                  'nt-guid (newsticker--guid item)))
           (when (and pos-date-start pos-date-end)
             (put-text-property pos-date-start pos-date-end 'nt-face 'date))
           (when (and pos-stat-start pos-stat-end)
             (put-text-property pos-stat-start pos-stat-end 'nt-face 'stat))
           (when (and pos-extra-start pos-extra-end)
             (put-text-property pos-extra-start pos-extra-end 'nt-face 'extra))

           ;; save rendered stuff
           (cond ((eq type 'desc)
                  (if (nthcdr 6 item)
                      (setcar (nthcdr 6 item) (buffer-substring pos (point)))
                    (setcdr (nthcdr 5 item) (list
					     (buffer-substring pos (point))))))
                 ((eq type 'item)
                  (if (nthcdr 7 item)
                      (setcar (nthcdr 7 item) (buffer-substring pos (point)))
                    (setcdr (nthcdr 6 item) (list
					     (buffer-substring pos (point)))))))
           ))))

(defun newsticker--buffer-statistics (feed-name-symbol)
  "Return a statistic string for the feed given by FEED-NAME-SYMBOL.
See `newsticker-statistics-format'."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "%a"
     (format "%d" (newsticker--stat-num-items feed-name-symbol))
     (replace-regexp-in-string
      "%i"
      (format "%d" (newsticker--stat-num-items feed-name-symbol 'immortal))
      (replace-regexp-in-string
       "%n"
       (format "%d" (newsticker--stat-num-items feed-name-symbol 'new))
       (replace-regexp-in-string
        "%o"
        (format "%d" (newsticker--stat-num-items feed-name-symbol 'old))
        (replace-regexp-in-string
         "%O"
         (format "%d" (newsticker--stat-num-items feed-name-symbol 'obsolete))
         newsticker-statistics-format)))))))

(defun newsticker--buffer-set-faces (start end)
  "Add face properties according to mark property.
Scans the buffer between START and END."
  (save-excursion
    ;;(put-text-property start end 'face 'newsticker-default-face)
    (goto-char start)
    (let ((pos1 start)
	  (pos2 1)
	  (nt-face (get-text-property start 'nt-face))
	  (nt-age (get-text-property start 'nt-age)))
      (when nt-face
	(setq pos2 (next-single-property-change (point) 'nt-face))
	(newsticker--set-face-properties pos1 pos2 nt-face nt-age)
	(setq nt-face (get-text-property pos2 'nt-face))
	(setq pos1 pos2))
      (while (and (setq pos2 (next-single-property-change pos1 'nt-face))
		  (<= pos2 end)
		  (> pos2 pos1))
	(newsticker--set-face-properties pos1 pos2 nt-face nt-age)
	(setq nt-face (get-text-property pos2 'nt-face))
	(setq nt-age (get-text-property pos2 'nt-age))
	(setq pos1 pos2))))
  (newsticker--buffer-set-invisibility start end))

(defun newsticker--buffer-set-invisibility (start end)
  "Add face properties according to mark property.
Scans the buffer between START and END."
  (save-excursion
    ;; reset invisibility settings
    (put-text-property start end 'invisible nil)
    ;; let's go
    (goto-char start)
    (let ((pos1 start)
	  (pos2 1)
	  (nt-type (get-text-property start 'nt-type))
	  (nt-age (get-text-property start 'nt-age)))
      (when nt-type
	(setq pos2 (next-single-property-change (point) 'nt-type))
	(setq nt-type (get-text-property pos2 'nt-type))
	(setq pos1 pos2))
      (while (and (setq pos2 (next-single-property-change pos1 'nt-type))
		  (<= pos2 end)
		  (> pos2 pos1))
        ;; must shift one char to the left in order to handle inivisible
        ;; newlines, motion in invisible text areas and all that correctly
	(put-text-property (1- pos1) (1- pos2)
			   'invisible
			   (intern (concat (symbol-name nt-type)
					   "-"
					   (symbol-name nt-age))))
	(setq nt-type (get-text-property pos2 'nt-type))
	(setq nt-age (get-text-property pos2 'nt-age))
	(setq pos1 pos2)))))

(defun newsticker--set-face-properties (pos1 pos2 nt-face age)
  "Set the face for the text between the positions POS1 and POS2.
The face is chosen according the values of NT-FACE and AGE."
  (let ((face (cond ((eq nt-face 'feed)
		     'newsticker-feed-face)
		    ((eq nt-face 'item)
		     (cond ((eq age 'new)
			    'newsticker-new-item-face)
			   ((eq age 'old)
			    'newsticker-old-item-face)
			   ((eq age 'immortal)
			    'newsticker-immortal-item-face)
			   ((eq age 'obsolete)
			    'newsticker-obsolete-item-face)))
		    ((eq nt-face 'date)
		     'newsticker-date-face)
		    ((eq nt-face 'stat)
		     'newsticker-statistics-face)
		    ((eq nt-face 'extra)
		     'newsticker-extra-face)
;; 		    (t
;; 		     'newsticker-default-face)
		    )))
    (when face
      (put-text-property pos1 pos2 'face face))))
	 
(defun newsticker--insert-image (img string)
  "Insert IMG with STRING at point.
This is a work-around for a strange behavior of Emacs versions before
21.3.50.  Images inserted with `insert-image' vanished after doing
`fill-region'."
  ;; This should work:
  ;;(insert-image img string)
  ;; but it does not. Therefore we do this, which should be equivalent!
  (let ((pos (point)))
    ;;(insert string)
    (insert ":-)")
    (add-text-properties pos (point) (list 'display img))))

;; ======================================================================
;;; HTML rendering
;; ======================================================================
(defun newsticker-htmlr-render (pos1 pos2) ;
  "Replacement for `htmlr-render'.
Renders the HTML code in the region POS1 to POS2 using htmlr."
  (let ((str (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert
     (with-temp-buffer
       (insert str)
       (goto-char (point-min))
       ;; begin original htmlr-render
       (htmlr-reset)
       ;; something omitted here...
       (while (< (point) (point-max))
	 (htmlr-step))
       ;; end original htmlr-render
       (newsticker--remove-whitespace (buffer-string))))))


;; ======================================================================
;;; Functions working on the *newsticker* buffer
;; ======================================================================
(defun newsticker--buffer-make-item-completely-visible ()
  "Scroll buffer until current item is completely visible."
  (sit-for 0)
  (save-excursion
    (let ((nt (count-screen-lines (point-min) (window-start)))
	  (nb (count-screen-lines (point-min) (window-end)))
	  (ns (count-screen-lines (point-min) (point)))
	  (ne (save-excursion
		(1+ 
		 (count-screen-lines (point-min)
				     (newsticker--buffer-end-of-item))))))
      (when (< nb ne)
	;;(message "nt=%d, nb=%d, ns=%d, ne=%d -- %d" nt nb ns ne
	;; (min (- ns nt) (- ne ns)))
	(scroll-up (min (- ns nt) (- ne ns)))))))

(defun newsticker--buffer-goto (types &optional age backwards)
  "Search next occurrence of TYPES in current buffer.
TYPES is a list of symbols.  If TYPES is found point is moved, if
not point is left unchanged.  If optional parameter AGE is not
nil, the type AND the age must match.  If BACKWARDS is t, search
backwards."
  (let ((pos (save-excursion
               (catch 'found
                 (let ((tpos (point)))
                   (while (setq tpos
                                (if backwards
                                    (if (eq tpos (point-min))
                                        nil
                                      (or (previous-single-property-change
                                           tpos 'nt-type)
                                          (point-min)))
                                  (next-single-property-change
                                   tpos 'nt-type)))
                     (and (memq (get-text-property tpos 'nt-type) types)
			  (or (not age)
			      (eq (get-text-property tpos 'nt-age) age))
			  (throw 'found tpos))))))))
    (when pos
      (goto-char pos))
    pos))


(defun newsticker--buffer-hideshow (mark-age onoff )
  "Hide or show items of type MARK-AGE.
If ONOFF is nil the item is hidden, otherwise it is shown."
  (if onoff
      (remove-from-invisibility-spec mark-age )
    (add-to-invisibility-spec mark-age)))

(defun newsticker--buffer-beginning-of-item ()
  "Move point to the beginning of the item at point.
Return new position."
  (if (bobp)
      (point)
    (let ((type (get-text-property (point) 'nt-type))
	  (typebefore (get-text-property (1- (point)) 'nt-type)))
      (if (and (memq type '(item feed))
		   (not (eq type typebefore)))
	  (point)
	(newsticker--buffer-goto '(item feed) nil t)
	(point)))))

(defun newsticker--buffer-end-of-item ()
  "Move point to the end of the item at point.
Take care: end of item is at the end of its last line!"
  (when (newsticker--buffer-goto '(item feed nil))
    (point)))

;; ======================================================================
;;; manipulation of ticker text
;; ======================================================================
(defun newsticker--ticker-text-setup ()
  "Build the ticker text which is scrolled or flashed in the echo area."
  ;; reset scrollable text
  (setq newsticker--scrollable-text "")
  (setq newsticker--item-list nil)
  (setq newsticker--item-position 0)
  ;; build scrollable text from cache data
  (let ((have-something nil))
    (mapc
     (lambda (feed)
       (let ((feed-name (symbol-name (car feed))))
	 (let ((num-new (newsticker--stat-num-items (car feed) 'new))
	       (num-old (newsticker--stat-num-items (car feed) 'old))
	       (num-imm (newsticker--stat-num-items (car feed) 'immortal))
	       (num-obs (newsticker--stat-num-items (car feed) 'obsolete)))
	   (when (or (> num-new 0)
		     (and (> num-old 0)
			  (not newsticker-hide-old-items-in-echo-area))
		     (and (> num-imm 0)
			  (not newsticker-hide-immortal-items-in-echo-area))
		     (and (> num-obs 0)
			  (not newsticker-hide-obsolete-items-in-echo-area)))
	     (setq have-something t)
	     (mapc
	      (lambda (item)
		(let ((title ;(or (newsticker--preformatted-title item)
				 (newsticker--title item)
                                 ;)
                                 )
		      (age (newsticker--age item)))
		  (when
		      (or (eq age 'new)
			  (and (eq age 'old)
			       (not newsticker-hide-old-items-in-echo-area))
			  (and (eq age 'obsolete)
			       (not
				newsticker-hide-obsolete-items-in-echo-area))
			  (and (eq age 'immortal)
			       (not
				newsticker-hide-immortal-items-in-echo-area)))
		    (setq title (newsticker--remove-whitespace title))
		    ;; add to flash list
		    (add-to-list 'newsticker--item-list
				 (concat feed-name ": " title) t)
		    ;; and to the scrollable text
		    (setq newsticker--scrollable-text
			  (concat newsticker--scrollable-text
				  " " feed-name ": " title " +++")))))
	      (cdr feed))))))
     newsticker--cache)
    (when have-something
      (setq newsticker--scrollable-text
	    (concat "+++ "
		    (format-time-string "%A, %H:%M"
					newsticker--latest-update-time)
		    " ++++++" newsticker--scrollable-text)))))

(defun newsticker--ticker-text-remove (feed title)
  "Remove the item of FEED with TITLE from the ticker text."
  ;; reset scrollable text
  (setq newsticker--item-position 0)
  (let ((feed-name (symbol-name feed)))
    ;; remove from flash list
    (setq newsticker--item-list (remove (concat feed-name ": " title)
					newsticker--item-list))
    ;; and from the scrollable text
    (setq newsticker--scrollable-text
	  (replace-regexp-in-string
	   (regexp-quote (concat " " feed-name ": " title " +++"))
	   ""
	   newsticker--scrollable-text))))


;; ======================================================================
;;; manipulation of cached data
;; ======================================================================
(defun newsticker--cache-replace-age (data feed old-age new-age)
  "Mark all items in DATA in FEED which carry age OLD-AGE with NEW-AGE.
If FEED is 'any it applies to all feeds.  If OLD-AGE is 'any,
all marks are replaced by NEW-AGE.  Removes all pre-formatted contents."
  (mapc (lambda (a-feed)
	  (when (or (eq feed 'any)
		    (eq (car a-feed) feed))
	    (let ((items (cdr a-feed)))
	      (mapc (lambda (item)
		      (when (or (eq old-age 'any)
				(eq (newsticker--age item) old-age))
			(setcar (nthcdr 4 item) new-age)
                        (if (nthcdr 7 item)
                            (setcar (nthcdr 7 item) nil))
                        (if (nthcdr 6 item)
                            (setcar (nthcdr 6 item) nil))))
		    items))))
	data)
  data)

(defun newsticker--cache-mark-expired (data feed old-age new-age time)
  "Mark all expired obsolete entries from DATA in the feed FEED."
  (let ((exp-time (time-add (current-time) (seconds-to-time time))))
    (mapc
     (lambda (a-feed)
       (when (or (eq feed 'any)
		 (eq (car a-feed) feed))
	 (let ((items (cdr a-feed)))
	   (mapc
	    (lambda (item)
	      (when (and (eq (newsticker--age item) old-age)
			 (time-less-p (newsticker--time item) exp-time))
		(setcar (nthcdr 4 item) new-age)))
	    items))))
     data)
    data))

(defun newsticker--cache-contains (data feed title desc link age
					&optional guid)
  "Check DATA whether FEED contains an item with TITLE, DESC, LINK, and AGE.

If AGE equals 'feed the TITLE, DESCription and LINK do not
matter. If DESC is nil it is ignored as well.  If
`newsticker-desc-comp-max' is non-nil, only the first
`newsticker-desc-comp-max' characters of DESC are taken into
account.  Return the item or nil."
  (condition-case nil
      (catch 'found
	(when (and desc newsticker-desc-comp-max
		   (> (length desc) newsticker-desc-comp-max))
	  (setq desc (substring desc 0 newsticker-desc-comp-max)))
        (mapc
	 (lambda (this-feed)
	   (when (eq (car this-feed) feed)
	     (mapc (lambda (anitem)
		     (when (or
                            ;; global unique id can match
                            (and guid
                                 (string= guid (newsticker--guid anitem)))
                            ;; or title, desc, etc.
                            (and
                             (or (not (eq age 'feed))
                                 (eq (newsticker--age anitem) 'feed))
                             (string= (newsticker--title anitem)
                                      title)
                             (or (not link)
                                 (string= (newsticker--link anitem)
                                          link))
                             (or (not desc)
                                 (if (and desc newsticker-desc-comp-max
                                          (> (length (newsticker--desc anitem))
                                             newsticker-desc-comp-max))
                                     (string= (substring
                                               (newsticker--desc anitem)
                                               0 newsticker-desc-comp-max)
                                              desc)
                                   (string= (newsticker--desc anitem)
                                            desc)))))
		       (throw 'found anitem)))
		   (cdr this-feed))))
	 data)
	nil)
    (error nil)))

(defun newsticker--cache-add (data feed-name-symbol title desc link time age
				   position
				   &optional updated-age updated-time
				   preformatted-contents
				   preformatted-title
                                   extra-elements)
  "Add another item to cache data.
Add to DATA in the FEED-NAME-SYMBOL an item with TITLE, DESC,
LINK, TIME, AGE, and POSITION.  If this item is contained
already, its mark is set to UPDATED-AGE, its time is set to
UPDATED-TIME, and its pre-formatted contents is set to
PREFORMATTED-CONTENTS and PREFORMATTED-TITLE.  Returns the age
which the item got."
  (let ((item (newsticker--cache-contains data feed-name-symbol title
					  desc link age)))
    (if item
      ;; does exist already -- change age, update time and position
	(progn
	  (if (nthcdr 5 item)
	      (setcar (nthcdr 5 item) position)
	    (setcdr (nthcdr 4 item) (list position)))
	  (setcar (nthcdr 4 item) updated-age)
	  (if updated-time
	      (setcar (nthcdr 3 item) updated-time))
	  ;; replace cached pre-formatted contents
	  (if (nthcdr 6 item)
	      (setcar (nthcdr 6 item) preformatted-contents)
	    (setcdr (nthcdr 5 item) preformatted-contents))
	  (if (nthcdr 7 item)
	      (setcar (nthcdr 7 item) preformatted-title)
	    (setcdr (nthcdr 6 item) preformatted-title))
	  )
      ;; did not exist or age equals 'feed-name-symbol
      (catch 'found
        (mapc (lambda (this-feed)
		(when (eq (car this-feed) feed-name-symbol)
		  (setcdr this-feed (nconc (cdr this-feed)
					   (list (list title desc link
						       time age position
						       preformatted-contents
						       preformatted-title
                                                       extra-elements))))
		  (throw 'found this-feed)))
	      data)
        ;; the feed is not contained
        (add-to-list 'data (list feed-name-symbol
				 (list title desc link time age position
				       preformatted-contents
				       preformatted-title
                                       extra-elements))
                     t))))
  data)

(defun newsticker--cache-remove (data feed-symbol age)
  "Remove all entries from DATA in the feed FEED-SYMBOL with AGE.
FEED-SYMBOL may be 'any.  Entries from old feeds, which are no longer in
`newsticker-url-list' or `newsticker-url-list-defaults', are removed as
well."
  (let* ((pos data)
         (feed (car pos))
         (last-pos nil))
    (while feed
      (if (or (assoc (symbol-name (car feed)) newsticker-url-list)
              (assoc (symbol-name (car feed)) newsticker-url-list-defaults))
          ;; feed is still valid=active
          ;; (message "Keeping feed %s" (car feed))
          (if  (or (eq feed-symbol 'any)
                   (eq feed-symbol (car feed)))
              (let* ((item-pos (cdr feed))
                     (item (car item-pos))
                     (prev-pos nil))
                (while item
                  ;;(message "%s" (car item))
                  (if (eq age (newsticker--age item))
                      ;; remove this item
                      (progn 
                        ;;(message "Removing item %s" (car item))
                        (if prev-pos
                            (setcdr prev-pos (cdr item-pos))
                          (setcdr feed (cdr item-pos))))
                    ;;(message "Keeping item %s" (car item))
                    (setq prev-pos item-pos))
                  (setq item-pos (cdr item-pos))
                  (setq item (car item-pos)))))
        ;; feed is not active anymore
        ;; (message "Removing feed %s" (car feed))
        (if last-pos
            (setcdr last-pos (cdr pos))
          (setq data (cdr pos))))
      (setq last-pos pos)
      (setq pos (cdr pos))
      (setq feed (car pos)))))

;; ======================================================================
;;; Sorting
;; ======================================================================
(defun newsticker--cache-item-compare-by-time (item1 item2)
  "Compare two news items ITEM1 and ITEM2 by comparing their time values."
  (catch 'result
    (let ((age1  (newsticker--age item1))
	  (age2  (newsticker--age item2)))
      (if (not (eq age1 age2))
	  (cond ((eq age1 'obsolete)
		 (throw 'result nil))
		((eq age2 'obsolete)
		 (throw 'result t)))))
    (let* ((time1 (newsticker--time item1))
	   (time2 (newsticker--time item2)))
      (cond ((< (nth 0 time1) (nth 0 time2))
	     nil)
	    ((> (nth 0 time1) (nth 0 time2))
	     t)
	    ((< (nth 1 time1) (nth 1 time2))
	     nil)
	    ((> (nth 1 time1) (nth 1 time2))
	     t)
	    ((< (or (nth 2 time1) 0) (or (nth 2 time2) 0))
	     nil)
	    ((> (or (nth 2 time1) 0) (or (nth 2 time2) 0))
	     t)
	    (t
	     nil)))))

(defun newsticker--cache-item-compare-by-title (item1 item2)
  "Compare ITEM1 and ITEM2 by comparing their titles."
  (catch 'result
    (let ((age1  (newsticker--age item1))
	  (age2  (newsticker--age item2)))
      (if (not (eq age1 age2))
	  (cond ((eq age1 'obsolete)
		 (throw 'result nil))
		((eq age2 'obsolete)
		 (throw 'result t)))))
    (string< (newsticker--title item1) (newsticker--title item2))))

(defun newsticker--cache-item-compare-by-position (item1 item2)
  "Compare ITEM1 and ITEM2 by comparing their original positions."
  (catch 'result
    (let ((age1  (newsticker--age item1))
	  (age2  (newsticker--age item2)))
      (if (not (eq age1 age2))
	  (cond ((eq age1 'obsolete)
		 (throw 'result nil))
		((eq age2 'obsolete)
		 (throw 'result t)))))
    (< (or (newsticker--pos item1) 0) (or (newsticker--pos item2) 0))))

(defun newsticker--cache-sort ()
  "Sort the newsticker cache data."
  (let ((sort-fun (cond ((eq newsticker-sort-method 'sort-by-time)
			 'newsticker--cache-item-compare-by-time)
			((eq newsticker-sort-method 'sort-by-title)
			 'newsticker--cache-item-compare-by-title)
			((eq newsticker-sort-method 'sort-by-original-order)
			 'newsticker--cache-item-compare-by-position))))
    (mapc (lambda (feed-list)
	    (setcdr feed-list (sort (cdr feed-list)
				    sort-fun)))
	  newsticker--cache)))
  
(defun newsticker--cache-save ()
  "Save cache data."
  ;; FIXME: prevent from printing "Wrote ..." message?!
  (save-current-buffer
    (let ((coding-system-for-write 'utf-8)
	  (buf (find-file-noselect newsticker-cache-filename)))
      (when buf
	(set-buffer buf)
	(erase-buffer)
	(insert ";; -*- coding: utf-8 -*-\n")
	(insert (prin1-to-string newsticker--cache))
	(save-buffer)))
    ;; FIXME?: clear echo area from write-region's output
    ))
  
;; ======================================================================
;;; Statistics
;; ======================================================================
(defun newsticker--stat-num-items (feed &optional age)
  "Return number of items in the given FEED which have the given AGE.
If AGE is nil, the total number items is returned."
  (let ((items (cdr (assoc feed newsticker--cache)))
	(num 0))
    (while items
      (if age
          (if (eq (newsticker--age (car items)) age)
              (setq num (1+ num)))
        (if (memq (newsticker--age (car items)) '(new old immortal obsolete))
              (setq num (1+ num))))
      (setq items (cdr items)))
    num))


;; ======================================================================
;;; OPML
;; ======================================================================
(defun newsticker-opml-export ()
  "OPML subscription export.
Export subscriptions to a buffer in OPML Format."
  (interactive)
  (with-current-buffer (get-buffer-create "*OPML Export*")
    (set-buffer-file-coding-system 'utf-8)
    (insert (concat
	     "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	     "<!-- OPML generated by Emacs newsticker.el -->\n"
	     "<opml version=\"1.0\">\n"
	     "  <head>\n"
	     "    <title>mySubscriptions</title>\n"
	     "    <dateCreated>" (format-time-string "%a, %d %b %Y %T %z")
	     "</dateCreated>\n"
	     "    <ownerEmail>" user-mail-address "</ownerEmail>\n"
	     "    <ownerName>" (user-full-name) "</ownerName>\n"
	     "  </head>\n"
	     "  <body>\n"))
    (mapc (lambda (sub)
	    (insert "    <outline text=\"")
	    (insert (newsticker--title sub))
	    (insert "\" xmlUrl=\"")
	    (insert (cadr sub))
	    (insert "\"/>\n"))
	  (append newsticker-url-list newsticker-url-list-defaults))
    (insert "  </body>\n</opml>\n"))
  (pop-to-buffer "*OPML Export*")
  (when (fboundp 'sgml-mode)
    (sgml-mode)))



(provide 'newsticker)
;;; newsticker.el ends here
