Instructions to install latest JDE:

1. # create directory ~/.xemacs/xemacs-packages if you don't already
   # have it.

1. # get CEDET (from http://cedet.sourceforge.net/)
   % cd ~/.xemacs/xemacs-packages
   % tar -zxvf cedet-latest.tar.gz
   % cd cedet-*
   % make EMACS=xemacs
 
1. # get JDE (from http://jdee.sunsite.dk/)
   % cd ~/.xemacs/xemacs-packages
   % tar -zxvf jde-latest.tar.gz
   % mv $XEMACS_DIR/lib/xemacs/xemacs-packages/etc/jde{,.nouse}
   # last step is important - see: http://jdee.sunsite.dk/install.html
   % cd jde-2.3.5/lisp/
   % make EMACS=xemacs CEDET=../../cedet-1.0beta3b ELIB=$XEMACS_DIR/lib/xemacs/xemacs-packages/lisp/elib

 
3. add something like this to your ~/.xemacs/init.el
 
;; local copies of cedet (cogre, eieio, speedbar, semantic, etc)
;; doesn't conform to xemacs-packages structure
(let ((cedet-config 
       (expand-file-name "~/.xemacs/xemacs-packages/cedet-1.0beta3b/common/cedet.el")))
  (if (file-exists-p cedet-config)
      (progn (load-file cedet-config)
             (semantic-load-enable-code-helpers))))

You may want (semantic-load-enable-minimum-features) in place of
(semantic-load-enable-code-helpers) above.  Some have reported
performance issues. 
 
Restart xemacs - DONE.
