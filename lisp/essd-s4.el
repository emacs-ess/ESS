X-From-Line: bates@stat.wisc.edu Wed Jul 23 13:11:13 1997
Received: from franz.stat.wisc.edu (root@franz.stat.wisc.edu [128.105.5.95]) by milo.math.sc.edu (8.6.12/8.6.12) with ESMTP id NAA26959 for <rossini@stat.sc.edu>; Wed, 23 Jul 1997 13:11:12 -0400
Received: by franz.stat.wisc.edu
	id m0wr4wI-000hn9C
	(Debian Smail-3.2 1996-Jul-4 #2); Wed, 23 Jul 1997 12:11:02 -0500 (CDT)
Sender: bates@stat.wisc.edu (Douglas Bates)
Sender: bates@franz.stat.wisc.edu
To: Anthony Rossini <rossini@stat.sc.edu>
Cc: jmc@research.bell-labs.com
Subject: Re: S-mode-4.9-b8
References: <m0wr3RC-000hn9C@franz.stat.wisc.edu> <wncsox5og77.fsf@urn.math.sc.edu>
From: Douglas Bates <bates@stat.wisc.edu>
Date: 23 Jul 1997 12:11:02 -0500
In-Reply-To: Anthony Rossini's message of 23 Jul 1997 13:01:48 -0400
Message-ID: <6r204pg0d5.fsf@franz.stat.wisc.edu>
X-Mailer: Gnus v5.4.37/XEmacs 19.15
Xref: urn.math.sc.edu mail.S-mode:50
Lines: 122

I started with a vanilla essd-s4.el as below.  Essentially I started
from essd-s+3.el rather than going back through all the older
modifications. It is working enough to get me going.  I haven't
exercised it to any great extent.

As it stands it will do object name completion and call up help pages
so it is a start.  I'm sure there are many things that can be done to
enhance it but the new modular structure should make those things much
easier to accomplish.

;;; essd-s4.el --- S4 customization

;; Copyright (C) 1997 A.J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Jun 1997
;; Modified: $Date: 1997/07/25 14:57:53 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: essd-s4.el,v 1.3 1997/07/25 14:57:53 rossini Exp $
;;
;; Keywords: start up, configuration.

;; This file is part of ess-mode.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; This file defines S4 customizations for ess-mode.  Lots of thanks
;;; to RMH and JMC for code and suggestions

;;;
;;; $Log: essd-s4.el,v $
;;; Revision 1.3  1997/07/25 14:57:53  rossini
;;; based on suggestions from Doug Bates.
;;;
;;; Revision 1.2  1997/06/22 23:49:28  rossini
;;; revert.
;;;
;;; Revision 1.1  1997/06/19 21:16:24  rossini
;;; Initial revision
;;;
;;;

;;; Autoloads:

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

;;; Code:

(defvar S4-customize-alist
  '((ess-proc-prefix      .         "S")
    (ess-version-running  .         "S4")
    (inferior-ess-program .         "S")
    (ess-help-sec-regex   .         "^[A-Z. ---]+:$")
    (ess-help-sec-keys-alist .      '((?a . "ARGUMENTS:")
				      (?b . "BACKGROUND:")
				      (?B . "BUGS:")
				      (?d . "DETAILS:")
				      (?D . "DESCRIPTION:")
				      (?e . "EXAMPLES:")
				      (?n . "NOTE:")
				      (?o . "OPTIONAL ARGUMENTS:")
				      (?r . "REQUIRED ARGUMENTS:")
				      (?R . "REFERENCES:")
				      (?s . "SIDE EFFECTS:")
				      (?S . "SEE ALSO:")
				      (?u . "USAGE:")
				      (?v . "VALUE:")))
    (inferior-ess-objects-command . "objects(%d)")
    (inferior-ess-help-command .    "help(\"%s\")\n")
    (inferior-ess-exit-command .    "q()\n")
    (ess-loop-timeout              . 100000 )
    (inferior-ess-primary-prompt   . "[a-zA-Z0-9() ]*> ?")
    (inferior-ess-secondary-prompt   . "+ ?"))
 "Variables to customize for S")


(defun S4 ()
  "Call 'S version 4', from Bell Labs
New way to do it."
  (interactive)
  (setq ess-customize-alist S4-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(S): ess-proc-prefix=%s , buf=%s \n"
	   ess-proc-prefix
	   (current-buffer)))
  (inferior-ess))


 ; Provide package

(provide 'essd-s4)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:     ^L ;
;;; Sections:    ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:  defuns, defvars, defconsts
;;;              Random code beginning with a ;;;;* comment

;;; Local variables:
;;; mode: emacs-lisp
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; essd-s4.el ends here


