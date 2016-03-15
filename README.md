# annot
annot.el - a global annotator/highlighter for GNU Emacs

### Introduction ###

Here is the video:

<a href='http://www.youtube.com/watch?feature=player_embedded&v=cCIetEFzdvY' target='_blank'><img src='http://img.youtube.com/vi/cCIetEFzdvY/0.jpg' width='450' height=344 /></a>

`annot.el' is a general (and scalable) annotation manager that works on GNU
Emacs.  It lets you add/edit/remove annotations, highlights, or even annotated
images on any file and manages them for you.  All annotations are stored
separately for each annotated file and get reproduced when the file is opened
again. You can even store annotations on non-editable files.  Because annot
keeps track of md5 checksums of annotated files, annotations won't disappear
even when file names are changed.


### Installation ###

To install annot.el, download the file directly from
https://raw.githubusercontent.com/zefew/annot/master/src/annot.el, place it
under one of your load paths, and add the following line into your ~/.emacs,
~/.emacs.el or ~/.emacs.d/init.el:

> (require 'annot)

### Keybindings ###

```
[C-x C-a] - Add a new annotation
            Highlight the current region if any (annot-edit/add)
[C-x C-r] - Remove the annotation/highlight at point (annot-remove)
[C-x C-i] - Insert a new image at point (annot-add-image)
```

### User Commands ###

```
* `annot-edit/add'   - Add a new annotation or highlight if a region is
                       specified.  Edit an annotation if there's one already.
* `annot-remove'     - Remove the annotation/highlight at point.
* `annot-add-image'  - Insert an image at point.
* `annot-convert'    - Convert text within the active region into an
                       annot text annotation
* `annot-to-comment' - Convert a text annotation at point to a comment
* `annot-goto-next'  - Go to the next annot overlay
* `annot-goto-previous' - Go to the previous annot overlay

;; Deprecated (use `annot-edit/add' instead)
* `annot-add'       - add a new annotation/highlight at point.
* `annot-edit'      - edit the annotation at point.
```

### Screenshot ###

![https://raw.githubusercontent.com/zefew/annot/master/media/annot_example_20160315.png](https://raw.githubusercontent.com/zefew/annot/master/media/annot_example_20160315.png)

### Change List ###

List of changes: https://github.com/zefew/annot/commits/master

