### Introduction ###

<a href='http://www.youtube.com/watch?feature=player_embedded&v=cCIetEFzdvY' target='_blank'><img src='http://img.youtube.com/vi/cCIetEFzdvY/0.jpg' width='450' height=344 /></a>

`annot.el' is a general and scalable annotation manager that works on GNU Emacs.  It lets you add/remove/edit annotations on any file (or buffer) and manages them for later use. All annotations are stored separately for each annotated file and are restored when the file gets opened again. You can even store annotations on non-editable files.  Because annot keeps track of md5 checksums of annotated files, annotations won't disappear even when file names are changed.

### Installation ###

To install annot.el, either download directly from http://annot.googlecode.com/svn/trunk/src/annot.el or run

$ wget http://annot.googlecode.com/svn/trunk/src/annot.el

and install annot.el by placing it under one of your load paths and then by
inserting

> (require 'annot)

### Commands: ###

```
[C-x a]    -  add a new annotation/highlight or edit an existing annotation/highlight.
              You can also use [C-x C-a]. (annot-edit/add)
[C-x r]    -  remove annotation at point. (annot-remove)
[C-x w]    -  insert an image at point. (annot-add-image)
[C-x A]    -  convert the text within the currently active region into an annot text annotation. (annot-convert)
```

### Screenshot ###

![http://annot.googlecode.com/files/annot_example3.png](http://annot.googlecode.com/files/annot_example3.png)

### Change List ###

List of changes: http://code.google.com/p/annot/source/list