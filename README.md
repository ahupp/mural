mural is a fast, fuzzy tag search typeahead for emacs

## Features

* "typeahead" (aka autocomplete) means that the result set will update
  automatically as you type.  If you've used symbol search in Eclipse
  or the Facebook search box, it's like that.

* "fuzzy" means that you can type any sequence of letters in a tag.
  e.g, 'p_g_href' will match 'profile_get_href'.  This lets you find
  tags whose name you only vagely remember, is robust to some typos,
  and only requires typing a fraction of the characters in most cases.

* "fast" means it works at interactive speeds even on extremely large
  tag sets.  For example, on my tags file with ~900k entries a single
  query returns in 30-50ms.  This compares with several seconds for a
  tab completion in find-tag, and over 10s to do a complete fuzzy
  search via ido-mode.

* changes to the tags file are picked up without a restart using
  inotify.

* supports multiple tag files and repositories.  The repo to search is
  chosen based on the location of the file in the current buffer.

* supports both ctags and etags format

* in addition to the tags, searches filenames contained in the tags
  file.

## Installation

This has only been tested on emacs 23.  YMMV with any other version.
Requires g++ to build.

1. Run `make` and put the resulting `mural_server` binary somewhere in your $PATH
2. Put mural.el in your emacs load-path
3. Add the following to your .emacs file:

```elisp
(require 'mural)
(mural-add-tagfile "~/www/TAGS")
(mural-add-tagfile "~/another/repository/TAGS")
(global-set-key (kbd "C-o") 'mural-open-dwim)
```

## Usage

Hit C-o (or the binding of your choice) and the typeahead opens in the
minibuffer.  Typing any sequence of characters will show the top 10
ranked matches in the buffer. Use C-s or [right] to rotate through the
matches, or keep typing to refine them.  Methods are not included in
the results by default, to include them type "::", e.g "B::foo" will
find all methods that fuzzy match class "B" and method "foo".  The
search is case-insensitive when given a lower case string, but will
become case-sensitive if the query contains any upper-case characters.
Hitting enter on a result will take you to the desired file/line
number.

## Author, License, and Contributions

mural was written by Adam Hupp <adam@hupp.org> / <ahupp@fb.com> to
help navigate the Facebook codebase.  It is released under the BSD
license (see the included LICENSE file) and is copyright (c) 2012,
Facebook.

The canonical source for mural can be found at
http://github.com/ahupp/mural.  If you'd like to submit a fix or
feature just send a pull request to that repo.

## Why "mural"?

I live in the Mission District of San Franisco which is full of both
ugly graffiti (aka "tags") and beautiful murals.  While both are just
paint on a wall, the former makes my life a little more frustrating
while the latter makes it happier.

## Implementation Notes

On startup `mural-add-tagfile` spawns a new `mural_server` process
that loads the selected tagfile.  Each key press in the typeahead
sends a query string over stdin to this process.  The process returns
zero or more tab-delimited lines of the form

   MATCH tag file line

Followed by the string "DONE metadata".  If inotify detects that the
tag file has changed the process will reload it.

`mural_server` does not use any fancy indexing, this is a brute force
search across the tags.  Fuzzy search does not obviously lend itself
to a precomputed index and the current form is Fast Enough for Me.
The most important optimizations are avoiding excessive copies in the
filter step and using partial_sort rather than sort to find the top N
results.  These and a handful of other minor changes take the search
from 1.3s to 35ms on my machine with a large tags file.

Results are ranked by counting the number of intervening characters
between successive matches (smaller is better).  For example, if we
query for "profile_help" against the tags "profile_get_path_to_help"
and "profile_help", there are zero characters between successive
matches in the latter and many in the former.  Intuitively, we want to
prefer exact matches to partial matches.  Sybmol length breaks ties,
shorter is better.  This was the simplest thing that seemed to work
and I'm sure there's better solutions if I thought about it for more
than 10 seconds.

I don't use C++ in my day job, and this is the first I've written in
10 years.  If there's any egregious bad style or errors that's why.

The typeahead UI is built on ido-mode and hooks in with with
`defadvice` to customize the results.

