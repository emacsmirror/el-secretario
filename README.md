
# Table of Contents

1.  [Requirements and Installation](#orgb499bc1)
2.  [Introducing my secretary](#org987a0f8)
    1.  [Simple Configuration](#orgbea3ebe)
3.  [Vocabulary](#org3877d29)
    1.  [Item](#orgaaa458f)
    2.  [Source](#org76d41de)
4.  [The modules](#orgd92e27c)
    1.  [Notmuch](#org5a7bc00)
        1.  [Capturing and backlinks](#org4610422)
    2.  [Mu4e](#org726e6cf)
    3.  [Elfeed](#org8fb8d86)
    4.  [Org](#orgff72214)
        1.  [Tag state machine](#org1dfa505)
        2.  [Convenience functions](#org3651baf)
        3.  [Property hooks](#orgfa464d4)
    5.  [Space](#orgbdbc083)
        1.  [Relevant variables](#org06a9279)
        2.  [Relevant functions](#orgeeafbee)
    6.  [Files](#org432b4ea)
        1.  [Example](#orgce813bd)
    7.  [Function](#orgb591a16)
    8.  [Tasks](#org5bb17c6)
5.  [Creating a new source](#org254e54f)
6.  [Thanks](#orgc9a26ac)
7.  [Contribute](#org3422a45)
    1.  [Moar sources!](#orgf114ee6)
    2.  [Loading in Emacs](#org3e14afb)

The Emacs secretary that helps you through all your inboxes and tasks.


<a id="orgb499bc1"></a>

# Requirements and Installation

`el-secretario` has support for a number of external packages which needs to be
installed separately if you want to use them.

-   [Org-mode](https://orgmode.org/)
-   [org-ql](https://github.com/alphapapa/org-ql)
-   [Hercules](https://melpa.org/#/hercules)
-   [Notmuch](https://notmuchmail.org/)
-   [Mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html)
-   [Elfeed](https://github.com/skeeto/elfeed)

Install this package with the method of your choosing.

If you install the package from source, you need to install at least these Emacs Lisp dependencies manually:

-   [org-ql](https://github.com/alphapapa/org-ql)
-   [Hercules](https://melpa.org/#/hercules)

Install if you are using doom:

    (package! el-secretario
      :recipe (:repo "https://git.sr.ht/~zetagon/el-secretario"))

No calls to `require` or `use-package` is needed since all relevant functions are properly autoloaded.

Install manually from MELPA: `M-x package-install RET el-secretario RET`

Or via `use-package`:

    (use-package el-secretario)


<a id="org987a0f8"></a>

# Introducing my secretary

There are at least two fundamental ways of reading email. The first, and the one I think is more common, is to open the inbox and choose an email to read from the list. Let&rsquo;s call it the random access method. The other method is to open an email, preferably the oldest unread one, and when you are done open the next one by pressing a &ldquo;next email&rdquo;  button. Let&rsquo;s call it the linked list method.

This package was born from the realization that I like the linked list method, and that I would like to handle more things this way. So what el secretario does is that he turns different sources (e.g. org-mode todo items or RSS feeds) into linked list-style inboxes. And he doesn&rsquo;t stop there, he can also link different lists together so that email, org-mode items and RSS feeds come under the same unified inbox. El secretario can already turn many different [things](#orgd92e27c) into inboxes but he can also learn new things if you [teach](#org254e54f) him.

This was all very abstract so let&rsquo;s move on to a concrete example:


<a id="orgbea3ebe"></a>

## Simple Configuration

    ;; Create a function to start the review
    (defun el-secretario-daily-review ()
      (interactive)
      (el-secretario-start-session
       (lambda ()
         (list
    
          ;; First take care of email
          (el-secretario-notmuch-make-source "tag:lists/emacs-orgmode")
          ;; Then Take care of inbox
          (el-secretario-org-make-source nil ("~/org/Inbox.org"))
    
          ;; Go through TODOs
          (el-secretario-org-make-source '(todo "TODO") '("~/org/Todo.org"))))))

Above is a sample configuration, and here is a [gif](https://zetagon.srht.site/demo-edited.gif) where its used in action.

Calling `el-secretario-daily-review` will open up your oldest email. Pressing
`n` in the [which-key](https://github.com/justbur/emacs-which-key) prompt will take you to the next email sorted chronologically.
In this way your secretary will make sure you go through all your email without
having to worry about which or how many emails you will read.

Then when your secretary has gone through all your email for you, they will next
go through your todo inbox. Pressing `n` will in a similar way take you to the
next item in your inbox. When you&rsquo;ve refiled your tasks appropriately your
secretary will go through all your existing todos over in `Todo.org`

This uses the default keybindings this package provides. See
<el-secretario-keybindings.el>. This file isn&rsquo;t loaded automatically. Use
`require` or `use-package` for that. It&rsquo;s easy to add your own keybindings! Use
whatever keybinding mechanism you use to add keybindings the respective source&rsquo;s
keymap.

A more complete configuration can be found at [my configuration](https://github.com/Zetagon/literate-dotfiles/blob/master/config.org#el-secretario).


<a id="org3877d29"></a>

# Vocabulary


<a id="orgaaa458f"></a>

## Item

The fundamental building block. Items are the thing that you review from each
source, for example an email or a todo.


<a id="org76d41de"></a>

## Source

A session consists of a list of sources. Each source consists of a list of items.

The example above has one notmuch source, and two org sources.


<a id="orgd92e27c"></a>

# The modules

`el-secretario` comes with a set of modules that will help you with reviewing
various parts of your system.

In general functions and variables that are for users follow the format
`el-secretario-MODULE-NAME` and names for developers follow the format
`el-secretario-MODULE--NAME` (notice the two dashes).


<a id="org5a7bc00"></a>

## Notmuch

A very simple module that goes through your email in chronological order. The
relevant function is just `el-secretario-notmuch-make-source`, look at its
docstring for more info.


<a id="org4610422"></a>

### Capturing and backlinks

`el-secretario-notmuch-capture-get-thread-link` can be used in a capture
template to get a link to the thread of the current email message. During review
org entries that link to the current thread or message will automatically show
in the status buffer as backlinks.

    (setq org-capture-templates
          '(("e" "Email" entry (file "~/org/Inbox.org")
             "* TODO %a
    %(el-secretario-notmuch-capture-get-thread-link)")))

Note that the backlinks buffer will only be populated when
`el-secretario--notmuch-search-show-thread` (our own version of
`notmuch-show-next-thread-show`) is called. `M-x
el-secretario-notmuch/open-link-for-current-email` can be used to open and
populate the backlinks buffer manually.


<a id="org726e6cf"></a>

## Mu4e

Very similar to the notmuch module. It goes through your mu4e email. The
relevant function is just `el-secretario-mu4e-make-source`, look at its
docstring for more info.


<a id="org8fb8d86"></a>

## Elfeed

Very similar to the notmuch module. It goes through your elfeed items in
chronological order, oldest first. The relevant function is just
`el-secretario-elfeed-make-source`, look at its docstring for more info.


<a id="orgff72214"></a>

## Org

A very simple module that goes through your todos. The relevant function is just
`el-secretario-org-make-source`, look at its docstring for more info.


<a id="org1dfa505"></a>

### Tag state machine

El secretario can update tags of headings according to a state machine. The
first time you review an item one state transition is done. The state machine is
defined per source with the `TAG-TRANSITIONS` argument to
`el-secretario-org-make-source`. It is a list of `(TAG . NEW-TAG)` cons pairs.
Each reviewed heading that has the tag `TAG` gets the tag `TAG` removed and
`NEW-TAG` added. If `TAG` is the empty string `NEW-TAG` is always added.

1.  Example

        (el-secretario-org-make-source '(todo)
                                       "~/org/Todo.org"
                                       :tag-transitions
                                       '(("a" . "b")
                                         ("b" . "c")
                                         ("" . "d")
                                         ("d" . "")))
    
        * TODO Foo :a:
        * TODO Bar :b:
    
    With the el-secretario source and org file above, one review will result in the
    org file below. All &ldquo;a&rdquo; tags have turned into &ldquo;b&rdquo; tags, and all &ldquo;b&rdquo; tags have
    turned into &ldquo;c&rdquo; tags. &ldquo;d&rdquo; is added to both.
    
        * TODO Foo :b:d:
        * TODO Bar :c:d:
    
    A second review will have converted all tags to &ldquo;c&rdquo;.
    
        * TODO Foo :c:
        * TODO Bar :c:


<a id="org3651baf"></a>

### Convenience functions

This module has some convenience functions:

-   `el-secretario-org-remove-tag`
-   `el-secretario-org-up-heading`


<a id="orgfa464d4"></a>

### Property hooks

Property hooks are similar to normal [hooks](https://www.gnu.org/software/emacs/manual/html_node/elisp/Hooks.html) in that they allow the user to run
custom code at specific points in time. The difference is that property hooks
are defined by setting a property to a headline which means that they are local
to the headline. You can set a property hook by adding the corresponding
property with unquoted lisp code as value. You can run your own property hooks
with the function `el-secretario-org--run-property-hook`.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Property</th>
<th scope="col" class="org-left">Run condition</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">EL-SECRETARIO-REVIEW-TASK-HOOK</td>
<td class="org-left">When shown in a review in the org source</td>
</tr>
</tbody>
</table>

1.  Run property hook when marking a task as finished

    In order to run a function when a specific task is done, you can add the following to your config.
    
        (add-hook 'org-after-todo-state-change-hook #'el-secretario-tasks--finish-task-hook)
        (defun my/el-secretario-run-finish-task-hook ()
          (when (member org-state org-done-keywords)
            (el-secretario-org--run-property-hook (el-secretario-org--parse-headline)
                             :EL-SECRETARIO-FINISH-TASK-HOOK)))


<a id="orgbdbc083"></a>

## Space

A spaced repetition module for tasks (and not memorization!). When you begin to
have lots of todos it becomes very tiring to review all of them all the time.
This module provides a way to defer todos into the future using a crude spaced
repetition algorithm (the length of the deferral is incremented by one day each time).

Currently this module doesn&rsquo;t stand on it&rsquo;s own and serves more as a library
that augments the [org module](#orgff72214). See [my config](https://github.com/Zetagon/literate-dotfiles/blob/master/config.org#el-secretario) for an example of how to use it.


<a id="org06a9279"></a>

### Relevant variables

-   `el-secretario-org-space-increment-percentage`


<a id="orgeeafbee"></a>

### Relevant functions

-   `el-secretario-org-space-reschedule`
-   `el-secretario-org-space-schedule-and-reset`
-   `el-secretario-org-space-compare-le`
    
    Passing this function as a comparison function to `make-el-secretario-source`
    will ensure that you review your items sorted so that the earliest scheduled
    items comes first. This can be useful to create a queue of tasks that are
    roughly sorted by how relevant they are.


<a id="org432b4ea"></a>

## Files

A simple module that goes through a list of files in order.
`el-secretario-files-make-source` is the entry point.


<a id="orgce813bd"></a>

### Example

Visit all your downloaded files:

    (el-secretario-start-session
     (el-secretario-files-make-source (directory-files "~/Downloads")))


<a id="orgb591a16"></a>

## Function

An extremely simple source for when you want a function to be called
automatically during a specific time in the review. It calls the provided
function each time the source is activated and goes to the next source
immediately when `el-secretario-next-item` is called. To use it put
`(el-secretario-function-source :func #'YOUR-FUNCTION)` in your source list.


<a id="org5bb17c6"></a>

## TODO Tasks

This one is still a little bit weird and I don&rsquo;t exactly know what it&rsquo;s supposed
to do so ignore it for now!


<a id="org254e54f"></a>

# Creating a new source

A source is a [eieio](eieio#Top) class that inherits from `el-secretario-source`. It needs to
implement the following methods:

-   `el-secretario-source-next-item`
-   `el-secretario-source-previous-item`
-   `el-secretario-source-activate`

Optionally `el-secretario-source-init` can be implemented if your source needs
to do some setup only once (e.g. setup some state).

See the docstrings for respective method for what they are supposed to do.

Each source can fill the `keymap` slot (as defined in `el-secretario-source`)
with a keymap. Otherwise the default keymap will be used.

See [the example source](./el-secretario-example.el) and [its unit tests](tests/test-el-secretario.el).


<a id="orgc9a26ac"></a>

# Thanks

`el-secretario` is mostly a glue-package. It couldn&rsquo;t exists without all the
fantastic things it glues together! Huge thanks to the creators of:

-   [Org-mode](https://orgmode.org/)
-   [Notmuch](https://notmuchmail.org/)
-   [Mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html)
-   [Elfeed](https://github.com/skeeto/elfeed)
-   [Hercules](https://melpa.org/#/hercules)
-   [which-key](https://github.com/justbur/emacs-which-key)
-   [dash.el](https://github.com/magnars/dash.el)
-   [Emacs](https://www.gnu.org/software/emacs/)


<a id="org3422a45"></a>

# Contribute

I am currently trying out sourcehut. Send any patches or
comments to <https://lists.sr.ht/~zetagon/el-secretario-devel>. Bugs should be reported to <https://todo.sr.ht/~zetagon/el-secretario> (which is also a place you could look at to find things to contribute).

Use it and come with feedback on basically everything! Usability, features, bugs
etc. I have a hard time figuring out what&rsquo;s unclear in the documentation so
feedback on that is especially welcome.

It would be very nice to have a mascot for the project, so I would be
very happy if you would contribute with a nice drawing.


<a id="orgf114ee6"></a>

## Moar sources!

`el-Secretario` gets better the more sources there are. I&rsquo;d be very happy to
review contributions with new sources or improvements over existing ones. See
[Creating a new source](#org254e54f).


<a id="org3e14afb"></a>

## Loading in Emacs

I still don&rsquo;t know how to properly load things in an Emacs package, if you know how to properly split parts of this package into separate modules so that the user can choose which parts they want to load, feel more than free to submit a patch. In the meantime I will not prioritize this and just load everything when the main module is loaded.

