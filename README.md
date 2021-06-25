
# Table of Contents

1.  [Requirements and Installation](#org29ed7ad)
2.  [Basic idea](#org91d6ab1)
    1.  [Simple Configuration](#org07f4330)
3.  [Vocabulary](#orgc8a8f6c)
    1.  [Item](#org8246692)
    2.  [Source](#orgcedbd4a)
4.  [The modules](#orgacefa84)
    1.  [Notmuch](#org21c05e3)
        1.  [Capturing and backlinks](#org3b156c6)
    2.  [Mu4e](#org2086ced)
    3.  [Elfeed](#org8293d89)
    4.  [Org](#orgc672681)
        1.  [Convenience functions](#orgb184da4)
        2.  [Property hooks](#orgf2986c2)
    5.  [Space](#org9c24572)
        1.  [Relevant variables](#org44f8adf)
        2.  [Relevant functions](#org2c9a431)
    6.  [Function](#org85cae8e)
    7.  [Tasks](#org3770ac9)
5.  [Creating a new source](#orgac2062a)
6.  [Thanks](#orgdfaf019)
7.  [Contribute](#org4ee402d)
    1.  [Moar sources!](#org1e365a1)
    2.  [Loading in Emacs](#orgc07a030)

A framework for reviewing your tasks.


<a id="org29ed7ad"></a>

# Requirements and Installation

`el-secretario` has support for a number of external packages which needs to be
installed separately if you want to use them.

-   [Org-mode](https://orgmode.org/)
-   [Notmuch](https://notmuchmail.org/)
-   [Mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html)
-   [Elfeed](https://github.com/skeeto/elfeed)

Install this package with the method of your choosing. Currently it isn&rsquo;t on
Melpa so a method that uses git is preferred.

If you are using doom:

    (package! el-secretario
      :recipe (:repo "https://git.sr.ht/~zetagon/el-secretario"))

No calls to `require` or `use-package` is needed
since all relevant functions are properly autoloaded. If you want the default
bindings use:

    (eval-after-load 'el-secretario
      '(require 'el-secretario-keybindings))


<a id="org91d6ab1"></a>

# Basic idea

The basic idea is that there are a bunch of decisions you don&rsquo;t want to make, so
you have hired a secretary to do them for you. Your secretary will help you go
through your list of tasks by showing you the relevant tasks one by one. This
eliminates the information overload and decision paralysis you get from 10 000
line todo file, and it lets you focus on each task in an isolated controlled environment.

This was all very abstract so let&rsquo;s move on to a concrete example:


<a id="org07f4330"></a>

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
          (el-secretario-org-make-source (todo "TODO") ("~/org/orgzly/Todo.org"))))))

Above is a sample configuration, and here is a [gif](https://zetagon.srht.site/demo-edited.gif) where it&rsquo;s used in action.

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


<a id="orgc8a8f6c"></a>

# Vocabulary


<a id="org8246692"></a>

## Item

The fundamental building block. Items are the thing that you review from each
source, for example an email or a todo.


<a id="orgcedbd4a"></a>

## Source

A session consists of a list of sources. Each source consists of a list of items.

The example above has one notmuch source, and two org sources.


<a id="orgacefa84"></a>

# The modules

`el-secretario` comes with a set of modules that will help you with reviewing
various parts of your system.

In general functions and variables that are for users follow the format
`el-secretario-MODULE-NAME` and names for developers follow the format
`el-secretario-MODULE--NAME` (notice the two dashes). Interactive commands use
the `el-secretario-MODULE/NAME` format.


<a id="org21c05e3"></a>

## Notmuch

A very simple module that goes through your email in chronological order. The
relevant function is just `el-secretario-notmuch-make-source`, look at it&rsquo;s
docstring for more info.


<a id="org3b156c6"></a>

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


<a id="org2086ced"></a>

## Mu4e

Very similar to the notmuch module. It goes through your mu4e email. The
relevant function is just `el-secretario-mu4e-make-source`, look at it&rsquo;s
docstring for more info.


<a id="org8293d89"></a>

## Elfeed

Very similar to the notmuch module. It goes through your elfeed items in
chronological order, oldest first. The relevant function is just
`el-secretario-elfeed-make-source`, look at it&rsquo;s docstring for more info.


<a id="orgc672681"></a>

## Org

A very simple module that goes through your todos. The relevant function is just
`el-secretario-org-make-source`, look at it&rsquo;s docstring for more info.


<a id="orgb184da4"></a>

### Convenience functions

This module has some convenience functions:

-   `el-secretario-org-remove-tag`
-   `el-secretario-org-up-heading`


<a id="orgf2986c2"></a>

### Property hooks

Property hooks are similar to normal [hooks](https://www.gnu.org/software/emacs/manual/html_node/elisp/Hooks.html) in that they allow the user to run
custom code at specific points in time. The difference is that property hooks
are defined by setting a property to a headline which means that they are local
to the headline. You can set a property hook by adding the corresponding
property with unquoted lisp code as value. You can run your own property hooks
with the function `el-secretario-tasks--run-task-hook`.

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
<td class="org-left">When shown in a review</td>
</tr>


<tr>
<td class="org-left">EL-SECRETARIO-FINISH-TASK-HOOK</td>
<td class="org-left">When a task is marked as done</td>
</tr>


<tr>
<td class="org-left">EL-SECRETARIO-BEGIN-TASK-HOOK</td>
<td class="org-left">When <code>el-secretario-tasks-begin-task</code> is called</td>
</tr>
</tbody>
</table>

1.  EL-SECRETARIO-FINISH-TASK-HOOK

    In order to make this work add the following to your config.
    
        (add-hook 'org-after-todo-state-change-hook #'el-secretario-tasks--finish-task-hook)

2.  EL-SECRETARIO-BEGIN-TASK-HOOK

    This hook has a default value that is `(org-clock-in)` which means you can use
    `el-secretario-tasks-begin-task` instead of `org-clock-in`. Keep in mind though
    that if you set this property you have to use the value `(progn (org-clock-in)
    ...)` to retain the clock in behaviour.


<a id="org9c24572"></a>

## Space

A spaced repetition module for tasks (and not memorization!). When you begin to
have lots of todos it becomes very tiring to review all of them all the time.
This module provides a way to defer todos into the future using a crude spaced
repetition algorithm (the length of the deferral is incremented by one day each time).

Currently this module doesn&rsquo;t stand on it&rsquo;s own and serves more as a library
that augments the [org module](#orgc672681). See [my config](https://github.com/Zetagon/literate-dotfiles/blob/master/config.org#el-secretario) for an example of how to use it.


<a id="org44f8adf"></a>

### Relevant variables

-   `el-secretario-space-increment-percentage`


<a id="org2c9a431"></a>

### Relevant functions

-   `el-secretario-space-reschedule`
-   `el-secretario-space-schedule-and-reset`
-   `el-secretario-space-compare-le`
    
    Passing this function as a comparison function to `make-el-secretario-source`
    will ensure that you review your items sorted so that the earliest scheduled
    items comes first. This can be useful to create a queue of tasks that are
    roughly sorted by how relevant they are.


<a id="org85cae8e"></a>

## Function

An extremely simple source for when you want a function to be called
automatically during a specific time in the review. It calls the provided
function each time the source is activated and goes to the next source
immediately when `el-secretario/next-item` is called. To use it put
`(el-secretario-function-source :func #'YOUR-FUNCTION)` in your source list.


<a id="org3770ac9"></a>

## TODO Tasks

This one is still a little bit weird and I don&rsquo;t exactly know what it&rsquo;s supposed
to do so ignore it for now!


<a id="orgac2062a"></a>

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


<a id="orgdfaf019"></a>

# Thanks

`el-secretario` is mostly a glue-package. It couldn&rsquo;t exists without all the
fantastic things it glues together! Huge thanks to the creators of:

-   [Org-mode](https://orgmode.org/)
-   [Notmuch](https://notmuchmail.org/)
-   [Mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html)
-   [Elfeed](https://github.com/skeeto/elfeed)
-   [Hercules](https://melpa.org/#/hercules)
-   [which-key](https://github.com/justbur/emacs-which-key)
-   [Emacs](https://www.gnu.org/software/emacs/)


<a id="org4ee402d"></a>

# Contribute

I am currently trying out sourcehut. Send any patches or
comments to <mailto:~zetagon/el-secretario@lists.sr.ht>. You can browse the list
[here](https://lists.sr.ht/~zetagon/el-secretario).

Use it and come with feedback on basically everything! Usability, features, bugs
etc. I have a hard time figuring out what&rsquo;s unclear in the documentation so
feedback on that is especially welcome.

It would be very nice to have a mascot for the project though, so I would be
very happy if you would contribute with a nice drawing.


<a id="org1e365a1"></a>

## Moar sources!

El secretario gets better the more sources there are. I&rsquo;d be very happy to
review contributions with new sources or improvements over existing ones. See
[Creating a new source](#orgac2062a).


<a id="orgc07a030"></a>

## Loading in Emacs

I still don&rsquo;t know how to properly load things in an Emacs package, if you know how to properly split parts of this package into separate modules so that the user can choose which parts they want to load, feel more than free to submit a patch. In the meantime I will not prioritize this and just load everything when the main module is loaded.

