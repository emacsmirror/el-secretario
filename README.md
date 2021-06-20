
# Table of Contents

1.  [Basic idea](#orge869f00)
    1.  [Simple Configuration](#org3da6797)
2.  [Vocabulary](#org7a93f00)
    1.  [Item](#orgba2a389)
    2.  [Source](#org1223d97)
3.  [The modules](#org265e6b6)
    1.  [Notmuch](#orgbb22c2a)
        1.  [Capturing and backlinks](#org14ded58)
    2.  [Mu4e](#orgadc9212)
    3.  [Elfeed](#org390404a)
    4.  [Org](#orga8b57ef)
        1.  [Convenience functions](#orgea5907f)
        2.  [Property hooks](#org8b64771)
    5.  [Space](#orgd63d74c)
        1.  [Relevant variables](#org8dbdeac)
        2.  [Relevant functions](#org7523cd3)
    6.  [Function](#orgb64e1d1)
    7.  [Tasks](#org067c13f)
4.  [Creating a new source](#org8045b99)
5.  [Tasks](#org213a92d)
6.  [Contribute](#org0542007)
    1.  [Loading in Emacs](#orgfe07812)

A framework for reviewing your tasks.


<a id="orge869f00"></a>

# Basic idea

The basic idea is that there are a bunch of decisions you don&rsquo;t want to make, so
you have hired a secretary to do them for you. Your secretary will help you go
through your list of tasks by showing you the relevant tasks one by one. This
eliminates the information overload and decision paralysis you get from 10 000
line todo file, and it lets you focus on each task in an isolated controlled environment.

This was all very abstract so let&rsquo;s move on to a concrete example:


<a id="org3da6797"></a>

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
`n` in the [hercules](https://melpa.org/#/hercules) prompt will take you to the next email sorted chronologically.
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


<a id="org7a93f00"></a>

# Vocabulary


<a id="orgba2a389"></a>

## Item

The fundamental building block. Items are the thing that you review from each
source, for example an email or a todo.


<a id="org1223d97"></a>

## Source

A session consists of a list of sources. Each source consists of a list of items.

The example above has one notmuch source, and two org sources.


<a id="org265e6b6"></a>

# The modules

`el-secretario` comes with a set of modules that will help you with reviewing
various parts of your system.

In general functions and variables that are for users follow the format
`el-secretario-MODULE-NAME` and names for developers follow the format
`el-secretario-MODULE--NAME` (notice the two dashes). Interactive commands use
the `el-secretario-MODULE/NAME` format.


<a id="orgbb22c2a"></a>

## Notmuch

A very simple module that goes through your email in chronological order. The
relevant function is just `el-secretario-notmuch-make-source`, look at it&rsquo;s
docstring for more info.


<a id="org14ded58"></a>

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


<a id="orgadc9212"></a>

## Mu4e

Very similar to the notmuch module. It goes through your mu4e email. The
relevant function is just `el-secretario-mu4e-make-source`, look at it&rsquo;s
docstring for more info.


<a id="org390404a"></a>

## Elfeed

Very similar to the notmuch module. It goes through your elfeed items in
chronological order, oldest first. The relevant function is just
`el-secretario-elfeed-make-source`, look at it&rsquo;s docstring for more info.


<a id="orga8b57ef"></a>

## Org

A very simple module that goes through your todos. The relevant function is just
`el-secretario-org-make-source`, look at it&rsquo;s docstring for more info.


<a id="orgea5907f"></a>

### Convenience functions

This module has some convenience functions:

-   `el-secretario-org-remove-tag`
-   `el-secretario-org-up-heading`


<a id="org8b64771"></a>

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


<a id="orgd63d74c"></a>

## Space

A spaced repetition module for tasks (and not memorization!). When you begin to
have lots of todos it becomes very tiring to review all of them all the time.
This module provides a way to defer todos into the future using a crude spaced
repetition algorithm (the length of the deferral is incremented by one day each time).

Currently this module doesn&rsquo;t stand on it&rsquo;s own and serves more as a library
that augments the [org module](#orga8b57ef). See [my config](https://github.com/Zetagon/literate-dotfiles/blob/master/config.org#el-secretario) for an example of how to use it.


<a id="org8dbdeac"></a>

### Relevant variables

-   `el-secretario-space-increment-percentage`


<a id="org7523cd3"></a>

### Relevant functions

-   `el-secretario-space-reschedule`
-   `el-secretario-space-schedule-and-reset`
-   `el-secretario-space-compare-le`
    
    Passing this function as a comparison function to `make-el-secretario-source`
    will ensure that you review your items sorted so that the earliest scheduled
    items comes first. This can be useful to create a queue of tasks that are
    roughly sorted by how relevant they are.


<a id="orgb64e1d1"></a>

## Function

An extremely simple source for when you want a function to be called
automatically during a specific time in the review. It calls the provided
function each time the source is activated and goes to the next source
immediately when `el-secretario/next-item` is called. To use it put
`(el-secretario-function-source :func #'YOUR-FUNCTION)` in your source list.


<a id="org067c13f"></a>

## TODO Tasks

This one is still a little bit weird and I don&rsquo;t exactly know what it&rsquo;s supposed
to do so ignore it for now!


<a id="org8045b99"></a>

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


<a id="org213a92d"></a>

# Tasks

Main entry point is `el-secretario-tasks-choose-task`. See it&rsquo;s docstring for more info.

When you choose to begin **FOO** or **baz** the default begin hook will run, but
when you start **bar** the secretary will run `(insert "foobar")` at the beginning
of the headline. When you finish **baz** the secretary also will run `(insert
"foobar")` at the beginning of the headline.

    * DONE FOO
    :PROPERTIES:
    :EL-SECRETARIO-PRIORITY: 74
    :END:
    
    * bar
    :PROPERTIES:
    :EL-SECRETARIO-PRIORITY: 85
    :EL-SECRETARIO-BEGIN-TASK-HOOK: (insert "foobar")
    :END:
    
    * DONE baz
    :PROPERTIES:
    :EL-SECRETARIO-PRIORITY: 106
    :EL-SECRETARIO-FINISH-TASK-HOOK: (insert "foobar")
    :END:


<a id="org0542007"></a>

# Contribute

I am currently trying out sourcehut so please send any contributions to
<https://git.sr.ht/~zetagon/el-secretario> if you can. Send any patches or
comments to <mailto:~zetagon/el-secretario@lists.sr.ht>. You can browse the list
[here](https://lists.sr.ht/~zetagon/el-secretario).

Use it and come with feedback on basically everything! Usability, features, bugs
etc. I have a hard time figuring out what&rsquo;s unclear in the documentation so
feedback on that is especially welcome.

It would be very nice to have a mascot for the project though, so I would be
very happy if you would contribute with a nice drawing.


<a id="orgfe07812"></a>

## Loading in Emacs

I still don&rsquo;t know how to properly load things in an Emacs package, if you know how to properly split parts of this package into separate modules so that the user can choose which parts they want to load, feel more than free to submit a pull request. In the meantime I will not prioritize this and just load everything when the main module is loaded.

