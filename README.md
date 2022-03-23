The Emacs secretary that helps you through all your inboxes and tasks.

-   Project page: <https://sr.ht/~zetagon/el-secretario/>
-   Patches or general discussions: <https://lists.sr.ht/~zetagon/el-secretario-devel>
-   Bug reports: <https://todo.sr.ht/~zetagon/el-secretario>


# Requirements and Installation

`el-secretario` is available on melpa and is divided into different packages:

-   **`el-secretario`:** The main package
-   **`el-secretario-org`:** [Org-mode](https://orgmode.org/) integration
-   **`el-secretario-notmuch`:** [Notmuch](https://notmuchmail.org/) integration
-   **`el-secretario-mu4e`:** [Mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html) integration
-   **`el-secretario-elfeed`:** [Elfeed](https://github.com/skeeto/elfeed) integration

**Note:** `el-secretario` depends on a newer version of which-key than might be on elpa. See [Bug:  which-key: No bindings found in el-secretario-org-keymap](https://todo.sr.ht/~zetagon/el-secretario/2)

No calls to `require` (explicit or with `use-package`) is needed since all relevant functions are properly autoloaded.

If you are using doom:

    (package! el-secretario)

Install manually from MELPA: `M-x package-install RET el-secretario RET`

Or via `use-package`:

    (use-package el-secretario
      :defer t)


# Introducing my secretary

There are at least two fundamental ways of reading email. The first, and the one I think is more common, is to open the inbox and choose an email to read from the list. Let&rsquo;s call it the random access method. The other method is to open an email, preferably the oldest unread one, and when you are done open the next one by pressing a &ldquo;next email&rdquo;  button. Let&rsquo;s call it the linked list method.

This package was born from the realization that I like the linked list method, and that I would like to handle more things this way. So what el secretario does is that he turns different sources (e.g. org-mode todo items or RSS feeds) into linked list-style inboxes. And he doesn&rsquo;t stop there, he can also link different lists together so that email, org-mode items and RSS feeds come under the same unified inbox. El secretario can already turn many different [things](#org7433bdc) into inboxes but he can also learn new things if you [teach](#orgf29004b) him.

This was all very abstract so let&rsquo;s move on to a concrete example:


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

Pressing a key that isn&rsquo;t in the `which-key` menu will disable the menu, which
essentially pauses the review.  You can reactivate it with
`el-secretario-activate-keymap`.

A more complete configuration can be found at [my configuration](https://github.com/Zetagon/literate-dotfiles/blob/master/config.org#el-secretario).


# Vocabulary


## Item

The fundamental building block. Items are the thing that you review from each
source, for example an email or a todo.


## Source

A session consists of a list of sources. Each source consists of a list of items.

The example above has one notmuch source, and two org sources.


<a id="org7433bdc"></a>

# The modules

`el-secretario` comes with a set of modules that will help you with reviewing
various parts of your system.

In general functions and variables that are for users follow the format
`el-secretario-MODULE-NAME` and names for developers follow the format
`el-secretario-MODULE--NAME` (notice the two dashes).


## Notmuch

A very simple module that goes through your email in chronological order. The
relevant function is just `el-secretario-notmuch-make-source`, look at its
docstring for more info.


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


## Mu4e

Very similar to the notmuch module. It goes through your mu4e email. The
relevant function is just `el-secretario-mu4e-make-source`, look at its
docstring for more info.


## Elfeed

Very similar to the notmuch module. It goes through your elfeed items in
chronological order, oldest first. The relevant function is just
`el-secretario-elfeed-make-source`, look at its docstring for more info.


<a id="org91de09a"></a>

## Org

A very simple module that goes through your todos. The relevant function is just
`el-secretario-org-make-source`, look at its docstring for more info.


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


### Convenience functions

This module has some convenience functions:

-   `el-secretario-org-remove-tag`
-   `el-secretario-org-up-heading`


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


## Space

A spaced repetition module for tasks (and not memorization!). When you begin to
have lots of todos it becomes very tiring to review all of them all the time.
This module provides a way to defer todos into the future using a crude spaced
repetition algorithm (the length of the deferral is incremented by one day each time).

Currently this module doesn&rsquo;t stand on it&rsquo;s own and serves more as a library
that augments the [org module](#org91de09a). See [my config](https://github.com/Zetagon/literate-dotfiles/blob/master/config.org#el-secretario) for an example of how to use it.


### Relevant variables

-   `el-secretario-org-space-increment-percentage`


### Relevant functions

-   `el-secretario-org-space-reschedule`
-   `el-secretario-org-space-schedule-and-reset`
-   `el-secretario-org-space-compare-le`
    
    Passing this function as a comparison function to `make-el-secretario-source`
    will ensure that you review your items sorted so that the earliest scheduled
    items comes first. This can be useful to create a queue of tasks that are
    roughly sorted by how relevant they are.


## Files

A simple module that goes through a list of files in order.
`el-secretario-files-make-source` is the entry point.


### Example

Visit all your downloaded files:

    (el-secretario-start-session
     (el-secretario-files-make-source (directory-files "~/Downloads")))


## Function

An extremely simple source for when you want a function to be called
automatically during a specific time in the review. It calls the provided
function each time the source is activated and goes to the next source
immediately when `el-secretario-next-item` is called. To use it put
`(el-secretario-function-source :func #'YOUR-FUNCTION)` in your source list.


## TODO Tasks

This one is still a little bit weird and I don&rsquo;t exactly know what it&rsquo;s supposed
to do so ignore it for now!


# Customization


## Keybindings

It&rsquo;s easy to add your own keybindings! Use whatever keybinding mechanism you use
to add keybindings the respective source&rsquo;s keymap.

For example to bind `org-capture` in the org keymap:

    (define-key el-secretario-org-keymap
          "c" '("Capture" . org-capture))

If you want different keybindings for different instances of the same source
type you can provide your own keymap. The example below has two different
keymaps for the two sources.

    (defvar my/el-secretario-org-map (make-sparse-keymap))
    (define-key my/el-secretario-org-keymap
      "c" '("Capture with template a" . (lambda () (interactive) (org-capture nil "a"))))
    
    (defvar my/el-secretario-org-map-2 (make-sparse-keymap))
    (define-key my/el-secretario-org-keymap
      "c" '("Capture with template b" . (lambda () (interactive) (org-capture nil "b"))))
    
    (defun el-secretario-review ()
      (el-secretario-start-session
       (lambda ()
         (list
          (el-secretario-org-make-source '(todo "TODO") '("~/org/Todo.org")
                                         :keymap my/el-secretario-org-map)
    
          (el-secretario-org-make-source '(todo "TODO") '("~/org/Inbox.org")
                                         :keymap my/el-secretario-org-map-2)))))


<a id="orgf29004b"></a>

## Creating a new source

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

See [the example source](./el-secretario-example.el) and [its unit tests](tests/test-el-secretario.el) for an easy to read example.


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


# Contribute

There are three ways to contribute to this project:

-   Feedback
    
    Any feedback is very welcome! Documentation, usability, features etc.

-   Patches
    
    el-secretario is designed to be extensible. [Write your own sources](#orgf29004b) and
    contribute them, or improve the existing ones.

-   Money
    
    I have a ko-fi page if you want to throw money at me: <https://ko-fi.com/zetagon>

It would be very nice to have a mascot for the project, so I would be
very happy if you would contribute with a nice drawing.

