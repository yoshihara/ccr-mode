# ccr-mode

## Description

Major mode for editing Commit Comment Report.

This is for [commit-comment-service](http://www.clear-code.com/services/commit-comment.html).

## Settings

put below configuration to your .emacs or init.el.

    (add-to-list 'load-path "/path/to/ccr-mode_clone_directory")
    (require 'ccr-mode)

## Usage

* In a report file, ```M-x ccr-mode RET```.
* Insert date (YYYY-MM-DD:%:): ```C-c d```
* Open commit page (in GitHub only) with your browser: ```C-c o```
