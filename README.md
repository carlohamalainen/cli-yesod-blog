cli-yesod-blog
==============

Yesod blog with a basic CLI for managing posts.

TODO
====

What is going wrong here?

    $ blog-utils --list | cut -f 1 -d ' ' | xargs -n 1 blog-utils --show > /dev/null
    blog-utils: user error (Could not find environment: Production)

Change all url-building done with ++ into something safer like </>.
