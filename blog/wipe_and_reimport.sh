blog-utils --list | cut -f 1 -d ' ' | xargs -n 1 blog-utils --delete-post
ls ../*html | grep -v comment | xargs -n 1 blog-utils --add-from-file 
blog-utils --list | cut -f 1 -d ' ' | xargs -n 1 blog-utils --set-post-visible
blog-utils --list | cut -f 1 -d ' ' | xargs -n 1 blog-utils --show | grep '^comment' | cut -f 3 -d ' ' | xargs -n 1 blog-utils --set-comment-visible
