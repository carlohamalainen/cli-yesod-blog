#!/usr/bin/env python
# -*- coding: utf-8 -*- 

# Quick hack for extracting Wordpress blog posts and comments from a MySQL database. Very minimal;
# ignores user accounts, URLs of commenters, etc.

import MySQLdb.converters
import time
import sqlobject
import unicodedata

def strip_accents(s):
    """
    Strip out accented characters. Probably got this from Stackoverflow.
    """

    return ''.join(c for c in unicodedata.normalize('NFD', s) if unicodedata.category(c) != 'Mn')

def _mysql_timestamp_converter(raw):
    """
    Convert a MySQL TIMESTAMP to a floating point number representing
    the seconds since the Un*x Epoch. It uses custom code the input seems
    to be the new (MySQL 4.1+) timestamp format, otherwise code from the
    MySQLdb module is used.

    Presumably got this off Stackoverflow too.
    """

    if raw[4] == '-':
        return time.mktime(time.strptime(raw, '%Y-%m-%d %H:%M:%S'))
    else:
        return MySQLdb.converters.mysql_timestamp_converter(raw)

# | wp_ak_twitter             |
# | wp_commentmeta            |
# | wp_comments               |
# | wp_links                  |
# | wp_options                |
# | wp_postmeta               |
# | wp_posts                  |
# | wp_term_relationships     |
# | wp_term_taxonomy          |
# | wp_terms                  |
# | wp_usermeta               |
# | wp_users                  |

if __name__ == '__main__':
    conversions = MySQLdb.converters.conversions.copy()
    conversions[MySQLdb.constants.FIELD_TYPE.TIMESTAMP] = _mysql_timestamp_converter

    MySQLConnection = sqlobject.mysql.builder()

    user = 'root'
    import getpass
    password = getpass.getpass()
    db = 'wordpresscarlo2'

    connection = MySQLConnection(user=user, password=password, db=db, conv=conversions)

    class wp_posts(sqlobject.SQLObject):
        class sqlmeta:
            fromDatabase = True
            table = 'wp_posts'
        _connection = connection

    class wp_comments(sqlobject.SQLObject):
        class sqlmeta:
            fromDatabase = True
            table = 'wp_comments'
            idName = 'comment_ID'
        _connection = connection

    published_posts = []

    for p in wp_posts.select():
        if p.postStatus != 'publish': continue

        comments = [(c.commentAuthor, c.commentContent) for c in wp_comments.select(wp_comments.q.commentPostID==p.id) if c.commentApproved == '1' and c.commentType == '']
        print p.postDate.year, p.postDate.month, p.postDate.day, strip_accents(p.postTitle.decode('latin-1')), comments


    # FIXME reverse comments? Sort by date?


