mw-cedict
=========

This is a little software package that lets you import the
[CC-CEDICT](http://www.mdbg.net/chindict/chindict.php?page=cc-cedict)
Chinese-English dictionary into an existing MediaWiki instance. It’s useful
e.g. for creating custom vocabulary lists on the wiki.

Requirements
------------

* A Unix-like system with standard tools (Make, wget etc.)
* [SWI-Prolog](http://www.swi-prolog.org/) 6.3 or higher
* [pinyin](https://github.com/texttheater/pinyin/)
* Python 2.7
* [mwclient](https://github.com/mwclient/mwclient)
* API write access to a [MediaWiki](https://www.mediawiki.org/) instance

Usage
-----

1. Copy `config.py.sample` to `config.py` and fill in the details of your wiki.
2. Run `make update` to upload the dictionary to your wiki. This can take a long
   time.
3. …

To be continued.
