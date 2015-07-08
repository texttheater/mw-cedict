#!/usr/bin/env python

from __future__ import print_function

import config
import json
import logging
import mwclient
import sys

try:
    _, dictfile = sys.argv
except ValueError:
    print('Usage: python update.py cedict.json', file=sys.stderr)
    sys.exit(1)

site = mwclient.Site('texttheater.net', '/wiki/')
site.login(config.username, config.password)

with open(sys.argv[1]) as f:
    for entry in json.load(f):
        page = site.Pages[u'CEDICT:{}'.format(entry['simplfied'])]
        page.save(u"""\
    <table class=cedict-entry>
      <onlyinclude>
        <tr>
          <td>{}</td>
          <td>{}</td>
          <td>{}</td>
        </tr>
      </onlyinclude>
    </table>""".format(entry['simplfied'], entry['pinyin'], entry['explanation']),
                bot=True)
    # TODO add source and license information

# TODO add templates
# TODO add CSS?