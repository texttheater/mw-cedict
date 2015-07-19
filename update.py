#!/usr/bin/env python

from __future__ import print_function

import config
import json
import logging
import mwclient
import re
import sys
import urllib

from xml.sax.saxutils import escape

try:
    _, dictfile = sys.argv
except ValueError:
    print('Usage: python update.py cedict.json', file=sys.stderr)
    sys.exit(1)

reference_pattern = re.compile(r'([^ []+)\[[^]]+\]')

def format_explanation(explanation):
    def reference_match_to_hyperlink(match):
        search_string = urllib.quote(match.group(0).encode('UTF-8'))
        reference_text = escape(match.group(1))
        return u'[https://www.mdbg.net/chindict/chindict.php?page=worddict&wdrst=0&wdqb={} {}]'.format(
                search_string, reference_text)
    return reference_pattern.sub(reference_match_to_hyperlink, explanation)

site = mwclient.Site(config.host, config.path)
site.login(config.username, config.password)

with open(sys.argv[1]) as f:
    for entry in json.load(f):
        page = site.Pages[u'CEDICT:{}'.format(entry['simplified'])]
        page.save(u"""\
    <table class="wikitable cedict-entry">
      <onlyinclude>
        <tr>
          <td>{}</td>
          <td>{}</td>
          <td>{}</td>
        </tr>
      </onlyinclude>
    </table>""".format(entry['simplified'], entry['pinyin'],
            format_explanation(entry['explanation'])), bot=True)
    # TODO add source and license information

# TODO add templates
# TODO add CSS?
