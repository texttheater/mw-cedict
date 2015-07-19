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
    explanation = explanation.replace('/', ' / ')
    explanation = reference_pattern.sub(reference_match_to_hyperlink, explanation)
    return explanation

site = mwclient.Site(config.host, config.path)
site.login(config.username, config.password)

with open(sys.argv[1]) as f:
    page = site.Pages['Template:CC-CEDICT attribution']
    page.save("""\
The contents of this page are based on dictionary entries from [https://www.mdbg.net/chindict/chindict.php?page=cc-cedict CC-CEDICT] and licensed under a [https://creativecommons.org/licenses/by-sa/3.0/ Creative Commons Attribution-Share Alike 3.0 License].""")
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
</table>

{{{{CC-CEDICT attribution}}}}""".format(entry['simplified'], entry['pinyin'],
            format_explanation(entry['explanation'])), bot=True)

# TODO add templates
# TODO add CSS?
