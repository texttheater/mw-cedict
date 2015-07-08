default : cedict.json

update : cedict.json
	./update.py $<

cedict.json : cedict.txt cedict.pl
	swipl -l cedict -g "use_module(library(http/json)), cedict_read_file('$<', Dict), tell('$@'), json_write(current_output, Dict), told, halt"

cedict.txt : cedict_1_0_ts_utf-8_mdbg.txt
	cat $< | sed -e 's/\r//' > $@

cedict_1_0_ts_utf-8_mdbg.txt :
	wget http://www.mdbg.net/chindict/export/cedict/cedict_1_0_ts_utf-8_mdbg.txt.gz
