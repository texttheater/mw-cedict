cedict.json : cedict.txt cedict.pl
	swipl -l cedict -g "use_module(library(http/json)), cedict_read_file('$<', Dict), tell('$@'), json_write(current_output, Dict), told, halt"

cedict.txt : cedict_1_0_ts_utf-8_mdbg.txt.gz
	zcat $< | sed -e 's/\r//' > $@

cedict_1_0_ts_utf-8_mdbg.txt.gz :
	wget http://www.mdbg.net/chindict/export/cedict/$@

clean :
	rm -f cedict.json cedict*.txt cedict*.txt.gz

update : cedict.json
	./update.py $<
